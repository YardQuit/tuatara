;;;; test/syntax-editing.lisp --- Tree-sitter: slow parses, language modes, commenting, indenting and selecting.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

;; how the parse of a big file stops when it takes too long
(define-test (syntax slow-parses)
  ;; a parse that takes longer than it may is given up: the buffer shows no
  ;; syntax colours, is not parsed again (which would take as long), and the
  ;; editor says so
  (let ((text (with-output-to-string (o)
                (dotimes (i 20000) (format o "fn f~d() {{ let x = ~d; }}~%" i i)))))
    (let* ((grammar (tuatara.syntax:find-grammar "rust"))
           (parser (tuatara.syntax:make-parser grammar (tuatara.syntax:kept-helper :shared))))
      (unwind-protect
           (multiple-value-bind (tree why)
               (tuatara.syntax:parse-rope parser (tuatara.text:string->rope text) :limit 0.0001)
             (false tree)
             (is eq :slow why))
        (tuatara.syntax:delete-parser parser)))
    (let* ((buffer (tuatara.buffer:make-buffer "*slow*" :text text))
           (said nil)
           (tuatara.syntax:*syntax-time-limit* 0.0001)
           (tuatara.syntax:*syntax-time-per-megabyte* 0)
           (tuatara.runtime:*host* (make-answering-host 'tuatara.syntax:host-say-syntax-slow
                                                        (lambda (syntax) (setf said syntax)))))
      (unwind-protect
           (progn
             (tuatara.modes:set-major-mode buffer 'tuatara.defaults::rust-mode)
             (let ((syntax (tuatara.syntax:buffer-syntax buffer)))
               (let ((tuatara.syntax:*syntax-wait* 10)) (tuatara.syntax:syntax-ready-tree syntax))
               (is eq :slow (tuatara.syntax:syntax-broken syntax))
               (is eq syntax said)
               (false (tuatara.syntax:syntax-tree syntax))
               (false (tuatara.syntax:syntax-job syntax))))
        (tuatara.buffer:kill-buffer buffer)))))

(define-test (syntax runaway-parses)
  ;; a parse is given up once it takes more memory than it may, as a grammar
  ;; gone wrong would take without end, and all it took is given back; the
  ;; buffer shows no syntax colours and the editor says so
  (let ((numbers (format nil "[~{~d~^,~}]~%" (loop repeat 100000 collect (random 10))))
        (held (lambda () (held-in-helper (tuatara.syntax:kept-helper :shared)))))
    (let* ((parser (tuatara.syntax:make-parser (tuatara.syntax:find-grammar "json")
                                               (tuatara.syntax:kept-helper :shared)))
           (before (funcall held)))
      (unwind-protect
           (multiple-value-bind (tree why)
               (tuatara.syntax:parse-rope parser (tuatara.text:string->rope numbers) :budget (* 1024 1024))
             (false tree)
             (is eq :memory why))
        (tuatara.syntax:delete-parser parser))
      (true (held-falls-to (tuatara.syntax:kept-helper :shared) (+ before (* 1024 1024)))))
    ;; within the budget its length and tokens call for, it is parsed
    (let ((parser (tuatara.syntax:make-parser (tuatara.syntax:find-grammar "json")
                                              (tuatara.syntax:kept-helper :shared))))
      (unwind-protect
           (let ((tree (tuatara.syntax:parse-rope parser (tuatara.text:string->rope numbers)
                                                  :budget (tuatara.syntax:parse-memory-limit (length numbers)))))
             (true tree)
             (when tree (tuatara.syntax:delete-tree tree)))
        (tuatara.syntax:delete-parser parser)))
    (let* ((buffer (tuatara.buffer:make-buffer "*runaway*" :text numbers))
           (said nil)
           (tuatara.syntax:*syntax-memory-limit* (* 1024 1024))
           (tuatara.syntax:*syntax-memory-per-octet* 0)
           (tuatara.runtime:*host* (make-answering-host 'tuatara.syntax:host-say-syntax-slow
                                                        (lambda (syntax) (setf said syntax)))))
      (unwind-protect
           (progn
             (tuatara.modes:set-major-mode buffer 'tuatara.defaults::json-mode)
             (let ((syntax (tuatara.syntax:buffer-syntax buffer)))
               (let ((tuatara.syntax:*syntax-wait* 10)) (tuatara.syntax:syntax-ready-tree syntax))
               (is eq :memory (tuatara.syntax:syntax-broken syntax))
               (is eq syntax said)
               (false (tuatara.syntax:syntax-tree syntax))))
        (tuatara.buffer:kill-buffer buffer))))
  ;; grammars that built without end, patched: Beancount's on a balance line
  ;; ending in # before a lone * at the end (syntax/patches/beancount.patch),
  ;; Tera's on a lone { at the end, as while typing {{ (tera.patch): parsed at once
  (loop for (grammar text) in (list (list "beancount" (format nil "2014-01-01 balance Assets:Cash 0.0 #~%*~%"))
                                    (list "tera" "{")
                                    (list "tera" (format nil "{% for x in y %}~%  {{ x }}~%{% endfor %}{")))
        do (let ((parser (tuatara.syntax:make-parser (tuatara.syntax:find-grammar grammar)
                                                     (tuatara.syntax:kept-helper :shared)))
                 (start (get-internal-real-time)))
             (unwind-protect
                  (let ((tree (tuatara.syntax:parse-rope parser (tuatara.text:string->rope text)
                                                         :limit 5 :budget (* 64 1024 1024))))
                    (true tree "~a" grammar)
                    (true (< (- (get-internal-real-time) start) (* 5 internal-time-units-per-second)) "~a" grammar)
                    (when tree (tuatara.syntax:delete-tree tree)))
               (tuatara.syntax:delete-parser parser)))))

;; a grammar slow on a large text is not run again after every key
(define-test (syntax slow-update-waits-for-a-pause)
  ;; once a parse after an edit took long (a log of megabytes), the next edit is
  ;; not parsed at once: the parse waits until typing pauses for twice that long,
  ;; and starts then (REFRESH-SYNTAX); quick parses after edits start at once
  (let ((buffer (tuatara.buffer:make-buffer "*pause*" :text (format nil "fn main() {~%    let x = 1;~%}~%")))
        (tuatara.syntax:*syntax-wait* 10))
    (unwind-protect
         (progn
           (tuatara.modes:set-major-mode buffer 'tuatara.defaults::rust-mode)
           (let ((syntax (tuatara.syntax:buffer-syntax buffer))
                 (second internal-time-units-per-second))
             (tuatara.syntax:syntax-ready-tree syntax)
             ;; quick: the edit's parse starts at once
             (setf (tuatara.syntax::syntax-last-update syntax) (floor second 100))
             (tuatara.buffer:buffer-insert buffer 0 "// a")
             (true (tuatara.syntax:syntax-job syntax) "a quick grammar parses at once")
             (tuatara.syntax:syntax-ready-tree syntax)
             (loop repeat 1000 while (tuatara.syntax:syntax-job syntax)
                   do (sleep 0.01) (tuatara.syntax:catch-up syntax))
             ;; slow, 0.4 s the last time: the edit waits, its tree told of it
             (setf (tuatara.syntax::syntax-last-update syntax) (floor (* 4 second) 10))
             (tuatara.buffer:buffer-insert buffer 0 "// b")
             (false (tuatara.syntax:syntax-job syntax) "a slow grammar waits for a pause")
             (false (tuatara.syntax:syntax-current syntax))
             (let ((wait (tuatara.syntax:refresh-syntax syntax)))
               (true (and wait (< 0.6 wait 0.81)) "about 0.8 s to wait, not ~a" wait))
             ;; typing paused long enough: parsed now
             (setf (tuatara.syntax::syntax-edited syntax) (- (get-internal-real-time) second))
             (false (tuatara.syntax:refresh-syntax syntax))
             (true (tuatara.syntax:syntax-job syntax) "parsed once typing paused")))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax parsed-afresh-when-left-alone)
  ;; a tree brought up to date by parses after edits is parsed afresh, in the
  ;; background, once the text is left alone; an edit stops that parse, and one
  ;; given up leaves the tree and its colours as they were
  (flet ((settle (syntax)
           ;; until the parse under way, a parse afresh too, is taken in
           (loop repeat 1000 while (tuatara.syntax:syntax-job syntax)
                 do (sleep 0.01) (tuatara.syntax:catch-up syntax))))
    (let ((buffer (tuatara.buffer:make-buffer "*afresh*" :text (format nil "fn main() {~%    let x = 1;~%}~%")))
          (tuatara.syntax:*syntax-wait* 10))
      (unwind-protect
           (progn
             (tuatara.modes:set-major-mode buffer 'tuatara.defaults::rust-mode)
             (let ((syntax (tuatara.syntax:buffer-syntax buffer)))
               (tuatara.syntax:syntax-ready-tree syntax)
               ;; parsed from nothing: nothing to put right
               (false (tuatara.syntax:syntax-patched syntax))
               (false (tuatara.syntax:refresh-syntax syntax))
               (tuatara.buffer:buffer-insert buffer 0 (format nil "// hi~%"))
               (tuatara.syntax:syntax-ready-tree syntax)
               (true (tuatara.syntax:syntax-patched syntax))
               ;; not left alone long enough yet: how long until it is
               (let ((tuatara.syntax:*syntax-refresh-delay* 60))
                 (let ((wait (tuatara.syntax:refresh-syntax syntax)))
                   (true (and wait (< 50 wait 60.01)))))
               (let ((tuatara.syntax:*syntax-refresh-delay* nil))
                 (false (tuatara.syntax:refresh-syntax syntax)))
               ;; left alone long enough: parsed afresh, redisplay not waiting for it
               ;; (the ten times its parse from nothing took left out, as the
               ;; machine may be slow)
               (let ((tuatara.syntax:*syntax-refresh-delay* 0))
                 (setf (tuatara.syntax::syntax-fresh-cost syntax) 0)
                 (false (tuatara.syntax:refresh-syntax syntax))
                 (true (tuatara.syntax::parse-job-refresh (tuatara.syntax:syntax-job syntax))))
               (settle syntax)
               (false (tuatara.syntax:syntax-patched syntax))
               (true (tuatara.syntax:syntax-current syntax))
               (true (tuatara.syntax:syntax-tree syntax))))
        (tuatara.buffer:kill-buffer buffer)))
    ;; an edit stops a parse afresh under way: the parse after the edit follows
    (let* ((text (with-output-to-string (o)
                   (dotimes (i 20000) (format o "fn f~d() {{ let x = ~d; }}~%" i i))))
           (buffer (tuatara.buffer:make-buffer "*afresh*" :text text))
           (tuatara.syntax:*syntax-wait* 60))
      (unwind-protect
           (progn
             (tuatara.modes:set-major-mode buffer 'tuatara.defaults::rust-mode)
             (let ((syntax (tuatara.syntax:buffer-syntax buffer)))
               (tuatara.syntax:syntax-ready-tree syntax)
               (tuatara.buffer:buffer-insert buffer 0 (format nil "// a~%"))
               (tuatara.syntax:syntax-ready-tree syntax)
               (let ((tuatara.syntax:*syntax-refresh-delay* 0))
                 ;; a text is left alone ten times as long as its parse from nothing took
                 (let ((wait (tuatara.syntax:refresh-syntax syntax)))
                   (true (and wait
                              (< (* 9 (/ (tuatara.syntax::syntax-fresh-cost syntax) internal-time-units-per-second))
                                 wait))))
                 (setf (tuatara.syntax::syntax-fresh-cost syntax) 0)
                 (tuatara.syntax:refresh-syntax syntax))
               (let ((job (tuatara.syntax:syntax-job syntax)))
                 (true (and job (tuatara.syntax::parse-job-refresh job)))
                 (tuatara.buffer:buffer-insert buffer 0 (format nil "// b~%"))
                 (true (tuatara.syntax::parse-job-obsolete job)))
               (settle syntax)
               (tuatara.syntax:syntax-ready-tree syntax)
               (true (tuatara.syntax:syntax-current syntax))
               (false (tuatara.syntax:syntax-no-refresh syntax))
               (false (tuatara.syntax:syntax-broken syntax))
               ;; a parse afresh given up: the tree kept, not tried again
               (let ((tuatara.syntax:*syntax-refresh-delay* 0)
                     (tuatara.syntax:*syntax-time-limit* 0.0001)
                     (tuatara.syntax:*syntax-time-per-megabyte* 0))
                 (setf (tuatara.syntax::syntax-fresh-cost syntax) 0)
                 (tuatara.syntax:refresh-syntax syntax))
               (settle syntax)
               (true (tuatara.syntax:syntax-no-refresh syntax))
               (false (tuatara.syntax:syntax-broken syntax))
               (true (tuatara.syntax:syntax-tree syntax))
               (let ((tuatara.syntax:*syntax-refresh-delay* 0))
                 (false (tuatara.syntax:refresh-syntax syntax))
                 (false (tuatara.syntax:syntax-job syntax)))))
        (tuatara.buffer:kill-buffer buffer)))))

(define-test (syntax colours-before-the-whole-parse)
  ;; a text too large to parse whole: the lines shown are coloured from a parse
  ;; of them and the text near them, as a parse of the whole text colours them
  (let* ((text (with-output-to-string (s)
                 (dotimes (i 400) (format s "fn f~d(p: u8) -> u8 {~%    g(p, \"line ~d\") // note~%}~%" i i))))
         (buffer (tuatara.buffer:make-buffer "*syntax-slice*" :text text))
         (grammar (tuatara.syntax:find-grammar "rust"))
         (query (tuatara.syntax:compile-query grammar (tuatara.defaults::language-rules (language-named 'rust))))
         (faces (tuatara.defaults::query-faces query :rich 0)))
    (unwind-protect
         (let* ((tuatara.syntax:*syntax-size-limit* 1000)
                (tuatara.syntax::*syntax-slice-reach* 200)
                (syntax (tuatara.syntax:attach-syntax buffer grammar))
                (start (search "fn f200(" text))
                (end (search "fn f203(" text)))
           (true (tuatara.syntax:syntax-too-big syntax))
           (false (tuatara.syntax:syntax-ready-tree syntax))
           (let ((shown (first (tuatara.syntax:syntax-highlights syntax query faces (list (cons start end))))))
             (true shown)
             (is equal (multiple-value-bind (tree rope parser) (parse-string "rust" text)
                         (unwind-protect (tuatara.syntax:rich-highlights query tree rope start end faces)
                           (release-parse tree parser)))
                 shown)))
      (tuatara.syntax:detach-syntax buffer)
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax limits)
  ;; a text over the size limit is not parsed until it shrinks
  (let ((buffer (tuatara.buffer:make-buffer "*syntax-limit*" :text (make-string 300 :initial-element #\x))))
    (unwind-protect
         (let* ((tuatara.syntax:*syntax-size-limit* 200)
                (syntax (tuatara.syntax:attach-syntax buffer (tuatara.syntax:find-grammar "rust"))))
           (true (tuatara.syntax:syntax-too-big syntax))
           (false (tuatara.syntax:syntax-ready-tree syntax))
           (tuatara.buffer:buffer-delete buffer 0 250)
           (let ((tuatara.syntax:*syntax-wait* 30))
             (true (tuatara.syntax:syntax-ready-tree syntax)))
           (false (tuatara.syntax:syntax-too-big syntax)))
      (tuatara.syntax:detach-syntax buffer)
      (tuatara.buffer:kill-buffer buffer)))
  ;; a file visited safely gets no mode by itself, and is never given to a
  ;; parser, even in the mode of its language
  (let* ((directory (uiop:ensure-directory-pathname (merge-pathnames (format nil "tuatara-syntax-~a/" (random 1000000))
                                                                      (uiop:temporary-directory))))
         (path (merge-pathnames "evidence.rs" directory)))
    (ensure-directories-exist path)
    (unwind-protect
         (progn
           (with-open-file (o path :direction :output) (write-string "fn main() {}" o))
           (let ((buffer (tuatara.io:visit-file path :safe t)))
             (unwind-protect
                  (progn
                    (true (tuatara.io:safe-visit-p buffer))
                    (is eq 'tuatara.modes:fundamental-mode (type-of (tuatara.modes:buffer-major-mode buffer)))
                    (tuatara.modes:set-major-mode buffer 'tuatara.defaults::rust-mode)
                    (false (tuatara.syntax:buffer-syntax buffer)))
               (tuatara.buffer:kill-buffer buffer))))
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))))

(define-test (syntax language-modes)
  (is eq 'tuatara.defaults::rust-mode (tuatara.modes:mode-for-file #p"/src/main.rs"))
  (is eq 'tuatara.defaults::bash-mode (tuatara.modes:mode-for-file #p"/bin/run.sh"))
  (is eq 'tuatara.defaults::bash-mode (tuatara.modes:mode-for-file #p"/home/me/.bashrc"))
  (is eq 'tuatara.defaults::bash-mode (tuatara.modes:mode-for-file #p"/pkg/PKGBUILD"))
  (is eq 'tuatara.defaults::bash-mode (tuatara.modes:mode-for-file #p"/usr/share/bash-completion/completions/git"))
  (is eq 'tuatara.defaults::bash-mode (tuatara.modes:mode-for-file #p"/bin/script" "#!/usr/bin/env bash"))
  (is eq 'tuatara.defaults::bash-mode (tuatara.modes:mode-for-file #p"/bin/script" "#!/bin/sh -e"))
  (is eq 'tuatara.defaults::lisp-mode (tuatara.modes:mode-for-file #p"/src/x.lisp"))
  (dolist (name '("/src/Containerfile" "/src/Dockerfile" "/src/containerfile" "/src/Containerfile.dev"
                  "/src/Dockerfile.rust" "/src/app.dockerfile" "/src/app.containerfile"))
    (is eq 'tuatara.defaults::containerfile-mode (tuatara.modes:mode-for-file (pathname name)) name))
  ;; entering a language's mode parses the buffer; leaving it stops
  (let ((buffer (tuatara.buffer:make-buffer "*syntax-mode*" :text "fn main() {}")))
    (unwind-protect
         (progn
           (tuatara.modes:set-major-mode buffer 'tuatara.defaults::rust-mode)
           (true (tuatara.syntax:buffer-syntax buffer))
           (is string= "rust"
               (tuatara.syntax:grammar-name (tuatara.syntax:syntax-grammar (tuatara.syntax:buffer-syntax buffer))))
           (tuatara.modes:set-major-mode buffer 'tuatara.defaults::text-mode)
           (false (tuatara.syntax:buffer-syntax buffer)))
      (tuatara.buffer:kill-buffer buffer))))

(defun colour-session (keys text extension &key (width 60))
  "Run KEYS on a file with EXTENSION holding TEXT; return the fake frontend."
  (let* ((directory (uiop:ensure-directory-pathname (merge-pathnames (format nil "tuatara-syntax-~a/" (random 1000000))
                                                                      (uiop:temporary-directory))))
         (path (merge-pathnames (format nil "sample.~a" extension) directory)))
    (ensure-directories-exist path)
    (with-open-file (o path :direction :output) (write-string text o))
    (unwind-protect
         (multiple-value-bind (f editor) (run-keys keys :file path :width width)
           (tuatara.buffer:kill-buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor)))
           f)
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))))

(defmacro with-file-session (((f editor) keys text extension) &body body)
  "Run KEYS on a file with EXTENSION holding TEXT, and BODY with F and EDITOR
bound to the fake frontend and the editor; the file and its buffer go afterwards."
  (let ((directory (gensym "DIRECTORY")) (path (gensym "PATH")))
    `(let* ((,directory (uiop:ensure-directory-pathname
                         (merge-pathnames (format nil "tuatara-syntax-~a/" (random 1000000)) (uiop:temporary-directory))))
            (,path (merge-pathnames (format nil "sample.~a" ,extension) ,directory)))
       (ensure-directories-exist ,path)
       (with-open-file (o ,path :direction :output) (write-string ,text o))
       (unwind-protect
            (multiple-value-bind (,f ,editor) (run-keys ,keys :file ,path :width 60)
              (declare (ignorable ,f))
              (unwind-protect (progn ,@body)
                (tuatara.buffer:kill-buffer (tuatara.display:window-buffer (tuatara.command:editor-window ,editor)))))
         (uiop:delete-directory-tree ,directory :validate t :if-does-not-exist :ignore)))))

(defun session-region-text (editor)
  "The text of the active region of EDITOR's current buffer, or NIL when none is active."
  (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
    (when (tuatara.buffer:buffer-mark-active-p b)
      (multiple-value-bind (start end) (tuatara.buffer:buffer-region b)
        (tuatara.buffer:buffer-string b :start start :end end)))))

(define-test (syntax rules-compiled-in-the-background)
  ;; the rules of a language are compiled in the background the first time they
  ;; are needed: the text shows at once, its colours when they are compiled
  (let ((language (tuatara.defaults::find-language 'verilog)))
    (setf (tuatara.defaults::language-query language) nil
          (tuatara.defaults::language-injection-query language) nil
          (tuatara.defaults::language-locals-query language) nil)
    (let ((tuatara.defaults::*rules-wait* 0)
          (tuatara-user:*grammars-on* '(:common verilog)))
      (with-file-session ((f editor) "" (format nil "module m(input a, output b);~%  assign b = a;~%endmodule~%") "v")
        (let ((thread (gethash language tuatara.defaults::*warming*)))
          (when thread
            (is equal '() (aref (tuatara.frontend.fake:fake-spans f) 0))
            (sb-thread:join-thread thread :default nil)))
        (true (tuatara.defaults::rules-ready-p language))
        (let ((tuatara.command:*editor* editor)) (tuatara.command:redisplay editor))
        (true (plusp (length (aref (tuatara.frontend.fake:fake-spans f) 0))))))))

(define-test (syntax long-lines-coloured-where-shown)
  ;; a line scrolled sideways is coloured only where it shows: the rest of a
  ;; line of megabytes (a minified script) is not looked at
  (let* ((text (format nil "~{~a~}~%" (loop for i below 2000 collect (format nil "function f~d(a){return a+~d};" i i))))
         (rows (tuatara.display:layout (tuatara.text:string->rope text) 0 80 3 :hscroll 30000)))
    (destructuring-bind ((start . end)) (tuatara.display:shown-ranges rows)
      (true (<= 29000 start 30000))
      (true (<= (- end start) 200))))
  (let ((text (format nil "~{~a~}~%" (loop for i below 3000 collect (format nil "function f~d(a){return a+~d};" i i)))))
    (with-file-session ((f editor) "<end>" text "js")
      ;; the end of the line, coloured
      (true (find :keyword (aref (tuatara.frontend.fake:fake-spans f) 0)
                  :key (lambda (s) (let ((face (third s))) (if (listp face) (first face) face))))))))

(define-modal-test (syntax marking-expressions)
  ;; m I and m A select inside and around the brackets around point, the
  ;; grammar leaving out the bracket in the string; again at once, one pair out
  (let ((text "(a (b \"x ) y\" c) d)"))
    (with-file-session ((f editor) "C-u 1 4 l m I" text "lisp")
      (is string= "b \"x ) y\" c" (session-region-text editor))
      (is = 4 (session-point editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "Marked inner expression")))
    (with-file-session ((f editor) "C-u 1 4 l m I m I" text "lisp")
      (is string= "a (b \"x ) y\" c) d" (session-region-text editor)))
    (with-file-session ((f editor) "C-u 1 4 l m A" text "lisp")
      (is string= "(b \"x ) y\" c)" (session-region-text editor)))
    (with-file-session ((f editor) "C-u 1 4 l C-u 2 m A" text "lisp")
      (is string= text (session-region-text editor)))
    (with-file-session ((f editor) "C-u 2 m A" text "lisp")
      (false (session-region-text editor)))
    ;; on a bracket: that bracket's pair
    (with-file-session ((f editor) "C-u 3 l m A" text "lisp")
      (is string= "(b \"x ) y\" c)" (session-region-text editor)))
    (with-file-session ((f editor) "C-u 1 5 l m A" text "lisp")
      (is string= "(b \"x ) y\" c)" (session-region-text editor)))
    ;; no pair further out, and an empty pair inside
    (with-file-session ((f editor) "C-u 1 4 l m A m A m A" text "lisp")
      (true (tuatara.frontend.fake:fake-frame-showing f "Not inside a balanced expression")))
    (with-file-session ((f editor) "l m I" "()" "lisp")
      (true (tuatara.frontend.fake:fake-frame-showing f "Empty expression"))
      (false (session-region-text editor)))
    ;; with no grammar every bracket counts, the one in the string too
    (with-file-session ((f editor) "C-u 1 4 l m I" text "txt")
      (is string= "a (b \"x ) y\" c" (session-region-text editor)))))

(define-modal-test (syntax commenting-lines)
  ;; C comments or uncomments whole lines: the marker at the smallest
  ;; indentation, blank lines left alone, one undo
  (flet ((after (keys text extension)
           (let (result)
             (with-file-session ((f editor) keys text extension)
               (setf result (session-text editor)))
             result)))
    (is string= (format nil ";; (foo)~%;;   (bar)~%") (after "V j C" (format nil "(foo)~%  (bar)~%") "lisp"))
    (is string= (format nil "(foo)~%  (bar)~%") (after "V j C" (format nil ";; (foo)~%;;   (bar)~%") "lisp"))
    (is string=
        (format nil "  ;; (foo)~%~%  ;;   (bar)~%")
        (after "V j j C" (format nil "  (foo)~%~%    (bar)~%") "lisp"))
    ;; lines not all commented are commented once more
    (is string= (format nil ";; ;; a~%;; (b)~%") (after "V j C" (format nil ";; a~%(b)~%") "lisp"))
    (is string= (format nil "header~%") (after "C" (format nil ";;;; header~%") "lisp"))
    ;; the current line only, without a selection
    (is string= (format nil "  // let x = 1;~%  foo();~%") (after "C" (format nil "  let x = 1;~%  foo();~%") "rs"))
    (is string= (format nil "x~%") (after "C" (format nil "//x~%") "rs"))
    (is string= (format nil "# echo a~%#   echo b~%") (after "V j C" (format nil "echo a~%  echo b~%") "sh"))
    ;; undone in one step
    (is string= (format nil "(foo)~%  (bar)~%") (after "V j C u" (format nil "(foo)~%  (bar)~%") "lisp"))
    ;; a buffer with no comments says so and changes nothing
    (with-file-session ((f editor) "C" "plain" "txt")
      (is string= "plain" (session-text editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "No comment syntax is defined")))))

(define-modal-test (syntax indenting-leaves-text-alone)
  ;; indenting lines leaves the blanks of lines inside a comment or a string
  ;; begun on a line before, and of a line a string runs up to: they are text
  (flet ((indented (text mode)
           (let ((buffer (tuatara.buffer:make-buffer "*indent-text*" :text text)))
             (unwind-protect
                  (progn
                    (tuatara.modes:set-major-mode buffer mode)
                    (let ((tuatara.syntax:*syntax-wait* 10))
                      (tuatara.syntax:syntax-ready-tree (tuatara.syntax:buffer-syntax buffer))
                      (tuatara.defaults::indent-lines buffer 0 (tuatara.buffer:buffer-size buffer)))
                    (tuatara.buffer:buffer-string buffer))
               (tuatara.buffer:kill-buffer buffer)))))
    (let ((python (format nil "def f():~%    \"\"\"Doc.~%       kept as it is~%    \"\"\"~%    x = '''~%  raw~%'''~%    return x~%")))
      (is string= python (indented python 'tuatara.defaults::python-mode)))
    (let ((java (format nil "/**~% * Doc.~% *   kept~% */~%class A {~%  int x;~%}~%")))
      (is string= java (indented java 'tuatara.defaults::java-mode)))
    ;; C-M-\ (indent-region) does it to the selection, and without one says so
    (let (result)
      (with-file-session ((f editor) "V j C-M-\\" (format nil "class A {~%int x;~%}~%") "java")
        (setf result (session-text editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "Indented 1 line")))
      (is string= (format nil "class A {~%  int x;~%}~%") result))
    (with-file-session ((f editor) "C-M-\\" (format nil "class A {~%int x;~%}~%") "java")
      (true (tuatara.frontend.fake:fake-frame-showing f "No selection: indent-region was not run")))
    ;; the lines around them are indented still
    (is string= (format nil "class A {~%  /*~%   * x~%   */~%  int y;~%}~%")
        (indented (format nil "class A {~%/*~%   * x~%   */~%int y;~%}~%") 'tuatara.defaults::java-mode))))

(define-modal-test (syntax commenting-with-ends)
  ;; a language whose comments have an end (HTML, CSS) comments each line
  ;; between them, and uncomments it again
  (flet ((after (keys text extension)
           (let (result)
             (with-file-session ((f editor) keys text extension)
               (setf result (session-text editor)))
             result)))
    (is string= (format nil "<!-- <p>a</p> -->~%") (after "C" (format nil "<p>a</p>~%") "html"))
    (is string= (format nil "<p>a</p>~%") (after "C" (format nil "<!-- <p>a</p> -->~%") "html"))
    (is string= (format nil "  /* a { } */~%  /* b { } */~%") (after "V j C" (format nil "  a { }~%  b { }~%") "css"))
    ;; a blank line is left alone, the others line up
    (is string= (format nil "/* a { } */~%~%/*   b { } */~%") (after "V j j C" (format nil "a { }~%~%  b { }~%") "css"))))

(define-modal-test (syntax comment-syntax-in-definitions)
  ;; each language's comments as its mode's definition gives them, or the definition of the mode it is made from
  (loop for (mode line block)
          in '((tuatara.defaults::lisp-mode (";" ";;") (nil)) (tuatara.defaults::lisp-interaction-mode (";" ";;") (nil))
               (tuatara.defaults::ielm-mode (";" ";;") (nil)) (tuatara.defaults::emacs-lisp-mode (";" ";;") (nil))
               (tuatara.defaults::scheme-mode (";" ";;") (nil)) (tuatara.defaults::wat-mode (";;" ";;") (nil))
               (tuatara.defaults::rust-mode ("//" "//") (nil)) (tuatara.defaults::c-mode ("//" "//") (nil))
               (tuatara.defaults::js-mode ("//" "//") (nil)) (tuatara.defaults::bash-mode ("#" "#") (nil))
               (tuatara.defaults::containerfile-mode ("#" "#") (nil)) (tuatara.defaults::python-mode ("#" "#") (nil))
               (tuatara.defaults::org-mode ("#" "#") (nil)) (tuatara.defaults::git-commit-mode ("#" "#") (nil))
               (tuatara.defaults::yaml-mode ("#" "#") (nil)) (tuatara.defaults::docker-compose-mode ("#" "#") (nil))
               (tuatara.defaults::sql-mode ("--" "--") (nil)) (tuatara.defaults::haskell-mode ("--" "--") (nil))
               (tuatara.defaults::erlang-mode ("%%" "%%") (nil)) (tuatara.defaults::latex-mode ("%" "%") (nil))
               (tuatara.defaults::fortran-mode ("!" "!") (nil)) (tuatara.defaults::vimrc-mode ("\"" "\"") (nil))
               (tuatara.defaults::forth-mode ("\\" "\\") (nil)) (tuatara.defaults::tla-mode ("\\*" "\\*") (nil))
               (tuatara.defaults::basic-mode ("REM" "REM") (nil))
               (tuatara.defaults::jj-description-mode ("JJ:" "JJ:") (nil))
               (tuatara.defaults::html-mode (nil) ("<!--" "-->")) (tuatara.defaults::css-mode (nil) ("/*" "*/"))
               (tuatara.defaults::ocaml-mode (nil) ("(*" "*)")) (tuatara.defaults::jinja2-mode (nil) ("{#" "#}"))
               (tuatara.defaults::uxntal-mode (nil) ("(" ")")) (tuatara.defaults::vento-mode (nil) ("{{#" "#}}"))
               (tuatara.defaults::text-mode (nil) (nil)) (tuatara.defaults::json-mode (nil) (nil))
               (tuatara.modes:fundamental-mode (nil) (nil)))
        do (let ((instance (make-instance mode)))
             (is equal line (multiple-value-list (tuatara.modes:mode-comment-syntax instance)) "~(~a~)" mode)
             (is equal block (multiple-value-list (tuatara.modes:mode-block-comment-syntax instance)) "~(~a~)" mode)))
  ;; C comments a line of a language among those made from Helix's as its definition says
  (flet ((after (keys text extension)
           (let (result)
             (with-file-session ((f editor) keys text extension)
               (setf result (session-text editor)))
             result)))
    (is string= (format nil "-- select 1;~%") (after "C" (format nil "select 1;~%") "sql"))
    (is string= (format nil "%% f() -> ok.~%") (after "C" (format nil "f() -> ok.~%") "erl"))
    ;; a language whose grammar is off would ask first, taking C as the answer
    (let ((tuatara-user:*grammars-on* '(:common jinja)))
      (is string= (format nil "{# a #}~%") (after "C" (format nil "a~%") "j2")))))

(define-modal-test (syntax indenting-lisp)
  ;; > indents Lisp: indent-before.lisp, indented whole, gives indent-after.lisp
  (let ((tuatara.syntax:*syntax-wait* 10))
    (flet ((after (keys text)
             (let (result)
               (with-file-session ((f editor) keys text "lisp")
                 (setf result (session-text editor)))
               result))
           (body (text)
             ;; without the two lines of heading, which differ
             (subseq text (1+ (position #\Newline text :start (1+ (position #\Newline text)))))))
      (is string= (body (uiop:read-file-string (syntax-sample "indent-after.lisp")))
          (body (after "% >" (uiop:read-file-string (syntax-sample "indent-before.lisp")))))
      ;; one line is indented as TAB indents it; in a region a line lines up
      ;; with the one before it at its depth, as with indent-region
      (is string= (format nil "(~% foo bar~%     baz)~%") (after "j j >" (format nil "(~% foo bar~%baz)~%")))
      (is string= (format nil "(~% foo bar~% baz)~%") (after "% >" (format nil "(~% foo bar~%baz)~%")))
      ;; the lines a selection touches
      (is string= (format nil "(when x~%  (foo)~%  (bar))~%") (after "j V j >" (format nil "(when x~%(foo)~%(bar))~%")))
      ;; a bracket at the start of a line in a docstring starts no form
      (is string= (format nil "(defun f ()~%  \"A list:~%(1 2)~%\"~%  (list 1~%        2))~%")
          (after "j j j j j >" (format nil "(defun f ()~%  \"A list:~%(1 2)~%\"~%  (list 1~%2))~%")))
      ;; a line inside a string stays as it is
      (is string=
          (format nil "(defun f ()~%  \"A list:~%(1 2)\")~%")
          (after "j j >" (format nil "(defun f ()~%  \"A list:~%(1 2)\")~%")))
      ;; text the grammar cannot read is indented all the same
      (is string= (format nil "(defun f ()~%  (let ((2^n 4))~%    (print 2^n)))~%")
          (after "j j >" (format nil "(defun f ()~%  (let ((2^n 4))~%(print 2^n)))~%")))
      ;; undone in one step
      (is string= (format nil "(when x~%(foo)~%(bar))~%") (after "% > u" (format nil "(when x~%(foo)~%(bar))~%")))))
  ;; a buffer whose mode has no rules for indenting says so
  (with-file-session ((f editor) ">" "plain" "txt")
    (is string= "plain" (session-text editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "No indentation rules for"))))

(define-modal-test (syntax indenting-new-lines)
  ;; RET, o and O put the new line at its column as the language's mode
  ;; indents it, point where its text begins
  (let ((tuatara.syntax:*syntax-wait* 30) (tuatara.defaults::*rules-wait* 30))
    (flet ((after (keys text extension)
             (let (result)
               (with-file-session ((f editor) keys text extension)
                 (setf result (session-text editor)))
               result)))
      (is string= (format nil "(defun f ()~%  (g)~%") (after "A RET ( g ) C-g" (format nil "(defun f ()~%") "lisp"))
      ;; a line begun and left blank loses its blanks
      (is string= (format nil "(defun f ()~%~%") (after "A RET C-g" (format nil "(defun f ()~%") "lisp"))
      (is string= (format nil "(defun f ()~%~%  x~%") (after "A RET RET x C-g" (format nil "(defun f ()~%") "lisp"))
      ;; one u undoes RET and the indenting with it
      (is string= (format nil "(defun f ()~%") (after "A RET C-g u" (format nil "(defun f ()~%") "lisp"))
      (is string= (format nil "(let ((a 1))~%  a)~%") (after "A RET a ) C-g" (format nil "(let ((a 1))~%") "lisp"))
      ;; a package's settings after :config two columns in, as Emacs's use-package
      (is string=
          (format nil "(load-package \"showing\"~%  :config)~%")
          (after "A RET : c o n f i g ) C-g" (format nil "(load-package \"showing\"~%") "lisp"))
      (is string= (format nil "fn main() {~%    x~%}~%") (after "o x C-g" (format nil "fn main() {~%}~%") "rs"))
      (is string= (format nil "fn main() {~%    x~%}~%") (after "j O x C-g" (format nil "fn main() {~%}~%") "rs"))
      (is string=
          (format nil "if true; then~%  echo a~%fi~%")
          (after "o e c h o SPC a C-g" (format nil "if true; then~%fi~%") "sh"))
      ;; O leaves the line it pushes down as it was
      (is string=
          (format nil "fn main() {~%    x~%        }~%")
          (after "j O x C-g" (format nil "fn main() {~%        }~%") "rs"))
      ;; a buffer with no rules for indenting: column 0, as before
      (is string= (format nil "  words~%more~%") (after "A RET m o r e C-g" (format nil "  words~%") "txt"))
      ;; a Python block goes on after its last line (@extend), but not after its return
      (is string=
          (format nil "def f(x):~%    y = x~%    z~%")
          (after "j A RET z C-g" (format nil "def f(x):~%    y = x~%") "py"))
      (is string=
          (format nil "def f(x):~%    return x~%z~%")
          (after "j A RET z C-g" (format nil "def f(x):~%    return x~%") "py"))
      (is string=
          (format nil "def f(x):~%    y = x~%    z~%")
          (after "j o z C-g" (format nil "def f(x):~%    y = x~%") "py"))
      ;; nested: the innermost block reaches on
      (is string= (format nil "def f(x):~%    if x:~%        y = 1~%        z~%")
          (after "j j A RET z C-g" (format nil "def f(x):~%    if x:~%        y = 1~%") "py"))
      ;; a Go block's closing brace is not passed by it: spaces, or a tab where Go is set to have tabs
      (is string= (format nil "func f() {~%    x~%}~%") (after "o x C-g" (format nil "func f() {~%}~%") "go"))
      (with-mode-tabs ((go :tabs t))
        (is string= (format nil "func f() {~%~cx~%}~%" #\Tab) (after "o x C-g" (format nil "func f() {~%}~%") "go"))))
    ;; turned off in the init file: column 0
    (let ((tuatara.defaults::*indent-new-lines* nil))
      (let (result)
        (with-file-session ((f editor) "o x C-g" (format nil "fn main() {~%}~%") "rs")
          (setf result (session-text editor)))
        (is string= (format nil "fn main() {~%x~%}~%") result)))))

(define-modal-test (syntax new-line-in-a-block-not-closed-yet)
  ;; RET after a shell block's opening line puts the new line inside the block
  ;; before its closing word is typed, as it does once the block is closed
  (let ((tuatara.syntax:*syntax-wait* 30) (tuatara.defaults::*rules-wait* 30))
    (flet ((column (text)
             (let (result)
               (with-file-session ((f editor) "M-> A RET x C-g" text "sh")
                 (setf result (session-text editor)))
               (let ((line (find "x" (uiop:split-string result :separator '(#\Newline))
                                 :test #'string= :key (lambda (line) (string-left-trim " " line)) :from-end t)))
                 (and line (position #\x line))))))
      (loop for (text expected)
              in '(("if [ -n \"$1\" ]; then" 2) ("if true; then~%  echo a" 2) ("if a; then~%  x~%else" 2)
                   ("if a; then~%  x~%elif b; then" 2) ("if a; then~%  if b; then" 4)
                   ("for a in b; do" 2) ("while true; do" 2) ("until a; do" 2) ("select a in b c; do" 2)
                   ("f() {" 2) ("function f {" 2) ("f() {~%  echo a" 2) ("{" 2)
                   ("case $1 in" 2) ("case $1 in~%  a)" 4) ("case $1 in~%  a) echo;;" 2)
                   ("case $1 in~%  a)~%    echo" 4) ("case $1 in~%  a)~%    echo~%    ;;" 2)
                   ;; as yet a blank line ends a block not closed, and an unclosed function
                   ;; reads as one piece of text the grammar cannot place: Emacs gives 2 and 6
                   ("if a; then~%" 0) ("f() {~%  case $1 in~%    a)" 4)
                   ;; closed, the line after is outside
                   ("if true; then~%  echo~%fi" 0) ("for a in b; do~%  x~%done" 0)
                   ("case x in~%  a) echo;;~%esac" 0) ("case x in~%  a)~%esac" 0))
            do (is = expected (column (format nil text)) "after ~s" text)))))

(define-test (syntax indenting-lisp-quickly)
  ;; a form of 100,000 lines is indented in one pass, and one line at its end
  ;; without reading more than that form
  (let* ((text (with-output-to-string (out)
                 (format out "(defparameter *table*~%'(")
                 (dotimes (i 100000) (format out "(~d \"row ~d\" :key ~d)~%" i i (* 7 i)))
                 (format out "))~%")))
         (buffer (tuatara.buffer:make-buffer "*indent*" :text text)))
    (unwind-protect
         (progn
           (tuatara.modes:set-major-mode buffer 'tuatara.defaults::lisp-mode)
           (let ((tuatara.syntax:*syntax-wait* 60))
             (tuatara.syntax:syntax-ready-tree (tuatara.syntax:buffer-syntax buffer)))
           (let ((start (thread-cpu-time)))
             (is = 100001 (tuatara.defaults::indent-lines buffer 0 (tuatara.buffer:buffer-size buffer)))
             (true (< (- (thread-cpu-time) start) (* 2 internal-time-units-per-second))))
           (is string= "    (99999 \"row 99999\" :key 699993)"
               (let ((line (tuatara.buffer:buffer-goto-line buffer 100000)))
                 (tuatara.buffer:buffer-string buffer :start line :end (tuatara.buffer:buffer-line-end buffer line))))
           (let ((line (tuatara.buffer:buffer-goto-line buffer 100000))
                 (start (thread-cpu-time)))
             (tuatara.buffer:buffer-edit buffer (vector (tuatara.text:make-edit line (+ line 3))))
             (is = 1 (tuatara.defaults::indent-lines buffer line line))
             (true (< (- (thread-cpu-time) start) (* 1/2 internal-time-units-per-second)))))
      (tuatara.buffer:kill-buffer buffer))))

(define-modal-test (syntax indenting-by-the-tree)
  ;; Rust and shell scripts are indented by their languages' rules: the samples,
  ;; every line indented as expected, stay as they are, and come back from the
  ;; left margin, but for lines inside strings, which keep their blanks
  (flet ((indented (text mode)
           (let ((buffer (tuatara.buffer:make-buffer "*indent*" :text text)))
             (unwind-protect
                  (progn
                    (tuatara.modes:set-major-mode buffer mode)
                    (let ((tuatara.syntax:*syntax-wait* 10))
                      (tuatara.syntax:syntax-ready-tree (tuatara.syntax:buffer-syntax buffer))
                      (tuatara.defaults::indent-lines buffer 0 (tuatara.buffer:buffer-size buffer)))
                    (tuatara.buffer:buffer-string buffer))
               (tuatara.buffer:kill-buffer buffer))))
         (flat (text)
           (format nil "~{~a~^~%~}" (mapcar (lambda (line) (string-left-trim " " line))
                                            (uiop:split-string text :separator '(#\Newline))))))
    (let ((rust (uiop:read-file-string (syntax-sample "indentation/rust.rs")))
          (shell (uiop:read-file-string (syntax-sample "indentation/bash.sh"))))
      (is string= rust (indented rust 'tuatara.defaults::rust-mode))
      (is string= shell (indented shell 'tuatara.defaults::bash-mode))
      (is string= (cl-ppcre:regex-replace-all "(?m)^ +(indented (in string\";|raw\"#;))$" rust "\\1")
          (indented (flat rust) 'tuatara.defaults::rust-mode))
      (is string= shell (indented (flat shell) 'tuatara.defaults::bash-mode)))
    ;; C, whose sample is indented two columns a level; four is the level here
    (let ((c (uiop:read-file-string (syntax-sample "indentation/c.c")))
          (language (language-named 'c)))
      (let ((level (tuatara.defaults::language-indent language)))
        (unwind-protect
             (progn
               (tuatara-user:set-indentation 'c 2)
               (is string= c (indented c 'tuatara.defaults::c-mode))
               (is string= c (indented (flat c) 'tuatara.defaults::c-mode)))
          (setf (tuatara.defaults::language-indent language) level)))
      (is = 4 (tuatara.defaults::language-indent language))))
  ;; > in a Rust or shell buffer, on one line or the lines a selection touches
  (flet ((after (keys text extension)
           (let (result)
             (with-file-session ((f editor) keys text extension)
               (setf result (session-text editor)))
             result)))
    (is string=
        (format nil "fn f() {~%    let x = 1;~%}~%")
        (after "j >" (format nil "fn f() {~%let x = 1;~%}~%") "rs"))
    (is string=
        (format nil "if true; then~%  echo a~%fi~%")
        (after "j V j >" (format nil "if true; then~%echo a~%  fi~%") "sh"))
    (is string=
        (format nil "if true; then~%echo a~%  fi~%")
        (after "j V j > u" (format nil "if true; then~%echo a~%  fi~%") "sh"))))

(define-modal-test (syntax selecting-by-the-tree)
  ;; m f / m F, m t / m T, m r / m R and m c / m C select the function, type,
  ;; parameter or comment around point, inside or all of it; M-o grows the
  ;; selection to the syntax around it, M-i goes back, M-n and M-p step to the
  ;; piece beside it
  (flet ((region (keys text extension)
           (let (result)
             (with-file-session ((f editor) keys text extension)
               (setf result (session-region-text editor)))
             result))
         (to (line column)
           ;; keys that put point on LINE (from 0) at COLUMN
           (format nil "~{~a~^ ~}" (append (make-list line :initial-element "j")
                                           (make-list column :initial-element "l")))))
    (let ((rust (format nil "fn main() {~%    let x = foo(a, (b, c), d);~%    // one~%    // two~%}~%struct P { x: i32 }~%")))
      (is string= (format nil "{~%    let x = foo(a, (b, c), d);~%    // one~%    // two~%}")
          (region (format nil "~a m f" (to 1 20)) rust "rs"))
      (is string=
          (subseq rust 0 (search "struct" rust))
          (format nil "~a~%" (region (format nil "~a m F" (to 1 20)) rust "rs")))
      (is string= "(b, c)" (region (format nil "~a m r" (to 1 20)) rust "rs"))
      (is string= "(b, c)," (region (format nil "~a m R" (to 1 20)) rust "rs"))
      (is string= "a," (region (format nil "~a m R" (to 1 16)) rust "rs"))
      (is string= "{ x: i32 }" (region (format nil "~a m t" (to 5 12)) rust "rs"))
      (is string= "struct P { x: i32 }" (region (format nil "~a m T" (to 5 12)) rust "rs"))
      (is string= "// two" (region (format nil "~a m c" (to 3 6)) rust "rs"))
      (is string= (format nil "// one~%    // two") (region (format nil "~a m C" (to 3 6)) rust "rs"))
      ;; pressed again at once, the next function out: from a closure's body to the fn's
      (let ((nested (format nil "fn f() {~%    let g = |x| {~%        x + 1~%    };~%}~%")))
        (is string= (format nil "{~%        x + 1~%    }") (region (format nil "~a m f" (to 2 8)) nested "rs"))
        (is string= (subseq nested (search "{" nested) (1+ (search "}" nested :from-end t)))
            (region (format nil "~a m f m f" (to 2 8)) nested "rs")))
      ;; growing, shrinking and stepping
      (is string= "(b, c)" (region (format nil "~a M-o" (to 1 20)) rust "rs"))
      (is string= "(a, (b, c), d)" (region (format nil "~a M-o M-o" (to 1 20)) rust "rs"))
      (is string= "foo(a, (b, c), d)" (region (format nil "~a M-o M-o M-o" (to 1 20)) rust "rs"))
      (is string= "foo" (region (format nil "~a M-o" (to 1 12)) rust "rs"))
      (is string= "(b, c)" (region (format nil "~a M-o M-o M-o M-i M-i" (to 1 20)) rust "rs"))
      (false (region (format nil "~a M-o M-i" (to 1 20)) rust "rs"))
      (is string= "d" (region (format nil "~a M-o M-n" (to 1 20)) rust "rs"))
      (is string= "a" (region (format nil "~a M-o M-p" (to 1 20)) rust "rs"))
      (is string= "(a, (b, c), d)" (region (format nil "~a M-o M-o M-o M-i" (to 1 20)) rust "rs"))
      ;; a selection not grown by M-o shrinks to the first piece inside it
      (is string= "x: i32" (region (format nil "~a m t M-i" (to 5 12)) rust "rs")))
    ;; shell scripts and Lisp
    (is string=
        (format nil "{~%  echo \"hi\" there~%}")
        (region "j l l m f" (format nil "greet() {~%  echo \"hi\" there~%}~%") "sh"))
    (is string= "there"
        (region "j l l l l l l l l l l l l m r" (format nil "greet() {~%  echo \"hi\" there~%}~%") "sh"))
    (let ((lisp (format nil "(defun area (shape)~%  \"The area.\"~%  (* (width shape) 2))~%(defstruct point x y)~%")))
      (is string= (format nil "\"The area.\"~%  (* (width shape) 2)") (region "j j l l l l l l m f" lisp "lisp"))
      (is string= (subseq lisp 0 (search (format nil "~%(defstruct") lisp)) (region "j j l l l l l l m F" lisp "lisp"))
      (is string= "shape" (region "l l l l l l l l l l l l l m r" lisp "lisp"))
      (is string= "x y" (region "j j j l l l l l l l l l l l l l l l l l m t" lisp "lisp"))
      (is string= "width" (region "j j l l l l l l l M-o" lisp "lisp"))
      (is string= "(width shape)" (region "j j l l l l l l l M-o M-o" lisp "lisp"))))
  ;; with nothing around point, or no tree, it says so
  (with-file-session ((f editor) "m f" (format nil "struct P { x: i32 }~%") "rs")
    (false (session-region-text editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "No function around point")))
  (with-file-session ((f editor) "M-o" "plain" "txt")
    (true (tuatara.frontend.fake:fake-frame-showing f "No syntax tree to select by"))))

(define-modal-test (syntax function-selected-from-its-first-line)
  ;; point on a function's first line, at its first column or on its name, is
  ;; in that function: m f selects its body, m F all of it
  (flet ((region (keys text extension)
           (let (result)
             (with-file-session ((f editor) keys text extension)
               (setf result (session-region-text editor)))
             result)))
    (let ((shell (format nil "sum_up() {~%  echo $(( $1 + $2 ))~%}~%")))
      (is string= (format nil "{~%  echo $(( $1 + $2 ))~%}") (region "g g m f" shell "sh"))
      (is string= (format nil "{~%  echo $(( $1 + $2 ))~%}") (region "g g l l m f" shell "sh"))
      (is string= (format nil "sum_up() {~%  echo $(( $1 + $2 ))~%}") (region "g g m F" shell "sh")))
    (is string= (format nil "{~%    x();~%}") (region "g g m f" (format nil "fn main() {~%    x();~%}~%") "rs"))
    ;; a closure's first line inside a function: the closure's body, not the function's
    (is string= (format nil "{~%        x + 1~%    }")
        (region "j l l l l l l l l l l l l m f"
                (format nil "fn f() {~%    let g = |x| {~%        x + 1~%    };~%}~%") "rs"))
    (is string= "(* 2 x)" (region "g g m f" (format nil "(defun twice (x)~%  (* 2 x))~%") "lisp"))
    ;; an Ada procedure's first line is its heading, a whole function of its own with no body: the body is
    ;; that of the procedure the heading begins
    (let* ((tuatara-user:*grammars-on* '(:common ada))
           (parsed (lambda ()
                     (loop with buffer = (tuatara.command:current-buffer)
                           repeat 600
                           for language = (tuatara.buffer:buffer-local buffer :syntax-pending)
                           until (or (null language) (tuatara.syntax:buffer-syntax buffer))
                           do (tuatara.defaults::start-parsing buffer language) (sleep 0.05))))
           (selected (region (list parsed "g g m f") (format nil "procedure P is~%begin~%   null;~%end P;~%") "adb")))
      (is equal "null;" selected))))

(define-modal-test (syntax selecting-tests-entries-elements)
  ;; m y / m Y a test, m e / m E an entry, m x / m X an element, inside or all
  ;; of it: from the grammar and its rules alone, nothing else installed
  (let ((tuatara.syntax:*syntax-wait* 30) (tuatara.defaults::*rules-wait* 30))
    (flet ((region (keys text extension)
             (let (result)
               (with-file-session ((f editor) keys text extension)
                 (setf result (session-region-text editor)))
               result))
           (to (line column)
             (format nil "~{~a~^ ~}" (append (make-list line :initial-element "j")
                                             (make-list column :initial-element "l")))))
      (let ((rust (format nil "#[test]~%fn a() {~%    x();~%}~%~%fn b() {}~%~%#[test]~%#[should_panic]~%// why~%fn c() {}~%")))
        (is string= (format nil "{~%    x();~%}") (region (format nil "~a m y" (to 2 4)) rust "rs"))
        (is string= (format nil "fn a() {~%    x();~%}") (region (format nil "~a m Y" (to 2 4)) rust "rs"))
        ;; a function after a test is not one; one after more attributes and a comment is
        (false (region (format nil "~a m Y" (to 5 4)) rust "rs"))
        (is string= "fn c() {}" (region (format nil "~a m Y" (to 10 4)) rust "rs")))
      (is string= "assert f() == 2"
          (region (format nil "~a m y" (to 1 6)) (format nil "def test_f():~%    assert f() == 2~%") "py"))
      (let ((json (format nil "{\"a\": 1, \"b\": [2, 3]}~%")))
        (is string= "1" (region (format nil "~a m e" (to 0 6)) json "json"))
        (is string= "\"a\": 1" (region (format nil "~a m E" (to 0 6)) json "json"))
        (is string= "3" (region (format nil "~a m E" (to 0 18)) json "json"))
        (is string= "\"b\": [2, 3]" (region (format nil "~a m E m E" (to 0 18)) json "json")))
      (is string= "name: app" (region (format nil "~a m E" (to 0 7)) (format nil "name: app~%ports:~%  - 80~%") "yaml"))
      (let ((html (format nil "<ul><li>one</li><li>two</li></ul>~%")))
        (is string= "two" (region (format nil "~a m x" (to 0 20)) html "html"))
        (is string= "<li>two</li>" (region (format nil "~a m X" (to 0 20)) html "html"))
        (is string= "<ul><li>one</li><li>two</li></ul>" (region (format nil "~a m X m X" (to 0 20)) html "html"))))
    (with-file-session ((f editor) "j m y" (format nil "fn main() {~%    x();~%}~%") "rs")
      (false (session-region-text editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "No test around point")))))

(define-test (syntax selecting-quickly)
  ;; selecting a function or a test reads the text near point, not the
  ;; whole file
  (let* ((text (with-output-to-string (out)
                 (dotimes (i 20000)
                   (format out "#[test]~%fn f~d(a: i32, b: i32) -> i32 {~%    // sum~%    a + b~%}~%~%" i))))
         (buffer (tuatara.buffer:make-buffer "*select*" :text text)))
    (unwind-protect
         (progn
           (tuatara.modes:set-major-mode buffer 'tuatara.defaults::rust-mode)
           (let ((tuatara.syntax:*syntax-wait* 60))
             (tuatara.syntax:syntax-ready-tree (tuatara.syntax:buffer-syntax buffer)))
           (let* ((language (tuatara.defaults::find-language 'rust))
                  (query (tuatara.defaults::language-compiled-query language :text-objects))
                  (rope (tuatara.buffer:buffer-text buffer))
                  (point (tuatara.buffer:buffer-goto-line buffer 119995))
                  (start (thread-cpu-time))
                  (ranges (tuatara.defaults::text-object-ranges
                           query (tuatara.syntax:syntax-tree (tuatara.syntax:buffer-syntax buffer)) rope
                           "function.around"
                           (- point 65536)
                           (min (tuatara.text:rope-octet-length rope) (+ point 65536)))))
             (true (< (- (thread-cpu-time) start) (* 1/5 internal-time-units-per-second)))
             (is string= (format nil "fn f19999(a: i32, b: i32) -> i32 {~%    // sum~%    a + b~%}")
                 (let ((r (tuatara.defaults::smallest-around ranges point point nil)))
                   (and r (tuatara.buffer:buffer-string buffer :start (car r) :end (cdr r)))))
             ;; so does a test, its rule anchored to the attribute before it
             (let* ((start (thread-cpu-time))
                    (tests (tuatara.defaults::text-object-ranges
                            query (tuatara.syntax:syntax-tree (tuatara.syntax:buffer-syntax buffer)) rope
                            "test.around" (- point 65536) (min (tuatara.text:rope-octet-length rope) (+ point 65536)))))
               (true (< (- (thread-cpu-time) start) (* 1/5 internal-time-units-per-second)))
               (true (tuatara.defaults::smallest-around tests point point nil)))))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax kept-slice-asks-for-no-helper)
  ;; colours of lines kept from before are given again without looking for the helper
  (let* ((text (with-output-to-string (s) (dotimes (i 400) (format s "fn f~d() {}~%" i))))
         (buffer (tuatara.buffer:make-buffer "*syntax-kept-slice*" :text text))
         (grammar (tuatara.syntax:find-grammar "rust"))
         (query (tuatara.syntax:compile-query grammar (tuatara.defaults::language-rules (language-named 'rust))))
         (faces (tuatara.defaults::query-faces query :rich 0))
         (asked 0))
    (unwind-protect
         (let* ((tuatara.syntax:*syntax-size-limit* 1000)
                (syntax (tuatara.syntax:attach-syntax buffer grammar))
                (parts (list (cons (search "fn f200(" text) (search "fn f203(" text)))))
           (true (tuatara.syntax:syntax-highlights syntax query faces parts))
           (sb-int:encapsulate 'tuatara.syntax:buffer-helper-soon 'counted
                               (lambda (function &rest arguments)
                                 (incf asked)
                                 (apply function arguments)))
           (unwind-protect
                (dotimes (i 3) (true (tuatara.syntax:syntax-highlights syntax query faces parts)))
             (sb-int:unencapsulate 'tuatara.syntax:buffer-helper-soon 'counted))
           (is = 0 asked))
      (tuatara.syntax:detach-syntax buffer)
      (tuatara.buffer:kill-buffer buffer))))
