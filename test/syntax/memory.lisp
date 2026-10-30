;;;; test/syntax/memory.lisp --- Parses when memory runs short or a scanner writes past its own, in a process of its own.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Run by the tests of test/syntax.lisp with sbcl --script, in a process
;;;; held to little address space (ulimit -v): CASE OUT DIRECTORY, DIRECTORY
;;;; holding the grammar of test/grammars/misbehaving/ compiled. Fills the
;;;; address space, or has a scanner write past the memory given to it,
;;;; parses, and writes to OUT what happened, a property list (see the
;;;; tests). Nothing is written when the process ends before.

(load (merge-pathnames "../../tools/quicklisp.lisp" *load-truename*))
(push (merge-pathnames "../../" (make-pathname :name nil :type nil :defaults *load-truename*)) asdf:*central-registry*)
(ql:quickload :tuatara :silent t)

(defun c-malloc (octets)
  "OCTETS of memory from the C library, or a null pointer."
  (sb-alien:alien-funcall (sb-alien:extern-alien "malloc" (function sb-sys:system-area-pointer sb-alien:unsigned-long))
                          octets))

(defun c-free (pointer)
  "Give POINTER back to the C library."
  (sb-alien:alien-funcall (sb-alien:extern-alien "free" (function sb-alien:void sb-sys:system-area-pointer)) pointer))

(defun fill-memory (&optional (left 4))
  "Take the address space there is a megabyte at a time but LEFT megabytes;
the blocks taken, for LET-GO."
  (let ((blocks (loop for block = (c-malloc (* 1024 1024))
                      until (tuatara.io:null-pointer-p block)
                      collect block)))
    (loop repeat left while blocks do (c-free (pop blocks)))
    blocks))

(defun let-go (blocks)
  "Give back the BLOCKS FILL-MEMORY took."
  (mapc #'c-free blocks)
  nil)

(defun memory-back-p ()
  "True when the memory kept back for running short is kept again."
  (tuatara.syntax::ts-call "tuatara_memory_back" :bool))

(defun json-text (octets)
  "A JSON array of about OCTETS octets."
  (with-output-to-string (o)
    (write-string "[1" o)
    (loop repeat (floor octets 2) do (write-string ",1" o))
    (write-string "]" o)))

(defun parse-done (syntax)
  "Wait for SYNTAX's parses to finish, a minute at most, and take their result."
  (loop repeat 6000
        do (tuatara.syntax:catch-up syntax)
        while (tuatara.syntax:syntax-job syntax)
        do (sleep 0.01)))

(defun run-case (case directory)
  "Run CASE; what it found, as a property list."
  (let ((found '()))
    (flet ((note (key value) (setf (getf found key) value)))
      (cond
        ((string= case "reserve")
         (let* ((grammar (tuatara.syntax:find-grammar "json"))
                (parser (tuatara.syntax:make-parser grammar))
                (rope (tuatara.text:string->rope (json-text (* 4 1024 1024))))
                (tuatara.syntax::*parse-thread-idle* 600)
                (buffer (tuatara.buffer:make-buffer "*short*" :text "[1]"))
                ;; the buffer's parse thread is made while there is memory
                (syntax (tuatara.syntax:attach-syntax buffer grammar)))
           (parse-done syntax)
           (let ((blocks (fill-memory)))
             (multiple-value-bind (tree why unusable) (tuatara.syntax:parse-rope parser rope)
               (note :first (list (and tree t) why unusable)))
             ;; what the parse let go of taken too, so that the reserve cannot be had
             (setf blocks (append (fill-memory) blocks))
             (note :back-while-full (memory-back-p))
             (multiple-value-bind (tree why) (tuatara.syntax:parse-rope parser (tuatara.text:string->rope "[1]"))
               (note :while-short (list (and tree t) why)))
             (tuatara.buffer:buffer-insert buffer 1 (subseq (json-text (* 4 1024 1024)) 1))
             (parse-done syntax)
             (note :buffer-broken (tuatara.syntax:syntax-broken syntax))
             ;; the figure of the memory left, read while it is all but full
             ;; (the one kept is at most a second old)
             (sleep 1.1)
             (tuatara.syntax:parse-memory-limit (* 4 1024 1024))
             (let-go blocks))
           (note :back-after (memory-back-p))
           (note :limit-after (tuatara.syntax:parse-memory-limit (* 4 1024 1024)))
           (tuatara.buffer:buffer-insert buffer 1 "2,")
           (parse-done syntax)
           (note :buffer-broken-after (tuatara.syntax:syntax-broken syntax))
           (note :buffer-tree-after (and (tuatara.syntax:syntax-tree syntax) (tuatara.syntax:syntax-current syntax) t))
           (let ((tree (tuatara.syntax:parse-rope parser rope)))
             (note :parsed-after (and tree t)))))
        ((string= case "tight")
         ;; little memory left, but some: the budget held down by it is spent
         (let* ((grammar (tuatara.syntax:find-grammar "json"))
                (tuatara.syntax::*parse-thread-idle* 600)
                (buffer (tuatara.buffer:make-buffer "*tight*" :text "[1]"))
                (syntax (tuatara.syntax:attach-syntax buffer grammar)))
           (parse-done syntax)
           (let ((blocks (fill-memory 64)))
             (note :tight-limit (second (multiple-value-list (tuatara.syntax:parse-memory-limit (* 4 1024 1024)))))
             (tuatara.buffer:buffer-insert buffer 1 (subseq (json-text (* 4 1024 1024)) 1))
             (parse-done syntax)
             (note :buffer-broken (tuatara.syntax:syntax-broken syntax))
             (let-go blocks))
           (tuatara.buffer:buffer-insert buffer 1 "2,")
           (parse-done syntax)
           (note :buffer-broken-after (tuatara.syntax:syntax-broken syntax))
           (note :buffer-tree-after (and (tuatara.syntax:syntax-tree syntax) (tuatara.syntax:syntax-current syntax) t))))
        ((string= case "overflow")
         ;; a scanner writing past the buffer for its state, a little or much
         (let ((grammar (let ((tuatara.syntax:*grammar-directories*
                                (cons (pathname directory) (tuatara.syntax:grammar-directories))))
                          (tuatara.syntax:find-grammar "misbehaving"))))
           (note :faults (loop for text in '("abc !o def" "abc !O def")
                               collect (handler-case (progn (tuatara.syntax:parse-rope (tuatara.syntax:make-parser grammar)
                                                                                       (tuatara.text:string->rope text))
                                                            :parsed)
                                         (sb-sys:memory-fault-error () :fault))))
           (let* ((buffer (tuatara.buffer:make-buffer "*overflow*" :text "abc !O def"))
                  (syntax (tuatara.syntax:attach-syntax buffer grammar)))
             (parse-done syntax)
             (note :buffer-fault (and (typep (tuatara.syntax:grammar-faulted grammar) 'sb-sys:memory-fault-error) t))
             (note :buffer-broken (tuatara.syntax:syntax-broken syntax))
             ;; editing goes on
             (tuatara.buffer:buffer-insert buffer 0 "more "))
           (let ((tree (tuatara.syntax:parse-rope (tuatara.syntax:make-parser (tuatara.syntax:find-grammar "json"))
                                                  (tuatara.text:string->rope "[1, 2]"))))
             (note :other-parses (and tree t)))))
        ((string= case "huge")
         (let* ((grammar (let ((tuatara.syntax:*grammar-directories*
                                 (cons (pathname directory) (tuatara.syntax:grammar-directories))))
                           (tuatara.syntax:find-grammar "misbehaving")))
                (parser (tuatara.syntax:make-parser grammar)))
           (multiple-value-bind (tree why unusable)
               (tuatara.syntax:parse-rope parser (tuatara.text:string->rope "abc !h def"))
             (note :in-parse (list (and tree t) why unusable)))
           (note :back (memory-back-p))
           ;; in a buffer: the parser let go is counted
           (let* ((buffer (tuatara.buffer:make-buffer "*huge*" :text "abc !h def"))
                  (syntax (tuatara.syntax:attach-syntax buffer grammar)))
             (parse-done syntax)
             (note :buffer-broken (tuatara.syntax:syntax-broken syntax))
             (note :let-go (tuatara.syntax:parsers-let-go)))
           (let ((tree (tuatara.syntax:parse-rope (tuatara.syntax:make-parser grammar)
                                                  (tuatara.text:string->rope "abc begin def"))))
             (note :parsed-after (and tree t)))))))
    found))

(destructuring-bind (case out directory) (last sb-ext:*posix-argv* 3)
  (let ((found (run-case case directory)))
    (with-open-file (o out :direction :output :if-exists :supersede)
      (with-standard-io-syntax (prin1 found o)))))
