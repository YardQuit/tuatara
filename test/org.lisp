;;;; test/org.lisp --- Org's syntax: Tuatara's parse of a corpus against Emacs's own, and Org colouring.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test org
  :description "Org documents are parsed into the tree Emacs's org-element makes of them: for each
file of test/org/, the same nodes, positions and properties as tools/org-oracle.el wrote from Emacs.")

(defparameter *org-oracle-properties*
  '((headline :level :todo-keyword :todo-type :priority :commentedp :raw-value :tags :archivedp)
    (plain-list :type) (item :bullet :checkbox :counter) (src-block :language :switches :parameters :value)
    (example-block :switches :value) (export-block :type :value) (comment-block :value) (special-block :type)
    (drawer :drawer-name) (node-property :key :value) (keyword :key :value)
    (babel-call :call :inside-header :arguments :end-header) (dynamic-block :block-name :arguments)
    (latex-environment :value) (fixed-width :value) (comment :value) (footnote-definition :label)
    (table :type :tblfm) (table-row :type) (planning :closed :deadline :scheduled)
    (clock :status :duration :value) (diary-sexp :value) (verbatim :value) (code :value)
    (link :type :path :format :raw-link) (timestamp :type :raw-value) (entity :name :use-brackets-p)
    (latex-fragment :value) (footnote-reference :label :type) (inline-src-block :language :parameters :value)
    (inline-babel-call :call :inside-header :arguments :end-header) (macro :key :args) (target :value)
    (statistics-cookie :value) (subscript :use-brackets-p) (superscript :use-brackets-p)
    (export-snippet :back-end :value) (citation :style :prefix :suffix) (citation-reference :key))
  "The properties tools/org-oracle.el writes for each type of node.")

(defun org-oracle-form (node)
  "NODE, a parsed Org node, written as tools/org-oracle.el writes Emacs's: (TYPE PLIST CHILDREN...)."
  (let* ((type (tuatara.defaults::org-node-type node))
         (name (string-downcase (symbol-name type)))
         (plist (append
                 (loop for (key value) on (list :begin (tuatara.defaults::org-node-begin node)
                                                :end (tuatara.defaults::org-node-end node)
                                                :contents-begin (tuatara.defaults::org-node-contents-begin node)
                                                :contents-end (tuatara.defaults::org-node-contents-end node)
                                                :post-blank (tuatara.defaults::org-node-post-blank node)
                                                :post-affiliated (tuatara.defaults::org-node-post-affiliated node))
                             by #'cddr
                       when value append (list key value))
                 (loop for key in (cdr (assoc name *org-oracle-properties* :key #'string-downcase :test #'string=))
                       for value = (tuatara.defaults::org-property node key)
                       when value append (list key (if (and (symbolp value) (not (member value '(t nil))))
                                                       (string-downcase (symbol-name value))
                                                       value)))
                 (let ((value (tuatara.defaults::org-property node :name))) (and value (list :name value))))))
    (list* name plist (mapcar #'org-oracle-form (tuatara.defaults::org-node-children node)))))

(defun org-expected-form (form)
  "FORM, read from an .expected file, with its symbols as lower-case strings, to compare with ORG-ORACLE-FORM's."
  (cond ((and (consp form) (symbolp (first form)))
         (list* (string-downcase (symbol-name (first form)))
                (loop for (key value) on (second form) by #'cddr
                      append (list key (cond ((member value '(t nil)) value)
                                             ((symbolp value) (string-downcase (symbol-name value)))
                                             (t value))))
                (mapcar #'org-expected-form (cddr form))))
        (t form)))

(defun org-plist-difference (ours theirs)
  "The keys whose values differ between the plists OURS and THEIRS, with both values."
  (let ((keys (remove-duplicates (append (loop for k in ours by #'cddr collect k) (loop for k in theirs by #'cddr collect k)))))
    (loop for key in keys
          unless (equal (getf ours key) (getf theirs key))
            collect (list key :ours (getf ours key) :emacs (getf theirs key)))))

(defun org-tree-difference (ours theirs &optional (path '()))
  "The first place where the trees OURS and THEIRS differ, as a description, or NIL."
  (let ((path (append path (list (first theirs)))))
    (cond ((not (equal (first ours) (first theirs)))
           (list :at path :type :ours (first ours) :emacs (first theirs) :ours-plist (second ours) :emacs-plist (second theirs)))
          ((org-plist-difference (second ours) (second theirs))
           (list :at path :plist (org-plist-difference (second ours) (second theirs)) :emacs-plist (second theirs)))
          (t (loop for a in (cddr ours)
                   for b in (cddr theirs)
                   for difference = (org-tree-difference a b path)
                   when difference return difference
                   finally (return (unless (= (length (cddr ours)) (length (cddr theirs)))
                                     (list :at path :children :ours (mapcar #'first (cddr ours))
                                                               :emacs (mapcar #'first (cddr theirs))))))))))

(defun org-corpus-difference (name &optional (directory (asdf:system-relative-pathname :tuatara "test/org/")))
  "Where Tuatara's parse of NAME.org in DIRECTORY (test/org/) differs from Emacs's, or NIL."
  (let* (
         (text (alexandria:read-file-into-string (merge-pathnames (format nil "~a.org" name) directory) :external-format :utf-8))
         (expected (with-open-file (in (merge-pathnames (format nil "~a.expected" name) directory) :external-format :utf-8)
                     (let ((*package* (find-package :tuatara.tests)) (*read-default-float-format* 'double-float))
                       (read in)))))
    (org-tree-difference (org-oracle-form (tuatara.defaults::org-parse text)) (org-expected-form expected))))

(define-test (org corpus-as-emacs-parses-it)
  (dolist (name '("headlines" "blocks" "lists" "tables" "objects" "edge" "found"))
    (is equal nil (org-corpus-difference name) "~a.org" name)))

(define-test (org quick-on-large-documents)
  ;; the corpus made into a document of some 600 KB: parsed well within a second (about 0.1 s),
  ;; so that a parser looking through the rest of the text at each line, as one did, fails here
  (let* ((directory (asdf:system-relative-pathname :tuatara "test/org/"))
         (one (apply #'concatenate 'string
                     (mapcar (lambda (name) (alexandria:read-file-into-string (merge-pathnames (format nil "~a.org" name) directory)
                                                                              :external-format :utf-8))
                             '("headlines" "blocks" "lists" "tables" "objects" "edge" "found"))))
         (text (with-output-to-string (out) (loop repeat (ceiling 600000 (length one)) do (write-string one out)))))
    (tuatara.defaults::org-parse text)
    (let ((start (get-internal-real-time)))
      (tuatara.defaults::org-parse text)
      (true (< (- (get-internal-real-time) start) internal-time-units-per-second)
            "~:d characters took ~,2f s" (length text)
            (/ (- (get-internal-real-time) start) internal-time-units-per-second)))))

(define-test (org deep-nesting-read-as-text)
  ;; text made of nesting alone: parsed at once, what lies too deep read as plain text, the stack never exhausted
  (dolist (text (list (format nil "~{~a~}a~{~a~}" (make-list 5000 :initial-element "*/") (make-list 5000 :initial-element "/*"))
                      (format nil "~{~a~}x~{~a~}" (make-list 3000 :initial-element "[fn::") (make-list 3000 :initial-element "]"))
                      (format nil "~{#+begin_a~d~%~}x~%~{#+end_a~d~%~}" (loop for i below 3000 collect i) (loop for i from 2999 downto 0 collect i))))
    (true (tuatara.defaults::org-parse text)))
  ;; a long paragraph of opening marks that no mark closes: read in time growing with its length alone
  (let ((text (with-output-to-string (s) (loop repeat 40000 do (write-string "*a /b " s))))
        (start (get-internal-real-time)))
    (tuatara.defaults::org-parse text)
    (true (< (- (get-internal-real-time) start) internal-time-units-per-second))))

(defun org-row-faces (f row)
  "The faces of ROW of the frontend F's last frame, as (TEXT FACE) for each span."
  (let ((line (tuatara.frontend.fake:fake-screen-line f row)))
    (loop for (start end face) in (aref (tuatara.frontend.fake:fake-spans f) row)
          collect (list (subseq line start (min end (length line))) face))))

(defun org-faces-include (f row text face)
  "True when the frontend F's ROW shows TEXT in FACE (alone, or first among those combined)."
  (find-if (lambda (span) (and (string= (first span) text)
                               (let ((f (second span))) (if (listp f) (eq (first f) face) (eq f face)))))
           (org-row-faces f row)))

(defparameter *org-coloured*
  (format nil "#+title: Notes~%* TODO [#A] Task :work:~%  SCHEDULED: <2026-10-02 Fri>~%** DONE Done~%Some *bold* and =verbatim= and [[https://orgmode.org][Org]].~%#+begin_src lisp~%(+ 1 2)~%#+end_src~%- [X] checked~%| a | b |~%")
  "An Org text with something of each kind coloured.")

(define-test (org planning-keywords-in-agenda-colours)
  ;; DEADLINE: in the agenda's deadline colour, CLOSED: faint as Org's special keywords
  (let ((f (let ((tuatara-user:*org-bullets* nil) (tuatara-user:*org-startup-indented* nil))
             (run-keys "M-x o r g - m o d e RET"
                       :text (format nil "* TODO Task~%DEADLINE: <2026-10-30 Fri> SCHEDULED: <2026-10-27 Tue>~%* DONE Old~%CLOSED: [2026-10-26 Mon 09:12]~%Body~%")
                       :width 80 :height 8))))
    (true (org-faces-include f 1 "DEADLINE:" :org-deadline-keyword))
    (true (org-faces-include f 1 "SCHEDULED:" :org-scheduled-keyword))
    (true (org-faces-include f 3 "CLOSED:" :org-special-keyword))))

(define-test (org coloured-as-org-colours)
  (let ((f (let ((tuatara-user:*org-bullets* nil) (tuatara-user:*org-checkbox-signs* nil) (tuatara-user:*org-list-bullets* nil)
                 (tuatara-user:*org-startup-indented* nil))
             (run-keys "M-x o r g - m o d e RET" :text *org-coloured* :width 80 :height 16))))
    (true (org-faces-include f 0 "#+title:" :org-document-info-keyword))
    (true (org-faces-include f 0 " Notes" :org-document-title))
    (true (org-faces-include f 1 "TODO" :org-todo))
    (true (org-faces-include f 1 "[#A]" :org-priority))
    (true (org-faces-include f 1 ":work:" :org-tag))
    (true (org-faces-include f 1 "* " :org-level-1))
    ;; SCHEDULED: and DEADLINE: in the agenda's colours of them, apart from the text under them
    (true (org-faces-include f 2 "SCHEDULED:" :org-scheduled-keyword))
    (true (org-faces-include f 2 "<2026-10-02 Fri>" :org-date))
    (true (org-faces-include f 3 "DONE" :org-done))
    (true (org-faces-include f 4 "*bold*" :org-bold))
    (true (org-faces-include f 4 "=verbatim=" :org-verbatim))
    ;; a link shown as its label, as Org shows it
    (true (org-faces-include f 4 "Org" :org-link))
    (true (org-faces-include f 5 "#+begin_src lisp" :org-block-begin-line))
    ;; code in its language's colours, over the block's own
    (true (find-if (lambda (span) (and (string= (first span) "1") (listp (second span))
                                       (member :org-block (second span)) (not (eq (first (second span)) :org-block))))
                   (org-row-faces f 6)))
    (true (org-faces-include f 7 "#+end_src" :org-block-end-line))
    (true (org-faces-include f 8 "[X]" :org-checkbox))
    (true (org-faces-include f 9 "| a | b |" :org-table)))
  ;; a large text is parsed in the background: coloured once that is done, the editor not waiting
  (let ((tuatara.defaults::*org-parse-at-once* 10))
    (let* ((f (tuatara.frontend.fake:make-fake-frontend :width 80 :height 16))
           (start (get-internal-real-time)))
      (labels ((check ()
                 (if (and (not (org-faces-include f 1 "TODO" :org-todo))
                          (< (- (get-internal-real-time) start) (* 10 internal-time-units-per-second)))
                     (progn (sleep 0.02) (tuatara.frontend.fake:fake-queue-action f #'check))
                     (tuatara.frontend.fake:fake-queue-keys f "j"))))
        (tuatara.frontend.fake:fake-queue-action f #'check))
      (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                    :buffer (let ((b (tuatara.buffer:make-buffer "background.org" :text *org-coloured*)))
                                              (tuatara.defaults::set-major-mode b 'tuatara.defaults::org-mode)
                                              b))
      (true (org-faces-include f 1 "TODO" :org-todo)))))

(defun org-plain-session (text)
  "The frontend of an Org session on TEXT shown as written: no bullets or
signs, not indented, drawers open."
  (let ((tuatara-user:*org-bullets* nil) (tuatara-user:*org-checkbox-signs* nil) (tuatara-user:*org-list-bullets* nil)
        (tuatara-user:*org-startup-indented* nil) (tuatara-user:*org-cycle-hide-drawers* nil))
    (run-keys "M-x o r g - m o d e RET" :text text :width 100 :height 12)))

(define-test (org state-notes-in-log-colour)
  ;; a task's note of a change of state is drawn wholly in :org-log-note, its time and note too, in a :LOGBOOK: or not
  (let ((f (org-plain-session
            (format nil "* DONE Fix the gate~%- State \"DONE\"       from \"WAIT\"       [2026-10-01 Thu 18:00] \\\\~%  parts came~%:LOGBOOK:~%- State \"WAIT\"       from \"TODO\"       [2026-09-30 Wed 09:00]~%CLOCK: [2026-10-01 Thu 09:00]--[2026-10-01 Thu 10:00] =>  1:00~%:END:~%- Bought on [2026-10-01 Thu]~%"))))
    (flet ((log-note-row-p (row)
             (let ((spans (remove-if (lambda (span) (string= "" (string-trim " " (first span)))) (org-row-faces f row))))
               (and spans (every (lambda (span) (let ((face (second span))) (eq :org-log-note (if (listp face) (first face) face))))
                                 spans)))))
      (true (search "State \"DONE\"" (tuatara.frontend.fake:fake-screen-line f 1)))
      (true (search "State \"WAIT\"" (tuatara.frontend.fake:fake-screen-line f 4)))
      (true (log-note-row-p 1) "~s" (org-row-faces f 1))
      (true (log-note-row-p 2) "~s" (org-row-faces f 2))
      (true (log-note-row-p 4) "~s" (org-row-faces f 4))
      ;; the drawer and its clock line as before; another item's timestamp in its own colour
      (true (org-faces-include f 3 ":LOGBOOK:" :org-drawer))
      (true (org-faces-include f 5 "CLOCK:" :org-special-keyword))
      (true (org-faces-include f 7 "[2026-10-01 Thu]" :org-date)))))

(define-test (org todo-keywords-in-their-faces)
  ;; TODO and DONE in theirs, WAIT and CANCELLED in their own; a cancelled task's title as a done one's
  (let ((f (org-plain-session (format nil "* TODO A~%* WAIT B~%* DONE C~%* CANCELLED D~%"))))
    (true (org-faces-include f 0 "TODO" :org-todo))
    (true (org-faces-include f 1 "WAIT" :org-wait))
    (true (org-faces-include f 2 "DONE" :org-done))
    (true (org-faces-include f 3 "CANCELLED" :org-cancelled))
    (true (org-faces-include f 3 "D" :org-headline-done) "~s" (org-row-faces f 3))))

(defun org-row-coloured-p (f row text)
  "True when the frontend F's ROW shows TEXT in a language's colour over :org-block."
  (find-if (lambda (span) (and (string= (first span) text) (listp (second span))
                               (member :org-block (second span)) (not (eq (first (second span)) :org-block))))
           (org-row-faces f row)))

(defun org-row-org-faces (f row)
  "The faces of Org's own (and :comment) on ROW of the frontend F's last frame."
  (loop for (nil face) in (org-row-faces f row)
        append (loop for one in (if (listp face) face (list face))
                     when (or (eq one :comment) (uiop:string-prefix-p "ORG-" (symbol-name one)))
                       collect one)))

(define-test (org coloured-in-the-style-set)
  ;; set-colouring and rotate-colouring choose Org's style as they choose a language's: none, or the
  ;; sparse style's levels (1 headlines and comments, 2 keywords, tags and emphasis, 3 dates)
  (let ((rules tuatara.defaults::*colouring-rules*))
    (flet ((coloured (&optional (keys ""))
             (let ((tuatara-user:*org-bullets* nil) (tuatara-user:*org-checkbox-signs* nil) (tuatara-user:*org-list-bullets* nil)
                   (tuatara-user:*org-startup-indented* nil))
               (run-keys (format nil "M-x o r g - m o d e RET ~a" keys) :text *org-coloured* :width 80 :height 16))))
      (unwind-protect
           (progn
             (setf tuatara.defaults::*colouring-rules* '())
             (tuatara-user:set-colouring 'org-mode :none)
             (let ((f (coloured)))
               (loop for row from 0 below 10 do (is equal '() (org-row-org-faces f row) "row ~d plain" row))
               ;; the code of a source block too
               (false (org-row-coloured-p f 6 "1")))
             (tuatara-user:set-colouring 'org-mode :sparse :level 1)
             (let ((f (coloured)))
               (true (member :org-level-1 (org-row-org-faces f 1)))
               (false (member :org-todo (org-row-org-faces f 1)))
               (false (member :org-tag (org-row-org-faces f 1)))
               (false (member :org-date (org-row-org-faces f 2)))
               (false (member :org-bold (org-row-org-faces f 4)))
               (true (org-faces-include f 5 "#+begin_src lisp" :org-block-begin-line)))
             (tuatara-user:set-colouring 'org-mode :sparse :level 2)
             (let ((f (coloured)))
               (true (org-faces-include f 1 "TODO" :org-todo))
               (true (org-faces-include f 1 ":work:" :org-tag))
               (true (org-faces-include f 4 "*bold*" :org-bold))
               (false (member :org-date (org-row-org-faces f 2))))
             (tuatara-user:set-colouring 'org-mode :sparse)
             (true (org-faces-include (coloured) 2 "<2026-10-02 Fri>" :org-date))
             ;; what is set for text-mode or every buffer, Org's own first
             (setf tuatara.defaults::*colouring-rules* '())
             (tuatara-user:set-colouring 'text-mode :none)
             (is equal '() (org-row-org-faces (coloured) 1))
             (setf tuatara.defaults::*colouring-rules* '())
             (tuatara-user:set-colouring t :none)
             (is equal '() (org-row-org-faces (coloured) 1))
             (tuatara-user:set-colouring 'org-mode :rich)
             (true (org-faces-include (coloured) 1 "TODO" :org-todo))
             ;; M-x rotate-colouring in Org goes from rich to sparse to none and round again, every Org buffer with it
             (setf tuatara.defaults::*colouring-rules* '())
             (let ((f (coloured "M-x r o t a t e - c o l o u r i n g RET")))
               (true (tuatara.frontend.fake:fake-frame-showing f "Org colouring: sparse"))
               (true (org-faces-include f 1 "TODO" :org-todo)))
             (let ((f (coloured "M-x r o t a t e - c o l o u r i n g RET")))
               (true (tuatara.frontend.fake:fake-frame-showing f "Org colouring: none"))
               (is equal '() (org-row-org-faces f 1)))
             (let ((f (coloured "M-x r o t a t e - c o l o u r i n g RET")))
               (true (tuatara.frontend.fake:fake-frame-showing f "Org colouring: rich"))
               (true (org-faces-include f 1 "TODO" :org-todo))))
        (setf tuatara.defaults::*colouring-rules* rules)))))

(define-test (org source-blocks-coloured)
  ;; each block in its language, Org's names for them too (emacs-lisp, sh); text and unknown ones plain
  (let ((f (run-keys "M-x o r g - m o d e RET"
                     :text (format nil "#+begin_src emacs-lisp~%(defun x () 1)~%#+end_src~%#+begin_src sh~%echo $HOME~%#+end_src~%#+begin_src text~%(defun y () 1)~%#+end_src~%#+begin_src nosuchlanguage~%(defun z () 1)~%#+end_src~%#+begin_src python~%def f(): return 1~%#+end_src~%")
                     :width 80 :height 20)))
    (true (org-row-coloured-p f 1 "defun"))
    (true (or (org-row-coloured-p f 4 "echo") (org-row-coloured-p f 4 "$HOME")))
    (false (org-row-coloured-p f 7 "defun"))
    (true (org-faces-include f 7 "(defun y () 1)" :org-block))
    (true (org-faces-include f 10 "(defun z () 1)" :org-block))
    (true (org-row-coloured-p f 13 "def")))
  ;; the names Org gives languages, and the grammar's own language before another that borrows it
  (is eq 'tuatara.defaults::elisp (tuatara.defaults::language-name (tuatara.defaults::org-src-language "emacs-lisp")))
  (is string= "BASH" (symbol-name (tuatara.defaults::language-name (tuatara.defaults::org-src-language "bash"))))
  (is string= "PYTHON" (symbol-name (tuatara.defaults::language-name (tuatara.defaults::org-src-language "python"))))
  (false (tuatara.defaults::org-src-language "text")))

(define-test (org source-blocks-coloured-when-ready)
  ;; a language's rules not compiled yet when the note is first drawn: its block coloured once they
  ;; are, without moving into it; the rules compiled ahead for the note's languages, unasked
  (let* ((language (tuatara.defaults::org-src-language "ruby"))
         (f (tuatara.frontend.fake:make-fake-frontend :width 80 :height 12))
         (start (get-internal-real-time)))
    (setf (tuatara.defaults::language-query language) nil
          (tuatara.defaults::language-injection-query language) nil
          (tuatara.defaults::language-locals-query language) nil)
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
      (when scratch (tuatara.buffer:kill-buffer scratch)))
    (tuatara.buffer:make-buffer "*scratch*" :text (format nil "* Notes~%Text.~%#+begin_src ruby~%def greet(name) = puts(name)~%#+end_src~%"))
    (let ((tuatara.defaults::*rules-wait* 0))
      (tuatara.frontend.fake:fake-queue-keys f "M-x o r g - m o d e RET")
      (labels ((wait ()
                 (if (and (not (tuatara.defaults::rules-ready-p language))
                          (< (- (get-internal-real-time) start) (* 20 internal-time-units-per-second)))
                     (progn (sleep 0.02) (tuatara.frontend.fake:fake-queue-action f #'wait))
                     ;; the editor redrawn as it is when woken, nothing typed into the block
                     (tuatara.frontend.fake:fake-queue-keys f "C-l"))))
        (tuatara.frontend.fake:fake-queue-action f #'wait))
      (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults :buffer (tuatara.buffer:find-buffer "*scratch*")))
    (true (tuatara.defaults::rules-ready-p language))
    (true (org-row-coloured-p f 3 "def"))))

(defparameter *org-folding*
  (format nil "* One~%Text of one.~%** One A~%Text of one A.~%** One B~%Text of one B.~%* Two~%Text of two.~%")
  "An Org text of two headlines, the first with two children.")

(defun org-screen (keys &key (text *org-folding*) (height 12))
  "The lines an Org session typing KEYS on TEXT shows, trailing blanks off, and the editor."
  (multiple-value-bind (f editor) (run-keys (format nil "M-x o r g - m o d e RET ~a" keys) :text text :width 60 :height height)
    (values (loop for i below (- height 2) collect (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f i)))
            editor)))

(defun org-text-after (keys &optional (text *org-folding*))
  "The text an Org session typing KEYS on TEXT leaves."
  (session-text (nth-value 1 (run-keys (format nil "M-x o r g - m o d e RET ~a" keys) :text text :width 60 :height 12))))

(defun org-line-of-point (editor)
  "The line point is on in the editor's current buffer, and that buffer's name."
  (let* ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor)))
         (rope (tuatara.buffer:buffer-text buffer))
         (point (tuatara.buffer:buffer-point buffer)))
    (values (tuatara.text:octets-string (tuatara.text:rope-octets rope :start (tuatara.text:rope-line-start rope point)
                                                                        :end (tuatara.text:rope-line-end rope point)))
            (tuatara.buffer:buffer-name buffer))))

(defun org-table-after (keys text)
  "The text an Org session leaves after KEYS typed with point where TEXT has @
(taken out), with @ where point is after."
  (let* ((at (position #\@ text))
         (text (remove #\@ text))
         (line (count #\Newline text :end at))
         (column (- at (1+ (or (position #\Newline text :end at :from-end t) -1))))
         (digits (lambda (n) (format nil "~{~c~^ ~}" (coerce (princ-to-string n) 'list))))
         (moves (format nil "~@[C-u ~a j ~]~@[C-u ~a l ~]"
                        (and (plusp line) (funcall digits line)) (and (plusp column) (funcall digits column))))
         (editor (nth-value 1 (run-keys (format nil "M-x o r g - m o d e RET ~a~a" moves keys) :text text :width 60 :height 12)))
         (buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor)))
         (point (tuatara.text:byte->char (tuatara.buffer:buffer-text buffer) (tuatara.buffer:buffer-point buffer)))
         (after (session-text editor)))
    (concatenate 'string (subseq after 0 point) "@" (subseq after point))))

(defun forget-file-buffers ()
  "Kill the buffers visiting files, their changes thrown away."
  (dolist (b (copy-list tuatara.buffer:*buffers*))
    (when (tuatara.buffer:buffer-file b)
      (tuatara.buffer:buffer-mark-saved b)
      (tuatara.buffer:kill-buffer b))))

(define-test (org one-parse-at-a-time)
  ;; a large part of an Org buffer (a headline with a long text under it) is parsed in the background by
  ;; one thread at a time, whatever is typed, the latest text parsed last; the buffer's text is not
  ;; copied on every key
  (let ((tuatara.defaults::*org-parse-at-once* 1000)
        (f (tuatara.frontend.fake:make-fake-frontend :width 80 :height 12))
        (most 0)
        (text (with-output-to-string (out)
                (format out "* Heading~%")
                (dotimes (i 20000) (format out "Some body text ~d with [[https://x.org][a link]].~%" i)))))
    (flet ((parsers () (count "tuatara org parse" (sb-thread:list-all-threads) :key #'sb-thread:thread-name :test #'equal)))
      (dotimes (i 12)
        (tuatara.frontend.fake:fake-queue-keys f (if (zerop i) "i x" "x"))
        (tuatara.frontend.fake:fake-queue-action f (lambda () (setf most (max most (parsers))))))
      (tuatara.frontend.fake:fake-queue-keys f "C-g")
      (let ((buffer (tuatara.buffer:make-buffer "big.org" :text text)))
        (tuatara.defaults::set-major-mode buffer 'tuatara.defaults::org-mode)
        (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults :buffer buffer)
        (is >= 1 most)
        ;; the last text typed is parsed in the end
        (loop repeat 300 until (zerop (parsers)) do (sleep 0.05))
        (true (tuatara.defaults::org-view-current (tuatara.defaults::org-view-at buffer 0)))
        (tuatara.buffer:buffer-mark-saved buffer)
        (tuatara.buffer:kill-buffer buffer)))))

(define-test (org crlf-line-ends)
  ;; an Org file with CR LF line ends: Org's commands do what they do with LF alone, every line end staying CR LF
  (flet ((crlf (text) (cl-ppcre:regex-replace-all (string #\Newline) text (format nil "~c~%" #\Return))))
    (let ((tuatara.defaults::*org-now* (encode-universal-time 0 0 9 2 10 2026)))
      (dolist (case (list (list '("C-c C-c x") (format nil "|a|bb|~%|ccc|d|~%"))
                          (list '("C-c C-q x RET y") (format nil "* Head :old:~%"))
                          (list '("M-j") (format nil "* A~%a~%* B~%b~%"))
                          (list '("C-n M-j") (format nil "* A~%a~%** A1~%c~%* B~%b~%"))
                          (list '("C-c C-t x") (format nil "* Task~%"))
                          (list '("C-c C-x C-i" "C-c C-x C-o") (format nil "* Task~%body~%"))
                          (list '("C-c C-x C-i" "C-c C-x C-i") (format nil "* Task~%body~%* Other~%"))
                          ;; cancelled, the clock's line goes, and the drawer made for it
                          (list '("C-c C-x C-i" "C-c C-x C-q") (format nil "* Task~%:LOGBOOK:~%CLOCK: [2026-10-01 Thu 08:00]--[2026-10-01 Thu 08:30] =>  0:30~%:END:~%") t)
                          (list '("C-c C-x C-i" "C-c C-x C-q") (format nil "* Task~%body~%") t)
                          (list '("M-RET x") (format nil "* Task~%"))
                          (list '("C-c C-z n o t e RET") (format nil "* Task~%"))
                          (list '("C-e C-c C-l" "h t t p : / / e x RET RET") (format nil "* Link [[http://a][A]] z~%"))
                          (list '("TAB C-n d") (format nil "* A~%a~%a~%* B~%b~%"))))
        (destructuring-bind (keys text &optional same) case
          (let ((plain (session-text (nth-value 1 (run-keys-in-turns keys :text text :width 160 :height 12
                                                                          :mode 'tuatara.defaults::org-mode))))
                (crlf (session-text (nth-value 1 (run-keys-in-turns keys :text (crlf text) :width 160 :height 12
                                                                         :crlf t :mode 'tuatara.defaults::org-mode)))))
            (setf tuatara.defaults::*org-clock* nil)
            (is eq same (string= text plain) "~a changes the text" keys)
            (is string= (crlf plain) crlf "~a" keys)))))
    ;; coloured as with LF alone, links shown as their labels
    (let ((plain (run-keys-in-turns '("C-g") :text *org-coloured* :width 80 :height 16 :mode 'tuatara.defaults::org-mode))
          (crlf (run-keys-in-turns '("C-g") :text (crlf *org-coloured*) :width 80 :height 16 :crlf t
                                                  :mode 'tuatara.defaults::org-mode)))
      (dotimes (row 10)
        (is equal (org-row-faces plain row) (org-row-faces crlf row) "row ~d" row)))))

(define-test (org big-file-parse-awaited)
  ;; a structure command on a file past *org-parse-at-once* waits for the parse made in the
  ;; background, the editor reading keys meanwhile, rather than parsing at once in the command
  (let ((text (with-output-to-string (o) (dotimes (i 4000) (format o "* Headline ~d~%some text under it, line ~d~%" i i)))))
    (true (> (length text) tuatara.defaults::*org-parse-at-once*))
    (true (search (format nil "* TODO Headline 0~%") (org-text-after "C-c C-t" text)))
    ;; the headline index, made of the parse (awaited by C-c C-t): the indented view's prefix stands after an edit too
    (let ((tuatara-user:*org-startup-indented* t))
      (multiple-value-bind (f editor) (run-keys "M-x o r g - m o d e RET C-c C-t j i x C-g" :text text :width 60 :height 8)
        (declare (ignore editor))
        (true (search "  xsome text under it" (tuatara.frontend.fake:fake-screen-line f 1)))))))

(define-test (org file-names-make-no-keywords)
  ;; names a file makes up (#+ATTR_..., a block's :parameters, #+ATTR_LATEX's settings)
  ;; are looked up, never made into keywords that would stay for the session
  (let* ((name (format nil "TUATARA-UNSEEN-~d" (random 1000000000)))
         (text (format nil "#+NAME: t~%#+ATTR_LATEX: :~a 1 :width 5cm~%#+ATTR_~a: :x 1~%| a |~%" name name))
         (table (tuatara.defaults::org-find-in-tree (tuatara.defaults::org-parse text)
                                                    (lambda (n) (eq (tuatara.defaults::org-node-type n) 'tuatara.defaults::table)))))
    (true table)
    ;; the known ones still kept
    (is string= "t" (getf (tuatara.defaults::org-property table :affiliated) :name))
    (is equal '(:width "5cm") (tuatara.defaults::org-latex-attributes table "latex"))
    (is equal (list :scope "file" (format nil ":~(~a~)" name) 1)
        (tuatara.defaults::dblock-read-params (format nil ":scope file :~(~a~) 1" name)))
    (false (find-symbol name :keyword))
    (false (find-symbol (format nil "ATTR_~a" name) :keyword))))

(define-test (org macro-text-shown-green)
  (let ((f (run-keys "M-x o r g - m o d e RET"
                     :text (format nil "Subject {{{redact(John Otago)}}} met {{{protect(Ann)}}}.~%") :width 80 :height 6)))
    (true (org-faces-include f 0 "John Otago" :org-macro-text))
    (true (org-faces-include f 0 "Ann" :org-macro-text))
    ;; the braces around them hidden, as a link's brackets are
    (true (uiop:string-prefix-p "Subject John Otago met Ann." (tuatara.frontend.fake:fake-screen-line f 0)))))

(define-test (org export-drawer-coloured)
  ;; an :EXPORT: drawer's settings as properties are: the name, the value; a name that is no setting as an error
  (let ((f (run-keys "M-x o r g - m o d e RET"
                     :text (format nil ":EXPORT:~%widths: 30% *~%colour: red~%:END:~%") :width 80 :height 8)))
    (true (org-faces-include f 1 "widths:" :org-special-keyword))
    (true (org-faces-include f 1 " 30% *" :org-property-value))
    (true (org-faces-include f 2 "colour:" :error))))

(define-test (org macro-calls-shown-as-their-text)
  ;; any macro call with arguments shows as them, as redact and protect do; one without stays as written
  (let ((f (run-keys "M-x o r g - m o d e RET"
                     :text (format nil "Press {{{kbd(C-x C-s)}}} in {{{title}}}.~%| {{{kbd(a)}}} | b |~%| ccc | d |~%") :width 80 :height 6)))
    (true (uiop:string-prefix-p "Press C-x C-s in {{{title}}}." (tuatara.frontend.fake:fake-screen-line f 0))))
  (flet ((lines (&rest lines) (format nil "~{~a~^~%~}" lines)))
    (is string= (lines "| {{{kbd(a)}}}   | b |" "| ccc | d |")
        (remove #\@ (org-table-after "C-c C-c" (lines "| {{{kbd(a)}}}@ | b |" "| ccc | d |"))))))

(define-test (org hiding-braces-shown-at-point)
  ;; point in a call: its braces shown, to be edited; the other call's still hidden
  (let ((f (run-keys (format nil "M-x o r g - m o d e RET g g ~{~a~^ ~}" (make-list 9 :initial-element "l"))
                     :text (format nil "Subject {{{R(John Otago)}}} met {{{P(Ann)}}}.~%") :width 80 :height 6)))
    (true (uiop:string-prefix-p "Subject {{{R(John Otago)}}} met Ann." (tuatara.frontend.fake:fake-screen-line f 0)))))

(define-test (org wrapping-in-a-macro)
  ;; C-c m: the selection or the word at point wrapped in a macro chosen from a list: R, P, those the note declares
  (flet ((after (keys text)
           (let ((tuatara.defaults::*org-last-macro* "R"))
             (multiple-value-bind (f editor) (run-keys (format nil "M-x o r g - m o d e RET ~a" keys) :text text :width 100 :height 8)
               (declare (ignore f))
               (session-text editor)))))
    (is string= (format nil "Met {{{R(John)}}} today.~%") (after "g g l l l l C-c m RET" (format nil "Met John today.~%")))
    (is string= (format nil "From {{{R(90.233.1.40)}}}.~%") (after "g g l l l l l l C-c m R RET" (format nil "From 90.233.1.40.~%")))
    (is string= (format nil "Met {{{P(John Otago)}}} today.~%") (after "g g l l l l v l l l l l l l l l l C-c m P RET" (format nil "Met John Otago today.~%")))
    ;; a macro the note declares, by its name
    (is string= (format nil "#+macro: kbd @@latex:\\texttt{$1}@@~%Press {{{kbd(C-x)}}} now.~%")
        (after "j l l l l l l C-c m k b d RET" (format nil "#+macro: kbd @@latex:\\texttt{$1}@@~%Press C-x now.~%")))
    ;; in a call: another macro, or none, the text alone again
    (is string= (format nil "Met {{{P(John)}}} today.~%") (after "g g l l l l l l l l C-c m P RET" (format nil "Met {{{R(John)}}} today.~%")))
    (is string= (format nil "Met John today.~%") (after "g g l l l l l l l l C-c m n o n e RET" (format nil "Met {{{redact(John)}}} today.~%")))
    ;; C-c M: the call point is in taken away, any macro's
    (is string= (format nil "Met John Otago and C-x.~%") (after "g g l l l l l l l l C-c M" (format nil "Met {{{R(John Otago)}}} and C-x.~%")))
    (is string= (format nil "Press C-x now.~%") (after "g g l l l l l l l l l C-c M" (format nil "Press {{{kbd(C-x)}}} now.~%")))))

(define-test (org todo-keyword-after-any-spaces)
  ;; spaces or tabs after the stars, more than one: the keyword is changed where it is, the title kept
  (true (uiop:string-prefix-p (format nil "*   WAIT Task~%") (org-text-after "C-c C-t" (format nil "*   TODO Task~%"))))
  (true (uiop:string-prefix-p (format nil "**~cTODO Task~%" #\Tab) (org-text-after "C-c C-t" (format nil "**~cTask~%" #\Tab))))
  (true (uiop:string-prefix-p (format nil "*  Task~%") (org-text-after "C-c C-t C-c C-t C-c C-t C-c C-t" (format nil "*  Task~%"))))
  ;; a task that repeats, marked done: its keyword the first again, where it was
  (true (uiop:string-prefix-p (format nil "*   TODO Task~%SCHEDULED: <2026-")
                              (org-text-after "C-c C-t C-c C-t" (format nil "*   TODO Task~%SCHEDULED: <2026-09-28 Mon +1w>~%")))))

(define-test (org keys-wait-for-no-parse-on-plain-lines)
  ;; RET and TAB on a line that can hold no link, table or checkbox: the file is not parsed for them
  (let ((calls 0))
    (sb-int:encapsulate 'tuatara.defaults::org-tree-now 'count
                        (lambda (function &rest arguments) (incf calls) (apply function arguments)))
    (unwind-protect
         (flet ((parses (keys text)
                  (setf calls 0)
                  (run-keys (format nil "M-x o r g - m o d e RET ~a" keys) :text text :width 60 :height 8)
                  calls))
           (let ((plain (format nil "Plain words here~%and more~%")))
             (is = (parses "" plain) (parses "RET RET TAB" plain)))
           ;; a line with a link, a table's line: parsed, as before
           (let ((link (format nil "See [[*Notes]] here~%* Notes~%")))
             (true (< (parses "" link) (parses "RET" link))))
           (let ((table (format nil "| a | b |~%| c | d |~%")))
             (true (< (parses "" table) (parses "TAB" table)))))
      (sb-int:unencapsulate 'tuatara.defaults::org-tree-now 'count))))

(define-test (org largest-file-opens-as-text)
  ;; an Org file larger than Org's readers take opens in text mode: no whole-buffer parse of gigabytes
  (is eq 'tuatara.defaults::org-mode (tuatara.modes:mode-for-size 'tuatara.defaults::org-mode tuatara.defaults::*org-file-read-limit*))
  (is eq 'tuatara.defaults::text-mode (tuatara.modes:mode-for-size 'tuatara.defaults::org-mode (1+ tuatara.defaults::*org-file-read-limit*)))
  (with-test-directory (dir)
    (let ((path (namestring (merge-pathnames "big.org" dir))))
      (alexandria:write-string-into-file (format nil "* A~%~a~%" (make-string 500 :initial-element #\x)) path)
      (let ((tuatara.defaults::*org-file-read-limit* 100))
        (with-visited (buffer path)
          (tuatara.modes:set-auto-mode buffer)
          (is eq 'tuatara.defaults::text-mode (type-of (tuatara.modes:buffer-major-mode buffer))))))))
