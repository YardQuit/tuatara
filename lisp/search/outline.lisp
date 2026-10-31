;;;; lisp/search/outline.lisp --- Going to a heading or a definition: buffer-outline.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; M-x buffer-outline (M-g o) lists the headings of the
;;;; buffer and goes to the one chosen, as consult-outline does in Emacs: Org's
;;;; headings, Lisp's ;;; comments and top-level forms, and in a language parsed
;;;; with tree-sitter the definitions and headings its rules name (syntax/tags/,
;;;; Helix's), else its functions and types.
;;;; Each shows as its line, coloured as in the buffer, after its number, in the
;;;; order of the text; moving among them shows each in the window, RET stays
;;;; there and C-g goes back. The headings are found in the background.

(in-package #:tuatara.defaults)

(define-face :outline-preview "The line of the heading shown while one is chosen in \\[buffer-outline]."
  :inherit :hl-line)

;;; Where the headings are

(defgeneric mode-outline (mode buffer)
  (:documentation "How the headings of BUFFER are found in MODE: a function of
the text (a rope), its syntax tree (NIL when it has none) and HALT, a function
answering true when the search should stop, returning the headings in the
order of the text, each (START LEVEL): the octet it begins at and its depth,
1 for the outermost. That function runs in a thread of its own."))

(defparameter *outline-regex* "^[*\\f]+"
  "Where a heading begins in a mode with no headings of its own: a line
beginning with stars or a page break, the more of them the deeper.")

(defparameter *org-outline-regex* "^\\*+ "
  "Where an Org heading begins: a star or more at the start of a line, and a
space; the more stars, the deeper.")

(defparameter *lisp-outline-regex* "^(?:;;;+ \\S|\\()"
  "Where a heading begins in Lisp: a comment of three semicolons or more and a
space before its text, the more semicolons the deeper; and a top-level form,
an opening bracket at the start of a line, deeper than any comment.")

(defconstant +form-level+ 1000
  "The depth of a Lisp top-level form among the headings of its file: under
every comment heading.")

(defun regex-outline (regex level-of)
  "A finder of headings (see MODE-OUTLINE) where REGEX matches at the start of
a line, each as deep as LEVEL-OF, a function of the text matched, says."
  (let ((scanner (tuatara.syntax:compile-regex regex :case-fold nil)))
    (lambda (rope tree halt)
      (declare (ignore tree))
      (let ((headings '()))
        (tuatara.syntax:map-regex-matches
         (lambda (start end text groups)
           (declare (ignore end groups))
           (push (list start (funcall level-of text)) headings)
           nil)
         rope scanner 0 (tuatara.text:rope-bytes rope) :halt halt)
        (nreverse headings)))))

(defun definition-captures (query)
  "For each capture of QUERY, by number, true when it names a definition or a
heading: @definition, or @definition and a kind (@definition.function)."
  (map 'simple-vector
       (lambda (name)
         (or (string= name "definition")
             (and (> (length name) 11) (string= "definition." name :end2 11))))
       (tuatara.syntax:query-captures query)))

(defun tree-headings (query tree rope halt wanted)
  "The headings QUERY names in TREE, the syntax tree of ROPE, as MODE-OUTLINE
gives them: each node captured as a definition, WANTED saying by capture
number which captures are, as deep as the definitions around it make it, the
first on each line. Gives what it found so far when HALT answers true."
  (let ((nodes '()) (matches 0) (total (tuatara.text:rope-bytes rope)))
    (block scan
      (tuatara.syntax:map-query-matches
       (lambda (pattern captures count)
         (declare (ignore pattern))
         (when (and (zerop (logand (incf matches) 255)) (funcall halt))
           (return-from scan))
         (dotimes (i count)
           (when (aref wanted (aref captures (* 4 i)))
             (push (cons (min total (aref captures (+ 1 (* 4 i)))) (min total (aref captures (+ 2 (* 4 i)))))
                   nodes))))
       query tree rope 0 total))
    ;; in the order of the text, the outer first of two that begin together
    (setf nodes (sort nodes (lambda (a b) (or (< (car a) (car b)) (and (= (car a) (car b)) (> (cdr a) (cdr b)))))))
    (let ((open '()) (headings '()) (last-line -1) (previous nil))
      (dolist (node nodes (nreverse headings))
        ;; a node two rules name is one heading
        (unless (equal node previous)
          (setf previous node)
          (destructuring-bind (start . end) node
            (loop while (and open (<= (first open) start)) do (pop open))
            (let ((line (tuatara.text:rope-line-start rope start)))
              (unless (= line last-line)
                (setf last-line line)
                (push (list start (1+ (length open))) headings)))
            (push end open)))))))

(defmethod mode-outline ((mode fundamental-mode) buffer)
  "The headings of a buffer in MODE: the definitions its language's rules name,
from its syntax tree, else its lines that look like headings (*OUTLINE-REGEX*)."
  (declare (ignore buffer))
  (let* ((language (mode-language mode))
         (query (and language (or (language-compiled-query language :tags) (language-compiled-query language :definitions))))
         (lines (regex-outline *outline-regex* #'length)))
    (if query
        (let ((wanted (definition-captures query)))
          (lambda (rope tree halt)
            (if tree
                (tree-headings query tree rope halt wanted)
                (funcall lines rope tree halt))))
        lines)))

(defmethod mode-outline ((mode csv-mode) buffer)
  "No headings in a table of fields and records: a file of gigabytes is not read for them."
  (declare (ignore buffer))
  (lambda (rope tree halt)
    (declare (ignore rope tree halt))
    '()))

(defmethod mode-outline ((mode org-mode) buffer)
  "The headings of an Org note, at the level their stars give."
  (declare (ignore buffer))
  (regex-outline *org-outline-regex* (lambda (text) (1- (length text)))))

(defun lisp-outline ()
  "A finder of the headings of Lisp (see MODE-OUTLINE): its comments of three
semicolons or more, and its top-level forms."
  (regex-outline *lisp-outline-regex*
                 (lambda (text)
                   (if (char= (char text 0) #\()
                       +form-level+
                       (- (or (position #\; text :test-not #'char=) (length text)) 2)))))

(macrolet ((lisp-outlines (&rest modes)
             `(progn ,@(loop for mode in modes
                             collect `(defmethod mode-outline ((mode ,mode) buffer)
                                        "The headings of Lisp: comments of three semicolons or more, and top-level forms (LISP-OUTLINE)."
                                        (declare (ignore buffer))
                                        (lisp-outline))))))
  (lisp-outlines lisp-mode emacs-lisp-mode clojure-mode scheme-mode racket-mode fennel-mode janet-mode hy-mode))

;;; The lines of the headings

(defparameter *outline-line-limit* 400
  "The most octets of a heading's line \\[buffer-outline] shows: a line of
megabytes, as a minified script has, is cut short.")

(defun outline-line-end (rope line-start)
  "Where the line beginning at LINE-START in ROPE ends, or where it is cut
short, *OUTLINE-LINE-LIMIT* octets on, at the start of a character."
  (let ((limit (min (tuatara.text:rope-bytes rope) (+ line-start *outline-line-limit*))))
    (or (tuatara.text:rope-find-line-end rope :start line-start :end limit)
        (loop for end downfrom limit above line-start
              when (tuatara.text:char-boundary-p rope end) return end
              finally (return line-start)))))

(defun outline-line-text (rope start end tab-width ranges)
  "The text of ROPE from octet START below END as \\[buffer-outline] shows it:
each character as the buffer shows it (TUATARA.DISPLAY:CHAR-DISPLAY), a tab
up to the next of every TAB-WIDTH columns, no blanks at the end; and RANGES,
\(START END FACE) ranges of octets painted in order, as (START END FACE) spans
over that text, as two values."
  (let* ((string (let ((s (tuatara.text:rope-string rope :start start :end end)))
                   (if (and (plusp (length s)) (char= (char s (1- (length s))) #\Return))
                       (subseq s 0 (1- (length s)))
                       s)))
         (size (- end start))
         ;; where each octet's character begins in the text shown
         (index-at (make-array (1+ size) :initial-element 0))
         (octet 0) (column 0) (here 0)
         (text (with-output-to-string (out)
                 (loop for char across string
                       do (multiple-value-bind (shown columns) (tuatara.display:char-display char column tab-width)
                            (loop for o from octet below (min size (+ octet (tuatara.text:char-octet-length char)))
                                  do (setf (aref index-at o) here))
                            (incf octet (tuatara.text:char-octet-length char))
                            (if (characterp shown) (write-char shown out) (write-string shown out))
                            (incf here (if (characterp shown) 1 (length shown)))
                            (incf column columns)))
                 (loop for o from (min octet size) to size
                       do (setf (aref index-at o) here))))
         (text (string-right-trim " " text))
         (faces (make-array (length text) :initial-element nil)))
    (loop for (s e face) in ranges
          for from = (aref index-at (max 0 (min size (- s start))))
          for to = (min (length text) (aref index-at (max 0 (min size (- e start)))))
          do (loop for i from from below to
                   do (setf (aref faces i) (if (eq face :none) nil face))))
    (values text
            (let ((spans '()) (i 0))
              (loop while (< i (length text))
                    do (let ((face (aref faces i)) (from i))
                         (loop while (and (< i (length text)) (eq (aref faces i) face)) do (incf i))
                         (when face (push (list from i face) spans))))
              (nreverse spans)))))

(defun outline-entries (headings rope tab-width colour halt)
  "HEADINGS, as MODE-OUTLINE gives them, each with its line: as (START LEVEL
NUMBER TEXT SPANS), NUMBER the line's, counted from 1, TEXT the line as
OUTLINE-LINE-TEXT shows it and SPANS its colours, from COLOUR, a function of
the octets a line begins and ends at returning (START END FACE) ranges of
octets to paint in order, or NIL for none. Gives what it has so far when HALT
answers true."
  (let ((indexed (tuatara.text:rope-indexed-p rope)) (number 1) (counted 0) (entries '()))
    (loop for (start level) in headings
          for i from 0
          until (and (zerop (logand i 255)) (funcall halt))
          do (let* ((line-start (tuatara.text:rope-line-start rope start))
                    (line-end (outline-line-end rope line-start)))
               (if indexed
                   (setf number (1+ (tuatara.text:byte->line rope line-start)))
                   (progn (incf number (tuatara.text:rope-count-newlines rope counted line-start))
                          (setf counted line-start)))
               (multiple-value-bind (text spans)
                   (outline-line-text rope line-start line-end tab-width
                                      (and colour (funcall colour line-start line-end)))
                 (push (list start level number text spans) entries))))
    (nreverse entries)))

(defun outline-tree (syntax)
  "The syntax tree of SYNTAX's text, after waiting for the parse under way when
there is no tree yet, while \\`C-g' is not pressed (which gives up the
command, the parse going on); NIL when the text has none, being too large or
not parsed."
  (or (tuatara.syntax:syntax-ready-tree syntax)
      (when (tuatara.syntax:parse-under-way-p syntax)
        (unless (wait-while-interruptibly (lambda (seconds) (tuatara.syntax:parse-under-way-p syntax seconds))
                                          :message "Reading the text...")
          (keyboard-quit-signal))
        (tuatara.syntax:syntax-ready-tree syntax))))

(defun outline-collect (finder rope tree colours faces tab-width &optional buffer)
  "The entries of the headings FINDER, from MODE-OUTLINE, finds in ROPE, whose
syntax tree is TREE or NIL, as OUTLINE-ENTRIES gives them, their lines
coloured by the query COLOURS with FACES when it is given: found in a thread
of the background while the editor waits, \\`C-g' giving up the command; when
the search does not end at once then, BUFFER's file stays mapped for it
\(BUFFER-LENT-P) while it ends on its own."
  (let* ((halted nil) (result nil) (failure nil)
         (copy (and tree (tuatara.syntax:copy-tree-pointer tree)))
         (thread (ignore-errors
                  (tuatara.runtime:make-guarded-thread
                   (lambda ()
                     (let ((halt (lambda () halted)))
                       (unwind-protect
                            (handler-case
                                (tuatara.runtime:reporting-errors ("finding the headings")
                                  (setf result
                                        (outline-entries
                                         (funcall finder rope copy halt) rope tab-width
                                         (and colours copy
                                              (lambda (start end)
                                                (tuatara.syntax:rich-highlights colours copy rope start end faces)))
                                         halt)))
                              (error (e) (setf failure e)))
                         (when copy (tuatara.syntax:delete-tree copy)))))
                   :name "tuatara outline"))))
    (cond ((null thread)
           (when copy (tuatara.syntax:delete-tree copy))
           (error "There is no thread to find the headings in"))
          ((multiple-value-bind (done state)
                   (wait-interruptibly thread :message "Finding the headings..."
                                              :stop (lambda (thread) (declare (ignore thread)) (setf halted t)))
             (when (and (eq state :running) buffer)
               (setf (tuatara.buffer:buffer-lent-p buffer) t))
             (not done))
           (keyboard-quit-signal))
          (failure (error "The headings could not be found: ~a" failure))
          (t result))))

;;; Choosing one

(defvar *outline-shown* nil
  "While \\[buffer-outline] shows a heading: the window it shows it in and the
octet the heading begins at, as a cons.")

(defun outline-shown-range (editor window rows)
  "The line of the heading \\[buffer-outline] shows in WINDOW, as a list of one
(START END :outline-preview) range over ROWS; NIL when it shows none there."
  (declare (ignore editor))
  (let ((shown *outline-shown*))
    (when (and shown (eq (car shown) window))
      (line-rows-range rows (cdr shown) :outline-preview))))

(defparameter *outline-lines-above* 3
  "The lines \\[buffer-outline] shows above a heading it brings into sight.")

(defun outline-show (window position)
  "Move WINDOW's point to POSITION; when that is out of sight, or where the
list of headings may cover it, show it *OUTLINE-LINES-ABOVE* lines from the
top of the window, the text under it in the rows below."
  (let* ((rows (tuatara.display:window-rows window))
         ;; the rows no list of candidates covers, however tall
         (seen (min (length rows)
                    (max 1 (- (or (tuatara.display:window-full-height window) (tuatara.display:window-height window))
                              (1- *panel-max-rows*))))))
    (setf (tuatara.display:window-point window) position)
    (unless (and (plusp (length rows))
                 (<= (tuatara.display:row-start (aref rows 0)) position)
                 (< position (tuatara.display:row-end (aref rows (1- seen)))))
      (let ((rope (tuatara.buffer:buffer-text (tuatara.display:window-buffer window))))
        (setf (tuatara.display:window-start window)
              (tuatara.display:scroll-lines rope (tuatara.text:rope-line-start rope position) (- *outline-lines-above*)))))))

(defun outline-input (input)
  "The depth INPUT asks for, when it begins with a digit from 1 to 9 and a
space, else NIL; and the words after that, as two values."
  (if (and (>= (length input) 2) (digit-char-p (char input 0)) (char/= (char input 0) #\0)
           (char= (char input 1) #\Space))
      (values (digit-char-p (char input 0)) (subseq input 2))
      (values nil input)))

(defun outline-table (candidates lowest)
  "A completion table of the headings CANDIDATES in their order: those all the
words typed match, as completion matches them, and when the input begins with
a digit from 1 to 9 and a space, only those that many levels deep or less,
counted from LOWEST, the depth of the outermost."
  (lambda (input)
    (multiple-value-bind (depth words) (outline-input input)
      (let ((deepest (and depth (+ lowest depth -1))))
        (loop for candidate in candidates
              when (and (or (null deepest) (<= (second (candidate-value candidate)) deepest))
                        (candidate-score words candidate))
                collect candidate)))))

(defun outline-selection (shown origin)
  "The index among SHOWN, the candidates of headings in the order of the text,
of the heading ORIGIN is under: the last to begin at or before it, else the
first."
  (let ((index 0))
    (loop for i from 0 below (length shown)
          while (<= (first (candidate-value (aref shown i))) origin)
          do (setf index i))
    index))

(defun outline-choose (buffer window entries)
  "Have a heading of BUFFER chosen from ENTRIES, from OUTLINE-ENTRIES, each
shown in WINDOW as it is selected, and go there, the mark left where point
was; or, when the choosing is given up, go back as things were."
  (let* ((origin (tuatara.buffer:buffer-point buffer))
         (start (tuatara.display:window-start window))
         (mark (tuatara.buffer:buffer-mark buffer))
         (active (tuatara.buffer:buffer-mark-active-p buffer))
         (digits (length (princ-to-string (reduce #'max entries :key #'third))))
         (lowest (reduce #'min entries :key #'second))
         (candidates (loop for (position level number text spans) in entries
                           collect (make-candidate text :value (list position level)
                                                        :columns (list (list (format nil "~vd" digits number) :line-number)
                                                                       (list text spans)))))
         (depth (prefix-argument))
         (chosen nil))
    (setf (tuatara.buffer:buffer-mark-active-p buffer) nil)
    (unwind-protect
         (let ((*outline-shown* nil))
           (setf chosen
                 (completing-read "Go to heading: " (outline-table candidates lowest)
                                  :require-match t :filter nil :separator (code-char 0)
                                  :initial (if (and (integerp depth) (<= 1 depth 9)) (format nil "~d " depth) "")
                                  :select (lambda (shown input) (declare (ignore input)) (outline-selection shown origin))
                                  :preview (lambda (candidate)
                                             (let ((position (if candidate (first (candidate-value candidate)) origin)))
                                               (setf *outline-shown* (and candidate (cons window position)))
                                               (outline-show window position))))))
      (unless chosen
        (setf (tuatara.display:window-point window) origin
              (tuatara.display:window-start window) start
              (tuatara.buffer:buffer-mark buffer) mark
              (tuatara.buffer:buffer-mark-active-p buffer) active)))
    (let ((position (first chosen)))
      (setf (tuatara.display:window-point window) position)
      (unless (= position origin)
        (push-mark :position origin :quiet t :buffer buffer)))))

(define-command buffer-outline ()
  "Go to a heading of the current buffer, chosen from a list of them: Org's
headings, Lisp's comments of three semicolons and top-level forms, and in a
language parsed with tree-sitter its definitions and headings (a function, a
type, a Markdown heading), each shown as its line after its number, in the
order of the text. Typing narrows the
list, and a digit and a space typed first (2 SPC) keep only the headings that
many levels deep or less, as a prefix argument does. Moving in the list shows
each heading in the window; \\<minibuffer>\\[minibuffer-done] goes there, leaving the mark where
point was, and \\[minibuffer-abort] goes back. The list opens on the heading point is under.
In a narrowed buffer, the headings of the part shown."
  (let* ((buffer (current-buffer))
         (mode (buffer-major-mode buffer))
         (finder (mode-outline mode buffer))
         (syntax (and (not (tuatara.io:safe-visit-p buffer)) (tuatara.syntax:buffer-syntax buffer)))
         (tree (and syntax (outline-tree syntax)))
         (rope (if tree (tuatara.syntax:syntax-rope syntax) (tuatara.buffer:buffer-text buffer)))
         (language (mode-language mode))
         (colours (and tree language (not (eq (language-colouring language mode) :none))
                       (rules-at-hand language) (language-compiled-query language :colouring)))
         (faces (and colours (multiple-value-call #'query-faces colours (language-colouring language mode))))
         (entries (multiple-value-bind (low high) (tuatara.buffer:buffer-bounds buffer)
                    ;; of a narrowed buffer, the headings of the part shown
                    (remove-if-not (lambda (entry) (and (<= low (first entry)) (< (first entry) high)))
                                   (outline-collect finder rope tree colours faces (tuatara.display:tab-width-of buffer) buffer)))))
    (if entries
        (outline-choose buffer (current-window) entries)
        (message "No headings~:[~; in the part shown~]" (tuatara.buffer:buffer-narrowed-p buffer)))))

(defun install-outline ()
  "Bind \\[buffer-outline] to \\`M-g o', and light the line of the
heading it shows."
  (define-key *global-keymap* "M-g o" 'buffer-outline)
  (pushnew 'outline-shown-range *line-highlight-functions*))
