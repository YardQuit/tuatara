;;;; lisp/modes/lisp-indent.lisp --- Indenting Common Lisp as Emacs's lisp-mode does.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A line of Lisp is indented as Emacs indents it in lisp-mode:
;;;; calculate-lisp-indent finds where a line would line up by default, and
;;;; common-lisp-indent-function (cl-indent.el) knows the macros that indent
;;;; their bodies otherwise, from each one's indentation method. The text is
;;;; read as Lisp is read, from the start of the top-level form: which lists
;;;; are open at each line, and the complete expressions inside each. Lines
;;;; indented together are read in one pass, as Emacs's indent-region reads
;;;; them.

(in-package #:tuatara.defaults)

;;; Reading the text

(defstruct (lisp-frame (:constructor make-lisp-frame (open start)) (:copier nil))
  "A list open at the place looked at: OPEN, the octet of its opening bracket;
START, where it starts with any quote or other mark before the bracket; and the
complete expressions inside it so far, COUNT of them, the start and end of
each in turn in ELEMENTS, the oldest first."
  (open 0 :type fixnum :read-only t)
  (start 0 :type fixnum :read-only t)
  (elements (load-time-value (make-array 0 :element-type 'fixnum) t) :type (simple-array fixnum (*)))
  (count 0 :type fixnum))

(setf (documentation 'make-lisp-frame 'function) "A LISP-FRAME for the list opened at OPEN, marks before it from START."
      (documentation 'lisp-frame-p 'function) "True when OBJECT is a LISP-FRAME."
      (documentation 'lisp-frame-open 'function) "The octet of the list's opening bracket."
      (documentation 'lisp-frame-start 'function) "Where the list starts, marks before its bracket included."
      (documentation 'lisp-frame-elements 'function) "The start and end of each complete expression in the list in turn, the oldest first."
      (documentation 'lisp-frame-count 'function) "How many complete expressions the list holds so far.")

(declaim (inline frame-add))
(defun frame-add (frame start end)
  "Note a complete expression from START below END inside FRAME."
  (declare (type lisp-frame frame) (type fixnum start end) (optimize speed))
  (let* ((elements (lisp-frame-elements frame))
         (k (* 2 (lisp-frame-count frame))))
    (declare (type fixnum k))
    (when (>= k (length elements))
      (let ((bigger (make-array (max 8 (* 2 (length elements))) :element-type 'fixnum)))
        (replace bigger elements)
        (setf elements bigger
              (lisp-frame-elements frame) bigger)))
    (setf (aref elements k) start
          (aref elements (1+ k)) end)
    (incf (lisp-frame-count frame))))

(defun element-start (frame k)
  "Where complete expression K in FRAME starts, counting from 0, the oldest first."
  (aref (lisp-frame-elements frame) (* 2 k)))

(defun element-end (frame k)
  "Where complete expression K in FRAME ends, counting from 0, the oldest first."
  (aref (lisp-frame-elements frame) (1+ (* 2 k))))

(defun lisp-prefix-octet-p (octet)
  "True when OCTET is a mark put before an expression: ' ` , @ or #."
  (member octet '(39 96 44 64 35)))

(defconstant +lisp-chunk+ 65536
  "How many octets reading Lisp text takes from the text at a time.")

(defstruct (lisp-scan (:constructor make-lisp-scan (rope position)) (:copier nil))
  "ROPE's Lisp, read from the start of a top-level form up to octet POSITION:
STACK, the lists open there, the innermost first; PREFIX, where the marks
before an expression not yet read begin, or NIL; INSIDE, :STRING when POSITION
is inside a string that the octet DELIMITER ends and that began at
STRING-START, :COMMENT when inside block comments nested DEPTH deep, else NIL.
CHUNK holds ROPE's octets from CHUNK-START, read ahead."
  (rope nil :read-only t)
  (position 0 :type fixnum)
  (stack '())
  (prefix nil)
  (inside nil)
  (delimiter 0 :type fixnum)
  (string-start 0 :type fixnum)
  (depth 0 :type fixnum)
  (chunk (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (*)))
  (chunk-start 0 :type fixnum))

(setf (documentation 'make-lisp-scan 'function) "A LISP-SCAN of ROPE about to read from POSITION, nothing open."
      (documentation 'lisp-scan-p 'function) "True when OBJECT is a LISP-SCAN."
      (documentation 'lisp-scan-rope 'function) "The text read."
      (documentation 'lisp-scan-position 'function) "The octet reading goes on from."
      (documentation 'lisp-scan-stack 'function) "The lists open where reading stopped, the innermost first."
      (documentation 'lisp-scan-prefix 'function) "Where the marks before an expression not yet read begin, or NIL."
      (documentation 'lisp-scan-inside 'function) ":STRING or :COMMENT when reading stopped inside one, else NIL."
      (documentation 'lisp-scan-delimiter 'function) "The octet ending the string reading stopped inside."
      (documentation 'lisp-scan-string-start 'function) "Where the string reading stopped inside begins."
      (documentation 'lisp-scan-depth 'function) "How deeply the block comments reading stopped inside nest."
      (documentation 'lisp-scan-chunk 'function) "Octets of the text read ahead."
      (documentation 'lisp-scan-chunk-start 'function) "The offset of the first octet read ahead.")

(defun advance-scan (scan target)
  "Read SCAN's text on to octet TARGET, or just past it when an expression or a
comment runs over it, and return SCAN."
  (declare (type lisp-scan scan) (type fixnum target) (optimize speed))
  (let* ((rope (lisp-scan-rope scan))
         (size (tuatara.text:rope-bytes rope))
         (i (lisp-scan-position scan))
         (stack (lisp-scan-stack scan))
         (prefix (lisp-scan-prefix scan))
         (chunk (lisp-scan-chunk scan))
         (base (lisp-scan-chunk-start scan)))
    (declare (type fixnum size i base) (type (simple-array (unsigned-byte 8) (*)) chunk))
    (labels ((at (j)
               (declare (type fixnum j))
               (let ((k (- j base)))
                 (declare (type fixnum k))
                 (cond ((< -1 k (length chunk)) (aref chunk k))
                       ((< -1 j size)
                        (setf base (max 0 (- j 16))
                              chunk (tuatara.text:rope-octets rope :start base :end (min size (+ base +lisp-chunk+))))
                        (aref chunk (- j base)))
                       (t 0))))
             (element (start end)
               (declare (type fixnum start end))
               (when stack (frame-add (first stack) (or prefix start) end))
               (setf prefix nil))
             (finish-string ()
               ;; on through the string to its end; NIL when TARGET comes first
               (loop with delimiter of-type fixnum = (lisp-scan-delimiter scan)
                     while (< i target)
                     do (let ((b (at i)))
                          (cond ((= b 92) (incf i 2))
                                ((= b delimiter)
                                 (incf i)
                                 (setf (lisp-scan-inside scan) nil)
                                 (element (lisp-scan-string-start scan) i)
                                 (return t))
                                (t (incf i))))
                     finally (return nil)))
             (finish-comment ()
               ;; on through nested block comments to their end; NIL when TARGET comes first
               (loop while (< i target)
                     do (cond ((and (= (at i) 124) (= (at (1+ i)) 35))
                               (incf i 2)
                               (when (zerop (decf (lisp-scan-depth scan)))
                                 (setf (lisp-scan-inside scan) nil)
                                 (return t)))
                              ((and (= (at i) 35) (= (at (1+ i)) 124))
                               (incf i 2)
                               (incf (lisp-scan-depth scan)))
                              (t (incf i)))
                     finally (return nil)))
             (read-atom (start j)
               ;; an atom from START, its text read from J; |...| in it is part of it, blanks and all
               (declare (type fixnum start j))
               (loop while (and (< j size) (lisp-symbol-octet-p (at j)))
                     do (case (at j)
                          (92 (incf j 2))
                          (124 (incf j)
                           (loop while (and (< j size) (/= (at j) 124))
                                 do (incf j (if (= (at j) 92) 2 1)))
                           (incf j))
                          (t (incf j))))
               (setf j (min j size))
               (element start j)
               (setf i (max (1+ start) j))))
      (declare (inline at))
      (when (case (lisp-scan-inside scan)
              (:string (finish-string))
              (:comment (finish-comment))
              (t t))
        (loop while (< i target)
              do (let ((b (at i)))
                   (declare (type (unsigned-byte 8) b))
                   (case b
                     ((32 9 10 12 13) (incf i))
                     (59                                                        ; ; to the line's end
                      (loop while (and (< i size) (/= (at i) 10) (/= (at i) 13)) do (incf i)))
                     (40
                      (push (make-lisp-frame i (or prefix i)) stack)
                      (setf prefix nil)
                      (incf i))
                     (41
                      (let ((frame (pop stack)))
                        (setf prefix nil)
                        (incf i)
                        (when frame (element (lisp-frame-start frame) i))))
                     ((34 124)                                                  ; "..." or |...|
                      (setf (lisp-scan-inside scan) :string
                            (lisp-scan-delimiter scan) b
                            (lisp-scan-string-start scan) i)
                      (incf i)
                      (unless (finish-string) (return)))
                     ((39 96 64)                                                ; ' ` @
                      (unless prefix (setf prefix i))
                      (incf i))
                     (44                                                        ; , ,@
                      (unless prefix (setf prefix i))
                      (incf i)
                      (when (= (at i) 64) (incf i)))
                     (35
                      (case (at (1+ i))
                        (124                                                    ; #| |#
                         (incf i 2)
                         (setf (lisp-scan-inside scan) :comment
                               (lisp-scan-depth scan) 1)
                         (unless (finish-comment) (return)))
                        (92 (read-atom i (+ i 3)))                              ; #\x: x is itself
                        (t                                                      ; # before a form
                         (unless prefix (setf prefix i))
                         (incf i))))
                     (t (read-atom i i)))))))
    (setf (lisp-scan-position scan) i
          (lisp-scan-stack scan) stack
          (lisp-scan-prefix scan) prefix
          (lisp-scan-chunk scan) chunk
          (lisp-scan-chunk-start scan) base)
    scan))

;;; Where a line lines up

(defparameter *lisp-body-indent* 2
  "Columns a body is indented by from its list's opening bracket.")

(defparameter *lisp-comment-column* 40
  "The column a line holding only a comment begun with one semicolon goes to.")

(defparameter *lisp-indent-methods*
  (let ((table (make-hash-table :test 'equal))
        (methods
          '(("block" 1) ("case" (4 &rest (&whole 2 &rest 1))) ("ccase" . "case") ("ecase" . "case")
            ("typecase" . "case") ("etypecase" . "case") ("ctypecase" . "case") ("catch" 1)
            ("cond" (&rest (&whole 2 &rest 1))) ("defvar" (4 2 2))
            ("defclass" (6 4 (&whole 2 &rest 1) (&whole 2 &rest 1))) ("defconstant" . "defvar")
            ("defcustom" (4 2 2 2)) ("defparameter" . "defvar") ("defconst" . "defcustom")
            ("define-condition" . "defclass") ("define-modify-macro" (4 &lambda &body))
            ("defsetf" (4 &lambda 4 &body)) ("defun" (4 &lambda &body)) ("defgeneric" (4 &lambda &body))
            ("define-setf-method" . "defun") ("define-setf-expander" . "defun") ("defmacro" . "defun")
            ("defsubst" . "defun") ("deftype" . "defun") ("defmethod" lisp-indent-defmethod)
            ("defpackage" (4 2)) ("defstruct" ((&whole 4 &rest (&whole 2 &rest 1)) &rest (&whole 2 &rest 1)))
            ("destructuring-bind" ((&whole 6 &rest 1) 4 &body)) ("do" lisp-indent-do) ("do*" . "do")
            ("dolist" ((&whole 4 2 1) &body)) ("dotimes" . "dolist") ("eval-when" 1)
            ("flet" ((&whole 4 &rest (&whole 1 &lambda &body)) &body)) ("labels" . "flet")
            ("macrolet" . "flet") ("generic-flet" . "flet") ("generic-labels" . "flet")
            ("handler-case" (4 &rest (&whole 2 &lambda &body))) ("restart-case" . "handler-case")
            ("if" (&rest nil)) ("lambda" (&lambda &rest lisp-indent-function-lambda-hack))
            ("let" ((&whole 4 &rest (&whole 1 1 2)) &body)) ("let*" . "let") ("compiler-let" . "let")
            ("handler-bind" . "let") ("restart-bind" . "let") ("locally" 1) (":method" (&lambda &body))
            ("multiple-value-bind" ((&whole 6 &rest 1) 4 &body)) ("multiple-value-call" (4 &body))
            ("multiple-value-prog1" 1) ("multiple-value-setq" (4 2)) ("multiple-value-setf" . "multiple-value-setq")
            ("pprint-logical-block" (4 2)) ("print-unreadable-object" ((&whole 4 1 &rest 1) &body))
            ("prog" (&lambda &rest lisp-indent-tagbody)) ("prog*" . "prog") ("prog1" 1) ("prog2" 2)
            ("progn" 0) ("progv" (4 4 &body)) ("return" 0) ("return-from" (nil &body))
            ("symbol-macrolet" . "let") ("tagbody" lisp-indent-tagbody) ("throw" 1) ("unless" 1)
            ("unwind-protect" (5 &body)) ("when" 1) ("with-accessors" . "multiple-value-bind")
            ("with-condition-restarts" . "multiple-value-bind") ("with-compilation-unit" (&lambda &body))
            ("with-output-to-string" (4 2)) ("with-slots" . "multiple-value-bind")
            ("with-standard-io-syntax" (2))
            ;; forms whose names Lisp code of other dialects uses too
            ("autoload" defun) ("defalias" defun) ("defvaralias" defun) ("define-category" defun)
            ("condition-case" 2) ("save-excursion" 0) ("save-restriction" 0) ("save-current-buffer" 0)
            ("while" 1) ("ignore-errors" 0) ("with-no-warnings" 0)
            ("dlet" 1) ("letrec" 1) ("while-let" 1) ("named-let" 2) ("cl-flet" 1)
            ("if-let" 2) ("if-let*" 2) ("when-let" 1) ("when-let*" 1) ("and-let*" 1)
            ("with-temp-file" 1) ("with-timeout" 1)
            ;; the init file's own: a package's settings after :config
            ("load-package" 1))))
    (dolist (entry methods table)
      (unless (gethash (first entry) table)
        (setf (gethash (first entry) table)
              (if (stringp (cdr entry))
                  (gethash (cdr entry) table)
                  (second entry))))))
  "Each macro's indentation method, by name in lower case: an integer (that
many distinguished arguments, then a body), a list (how each argument is
indented, see LISP-INDENT-259), or the name of a function doing it.")

(defparameter *lisp-defun-method* '(4 &lambda &body)
  "The method of forms indented as defun is.")

(defstruct (lisp-line (:constructor make-lisp-line (rope point frames moved text)) (:copier nil))
  "What indenting one line needs: the text, ROPE; the octet POINT the line
starts at; the FRAMES open there, the innermost first; MOVED, NIL or a table of
the lines before it indented in the same pass, each line's start to (COLUMN .
WIDTH), the column its text moves to and the octets of blanks it begins with
now; and TEXT, the line without the blanks it begins with (LISP-LINE-HEAD)."
  (rope nil :read-only t)
  (point 0 :type fixnum :read-only t)
  (frames '() :read-only t)
  (moved nil :read-only t)
  (text "" :type string :read-only t))

(setf (documentation 'make-lisp-line 'function) "A LISP-LINE from its parts."
      (documentation 'lisp-line-p 'function) "True when OBJECT is a LISP-LINE."
      (documentation 'lisp-line-rope 'function) "The text."
      (documentation 'lisp-line-point 'function) "The octet the line starts at."
      (documentation 'lisp-line-frames 'function) "The lists open at the line, the innermost first."
      (documentation 'lisp-line-moved 'function) "The lines before indented in the same pass: start to (COLUMN . WIDTH)."
      (documentation 'lisp-line-text 'function) "The line without the blanks it begins with, its first *LISP-LINE-HEAD* octets.")

(defun lisp-octet (line position)
  "The octet at POSITION of LINE's text, or 0 outside it."
  (let ((rope (lisp-line-rope line)))
    (if (< -1 position (tuatara.text:rope-bytes rope))
        (tuatara.text:rope-octet rope position)
        0)))

(defun lisp-column (line position)
  "The column of POSITION in LINE's text: its characters from the line's start,
a tab to the next tab stop, the lines before indented in the same pass taken
as indented (TEXT-COLUMN)."
  (text-column (lisp-line-rope line) position (lisp-line-moved line)))

(defun lisp-text (line start end)
  "The text of LINE from octet START below END, kept within the text; a
character cut at either end shows as its octets."
  (let* ((rope (lisp-line-rope line))
         (size (tuatara.text:rope-bytes rope))
         (start (max 0 (min start size)))
         (end (max start (min end size))))
    (tuatara.text:octets-string (tuatara.text:rope-octets rope :start start :end end))))

(defun same-line-p (line a b)
  "True when positions A and B of LINE's text are on one line."
  (not (tuatara.text:rope-find-octet (lisp-line-rope line) 10 :start (min a b) :end (max a b))))

(defun expression-proper (line position)
  "Where the expression whose marks begin at POSITION of LINE's text starts:
past those marks (' ` , @ and #, not the # of #\\x) and blanks between them."
  (let ((p position))
    (loop for b = (lisp-octet line p)
          while (or (member b '(32 9 10 12 13 39 96 44 64))
                    (and (= b 35) (/= (lisp-octet line (1+ p)) 92)))
          do (incf p))
    p))

(defun before-marks (line position)
  "POSITION of LINE's text moved back over the marks right before it: ' ` , @ #."
  (let ((p position))
    (loop while (and (> p 0) (lisp-prefix-octet-p (lisp-octet line (1- p))))
          do (decf p))
    p))

(defun nested-comment-end (octet-at position end)
  "Where the block comment #| |# beginning at POSITION ends, the ones nested in
it included, OCTET-AT giving the octet at a position of the text (0 past it)
and END where the text ends: just past its closing |#, or END."
  (let ((depth 0) (p position))
    (loop while (< p end)
          do (cond ((and (= (funcall octet-at p) 35) (= (funcall octet-at (1+ p)) 124))
                    (incf depth)
                    (incf p 2))
                   ((and (= (funcall octet-at p) 124) (= (funcall octet-at (1+ p)) 35))
                    (incf p 2)
                    (when (zerop (decf depth)) (return)))
                   (t (incf p))))
    (min p end)))

(defun block-comment-end (line position)
  "Where the block comment #| |# beginning at POSITION of LINE's text ends,
the ones nested in it included (NESTED-COMMENT-END)."
  (nested-comment-end (lambda (p) (lisp-octet line p)) position (tuatara.text:rope-bytes (lisp-line-rope line))))

(defun line-first-expression (line position)
  "Where the first expression on the line holding POSITION starts, read from
the line's start: past blanks, closing brackets, block comments and marks,
then back over the marks right before it; never past POSITION."
  (let ((p (tuatara.text:rope-line-start (lisp-line-rope line) position)))
    (loop while (< p position)
          do (let ((b (lisp-octet line p)))
               (cond ((and (= b 35) (= (lisp-octet line (1+ p)) 124)) (setf p (block-comment-end line p)))
                     ((member b '(32 9 12 13 41 39 96 44 64 35)) (incf p))
                     (t (return)))))
    (before-marks line (min p position))))

(defun lisp-normal-indent (line frame)
  "The column the line lines up at by default inside FRAME, and the start of
the last complete expression before the line (NIL when none, and the column is
then just after the bracket). The line goes under the first expression when
that is a list; while the expressions so far are all on the first one's line,
under the second, or under the first when it is alone or a blank follows the
bracket; else under the first expression on the line of the last one."
  (let ((open (lisp-frame-open frame))
        (count (lisp-frame-count frame)))
    (if (zerop count)
        (values (1+ (lisp-column line open)) nil)
        (let* ((last (element-start frame (1- count)))
               (proper (expression-proper line (element-start frame 0)))
               (at (cond ((= (lisp-octet line proper) 40) proper)          ; a list first: under its bracket
                         ((same-line-p line proper last)                   ; the first line inside
                          (if (or (= count 1) (member (lisp-octet line (1+ open)) '(32 9 12)))
                              (before-marks line proper)                    ; under the first expression
                              (before-marks line (expression-proper line (element-start frame 1)))))
                         (t (line-first-expression line last)))))         ; under the first on the last one's line
          (values (lisp-column line at) last)))))

(defun lisp-indent-call-method (line method path frame sexp-column normal-indent)
  "Apply METHOD for the form in FRAME to the line at PATH: call it when it
names a function, else read it as a list (LISP-INDENT-259)."
  (if (and method (symbolp method))
      (funcall method line path frame sexp-column normal-indent)
      (lisp-indent-259 line method path frame sexp-column normal-indent)))

(defun lisp-lambda-list-indent (line sexp-column containing)
  "The column in a lambda list for LINE: a lambda-list keyword under the list's
first element, other parameters after the last keyword before them, indented
by two."
  (let ((keyword "(?im)&(optional|rest|key|allow-other-keys|aux|whole|body|environment)([ \\t]|$)"))
    (if (cl-ppcre:scan (concatenate 'string "^" keyword) (lisp-line-text line))
        (1+ sexp-column)
        (let* ((text (lisp-text line containing (max containing (1- (lisp-line-point line)))))
               (found nil))
          (cl-ppcre:do-matches (s e keyword text) (setf found s))
          (if found
              (+ (lisp-column line (+ containing (tuatara.text:string-octet-length text :end found))) 2)
              (1+ sexp-column))))))

(defun lisp-indent-259 (line method path frame sexp-column normal-indent)
  "Indent by METHOD the line at PATH in the form in FRAME whose bracket is at
SEXP-COLUMN. METHOD is a list saying how each argument in turn is indented:
an integer, that many columns from the bracket; NIL, by default; &lambda, as a
lambda list; &body, as a body from there on; &rest, the entry after it for
every argument left; the name of a function that does it; or
(&whole N . ENTRIES), an argument that is a list itself, N saying how it is
indented and ENTRIES how its own arguments are."
  (let ((containing (lisp-frame-open (first (lisp-line-frames line))))
        (p path) n tem tail)
    (loop while p
          do (setf n (1- (car p)) p (cdr p) tail nil)
             (loop while n
                   do (setf tem (car method))
                      (cond ((and tail (not (or (consp tem) (symbolp tem))))
                             (return-from lisp-indent-259 normal-indent))
                            ((eq tem '&body)
                             (return-from lisp-indent-259
                               (if (and (= n 0) (null p)) (+ sexp-column *lisp-body-indent*) normal-indent)))
                            ((eq tem '&rest)
                             (setf tail (> n 0) n 0 method (cdr method)))
                            ((> n 0)
                             (setf n (1- n) method (cdr method))
                             (when (< n 0) (return-from lisp-indent-259 normal-indent)))
                            ((null tem)
                             (return-from lisp-indent-259
                               (if (consp normal-indent) normal-indent (list normal-indent containing))))
                            ((eq tem '&lambda)
                             (return-from lisp-indent-259
                               (cond ((null p) (list (+ sexp-column 4) containing))
                                     ((null (cdr p)) (list (lisp-lambda-list-indent line sexp-column containing) containing))
                                     (t normal-indent))))
                            ((integerp tem)
                             (return-from lisp-indent-259
                               (if (null p) (list (+ sexp-column tem) containing) normal-indent)))
                            ((symbolp tem)
                             (return-from lisp-indent-259 (funcall tem line path frame sexp-column normal-indent)))
                            (t
                             ;; destructuring: (&whole X ...)
                             (if p
                                 (setf method (cddr tem) n nil)
                                 (let ((whole (cadr tem)))
                                   (return-from lisp-indent-259
                                     (cond (tail normal-indent)
                                           ((null whole) (list normal-indent containing))
                                           ((integerp whole) (list (+ sexp-column whole) containing))
                                           (t (funcall whole line path frame sexp-column normal-indent))))))))))
    nil))

(defun lisp-indent-tagbody (line path frame sexp-column normal-indent &optional (body 3))
  "Indent a tagbody's tags by 1 and its forms by BODY (3)."
  (declare (ignore frame))
  (if (cdr path)
      normal-indent
      (let ((text (lisp-line-text line)))
        (list (if (and (plusp (length text)) (lisp-symbol-octet-p (char-code (char text 0)))
                       (not (member (char text 0) '(#\# #\|))))
                  (+ sexp-column 1)
                  (+ sexp-column body))
              (lisp-frame-open (first (lisp-line-frames line)))))))

(defun lisp-indent-do (line path frame sexp-column normal-indent)
  "Indent do and do*: the variables by default, the end test's forms one column
in from its bracket, and the body as a tagbody's, its tags by 1 and its forms
by 2."
  (if (>= (car path) 3)
      (lisp-indent-tagbody line path frame sexp-column normal-indent *lisp-body-indent*)
      (lisp-indent-259 line '((&whole nil &rest) (&whole nil &rest 1)) path frame sexp-column normal-indent)))

(defun lisp-indent-defmethod (line path frame sexp-column normal-indent)
  "Indent defmethod as defun, each qualifier between the name and the lambda
list indented as the name is. The qualifiers are counted in the top-level
form, which is the method's own form unless the method is inside another."
  (let* ((top (car (last (lisp-line-frames line))))
         (qualifiers (if (>= (car path) 3)
                         (loop for k from 2 below (lisp-frame-count top)
                               while (let ((b (lisp-octet line (element-start top k))))
                                       (and (lisp-symbol-octet-p b) (not (member b '(35 92 124)))))
                               count t)
                         0)))
    (lisp-indent-259 line (if (plusp qualifiers)
                              (append '(4) (make-list qualifiers :initial-element 4) '(&lambda &body))
                              *lisp-defun-method*)
                     path frame sexp-column normal-indent)))

(defun lisp-indent-function-lambda-hack (line path frame sexp-column normal-indent)
  "Indent a lambda's body: in (function (lambda ...)), under function rather
than lambda."
  (if (or (cdr path) (> (car path) 3))
      normal-indent
      (let ((outer (second (member frame (lisp-line-frames line)))))
        (if (and outer (cl-ppcre:scan "^(?i)(lisp:+)?function(?s:.)"
                                       (lisp-text line (1+ (lisp-frame-open outer)) (+ (lisp-frame-open outer) 20))))
            (+ *lisp-body-indent* -1 (lisp-column line (1+ (lisp-frame-open outer))))
            (+ sexp-column *lisp-body-indent*)))))

(defun lisp-method-of (name)
  "The indentation method of the form NAME, in lower case: its own, or that of
the name after its package prefix; NIL when none."
  (multiple-value-bind (method found) (gethash name *lisp-indent-methods*)
    (cond (found (if (eq method 'defun) *lisp-defun-method* method))
          ((cl-ppcre:scan ":[^:]+" name)
           (let ((short (subseq name (1+ (position #\: name)))))
             (setf short (string-left-trim ":" short))
             (let ((method (gethash short *lisp-indent-methods*)))
               (if (eq method 'defun) *lisp-defun-method* method)))))))

(defun lisp-form-name (line frame)
  "The name of the form FRAME is, in lower case, or NIL when its first
expression is no symbol."
  (when (plusp (lisp-frame-count frame))
    (let* ((start (expression-proper line (element-start frame 0)))
           (b (lisp-octet line start))
           (size (tuatara.text:rope-bytes (lisp-line-rope line))))
      (when (and (lisp-symbol-octet-p b) (not (member b '(35 92 124))))
        (let ((end (loop with i = start
                         while (and (< i size) (lisp-symbol-octet-p (lisp-octet line i)))
                         do (if (= (lisp-octet line i) 92) (incf i 2) (incf i))
                         finally (return (min i size)))))
          (string-downcase (lisp-text line start end)))))))

(defun extended-loop-p (line frame)
  "True when the loop form in FRAME is an extended loop: its second expression,
before the line or after, is a keyword or a word, not a list."
  (let ((count (lisp-frame-count frame)))
    (if (>= count 2)
        (let ((b (lisp-octet line (element-start frame 1))))
          (or (= b 58) (alphanumericp (code-char b))))
        ;; the second expression comes at or after the line: look in the text
        (let* ((rope (lisp-line-rope line))
               (size (tuatara.text:rope-bytes rope))
               (p (if (= count 1) (element-end frame 0) (1+ (lisp-frame-open frame)))))
          (loop
            (when (>= p size) (return t))
            (let ((b (tuatara.text:rope-octet rope p)))
              (cond ((member b '(32 9 10 12 13)) (incf p))
                    ((= b 59) (setf p (1+ (tuatara.text:rope-line-end rope p))))
                    ((= b 41) (return t))
                    (t (return (or (= b 58) (alphanumericp (code-char b))))))))))))

(defun lisp-loop-indent (line frame)
  "The column in a loop form: in an extended loop, of clauses such as for and
collect, six columns in from its bracket, under the first clause; in a simple
loop, of forms only, one column in."
  (let ((column (lisp-column line (lisp-frame-open frame))))
    (list (if (extended-loop-p line frame) (+ column 6) (+ column 1))
          column)))

(defun common-lisp-indent (line normal-indent)
  "The column for LINE by the forms around it, each by its indentation method
(*LISP-INDENT-METHODS*), from the innermost out to the third; NIL when none of
them says. The elements of a quoted list or a vector go one column in from its
bracket. The innermost form, with no method of its own, is indented as defun
when its name begins with def, and as one argument then a body when it begins
with with-, without- or do-."
  (let* ((frames (lisp-line-frames line))
         (innermost (first frames)))
    (when (cl-ppcre:scan "^\\((?i)loop" (lisp-text line (lisp-frame-open innermost) (+ (lisp-frame-open innermost) 5)))
      (return-from common-lisp-indent (lisp-loop-indent line innermost)))
    (let ((depth 0) (path '()) (calculated nil) (tentative nil)
          ;; the innermost list's column, whichever form's method decides
          (sexp-column (lisp-column line (lisp-frame-open innermost))))
      (loop for (frame . outer) on frames
            while (and (not calculated) (< depth 3))
            do (let* ((open (lisp-frame-open frame))
                      (name (let ((name (lisp-form-name line frame)))
                              ;; a name with no method of its own is taken without its package
                              (if (and name (not (nth-value 1 (gethash name *lisp-indent-methods*)))
                                       (position #\: name))
                                  (string-left-trim ":" (subseq name (position #\: name)))
                                  name)))
                      (method (and name (lisp-method-of name)))
                      (tentative-defun nil)
                      (before (lisp-octet line (1- open)))
                      (before2 (lisp-octet line (- open 2))))
                 (push (lisp-frame-count frame) path)
                 (when (and name (null method) (null (cdr path)))
                   (cond ((cl-ppcre:scan "^def" name) (setf tentative-defun t))
                         ((cl-ppcre:scan "^(with|without|do)-" name) (setf method '(&lambda &body)))))
                 (cond ((and (= before 39) (not (= before2 35)))          ; '(
                        (setf calculated (1+ sexp-column)))
                       ((progn
                          ;; ,( and ,@(: its own method still counts, but nothing further out
                          (when (or (= before 44) (and (= before 64) (= before2 44)))
                            (setf tentative normal-indent depth 3))
                          nil))
                       ((= before 35)                                      ; #(
                        (setf calculated (1+ sexp-column)))
                       ((null method)
                        (when tentative-defun
                          (setf tentative (lisp-indent-call-method line *lisp-defun-method* path frame sexp-column normal-indent)
                                normal-indent tentative)))
                       ((integerp method)
                        (setf calculated (cond ((cdr path) normal-indent)
                                               ((<= (car path) method) (list (+ sexp-column 4) (lisp-frame-open innermost)))
                                               ((= (car path) (1+ method)) (+ sexp-column *lisp-body-indent*))
                                               (t normal-indent))))
                       (t (setf calculated (lisp-indent-call-method line method path frame sexp-column normal-indent))))
                 (unless calculated
                   (if outer (incf depth) (setf depth 3)))))
      (or calculated tentative))))

(defun stops-looking-back-p (line position frame)
  "True when the expression at POSITION of LINE's text is where looking back
for a keyword to line up under stops: first on its line, or just after FRAME's
bracket, blanks between or not."
  (or (<= position (1+ (lisp-frame-open frame)))
      (let ((p position))
        (loop while (and (> p 0) (member (lisp-octet line (1- p)) '(32 9))) do (decf p))
        (or (zerop p)
            (= (lisp-octet line (1- p)) 10)
            (and (< p position) (= (lisp-octet line (1- p)) 40))))))

(defun keyword-alignment (line frame)
  "The column of a keyword a line starting with a keyword lines up under:
going back from the last expression in FRAME to the first on its line, that
one or the one before it on that line, when it is a keyword."
  (let ((text (lisp-line-text line))
        (k (1- (lisp-frame-count frame))))
    (when (and (plusp (length text)) (char= (char text 0) #\:) (>= k 0))
      (loop until (or (zerop k) (stops-looking-back-p line (element-start frame k) frame))
            do (decf k))
      (let ((start (element-start frame k))
            (before (and (plusp k) (element-start frame (1- k)))))
        (when (> start (expression-proper line (element-start frame 0)))
          (cond ((= (lisp-octet line start) 58) (lisp-column line start))
                ((and before (same-line-p line before start) (= (lisp-octet line before) 58))
                 (lisp-column line before))))))))

(defun lisp-calculate-indent (line)
  "Where LINE lines up, by the forms around it, else under a keyword, else by
default: a column, or a list (COLUMN START) when the lines after it at the same
depth need not line up with it, START being where the list around it opens."
  (let ((frames (lisp-line-frames line)))
    (if (null frames)
        0
        (multiple-value-bind (normal last) (lisp-normal-indent line (first frames))
          (if last
              (or (common-lisp-indent line normal)
                  (keyword-alignment line (first frames))
                  normal)
              normal)))))

(defun lisp-line-column (line indent)
  "The column LINE goes to when it lines up at INDENT; NIL to leave it: a line
beginning with ;;; stays as it is, and one holding a comment begun with a
single semicolon goes to *LISP-COMMENT-COLUMN*."
  (let ((text (lisp-line-text line)))
    (cond ((cl-ppcre:scan "^;;;" text) nil)
          ((cl-ppcre:scan "^;(?!;)" text) *lisp-comment-column*)
          ((consp indent) (car indent))
          (t indent))))

;;; Indenting lines

(defparameter *lisp-indent-reach* (* 1024 1024)
  "The most octets before a line that are read to indent it as Lisp: a
top-level form that starts further back (a large quoted list of data) is read
from the first line within reach, as if it started there.")

(defun top-level-start (rope position syntax)
  "Where the top-level form holding the line starting at POSITION of ROPE
starts: by SYNTAX's tree when there is one and that form reads without error,
so a bracket in a string is never taken for a form's start; else the nearest
line before starting with a bracket. Never more than *LISP-INDENT-REACH*
octets back: then the start of the first line within it."
  (let* ((chain (and syntax (tuatara.syntax:node-ancestry syntax position position)))
         (top (and (rest chain) (car (last chain 2))))
         (start (if (and top
                         (not (tuatara.syntax:tree-node-broken top))
                         (string/= (tuatara.syntax:tree-node-name (car (last chain))) "ERROR"))
                    (min position (tuatara.syntax:tree-node-start top))
                    (if (plusp position) (lisp-text-start rope (1- position) *lisp-indent-reach*) 0))))
    (cond ((and start (<= (- position start) *lisp-indent-reach*)) start)
          ((<= position *lisp-indent-reach*) 0)
          (t (let ((newline (tuatara.text:rope-find-line-end rope :start (- position *lisp-indent-reach*) :end position)))
               (if newline (1+ newline) position))))))

(defparameter *lisp-line-head* 256
  "The most octets of a line's text, past the blanks it begins with, that
indenting it as Lisp looks at (LISP-LINE-TEXT): enough for the words a line
begins with that change where it goes.")

(defun lisp-line-head (rope line line-end)
  "The text of the line of ROPE from LINE below LINE-END past the blanks it
begins with, no more than *LISP-LINE-HEAD* octets of it."
  (let ((text (tuatara.text:rope-skip-octets rope line tuatara.text:*blank-line-octets* :bound line-end)))
    (tuatara.text:octets-string (tuatara.text:rope-octets rope :start text :end (min line-end (+ text *lisp-line-head*))))))

(defun lisp-indent-lines (rope start end &optional syntax blank-lines)
  "How the lines of ROPE from the one holding octet START to the one holding
END are indented as Lisp: a list of (LINE-START . COLUMN), in order, for the
lines whose indentation changes; blank lines are left alone. The lines are
read in one pass: each as if those before it were already indented, and where
a line lines up remembered for the lines after it at the same depth. SYNTAX,
when given, is the text's syntax tree, for finding where the first line's
top-level form starts; BLANK-LINES true indents blank lines too, as a line
just begun is."
  (let* ((first (tuatara.text:rope-line-start rope start))
         (stop (tuatara.text:rope-line-start rope end))
         (scan (make-lisp-scan rope (top-level-start rope first syntax)))
         (moved (make-hash-table))
         (remembered '())
         (depth 0)
         (changes '()))
    (loop for line = first then (1+ line-end)
          for line-end = (tuatara.text:rope-line-end rope line)
          for first-line = t then nil
          do (advance-scan scan line)
             (let* ((frames (lisp-scan-stack scan))
                    (here (length frames))
                    (in-string (eq (lisp-scan-inside scan) :string)))
               ;; what was worked out at each depth, the current one first
               (cond (first-line (setf remembered (make-list (1+ here)) depth here))
                     (in-string)
                     ((< here depth) (setf remembered (nthcdr (- depth here) remembered) depth here))
                     ((> here depth) (setf remembered (nconc (make-list (- here depth)) remembered) depth here)))
               (unless in-string
                 (let* ((this (make-lisp-line rope line frames moved (lisp-line-head rope line line-end)))
                        (indent (cond (first-line (lisp-calculate-indent this))
                                      ((car remembered))
                                      (t (let ((indent (lisp-calculate-indent this)))
                                           (when (integerp indent) (setf (car remembered) indent))
                                           indent))))
                        (column (lisp-line-column this indent)))
                   (multiple-value-bind (width blank current) (rope-line-blanks rope line)
                     (when (and column (or blank-lines (not blank)) (/= column current))
                       (setf (gethash line moved) (cons column width))
                       (push (cons line column) changes))))))
          until (>= line stop))
    (nreverse changes)))

(defmethod mode-indent-lines ((mode lisp-mode) buffer start end &optional blank-lines)
  "How the lines of BUFFER from START to END are indented as Lisp (LISP-INDENT-
LINES)."
  (lisp-indent-lines (tuatara.buffer:buffer-text buffer) start end (ready-syntax buffer) blank-lines))
