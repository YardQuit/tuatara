;;;; lisp/modes/lisp.lisp --- Common Lisp: the mode, and evaluating in the running editor.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The editor is itself Lisp, so evaluating an expression here runs it in the
;;;; editor: this is how the editor is configured and extended while it runs.
;;;; Expressions are read in the TUATARA-USER package, as the init file is, or
;;;; the one IN-PACKAGE chose in that buffer. An evaluation runs on a thread of
;;;; its own that C-g stops; what it prints goes to *Messages*, never to the
;;;; terminal, and its errors, running out of stack included, are reported
;;;; rather than taking the editor down. C-x C-e, C-M-x, M-x eval-region and
;;;; eval-buffer here, and M-: anywhere.

(in-package #:tuatara.defaults)

(define-major-mode lisp-mode prog-mode
  "Common Lisp source. \\<lisp-mode>\\[eval-last-expression] evaluates the
expression before point in the running editor and shows the value; with
\\[universal-argument] it inserts the value instead. \\[eval-top-level-form]
evaluates the top-level form around point."
  :lighter "Lisp" :comment (";" ";;") :extensions ("lisp" "lsp" "cl" "asd" "ros") :file-names (".sbclrc")
  :interpreters ("sbcl"))

(define-major-mode lisp-interaction-mode lisp-mode
  "Lisp for *lisp-scratch*: Lisp mode, and
\\<lisp-interaction-mode>\\[eval-print-last-expression] evaluates the expression
before point and puts what it printed and its value on the lines after it."
  :lighter "Lisp Interaction")

;;; Lisp text: where expressions begin and end

(declaim (inline lisp-symbol-octet-p))
(defun lisp-symbol-octet-p (octet)
  "True when OCTET may be part of a symbol or number: not blank, a bracket, a
quote mark, a comma, a semicolon or a double quote."
  (case octet
    ((32 9 10 12 13 34 39 40 41 44 59 96) nil)
    (t t)))

(defun lisp-name-part-p (char)
  "True when CHAR can be part of a Lisp name as it is written
(LISP-SYMBOL-OCTET-P), and is not a bar: the name of |a b| is a b."
  (let ((code (char-code char)))
    (or (> code 255) (and (lisp-symbol-octet-p code) (/= code 124)))))

(defun lisp-elements (octets &key (elements t) (end (length octets)))
  "The complete expressions in OCTETS, Lisp text, as a list of (START END
DEPTH), in the order they end: atoms, strings and bracketed lists, START and
END offsets into OCTETS, a quote or other prefix before one counting as part of
it, DEPTH the brackets around it. Comments (; to the end of the line, #| |#,
nested) are skipped, and so is the character after #\\, as the reader skips
them, so that brackets there count for nothing. An unclosed list is no element,
though what is complete inside it is. The second value lists where the lists
still open at the end begin, the innermost first (at a quote or other prefix
before its bracket, when it has one). With ELEMENTS NIL, only that second
value is worked out, the first NIL: nothing is made for each element. Only
the octets below END are read."
  (declare (type (simple-array (unsigned-byte 8) (*)) octets))
  (let ((collect elements) (elements '()) (opens '()) (n end) (i 0) (prefix nil))
    (labels ((at (j) (if (< j n) (aref octets j) 0))
             (element (start end)
               (when collect (push (list (or prefix start) end (length opens)) elements))
               (setf prefix nil)))
      (loop while (< i n)
            do (let ((b (at i)))
                 (cond
                   ((member b '(32 9 10 12 13)) (incf i))
                   ((= b 59) (setf i (or (position 10 octets :start i :end n) n)))              ; ;
                   ((and (= b 35) (= (at (1+ i)) 124))                                   ; #| |#
                    (setf i (nested-comment-end #'at i n)))
                   ((= b 40) (push (or prefix i) opens) (setf prefix nil) (incf i))    ; (
                   ((= b 41) (when opens (let ((open (pop opens))) (when collect (push (list open (1+ i) (length opens)) elements)))) ; )
                    (setf prefix nil) (incf i))
                   ((= b 34)                                                             ; "..."
                    (let ((j (1+ i)))
                      (loop while (and (< j n) (/= (at j) 34))
                            do (when (= (at j) 92) (incf j))
                               (incf j))
                      (element i (min n (1+ j)))
                      (setf i (min n (1+ j)))))
                   ((member b '(39 96)) (unless prefix (setf prefix i)) (incf i))      ; ' `
                   ((= b 44) (unless prefix (setf prefix i)) (incf i)                   ; , ,@
                    (when (= (at i) 64) (incf i)))
                   ((and (= b 35) (member (at (1+ i)) '(39 43 45 46)))                 ; #' #+ #- #.
                    (unless prefix (setf prefix i)) (incf i 2))
                   ((and (= b 35) (= (at (1+ i)) 40))                                   ; #( a vector
                    (unless prefix (setf prefix i)) (incf i))
                   (t                                                                     ; an atom
                    (let ((j i))
                      (when (and (= b 35) (= (at (1+ j)) 92)) (incf j 3))               ; #\x: x is itself
                      (loop while (and (< j n) (lisp-symbol-octet-p (at j)))
                            do (case (at j)
                                 (92 (incf j 2))                                          ; \x
                                 (124 (let ((close (position 124 octets :start (min n (1+ j)) :end n)))   ; |...|
                                        (setf j (if close (1+ close) n))))
                                 (t (incf j))))
                      (element i (min j n))
                      (setf i (max (1+ i) (min j n)))))))))
    (values (nreverse elements) opens)))

(defun lisp-text-start (rope position &optional reach block)
  "Where to start reading ROPE to find the expressions around POSITION: the
start of the nearest line at or before it that begins with an open bracket,
taken to be where a top-level form begins (LINE-OPENER-BEFORE, reading into
BLOCK when given); else the text's start. With REACH, looked for no further
than REACH octets back: NIL when it is not found there."
  (or (line-opener-before rope (1+ (min position (1- (tuatara.text:rope-bytes rope)))) :openers '(40)
                                                                                    :reach (or reach most-positive-fixnum)
                                                                                    :block block)
      (and (null reach) 0)))

(defun expression-before (buffer position)
  "Two values: the start and end of the Lisp expression that ends last at or
before POSITION in BUFFER, a quote before it included; NIL when there is none.
Strings, comments and #\( are told apart."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (start (lisp-text-start rope position))
         (best nil))
    (dolist (e (lisp-elements (tuatara.text:rope-octets rope :start start :end position)))
      (when (or (null best) (> (second e) (second best))) (setf best e)))
    (when best (values (+ start (first best)) (+ start (second best))))))

(defun bracket-form-bounds (rope start)
  "Two values: START and the end of the Lisp form of ROPE opening with the
bracket at START, looked for in the 4 MB after it; NIL when no open bracket is
at START, or the form is not closed there."
  (when (and (< start (tuatara.text:rope-bytes rope)) (= (tuatara.text:rope-octet rope start) 40))
    (let* ((end (min (tuatara.text:rope-bytes rope) (+ start (* 4 1024 1024))))
           (form (find-if (lambda (e) (and (zerop (first e)) (zerop (third e))))
                          (lisp-elements (tuatara.text:rope-octets rope :start start :end end)))))
      (when form (values start (+ start (second form)))))))

(defun top-level-form-bounds (buffer position)
  "Two values: the start and end of the top-level form around POSITION in
BUFFER, the form that begins with an open bracket at the start of a line; NIL
when there is none, or it is not closed."
  (let ((rope (tuatara.buffer:buffer-text buffer)))
    (bracket-form-bounds rope (lisp-text-start rope position))))

;;; Symbols: what a name in Lisp text stands for

(defun lisp-symbol-named (name package)
  "The symbol NAME, written as Lisp text writes one (name, package:name,
package::name or :keyword), stands for read in PACKAGE, or NIL when there is
none: nothing is made, as reading would."
  (let ((colon (position #\: name)))
    (flet ((find-it (string in) (and in (find-symbol (string-upcase string) in))))
      (cond ((null colon) (find-it name package))
            ((zerop colon) (find-it (string-left-trim ":" name) :keyword))
            (t (find-it (string-left-trim ":" (subseq name colon))
                        (find-package (string-upcase (subseq name 0 colon)))))))))

(defun lisp-signature (symbol)
  "SYMBOL's name and parameters as Lisp text, (name parameter ...) in lower
case, or NIL when it names no function, macro or special operator whose
parameters are known."
  (when (and symbol (fboundp symbol))
    (multiple-value-bind (parameters known) (ignore-errors (values (sb-introspect:function-lambda-list symbol) t))
      (when known
        (labels ((show (x)
                   ;; a parameter by its name, whatever package it was written in
                   (cond ((keywordp x) (format nil ":~(~a~)" (symbol-name x)))
                         ((symbolp x) (string-downcase (symbol-name x)))
                         ((consp x)
                          (let ((parts '()) (tail x))
                            (loop while (consp tail) do (push (show (pop tail)) parts))
                            (format nil "(~{~a~^ ~}~@[ . ~a~])" (nreverse parts) (and tail (show tail)))))
                         (t (let ((*print-pretty* nil)) (prin1-to-string x))))))
          (show (cons symbol parameters)))))))

(defun symbol-at-point (buffer)
  "The name, as written, of the Lisp symbol point is in or just after in BUFFER,
package prefix and all; NIL when there is none."
  (multiple-value-bind (start end) (word-bounds-at buffer (tuatara.buffer:buffer-point buffer) #'lisp-name-part-p)
    (when (< start end)
      (tuatara.buffer:buffer-string buffer :start start :end end))))

(defvar *lisp-operator-reach* (* 64 1024)
  "Octets back from point that the start of the form around it is looked for
when the parameters of the call point is in are shown.")

(defvar *operator-octets* (make-array 0 :element-type '(unsigned-byte 8))
  "The octets OPERATOR-AT last read, kept to read the next form into.")

(defvar *operator-block* (make-array 65537 :element-type '(unsigned-byte 8))
  "The octets OPERATOR-AT looks back through for the start of the form, kept
to read into each time.")

(defun operator-at (buffer position)
  "The name of the operator of the innermost list around POSITION in BUFFER,
Lisp text, as written there; NIL when POSITION is in no list, or the list is
quoted, or starts with no name, or the form around it starts more than
*LISP-OPERATOR-REACH* octets back. Only that form is read, into the octets
read last time (*OPERATOR-OCTETS*, *OPERATOR-BLOCK*), so that nothing is made
for it. On the editor's thread only."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (start (lisp-text-start rope position *lisp-operator-reach* *operator-block*)))
    (when start
      (let ((n (- position start)) (octets *operator-octets*))
        (when (< (length octets) n)
          (setf octets (make-array (max n 4096) :element-type '(unsigned-byte 8))
                *operator-octets* octets))
        (tuatara.text:rope-copy-into rope start position octets)
        (let ((open (first (nth-value 1 (lisp-elements octets :elements nil :end n)))))
          (when (and open (= (aref octets open) 40))
            (let* ((from (position-if-not (lambda (b) (member b '(32 9 10 12 13))) octets :start (1+ open) :end n))
                   (to (and from (position-if (lambda (b) (member b '(32 9 10 12 13 40 41 34 59 39 96))) octets :start from :end n))))
              (when (and from (not (member (aref octets from) '(40 41 34 59 39 96 35 44))))
                (tuatara.text:octets-string octets :start from :end (or to n))))))))))

(defun lisp-eldoc (editor)
  "Post-command function: in a Lisp buffer, when the echo area has nothing else
to say, show the parameters of the function or macro whose call point is in."
  (let ((buffer (tuatara.display:window-buffer (editor-window editor))))
    (when (and (null (editor-message editor))
               (typep (buffer-major-mode buffer) 'lisp-mode)
               (tuatara.buffer:buffer-indexed-p buffer))
      ;; the operator read again only once the text or point changed
      (let* ((version (tuatara.buffer:buffer-version buffer))
             (point (tuatara.buffer:buffer-point buffer))
             (seen (tuatara.buffer:buffer-local buffer 'lisp-eldoc-seen))
             (name (if (and seen (eq (first seen) version) (eql (second seen) point))
                       (third seen)
                       (let ((name (ignore-errors (operator-at buffer point))))
                         (setf (tuatara.buffer:buffer-local buffer 'lisp-eldoc-seen) (list version point name))
                         name)))
             (signature (and name (lisp-signature (lisp-symbol-named name (lisp-package buffer))))))
        (when signature
          (setf (editor-message editor) signature))))))

;;; Evaluating in the running editor

(defvar *evaluation-grace* 1/10
  "Seconds an evaluation runs before the echo area says so and \\`C-g' can stop it.")

(defun lisp-package (buffer)
  "The package BUFFER's Lisp is read and evaluated in: the one IN-PACKAGE last
chose there, else TUATARA-USER, the init file's."
  (or (let ((name (tuatara.buffer:buffer-local buffer :lisp-package))) (and name (find-package name)))
      (find-package :tuatara-user)))

(defparameter *definition-head-names*
  '("DEFUN" "DEFMACRO" "DEFVAR" "DEFPARAMETER" "DEFCONSTANT" "DEFGENERIC" "DEFMETHOD"
    "DEFCLASS" "DEFSTRUCT" "DEFTYPE" "DEFSETF")
  "The names of the forms that define a name, besides those beginning DEFINE-.")

(defun definition-head-p (head)
  "True when HEAD, a symbol's name, is that of a form that defines a name:
DEFUN, DEFVAR, DEFINE-COMMAND and the like (*DEFINITION-HEAD-NAMES*)."
  (or (member head *definition-head-names* :test #'string=)
      (and (> (length head) 7) (string= "DEFINE-" head :end2 7))))

(defvar *evaluated-definitions* (make-hash-table :test 'eq :synchronized t)
  "For each name a definition of was evaluated from a buffer
(\\<lisp-mode>\\[eval-top-level-form], \\[eval-last-expression], eval-buffer), the
buffers it was evaluated from.")

(defvar *evaluating-buffer* nil
  "While an evaluation runs, the buffer its text comes from, or NIL for text
typed at \\[eval-expression].")

(defun definition-form-name (form)
  "The name FORM defines when it is a definition, (def... name ...) or
(define-... name ...): the symbol, (setf name)'s too, or a quoted one
\(define-recorded-keys 'name ...); else NIL."
  (when (and (consp form) (symbolp (first form)) (consp (rest form)))
    (let ((head (symbol-name (first form)))
          (name (second form)))
      (when (definition-head-p head)
        (cond ((and name (symbolp name) (not (keywordp name))) name)
              ((and (consp name) (member (first name) '(setf quote)) (symbolp (second name))) (second name)))))))

(defvar *init-file-definitions* (make-hash-table :test 'eq :synchronized t)
  "For each name a function, command or recorded keys were defined for in the
init file, (FILE LINE HEAD FUNCTION): where, the name of the defining form
\(define-command), and the function it was given then.")

(defun note-init-file-definition (form file line)
  "Remember that FORM, read from the init file FILE at LINE, defined the
function its name now has, when it is a definition of one."
  (let ((name (and (not (and (consp (second form)) (eq (first (second form)) 'setf)))
                   (definition-form-name form))))
    (when (and name (fboundp name))
      (setf (gethash name *init-file-definitions*) (list file line (first form) (fdefinition name))))))

(defun init-file-definition (symbol)
  "Where the init file defined SYMBOL's function, as (FILE LINE HEAD), HEAD
the defining form's name, while it is still the one the init file gave it; NIL
otherwise."
  (destructuring-bind (&optional file line head function) (gethash symbol *init-file-definitions*)
    (and file (fboundp symbol) (eq function (fdefinition symbol)) (list file line head))))

(defun note-evaluated-definition (form)
  "Remember that FORM, when it is a definition, was evaluated from *EVALUATING-BUFFER*."
  (let ((name (and *evaluating-buffer* (definition-form-name form))))
    (when name
      (pushnew *evaluating-buffer* (gethash name *evaluated-definitions*)))))

(defun evaluate-forms (text package box)
  "Read and evaluate the forms of TEXT one after another, as a file's are:
each read in the package the one before left, starting in PACKAGE, so that
IN-PACKAGE carries on to the next. TEXT may instead be a list of forms already
read, which are evaluated as they are, nothing more read. The car of BOX
follows the package in force. Returns the values of the last form and how many
forms there were."
  (let ((*package* package) (*read-eval* t) (values '()) (count 0) (eof '#:eof))
    ;; a macro, not a function: the backtrace of an error ends at this function's own frame
    (macrolet ((evaluate (form)
                 `(let ((form ,form))
                    (setf values (multiple-value-list (eval form)))
                    (note-evaluated-definition form)
                    (incf count)
                    (setf (car box) *package*))))
      (if (listp text)
          (dolist (form text) (evaluate form))
          (with-input-from-string (in text)
            (loop for form = (handler-case (read in nil eof)
                               (end-of-file ()
                                 (error "The text ends inside a form: a bracket or a string is not closed.")))
                  until (eq form eof)
                  do (evaluate form)))))
    (values values count)))

(defun run-evaluation (text buffer &key typed current)
  "Evaluate the forms of TEXT (EVALUATE-FORMS), or the list of forms already
read that TEXT may be instead, for BUFFER on a thread of its
own, while the editor waits: \\`C-g' stops it, and keys typed meanwhile wait their
turn (WAIT-INTERRUPTIBLY). What it prints and warns is kept, and its reading
finds an empty input: the terminal is never written to or read from. Every
serious condition is caught, running out of stack included, as is a call to
the debugger. Returns a plist: :OUTCOME, a list (:VALUES values count), (:ERROR
condition) or :STOPPED; :OUTPUT, the text printed; :WARNINGS, how many;
:PACKAGE, the package in force at the end; and :BACKTRACE, when
*DEBUG-ON-ERROR* is on and it met an error, the calls under way then
(CAPTURE-BACKTRACE). Unless TYPED (at \\[eval-expression]), TEXT is BUFFER's,
and the definitions in it are remembered as BUFFER's, for \\[xref-find-definitions].
The Lisp evaluated finds CURRENT the current buffer, when given, as IELM's
working buffer; else the one shown in the selected window."
  (let* ((editor *editor*)
         (box (list (lisp-package buffer)))
         (result nil)
         (thread
           (sb-thread:make-thread
            (lambda ()
              (let* ((*editor* editor)
                     (*current-buffer* current) ; SET-BUFFER lasts until the evaluation ends
                     (*evaluating-buffer* (unless typed buffer))
                     (output (make-string-output-stream))
                     (input (make-string-input-stream ""))
                     (io (make-two-way-stream input output))
                     (*standard-output* output) (*error-output* output) (*trace-output* output)
                     (*standard-input* input) (*terminal-io* io) (*query-io* io) (*debug-io* io)
                     (warnings 0)
                     (trace nil)
                     (outcome
                       (catch 'stop-evaluation
                         (let ((sb-ext:*invoke-debugger-hook*
                                 (lambda (condition hook) (declare (ignore hook))
                                   (when (or *debug-on-error* tuatara.runtime:*debug-on*)
                                     (setf trace (capture-backtrace :stop 'evaluate-forms)))
                                   (throw 'stop-evaluation (list :error condition))))
                               (*debugger-hook*
                                 (lambda (condition hook) (declare (ignore hook))
                                   (when (or *debug-on-error* tuatara.runtime:*debug-on*)
                                     (setf trace (capture-backtrace :stop 'evaluate-forms)))
                                   (throw 'stop-evaluation (list :error condition)))))
                           (handler-case
                               (handler-bind ((warning (lambda (w) (declare (ignore w)) (incf warnings)))
                                              (error (lambda (e) (declare (ignore e))
                                                       ;; the calls under way, before they are unwound
                                                       (when (or *debug-on-error* tuatara.runtime:*debug-on*)
                                                         (setf trace (capture-backtrace :stop 'evaluate-forms))))))
                                 (multiple-value-bind (values count) (evaluate-forms text (car box) box)
                                   (list :values values count)))
                             ;; the empty input run dry: say what that means here
                             (end-of-file (e)
                               (list :error (if (member (stream-error-stream e) (list input io))
                                                (make-condition 'simple-error :format-control
                                                                "end of file: code evaluated in the editor has no input to read")
                                                e)))
                             (serious-condition (e) (list :error e)))))))
                (when (and (consp outcome) (eq (first outcome) :error))
                  (tuatara.runtime:debug-report :error "evaluating Lisp: ~a~{~%    ~a~}" (second outcome) trace))
                (setf result (list :outcome outcome :output (get-output-stream-string output)
                                   :warnings warnings :package (car box)
                                   :backtrace (and (consp outcome) (eq (first outcome) :error) trace)))))
            :name "tuatara evaluation")))
    (unless (wait-interruptibly thread :message "Evaluating..." :grace *evaluation-grace*
                                       :stop (lambda (thread)
                                               (ignore-errors
                                                (sb-thread:interrupt-thread
                                                 thread (lambda () (throw 'stop-evaluation :stopped))))))
      (unless result
        (setf result (list :outcome :stopped :output "" :warnings 0 :package (car box)))))
    result))

(defun printed-value (value)
  "VALUE printed readably, or #<a value that cannot be printed: WHY> when
printing it fails."
  (handler-case (prin1-to-string value)
    (error (e) (format nil "#<a value that cannot be printed: ~a>" (tuatara.runtime:condition-words e)))))

(defun write-output-and-values (out output values)
  "Write to OUT what an evaluation printed, OUTPUT, the blanks at its end
dropped and a newline after it when it has none, then each of VALUES
readably on a line of its own (PRINTED-VALUE), long lists and deep nesting
cut short."
  (let ((printed (string-right-trim '(#\Space #\Tab) output))
        (*print-circle* t) (*print-length* 200) (*print-level* 20))
    (when (plusp (length printed))
      (write-string printed out)
      (unless (char= (char printed (1- (length printed))) #\Newline) (terpri out)))
    (dolist (value values)
      (write-line (printed-value value) out))))

(defun format-values (values)
  "The printed VALUES, readably, separated by a semicolon, cut to fit one echo
row; ; No values for none."
  (if (null values)
      "; No values"
      (let ((*print-length* 40) (*print-level* 6) (*print-circle* t))
        (tuatara.display:shorten-string (format nil "~{~a~^ ; ~}" (mapcar #'printed-value values)) 200 :one-line t))))

(defun expression-summary (text)
  "TEXT, the expressions evaluated, on one line for saying what gave a value:
its blanks and line ends made single spaces, cut to 60 columns with …."
  (tuatara.display:shorten-string (cl-ppcre:regex-replace-all "\\s+" text " ") 60 :one-line t))

(defun integer-annotation (value)
  "For an integer VALUE, what the echo area shows after it when it is the one
value of one expression: its octal and hex, and the character it is the code of
up to 127, written as Lisp reads them: \" (#o101, #x41, #\\A)\"; NIL for
anything else."
  (when (integerp value)
    (format nil " (#o~o, #x~(~x~)~@[, ~s~])" value value (and (<= 0 value 127) (code-char value)))))

(defun evaluate-and-report (buffer text &key insert-at (space t) print-at typed)
  "Evaluate the forms of TEXT for BUFFER (RUN-EVALUATION) and say how it went:
the values in the echo area, or with INSERT-AT inserted there (after a space
when SPACE); an error or a stop instead. What was printed goes to *Messages*,
line by line, and the echo area says so and how many warnings there were. With
PRINT-AT, what was printed and the values go on the lines after that place
instead (WRITE-OUTPUT-AND-VALUES). The package in force at the end becomes BUFFER's.
TYPED says TEXT was typed at \\[eval-expression], not taken from BUFFER. The
value is said after what was evaluated, on one line: (setf x 1) => 1."
  (let* ((result (run-evaluation text buffer :typed typed))
         (outcome (getf result :outcome))
         (printing (and print-at (consp outcome) (eq (first outcome) :values)))
         (lines (tuatara.text:split-words (getf result :output) '(#\Newline)))
         (warnings (getf result :warnings))
         (note (let ((parts (remove nil (list (and (plusp warnings) (format nil "~d warning~:p" warnings))
                                              (and lines (not printing) (format nil "printed ~d line~:p" (length lines)))))))
                 ;; an argument to MESSAGE, which writes in the keys of its control string only
                 (and parts (format nil (substitute-command-keys "  (~{~a~^, ~}: \\[view-echo-area-messages])"
                                                                 :marks t :for-format t)
                                    parts)))))
    (unless printing
      (dolist (line lines) (log-message line)))
    (setf (tuatara.buffer:buffer-local buffer :lisp-package) (package-name (getf result :package)))
    (cond ((eq outcome :stopped) (message "Evaluation stopped~@[~a~]" note))
          ((eq (first outcome) :error)
           (message "Error: ~a~@[~a~]" (tuatara.runtime:condition-words (second outcome)) note)
           (when (and (getf result :backtrace) *backtrace-function*)
             (funcall *backtrace-function* (second outcome) (getf result :backtrace))))
          (t (destructuring-bind (values count) (rest outcome)
               (let ((*package* (getf result :package)))
                 (cond (printing
                        (goto (tuatara.buffer:buffer-insert buffer print-at (with-output-to-string (out)
                                                                         (terpri out)
                                                                         (write-output-and-values out (getf result :output) values))))
                        (when note (message "~a" (string-left-trim " " note))))
                       (insert-at
                        (goto (tuatara.buffer:buffer-insert buffer insert-at
                                                            (format nil "~:[~; ~]~a" space (format-values values)))))
                       (t (message "~@[~a ~]=> ~a~@[~a~]~@[  (~:d forms)~]~@[~a~]"
                                   ;; what was evaluated, unless it reads as its value (65, "x")
                                   (let ((summary (expression-summary text)))
                                     (and (string/= summary (format-values values)) summary))
                                   (format-values values)
                                   ;; one expression's one value, an integer: its octal, hex and character
                                   (and (= (length values) 1) (<= count 1) (integer-annotation (first values)))
                                   (and (> count 1) count) note)))))))))

(define-command eval-print-last-expression ()
  "Evaluate the expression before point in the running editor and put what it
printed and its value on the lines after it. \\`C-g' stops an evaluation that
runs on."
  (let ((b (current-buffer)) (end (point)))
    (multiple-value-bind (start stop) (expression-before b end)
      (if (null start)
          (message "No expression before point")
          (evaluate-and-report b (tuatara.buffer:buffer-string b :start start :end stop) :print-at end)))))

(define-command eval-last-expression ()
  "Evaluate the expression before point in the running editor and show its
value in the echo area; with a prefix argument (\\[universal-argument]), insert
the value after point. \\`C-g' stops an evaluation that runs on; what it prints
goes to *Messages*."
  (let ((b (current-buffer)) (end (point)))
    (multiple-value-bind (start stop) (expression-before b end)
      (if (null start)
          (message "No expression before point")
          (evaluate-and-report b (tuatara.buffer:buffer-string b :start start :end stop)
                               :insert-at (and (prefix-argument) end))))))

(defun defvar-again (buffer text)
  "TEXT, a form of BUFFER, as \\[eval-top-level-form] evaluates it: a DEFVAR
with a value as a DEFPARAMETER, so that the variable takes the value again
when it has one already, as a list of that form; else TEXT itself."
  (let ((form (ignore-errors (let ((*package* (lisp-package buffer)) (*read-eval* nil))
                               (read-from-string text)))))
    (if (and (consp form) (eq (first form) 'defvar) (consp (cdr form)) (consp (cddr form)))
        (list (list* 'defparameter (rest form)))
        text)))

(define-command eval-top-level-form ()
  "Evaluate the top-level form around point (the one starting with an open
bracket at the start of a line) in the running editor and show its value. A
DEFVAR there sets its variable to its value even when it has one already, so
that a changed value is taken; \\[eval-last-expression] after it evaluates it as
it is."
  (multiple-value-bind (start end) (top-level-form-bounds (current-buffer) (point))
    (if start
        (evaluate-and-report (current-buffer)
                             (defvar-again (current-buffer) (tuatara.buffer:buffer-string (current-buffer) :start start :end end)))
        (message "No top-level form around point"))))

(define-command eval-region ()
  "Evaluate the Lisp forms in the selection one after another, as a file's are
(IN-PACKAGE carries on to the next), and show the last one's value. Without a
selection nothing is evaluated, nor with one of more than *REWRITE-THRESHOLD*
octets."
  (let ((b (current-buffer)))
    (multiple-value-bind (start end) (and (tuatara.buffer:buffer-mark-active-p b) (tuatara.buffer:buffer-region b))
      (cond ((not (and start (< start end))) (message "No selection: eval-region was not run"))
            ((too-large-here-p start end "Evaluating"))
            (t (evaluate-and-report b (tuatara.buffer:buffer-string b :start start :end end)))))))

(define-command eval-buffer ()
  "Evaluate every Lisp form in the current buffer one after another, as a
file's are, and show the last one's value; not a buffer of more than
*REWRITE-THRESHOLD* octets."
  (let ((b (current-buffer)))
    (unless (too-large-here-p 0 (tuatara.buffer:buffer-size b) "Evaluating")
      (evaluate-and-report b (tuatara.buffer:buffer-string b)))))

(define-command eval-expression ()
  "Read a Lisp expression in the minibuffer and evaluate it in the running
editor, in the current buffer's package; show its value, or with a prefix
argument (\\[universal-argument]) insert it at point.
\\<minibuffer>\\[minibuffer-complete] or \\`C-M-i' completes the name at the cursor
from a list over the prompt."
  (let* ((b (current-buffer))
         (insert-at (and (prefix-argument) (point)))
         (package (lisp-package b))
         (text (read-string "Eval: " :complete (lambda () (lisp-complete-in-minibuffer package)))))
    (if (zerop (length (string-trim '(#\Space #\Tab) text)))
        (message "Nothing to evaluate")
        (evaluate-and-report b text :insert-at insert-at :space nil :typed t))))

(defparameter *buffer-evaluating-commands*
  '(eval-print-last-expression eval-last-expression eval-top-level-form eval-region eval-buffer
    pp-macroexpand-last-expression pp-macroexpand-all-last-expression)
  "The commands that evaluate a buffer's own text in the running editor, or
expand its macro calls there: they run in Lisp buffers only, and never in one
opened safely.")

(apply #'commands-for-mode 'lisp-mode "Lisp buffers" *buffer-evaluating-commands*)

(defun safe-evaluation-refusal (command buffer)
  "What to say instead of running COMMAND, one of *BUFFER-EVALUATING-COMMANDS*,
in BUFFER opened safely: its text is never run; NIL otherwise."
  (and (member command *buffer-evaluating-commands*)
       (tuatara.io:safe-visit-p buffer)
       (format nil "~(~a~) runs the text it evaluates, and ~a is opened safely: nothing was evaluated"
               command (tuatara.buffer:buffer-name buffer))))

(defun install-lisp-mode (editor)
  "Bind the evaluation keys in Lisp buffers, and \\`C-j' in
*lisp-scratch*'s mode; have EDITOR show the parameters of the call point is in
after every other command's work (LISP-ELDOC); refuse evaluating the text of
a buffer opened safely (SAFE-EVALUATION-REFUSAL)."
  (pushnew 'safe-evaluation-refusal *command-refusal-functions*)
  (unless (member 'lisp-eldoc (editor-post-command-functions editor))
    (setf (editor-post-command-functions editor)
          (append (editor-post-command-functions editor) (list 'lisp-eldoc))))
  (let ((map (mode-keymap 'lisp-mode)))
    (define-key map "C-x C-e" 'eval-last-expression)
    (define-key map "C-M-x" 'eval-top-level-form))
  (define-key (mode-keymap 'lisp-interaction-mode) "C-j" 'eval-print-last-expression))

(defmethod mode-fills-strings-p ((mode lisp-mode))
  "True: filling in a Lisp string fills its text, as a docstring's."
  t)
