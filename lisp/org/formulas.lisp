;;;; lisp/org/formulas.lisp --- Org's table formulas (#+TBLFM), worked out as Org's Calc works them out.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A table's #+TBLFM: line holds its formulas, :: between them: $3=$1+$2
;;;; for a column, @4$2=vsum(@2..@3) for a field, @2$2..@3$3=$1*10 for a
;;;; range of fields. C-c * works them all out; C-c C-c on the #+TBLFM: line
;;;; too; =$1*2 typed in a field (:=... for the field alone), then TAB, RET or
;;;; C-c C-c, makes and works out a formula, and C-c = asks for one. The
;;;; arithmetic is Emacs Calc's as Org sets it up: integers exact, decimal
;;;; numbers to twelve digits shown to eight (0.1+0.2 is 0.3, 1.5*2 is 3.),
;;;; * before /, % and \ (integer division). Formulas in Emacs Lisp ('(...)
;;;; or ;L) are not run: nothing in a file is ever run.

(in-package #:tuatara.defaults)

;;; Numbers as Calc has them

(define-record (org-calc-float (:constructor %make-org-calc-float (value)))
  "A decimal number with a point, as table formulas keep it: VALUE is exact (a
rational) and already rounded to the working digits."
  (value nil :documentation "The exact value of an ORG-CALC-FLOAT, a rational already rounded to the working digits."))

(defvar *org-calc-digits* 12
  "How many significant digits decimal numbers are worked out to in table formulas (;p20 changes it for a formula).")

(defun org-round-half-away (x)
  "X, a rational, rounded to an integer, halves away from zero."
  (if (minusp x) (- (floor (+ (- x) 1/2))) (floor (+ x 1/2))))

(defun org-decimal-exponent (a)
  "The power of ten of A's first digit, A a positive rational: L with 10^L <= A < 10^(L+1)."
  (let ((l (floor (* (- (integer-length (numerator a)) (integer-length (denominator a))) 0.30103d0))))
    (loop while (>= a (expt 10 (1+ l))) do (incf l))
    (loop while (< a (expt 10 l)) do (decf l))
    l))

(defun org-round-digits (r digits)
  "R, a rational, rounded to DIGITS significant digits, halves away from zero."
  (if (zerop r)
      0
      (let* ((a (abs r))
             (scale (expt 10 (- (1- digits) (org-decimal-exponent a)))))
        (* (signum r) (/ (org-round-half-away (* a scale)) scale)))))

(defun org-calc-float (r)
  "A decimal number of the rational R, rounded to *ORG-CALC-DIGITS*."
  (%make-org-calc-float (org-round-digits (rational r) *org-calc-digits*)))

(defun org-calc-value (x)
  "The exact value of X, an integer or a decimal number."
  (if (org-calc-float-p x) (org-calc-float-value x) x))

(defparameter *org-calc-largest* 1000
  "The most digits, the largest exponent, width or precision a formula's
number may have; past it the field shows #ERROR, at once, instead of a
number too large to work out.")

(defun org-calc-error ()
  "Signal that the field being worked out shows #ERROR (ORG-FORMULA-ERROR)."
  (error 'org-formula-error :text "#ERROR"))

(defparameter *org-formula-lisp-refused* "Formulas in Emacs Lisp are not run: Tuatara runs nothing from a file"
  "What a table says of a formula written in Emacs Lisp, which is never run.")

(defun org-calc-bounded (n)
  "N, an integer a formula gives a count of digits or an exponent with, when
it is within *ORG-CALC-LARGEST*; else an error for the field (#ERROR)."
  (if (> (abs n) *org-calc-largest*) (org-calc-error) n))

(defun org-calc-number-p (x)
  "True when X is a number of the formulas: an integer or a decimal number."
  (or (integerp x) (org-calc-float-p x)))

(defun org-calc-read-number (text)
  "The number TEXT (a field's text) is, as table formulas read it: 12 an integer, 1.5,
3. and 1e10 decimal numbers; NIL when it is none."
  (let ((text (string-trim '(#\Space #\Tab) text)))
    (multiple-value-bind (match groups)
        (cl-ppcre:scan-to-strings "^([-+]?)([0-9]*)(?:\\.([0-9]*))?(?:[eE]([-+]?[0-9]+))?$" text)
      (when (and match (or (plusp (length (aref groups 1))) (plusp (length (or (aref groups 2) "")))))
        (org-calc-bounded (+ (length (aref groups 1)) (length (or (aref groups 2) ""))))
        (let* ((sign (if (string= (aref groups 0) "-") -1 1))
               (whole (if (plusp (length (aref groups 1))) (parse-integer (aref groups 1)) 0))
               (fraction (or (aref groups 2) ""))
               (exponent (if (aref groups 3) (org-calc-bounded (parse-integer (aref groups 3))) 0))
               (value (* sign (+ whole (if (plusp (length fraction))
                                           (/ (parse-integer fraction) (expt 10 (length fraction)))
                                           0))
                         (expt 10 exponent))))
          (if (or (aref groups 2) (aref groups 3))
              (org-calc-float value)
              value))))))

(defun org-format-calc-float (r digits &key scientific)
  "The text table formulas show for the decimal number R (a rational) to DIGITS
significant digits: 0.33333333, 3., 2500., 1.2e-3, 1e12; with SCIENTIFIC
always with an exponent."
  (if (zerop r)
      "0."
      (let* ((a (abs r))
             (l (org-decimal-exponent a))
             (m (org-round-half-away (* a (expt 10 (- (1- digits) l))))))
        (when (>= m (expt 10 digits)) (setf m (/ m 10)) (incf l))
        (let* ((digits-text (string-right-trim "0" (princ-to-string m)))
               (d (length digits-text))
               (e (- l (1- d)))
               (sign (if (minusp r) "-" "")))
          (if (or scientific (>= l 12) (<= l -3))
              (format nil "~a~a~@[.~a~]e~d" sign (char digits-text 0) (and (> d 1) (subseq digits-text 1)) l)
              (cond ((>= e 0) (format nil "~a~a~a." sign digits-text (make-string e :initial-element #\0)))
                    ((plusp (+ d e))
                     (format nil "~a~a.~a" sign (subseq digits-text 0 (+ d e)) (subseq digits-text (+ d e))))
                    (t (format nil "~a0.~a~a" sign (make-string (- (+ d e)) :initial-element #\0) digits-text))))))))

(defun org-calc-text (x &optional mode)
  "The text a field shows for X, a value of a formula, MODE a
display mode of the formula's flags: (:normal N), (:fixed N) or (:scientific N)."
  (cond ((stringp x) x)
        ((integerp x) (princ-to-string x))
        ((org-calc-float-p x)
         (let ((r (org-calc-float-value x)))
           (case (first mode)
             (:normal (org-format-calc-float r (org-calc-bounded (second mode))))
             (:scientific (org-format-calc-float r (org-calc-bounded (second mode)) :scientific t))
             (:fixed (let* ((n (org-calc-bounded (second mode))) (scaled (org-round-half-away (* (abs r) (expt 10 n))))
                            (text (format nil "~v,'0d" (1+ n) scaled)))
                       (format nil "~:[~;-~]~a.~a" (and (minusp r) (plusp scaled))
                               (subseq text 0 (- (length text) n)) (subseq text (- (length text) n)))))
             (t (org-format-calc-float r 8)))))
        (t (org-calc-error))))

;;; Arithmetic

(define-condition org-formula-error (error)
  ((text :initarg :text :reader org-formula-error-text))
  (:report (lambda (c stream) (write-string (org-formula-error-text c) stream)))
  (:documentation "A formula that cannot be worked out: TEXT says why."))

(defun org-formula-fail (control &rest arguments)
  "Give up working out a formula, saying why (CONTROL and ARGUMENTS as FORMAT has them)."
  (error 'org-formula-error :text (apply #'format nil control arguments)))

(defun org-calc-check (x)
  "X when it is a number, else an error for the field (#ERROR)."
  (unless (org-calc-number-p x) (org-calc-error))
  x)

(defun org-calc-arith (operator a b)
  "A OPERATOR B, as table formulas work it out: integers kept exact, decimal numbers rounded."
  (let* ((a (org-calc-check a)) (b (org-calc-check b))
         (x (org-calc-value a)) (y (org-calc-value b))
         (exact (and (integerp a) (integerp b))))
    (flet ((result (r) (if (and exact (integerp r)) r (org-calc-float r))))
      (ecase operator
        (+ (result (+ x y)))
        (- (result (- x y)))
        (* (result (* x y)))
        (/ (when (zerop y) (org-calc-error)) (result (/ x y)))
        (% (when (zerop y) (org-calc-error)) (result (mod x y)))
        (\\ (when (zerop y) (org-calc-error)) (floor x y))
        (^ (when (and (integerp y) (/= 0 x) (/= 1 (abs x))
                      (> (* (abs y)
                            (max (integer-length (numerator (rational x))) (integer-length (denominator (rational x)))
                                 1))
                         (* 100 *org-calc-largest*)))
             (org-calc-error))
           (cond ((and (integerp y) exact (>= y 0)) (expt x y))
                 ((integerp y) (when (and (zerop x) (minusp y)) (org-calc-error))
                  (result (expt x y)))
                 ((minusp x) (org-calc-error))
                 (t (org-calc-float (expt (float x 1d0) (float y 1d0))))))))))

(defun org-calc-compare (operator a b)
  "1 when A OPERATOR B holds, else 0, OPERATOR one of == != < > <= >=."
  (let ((x (org-calc-value (org-calc-check a))) (y (org-calc-value (org-calc-check b))))
    (if (ecase operator (== (= x y)) (!= (/= x y)) (< (< x y)) (> (> x y)) (<= (<= x y)) (>= (>= x y))) 1 0)))

(defun org-calc-true-p (x)
  "True when X counts as true: a number not zero."
  (not (zerop (org-calc-value (org-calc-check x)))))

(defun org-calc-real (x)
  "X as a double float, for the functions worked out that way."
  (float (org-calc-value (org-calc-check x)) 1d0))

(defun org-calc-flatten (arguments)
  "The numbers of ARGUMENTS, ranges (lists) opened up."
  (loop for argument in arguments
        append (if (listp argument) (mapcar #'org-calc-check argument) (list (org-calc-check argument)))))

(defun org-calc-sum (numbers)
  "The sum of NUMBERS, exact while they are integers."
  (reduce (lambda (a b) (org-calc-arith '+ a b)) numbers :initial-value 0))

(defun org-calc-function (name arguments)
  "The table formulas' function NAME (vsum, vmean...) applied to ARGUMENTS (values; a range is a list of them)."
  (flet ((one () (unless (and (= 1 (length arguments)) (not (listp (first arguments))))
                   (org-formula-fail "~a takes one number" name))
              (first arguments))
         (all () (org-calc-flatten arguments))
         (real (r) (org-calc-float (rational r)))
         (degrees (x) (* (org-calc-real x) (/ pi 180))))
    (let ((name (string-downcase name)))
      (cond
        ((string= name "vsum") (org-calc-sum (all)))
        ((string= name "vprod") (reduce (lambda (a b) (org-calc-arith '* a b)) (all) :initial-value 1))
        ((string= name "vcount") (length (all)))
        ((string= name "vmean") (let ((numbers (all)))
                                  (if numbers
                                      (org-calc-arith '/ (org-calc-sum numbers) (length numbers))
                                      (org-calc-error))))
        ((string= name "vmedian")
         (let* ((numbers (sort (copy-list (all)) #'< :key #'org-calc-value)) (n (length numbers)))
           (cond ((zerop n) (org-calc-error))
                 ((oddp n) (nth (floor n 2) numbers))
                 (t (org-calc-arith '/ (org-calc-arith '+
                                                       (nth (1- (floor n 2)) numbers)
                                                       (nth (floor n 2) numbers)) 2)))))
        ((member name '("vmax" "max") :test #'string=)
         (let ((numbers (all)))
           (if numbers (reduce (lambda (a b) (if (> (org-calc-value b) (org-calc-value a)) b a)) numbers)
               (org-calc-error))))
        ((member name '("vmin" "min") :test #'string=)
         (let ((numbers (all)))
           (if numbers (reduce (lambda (a b) (if (< (org-calc-value b) (org-calc-value a)) b a)) numbers)
               (org-calc-error))))
        ((string= name "abs")
         (let ((x (org-calc-check (one)))) (if (integerp x) (abs x) (org-calc-float (abs (org-calc-value x))))))
        ((string= name "round")
         (if (= 2 (length arguments))
             (let ((x (org-calc-check (first arguments)))
                   (n (org-calc-bounded (truncate (org-calc-value (org-calc-check (second arguments)))))))
               (if (integerp x) x
                   (org-calc-float (/ (org-round-half-away (* (org-calc-value x) (expt 10 n))) (expt 10 n)))))
             (org-round-half-away (org-calc-value (org-calc-check (one))))))
        ((string= name "floor") (floor (org-calc-value (org-calc-check (one)))))
        ((string= name "ceil") (ceiling (org-calc-value (org-calc-check (one)))))
        ((string= name "sqrt") (let* ((x (org-calc-check (one))) (v (org-calc-value x)))
                                 (cond ((minusp v) (org-calc-error))
                                       ((and (integerp x) (= v (expt (isqrt v) 2))) (isqrt v))
                                       (t (real (sqrt (float v 1d0)))))))
        ((string= name "exp") (real (exp (org-calc-real (one)))))
        ((member name '("ln" "log10") :test #'string=)
         (let ((x (org-calc-real (one))))
           (when (<= x 0) (org-calc-error))
           (real (if (string= name "ln") (log x) (log x 10d0)))))
        ((string= name "sin") (real (sin (degrees (one)))))
        ((string= name "cos") (real (cos (degrees (one)))))
        ((string= name "tan") (real (tan (degrees (one)))))
        (t (org-formula-fail "~a is no function formulas know here" name))))))

;;; Formulas read

(defun org-formula-tokens (text)
  "The tokens of TEXT, a formula's right-hand side: numbers, references (as
strings beginning @ or $), names, operators and brackets."
  (let ((tokens '()) (i 0) (n (length text)))
    (loop while (< i n)
          do (let ((c (char text i)))
               (cond
                 ((member c '(#\Space #\Tab)) (incf i))
                 ((or (digit-char-p c) (and (char= c #\.) (< (1+ i) n) (digit-char-p (char text (1+ i)))))
                  (multiple-value-bind (s e) (cl-ppcre:scan "^[0-9]*(?:\\.[0-9]*)?(?:[eE][-+]?[0-9]+)?" text :start i)
                    (declare (ignore s))
                    ;; "1..2" is no number: the dots make a range
                    (let ((dots (search ".." text :start2 i :end2 e)))
                      (when dots (setf e dots)))
                    (push (list :number (or (org-calc-read-number (subseq text i e))
                                            (org-formula-fail "~a is no number" (subseq text i e))))
                          tokens)
                    (setf i e)))
                 ((member c '(#\@ #\$))
                  (multiple-value-bind (s e)
                      (cl-ppcre:scan "^(?:@(?:#|<+|>+|[-+]?I+|[-+]?[0-9]+))?(?:\\$(?:#|<+|>+|[-+]?[0-9]+))?" text
                                     :start i)
                    (declare (ignore s))
                    (when (= e i)
                      (org-formula-fail "~a is no reference formulas know here" (subseq text i (min n (+ i 8)))))
                    (push (list :reference (subseq text i e)) tokens)
                    (setf i e)))
                 ((alpha-char-p c)
                  (let ((e (or (position-if-not (lambda (c) (or (alphanumericp c) (char= c #\_))) text :start i) n)))
                    (push (list :name (subseq text i e)) tokens)
                    (setf i e)))
                 (t
                  (let ((two (and (< (1+ i) n) (subseq text i (+ i 2)))))
                    (cond ((member two '("==" "!=" "<=" ">=" "&&" "||" "..") :test #'equal)
                           (push (list :operator two) tokens) (incf i 2))
                          ((find c "+-*/%\\^<>!(),=")
                           (push (list :operator (string c)) tokens) (incf i))
                          ((char= c #\')
                           (org-formula-fail *org-formula-lisp-refused*))
                          (t (org-formula-fail "~a is not understood in a formula" c))))))))
    (nreverse tokens)))

(define-setting *org-formula-depth-limit* 200
  "The most a formula's parts may be nested.
Parts nest within brackets, signs and powers; a formula nested deeper is
refused."
  :values (integer 1 *))

(defun org-formula-parse (text)
  "TEXT, a formula's right-hand side, read into a tree to work out: numbers,
(:REF TEXT), (:RANGE FROM TO), (:CALL NAME ARGUMENTS), (:OP OPERATOR A B),
(:NEG A), (:NOT A), (:NAME TEXT). One nested deeper than
*ORG-FORMULA-DEPTH-LIMIT* is refused."
  (let ((tokens (org-formula-tokens text)) (depth 0))
    (labels ((peek () (first tokens))
             (operator-p (op) (and (eq (first (peek)) :operator) (string= (second (peek)) op)))
             (take () (pop tokens))
             (expect (op) (if (operator-p op) (take) (org-formula-fail "~a expected in ~a" op text)))
             (binary (next ops)
               (let ((left (funcall next)))
                 (loop while (and (eq (first (peek)) :operator) (member (second (peek)) ops :test #'string=))
                       do (let ((op (second (take))))
                            (setf left (list :op (intern (if (string= op "=") "==" op) :keyword) left (funcall next)))))
                 left))
             (or-level () (binary #'and-level '("||")))
             (and-level () (binary #'compare-level '("&&")))
             (compare-level () (binary #'add-level '("==" "!=" "<" ">" "<=" ">=" "=")))
             (add-level () (binary #'mod-level '("+" "-")))
             (mod-level () (binary #'div-level '("%")))
             (div-level () (binary #'mul-level '("/" "\\")))
             (mul-level () (binary #'unary '("*")))
             (unary ()
               (when (> (incf depth) *org-formula-depth-limit*)
                 (org-formula-fail "~a is nested too deeply" text))
               (prog1 (cond ((operator-p "-") (take) (list :neg (unary)))
                            ((operator-p "+") (take) (unary))
                            ((operator-p "!") (take) (list :not (unary)))
                            (t (power)))
                 (decf depth)))
             (power ()
               (let ((base (primary)))
                 (if (operator-p "^")
                     (progn (take) (list :op :^ base (unary)))
                     base)))
             (primary ()
               (let ((token (take)))
                 (case (first token)
                   (:number (second token))
                   (:reference (if (operator-p "..")
                                   (progn (take)
                                          (let ((to (take)))
                                            (unless (eq (first to) :reference)
                                              (org-formula-fail "A range needs a reference after .."))
                                            (list :range (second token) (second to))))
                                   (list :ref (second token))))
                   (:name (if (operator-p "(")
                              (progn (take)
                                     (let ((arguments '()))
                                       (unless (operator-p ")")
                                         (loop (push (or-level) arguments)
                                          (if (operator-p ",") (take) (return))))
                                       (expect ")")
                                       (list :call (second token) (nreverse arguments))))
                              (list :name (second token))))
                   (:operator (if (string= (second token) "(")
                                  (prog1 (or-level) (expect ")"))
                                  (org-formula-fail "~a out of place in ~a" (second token) text)))
                   (t (org-formula-fail "~a ends too soon" text))))))
      (prog1 (or-level)
        (when tokens (org-formula-fail "~a out of place in ~a" (second (first tokens)) text))))))

;;; References

(define-record (org-formula-context (:conc-name org-context-))
  "Where a formula is worked out: GRID, the table's rows of fields (a vector
of vectors of their text, rules left out); DATA, the place of each in the
table's rows; RULES, the places of its rules; COLUMNS, how many it has; ROW
and COLUMN, the field being worked out (from 0, in GRID); FLAGS, a list of :N, :E."
  (grid nil
        :documentation "The rows of fields of the table of ORG-FORMULA-CONTEXT, a vector of vectors of their text,
rules left out.")
  (data nil :documentation "The place in the table's rows of each field in the grid of ORG-FORMULA-CONTEXT.")
  (rules nil :documentation "The places of the rules of the table of ORG-FORMULA-CONTEXT.")
  (columns nil :documentation "How many columns the table of ORG-FORMULA-CONTEXT has.")
  (row nil :documentation "The row of the field being worked out, from 0, in the grid of ORG-FORMULA-CONTEXT.")
  (column nil :documentation "The column of the field being worked out, from 0, in the grid of ORG-FORMULA-CONTEXT.")
  (flags nil :documentation "The flags of ORG-FORMULA-CONTEXT, a list of :N and :E."))

(defun org-reference-row (context designator &key (as :start))
  "The data row (a place in the context's GRID) DESIGNATOR, the part of a
reference after @, means: 3, -1, +2, <, >, I, II; for a rule (I), the row
after it, or AS :END the row before it."
  (let* ((data (org-context-data context))
         (count (length data))
         (current (org-context-row context)))
    (flet ((check (row)
             (unless (and row (< -1 row count))
               (org-formula-fail "Row descriptor ~a leads outside the table" designator))
             row))
      (cond
        ((null designator) current)
        ((char= (char designator 0) #\<) (check (1- (length designator))))
        ((char= (char designator 0) #\>) (check (- count (length designator))))
        ((cl-ppcre:scan "^[-+]?I+$" designator)
         (let* ((sign (find (char designator 0) "-+"))
                (k (length (string-left-trim "-+" designator)))
                (rules (org-context-rules context))
                (here (svref data current))
                (after (tuatara.runtime:first-index-at-or-after rules (1+ here)))
                (index (cond ((null sign) (1- k))
                             ((char= sign #\-) (- (tuatara.runtime:first-index-at-or-after rules here) k))
                             (t (+ after (1- k)))))
                (rule (and (< -1 index (length rules)) (svref rules index))))
           (unless rule (org-formula-fail "There is no rule ~a in the table" designator))
           (let ((next (tuatara.runtime:first-index-at-or-after data (1+ rule))))
             (check (if (eq as :end) (1- next) next)))))
        ((find (char designator 0) "-+") (check (+ current (parse-integer designator))))
        (t (let ((n (parse-integer designator)))
             (check (if (zerop n) current (1- n)))))))))

(defun org-reference-column (context designator)
  "The column (from 0) DESIGNATOR, the part of a reference after $, means: 3, -1, <, >."
  (let ((columns (org-context-columns context))
        (current (org-context-column context)))
    (flet ((check (column)
             (unless (< -1 column columns)
               (org-formula-fail "Column descriptor $~a leads outside the table" designator))
             column))
      (cond ((null designator) current)
            ((char= (char designator 0) #\<) (check (1- (length designator))))
            ((char= (char designator 0) #\>) (check (- columns (length designator))))
            ((find (char designator 0) "-+") (check (+ current (parse-integer designator))))
            (t (let ((n (parse-integer designator))) (check (if (zerop n) current (1- n)))))))))

(defun org-reference-parts (reference)
  "The row and column designators of REFERENCE (@3$2, @-1, $<), each NIL when absent."
  (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^(?:@([^$]+))?(?:\\$(.+))?$" reference)
    (declare (ignore match))
    (values (aref groups 0) (aref groups 1))))

(defun org-field-text (context row column)
  "The text of the field at data row ROW and COLUMN of CONTEXT's table."
  (svref (svref (org-context-grid context) row) column))

(defun org-field-value (context text &key in-range)
  "The value of a field holding TEXT: its number; an empty one 0 (in a range
left out, unless the flags have E); text not a number as it is (0 with N)."
  (let ((flags (org-context-flags context)) (trimmed (string-trim '(#\Space #\Tab) text)))
    (cond ((string= trimmed "")
           (if (and in-range (not (member :e flags))) :empty 0))
          ((org-calc-read-number trimmed))
          ((member :n flags) 0)
          (t trimmed))))

(defun org-reference-value (context reference)
  "The value of REFERENCE (@R$C, @R, $C) where CONTEXT is."
  (cond ((string= reference "@#") (1+ (org-context-row context)))
        ((string= reference "$#") (1+ (org-context-column context)))
        (t (multiple-value-bind (row column) (org-reference-parts reference)
             (org-field-value context (org-field-text context (org-reference-row context row)
                                                      (org-reference-column context column)))))))

(defun org-range-value (context from to)
  "The values of the fields from reference FROM to TO, row by row, the empty ones left out unless the flags have E."
  (multiple-value-bind (from-row from-column) (org-reference-parts from)
    (multiple-value-bind (to-row to-column) (org-reference-parts to)
      (let* ((r1 (org-reference-row context from-row))
             (r2 (org-reference-row context to-row :as :end))
             (c1 (org-reference-column context from-column))
             (c2 (org-reference-column context to-column)))
        (remove :empty
                (loop for row from (min r1 r2) to (max r1 r2)
                      append (loop for column from (min c1 c2) to (max c1 c2)
                                   collect (org-field-value context (org-field-text context row column)
                                                            :in-range t))))))))

(defun org-formula-evaluate (tree context)
  "The value of TREE, a formula read by ORG-FORMULA-PARSE, where CONTEXT is."
  (labels ((value (tree)
             (cond ((org-calc-number-p tree) tree)
                   (t (ecase (first tree)
                        (:ref (org-reference-value context (second tree)))
                        (:range (org-range-value context (second tree) (third tree)))
                        (:neg (let ((x (org-calc-check (value (second tree)))))
                                (if (integerp x) (- x) (org-calc-float (- (org-calc-value x))))))
                        (:not (if (org-calc-true-p (value (second tree))) 0 1))
                        (:name (org-formula-fail "~a is no reference or function formulas know here" (second tree)))
                        (:call (if (string-equal (second tree) "if")
                                   (destructuring-bind (test then &optional (else 0)) (third tree)
                                     (if (org-calc-true-p (value test)) (value then) (value else)))
                                   (org-calc-function (second tree) (mapcar #'value (third tree)))))
                        (:op (let ((op (second tree)))
                               (case op
                                 (:&& (if (and (org-calc-true-p (value (third tree)))
                                               (org-calc-true-p (value (fourth tree)))) 1 0))
                                 (:|\|\|| (if (or (org-calc-true-p (value (third tree)))
                                                  (org-calc-true-p (value (fourth tree)))) 1 0))
                                 ((:== :!= :< :> :<= :>=)
                                  (org-calc-compare (intern (symbol-name op) :tuatara.defaults) (value (third tree))
                                                    (value (fourth tree))))
                                 (t (org-calc-arith (intern (symbol-name op) :tuatara.defaults)
                                                    (value (third tree)) (value (fourth tree)))))))))))
           (scalar (tree)
             (let ((x (value tree)))
               (when (listp x) (org-formula-fail "A range is no field's value: vsum() or another function takes one"))
               x)))
    (scalar tree)))

;;; Formats

(defun org-printf (control value)
  "VALUE formatted by CONTROL, a format as a table formula writes it, for a
number: %.2f, %d, %5.1f%%, %e, %g, %s."
  (let ((number (and (org-calc-number-p value) (org-calc-value value))))
    (cl-ppcre:regex-replace-all
     "%([-+ 0#]*)([0-9]*)(?:\\.([0-9]+))?([dfeEgGs%])" control
     (lambda (match flags width precision conversion)
       (declare (ignore match))
       (let* ((precision (and (plusp (length precision)) (org-calc-bounded (parse-integer precision))))
              (text (cond
                      ((string= conversion "%") "%")
                      ((or (null number) (string= conversion "s")) (org-calc-text value))
                      ((string= conversion "d") (princ-to-string (truncate number)))
                      ((string= conversion "f")
                       (let* ((p (or precision 6))
                              (scaled (round (* (rational (float number 1d0)) (expt 10 p))))
                              (digits (format nil "~v,'0d" (1+ p) (abs scaled))))
                         (format nil "~:[~;-~]~a~:[.~a~;~*~]" (minusp scaled) (subseq digits 0 (- (length digits) p))
                                 (zerop p) (subseq digits (- (length digits) p)))))
                      ((member conversion '("e" "E") :test #'string=)
                       (let ((text (format nil "~,v,2,,,,'eE" (or precision 6) (float number 1d0))))
                         (if (string= conversion "E") (string-upcase text) text)))
                      (t (org-format-calc-float (rational number) (max 1 (or precision 6)))))))
         (if (plusp (length width))
             (let ((width (org-calc-bounded (parse-integer width))))
               (if (find #\- flags)
                   (format nil "~va" width text)
                   (format nil "~v@a" width text)))
             text)))
     :simple-calls t)))

(defun org-read-formula-flags (text)
  "The flags after ; in a formula, as a plist: :FLAGS (:N :E), :MODE (as
ORG-CALC-TEXT takes it), :DIGITS (;p20), :PRINTF (%.2f...), :UNSUPPORTED."
  (let ((flags '()) (mode nil) (digits nil) (printf nil) (unsupported nil))
    (let ((percent (position #\% text)))
      (when percent
        (setf printf (subseq text percent) text (subseq text 0 percent))))
    (loop with i = 0
          while (< i (length text))
          do (let ((c (char text i)))
               (multiple-value-bind (s e) (cl-ppcre:scan "^[0-9]+" text :start (1+ i))
                 (declare (ignore s))
                 (let ((n (and e (parse-integer text :start (1+ i) :end e))))
                   (case c
                     (#\N (push :n flags))
                     (#\E (push :e flags))
                     (#\p (when n (setf digits n)))
                     (#\n (when n (setf mode (list :normal n))))
                     (#\f (when n (setf mode (list :fixed n))))
                     ((#\s #\e) (when n (setf mode (list :scientific n))))
                     (#\L (setf unsupported *org-formula-lisp-refused*))
                     ((#\T #\t #\U) (setf unsupported "Times and durations (;T ;t ;U) are not worked out yet"))
                     ((#\Space #\Tab) nil)
                     (t (setf unsupported (format nil "The flag ~a is not known here" c))))
                   (setf i (if n e (1+ i)))))))
    (list :flags flags :mode mode :digits digits :printf printf :unsupported unsupported)))

;;; Formulas of a table

(define-record (org-formula (:conc-name org-formula-))
  "One formula of a table: TEXT as #+TBLFM: has it, LHS (what it fills as :COLUMN
with COLUMN, :FIELD with ROW and COLUMN, :RANGE with FROM and TO), RHS (the
text to work out), its FLAGS (as ORG-READ-FORMULA-FLAGS gives them) and
PARSED, its right-hand side once read."
  (text nil :documentation "The text of an Org formula as #+TBLFM: has it.")
  (kind nil :documentation "The kind of left-hand side of an Org formula: :COLUMN, :FIELD or :RANGE.")
  (lhs nil
       :documentation "What an Org formula fills, as :COLUMN with COLUMN, :FIELD with ROW and COLUMN, or :RANGE with
FROM and TO.")
  (rhs nil :documentation "The right-hand side of an Org formula, the text to work out.")
  (flags nil :documentation "The flags of an Org formula, as ORG-READ-FORMULA-FLAGS gives them.")
  (parsed nil :documentation "The right-hand side of an Org formula once read, or NIL before."))

(defun org-read-formula (text)
  "TEXT, one formula of #+TBLFM: (LHS=RHS;FLAGS), read."
  (let ((equals (position #\= text)))
    (unless equals (org-formula-fail "~a has no =" text))
    (let* ((lhs (string-trim " " (subseq text 0 equals)))
           (rest (subseq text (1+ equals)))
           (semicolon (position #\; rest))
           (rhs (string-trim " " (if semicolon (subseq rest 0 semicolon) rest)))
           (flags (org-read-formula-flags (if semicolon (subseq rest (1+ semicolon)) ""))))
      (make-org-formula
       :text text :lhs lhs :rhs rhs :flags flags
       :kind (cond ((cl-ppcre:scan "^\\$(?:[0-9]+|<+|>+)$" lhs) :column)
                   ((search ".." lhs) :range)
                   ((cl-ppcre:scan "^@(?:[0-9]+|<+|>+|I+)\\$(?:[0-9]+|<+|>+)$" lhs) :field)
                   (t (org-formula-fail "~a is no column, field or range formulas know here" lhs)))))))

(defun org-table-formula-texts (formulas-line)
  "The formulas of a #+TBLFM: line's text (after the colon), :: between them."
  (remove "" (mapcar (lambda (s) (string-trim '(#\Space #\Tab) s)) (cl-ppcre:split "::" formulas-line))
          :test #'string=))

(defun org-formula-sort-key (text)
  "The key Org sorts a table's formulas by: column formulas by column, then field formulas by row and column."
  (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^(?:@([0-9]+))?(?:\\$?([0-9]+))?" text)
    (declare (ignore match))
    (format nil "~:[~;@~:*~5,'0d~]~:[~;$~:*~5,'0d~]~a"
            (and (aref groups 0) (parse-integer (aref groups 0)))
            (and (aref groups 1) (parse-integer (aref groups 1)))
            (if (or (aref groups 0) (aref groups 1)) "" (concatenate 'string "~" text)))))

(defun org-context-for (model)
  "A context for working out formulas in MODEL's table, its rows as a grid of fields."
  (let* ((rows (org-table-rows model))
         (columns (max 1 (org-table-columns model)))
         (data (coerce (loop for row in rows for i from 0 unless (eq row :rule) collect i) 'simple-vector))
         (rules (coerce (loop for row in rows for i from 0 when (eq row :rule) collect i) 'simple-vector))
         (grid (coerce (loop for row in rows
                             unless (eq row :rule)
                             collect (let ((fields (make-array columns :initial-element "")))
                                       (loop for field in row for c from 0 do (setf (svref fields c) field))
                                       fields))
                       'simple-vector)))
    (make-org-formula-context :grid grid :data data :rules rules :columns columns :row 0 :column 0)))

(defun org-formula-targets (formula context)
  "The fields (DATA-ROW . COLUMN) FORMULA fills in CONTEXT's table: a column
formula's column below the first rule (all its rows when the table has none),
a field formula's field, a range formula's fields."
  (let ((lhs (org-formula-lhs formula)))
    (ecase (org-formula-kind formula)
      (:column
       (let* ((column (org-reference-column context (subseq lhs 1)))
              (rules (org-context-rules context))
              (first-rule (and (plusp (length rules)) (svref rules 0))))
         (loop for index across (org-context-data context)
               for row from 0
               when (or (null first-rule) (> index first-rule))
               collect (cons row column))))
      (:field
       (multiple-value-bind (row column) (org-reference-parts lhs)
         (list (cons (org-reference-row context row) (org-reference-column context column)))))
      (:range
       (let* ((dots (search ".." lhs)))
         (multiple-value-bind (r1 c1) (org-reference-parts (subseq lhs 0 dots))
           (multiple-value-bind (r2 c2) (org-reference-parts (subseq lhs (+ dots 2)))
             (let ((r1 (org-reference-row context r1)) (r2 (org-reference-row context r2 :as :end))
                   (c1 (org-reference-column context c1)) (c2 (org-reference-column context c2)))
               (loop for row from (min r1 r2) to (max r1 r2)
                     append (loop for column from (min c1 c2) to (max c1 c2) collect (cons row column)))))))))))

(defun org-formula-tree (formula)
  "FORMULA's right-hand side read (ORG-FORMULA-PARSE), once."
  (or (org-formula-parsed formula)
      (setf (org-formula-parsed formula) (org-formula-parse (org-formula-rhs formula)))))

(defun org-formula-result (formula context)
  "The text FORMULA puts in the field CONTEXT is at: its value as table
formulas show it, in its format; #ERROR when it cannot be worked out there."
  (let* ((flags (org-formula-flags formula))
         (*org-calc-digits* (min *org-calc-largest* (or (getf flags :digits) *org-calc-digits*))))
    (setf (org-context-flags context) (getf flags :flags))
    (handler-case
        (let ((value (org-formula-evaluate (org-formula-tree formula) context)))
          (if (getf flags :printf)
              (org-printf (getf flags :printf) value)
              (org-calc-text value (getf flags :mode))))
      (org-formula-error (e)
        (if (string= (org-formula-error-text e) "#ERROR") "#ERROR" (error e)))
      ;; a number too large or too small for the machine (exp(1000))
      (arithmetic-error () "#ERROR"))))

(defun org-apply-formulas (model formulas)
  "Work out FORMULAS (ORG-FORMULA objects) in MODEL's table: row by row, the
column formulas in order, then the
field and range formulas, a field's own formula before its column's. Signals
ORG-FORMULA-ERROR, the table as it was, when one cannot be worked out at all."
  (let* ((context (org-context-for model))
         (grid (org-context-grid context))
         (columns (org-context-columns context))
         (column-formulas (remove-if-not (lambda (formula) (eq (org-formula-kind formula) :column)) formulas))
         (fields (remove :column formulas :key #'org-formula-kind))
         (own (make-hash-table)))
    (flet ((key (target) (+ (* (car target) columns) (cdr target)))
           (fill-field (formula row column)
             (setf (org-context-row context) row (org-context-column context) column)
             (setf (svref (svref grid row) column) (org-formula-result formula context))))
      (dolist (formula fields)
        (dolist (target (org-formula-targets formula context))
          (setf (gethash (key target) own) t)))
      ;; each column formula's rows, as a vector by row: the column it fills there, or NIL
      (let ((by-row (mapcar (lambda (formula)
                              (let ((rows (make-array (length grid) :initial-element nil)))
                                (dolist (target (org-formula-targets formula context))
                                  (unless (gethash (key target) own)
                                    (setf (svref rows (car target)) (cdr target))))
                                (cons formula rows)))
                            column-formulas)))
        (dotimes (row (length grid))
          (loop for (formula . rows) in by-row
                for column = (svref rows row)
                when column do (fill-field formula row column))))
      (dolist (formula fields)
        (loop for (row . column) in (org-formula-targets formula context)
              do (fill-field formula row column))))
    (let ((k 0))
      (setf (org-table-rows model)
            (loop for row in (org-table-rows model)
                  collect (if (eq row :rule) row (prog1 (coerce (svref grid k) 'list) (incf k))))))))

(defun org-runnable-formulas (texts)
  "The formulas of TEXTS read, and as a second value why those left out cannot be worked out, or NIL."
  (let ((formulas '()) (left-out nil))
    (dolist (text texts)
      (handler-case
          (let ((formula (org-read-formula text)))
            (if (getf (org-formula-flags formula) :unsupported)
                (setf left-out (format nil "~a: ~a" text (getf (org-formula-flags formula) :unsupported)))
                (progn (when (uiop:string-prefix-p "'" (org-formula-rhs formula))
                         (org-formula-fail *org-formula-lisp-refused*))
                       (push formula formulas))))
        (org-formula-error (e) (setf left-out (format nil "~a: ~a" text (org-formula-error-text e))))))
    (values (nreverse formulas) left-out)))

;;; The table's #+TBLFM: line in the model

(defun org-table-formulas-of (parse)
  "The formulas of the first #+TBLFM: line of the Org table of PARSE (an
ORG-PARSE) point is in, as texts."
  (let ((table (org-table-at (org-parse-tree parse) (org-parse-text parse) (org-parse-point parse))))
    (and table (org-table-formula-texts (or (first (org-property table :tblfm)) "")))))

(defun org-formula-lhs-p (text lhs)
  "True when TEXT, a table's formula, sets LHS ($3, @2$1): what stands before
its = is LHS."
  (let ((equals (position #\= text)))
    (and equals (string= (string-trim " " (subseq text 0 equals)) lhs))))

(defun org-table-data-row-number (model row)
  "The number of the data row ROW of MODEL is, counting from 1, its rules not
counted, as a formula's @ names it."
  (1+ (count-if-not (lambda (r) (eq r :rule)) (org-table-rows model) :end row)))

(defun org-store-formula (texts lhs rhs)
  "TEXTS, a table's formulas, with the formula of LHS made LHS=RHS (taken out
when RHS is empty), sorted by what each sets (ORG-FORMULA-SORT-KEY)."
  (let ((others (remove-if (lambda (text) (org-formula-lhs-p text lhs))
                           texts)))
    (sort (if (string= (string-trim " " rhs) "") others (cons (format nil "~a=~a" lhs rhs) others))
          #'string< :key #'org-formula-sort-key)))

(defun org-table-recalculate-model (model texts)
  "Work out the formulas TEXTS in MODEL's table; true when they could be. Says
which could not be worked out, and why."
  (multiple-value-bind (formulas left-out) (org-runnable-formulas texts)
    (handler-case
        (progn (org-apply-formulas model formulas)
               (if left-out (message "Left out: ~a" left-out) (message "Worked out ~d formula~:p" (length formulas)))
               t)
      (org-formula-error (e) (message "~a" (org-formula-error-text e)) nil))))

(define-command org-table-recalculate ()
  "Work out the formulas of the Org table point is in, its #+TBLFM: line
\(\\`C-c *'): column formulas ($3=$1+$2) below the
first rule (in every row when it has none), field formulas (@4$2=vsum(@2..@3))
and range formulas (@2$2..@3$3=$1*10), worked out with the table
arithmetic. Formulas in Emacs Lisp are left out: nothing in a file is run."
  (let ((done (with-org-table (parse model)
                (let ((texts (org-table-formulas-of parse)))
                  (cond ((null texts) (message "This table has no formulas (#+TBLFM:)") :not-done)
                        ((org-table-recalculate-model model texts) t)
                        (t :not-done))))))
    (unless done (message "Not in a table"))))

(defun org-tblfm-line-table (tree text position)
  "The Org table whose #+TBLFM: line holds the character POSITION of TEXT, or NIL."
  (let ((line-start (string-line-start text position)))
    (when (cl-ppcre:scan "(?i)^[ \\t]*#\\+TBLFM:" (org-line-at text line-start))
      (org-node-at tree line-start
                   (lambda (node)
                     (and (eq (org-node-type node) 'table) (eq (org-property node :type) 'org)
                          (org-node-contents-end node) (>= line-start (org-node-contents-end node))))))))

(defun org-recalculate-from-tblfm (parse position)
  "When point, at the character POSITION of PARSE (an ORG-PARSE), is on the
#+TBLFM: line of an Org table, work its formulas out, point staying; true when
it was."
  (let* ((text (org-parse-text parse))
         (table (org-tblfm-line-table (org-parse-tree parse) text position)))
    (when table
      (let* ((model (org-table-read text table (org-node-contents-begin table)))
             (texts (org-table-formula-texts (or (first (org-property table :tblfm)) ""))))
        (when (org-table-recalculate-model model texts)
          (multiple-value-bind (new) (org-table-render model)
            (let ((begin (org-table-begin model)) (end (org-table-end model)))
              (unless (string= new (subseq text begin end))
                (org-replace-chars parse begin end new :point (+ position (- (length new) (- end begin)))))))))
      t)))

(defun org-table-store-and-apply (model lhs rhs)
  "Make LHS=RHS one of the formulas of MODEL's table (taken out when RHS is
empty) and work it out; the #+TBLFM: line written with the table."
  (let ((texts (org-store-formula (org-table-formulas model) lhs rhs)))
    (setf (org-table-formulas model) texts
          (org-table-formulas-changed model) t)
    (when (plusp (length (string-trim " " rhs)))
      (handler-case
          (multiple-value-bind (formulas left-out) (org-runnable-formulas (list (format nil "~a=~a" lhs rhs)))
            (if left-out
                (message "~a" left-out)
                (org-apply-formulas model formulas)))
        (org-formula-error (e) (message "~a" (org-formula-error-text e)))))))

(defun org-table-maybe-eval-formula (model)
  "When the field point is in holds =FORMULA (a column formula) or :=FORMULA
(the field's own), make it a formula of MODEL's table and work it out."
  (let* ((row (org-table-row model))
         (field (and (org-table-data-row-p model row)
                     (nth (org-table-field model) (nth row (org-table-rows model))))))
    (multiple-value-bind (match groups) (and field (cl-ppcre:scan-to-strings "^(:?)=(.*[^=])$" field))
      (when match
        (setf (nth (org-table-field model) (nth row (org-table-rows model))) "")
        (let ((data-row (org-table-data-row-number model row))
              (column (1+ (org-table-field model))))
          (org-table-store-and-apply model
                                     (if (string= (aref groups 0) ":")
                                         (format nil "@~d$~d" data-row column)
                                         (format nil "$~d" column))
                                     (aref groups 1)))))))

(define-command org-table-eval-formula ()
  "Ask for the formula of the column point is in, the one it has offered, and
work it out (\\`C-c ='); with
\\[universal-argument], the formula of the field point is in. An empty one
takes the formula away."
  (let ((field-formula (prefix-argument)))
    (unless (with-org-table (parse model)
              (let* ((row (org-table-row model))
                     (data-row (org-table-data-row-number model row))
                     (lhs (if field-formula
                              (format nil "@~d$~d" data-row (1+ (org-table-field model)))
                              (format nil "$~d" (1+ (org-table-field model)))))
                     (old (find-if (lambda (text) (org-formula-lhs-p text lhs))
                                   (org-table-formulas model)))
                     (rhs (multiple-value-bind (rhs after)
                              (org-table-ask parse model
                                             (lambda ()
                                               (read-string (format nil "~:[Column~;Field~] formula ~a = "
                                                                    field-formula lhs)
                                                            :initial (if old (subseq old (1+ (position #\= old))) ""))))
                            (setf parse after)
                            rhs)))
                (org-table-store-and-apply model lhs rhs)))
      (message "Not in a table"))))

;;; What a table's formulas may say

(defparameter *org-formula-functions*
  '(("vsum(@2..@5)" "the sum; a range, or numbers apart by commas")
    ("vprod(range)" "the product")
    ("vcount(range)" "how many values it has")
    ("vmean(range)" "the mean")
    ("vmedian(range)" "the median")
    ("vmax(range), max(range)" "the largest")
    ("vmin(range), min(range)" "the smallest")
    ("abs(x)" "without its sign")
    ("round(x), round(x, 2)" "to a whole number, or to that many places")
    ("floor(x), ceil(x)" "down and up to a whole number")
    ("sqrt(x), exp(x)" "the square root; e to the power x")
    ("ln(x), log10(x)" "the natural logarithm; the logarithm to base 10")
    ("sin(x), cos(x), tan(x)" "x in degrees"))
  "The functions a table formula may call, each as it is written and what
it gives.")

(defparameter *org-formula-flags*
  '(("N" "a field that is no number counts as 0, rather than staying the text it is")
    ("E" "an empty field in a range counts as 0, rather than being left out")
    ("p20" "work decimal numbers out to 20 digits; 12 as standard")
    ("n3" "show decimal numbers to 3 digits; 8 as standard")
    ("f2" "show decimal numbers with 2 places after the point")
    ("s3, e3" "show decimal numbers in scientific form, to 3 digits")
    ("%.2f" "write the result as C's printf does: %d, %f, %e, %E, %g, %G, %s, with a width and flags (%5.1f%%)")
    ("L, T, t, U" "not worked out: Emacs Lisp is never run, and times and durations are not yet; the field says so"))
  "The flags written after ; in a table formula, each as it is written and what
it does.")

(define-command describe-table-formula-settings ()
  "Show what a #+TBLFM: line under an Org table may hold: the places a
formula fills, the references it takes, its operators, functions and flags,
with small examples. The lines are filled to *HELP-COLUMN*."
  (show-help
   (with-output-to-string (s)
     (write-help-section s "Formulas under a table"
                         :intro "\\<org-mode>A #+TBLFM: line right under a table holds its formulas, apart by ::.
                         \\[org-table-recalculate] works them all out, as does \\[org-ctrl-c-ctrl-c] on the line.
                         =$1*2 typed in a field, then TAB, RET or \\[org-ctrl-c-ctrl-c], makes the formula of the
                         column and works it out; := makes one for the field alone.
                         \\[org-table-eval-formula] asks for one. The sums are Calc's: integers exact, decimal
                         numbers to 12 digits shown to 8, * before /, and nothing in Emacs Lisp is run."
                         :example "| 2 | 3 |   |
| 4 | 5 |   |
#+TBLFM: $3=$1+$2
#+TBLFM: @3$3=vsum(@1$3..@2$3) :: $4=$1*10;%.1f")
     (write-help-section s "What a formula fills"
                         :rows '("$3=..." "the third column, in every row (columns count from 1)"
                                 "$<=..., $>=..." "the first and the last column"
                                 "@4$2=..." "one field: row 4, column 2"
                                 "@2$2..@3$3=..." "a range of fields"
                                 "@<, @>" "the first and the last row, in every column"
                                 "@I, @II" "the row after the first, the second rule (a |---| line)")
                         :width 20)
     (write-help-section s "References"
                         :rows '("$1" "column 1 of this row"
                                 "@2" "row 2, in this column"
                                 "@2$1" "the field in row 2, column 1"
                                 "$-1, @-1" "the column or row before this one; $+1, @+1 the one after"
                                 "$<, $>, @<, @>" "the first and last; $<< the second, @>> the one before the last"
                                 "@I, @II" "the row after the first, the second rule of the table"
                                 "@-I, @+II" "the row after the nearest rule before this row; after the second
                                 rule after it"
                                 "@#, $#" "the number of this row and of this column"
                                 "@2..@4, $1..$3" "a range of fields, for the functions below")
                         :width 20)
     (write-help-section s "What a field holds"
                         :intro "A number: 12, 1.5, 3. or 1e10. An empty field is 0, and in a range is left out
                         (flag E counts it). Text that is no number stays the text; flag N makes it 0.")
     (write-help-section s "Operators"
                         :rows '("+ - * / ^" "the sum, difference, product and quotient; ^ raises to a power"
                                 "%  \\" "the remainder; integer division"
                                 "== != < > <= >= =" "give 1 for true and 0 for false; = is the same as =="
                                 "&& || !" "and, or, not; a number that is not 0 is true"
                                 "( )" "group; -x is the negative"))
     (write-help-section s "Functions"
                         :rows (loop for (written words) in *org-formula-functions* append (list written words))
                         :width 26)
     (write-help-section s "Flags after ; at the end of a formula"
                         :rows (loop for (written words) in *org-formula-flags* append (list written words))
                         :width 14))))

(defun install-org-formulas ()
  "Bind Org's keys for table formulas: \\`C-c *' works them out, \\`C-c =' asks for one."
  (let ((keymap (mode-keymap 'org-mode)))
    (define-key keymap "C-c *" 'org-table-recalculate)
    (define-key keymap "C-c =" 'org-table-eval-formula)))
