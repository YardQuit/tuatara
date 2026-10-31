;;;; lisp/editing/lists.lisp --- Lists and definitions: Emacs's C-M- commands.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Moving over, out of and into bracketed groups (C-M-n, C-M-p, C-M-u,
;;;; C-M-d), marking and killing balanced expressions (C-M-SPC, C-M-DEL, M-x
;;;; kill-sexp), deleting the brackets or quote marks around one (M-x
;;;; delete-pair), and moving to the start and end of a definition (C-M-a,
;;;; C-M-e) or marking one (M-x mark-defun), as Emacs does all of these.
;;;; Strings and comments are told by the syntax tree where the buffer has
;;;; one; without it every bracket counts. In Lisp, and in text, a definition
;;;; is what Emacs takes one to be: an opening bracket at the start of a line,
;;;; in no string or comment. In a language whose definitions its rules name
;;;; (Rust, C, shell scripts) it is one of those, one inside another counting
;;;; too, as in Emacs's tree-sitter modes.

(in-package #:tuatara.defaults)

;;; Groups

(defun group-scan (buffer position)
  "A bracket scan of BUFFER's text near POSITION: strings, comments and
character literals left out where BUFFER has a syntax tree."
  (tuatara.syntax:bracket-scan-near (ready-syntax buffer) (tuatara.buffer:buffer-text buffer) position))

(defun move-by-groups (count)
  "Move point over COUNT bracketed groups, back when COUNT is negative, as
forward-list does."
  (let ((b (current-buffer)) (forward (plusp count)))
    (dotimes (i (abs count))
      (multiple-value-bind (start end) (if forward
                                           (tuatara.syntax:next-group (group-scan b (point)) (point))
                                           (tuatara.syntax:previous-group (group-scan b (point)) (point)))
        (cond (start (goto (if forward end start)))
              ((eq end :none) (goto (if forward (tuatara.buffer:buffer-size b) 0)) (return))
              ((eq end :unbalanced) (message "The brackets do not balance") (return))
              ((eq end :far)
               (message "No ~:[previous~;next~] group within ~d KB" forward (floor tuatara.syntax:+bracket-reach+ 1024))
               (return))
              (t (message "No ~:[previous~;next~] group" forward) (return)))))))

(define-command forward-list ()
  "Move forward over the next bracketed group: the words, strings and comments
before it passed over, the group taken whole. With a count, that many groups;
a negative count moves backward. With no group before the end of the text, go
to its end; where the group around point ends first, stay, and say so."
  (move-by-groups (prefix)))

(define-command backward-list ()
  "Move backward over the bracketed group before point, to its opening
bracket; the opposite of forward-list. With a count, that many; a negative
count moves forward."
  (move-by-groups (- (prefix))))

(defun group-around (buffer syntax position forward)
  "Where moving out of the group around POSITION in BUFFER goes: after its
closing bracket when FORWARD, else to its opening one; NIL at the top level.
In a string or comment (by SYNTAX, BUFFER's tree, when it has one) the
brackets inside it around POSITION come first; with none, out of a string to
its start or end, and out of a comment on to the group around it."
  (let ((rope (tuatara.buffer:buffer-text buffer)))
    (multiple-value-bind (kind start end) (and syntax (tuatara.syntax:text-kind-around syntax position))
      (if kind
          (let* ((scan (if (eq kind :string)
                           (tuatara.syntax:bracket-scan-between rope (1+ start) (max (1+ start) (1- end)))
                           (tuatara.syntax:bracket-scan-between rope start end)))
                 (inside (if forward
                             (tuatara.syntax:group-end-around scan position)
                             (tuatara.syntax:group-start-around scan position))))
            (cond (inside)
                  ((eq kind :string) (if forward end start))
                  (t (let ((out (if forward end start)))
                       ;; a comment to the end of its line ends with the line
                       (when (and forward (tuatara.syntax:text-kind-around syntax out)
                                  (< out (tuatara.buffer:buffer-size buffer)))
                         (incf out))
                       (group-around buffer syntax out forward)))))
          (let ((scan (tuatara.syntax:bracket-scan-near syntax rope position)))
            (if forward
                (tuatara.syntax:group-end-around scan position)
                (tuatara.syntax:group-start-around scan position)))))))

(defun move-out-of-groups (count)
  "Move point out of COUNT groups around it, forward past their closing
brackets, or back to their opening ones when COUNT is negative."
  (let* ((b (current-buffer)) (syntax (ready-syntax b)))
    (dotimes (i (abs count))
      (let ((out (group-around b syntax (point) (plusp count))))
        (unless out (message "At top level") (return))
        (goto out)))))

(define-command backward-up-list ()
  "Move back out of the bracketed group around point, to its opening bracket.
In a string, brackets inside it around point first, then to the string's
start; in a comment, brackets inside it, then on to the group around the
comment. With a count, that many levels out; a negative count moves forward,
past the closing brackets."
  (move-out-of-groups (- (prefix))))

(define-command up-list ()
  "Move forward out of the bracketed group around point, past its closing
bracket; out of a string first to its end. With a count, that many levels out;
a negative count moves back to the opening brackets."
  (move-out-of-groups (prefix)))

(define-command down-list ()
  "Move forward into the next bracketed group, just past its opening bracket,
the words, strings and comments before it passed over. With a count, that many
levels in; a negative count moves backward into the group before point, to
just before its closing bracket. Not in a string or comment."
  (let* ((b (current-buffer))
         (syntax (ready-syntax b))
         (count (prefix)))
    (if (and syntax (tuatara.syntax:text-kind-around syntax (point)))
        (message "This command doesn't work in strings or comments")
        (dotimes (i (abs count))
          (let ((scan (group-scan b (point))))
            (multiple-value-bind (q opening) (if (plusp count)
                                                 (tuatara.syntax:scan-next-bracket scan (point))
                                                 (tuatara.syntax:scan-previous-bracket scan (point)))
              (if (and q (eq (and opening t) (plusp count)))
                  (goto (if (plusp count) (1+ q) q))
                  (progn (message "At bottom level") (return)))))))))

;;; Killing and marking expressions

(defun kill-sexps (count)
  "Kill COUNT balanced expressions after point, or before it when COUNT is
negative, as one kill."
  (let ((b (current-buffer)) (p (point)))
    (multiple-value-bind (q why) (sexp-motion b p count)
      (cond ((null q)
             (message (if (eq why :unbalanced)
                          "The brackets do not balance"
                          (if (plusp count) "No next sexp" "No previous sexp"))))
            ((/= q p)
             (goto (kill-span b (min p q) (max p q) :prepend (< q p))))))))

(define-command kill-sexp ()
  "Kill the balanced expression after point: to where forward-sexp goes. With
a count, that many; a negative count kills those before point. Kills in a row
join into one."
  (kill-sexps (prefix)))

(define-command backward-kill-sexp ()
  "Kill the balanced expression before point: to where backward-sexp goes.
With a count, that many; a negative count kills those after point. Kills in a
row join into one."
  (kill-sexps (- (prefix))))

(define-command mark-sexp ()
  "Set the mark after the next balanced expression, point staying, and
activate it; with a count, that many expressions, or before point when
negative. Again, or with the region active, move the mark one expression
further from point, on past it or back before it when it is before point; with
a count, that many."
  (let* ((b (current-buffer))
         (argument (prefix-argument))
         (mark (tuatara.buffer:buffer-mark b))
         (extend (and mark (or (eq (editor-last-command *editor*) 'mark-sexp)
                               (tuatara.buffer:buffer-mark-active-p b))))
         (from (if extend mark (point)))
         (count (or argument (if (and extend (< mark (point))) -1 1))))
    (multiple-value-bind (q why) (sexp-motion b from count)
      (cond ((null q)
             (message (if (eq why :unbalanced)
                          "The brackets do not balance"
                          (if extend "No more sexp to select" "No sexp to select"))))
            (extend (setf (tuatara.buffer:buffer-mark b) q
                          (tuatara.buffer:buffer-mark-active-p b) t))
            ;; a new mark puts the one before on the mark ring
            (t (push-mark :position q :activate t))))))

;;; Pairs

(define-command delete-pair ()
  "Delete the brackets or quote marks around the balanced expression after
point: the character there, after any spaces and tabs, and the one that closes
the expression it begins. With a count, those around that many expressions; a
negative count deletes the pair around those before point instead. When the
two are no pair of *PAIR-DELIMITERS*, delete nothing and say so."
  (let* ((b (current-buffer))
         (count (prefix))
         (forward (plusp count))
         (from (skip-blanks b (point) :backward (not forward) :octets '(32 9))))
    (multiple-value-bind (to why) (sexp-motion b from count)
      (if (null to)
          (sexp-failure-message why)
          (let* ((start (min from to)) (end (max from to))
                 (open (tuatara.buffer:buffer-char-at b start))
                 (close (and (< start end) (tuatara.buffer:buffer-char-at b (tuatara.buffer:buffer-forward-char b end -1)))))
            (if (or (>= (tuatara.buffer:buffer-forward-char b start) end)
                    (not (eql close (pair-closer open))))
                (message (if forward "Not before matching pair" "Not after matching pair"))
                (let ((p (point))
                      (inner-end (tuatara.buffer:buffer-forward-char b end -1)))
                  (tuatara.buffer:buffer-edit b (vector (tuatara.text:make-edit start (tuatara.buffer:buffer-forward-char b start))
                                                        (tuatara.text:make-edit inner-end end)))
                  ;; point stays by its text: before the pair, or after it and two characters on
                  (goto (cond ((<= p start) p)
                              ((>= p end) (- p (- (tuatara.buffer:buffer-forward-char b start) start) (- end inner-end)))
                              (t (- p (- (tuatara.buffer:buffer-forward-char b start) start))))))))))))

;;; Definitions

(defun definition-rules (buffer)
  "The compiled rules naming the definitions of BUFFER's language and
BUFFER's up-to-date syntax tree, as two values, when its language has such
rules and BUFFER a tree; NIL when a definition is a line that begins with an
opening bracket."
  (let* ((language (mode-language (buffer-major-mode buffer)))
         (query (and language (language-compiled-query language :definitions)))
         (syntax (and query (ready-syntax buffer))))
    (when syntax (values query syntax))))

(defparameter *definition-reach* (* 4 1024 1024)
  "The most octets either side of a place looked through for the definitions
nearest it, in a language whose rules name them.")

(defun definition-ranges (query syntax from to)
  "The definitions QUERY names in SYNTAX's tree that touch octets FROM below
TO, each (START . END): its captures called definition."
  (let ((ranges '()) (names (tuatara.syntax:query-captures query)))
    (tuatara.syntax:map-query-matches
     (lambda (pattern captures count)
       (declare (ignore pattern))
       (dotimes (i count)
         (when (string= "definition" (svref names (aref captures (* 4 i))))
           (pushnew (cons (aref captures (+ 1 (* 4 i))) (aref captures (+ 2 (* 4 i)))) ranges :test #'equal))))
     query (tuatara.syntax:syntax-tree syntax) (tuatara.syntax:syntax-rope syntax) from to)
    ranges))

(defun definitions-around (query syntax position)
  "The definitions nearest POSITION of SYNTAX's text, as QUERY names them,
three values, each (START . END) or NIL: the smallest around POSITION,
starting before it; the one ending last at or before POSITION within that,
the outermost; and the one starting first at or after POSITION within that,
the outermost."
  (let ((size (tuatara.text:rope-bytes (tuatara.syntax:syntax-rope syntax)))
        (reach 65536))
    (loop
      (let* ((from (max 0 (- position reach)))
             (to (min size (+ position reach)))
             (ranges (definition-ranges query syntax from to))
             (parent nil) (previous nil) (next nil))
        (dolist (r ranges)
          (when (and (< (car r) position (cdr r))
                     (or (null parent) (< (- (cdr r) (car r)) (- (cdr parent) (car parent)))))
            (setf parent r)))
        (dolist (r ranges)
          (when (or (null parent) (and (<= (car parent) (car r)) (<= (cdr r) (cdr parent)) (not (eq r parent))))
            (when (and (<= (cdr r) position)
                       (or (null previous) (> (cdr r) (cdr previous))
                           (and (= (cdr r) (cdr previous)) (< (car r) (car previous)))))
              (setf previous r))
            (when (and (>= (car r) position)
                       (or (null next) (< (car r) (car next))
                           (and (= (car r) (car next)) (> (cdr r) (cdr next)))))
              (setf next r))))
        (when (or (>= reach *definition-reach*)
                  (and (or previous (<= from (if parent (car parent) 0)))
                       (or next (>= to (if parent (cdr parent) size)))))
          (return (values parent previous next)))
        (setf reach (* 2 reach))))))

(defun navigate-definitions (query syntax position count side &optional recursing)
  "Where COUNT definitions on from POSITION go, back when COUNT is negative,
to their starts when SIDE is :START, else their ends: among the definitions
beside each other first, then out to the one around them, never into one. NIL
when there are not so many."
  (let ((p position))
    (dotimes (i (abs count) p)
      (multiple-value-bind (parent previous next) (definitions-around query syntax p)
        (if (plusp count)
            (let ((target (or next parent)))
              (cond ((and (eq side :start) (if next (and (not recursing) (= p (car next))) parent))
                     ;; at a start already: the start after this definition, or after the one around
                     (setf p (or (navigate-definitions query syntax (cdr target) 1 :start t) (return nil))))
                    (target (setf p (if (eq side :start) (car target) (cdr target))))
                    (t (return nil))))
            (let ((target (or previous parent)))
              (cond ((and (eq side :end) (if previous (and (not recursing) (= p (cdr previous))) parent))
                     ;; at an end already: the end before this definition, or before the one around
                     (setf p (or (navigate-definitions query syntax (car target) -1 :end t) (return nil))))
                    (target (setf p (if (eq side :start) (car target) (cdr target))))
                    (t (return nil)))))))))

(defun line-start-p (buffer position)
  "True when POSITION of BUFFER is at the start of a line."
  (= position (tuatara.buffer:buffer-line-start buffer position)))

(defun next-line-start (buffer position)
  "The start of the line after the one POSITION of BUFFER is on, or the end of
the text when that is the last."
  (let ((end (tuatara.buffer:buffer-line-end buffer position)))
    (if (< end (tuatara.buffer:buffer-size buffer)) (tuatara.buffer:buffer-forward-char buffer end) end)))

(defun definition-skip (buffer position)
  "POSITION, where a definition starts or ends, moved to where point is left:
past a semicolon there (the one after a C struct, not part of it); then
before only blanks to the end of the line, to the start of the next; after
only blanks from the start of its line, to that start."
  (let* ((position (if (eql (tuatara.buffer:buffer-char-at buffer position) #\;)
                       (tuatara.buffer:buffer-forward-char buffer position)
                       position))
         (line (tuatara.buffer:buffer-line-start buffer position))
         (after (skip-blanks buffer position :octets '(32 9))))
    (cond ((and (/= position line)
                (member (tuatara.buffer:buffer-char-at buffer after) '(#\Newline #\Return)))
           (next-line-start buffer position))
          ((and (/= position line) (= line (skip-blanks buffer position :backward t :octets '(32 9))))
           line)
          (t position))))

(defun rules-definition-move (buffer query syntax position count side)
  "Where \\[beginning-of-defun] (SIDE :START, COUNT back) or \\[end-of-defun]
(SIDE :END, COUNT on) goes from POSITION in BUFFER by its language's
definition rules, and true as a second value when it moved; a try that ends
where it began tries one further."
  (let ((count count))
    (loop repeat 2
          do (let ((dest (navigate-definitions query syntax position count side)))
               (when dest
                 (let ((p (definition-skip buffer dest)))
                   (unless (= p position) (return-from rules-definition-move (values p t))))))
             (setf count (if (plusp count) (1+ count) (1- count))))
    (values position nil)))

(defun bracket-definition-p (syntax position)
  "True when the opening bracket at POSITION, at the start of its line, starts
a definition: it is in no string or comment of SYNTAX's tree, where there is one."
  (or (null syntax)
      (notany (lambda (node) (or (tuatara.syntax:string-node-p node) (tuatara.syntax:comment-node-p node)))
              (tuatara.syntax:node-ancestry syntax position (1+ position)))))

(defun line-opener-before (rope position &key (openers tuatara.text:*opening-octets*) (reach most-positive-fixnum) test block)
  "The last octet before POSITION of ROPE that is one of OPENERS (an opening
bracket) at the start of a line, and for which TEST, a function of its
offset, is true when given; looked for no further than REACH octets back. NIL
when there is none, with :FAR as a second value when the reach ended before
the start of the text. BLOCK, octets of the caller's at least 65,537 long,
is read into in place of new ones. Complexity: O(distance back), read in
blocks growing from a kilobyte."
  (let ((floor (max 0 (- position reach)))
        (high (1- position))
        (size 1024))
    (declare (type fixnum floor high size))
    ;; back through the text a block at a time, the octet before each block included
    (loop while (>= high floor)
          do (let* ((low (max floor (- high (1- size))))
                    (from (max 0 (1- low)))
                    (block (if block
                               (progn (tuatara.text:rope-copy-into rope from (1+ high) block) block)
                               (tuatara.text:rope-octets rope :start from :end (1+ high)))))
               (declare (type fixnum low from))
               (loop for j of-type fixnum from high downto low
                     for k = (- j from)
                     when (and (or (zerop j) (= (aref block (1- k)) 10))
                               (member (aref block k) openers)
                               (or (null test) (funcall test j)))
                       do (return-from line-opener-before j))
               (setf high (1- low)
                     size (min 65536 (* 2 size)))))
    (values nil (and (plusp floor) :far))))

(defun line-opener-after (rope position &key (openers tuatara.text:*opening-octets*) (reach most-positive-fixnum) test)
  "The first octet after POSITION of ROPE that is one of OPENERS (an opening
bracket) at the start of a line, and for which TEST, a function of its
offset, is true when given; looked for no further than REACH octets on. NIL
when there is none, with :FAR as a second value when the reach ended before
the end of the text. Complexity: O(distance on), read in blocks of 64 KB
into one array."
  (let* ((size (tuatara.text:rope-bytes rope))
         (ceiling (min size (+ position 1 (min reach size))))
         (low (1+ position))
         (block (and (< low ceiling)
                     (make-array (1+ (min 65536 (- ceiling low))) :element-type '(unsigned-byte 8)))))
    (loop while (< low ceiling)
          do (let* ((high (min ceiling (+ low 65536)))
                    (from (1- low)))
               (tuatara.text:rope-copy-into rope from high block)
               (loop for j from low below high
                     for k = (- j from)
                     when (and (= (aref block (1- k)) 10)
                               (member (aref block k) openers)
                               (or (null test) (funcall test j)))
                       do (return-from line-opener-after j))
               (setf low high)))
    (values nil (and (< ceiling size) :far))))

(defun line-bracket-before (buffer syntax position)
  "The last opening bracket before POSITION in BUFFER that begins a line and
a definition (BRACKET-DEFINITION-P); NIL when there is none, with :FAR as a
second value when none is within *TEXT-SCAN-LIMIT* octets."
  (line-opener-before (tuatara.buffer:buffer-text buffer) position :reach *text-scan-limit*
                                                                   :test (lambda (j) (bracket-definition-p syntax j))
                                                                   :block (make-array 65537 :element-type '(unsigned-byte 8))))

(defun line-bracket-after (buffer syntax position)
  "The first opening bracket after POSITION in BUFFER that begins a line and
a definition (BRACKET-DEFINITION-P); NIL when there is none, with :FAR as a
second value when none is within *TEXT-SCAN-LIMIT* octets."
  (line-opener-after (tuatara.buffer:buffer-text buffer) position :reach *text-scan-limit*
                                                                  :test (lambda (j) (bracket-definition-p syntax j))))

(defun bracket-definition-start (buffer position count)
  "The start of the definition COUNT back from POSITION in BUFFER, for
definitions that are lines beginning with an opening bracket: the COUNTth such
bracket before POSITION, or after it when COUNT is negative, and T as a second
value. With too few, the start of the text (its end), and NIL, or POSITION and
:FAR when the looking stopped at *TEXT-SCAN-LIMIT*."
  (let ((syntax (ready-syntax buffer))
        (p position))
    (cond ((plusp count)
           (dotimes (i count (values p t))
             (multiple-value-bind (q far) (line-bracket-before buffer syntax p)
               (cond (q (setf p q))
                     (far (return (values position :far)))
                     (t (return (values 0 nil)))))))
          ((minusp count)
           (dotimes (i (- count) (values p t))
             (multiple-value-bind (q far) (line-bracket-after buffer syntax p)
               (cond (q (setf p q))
                     (far (return (values position :far)))
                     (t (return (values (tuatara.buffer:buffer-size buffer) nil)))))))
          (t (values position t)))))

(defun beginning-of-defun-position (buffer position count)
  "Where \\[beginning-of-defun] goes from POSITION in BUFFER with COUNT, and
why not as a second value when it found no definition's start: NIL, or :FAR
when the looking stopped at its limit (see BEGINNING-OF-DEFUN)."
  (multiple-value-bind (query syntax) (definition-rules buffer)
    (if query
        (multiple-value-bind (p moved) (rules-definition-move buffer query syntax position (- count) :start)
          (values p (or moved (zerop count))))
        (multiple-value-bind (p found) (bracket-definition-start buffer position count)
          (values p found)))))

(defun comment-marker-at-p (buffer position)
  "True when the comment marker of BUFFER's major mode begins at POSITION."
  (let ((marker (mode-comment-syntax (buffer-major-mode buffer))))
    (and marker
         (<= (+ position (length marker)) (tuatara.buffer:buffer-size buffer))
         (string= marker (tuatara.buffer:buffer-string buffer :start position :end (+ position (length marker)))))))

(defun end-of-defun-skip (buffer position)
  "POSITION, a definition's end, moved as \\[end-of-defun] leaves point: on
past spaces and tabs, and past the line end when only a comment or nothing
follows."
  (if (line-start-p buffer position)
      position
      (let ((p (skip-blanks buffer position :octets '(32 9))))
        (if (or (member (tuatara.buffer:buffer-char-at buffer p) '(#\Newline #\Return)) (comment-marker-at-p buffer p))
            (next-line-start buffer p)
            p))))

(defun bracket-definition-end (buffer start)
  "The end of the definition beginning at START in BUFFER: where forward-sexp
goes from there. Signals an error when its brackets do not balance."
  (multiple-value-bind (end why) (sexp-motion buffer start 1)
    (declare (ignore why))
    (or end (error "The brackets do not balance"))))

(defun bracket-end-of-defun (buffer position count)
  "Where \\[end-of-defun] goes from POSITION in BUFFER with COUNT, for
definitions that are lines beginning with an opening bracket."
  (let* ((count (if (zerop count) 1 count))
         (success nil)
         (beginning (bracket-definition-start buffer (tuatara.buffer:buffer-line-end buffer position) 1))
         (p (bracket-definition-end buffer beginning)))
    (when (<= count 1) (setf p (end-of-defun-skip buffer p)))
    (cond ((plusp count)
           ;; started inside a definition, moved past it already
           (if (> p position) (decf count) (setf p position))
           (unless (zerop count)
             (multiple-value-bind (q found) (bracket-definition-start buffer p (- count))
               (setf p q success (eq found t))
               (when success (setf p (bracket-definition-end buffer q))))))
          (t
           (if (< p position) (incf count) (setf p beginning))
           (unless (zerop count)
             (multiple-value-bind (q found) (bracket-definition-start buffer p (- count))
               (setf p q success (eq found t))
               (when success (setf beginning q p (bracket-definition-end buffer q)))))))
    (setf p (end-of-defun-skip buffer p))
    ;; going back but not got back: harder
    (loop while (and (minusp count) (>= p position) success)
          do (multiple-value-bind (q found) (bracket-definition-start buffer beginning (- count))
               (setf success (eq found t) p q)
               (if (or (>= q beginning) (not success))
                   (setf count 0)
                   (setf beginning q
                         p (end-of-defun-skip buffer (bracket-definition-end buffer q))))))
    p))

(defun end-of-defun-position (buffer position count)
  "Where \\[end-of-defun] goes from POSITION in BUFFER with COUNT (see
END-OF-DEFUN), and true as a second value unless it found no definition's end."
  (multiple-value-bind (query syntax) (definition-rules buffer)
    (if query
        (rules-definition-move buffer query syntax position (if (zerop count) 1 count) :end)
        (values (bracket-end-of-defun buffer position count) t))))

(defun push-mark-before-defun-run (command)
  "Leave the mark where point is before the first of a run of COMMAND,
unless the command before was the same, or the region is active."
  (unless (or (eq (editor-last-command *editor*) command) (region-active-p))
    (push-mark)))

(define-command beginning-of-defun ()
  "Move back to the start of the definition point is in, or of the one before;
with a count, that many definitions back, or forward to the start of the ones
after when negative. In Lisp and in text a definition starts at an opening
bracket at the start of a line, one in a string or comment not counting, and
with none before point \\[beginning-of-defun] goes to the start of the text.
In a language whose rules name its definitions (Rust's functions, impls,
structs and enums; C's functions, types and macros; shell functions) it is one
of those, one inside another counting too: \\[beginning-of-defun] goes to the
start of the one around point, or of the one before it beside it. The first
of a run leaves the mark where point was, unless the region is active."
  (multiple-value-bind (p found) (beginning-of-defun-position (current-buffer) (point) (prefix))
    (push-mark-before-defun-run 'beginning-of-defun)
    (goto p)
    (case found
      (:far (message "No definition within ~a" (scan-limit-words)))
      ((nil) (when (definition-rules (current-buffer))
               (message "No definition ~:[after~;before~] point" (plusp (prefix))))))))

(define-command end-of-defun ()
  "Move forward to the end of the definition point is in, or of the one after,
and on to the start of the next line when only blanks or a comment follow it
there; with a count, that many definitions on, or back to the ends of those
before when negative. What a definition is is said at beginning-of-defun
(\\[beginning-of-defun]). The first of a run leaves the mark where point was,
unless the region is active."
  (multiple-value-bind (p found) (end-of-defun-position (current-buffer) (point) (prefix))
    (push-mark-before-defun-run 'end-of-defun)
    (goto p)
    (unless found (message "No definition ~:[before~;after~] point" (plusp (prefix))))))

(defun blank-line-before-p (buffer line end)
  "True when the text from LINE to END in BUFFER is only spaces and tabs."
  (= line (skip-blanks buffer end :backward t :octets '(32 9))))

(defun comment-only-line-p (buffer syntax line)
  "True when the line of BUFFER beginning at LINE holds only blanks and
comments: those of SYNTAX's tree, where BUFFER has one, else a comment by the
major mode's marker."
  (let ((end (tuatara.buffer:buffer-line-end buffer line)))
    (if syntax
        (let ((rope (tuatara.buffer:buffer-text buffer)) (p line))
          (loop
            (loop while (and (< p end) (member (tuatara.text:rope-octet rope p) '(32 9 13 12))) do (incf p))
            (when (>= p end) (return t))
            (multiple-value-bind (kind start stop) (tuatara.syntax:text-kind-around syntax (1+ p))
              (unless (and (eq kind :comment) (<= start p)) (return nil))
              (setf p stop))))
        (comment-line-p buffer (tuatara.buffer:buffer-line-text buffer line)))))

(defun defun-start-with-comments (buffer position count)
  "Where going back to a definition with its comments goes from POSITION in
BUFFER with COUNT: to the start of a definition as \\[beginning-of-defun]
goes, then up over the lines of comments right above it."
  (let* ((syntax (ready-syntax buffer))
         (current (tuatara.buffer:buffer-line-start buffer (beginning-of-defun-position buffer position count))))
    (loop
      (when (zerop current) (return 0))
      (let ((eol (1- current)))
        ;; a comment of several lines, begun on a line before: from its first line
        (loop (multiple-value-bind (kind start) (and syntax (tuatara.syntax:text-kind-around syntax eol))
                (if (and (eq kind :comment) (< start (tuatara.buffer:buffer-line-start buffer eol)))
                    (setf eol (tuatara.buffer:buffer-line-end buffer start))
                    (return))))
        (let ((line (tuatara.buffer:buffer-line-start buffer eol)))
          (if (and (not (blank-line-before-p buffer line eol))
                   (comment-only-line-p buffer syntax line))
              (setf current line)
              (return (next-line-start buffer eol))))))))

(define-command mark-defun ()
  "Select the definition point is in, or the one after: point at its start,
with the comments right above it and the blank lines above those, and the mark
after it. With a count, that many definitions from there; with a negative
count, the ones before instead, and again back from there. With the region
active, add the next definition to it, or the one before when the count is
negative. What a definition is is said at beginning-of-defun
(\\[beginning-of-defun])."
  (let* ((b (current-buffer))
         (count (if (eq (editor-last-command *editor*) 'mark-defun-back) (- (prefix)) (prefix))))
    (when (minusp count) (setf (editor-this-command *editor*) 'mark-defun-back))
    (flet ((ends (from n)
             (let ((p from))
               (dotimes (i n p) (setf p (end-of-defun-position b p 1)))))
           (set-mark (p)
             (setf (tuatara.buffer:buffer-mark b) p (tuatara.buffer:buffer-mark-active-p b) t)))
      (if (and (tuatara.buffer:buffer-mark-active-p b) (tuatara.buffer:buffer-mark b))
          (if (>= count 0)
              (set-mark (ends (tuatara.buffer:buffer-mark b) count))
              (goto (defun-start-with-comments b (point) (- count))))
          (let* ((here (point))
                 (start (defun-start-with-comments b here 1))
                 (end (end-of-defun-position b start 1)))
            ;; the definition found ended before point: the one after it is wanted
            (when (or (and (<= end here) (plusp count)) (zerop start))
              (setf end (end-of-defun-position b here 1)
                    start (defun-start-with-comments b end 1)))
            ;; a new selection puts the mark before on the mark ring
            (if (plusp count)
                (progn (push-mark :position (ends start count) :quiet t :activate t) (goto start))
                (progn (goto (if (= count -1) start (beginning-of-defun-position b start (- (1+ count)))))
                       (push-mark :position end :quiet t :activate t)))))
      ;; with the blank lines above: spaces, tabs, line ends and form feeds
      (let ((p (skip-blanks b (point) :backward t :octets '(32 9 10 13 12))))
        (goto (if (zerop p) 0 (next-line-start b p)))))))
