;;;; lisp/editing/fill.lisp --- Filling: re-flowing paragraphs to a width, as Emacs's fill-paragraph does.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.defaults)

(defvar *fill-column* 80
  "The column filled lines end by, unless a buffer has its own: one given to
a fill command as its count, or set with \\[set-fill-column], kept while the
editor runs.")

(defparameter *fill-prefix-characters* "-–!|#%;>*·•‣⁃◦ 	"
  "Characters the text before a paragraph's words may be made of, kept at
the start of every line filled: an indentation, a comment's ;; or #, a
quotation's >, a list's -.")

(defun buffer-fill-column (buffer)
  "The column BUFFER's lines are filled to."
  (tuatara.buffer:buffer-local buffer :fill-column *fill-column*))

(defvar *sentence-end-double-space* nil
  "True for two spaces between sentences, as a typist writes them: a sentence
then ends at . ? ! or … followed by two spaces, a tab or a line end, and
filling keeps two spaces after a sentence's end where there were two or a line
end. NIL, one space ends a sentence and filling puts one. Either way an
abbreviation's full stop (*SENTENCE-ABBREVIATIONS*: e.g., Dr.) ends none. A
buffer whose file says which it uses (sentence-end-double-space: t in its
Local Variables: block) goes by that.")

(defun sentence-end-double-space-p (buffer)
  "True when BUFFER's sentences are two spaces apart (*SENTENCE-END-DOUBLE-SPACE*,
or what its file says)."
  (tuatara.buffer:buffer-local buffer :sentence-end-double-space *sentence-end-double-space*))

(defun read-number-as (parse prompt default valid refusal)
  "Ask for a number in the minibuffer, read from what is typed by PARSE, a
function of the text giving the number or NIL; PROMPT a format string given
DEFAULT, the number offered: the number typed, or DEFAULT when nothing is.
Asks again, saying REFUSAL first, while PARSE gives no number that VALID, a
function of it, is true for."
  (loop for asking = (format nil prompt default) then (format nil "~a ~?" refusal prompt (list default))
        do (let* ((text (string-trim " " (read-string asking)))
                  (n (and (plusp (length text)) (funcall parse text))))
             (cond ((zerop (length text)) (return default))
                   ((and n (funcall valid n)) (return n))))))

(defun read-whole-number (prompt default &key (valid #'plusp) (refusal "Not a whole number above 0."))
  "Ask for a whole number in the minibuffer, PROMPT a format string given
DEFAULT, the number offered: the number typed, or DEFAULT when nothing is.
Asks again, saying REFUSAL first, while what is typed is no whole number that
VALID, a function of it, is true for (as standard, more than 0)."
  (read-number-as (lambda (text) (and (every #'digit-char-p text) (parse-integer text)))
                  prompt default valid refusal))

(defun read-decimal-number (prompt default &key (valid #'plusp) (refusal "Not a number above 0."))
  "Ask for a number in the minibuffer, digits with perhaps a point and more
digits after (0.8), PROMPT a format string given DEFAULT, the number offered:
the number typed, exact, or DEFAULT when nothing is. Asks again, saying
REFUSAL first, while what is typed is no such number that VALID, a function
of it, is true for (as standard, more than 0)."
  (read-number-as #'tuatara.text:parse-decimal prompt default valid refusal))

(defun fraction-p (value)
  "True when VALUE is a number above 0 and at most 1, as a share or a
confidence is."
  (and (realp value) (< 0 value) (<= value 1)))

(defun read-fraction (prompt default)
  "Ask for a number above 0 and at most 1 (FRACTION-P) in the minibuffer, as
READ-DECIMAL-NUMBER asks, PROMPT a format string given DEFAULT, the number
offered."
  (read-decimal-number prompt default :valid #'fraction-p :refusal "Not a number above 0 and at most 1."))

(define-command set-fill-column ()
  "Set the column the current buffer's lines are filled to by \\[fill-paragraph]
and \\[fill-region], wrapped at by \\[soft-wrap-mode] and marked by
\\[visual-line-mode], while the editor runs: a count given first
(\\[universal-argument] 72 \\[set-fill-column]), or else the column asked for,
the buffer's offered, which \\<minibuffer>\\[minibuffer-done] alone keeps."
  (let* ((b (current-buffer))
         (was (buffer-fill-column b))
         (n (prefix-argument))
         (column (if (and (integerp n) (plusp n)) n (read-whole-number "Set fill column (now ~d): " was :refusal "Not a column."))))
    (setf (tuatara.buffer:buffer-local b :fill-column) column)
    (if (= column was)
        (message "Fill column stays ~d" column)
        (message "Fill column set to ~d (was ~d)" column was))))

(defun blank-text-p (text)
  "True when TEXT is only spaces and tabs."
  (every (lambda (c) (member c '(#\Space #\Tab))) text))

(defun fill-prefix-of (text characters)
  "The run at the start of TEXT made of CHARACTERS: a line's fill prefix."
  (subseq text 0 (or (position-if-not (lambda (c) (find c characters)) text) (length text))))

(defun comment-line-p (buffer text)
  "True when TEXT, a line of BUFFER, is a comment: its first non-blank
characters are the language's comment marker."
  (let ((marker (mode-comment-syntax (buffer-major-mode buffer)))
        (words (string-left-trim '(#\Space #\Tab) text)))
    (and marker (>= (length words) (length marker)) (string= marker words :end2 (length marker)))))

(defparameter *fill-paragraph-limit* (* 1024 1024)
  "The most octets a paragraph may span to be filled, by itself
(\\[fill-paragraph], \\<normal>\\[fill-paragraph]) or in a selection
(\\[fill-region]); a larger one is not filled, and says so, so that filling
never holds the editor up for long. Set it larger in the init file to fill
larger paragraphs.")

(defgeneric mode-unfilled-line-p (mode text)
  (:documentation "True when TEXT, a line of a buffer in major mode MODE, is no prose: it
is never filled, and it ends a paragraph (in Org a headline, a keyword line,
a table's line, a drawer's, a planning line or a fixed-width one; in Markdown
a table's line or a code fence).")
  (:method (mode text)
    (declare (ignore mode text))
    nil))

(defgeneric mode-verbatim-line-p (mode buffer line)
  (:documentation "True when the line of BUFFER, in major mode MODE, starting at LINE is
inside a block kept as written (an Org source or example block, a Markdown
code fence), which is never filled.")
  (:method (mode buffer line)
    (declare (ignore mode buffer line))
    nil))

(defgeneric mode-verbatim-after (mode text verbatim)
  (:documentation "Whether the line after the line TEXT, in a buffer of major mode MODE,
is inside a block kept as written, VERBATIM saying whether TEXT's line is
\(MODE-VERBATIM-LINE-P): what reading on line by line finds.")
  (:method (mode text verbatim)
    (declare (ignore mode text))
    verbatim))

(defvar *verbatim-carried* nil
  "While a region is filled, a cons: an anchor on the line last asked whether
it is inside a block kept as written, and the answer, so that the lines after
it are read on from there, not back from each.")

(defun verbatim-line-p (buffer line)
  "True when the line of BUFFER starting at LINE is inside a block kept as
written (MODE-VERBATIM-LINE-P), read on from the line last asked of while a
region is filled (*VERBATIM-CARRIED*)."
  (let* ((mode (buffer-major-mode buffer))
         (version (tuatara.buffer:buffer-version buffer))
         (kept *verbatim-carried*)
         (from (and kept (car kept) (tuatara.text:anchor-position (car kept) version)))
         (verbatim (if (and from (<= from line))
                       (let ((verbatim (cdr kept)))
                         (loop for p = from then (tuatara.buffer:buffer-forward-char buffer (tuatara.buffer:buffer-line-end buffer p))
                               while (< p line)
                               do (setf verbatim (mode-verbatim-after mode (tuatara.buffer:buffer-line-text buffer p) verbatim)))
                         verbatim)
                       (mode-verbatim-line-p mode buffer line))))
    (when kept
      (setf (car kept) (tuatara.text:make-anchor version line)
            (cdr kept) verbatim))
    verbatim))

(defun lines-above (buffer line function)
  "Call FUNCTION with the text of each line of BUFFER above the one starting
at LINE, the nearest first, within *FILL-PARAGRAPH-LIMIT* octets, until it
returns true; that value, or NIL."
  (let ((floor (max 0 (- line *fill-paragraph-limit*))) (p line))
    (loop while (> p floor)
          do (setf p (tuatara.buffer:buffer-line-start buffer (1- p)))
             (let ((value (funcall function (tuatara.buffer:buffer-line-text buffer p))))
               (when value (return value))))))

(defun markdown-table-or-fence-p (text)
  "True when TEXT, a line of Markdown, is a table's line (| first) or a code
fence (``` or ~~~)."
  (cl-ppcre:scan "^\\s*(?:\\||```|~~~)" text))

(defmethod mode-unfilled-line-p ((mode markdown-mode) text)
  "True for a table row or a fence line of Markdown, which filling leaves as it
is."
  (markdown-table-or-fence-p text))

(defun markdown-fence-p (text)
  "True when TEXT, a line of Markdown, is a code fence (``` or ~~~)."
  (cl-ppcre:scan "^\\s*(?:```|~~~)" text))

(defmethod mode-verbatim-line-p ((mode markdown-mode) buffer line)
  "True when LINE of BUFFER is inside a fenced block of Markdown (``` or ~~~)."
  (let ((fences 0))
    (lines-above buffer line (lambda (text) (when (markdown-fence-p text) (incf fences)) nil))
    (oddp fences)))

(defmethod mode-verbatim-after ((mode markdown-mode) text verbatim)
  "A fence begins or ends a fenced block."
  (if (markdown-fence-p text) (not verbatim) verbatim))

(defun fill-separator-p (buffer text comments)
  "True when TEXT, a line of BUFFER, is not part of a paragraph of COMMENTS
(comment lines when true, else lines of text): blank, no prose in its mode
\(MODE-UNFILLED-LINE-P), a line of the other kind, or a comment with nothing
after its marker."
  (let ((comment (comment-line-p buffer text)))
    (or (blank-text-p text)
        (mode-unfilled-line-p (buffer-major-mode buffer) text)
        (not (eq (and comment t) (and comments t)))
        (and comment (blank-text-p (string-left-trim ";#/-!* 	" text))))))

(defun list-item-start-p (buffer text)
  "True when TEXT, a line of BUFFER, begins an item of a list (after its
indentation, and in a comment after the comment's marker): -, +, •, a
number with . or ), a letter with ), or an indented *. Each item is a
paragraph of its own, as Org fills one."
  (let* ((marker (mode-comment-syntax (buffer-major-mode buffer)))
         (words (string-left-trim '(#\Space #\Tab) text))
         (words (if (and marker (comment-line-p buffer text))
                    (string-left-trim (concatenate 'string marker " 	") words)
                    words)))
    (or (cl-ppcre:scan "^(?:[-+•‣◦⁃]|[0-9]+[.)]|[A-Za-z]\\))(?:[ \t]|$)" words)
        ;; * at the start of a line is an Org heading, indented an item
        (and (cl-ppcre:scan "^\\*(?:[ \t]|$)" words) (find (char text 0) '(#\Space #\Tab))))))

(defun paragraph-lines (buffer line low high separator-p &optional item-start-p)
  "Two values: the starts of the first and the last line of the paragraph of
BUFFER about LINE, a line start: the lines above it back to LOW and below it
up to HIGH, up to a line SEPARATOR-P, a function of a line's start, is true
of, and, with ITEM-START-P given, up to a line it is true of, which begins a
paragraph of its own. An error when the paragraph is larger than
*FILL-PARAGRAPH-LIMIT*."
  (let ((from line) (to line))
    (flet ((too-large ()
             (error "The paragraph is larger than ~d KB, *fill-paragraph-limit*: it is not filled"
                    (floor *fill-paragraph-limit* 1024))))
      (loop while (and (> from low) (not (and item-start-p (funcall item-start-p from))))
            do (let ((before (tuatara.buffer:buffer-line-start buffer (1- from))))
                 (when (< before (- line *fill-paragraph-limit*)) (too-large))
                 (if (funcall separator-p before) (return) (setf from before))))
      (loop (let ((next (tuatara.buffer:buffer-forward-char buffer (tuatara.buffer:buffer-line-end buffer to))))
              (when (> next (+ line *fill-paragraph-limit*)) (too-large))
              (if (or (>= next high) (= next (tuatara.buffer:buffer-line-end buffer to))
                      (funcall separator-p next)
                      (and item-start-p (funcall item-start-p next)))
                  (return)
                  (setf to next)))))
    (values from to)))

(defun paragraph-bounds (buffer position)
  "Two values: the start of the first line and the end of the last line (its
line end included) of the paragraph at POSITION in BUFFER, or NIL when
POSITION is on a line between paragraphs. A paragraph is a run of comment
lines or of lines of text, not both, an item of a list one of its own
\(LIST-ITEM-START-P); in a programming mode only comments are filled."
  (let* ((line (tuatara.buffer:buffer-line-start buffer position))
         (comments (comment-line-p buffer (tuatara.buffer:buffer-line-text buffer line))))
    (unless (or (and (typep (buffer-major-mode buffer) 'prog-mode) (not comments))
                (fill-separator-p buffer (tuatara.buffer:buffer-line-text buffer line) comments)
                (verbatim-line-p buffer line))
      (let ((size (tuatara.buffer:buffer-size buffer)))
        (multiple-value-bind (start end)
            (paragraph-lines buffer line 0 size
                             (lambda (l) (fill-separator-p buffer (tuatara.buffer:buffer-line-text buffer l) comments))
                             (lambda (l) (list-item-start-p buffer (tuatara.buffer:buffer-line-text buffer l))))
          (values start (min size (tuatara.buffer:buffer-forward-char buffer (tuatara.buffer:buffer-line-end buffer end)))))))))

(defgeneric mode-fills-strings-p (mode)
  (:documentation "True when \\[fill-paragraph] in a string of MODE's buffers
fills the paragraph of the string at point (STRING-PARAGRAPH-BOUNDS); NIL
when only comments are filled there.")
  (:method (mode) (declare (ignore mode)) nil))

(defun string-at (buffer position)
  "The start and end of the string of BUFFER that POSITION is in, or whose
opening quote mark it is at, by BUFFER's syntax tree; NIL when there is none."
  (let ((syntax (ready-syntax buffer)))
    (when syntax
      (flet ((in (p)
               (multiple-value-bind (kind start end) (tuatara.syntax:text-kind-around syntax p)
                 (and (eq kind :string) (eql (tuatara.buffer:buffer-char-at buffer start) #\")
                      (values start end)))))
        (multiple-value-bind (start end) (in position)
          (cond (start (values start end))
                ((eql (tuatara.buffer:buffer-char-at buffer position) #\")
                 (in (tuatara.buffer:buffer-forward-char buffer position)))))))))

(defun string-paragraph-bounds (buffer position)
  "Two values: the start and end of the paragraph at POSITION of a string of
BUFFER: on the string's first line, that line after the opening quote mark
alone, as a documentation string's summary; else the lines about POSITION
between blank ones, from the string's second line up to its closing quote
mark. NIL when POSITION is in no string, or the major mode fills none
(MODE-FILLS-STRINGS-P)."
  (when (mode-fills-strings-p (buffer-major-mode buffer))
    (multiple-value-bind (start end) (string-at buffer position)
      (when start
        (let ((text-start (tuatara.buffer:buffer-forward-char buffer start))
              (text-end (max (tuatara.buffer:buffer-forward-char buffer start) (tuatara.buffer:buffer-forward-char buffer end -1)))
              (first-end (tuatara.buffer:buffer-line-end buffer start)))
          (if (<= position first-end)
              (values text-start (min text-end (tuatara.buffer:buffer-forward-char buffer first-end)))
              (let ((second (tuatara.buffer:buffer-forward-char buffer first-end))
                    (line (tuatara.buffer:buffer-line-start buffer position)))
                (flet ((blank (l) (blank-text-p (tuatara.buffer:buffer-string buffer :start l :end (min text-end (tuatara.buffer:buffer-line-end buffer l))))))
                  (unless (blank line)
                    (multiple-value-bind (from to) (paragraph-lines buffer line second text-end #'blank)
                      (values from (min text-end (tuatara.buffer:buffer-forward-char buffer (tuatara.buffer:buffer-line-end buffer to))))))))))))))

(defun text-columns (text &optional (column 0))
  "The column on the screen after TEXT begun at COLUMN (the start of a line
unless given): a tab to the next tab stop (TAB-COLUMNS), a wide character
two columns, a combining mark none."
  (let ((width (tab-columns)) (before 0))
    (loop for c across text
          do (multiple-value-bind (columns next) (tuatara.display:advance-column (char-code c) column before width nil)
               (incf column columns)
               (setf before next)))
    column))

(defun sentence-end-word-p (word)
  "True when WORD ends a sentence: its last character, past any closing quotes
and brackets, is . ? ! … or ‽, and not the full stop of an abbreviation
\(ABBREVIATION-ENDING-P: e.g., Dr.)."
  (let ((end (position-if-not #'sentence-closing-char-p word :from-end t)))
    (and end (sentence-end-char-p (char word end))
         (not (and (char= (char word end) #\.) (abbreviation-ending-p (subseq word 0 (1+ end)))))
         t)))

(defun line-words (line start)
  "The words of LINE from START, each (WORD . WIDE): WIDE true when two blanks
or more, or the line's end, follow it."
  (loop for (from to) on (cl-ppcre:all-matches "[^ \\t]+" line :start start) by #'cddr
        collect (cons (subseq line from to)
                      (let ((next (position-if-not (lambda (c) (member c '(#\Space #\Tab))) line :start to)))
                        (or (null next) (>= (- next to) 2))))))

(defun fill-lines (lines column characters &key double-space)
  "LINES of one paragraph filled to COLUMN: the words re-flowed, each line
starting with the first line's prefix (made of CHARACTERS) or the second
line's after it; a paragraph of one line whose prefix is a list's mark (-,
1., a), after a comment's marker too) continues under its words. With
DOUBLE-SPACE, a word ending a sentence that had two blanks or a line end after
it is followed by two spaces. A list of strings."
  (let* ((first (let* ((prefix (fill-prefix-of (first lines) characters))
                        ;; a number or letter marking an item belongs to the prefix as a - does
                        (number (nth-value 1 (cl-ppcre:scan "^(?:[0-9]+[.)]|[A-Za-z]\\))[ \t]+" (first lines) :start (length prefix)))))
                   (if number (subseq (first lines) 0 number) prefix)))
         (rest (cond ((second lines) (fill-prefix-of (second lines) characters))
                     (t (multiple-value-bind (match groups)
                            (cl-ppcre:scan-to-strings "^(.*?)((?:[-+•‣◦⁃·–*]|[0-9]+[.)]|[A-Za-z]\\))[ \t]+)$" first)
                          (if match
                              ;; an item's lines go on under its words: its mark as spaces, a comment's marker kept
                              (concatenate 'string (aref groups 0)
                                           (make-string (- (text-columns first) (text-columns (aref groups 0))) :initial-element #\Space))
                              first)))))
         (words (loop for line in lines
                      for i from 0
                      append (line-words line (length (if (zerop i) first (fill-prefix-of line characters))))))
         (out '()) (current nil) (width 0) (gap " "))
    (loop for (word . wide) in words
          do (let ((prefix (if out rest first)))
               (cond ((null current)
                      (setf current (concatenate 'string prefix word)
                            width (text-columns current)))
                     ((<= (+ width (length gap) (text-columns word)) column)
                      (setf current (concatenate 'string current gap word))
                      (incf width (+ (length gap) (text-columns word))))
                     (t (push current out)
                        (setf current (concatenate 'string rest word)
                              width (text-columns current))))
               (setf gap (if (and double-space wide (sentence-end-word-p word)) "  " " "))))
    (when current (push current out))
    (nreverse out)))

(defparameter *help-column* 72
  "The column the lines of help made of words are filled to: a command's,
a function's or a variable's documentation, and the help on settings.")

(defun marked-words (text)
  "The words of TEXT, split at spaces and line ends; a key written into it
between its marks (SUBSTITUTE-COMMAND-KEYS) is one word, spaces and all."
  (let ((words '()) (word (make-string-output-stream)) (open nil))
    (flet ((finish ()
             (let ((w (get-output-stream-string word)))
               (when (plusp (length w)) (push w words)))))
      (loop for c across text
            do (cond ((and (not open) (member c '(#\Space #\Newline))) (finish))
                     (t (when (key-mark-p c) (setf open (not open)))
                        (write-char c word))))
      (finish))
    (nreverse words)))

(defun write-filled (stream head text &key (indent (length head)) (column *help-column*))
  "Write HEAD, then the words of TEXT filled after it into lines no longer
than COLUMN (TUATARA.DISPLAY:FILL-WORDS), the lines after the first set in by
INDENT, to STREAM; the keys TEXT names written in and marked to show as keys
\(SUBSTITUTE-COMMAND-KEYS), each word as wide as it is shown; a word longer
than a line has a line of its own."
  (let* ((gap (and (plusp (length head)) (char/= (char head (1- (length head))) #\Space)))
         (lines (tuatara.display:fill-words (marked-words (substitute-command-keys text :marks t)) column
                                            :start (+ (length head) (if gap 1 0)) :indent indent
                                            :width (lambda (c) (if (key-mark-p c) 0 (tuatara.display:char-columns c))))))
    (write-string head stream)
    (when (and gap lines) (write-char #\Space stream))
    (format stream "~{~a~^~%~}" (cons (or (first lines) "")
                                      (mapcar (lambda (line) (format nil "~va~a" indent "" line)) (rest lines))))
    (terpri stream)))

(defun write-documentation (stream doc &key (column *help-column*))
  "Write DOC, a docstring, to STREAM filled to COLUMN (WRITE-FILLED), its
parts as TUATARA.DISPLAY:DOCSTRING-PARAGRAPHS finds them, as the completion
popup shows them too: the lines of a paragraph joined and filled; a line
beginning at its start with - or * or a number and a full stop begins a list
item, the lines after it its words until a blank line or the next item, filled
under its first word; any other line set in (a table, an example; under an
item, set in further than its words) and the blank lines kept as written."
  (let ((keymap ""))
    (flet ((keep-keymap (line)
             ;; a \\<KEYMAP> names the keymap for the keys after it, in the later parts too
             (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "\\\\<([^>]+)>" line)
               (when match (setf keymap (format nil "\\<~a>" (aref groups 0)))))))
      (dolist (part (tuatara.display:docstring-paragraphs doc))
        (ecase (first part)
          (:blank (terpri stream))
          (:as-is (let ((line (second part)))
                    (write-line (substitute-command-keys (concatenate 'string keymap line) :marks t) stream)
                    (keep-keymap line)))
          (:filled (destructuring-bind (head indent words lines) (rest part)
                     (write-filled stream head (format nil "~a~{~a~^ ~}" keymap words) :indent indent :column column)
                     (mapc #'keep-keymap lines))))))))

(defun fill-paragraph-at (buffer position &key (from 0) to)
  "Fill the paragraph of BUFFER at POSITION (STRING-PARAGRAPH-BOUNDS in a
string, else PARAGRAPH-BOUNDS) to its fill column, as one change, only the
part of it from FROM to TO when they cut it; point stays by the same word. The column is counted from the window's margin, so that in
Org's indented view, which shows text under a headline further in, the lines
end at the fill column as shown. True when there was a paragraph."
  (multiple-value-bind (start end) (multiple-value-bind (start end) (string-paragraph-bounds buffer position)
                                     (if start (values start end) (paragraph-bounds buffer position)))
    (when start
      (setf start (max start from)
            end (if to (min end to) end)))
    (when (and start (< start end))
      (let* ((text (tuatara.buffer:buffer-string buffer :start start :end end))
             (ends-with-newline (and (plusp (length text)) (member (char text (1- (length text))) '(#\Newline #\Return))))
             ;; the lines without their line ends, which are made again as the buffer makes them
             (lines (cl-ppcre:split "\\r\\n|\\n|\\r" text :limit (1+ (length text))))
             (lines (if ends-with-newline (butlast lines) lines))
             (newline (tuatara.buffer:buffer-newline buffer))
             (characters (let ((marker (mode-comment-syntax (buffer-major-mode buffer))))
                           (if marker (concatenate 'string *fill-prefix-characters* marker) *fill-prefix-characters*)))
             (filled (with-output-to-string (out)
                       (loop for (line . more) on (let ((*tab-columns* (buffer-tab-width buffer)))
                                                    ;; counted from the window's margin: less what the view shows before the lines
                                                    (fill-lines lines (max 1 (- (buffer-fill-column buffer) (tuatara.display:line-prefix-columns buffer start)))
                                                                characters :double-space (sentence-end-double-space-p buffer)))
                             do (write-string line out)
                                (when (or more ends-with-newline) (write-string newline out)))))
             (point (point))
             (inside (and (<= start point end)
                          (count-if-not (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return)))
                                        (tuatara.buffer:buffer-string buffer :start start :end point)))))
        (unless (string= text filled)
          (tuatara.buffer:buffer-replace buffer start end filled)
          (when inside
            (let ((p start) (seen 0))
              (loop while (and (< seen inside) (< p (tuatara.buffer:buffer-size buffer)))
                    do (unless (member (tuatara.buffer:buffer-char-at buffer p) '(#\Space #\Tab #\Newline #\Return))
                         (incf seen))
                       (setf p (tuatara.buffer:buffer-forward-char buffer p)))
              (goto p))))
        t))))

(defun keep-fill-column (buffer column)
  "Make COLUMN BUFFER's fill column from now on, while the editor runs, and say so."
  (setf (tuatara.buffer:buffer-local buffer :fill-column) column)
  (message "Fill column ~d for this buffer" column))

(defun take-fill-column-argument (buffer)
  "When the prefix argument is a number, make it BUFFER's fill column from now
on, while the editor runs, and say so; true when it was."
  (let ((n (prefix-argument)))
    (when (and (integerp n) (plusp n))
      (keep-fill-column buffer n)
      t)))

(defvar *fill-ask-column* t
  "True to ask for the column each time \\[fill-paragraph] or \\[fill-region] fills
(\\<normal>\\[fill-paragraph] and \\[fill-region] too), the buffer's fill column
offered: \\<minibuffer>\\[minibuffer-done] fills to it, and another number typed
fills to that column and makes it the buffer's from then on, while the editor
runs. A count given first (\\[universal-argument] 72 \\[fill-paragraph]) answers
the question. NIL fills at once.")

(defun ask-fill-column (buffer)
  "Unless a count gave it, ask for the column to fill BUFFER's text to, as
*FILL-ASK-COLUMN* says: its fill column offered, a number typed made its fill
column. Asks again while what is typed is no column."
  (when (and (not (take-fill-column-argument buffer)) *fill-ask-column*)
    (let* ((column (buffer-fill-column buffer))
           (n (read-whole-number "Fill column (default ~d): " column :refusal "Not a column.")))
      (unless (= n column) (keep-fill-column buffer n)))))

(defun nothing-to-fill (buffer)
  "Say that nothing is filled at point in BUFFER, and why when a programming
mode fills only its comments (and strings) there."
  (let* ((mode (buffer-major-mode buffer))
         (line (tuatara.buffer:buffer-line-start buffer (point))))
    (if (and (typep mode 'prog-mode) (not (blank-text-p (tuatara.buffer:buffer-line-text buffer line))))
        (message "Nothing to fill here: ~a mode fills only comments~:[~; and strings~]"
                 (tuatara.modes:mode-lighter (type-of mode)) (mode-fills-strings-p mode))
        (message "Nothing to fill here"))))

(define-command fill-paragraph ()
  "Fill the paragraph at point: its words re-flowed so that no line passes
the fill column (80, *FILL-COLUMN*, or the buffer's own), the text before
them on each line kept (an indentation, a comment's ;;, a quotation's >). In
a programming language, the comment at point; in Lisp also a string's, the
lines between blank ones, its first line filled alone as a documentation
string's summary is. Asks for the column first, the buffer's offered
(*FILL-ASK-COLUMN*); with a count, \\[universal-argument] 100 \\[fill-paragraph],
fill to that column. A column given either way is kept as the buffer's while
the editor runs."
  (let ((b (current-buffer)) (p (point)))
    (if (not (or (string-paragraph-bounds b p) (paragraph-bounds b p)))
        (nothing-to-fill b)
        (progn (ask-fill-column b)
               (fill-paragraph-at b p)))))

(define-command fill-region ()
  "Fill the lines the selection touches, each paragraph in them as
fill-paragraph fills one, the lines before and after the selection left as
they are: the column asked for first, or given by a count, and kept as the
buffer's. A selection of more than *REWRITE-THRESHOLD* octets is refused."
  (let ((b (current-buffer)))
    (multiple-value-bind (start end) (tuatara.buffer:buffer-region b)
      (if (and start (< start end) (tuatara.buffer:buffer-mark-active-p b)
               (not (too-large-here-p start end "Filling")))
          (let* ((p (tuatara.buffer:buffer-line-start b start))
                 ;; to the end of the selection's last line, its line break too
                 (last (if (= end (tuatara.buffer:buffer-line-start b end))
                           end
                           (tuatara.buffer:buffer-forward-char b (tuatara.buffer:buffer-line-end b end))))
                 (ends (tuatara.text:make-anchor (tuatara.buffer:buffer-version b) last :bias :after))
                 (count 0)
                 (*verbatim-carried* (list nil)))
            (ask-fill-column b)
            (loop while (< p (tuatara.text:anchor-position ends (tuatara.buffer:buffer-version b)))
                  do (multiple-value-bind (s e) (paragraph-bounds b p)
                       (if s
                           (progn (fill-paragraph-at b p :from (tuatara.buffer:buffer-line-start b start)
                                                         :to (tuatara.text:anchor-position ends (tuatara.buffer:buffer-version b)))
                                  (incf count)
                                  (multiple-value-bind (s2 e2) (paragraph-bounds b s)
                                    (declare (ignore s2))
                                    (setf p (or e2 e))))
                           (let ((next (tuatara.buffer:buffer-forward-char b (tuatara.buffer:buffer-line-end b p))))
                             (when (= next p) (return))
                             (setf p next)))))
            (setf (tuatara.buffer:buffer-mark-active-p b) nil)
            (if (and (zerop count) (typep (buffer-major-mode b) 'prog-mode))
                (message "Nothing filled: ~a mode fills only comments"
                         (tuatara.modes:mode-lighter (type-of (buffer-major-mode b))))
                (message "Filled ~d paragraph~:p to column ~d" count (buffer-fill-column b))))
          (unless (and start (< start end) (tuatara.buffer:buffer-mark-active-p b))
            (message "No selection: fill-region was not run"))))))
