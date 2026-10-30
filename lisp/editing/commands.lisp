;;;; lisp/editing/commands.lisp --- Base editing commands with Emacs-like behaviour.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; One implementation per behaviour: the standard bindings and the modal keys
;;;; both call these.

(in-package #:tuatara.defaults)

(defun prefix (&optional (default 1))
  "The numeric prefix argument of the current command, or DEFAULT."
  (or (prefix-argument) default))

(defparameter *insert-count-limit* 10000000
  "The most characters or lines a count may insert at once (C-u 100 x): more
would take the editor away for too long.")

(defun repeated-text (text count)
  "TEXT COUNT times over, as one string."
  (let ((out (make-string (* count (length text)))))
    (dotimes (i count out)
      (replace out text :start1 (* i (length text))))))

(defun insert-count ()
  "The count of a command that inserts that many: the prefix argument, or 1;
an error when it is negative or more than *INSERT-COUNT-LIMIT*."
  (let ((n (prefix)))
    (cond ((minusp n) (error "Repetition argument has to be non-negative"))
          ((> n *insert-count-limit*)
           (error "~:d is more than can be inserted at once (at most ~:d)" n *insert-count-limit*))
          (t n))))

;;; Point kept out of folds

(defvar *point-before-command* nil
  "The buffer of the selected window and its point before the command running, (BUFFER . POINT).")

(defvar *version-before-command* nil
  "The version of the selected window's buffer before the command running, to
tell whether the command changed it.")

(defun note-point-before-command (editor command)
  "Pre-command function: remember the selected buffer's point and version, for
KEEP-POINT-OUT-OF-FOLDS and the functions run after a command that look at
what it changed."
  (declare (ignore command))
  (let ((buffer (tuatara.display:window-buffer (editor-window editor))))
    (setf *point-before-command* (cons buffer (tuatara.buffer:buffer-point buffer))
          *version-before-command* (tuatara.buffer:buffer-version buffer))))

(defun keep-point-out-of-folds (editor)
  "Post-command function: a point the command moved into a fold, or into the
lines search filters hide, goes to its end when it came from before the
fold, else to its start (TUATARA.DISPLAY:POINT-OUT-OF-FOLDS)."
  (let* ((buffer (tuatara.display:window-buffer (editor-window editor)))
         (before (and (eq (car *point-before-command*) buffer) (cdr *point-before-command*))))
    (when (tuatara.display:buffer-hides-p buffer)
      (tuatara.display:point-out-of-folds buffer before))))

(defun widen-to-show (buffer byte)
  "Widen BUFFER when the octet BYTE is outside the part of it shown, saying
so, for a jump there: one never lands at the edge instead. Made while
BUFFER's narrowing is lifted for a change, the jump has it stay widened."
  (when (or (/= byte (tuatara.buffer:within-narrowing buffer byte))
            (tuatara.buffer:lifted-narrowing-outside-p buffer byte))
    (tuatara.buffer:widen-buffer buffer)
    (tuatara.buffer:let-go-of-lifted-narrowing buffer)
    (let ((line (tuatara.buffer:buffer-known-line-number buffer byte)))
      ;; seen whatever the command says after it
      (say-with-command-messages "Widened ~a to show line ~:[…~;~:*~:d~]" (tuatara.buffer:buffer-name buffer) (and line (1+ line))))))

(defun goto-shown (buffer byte &key (open byte))
  "Put BUFFER's point at the octet BYTE, the fold that hides OPEN (BYTE unless
given) opened first, so that point is never inside what is hidden; BUFFER
widened first, and that said, when BYTE is outside the part of it shown."
  (widen-to-show buffer byte)
  (let ((fold (tuatara.display:fold-at buffer open)))
    (when fold (tuatara.display:unfold-region buffer (car fold) (cdr fold))))
  (setf (tuatara.buffer:buffer-point buffer) byte))

(defun point () "Point of the current buffer." (tuatara.buffer:buffer-point (current-buffer)))
(defun goto (byte) "Move point of the current buffer to BYTE." (setf (tuatara.buffer:buffer-point (current-buffer)) byte))

;;; Motion

(defun cluster-forward (buffer byte &optional (count 1))
  "Byte offset COUNT characters as a reader counts them after BYTE in BUFFER
\(before, for a negative COUNT), clamped to the text: grapheme clusters, a
letter with its accents or an emoji with its skin tone one step
\(TUATARA.TEXT:ROPE-NEXT-BOUNDARY), a carriage return and the newline after it
one step in a :CRLF buffer (TUATARA.BUFFER:BUFFER-FORWARD-CHAR)."
  (let ((rope (tuatara.buffer:buffer-text buffer)))
    (loop repeat (abs count)
          for next = (if (plusp count)
                         (max (tuatara.buffer:buffer-forward-char buffer byte)
                              (tuatara.text:rope-next-boundary rope byte t))
                         (min (tuatara.buffer:buffer-forward-char buffer byte -1)
                              (tuatara.text:rope-previous-boundary rope byte t)))
          until (= next byte)
          do (setf byte next))
    byte))

(defun rectangle-char-motion (count)
  "Move the corner of the rectangle at point COUNT characters along its line
(back when negative): past the end of the line a column at a time, where the
corner stands in the columns beyond, but never back past its start."
  (let* ((b (current-buffer))
         (p (point))
         (start (tuatara.buffer:buffer-line-start b p))
         (end (tuatara.buffer:buffer-line-end b p))
         ;; the columns the corner stands past the line's end
         (beyond (- (rectangle-point-column b) (tuatara.display:buffer-screen-column b p)))
         (n (abs count)))
    (if (plusp count)
        (loop while (and (plusp n) (zerop beyond) (< p end))
              do (setf p (cluster-forward b p)) (decf n)
              finally (incf beyond n))
        (let ((back (min n beyond)))
          (decf beyond back) (decf n back)
          (loop while (and (plusp n) (> p start))
                do (setf p (cluster-forward b p -1)) (decf n))))
    (goto p)
    (keep-rectangle-column b (+ (tuatara.display:buffer-screen-column b p) beyond))))

(defun char-motion (count)
  "Move point COUNT characters on, back when negative, across lines, a letter
with its accents or an emoji with its skin tone one character (CLUSTER-FORWARD);
in the hex view, COUNT octets."
  (unless (hex-motion :octet count)
    (goto (cluster-forward (current-buffer) (point) count))))

(defun edge-words (forward narrowed)
  "What is said of a motion or a change that found nowhere to go: \"End of
buffer\" when FORWARD, else \"Beginning of buffer\"; \"End of the part
shown\" and \"Beginning of the part shown\" when NARROWED."
  (if narrowed
      (if forward "End of the part shown" "Beginning of the part shown")
      (if forward "End of buffer" "Beginning of buffer")))

(defun fail-play-at-edge (forward)
  "While recorded keys play, fail the play (NOTE-COMMAND-FAILED): a motion
found nowhere to go at the end of the current buffer when FORWARD, else at its
start (of the part shown, when it is narrowed), which the echo area says
\(EDGE-WORDS). Outside a play, nothing is said."
  (when (keys-playing-p)
    (message "~a" (edge-words forward (tuatara.buffer:buffer-narrowed-p (current-buffer))))
    (note-command-failed)))

(defun motion-place (by-line)
  "Where the cursor is, as a motion compares it before and after: in the hex
view, its octet; with BY-LINE, the start of its line, or of its row where long
lines wrap at a word's edge; else point."
  (let ((b (current-buffer)))
    (cond ((hex-view-here-p) (tuatara.display:hex-point b))
          ((not by-line) (point))
          ((and (tuatara.display:soft-wrap-of b) (not (rectangle-live-p b)))
           (tuatara.display:window-row-start (current-window) (point)))
          (t (tuatara.buffer:buffer-line-start b (point))))))

(declaim (inline call-failing-play-at-edge))
(defun call-failing-play-at-edge (count by-line motion)
  "Call MOTION, a function moving the cursor COUNT characters, or lines with
BY-LINE (back when COUNT is negative). While recorded keys play, a motion that
leaves the cursor where it was, or on the same line, has met the buffer's start
or end and fails the play (FAIL-PLAY-AT-EDGE)."
  (if (or (zerop count) (not (keys-playing-p)))
      (funcall motion)
      (let ((before (motion-place by-line)))
        (funcall motion)
        (when (= before (motion-place by-line))
          (fail-play-at-edge (plusp count))))))

(define-command forward-char ()
  "Move point forward one character (or the prefix argument's worth); in the
hex view, one octet. In a rectangle, the corner at point moves on past the end
of the line rather than on to the next. While recorded keys play, moving at
the end of the buffer fails the play."
  (let ((n (prefix)))
    (if (rectangle-live-p (current-buffer))
        (rectangle-char-motion n)
        (call-failing-play-at-edge n nil (lambda () (char-motion n))))))

(define-command backward-char ()
  "Move point backward one character (or the prefix argument's worth); in the
hex view, one octet. In a rectangle, the corner at point stops at the start of
the line. While recorded keys play, moving at the start of the buffer fails
the play."
  (let ((n (- (prefix))))
    (if (rectangle-live-p (current-buffer))
        (rectangle-char-motion n)
        (call-failing-play-at-edge n nil (lambda () (char-motion n))))))

(defun move-to-line-start ()
  "Move point to the start of its line; in the hex view, of its row of octets."
  (unless (hex-motion :row-start)
    (goto (tuatara.buffer:buffer-line-start (current-buffer) (point)))))

(defun move-to-line-end ()
  "Move point to the end of its line, before its newline; in the hex view, of
its row of octets."
  (unless (hex-motion :row-end)
    (goto (tuatara.buffer:buffer-line-end (current-buffer) (point)))))

(defun soft-wrap-row (buffer)
  "The row of the selected window that point is on, where BUFFER's long lines
wrap at a word's edge (\\[soft-wrap-mode]) and no rectangle is live; else NIL."
  (when (and (tuatara.display:soft-wrap-of buffer) (not (rectangle-live-p buffer)))
    (let ((window (current-window)))
      (aref (tuatara.display:window-layout-rows window (tuatara.display:window-row-start window (point)) 1) 0))))

(define-command beginning-of-line ()
  "Move point to the start of the current line; where long lines wrap at a
word's edge (\\[soft-wrap-mode]), of the screen row it is on; in the hex
view, of the row of octets."
  (let ((row (soft-wrap-row (current-buffer))))
    (if row
        (goto (tuatara.display:row-start row))
        (move-to-line-start))))

(define-command end-of-line ()
  "Move point to the end of the current line, before its newline; where long
lines wrap at a word's edge (\\[soft-wrap-mode]), of the screen row it is
on; in the hex view, of the row of octets."
  (let ((row (soft-wrap-row (current-buffer))))
    (if row
        (goto (row-position row most-positive-fixnum))
        (move-to-line-end))))

(defun push-mark-before-jump ()
  "Leave the mark where point is before a jump, so you can come back to it,
unless the region is active or a count was given."
  (unless (or (region-active-p) (prefix-argument))
    (push-mark)))

(define-command beginning-of-buffer ()
  "Move point to the start of the buffer. In a narrowed buffer, to the start of
the part shown (\\[narrow-to-region]). The mark is left where point was
\(the mark before on the mark ring), unless the region is active."
  (unless (hex-motion :start)
    (push-mark-before-jump)
    (goto 0)))

(define-command end-of-buffer ()
  "Move point to the end of the buffer. In a narrowed buffer, to the end of
the part shown (\\[narrow-to-region]). The mark is left where point was
\(the mark before on the mark ring), unless the region is active."
  (unless (hex-motion :end)
    (push-mark-before-jump)
    (goto (tuatara.buffer:buffer-size (current-buffer)))))

(defvar *vertical-motion-commands* '(next-line previous-line)
  "The commands that move point up and down: run one after another, they keep
to the column the first started from.")

(defun text-line-start-away (buffer position lines)
  "The start of the line LINES lines below the one holding POSITION in BUFFER
(above when negative), the first or last line when there are fewer, folds or
not: by the line numbers where they are known, else by looking for the
newlines, so that moving never waits for a large file's lines to be counted."
  (if (tuatara.buffer:buffer-indexed-p buffer)
      (let ((line (tuatara.buffer:buffer-line-number buffer position)))
        (tuatara.buffer:buffer-goto-line
         buffer (max 0 (min (+ line lines) (1- (tuatara.buffer:buffer-line-count buffer))))))
      (tuatara.display:step-lines (tuatara.buffer:buffer-text buffer) position lines)))

(defun lines-between (buffer from to)
  "How many lines there are from the line starting at FROM in BUFFER down to
the one starting at TO, FROM not after TO: by the line numbers where they are
known, else by looking for the newlines between."
  (if (tuatara.buffer:buffer-indexed-p buffer)
      (- (tuatara.buffer:buffer-line-number buffer to) (tuatara.buffer:buffer-line-number buffer from))
      (let ((rope (tuatara.buffer:buffer-text buffer)) (n 0) (line from))
        (loop while (< line to)
              do (setf line (1+ (tuatara.text:rope-line-end rope line)))
                 (incf n))
        n)))

(defun visible-line-start-away (buffer position lines)
  "The start of the line LINES lines a window shows below the one holding
POSITION in BUFFER (above when negative), the first or last when there are
fewer: a line whose end a fold hides, and the text the fold hides, count as
one line. The lines between folds are stepped over at once, as
TEXT-LINE-START-AWAY steps; each fold on the way is found by halving."
  (let ((pos (tuatara.display:visible-line-start buffer position))
        (left (abs lines)))
    (if (plusp lines)
        (loop
          (let ((target (text-line-start-away buffer pos left))
                (fold (tuatara.display:fold-from buffer pos)))
            (when (or (null fold) (>= (car fold) target)) (return target))
            ;; the plain lines down to the one the fold hides the end of, then that line as one
            (let ((line (tuatara.buffer:buffer-line-start buffer (car fold))))
              (decf left (lines-between buffer pos line))
              (when (<= left 0) (return line))
              (let* ((end (tuatara.display:visible-line-end buffer line))
                     (next (text-line-start-away buffer end 1)))
                (when (<= next end) (return line))
                (setf pos next)
                (when (zerop (decf left)) (return pos))))))
        (loop
          (let ((target (text-line-start-away buffer pos (- left)))
                (fold (tuatara.display:fold-before buffer pos)))
            (when (or (null fold) (< (cdr fold) target)) (return target))
            ;; the plain lines up to the one after the fold, then the fold's line as one
            (let ((after (text-line-start-away buffer (cdr fold) 1)))
              (when (or (<= after (cdr fold)) (> after pos)) (return target))
              (decf left (lines-between buffer after pos))
              (when (<= left 0) (return target))
              (setf pos (tuatara.display:visible-line-start buffer (1- after)))
              (when (zerop (decf left)) (return pos))))))))

(defun line-start-away (buffer position lines)
  "The start of the line LINES lines below the one holding POSITION in BUFFER
(above when negative), the first or last line when there are fewer. Lines
are those a window shows: where BUFFER has folds, a line whose end a fold
hides goes with the text hidden as one line (VISIBLE-LINE-START-AWAY), and
the lines search filters hide are passed over."
  (if (and (/= lines 0) (tuatara.display:buffer-hides-p buffer))
      (visible-line-start-away buffer position lines)
      (text-line-start-away buffer position lines)))

(defun vertical-motion (lines)
  "Move point LINES lines down (up if negative), keeping the goal column across
consecutive vertical motions; in the hex view, LINES rows of octets. The corner
of a rectangle keeps its column on a shorter line, standing past its end."
  (when (hex-motion :row lines) (return-from vertical-motion nil))
  (when (and (tuatara.display:soft-wrap-of (current-buffer)) (not (rectangle-live-p (current-buffer))))
    (return-from vertical-motion (row-motion (current-window) (current-buffer) lines)))
  (let* ((b (current-buffer)) (w (current-window))
         (vertical-p (member (editor-last-command *editor*) *vertical-motion-commands*))
         ;; the column on the screen, a tab and a wide character as wide as shown; a
         ;; rectangle's corner where it stands, past a short line's end too
         (goal (or (and vertical-p (tuatara.display:window-goal-column w))
                   (rectangle-point-column b)))
         (line (line-start-away b (point) lines)))
    (setf (tuatara.display:window-goal-column w) goal)
    (goto (tuatara.display:buffer-move-to-screen-column b line goal))
    (keep-rectangle-column b goal)))

(defun row-position (row column)
  "The position COLUMN columns into ROW, or where ROW's text ends when it is
shorter: its line's end, or on a row that goes on in the next, its last
character. A column of what is shown before the row's text (Org's indented
view), standing for no character, is the text's first."
  (let ((n (tuatara.display:row-columns row)) (bytes (tuatara.display:row-bytes row)))
    (cond ((and (< column n) (minusp (aref bytes column)))
           (or (find-if-not #'minusp bytes :start column) (tuatara.display:row-shown-end row)))
          ((< column n) (aref bytes column))
          ((and (plusp n) (not (tuatara.display:row-last-p row))
                (= (tuatara.display:row-shown-end row) (tuatara.display:row-end row)))
           (aref bytes (1- n)))
          (t (tuatara.display:row-shown-end row)))))

(defun row-motion (window buffer lines)
  "Move point LINES rows of WINDOW down (up when negative), where BUFFER's long
lines wrap at a word's edge, keeping its column in the row across vertical
motions one after another."
  (let* ((point (tuatara.buffer:buffer-point buffer))
         (vertical-p (member (editor-last-command *editor*) *vertical-motion-commands*))
         (start (tuatara.display:window-row-start window point))
         (here (aref (tuatara.display:window-layout-rows window start 1) 0))
         (goal (or (and vertical-p (tuatara.display:window-goal-column window))
                   (tuatara.display:row-column-of here point)
                   0))
         (target (if (plusp lines)
                     (let ((rows (tuatara.display:window-layout-rows window start (1+ lines))))
                       (aref rows (1- (length rows))))
                     (aref (tuatara.display:window-layout-rows
                            window (tuatara.display:rows-back window point (- lines)) 1)
                           0))))
    (setf (tuatara.display:window-goal-column window) goal)
    (goto (row-position target goal))))

(define-command next-line ()
  "Move point down one line (or the prefix argument's worth), keeping the column when possible;
where long lines wrap at a word's edge (\\[soft-wrap-mode]), one row. While
recorded keys play, moving down on the last line fails the play."
  (let ((n (prefix)))
    (call-failing-play-at-edge n t (lambda () (vertical-motion n)))))

(define-command previous-line ()
  "Move point up one line (or the prefix argument's worth), keeping the column when possible;
where long lines wrap at a word's edge (\\[soft-wrap-mode]), one row. While
recorded keys play, moving up on the first line fails the play."
  (let ((n (- (prefix))))
    (call-failing-play-at-edge n t (lambda () (vertical-motion n)))))

(define-command forward-word ()
  "Move point to the end of the next word (or the prefix argument's worth,
back with a negative one): past what is not a word, then past the word. No
further than *TEXT-SCAN-LIMIT* (64 MB), which is said at once."
  (let ((n (prefix)))
    (goto (if (minusp n) (words-backward (current-buffer) (point) (- n)) (words-forward (current-buffer) (point) n)))))

(define-command backward-word ()
  "Move point to the start of the previous word (or the prefix argument's
worth, forward with a negative one). No further than *TEXT-SCAN-LIMIT* (64
MB), which is said at once."
  (let ((n (prefix)))
    (goto (if (minusp n) (words-forward (current-buffer) (point) (- n)) (words-backward (current-buffer) (point) n)))))

(defun parse-line-number (string)
  "The integer in STRING, ignoring the commas, underscores and spaces people
use to group digits (100,000,000); NIL when there is none."
  (parse-integer (remove-if (lambda (c) (find c ",_ ")) string) :junk-allowed t))

(define-command goto-line ()
  "Move point to the start of a line number read from the minibuffer (1-based),
the mark left where it was unless the region is active. Digits may be grouped as the mode line shows them: 100,000,000. In the hex view,
go to an offset instead (0x for hex). While a large file's lines are still
being counted, a line in its first 64 MB is found at once; one further in
waits until the count has passed it, not for the whole file. A line outside
the part shown of a narrowed buffer widens it, and that is said."
  (if (hex-view-here-p)
      (hex-goto-offset)
      (let ((n (parse-line-number (read-string "Go to line: ")))
            (b (current-buffer)))
        (if n
            (progn
              (unless (region-active-p b) (push-mark))
              (goto-shown b (or (and (not (tuatara.buffer:buffer-indexed-p b))
                                     (or (tuatara.buffer:buffer-scan-line b (max 0 (1- n)))
                                         (wait-for-counting b (max 0 (1- n)))))
                                (tuatara.buffer:buffer-goto-line b (max 0 (1- n))))))
            (message "Not a number")))))

(defun scroll-by-count (window direction)
  "Scroll WINDOW as a scroll command does, DIRECTION 1 forward or -1 back: with
a prefix argument, that many rows, the other way when it is negative; else by
nearly a screenful, the rows it shows, where long lines wrap."
  (tuatara.display:window-scroll window (* direction (if (prefix-argument)
                                                         (prefix)
                                                         (max 1 (- (tuatara.display:window-height window) 2))))))

(define-command scroll-up-command ()
  "Scroll the window forward by nearly a screenful: by the rows it shows, where
long lines wrap. With a count, that many rows; back when it is negative."
  (scroll-by-count (current-window) 1))

(define-command scroll-down-command ()
  "Scroll the window backward by nearly a screenful: by the rows it shows, where
long lines wrap. With a count, that many rows; forward when it is negative."
  (scroll-by-count (current-window) -1))

(defparameter *wheel-scroll-lines* 3
  "How many lines a turn of the mouse wheel scrolls.")

(define-command scroll-by-wheel ()
  "Scroll the window under the mouse, else the selected one, as the mouse wheel
turned: *WHEEL-SCROLL-LINES* lines on for \\`<wheel-down>', back for
\\`<wheel-up>'; that window's point stays in view."
  (let* ((key (editor-this-key *editor*))
         (column (and key (tuatara.frontend:key-column key)))
         (row (and key (tuatara.frontend:key-row key))))
    (tuatara.display:window-scroll (or (and row (window-at-cell column row)) (current-window))
                                   (* (if (and key (eq (tuatara.frontend:key-base key) :wheel-up)) -1 1)
                                      *wheel-scroll-lines*))))

(define-command recenter ()
  "Scroll so that the current line is in the middle of the window."
  (tuatara.display:window-recenter (current-window)))

;;; Balanced expressions

(defun sexp-boundary-p (char)
  "True for characters that end a symbol: whitespace and brackets."
  (or (null char) (member char '(#\Space #\Tab #\Newline #\Return)) (find char tuatara.text:*openers*) (find char tuatara.text:*closers*)))

(defparameter *text-scan-limit* (* 64 1024 1024)
  "The most octets looked through, about a tenth of a second's worth, for
what is found by the text alone: a bracket's partner (past it the brackets
count as not balancing), a paragraph's end, a delimiter's pair. So that a
stray bracket, or a file of gigabytes without a blank line, costs no more.")

(defun scan-limit-words ()
  "*TEXT-SCAN-LIMIT* in words: \"64 MB\"."
  (let ((mb (/ *text-scan-limit* (* 1024 1024))))
    (if (>= mb 1) (format nil "~d MB" (round mb)) (format nil "~d bytes" *text-scan-limit*))))

(defparameter *sexp-boundary-octets*
  (append tuatara.text:*blank-octets* tuatara.text:*bracket-octets*)
  "The octets of the characters that end a symbol (SEXP-BOUNDARY-P).")

(defun symbol-run-end (buffer position &key backward)
  "Where the symbol at POSITION of BUFFER ends, or with BACKWARD where the one
before POSITION begins: the next character that ends a symbol
(SEXP-BOUNDARY-P), or the end of the text. Refused when *TEXT-SCAN-LIMIT*
comes first."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (size (tuatara.text:rope-bytes rope))
         (scan (if backward (max 0 (- position *text-scan-limit*)) (min size (+ position *text-scan-limit*))))
         (found (if backward
                    (tuatara.text:rope-find-any-octet rope *sexp-boundary-octets* :start scan :end position :from-end t)
                    (tuatara.text:rope-find-any-octet rope *sexp-boundary-octets* :start position :end scan))))
    (cond (found (if backward (1+ found) found))
          ((= scan (if backward 0 size)) scan)
          (t (error "No ~:[end~;start~] of the expression within ~a" backward (scan-limit-words))))))

;;; Words, read by their octets

(defparameter *word-or-wide-octets*
  (loop for octet below 256 when (or (>= octet 128) (alphanumericp (code-char octet))) collect octet)
  "The octets that may make a word character (WORD-CHAR-P): an ASCII letter or
digit, or any octet past ASCII, which only its character can tell.")

(defparameter *not-ascii-word-octets*
  (loop for octet below 256 unless (and (< octet 128) (alphanumericp (code-char octet))) collect octet)
  "The octets that are not an ASCII letter or digit.")

(defun word-run-end (buffer position word limit)
  "The first position from POSITION in BUFFER, below LIMIT, whose character is
not a word character (WORD-CHAR-P) when WORD is true, or is one when WORD is
NIL; LIMIT when there is none. Octets that settle nothing, ASCII ones, are
passed over a window at a time."
  (let ((rope (tuatara.buffer:buffer-text buffer)) (p position))
    (loop
      (when (>= p limit) (return limit))
      (let ((q (tuatara.text:rope-find-any-octet rope (if word *not-ascii-word-octets* *word-or-wide-octets*)
                                                 :start p :end limit)))
        (cond ((null q) (return limit))
              ((< (tuatara.text:rope-octet rope q) 128) (return q))
              ((eq (not (word-char-p (tuatara.buffer:buffer-char-at buffer q))) (not word))
               (setf p (tuatara.buffer:buffer-forward-char buffer q)))
              (t (return q)))))))

(defun word-run-start (buffer position word limit)
  "The last position back from POSITION in BUFFER, above LIMIT, where the
character before is not a word character (WORD-CHAR-P) when WORD is true, or
is one when WORD is NIL; LIMIT when there is none. Octets that settle nothing,
ASCII ones, are passed over a window at a time."
  (let ((rope (tuatara.buffer:buffer-text buffer)) (p position))
    (loop
      (when (<= p limit) (return limit))
      (let ((q (tuatara.text:rope-find-any-octet rope (if word *not-ascii-word-octets* *word-or-wide-octets*)
                                                 :start limit :end p :from-end t)))
        (cond ((null q) (return limit))
              ((< (tuatara.text:rope-octet rope q) 128) (return (1+ q)))
              (t
               ;; the octet past ASCII ends a character: that character decides
               (let ((start (tuatara.buffer:buffer-forward-char buffer (1+ q) -1)))
                 (if (eq (not (word-char-p (tuatara.buffer:buffer-char-at buffer start))) (not word))
                     (setf p start)
                     (return (1+ q))))))))))

(defun words-forward (buffer position count &optional (limit (tuatara.buffer:buffer-end buffer)))
  "Where COUNT words forward of POSITION end in BUFFER, as forward-word goes:
past what is not a word, then past the word; no further than LIMIT (the end
of the text, or of the part shown of a narrowed buffer), where it stops at
once whatever the count. Refused when *TEXT-SCAN-LIMIT* comes first."
  (let* ((scan (min limit (+ position *text-scan-limit*))) (p position))
    (loop repeat count
          until (>= p limit)
          do (setf p (word-run-end buffer p nil scan))
             (when (and (= p scan) (< scan limit)) (error "No word within ~a" (scan-limit-words)))
             (setf p (word-run-end buffer p t scan))
             (when (and (= p scan) (< scan limit)) (error "No end of the word within ~a" (scan-limit-words))))
    p))

(defun words-backward (buffer position count &optional (limit (tuatara.buffer:buffer-start buffer)))
  "Where COUNT words back from POSITION begin in BUFFER, as backward-word goes;
no further back than LIMIT (the start of the text, or of the part shown of a
narrowed buffer), where it stops at once whatever the count.
Refused when *TEXT-SCAN-LIMIT* comes first."
  (let* ((scan (max limit (- position *text-scan-limit*))) (p position))
    (loop repeat count
          until (<= p limit)
          do (setf p (word-run-start buffer p nil scan))
             (when (and (= p scan) (> scan limit)) (error "No word within ~a before point" (scan-limit-words)))
             (setf p (word-run-start buffer p t scan))
             (when (and (= p scan) (> scan limit)) (error "No start of the word within ~a" (scan-limit-words))))
    p))

(defun sexp-end-after (buffer position)
  "The byte offset where the balanced expression after POSITION in BUFFER
ends, by the text alone: skipping whitespace, then a bracketed group or a
symbol. Brackets in strings and comments are not told apart. A second value is
false when the brackets do not balance (no partner before the end of the text,
or within *TEXT-SCAN-LIMIT*); the offset is then just after the bracket. A
symbol without its end within *TEXT-SCAN-LIMIT* is refused."
  (let ((p (skip-blanks buffer position)) (balanced t))
    (let ((c (tuatara.buffer:buffer-char-at buffer p)))
      (cond ((null c))
            ((find c tuatara.text:*openers*)
             (let ((end (tuatara.text:rope-bracket-match (tuatara.buffer:buffer-text buffer) p
                                                         :limit *text-scan-limit*)))
               (if end
                   (setf p end)
                   (setf p (tuatara.buffer:buffer-forward-char buffer p) balanced nil))))
            ((find c tuatara.text:*closers*))
            (t (setf p (symbol-run-end buffer p)))))
    (values p balanced)))

(defun sexp-start-before (buffer position)
  "The byte offset where the balanced expression ending at or before POSITION
in BUFFER begins, by the text alone: skipping whitespace backward, then a
bracketed group or a symbol with any quote before it. Brackets in strings and
comments are not told apart. A second value is false when the brackets do not
balance (no partner after the start of the text, or within
*TEXT-SCAN-LIMIT*); the offset is then that of the bracket. A symbol without
its start within *TEXT-SCAN-LIMIT* is refused."
  (let ((p position) (balanced t))
    (flet ((before (p) (and (plusp p) (tuatara.buffer:buffer-char-at buffer (tuatara.buffer:buffer-forward-char buffer p -1)))))
      (setf p (skip-blanks buffer p :backward t))
      (let ((c (before p)))
        (cond ((null c))
              ((find c tuatara.text:*closers*)
               (let ((start (tuatara.text:rope-bracket-match (tuatara.buffer:buffer-text buffer) p :backward t
                                                             :limit *text-scan-limit*)))
                 (if start
                     (setf p start)
                     (setf p (tuatara.buffer:buffer-forward-char buffer p -1) balanced nil))))
              ((find c tuatara.text:*openers*))
              (t (setf p (symbol-run-end buffer p :backward t))
                 ;; a quote or other prefix character belongs to the expression
                 (loop while (and (plusp p) (member (before p) '(#\' #\` #\, #\# #\@)))
                       do (setf p (tuatara.buffer:buffer-forward-char buffer p -1)))))))
    (values p balanced)))

(defun ready-syntax (buffer)
  "BUFFER's syntax tree once it is up to date, after a moment's wait for a
parse under way; NIL when BUFFER is not parsed."
  (let ((syntax (tuatara.syntax:buffer-syntax buffer)))
    (and syntax (tuatara.syntax:syntax-ready-tree syntax) syntax)))

(defun skip-blanks (buffer position &key backward (octets tuatara.text:*blank-octets*))
  "POSITION of BUFFER moved over spaces, tabs and line ends, or over the
characters of OCTETS, a list of ASCII codes: forward to the first character
that is none of them, or the end of the text; with BACKWARD, back to just after
the last one before it that is none of them, or the start."
  (tuatara.text:rope-skip-octets (tuatara.buffer:buffer-text buffer) position octets :backward backward))

(defun sexp-end-at (buffer position)
  "Where the balanced expression after POSITION in BUFFER ends, as
forward-sexp moves: the position after it, or the end of the text when only
blanks follow. Else NIL and why, as a second value: :END when a closing
bracket comes first, POSITION being at the end of the expression around it;
:UNBALANCED when the next bracket has no partner."
  (let* ((syntax (ready-syntax buffer))
         (end (if syntax (tuatara.syntax:expression-end syntax position) :text)))
    (case end
      ((nil) (values nil :end))
      (:unbalanced (values nil :unbalanced))
      (:text (if (find (tuatara.buffer:buffer-char-at buffer (skip-blanks buffer position)) tuatara.text:*closers*)
                 (values nil :end)
                 (multiple-value-bind (end balanced) (sexp-end-after buffer position)
                   (if balanced (values end nil) (values nil :unbalanced)))))
      (t (values end nil)))))

(defun sexp-start-at (buffer position)
  "Where the balanced expression before POSITION in BUFFER starts, as
backward-sexp moves, a quote before it included: its first position, or the
start of the text when only blanks come before. Else NIL and why: :START when
an opening bracket comes first, POSITION being at the start of the
expression around it; :UNBALANCED when the bracket before has no partner."
  (let* ((syntax (ready-syntax buffer))
         (start (if syntax (tuatara.syntax:expression-start syntax position) :text)))
    (case start
      ((nil) (values nil :start))
      (:unbalanced (values nil :unbalanced))
      (:text (let ((before (skip-blanks buffer position :backward t)))
               (if (and (plusp before)
                        (find (tuatara.buffer:buffer-char-at buffer (tuatara.buffer:buffer-forward-char buffer before -1)) tuatara.text:*openers*))
                   (values nil :start)
                   (multiple-value-bind (start balanced) (sexp-start-before buffer position)
                     (if balanced (values start nil) (values nil :unbalanced))))))
      (t (values start nil)))))

(defun sexp-motion (buffer position count)
  "Where COUNT balanced expressions on from POSITION in BUFFER end, or where
COUNT back from it start when COUNT is negative, as forward-sexp moves; else
NIL and why, as SEXP-END-AT and SEXP-START-AT say it, for the first that
fails."
  (let ((p position))
    (dotimes (i (abs count) p)
      (multiple-value-bind (next why) (if (plusp count) (sexp-end-at buffer p) (sexp-start-at buffer p))
        (unless next (return (values nil why)))
        (setf p next)))))

(defun sexp-failure-message (why)
  "Say why a balanced expression could not be moved over: WHY as SEXP-END-AT
and SEXP-START-AT give it."
  (message (ecase why
             (:end "At the end of the expression around point")
             (:start "At the start of the expression around point")
             (:unbalanced "The brackets do not balance"))))

(defun move-by-sexps (count)
  "Move point over COUNT balanced expressions, back when COUNT is negative,
one at a time, saying why when it cannot go on."
  (let ((b (current-buffer)))
    (dotimes (i (abs count))
      (multiple-value-bind (next why) (sexp-motion b (point) (if (plusp count) 1 -1))
        (unless next (sexp-failure-message why) (return))
        (goto next)))))

(define-command forward-sexp ()
  "Move forward over a balanced expression. In a language with a grammar that
is the next expression as the grammar reads it: a word, or a bracketed group,
a string or a quoted form taken whole, comments passed over; strings, comments
and character literals never count as brackets. Elsewhere it is a bracketed
group or a symbol. At a closing bracket it stops: the expression around point
ends there. With a count, that many; a negative count moves backward."
  (move-by-sexps (prefix)))

(define-command backward-sexp ()
  "Move backward over a balanced expression, the opposite of forward-sexp: a
quote before the expression goes with it. At an opening bracket it stops: the
expression around point starts there. With a count, that many; a negative
count moves forward."
  (move-by-sexps (- (prefix))))

;;; Inserting and deleting

(defvar *post-self-insert-functions* '()
  "Functions called with the buffer and the character just typed, after
self-insert-command put it in (not under a count, and not in the hex view):
what closes a delimiter as it is typed. Their changes join the character's
undo step.")

(defvar *delete-backward-functions* '()
  "Functions asked, with the buffer, before \\[delete-backward-char] deletes the
character before point without a count: the first to answer true has done the
deleting itself (both halves of an empty pair, say).")

(define-command self-insert-command ()
  "Insert the character that invoked this command, prefix-argument times.
Consecutively typed characters form one undo group. With an input method on
\(\\[set-input-method]), and no count, the character goes through its rules."
  (let* ((key (editor-this-key *editor*))
         (method (and key (null (prefix-argument)) (buffer-input-method))))
    (when (and key (characterp (tuatara.frontend:key-base key)))
      (let ((string (make-string (insert-count) :initial-element (tuatara.frontend:key-base key))))
        (cond
          ((and method (not (hex-view-here-p)))
           ;; through the buffer's input method; what it takes as typed goes on as typing does
           (when (input-method-type (current-buffer) method (tuatara.frontend:key-base key))
             (dolist (f *post-self-insert-functions*)
               (funcall f (current-buffer) (tuatara.frontend:key-base key)))))
          ((hex-view-here-p)
           ;; the hex view types over octets: hex digits, or characters in its text
           (hex-type string))
          (t
           (goto (tuatara.buffer:buffer-insert (current-buffer) (point) string :group (typing-goes-on-p)))
           (unless (prefix-argument)
             (dolist (f *post-self-insert-functions*)
               (funcall f (current-buffer) (tuatara.frontend:key-base key))))))))))

;;; Indenting lines

(defun line-indentation (buffer line)
  "The number of spaces and tabs the line starting at LINE in BUFFER begins
with, whether the line holds nothing else, and the column its text starts at
(a tab to the next tab stop, BUFFER-TAB-WIDTH), as three values."
  (rope-line-blanks (tuatara.buffer:buffer-text buffer) line (buffer-tab-width buffer)))

(defun range-last-place (buffer start end)
  "A place on the last line of BUFFER's range START below END: END, or the
octet before it when END is a line's start, so that a range ending there
leaves that line out."
  (if (and (> end start) (= end (tuatara.buffer:buffer-line-start buffer end)))
      (tuatara.buffer:buffer-forward-char buffer end -1)
      end))

(defun indent-lines (buffer start end &key blank-lines group)
  "Indent the lines of BUFFER from the one holding START to the one ending the
range below END, each to the column its mode gives: each line as if those
before it were already indented. Blank lines are left alone unless
BLANK-LINES, and a line already at its column keeps its spaces and tabs; the
others are indented with spaces, or tabs and spaces where BUFFER wants tabs
(INDENTATION-TEXT). One undoable change, joining the one before when GROUP.
Returns how many lines changed, :NONE when the mode has no rules for
indenting, or :TOO-LARGE, having said so, for more text than is indented at
once (TOO-LARGE-HERE-P)."
  (when (too-large-here-p start end "Indenting")
    (return-from indent-lines :too-large))
  (let* ((last (range-last-place buffer start end))
         (changes (let ((*tab-columns* (buffer-tab-width buffer)))
                    (mode-indent-lines (buffer-major-mode buffer) buffer start last blank-lines))))
    (if (eq changes :none)
        :none
        (let ((edits (loop for (line . column) in changes
                           for (width blank current) = (multiple-value-list (line-indentation buffer line))
                           unless (or (and blank (not blank-lines)) (= current column))
                             collect (tuatara.text:make-edit line (+ line width)
                                                             (and (plusp column)
                                                                  (tuatara.text:string-octets
                                                                   (indentation-text buffer column)))))))
          (when edits
            (tuatara.buffer:buffer-edit buffer (coerce edits 'vector) :group group))
          (length edits)))))

(defvar *indent-new-lines* t
  "True to indent a line as it is begun: after \\`RET', and on a line opened
with NORMAL state's \\<normal>\\[tuatara-open-below] or \\[tuatara-open-above], the
new line goes to its column as the language's mode indents it (Lisp, Rust and
shell scripts among them). Set it to NIL in the init file to keep new lines at
column 0.")

(defvar *typed-line* nil
  "True while the line being indented was just begun (by \\`RET' or NORMAL state's
\\<normal>\\[tuatara-open-below]): a node that ends on the line before reaches
on to it where the indentation rules allow (@extend), so that the new line is
indented as inside that node.")

(defun indent-typed-line (buffer &key group)
  "Indent the line of BUFFER holding point, blank or not, as its mode indents
it, and put point where its text begins; nothing when the mode has no rules
for indenting or *INDENT-NEW-LINES* is NIL. The change joins the undoable one
before when GROUP. A blank line so indented is remembered, for
FORGET-TYPED-INDENTATION."
  (when *indent-new-lines*
    (let ((line (tuatara.buffer:buffer-line-start buffer (point))))
      (unless (eq (let ((*typed-line* t)) (indent-lines buffer line line :blank-lines t :group group)) :none)
        (let ((line (tuatara.buffer:buffer-line-start buffer (point))))
          (multiple-value-bind (width blank) (line-indentation buffer line)
            (goto (+ line width))
            (when (and blank (plusp width))
              (setf (tuatara.buffer:buffer-local buffer 'typed-indentation)
                    (cons (tuatara.buffer:buffer-text buffer) line)))))))))

(defun forget-typed-indentation (buffer)
  "Take away the blanks INDENT-TYPED-LINE gave a blank line of BUFFER, as the
line is left with nothing typed on it: it stays empty, with no blanks at its
end, and undoing the change that began it undoes this too. True when it did."
  (let ((kept (shiftf (tuatara.buffer:buffer-local buffer 'typed-indentation) nil)))
    (when (and kept (eq (car kept) (tuatara.buffer:buffer-text buffer)))
      (let ((line (cdr kept)))
        (multiple-value-bind (width blank) (line-indentation buffer line)
          (when (and blank (plusp width))
            (tuatara.buffer:buffer-edit buffer (vector (tuatara.text:make-edit line (+ line width))) :group t)
            (goto line)
            t))))))

(defun in-string-or-comment-p (buffer position)
  "True when POSITION of BUFFER is inside a string or a comment, as its syntax
tree says when one is ready; NIL without one."
  (let ((syntax (ready-syntax buffer)))
    (and syntax (member (tuatara.syntax:text-kind-around syntax position) '(:string :comment)) t)))

(define-command newline ()
  "Insert a line end at point: a newline, or in a buffer whose lines end with a
carriage return and a newline (a Windows file), both; with a count, that many,
and none for 0. The new line is then indented as the language's mode indents
it, and the spaces left at the end of the line before go, outside strings and
comments (see *INDENT-NEW-LINES*)."
  (let* ((b (current-buffer)) (n (insert-count)) (trimmed nil))
    (when (zerop n) (return-from newline))
    ;; a line just begun and left blank loses its blanks
    (forget-typed-indentation b)
    (when (and *indent-new-lines* (not (in-string-or-comment-p b (point))))
      (let ((start (space-bounds b (point))) (p (point)))
        (when (< start p)
          (tuatara.buffer:buffer-delete b start p)
          (goto start)
          (setf trimmed t))))
    (goto (tuatara.buffer:buffer-insert b (point) (let ((line-end (tuatara.buffer:buffer-newline b)))
                                                    (with-output-to-string (out)
                                                      (loop repeat n do (write-string line-end out))))
                                        :group trimmed))
    (indent-typed-line b :group t)))

(define-command insert-tab ()
  "Insert a tab character at point, whatever the tab settings say;
\\[tab-to-tab-stop] (tab-to-tab-stop) goes on to the next tab stop as they say."
  (goto (tuatara.buffer:buffer-insert (current-buffer) (point) (string #\Tab))))

(defparameter *typing-commands* '(self-insert-command delete-backward-char delete-char)
  "The commands of typing: characters typed and deleted one after another undo
together, as one step, a whole stretch of typing in INSERT state with its
corrections taken back by one \\[undo].")

(defun typing-goes-on-p ()
  "True when the command before this one was typing too (*TYPING-COMMANDS*),
so that this one's change joins its undo step."
  (and (member (editor-last-command *editor*) *typing-commands*) t))

(defun show-fold-and-refuse (buffer fold message)
  "Show what FOLD of BUFFER, (START . END), hides, and refuse the command
with MESSAGE."
  (tuatara.display:unfold-region buffer (car fold) (cdr fold))
  (error "~a" message))

(defun refuse-hidden-deletion (buffer start end)
  "Refuse to delete the text of BUFFER from START below END when that would
take part of the text a fold hides, but not all of it: the fold is shown, so
that the same key, pressed again, deletes what can then be seen. A deletion
that takes a fold whole, such as a folded headline's line with all it hides,
goes ahead. Only the folds in that stretch are looked at."
  (refuse-hidden-deletions buffer (list (cons start end))))

(defun kill-span (buffer start end &key prepend)
  "Kill the text of BUFFER from START below END (kill-text, PREPEND as it
takes it) and delete it, unless that would take part of the text a fold
hides (REFUSE-HIDDEN-DELETION). Returns START."
  (refuse-hidden-deletion buffer start end)
  (kill-text (region-text buffer start end) :prepend prepend)
  (tuatara.buffer:buffer-delete buffer start end)
  start)

(defun refuse-hidden-deletions (buffer spans)
  "Refuse a command that would delete the stretches SPANS of BUFFER (a list
or vector, each (START . END) or (START END), in any order) when any of them
would take part of the text a fold hides, but not all of it: nothing is
deleted anywhere, every such fold is shown, and the same key, pressed again,
deletes what can then be seen. Stretches that meet are taken as one, so that
together they may take a fold whole, as a folded headline's line with all it
hides. Only the folds in those stretches are looked at, and nothing at all
when BUFFER has no folds. A deletion that would change the lines search
filters hide is refused outright, whole or not (REFUSE-HIDDEN-LINES)."
  (let ((folds (tuatara.display:folds-now buffer)))
    (when folds
      (let ((merged (tuatara.text:merge-ranges
                     (map 'list (lambda (span)
                                  (let ((s (car span)) (e (if (consp (cdr span)) (cadr span) (cdr span))))
                                    (cons (min s e) (max s e))))
                          spans)))
            (cut '()))
        (dolist (span merged)
          ;; every fold the stretch cuts, each found from where the one before ends
          (loop with start = (car span)
                for (fold kind) = (multiple-value-list (tuatara.display:folds-cut folds start (cdr span)))
                while fold
                do (when (eq kind :hidden) (refuse-hidden-lines))
                   (push fold cut) (setf start (cdr fold))))
        (when cut
          (tuatara.display:unfold-ranges buffer (coerce (nreverse cut) 'simple-vector))
          (error "That would delete hidden text, shown now: the same key again deletes it"))))))

(defun refuse-hidden-lines ()
  "Refuse a command that would change the lines search filters hide: they
are never changed by an edit of the lines shown."
  (error "~a" (substitute-command-keys
               "That would change lines the search filters hide: \\[remove-search-filters] shows them")))

(defun check-hidden-lines (buffer start end)
  "Refuse a command that would rewrite the text of BUFFER from START below
END as a whole (sorting it, keeping or deleting lines, replacing all over it)
when search filters hide any of its lines there (REFUSE-HIDDEN-LINES): the
lines of a stretch they hide are those after the newline it starts at, to
the newline it ends at."
  (let ((bounds (tuatara.display:hidden-bounds buffer)))
    (when (and bounds (< start end)
               (loop for i from 0 below (length bounds) by 2
                     thereis (and (< (1+ (aref bounds i)) end) (>= (aref bounds (1+ i)) start))))
      (refuse-hidden-lines))))

(defun check-writable (buffer)
  "Signal an error when BUFFER is read-only."
  (when (tuatara.buffer:buffer-local buffer :read-only)
    (error "Buffer is read-only: ~a" (tuatara.buffer:buffer-name buffer))))

(defun delete-spans (buffer spans &key replacements folds-checked before group)
  "Delete SPANS of BUFFER, each (START . END) or (START END ...), in order and
apart, as one change for undo: how text is taken away or replaced at many
places at once, at banked lines, the places of a split, cursors, a
rectangle's rows and a selection pasted over. With REPLACEMENTS, a list as
long as SPANS, each (a string, octets, a rope or NIL) goes in its span's
place. Refused, nothing changed, when BUFFER may not be written
\(CHECK-WRITABLE) and, unless FOLDS-CHECKED (every span that would was let go
of first), when that would take part of the text a fold hides, but not all of
it \(REFUSE-HIDDEN-DELETIONS). BEFORE, when given, is called once the change is
known to go ahead and before it is made. More octets than
*LARGE-EDIT-OCTETS* going, or a rope as large coming, with nothing put in
but at the first span, make one splice that copies nothing for undo
\(TUATARA.BUFFER:BUFFER-SPLICE-SPANS). GROUP joins the newest undo group.
Returns the positions just after each span's new text, a vector; NIL when
nothing changed. O(spans + octets put in), the folds looked at only in the
spans."
  (let* ((spans (map 'list (lambda (span)
                             (cons (car span) (if (consp (cdr span)) (cadr span) (cdr span))))
                     spans))
         (first (first replacements)))
    (check-writable buffer)
    (unless folds-checked (refuse-hidden-deletions buffer spans))
    (when before (funcall before))
    (if (and (every #'null (rest replacements))
             (or (> (loop for (s . e) in spans sum (- e s)) tuatara.buffer:*large-edit-octets*)
                 (and (tuatara.text:rope-p first) (> (tuatara.text:rope-bytes first) tuatara.buffer:*large-edit-octets*))))
        (let ((rope (etypecase first
                      (null nil)
                      (tuatara.text:rope first)
                      (string (tuatara.text:string->rope first))
                      ((array (unsigned-byte 8) (*)) (tuatara.text:octets->rope (coerce first 'tuatara.text:octets))))))
          (vector (tuatara.buffer:buffer-splice-spans buffer spans (and rope (plusp (tuatara.text:rope-bytes rope)) rope))))
        (let ((edits (loop for (s . e) in spans
                           for rest = replacements then (rest rest)
                           for octets = (let ((text (first rest)))
                                          (etypecase text
                                            (null nil)
                                            (string (tuatara.text:string-octets text))
                                            ((array (unsigned-byte 8) (*)) (coerce text 'tuatara.text:octets))
                                            (tuatara.text:rope (tuatara.text:rope-octets text))))
                           collect (tuatara.text:make-edit s e (and octets (plusp (length octets)) octets)))))
          (when (some (lambda (edit) (or (< (tuatara.text:edit-start edit) (tuatara.text:edit-end edit))
                                         (plusp (tuatara.text:edit-insert-length edit))))
                      edits)
            (tuatara.buffer:buffer-edit buffer (coerce (if replacements
                                                           edits
                                                           (remove-if (lambda (edit) (= (tuatara.text:edit-start edit) (tuatara.text:edit-end edit)))
                                                                      edits))
                                                       'simple-vector)
                                        :group group))))))

(define-command delete-char ()
  "Delete the character after point (with a count, that many; a negative one,
before point), a letter with its accents or an emoji with its skin tone
whole (CLUSTER-FORWARD). Typing and deleting one after another undo together.
Text a fold hides is not deleted unseen: the fold is shown instead, and the
key pressed again deletes."
  (let* ((b (current-buffer)) (p (point)) (other (cluster-forward b p (prefix)))
         (start (min p other)) (end (max p other)))
    ;; with a negative count, the characters before point
    (when (< start end)
      (refuse-hidden-deletion b start end)
      (tuatara.buffer:buffer-delete b start end :group (typing-goes-on-p)))))

(define-command delete-backward-char ()
  "Delete the character before point (with a count, that many): of a letter
with accents, the last accent alone, of an emoji with a skin tone, the tone.
Typing and deleting one after another undo together. Text a fold hides is
not deleted unseen: the fold is shown instead, and the key pressed again
deletes."
  (let* ((b (current-buffer)) (p (point)) (start (tuatara.buffer:buffer-forward-char b p (- (prefix)))))
    (unless (and (null (prefix-argument))
                 (some (lambda (f) (funcall f b)) *delete-backward-functions*))
      (when (< start p)
        (refuse-hidden-deletion b start p)
        (tuatara.buffer:buffer-delete b start p :group (typing-goes-on-p)) (goto start)))))

;;; The kill ring

(defun kill-command-p (command)
  "True for commands that add to the kill ring, so consecutive kills append."
  (member command '(kill-line kill-region kill-whole-line kill-sexp backward-kill-sexp zap-to-char zap-up-to-char)))

(defvar *use-system-clipboard* t
  "True to put what is killed or copied on the system clipboard as well, where
other programs paste it from, and to paste what another program copied there
since; NIL keeps killed text to the editor.")

(defvar *clipboard-max-size* (* 8 1024 1024)
  "The most characters a kill may have and still go on the system clipboard.")

(defvar *kill-ring-max* 120
  "The most kills the kill ring keeps: older ones are forgotten.")

(defvar *kill-ring-max-size* (* 64 1024 1024)
  "The most characters (octets, for a kill kept where it is) the kills on the
kill ring may hold together: past it the oldest are forgotten, the latest
always kept; NIL for no such limit.")

(defun push-kill (text)
  "Put TEXT first on the kill ring, forgetting the kills past *KILL-RING-MAX*,
and the oldest past *KILL-RING-MAX-SIZE* together."
  (let ((tail (nthcdr (1- (max 1 *kill-ring-max*)) (push text (editor-kill-ring *editor*)))))
    (when tail (setf (cdr tail) nil)))
  (when *kill-ring-max-size*
    (let ((total 0))
      (loop for cell on (editor-kill-ring *editor*)
            do (incf total (kill-size (car cell)))
               (when (and (cdr cell) (> (+ total (kill-size (cadr cell))) *kill-ring-max-size*))
                 (setf (cdr cell) nil)
                 (return))))))

(defvar *clipboard-sent* nil
  "The text last put on the system clipboard or taken from it, to tell what
another program copied from what is there already.")

(defun send-to-clipboard (text)
  "Put TEXT on the system clipboard, as *USE-SYSTEM-CLIPBOARD* says: a kill
kept as a rope (too large for it) is not, nor one from an encrypted file's
text, which \\[copy-to-clipboard] puts there on request."
  (when (and *use-system-clipboard* *editor* (stringp text) (<= (length text) *clipboard-max-size*)
             (not (tuatara.buffer:buffer-local (current-buffer) :plaintext))
             (tuatara.frontend:frontend-set-clipboard (editor-frontend *editor*) text))
    (setf *clipboard-sent* text)))

(defun take-in-clipboard ()
  "Before a paste: when another program has copied text to the system
clipboard since, put it first on the kill ring."
  (when (and *use-system-clipboard* *editor*)
    (let ((text (tuatara.frontend:frontend-clipboard (editor-frontend *editor*))))
      (when (and text (plusp (length text))
                 (not (equal text *clipboard-sent*))
                 (not (equal text (first (editor-kill-ring *editor*)))))
        (push-kill text)
        (setf *clipboard-sent* text)))))

(defun span-texts (buffer spans &key string)
  "The text of SPANS of BUFFER, each (START . END), one after another, as a
kill: a string, or when more octets than *CLIPBOARD-MAX-SIZE* and STRING is
false, a rope sharing the buffer's own (TUATARA.BUFFER:BUFFER-SLICE), made at
once and copying nothing whatever its size. Such a kill stays good after its
buffer is killed."
  (if (and (not string) (> (loop for (start . end) in spans sum (- end start)) *clipboard-max-size*))
      (reduce #'kill-join (mapcar (lambda (span) (tuatara.buffer:buffer-slice buffer (car span) (cdr span))) spans))
      (apply #'concatenate 'string
             (mapcar (lambda (span) (tuatara.buffer:buffer-string buffer :start (car span) :end (cdr span))) spans))))

(defun region-text (buffer start end &key string)
  "The text of BUFFER from START below END as its windows show it, to copy,
kill or hand on: the lines search filters hide there are left out whole
\(TUATARA.DISPLAY:SHOWN-PARTS), everything else taken as it is, the text of
folds included. A string, or a rope when it is more octets than
*CLIPBOARD-MAX-SIZE* and STRING is false (SPAN-TEXTS). The second value is
how many lines were left out, 0 when none were."
  (let* ((low (min start end)) (high (max start end))
         (parts (tuatara.display:shown-parts buffer low high)))
    (values (if parts (span-texts buffer parts :string string) "")
            (if (equal parts (list (cons low high)))
                0
                (tuatara.display:lines-left-out buffer low high parts)))))

(defun left-out-note (lines)
  "Words to add to what a command that took text says when LINES lines the
search filters hide were left out of it; empty when none were."
  (if (plusp lines)
      (format nil ", ~:d line~:p hidden by the search filters left out" lines)
      ""))

(defun kill-size (kill)
  "The characters of KILL, a string, or the octets of a kill kept as a rope."
  (if (stringp kill) (length kill) (tuatara.text:rope-bytes kill)))

(defun kill-join (a b)
  "The kills A and B as one: a string when both are strings, else a rope."
  (if (and (stringp a) (stringp b))
      (concatenate 'string a b)
      (flet ((as-rope (kill) (if (stringp kill) (tuatara.text:string->rope kill) kill)))
        (let ((head (as-rope a)))
          (tuatara.text:splice-rope head (tuatara.text:rope-bytes head) (tuatara.text:rope-bytes head) (as-rope b))))))

(defun kill-copies (kill n)
  "KILL N times over, as one kill. A rope is doubled, not joined N times over,
and so is a text that would come to more than *CLIPBOARD-MAX-SIZE* octets
made a string."
  (cond ((and (stringp kill) (> (* n (length kill)) *clipboard-max-size*))
         (kill-copies (tuatara.text:string->rope kill) n))
        ((stringp kill) (with-output-to-string (out) (dotimes (i n) (write-string kill out))))
        ((<= n 1) kill)
        (t (let* ((half (kill-copies kill (floor n 2)))
                  (twice (kill-join half half)))
             (if (oddp n) (kill-join twice kill) twice)))))

(defun kill-ends-line-p (kill)
  "True when KILL ends with a newline."
  (if (stringp kill)
      (and (plusp (length kill)) (char= (char kill (1- (length kill))) #\Newline))
      (and (plusp (tuatara.text:rope-bytes kill))
           (= 10 (tuatara.text:rope-octet kill (1- (tuatara.text:rope-bytes kill)))))))

(defun kill-text (text &key (append (kill-command-p (editor-last-command *editor*))) (prepend nil))
  "Put TEXT on the kill ring, a string, or a rope for a kill too large for the
system clipboard (REGION-TEXT); appending or prepending to the last kill when
the previous command was a kill too, and on the system clipboard
(SEND-TO-CLIPBOARD)."
  (let ((ring (editor-kill-ring *editor*)))
    (cond ((and append ring)
           (setf (first ring) (if prepend (kill-join text (first ring)) (kill-join (first ring) text))))
          (t (push-kill text)))
    (send-to-clipboard (first (editor-kill-ring *editor*)))))

(define-command kill-line ()
  "Kill the rest of the current line; at the end of a line, kill the newline.
On a line whose end a fold hides (a folded headline), nothing is killed: the
fold is shown instead, and the key pressed again kills what can then be seen.
In a narrowed buffer, no further than the end of the part shown."
  (let* ((b (current-buffer)) (p (point)) (end (tuatara.buffer:within-narrowing b (tuatara.buffer:buffer-line-end b p))))
    (let ((fold (tuatara.display:fold-hiding b end nil)))
      (when fold
        (show-fold-and-refuse b fold "The rest of this line is hidden, shown now: the same key again kills to its end")))
    (when (= end p) (setf end (tuatara.buffer:within-narrowing b (tuatara.buffer:buffer-forward-char b p))))
    (when (> end p) (kill-span b p end))))

(defvar *kill-parts* nil
  "The last kill made of parts, and its parts, as (KILL SHAPE . PARTS): SHAPE
:BLOCK for a rectangle cut or copied with \\[kill-region] or
\\[kill-ring-save], its rows the parts, or :PIECES for a kill made at several
places or cursors, the text each gave it the parts, top to bottom. While that
kill is first on the kill ring, \\[yank] and \\<normal>\\[yank-over-selection]
paste a block as a block, and pasting at as many cursors gives each its own
part back (KILL-PARTS).")

(defun parts-kill-text (texts)
  "The one kill TEXTS, taken one from each of several places or cursors, top
to bottom, make: whole lines one after another, one copy of texts that agree,
else every text one a line; NIL when every one is empty."
  (cond ((every (lambda (text) (zerop (length text))) texts) nil)
        ((every (lambda (text) (and (plusp (length text)) (char= (char text (1- (length text))) #\Newline))) texts)
         (with-output-to-string (out) (dolist (text texts) (write-string text out))))
        ((null (rest (remove-duplicates texts :test #'string=))) (first texts))
        (t (format nil "~{~a~^~%~}" texts))))

(defun kill-in-parts (parts &key (shape :pieces) (kill (parts-kill-text parts)))
  "Put KILL, made of PARTS (top to bottom), on the kill ring as one kill and
remember its parts and their SHAPE (*KILL-PARTS*): :PIECES, one from each place
or cursor, by default made into one text as PARTS-KILL-TEXT says, or :BLOCK, a
rectangle's rows."
  (when kill
    (kill-text kill :append nil)
    (setf *kill-parts* (list* (first (editor-kill-ring *editor*)) shape parts))))

(defun kill-parts (kill &optional n)
  "The parts KILL was made of (*KILL-PARTS*), and as a second value their
shape, :PIECES or :BLOCK; NIL when it was not made of parts, or with N of
other than N parts."
  (when (and kill (eq kill (car *kill-parts*)) (or (null n) (= n (length (cddr *kill-parts*)))))
    (values (cddr *kill-parts*) (cadr *kill-parts*))))

(define-command kill-region ()
  "Kill the text between point and mark; a rectangle (\\[rectangle-mark-mode])
is killed as a block, which \\[yank] pastes back as one. A region that takes
part of the text a fold hides, but not all of it, is not killed: the fold is
shown instead, and the key pressed again kills."
  (multiple-value-bind (start end) (tuatara.buffer:buffer-region (current-buffer))
    (cond ((rectangle-live-p (current-buffer)) (funcall 'kill-live-rectangle t))
          ((and start (< start end))
           (let ((b (current-buffer)))
             (goto (kill-span b start end :prepend (< (point) end)))
             (setf (tuatara.buffer:buffer-mark-active-p b) nil)))
          (t (message "The region is empty")))))

(define-command kill-ring-save ()
  "Copy the text between point and mark to the kill ring without deleting it;
a rectangle (\\[rectangle-mark-mode]) is copied as a block, which \\[yank]
pastes as one. The lines search filters hide (\\[search-filter-regex]) are
left out, the copy holding only the lines shown."
  (multiple-value-bind (start end) (tuatara.buffer:buffer-region (current-buffer))
    (cond ((rectangle-live-p (current-buffer)) (funcall 'kill-live-rectangle nil))
          ((and start (< start end))
           (copy-region-text (current-buffer) start end)
           (setf (tuatara.buffer:buffer-mark-active-p (current-buffer)) nil))
          (t (message "The region is empty")))))

(defun copy-region-text (buffer start end)
  "Copy the text of BUFFER from START below END to the kill ring and the
system clipboard as its windows show it (REGION-TEXT), and say so: with its
size when it is kept as a rope, and with how many lines the search filters
hide were left out of it (LEFT-OUT-NOTE)."
  (multiple-value-bind (text left-out) (region-text buffer start end)
    (kill-text text :append nil)
    (let ((kill (first (editor-kill-ring *editor*))))
      (if (stringp kill)
          (message "Copied~a" (left-out-note left-out))
          (message "Copied ~a, kept where it is (too large for the system clipboard)~a"
                   (octets-words (kill-size kill)) (left-out-note left-out))))))

(defun lf-newlines (text)
  "TEXT with each carriage return that comes before a newline taken away."
  (if (not (find #\Return text))
      text
      (with-output-to-string (out)
        (loop for i from 0 below (length text)
              for c = (char text i)
              unless (and (char= c #\Return) (< (1+ i) (length text)) (char= (char text (1+ i)) #\Newline))
                do (write-char c out)))))

(defun search-count (needle text &key (end (length text)))
  "How many times NEEDLE stands in TEXT below END, none overlapping."
  (loop with start = 0
        for at = (search needle text :start2 start :end2 end)
        while at count t do (setf start (+ at (length needle)))))

(defun crlf-newlines (text)
  "TEXT with a carriage return put before each newline that has none."
  (if (not (find #\Newline text))
      text
      (with-output-to-string (out)
        (loop for i from 0 below (length text)
              for c = (char text i)
              do (when (and (char= c #\Newline) (not (and (plusp i) (char= (char text (1- i)) #\Return))))
                   (write-char #\Return out))
                 (write-char c out)))))

(defun text-for-buffer (kill buffer)
  "KILL, text to put into BUFFER (from the kill ring, a template, a file),
with BUFFER's line ends: where BUFFER's lines end with a carriage return and
a newline, a newline alone given a carriage return before it; elsewhere, the
carriage return before a newline taken away. A kill kept as a rope, too
large for the system clipboard, goes in as it is."
  (cond ((not (stringp kill)) kill)
        ((buffer-crlf-p buffer) (crlf-newlines kill))
        (t (lf-newlines kill))))

(define-command yank ()
  "Insert the most recently killed text at point, or what another program
copied to the system clipboard since, its lines ending as the buffer's do. A
rectangle killed or copied with \\[kill-region] or \\[kill-ring-save] goes in
as a block. The mark is left at its start, the mark before on the mark ring."
  (take-in-clipboard)
  (let* ((kill (first (editor-kill-ring *editor*)))
         (text (and kill (text-for-buffer kill (current-buffer)))))
    (cond ((eq (nth-value 1 (kill-parts kill)) :block)
           (funcall 'paste-rows-at-point (kill-parts kill)))
          (text
           (let ((p (point)))
             (push-mark)
             (goto (tuatara.buffer:buffer-insert (current-buffer) p text))))
          (t (message "The kill ring is empty")))))

;;; Mark

(define-command mark-whole-buffer ()
  "Select the whole buffer: mark at the end, point at the start; where point
was goes on the mark ring, with the mark before."
  (push-mark :quiet t)
  (push-mark :position (tuatara.buffer:buffer-size (current-buffer)) :activate t)
  (goto 0))

(defvar *repeat-selection* nil
  "The last command that acted on a selection and how to make that selection
again, as (ENTRY . FUNCTION): ENTRY the command as EDITOR-REPEATABLE kept it,
FUNCTION one of no arguments making the same selection from point.")

(define-command repeat ()
  "Run the last command again, other than repeat itself, with the prefix
argument it had (one given to repeat is used instead) and as the key that ran
it (C-x C-- shrinks again, it does not grow); repeat again, and it runs that
command once more, as often as you like. A command that acted on a selection
has that selection made again first, as large, from point: the same number of
whole lines, of characters, the same text object, or a rectangle as many rows
by as many columns. Only that selection and that command: a longer run of keys
is a keyboard macro's work."
  (let ((entry (editor-repeatable *editor*)))
    (if (or (null entry) (eq (car entry) 'repeat))
        (message "There is no command to repeat")
        (destructuring-bind (command argument . key) entry
          (when (eq (car *repeat-selection*) entry)
            (let ((count (editor-prefix-argument *editor*)))
              (setf (editor-prefix-argument *editor*) nil)
              (funcall (cdr *repeat-selection*))
              (setf (editor-prefix-argument *editor*) count)))
          (unless (prefix-argument)
            (setf (editor-prefix-argument *editor*) argument))
          (let ((typed (editor-this-key *editor*)))
            (setf (editor-this-key *editor*) (or key typed))
            (unwind-protect (funcall (find-command command))
              (setf (editor-this-key *editor*) typed)))))))

(pushnew 'repeat *not-repeatable*)

;;; The mark ring

(defvar *mark-ring-max* 16
  "How many marks each buffer's mark ring keeps; past that the oldest goes.")

(defvar *global-mark-ring-max* 16
  "How many marks the global mark ring keeps; past that the oldest goes.")

(defvar *set-mark-command-repeat-pop* nil
  "True for \\[set-mark-command] pressed again right after
\\[universal-argument] \\[set-mark-command] (or \\[pop-global-mark]) to go on
back to the mark before; NIL, as standard, for it to set the mark.")

(defvar *global-mark-ring* '()
  "The marks set in one buffer after another, the newest first, each (BUFFER
. ANCHOR): where \\[pop-global-mark] goes back to.")

(defun mark-ring (&optional (buffer (current-buffer)))
  "BUFFER's mark ring: its marks before the mark, the newest first, as anchors."
  (tuatara.buffer:buffer-local buffer 'mark-ring '()))

(defun anchor-in (buffer anchor)
  "Where ANCHOR, made in BUFFER, stands in it now; NIL when it no longer can."
  (let ((position (ignore-errors (tuatara.text:anchor-position anchor (tuatara.buffer:buffer-version buffer)))))
    (and position (<= 0 position (tuatara.buffer:buffer-size buffer)) position)))

(defun push-mark (&key (position (point)) quiet activate (buffer (current-buffer)))
  "Set BUFFER's mark at POSITION (point by default), the mark it had put on
its mark ring (*MARK-RING-MAX* kept); when the newest
mark of the global mark ring is another buffer's, this one goes there too
\(*GLOBAL-MARK-RING-MAX* kept). Says Mark set unless QUIET; ACTIVATE makes the
region active."
  (let ((old (tuatara.buffer:buffer-mark buffer))
        (version (tuatara.buffer:buffer-version buffer)))
    (when old
      (let ((ring (cons (tuatara.text:make-anchor version old) (mark-ring buffer))))
        (setf (tuatara.buffer:buffer-local buffer 'mark-ring)
              (if (> (length ring) *mark-ring-max*) (subseq ring 0 *mark-ring-max*) ring))))
    (setf (tuatara.buffer:buffer-mark buffer) position)
    (unless (and *global-mark-ring* (eq (car (first *global-mark-ring*)) buffer))
      (push (cons buffer (tuatara.text:make-anchor version position)) *global-mark-ring*)
      (when (> (length *global-mark-ring*) *global-mark-ring-max*)
        (setf *global-mark-ring* (subseq *global-mark-ring* 0 *global-mark-ring-max*))))
    (unless quiet (message "Mark set"))
    (when activate (setf (tuatara.buffer:buffer-mark-active-p buffer) t))
    nil))

(defun pop-mark (&optional (buffer (current-buffer)))
  "Take the newest mark off BUFFER's mark ring as its mark, the mark it had
going to the ring's far end, and make the region inactive. Point stays."
  (let ((ring (mark-ring buffer)))
    (when ring
      (let ((mark (tuatara.buffer:buffer-mark buffer))
            (next (anchor-in buffer (first ring))))
        (setf (tuatara.buffer:buffer-local buffer 'mark-ring)
              (append (rest ring) (and mark (list (tuatara.text:make-anchor (tuatara.buffer:buffer-version buffer) mark)))))
        (when next (setf (tuatara.buffer:buffer-mark buffer) next)))))
  (setf (tuatara.buffer:buffer-mark-active-p buffer) nil))

(defun region-active-p (&optional (buffer (current-buffer)))
  "True while BUFFER has a mark and the region is active."
  (and (tuatara.buffer:buffer-mark buffer) (tuatara.buffer:buffer-mark-active-p buffer)))

(define-command push-mark-command ()
  "Set the mark at point and activate the region, the mark before going on the
mark ring; when the mark is at point already, only activate it. With a
count, set it whatever."
  (push-mark-at-point (prefix-argument) nil))

(defun push-mark-at-point (always quiet)
  "What \\[push-mark-command] does: set the mark at point, active, unless
ALWAYS is false and it is there already, then only activate it; QUIET says
nothing."
  (let* ((b (current-buffer)) (mark (tuatara.buffer:buffer-mark b)))
    (if (or always (null mark) (/= mark (point)))
        (push-mark :quiet quiet :activate t)
        (progn (setf (tuatara.buffer:buffer-mark-active-p b) t)
               (unless quiet (message "Mark activated"))))))

(define-command pop-to-mark-command ()
  "Go to the mark, and take the mark before it off the mark ring as the mark.
\\[universal-argument] \\[set-mark-command] again and again goes back through
the marks set in this buffer, and round."
  (let* ((b (current-buffer)) (mark (tuatara.buffer:buffer-mark b)))
    (if (null mark)
        (message "No mark set in this buffer")
        (progn (when (= (point) mark) (message "Mark popped"))
               (goto mark)
               (pop-mark b)))))

(define-command set-mark-command ()
  "Set the mark at point and activate the region (\\[set-mark-command]).
The mark before goes on the buffer's mark ring, and the
new one on the global mark ring when the last one there is another buffer's.
Pressed again at once, it makes the region inactive, and once more active.
With \\[universal-argument], go to the mark and take the one before it off the ring
\(pop-to-mark-command), back through the marks set here; \\[universal-argument]
\\[universal-argument] sets it whatever. \\[pop-global-mark] goes back through the
buffers where marks were set."
  (let ((arg (prefix-argument))
        (last (editor-last-command *editor*))
        (b (current-buffer)))
    (cond ((and arg (>= arg 16))
           (push-mark-at-point nil nil))
          ((and *set-mark-command-repeat-pop* (eq last 'pop-global-mark) (null arg))
           (setf (editor-this-command *editor*) 'pop-global-mark)
           (pop-global-mark))
          ((or arg (and *set-mark-command-repeat-pop* (eq last 'pop-to-mark-command)))
           (setf (editor-this-command *editor*) 'pop-to-mark-command)
           (pop-to-mark-command))
          ((eq last 'set-mark-command)
           (if (region-active-p b)
               (progn (setf (tuatara.buffer:buffer-mark-active-p b) nil)
                      (message "Mark deactivated"))
               (progn (setf (tuatara.buffer:buffer-mark-active-p b) t)
                      (message "Mark activated"))))
          (t (push-mark-at-point nil nil)))))

(define-command pop-global-mark ()
  "Go to the newest mark of the global mark ring, in its buffer, that mark going
to the ring's far end. Pressed again and again (\\[pop-global-mark]), it goes
back through the buffers where marks were set, and round."
  (setf *global-mark-ring* (remove-if-not (lambda (entry) (live-buffer-p (car entry)))
                                          *global-mark-ring*))
  (if (null *global-mark-ring*)
      (message "No global mark set")
      (destructuring-bind (buffer . anchor) (first *global-mark-ring*)
        (setf *global-mark-ring* (append (rest *global-mark-ring*) (list (first *global-mark-ring*))))
        (let ((position (anchor-in buffer anchor)))
          (unless (eq buffer (current-buffer))
            (show-buffer buffer))
          (when position
            (goto-shown buffer position))))))

(define-command exchange-point-and-mark ()
  "Swap point and mark."
  (let ((mark (tuatara.buffer:buffer-mark (current-buffer))))
    (if mark
        (let ((p (point))) (goto mark) (setf (tuatara.buffer:buffer-mark (current-buffer)) p))
        (message "No mark set"))))

(defvar *keyboard-quit-functions* '()
  "Functions of no arguments \\[keyboard-quit] calls before it quits: what is
shown until then lets go (a search's matches lit after it ended).")

(define-command keyboard-quit ()
  "Abandon the current operation and deactivate the region; what stays shown
until then goes, such as the matches of the last search."
  (setf (tuatara.buffer:buffer-mark-active-p (current-buffer)) nil)
  (mapc #'funcall *keyboard-quit-functions*)
  (keyboard-quit-signal))

(defun quit-key-text ()
  "The key that gives up what is going on, as messages name it, between the
marks of a key (MARK-KEY): the one bound to keyboard-quit, else the one bound
to tuatara-quit, which gives up so outside INSERT state; M-x keyboard-quit
when neither has a key."
  (let ((keymaps (keymaps-for-keys)))
    (mark-key (or (command-key 'keyboard-quit keymaps)
                  (command-key 'tuatara-quit keymaps)
                  "M-x keyboard-quit"))))

;;; Undo

(define-command undo ()
  "Undo the last change. In a narrowed buffer a change outside the part shown
is undone too, and that is said."
  (multiple-value-bind (group outside) (tuatara.buffer:buffer-undo (current-buffer))
    (cond ((null group) (message "Nothing to undo"))
          (outside (message "Undid a change outside the part shown")))))

(define-command redo ()
  "Redo the last undone change: the cursor goes after what it makes again, but
for a change made in the background (a large replacement, a sort), after which
it stays where it is."
  (unless (tuatara.buffer:buffer-redo (current-buffer)) (message "Nothing to redo")))

(defun say-undo-let-go (buffer)
  "Say that the oldest changes of BUFFER can no longer be undone, its undo
history having reached *UNDO-LIMIT*."
  (message "~a: its oldest changes can no longer be undone (*undo-limit* reached)"
           (tuatara.buffer:buffer-name buffer)))

(pushnew 'say-undo-let-go tuatara.buffer:*undo-let-go-functions*)

;;; The name around a place

(defun text-word-bounds (text point part-p)
  "The start and end, as two values, of the word around index POINT of TEXT:
the characters either side of POINT that PART-P, a function of a character,
is true of."
  (let ((start (position-if-not part-p text :end point :from-end t))
        (end (position-if-not part-p text :start point)))
    (values (if start (1+ start) 0) (or end (length text)))))

(defun word-bounds-at (buffer position part-p &optional (reach 256))
  "The start and end, as two values, of the word around byte POSITION of
BUFFER, a character boundary: the characters either side of it that PART-P,
a function of a character, is true of (TEXT-WORD-BOUNDS), looked for no
further than about REACH octets either way; both POSITION when there are
none. Whole characters are taken, however many octets each is."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (from (tuatara.text:rope-char-start rope (max 0 (- position reach))))
         (to (tuatara.text:rope-char-start rope (min (tuatara.text:rope-bytes rope) (+ position reach))))
         (text (tuatara.buffer:buffer-string buffer :start from :end (max to position)))
         (here (loop with at = from
                     for i from 0
                     while (and (< at position) (< i (length text)))
                     do (incf at (tuatara.text:char-octet-length (char text i)))
                     finally (return i))))
    (multiple-value-bind (start end) (text-word-bounds text here part-p)
      (let ((start-byte (- position (tuatara.text:string-octet-length text :start start :end here))))
        (values start-byte (+ position (tuatara.text:string-octet-length text :start here :end end)))))))

;;; Buffers

(defun buffer-listed-file (buffer)
  "The file the lists of buffers name for BUFFER: the file it visits, or the
picture, PDF or document it shows; NIL for none."
  (or (tuatara.buffer:buffer-file buffer) (tuatara.buffer:buffer-local buffer :viewed-file)))

(defun buffer-candidates ()
  "Completion candidates for the live buffers other than the current one, each
with its size, its major mode and its file (BUFFER-LISTED-FILE) beside it, the
modes in a column as wide as the longest, and a + when it has changes not
saved."
  (let* ((buffers (remove (current-buffer) tuatara.buffer:*buffers*))
         (mode-width (loop for b in buffers
                           maximize (length (or (tuatara.buffer:buffer-local b :mode-name) "")) into widest
                           finally (return (max 12 widest)))))
    (mapcar (lambda (b)
              (let ((file (buffer-listed-file b)))
                (make-candidate (tuatara.buffer:buffer-name b)
                                :annotation (list (list (if (tuatara.buffer:buffer-modified-p b) "+" "") :completion-modified 1)
                                                  (list (octets-words (tuatara.buffer:buffer-size b)) :list-size -8)
                                                  (list (or (tuatara.buffer:buffer-local b :mode-name) "") :completion-major-mode mode-width)
                                                  (list (if file (abbreviated-path file) "") :completion-annotation 2/5 :middle)))))
            buffers)))

(defun recent-buffer-names ()
  "The names of the buffers used before the current one, the latest first:
the history that puts them first among the candidates, so that
\\<minibuffer>\\[minibuffer-done] alone goes back to the buffer used last."
  (loop for b in *buffer-order*
        unless (or (eq b (current-buffer)) (not (live-buffer-p b)))
          collect (tuatara.buffer:buffer-name b)))

(defun split-choices (text)
  "The choices in TEXT, separated by commas, spaces around them dropped."
  (remove "" (mapcar (lambda (k) (string-trim " " k)) (uiop:split-string text :separator ","))
          :test #'string=))

(defun read-several (prompt offered &key (initial "") source (none "none") (more "no more"))
  "Ask with PROMPT for several of the choices OFFERED gives, separated by
commas, completing each in turn: OFFERED, a function of no arguments, gives
them as they are now, each a string or (STRING . ANNOTATION), none chosen
twice. \\<minibuffer>\\[minibuffer-done] adds the selected one and asks for the
next, \\[minibuffer-complete] puts it in, \\[minibuffer-submit-input] adds what
is typed. While no choice is begun, an empty line comes first, selected,
saying NONE (nothing chosen yet) or MORE: \\[minibuffer-done] there ends the
question. INITIAL is the text it begins with; SOURCE, for choices found in
the background meanwhile, as COMPLETING-READ takes it. Returns the strings
chosen, in order."
  (let ((text initial))
    (loop
      (let ((got (read-several-once prompt offered text source none more)))
        ;; the empty line taken, or nothing added: the question ends
        (when (equal (split-choices got) (split-choices text))
          (return (split-choices got)))
        (setf text (format nil "~{~a, ~}" (split-choices got)))))))

(defun read-several-once (prompt offered initial source none more)
  "One round of READ-SEVERAL: the line as it is when one choice is taken or the question ends."
  (completing-read prompt
                    (lambda (input)
                      (let* ((comma (position #\, input :from-end t))
                             (before (if comma (subseq input 0 (1+ comma)) ""))
                             (taken (split-choices before)))
                        (append
                         (when (zerop (length (string-trim " " (subseq input (length before)))))
                           (list (make-candidate "" :value before :annotation (if taken more none))))
                         (loop for choice in (funcall offered)
                               for string = (if (consp choice) (car choice) choice)
                               unless (member string taken :test #'string=)
                                 collect (make-candidate string :value (concatenate 'string before string)
                                                                :annotation (and (consp choice) (cdr choice)))))))
                    :separator #\, :initial initial :source source))

(defun read-buffer (prompt &key require-match)
  "Ask PROMPT for a buffer, completing on the live buffers other than the
current one (BUFFER-CANDIDATES), the ones used lately first, so that
\\<minibuffer>\\[minibuffer-done] alone takes the one used last. With
*PREVIEW-BUFFERS*, the buffer selected is shown meanwhile in the window you
are in, what it showed coming back when the question ends. With
REQUIRE-MATCH, one of them must be chosen; else a new name is taken as typed.
Returns the name."
  (call-with-window-preview
   (lambda (candidate made)
     (declare (ignore made))
     (let ((buffer (tuatara.buffer:find-buffer (candidate-string candidate))))
       (and buffer (live-buffer-p buffer) buffer)))
   (lambda (show)
     (completing-read prompt (buffer-candidates) :history (recent-buffer-names) :require-match require-match
                                                 :preview (and *preview-buffers* show)))))

(define-command switch-to-buffer ()
  "Show another buffer, completing on the live buffers, the ones used lately
first (\\<minibuffer>\\[minibuffer-done] alone goes back to the one used
last); a new name creates one. With *preview-buffers*, the buffer selected is
shown meanwhile."
  (let ((name (read-buffer "Switch to buffer: ")))
    (unless (zerop (length name))
      (show-buffer (find-or-make-buffer name)))))

(define-command switch-other-buffer ()
  "Switch to the buffer used before this one (one no other window shows,
when there is one), so that doing it again comes back."
  (show-buffer (other-buffer (current-buffer))))

(defvar *kill-buffer-query-functions* '()
  "Functions of a buffer asked before it is killed; each returns true to go
ahead. A replacement running in the background asks here whether to stop.")

(defvar *quit-query-functions* '()
  "Functions of no arguments asked before the editor exits; each returns NIL
to stay, true to go ahead, or a function of no arguments to call once the
exit is certain (stopping its work), so that staying after all changes
nothing.")

(defun kill-buffer-allowed-p (buffer)
  "True when every one of *KILL-BUFFER-QUERY-FUNCTIONS* lets BUFFER be killed."
  (every (lambda (f) (funcall f buffer)) *kill-buffer-query-functions*))

(defun kill-buffer-asking (buffer &key before)
  "Kill BUFFER, asking first when it has changes not saved, once every one of
*KILL-BUFFER-QUERY-FUNCTIONS* lets it; BEFORE, a function of none, is called
just before it goes. True when it was killed."
  (when (and (kill-buffer-allowed-p buffer)
             (or (not (tuatara.buffer:buffer-modified-p buffer))
                 (yes-or-no-p* (format nil "Buffer ~a modified; kill anyway?" (tuatara.buffer:buffer-name buffer)))))
    (when before (funcall before))
    (tuatara.buffer:kill-buffer buffer)
    t))

(define-command kill-buffer ()
  "Kill the current buffer, asking first when it has changes not saved. Each
window showing it shows the buffer it showed before, or another; a window made
or kept for it closes."
  (kill-buffer-asking (current-buffer)))

(define-command new-empty-buffer ()
  "Show a new empty buffer, untitled (untitled<2> beside one already there),
for text that has no file yet: \\[save-buffer] asks where to save it. Leaving
the editor asks first while it holds text not saved, as for a file."
  (let ((buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "untitled"))))
    (setf (tuatara.buffer:buffer-local buffer 'unsaved-asked) t)
    (show-buffer buffer)
    buffer))

(defun unsaved-p (buffer)
  "True when BUFFER has changes that leaving the editor would lose and asks
about: one visiting a file, or one made by \\[new-empty-buffer], modified."
  (and (or (tuatara.buffer:buffer-file buffer) (tuatara.buffer:buffer-local buffer 'unsaved-asked))
       (tuatara.buffer:buffer-modified-p buffer)))

(defun save-buffers-where (test)
  "Save each buffer visiting a file with changes not saved that TEST, a
function of the buffer, is true of; how many were saved."
  (let ((saved 0))
    (dolist (buffer (copy-list tuatara.buffer:*buffers*) saved)
      (when (and (tuatara.buffer:buffer-file buffer) (tuatara.buffer:buffer-modified-p buffer) (funcall test buffer))
        (let ((*current-buffer* buffer))
          (save-buffer))
        (unless (tuatara.buffer:buffer-modified-p buffer) (incf saved))))))

(define-command save-some-buffers ()
  "Save every buffer visiting a file with changes not saved, after asking once,
the files named: the files a rename across a project changed, say."
  (let ((changed (remove-if-not (lambda (b) (and (tuatara.buffer:buffer-file b) (tuatara.buffer:buffer-modified-p b)))
                                tuatara.buffer:*buffers*)))
    (cond ((null changed) (message "(No files need saving)"))
          ((yes-or-no-p* (format nil "Save ~d changed file~:p: ~{~a~^, ~}?" (length changed) (mapcar #'tuatara.buffer:buffer-name changed)))
           (message "Saved ~d file~:p" (save-buffers-where (lambda (b) (member b changed)))))
          (t (message "Nothing saved")))))

(define-command save-buffers-kill-editor ()
  "Exit the editor, asking first if a buffer visiting a file, or one made by
\\[new-empty-buffer], has unsaved changes. Other buffers without a file, such as the
scratch buffers, are not asked about."
  (let ((answers (loop for query in *quit-query-functions*
                       for answer = (funcall query)
                       unless answer return :stay
                       collect answer)))
    (when (and (not (eq answers :stay))
               (or (notany #'unsaved-p tuatara.buffer:*buffers*)
                   (yes-or-no-p* "Modified buffers exist; exit anyway?")))
      ;; what the questions said to stop, now that the exit is certain
      (dolist (answer answers) (when (functionp answer) (funcall answer)))
      (setf (editor-running *editor*) nil))))
