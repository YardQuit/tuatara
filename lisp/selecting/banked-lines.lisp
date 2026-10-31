;;;; lisp/selecting/banked-lines.lisp --- Banked lines: lines set aside, to copy, cut or paste over together.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.defaults)

;;; Banks: stretches of whole lines, kept by anchors that follow the text

(defun buffer-banks (buffer)
  "BUFFER's banks, each (START END LAST): anchors at the start of its first
line, past its last line's newline, and at the end of its last line's text.
BANKED-SPANS says where the banked lines are now; setting this to NIL lets
go of every one of them, the text left as it is."
  (tuatara.buffer:buffer-local buffer 'banked-lines))

(defun (setf buffer-banks) (banks buffer)
  "Set BUFFER's banks to BANKS."
  (setf (tuatara.buffer:buffer-local buffer 'banked-lines) banks))

(defun bank-bounds (buffer bank)
  "Two values: where BANK of BUFFER starts and ends now, the end past the
newline of its last line; the end no further than the start once its lines
have gone."
  (let ((version (tuatara.buffer:buffer-version buffer)))
    (destructuring-bind (start end last) bank
      (let ((s (tuatara.text:anchor-position start version))
            (e (tuatara.text:anchor-position end version)))
        (if (< s e)
            (values s (min e (line-after buffer (max s (tuatara.text:anchor-position last version)))))
            (values s e))))))

(defun make-bank (buffer start end)
  "A bank of BUFFER's whole lines from START below END, which follows them
through edits: text put in just before its first line, or after the text of
its last line (a line opened below it), stays outside it."
  (let* ((version (tuatara.buffer:buffer-version buffer))
         (last (tuatara.buffer:buffer-line-end
                buffer (if (and (> end start) (= end (tuatara.buffer:buffer-line-start buffer end)))
                           (tuatara.buffer:buffer-forward-char buffer end -1)
                           end))))
    (list (tuatara.text:make-anchor version start :bias :after)
          (tuatara.text:make-anchor version end)
          (tuatara.text:make-anchor version (max start (min last end))))))

(defun banked-spans (buffer)
  "BUFFER's banked lines in order, each (START . END) widened to the whole lines
it touches, those that come to share a line made one. Banks whose text has
gone are forgotten."
  (let ((live '()) (spans '()))
    (dolist (bank (buffer-banks buffer))
      (multiple-value-bind (start end) (bank-bounds buffer bank)
        (when (< start end)
          (push bank live)
          (push (whole-lines-span buffer start end) spans))))
    (setf (buffer-banks buffer) (nreverse live))
    ;; banks that only touch stay apart: each is undone on its own
    (tuatara.text:merge-ranges spans :touching nil)))

(defun banked-p (&optional (buffer (current-buffer)))
  "True when BUFFER has lines banked."
  (and (buffer-banks buffer) (banked-spans buffer) t))

(defun span-line-count (buffer spans)
  "How many lines of BUFFER SPANS, each (START . END), take in."
  (loop for (start . end) in spans
        sum (if (< start end)
                (1+ (- (tuatara.buffer:buffer-line-number buffer (tuatara.buffer:buffer-forward-char buffer end -1))
                       (tuatara.buffer:buffer-line-number buffer start)))
                0)))

(defun banked-line-count (buffer)
  "How many lines of BUFFER are banked."
  (span-line-count buffer (banked-spans buffer)))

(defun bank-lines (buffer span)
  "Bank the whole lines of SPAN, (START . END), in BUFFER, those search
filters hide left out (TUATARA.DISPLAY:SHOWN-PARTS)."
  (dolist (part (tuatara.display:shown-parts buffer (car span) (cdr span)))
    (push (make-bank buffer (car part) (cdr part)) (buffer-banks buffer))))

(defun unbank-lines (buffer span)
  "Take the whole lines of SPAN, (START . END), of BUFFER out of its banks,
the banks around them kept."
  (destructuring-bind (start . end) span
    (setf (buffer-banks buffer)
          (loop for bank in (buffer-banks buffer)
                append (multiple-value-bind (s e) (bank-bounds buffer bank)
                         (if (or (<= e start) (>= s end))
                             (list bank)
                             (remove nil (list (and (< s start) (make-bank buffer s start))
                                               (and (> e end) (make-bank buffer end e))))))))))

(defun banked-run-at (buffer position)
  "The run of banked lines of BUFFER that takes in POSITION's line, as
(START . END), those beside each other being one; NIL when it is not banked."
  (let ((line (tuatara.display:visible-line-start buffer position)))
    (find-if (lambda (span) (and (<= (car span) line) (< line (cdr span))))
             (tuatara.text:merge-ranges (banked-spans buffer)))))

(defun lines-banked-p (buffer span)
  "True when every line of SPAN, (START . END), of BUFFER that search filters
do not hide is banked."
  (every (lambda (part)
           (let ((run (banked-run-at buffer (car part))))
             (and run (>= (cdr run) (cdr part)))))
         (tuatara.display:shown-parts buffer (car span) (cdr span))))

(defun effective-line-spans (buffer)
  "What \\<normal>\\[copy-selection-or-char], \\[delete-selection-or-char] and \\[yank-over-selection] take
while lines are banked: the banked lines, and the lines of the selection made at
the time; in order, those beside each other made one, the lines search
filters hide left out (TUATARA.DISPLAY:SHOWN-PARTS)."
  (loop for (start . end) in (tuatara.text:merge-ranges
                              (sort (append (banked-spans buffer)
                                            (multiple-value-bind (start end) (selection-range)
                                              (and start (< start end) (list (whole-lines-span buffer start end)))))
                                    #'< :key #'car))
        append (tuatara.display:shown-parts buffer start end)))

(defun spend-banks (buffer spans)
  "Take the lines of SPANS out of BUFFER's banks: what \\<normal>\\[copy-selection-or-char],
\\[delete-selection-or-char] or \\[yank-over-selection] acted on."
  (dolist (span spans) (unbank-lines buffer span)))

(defun banked-highlights (editor window rows)
  "The banked lines among ROWS of WINDOW, to be shown in :banked-line."
  (declare (ignore editor))
  (let ((buffer (tuatara.display:window-buffer window)))
    (when (and (buffer-banks buffer) (plusp (length rows)))
      (let ((from (tuatara.display:row-start (aref rows 0)))
            (to (tuatara.display:row-end (aref rows (1- (length rows))))))
        (loop for bank in (buffer-banks buffer)
              for span = (multiple-value-bind (start end) (bank-bounds buffer bank)
                           (and (< start end) (<= start to) (>= end from)
                                (whole-lines-span buffer start end)))
              when span collect (list (car span) (cdr span) :banked-line))))))

;;; The keys

(defun bank-total-note (buffer)
  "The count of BUFFER's banked lines, for the echo area."
  (format nil "(~d total)" (banked-line-count buffer)))

(define-command bank-selected-lines ()
  "Bank the lines the selection touches, or the line at point: set them aside,
highlighted, and let go of the selection, to go on and bank more elsewhere.
\\<normal>\\[copy-selection-or-char], \\[delete-selection-or-char] and \\[yank-over-selection] then act on
every banked line at once, with the lines of a selection made at the time;
\\[tuatara-change] does not. On lines all banked already it takes them out again,
and on lines partly banked it banks the rest. With a count, it banks that many
lines from point. A line whose end a fold hides is banked with all the fold
hides, as a folded headline with its body; the lines search filters hide are
not banked, and what is banked before they hide it is taken without them.
Banked lines outlast \\[tuatara-quit]: \\[unbank-line],
\\[unbank-section] and \\[clear-banked-lines] take them back."
  (let* ((b (current-buffer)) (count (prefix-argument)))
    (multiple-value-bind (start end) (selection-range)
      (cond ((or (and start (< start end)) (and count (> count 1)))
             (let* ((span (if (and start (< start end))
                              (whole-lines-span b start end)
                              (cons (tuatara.display:visible-line-start b (point))
                                    (line-after b (line-start-away b (point) (1- count))))))
                    (parts (tuatara.display:shown-parts b (car span) (cdr span)))
                    (lines (span-line-count b parts))
                    (left-out (if (rest parts) (tuatara.display:lines-left-out b (car span) (cdr span) parts) 0))
                    (unbanking (lines-banked-p b span)))
               (if unbanking (unbank-lines b span) (bank-lines b span))
               (setf (tuatara.buffer:buffer-mark-active-p b) nil)
               (line-selection-clear b)
               (message "~:[Banked~;Unbanked~] ~d line~:p ~a~a~:[: navigate, then \\<normal>\\[copy-selection-or-char]/\\[delete-selection-or-char]/\\[yank-over-selection]~;~]"
                        unbanking lines (bank-total-note b) (left-out-note left-out) unbanking)))
            ((banked-run-at b (point))
             (unbank-lines b (whole-lines-span b (point) (point)))
             (message "Unbanked this line ~a" (bank-total-note b)))
            (t
             (let ((span (whole-lines-span b (point) (point))))
               (if (>= (car span) (cdr span))
                   (message "Nothing to bank: empty final line")
                   (progn (bank-lines b span)
                          (message "Banked this line ~a: navigate, then \\<normal>\\[copy-selection-or-char]/\\[delete-selection-or-char]/\\[yank-over-selection]"
                                   (bank-total-note b))))))))))

(define-command unbank-line ()
  "Take the line at point out of the bank, the other banked lines kept; on a
line not banked, say so."
  (let ((b (current-buffer)))
    (if (banked-run-at b (point))
        (progn (unbank-lines b (whole-lines-span b (point) (point)))
               (message "Unbanked this line ~a" (bank-total-note b)))
        (message "No banked line at point"))))

(define-command unbank-section ()
  "Take the run of banked lines point is on out of the bank, all the lines
beside each other at once; the banks elsewhere are kept."
  (let* ((b (current-buffer)) (run (banked-run-at b (point))))
    (if run
        (let ((lines (span-line-count b (list run))))
          (unbank-lines b run)
          (message "Unbanked ~d line~:p ~a" lines (bank-total-note b)))
        (message "No banked section at point"))))

(define-command clear-banked-lines ()
  "Let go of every banked line of this buffer; the text is left as it is."
  (let* ((b (current-buffer)) (count (banked-line-count b)))
    (setf (buffer-banks b) '())
    (if (zerop count)
        (message "No banked lines")
        (message "Discarded ~d banked line~:p" count))))

(defun copy-banked ()
  "\\<normal>\\[copy-selection-or-char] on banked lines: copy them, with the lines of any
selection, as one kill, and take them out of the bank."
  (let* ((b (current-buffer)) (spans (effective-line-spans b)) (lines (span-line-count b spans)))
    (kill-text (span-texts b spans) :append nil)
    (spend-banks b spans)
    (setf (tuatara.buffer:buffer-mark-active-p b) nil)
    (line-selection-clear b)
    (message "Copied ~d line~:p" lines)))

(defun delete-banked ()
  "\\<normal>\\[delete-selection-or-char] on banked lines: kill them, with the lines of any
selection, as one kill and one change (DELETE-SPANS). When that would take
part of the text a fold hides, but not all of it, nothing is killed: the fold
is shown instead, and the key pressed again kills."
  (let* ((b (current-buffer)) (spans (effective-line-spans b)) (lines (span-line-count b spans)))
    (delete-spans b spans :before (lambda ()
                                    (kill-text (span-texts b spans) :append nil)
                                    (spend-banks b spans)))
    (setf (tuatara.buffer:buffer-mark-active-p b) nil)
    (line-selection-clear b)
    (message "Deleted ~d line~:p" lines)))

(defun yank-over-banked (text)
  "\\<normal>\\[yank-over-selection] on banked lines: put TEXT in place of them and of
the lines of any selection, as one change (DELETE-SPANS), where the first of them began. When
the first ended in a newline and TEXT does not, the newline is put back after
it, so the lines below stay lines of their own. When that would take part of
the text a fold hides, but not all of it, nothing is replaced: the fold is
shown instead, and the key pressed again replaces."
  (let* ((b (current-buffer)) (spans (effective-line-spans b)) (lines (span-line-count b spans))
         (first (first spans))
         (newline (tuatara.buffer:buffer-newline b))
         (restore (and (plusp (length text))
                       (< (car first) (cdr first))
                       (member (char-before-position b (cdr first)) '(#\Newline #\Return))
                       (not (member (char text (1- (length text))) '(#\Newline #\Return)))))
         (insert (if restore (concatenate 'string text newline) text)))
    (let ((after (aref (delete-spans b spans :replacements (cons insert (make-list (length (rest spans))))
                                             :before (lambda () (spend-banks b spans)))
                       0)))
      (setf (tuatara.buffer:buffer-mark-active-p b) nil)
      (line-selection-clear b)
      (goto (- after (if restore (tuatara.text:string-octet-length newline) 0)))
      (message "Replaced ~d line~:p" lines))))

(defun install-banks ()
  "Show banked lines in their face."
  (pushnew 'banked-highlights *highlight-functions*))
