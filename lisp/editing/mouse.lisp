;;;; lisp/editing/mouse.lisp --- The mouse: putting point, selecting, sizing windows by their edges, files dropped.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.defaults)

(defvar *mouse-anchor* nil
  "What the first mouse button last went down on, for a drag to go on from:
(:TEXT WINDOW POSITION) in a window's text, (:MODE-LINE WINDOW) on its mode
line, (:DIVIDER WINDOW) on the divider at its right; NIL elsewhere.")

(defun mouse-key-cell ()
  "The screen cell of the mouse key just pressed: two values, its column and
row; NIL for a key of the keyboard."
  (let ((key (editor-this-key *editor*)))
    (when key
      (values (tuatara.frontend:key-column key) (tuatara.frontend:key-row key)))))

(defun mouse-goto (position)
  "Move point to POSITION, which the mouse pointed at; in the hex view, put the
hex cursor on that octet."
  (let ((b (current-buffer)))
    (if (tuatara.display:hex-view-p b)
        (setf (tuatara.display:hex-point b) position)
        (goto position))))

(defun window-after-p (window arrangement)
  "True when another window lies right after WINDOW: below it for ARRANGEMENT
:STACKED, to its right for :SIDE-BY-SIDE; its mode line or divider then
parts the two, and dragging it gives rows or columns from one to the other."
  (let ((top (tuatara.display:area-top window)) (left (tuatara.display:area-left window))
        (height (tuatara.display:area-height window)) (width (tuatara.display:area-width window)))
    (some (lambda (other)
            (let ((other-top (tuatara.display:area-top other)) (other-left (tuatara.display:area-left other)))
              (if (eq arrangement :stacked)
                  (and (= other-top (+ top height))
                       (< other-left (+ left width))
                       (< left (+ other-left (tuatara.display:area-width other))))
                  (and (= other-left (+ left width))
                       (< other-top (+ top height))
                       (< top (+ other-top (tuatara.display:area-height other)))))))
          (editor-windows *editor*))))

(defun window-edge-at (column row)
  "Two values for screen cell COLUMN, ROW: :MODE-LINE and the window whose mode
line it is on, :DIVIDER and the window whose divider at its right it is on, or
:HEADER-LINE and the window whose header line it is on; NIL elsewhere."
  (dolist (window (editor-windows *editor*) nil)
    (let* ((top (tuatara.display:area-top window)) (left (tuatara.display:area-left window))
           (right (+ left (tuatara.display:area-width window) -1))
           (mode-line (+ (tuatara.display:window-text-top window) (tuatara.display:window-height window))))
      (cond ((and (= row mode-line) (<= left column right))
             (return (values :mode-line window)))
            ((and (tuatara.display:window-divider window) (= column right) (<= top row (1- mode-line)))
             (return (values :divider window)))
            ((and (= row top) (plusp (tuatara.display:window-header-rows window)) (<= left column right))
             (return (values :header-line window)))))))

(define-command mouse-set-point ()
  "Put point where the first mouse button went down, selecting the window
there, and let go of the selection; dragging then selects from there. On a
window's mode line or header line it selects that window, and dragging the
mode line, or the divider at a window's right, makes the windows beside it
taller or wider."
  (multiple-value-bind (column row) (mouse-key-cell)
    (let ((window (and row (window-at-cell column row))))
      (setf *mouse-anchor* nil)
      (cond (window
             (unless (eq window (editor-window *editor*))
               (select-window window *editor*))
             (setf (tuatara.buffer:buffer-mark-active-p (current-buffer)) nil)
             ;; what is dragged out next is a plain selection, whatever the one before was
             (fresh-selection (current-buffer))
             (mouse-goto (window-cell-position window column row))
             (setf *mouse-anchor* (list :text window (point))))
            (row
             (multiple-value-bind (edge window) (window-edge-at column row)
               (when edge
                 (unless (eq window (editor-window *editor*))
                   (select-window window *editor*))
                 (setf *mouse-anchor* (and (not (eq edge :header-line)) (list edge window))))))))))

(defun drag-edge (edge window column row)
  "Move WINDOW's mode line (EDGE :MODE-LINE) to screen ROW, or the divider at
its right (:DIVIDER) to COLUMN, as far as the windows' smallest sizes allow;
only where another window lies beyond it."
  (let ((arrangement (if (eq edge :mode-line) :stacked :side-by-side)))
    (when (and (member window (editor-windows *editor*)) (window-after-p window arrangement))
      (let ((delta (if (eq edge :mode-line)
                       (- row (+ (tuatara.display:window-text-top window) (tuatara.display:window-height window)))
                       (- column (+ (tuatara.display:area-left window) (tuatara.display:area-width window) -1)))))
        (unless (zerop delta)
          (tuatara.display:resize-area window delta arrangement)
          (fit-windows *editor*))))))

(define-command mouse-drag-region ()
  "Select from where the first mouse button went down to where the mouse is
now, point following it, in the window it went down in; held above or below
that window's text, it scrolls a line at a time. Gone down on a mode line or
a divider, it moves that instead, sizing the windows on either side."
  (let ((anchor *mouse-anchor*))
    (multiple-value-bind (column row) (mouse-key-cell)
      (when (and anchor row)
        (if (member (first anchor) '(:mode-line :divider))
            (drag-edge (first anchor) (second anchor) column row)
            (when (eq (second anchor) (editor-window *editor*))
              (let* ((window (second anchor))
                     (b (current-buffer))
                     (top (tuatara.display:window-text-top window))
                     (bottom (+ top (tuatara.display:window-height window)))
                     (rows (tuatara.display:window-rows window)))
                (cond ((< row top)
                       (when (plusp (tuatara.display:window-start window))
                         (tuatara.display:window-scroll window -1))
                       (setf row top))
                      ((>= row bottom)
                       (when (and (plusp (length rows)) (not (tuatara.display:row-last-p (aref rows (1- (length rows))))))
                         (tuatara.display:window-scroll window 1))
                       (setf row (1- bottom))))
                (setf (tuatara.buffer:buffer-mark b) (min (third anchor) (tuatara.buffer:buffer-size b))
                      (tuatara.buffer:buffer-mark-active-p b) t)
                (mouse-goto (window-cell-position window column row)))))))))

(define-command mouse-yank-primary ()
  "Insert where the middle button was pressed the text selected last in any
program (the desktop's primary selection); the mark at its start."
  (let ((text (tuatara.frontend:frontend-primary-selection (editor-frontend *editor*))))
    (unless (and text (plusp (length text))) (error "Nothing is selected anywhere"))
    (mouse-set-point)
    (let ((p (point)))
      (push-mark)
      (goto (tuatara.buffer:buffer-insert (current-buffer) p (text-for-buffer text (current-buffer) p))))))

(defun send-to-primary-selection ()
  "Make the text selected in the current buffer the desktop's primary
selection, as *USE-SYSTEM-CLIPBOARD* says: when there is some, no more than
*CLIPBOARD-MAX-SIZE* octets, and none of an encrypted file's text."
  (let ((b (current-buffer)))
    (when (and *use-system-clipboard* (tuatara.buffer:buffer-mark-active-p b)
               (not (tuatara.buffer:buffer-local b :plaintext)))
      (let ((start (min (point) (tuatara.buffer:buffer-mark b)))
            (end (max (point) (tuatara.buffer:buffer-mark b))))
        (when (< 0 (- end start) *clipboard-max-size*)
          (tuatara.frontend:frontend-set-primary-selection
           (editor-frontend *editor*) (tuatara.buffer:buffer-string b :start start :end end)))))))

(define-command mouse-release ()
  "The first mouse button let go: a drag ends there, what it selected staying
selected and becoming the desktop's primary selection, which the middle
button pastes in other programs."
  (when (mouse-in-text-p) (send-to-primary-selection))
  (setf *mouse-anchor* nil))

(defun mouse-select (start end)
  "Select from START to END, point at END, a drag going on from START: a
plain selection, whatever kind the one before was (a rectangle)."
  (select-bounds (current-buffer) start end t)
  (setf (third *mouse-anchor*) start))

(defun mouse-in-text-p ()
  "True when the first mouse button last went down in a window's text."
  (eq (first *mouse-anchor*) :text))

(defun click-class (char)
  "What a run of characters clicked twice is made of around CHAR: :WORD for
letters, digits and _, :SPACE for spaces and tabs, :OTHER for the rest; NIL at
a line's end."
  (cond ((or (null char) (char= char #\Newline) (char= char #\Return)) nil)
        ((name-char-p char) :word)
        ((or (char= char #\Space) (char= char #\Tab)) :space)
        (t :other)))

(defparameter *click-run-reach* 4096
  "The most octets either side of a place clicked twice that the run of
characters selected reaches.")

(defun click-run (buffer position)
  "The run of characters of one CLICK-CLASS around POSITION in BUFFER, as two
values, its start and end, within *CLICK-RUN-REACH* octets either side; on a bracket, the bracketed group, or the bracket
alone when it has no partner. NIL at a line's end with nothing before it."
  (let* ((char (tuatara.buffer:buffer-char-at buffer position))
         (before (and (plusp position)
                      (tuatara.buffer:buffer-char-at buffer (tuatara.buffer:buffer-forward-char buffer position -1))))
         (class (click-class char)))
    (cond ((and char (find char tuatara.text:*openers*))
           (values position (sexp-end-after buffer position)))
          ((and char (find char tuatara.text:*closers*))
           (let ((after (tuatara.buffer:buffer-forward-char buffer position)))
             (multiple-value-bind (start balanced) (sexp-start-before buffer after)
               (values (if balanced start position) after))))
          ((or class (click-class before))
           (let ((class (or class (click-class before))))
             (word-bounds-at buffer position (lambda (c) (eq class (click-class c))) *click-run-reach*))))))

(define-command mouse-select-word ()
  "Select the word the first mouse button went down on twice, point at its end:
the run of letters, digits and _ there, or of spaces, or of other signs; on a
bracket, the bracketed group."
  (mouse-set-point)
  (let ((b (current-buffer)))
    (when (and (mouse-in-text-p) (not (tuatara.display:hex-view-p b)))
      (multiple-value-bind (start end) (click-run b (point))
        (when (and start (< start end))
          (mouse-select start end))))))

(define-command mouse-select-line ()
  "Select the line the first mouse button went down on three times, its line
break too."
  (mouse-set-point)
  (let ((b (current-buffer)))
    (when (and (mouse-in-text-p) (not (tuatara.display:hex-view-p b)))
      (let ((end (tuatara.buffer:buffer-line-end b (point))))
        (mouse-select (tuatara.buffer:buffer-line-start b (point))
                      (if (< end (tuatara.buffer:buffer-size b)) (tuatara.buffer:buffer-forward-char b end) end))))))

;;; Files dropped on the window

(defvar *open-dropped-files-safely* nil
  "True to open files dropped on the window safely, as
\\[universal-argument] \\[find-file] does: read-only, no mode, nothing written
beside them. For a window that files from sources not trusted are dropped on.")

(defun visit-dropped (path)
  "Visit PATH, a file dropped on the window, as \\[find-file] does (safely
when *OPEN-DROPPED-FILES-SAFELY*), with VISIT-PATH, without showing it: a PDF
or a picture as such, a binary file in the hex view, why a file opened safely
said. Its buffer, or NIL and why not."
  (handler-case
      (multiple-value-bind (buffer existing) (visit-path path :safe *open-dropped-files-safely* :show #'identity)
        (if (and *open-dropped-files-safely* existing (not (tuatara.io:safe-visit-p buffer)))
            (values buffer (format nil "~a is open already, with its mode" (tuatara.buffer:buffer-name buffer)))
            buffer))
    (error (e) (values nil (princ-to-string e)))))

(define-command open-dropped-files ()
  "Visit the files dropped on the window, in the window they were dropped on,
as \\[find-file] does, the last one shown; safely, as
\\[universal-argument] \\[find-file] does, when *OPEN-DROPPED-FILES-SAFELY* is
true. A directory, a named pipe or a device is not opened, and the echo area
says so."
  (let ((drop *dropped*))
    (if (null drop)
        (message "Drop files on the window to open them")
        (let* ((column (tuatara.frontend:event-column drop))
               (row (tuatara.frontend:event-row drop))
               (window (and column row (window-at-cell column row)))
               (shown nil) (count 0) (troubles '()))
          (when (and window (not (eq window (editor-window *editor*))))
            (select-window window *editor*))
          (dolist (path (tuatara.frontend:event-files drop))
            (multiple-value-bind (buffer trouble) (visit-dropped path)
              (when buffer (setf shown buffer) (incf count))
              (when trouble (push trouble troubles))))
          (when shown (show-buffer shown))
          ;; one file alone: what its visit said stays (why it opened safely)
          (cond (troubles (message "~{~a~^; ~}" (reverse troubles)))
                ((> count 1) (message "Opened ~d files; \\[switch-to-buffer] lists them" count)))))))

;; a mouse key's place is gone by the time . would run it again
(dolist (command '(mouse-set-point mouse-drag-region mouse-select-word mouse-select-line scroll-by-wheel))
  (pushnew command *not-repeatable*))
