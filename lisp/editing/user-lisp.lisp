;;;; lisp/editing/user-lisp.lisp --- Emacs's names for the basics, for the Lisp one types.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The Lisp typed at M-:, in *lisp-scratch* and in the init file is read in
;;;; TUATARA-USER. These are the functions an Emacs user reaches for there, by
;;;; Emacs's names and to Emacs's rules: a position is a count of characters
;;;; from 1, the start of the text being (point-min). Inside the editor
;;;; positions are octet offsets from 0; the functions here convert.

(in-package #:tuatara.defaults)

(defun buffer-named (buffer-or-name)
  "The buffer BUFFER-OR-NAME names: a buffer, the name of a live one, or NIL
for the current buffer. Signals an error when there is no such buffer."
  (cond ((null buffer-or-name) (current-buffer))
        ((typep buffer-or-name 'tuatara.buffer:buffer) buffer-or-name)
        ((stringp buffer-or-name)
         (or (tuatara.buffer:find-buffer buffer-or-name) (error "No buffer named ~a" buffer-or-name)))
        (t (error "Wrong type argument: ~s is neither a buffer nor a buffer's name" buffer-or-name))))

(defun position-at (buffer byte)
  "Offset BYTE of BUFFER as a position: characters counted from 1."
  (tuatara.buffer:ensure-indexed buffer)
  (1+ (tuatara.text:byte->char (tuatara.buffer:buffer-text buffer) byte)))

(defun byte-at (buffer position &key clamp)
  "The offset of POSITION, characters counted from 1, in BUFFER. Outside the
text, or the part shown while BUFFER is narrowed, it signals an error, or
with CLAMP is taken to the nearer end."
  (tuatara.buffer:ensure-indexed buffer)
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (first (position-at buffer (tuatara.buffer:buffer-start buffer)))
         (last (position-at buffer (tuatara.buffer:buffer-end buffer))))
    (unless (integerp position)
      (error "Wrong type argument: ~s is not a position" position))
    (unless (or clamp (<= first position last))
      (error "Args out of range: position ~d is outside ~d to ~d" position first last))
    (tuatara.text:char->byte rope (1- (max first (min last position))))))

(defun range-at (buffer start end)
  "Two offsets for positions START and END of BUFFER, the lower first."
  (let ((a (byte-at buffer start)) (b (byte-at buffer end)))
    (values (min a b) (max a b))))

;;; Point and positions

(defun tuatara-user:point ()
  "Return the value of point in the current buffer, as a position: the
characters before it counted from 1."
  (let ((b (current-buffer)))
    (position-at b (tuatara.buffer:buffer-point b))))

(defun tuatara-user:point-min ()
  "Return the position of the start of the current buffer's text: 1, or
while it is narrowed (\\[narrow-to-region]) the start of the part shown."
  (let ((b (current-buffer)))
    (position-at b (tuatara.buffer:buffer-start b))))

(defun tuatara-user:point-max ()
  "Return the position of the end of the current buffer's text: one more than
the characters in it, or while it is narrowed the end of the part shown."
  (let ((b (current-buffer)))
    (position-at b (tuatara.buffer:buffer-end b))))

(defun tuatara-user:goto-char (position)
  "Set point to POSITION, characters counted from 1, taken to the start or end
of the text when it is outside it, or of the part shown while the buffer is
narrowed; return POSITION."
  (let ((b (current-buffer)))
    (setf (tuatara.buffer:buffer-point b) (byte-at b position :clamp t))
    position))

(defun tuatara-user:bobp ()
  "Return true when point is at the start of the current buffer (of the part
shown, while it is narrowed)."
  (let ((b (current-buffer)))
    (= (tuatara.buffer:buffer-point b) (tuatara.buffer:buffer-start b))))

(defun tuatara-user:eobp ()
  "Return true when point is at the end of the current buffer (of the part
shown, while it is narrowed)."
  (let ((b (current-buffer)))
    (= (tuatara.buffer:buffer-point b) (tuatara.buffer:buffer-end b))))

(defun shown-line-start (buffer byte)
  "The start of the line holding BYTE in BUFFER, no further back than the
start of the part shown while it is narrowed."
  (tuatara.buffer:within-narrowing buffer (tuatara.buffer:buffer-line-start buffer byte)))

(defun shown-line-end (buffer byte)
  "The end of the line holding BYTE in BUFFER, before its newline, no further
than the end of the part shown while it is narrowed."
  (tuatara.buffer:within-narrowing buffer (tuatara.buffer:buffer-line-end buffer byte)))

(defun tuatara-user:bolp ()
  "Return true when point is at the start of a line, or of the part shown
while the buffer is narrowed."
  (let* ((b (current-buffer)) (p (tuatara.buffer:buffer-point b)))
    (= p (shown-line-start b p))))

(defun tuatara-user:eolp ()
  "Return true when point is at the end of a line, or of the part shown while
the buffer is narrowed."
  (let* ((b (current-buffer)) (p (tuatara.buffer:buffer-point b)))
    (= p (shown-line-end b p))))

(defun tuatara-user:char-after (&optional position)
  "Return the character after POSITION, point when it is not given, or NIL at
the end of the text (of the part shown, while the buffer is narrowed)."
  (let* ((b (current-buffer))
         (byte (if position (byte-at b position :clamp t) (tuatara.buffer:buffer-point b))))
    (and (< byte (tuatara.buffer:buffer-end b)) (tuatara.buffer:buffer-char-at b byte))))

(defun tuatara-user:char-before (&optional position)
  "Return the character before POSITION, point when it is not given, or NIL at
the start of the text (of the part shown, while the buffer is narrowed)."
  (let* ((b (current-buffer))
         (byte (if position (byte-at b position :clamp t) (tuatara.buffer:buffer-point b))))
    (and (> byte (tuatara.buffer:buffer-start b))
         (tuatara.buffer:buffer-char-at b (tuatara.buffer:buffer-forward-char b byte -1)))))

;;; Lines

(defun line-start-lines-on (buffer byte lines)
  "Two values: the start of the line LINES lines on from the one holding BYTE
in BUFFER (back when negative), and how many of them could not be gone, the
text ending sooner; while BUFFER is narrowed, the lines of the part shown,
its first beginning where the part does."
  (multiple-value-bind (low size) (tuatara.buffer:buffer-bounds buffer)
    (let ((p (shown-line-start buffer byte)) (moved 0))
      (if (minusp lines)
          (loop repeat (- lines)
                while (> p low)
                do (setf p (shown-line-start buffer (1- p)))
                   (incf moved))
          (loop repeat lines
                do (let ((end (tuatara.text:rope-line-end (tuatara.buffer:buffer-text buffer) p)))
                     (if (< end size)
                         (setf p (1+ end) moved (1+ moved))
                         (return)))))
      (values p (- (abs lines) moved)))))

(defun tuatara-user:line-beginning-position (&optional (n 1))
  "Return the position where the current line starts; with N other than 1, the
start of the line N - 1 lines on (back when negative)."
  (let ((b (current-buffer)))
    (position-at b (line-start-lines-on b (tuatara.buffer:buffer-point b) (1- n)))))

(defun tuatara-user:line-end-position (&optional (n 1))
  "Return the position where the current line ends, before its newline; with N
other than 1, the end of the line N - 1 lines on (back when negative)."
  (let ((b (current-buffer)))
    (position-at b (shown-line-end b (line-start-lines-on b (tuatara.buffer:buffer-point b) (1- n))))))

(defun tuatara-user:forward-line (&optional (n 1))
  "Move point to the start of the line N lines on (back when negative) and
return how many lines could not be gone, the text (or the part shown, while
the buffer is narrowed) ending first: 0 when all were. A last line without a
newline counts as gone to when point ends at its end."
  (let* ((b (current-buffer)) (from (tuatara.buffer:buffer-point b)))
    (multiple-value-bind (p short) (line-start-lines-on b from n)
      (when (and (plusp n) (plusp short))
        ;; lines ran out: to the end of the text, which counts as a line gone
        ;; when point moved to the end of one not ended by a newline
        (setf p (tuatara.buffer:buffer-end b))
        (when (and (/= p from) (/= 10 (tuatara.text:rope-octet (tuatara.buffer:buffer-text b) (1- p))))
          (decf short)))
      (setf (tuatara.buffer:buffer-point b) p)
      (if (minusp n) (- short) short))))

(defun tuatara-user:forward-char (&optional (n 1))
  "Move point N characters on (back when negative). At the start or end of the
text, or of the part shown while the buffer is narrowed, it stops there and
signals an error."
  (let* ((b (current-buffer)) (p (tuatara.buffer:buffer-point b))
         (to (tuatara.buffer:within-narrowing b (tuatara.buffer:buffer-forward-char b p n)))
         (moved (abs (- (position-at b to) (position-at b p)))))
    (setf (tuatara.buffer:buffer-point b) to)
    (when (< moved (abs n))
      (error (if (minusp n) "Beginning of buffer" "End of buffer")))
    nil))

(defun tuatara-user:backward-char (&optional (n 1))
  "Move point N characters back (on when negative); see FORWARD-CHAR."
  (tuatara-user:forward-char (- n)))

(defun tuatara-user:count-words (start end)
  "Return how many words there are between positions START and END: runs of
letters and digits."
  (let ((b (current-buffer)))
    (multiple-value-bind (low high) (range-at b start end)
      (nth-value 2 (count-text (tuatara.buffer:buffer-text b) low high)))))

(defun tuatara-user:line-number-at-pos (&optional position absolute)
  "Return the number of the line POSITION (point when NIL) is on, the first
line being 1: of the part shown while the buffer is narrowed, or with
ABSOLUTE of the whole buffer."
  (let* ((b (current-buffer))
         (byte (if position (byte-at b position) (tuatara.buffer:buffer-point b))))
    (1+ (tuatara.text:rope-count-newlines (tuatara.buffer:buffer-text b)
                                          (if absolute 0 (tuatara.buffer:buffer-start b)) byte))))

(defun tuatara-user:count-lines (start end)
  "Return how many lines there are between positions START and END: the
newlines between them, and one more when they differ and the greater is not
at the start of a line."
  (let ((b (current-buffer)))
    (multiple-value-bind (low high) (range-at b start end)
      (let ((newlines (tuatara.text:rope-count-newlines (tuatara.buffer:buffer-text b) low high)))
        (if (and (< low high) (/= high (tuatara.buffer:buffer-line-start b high)))
            (1+ newlines)
            newlines)))))

;;; Text

(defun tuatara-user:insert (&rest arguments)
  "Insert ARGUMENTS, strings and characters, at point, point going after them."
  (let ((b (current-buffer))
        (text (with-output-to-string (out)
                (dolist (a arguments)
                  (etypecase a
                    (string (write-string a out))
                    (character (write-char a out)))))))
    (when (plusp (length text))
      (setf (tuatara.buffer:buffer-point b)
            (tuatara.buffer:buffer-insert b (tuatara.buffer:buffer-point b) text)))
    nil))

(defun tuatara-user:delete-region (start end)
  "Delete the text between positions START and END."
  (let ((b (current-buffer)))
    (multiple-value-bind (low high) (range-at b start end)
      (tuatara.buffer:buffer-delete b low high)
      nil)))

(defun tuatara-user:erase-buffer ()
  "Delete all the text of the current buffer, widening it when it is narrowed."
  (let ((b (current-buffer)))
    (tuatara.buffer:widen-buffer b)
    (tuatara.buffer:buffer-delete b 0 (tuatara.buffer:buffer-size b))
    nil))

(defun tuatara-user:buffer-substring (start end)
  "Return the text between positions START and END as a string."
  (let ((b (current-buffer)))
    (multiple-value-bind (low high) (range-at b start end)
      (tuatara.buffer:buffer-string b :start low :end high))))

(defun tuatara-user:buffer-string ()
  "Return the text of the current buffer as a string: all of it, or while it
is narrowed (\\[narrow-to-region]) the part shown."
  (let ((b (current-buffer)))
    (tuatara.buffer:buffer-string b :start (tuatara.buffer:buffer-start b) :end (tuatara.buffer:buffer-end b))))

(defun tuatara-user:buffer-size (&optional buffer)
  "Return how many characters BUFFER holds, the current buffer when it is not given."
  (let ((b (buffer-named buffer)))
    (1- (position-at b (tuatara.buffer:buffer-size b)))))

(defun tuatara-user:kill-new (string)
  "Put STRING first on the kill ring, and on the system clipboard as kills go."
  (push-kill string)
  (send-to-clipboard string)
  string)

;;; Buffers

(defun tuatara-user:buffer-name (&optional buffer)
  "Return the name of BUFFER, the current buffer when it is not given."
  (tuatara.buffer:buffer-name (buffer-named buffer)))

(defun tuatara-user:buffer-file-name (&optional buffer)
  "Return the name of the file BUFFER visits, the current buffer when it is not
given, or NIL when it visits none."
  (let ((file (tuatara.buffer:buffer-file (buffer-named buffer))))
    (and file (tuatara.io:native file))))

(defun tuatara-user:get-buffer (buffer-or-name)
  "Return the live buffer BUFFER-OR-NAME names, or NIL when there is none."
  (if (typep buffer-or-name 'tuatara.buffer:buffer)
      (and (live-buffer-p buffer-or-name) buffer-or-name)
      (tuatara.buffer:find-buffer buffer-or-name)))

(defun tuatara-user:get-buffer-create (name)
  "Return the buffer called NAME, made, empty, when there is none."
  (find-or-make-buffer name))

(defun tuatara-user:buffer-list ()
  "Return a list of the live buffers."
  (copy-list tuatara.buffer:*buffers*))

(defun tuatara-user:set-buffer (buffer-or-name)
  "Make BUFFER-OR-NAME the current buffer until the command ends, without
showing it: what the functions here do goes to it. Returns the buffer."
  (setf *current-buffer* (buffer-named buffer-or-name)))

(defmacro tuatara-user:with-current-buffer (buffer-or-name &body body)
  "Evaluate BODY with BUFFER-OR-NAME as the current buffer, not shown, and
return the value of its last form."
  `(let ((*current-buffer* (buffer-named ,buffer-or-name)))
     ,@body))

(defun call-saving-excursion (function)
  "Call FUNCTION, then make the buffer current before it current again, and its
point where it was, followed through the edits made meanwhile."
  (let* ((buffer (current-buffer))
         (anchor (tuatara.text:make-anchor (tuatara.buffer:buffer-version buffer)
                                           (tuatara.buffer:buffer-point buffer))))
    (unwind-protect (let ((*current-buffer* buffer)) (funcall function))
      (when (live-buffer-p buffer)
        (setf (tuatara.buffer:buffer-point buffer)
              (tuatara.text:anchor-position anchor (tuatara.buffer:buffer-version buffer)))))))

(defmacro tuatara-user:save-excursion (&body body)
  "Evaluate BODY, then make the buffer current before it current again, with its
point where it was (moved along by the text inserted or deleted before it);
return the value of BODY's last form."
  `(call-saving-excursion (lambda () ,@body)))

(defun tuatara-user:switch-to-buffer (buffer-or-name)
  "Show BUFFER-OR-NAME in the selected window, made first when no buffer has
that name, and return it."
  (let ((b (if (stringp buffer-or-name)
               (find-or-make-buffer buffer-or-name)
               (buffer-named buffer-or-name))))
    (show-buffer b)
    b))

(defun tuatara-user:find-file (filename)
  "Visit the file FILENAME in a buffer, as \\[find-file] does, show it and return it."
  (visit-path (tuatara.io:expand-path filename)))
