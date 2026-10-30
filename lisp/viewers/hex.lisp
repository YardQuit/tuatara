;;;; lisp/viewers/hex.lisp --- The hex view's commands: moving by octets, overwriting, finding octets.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; M-x hex-mode shows the current buffer as a hex dump (the display's hex view)
;;;; and back. In it the ordinary motion keys, and NORMAL state's, move by octets and
;;;; by rows of sixteen; M-g g asks for an offset; typing overwrites octets rather
;;;; than inserting: in the hex digits, hex digits, a half octet each; in the
;;;; text beside them, characters; TAB goes from one to the other, as hex
;;;; editors do. hex-set-byte writes one octet given in hex, and
;;;; hex-search-forward and hex-search-backward find a sequence of octets. The
;;;; mode line shows the hex point's offset and where typing goes. Nothing
;;;; about the text changes but what is typed over it, so a file safely opened
;;;; stays as it was.

(in-package #:tuatara.defaults)

(defun hex-view-here-p ()
  "True when the current buffer is shown as a hex dump."
  (tuatara.display:hex-view-p (current-buffer)))

(defun hex-motion (kind &optional (n 1))
  "In the hex view, move its cursor: N octets (:OCTET), N rows (:ROW), to the
start or end of its row (:ROW-START, :ROW-END), or to the text's start or end
(:START, :END). True when the current buffer is in the hex view; NIL, nothing
done, when not, for the ordinary motion to go on."
  (when (hex-view-here-p)
    (let* ((b (current-buffer))
           (p (tuatara.display:hex-point b))
           (size (tuatara.buffer:buffer-size b))
           (row (tuatara.display:hex-row-start p)))
      (setf (tuatara.display:hex-point b)
            (ecase kind
              (:octet (+ p n))
              (:row (+ p (* n tuatara.display:+hex-row+)))
              (:row-start row)
              (:row-end (min size (+ row (1- tuatara.display:+hex-row+))))
              (:start 0)
              (:end size)))
      t)))

(defun hex-overwrite (octets &key group)
  "Write OCTETS over as many octets at the hex point of the current buffer
(past the end of the text they are added) and move the hex point past them. The
characters those octets belong to are replaced whole, one undoable step; with
GROUP, part of the step made last (the second half of an octet typed in hex
digits, with its first)."
  (let* ((b (current-buffer))
         (rope (tuatara.buffer:buffer-text b))
         (size (tuatara.text:rope-bytes rope))
         (p (tuatara.display:hex-point b))
         (end (+ p (length octets)))
         (from (tuatara.text:rope-char-start rope p))
         (to (cond ((>= end size) size)
                   ((tuatara.text:char-boundary-p rope end) end)
                   (t (tuatara.text:rope-next-boundary rope (tuatara.text:rope-previous-boundary rope end)))))
         (new (concatenate '(vector (unsigned-byte 8))
                           (tuatara.text:rope-octets rope :start from :end p)
                           octets
                           (if (< end to) (tuatara.text:rope-octets rope :start end :end to) #()))))
    (let ((tuatara.buffer:*hex-view-resizing* t))
      (if group
          (tuatara.buffer:buffer-edit b (vector (tuatara.text:make-edit from to (coerce new 'tuatara.text:octets))) :group t)
          (tuatara.buffer:buffer-replace b from to (coerce new 'tuatara.text:octets))))
    (setf (tuatara.display:hex-point b) end)))

(defun hex-text-column-p (buffer)
  "True when typing in BUFFER's hex view writes characters, in the text beside
the hex digits, rather than hex digits."
  (eq (tuatara.buffer:buffer-local buffer :hex-column) :text))

(define-command hex-switch-column ()
  "In the hex view, go from the hex digits to the text beside them, or back:
typing then writes characters there, or hex digits here,
over the octets. \\`TAB' in the hex view."
  (let ((b (current-buffer)))
    (if (not (hex-view-here-p))
        (message "Not in the hex view: hex-switch-column was not run")
        (progn
          (setf (tuatara.buffer:buffer-local b :hex-column) (if (hex-text-column-p b) :hex :text))
          ;; a half octet begun is over: its high half stays written
          (setf (tuatara.display:hex-point b) (tuatara.display:hex-point b))
          (message (if (hex-text-column-p b)
                       "Typing writes characters, in the text (\\`TAB' for the hex digits)"
                       "Typing writes hex digits (\\`TAB' for the text)"))))))

(defun hex-type (string)
  "Type STRING over the octets at the hex point of the current buffer: in the
text, its characters' octets; in the hex digits, its hex digits, the first of
two the octet's high half, the second its low half, then on to the next: each
octet so typed one step to undo. Refused in a buffer that may not be written
\(CHECK-WRITABLE)."
  (let ((b (current-buffer)))
    (check-writable b)
    (if (hex-text-column-p b)
        (hex-overwrite (tuatara.text:string-octets string))
        (loop for c across string
              for digit = (digit-char-p c 16)
              do (if (null digit)
                     (return (message "~a is no hex digit: 0-9 and a-f write here (\\`TAB' for the text)" c))
                     (let* ((p (tuatara.display:hex-point b))
                            (rope (tuatara.buffer:buffer-text b))
                            (old (if (< p (tuatara.text:rope-bytes rope))
                                     (aref (tuatara.text:rope-octets rope :start p :end (1+ p)) 0)
                                     0)))
                       (if (let ((half (tuatara.buffer:buffer-local b :hex-nibble)))
                             ;; where the first half was typed, followed through what changed since
                             (and half (eql p (tuatara.text:anchor-position half (tuatara.buffer:buffer-version b)))))
                           ;; the low half: the octet done, one step to undo with its high half, on to the next
                           (hex-overwrite (coerce (list (logior (logand old #xf0) digit)) 'tuatara.text:octets) :group t)
                           ;; the high half: the cursor stays, on the low half
                           (progn (hex-overwrite (coerce (list (logior (ash digit 4) (logand old #x0f))) 'tuatara.text:octets))
                                  (setf (tuatara.display:hex-point b) p
                                        (tuatara.buffer:buffer-local b :hex-nibble)
                                        (tuatara.text:make-anchor (tuatara.buffer:buffer-version b) p))))))))))

(defun parse-octets (text)
  "The octets TEXT writes in hex, two digits each, spaces allowed between (\"de ad be ef\"), or NIL."
  (let ((digits (remove-if (lambda (c) (member c '(#\Space #\Tab))) text)))
    (when (and (plusp (length digits)) (evenp (length digits)) (every (lambda (c) (digit-char-p c 16)) digits))
      (coerce (loop for i from 0 below (length digits) by 2
                    collect (parse-integer digits :start i :end (+ i 2) :radix 16))
              'tuatara.text:octets))))

(defun octets-needle (octets)
  "OCTETS as the string that stands for exactly them: ASCII as itself, other
octets as raw bytes' characters, which encode back to the same octets."
  (map 'string (lambda (b) (code-char (if (< b #x80) b (+ #xDC00 b)))) octets))

(defun parse-offset (text)
  "The offset TEXT gives: hexadecimal after 0x, else decimal with digits grouped as the mode line shows them; NIL when it is neither."
  (let ((text (string-trim " " text)))
    (cond ((and (> (length text) 2) (string-equal "0x" text :end2 2))
           (ignore-errors (parse-integer text :start 2 :radix 16)))
          (t (parse-line-number text)))))

(defvar *hex-view-functions* '()
  "Functions of a buffer that show its file's octets themselves, for a buffer
whose text is not its file's (a PDF shown as its pages): each returns true
when it did, and \\[hex-mode] does nothing more.")

(define-command hex-mode ()
  "Show the current buffer as a hex dump, or as text again: each row the
offset, sixteen octets in hex and the text they make, a dot for an octet that
is no printable ASCII character. The ordinary motion keys move by octets and
rows; \\[goto-line] asks for an offset; typing overwrites octets: hex digits
where the cursor is on them, characters in the text beside them, \\`TAB' going
from one to the other (hex-switch-column); hex-set-byte writes
one given in hex; hex-search-forward and hex-search-backward find octets. Only
the rows shown are made, so a file of any size shows at once. In a PDF shown
as its pages, the PDF's own octets, opened safely in the window. While the
hex view is on, nothing else changes the buffer's size: the keys that would
insert or delete text say so and do nothing. A narrowed buffer is not shown
in hex, every octet of it being shown there: \\[widen] first."
  (let* ((b (current-buffer)) (on (not (tuatara.display:hex-view-p b))))
    (when (and on (tuatara.buffer:buffer-narrowed-p b))
      (message "~a is narrowed, and the hex view shows every octet: \\[widen] first" (tuatara.buffer:buffer-name b))
      (return-from hex-mode nil))
    (when (some (lambda (function) (funcall function b)) *hex-view-functions*)
      (return-from hex-mode nil))
    (if on
        (hex-view-on b (point))
        (setf (tuatara.buffer:buffer-local b :hex-view) nil))
    (message "Hex view ~:[off~;on~]" on)))

(define-command hex-set-byte ()
  "In the hex view, write one octet over the one at the cursor, asking for it
in hex (41 is A), and move on to the next."
  (if (not (hex-view-here-p))
      (message "Not in the hex view: hex-set-byte was not run")
      (let ((octets (parse-octets (read-string (format nil "Octet at 0x~(~x~) (hex): " (tuatara.display:hex-point (current-buffer)))))))
        (if (and octets (= (length octets) 1))
            (hex-overwrite octets)
            (message "Not one octet in hex, such as 41: nothing written")))))

(defun octets-stopper ()
  "A function for the search engine's STOP argument: once the search has run a
tenth of a second it says so; each call answers whether a key is waiting, so
that a search through gigabytes gives way to \\`C-g'."
  (let ((editor *editor*) (shown nil)
        (show-at (tuatara.runtime:deadline-in 1/10)))
    (lambda ()
      (when (and (not shown) (tuatara.runtime:deadline-passed-p show-at))
        (setf shown t)
        (echo "Searching... (\\`C-g' stops)")
        (redisplay-or-say editor))
      (key-waiting-p))))

(defun hex-search (backward)
  "Ask for octets in hex and move the hex view's cursor to the next place
after it that holds them, or with BACKWARD the last before it."
  (let* ((b (current-buffer))
         (octets (parse-octets (read-string (format nil "Search ~:[forward~;backward~] for octets (hex): " backward)))))
    (if (null octets)
        (message "Not octets in hex, such as de ad be ef")
        (let* ((rope (tuatara.buffer:buffer-text b)) (p (tuatara.display:hex-point b))
               (needle (octets-needle octets)) (stop (octets-stopper)))
          (multiple-value-bind (hit end)
              (if backward
                  (tuatara.syntax:search-backward rope needle :start p :case-fold nil :stop stop)
                  (tuatara.syntax:search-forward rope needle :start (min (1+ p) (tuatara.text:rope-bytes rope))
                                                             :case-fold nil :stop stop))
            (cond ((eq end :stopped) (message "Search stopped"))
                  (hit (setf (tuatara.buffer:buffer-local b :hex-view) t
                             (tuatara.display:hex-point b) hit)
                       (message "Found at 0x~(~x~) (~:d)" hit hit))
                  (t (message "Not found"))))))))

(define-command hex-search-forward ()
  "Find the next place after the cursor that holds the octets asked for in hex
(\"de ad be ef\"), and show it in the hex view."
  (hex-search nil))

(define-command hex-search-backward ()
  "Find the last place before the cursor that holds the octets asked for in
hex, and show it in the hex view."
  (hex-search t))

(defun hex-goto-offset ()
  "In the hex view, ask for an offset (0x for hex, else decimal) and put the cursor there."
  (let ((offset (parse-offset (read-string "Go to offset (0x for hex): "))))
    (if offset
        (setf (tuatara.display:hex-point (current-buffer)) offset)
        (message "Not an offset"))))

(defun hex-mode-line (editor buffer)
  "Mode line function: in the hex view, Hex and the cursor's offset."
  (declare (ignore editor))
  (when (tuatara.display:hex-view-p buffer)
    (format nil "Hex 0x~(~8,'0x~) ~:[digits~;text~]" (tuatara.display:hex-point buffer) (hex-text-column-p buffer))))

(defun install-hex ()
  "Show the hex view's offset in the mode line, and have \\[hex-mode] in a
file's pages or picture open the file's own octets."
  (pushnew 'hex-mode-line *mode-line-functions*)
  (pushnew 'file-view-hex *hex-view-functions*))
