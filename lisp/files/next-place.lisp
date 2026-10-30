;;;; lisp/files/next-place.lisp --- Going from place to place of the list in force: M-g n and M-g p.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A compile's output, the lines a search of files found and what the
;;;; language servers found wrong in a buffer are each a list of places. The
;;;; one used last is the list in force, and M-g n and M-g p go to the next of
;;;; its places or the one before, whichever kind of list it is: each kind says
;;;; how it steps (STEP-PLACE). A place in a file opens as a line chosen from a
;;;; search of files does (SHOW-FILE-LINE).

(in-package #:tuatara.defaults)

(defvar *places-in-force* nil
  "The list of places \\[next-place] and \\[previous-place] go through: the one
used last, a compile's output (\\[compile]), the lines a search of files found
\(\\[search-in-files]), or what the language servers found wrong in a buffer;
NIL before any has been used.")

(defgeneric step-place (places direction)
  (:documentation "Go to the place of PLACES, a list of places, after the one gone
to last (DIRECTION 1) or before it (-1), and say what is there; say so when
there is none that way."))

(defgeneric places-buffer (places)
  (:documentation "The buffer PLACES, a list of places, lives in, whose killing ends
it as the list in force (a compile's output); NIL for one of no buffer.")
  (:method (places) (declare (ignore places)) nil))

(defun forget-places-of (buffer)
  "Kill-buffer function: when the list of places in force lives in BUFFER,
there is none in force any more, its text not kept for it."
  (when (and *places-in-force* (eq buffer (places-buffer *places-in-force*)))
    (setf *places-in-force* nil)))

(defun use-places (places)
  "Make PLACES the list of places in force, the one \\[next-place] and
\\[previous-place] go through. Returns PLACES."
  (setf *places-in-force* places))

(defun show-file-line (path line &key (offset 0) column other)
  "Visit the file at PATH as \\[find-file] does and put point on its line LINE
\(from 1; its last line for one past it), OFFSET octets in, or with COLUMN
that many characters in from 1, never past the line's end, the line in the
middle of the window: in the selected window, or with OTHER in another; a
buffer narrowed to a part not holding it widened first, and that said. A
file shown otherwise than as text (a PDF's pages) is shown as it is. A file no
longer there is said, not made. Returns the buffer, or NIL then."
  (unless (tuatara.io:path-kind (tuatara.io:native (tuatara.io:expand-path path)))
    (message "~a is not there" (abbreviated-path path))
    (return-from show-file-line nil))
  (let ((buffer (visit-path path :show (if other
                                           (lambda (buffer) (pop-to-buffer buffer :other t))
                                           (lambda (buffer) (show-buffer buffer))))))
    (when (and (tuatara.buffer:buffer-file buffer)
               (eq buffer (tuatara.display:window-buffer (current-window))))
      (let* ((start (tuatara.buffer:buffer-goto-line buffer (max 0 (1- line))))
             (rope (tuatara.buffer:buffer-text buffer))
             (end (tuatara.text:rope-line-end rope start)))
        (goto-shown buffer (if column
                               (min end (cluster-forward buffer start (max 0 (1- column))))
                               (tuatara.text:rope-char-start rope (min end (+ start offset)))))
        (tuatara.display:window-recenter (current-window))))
    buffer))

(define-command next-place ()
  "Go to the next place of the list of places used last: the next line naming
a file and line in a compile's output (\\[compile]), the next line a search of
files found (\\[search-in-files]), or the next place the language servers found
something wrong in a buffer. A place in a file opens it at that line, in
another window from the compile's output, else in this one."
  (if *places-in-force*
      (step-place *places-in-force* 1)
      (message "No list of places yet: \\[compile], \\[search-in-files] or a language server's diagnostics make one")))

(define-command previous-place ()
  "Go to the place before the one gone to last in the list of places used
last: a compile's output (\\[compile]), the lines a search of files found
\(\\[search-in-files]), or the places the language servers found something
wrong in a buffer, as \\[next-place] goes to the next."
  (if *places-in-force*
      (step-place *places-in-force* -1)
      (message "No list of places yet: \\[compile], \\[search-in-files] or a language server's diagnostics make one")))

(defun install-next-place ()
  "Bind \\`M-g n' to \\[next-place] and \\`M-g p' to \\[previous-place], and let a
list of places go with the buffer it lives in."
  (define-keys *global-keymap* '(("M-g n" next-place) ("M-g p" previous-place)))
  (pushnew 'forget-places-of tuatara.buffer:*kill-buffer-functions*))
