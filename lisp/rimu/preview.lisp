;;;; lisp/rimu/preview.lisp --- Rimu's preview: the entry on the line, beside the list.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; While a rimu list is in the selected window, the entry on its current line
;;;; is shown in a window beside it once point has rested there
;;;; *RIMU-PREVIEW-DELAY* seconds: the start of a file (a binary one in the
;;;; hex view), a directory's own list, what a named pipe or a device is. A
;;;; file is read on a thread of its own without changing its access time
;;;; where the system allows; one that would be opened safely gets no major
;;;; mode. A PDF shows its text, and an image its picture where the frontend
;;;; shows pictures, taken out by a program shut in by the sandbox (decode.lisp)
;;;; on that thread, the file kept open between the two; an Office document
;;;; (Word, Excel, PowerPoint) its first page, drawn from the PDF LibreOffice
;;;; makes of it in the sandbox, and its text as an Org note read by Tuatara
;;;; itself (office-view.lisp); a kind a package reads (*PREVIEW-TEXT-FUNCTIONS*)
;;;; the text it makes of it; moving on stops it. P turns the preview on and off. o edits the file in the preview's
;;;; place, the quick edit: the file itself, opened as C-x C-f opens it, until
;;;; q (NORMAL), C-c C-c or the cursor going back to the list returns to the
;;;; preview, asking whether to save.

(in-package #:tuatara.defaults)

(defvar *rimu-preview* t
  "True for rimu lists to show the entry on the current line in a window
beside the list, once point has rested on it *RIMU-PREVIEW-DELAY* seconds: the
start of a file, a binary one in the hex view; the list of a directory; what a
named pipe, a socket or a device is, never opening it.
\\<rimu-mode>\\[list-toggle-preview] turns it on or off in one list.")

(defvar *rimu-preview-delay* 0.1
  "Seconds point rests on an entry of a rimu list before the preview shows it,
so that moving over many lines quickly reads none of them.")

(defvar *rimu-preview-size* (expt 2 20)
  "The most of a file rimu's preview shows, in octets from its start: 1 MB by
default. The preview of a larger file shows its start, its name saying so.")

(defstruct (rimu-preview (:include side-preview) (:constructor make-rimu-preview (list-window)) (:copier nil))
  "The preview of a rimu list, a SIDE-PREVIEW whose SHOWN is the entry shown
or being read: WANTED, the entry on the list's current line with the list's
directory, (ENTRY . DIRECTORY), and DUE, the internal real time it is shown
at; JOB, a file being read for it; BUFFER-ENTRY, the entry BUFFER shows.
During a quick edit, EDITING is the buffer of the file edited in the
preview's window, EDIT-ENTRY its entry, and EDIT-OWN true when the quick edit
opened it."
  wanted due job buffer-entry editing edit-entry edit-own)

(setf (documentation 'rimu-preview-p 'function) "True when OBJECT is a RIMU-PREVIEW.")

(defvar *preview-text-functions* '()
  "Functions giving the preview of a file a text of their own, for kinds of
file rimu's preview does not know (a package's): each is called, on the
preview's own thread, with the file open as FD, its PATH, its SIZE in
octets, its KIND (TUATARA.IO:OCTETS-KIND's keyword, or NIL) and its first
OCTETS, and returns the text to show, read only, or NIL to leave the file to
the next and at last to its octets. The text is the file read by Tuatara
itself, nothing in it run.")

(defstruct (rimu-preview-job (:constructor make-rimu-preview-job (entry directory)) (:copier nil))
  "The start of the file ENTRY of DIRECTORY read on THREAD for the preview:
OCTETS and the file's SIZE once done, with KIND, what the octets tell of it
(TUATARA.IO:OCTETS-KIND, as (WORDS KEYWORD)), for a PDF its TEXT when it could
be taken out and for an image its IMAGE, with NOTES for the line over it
(DECODE-PDF, DECODE-IMAGE), or the REASON it could not be read; DONE true
then; READ-HERE true when its TEXT was read by Tuatara itself
(*PREVIEW-TEXT-FUNCTIONS*). CANCEL, a cons, stops it when its car turns true."
  entry directory thread octets size kind text image notes reason done read-here (cancel (list nil)))

(setf (documentation 'rimu-preview-job-p 'function) "True when OBJECT is a RIMU-PREVIEW-JOB.")

(defvar *rimu-preview-state* nil
  "The preview beside the rimu list in the selected window, a RIMU-PREVIEW;
NIL when there is none.")

(defun rimu-previewing-p (buffer)
  "True when BUFFER is a rimu list, not one made for a preview, with its
preview on. In a list opened safely the preview shows each file as a file
opened safely is shown: its text with no major mode, a picture or a PDF only
as the sandbox draws it."
  (and (rimu-list-p buffer)
       (tuatara.buffer:buffer-local buffer :list-preview)
       (not (tuatara.buffer:buffer-local buffer 'rimu-preview-buffer))))

(defvar *rimu-quick-edit-keymap* (make-keymap :name "rimu quick edit")
  "Keys of a file being quick-edited from a rimu list (\\<rimu-mode>\\[rimu-quick-edit]),
before the modal keys and its mode's there, in every state.")

(defvar *rimu-quick-edit-normal-keymap* (make-keymap :name "rimu quick edit, NORMAL")
  "Keys of a file being quick-edited from a rimu list in NORMAL state,
before NORMAL's own.")

(defun rimu-quick-edit-end (state &key discard)
  "End the preview STATE's quick edit, the preview free to follow the list
again: the file's buffer loses the quick edit's keys, and goes with the
preview's next change when the quick edit opened it and nothing waits to be
saved, or with DISCARD its changes let go. True when it stays with changes not
saved."
  (let ((buffer (rimu-preview-editing state))
        (own (rimu-preview-edit-own state)))
    (setf (rimu-preview-editing state) nil
          (rimu-preview-edit-entry state) nil
          (rimu-preview-edit-own state) nil
          (rimu-preview-shown state) nil
          (rimu-preview-wanted state) nil)
    (when (live-buffer-p buffer)
      ;; its own keys before the quick edit's, as a PDF's view has them, given back
      (setf (tuatara.buffer:buffer-local buffer :override-keymap) (tuatara.buffer:buffer-local buffer 'rimu-own-override)
            (tuatara.buffer:buffer-local buffer 'rimu-own-override) nil
            (tuatara.buffer:buffer-local buffer :tuatara-normal-keymap) nil)
      (let* ((modified (tuatara.buffer:buffer-modified-p buffer))
             (saving (let ((job (tuatara.io:save-of buffer)))
                       (and job (eq (tuatara.io:save-job-status job) :running))))
             (goes (and own (or (not modified) discard) (not saving))))
        (setf (rimu-preview-own state) goes)
        (and modified (not goes))))))

(defun rimu-quick-edit-leave (state)
  "End the preview STATE's quick edit as \\<*rimu-quick-edit-keymap*>\\[rimu-quick-edit-done] does, asking first
whether to save the file's changes: saved, or let go when the quick edit
opened the file (a buffer open before keeps them). \\[keyboard-quit] at the
question goes on with the quick edit. True when the file was saved."
  (let* ((buffer (rimu-preview-editing state))
         (modified (and (live-buffer-p buffer) (tuatara.buffer:buffer-modified-p buffer)))
         (save (and modified (y-or-n-p* (format nil "Save ~a?" (tuatara.buffer:buffer-name buffer))))))
    (when save
      (save-buffer-with-care buffer))
    (when (rimu-quick-edit-end state :discard (and modified (not save)))
      (message "~a keeps its changes, not saved" (tuatara.buffer:buffer-name buffer)))
    save))

(defun rimu-quick-edit-finish (state)
  "End the preview STATE's quick edit, asking first whether to save its
changes (RIMU-QUICK-EDIT-LEAVE), and go on in the list: read again when the file
was saved; its preview closed when the list has none."
  (let* ((list-window (rimu-preview-list-window state))
         (saved (rimu-quick-edit-leave state))
         (list (tuatara.display:window-buffer list-window)))
    (cond ((not (rimu-previewing-p list)) (rimu-close-preview))
          (saved (rimu-update list :reread t)))))

(defun rimu-quick-edit-return (state)
  "End the preview STATE's quick edit, the cursor come back to the list, as
RIMU-QUICK-EDIT-FINISH does; \\[keyboard-quit] at the question about the changes
takes the cursor back into the quick edit."
  (let ((window (rimu-preview-window state)))
    (handler-case (rimu-quick-edit-finish state)
      (quit ()
        (when (and window (member window (editor-windows)))
          (select-window window))
        (message "The quick edit goes on")))))

(defun rimu-end-quick-edit-first ()
  "End the quick edit beside the rimu list, when one goes on, asking first
whether to save (RIMU-QUICK-EDIT-LEAVE): before a command that takes the
preview's window away."
  (let ((state *rimu-preview-state*))
    (when (and state (rimu-preview-editing state))
      (rimu-quick-edit-leave state))))

(defun rimu-close-preview ()
  "Close the preview: its window, but when it is the only one, and its buffer
when that was made for it. A file being quick-edited stays open when it has
changes not saved, and goes otherwise when the quick edit opened it."
  (let ((state (shiftf *rimu-preview-state* nil)))
    (when state
      (let ((job (rimu-preview-job state)))
        (when job (setf (car (rimu-preview-job-cancel job)) t)))
      (when (and (rimu-preview-editing state) (rimu-quick-edit-end state))
        (message "~a keeps its changes, not saved" (tuatara.buffer:buffer-name (rimu-preview-buffer state))))
      (side-preview-close state))))

(defun rimu-preview-gone-p (state list)
  "True when the entry the preview STATE shows or waits for is no longer among
those LIST shows: a search filter hid it, or the list was read again."
  (let ((entry (or (rimu-preview-buffer-entry state) (car (rimu-preview-wanted state)))))
    (and entry (not (find entry (tuatara.buffer:buffer-local list 'list-entries #()))))))

(defun rimu-preview-empty (state)
  "Have the preview STATE show no entry, saying so, a file being read for it stopped."
  (let ((job (rimu-preview-job state)))
    (when job (setf (car (rimu-preview-job-cancel job)) t)))
  (setf (rimu-preview-job state) nil
        (rimu-preview-wanted state) nil
        (rimu-preview-shown state) nil)
  (rimu-preview-show state (side-preview-note "rimu" "No entry on this line.") t nil))

(defun rimu-preview-follow (editor)
  "After each command: while the selected window shows a rimu list with its
preview on, have the preview follow the entry on the list's current line,
shown by RIMU-PREVIEW-TICK once point has rested there; close the preview when
the selected window shows neither the list nor the preview (SIDE-PREVIEW-FOLLOW).
The cursor gone into the preview of a PDF shows the whole PDF there as its
pages, a quick edit (RIMU-QUICK-EDIT-FILE), until it comes back to the list. A
quick edit in the preview's window ends when the cursor comes back to the
list, asking first whether to save its changes (RIMU-QUICK-EDIT-RETURN);
elsewhere it stays, and ends without a question when its window or the list's
is taken away."
  (side-preview-follow
   editor '*rimu-preview-state*
   :list-p #'rimu-previewing-p
   :make #'make-rimu-preview
   :close #'rimu-close-preview
   :held (lambda (state)
           (when (rimu-preview-editing state)
             (cond ((not (member (rimu-preview-window state) (editor-windows editor)))
                    (rimu-close-preview)
                    nil)
                   ((eq (editor-window editor) (rimu-preview-list-window state))
                    (rimu-quick-edit-return state)
                    (let ((state *rimu-preview-state*)) (and state (rimu-preview-editing state) t)))
                   (t t))))
   :follow (lambda (state list)
             (let ((entry (rimu-entry-at list (tuatara.buffer:buffer-point list))))
               (cond ((and entry (not (eq entry (car (rimu-preview-wanted state)))))
                      (setf (rimu-preview-wanted state) (cons entry (tuatara.buffer:buffer-local list 'rimu-directory))
                            (rimu-preview-due state) (+ (get-internal-real-time)
                                                        (round (* *rimu-preview-delay* internal-time-units-per-second)))))
                     ((and (null entry) (rimu-preview-gone-p state list))
                      (rimu-preview-empty state)))))
   :inside (lambda (state)
             ;; the cursor gone into a PDF's or an Office document's preview: the whole of it there, as its pages, until it goes back
             (let ((preview (rimu-preview-buffer state))
                   (list (tuatara.display:window-buffer (rimu-preview-list-window state))))
               (when (and (rimu-preview-own state) preview (live-buffer-p preview)
                          (tuatara.buffer:buffer-local preview 'rimu-preview-pdf)
                          (rimu-preview-buffer-entry state)
                          ;; a folder as a commit left it: the file as it is now is not opened in its place
                          (not (rimu-rewound-p list))
                          ;; its pages can be drawn: else the preview stays as it is, never the file opened
                          (eq (tuatara.io:sandbox-status-now) :ready)
                          (find-pdf-decoder))
                 (rimu-quick-edit-file state (rimu-preview-buffer-entry state)
                                       (tuatara.buffer:buffer-local list 'rimu-directory)
                                       (tuatara.io:safe-visit-p list))
                 ;; the modal keys were taken from the preview this command: from the PDF's view now
                 (funcall 'modal-sync editor))))))

(defun rimu-preview-show (state buffer own entry)
  "Show BUFFER, the preview of ENTRY, in the preview STATE's window, as
SIDE-PREVIEW-SHOW does; one made for it has ENTRY's folder as its directory,
where finding a file starts."
  (let ((directory (cdr (rimu-preview-wanted state))))
    (when (and own directory (not (rimu-list-p buffer)))
      (setf (tuatara.buffer:buffer-local buffer :directory) (tuatara.io:parent-path (rimu-path directory entry)))))
  (setf (rimu-preview-buffer-entry state) (and (side-preview-show state buffer own) entry)))

(defun rimu-display-show (buffer)
  "Show BUFFER, opened from the entry on the current line of the rimu list in
the selected window, in another window, for \\<rimu-mode>\\[rimu-display]: with the list's
preview on, in the preview's window, which shows it as that entry's until
point moves on; else in another window, the one display-buffer chooses, the
preview closed."
  (let* ((list-window (current-window))
         (list (tuatara.display:window-buffer list-window))
         (state *rimu-preview-state*))
    (if (and state (rimu-previewing-p list) (eq (rimu-preview-list-window state) list-window))
        (let ((entry (rimu-entry-at list (tuatara.buffer:buffer-point list)))
              (job (rimu-preview-job state)))
          (when job (setf (car (rimu-preview-job-cancel job)) t))
          (setf (rimu-preview-job state) nil
                (rimu-preview-wanted state) (cons entry (tuatara.buffer:buffer-local list 'rimu-directory))
                (rimu-preview-shown state) entry)
          (unless (rimu-preview-show state buffer nil entry)
            (display-buffer buffer :other t)))
        (progn (rimu-close-preview)
               (display-buffer buffer :other t)))))

(defun rimu-preview-words (entry)
  "What the preview says of ENTRY, which it does not open: a named pipe, a
socket, a device, a link that leads nowhere, an entry that could not be
examined."
  (let ((name (escape-controls (tuatara.io:listed-name entry)))
        (kind (rimu-leads-to entry)))
    (cond ((null (tuatara.io:listed-stamp entry)) (format nil "~a could not be examined." name))
          ((null kind) (format nil "~a is a symbolic link that leads nowhere: ~a" name
                               (escape-controls (or (tuatara.io:listed-target entry) ""))))
          (t (format nil "~a is ~a; rimu does not open it." name (tuatara.io:kind-words kind))))))

(defun rimu-preview-directory-list (state entry directory)
  "Show in the preview STATE's window a list of the directory ENTRY of
DIRECTORY, made for the preview: it has no preview of its own. Returns it."
  (let* ((path (directory-name (rimu-path directory entry) :clean t))
         (list (rimu-make-list path :name (format nil "~a (preview)" (rimu-buffer-name path))
                                    :like (tuatara.display:window-buffer (rimu-preview-list-window state)))))
    (setf (tuatara.buffer:buffer-local list 'rimu-preview-buffer) t)
    (rimu-preview-show state list t entry)
    (rimu-update list :reread t :fresh t)
    list))

(defun rimu-preview-start (state editor)
  "Begin showing the entry the preview STATE wants: a file is read on a thread
of its own, or shown in the buffer visiting it already; the rest at once."
  (destructuring-bind (entry . directory) (rimu-preview-wanted state)
    (let ((kind (rimu-leads-to entry))
          ;; a folder as a commit left it: its files as they were then
          (rewound (rimu-rewound-p (tuatara.display:window-buffer (rimu-preview-list-window state)))))
      (setf (rimu-preview-shown state) entry)
      (cond ((eq kind :directory)
             (rimu-preview-directory-list state entry directory))
            ((eq kind :regular)
             (let ((visiting (and (not rewound)
                                  ;; not the file's buffer a quick edit left to go with the preview
                                  (buffer-visiting (rimu-path directory entry)
                                                   :except (and (rimu-preview-own state) (rimu-preview-buffer state))))))
               (if visiting
                   (rimu-preview-show state visiting nil entry)
                   (let* ((job (make-rimu-preview-job entry directory))
                          (limit *rimu-preview-size*)
                          (frontend (editor-frontend editor))
                          ;; pictures where the frontend shows them, no larger than the preview can be
                          (pixels (and (tuatara.frontend:frontend-images-p frontend)
                                       (multiple-value-bind (width height) (tuatara.frontend:frontend-cell-pixels frontend)
                                         (and width (list (* width (max 1 (ceiling (editor-width editor) 2)))
                                                          (* height (max 1 (editor-height editor)))))))))
                     (setf (rimu-preview-job-thread job)
                           (tuatara.runtime:make-guarded-thread
                            (tuatara.runtime:carry-settings
                             *decoding-carried-settings*
                             (lambda ()
                               (unwind-protect
                                    (handler-case
                                        (rimu-call-with-entry
                                         rewound directory (tuatara.io:listed-octets entry)
                                         (lambda (fd path size)
                                           (let* ((octets (tuatara.io:read-fd-start fd path size limit))
                                                  (kind (multiple-value-list (octets-kind-of-file fd octets)))
                                                  (office (office-preview-format (second kind) (tuatara.io:listed-name entry))))
                                             (setf (rimu-preview-job-octets job) octets
                                                   (rimu-preview-job-size job) size
                                                   (rimu-preview-job-kind job) kind)
                                             (cond (office
                                                    (multiple-value-bind (text notes image)
                                                        (office-preview fd path size office :pixels pixels :cancel (rimu-preview-job-cancel job))
                                                      (setf (rimu-preview-job-text job) text
                                                            (rimu-preview-job-image job) image
                                                            (rimu-preview-job-notes job) notes)))
                                                   ((eq (second kind) :pdf)
                                                    (multiple-value-bind (text notes image)
                                                        (decode-pdf fd :cancel (rimu-preview-job-cancel job) :limit limit :pixels pixels)
                                                      (setf (rimu-preview-job-text job) text
                                                            (rimu-preview-job-image job) image
                                                            (rimu-preview-job-notes job) notes)))
                                                   ((and pixels (assoc (second kind) *image-formats*))
                                                    (multiple-value-bind (image notes)
                                                        (decode-image fd (second kind) (first pixels) (second pixels)
                                                                      :cancel (rimu-preview-job-cancel job))
                                                      (setf (rimu-preview-job-image job) image
                                                            (rimu-preview-job-notes job) notes)))
                                                   (t
                                                    (let ((text (some (lambda (function) (funcall function fd path size (second kind) octets))
                                                                      *preview-text-functions*)))
                                                      (when text
                                                        (setf (rimu-preview-job-text job) text
                                                              (rimu-preview-job-read-here job) t))))))))
                                      (serious-condition (e) (setf (rimu-preview-job-reason job) (princ-to-string e))))
                                 ;; however it ends, the preview is over and shown
                                 (setf (rimu-preview-job-done job) t)
                                 (wake-editor editor))))
                            :name "tuatara rimu preview")
                           (rimu-preview-job state) job)))))
            (t (rimu-preview-show state (side-preview-note (tuatara.io:listed-name entry) (rimu-preview-words entry)) t entry))))))

(defun rimu-preview-kind-line (buffer kind name &optional notes)
  "Give the preview BUFFER of the file NAME a header line saying what the file
is, when its first octets tell (KIND, as (WORDS KEYWORD) from
TUATARA.IO:OCTETS-KIND), over the hex dump too, followed by NOTES, each (TEXT
. FACE), such as a PDF's pages; and, in the :warning face, the extension its
name has when that is not one of the kind's."
  (destructuring-bind (&optional words keyword) kind
    (when words
      (let* ((dot (position #\. name :from-end t))
             (named (and (tuatara.io:kind-name-disagrees-p keyword name)
                         (format nil "(named ~a)" (subseq name dot))))
             (spans '())
             (text (with-output-to-string (out)
                     (format out " ~a" words)
                     (loop for (note . face) in notes
                           for start = (file-position out)
                           do (write-string note out)
                              (when face (push (list start (file-position out) face) spans)))
                     (when named
                       (format out "  ")
                       (push (list (file-position out) (+ (file-position out) (length named)) :warning) spans)
                       (write-string named out)))))
        (setf spans (nreverse spans))
        (setf (tuatara.buffer:buffer-local buffer :header-line) (lambda (window) (declare (ignore window)) (values text spans))
              (tuatara.buffer:buffer-local buffer :header-line-over-hex) t)))))

(defun rimu-preview-take-in (state job)
  "Show in the preview STATE the start of the file JOB read, unless point has
moved on to another entry meanwhile: a PDF's first page and its text, an
image's picture and its octets, the picture shown where the frontend shows
pictures and the list has not been switched to text (\\<rimu-mode>\\[rimu-toggle-picture]);
a binary file, one with a NUL octet in its first 64 KB, in the hex view; a
text file with the major mode its name calls for, unless it would be visited
safely or the list is opened safely."
  (let ((entry (rimu-preview-job-entry job)))
    (when (eq entry (car (rimu-preview-wanted state)))
      (let ((name (escape-controls (tuatara.io:listed-name entry))))
        (if (rimu-preview-job-reason job)
            (rimu-preview-show state (side-preview-note name (rimu-preview-job-reason job)) t entry)
            (let* ((text (rimu-preview-job-text job))
                   (octets (rimu-preview-job-octets job))
                   (cut (and (null text) (< (length octets) (rimu-preview-job-size job))))
                   (path (rimu-path (rimu-preview-job-directory job) entry))
                   (office (office-preview-format (second (rimu-preview-job-kind job)) (tuatara.io:listed-name entry)))
                   (buffer (tuatara.buffer:make-buffer
                            (tuatara.buffer:unique-buffer-name
                             (format nil "~a (preview~@[, first ~a~]~:[~;, text~]~:[~;, image~])" name
                                     (and cut (octets-words (length octets))) text (rimu-preview-job-image job)))
                            :text (or text octets))))
              (cond
                ;; an Office document read as text, an Org note
                ((and text office)
                 (ignore-errors (set-major-mode buffer 'org-mode)))
                (text)
                ((tuatara.io:binary-head-p octets)
                 (hex-view-on buffer))
                ((not (or (tuatara.io:safe-visit-p (tuatara.display:window-buffer (rimu-preview-list-window state)))
                          (ignore-errors (visit-safely-reason path))))
                 (let ((first-line (tuatara.text:octets-string octets :end (or (tuatara.text:octets-line-end-position octets 0 (min (length octets) 512))
                                                                               (min (length octets) 512)))))
                   (ignore-errors (set-major-mode buffer (mode-for-file path (string-right-trim '(#\Return) first-line)))))))
              (setf (tuatara.buffer:buffer-local buffer :read-only) t
                    ;; an Office document's text read by Tuatara itself, its page alone drawn in the sandbox
                    (tuatara.buffer:buffer-local buffer :sandboxed) (and (or (and text (not office) (not (rimu-preview-job-read-here job)))
                                                                             (rimu-preview-job-image job))
                                                                         t)
                    ;; the cursor into it shows its pages: a PDF's, or an Office document's that can be read or was drawn
                    (tuatara.buffer:buffer-local buffer 'rimu-preview-pdf) (and (or (eq (second (rimu-preview-job-kind job)) :pdf)
                                                                                    (and office (or (getf office :read) (rimu-preview-job-image job))))
                                                                                t)
                    ;; its text the file's own octets (an image's), for the hex view
                    (tuatara.buffer:buffer-local buffer 'rimu-preview-octets) (null text)
                    (tuatara.buffer:buffer-local buffer :image) (rimu-preview-job-image job)
                    (tuatara.buffer:buffer-local buffer :image-hidden)
                    (tuatara.buffer:buffer-local (tuatara.display:window-buffer (rimu-preview-list-window state)) 'rimu-preview-text))
              (rimu-preview-kind-line buffer (rimu-preview-job-kind job) (tuatara.io:listed-name entry) (rimu-preview-job-notes job))
              (rimu-preview-show state buffer t entry)))))))

(defun rimu-preview-tick (editor)
  "Show the entry the preview wants once point has rested on it, and take in
the file read for it: T while one is read, the seconds until the next is due,
or NIL."
  (let ((state *rimu-preview-state*))
    (cond ((null state) nil)
          ((not (member (rimu-preview-list-window state) (editor-windows editor)))
           (rimu-close-preview)
           nil)
          (t
           (let ((job (rimu-preview-job state)))
             ;; point moved on while a file was read or decoded: stop it
             (when (and job (not (rimu-preview-job-done job))
                        (not (eq (rimu-preview-job-entry job) (car (rimu-preview-wanted state)))))
               (setf (car (rimu-preview-job-cancel job)) t
                     (rimu-preview-job state) nil
                     (rimu-preview-shown state) nil
                     job nil))
             (when (and job (rimu-preview-job-done job))
               (setf (rimu-preview-job state) nil)
               (rimu-preview-take-in state job)
               (setf job nil))
             (if job
                 t
                 (let ((wanted (rimu-preview-wanted state)))
                   (cond ((or (null wanted) (eq (car wanted) (rimu-preview-shown state))) nil)
                         ((> (rimu-preview-due state) (get-internal-real-time))
                          (float (/ (- (rimu-preview-due state) (get-internal-real-time)) internal-time-units-per-second)))
                         (t (rimu-preview-start state editor)
                            (and (rimu-preview-job state) t))))))))))

(define-command rimu-toggle-picture ()
  "Switch the preview between the picture of a file and its text: the first
page of a PDF or its text, an image or its octets in the hex view. The list
keeps the choice for the files after. Pictures show in the window and in
terminals that draw them (kitty)."
  (let* ((list (current-buffer))
         (text (not (tuatara.buffer:buffer-local list 'rimu-preview-text)))
         (state *rimu-preview-state*))
    (setf (tuatara.buffer:buffer-local list 'rimu-preview-text) text)
    (when (and state (rimu-preview-own state) (rimu-preview-buffer state))
      (setf (tuatara.buffer:buffer-local (rimu-preview-buffer state) :image-hidden) text))
    (message "Preview: ~:[pictures~;text~]" text)))

(defun rimu-preview-hex (buffer)
  "For \\[hex-mode] in BUFFER, a preview showing a picture over the file's own
octets (an image's): the octets in the hex view in place of the picture, or
the picture again. True when BUFFER is one."
  (when (and (tuatara.buffer:buffer-local buffer :image) (tuatara.buffer:buffer-local buffer 'rimu-preview-octets))
    (let ((octets (not (and (tuatara.buffer:buffer-local buffer :image-hidden) (tuatara.display:hex-view-p buffer)))))
      (setf (tuatara.buffer:buffer-local buffer :image-hidden) octets)
      (when octets
        (setf (tuatara.buffer:buffer-local buffer :hex-view) t))
      (message "~:[The picture again~;Hex view on: the file's octets in place of the picture~]" octets))
    t))

(defun rimu-quick-edit-file (state entry directory safe &optional place)
  "Edit the file ENTRY of DIRECTORY in the preview STATE's window, visited as
\\[find-file] visits it, safely where that would be or with SAFE; the cursor goes
there, and with PLACE, (POINT START) where the preview of its start stood, to
the same place. A binary file (TUATARA.IO:BINARY-HEAD-P of its first 64 KB) the quick edit opens is shown in
the hex view, as its preview is; one open before stays as it is shown. With no
room beside the list, the file is shown in the list's window."
  (let* ((path (rimu-path directory entry))
         (expanded (ignore-errors (tuatara.io:expand-path path)))
         (before (or (buffer-visiting path)
                     ;; a PDF shown as its pages already
                     (find (and expanded (tuatara.io:native expanded)) *pdf-views*
                           :key (lambda (b) (let ((view (pdf-view-of b))) (and view (pdf-view-path view))))
                           :test #'equal)))
         (buffer (visit-path path :safe safe :show (lambda (b) (rimu-preview-show state b nil entry)))))
    (if (null (rimu-preview-window state))
        (progn (rimu-close-preview)
               (show-buffer buffer))
        (let ((window (rimu-preview-window state)))
          (setf (rimu-preview-editing state) buffer
                (rimu-preview-edit-entry state) entry
                (rimu-preview-edit-own state) (null before)
                (tuatara.buffer:buffer-local buffer 'rimu-own-override) (tuatara.buffer:buffer-local buffer :override-keymap)
                (tuatara.buffer:buffer-local buffer :override-keymap)
                (let ((own (tuatara.buffer:buffer-local buffer :override-keymap)))
                  (if own
                      ;; the quick edit's keys first, then the buffer's own (a PDF view's C-s)
                      (let ((keys (make-keymap :name "rimu quick edit" :parent own)))
                        (define-key keys "C-c C-c" 'rimu-quick-edit-done)
                        keys)
                      *rimu-quick-edit-keymap*))
                ;; NORMAL's q back to the preview, but not in a PDF's view, whose keys are its own
                (tuatara.buffer:buffer-local buffer :tuatara-normal-keymap)
                (and (not (pdf-view-of buffer)) *rimu-quick-edit-normal-keymap*))
          (let* ((size (tuatara.buffer:buffer-size buffer))
                 (binary (and (null before)
                              (tuatara.io:binary-head-p (tuatara.text:rope-octets (tuatara.buffer:buffer-text buffer)
                                                                       :start 0 :end (min size 65536))))))
            (when binary
              (setf (tuatara.buffer:buffer-local buffer :hex-view) t))
            (when (and place (null before))
              (setf (tuatara.display:window-point-anchor window)
                    (tuatara.text:make-anchor (tuatara.buffer:buffer-version buffer) (min size (first place)) :bias :after)
                    (tuatara.display:window-start window) (min size (second place))))
            (select-window window)
            (cond ((pdf-view-of buffer)
                   (message "~a, all of it: the cursor back in the list shows its preview again" (tuatara.buffer:buffer-name buffer)))
                  ((and binary (tuatara.buffer:buffer-local buffer :read-only))
                   (message "~a is binary: shown in the hex view, read-only; \\[read-only-mode] lets you overwrite its octets, \\<*rimu-quick-edit-normal-keymap*>\\[rimu-quick-edit-done] goes back to the preview"
                            (tuatara.buffer:buffer-name buffer)))
                  (binary
                   (message "~a is binary: shown in the hex view, where typing overwrites its octets; \\<*rimu-quick-edit-normal-keymap*>\\[rimu-quick-edit-done] goes back to the preview"
                            (tuatara.buffer:buffer-name buffer)))
                  ((not (tuatara.io:safe-visit-p buffer))
                   (message "Quick edit: \\<*rimu-quick-edit-normal-keymap*>\\[rimu-quick-edit-done] goes back to the preview"))))))))

(define-command rimu-quick-edit ()
  "Edit the file on the current line where the preview shows it, the quick
edit: the preview becomes the file itself, opened as \\[find-file] opens it
(safely where that would be, and with \\[universal-argument] safely always), and the
cursor goes there. \\<*rimu-quick-edit-normal-keymap*>\\[rimu-quick-edit-done] in NORMAL state, \\<*rimu-quick-edit-keymap*>\\[rimu-quick-edit-done] in any, or the cursor going back
to the list returns to the preview, asking first whether to save the changes.
On a directory, the cursor goes into its list in the preview's window, to move
about in. With the preview off, a window beside the list is made for it. A
named pipe, a socket or a device is not opened."
  (let* ((list (current-buffer))
         (list-window (current-window))
         (entry (rimu-entry-at-point))
         (directory (tuatara.buffer:buffer-local list 'rimu-directory))
         (state *rimu-preview-state*)
         (place nil))
    (unless (and state (eq (rimu-preview-list-window state) list-window))
      (rimu-close-preview)
      (setf state (setf *rimu-preview-state* (make-rimu-preview list-window))))
    (cond ((null entry) (message "No file on this line"))
          ((tuatara.io:safe-visit-p list)
           (message "This list is opened safely: RET opens the file safely, nothing is edited here"))
          ((and (rimu-preview-editing state) (eq entry (rimu-preview-edit-entry state)))
           (select-window (rimu-preview-window state)))
          ((rimu-unopenable-reason entry) (message "~a" (rimu-unopenable-reason entry)))
          (t
           (when (rimu-preview-editing state)
             (rimu-quick-edit-leave state))
           (let ((window (rimu-preview-window state)))
             ;; where the preview of this very file stands, for the quick edit to stand there too
             (setf place (and (eq (rimu-preview-buffer-entry state) entry) (rimu-preview-own state)
                              (not (rimu-list-p (rimu-preview-buffer state)))
                              window (member window (editor-windows))
                              (list (tuatara.display:window-point window) (tuatara.display:window-start window)))))
           (setf (rimu-preview-job state) nil
                 (rimu-preview-wanted state) (cons entry directory)
                 (rimu-preview-shown state) entry)
           (if (rimu-directory-p entry)
               (let ((shown (rimu-preview-buffer state)))
                 (unless (and shown (live-buffer-p shown) (rimu-list-p shown)
                              (member (rimu-preview-window state) (editor-windows))
                              (equal (tuatara.buffer:buffer-local shown 'rimu-directory)
                                     (directory-name (rimu-path directory entry) :clean t)))
                   (rimu-preview-directory-list state entry directory))
                 (when (rimu-preview-window state)
                   (select-window (rimu-preview-window state))))
               (rimu-quick-edit-file state entry directory (and (prefix-argument) t) place))))))

(define-command rimu-quick-edit-done ()
  "Go back from the quick edit of this file to the preview beside the rimu
list (as the cursor going back to the list does), asking first whether to save
the changes made: \\`y' saves them; \\`n' lets them go (a buffer open before the
quick edit keeps them); \\[keyboard-quit] goes on with the quick edit."
  (let ((state *rimu-preview-state*))
    (when (rimu-quick-edit-buffer-p (current-buffer))
      (let ((list-window (rimu-preview-list-window state)))
        (rimu-quick-edit-finish state)
        (select-window list-window)))))

(defun rimu-quick-edit-buffer-p (buffer)
  "True when BUFFER is a file being quick-edited from a rimu list (\\<rimu-mode>\\[rimu-quick-edit])."
  (let ((state *rimu-preview-state*))
    (and state buffer (eq buffer (rimu-preview-editing state)))))

(commands-for-buffers 'rimu-quick-edit-buffer-p "a quick edit from a rimu list (\\<rimu-mode>\\[rimu-quick-edit])"
                      'rimu-quick-edit-done)

(defun rimu-quick-edit-mode-line (editor buffer)
  "What the mode line of a file being quick-edited from a rimu list adds: Quick edit."
  (declare (ignore editor))
  (let ((state *rimu-preview-state*))
    ;; a PDF's view says its pages already
    (and state (eq buffer (rimu-preview-editing state)) (not (pdf-view-of buffer)) "Quick edit")))
