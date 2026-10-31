;;;; lisp/viewers/pdf-view.lisp --- A PDF shown page by page, drawn in the sandbox.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Visiting a PDF shows its pages, not its octets: a buffer of its own, named
;;;; as the file, whose picture is the page shown and whose text is the PDF's
;;;; text, pages marked. Each page is drawn when it is turned to, on a thread
;;;; of its own, by the program decode.lisp knows, shut in by the sandbox, as
;;;; wide as the zoom asks (fitted to the window, to its width, or larger); the
;;;; next page is drawn ahead, and the pages drawn last are kept. The page is
;;;; shown at its own size from a row and column of it, so that it scrolls. The
;;;; text is taken out once, in the background; what the PDF says of itself and
;;;; the files embedded in it too, on asking. The buffer visits no file, so
;;;; nothing saves over the PDF; C-u C-x C-f opens its octets. Where pictures
;;;; are not shown, the view is the text. The page and zoom of each PDF are
;;;; kept in the state directory, for the next time it is opened.
;;;; As Emacs's doc-view and pdf-tools do, the pages are pictures of the file.

(in-package #:tuatara.defaults)

(define-major-mode pdf-view-mode special-mode
  "A PDF shown as its pages (\\[find-file] of a PDF), each drawn from the file
by a program shut in by the sandbox.
\\<pdf-view-mode>\\[pdf-view-next-page] and \\[pdf-view-previous-page] turn to the next and the
previous page (\\`<next>' and \\`<prior>' too), \\[pdf-view-first-page] and \\[pdf-view-last-page] to the
first and the last, \\[pdf-view-goto-page] to a page by its number; \\[pdf-view-page-top] shows the top of the
page and \\[pdf-view-page-end] its bottom; \\[pdf-view-scroll-down] and \\[pdf-view-scroll-up]
scroll down and up, on to the next or the previous page at its end, \\[pdf-view-scroll-right]
and \\[pdf-view-scroll-left] across. \\[pdf-view-zoom-in] and \\[pdf-view-zoom-out] zoom in and out, \\[pdf-view-zoom-reset] back, \\[pdf-view-fit-page] fits the
page in the window, \\[pdf-view-fit-width] its width. \\[pdf-view-search] finds text and turns to its page;
\\[pdf-view-toggle-text] switches to the PDF's text, to read, search and copy, where the
keys move as in any text, and back; \\[pdf-view-toggle-dark] shows the pages dark.
\\[pdf-view-facts] says what the PDF says of itself, \\[pdf-view-attachments] lists the files embedded
in it. \\`q' closes. The mode line says the page and the zoom. The buffer
visits no file: nothing is saved over the PDF, and \\[universal-argument] \\[find-file] opens
its octets. The page and zoom are kept for the next time the PDF is opened."
  :lighter "PDF" :interactive nil)

(defvar *pdf-view-keymap* (mode-keymap 'pdf-view-mode)
  "Keys of a PDF shown as its pages (pdf-view-mode's keymap).")

(defvar *pdf-view-override-keymap* (make-keymap :name "PDF view, before the modal keys")
  "Keys of a PDF's view that come before the modal keys: \\`C-s', which searches
its pages.")

(defvar *pdf-view-dark-pages* nil
  "True to show a PDF's pages dark: white as the theme's background, black as
its text, the colours between in proportion. \\<pdf-view-mode>\\[pdf-view-toggle-dark] turns it on and
off while the editor runs.")

(defparameter *pdf-view-text-limit* (* 64 1024 1024)
  "The most octets of a PDF's text its view takes out.")

(defparameter *pdf-view-memory* (* 256 1024 1024)
  "How much memory the pages a PDF's view keeps drawn may take, their dark
pictures counted too: past it, those farthest from the page shown are let go
of, so that turning back and on is at once.")

(defparameter *pdf-view-ahead* '(1 2 3 -1)
  "The pages a PDF's view draws besides the one shown, before you turn to
them, as steps from it: the next three and the one before.")

(defparameter *pdf-view-drawing-at-once* 2
  "How many pages a PDF's view draws at once, each on a processor of its own.")

(defparameter *pdf-view-zoom-step* 1.25
  "How much larger each zoom in makes the page, and smaller each zoom out.")

(defparameter *pdf-view-scroll-rows* 3
  "The rows of cells a PDF's page scrolls by, down or up.")

(defparameter *pdf-view-scroll-columns* 8
  "The columns of cells a PDF's page scrolls by, across.")

(defparameter *pdf-view-places-kept* 200
  "How many PDFs have their page and zoom kept for the next time.")

(defstruct (pdf-view (:constructor make-pdf-view (path)) (:copier nil))
  "What a PDF's view (pdf-view-mode) knows: the PDF's PATH, the PAGE shown
(from 1), how many PAGES it has and the INFO its :INFO program printed, once
the text is taken out; ZOOM, how the page is fitted, :PAGE or :WIDTH, and
FACTOR, how much larger than that it is drawn; TOP and LEFT, the row and
column of cells of the page shown at the window's top left (TOP :BOTTOM for
the page's last rows); the pages drawn, IMAGES, each (PAGE WIDTH IMAGE DARK),
the last drawn first, WIDTH in pixels, DARK its dark picture once made, as
(FACE-GENERATION . IMAGE), for the theme's colours then; JOB,
the pages being drawn (a list), TEXT-JOB, the text being taken out; LAST-SEARCH, the last text searched for and where it was found;
SAVED, whether the page and zoom are kept as they are; PAGE-STARTS, where
in its text (in octets) each page after the first begins; and COPY, the
PDF's octets in memory that its programs read (a PDF-COPY), made once;
MAKER, a function of the open file at PATH giving that copy's descriptor
\(TUATARA.IO:SEALED-COPY when NIL, a document made a PDF otherwise), MAKING,
held while it is made; READING, a function of the view's buffer showing the
file read as text instead, or NIL for the PDF's own text; WORDS, what the
line over it calls the file shown (a PDF document when NIL)."
  path (page 1) pages (info "") (zoom :page) (factor 1) (top 0) (left 0)
  (images '()) job text-job last-search (saved t)
  (page-starts (make-array 0)) copy (lock (sb-thread:make-mutex :name "pdf view copy"))
  maker (making (sb-thread:make-mutex :name "pdf view making")) reading words)

(setf (documentation 'pdf-view-p 'function) "True when OBJECT is a PDF-VIEW.")

(defstruct (pdf-view-job (:constructor make-pdf-view-job (page width)) (:copier nil))
  "Work for a PDF's view on THREAD: page PAGE drawn WIDTH pixels wide (its
IMAGE, its dark picture the RESULT), or the text taken out (PAGE NIL: TEXT,
NOTES and INFO, where its pages begin the RESULT); DONE once over. CANCEL, a
cons, stops it when its car turns true."
  page width thread image text notes info result done (cancel (list nil)))

(setf (documentation 'pdf-view-job-p 'function) "True when OBJECT is a PDF-VIEW-JOB.")

(defvar *pdf-views* '()
  "The buffers showing a PDF as its pages.")

(defun pdf-view-of (buffer)
  "The PDF-VIEW of BUFFER, or NIL when it shows no PDF."
  (and (live-buffer-p buffer) (tuatara.buffer:buffer-local buffer 'pdf-view)))

;;; Work in the background

(defstruct (pdf-copy (:constructor make-pdf-copy (fd stamp)) (:copier nil))
  "A PDF's octets in memory, sealed (TUATARA.IO:SEALED-COPY), for its view's
programs to read: FD, the copy; STAMP, the file's when it was made; USERS,
how many are reading it; DROPPED once the view needs it no more, closed when
the last user is done."
  fd stamp (users 0) dropped)

(setf (documentation 'pdf-copy-p 'function) "True when OBJECT is a PDF-COPY.")

(defun pdf-copy-release (view copy)
  "One user of COPY, VIEW's, done with it: closed when it was dropped and no one reads it."
  (sb-thread:with-mutex ((pdf-view-lock view))
    (decf (pdf-copy-users copy))
    (when (and (pdf-copy-dropped copy) (zerop (pdf-copy-users copy)))
      (ignore-errors (sb-posix:close (pdf-copy-fd copy))))))

(defun pdf-copy-drop (view)
  "VIEW needs its copy no more: closed now, or by its last user."
  (sb-thread:with-mutex ((pdf-view-lock view))
    (let ((copy (pdf-view-copy view)))
      (when (pdf-copy-p copy)
        (setf (pdf-copy-dropped copy) t)
        (when (zerop (pdf-copy-users copy))
          (ignore-errors (sb-posix:close (pdf-copy-fd copy)))))
      ;; work started after (an embedded file saved) makes a copy of its own, closed when done
      (setf (pdf-view-copy view) :dropped))))

(defun call-with-pdf-copy (view function)
  "Call FUNCTION with the descriptor of VIEW's PDF copied in memory, sealed:
the copy made the first time, and again when the file changed since. The
file is copied once for all the pages drawn, not for each."
  (let ((copy (tuatara.io:call-with-file
               (pdf-view-path view)
               (lambda (fd path size)
                 (declare (ignore size))
                 (let ((stamp (tuatara.io:fd-stamp fd path))
                       (make (or (pdf-view-maker view) #'tuatara.io:sealed-copy)))
                   ;; a copy slow to make (a document made a PDF) made once, the editor's thread never waiting on it
                   (sb-thread:with-mutex ((pdf-view-making view))
                     (let* ((copy (sb-thread:with-mutex ((pdf-view-lock view)) (pdf-view-copy view)))
                            (made (and (not (and (pdf-copy-p copy) (tuatara.io:same-content-p stamp (pdf-copy-stamp copy))))
                                       (funcall make fd))))
                       (sb-thread:with-mutex ((pdf-view-lock view))
                         (let ((copy (pdf-view-copy view)))
                           (cond ((eq copy :dropped)
                                  (setf copy (make-pdf-copy (or made (funcall make fd)) stamp))
                                  (setf (pdf-copy-dropped copy) t))
                                 (made
                                  ;; the file changed (or no copy yet): the old one closed once its users are done
                                  (when copy
                                    (setf (pdf-copy-dropped copy) t)
                                    (when (zerop (pdf-copy-users copy)) (ignore-errors (sb-posix:close (pdf-copy-fd copy)))))
                                  (setf copy (make-pdf-copy made stamp)
                                        (pdf-view-copy view) copy)))
                           (incf (pdf-copy-users copy))
                           copy)))))))))
    (unwind-protect (funcall function (pdf-copy-fd copy))
      (pdf-copy-release view copy))))

(defun pdf-view-start (view job work)
  "Run WORK, a function of the open PDF of VIEW (its copy in memory,
CALL-WITH-PDF-COPY), for JOB on a thread of its own; what it returns goes to
JOB's RESULT, and JOB is DONE after; the editor is woken then. Returns JOB.
The pages and the text are not jobs of the job runner: many pages are drawn
and given up as you turn them, the view's tick taking in each, and the mode
line says the page, P left unlit."
  (let ((editor *editor*))
    (setf (pdf-view-job-thread job)
          (tuatara.runtime:make-guarded-thread
           (tuatara.runtime:carry-settings
            *decoding-carried-settings*
            (lambda ()
              (unwind-protect
                   (setf (pdf-view-job-result job)
                         (tuatara.runtime:ignoring-errors-reported ("drawing ~a" (pdf-view-path view))
                           (call-with-pdf-copy view work)))
                ;; however it ends, the job is over, the editor told
                (setf (pdf-view-job-done job) t)
                (when editor (wake-editor editor)))))
           :name "tuatara pdf view"))
    job))

(defstruct (pdf-view-task (:include tuatara.runtime:job) (:constructor make-pdf-view-task (buffer words finish quiet)) (:copier nil))
  "Work for a PDF's view besides its pages and text, a job: WORDS, what it
does (reading what report.pdf says of itself); FINISH, a function of its
RESULT, takes that in on the editor's thread, the view killed meanwhile or
not; QUIET, true for work that only shows something, stopped without asking
when the view is killed."
  words finish quiet result)

(setf (documentation 'pdf-view-task-p 'function) "True when OBJECT is a PDF-VIEW-TASK.")

(tuatara.runtime:define-job-words pdf-view-task
  :doing (pdf-view-task-words job)
  :description (pdf-view-task-words job)
  :verb (pdf-view-task-words job)
  :left-words "nothing done")

(defmethod tuatara.runtime:job-at-exit ((job pdf-view-task))
  "Stop work that only shows something without asking; ask before stopping any other."
  (if (pdf-view-task-quiet job) :quiet :stop))

(defmethod tuatara.runtime:job-take-in ((job pdf-view-task))
  "Hand the task's result to its FINISH; say how it ended when it did not end done."
  (if (eq (tuatara.runtime:job-status job) :done)
      (funcall (pdf-view-task-finish job) (pdf-view-task-result job))
      (message "~a: ~a" (pdf-view-task-words job) (tuatara.runtime:job-ending-words job))))

(defun pdf-view-task (view buffer words work finish &key quiet)
  "Run WORK, a function of the open PDF of VIEW, BUFFER's, in the background,
a job doing WORDS (QUIET as a PDF-VIEW-TASK has it); FINISH, a function of
its result, takes it in on the editor's thread, VIEW killed in between or not."
  (tuatara.runtime:start-job-thread
   (make-pdf-view-task buffer words finish quiet)
   (tuatara.runtime:carry-settings
    *decoding-carried-settings*
    (lambda (job)
      (setf (pdf-view-task-result job) (call-with-pdf-copy view work))))
   :name "tuatara pdf view task"))

(defun pdf-view-start-text (view)
  "Start taking VIEW's text out, with its pages' count and size."
  (let ((job (make-pdf-view-job nil nil)))
    (setf (pdf-view-text-job view) job)
    (pdf-view-start view job
                    (lambda (fd)
                      (let ((*pdf-text-pages* 1000000))
                        (multiple-value-bind (text notes image info)
                            (decode-pdf fd :cancel (pdf-view-job-cancel job) :limit *pdf-view-text-limit*
                                           ;; the pages' size, for the first page to be drawn before the text is out
                                           :on-info (lambda (info) (setf (pdf-view-job-info job) info)))
                          (declare (ignore image))
                          (setf (pdf-view-job-text job) text
                                (pdf-view-job-notes job) notes
                                (pdf-view-job-info job) info
                                ;; where its pages begin, found here rather than on the editor's thread
                                (pdf-view-job-result job) (and text (text-page-starts text)))))))))

(defun pdf-view-start-page (view page width)
  "Start drawing PAGE of VIEW WIDTH pixels wide."
  (let ((job (make-pdf-view-job page width))
        (colours (and *pdf-view-dark-pages* (dark-colours))))
    (push job (pdf-view-job view))
    (pdf-view-start view job
                    (lambda (fd)
                      (let ((image (decode-pdf-page fd page width -1 :cancel (pdf-view-job-cancel job))))
                        (setf (pdf-view-job-image job) image)
                        ;; its dark picture made here too, for the theme as it was: no wait on the editor's thread
                        (and image colours (cons (first colours) (dark-image image colours))))))))

;;; Sizes

(defun picture-cell-pixels ()
  "The width and height in pixels of a cell of the frontend, as two values,
when it shows pictures (a PDF's pages, a terminal's pictures); else NIL."
  (let ((frontend (editor-frontend *editor*)))
    (and (tuatara.frontend:frontend-images-p frontend)
         (tuatara.frontend:frontend-cell-pixels frontend))))

(defun pdf-view-ratio (view)
  "The width of VIEW's pages over their height, as its first page's size
says; that of A4 upright until it is known."
  (let ((size (page-size (pdf-view-info view))))
    (if size (/ (first size) (second size)) (/ 210 297))))

(defun pdf-view-width (buffer)
  "The width in pixels the page of BUFFER, a PDF's view, is drawn for its
window and its zoom, a page fitted whole never taller than the window; NIL
when no window shows it or pictures are not shown. No wider than the
frontend can place, 297 cells either way."
  (let ((window (buffer-window buffer))
        (view (pdf-view-of buffer)))
    (multiple-value-bind (cell-width cell-height) (picture-cell-pixels)
      (when (and window cell-width)
        (let* ((box-width (* cell-width (max 1 (tuatara.display:window-width window))))
               (box-height (* cell-height (max 1 (tuatara.display:window-height window))))
               (ratio (pdf-view-ratio view))
               (fitted (if (eq (pdf-view-zoom view) :width) box-width (min box-width (* box-height ratio))))
               (most (min (* 297 cell-width) (* 297 cell-height ratio) 8000)))
          (max 16 (floor (min most (* fitted (pdf-view-factor view))))))))))

(defun pdf-view-image-cells (image)
  "The columns and rows of cells IMAGE takes at its own size, as two values."
  (multiple-value-bind (cell-width cell-height) (picture-cell-pixels)
    (if cell-width
        (values (ceiling (tuatara.frontend:image-width image) cell-width)
                (ceiling (tuatara.frontend:image-height image) cell-height))
        (values 0 0))))

;;; Dark pages

(defun dark-colours ()
  "What dark pages are made with now, as the theme's text and ground colours
give it: (FACE-GENERATION INK-RED INK-GREEN INK-BLUE PAPER-RED PAPER-GREEN
PAPER-BLUE), taken on the editor's thread for a drawing thread to use; NIL
when the theme gives no colours for them (the terminal's own, or none), the
pages then shown as they are."
  (multiple-value-bind (fg bg) (tuatara.display:resolve-face-spec :default)
    (let ((ink (tuatara.display:colour-hex fg)) (paper (tuatara.display:colour-hex bg)))
      (when (and ink paper)
        (multiple-value-bind (fr fg* fb) (tuatara.display:parse-hex-colour ink)
          (multiple-value-bind (br bg* bb) (tuatara.display:parse-hex-colour paper)
            (list tuatara.display:*face-generation* fr fg* fb br bg* bb)))))))

(defun dark-image (image &optional (colours (dark-colours)))
  "IMAGE with its colours turned about: white as the theme's background, black
as its text, the colours between in proportion; COLOURS as DARK-COLOURS gives
them. Each octet looked up in a table made once a channel, so that a large
page takes little time."
  (destructuring-bind (generation fr fg fb br bg bb) colours
    (declare (ignore generation))
    (let* ((channels (tuatara.frontend:image-channels image))
           (from (tuatara.frontend:image-pixels image))
           (to (make-array (length from) :element-type '(unsigned-byte 8)))
           (tables (map 'vector (lambda (ink paper)
                                  (let ((table (make-array 256 :element-type '(unsigned-byte 8))))
                                    (dotimes (v 256 table)
                                      (setf (aref table v) (+ ink (round (* (- paper ink) v) 255))))))
                        (list fr fg fb) (list br bg bb)))
           (red (svref tables 0)) (green (svref tables 1)) (blue (svref tables 2)))
      (declare (type (simple-array (unsigned-byte 8) (*)) from to red green blue)
               (type (integer 1 4) channels)
               (optimize speed))
      (loop for i of-type fixnum from 0 below (- (length from) (1- channels)) by channels
            do (setf (aref to i) (aref red (aref from i))
                     (aref to (+ i 1)) (aref green (aref from (+ i 1)))
                     (aref to (+ i 2)) (aref blue (aref from (+ i 2))))
               (when (= channels 4) (setf (aref to (+ i 3)) (aref from (+ i 3)))))
      (tuatara.frontend:make-image (tuatara.frontend:image-width image) (tuatara.frontend:image-height image)
                                   channels to))))

(defun pdf-view-shown-image (entry)
  "The picture of ENTRY, one of a view's IMAGES, as it is shown: dark while
*PDF-VIEW-DARK-PAGES* and the theme gives colours for it, the dark one made
with it when it was drawn, or made again here when the theme's colours
changed since."
  (if (and *pdf-view-dark-pages* (dark-colours))
      (let ((dark (fourth entry)) (generation tuatara.display:*face-generation*))
        (if (and dark (eql (car dark) generation))
            (cdr dark)
            (progn
              (tuatara.runtime:debug-report :redisplay "pdf view: page ~d made dark for theme ~d" (first entry) generation)
              (cdr (setf (fourth entry) (cons generation (dark-image (third entry))))))))
      (third entry)))

;;; Showing the page

(defun pdf-view-drawn (view page width)
  "The entry of VIEW's IMAGES of PAGE drawn WIDTH pixels wide, or failing that
drawn any width, or NIL; one that could not be drawn left out."
  (let ((drawn (remove-if-not (lambda (entry) (and (eql (first entry) page) (third entry))) (pdf-view-images view))))
    (or (find width drawn :key #'second) (first drawn))))

(defun pdf-view-show (buffer)
  "Give BUFFER, a PDF's view, the picture of its page and where it is shown
from, the rows and columns scrolled kept within the page."
  (let* ((view (pdf-view-of buffer))
         (entry (pdf-view-drawn view (pdf-view-page view) (pdf-view-width buffer)))
         (window (buffer-window buffer)))
    (if (null entry)
        ;; the page on its way: the one before stays until it comes, so that nothing flashes
        ;; between them; one that could not be drawn, or none before: nothing (never the text)
        (when (find-if (lambda (e) (and (eql (first e) (pdf-view-page view)) (null (third e)))) (pdf-view-images view))
          (setf (tuatara.buffer:buffer-local buffer :image) nil))
        (let ((image (pdf-view-shown-image entry)))
          (unless (eq image (tuatara.buffer:buffer-local buffer :image))
            (tuatara.runtime:debug-report :redisplay "pdf view: page ~d shown, picture ~d, ~d pixels wide~:[~; (drawn for ~d)~], ~:[as it is~;dark~], theme ~d"
                                          (pdf-view-page view) (tuatara.frontend:image-id image) (tuatara.frontend:image-width image)
                                          (not (eql (second entry) (pdf-view-width buffer))) (second entry)
                                          *pdf-view-dark-pages* tuatara.display:*face-generation*))
          (multiple-value-bind (columns rows) (pdf-view-image-cells image)
            (let ((last-top (max 0 (- rows (if window (tuatara.display:window-height window) rows))))
                  (last-left (max 0 (- columns (if window (tuatara.display:window-width window) columns)))))
              (setf (pdf-view-top view) (if (eq (pdf-view-top view) :bottom) last-top (min (max 0 (pdf-view-top view)) last-top))
                    (pdf-view-left view) (min (max 0 (pdf-view-left view)) last-left)
                    (tuatara.buffer:buffer-local buffer :image) image
                    (tuatara.buffer:buffer-local buffer :image-offset) (list (pdf-view-top view) (pdf-view-left view)))))))))

(defun pdf-view-take-in-text (buffer view job)
  "Give BUFFER, VIEW's, the text JOB took out, and VIEW its pages; say what
there is when there is no text."
  (let ((text (pdf-view-job-text job))
        (info (or (pdf-view-job-info job) "")))
    (setf (pdf-view-info view) info
          (pdf-view-pages view) (ignore-errors (parse-integer (info-field info "Pages")))
          (pdf-view-page-starts view) (or (pdf-view-job-result job) (make-array 0)))
    (set-made-text buffer (or text (format nil "~a has no text~{~a~}.~%" (tuatara.io:native-file-name (pdf-view-path view))
                                            (mapcar #'car (pdf-view-job-notes job)))))
    (setf (tuatara.buffer:buffer-point buffer) 0
          (pdf-view-page view) (max 1 (min (pdf-view-page view) (or (pdf-view-pages view) (pdf-view-page view))))
          (tuatara.buffer:buffer-local buffer :header-line)
          (let* ((kind (format nil " ~a" (or (pdf-view-words view) "PDF document")))
                 (words (format nil "~a~{~a~}" kind (mapcar #'car (pdf-view-job-notes job))))
                 (spans (loop with at = (length kind)
                             for (note . face) in (pdf-view-job-notes job)
                             when face collect (list at (+ at (length note)) face)
                             do (incf at (length note)))))
            (lambda (window) (declare (ignore window)) (values words spans)))
          (tuatara.buffer:buffer-local buffer :header-line-over-hex) t)
    ;; a document whose pages could not be made: read as text instead
    (when (and (null (pdf-view-pages view)) (pdf-view-reading view))
      (message "~a could not be drawn as pages (the debug log says why): read as text" (tuatara.io:native-file-name (pdf-view-path view)))
      (funcall (pdf-view-reading view) buffer))))

(defun pdf-view-tick (editor)
  "Tick function: take in the pages drawn and the text taken out for the PDFs
shown, show each its page, and draw the page shown,
then those around it (PDF-VIEW-WANTED-PAGES), as wide as its window and zoom
ask, some at once; let go of those past *PDF-VIEW-MEMORY*; keep the page and
zoom of those changed a while ago. T while a page or a text is being made,
and once more after the text is taken in: its header line takes a row of
the window, so the page is fitted again to the rows left."
  (declare (ignore editor))
  (let ((working nil))
    (dolist (buffer (copy-list *pdf-views*))
      (let ((view (pdf-view-of buffer)))
        (if (null view)
            (setf *pdf-views* (remove buffer *pdf-views*))
            (let ((text (pdf-view-text-job view)))
              (when (and text (pdf-view-job-done text))
                (setf (pdf-view-text-job view) nil
                      working t)
                (pdf-view-take-in-text buffer view text))
              ;; what pdfinfo said, while the text is still being taken out: enough to draw the pages
              (when (and text (not (pdf-view-job-done text)) (pdf-view-job-info text) (string= "" (pdf-view-info view)))
                (setf (pdf-view-info view) (pdf-view-job-info text)
                      (pdf-view-pages view) (ignore-errors (parse-integer (info-field (pdf-view-info view) "Pages")))))
              ;; the pages drawn, taken in
              (dolist (job (reverse (pdf-view-job view)))
                (when (pdf-view-job-done job)
                  (setf (pdf-view-job view) (remove job (pdf-view-job view)))
                  (pdf-view-take-in-page view job)))
              (let ((width (pdf-view-width buffer))
                    (wanted (pdf-view-wanted-pages view)))
                ;; pages no longer wanted, or drawn for a width no longer asked, are not worth finishing
                (dolist (job (pdf-view-job view))
                  (unless (and (member (pdf-view-job-page job) wanted) (or (null width) (eql width (pdf-view-job-width job))))
                    (setf (car (pdf-view-job-cancel job)) t
                          (pdf-view-job view) (remove job (pdf-view-job view)))))
                ;; the page shown first, then those ahead, as wide as the window and zoom ask now, some at once
                (when (and width (or (null (pdf-view-text-job view)) (plusp (length (pdf-view-info view)))))
                  (dolist (page wanted)
                    (when (and (< (length (pdf-view-job view)) *pdf-view-drawing-at-once*)
                               (not (find-if (lambda (d) (and (eql (first d) page) (eql (second d) width))) (pdf-view-images view)))
                               (not (find page (pdf-view-job view) :key #'pdf-view-job-page)))
                      (pdf-view-start-page view page width))))
                (pdf-view-let-go view width)
                (pdf-view-show buffer))
              (unless (pdf-view-saved view)
                (pdf-view-keep-place view))
              (when (or (pdf-view-job view) (pdf-view-text-job view))
                (setf working t))))))
    working))

(defun pdf-view-take-in-page (view job)
  "Take in the page JOB drew for VIEW: kept among VIEW's IMAGES, in place of
one drawn as wide before; one that could not be drawn kept too, its picture
NIL, so that it is not tried again and again, and said when it is the page shown."
  (tuatara.runtime:debug-report :job "pdf view: page ~d drawn ~d pixels wide~:[, nothing drawn~;, picture ~:*~d~]"
                                (pdf-view-job-page job) (pdf-view-job-width job)
                                (and (pdf-view-job-image job) (tuatara.frontend:image-id (pdf-view-job-image job))))
  (unless (car (pdf-view-job-cancel job))
    (let ((page (pdf-view-job-page job)) (width (pdf-view-job-width job)))
      (setf (pdf-view-images view)
            (cons (list page width (pdf-view-job-image job) (and (pdf-view-job-image job) (pdf-view-job-result job)))
                  (remove-if (lambda (d) (and (eql (first d) page) (eql (second d) width))) (pdf-view-images view))))
      (when (and (null (pdf-view-job-image job)) (eql page (pdf-view-page view)))
        (message "Page ~d could not be drawn (T shows its text)" page)))))

(defun pdf-view-wanted-pages (view)
  "The pages VIEW draws, the most wanted first: the one shown, then those
*PDF-VIEW-AHEAD* names, within its pages."
  (let ((page (pdf-view-page view)) (last (pdf-view-pages view)))
    (remove-duplicates
     (loop for step in (cons 0 *pdf-view-ahead*)
           for p = (+ page step)
           when (and (>= p 1) (or (null last) (<= p last))) collect p)
     :from-end t)))

(defun entry-octets (entry)
  "The memory ENTRY, one of a view's IMAGES, takes: its picture and its dark one."
  (+ (let ((image (third entry))) (if image (length (tuatara.frontend:image-pixels image)) 0))
     (let ((dark (cdr (fourth entry)))) (if dark (length (tuatara.frontend:image-pixels dark)) 0))))

(defun pdf-view-let-go (view width)
  "Let go of the pages VIEW keeps drawn past *PDF-VIEW-MEMORY*: those drawn
for another width first, then those farthest from the page shown; never the
page shown as it is drawn now, nor more than 64 kept."
  (let* ((page (pdf-view-page view))
         (ranked (sort (copy-list (pdf-view-images view)) #'<
                       :key (lambda (d) (+ (abs (- (first d) page)) (if (eql (second d) width) 0 100000)))))
         (total 0) (kept '()))
    (loop for entry in ranked
          for count from 0
          do (incf total (entry-octets entry))
             (when (or (and (eql (first entry) page) (eql (second entry) width))
                       (and (<= total *pdf-view-memory*) (< count 64)))
               (push entry kept)))
    (when (< (length kept) (length (pdf-view-images view)))
      (setf (pdf-view-images view) (remove-if-not (lambda (d) (member d kept :test #'eq)) (pdf-view-images view))))))

(defun pdf-view-forget (buffer)
  "Stop what is being made for BUFFER's PDF, when it is killed, and keep its
page and zoom for the next time."
  (let ((view (pdf-view-of buffer)))
    (when view
      (dolist (job (cons (pdf-view-text-job view) (pdf-view-job view)))
        (when job (setf (car (pdf-view-job-cancel job)) t)))
      (unless (pdf-view-saved view) (pdf-view-keep-place view))
      (pdf-copy-drop view)
      (setf *pdf-views* (remove buffer *pdf-views*)))))

;;; The page and zoom of each PDF, for the next time

(defun parse-pdf-place-line (line)
  "The entry of the PDFs' places LINE writes, (PATH PAGE ZOOM FACTOR): the
page, page or width, the zoom factor and the PDF's path (ESCAPE-PATH); NIL
for a line that makes no sense."
  (let* ((first (position #\Space line))
         (second (and first (position #\Space line :start (1+ first))))
         (third (and second (position #\Space line :start (1+ second)))))
    (when (and third (< (1+ third) (length line)))
      (list (unescape-path (subseq line (1+ third)))
            (max 1 (or (ignore-errors (parse-integer line :end first)) 1))
            (if (string= (subseq line (1+ first) second) "width") :width :page)
            (let ((n (parse-decimal (subseq line (1+ second) third))))
              (if (and n (< 0.05 n 20)) (float n) 1))))))

(defun pdf-place-line (entry)
  "The line of the PDFs' places for ENTRY, (PATH PAGE ZOOM FACTOR)."
  (destructuring-bind (path page zoom factor) entry
    (format nil "~d ~(~a~) ~,3f ~a" page zoom (float factor) (escape-path path))))

(define-state-list *pdf-places* "pdf-places" 'parse-pdf-place-line 'pdf-place-line '*pdf-view-places-kept*
  :path-of #'first)

(defun read-pdf-places ()
  "The PDFs' places as kept: a list of (PATH PAGE ZOOM FACTOR), the latest
first (STATE-LIST-ENTRIES)."
  (state-list-entries *pdf-places*))

(defun pdf-view-keep-place (view)
  "Keep VIEW's page and zoom for the next time its PDF is opened: written in
the background, with what sessions side by side put there meanwhile
\(STATE-LIST-PUT)."
  (setf (pdf-view-saved view) t)
  (state-list-put *pdf-places* (list (list (pdf-view-path view) (pdf-view-page view) (pdf-view-zoom view) (pdf-view-factor view)))))

(defun pdf-view-changed (view)
  "Note that VIEW's page or zoom changed: kept once the view is left or next looked at."
  (setf (pdf-view-saved view) nil))

;;; Opening

(defun pdf-file-p (path)
  "True when the file at PATH is a PDF by its first octets (TUATARA.IO:FILE-HEAD),
whatever its name."
  (let ((head (tuatara.io:file-head path)))
    (and head (eq :pdf (nth-value 1 (tuatara.io:octets-kind head))))))

(defun pdf-view-visit (path show)
  "Show the PDF at PATH as its pages, with SHOW, a function of the buffer, when
it is a PDF by its octets and a program shut in by the sandbox can draw it;
the view already made for it, when there is one; at the page and zoom it was
left at. Returns the buffer, or NIL for a file visited as usual (not a PDF,
or nothing to draw it with, which is said)."
  (let ((native (file-view-path path)))
    (when (and native (pdf-file-p native)
               (file-view-can-draw-p native (find-pdf-decoder)
                                     (format nil "no program for PDFs (install ~a: ~a)"
                                             (getf (first *pdf-decoders*) :name) (first (getf (first *pdf-decoders*) :text)))))
      (file-view-show native *pdf-views* (lambda (b) (let ((v (pdf-view-of b))) (and v (pdf-view-path v))))
                      (lambda () (pdf-view-make native)) show))))

(defun pdf-view-make (native &key make reading words (zoom :page))
  "A new view of the PDF at NATIVE, at the page and zoom it was left at, else
at its first page fitted as ZOOM says (:page, the whole page in the window,
or :width, its width), its text being taken out; MAKE, its MAKER, and
READING and WORDS as a PDF-VIEW has them, for a document shown as the PDF
made of it."
  (let ((buffer (make-file-view-buffer native 'pdf-view-mode (format nil "Reading ~a...~%" (tuatara.io:native-file-name native)) "its pages"))
        (view (let ((view (make-pdf-view native)))
                (setf (pdf-view-maker view) make (pdf-view-reading view) reading (pdf-view-words view) words (pdf-view-zoom view) zoom)
                view))
        (place (find native (read-pdf-places) :key #'first :test #'equal)))
    (when place
      (destructuring-bind (page zoom factor) (rest place)
        (setf (pdf-view-page view) page (pdf-view-zoom view) zoom (pdf-view-factor view) factor)))
    (setf (tuatara.buffer:buffer-local buffer :override-keymap) *pdf-view-override-keymap*
          (tuatara.buffer:buffer-local buffer :refill) (lambda (buffer)
                                                         (pdf-view-reload buffer)
                                                         (message "Read ~a again" (tuatara.buffer:buffer-name buffer)))
          (tuatara.buffer:buffer-local buffer 'pdf-view) view)
    (push buffer *pdf-views*)
    (pdf-view-start-text view)
    buffer))

(defun pdf-view-reload (buffer)
  "Read the PDF BUFFER shows again, as it is on disk now: its text and pages
taken out and drawn anew, at the same page, zoom and place on the page (the
last page, when it has fewer now). The page shown stays on screen until its
new picture comes."
  (let ((view (pdf-view-of buffer)))
    (when view
      (dolist (job (cons (pdf-view-text-job view) (pdf-view-job view)))
        (when job (setf (car (pdf-view-job-cancel job)) t)))
      (setf (pdf-view-job view) '()
            (pdf-view-images view) '()
            (pdf-view-info view) "")
      (pdf-view-start-text view))))

;;; Pages and text

(defun pdf-view-text-p (buffer)
  "True when BUFFER, a PDF's view, shows its text: turned to it, or pictures not shown."
  (or (tuatara.buffer:buffer-local buffer :image-hidden)
      (not (tuatara.frontend:frontend-images-p (editor-frontend *editor*)))))

(defparameter *pdf-page-mark* "── Page "
  "How the text of a PDF marks where a page begins.")

(defun pdf-view-page-at (view end)
  "The page of VIEW's PDF whose text holds the line ending at octet END: one
more than the pages beginning before END (its PAGE-STARTS, by halves)."
  (let* ((starts (pdf-view-page-starts view))
         (low 0) (high (length starts)))
    (setf low (tuatara.runtime:bisect (middle low high) (< (svref starts middle) end)) high low)
    (1+ low)))

(defun text-page-starts (text)
  "Where in TEXT, a PDF's text with a line saying where each page after the
first begins, each such page begins, in octets, as a vector."
  (let ((starts (make-array 0 :adjustable t :fill-pointer 0)))
    (loop with octets = 0 and from = 0
          for mark = (search *pdf-page-mark* text :start2 from)
          while mark
          do (let ((line (string-line-start text mark)))
               (incf octets (tuatara.text:string-octet-length text :start from :end line))
               (vector-push-extend octets starts)
               (setf from line)
               ;; past this mark for the next
               (let ((after (+ mark (length *pdf-page-mark*))))
                 (incf octets (tuatara.text:string-octet-length text :start from :end after))
                 (setf from after))))
    (coerce starts 'simple-vector)))

(defun pdf-view-text-page (buffer)
  "The page point is on in BUFFER, a PDF's view showing its text (PDF-VIEW-PAGE-AT)."
  (pdf-view-page-at (pdf-view-of buffer) (tuatara.buffer:buffer-line-end buffer (tuatara.buffer:buffer-point buffer))))

(defun pdf-view-page-start (view page)
  "Where in VIEW's text PAGE begins, in octets; NIL for a page past those it has."
  (let ((starts (pdf-view-page-starts view)))
    (cond ((<= page 1) 0)
          ((<= (- page 2) (1- (length starts))) (svref starts (- page 2))))))

(defun pdf-view-go (page &key (top 0))
  "Show PAGE of the PDF in the current buffer, from its row TOP (:BOTTOM for
its last rows): in its pages, or in its text the line where the page begins."
  (let* ((buffer (current-buffer))
         (view (pdf-view-of buffer))
         (page (max 1 (min page (or (pdf-view-pages view) most-positive-fixnum)))))
    (if (pdf-view-text-p buffer)
        (let ((at (pdf-view-page-start view page)))
          (if at
              (goto at)
              (message "No page ~d in the text" page)))
        (progn
          (setf (pdf-view-page view) page (pdf-view-top view) top)
          (pdf-view-changed view)
          (pdf-view-show buffer)))
    page))

(defun pdf-view-page-now (buffer)
  "The page of BUFFER, a PDF's view, as shown: in its text, where point is."
  (if (pdf-view-text-p buffer)
      (pdf-view-text-page buffer)
      (pdf-view-page (pdf-view-of buffer))))

(defun pdf-view-usual (keys)
  "Do what KEYS do outside a PDF's view, as in the text of any buffer: the modal
support keys, then the global ones."
  (let* ((specs (tuatara.frontend:parse-key-sequence keys))
         ;; the modal keymaps are made after this file is loaded
         (command (or (lookup-key (symbol-value '*tuatara-support-keymap*) specs)
                      (lookup-key (symbol-value '*global-keymap*) specs))))
    (when (and command (symbolp command) (not (eq command :undefined)) (fboundp command))
      (funcall command))))

(defmacro define-pdf-view-command (name keys doc &body body)
  "Define NAME, a command of a PDF's view documented by DOC, whose BODY runs on
the pages, and which in the text does what KEYS do in any text (PDF-VIEW-USUAL)."
  `(define-command ,name ()
     ,doc
     (let* ((buffer (current-buffer)) (view (pdf-view-of buffer)))
       (declare (ignorable view))
       (cond ((null view))
             ((and ,keys (pdf-view-text-p buffer)) (pdf-view-usual ,keys))
             (t ,@body)))))

(define-pdf-view-command pdf-view-next-page "l"
  "Turn to the next page of the PDF. In its text, move as \\`l' does in any text."
  (let ((page (pdf-view-page-now buffer)))
    (if (and (pdf-view-pages view) (>= page (pdf-view-pages view)))
        (message "The last page")
        (pdf-view-go (1+ page)))))

(define-pdf-view-command pdf-view-previous-page "h"
  "Turn to the page before in the PDF. In its text, move as \\`h' does in any text."
  (let ((page (pdf-view-page-now buffer)))
    (if (<= page 1)
        (message "The first page")
        (pdf-view-go (1- page)))))

(define-pdf-view-command pdf-view-next-page-anywhere nil
  "Turn to the next page of the PDF; in its text, go to where it begins."
  (let ((page (pdf-view-page-now buffer)))
    (if (and (pdf-view-pages view) (>= page (pdf-view-pages view)))
        (message "The last page")
        (pdf-view-go (1+ page)))))

(define-pdf-view-command pdf-view-previous-page-anywhere nil
  "Turn to the page before in the PDF; in its text, go to where it begins."
  (let ((page (pdf-view-page-now buffer)))
    (if (<= page 1)
        (message "The first page")
        (pdf-view-go (1- page)))))

(define-pdf-view-command pdf-view-first-page "g h"
  "Turn to the first page of the PDF. In its text, move as \\`g h' does in any text."
  (pdf-view-go 1))

(define-pdf-view-command pdf-view-last-page "g l"
  "Turn to the last page of the PDF. In its text, move as \\`g l' does in any text."
  (if (pdf-view-pages view)
      (pdf-view-go (pdf-view-pages view))
      (message "The pages are still being counted")))

(define-pdf-view-command pdf-view-page-top "g g"
  "Show the top of the page. In its text, move as \\`g g' does in any text."
  (setf (pdf-view-top view) 0)
  (pdf-view-show buffer))

(define-pdf-view-command pdf-view-page-end "g e"
  "Show the bottom of the page. In its text, move as \\`g e' does in any text."
  (setf (pdf-view-top view) :bottom)
  (pdf-view-show buffer))

(define-pdf-view-command pdf-view-goto-page nil
  "Turn to a page of the PDF by its number, asked for; in its text, go to where it begins."
  (let* ((last (pdf-view-pages view))
         (typed (read-string (format nil "Go to page~@[ (1-~d)~]: " last)))
         (page (ignore-errors (parse-integer typed))))
    (if page
        (pdf-view-go page)
        (message "No page number: ~a" typed))))

(defun pdf-view-last-top (buffer view)
  "The row of the page shown in BUFFER, VIEW's, from which its last rows fill
the window: 0 for a page fitted whole, or while it is not drawn yet."
  (let ((window (buffer-window buffer))
        (image (tuatara.buffer:buffer-local buffer :image)))
    (if (or (null image) (null window) (and (not (eq (pdf-view-zoom view) :width)) (= 1 (pdf-view-factor view))))
        0
        (max 0 (- (nth-value 1 (pdf-view-image-cells image)) (tuatara.display:window-height window))))))

(define-pdf-view-command pdf-view-scroll-down "j"
  "Scroll the page down, and at its end on to the top of the next. In its
text, move as \\`j' does in any text."
  (let ((last-top (pdf-view-last-top buffer view)))
    (when (eq (pdf-view-top view) :bottom) (setf (pdf-view-top view) last-top))
    (cond ((< (pdf-view-top view) last-top)
           (setf (pdf-view-top view) (min last-top (+ (pdf-view-top view) *pdf-view-scroll-rows*)))
           (pdf-view-show buffer))
          ((and (pdf-view-pages view) (>= (pdf-view-page view) (pdf-view-pages view)))
           (message "The end of the last page"))
          (t (pdf-view-go (1+ (pdf-view-page view)))))))

(define-pdf-view-command pdf-view-scroll-up "k"
  "Scroll the page up, and at its top on to the end of the page before. In
its text, move as \\`k' does in any text."
  (when (eq (pdf-view-top view) :bottom)
    (setf (pdf-view-top view) (pdf-view-last-top buffer view)))
  (cond ((plusp (pdf-view-top view))
         (setf (pdf-view-top view) (max 0 (- (pdf-view-top view) *pdf-view-scroll-rows*)))
         (pdf-view-show buffer))
        ((<= (pdf-view-page view) 1)
         (message "The top of the first page"))
        (t (pdf-view-go (1- (pdf-view-page view)) :top :bottom))))

(define-command pdf-view-scroll-by-wheel ()
  "Scroll the page as the mouse wheel turned, as \\[pdf-view-scroll-down] and
\\[pdf-view-scroll-up] do: down for \\`<wheel-down>', on to the next page at
a page's end (each turn a page, the page fitted whole), up for
\\`<wheel-up>'. In its text, scroll as the wheel does in any text."
  (let* ((buffer (current-buffer))
         (key (editor-this-key *editor*))
         (up (and key (eq (tuatara.frontend:key-base key) :wheel-up))))
    (cond ((or (null (pdf-view-of buffer)) (pdf-view-text-p buffer)) (scroll-by-wheel))
          (up (pdf-view-scroll-up))
          (t (pdf-view-scroll-down)))))

(define-pdf-view-command pdf-view-scroll-right "L"
  "Scroll a page wider than the window to the right. In its text, move as \\`L' does in any text."
  (incf (pdf-view-left view) *pdf-view-scroll-columns*)
  (pdf-view-show buffer))

(define-pdf-view-command pdf-view-scroll-left "H"
  "Scroll a page wider than the window to the left. In its text, move as \\`H' does in any text."
  (setf (pdf-view-left view) (max 0 (- (pdf-view-left view) *pdf-view-scroll-columns*)))
  (pdf-view-show buffer))

(defun pdf-view-zoom-words (view)
  "VIEW's zoom as the mode line says it: fit page, fit width, or how large in per cent."
  (if (= 1 (pdf-view-factor view))
      (if (eq (pdf-view-zoom view) :width) "fit width" "fit page")
      (format nil "~d%" (round (* 100 (pdf-view-factor view))))))

(defun pdf-view-set-zoom (buffer view zoom factor)
  "Fit VIEW's pages, in BUFFER, as ZOOM says, :PAGE or :WIDTH, FACTOR times
larger; the page drawn anew for it."
  (setf (pdf-view-zoom view) zoom
        (pdf-view-factor view) (max 0.1 (min 10 factor))
        (pdf-view-left view) 0)
  (pdf-view-changed view)
  (pdf-view-show buffer)
  (message "Zoom: ~a" (pdf-view-zoom-words view)))

(define-pdf-view-command pdf-view-zoom-in nil
  "Draw the PDF's pages larger, by *PDF-VIEW-ZOOM-STEP*; scroll with \\<pdf-view-mode>\\[pdf-view-scroll-down], \\[pdf-view-scroll-up], \\[pdf-view-scroll-left] and \\[pdf-view-scroll-right]."
  (pdf-view-set-zoom buffer view (pdf-view-zoom view) (* (pdf-view-factor view) *pdf-view-zoom-step*)))

(define-pdf-view-command pdf-view-zoom-out nil
  "Draw the PDF's pages smaller, by *PDF-VIEW-ZOOM-STEP*."
  (pdf-view-set-zoom buffer view (pdf-view-zoom view) (/ (pdf-view-factor view) *pdf-view-zoom-step*)))

(define-pdf-view-command pdf-view-zoom-reset nil
  "Draw the PDF's pages at the size of the fit chosen again, page or width
(\\<pdf-view-mode>\\[pdf-view-fit-page], \\[pdf-view-fit-width]), no longer zoomed in or out."
  (pdf-view-set-zoom buffer view (pdf-view-zoom view) 1))

(define-pdf-view-command pdf-view-fit-page nil
  "Draw the PDF's pages as large as the window shows them whole."
  (pdf-view-set-zoom buffer view :page 1))

(define-pdf-view-command pdf-view-fit-width nil
  "Draw the PDF's pages as wide as the window, to scroll down."
  (pdf-view-set-zoom buffer view :width 1))

(define-pdf-view-command pdf-view-toggle-dark nil
  "Show the PDF's pages dark, white as the theme's background and black as its
text, or as they are again; for every PDF while the editor runs
(*PDF-VIEW-DARK-PAGES*)."
  (setf *pdf-view-dark-pages* (not *pdf-view-dark-pages*))
  (pdf-view-show buffer)
  (message "Pages ~:[as they are~;dark~]" *pdf-view-dark-pages*))

(define-command pdf-view-toggle-text ()
  "Switch the PDF's view between its pages and its text, to read, search and
copy; the text opens at the page shown, and the pages at the page point is on.
A Word document, an Excel workbook or a PowerPoint presentation shown as its
pages is switched to it read as text, as an Org note, and back with the same
key."
  (let* ((buffer (current-buffer)) (view (pdf-view-of buffer)))
    (cond ((null view))
          ((pdf-view-reading view) (funcall (pdf-view-reading view) buffer))
          ((not (tuatara.frontend:frontend-images-p (editor-frontend *editor*)))
           (message "Pages are not shown here: the text it is"))
          (t (let ((page (pdf-view-page-now buffer))
                   (text (not (tuatara.buffer:buffer-local buffer :image-hidden))))
               (setf (tuatara.buffer:buffer-local buffer :image-hidden) text)
               (pdf-view-go page)
               (message "~:[Pages~;Text~], page ~d" text page))))))

(define-pdf-view-command pdf-view-search "C-s"
  "Find text in the PDF and turn to its page: from the page shown on, asked
for; again with nothing typed, the next place of the same text. Case is
ignored while the text has no capital letter, in every language (é finds É).
In its text, search as \\`C-s' does in any text."
  (let* ((last (pdf-view-last-search view))
         (typed (read-string (format nil "Search the PDF~@[ (~a)~]: " (car last))))
         (query (if (and (string= typed "") last) (car last) typed))
         (rope (tuatara.buffer:buffer-text buffer)))
    (if (string= query "")
        (message "Nothing to search for")
        (multiple-value-bind (found end)
            (matcher-search rope (make-pattern-matcher query :regex nil)
                            (min (tuatara.text:rope-bytes rope)
                                 (if (and last (string= query (car last)) (string= typed ""))
                                     (1+ (cdr last))
                                     (or (pdf-view-page-start view (pdf-view-page view)) 0)))
                            (tuatara.text:rope-bytes rope)
                            :stop (octets-stopper))
          (cond ((eq end :stopped) (message "Search stopped"))
                (found
                 (let ((page (pdf-view-page-at view (tuatara.buffer:buffer-line-end buffer found))))
                   (setf (pdf-view-last-search view) (cons query found))
                   (pdf-view-go page)
                   (message "Page ~d: ~a" page
                            (string-trim " " (tuatara.text:rope-string rope :start (tuatara.buffer:buffer-line-start buffer found)
                                                                       :end (tuatara.buffer:buffer-line-end buffer found))))))
                (t
                 (setf (pdf-view-last-search view) (cons query -1))
                 (message "No more of ~s; \\[pdf-view-search] RET again searches from the start" query)))))))

;;; What the PDF says of itself, and the files embedded in it

(define-pdf-view-command pdf-view-facts nil
  "Show what the PDF says of itself, as help: title, subject, author, the
programs that made it (creator and producer), when it was made and changed,
its version, encryption and what it allows, JavaScript, forms, pages and
their size, as its program reads them in the sandbox; the file's own name and
size first. The PDF's words may be false: they are what it says."
  (let ((path (pdf-view-path view)))
    (pdf-view-task view buffer (format nil "Reading what ~a says of itself" (tuatara.io:native-file-name path))
                   (lambda (fd)
                     (let ((spec (getf (find-pdf-decoder) :info)))
                       (and spec
                            (multiple-value-bind (output outcome said)
                                (tuatara.io:sandbox-run (first spec) (cons "-isodates" (rest spec)) :input fd :output-limit 65536)
                              (if (eq outcome :done)
                                  ;; its File size is of what it read through a pipe: 0, not the file's
                                  (format nil "~{~a~%~}"
                                          (remove-if (lambda (line) (uiop:string-prefix-p "File size:" line))
                                                     (uiop:split-string (tuatara.text:lenient-string output)
                                                                        :separator '(#\Newline))))
                                  (format nil "~a could not read it: ~a" (first spec) said))))))
                   (lambda (facts)
                     (show-help (format nil "What ~a says of itself, read in the sandbox~%~%File:           ~a~%File size:      ~:d octets~%~%~a"
                                        (tuatara.io:native-file-name path)
                                        path (ignore-errors (tuatara.io:stamp-size (tuatara.io:path-stamp path)))
                                        (or facts "Nothing could be read."))))
                   :quiet t)
    (message "Reading what ~a says of itself..." (tuatara.io:native-file-name path))))

(define-major-mode pdf-attachments-mode special-mode
  "The files embedded in a PDF (\\<pdf-view-mode>\\[pdf-view-attachments] in its view), one a line:
\\<pdf-attachments-mode>\\[pdf-attachment-save] or \\`RET' on one saves it where you say, taken out of
the PDF in the sandbox and never opened; \\[list-next-entry] and \\[list-previous-entry] go from one to the next.
\\[search-filter-regex] narrows the list to the files whose lines match what is typed, as in
every list, and the keys work on the file on the line. Read-only; \\`q' closes."
  :lighter "Embedded" :interactive nil)

(defmethod list-entry-noun ((mode pdf-attachments-mode))
  "The list of a PDF's embedded files lists embedded files."
  "embedded file")

(defvar *pdf-attachments-keymap* (mode-keymap 'pdf-attachments-mode)
  "Keys of the list of a PDF's embedded files (pdf-attachments-mode's keymap).")

(defun embedded-names (listing)
  "The files a PDF holds, from LISTING, what its program printed of them (a
line N: NAME each, N from 1 in turn), as a list of (NUMBER NAME). A name with
a line of its own in it goes on with that line, never taken for another file."
  (let ((names '()))
    (dolist (line (uiop:split-string listing :separator '(#\Newline)) (nreverse names))
      (let* ((colon (position #\: line))
             (number (and colon (ignore-errors (parse-integer line :end colon)))))
        (cond ((and number (= number (1+ (length names))))
               (push (list number (string-trim " " (subseq line (1+ colon)))) names))
              ((and names (plusp (length line)))
               (setf (second (first names)) (format nil "~a ~a" (second (first names)) line))))))))

(define-pdf-view-command pdf-view-attachments nil
  "List the files embedded in the PDF, in a buffer beside it, as its program
reads them in the sandbox; there, \\<pdf-attachments-mode>\\[pdf-attachment-save] saves one."
  (let ((path (pdf-view-path view)))
    (pdf-view-task view buffer (format nil "Reading the files embedded in ~a" (tuatara.io:native-file-name path))
                   (lambda (fd)
                     (multiple-value-bind (output outcome said)
                         (tuatara.io:sandbox-run "pdfdetach" '("-list" "fd://0") :input fd :output-limit 1048576)
                       (if (eq outcome :done)
                           (embedded-names (tuatara.text:lenient-string output))
                           (list :failed said))))
                   (lambda (names)
                     (cond ((eq (first names) :failed)
                            (message "The embedded files could not be read: ~a" (second names)))
                           ((null names)
                            (message "~a has no embedded files" (tuatara.io:native-file-name path)))
                           (t
                            (let ((list (set-search-filtering (buffer-in-mode "*PDF embedded files*" 'pdf-attachments-mode)
                                                              :nouns "embedded files")))
                              ;; another PDF's files, or these read again: shown whole
                              (drop-search-filters list)
                              (render-pieces list (mapcar (lambda (entry)
                                                            (format nil "~3d  ~a~%" (first entry) (escape-controls (second entry))))
                                                          names)
                                             :entries names :point :start)
                              (setf (tuatara.buffer:buffer-local list :sandboxed) t
                                    (tuatara.buffer:buffer-local list :directory)
                                    (tuatara.io:parent-path path)
                                    (tuatara.buffer:buffer-local list 'pdf-attachments-view) view)
                              (display-buffer list :other t)
                              (message "~d embedded file~:p: \\<pdf-attachments-mode>\\[pdf-attachment-save] saves the one on the line"
                                       (length names))))))
                   :quiet t)
    (message "Reading the files embedded in ~a..." (tuatara.io:native-file-name path))))

(define-command pdf-attachment-save ()
  "Save the embedded file on the current line where you say, a new file, or
one there after asking: taken out of the PDF in the sandbox, as its program
reads it, and written as it is, never opened. Up to 64 MB. In a list narrowed
by search filters, the file on the line among those they leave."
  (let* ((list (current-buffer))
         (view (tuatara.buffer:buffer-local list 'pdf-attachments-view)))
    (when view
      (let ((entry (list-entry-at list)))
        (if (null entry)
            (message "No embedded file on this line")
            (destructuring-bind (number name) entry
              (let* ((suggested (attachment-file-name name (format nil "attachment-~d" number)))
                     (shown (escape-controls name))
                     ;; nothing written beside evidence: from a folder kept safe, your Downloads offered instead
                     (folder (if (or (safe-directory-of (pdf-view-path view)) (evidence-path-p (pdf-view-path view)))
                                 (let ((downloads (tuatara.io:native (tuatara.io:expand-path "~/Downloads/"))))
                                   (if (tuatara.io:path-stamp downloads) downloads (tuatara.io:native (tuatara.io:expand-path "~/"))))
                                 (buffer-directory list)))
                     (typed (read-native-path (format nil "Save ~a to: " suggested) :kind :any
                                              :initial (concatenate 'string (abbreviated-path folder) suggested) :writing t))
                     ;; a folder chosen: the file goes in it under the name suggested
                     (target (and typed
                                  (if (eq (tuatara.io:path-kind typed) :directory)
                                      (concatenate 'string (tuatara.io:slash-ended typed) suggested)
                                      typed))))
                (when (and target
                           (or (not (tuatara.io:path-stamp target))
                               (yes-or-no-p* (format nil "~a exists; replace it?" target))))
                  (pdf-view-task view list (format nil "Taking ~a out" shown)
                                 (lambda (fd)
                                   (multiple-value-bind (output outcome said)
                                       (tuatara.io:sandbox-run
                                        "sh" (list "-c" "pdfdetach -save \"$1\" -o /tmp/embedded fd://0 && cat /tmp/embedded"
                                                   "sh" (princ-to-string number))
                                        :input fd :output-limit (* 64 1024 1024))
                                     (if (eq outcome :done)
                                         (handler-case
                                             (progn (tuatara.io:replace-file target output)
                                                    (format nil "Saved ~a (~:d octets), not opened" target (length output)))
                                           (error (e) (format nil "~a could not be saved: ~a" shown e)))
                                         (format nil "~a could not be taken out: ~a" shown
                                                 (tuatara.io:sandbox-outcome-words outcome said :label "pdfdetach")))))
                                 (lambda (said) (message "~a" (or said (format nil "~a could not be taken out" shown)))))
                  (message "Taking ~a out..." shown)))))))))

;;; The mode line and the keys

(defun pdf-view-mode-line (editor buffer)
  "What the mode line of a PDF's view adds: the page, how many there are, and the zoom."
  (declare (ignore editor))
  (let ((view (pdf-view-of buffer)))
    (and view (format nil "Page ~d of ~:[?~;~:*~d~]~:[, ~a~;~*~]"
                      (pdf-view-page-now buffer) (pdf-view-pages view)
                      (pdf-view-text-p buffer) (pdf-view-zoom-words view)))))

(defun install-pdf-view ()
  "Bind the keys of a PDF's view, and have PDFs visited shown as their pages."
  (define-keys *pdf-view-keymap* '(("l" pdf-view-next-page) ("h" pdf-view-previous-page)
                                   ("<next>" pdf-view-next-page-anywhere) ("<prior>" pdf-view-previous-page-anywhere)
                                   ("g h" pdf-view-first-page) ("g l" pdf-view-last-page)
                                   ("g g" pdf-view-page-top) ("g e" pdf-view-page-end)
                                   ("g p" pdf-view-goto-page)
                                   ("j" pdf-view-scroll-down) ("k" pdf-view-scroll-up)
                                   ("<wheel-down>" pdf-view-scroll-by-wheel) ("<wheel-up>" pdf-view-scroll-by-wheel)
                                   ("H" pdf-view-scroll-left) ("L" pdf-view-scroll-right)
                                   ("=" pdf-view-zoom-in) ("+" pdf-view-zoom-in) ("-" pdf-view-zoom-out) ("0" pdf-view-zoom-reset)
                                   ("p" pdf-view-fit-page) ("w" pdf-view-fit-width)
                                   ("T" pdf-view-toggle-text) ("d" pdf-view-toggle-dark)
                                   ("i" pdf-view-facts) ("a" pdf-view-attachments)))
  (define-key *pdf-view-override-keymap* "C-s" 'pdf-view-search)
  (define-keys *pdf-attachments-keymap* '(("RET" pdf-attachment-save) ("s" pdf-attachment-save)
                                          ("n" list-next-entry) ("p" list-previous-entry)))
  (commands-for-mode 'pdf-attachments-mode "*PDF embedded files*" 'list-next-entry 'list-previous-entry)
  (pushnew 'pdf-view-visit *file-views*)
  (pushnew 'pdf-view-mode-line *mode-line-functions*)
  (pushnew 'sandboxed-mode-line *mode-line-functions*)
  (pushnew 'pdf-view-forget tuatara.buffer:*kill-buffer-functions*)
  (pushnew 'pdf-view-tick *tick-functions*))
