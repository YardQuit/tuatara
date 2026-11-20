;;;; lisp/viewers/decode.lisp --- Files decoded by programs of the system, shut in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; What a file holds that is not text, the text of a PDF or the pixels of an
;;;; image, is taken out by a program of the system run in the sandbox
;;;; (TUATARA.IO:SANDBOX-RUN): the editor only ever gets text or plain pixels
;;;; back, which it checks before use. For each kind of file a short list of
;;;; the programs known to do it, each with the arguments it needs; the first
;;;; found where the sandbox sees programs is used, and M-x check-health says
;;;; which. What cannot be decoded, and why, is said in a few words.
;;;; ImageMagick runs under a policy of its own besides: it reads only the
;;;; image formats it is told, writes only the pixels, starts no other program
;;;; and refuses images larger than any screen could want.

(in-package #:tuatara.defaults)

(defun static-header-line (text &optional spans)
  "A header line that never changes, as TUATARA.DISPLAY:BUFFER-HEADER-LINE
takes one: TEXT, with SPANS, each (START END FACE), over it."
  (lambda (window) (declare (ignore window)) (values text spans)))

(defparameter *decoding-carried-settings*
  (append tuatara.io:*sandbox-carried-settings*
          '(*pdf-decoders* *pdf-text-pages* *image-decoders* *magick-largest-side* *picture-memory*
            *image-view-largest*))
  "The settings that decoding reads, carried to the thread that draws a PDF's
page, takes out its text, draws an image or makes a preview, as they are on
the editor's when that starts (TUATARA.RUNTIME:CARRY-SETTINGS): the
sandbox's, and those of decoding.")

(define-record (decoding-job (:include tuatara.runtime:job) (:constructor nil) (:copier nil))
  "A job whose work is a file decoded in the sandbox for the view of its
buffer, such as a picture drawn or a PDF's text taken out: \\[keyboard-quit]
in that buffer stops it (STOP-DECODING), its programs ended with all they
started, as killing the buffer or leaving does.")

(defmethod tuatara.runtime:job-at-exit ((job decoding-job))
  "Decoding only shows something: stopped without asking."
  :quiet)

(define-record (drawing-job (:include decoding-job) (:constructor nil) (:copier nil))
  "A decoding job drawing the picture of the file at PATH (START-DRAWING-JOB):
RESULT, once done, what its work gave; FAILURE, why there is nothing, in
words, when the file could not be read or memory ran short."
  (path nil :documentation "The path of the file whose picture DRAWING-JOB draws.")
  (result nil :documentation "What the work of DRAWING-JOB gave, once it is done.")
  (failure nil
           :documentation "Why DRAWING-JOB gave nothing, in words, when the file could not be read or memory ran short,
or NIL."))

(tuatara.runtime:define-job-words drawing-job
  :doing "Drawing"
  :description (format nil "Drawing ~a" (tuatara.io:native-file-name (drawing-job-path job)))
  :verb "draw it"
  :left-words "nothing drawn")

(defun start-drawing-job (job function &key (name "tuatara drawing"))
  "Start JOB, a DRAWING-JOB, on a thread called NAME, the settings decoding
reads carried to it (*DECODING-CARRIED-SETTINGS*): FUNCTION is called with
the descriptor of JOB's file, opened there, and the cell that stops it
\(the job's cancel), and what it returns is JOB's RESULT. When the file
cannot be read, is another than the one JOB's buffer was made for
\(:VIEWED-STAMP), or memory runs short, the RESULT is NIL and FAILURE says why."
  (let ((same-as (let ((buffer (tuatara.runtime:job-buffer job)))
                   (and buffer (tuatara.buffer:buffer-local buffer :viewed-stamp)))))
    (tuatara.runtime:start-job-thread
     job
     (tuatara.runtime:carry-settings
      *decoding-carried-settings*
      (lambda (job)
        (setf (drawing-job-result job)
              (handler-case
                  (tuatara.io:call-with-file (drawing-job-path job)
                                             (lambda (fd path size)
                                               (declare (ignore path size))
                                               (funcall function fd (tuatara.runtime:job-cancel job)))
                                             :same-as same-as)
                (tuatara.io:file-system-error (e)
                  (setf (drawing-job-failure job) (tuatara.io:file-system-error-reason e))
                  nil)
                (storage-condition ()
                  (setf (drawing-job-failure job) "there was not memory enough to draw it")
                  nil)))))
     :name name)))

(define-record (viewer-search-job (:include decoding-job)
                                 (:constructor make-viewer-search-job (buffer words search found))
                                 (:copier nil))
  "A search through what a view of BUFFER shows (its octets, a PDF's text), a
job: WORDS, what is looked for, in words; SEARCH, a function of a function
telling it to stop, giving what it found as values; FOUND, called on the
editor's thread with those values, BUFFER current; RESULT, those values as a
list. \\[keyboard-quit] in BUFFER stops it, as it stops decoding."
  (words nil :documentation "What VIEWER-SEARCH-JOB looks for, in words.")
  (search nil :documentation "A function of a function telling it to stop, giving what it found as values.")
  (found nil :documentation "A function called on the editor's thread with the values found, the buffer current.")
  (result nil :documentation "The values VIEWER-SEARCH-JOB found, as a list."))

(tuatara.runtime:define-job-words viewer-search-job
  :doing "Searching"
  :description (format nil "Searching for ~a" (viewer-search-job-words job))
  :verb "search"
  :left-words "nothing found")

(defmethod tuatara.runtime:job-take-in ((job viewer-search-job))
  "Hand what the search found to its FOUND, its buffer current; say how it
ended when it did not end done."
  (let ((buffer (tuatara.runtime:job-buffer job)))
    (cond ((not (eq (tuatara.runtime:job-status job) :done))
           (message "Search for ~a: ~a" (viewer-search-job-words job) (tuatara.runtime:job-ending-words job)))
          ((live-buffer-p buffer)
           (let ((*current-buffer* buffer))
             (apply (viewer-search-job-found job) (viewer-search-job-result job)))))))

(defun start-viewer-search (buffer words search found)
  "Search what a view of BUFFER shows in the background, for WORDS (words for
what is looked for): SEARCH, a function of a function telling it to stop,
runs on a job of BUFFER's, and FOUND is called with what it gives, on the
editor's thread, BUFFER current (VIEWER-SEARCH-JOB)."
  (tuatara.runtime:start-job-thread
   (make-viewer-search-job buffer words search found)
   (lambda (job)
     (setf (viewer-search-job-result job)
           (multiple-value-list (funcall (viewer-search-job-search job)
                                         (lambda () (car (tuatara.runtime:job-cancel job)))))))
   :name "tuatara view search"))

(defun stop-decoding ()
  "Keyboard-quit function: stop the decoding jobs (DECODING-JOB) running for
the current buffer, their programs ended with all they started."
  (dolist (job (tuatara.runtime:jobs-of (current-buffer) :running t))
    (when (and (decoding-job-p job) (not (car (tuatara.runtime:job-cancel job))))
      (tuatara.runtime:job-stop job))))

(defparameter *pdf-decoders*
  '((:name "poppler"
     :info ("pdfinfo" "fd://0")
     :text ("pdftotext" "-layout" "-enc" "UTF-8" "-l" :pages "fd://0" "-")
     :page ("pdftoppm" "-cropbox" "-f" :page "-l" :page "-scale-to-x" :width "-scale-to-y" :height "fd://0")))
  "The programs known to take the text out of a PDF, the first found used. Each
is a list of :NAME, the package they come in, and three more entries. :TEXT is
the program printing the text and its arguments (:PAGES standing for how many
pages, the file read on standard input). :INFO is the one printing the number
of pages, their size and the like, as lines of Name: value. :PAGE is the one
drawing page :PAGE as a PPM or PAM image :WIDTH by :HEIGHT pixels (-1 for a
height that keeps the page's proportions), the part of it a PDF program
shows (its crop box), as :INFO gives its size.")

(defparameter *pdf-text-pages* 50
  "The pages of a PDF whose text is taken out for a preview, from the first.")

(defun find-pdf-decoder ()
  "The first of *PDF-DECODERS* whose program for the text the sandbox finds,
or NIL."
  (find-if (lambda (decoder) (tuatara.io:sandbox-find-program (first (getf decoder :text)))) *pdf-decoders*))

(defun decoding-unavailable-words (decoder what decoders)
  "Why WHAT (words: pictures, its text) cannot be decoded at all, in words: the
sandbox not ready (NO-SANDBOX-WORDS), or DECODER, the first of DECODERS found,
NIL (NO-DECODER-WORDS); NIL when it can be."
  (let ((status (tuatara.io:sandbox-status)))
    (cond ((not (eq status :ready)) (no-sandbox-words status))
          ((null decoder) (no-decoder-words what decoders)))))

(defun no-decoder-words (what decoders)
  "Why WHAT (words: pictures, its text, PDFs) cannot be decoded when no program
of DECODERS (*PDF-DECODERS*, *IMAGE-DECODERS*) is found, in words naming the
package and program of the first, as each view says it."
  (let ((decoder (first decoders)))
    (format nil "no program for ~a (install ~a: ~a)" what (getf decoder :name)
            (first (or (getf decoder :text) (getf decoder :program))))))

(defun decoder-words (decoder)
  "DECODER's programs as a sentence names them: pdftotext and pdfinfo (poppler)."
  (let ((text (first (getf decoder :text)))
        (info (and (getf decoder :info) (tuatara.io:sandbox-find-program (first (getf decoder :info)))
                   (first (getf decoder :info)))))
    (format nil "~a~@[ and ~a~] (~a)" text info (getf decoder :name))))

(defun pdf-decoder-words ()
  "What decodes PDFs here, as M-x check-health says it: the programs, or what
to install."
  (let ((decoder (find-pdf-decoder)))
    (if decoder
        (format nil "PDF text by ~a" (decoder-words decoder))
        (format nil "PDF text by none of ~{~a~^, ~}: ~a"
                (mapcar (lambda (d) (first (getf d :text))) *pdf-decoders*)
                (outside-program-install-words :pdf)))))

(defun sandbox-font-directories ()
  "Your own font directories, each (SOURCE . TARGET), TARGET where a program
in the sandbox, its home /tmp, finds them: $XDG_DATA_HOME/fonts (or
~/.local/share/fonts) and ~/.fonts, so that a document in a font you
installed shows in it."
  (mapcar #'cons (tuatara.io:user-font-directories) '("/tmp/.local/share/fonts" "/tmp/.fonts")))

(defparameter *page-drawing-seconds* 120
  "The most processor seconds a program drawing a document's pages as a PDF
\(SANDBOX-PDF), LibreOffice or WeasyPrint, may take.")

(defparameter *page-drawing-memory* (* 2 1024 1024 1024)
  "The most memory a program drawing a document's pages as a PDF (SANDBOX-PDF)
may take.")

(defun sandbox-pdf (program arguments fd &key (label program) files directories cancel)
  "The PDF PROGRAM, run shut in with ARGUMENTS (TUATARA.IO:SANDBOX-RUN),
prints of the document open at file descriptor (FD), given on its standard
input, as a sealed file in memory: it is shown your fonts
(SANDBOX-FONT-DIRECTORIES), and FILES and DIRECTORIES as SANDBOX-RUN takes
them, and held to *PAGE-DRAWING-SECONDS* and *PAGE-DRAWING-MEMORY*. An error
saying why, LABEL naming the program, when it prints no PDF. CANCEL, a cons,
stops it when its car turns true. Waits for it: call it from a thread working
in the background."
  (multiple-value-bind (octets outcome said)
      (tuatara.io:sandbox-run program arguments :input fd :seconds *page-drawing-seconds* :memory *page-drawing-memory*
                                                :cancel cancel :files files
                                                :directories (append directories (sandbox-font-directories)))
    (unless (and (eq outcome :done) (tuatara.io:octets-at-p octets "%PDF"))
      (refuse "~a" (or (sandbox-outcome-words outcome said :label label :seconds-setting '*page-drawing-seconds*)
                      (format nil "~a drew no pages" label))))
    (tuatara.io:memory-file "tuatara-pages" octets :kept nil)))

(defun no-sandbox-words (status)
  "Why nothing is decoded while the sandbox has STATUS (TUATARA.IO:SANDBOX-STATUS)."
  (case status
    (:missing "no sandbox, bwrap not found (M-x check-health)")
    (:broken "the sandbox cannot run (M-x check-health)")
    (t "decoding is off (*sandbox*)")))

(defun decoder-arguments (arguments values)
  "ARGUMENTS of a decoding program as it is run: each keyword among them
replaced by the arguments VALUES, a plist, gives it (a list of strings,
perhaps empty), the rest kept."
  (mapcan (lambda (a) (if (keywordp a) (copy-list (getf values a)) (list a))) arguments))

(defun picture-damage-words (said)
  "The first line of what a program drawing a picture SAID that tells of the
picture being damaged (cut short, its data corrupt), what it drew being only
part of it, without the program's name and where in its source it said it
(Premature end of JPEG file); NIL when it says nothing of the kind."
  (let ((line (find-if (lambda (line) (cl-ppcre:scan "(?i)premature end|corrupt|truncat|damaged|unexpected end|insufficient image data" line))
                       (uiop:split-string (or said "") :separator '(#\Newline)))))
    (and line (escape-controls
               (string-trim " " (cl-ppcre:regex-replace "(?: [`'(].*| @ .*)$" (cl-ppcre:regex-replace "^[a-z]+: " line "") ""))))))

(defparameter *pdf-info-fields*
  '(("Title" "Subject" "Keywords" "Author" "Creator" "Producer" "CreationDate" "ModDate")
    ("Custom Metadata" "Metadata Stream" "Tagged" "UserProperties" "Suspects" "Form" "JavaScript" "Pages"
     "Encrypted" "Page size" "Page rot" "File size" "Optimized" "PDF version"))
  "The fields pdfinfo writes of a PDF, in the order it writes them, each at the
start of a line, as two lists: those holding the PDF's own words, which may
have line breaks in them, then those pdfinfo finds for itself, a line each.")

(defun info-field-index (line names)
  "Where among NAMES the name LINE begins with, before its colon, is; NIL when
it begins with none of them."
  (let ((colon (position #\: line)))
    (and colon (position (subseq line 0 colon) names :test #'string=))))

(defun info-fields (info)
  "INFO, what an :INFO program printed of a PDF, as its fields in order, each
the text of one: its name, a colon and its value, with the line breaks the
PDF's own words hold. The fields pdfinfo finds for itself (*PDF-INFO-FIELDS*)
are read from the end, a line each, each before the one after it in their
order; a line Name: value naming no field of either list, one a newer pdfinfo
writes, is read past as a field of its own. The lines above them hold the
PDF's own words: a line beginning one of those fields after the one before it
in their order begins that field, any other line is part of the field before.
So the PDF's words never pass for a field pdfinfo found, such as how many
pages it has or whether it has JavaScript; a line of them can pass only for
another field of its own words, or one pdfinfo is not known to write. When no
field pdfinfo finds is read, debug mode says so."
  (destructuring-bind (own found) *pdf-info-fields*
    (let* ((lines (coerce (uiop:split-string info :separator '(#\Newline)) 'vector))
           (end (or (position "" lines :test-not #'string= :from-end t) -1))
           (limit (length found))
           (tail '())
           (fields '())
           (last -1))
      (loop while (>= end 0)
            do (let* ((line (aref lines end))
                      (index (info-field-index line found)))
                 (cond ((and index (< index limit)) (setf limit index))
                       ((or index (info-field-index line own)
                            (not (cl-ppcre:scan "^[A-Z][A-Za-z ]*: " line)))
                        (loop-finish)))
                 (push line tail)
                 (decf end)))
      (when (and (= limit (length found)) (>= end 0))
        (tuatara.runtime:debug-report :file "pdfinfo: none of the fields it finds itself read from its ~d lines"
                                      (1+ end)))
      (loop for i from 0 to end
            for line = (aref lines i)
            for index = (info-field-index line own)
            do (cond ((and index (> index last)) (setf last index) (push line fields))
                     (fields (setf (first fields) (format nil "~a~%~a" (first fields) line)))
                     ((string/= line "") (push line fields))))
      (append (nreverse fields) tail))))

(defun info-field (info name)
  "The value of the field NAME in INFO, the text an :INFO program printed, or
NIL: the fields as INFO-FIELDS reads them, so that no words of the PDF stand
in for what the program found."
  (dolist (field (info-fields info))
    (let ((colon (position #\: field)))
      (when (and colon (string-equal name (subseq field 0 colon)))
        (return (string-trim " " (subseq field (1+ colon))))))))

(defun pdf-pages-text (text)
  "TEXT, pages as the text program prints them, a form feed after each, with a
line saying where each page after the first begins."
  (let ((pages (uiop:split-string text :separator '(#\Page))))
    (when (and (rest pages) (string= "" (string-trim '(#\Newline #\Space) (first (last pages)))))
      (setf pages (butlast pages)))
    (with-output-to-string (out)
      (loop for page in pages
            for n from 1
            do (when (> n 1) (format out "~&~%── Page ~d ──~%~%" n))
               (write-string page out)))))

(defun page-size (info)
  "The width and height of the first page as INFO (what the :INFO program
printed) gives its size and turn, in points, exact, as a list; NIL when INFO
does not say its size."
  (let* ((size (info-field info "Page size"))
         (numbers (and size (ignoring-errors-reported ("reading the page size ~a" size)
                              (let ((words (uiop:split-string size)))
                                (list (parse-decimal (first words)) (parse-decimal (third words)))))))
         (turn (whole-number (info-field info "Page rot"))))
    (when (and numbers (every (lambda (n) (and (realp n) (plusp n))) numbers))
      (if (member turn '(90 270)) (reverse numbers) numbers))))

(defun page-pixels (info width height)
  "The width and height in pixels of the first page, as INFO (what the :INFO
program printed) gives its size and turn (PAGE-SIZE), as large as fits in
WIDTH by HEIGHT; NIL when INFO does not say its size."
  (let ((size (page-size info)))
    (when size
      (destructuring-bind (across down) size
        (let ((scale (min (/ width across) (/ height down))))
          (list (max 1 (round (* across scale))) (max 1 (round (* down scale)))))))))

(defun pages-note (pages)
  "The note over a PDF's text saying how many PAGES it has, as DECODE-PDF gives
its notes, (TEXT . FACE)."
  (cons (format nil ", ~:d page~:p" pages) nil))

(defun decode-pdf (fd &key cancel (limit (expt 2 20)) pixels on-info ((:text text-wanted) t))
  "The text of the PDF open as file descriptor (FD), taken out in the sandbox,
as four values: the text of its first *PDF-TEXT-PAGES* pages, up to LIMIT
octets (1 MB unless given; NIL when there is none); notes about it for a
line over it, each (TEXT . FACE): how many pages, JavaScript and encryption
in :warning, or why there is no text in :comment; and with PIXELS, a list
of the most width and height in pixels, its first page drawn as large as
fits there (a TUATARA.FRONTEND:IMAGE), or NIL; and what its :INFO program
said of it (pages, their size), as text. Waits for the programs: call it
from a thread working in the background. CANCEL, a cons, stops them when
its car turns true; NIL, NIL, NIL then. ON-INFO, when given, is called with
what the :INFO program said as soon as it has, before the text is taken
out. With TEXT NIL, no text is taken out, only how many pages it has and its
first page drawn."
  (let ((decoder (find-pdf-decoder))
        (notes '())
        (info "")
        (text nil))
    (flet ((note (text &optional face) (push (cons text face) notes))
           (stopped () (return-from decode-pdf (values nil nil nil)))
           (run (spec values output-limit)
             ;; SPEC's program, its arguments' keywords given by VALUES (DECODER-ARGUMENTS)
             (destructuring-bind (program &rest arguments) spec
               (multiple-value-list
                (tuatara.io:sandbox-run program (decoder-arguments arguments values)
                                        :input fd :cancel cancel :output-limit output-limit)))))
      (let ((why (decoding-unavailable-words decoder "its text" *pdf-decoders*)))
        (when why
          (return-from decode-pdf (values nil (list (cons (format nil ", not decoded: ~a" why) :comment)) nil))))
      ;; how many pages, their size, JavaScript and encryption
      (let ((pages nil) (spec (getf decoder :info)))
        (when (and spec (tuatara.io:sandbox-find-program (first spec)))
          (destructuring-bind (output outcome &rest ignore) (run spec '() 65536)
            (declare (ignore ignore))
            (when (eq outcome :cancelled) (stopped))
            (when (eq outcome :done)
              (setf info (tuatara.text:lenient-string output)
                    pages (whole-number (info-field info "Pages")))
              (when on-info (funcall on-info info))
              (when pages (push (pages-note pages) notes))
              (when (equal "yes" (info-field info "JavaScript")) (note ", with JavaScript" :warning))
              (when (uiop:string-prefix-p "yes" (info-field info "Encrypted")) (note ", encrypted" :warning)))))
        ;; the text
        (let ((program (and text-wanted (first (getf decoder :text)))))
          (destructuring-bind (&optional output outcome said)
              (and program (run (getf decoder :text) (list :pages (list (princ-to-string *pdf-text-pages*))) limit))
            (case outcome
              ((nil))
              (:cancelled (stopped))
              ((:done :too-much)
               (let ((all (tuatara.text:lenient-string output)))
                 (cond ((string= "" (string-trim *blank-characters* all))
                        (note ", no text in it (pages of pictures?)" :comment))
                       (t
                        (when (or (eq outcome :too-much) (and pages (> pages *pdf-text-pages*)))
                          (note ", the text of its start" :comment))
                        (setf text (pdf-pages-text all))))))
              (t (note (format nil ", not decoded: ~a" (sandbox-outcome-words outcome said :label program)) :comment))))))
      ;; the first page drawn
      (values text (reverse notes)
              (and pixels (decode-pdf-page fd 1 (first pixels) (second pixels) :info info :cancel cancel
                                                                                  :decoder decoder))
              info))))

(defun decode-pdf-page (fd page width height &key info cancel (decoder (find-pdf-decoder)))
  "Page PAGE (from 1) of the PDF open as file descriptor (FD), drawn in the
sandbox as large as fits in WIDTH by HEIGHT pixels, as a
TUATARA.FRONTEND:IMAGE; NIL when it could not be drawn, or CANCEL, a cons,
had a true car. INFO, what the :INFO program of DECODER printed, gives the
pages' size; without it the page is drawn WIDTH wide. Waits for the
program: call it from a thread working in the background."
  (let ((spec (getf decoder :page)))
    (when (and spec (eq (tuatara.io:sandbox-status) :ready) (tuatara.io:sandbox-find-program (first spec)))
      (destructuring-bind (across down) (or (page-pixels (or info "") width height) (list width -1))
        (destructuring-bind (program &rest arguments) spec
          (multiple-value-bind (output outcome)
              (tuatara.io:sandbox-run program
                                      (decoder-arguments arguments (list :page (list (princ-to-string page))
                                                                         :width (list (princ-to-string across))
                                                                         :height (list (princ-to-string down))))
                                      :input fd :cancel cancel
                                      ;; its height not known: up to 16 times its width, as a long receipt
                                      :output-limit (+ 4096
                                                       (* 3 across (if (plusp down) down (min 16384 (* 16 across))))))
            (and (eq outcome :done) (decoded-image output))))))))

;;; Pictures: the pixels of an image, made no larger than they are shown

(defparameter *image-decoders*
  (append (mapcar (lambda (program)
                    (list :name "ImageMagick"
                          :program (list* program '(:density :jpeg-hint :input "-auto-orient" "-resize" :size
                                                    "-alpha" "on" "-depth" "8" "PAM:-"))
                          :jpeg-hint '("-define" "jpeg:size=~dx~d")
                          :sandbox 'magick-sandbox))
                  '("magick" "convert-im6.q16" "convert-im6.q16hdri"))
          '((:name "GraphicsMagick"
             :program ("gm" "convert" :density :jpeg-hint :input "-auto-orient" "-resize" :size "-depth" "8" "PAM:-")
             :jpeg-hint ("-size" "~dx~d"))))
  "The programs known to turn an image into pixels, the first found used
\(ImageMagick 7's magick, then ImageMagick 6's convert as some systems still
have it, under the same policy, then GraphicsMagick). Each is a list of :NAME,
the package they come in, and :PROGRAM, the program and its arguments. In the
arguments, :INPUT stands for the image on standard input, its format named
\(PNG:-[0], its first frame). :SIZE stands for the most it may be (640x480>,
made no larger than it is). :DENSITY stands for how finely a drawing (SVG) is
drawn before that, -density N, when asked (nothing otherwise). Last, :JPEG-HINT
gives, for a JPEG, the arguments of its :JPEG-HINT with that size. The JPEG is
then decoded at the smallest fraction of its size (an eighth, two eighths...)
no smaller than that, from the start, so that a photo of a hundred megapixels
needs little more memory than a small one. It prints the pixels as a PAM (or
PPM) image. :SANDBOX names a function giving what else the sandbox gives it, as
TUATARA.IO:SANDBOX-RUN's :FILES and :ENVIRONMENT.")

(defvar *magick-largest-side* "64KP"
  "The widest and tallest image ImageMagick reads for a preview, in its own
words (64KP, 65,536 pixels); a larger one is refused before its pixels are
made.")

(define-setting *picture-memory* (* 4 1024 1024 1024)
  "The most memory the program drawing a large picture is given.
A picture's own size, read from its file, tells how much it needs (about 32
octets a pixel, a hundred-megapixel PNG some 3.9 GB), never less than
*SANDBOX-MEMORY*; one needing more than this is not drawn, and says so. A JPEG
needs little whatever its size, decoded at a fraction of it. A JPEG 2000 or
JPEG XL picture is given time for its size (PICTURE-SECONDS), so this bounds
that too.
- a size: a whole number of octets, or words such as \"8G\"; 4 GB as standard.
- nil: no most; a picture is given what it needs."
  :values (or (member nil) (octets 0 *)))

(defun picture-pixels (fd head)
  "How many pixels the picture open as file descriptor (FD), whose first
octets are HEAD, has, as their kind gives its size
(TUATARA.IO:OCTETS-PICTURE-SIZE: 12000 × 9000); NIL when it gives none."
  (multiple-value-bind (width height) (and head (ignoring-errors-reported ("reading the size of a picture")
                                                  (tuatara.io:octets-picture-size head)))
    (cond (width (* width height))
          ;; a TIFF whose first directory stands after its pixels, past HEAD
          ((and head (tuatara.io:tiff-octets-p head))
           (multiple-value-bind (width height) (tiff-file-size fd head)
             (and width (* width height)))))))

(defun raf-preview (fd head)
  "The JPEG a Fujifilm raw open as file descriptor (FD), whose first octets
are HEAD, carries of its picture (its header gives where, at octet 84, and
how long, at 88), read with pread; NIL when FD is no such raw or the JPEG
is not where it says."
  (when (and head (tuatara.io:octets-at-p head "FUJIFILMCCD-RAW"))
    (let ((offset (tuatara.io:octets-integer head 84 4 :big-endian t))
          (length (tuatara.io:octets-integer head 88 4 :big-endian t)))
      (when (and offset length (< 0 length (* 64 1024 1024)))
        (let ((jpeg (tuatara.io:read-fd-exactly fd length offset)))
          (and jpeg (tuatara.io:octets-at-p jpeg '(#xFF #xD8 #xFF)) jpeg))))))

(defun tiff-file-size (fd head)
  "The width and height of the first image of the TIFF open as file descriptor
(FD), whose first octets are HEAD, its first directory read where it
stands, as two values; NIL when it cannot be."
  (let* ((little (= (aref head 0) 73))
         (ifd (tuatara.io:octets-integer head 4 4 :big-endian (not little)))
         (count-octets (and ifd (tuatara.io:read-fd-exactly fd 2 ifd)))
         (count (and count-octets (min 512 (tuatara.io:octets-integer count-octets 0 2 :big-endian (not little)))))
         (directory (and count (plusp count) (tuatara.io:read-fd-exactly fd (+ 2 (* 12 count)) ifd)))
         (entries (and directory (tuatara.io:tiff-entries directory :ifd 0 :little little)))
         (width (fourth (assoc 256 entries)))
         (height (fourth (assoc 257 entries))))
    (and width height (values width height))))

(defun octets-kind-of-file (fd octets)
  "What the file open as file descriptor (FD) is by its first OCTETS, as two
values, the words and the keyword of TUATARA.IO:OCTETS-KIND, a TIFF's size
read where its first directory stands when that is past them
(TIFF-FILE-SIZE)."
  (multiple-value-bind (words kind) (tuatara.io:octets-kind octets)
    (if (and (eq kind :tiff) (not (search "×" words)))
        (multiple-value-bind (width height) (ignoring-errors-reported ("reading the size of a TIFF")
                                              (tiff-file-size fd octets))
          (values (if width (format nil "~a, ~a" words (tuatara.io:size-words width height)) words) kind))
        (values words kind))))

(defun picture-memory (kind pixels octets &optional (channels 0))
  "The memory the program drawing a picture of KIND of PIXELS pixels (NIL: not
known), its file OCTETS long, is given. It is *SANDBOX-MEMORY*, or what a large
one needs, its file read whole first, and four octets a pixel more for each of
its CHANNELS that its decoder keeps apart (a JPEG 2000 picture's components, a
JPEG XL picture's channels). For a JPEG it is the first, decoded at a fraction
of its size. For a camera's raw it is what its file's size calls for, developed
whole, up to *PICTURE-MEMORY*."
  (cond ((eq kind :raw)
         ;; its size not in its first octets (they hold a thumbnail's): reckoned from its file's
         (let ((need (+ (* 1024 1024 1024) (* 64 (or octets 0)))))
           (max (or tuatara-user:*sandbox-memory* 0) (if *picture-memory* (min need *picture-memory*) need))))
        ((and pixels (not (eq kind :jpeg)))
         (max (or tuatara-user:*sandbox-memory* 0) (+ (* 512 1024 1024) (* (+ 32 (* 4 channels)) pixels) (or octets 0))))
        (t tuatara-user:*sandbox-memory*)))

(defparameter *pictures-sized-first* '(:jp2 :j2k :jxl)
  "The kinds of picture drawn only when their header, read before anything of
them is decoded (CODED-PICTURE-HEADER), gives their size, and that size is
within what *PICTURE-MEMORY* allows: JPEG 2000 and JPEG XL.")

(defparameter *picture-seconds-per-megapixel* 3/2
  "The seconds, beyond *SANDBOX-SECONDS*, the program drawing a JPEG 2000 or
JPEG XL picture is given for each megapixel its header claims (PICTURE-SECONDS):
a picture of 20 megapixels is given 40 seconds, *SANDBOX-SECONDS* at 10. The
time so grows only as far as the largest picture *PICTURE-MEMORY* lets be
drawn: 138 seconds for one of three components and 85 megapixels, the
largest 4 GB allows.")

(defun picture-seconds (pixels)
  "The seconds, of the clock and of the processor, the program drawing a JPEG
2000 or JPEG XL picture of PIXELS pixels is given: *SANDBOX-SECONDS* and
*PICTURE-SECONDS-PER-MEGAPIXEL* for each megapixel; NIL when
*SANDBOX-SECONDS* is NIL, no limit. A picture larger than *PICTURE-MEMORY*
allows is never drawn, which bounds the time."
  (let ((base tuatara-user:*sandbox-seconds*))
    (when base
      (ceiling (+ base (* *picture-seconds-per-megapixel* (/ pixels 1000000)))))))

(defun coded-picture-header (fd head kind)
  "What the JPEG 2000 or JPEG XL picture of KIND (:jp2 or :j2k, either taken
for both, or :jxl) open as file descriptor (FD), whose first octets are HEAD,
says of itself in its header (TUATARA.IO:J2K-HEADER,
TUATARA.IO:JXL-FILE-HEADER). A file kept in boxes is read from FD where its
codestream begins when that is past HEAD. NIL when its header does not say, or
for another kind."
  (let* ((size (tuatara.io:fd-size fd))
         (read (lambda (at count)
                 (let ((count (and size (min count (- size at)))))
                   (and count (plusp count) (tuatara.io:read-fd-exactly fd count at))))))
    (case kind
      (:jxl (or (tuatara.io:jxl-file-header head)
                (tuatara.io:jxl-file-header head :read read)))
      ((:jp2 :j2k)
       (if (tuatara.io:octets-at-p head '(#xFF #x4F #xFF #x51))
           (tuatara.io:j2k-header head)
           (or (tuatara.io:jp2-header head)
               (let* ((start (tuatara.io:box-contents-start head '("jp2c") :read read))
                      (codestream (and start (funcall read start 65536))))
                 (and codestream (tuatara.io:j2k-header codestream)))))))))

(defun coded-picture-refusal (kind header)
  "Why the JPEG 2000 or JPEG XL picture of KIND whose header
\(CODED-PICTURE-HEADER) is HEADER is not drawn before anything of it is
decoded, in words: no size in its header, or a header no such picture has
\(more tiles or bits than JPEG 2000 allows); NIL when it may be drawn."
  (cond ((not (member kind *pictures-sized-first*)) nil)
        ((null header) "its header gives no size, or is damaged")
        ((and (getf header :tiles) (> (getf header :tiles) 65535))
         (format nil "its header is damaged (~:d tiles, more than JPEG 2000 allows)" (getf header :tiles)))
        ((and (getf header :precision) (> (getf header :precision) 38))
         (format nil "its header is damaged (~d-bit samples, more than JPEG 2000 allows)" (getf header :precision)))))

(defvar *large-picture-turn* (sb-thread:make-semaphore :count 1 :name "tuatara large picture")
  "Held while a large picture is drawn, one given more memory than
*SANDBOX-MEMORY*, so that one such picture is drawn at a time.")

(defun call-in-picture-turn (large cancel function)
  "Call FUNCTION and return what it returns; when LARGE, only once no other
large picture is being drawn (*LARGE-PICTURE-TURN*). NIL, :CANCELLED when
CANCEL, a cons, has its car turn true while it waits."
  (if large
      (loop (when (and cancel (car cancel)) (return (values nil :cancelled nil)))
            (when (sb-thread:wait-on-semaphore *large-picture-turn* :timeout 1/10)
              (return (unwind-protect (funcall function)
                        (sb-thread:signal-semaphore *large-picture-turn*)))))
      (funcall function)))

(defparameter *magick-policy-path* "/etc/tuatara/ImageMagick/policy.xml"
  "Where ImageMagick, shut in, finds the policy Tuatara gives it (MAGICK-SANDBOX).")

(defun magick-policy (memory)
  "The text of the policy Tuatara gives ImageMagick (MAGICK-SANDBOX), MEMORY
the octets the program is given (NIL: no limit), its own limits for memory,
mapped memory and disk set to that."
  (format nil "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<policymap>
  <policy domain=\"resource\" name=\"width\" value=\"~a\"/>
  <policy domain=\"resource\" name=\"height\" value=\"~:*~a\"/>
  <policy domain=\"resource\" name=\"list-length\" value=\"128\"/>
  <policy domain=\"resource\" name=\"thread\" value=\"2\"/>~@[
  <policy domain=\"resource\" name=\"memory\" value=\"~dMiB\"/>
  <policy domain=\"resource\" name=\"map\" value=\"~:*~dMiB\"/>
  <policy domain=\"resource\" name=\"disk\" value=\"~:*~dMiB\"/>~]
  <policy domain=\"delegate\" rights=\"none\" pattern=\"*\"/>
  <policy domain=\"filter\" rights=\"none\" pattern=\"*\"/>
  <policy domain=\"path\" rights=\"none\" pattern=\"@*\"/>
  <policy domain=\"coder\" rights=\"none\" pattern=\"*\"/>
  <policy domain=\"coder\" rights=\"read\" pattern=\"{~{~a~^,~}}\"/>
  <policy domain=\"coder\" rights=\"write\" pattern=\"PAM\"/>
</policymap>
" *magick-largest-side* (and memory (max 1 (floor memory (* 1024 1024)))) (mapcar #'cdr *image-formats*)))

(defun magick-sandbox (&key memory overlays)
  "What the sandbox gives ImageMagick: a policy of its own (MAGICK-POLICY,
MEMORY the octets it is given) at *MAGICK-POLICY-PATH*, through
MAGICK_CONFIGURE_PATH, and the same policy in place of each of OVERLAYS, the
other policy files it reads (MAGICK-POLICY-OVERLAYS). The policy reads only the
formats of *IMAGE-FORMATS* and writes only PAM pixels. It allows no program
started \(delegates), no filters and no file named by @. It allows no image
wider or taller than *MAGICK-LARGEST-SIDE*, two threads at most, and no more
memory than it is given."
  (let ((policy (magick-policy memory)))
    (list :files (mapcar (lambda (path) (cons path policy)) (cons *magick-policy-path* overlays))
          :environment '(("MAGICK_CONFIGURE_PATH" "/etc/tuatara/ImageMagick")))))

(defvar *magick-checked* (make-hash-table :test 'equal :synchronized t)
  "What was found of each decoding program by running it shut in, by what is
asked and the program's path, with the stamp of its file then: the policy
files it reads (MAGICK-POLICY-OVERLAYS), the formats it reads
\(IMAGE-READERS-MISSING). Asked again once the program's file changes.")

(defun decoder-words-before-keywords (decoder)
  "The program of DECODER and the arguments before the first keyword, as it
is run for anything but a picture (-list format)."
  (loop for word in (getf decoder :program) until (keywordp word) collect word))

(defun magick-checked (decoder what function)
  "What FUNCTION, a function of no arguments running DECODER's program, gives,
kept in *MAGICK-CHECKED* under WHAT, the program's path and the arguments it
is run with, while the program's file is unchanged; NIL when the program is
not found."
  (let* ((path (tuatara.io:sandbox-find-program (first (decoder-words-before-keywords decoder))))
         (stamp (and path (tuatara.io:path-stamp path :failure nil)))
         (key (list* what path (rest (decoder-words-before-keywords decoder))))
         (known (and path (gethash key *magick-checked*))))
    (when path
      (if (and known (equalp (car known) stamp))
          (cdr known)
          (let ((value (funcall function)))
            (setf (gethash key *magick-checked*) (cons stamp value))
            value)))))

(defun magick-policy-sections (text)
  "The policy files ImageMagick's -list policy TEXT names, each (PATH . LINES)
with the lines that follow it; [built-in] among them."
  (let ((sections '()))
    (dolist (line (uiop:split-string text :separator '(#\Newline)) (nreverse sections))
      (let ((line (string-trim '(#\Space #\Tab #\Return) line)))
        (if (uiop:string-prefix-p "Path: " line)
            (push (list (subseq line 6)) sections)
            (when sections (push line (cdr (first sections)))))))))

(defun magick-policy-listed (decoder overlays)
  "The policy files DECODER's program, shut in with Tuatara's policy and
OVERLAYS given it (MAGICK-SANDBOX), lists (-list policy), as
MAGICK-POLICY-SECTIONS gives them; NIL and why not, in words, when it
cannot list them."
  (let ((words (decoder-words-before-keywords decoder)))
    (multiple-value-bind (output outcome said)
        (apply #'tuatara.io:sandbox-run (first words) (append (rest words) (list "-list" "policy"))
               :output-limit (* 1024 1024) (magick-sandbox :overlays overlays))
      (if (eq outcome :done)
          (magick-policy-sections (tuatara.text:lenient-string output))
          (values nil (format nil "its policies could not be listed: ~a"
                              (sandbox-outcome-words outcome said :label (first words))))))))

(defun magick-policy-check (decoder)
  "The policy files besides Tuatara's own DECODER's program reads, as a list
of the paths and NIL, or of NIL and why it is not to be run, in words: it
cannot list them, or one of them, given Tuatara's policy in its place, still
holds another."
  (multiple-value-bind (sections why) (magick-policy-listed decoder '())
    (if why
        (list nil why)
        (let ((overlays (remove-duplicates (loop for (where) in sections
                                                 when (and (uiop:string-prefix-p "/" where)
                                                           (string/= where *magick-policy-path*))
                                                   collect where)
                                           :test #'string=)))
          (multiple-value-bind (again why) (if overlays (magick-policy-listed decoder overlays) sections)
            (let ((other (find-if (lambda (section)
                                    (and (uiop:string-prefix-p "/" (first section))
                                         (not (member "pattern: PAM" (rest section) :test #'string=))))
                                  again)))
              (cond (why (list nil why))
                    (other (list nil (format nil "it reads a policy that cannot be replaced (~a)" (first other))))
                    (t (list overlays nil)))))))))

(defun magick-policy-overlays (decoder)
  "The policy files besides Tuatara's own that DECODER's program, shut in,
reads (its -list policy), each given Tuatara's policy in its place
\(MAGICK-SANDBOX) and checked to hold it then (MAGICK-POLICY-CHECK), as two
values: the paths, and NIL or why the program is not to be run, in words.
Found once for each program file (MAGICK-CHECKED)."
  (let ((found (magick-checked decoder :policy (lambda () (magick-policy-check decoder)))))
    (values (first found) (second found))))

(defparameter *image-formats*
  '((:png . "PNG") (:jpeg . "JPEG") (:gif . "GIF") (:webp . "WEBP") (:bmp . "BMP") (:tiff . "TIFF") (:raw . "DNG")
    (:ico . "ICO") (:heif . "HEIC") (:avif . "AVIF") (:svg . "SVG") (:jp2 . "JP2") (:j2k . "J2K") (:jxl . "JXL"))
  "The kinds of image (TUATARA.IO:OCTETS-KIND's keywords) turned into pixels
for a preview, each with the name of its format that the decoding program is
told, so that it reads the file as that and nothing else. A camera's raw is
developed by ImageMagick's LibRaw reader (DNG), whatever its maker. A JPEG 2000
file or codestream is read by its OpenJPEG reader, a JPEG XL one by its libjxl
reader.")

(defparameter *image-format-libraries*
  '((:jpeg "libjpeg" "libjpeg-turbo") (:tiff "libtiff" "libtiff") (:webp "libwebp" "libwebp")
    (:heif "libheif" "libheif") (:avif "libheif" "libheif") (:svg "librsvg" "librsvg") (:raw "LibRaw" "libraw")
    (:jp2 "OpenJPEG" "openjpeg2") (:j2k "OpenJPEG" "openjpeg2") (:jxl "libjxl" "libjxl"))
  "For the kinds of *IMAGE-FORMATS* that the decoding program reads only with
a library of their own, each (KIND LIBRARY ARCH): the library's name, said
when a picture is not shown for the want of it, and Arch's package of it,
which Arch's ImageMagick leaves to be installed beside it (*OUTSIDE-PROGRAMS*
names them all).")

(defun find-image-decoder ()
  "The first of *IMAGE-DECODERS* whose program the sandbox finds, or NIL."
  (find-if (lambda (decoder) (tuatara.io:sandbox-find-program (first (getf decoder :program)))) *image-decoders*))

(defun image-readers-missing (decoder)
  "The kinds of *IMAGE-FORMATS* the program of DECODER cannot read here, by
what it lists of the formats it reads (-list format), run in the sandbox
once for each program file (MAGICK-CHECKED); NIL when it reads them all, or
when the sandbox cannot run it."
  (when (eq (tuatara.io:sandbox-status) :ready)
    (let ((listed (magick-checked
                   decoder :formats
                   (lambda ()
                     (let ((words (decoder-words-before-keywords decoder)))
                       (multiple-value-bind (output outcome)
                           (tuatara.io:sandbox-run (first words) (append (rest words) (list "-list" "format"))
                                                   :output-limit (* 1024 1024))
                         (if (eq outcome :done)
                             (loop for line in (uiop:split-string (tuatara.text:lenient-string output)
                                                                  :separator '(#\Newline))
                                   for word = (first (tuatara.text:split-words line))
                                   when word collect (string-right-trim "*" word))
                             :unlisted)))))))
      (unless (member listed '(nil :unlisted))
        (loop for (kind . format) in *image-formats*
              unless (member format listed :test #'string-equal) collect kind)))))

(defun image-decoder-words ()
  "What turns images into pixels here, as M-x check-health says it, with the
kinds of picture it has no reader for (IMAGE-READERS-MISSING) and the
library each needs."
  (let ((decoder (find-image-decoder)))
    (if decoder
        (format nil
                "pictures by ~a (~a)~@[, without readers for ~{~a~^, ~}~]"
                (first (getf decoder :program))
                (getf decoder :name)
                (mapcar (lambda (kind)
                          (format nil
                                  "~a~@[ (~a)~]"
                                  (cdr (assoc kind *image-formats*))
                                  (second (assoc kind *image-format-libraries*))))
                        (image-readers-missing decoder)))
        (format nil "pictures by none of ~{~a~^, ~}: ~a, or ~{~a~^ or ~}"
                (mapcar (lambda (d) (first (getf d :program))) *image-decoders*)
                (outside-program-install-words :pictures)
                (remove (getf (outside-program :pictures) :name) (mapcar (lambda (d) (getf d :name)) *image-decoders*)
                        :test #'string=)))))

(defun decoded-image (octets)
  "The picture OCTETS hold as a PAM image (P7) of 3 or 4 channels, or of 1 or 2
\(grey, grey and alpha, made 3 and 4: a black-and-white photo), or a PPM (P6),
8 bits to a channel, as a TUATARA.FRONTEND:IMAGE; NIL when they hold anything
else, or fewer pixels than they say."
  (let ((at 0) (length (length octets)))
    (labels ((line ()
               (let ((end (position 10 octets :start at :end (min length (+ at 80)))))
                 (when end
                   (prog1 (tuatara.text:octets-latin-1 octets :start at :end end) (setf at (1+ end))))))
             (field (line name)
               (and line (uiop:string-prefix-p name line)
                    (whole-number line :start (length name))))
             (image (width height channels)
               (let ((count (and width height channels (* width height channels))))
                 (when (and count (< 0 width 16385) (< 0 height 16385) (member channels '(1 2 3 4))
                            (<= (+ at count) length))
                   (if (<= channels 2)
                       ;; grey made the three colours alike, its alpha kept
                       (let ((pixels (make-array (* width height (+ 2 channels)) :element-type '(unsigned-byte 8))))
                         (loop for from from at below (+ at count) by channels
                               for to from 0 by (+ 2 channels)
                               do (let ((grey (aref octets from)))
                                    (setf (aref pixels to) grey (aref pixels (+ to 1)) grey (aref pixels (+ to 2)) grey)
                                    (when (= channels 2) (setf (aref pixels (+ to 3)) (aref octets (1+ from))))))
                         (tuatara.frontend:make-image width height (+ 2 channels) pixels))
                       (tuatara.frontend:make-image width height channels (subseq octets at (+ at count))))))))
      (let ((magic (line)))
        (cond ((equal magic "P7")
               (let (width height depth maxval)
                 (loop for line = (line)
                       while (and line (not (equal line "ENDHDR")))
                       do (setf width (or (field line "WIDTH ") width)
                                height (or (field line "HEIGHT ") height)
                                depth (or (field line "DEPTH ") depth)
                                maxval (or (field line "MAXVAL ") maxval))
                       finally (unless line (return-from decoded-image nil)))
                 (and (eql maxval 255) (image width height depth))))
              ((equal magic "P6")
               (let* ((size (line)) (maxval (line))
                      (numbers (and size (handler-case (mapcar #'parse-integer (uiop:split-string size))
                                           (parse-error () nil)))))
                 ;; a size line of two numbers alone, else no picture
                 (and (= (length numbers) 2) (equal maxval "255") (image (first numbers) (second numbers) 3)))))))))

(defun picture-refusal (kind header pixels memory decoder policy-refusal)
  "Why a picture of KIND is not drawn before its program runs, in words, or
NIL when it may be: its HEADER damaged or giving no size (CODED-PICTURE-REFUSAL),
the MEMORY drawing its PIXELS would take past *PICTURE-MEMORY*, or
POLICY-REFUSAL, why DECODER's program is not to be run (MAGICK-POLICY-OVERLAYS)."
  (cond ((coded-picture-refusal kind header))
        ((and memory *picture-memory* (> memory *picture-memory*))
         (format nil "~:[it~;~:*~,1f megapixels~] would take ~a to draw, more than *picture-memory* allows (~a)"
                 (and pixels (/ pixels 1d6)) (octets-words memory) (octets-words *picture-memory*)))
        (policy-refusal (format nil "~a: ~a" (first (getf decoder :program)) policy-refusal))))

(defun picture-arguments (decoder kind width height &key head enlarge density (frame 0))
  "The arguments DECODER's program is run with to draw picture FRAME (from 0)
of a file of KIND no larger than WIDTH by HEIGHT pixels (as DECODE-IMAGE
takes ENLARGE and DENSITY), HEAD its first octets, giving a JPEG's own size:
its :PROGRAM's keywords given (DECODER-ARGUMENTS)."
  (decoder-arguments
   (rest (getf decoder :program))
   (list :input (list (format nil "~a:-[~d]" (cdr (assoc kind *image-formats*)) frame))
         :size (list (format nil "~dx~d~:[>~;~]" (max 1 width) (max 1 height) enlarge))
         :density (and density (list "-density" (princ-to-string density)))
         :jpeg-hint (and (eq kind :jpeg) (getf decoder :jpeg-hint)
                         (destructuring-bind (option value) (getf decoder :jpeg-hint)
                           ;; no larger than the JPEG itself
                           (multiple-value-bind (across down) (and head (tuatara.io:octets-picture-size head))
                             (list option (format nil value (max 1 (min width (or across width)))
                                                  (max 1 (min height (or down height)))))))))))

(defun run-picture-decoder (decoder fd arguments width height &key cancel seconds memory octets overlays)
  "Run DECODER's program shut in on the picture open as file descriptor (FD),
OCTETS long, with ARGUMENTS (PICTURE-ARGUMENTS), its output held to WIDTH by
HEIGHT pixels, given SECONDS and MEMORY (and the policy OVERLAYS its sandbox
gives in place, MAGICK-SANDBOX), one picture needing more memory than
*SANDBOX-MEMORY* drawn at a time (CALL-IN-PICTURE-TURN): three values, as
TUATARA.IO:SANDBOX-RUN gives them."
  (let ((tuatara-user:*sandbox-memory* memory)
        ;; all of one run's programs together given no less than it
        (tuatara-user:*sandbox-total-memory* (and tuatara-user:*sandbox-total-memory*
                                                  (max tuatara-user:*sandbox-total-memory* (or memory 0))))
        (large (and memory tuatara-user:*sandbox-memory* (> memory tuatara-user:*sandbox-memory*))))
    (call-in-picture-turn
     large cancel
     (lambda ()
       (apply #'tuatara.io:sandbox-run (first (getf decoder :program)) arguments
              :input fd :cancel cancel :seconds seconds :output-limit (+ 4096 (* 4 (max 1 width) (max 1 height)))
              ;; a TIFF read from a pipe is copied to a file of the sandbox's first
              :file-limit (max tuatara.io:*sandbox-file-limit* (+ (* 1024 1024) (or octets 0)))
              (and (getf decoder :sandbox) (funcall (getf decoder :sandbox) :memory memory :overlays overlays)))))))

(defun decode-image (fd kind width height &key cancel density enlarge (frame 0))
  "The picture in the image open as file descriptor (FD), of KIND
\(TUATARA.IO:OCTETS-KIND's keyword), the first or the one FRAME counts from 0
\(a TIFF's page, an animation's frame), turned into pixels in the sandbox, no
larger than WIDTH by HEIGHT pixels (with ENLARGE, as large as fits in them,
its proportions kept, a smaller one made larger), as two values: the picture (a
TUATARA.FRONTEND:IMAGE), NIL when it could not be made; and notes about it
for a line over it, as DECODE-PDF gives them. Waits for the program: call
it from a thread working in the background. CANCEL, a cons, stops it when
its car turns true; NIL, NIL then. DENSITY, dots an inch, draws a drawing
\(SVG) that finely before it is sized, for print."
  (let ((decoder (find-image-decoder)))
    (flet ((done (image &optional words (face :comment))
             (return-from decode-image (values image (and words (list (cons words face)))))))
      (unless (assoc kind *image-formats*) (done nil))
      (let ((why (decoding-unavailable-words decoder "pictures" *image-decoders*)))
        (when why (done nil (format nil ", not shown: ~a" why))))
      (let* ((octets (tuatara.io:fd-size fd))
             (head (and octets (ignoring-errors-reported ("reading the head of a picture")
                                 (tuatara.io:read-fd-start fd "the picture" octets 65536))))
             (header (and head (member kind *pictures-sized-first*) (coded-picture-header fd head kind)))
             (pixels (if header (* (getf header :width) (getf header :height)) (picture-pixels fd head)))
             (seconds (if header (picture-seconds pixels) tuatara-user:*sandbox-seconds*))
             (memory (picture-memory kind pixels octets (cond ((getf header :components))
                                                              (header (+ 3 (getf header :extra-channels)))
                                                              (t 0))))
             ;; the policy files it reads besides Tuatara's, each given Tuatara's
             (policy (multiple-value-list (and (getf decoder :sandbox) (magick-policy-overlays decoder))))
             (program (first (getf decoder :program))))
        (let ((refusal (picture-refusal kind header pixels memory decoder (second policy))))
          (when refusal (done nil (format nil ", not shown: ~a" refusal))))
        (multiple-value-bind (output outcome said)
            (run-picture-decoder decoder fd (picture-arguments decoder kind width height :head head :enlarge enlarge
                                                                                          :density density :frame frame)
                                 width height :cancel cancel :seconds seconds :memory memory :octets octets
                                              :overlays (first policy))
          (case outcome
            (:cancelled (done nil))
            (:done
             (let ((image (decoded-image output))
                   (damage (picture-damage-words said)))
               (cond ((null image) (done nil (format nil ", not shown: ~a gave no picture" program)))
                     (damage (done image (format nil ", damaged: ~a" damage) :warning))
                     (t (done image)))))
            (:timeout
             (done nil (if header
                           (format nil ", not shown: ~a was stopped after ~d second~:p, all a picture of ~,1f ~
                                        megapixels is given (*sandbox-seconds* and more for each megapixel)"
                                   program seconds (/ pixels 1d6))
                           (format nil ", not shown: ~a" (sandbox-outcome-words outcome said :label program)))))
            (t
             ;; a TIFF-shaped file LibRaw does not develop, a scanner's say: drawn as a TIFF
             (when (and (eq kind :raw) head (tuatara.io:tiff-octets-p head))
               (return-from decode-image (decode-image fd :tiff width height :cancel cancel :density density
                                                                       :enlarge enlarge)))
             ;; a Fujifilm raw LibRaw does not develop (its compressed kind, on many cores): the camera's own
             ;; JPEG of it
             (let ((preview (and (eq kind :raw) (raf-preview fd head))))
               (when preview
                 (return-from decode-image (decode-picture preview :jpeg width height :cancel cancel))))
             (done nil (if (and (eq outcome :failed) (cl-ppcre:scan "(?i)no decode delegate" (or said "")))
                           (format nil ", not shown: ~a here cannot read ~a pictures~@[: it was built or installed ~
                                        without ~a~]"
                                   program (cdr (assoc kind *image-formats*))
                                   (second (assoc kind *image-format-libraries*)))
                           (format nil ", not shown: ~a" (sandbox-outcome-words outcome said :label program)))))))))))

(defun decode-picture (octets kind width height
                       &key (cancel (and tuatara.runtime:*job* (tuatara.runtime:job-cancel tuatara.runtime:*job*))))
  "The picture OCTETS hold, of KIND (TUATARA.IO:OCTETS-KIND's keyword: :jpeg,
:png, :tiff, :jxl, :jp2 for a JPEG 2000 file or :j2k for a bare JPEG 2000
codestream, either read as both, as a PDF's /JPXDecode picture is one or the
other...), turned into pixels in the sandbox as DECODE-IMAGE turns a file's. It
is no larger than WIDTH by HEIGHT pixels. Two values come back: a
TUATARA.FRONTEND:IMAGE, NIL when it could not be made, and notes saying why.
For a package reading pictures it finds in other files: only plain pixels come
back, checked, the picture itself never decoded in the editor. Waits for the
program: call it from a thread working in the background; in a job, stopping
the job (CANCEL, the job's own by default) stops it too."
  (tuatara.io:call-with-memory-file "tuatara-picture" octets
                                    (lambda (fd) (decode-image fd kind width height :cancel cancel))))

(defun sandboxed-mode-line (editor buffer)
  "What the mode line of a buffer adds when what it shows was made by a
program shut in by the sandbox (its :sandboxed local): Sandboxed, as the
text of a PDF, or the picture of an image or a page."
  (declare (ignore editor))
  (and (tuatara.buffer:buffer-local buffer :sandboxed) "Sandboxed"))
