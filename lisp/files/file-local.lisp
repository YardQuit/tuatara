;;;; lisp/files/file-local.lisp --- Settings a file gives itself in a line or block of its own.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A file may name settings for itself, in the form many editors read: on
;;;; its first line (its second after #!), between -*- and -*-,
;;;;
;;;;   # -*- mode: org; fill-column: 72 -*-
;;;;
;;;; or in a block near its end, each line with the same text before it:
;;;;
;;;;   # Local Variables:
;;;;   # tab-width: 8
;;;;   # End:
;;;;
;;;; Only settings that change how the text is shown and edited are taken: the
;;;; major mode, the tab width, tabs or spaces, the fill column, the spaces
;;;; between sentences, each checked, and those a package takes;
;;;; nothing a file says is ever run (eval and the rest are left alone, said
;;;; so), and a file opened safely is never read for them.

(in-package #:tuatara.defaults)

(define-setting *file-local-variables* t
  "Whether a file's own settings are taken as it is opened.
They are -*- mode: org -*- on its first line and a Local Variables block near
its end. Taken are its major mode, tab width, tabs or spaces, fill column and
spaces between sentences, and those a package takes; nothing else.
- t: taken.
- nil: ignored."
  :values boolean)

(defvar *file-local-setting-functions* '()
  "Functions a package adds to take more of a file's own settings: each called
with the buffer, the setting's name (a string, small letters) and its value
\(a whole number, T, NIL or text) when Tuatara does not take it itself, and
true when it took it. Nothing they are given is ever run.")

(defvar *file-local-taken-functions* '()
  "Functions called with a buffer once the settings its file names for itself
are taken, as it is opened (FILE-DECLARES).")

(defparameter *file-local-tail* 3000
  "How many characters at a file's end are looked through for its Local Variables: block.")

(defun file-local-value (text)
  "TEXT, a value as a file writes it, read without running anything: a whole
number, T or NIL, or else the text itself (a name)."
  (let ((text (string-trim '(#\Space #\Tab) text)))
    ;; nine digits at most: no setting takes more, and a longer number is kept as text
    (cond ((and (<= 1 (length text) 9) (every #'digit-char-p text)) (parse-integer text))
          ((string-equal text "t") t)
          ((string-equal text "nil") nil)
          (t text))))

(defun first-line-settings (line)
  "The settings LINE names between -*- and -*-, each (NAME . VALUE): a lone
word naming the mode, or NAME: VALUE pairs apart by semicolons."
  (let* ((open (search "-*-" line))
         (close (and open (search "-*-" line :start2 (+ open 3)))))
    (when close
      (let ((inside (string-trim '(#\Space #\Tab) (subseq line (+ open 3) close))))
        (if (not (find #\: inside))
            (and (plusp (length inside)) (list (cons "mode" inside)))
            (loop for part in (uiop:split-string inside :separator '(#\;))
                  for (name . value) = (setting-pair part)
                  when name collect (cons name (file-local-value value))))))))

(defun setting-pair (text)
  "TEXT, a setting written NAME: VALUE, as (NAME . VALUE), NAME in small
letters without the blanks around it and VALUE as written; NIL without a colon."
  (let ((colon (position #\: text)))
    (and colon (cons (string-downcase (string-trim '(#\Space #\Tab) (subseq text 0 colon)))
                     (subseq text (1+ colon))))))

(defun local-variables-heading (text)
  "Where the Local Variables block of TEXT begins, as three values: the index
of its first line, which says Local Variables:, and what that line has before
and after those words, the text each of the block's lines begins and ends
with. Only a line with nothing after the words but what closes a comment
\(-->, */) begins a block: the last such line in TEXT. NIL when there is none."
  (loop for at = (search "Local Variables:" text :from-end t) then (search "Local Variables:" text :from-end t :end2 at)
        while at
        do (let* ((line (tuatara.text:string-line-start text at))
                  (after (string-trim '(#\Space #\Tab #\Return)
                                      (subseq text
                                              (+ at (length "Local Variables:"))
                                              (tuatara.text:string-line-end text at)))))
             (when (notany (lambda (c) (or (alphanumericp c) (member c '(#\Space #\Tab)))) after)
               (return (values line (subseq text line at) after))))))

(defun local-variables-block (text)
  "The Local Variables block in TEXT, the end of a file: the line saying so
\(LOCAL-VARIABLES-HEADING), then the lines after it that begin with what it
begins with, up to End:. As four values: the text each line begins with and
ends with; its settings' lines, each (START END NAME . VALUE), START and END
the indices of the line in TEXT without its line end, VALUE as written; and
the index of its End: line or, without one, where its lines stop. NIL when
TEXT has none."
  (multiple-value-bind (heading prefix suffix) (local-variables-heading text)
    (when heading
      (let ((settings '()))
        (loop for start = (1+ (tuatara.text:string-line-end text heading)) then (1+ line-end)
              for line-end = (and (< start (length text)) (tuatara.text:string-line-end text start))
              while line-end
              do (let* ((end (if (and (> line-end start) (char= (char text (1- line-end)) #\Return)) (1- line-end) line-end))
                        (line (subseq text start end))
                        (body (and (uiop:string-prefix-p prefix line)
                                   (string-trim '(#\Space #\Tab)
                                                (let ((body (subseq line (length prefix))))
                                                  (if (and (plusp (length suffix)) (uiop:string-suffix-p body suffix))
                                                      (subseq body 0 (- (length body) (length suffix)))
                                                      body))))))
                   (when (or (null body) (string-equal body "End:"))
                     (return-from local-variables-block (values prefix suffix (nreverse settings) start)))
                   (let ((pair (setting-pair body)))
                     (when pair (push (list* start end pair) settings)))))
        (values prefix suffix (nreverse settings) (length text))))))

(defun end-block-settings (text)
  "The settings of the Local Variables: block in TEXT, the end of a file
\(LOCAL-VARIABLES-BLOCK), each (NAME . VALUE)."
  (loop for (nil nil name . value) in (nth-value 2 (local-variables-block text))
        collect (cons name (file-local-value value))))

(defun file-tail (buffer)
  "The end of BUFFER's text, where its Local Variables block is looked for:
its last *FILE-LOCAL-TAIL* characters, and the octet they begin at, as two
values."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (size (tuatara.text:rope-octet-length rope))
         (from (tuatara.text:rope-char-start rope (max 0 (- size (* 4 *file-local-tail*)))))
         (text (tuatara.text:rope-string rope :start from :end size))
         (cut (max 0 (- (length text) *file-local-tail*))))
    (values (subseq text cut) (+ from (tuatara.text:string-octet-length text :end cut)))))

(defun file-local-settings (buffer)
  "The settings BUFFER's file names for itself (FIRST-LINE-SETTINGS, then
END-BLOCK-SETTINGS), each (NAME . VALUE)."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (head (tuatara.text:rope-string rope :start 0 :end (tuatara.text:rope-char-start rope (min (tuatara.text:rope-octet-length rope) 1024))))
         (lines (uiop:split-string head :separator '(#\Newline)))
         (first (if (uiop:string-prefix-p "#!" (first lines)) (second lines) (first lines))))
    (append (and first (first-line-settings first))
            (end-block-settings (file-tail buffer)))))

(defun file-mode-p (mode)
  "True when MODE, a major mode's name, is one a file may name for itself: one
chosen for some files by their names (an extension, a whole name, a #! line's
program, a path), or fundamental-mode; never a list or help of Tuatara's, nor
a mode that runs what is typed (the Lisp prompt, *scratch*'s Lisp)."
  (or (eq mode 'fundamental-mode)
      (multiple-value-bind (extensions names interpreters paths) (mode-file-rules mode)
        (or extensions names interpreters paths))))

(defparameter *file-local-settings*
  '(("mode" :mode "the major mode, by name (org, markdown, fundamental): one that some files get by their names")
    ("tab-width" (:whole 1 64) "how many columns a tab fills: a whole number from 1 to 64")
    ("indent-tabs-mode" :boolean "t indents with tabs, nil with spaces")
    ("fill-column" (:whole 1 1000) "the column text is filled to: a whole number from 1 to 1000")
    ("sentence-end-double-space" :boolean "t for two spaces after a sentence, nil for one"))
  "The settings a file may give itself: each its name, the kind of value it
takes (:MODE, :BOOLEAN or (:WHOLE LOW HIGH)) and what it sets.")

(defun apply-file-local-setting (buffer name value)
  "Give BUFFER the setting NAME, with VALUE, that its file names for itself,
when it is one of *FILE-LOCAL-SETTINGS* and VALUE suits it; true when it was
taken."
  (let* ((entry (assoc name *file-local-settings* :test #'string=))
         (kind (second entry)))
    (cond ((null entry) nil)
          ((eq kind :mode)
           (let ((mode (and (stringp value)
                            (ignoring-errors-reported ("finding the mode ~a" value)
                              (mode-named (make-symbol (string-upcase value)))))))
             (when (and mode (file-mode-p mode))
               (set-major-mode buffer mode)
               t)))
          ((or (eq kind :boolean) (eq (first kind) :whole))
           (when (if (eq kind :boolean)
                     (member value '(t nil))
                     (and (integerp value) (<= (second kind) value (third kind))))
             (setf (tuatara.buffer:buffer-local buffer (intern (string-upcase name) :keyword)) value)
             t)))))

(defun file-declares (buffer name)
  "Whether BUFFER's file names the setting NAME for itself, as two values: its
value, and true when it does (taken as it was opened, or written since by
SET-FILE-LOCAL-SETTING)."
  (let ((pair (assoc name (tuatara.buffer:buffer-local buffer :file-local-settings) :test #'string=)))
    (values (cdr pair) (and pair t))))

(defun note-file-declares (buffer name value)
  "Remember that BUFFER's file names the setting NAME as VALUE (FILE-DECLARES)."
  (setf (tuatara.buffer:buffer-local buffer :file-local-settings)
        (acons name value (remove name
                                  (tuatara.buffer:buffer-local buffer :file-local-settings)
                                  :key #'car
                                  :test #'string=))))

(defun take-file-local-settings (buffer)
  "As BUFFER's file is opened, its mode set by its name by then: give BUFFER
the settings the file names for itself that Tuatara or a package takes, the
mode first, and say which of them are left alone; then call
*FILE-LOCAL-TAKEN-FUNCTIONS*. Never for a file opened safely."
  (when (and *file-local-variables* (tuatara.buffer:buffer-file buffer) (not (tuatara.io:safe-visit-p buffer)))
    (let* ((settings (file-local-settings buffer))
           (ordered (append (remove "mode" settings :key #'car :test-not #'string=)
                            (remove "mode" settings :key #'car :test #'string=)))
           (left (loop for (name . value) in ordered
                       if (or (apply-file-local-setting buffer name value)
                              (run-hook-until-success *file-local-setting-functions* buffer name value))
                         do (note-file-declares buffer name value)
                       else unless (member name '("coding" "lexical-binding") :test #'string=)
                              collect name)))
      (run-hook *file-local-taken-functions* buffer)
      (when left
        (message "~a names settings for itself that are left alone: ~{~a~^, ~}"
                 (tuatara.buffer:buffer-name buffer) (remove-duplicates left :test #'string=))))))

(defun install-file-local-variables ()
  "Take a file's own settings as it is opened, after its mode is set by its name."
  (setf tuatara.buffer:*find-file-functions*
        (append (remove 'take-file-local-settings tuatara.buffer:*find-file-functions*) (list 'take-file-local-settings))))

(define-command describe-file-local-settings ()
  "Show how a file names settings for itself, on its first line between -*- and
-*- or in a Local Variables: block near its end: where each may stand, the
settings that are taken and the values they take, and what is left alone and
why. The lines are filled to *HELP-COLUMN*."
  (show-help
   (with-output-to-string (s)
     (write-help-section s "Settings a file gives itself"
                         :intro "A file may say how it is shown and edited, in a line of its own that any
                         editor reading such lines understands. They are read as the file is opened, after its
                         mode is chosen by its name, and only while *FILE-LOCAL-VARIABLES* is t."
                         :example "# -*- mode: org; fill-column: 72 -*-")
     (write-help-section s "On the first line"
                         :intro "Between -*- and -*-, on the first line, or the second when the first begins
                         #!. NAME: VALUE pairs apart by semicolons, or one word alone, which names the mode:
                         -*- org -*-. Only the first 1024 bytes are looked at. The name may be written
                         in any case.")
     (write-help-section s "In a block near the end"
                         :intro (format nil "A line that says Local Variables: with nothing after it but what
                         closes a comment (--> or */), within the last ~d characters; the last such line
                         begins the block. Each line after it has the same text before it (and after it, for
                         a comment with a closer) as that line, and holds NAME: VALUE. The block ends at a line
                         that says End: or does not begin so."
                                        *file-local-tail*)
                         :example "# Local Variables:
# tab-width: 8
# End:")
     (write-help-section s "Settings taken"
                         :rows (loop for (name nil words) in *file-local-settings* append (list name words))
                         :width 28)
     (write-help-section s "Values"
                         :intro "A whole number (up to nine digits), t, nil, or else the text as written. A
                         value is only ever read as text or a number; nothing a file says is run.")
     (write-help-section s "What is left alone"
                         :intro "Any other name, eval and the rest, is left alone and the echo area names it,
                         except coding and lexical-binding, which are passed over without a word. A value that
                         does not suit its setting (tab-width: 0, a mode no file takes by name, such as a list
                         or the Lisp prompt) is left alone too. A file opened safely is never read for
                         settings, and a package may take more names (*FILE-LOCAL-SETTING-FUNCTIONS*), given the
                         value as a number, t, nil or text."))))

(defun file-local-text (value)
  "VALUE as a file's own setting writes it: t, nil, a number, or the text."
  (cond ((eq value t) "t") ((null value) "nil") (t (princ-to-string value))))

(defun file-local-marks (buffer)
  "What each line of a Local Variables block in BUFFER begins and ends with,
as two values: its mode's comment marker and a space (\"# \" in Org), or a
block comment's opener and closer (<!-- and -->), or nothing."
  (let ((mode (buffer-major-mode buffer)))
    (multiple-value-bind (marker commenting) (mode-comment-syntax mode)
      (if marker
          (values (format nil "~a " (or commenting marker)) "")
          (multiple-value-bind (opener closer) (mode-block-comment-syntax mode)
            (if opener
                (values (format nil "~a " opener) (format nil " ~a" closer))
                (values "" "")))))))

(defun set-file-local-setting (buffer name value)
  "Write NAME: VALUE into the Local Variables block at the end of BUFFER's
text, as one change, the buffer left modified and not saved; the line naming
NAME changed, else one added before End:, else a block added at the end, each
line written as a comment of the buffer's language (FILE-LOCAL-MARKS) and
ended as the buffer's lines are. BUFFER's file then declares it
\(FILE-DECLARES)."
  (let* ((size (tuatara.buffer:buffer-size buffer))
         (newline (tuatara.buffer:buffer-newline buffer))
         (setting (format nil "~a: ~a" name (file-local-text value))))
    (multiple-value-bind (tail from) (file-tail buffer)
      (multiple-value-bind (prefix suffix settings stop) (local-variables-block tail)
        (flet ((at (index) (+ from (tuatara.text:string-octet-length tail :end index)))
               (edit (start end &rest pieces)
                 (tuatara.buffer:buffer-edit buffer (vector (tuatara.text:make-edit start end (tuatara.text:string-octets (format nil "~{~a~}" pieces)))))))
          (let ((line (find name settings :key #'third :test #'string-equal)))
            (cond (line (edit (at (first line)) (at (second line)) prefix setting suffix))
                  ((null prefix)
                   (multiple-value-bind (prefix suffix) (file-local-marks buffer)
                     (edit size size
                           (if (and (plusp size) (/= (tuatara.text:rope-octet (tuatara.buffer:buffer-text buffer) (1- size)) 10)) newline "")
                           newline prefix "Local Variables:" suffix newline prefix setting suffix newline prefix "End:" suffix newline)))
                  ;; a line of its own, after the last line when that has no line end
                  ((= stop (length tail))
                   (edit size size (if (and (plusp (length tail)) (char/= (char tail (1- (length tail))) #\Newline)) newline "")
                         prefix setting suffix newline prefix "End:" suffix newline))
                  (t (edit (at stop) (at stop) prefix setting suffix newline)))))))
    (note-file-declares buffer name value)))
