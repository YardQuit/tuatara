;;;; lisp/files/formatted.lisp --- Files with formatted names: find-file-formatted, open-file-formatted, new-file-formatted.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A formatted name says what a file holds and when it was made:
;;;; 20260927T213446--my-notes-on-the-garden__garden_project.org is the
;;;; identifier (the time it was made), the title, and the keywords, as Denote
;;;; names files, so that the names read the same in Emacs.

(in-package #:tuatara.defaults)

;;; Options

(defvar *formatted-file-directory* "~/"
  "The directory of your files with formatted names:
open-file-formatted lists those in it and its subdirectories,
new-file-formatted makes new ones there, and the keywords in their names are
offered for every new file with a formatted name. Your home directory unless
set: (setf *formatted-file-directory* \"~/notes/\") in the init file.")

(defvar *formatted-file-keywords* '()
  "Keywords offered for a new file with a formatted name, besides the keywords
in the names of the files in *FORMATTED-FILE-DIRECTORY*: none as standard.
Set your own in the init file: (setf *formatted-file-keywords* '(\"project\"
\"garden\")).")

(defvar *formatted-file-sort-keywords* t
  "True to put the keywords of a formatted name in alphabetical order.")

(defvar *formatted-file-prompts* '(:title :subdirectory :keywords :file-type :template)
  "What new-file-formatted asks for a new file, in this order: :title,
:subdirectory (of *FORMATTED-FILE-DIRECTORY*), :keywords, :file-type (one of
*FORMATTED-FILE-TYPES*; else *FORMATTED-FILE-TYPE* is used) and :template (blank,
or one of your note templates' files, asked only when there are any).")

(defvar *formatted-file-extension* ""
  "The end of a new file's name that find-file-formatted offers when it asks
for one (New file EXTENSION): nothing typed there takes it. As \"org\" or
\".org\"; \"\" (as standard) for none offered, when nothing typed gives a name
with no end.")

(defvar *formatted-file-type* :org
  "The kind of a new file when *FORMATTED-FILE-PROMPTS* does not ask, and the
one offered first when it does: a name in *FORMATTED-FILE-TYPES*.")

(defparameter *formatted-file-org-header*
  "#+title: {title}
#+date: {date}
#+filetags: {keywords}
#+identifier: {identifier}

#+options: toc:t tags:nil
#+export_exclude_tags: noexport
#+export_select_tags: export
#+property: header-args :mkdirp yes

"
  "The header of a new Org file with a formatted name, as standard.")

(defparameter *formatted-file-text-header*
  "title:      {title}
date:       {date}
tags:       {keywords}
identifier: {identifier}
---------------------------

"
  "The header of a new plain text file with a formatted name, as standard.")

(defparameter *formatted-file-markdown-header*
  "---
title:      {title}
date:       {date}
tags:       {keywords}
identifier: {identifier}
---

"
  "The header of a new Markdown file with a formatted name, as standard.")

(defvar *formatted-file-types*
  (list (list :org ".org" :org *formatted-file-org-header*)
        (list :text ".txt" :text *formatted-file-text-header*)
        (list :markdown ".md" :markdown *formatted-file-markdown-header*)
        (list :org-encrypted ".org.gpg" :org *formatted-file-org-header*)
        (list :text-encrypted ".txt.gpg" :text *formatted-file-text-header*)
        (list :markdown-encrypted ".md.gpg" :markdown *formatted-file-markdown-header*))
  "The kinds of files with formatted names, each (NAME EXTENSION STYLE HEADER):
the end of the file's name, how its header writes the date and keywords, and
the header a new file begins with. In HEADER, {title}, {date}, {keywords} and
{identifier} stand for the file's own. STYLE :org writes the date [2026-09-27
Sun 21:34] and the keywords :garden:project:; :text 2026-09-27 and garden
project; :markdown 2026-09-27T21:34:46+02:00, [\"garden\", \"project\"],
and the title and identifier in quotes. The kinds ending in .gpg are
encrypted when saved, their text never written as it is; a name ending in
.age is refused. SET-FORMATTED-FILE-HEADER changes a header.")

(defun tuatara-user:set-formatted-file-header (kind header)
  "Make HEADER the header new files of KIND begin with, KIND a name in
*FORMATTED-FILE-TYPES* (:org, :text, :markdown). In HEADER,
{title}, {date}, {keywords} and {identifier} stand for the file's own:
(set-formatted-file-header :org (format nil \"#+title: {title}~%#+date: {date}~2%\"))."
  (let ((type (assoc kind *formatted-file-types*)))
    (unless type
      (error "~s is no kind of file; use one of ~{~(~s~)~^ ~}." kind (mapcar #'first *formatted-file-types*)))
    (setf (fourth type) header)
    kind))

(defvar *note-templates-directory* nil
  "The folder of your own templates for new notes (NOTE-TEMPLATES-DIRECTORY);
NIL, as standard, for note-templates/ in the configuration directory.")

(defun note-templates-directory ()
  "Where your own templates for new notes are: *NOTE-TEMPLATES-DIRECTORY*, or
note-templates/ in the configuration directory (~/.config/tuatara/note-templates/),
each a file NAME.org (or another ending) whose text a new note begins with
after its header, offered by NAME. In it %date% is today's date (2026-10-05)
and %author% your name."
  (or *note-templates-directory* (config-path "note-templates/")))

(defun formatted-templates ()
  "The templates a new note can begin with: blank, nothing after its header,
as (\"blank\" . \"\"), then each file in NOTE-TEMPLATES-DIRECTORY as (NAME
:FILE PATH), NAME its name without its ending, sorted."
  (append (list (cons "blank" ""))
          (sort (loop for (name kind) in (tuatara.io:directory-entries (note-templates-directory))
                      when (and (eq kind :regular) (own-file-name-p name))
                        collect (list (subseq name 0 (or (position #\. name :from-end t) (length name)))
                                      :file (concatenate 'string (note-templates-directory) name)))
                #'string< :key #'first)))

(defun formatted-template-text (template)
  "The text TEMPLATE, an entry of FORMATTED-TEMPLATES, begins a note with:
its own text, or its file's with %date% today's date and %author% your name."
  (let ((value (cdr template)))
    (if (and (consp value) (eq (first value) :file))
        (let* ((text (handler-case (read-file-text (second value))
                       (error (e) (message "The template ~a could not be read: ~a" (first template) e) "")))
               (text (cl-ppcre:regex-replace-all "%date%" text (format-time "%Y-%m-%d" (get-universal-time)))))
          (cl-ppcre:regex-replace-all "%author%" text (lambda (match) (declare (ignore match)) (user-full-name)) :simple-calls t))
        (or value ""))))

;;; Names

(defparameter *title-slug-dropped* "[]{}!@#$%^&*()+'\"?,.|;:~`‘’“”/="
  "Characters left out of the title part of a formatted name.")

(defparameter *keyword-slug-dropped* "[]{}!@#$%^&*()+'\"?,.|;:~`‘’“”/_ =-"
  "Characters left out of a keyword in a formatted name: its words are joined.")

(defun title-slug (title)
  "TITLE as the title part of a formatted name: in lower case, punctuation left
out, spaces and underscores made single hyphens, none at either end."
  (let ((out (make-string-output-stream)) (pending nil) (started nil))
    (loop for char across title
          do (cond ((find char *title-slug-dropped*))
                   ((or (char= char #\Space) (char= char #\_) (char= char #\-))
                    (when started (setf pending t)))
                   (t (when pending (write-char #\- out) (setf pending nil))
                      (write-char (char-downcase char) out)
                      (setf started t))))
    (get-output-stream-string out)))

(defun keyword-slug (keyword)
  "KEYWORD as a keyword of a formatted name: in lower case, its words joined,
punctuation left out."
  (string-downcase (remove-if (lambda (char) (find char *keyword-slug-dropped*)) keyword)))

(defun name-keywords (keywords)
  "KEYWORDS (strings) as a formatted name has them: made slugs, empty ones and
repeats dropped, sorted when *FORMATTED-FILE-SORT-KEYWORDS* says so."
  (let ((slugs (remove-duplicates (remove "" (mapcar #'keyword-slug keywords) :test #'string=)
                                  :test #'string= :from-end t)))
    (if *formatted-file-sort-keywords* (sort slugs #'string<) slugs)))

(defun time-identifier (time)
  "The identifier of a file made at universal TIME: 20260927T213446."
  (format-time "%Y%m%dT%H%M%S" time))

(defun identifier-time (identifier)
  "The universal time the IDENTIFIER of a formatted name (20260927T213446)
says, in local time; NIL when it says no time there is."
  (ignore-errors
   (flet ((part (start end) (parse-integer identifier :start start :end end)))
     (let ((year (part 0 4)) (month (part 4 6)) (day (part 6 8))
           (hour (part 9 11)) (minute (part 11 13)) (second (part 13 15)))
       (and (valid-date-p year month day) (<= 0 hour 23) (<= 0 minute 59) (<= 0 second 59)
            (day-universal-time (day-number year month day) hour minute second))))))

(defun formatted-file-name (identifier title keywords extension)
  "The formatted name of a file: IDENTIFIER, then -- and TITLE's slug, then __
and the KEYWORDS joined by _, then EXTENSION (with its dot), each part left out
when empty: 20260927T213446--my-notes__garden_project.org."
  (let ((title (title-slug title))
        (keywords (name-keywords keywords)))
    (format nil "~a~:[--~a~;~*~]~:[__~{~a~^_~}~;~*~]~a"
            identifier (zerop (length title)) title (null keywords) keywords extension)))

(defun name-extension (name)
  "The end of file NAME that says its kind: .org, or .org.gpg for an encrypted
one; an empty string when it has none."
  (let ((dot (position #\. name :from-end t)))
    (cond ((or (null dot) (zerop dot)) "")
          ((member (subseq name dot) '(".gpg" ".age") :test #'string=)
           (let ((inner (position #\. name :from-end t :end dot)))
             (if (and inner (plusp inner)) (subseq name inner) (subseq name dot))))
          (t (subseq name dot)))))

(defun encrypted-name-p (name)
  "True when file NAME ends as an encrypted file does: .gpg or .age."
  (let ((extension (name-extension name)))
    (some (lambda (end) (and (>= (length extension) (length end))
                             (string= end extension :start2 (- (length extension) (length end)))))
          '(".gpg" ".age"))))

(defun unencryptable-name-p (name)
  "True when file NAME ends as an encrypted file of a kind Tuatara cannot
encrypt (.age): such a file is not made, its text would be there to read."
  (and (encrypted-name-p name) (not (uiop:string-suffix-p (string-downcase name) ".gpg"))))

(defparameter *encrypted-text-headers*
  '("-----BEGIN PGP MESSAGE-----" "-----BEGIN AGE ENCRYPTED FILE-----" "-----BEGIN ENCRYPTED PRIVATE KEY-----")
  "The lines an encrypted file written as text begins with: gpg --armor, age
--armor, a private key kept under a passphrase (PKCS #8).")

(defparameter *encrypted-signatures*
  '((0 . "age-encryption.org/v1") (0 . "$ANSIBLE_VAULT;") (0 . "Salted__")
    (0 . (#x4c #x55 #x4b #x53 #xba #xbe)) (3 . "-FVE-FS-"))
  "Where in a file, and as what octets, encrypted files of other kinds say what
they are: age's own header, Ansible's vault, openssl enc, a LUKS volume (its
header, LUKS and BA BE), a BitLocker volume (-FVE-FS- after the jump). A
VeraCrypt or TrueCrypt volume says nothing, its header looking like chance
octets, as it is made to; its zero octets keep its text from being read.")

(defun encrypted-head-p (head)
  "True when HEAD, the first octets of a file (or its text, as characters),
begins as an encrypted file does: a line of *ENCRYPTED-TEXT-HEADERS* (after
blank space), or a key written as text with Proc-Type: 4,ENCRYPTED; one of
*ENCRYPTED-SIGNATURES* where it belongs; or the first packet of a binary
OpenPGP message, its tag and version as gpg writes them
\(TUATARA.IO:OPENPGP-PACKET-P)."
  (flet ((octet (i) (and (< i (length head))
                         (let ((x (elt head i))) (if (characterp x) (char-code x) x)))))
    (let* ((start (position-if-not (lambda (x) (member (if (characterp x) (char-code x) x) tuatara.text:*blank-octets*)) head))
           (text (map 'string (lambda (x) (if (characterp x) x (code-char x)))
                      (subseq head (or start 0) (min (length head) (+ (or start 0) 40))))))
      (or (some (lambda (header) (uiop:string-prefix-p header text)) *encrypted-text-headers*)
          (and (uiop:string-prefix-p "-----BEGIN " text)
               (search "Proc-Type: 4,ENCRYPTED"
                       (map 'string (lambda (x) (if (characterp x) x (code-char x))) (subseq head 0 (min (length head) 200)))))
          (loop for (at . signature) in *encrypted-signatures*
                thereis (let ((codes (if (stringp signature) (map 'list #'char-code signature) signature)))
                          (loop for code in codes
                                for i from at
                                always (eql (octet i) code))))
          (tuatara.io:openpgp-packet-p
           (if (stringp head)
               ;; text: its characters taken as octets, those past one octet as none
               (map '(vector (unsigned-byte 8)) (lambda (c) (if (< (char-code c) 256) (char-code c) 0))
                    (subseq head 0 (min (length head) 8)))
               head))))))

(defun encrypted-file-p (path &optional text)
  "True when the file at PATH is encrypted: as its name says (ENCRYPTED-NAME-P:
.gpg, .age), as its header on disk shows (ENCRYPTED-HEAD-P), or as TEXT, the
text a buffer shows of it, when given, begins. The text of such a file is
never read for searching, for a directory kept read or for a dynamic block,
even while a buffer shows it decrypted."
  (or (encrypted-name-p (tuatara.io:native-file-name path))
      (let ((head (tuatara.io:file-head path 4096))) (and head (encrypted-head-p head)))
      (and text (encrypted-head-p (subseq text 0 (min (length text) 200))))))

(defun identifier-at-start-p (name)
  "True when NAME begins with an identifier: eight digits, T, six digits."
  (and (>= (length name) 15)
       (every #'digit-char-p (subseq name 0 8))
       (char= (char name 8) #\T)
       (every #'digit-char-p (subseq name 9 15))))

(defun name-keyword-list (name)
  "The keywords in the formatted file NAME: after __, up to the extension."
  (let ((start (search "__" name)))
    (when start
      (let* ((from (+ start 2))
             (end (or (position #\. name :start from) (length name))))
        (tuatara.text:split-words (subseq name from end) "_")))))

(defparameter *name-part-markers* '("==" "--" "__" "@@")
  "What begins each part of a formatted name after its identifier: == the
signature, -- the title, __ the keywords, @@ an identifier not at the start.")

(defun name-parts (name)
  "The parts of the formatted file NAME, as four values: its identifier, its
signature, its title and its keywords (a list), NIL for those it has not:
20260503T131940==5=2--my-title__garden_project.org has all four."
  (let* ((base (file-name-base-of name))
         (end (length base))
         (identifier nil) (signature nil) (title nil) (keywords nil)
         (pos 0))
    (when (identifier-at-start-p base)
      (setf identifier (subseq base 0 15) pos 15))
    (loop while (< (1+ pos) end)
          do (let ((marker (subseq base pos (+ pos 2))))
               (unless (member marker *name-part-markers* :test #'string=) (return))
               (let* ((next (loop for i from (+ pos 2) below (1- end)
                                  when (member (subseq base i (+ i 2)) *name-part-markers* :test #'string=)
                                    return i
                                  finally (return end)))
                      (text (subseq base (+ pos 2) next)))
                 (cond ((string= marker "==") (setf signature text))
                       ((string= marker "--") (setf title text))
                       ((string= marker "__") (setf keywords (tuatara.text:split-words text "_")))
                       (t (setf identifier text)))
                 (setf pos next))))
    (values identifier signature title keywords)))

(defun identifier-date (identifier)
  "The date an IDENTIFIER (20260927T213446) stands for, as 2026-09-27; the
identifier itself when it is not one."
  (if (and identifier (identifier-at-start-p identifier))
      (format nil "~a-~a-~a" (subseq identifier 0 4) (subseq identifier 4 6) (subseq identifier 6 8))
      (or identifier "")))

(defun formatted-file-type-for (extension)
  "The kind of file in *FORMATTED-FILE-TYPES* whose names end with EXTENSION, or NIL."
  (find extension *formatted-file-types* :key #'second :test #'string-equal))

;;; Headers

(defun header-date (style time)
  "Universal TIME as a header of STYLE writes it: [2026-09-27 Sun 21:34] in
Org, 2026-09-27 in text, 2026-09-27T21:34:46+13:00 in Markdown."
  (ecase style
    (:org (org-timestamp-text time))
    (:text (format-time "%F" time))
    (:markdown (format-time "%FT%T%:z" time))))

(defun header-keywords (style keywords)
  "KEYWORDS (slugs) as a header of STYLE writes them."
  (ecase style
    (:org (if keywords (format nil ":~{~a:~}" keywords) ""))
    (:text (format nil "~{~a~^  ~}" keywords))
    (:markdown (format nil "[~{~s~^, ~}]" keywords))))

(defun fill-header (header values)
  "HEADER with each {name} in VALUES, an alist of (NAME . TEXT), replaced."
  (let ((out (make-string-output-stream)) (i 0))
    (loop
      (let ((open (search "{" header :start2 i)))
        (unless open (write-string header out :start i) (return))
        (let* ((close (position #\} header :start open))
               (value (and close (cdr (assoc (subseq header (1+ open) close) values :test #'string=)))))
          (cond (value (write-string header out :start i :end open)
                       (write-string value out)
                       (setf i (1+ close)))
                (t (write-string header out :start i :end (1+ open))
                   (setf i (1+ open)))))))
    (get-output-stream-string out)))

(defun formatted-file-header-text (type title keywords identifier time)
  "The header a new file of TYPE (an entry of *FORMATTED-FILE-TYPES*) begins with."
  (destructuring-bind (name extension style header) type
    (declare (ignore name extension))
    (let ((quoted (eq style :markdown)))
      (fill-header header
                   (list (cons "title" (if quoted (format nil "~s" title) title))
                         (cons "date" (header-date style time))
                         (cons "keywords" (header-keywords style (name-keywords keywords)))
                         (cons "identifier" (if quoted (format nil "~s" identifier) identifier)))))))

;;; Reading a note's front matter

(defun front-matter (head style)
  "The title and the tags HEAD, the start of a note, gives in its front
matter, the lines at its top before a blank line or the body, as two values:
in Org (STYLE :org) its #+title: and #+filetags: lines among its keyword
lines (comments among them too); in Markdown (:markdown) or text (:text) its
title: and tags: lines (title = and tags = in Markdown's TOML), Markdown's
between lines of --- or +++. A title in double quotes is read as written in
them (\" a quote); the tags a list of words, as :a:b: in Org, [\"a\", \"b\"]
in Markdown, a  b in text. NIL for a title it has not, or an empty one, and
for no tags. Only the first 4096 characters of HEAD are looked at."
  (let ((lines (uiop:split-string (subseq head 0 (min (length head) 4096)) :separator '(#\Newline)))
        (title nil) (tags nil))
    (when (and lines (member (string-right-trim '(#\Return #\Space) (first lines)) '("---" "+++") :test #'string=))
      (pop lines))
    (flet ((unquoted (value)
             (if (and (>= (length value) 2) (char= (char value 0) #\") (char= (char value (1- (length value))) #\"))
                 (cl-ppcre:regex-replace-all "\\\\([\"\\\\])" (subseq value 1 (1- (length value))) "\\1")
                 value)))
      (dolist (line lines)
        (let ((line (string-right-trim '(#\Return #\Space #\Tab) line)))
          (multiple-value-bind (key value)
              (multiple-value-bind (match groups)
                  (cl-ppcre:scan-to-strings (if (eq style :org) "^#\\+(\\w+):(.*)$" "^(\\w+)\\s*[:=](.*)$") line)
                (if match (values (aref groups 0) (string-trim '(#\Space #\Tab) (aref groups 1))) (values nil nil)))
            (cond ((and key (string-equal key "title") (null title))
                   (setf title (if (eq style :org) value (unquoted value))))
                  ((and key (string-equal key (if (eq style :org) "filetags" "tags")) (null tags))
                   (setf tags (tuatara.text:split-words (string-trim "[]" value)
                                                        (if (eq style :org) '(#\: #\Space #\Tab) '(#\, #\" #\' #\Space #\Tab)))))
                  (key)
                  ;; a comment among Org's keyword lines
                  ((and (eq style :org) (uiop:string-prefix-p "#" line)))
                  (t (return)))))))
    (values (and title (plusp (length title)) title) tags)))

(defun note-front-matter (path)
  "The title and the tags the note at PATH gives in its front matter
\(FRONT-MATTER), as two values, read from its first lines as
TUATARA.IO:FILE-HEAD reads them, untouched; NIL for a file of no kind notes
are kept in (*FORMATTED-FILE-TYPES*) or one that cannot be read."
  (let* ((type (formatted-file-type-for (name-extension (tuatara.io:native-file-name path))))
         (octets (and type (tuatara.io:file-head path 4096))))
    (and octets (front-matter (tuatara.text:lenient-string octets) (third type)))))

;;; Found on a thread of its own while a question is open

(defstruct (gathering (:constructor make-gathering ()) (:copier nil))
  "What a thread of its own finds while a question waits for it (a list of
files, the text of files to search): FOUND, what it gave since it was last
taken, the newest first; COUNT, how many it gave in all; DONE once it has
finished; CANCEL, a cons whose car, turned true, stops it; LOCK, held while
FOUND changes; WOKEN, when it last woke the editor; FINISHED, a semaphore
signalled once it has finished (WAIT-FOR-GATHERING)."
  (lock (sb-thread:make-mutex :name "gathering"))
  (found '()) (count 0) (done nil) (cancel (list nil)) (woken 0)
  (finished (sb-thread:make-semaphore :name "gathering finished")))

(setf (documentation 'gathering-p 'function) "True when OBJECT is a GATHERING."
      (documentation 'gathering-found 'function) "What a GATHERING's thread gave since it was last taken, newest first."
      (documentation 'gathering-count 'function) "How many things a GATHERING's thread gave in all."
      (documentation 'gathering-done 'function) "True once a GATHERING's thread has finished."
      (documentation 'gathering-cancel 'function) "The cons whose car, turned true, stops a GATHERING's thread."
      (documentation 'gathering-lock 'function) "The lock held while a GATHERING's FOUND changes."
      (documentation 'gathering-woken 'function) "When a GATHERING's thread last woke the editor, in internal time."
      (documentation 'gathering-finished 'function) "The semaphore signalled once a GATHERING's thread has finished.")

(defun start-gathering (name work)
  "Run WORK on a thread named NAME, called with a function that takes each
thing found and the cancel cons (car true: stop). Returns the GATHERING, from
which TAKE-GATHERED takes what was found, on the editor's thread; the editor
is woken as things are found, every twentieth of a second at most, and when
the work ends."
  (let ((gathering (make-gathering))
        (editor *editor*))
    (flet ((wake ()
             (setf (gathering-woken gathering) (get-internal-real-time))
             (when editor (wake-editor editor))))
      (tuatara.runtime:make-guarded-thread
       (lambda ()
         (unwind-protect
              (funcall work
                       (lambda (thing)
                         (sb-thread:with-mutex ((gathering-lock gathering))
                           (push thing (gathering-found gathering))
                           (incf (gathering-count gathering)))
                         (when (> (- (get-internal-real-time) (gathering-woken gathering))
                                  (floor internal-time-units-per-second 20))
                           (wake)))
                       (gathering-cancel gathering))
           (setf (gathering-done gathering) t)
           (sb-thread:signal-semaphore (gathering-finished gathering))
           (wake)))
       :name name))
    gathering))

(defun wait-for-gathering (gathering seconds)
  "Wait until GATHERING's thread has finished, SECONDS at most, asleep
meanwhile; true when it has."
  (or (gathering-done gathering)
      (and (sb-thread:wait-on-semaphore (gathering-finished gathering) :timeout seconds) t)))

(defun take-gathered (gathering)
  "What GATHERING's thread found since this was last asked, in the order it
was found, and as a second value true once it has finished, all taken."
  (let ((done (gathering-done gathering)))
    (values (reverse (sb-thread:with-mutex ((gathering-lock gathering))
                       (shiftf (gathering-found gathering) '())))
            done)))

(defun stop-gathering (gathering)
  "Stop GATHERING's thread at its next step: what it found is no longer wanted."
  (setf (car (gathering-cancel gathering)) t))

(defun gathering-source (gathering absorb)
  "A SOURCE for COMPLETING-READ (see there) taking in what GATHERING finds
while the question is open. ABSORB is called on the editor's thread with the
things found since, in the order found (none, at times), and returns how many
there are to choose from now, which the prompt tells while more come."
  (lambda ()
    (multiple-value-bind (new done) (take-gathered gathering)
      (values (and new t) (not done) (funcall absorb new)))))

(defun formatted-file-group (relative)
  "The group a file with a formatted name is listed under among them: the
subdirectory it is in, else the end of its name that says its kind (org, md,
org.gpg)."
  (let ((slash (position #\/ relative :from-end t)))
    (if slash
        (subseq relative 0 (1+ slash))
        (let ((extension (name-extension relative)))
          (if (plusp (length extension)) (subseq extension 1) "Other files")))))

(defun formatted-file-candidate (relative stamp &optional (now (get-universal-time)))
  "The completion candidate for the file at RELATIVE (its path from
*FORMATTED-FILE-DIRECTORY*, whose STAMP is known or NIL): matched by that path,
listed under its FORMATTED-FILE-GROUP, shown as three columns, its date, its
title and its signature and keywords, with its size and when it last changed
at the right."
  (multiple-value-bind (identifier signature title keywords) (name-parts (tuatara.io:native-file-name relative))
    (make-candidate relative
                    :group (formatted-file-group relative)
                    :columns (list (list (identifier-date identifier) :list-date)
                                   (list (or title "") nil)
                                   (list (format nil "~@[=~a  ~]~{~a~^ ~}" signature keywords) :completion-formatted-keywords))
                    :annotation (and stamp
                                     (list (list (octets-words (tuatara.io:stamp-size stamp)) :list-size -8)
                                           (list (age-words (tuatara.io:stamp-mtime stamp) now) :list-date -12))))))

(defun formatted-file-entry (relative stamp-of now)
  "The file at RELATIVE, its path from *FORMATTED-FILE-DIRECTORY*, as the
lists of your files with formatted names hold it: (RELATIVE MTIME
CANDIDATE), MTIME when it last changed (0 when unknown), CANDIDATE its
FORMATTED-FILE-CANDIDATE; STAMP-OF, a function, gives what the system says of
the file, NOW the time its age is told from."
  (let ((stamp (funcall stamp-of)))
    (list relative (if stamp (tuatara.io:stamp-mtime stamp) 0) (formatted-file-candidate relative stamp now))))

(defun list-formatted-files (root &key (keep (constantly t)) (entry #'formatted-file-entry))
  "Start listing your files with formatted names in the directory ROOT and
under it, those whose path from ROOT KEEP is true of (a function of it,
called away from the editor's thread), each as ENTRY makes it (a function of
that path, a function giving the file's stamp, and the time now; see
FORMATTED-FILE-ENTRY). Two values: all of them at once, the ones changed last
first, when ROOT is kept read (*INDEXED-DIRECTORIES*); else NIL and a
GATHERING that finds them in the background, nothing read on the editor's
thread."
  (let ((now (get-universal-time)))
    (multiple-value-bind (files indexed) (indexed-files root)
      (if indexed
          (values (loop for (relative . stamp) in files
                        when (and (identifier-at-start-p (tuatara.io:native-file-name relative)) (funcall keep relative))
                          collect (let ((stamp stamp)) (funcall entry relative (lambda () stamp) now)))
                  nil)
          (values nil
                  (start-gathering
                   "tuatara listing formatted files"
                   (lambda (give cancel)
                     (files-under root :cancel cancel
                                       :each (lambda (path relative)
                                               (when (and (identifier-at-start-p (tuatara.io:native-file-name path))
                                                          (funcall keep relative))
                                                 (funcall give (funcall entry relative
                                                                        (lambda () (ignore-errors (tuatara.io:path-stamp path)))
                                                                        now))))))))))))

(defun call-with-formatted-files (function &key (root (directory-name *formatted-file-directory*))
                                                 (keep (constantly t)) (predicate #'>) (key #'second))
  "Call FUNCTION while your files with formatted names in ROOT and under it
are listed (LIST-FORMATTED-FILES), those KEEP is true of, and return what it
returns; the listing stops then. FUNCTION is called with two arguments: a
function giving those listed so far, each (RELATIVE MTIME CANDIDATE), in the
order PREDICATE gives their KEY (the ones changed last first as standard);
and the SOURCE for COMPLETING-READ that takes in those found since in the
background and tells how many there are (all at once when ROOT is kept
read)."
  (multiple-value-bind (kept gathering) (list-formatted-files root :keep keep)
    (let ((entries (stable-sort kept predicate :key key)))
      (unwind-protect
           (funcall function
                    (lambda () entries)
                    (if gathering
                        (gathering-source gathering
                                          (lambda (new)
                                            (when new
                                              (setf entries (merge 'list (copy-list entries) (stable-sort new predicate :key key)
                                                                   predicate :key key)))
                                            (length entries)))
                        (lambda () (values nil nil (length entries)))))
        (when gathering (stop-gathering gathering))))))

(defun read-formatted-file (prompt &key (keep (constantly t)) (predicate #'>) (key #'second) none preview)
  "Ask PROMPT for one of your files with formatted names, those in
*FORMATTED-FILE-DIRECTORY* and its subdirectories whose path from there KEEP
is true of, chosen by any part of the name. They are listed under their
subdirectory or their kind, in the order PREDICATE gives their KEY (the ones
changed last first as standard; see CALL-WITH-FORMATTED-FILES), each as its
date, its title and its keywords, with its size and when it changed at the
right; listed in the background as you type, the list growing as they are
found (at once, when the directory is one of *INDEXED-DIRECTORIES*), the
prompt saying NONE (\"none in\" the directory as standard) when there are
none. \\<minibuffer>\\[minibuffer-done] takes the best match once all are
listed, or at once the one you moved to. With PREVIEW and
*PREVIEW-FORMATTED-FILES*, the file selected is shown meanwhile, opened
safely. Returns the path of the one chosen from that directory, or NIL and
what was typed when it is none of them."
  (let ((root (directory-name *formatted-file-directory*)))
    (call-with-formatted-files
     (lambda (listed source)
       (let ((chosen (call-with-window-preview
                      (lambda (candidate made)
                        (preview-file-buffer (concatenate 'string root (candidate-string candidate)) made))
                      (lambda (show)
                        (completing-read prompt
                                         (lambda (input) (declare (ignore input)) (mapcar #'third (funcall listed)))
                                         :require-match t :history (lambda () (mapcar #'first (funcall listed)))
                                         :separator (code-char 0) :source source
                                         :none (or none (format nil "none in ~a" (abbreviated-path root)))
                                         :preview (and preview *preview-formatted-files* show))))))
         (if (find chosen (funcall listed) :key #'first :test #'equal)
             chosen
             (values nil chosen))))
     :root root :keep keep :predicate predicate :key key)))

;;; Asking

(defun list-formatted-keywords ()
  "Start finding the keywords in the names of your files with formatted names
(LIST-FORMATTED-FILES), for READ-KEYWORDS: two values, the keywords of each
file at once when the directory is kept read, else NIL and the GATHERING
finding them in the background."
  (list-formatted-files (directory-name *formatted-file-directory*)
                        :entry (lambda (relative stamp-of now)
                                 (declare (ignore stamp-of now))
                                 (name-keyword-list (tuatara.io:native-file-name relative)))))

(defun read-keywords (prompt &optional (listing (multiple-value-list (list-formatted-keywords))))
  "Ask for keywords with PROMPT, separated by commas, completing each on the
keywords offered: *FORMATTED-FILE-KEYWORDS*, then those in the names of your
files with formatted names, each once, those found in the background while
the question is open (LISTING, what LIST-FORMATTED-KEYWORDS gives as a list,
when started before). \\<minibuffer>\\[minibuffer-done] takes the
selected one, \\[minibuffer-complete] puts it in, \\[minibuffer-submit-input]
takes what is typed. While no keyword is begun, an empty line comes first,
selected: no keywords, or no more. Returns a list of strings."
  (let ((offered '()) (seen (make-hash-table :test 'equal)))
    (flet ((offer (keywords)
             (dolist (keyword keywords)
               (unless (gethash keyword seen)
                 (setf (gethash keyword seen) t)
                 (push keyword offered)))
             (hash-table-count seen)))
      (offer *formatted-file-keywords*)
      (destructuring-bind (kept gathering) listing
        (mapc #'offer kept)
        (unwind-protect
             (read-several prompt (lambda () (reverse offered))
                           :none "no keywords" :more "no more keywords"
                           :source (and gathering
                                        (gathering-source gathering
                                                          (lambda (new) (dolist (keywords new) (offer keywords)) (hash-table-count seen)))))
          (when gathering (stop-gathering gathering)))))))

(defun read-formatted-extension ()
  "Ask for the end of a new file's name, as free text: org, md, txt or any
other, with a default: nothing typed takes *FORMATTED-FILE-EXTENSION*, which
the prompt names (default org) when there is one, and a lone dot none. Returns
it with a dot before it (typed or not), or \"\" for none."
  (let* ((offered (string-trim " ." (or *formatted-file-extension* "")))
         (typed (string-trim " " (read-string (if (plusp (length offered))
                                                   (format nil "New file EXTENSION (default ~a): " offered)
                                                   "New file EXTENSION: "))))
         (text (if (zerop (length typed)) offered (string-trim "." typed))))
    (if (zerop (length text)) "" (concatenate 'string "." text))))

(defun unique-identifier (directory time)
  "The identifier for a file made in DIRECTORY at universal TIME: that of the
next second when a file there has it already."
  (let ((names (mapcar #'first (tuatara.io:directory-entries directory))))
    (loop for identifier = (time-identifier time)
          while (some (lambda (name) (and (>= (length name) 15) (string= identifier name :end2 15))) names)
          do (incf time)
          finally (return identifier))))

(defun formatted-file-start-text (type title keywords identifier time template)
  "The text a new file with a formatted name begins with: the header of its
TYPE (an entry of *FORMATTED-FILE-TYPES*, or NIL for none) saying TITLE,
KEYWORDS, IDENTIFIER and TIME, then TEMPLATE's text."
  (concatenate 'string (if type (formatted-file-header-text type title keywords identifier time) "") template))

(defun make-formatted-file (directory title keywords extension &key (template "") (time (get-universal-time)) write)
  "Make a new buffer for a file in DIRECTORY with a formatted name from TITLE,
KEYWORDS (strings), the time and EXTENSION, beginning with the header of its
kind (*FORMATTED-FILE-TYPES*) and TEMPLATE, point after them, and show it. Nothing
is written until it is saved. Returns the buffer, or NIL (said) for a name
that says encrypted but is no .gpg name, or one there already. With WRITE,
the file is written at once instead and its path returned, unless its name
says encrypted: that one is made when its buffer is saved."
  (let* ((directory (directory-name directory))
         (identifier (unique-identifier directory time))
         (name (formatted-file-name identifier title keywords extension))
         (path (concatenate 'string directory name))
         (type (formatted-file-type-for extension)))
    (cond ((unencryptable-name-p name)
           (message "~a is not made: only .gpg files can be encrypted" name)
           nil)
          ((tuatara.io:path-stamp path :follow nil)
           (message "~a is there already: not made" (abbreviated-path path))
           nil)
          ((and write (not (encrypted-name-p name)))
           (tuatara.io:write-text-file
            (tuatara.text:string->rope (formatted-file-start-text type title keywords identifier time template))
            path :expected nil)
           path)
          (t
           (let ((buffer (tuatara.io:visit-file path))
                 (text (formatted-file-start-text type title keywords identifier time template)))
             (when (plusp (length text))
               (tuatara.buffer:buffer-insert buffer 0 text)
               (setf (tuatara.buffer:buffer-point buffer) (tuatara.buffer:buffer-size buffer)))
             (show-buffer buffer)
             buffer)))))

(defun file-name-base-of (name)
  "NAME without its extension (NAME-EXTENSION)."
  (subseq name 0 (- (length name) (length (name-extension name)))))

(define-command find-file-formatted ()
  "Visit a file, or make a new one with a formatted name. A name that exists
is visited as find-file visits it. The directory typed, taken as it is (the
line typed is selected while it ends in /), makes a new file there, asking its title, the end of its name
(free text, *FORMATTED-FILE-EXTENSION* the default) and its keywords. A new name asks for a title (the
name typed to begin with), the end of its name when it has none, and keywords,
separated by commas
(\\<minibuffer>\\[minibuffer-complete] completes the one being typed, on those in
*FORMATTED-FILE-KEYWORDS* and in the names of your files), and the file is made in the
directory typed, named from them and the time, with the end of the name typed:
20260927T213446--my-notes-on-the-garden__garden_project.org.
It begins with the header for its kind (*FORMATTED-FILE-TYPES*). A name is
an identifier from the time, -- and the title in lower case with hyphens, __
and the keywords joined by _. Nothing
is written until the file is saved."
  (multiple-value-bind (path new) (read-file-path "Find or create file: " :new-file t :note nil)
    (cond
      (new
       ;; the directory typed: a new file there, all of its name asked
       (let* ((title (read-string "New file TITLE: "))
              (extension (read-formatted-extension))
              (keywords (read-keywords "New file KEYWORDS: ")))
         (make-formatted-file path title keywords extension)))
      (path
       (let ((full (tuatara.io:native (tuatara.io:expand-path path))))
         (if (or (tuatara.io:path-stamp full)
                 (buffer-visiting full))
             (note-visited-path full (visit-path full))
             (multiple-value-bind (directory name) (tuatara.io:path-folder-and-name full)
               (if (unencryptable-name-p name)
                   (message "~a is not made: only .gpg files can be encrypted" name)
                   (let* ((title (read-string "New file TITLE: " :initial (file-name-base-of name)))
                          ;; a name typed without its end: asked
                          (extension (if (plusp (length (name-extension name)))
                                         (name-extension name)
                                         (read-formatted-extension)))
                          (keywords (read-keywords "New file KEYWORDS: ")))
                     (note-chosen-path full)
                     (make-formatted-file directory title keywords extension))))))))))

(defvar *formatted-file-subdirectory-history* '()
  "The directories chosen for new files, the latest first.")

(defun list-formatted-subdirectories ()
  "Start finding the subdirectories of *FORMATTED-FILE-DIRECTORY* (hidden ones
left out) in the background, for READ-FORMATTED-SUBDIRECTORY: the GATHERING
giving each, written short (ABBREVIATED-PATH)."
  (let ((root (directory-name *formatted-file-directory*)))
    (start-gathering
     "tuatara listing subdirectories"
     (lambda (give cancel)
       (files-under root :directories t :cancel cancel
                         :each (lambda (path relative)
                                 (declare (ignore relative))
                                 (funcall give (abbreviated-path path))))))))

(defun read-formatted-subdirectory (&optional (gathering (list-formatted-subdirectories)))
  "Ask for a directory of the files: *FORMATTED-FILE-DIRECTORY* or one of its
subdirectories (hidden ones left out), the one chosen last first; the
subdirectories are found in the background while the question is open (by
GATHERING, from LIST-FORMATTED-SUBDIRECTORIES, when started before), the
prompt saying how many so far."
  (let* ((directories (list (abbreviated-path (directory-name *formatted-file-directory*))))
         (tail directories)
         (count 1))
    (unwind-protect
         (completing-read "Select SUBDIRECTORY: " (lambda (input) (declare (ignore input)) directories)
                          :require-match t :history *formatted-file-subdirectory-history*
                          :separator (code-char 0)
                          :source (gathering-source gathering
                                                    (lambda (new)
                                                      (when new
                                                        (setf (cdr tail) (copy-list new)
                                                              tail (last tail)
                                                              ;; a new first cons: the question sees a new list
                                                              directories (cons (car directories) (cdr directories)))
                                                        (incf count (length new)))
                                                      count)))
      (stop-gathering gathering))))

(defun read-formatted-file-type ()
  "Ask for the kind of a new file, of *FORMATTED-FILE-TYPES* (those that can be
written), *FORMATTED-FILE-TYPE* the default, offered first. Returns the entry."
  (let* ((writable (remove-if (lambda (type) (unencryptable-name-p (concatenate 'string "x" (second type))))
                              *formatted-file-types*))
         (names (mapcar (lambda (type) (string-downcase (symbol-name (first type)))) writable))
         (chosen (completing-read "Select file TYPE: " names :require-match t
                                                              :default (string-downcase (symbol-name *formatted-file-type*)))))
    (find chosen writable :key (lambda (type) (string-downcase (symbol-name (first type)))) :test #'string=)))

(defun read-formatted-template ()
  "The text a new file begins with after its header: asked for among
FORMATTED-TEMPLATES (blank and your note templates' files)
when there are two or more."
  (let ((templates (formatted-templates)))
    (if (rest templates)
        (let ((chosen (completing-read "Note template: " (mapcar #'car templates) :require-match t)))
          (formatted-template-text (assoc chosen templates :test #'string=)))
        (formatted-template-text (first templates)))))

(defparameter *insert-file-limit* (* 16 1024 1024)
  "The most octets \\[insert-note-template] reads from a file that is no note template;
a larger file is refused, and said so.")

(defun read-file-text (path)
  "The text of the file at PATH, read as UTF-8 (an octet that is none as ?),
nothing in it acted on; an error says so when it is larger than
*INSERT-FILE-LIMIT* octets."
  (tuatara.io:file-text path :limit *insert-file-limit*
                             :too-large (lambda (name size)
                                          (declare (ignore size))
                                          (error "~a is larger than ~a (*insert-file-limit*): not read"
                                                 (tuatara.io:native-file-name name) (octets-words *insert-file-limit*)))))

(define-command insert-note-template ()
  "Read a note template, or any file, into the current buffer at point: one
of your templates (blank, and each file in
~/.config/tuatara/note-templates/), %date% and %author% filled in, or with
\"Another file...\" any file, as it is. The template's file is only read,
never opened, so saving the buffer never writes over it. When the buffer has
text already, asked first."
  (let* ((buffer (current-buffer))
         (templates (formatted-templates))
         (other "Another file...")
         (chosen (completing-read "Insert note template: "
                                  (append (mapcar (lambda (tp)
                                                    (make-candidate (first tp) :annotation (if (consp (cdr tp)) "note template" "nothing")))
                                                  templates)
                                          (list (make-candidate other :annotation "any file, as it is")))
                                  :require-match t)))
    (check-writable buffer)
    (let* ((path (and (string= chosen other) (read-file-path "Insert file: ")))
           (text (if path
                     (read-file-text (tuatara.io:native (tuatara.io:expand-path path)))
                     (formatted-template-text (assoc chosen templates :test #'string=))))
           (what (if path (tuatara.io:native-file-name path) chosen)))
      (cond ((zerop (length text)) (message "~a is empty: nothing inserted" what))
            ((and (plusp (tuatara.buffer:buffer-size buffer))
                  (not (y-or-n-p* (format nil "~a has text already: insert ~a at point anyway?" (tuatara.buffer:buffer-name buffer) what))))
             (message "Nothing inserted"))
            (t (let ((at (point)))
                 (tuatara.buffer:buffer-insert buffer at (text-for-buffer text buffer))
                 (goto at)
                 (message "Inserted ~a" what)))))))

(defun ask-formatted-file-answers (&key (title "") skip)
  "Ask what *FORMATTED-FILE-PROMPTS* says, in its order, but the questions
SKIP lists (:subdirectory, say), TITLE offered for the title; the
subdirectories and keywords to offer looked for in the background from the
first question on. Returns the directory (*FORMATTED-FILE-DIRECTORY*, or the
subdirectory chosen), the title, the keywords, the kind of file (an entry of
*FORMATTED-FILE-TYPES*) and the template's text, as five values."
  (let* ((prompts (remove-if (lambda (question) (member question skip)) *formatted-file-prompts*))
         (directory *formatted-file-directory*) (keywords '())
         (type (or (find *formatted-file-type* *formatted-file-types* :key #'first) (first *formatted-file-types*)))
         (template (formatted-template-text (first (formatted-templates))))
         (subdirectories (and (member :subdirectory prompts) (list-formatted-subdirectories)))
         (keyword-listing (and (member :keywords prompts) (multiple-value-list (list-formatted-keywords)))))
    (unwind-protect
         (dolist (question prompts)
           (ecase question
             (:title (setf title (read-string "New file TITLE: " :initial title)))
             (:subdirectory (setf directory (read-formatted-subdirectory subdirectories))
                            (push directory *formatted-file-subdirectory-history*))
             (:keywords (setf keywords (read-keywords "New file KEYWORDS: " keyword-listing)))
             (:file-type (setf type (read-formatted-file-type)))
             (:template (setf template (read-formatted-template)))))
      (when subdirectories (stop-gathering subdirectories))
      (when (second keyword-listing) (stop-gathering (second keyword-listing))))
    (values directory title keywords type template)))

(defun ask-formatted-file (&key (title ""))
  "Ask what *FORMATTED-FILE-PROMPTS* says (ASK-FORMATTED-FILE-ANSWERS, TITLE
offered for the title), and make the file: a new one with a formatted name in
*FORMATTED-FILE-DIRECTORY*."
  (multiple-value-bind (directory title keywords type template) (ask-formatted-file-answers :title title)
    (make-formatted-file directory title keywords (second type) :template template)))

(define-command new-file-formatted ()
  "Make a new file with a formatted name in *FORMATTED-FILE-DIRECTORY*:
asks what *FORMATTED-FILE-PROMPTS* says (as standard the title, the
subdirectory, the keywords, the kind of file and the template), and names it as
find-file-formatted does, with the header for its kind. Nothing is written
until it is saved."
  (ask-formatted-file))

(define-command open-file-formatted ()
  "Open one of your files with formatted names, those in *FORMATTED-FILE-DIRECTORY* and
its subdirectories, choosing among them by any part of the name: listed
under their subdirectory or their kind, the ones changed last first, each as
its date, its title and its keywords, with its size and when it changed at
the right. The files are listed in the background as you type, the list
growing as they are found (at once, when the directory is one of
*indexed-directories*); \\<minibuffer>\\[minibuffer-done] takes the best match
once all are listed, or at once the one you moved to; with
*preview-formatted-files*, the file selected is shown meanwhile, opened
safely. It makes no file:
new-file-formatted makes a new file there, and find-file-formatted a file
with a formatted name anywhere."
  (multiple-value-bind (chosen typed) (read-formatted-file "Open file: " :preview t)
    (if chosen
        (visit-path (concatenate 'string (directory-name *formatted-file-directory*) chosen))
        (message "~a is none of your files with formatted names" typed))))
