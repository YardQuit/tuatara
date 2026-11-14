;;;; test/kept-files.lisp --- The files people keep, read by every later release of a major version.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Beside the stable surface, which is names, the files Tuatara reads in its
;;;; directories are kept too. test/api/kept-files.sexp pins, at each version
;;;; bump, each list kept between sessions (its file's name, its class and the
;;;; version of its format) and the other files of the directories with their
;;;; class: kept (made by you: never lost to a later release), remembered
;;;; (what you did, read by every later release) or rebuilt (may be deleted at
;;;; any time). test/kept-files/VERSION/ holds a file of each list as that
;;;; release wrote it, and every later release must read them all to the same
;;;; entries. tools/pin-kept-files.sh writes the pin again, and the files of
;;;; the version being pinned, never over those there.

(in-package #:tuatara.tests)

(define-test kept-files
  :description "The lists kept between sessions and the other files of Tuatara's directories keep being read, by every later release of a major version: lines of a later release kept as they are, the files of each release read to the same entries.")

(defparameter *kept-files-file* (asdf:system-relative-pathname :tuatara "test/api/kept-files.sexp")
  "The pinned files Tuatara keeps: the version it was written at, then a row for each file.")

(defparameter *kept-file-samples* (asdf:system-relative-pathname :tuatara "test/kept-files/")
  "The directory of the files each release wrote, a directory for each, named for the version.")

(defparameter *kept-directories*
  '((:file "config" "init.lisp" :kept nil)
    (:file "config" "snippets/" :kept nil)
    (:file "config" "capture-templates/" :kept nil)
    (:file "config" "latex-templates/" :kept nil)
    (:file "config" "docx-templates/" :kept nil)
    (:file "config" "note-templates/" :kept nil)
    (:file "config" "logos/" :kept nil)
    (:file "data" "grammars/" :kept nil)
    (:file "state" "auto-save/" :kept nil)
    (:file "state" "session" :remembered 1)
    (:file "state" "session-daemon-NAME" :remembered 1)
    (:file "state" "grammar-store/" :rebuilt nil)
    (:file "data" "packages/" :rebuilt nil)
    (:file "cache" "" :rebuilt nil))
  "The files and directories of Tuatara's directories other than the lists kept
between sessions, each (:FILE DIRECTORY PATH CLASS VERSION): DIRECTORY config,
state, data or cache; PATH within it, a directory's ending in /; CLASS :KEPT,
:REMEMBERED or :REBUILT; VERSION that of its format where the file says one.")

(defparameter *kept-file-entries*
  `((tuatara.defaults::*recent-files*
     ("/notes/report.org" . nil) ("/evidence/disk 1.img" . t) (,(format nil "/odd/a\\b~cc~%d.txt" #\Tab) . nil))
    (tuatara.defaults::*saved-places*
     ("/notes/report.org" . 120) ("/notes/a b.org" . 0))
    (tuatara.defaults::*pdf-places*
     ("/papers/one.pdf" 12 :width 1.25) ("/papers/two three.pdf" 1 :page 1.0))
    (tuatara.defaults::*chosen-paths*
     "/notes/report.org" "/notes/")
    (tuatara.defaults::*bookmarks*
     ("Findings" "/cases/report.org" 120 "* Findings" ("work" "court") t)
     ("Unmapped" "/mnt/server-not-mapped/evidence/disk.img" 0 "" () t)
     ("Plain" "/notes/plain.txt" 7 ,(format nil "text~%after") () nil))
    (tuatara.defaults::*trusted*
     "/projects/one/" "/projects/two three/")
    (tuatara.defaults::*look-only*
     "/cases/2026-0001/seized repo/" "/cases/2026-0002/")
    (tuatara.packages::*package-trust*
     ("https://example.org/signed.git"
      "SHA256:abcDEF0123456789+/="
      "Mere Rotorua"
      "mere@example.org AAAAC3NzaC1lZDI1NTE5AAAAIExample+/=")
     ("https://example.org/tools.git" "unsigned" nil nil))
    (tuatara.packages::*fetched-packages*
     ("example https://example.org/tools.git release" "0123456789abcdef0123456789abcdef01234567" "v1.2.0" t
      "89abcdef0123456789abcdef0123456789abcdef" "v1.1.0" "SHA256:abcDEF0123456789+/=")
     ("mine ~/packages/ latest" "-" nil t nil nil nil)
     ("third https://example.org/third.git latest" "abcdef0" nil nil "1234567" nil nil))
    (tuatara.packages::*removed-packages*
     "example" "mine")
    (tuatara.defaults::*trusted-servers*
     ("me@build.example" t ("github.com" "git@git.example") t t "Dropped" "2026-11-05")
     ("box" nil () nil nil nil "2026-11-04"))
    (tuatara.defaults::*grammar-answers*
     ("yaml" . t) ("nix" . nil))
    (tuatara.defaults::*saved-patterns*
     ("invoice" :regex "INV-[0-9]+" "an invoice number") ("draft" :literal "[draft]" ""))
    (tuatara.defaults::*user-grammar-sums*
     ("mylang" . "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")))
  "For each list kept between sessions (its variable), the entries its sample
files hold, the latest first: the first using every field the list has, the
others leaving out what may be left out.")

(defun kept-file-state-row (list)
  "The pinned row of LIST, a list kept between sessions: (:STATE NAME CLASS
VERSION), and for a list of trust :TRUST T and :FIELDS, how many fields its
lines have, which an older release must know to leave a later line unread."
  (append (list :state (tuatara.defaults::state-list-name list) (tuatara.defaults::state-list-class list)
                (tuatara.defaults::state-list-version list))
          (and (tuatara.defaults::state-list-trust list)
               (list :trust t :fields (tuatara.defaults::state-list-fields list)))))

(defun kept-file-state-rows ()
  "A row for each list kept between sessions (KEPT-FILE-STATE-ROW)."
  (sort (mapcar #'kept-file-state-row tuatara.defaults::*state-lists*) #'string< :key #'second))

(defun kept-files ()
  "The files Tuatara keeps as they are now, a row for each: the lists kept
between sessions, then the other files of its directories (*KEPT-DIRECTORIES*)."
  (append (kept-file-state-rows) *kept-directories*))

(defun kept-file-row-name (row)
  "What names the file ROW of the pin is about: the list's name, or the directory and path."
  (subseq row 0 (if (eq (first row) :state) 2 3)))

(defun kept-class-rank (class)
  "How much is promised of a file of CLASS: more for :KEPT than :REMEMBERED, more for that than :REBUILT."
  (or (position class '(:rebuilt :remembered :kept)) -1))

(defun kept-file-row-parts (row)
  "ROW of the pin of kept files as three values: its class, its version and
what follows them (:TRUST and :FIELDS of a list of trust)."
  (let ((rest (nthcdr (if (eq (first row) :state) 2 3) row)))
    (values (first rest) (second rest) (cddr rest))))

(defun kept-file-breaks (pinned now)
  "The rows of PINNED, files kept, that NOW no longer keeps: a file gone,
promised less (a kept list become remembered), its format's version lowered,
or a list of trust no longer one or its lines' fields changed."
  (loop for row in pinned
        for found = (find (kept-file-row-name row) now :key #'kept-file-row-name :test #'equal)
        unless (and found
                    (multiple-value-bind (class version more) (kept-file-row-parts row)
                      (multiple-value-bind (class-now version-now more-now) (kept-file-row-parts found)
                        (and (>= (kept-class-rank class-now) (kept-class-rank class))
                             (or (null version) (and version-now (>= version-now version)))
                             (or (not (getf more :trust))
                                 (and (getf more-now :trust) (eql (getf more :fields) (getf more-now :fields))))))))
          collect row))

(defun read-kept-files (&optional (file *kept-files-file*))
  "The version FILE was pinned at, and its rows, as two values."
  (with-open-file (in file)
    (let ((head (read in)))
      (values (getf head :version)
              (loop for row = (read in nil in) until (eq row in) collect row)))))

(defun kept-file-entries (list)
  "The entries the sample files of LIST, a list kept between sessions, hold (*KEPT-FILE-ENTRIES*)."
  (rest (find list *kept-file-entries* :key (lambda (row) (symbol-value (first row))))))

(defun write-kept-file-samples (directory)
  "Write into DIRECTORY the file of each list kept between sessions as this
release writes it, holding the entries of *KEPT-FILE-ENTRIES*: those not
there yet, none over a file there. The names of those written."
  (ensure-directories-exist directory)
  (let ((written '()))
    (with-fresh-state
      (loop for (variable . entries) in *kept-file-entries*
            for list = (symbol-value variable)
            for to = (merge-pathnames (tuatara.defaults::state-list-name list) directory)
            unless (probe-file to)
              do (tuatara.defaults::state-list-put list entries)
                 (tuatara.defaults::finish-state-writes)
                 (alexandria:write-string-into-file (file-text (tuatara.defaults::state-list-path list)) to)
                 (push (tuatara.defaults::state-list-name list) written)))
    (nreverse written)))

(defun write-kept-files (&optional (file *kept-files-file*))
  "Pin the files Tuatara keeps as they are now in FILE, with the version it is
pinned at, and write the sample files of that version (WRITE-KEPT-FILE-SAMPLES)."
  (let ((version (asdf:component-version (asdf:find-system :tuatara))))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :supersede)
      (let ((*print-case* :downcase) (*print-right-margin* 100000))
        (format out ";; The files Tuatara keeps, pinned by tools/pin-kept-files.sh at each version bump: (:state NAME CLASS VERSION),~%~
                     ;; a list kept between sessions, with :trust t :fields N after them for a list of trust; (:file DIRECTORY PATH~%~
                     ;; CLASS VERSION), the other files of its directories. CLASS :kept, :remembered or :rebuilt; VERSION that of~%~
                     ;; the format, NIL where the file says none.~%")
        (prin1 (list :version version) out)
        (terpri out)
        (dolist (row (kept-files)) (prin1 row out) (terpri out))))
    (write-kept-file-samples (merge-pathnames (format nil "~a/" version) *kept-file-samples*))))

(defun kept-sample-directories ()
  "The directories of sample files, one for each release that wrote them."
  (directory (merge-pathnames "*/" *kept-file-samples*)))

(defun file-lines-but-comments (text)
  "The lines of TEXT that are not empty and do not begin with #."
  (remove-if (lambda (line) (or (zerop (length line)) (char= (char line 0) #\#)))
             (uiop:split-string text :separator '(#\Newline))))

(defun put-state-file (list text &key former)
  "Make TEXT the file of LIST, a list kept between sessions, in the state directory now in force;
with FORMER, under the name its file had before (STATE-LIST-FORMERLY)."
  (ensure-directories-exist (tuatara.io:state-directory))
  (tuatara.io:write-private-file (if former
                                     (tuatara.io:native (merge-pathnames (tuatara.defaults::state-list-formerly list)
                                                                         (tuatara.io:state-directory)))
                                     (tuatara.defaults::state-list-path list))
                                 text))

(defun list-sample-file (list directory)
  "The sample file of LIST, a list kept between sessions, in DIRECTORY, under
its name or the one it had before, and as a second value whether it is the
one it had before; NIL when there is neither."
  (let ((own (merge-pathnames (tuatara.defaults::state-list-name list) directory))
        (formerly (tuatara.defaults::state-list-formerly list)))
    (cond ((probe-file own) (values own nil))
          ((and formerly (probe-file (merge-pathnames formerly directory)))
           (values (merge-pathnames formerly directory) t)))))

(define-test (kept-files samples-read)
  ;; each release's files read by this one to the same entries, and written back with their
  ;; lines as they were, under the line saying the version; a file of a list renamed since, under
  ;; the name it had then, is moved over to its name now
  (let ((directories (kept-sample-directories)))
    (true directories "there are sample files")
    (dolist (directory directories)
      (dolist (list tuatara.defaults::*state-lists*)
        (multiple-value-bind (sample former) (list-sample-file list directory)
          (when sample
            (with-fresh-state
              (put-state-file list (file-text sample) :former former)
              (let ((text (file-text sample))
                    (path (tuatara.defaults::state-list-path list))
                    (where (format nil
                                   "~a of ~a"
                                   (tuatara.defaults::state-list-name list)
                                   (car (last (pathname-directory directory))))))
                (is equal (kept-file-entries list) (tuatara.defaults::read-state-file list path) "~a read" where)
                (is equal (kept-file-entries list) (tuatara.defaults::state-list-entries list) "~a listed" where)
                ;; a change to nothing in it: written back, its lines as they were
                (tuatara.defaults::state-list-drop list '("/nowhere/kept-files"))
                (tuatara.defaults::finish-state-writes)
                (let ((written (file-text path)))
                  (true (uiop:string-prefix-p (format nil
                                                      "# tuatara ~a "
                                                      (tuatara.defaults::state-list-name list)) written)
                        "~a begins with its version" where)
                  (is equal (file-lines-but-comments text) (file-lines-but-comments written) "~a written back" where))
                (is equal (kept-file-entries list) (tuatara.defaults::read-state-file list path) "~a read again" where)))))))
    ;; the session
    (dolist (directory directories)
      (let ((sample (merge-pathnames "session" directory)))
        (when (probe-file sample)
          (with-fresh-state
            (ensure-directories-exist (tuatara.io:state-directory))
            (tuatara.io:write-private-file (tuatara.defaults::session-path) (file-text sample))
            (multiple-value-bind (buffers layout) (tuatara.defaults::read-session)
              (is equal `((:file 12 ,(format nil "/notes/b~cc.org" #\Tab)) (:safe 0 "/evidence/disk.img")
                          (:directory 0 "/notes/") (:scratch 5 "*scratch*"))
                  buffers)
              (is equal '(:split :side-by-side (0.5d0 0.5d0)
                          (:window t 0 (:file 34 "/notes/a b.org"))
                          (:window nil 0 nil))
                  layout))))))))

(define-test (kept-files every-list-pinned)
  ;; no list is kept between sessions without its class decided, pinned, and a sample of its file
  (multiple-value-bind (version pinned) (read-kept-files)
    (true version "the pin says the version it was written at")
    (dolist (list tuatara.defaults::*state-lists*)
      (let ((name (tuatara.defaults::state-list-name list)))
        (true (member (tuatara.defaults::state-list-class list) '(:kept :remembered)) "~a has a class" name)
        (true (find (list :state name) pinned :key #'kept-file-row-name :test #'equal) "~a is pinned" name)
        ;; a list of trust pinned as one, with the fields its lines have
        (multiple-value-bind (class version more)
            (kept-file-row-parts (or (find (list :state name) pinned :key #'kept-file-row-name :test #'equal)
                                     (list :state name)))
          (declare (ignore class version))
          (is eq (and (tuatara.defaults::state-list-trust list) t) (getf more :trust) "~a's trust pinned" name)
          (when (tuatara.defaults::state-list-trust list)
            (is eql (tuatara.defaults::state-list-fields list) (getf more :fields) "~a's fields pinned" name)))
        (true (kept-file-entries list) "~a has sample entries" name)
        (true (some (lambda (directory) (list-sample-file list directory)) (kept-sample-directories))
              "~a has a sample file" name)))
    ;; bookmarks are made by hand: kept
    (is eq :kept (tuatara.defaults::state-list-class tuatara.defaults::*bookmarks*))
    (is eq :kept (third (find '(:state "bookmarks") pinned :key #'kept-file-row-name :test #'equal)))))

(define-test (kept-files files-kept-within-a-major-version)
  ;; a file gone, promised less or its version lowered breaks the pin; one added does not
  (let ((pinned '((:state "bookmarks" :kept 1) (:state "places" :remembered 2) (:state "gone" :remembered 1)
                  (:file "config" "snippets/" :kept nil) (:file "state" "session" :remembered 1)))
        (now '((:state "bookmarks" :remembered 1) (:state "places" :remembered 1) (:state "added" :remembered 1)
               (:file "config" "snippets/" :kept nil) (:file "state" "session" :kept 2))))
    (is equal '(("bookmarks") ("places") ("gone"))
        (mapcar (lambda (row) (rest (kept-file-row-name row))) (kept-file-breaks pinned now))))
  ;; a list of trust stays one, its lines' fields as pinned; a list may become one
  (let ((pinned '((:state "same" :remembered 1 :trust t :fields 4)
                  (:state "more-fields" :remembered 1 :trust t :fields 4)
                  (:state "no-longer" :remembered 1 :trust t :fields 2) (:state "became" :remembered 1)))
        (now '((:state "same" :remembered 2 :trust t :fields 4) (:state "more-fields" :remembered 1 :trust t :fields 5)
               (:state "no-longer" :remembered 1) (:state "became" :remembered 1 :trust t :fields 2))))
    (is equal '(("more-fields") ("no-longer"))
        (mapcar (lambda (row) (rest (kept-file-row-name row))) (kept-file-breaks pinned now))))
  ;; from 1.0.0 on, what the last release pinned is kept until the major version goes up
  (multiple-value-bind (version pinned) (read-kept-files)
    (let ((now-major (version-major (asdf:component-version (asdf:find-system :tuatara)))))
      (when (and (>= now-major 1) (= now-major (version-major version)))
        (let ((breaks (kept-file-breaks pinned (kept-files))))
          (is equal '() breaks "Files no longer kept within ~d.x: ~s" now-major breaks))))))

(defun later-line (list entry)
  "The line a later release writes for ENTRY of LIST: every field of today's,
those left out written empty, then a field of its own."
  (let ((line (funcall (tuatara.defaults::state-list-print list) entry)))
    (format nil "~a~a~clater field" line
            (make-string (- (tuatara.defaults::state-list-fields list) (1+ (count #\Tab line))) :initial-element #\Tab)
            #\Tab)))

(define-test (kept-files later-lines-kept)
  ;; a file a later release wrote: its fields added at the ends of lines passed over, its lines
  ;; of a new kind, its comment lines and its version line kept as they are when this release
  ;; writes it; in a list of what you trusted, a later version's line with more fields is not read
  (dolist (row *kept-file-entries*)
    (let* ((list (symbol-value (first row)))
           (name (tuatara.defaults::state-list-name list))
           (entries (rest row))
           (longer (mapcar (lambda (entry) (later-line list entry)) entries))
           (unknown "zz a kind of line a later release writes"))
      (dolist (version '(1 2))
        (with-fresh-state
          (put-state-file list (format nil "# tuatara ~a ~d~%# a comment of the later release~%~{~a~%~}~a~%" name version longer unknown))
          (is equal (if (and (= version 2) (tuatara.defaults::state-list-trust list)) '() entries)
              (tuatara.defaults::state-list-entries list) "~a ~d read" name version)
          (tuatara.defaults::state-list-drop list '("/nowhere/kept-files"))
          (tuatara.defaults::finish-state-writes)
          (is string= (format nil "# tuatara ~a ~d~%~@[~a~]~:[~;# a comment of the later release~%~]~{~a~%~}~a~%"
                              name version (tuatara.defaults::state-list-header list) (= version 2) longer unknown)
              (file-text (tuatara.defaults::state-list-path list))
              "~a ~d written back" name version))))))

(define-test (kept-files unread-twins-go)
  ;; a line this release cannot read goes with the entry of its name or path: trust taken back
  ;; leaves no line of a later kind trusting the same source
  (with-fresh-state
    (let* ((list tuatara.packages::*package-trust*)
           (where "https://example.org/signed.git"))
      (put-state-file list
                      (format nil "~a~cunsigned~c~c~%~a~cnewkind:ABC~c~c~%https://example.org/other.git~cunsigned~c~c~%"
                              where #\Tab #\Tab #\Tab where #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab))
      (tuatara.defaults::state-list-drop list (list where))
      (tuatara.defaults::finish-state-writes)
      (let ((text (file-text (tuatara.defaults::state-list-path list))))
        (false (search where text))
        (true (search "https://example.org/other.git" text)))))
  (with-fresh-state
    (let ((list tuatara.defaults::*grammar-answers*))
      (put-state-file list (format nil "yaml~cask~%nix~coff~%" #\Tab #\Tab))
      (tuatara.defaults::state-list-put list (list (cons "yaml" nil)))
      (tuatara.defaults::finish-state-writes)
      (is equal (list (format nil "yaml~coff" #\Tab) (format nil "nix~coff" #\Tab))
          (file-lines-but-comments (file-text (tuatara.defaults::state-list-path list))))))
  (with-fresh-state
    (let ((list tuatara.defaults::*trusted*))
      (put-state-file list
                      (format nil "# tuatara trusted 2~%/projects/one/~conly some days~%/projects/two/~%" #\Tab))
      (is equal '("/projects/two/") (tuatara.defaults::state-list-entries list))
      (tuatara.defaults::state-list-drop list '("/projects/one/"))
      (tuatara.defaults::finish-state-writes)
      (is equal '("/projects/two/") (file-lines-but-comments (file-text (tuatara.defaults::state-list-path list)))))))

(define-test (kept-files later-optional-fields)
  ;; fields a line of today may leave out, written empty by a later release before its own
  (let ((key "example https://example.org/tools.git release") (commit "0123456789abcdef0123456789abcdef01234567"))
    (flet ((parse (&rest fields)
             (tuatara.packages::parse-package-line (format nil "~{~a~^~a~}" (rest (loop for field in fields collect #\Tab collect field))))))
      (is equal (list key commit nil t "abcdef0" nil nil) (parse key commit "" "yes" "abcdef0"))
      (is equal (list key commit nil t nil nil nil) (parse key commit "" "yes" "" "" "" "later"))
      (is equal (list key commit "v1" t "abcdef0" "v0" nil) (parse key commit "v1" "yes" "abcdef0" "v0" "" "later"))))
  (is equal (list "Plain" "/p" 7 "ctx" '() nil)
      (tuatara.defaults::parse-bookmark-line (format nil "7~cPlain~c/p~cctx~c~c~cLATER" #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab)))
  ;; a version line says at most six figures
  (let ((list tuatara.defaults::*bookmarks*))
    (is eql 123456 (tuatara.defaults::state-line-version list "# tuatara bookmarks 123456"))
    (false (tuatara.defaults::state-line-version list "# tuatara bookmarks 1234567"))
    (false (tuatara.defaults::state-line-version list "# tuatara bookmarks 1x"))))

(define-test (kept-files unreadable-changes-bounded)
  ;; changes to a file that cannot be read wait, made fewer, and it is said once
  (with-fresh-state
    (let* ((list tuatara.defaults::*recent-files*)
           (tuatara-user:*recent-files-limit* 10)
           (path (tuatara.defaults::state-list-path list)))
      (put-state-file list (format nil "file /notes/first.txt~%"))
      (sb-posix:chmod path 0)
      (unwind-protect
           (dotimes (i 300)
             (tuatara.defaults::state-list-put list (list (cons (format nil "/notes/~d.txt" i) nil)))
             (when (evenp i) (tuatara.defaults::state-list-drop list (list (format nil "/notes/~d.txt" (1- i)))))
             (tuatara.defaults::finish-state-writes))
        (sb-posix:chmod path #o600))
      (true (<= (length (tuatara.defaults::state-list-pending list)) 3)
            "~d changes wait" (length (tuatara.defaults::state-list-pending list)))
      (is equal path (tuatara.defaults::state-list-unreadable list))
      ;; written once it can be read, the latest kept
      (tuatara.defaults::state-list-drop list '("/nowhere/kept-files"))
      (tuatara.defaults::finish-state-writes)
      (false (tuatara.defaults::state-list-unreadable list))
      (is equal "/notes/299.txt" (car (first (tuatara.defaults::read-state-file list path))))
      (is = 10 (length (tuatara.defaults::read-state-file list path))))))

(define-test (kept-files unreadable-not-written)
  ;; a file that cannot be read just then is never written over with only what was changed
  (with-fresh-state
    (let* ((list tuatara.defaults::*bookmarks*)
           (path (tuatara.defaults::state-list-path list))
           (entries (kept-file-entries list)))
      (tuatara.defaults::state-list-put list entries)
      (tuatara.defaults::finish-state-writes)
      (let ((before (file-text path)))
        (sb-posix:chmod path 0)
        (unwind-protect
             (progn
               (tuatara.defaults::state-list-put list (list (list "New" "/notes/new.txt" 0 "" () nil)))
               (tuatara.defaults::finish-state-writes))
          (sb-posix:chmod path #o600))
        (is string= before (file-text path))
        ;; written once it can be read again
        (tuatara.defaults::state-list-drop list '("/nowhere/kept-files"))
        (tuatara.defaults::finish-state-writes)
        (is equal (cons (list "New" "/notes/new.txt" 0 "" () nil) entries)
            (tuatara.defaults::read-state-file list path))))))

(define-test (kept-files bookmark-to-a-path-not-there-kept)
  ;; a bookmark to a server's directory not mapped just now: gone to, said so, and kept
  (with-fresh-state
    (let ((tuatara.defaults::*modal-editing* nil)
          (list tuatara.defaults::*bookmarks*))
      (tuatara.defaults::state-list-put list (kept-file-entries list))
      (tuatara.defaults::finish-state-writes)
      (let ((f (run-keys "C-x m b U n m a p p e d RET" :width 100)))
        (true (tuatara.frontend.fake:fake-frame-showing f "no longer there")))
      (tuatara.defaults::finish-state-writes)
      (true (find "Unmapped" (tuatara.defaults::read-state-file list (tuatara.defaults::state-list-path list))
                  :key #'first :test #'string=)))))

(define-test (kept-files bookmarks-never-dropped-for-their-number)
  ;; past *bookmarks-kept* a bookmark set is kept and says so; none is dropped, here, on disk, from
  ;; another session or among the lines a later release wrote; the remembered lists are still cut
  (with-test-directory (dir)
    (with-fresh-state
      (let* ((tuatara.defaults::*modal-editing* nil)
             (tuatara.defaults::*bookmark-set-asks-categories* nil)
             (tuatara-user:*bookmarks-kept* 3)
             (tuatara-user:*recent-files-limit* 3)
             (list tuatara.defaults::*bookmarks*)
             (path (tuatara.defaults::state-list-path list))
             (file (namestring (put-file (merge-pathnames "notes.txt" dir) (format nil "one~%two~%")))))
        (flet ((mark (name) (list name "/notes/plain.txt" 0 "" () nil))
               (names () (mapcar #'first (tuatara.defaults::read-state-file list path))))
          (put-state-file list (format nil "zz a later release's line one~%zz two~%zz three~%zz four~%"))
          (tuatara.defaults::state-list-put list (mapcar #'mark '("bm-e" "bm-d" "bm-c" "bm-b" "bm-a")))
          (tuatara.defaults::finish-state-writes)
          (is equal '("bm-e" "bm-d" "bm-c" "bm-b" "bm-a") (names))
          (is = 5 (length (tuatara.defaults::state-list-entries list)))
          (is = 4 (count-if (lambda (line) (uiop:string-prefix-p "zz" line))
                            (uiop:split-string (file-text path) :separator '(#\Newline))))
          ;; another session's put, past the limit too
          (let ((other (another-session list)))
            (tuatara.defaults::state-list-put other (list (mark "bm-f")))
            (tuatara.defaults::finish-state-writes)
            (tuatara.defaults::write-state-list other))
          (is equal '("bm-f" "bm-e" "bm-d" "bm-c" "bm-b" "bm-a") (names))
          ;; set by C-x m m: kept, and the echo area says how many there are
          (let ((f (run-keys "C-x m m RET" :file file :width 120)))
            (true (tuatara.frontend.fake:fake-frame-showing-across-rows f
                                                                        "7 bookmarks, more than *bookmarks-kept* (3)")))
          (tuatara.defaults::finish-state-writes)
          (is equal '("notes.txt" "bm-f" "bm-e" "bm-d" "bm-c" "bm-b" "bm-a") (names))
          ;; the oldest still listed and gone to
          (let ((f (run-keys "C-x m l" :width 120 :height 20)))
            (true (tuatara.frontend.fake:fake-frame-showing f "notes.txt"))
            (true (tuatara.frontend.fake:fake-frame-showing f "bm-a")))
          (true (find "bm-a" (tuatara.defaults::bookmark-names) :test #'string=))
          (forget-file-buffers)
          ;; a remembered list keeps no more than its limit
          (tuatara.defaults::state-list-put tuatara.defaults::*recent-files*
                                            (mapcar (lambda (n) (cons (format nil "/notes/~a.txt" n) nil))
                                              '(1 2 3 4 5)))
          (tuatara.defaults::finish-state-writes)
          (is = 3 (length (tuatara.defaults::read-recent-files))))))))

(define-test (kept-files header-written-once)
  ;; a later version's file written again and again keeps one header and its own comments once
  (with-fresh-state
    (let* ((list tuatara.defaults::*bookmarks*)
           (path (tuatara.defaults::state-list-path list)))
      (put-state-file list
                      (format nil "# tuatara bookmarks 2~%~a# a comment of the later release~%7~cPlain~c/p~cctx~c~c~%"
                              (tuatara.defaults::state-list-header list) #\Tab #\Tab #\Tab #\Tab #\Tab))
      (tuatara.defaults::state-list-drop list '("nothing one"))
      (tuatara.defaults::finish-state-writes)
      (let ((once (file-text path)))
        (tuatara.defaults::state-list-drop list '("nothing two"))
        (tuatara.defaults::finish-state-writes)
        (is string= once (file-text path))
        (is = 1 (count-matches "# Bookmarks, the latest" once))
        (is = 1 (count-matches "# a comment of the later release" once))))))

(define-test (kept-files forgotten-while-unreadable)
  ;; a trust taken back while the file cannot be read stays taken back, however many changes follow
  (with-fresh-state
    (let* ((list tuatara.defaults::*trusted*)
           (path (tuatara.defaults::state-list-path list)))
      (tuatara.defaults::state-list-put list (list "/t/"))
      (tuatara.defaults::finish-state-writes)
      (sb-posix:chmod path 0)
      (unwind-protect
           (progn
             (tuatara.defaults::state-list-drop list '("/t/"))
             (tuatara.defaults::finish-state-writes)
             (dotimes (i 70)
               (tuatara.defaults::state-list-put list (list (format nil "/p~d/" i)))
               (tuatara.defaults::finish-state-writes)
               (tuatara.defaults::state-list-drop list (list (format nil "/q~d/" i)))
               (tuatara.defaults::finish-state-writes)))
        (sb-posix:chmod path #o600))
      (tuatara.defaults::state-list-drop list '("/nowhere/kept-files/"))
      (tuatara.defaults::finish-state-writes)
      (let ((kept (tuatara.defaults::read-state-file list path)))
        (false (member "/t/" kept :test #'equal))
        (is = 70 (length kept))))))

(define-test (kept-files new-kind-not-a-twin)
  ;; a later release's line of a new kind is never taken for an entry's twin, nor read as one
  (with-fresh-state
    (let* ((list tuatara.defaults::*bookmarks*)
           (line (format nil "category~cwork~ccolour red" #\Tab #\Tab)))
      (put-state-file list (format nil "# tuatara bookmarks 2~%~a~%" line))
      (tuatara.defaults::state-list-put list (list (list "work" "/w" 0 "" () nil)))
      (tuatara.defaults::finish-state-writes)
      (true (member line
                    (file-lines-but-comments (file-text (tuatara.defaults::state-list-path list)))
                    :test #'string=))))
  (with-fresh-state
    (let* ((list tuatara.defaults::*grammar-answers*)
           (line (format nil "@category~cyaml" #\Tab)))
      (put-state-file list (format nil "# tuatara grammars 2~%~a~%" line))
      (is equal '() (tuatara.defaults::state-list-entries list))
      (tuatara.defaults::state-list-put list (list (cons "yaml" t)))
      (tuatara.defaults::finish-state-writes)
      (true (member line
                    (file-lines-but-comments (file-text (tuatara.defaults::state-list-path list)))
                    :test #'string=))))
  (dolist (row *kept-file-entries*)
    (let ((list (symbol-value (first row))))
      (false (tuatara.defaults::state-line-key list (format nil "@later~cthing" #\Tab))
             "~a takes a line of a new kind" (tuatara.defaults::state-list-name list))
      (dolist (entry (rest row))
        (is equal (funcall (tuatara.defaults::state-list-path-of list) entry)
            (tuatara.defaults::state-line-key list (funcall (tuatara.defaults::state-list-print list) entry))
            "~a's line key" (tuatara.defaults::state-list-name list))))))

(define-test (kept-files version-line-anywhere)
  ;; the version of a file is that of its version line wherever it stands, so a list of trust
  ;; reads none of a later version's longer lines, those before that line included
  (with-fresh-state
    (put-state-file tuatara.defaults::*trusted*
                    (format nil "/a/~cX~%# tuatara trusted 2~%/b/~cY~%/c/~%" #\Tab #\Tab))
    (is equal '("/c/") (tuatara.defaults::state-list-entries tuatara.defaults::*trusted*))))

(define-test (kept-files past-limit-counted-cheaply)
  ;; how many bookmarks there will be is counted from those shown, not by making the list again:
  ;; what one count makes does not grow with the bookmarks
  (flet ((octets-a-count (n)
           (with-fresh-state
             (let ((list tuatara.defaults::*bookmarks*))
               (tuatara.defaults::state-list-put list (loop for i below n collect (list (format nil "b~d" i) "/p" i "" () nil)))
               (tuatara.defaults::finish-state-writes)
               (tuatara.defaults::state-list-entries list)
               (let ((before (sb-ext:get-bytes-consed)))
                 (dotimes (i 100) (tuatara.defaults::state-list-past-limit list 1))
                 (/ (- (sb-ext:get-bytes-consed) before) 100))))))
    (let ((few (octets-a-count 1000)) (many (octets-a-count 20000)))
      (true (< (- many few) 1024) "~,1f octets a count of 20000, ~,1f of 1000" many few))))

(define-test (kept-files fold-is-exact)
  ;; changes waiting for a file made fewer do just what they did in turn, the limit's cuts too
  (let ((list tuatara.defaults::*chosen-paths*))
    (flet ((both (disk changes)
             (values (tuatara.defaults::apply-state-changes list disk changes)
                     (tuatara.defaults::apply-state-changes
                      list disk (reverse (mapcar #'cdr (tuatara.defaults::compact-state-changes
                                                         list (reverse (mapcar (lambda (c)
                                                                                 (cons "f" c)) changes)))))))))
      (multiple-value-bind (in-turn folded) (both '("/0" "/3") '((:put ("/1") 2) (:drop ("/0"))))
        (is equal '("/1") in-turn)
        (is equal in-turn folded))
      (let ((*random-state* (sb-ext:seed-random-state 5)) (wrong 0))
        (dotimes (trial 2000)
          (flet ((path () (format nil "/~d" (random 6))))
            (let* ((most (1+ (random 4)))
                   (disk (remove-duplicates (loop repeat (random 6) collect (path)) :test #'string= :from-end t))
                   (changes (loop repeat (1+ (random 12))
                                  collect (case (random 3)
                                            (0
                                             (list :put (remove-duplicates (list (path) (path)) :test #'string=) most))
                                            (1 (list :replace (list (path))))
                                            (2 (list :drop (list (path))))))))
              (multiple-value-bind (in-turn folded) (both disk changes)
                (unless (equal in-turn folded) (incf wrong))))))
        (is = 0 wrong "~d of 2000 folded otherwise" wrong)))))

(define-test (kept-files at-sources-refused)
  ;; a package's source never begins as a line of a new kind does
  (fail (tuatara.packages::check-package-where "@later/source"))
  (finish (tuatara.packages::check-package-where "https://example.org/tools.git")))
