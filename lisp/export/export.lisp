;;;; lisp/export/export.lisp --- Org and LaTeX files made into PDFs, in the background.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; In an Org buffer M-x org-export-latex-pdf makes the file into a PDF beside
;;;; it, as Org's own export makes it; org-export-latex-pdf-buffer shows the PDF
;;;; too, beside the file when the screen is wide enough, org-export-latex-pdf-xdg
;;;; opens it in the desktop's program, and the -enhanced ones add Tuatara's own
;;;; improvements. In a LaTeX buffer latex-compile-pdf, -buffer and -xdg do the
;;;; same for the LaTeX file. An Org buffer is first written as LaTeX, as it is
;;;; in the buffer, by ORG-LATEX-DOCUMENT; a LaTeX file is saved first. The
;;;; LaTeX is then run through the first of *LATEX-ENGINES*, xelatex as
;;;; standard, and through the next when that fails, as often as it takes for
;;;; its cross-references to settle. Their working files (.aux, .toc, .log)
;;;; are kept in the cache, one directory for each file and program, so the
;;;; directory of the file gets only its .tex and .pdf, and making the PDF
;;;; again after a change usually takes one run. All of it runs as a job of
;;;; the buffer: P is lit in its mode line, C-x J (stop-job) stops it. A
;;;; view of the PDF already open is read again when the PDF is written.
;;;;
;;;; Exporting runs what the file says (LaTeX's programming), so it is refused
;;;; for a file opened safely. Org's code blocks are never evaluated, and LaTeX
;;;; may not run programs (no shell escape). The LaTeX program is shut in by
;;;; the sandbox (src/io/sandbox.lisp): it reads the document's folder, the
;;;; trusted ones and TeX's own, writes in its working directory alone, and
;;;; reaches no network. Without a sandbox it runs all the same, and the
;;;; export says so.

(in-package #:tuatara.defaults)

(defvar *latex-engines* '("xelatex" "lualatex")
  "The programs that make a PDF of LaTeX, tried in this order: when one fails,
the next is tried. Each is a program's name, looked for on the PATH and then
in TeX Live's own folders (~/.texlive/2026/bin/x86_64-linux/ and the like,
the newest release first), or its full path, which may begin with ~
\(\"~/texlive/bin/xelatex\"), for one installed elsewhere. A file that
names the program it is meant for, by a first line such as \"% Intended LaTeX compiler: lualatex\" (which an
Org file's #+latex_compiler gives) or \"%!TEX program = lualatex\", has that one
tried first when it is in the list.")

(defparameter *latex-runs* 4
  "The most times a LaTeX program is run on a file for one PDF: again only
while its cross-references, contents and outline are still changing.")

(defparameter *latex-error-lines* 60
  "The most lines of a LaTeX program's errors shown in *LaTeX errors*.")

(defparameter *latex-carried-settings* '(*latex-engines* *latex-runs* *latex-error-lines* *export-trusted-folders*
                                         *sandbox-program*)
  "The settings an export reads as it works, carried to the thread it works on
as they are on the editor's when it starts (TUATARA.RUNTIME:CARRY-SETTINGS).")

;;; The programs

(defun setting-program (name)
  "The full path of the program NAME, a name looked for on the PATH or a path
\(a leading ~ the home directory); NIL when there is no such program."
  (and (stringp name) (plusp (length name))
       (tuatara.io:find-program (if (char= #\~ (char name 0))
                                    (tuatara.io:native (tuatara.io:expand-path name))
                                    name))))

(defun latex-engine-names ()
  "The programs *LATEX-ENGINES* names, as a list however it was set."
  (if (listp *latex-engines*) *latex-engines* (list *latex-engines*)))

(defparameter *tex-live-directories* '("~/.texlive/" "~/texlive/" "/usr/local/texlive/" "/opt/texlive/")
  "Where TeX Live is installed, each holding a directory for each year's
release, YEAR/bin/PLATFORM/ holding its programs: looked in, the newest
release first, for a program of *LATEX-ENGINES* the PATH does not have.")

(defun tex-live-program (name)
  "The full path of the program NAME in the newest TeX Live release under
*TEX-LIVE-DIRECTORIES* that has it, or NIL."
  (dolist (root *tex-live-directories*)
    (let ((root (ignore-errors (tuatara.io:native (tuatara.io:expand-path root)))))
      (when root
        (dolist (release (sort (loop for (entry kind) in (tuatara.io:directory-entries root)
                                     when (and (eq kind :directory) (every #'digit-char-p entry)) collect entry)
                               #'string>))
          (let ((bin (tuatara.io:path-join (tuatara.io:path-join root release) "bin/")))
            (dolist (platform (tuatara.io:directory-entries bin))
              (when (eq (second platform) :directory)
                (let ((found (tuatara.io:find-program
                              (tuatara.io:path-join (tuatara.io:path-join bin (concatenate 'string (first platform) "/")) name))))
                  (when found (return-from tex-live-program found)))))))))))

(defun latex-engines-found ()
  "The programs of *LATEX-ENGINES* found here, each (NAME . PATH), in order: a
name on the PATH, else in TeX Live's own folders (*TEX-LIVE-DIRECTORIES*),
for a desktop whose PATH does not have them; a path as it is."
  (loop for name in (latex-engine-names)
        for path = (or (setting-program name)
                       (and (stringp name) (plusp (length name)) (not (find #\/ name)) (tex-live-program name)))
        when path collect (cons (tuatara.io:native-file-name name) path)))

(defun latex-export-words ()
  "What makes PDFs of Org and LaTeX files here, as M-x check-health says it:
the programs found, and whether they run shut in by the sandbox."
  (let ((engines (latex-engines-found)))
    (if engines
        (multiple-value-bind (status detail) (tuatara.io:sandbox-status)
          (format nil "PDFs of Org and LaTeX by ~{~a~^, then ~}, ~:[unconfined (~a): a document could read any of your files~;shut in by the sandbox~*~]"
                  (mapcar #'cdr engines) (eq status :ready) detail))
        (format nil "PDFs of Org and LaTeX by none of ~{~a~^, ~}, on the PATH or in TeX Live's folders: ~a, or give their paths in *latex-engines*"
                (latex-engine-names) (outside-program-install-words :latex)))))

(defun intended-engine (tex)
  "The LaTeX program the file TEX says it is meant for, by its first lines, as
a name in lower case; NIL when it says none."
  (let ((head (tuatara.io:file-head tex 4096)))
    (when head
      (let ((text (tuatara.text:lenient-string head)))
        (dolist (mark '("% Intended LaTeX compiler: " "%!TEX TS-program = " "%!TEX program = "))
          (let ((at (search mark text :test #'char-equal)))
            (when at
              (let* ((start (+ at (length mark)))
                     (end (or (position-if (lambda (c) (member c '(#\Newline #\Return #\Space #\Tab))) text :start start)
                              (length text))))
                (return (string-downcase (subseq text start end)))))))))))

(defun engines-for (tex)
  "The programs found here to make a PDF of the file TEX with, each (NAME .
PATH), the one TEX says it is meant for first."
  (let* ((engines (latex-engines-found))
         (intended (intended-engine tex))
         (first (and intended (find intended engines :key #'car :test #'string-equal))))
    (if first (cons first (remove first engines)) engines)))

;;; The job

(defstruct (pdf-export (:include tuatara.runtime:job)
                       (:constructor make-pdf-export (buffer source kind then save rope enhanced))
                       (:copier nil))
  "A PDF being made of SOURCE, the file of BUFFER, a job (TUATARA.RUNTIME:JOB):
KIND :ORG or :LATEX; THEN what is done with the PDF, :FILE (nothing),
:VIEW (shown beside), :OUTSIDE (opened in the desktop's program), or
:LATEX-ONLY, no PDF made, the export stopping at the LaTeX file; SAVE,
the save of BUFFER it waits for first, or NIL; ROPE, an Org buffer's text as
it was when asked; ENHANCED, true for Tuatara's own improvements on Org's
LaTeX. STEP says what it does now; PROCESS is the program running for it;
TEX, the LaTeX file; TEXT, the Org text written as it, and MAP, which place
in TEXT each of its lines comes from (ORG-LATEX-DOCUMENT); PDF and ENGINE,
the PDF made and the program that made it; FAILED, the programs that failed
before; ERRORS, what each said, (PROGRAM . ERRORS) (LATEX-ERRORS), for
*LaTeX errors*; MISSING, the characters no font here has, each (CHARACTER .
LINE) by the LaTeX's line it is first on; NOTES, what else to say of it;
PART, the words for a part exported alone (the subtree \"Findings\"), with
ORIGINS, the line of the file each line of its text comes from, a vector,
and LINES, the file's lines, for what is said of them; TRUSTED, the texts of
its template and setup files that are your own, and PICTURES, the files its
pictures are drawn from (ORG-LATEX-DOCUMENT)."
  source kind then save rope enhanced (step "starting") tex text map pdf engine (failed '()) errors
  (missing '()) (notes '()) part origins lines (trusted '()) (pictures '()))

(setf (documentation 'pdf-export-p 'function) "True when OBJECT is a PDF-EXPORT.")

(defun export-name (job)
  "The name of the file JOB makes a PDF of."
  (tuatara.io:native-file-name (pdf-export-source job)))

(defun export-doing (kind)
  "What making a PDF of a file of KIND is called: an Org file is exported, a LaTeX file compiled."
  (if (eq kind :latex) "Compiling" "Exporting"))

(tuatara.runtime:define-job-words pdf-export
  :doing (export-doing (pdf-export-kind job))
  :description (format nil "~a ~@[~a of ~]~a to ~a" (export-doing (pdf-export-kind job)) (pdf-export-part job) (export-name job)
          (export-target-words job))
  :verb (format nil "~(~a~) ~a to ~a" (if (eq (pdf-export-kind job) :latex) "compile" "export") (export-name job)
          (export-target-words job))
  :left-words "the PDF is as it was")
(defmethod tuatara.runtime:job-state-words ((job pdf-export))
  "What an export is doing now, and at which step, in words."
  (format nil "~a: ~a" (export-doing (pdf-export-kind job)) (pdf-export-step job)))

(defun export-target-words (job)
  "What JOB makes, in a word: LaTeX for an export that stops at the LaTeX file, else PDF."
  (if (eq (pdf-export-then job) :latex-only) "LaTeX" "PDF"))
(defmethod tuatara.runtime:job-holds-queue-p ((job pdf-export))
  "An export holds back no other work on its buffer: it only reads the note."
  nil)

(defun run-for-export (job program arguments &key directory environment)
  "Run PROGRAM with ARGUMENTS for JOB, in DIRECTORY, with ENVIRONMENT when
given, its input empty and what it prints dropped, and wait for it; never
through a shell. Returns its exit code. JOB stopping ends it."
  (tuatara.runtime:job-check-stop)
  (unwind-protect
       (apply #'tuatara.io:call-with-program (cons program arguments)
              (lambda (process)
                (sb-ext:process-wait process)
                (tuatara.runtime:job-check-stop)
                (sb-ext:process-exit-code process))
              :job job :directory directory
              (and environment (list :environment environment)))
    (setf (pdf-export-process job) nil)))

;;; LaTeX shut in

(defparameter *latex-sandbox-seconds* 600
  "The seconds one run of a LaTeX program may take, shut in, before it is
ended, and the processor time it may take: ten minutes, for a long document
with many pictures, or the first run after TeX was installed, when LuaLaTeX
makes its list of the fonts there are.")

(defparameter *latex-sandbox-memory* (* 2 1024 1024 1024)
  "The octets of memory a LaTeX program may take, shut in (2 GB).")

(defparameter *latex-sandbox-file-limit* (* 4 1024 1024 1024)
  "The octets a LaTeX program, shut in, may write to one file (4 GB): its PDF
and working files.")

(defparameter *latex-passed-environment* '("TEXMF" "OSFONTDIR" "SOURCE_DATE_EPOCH" "FORCE_SOURCE_DATE"
                                           "XDG_CONFIG_HOME" "XDG_DATA_HOME" "XDG_CACHE_HOME")
  "The names, or beginnings of names (TEXMF for TEXMFHOME and the rest), of
what in the editor's environment a LaTeX program shut in is given as it is:
where TeX and the fonts are, and the date a PDF is said to be made.")

(defparameter *tex-tree-variables* '("TEXMFCNF" "TEXMFROOT" "TEXMFDIST" "TEXMFLOCAL" "TEXMFSYSVAR" "TEXMFSYSCONFIG"
                                     "TEXMFHOME" "TEXMFCONFIG" "TEXMFAUXTREES" "TEXMFVAR")
  "TeX's own names of the folders it reads its settings (texmf.cnf), packages
and fonts from, as kpsewhich tells them; the last, TEXMFVAR, is where it keeps
what it makes for itself.")

(defvar *tex-trees* (make-hash-table :test 'equal :synchronized t)
  "The folders of TeX found for each LaTeX program (TEX-TREES), by its path and the home folder.")

(defun tex-tree-paths (value)
  "The folders VALUE, one of TeX's own settings as kpsewhich expands it,
names: each beginning with /, the {a,b} lists, : and ; between, and !! before
them taken apart."
  (loop for part in (uiop:split-string value :separator '(#\: #\; #\, #\{ #\}))
        for path = (string-right-trim "/" (string-left-trim "!" (string-trim " " part)))
        when (and (plusp (length path)) (char= #\/ (char path 0)))
          collect path))

(defun shown-whole-p (folder)
  "True when FOLDER may be shown whole to a LaTeX program shut in: neither
the root, nor your home folder, nor a folder holding it."
  (let ((real (tuatara.io:real-place folder))
        (home (tuatara.io:real-place (latex-home))))
    (not (or (string= real "") (string= real home) (uiop:string-prefix-p (concatenate 'string real "/") home)))))

(defun tex-trees (path)
  "The folders of TeX that the LaTeX program at PATH reads, as two values: a
list of them, its own folder first; and the folder it keeps what it makes
for itself in (TEXMFVAR), or NIL. As kpsewhich beside it tells them, asked
once (a fifth of a second) and kept; only the program's folder when there is
no kpsewhich there. Folders under /usr, which a program shut in sees
already, are left out, and so are those not to be shown whole
\(SHOWN-WHOLE-P), which a setting may name."
  (let* ((own (string-right-trim "/" (tuatara.io:parent-path path)))
         (key (list path (sb-posix:getenv "HOME")))
         (found (or (gethash key *tex-trees*)
                    (setf (gethash key *tex-trees*)
                          (let* ((kpsewhich (tuatara.io:find-program (tuatara.io:path-join (tuatara.io:parent-path path) "kpsewhich")))
                                 (said (and kpsewhich
                                            (tuatara.io:program-output
                                             (list kpsewhich (format nil "-expand-var=~{$~a~^~%~}" *tex-tree-variables*))
                                             :seconds 10)))
                                 (lines (and said (org-text-lines said))))
                            (if (= (length lines) (length *tex-tree-variables*))
                                (list (loop for line in (butlast lines) append (tex-tree-paths line))
                                      (first (tex-tree-paths (car (last lines)))))
                                (list '() nil)))))))
    (flet ((system-p (folder) (or (string= folder "/usr") (uiop:string-prefix-p "/usr/" folder)
                                  (not (shown-whole-p folder)))))
      (values (remove-duplicates (cons own (remove-if #'system-p (first found))) :test #'string= :from-end t)
              (let ((var (second found))) (and var (not (system-p var)) var))))))

(defun tex-cache-overlay (var)
  "How TeX's folder VAR, where it keeps what it makes for itself (its fonts'
list, its formats), is shown to a LaTeX program shut in: what is there read
as it is, what it writes kept in a folder of the editor's own in the cache,
latex-texmf-var/, never in VAR, so that a document cannot leave there what a
LaTeX program run outside the sandbox would then read. As two values, the
entries for SANDBOX-RUN's OVERLAYS and WRITABLE (VAR made in the editor's
folder where VAR is not there yet)."
  (let* ((mine (private-cache-directory (format nil "latex-texmf-var/~a-~36r/" (tuatara.io:native-file-name var) (sxhash var))))
         (upper (string-right-trim "/" (tuatara.io:ensure-private-directory (tuatara.io:path-join mine "upper"))))
         (work (string-right-trim "/" (tuatara.io:ensure-private-directory (tuatara.io:path-join mine "work")))))
    (if (eq :directory (tuatara.io:path-kind var))
        (values (list (list var upper work)) '())
        (values '() (list (cons upper var))))))

(defun latex-home ()
  "Your home folder as a LaTeX program is told it, shut in: as the editor has
it in its environment, so that TeX and the fonts find your own where they
always do."
  (string-right-trim "/" (or (sb-posix:getenv "HOME") (tuatara.io:home-directory))))

(defparameter *latex-picture-file-kinds* '(:png :jpeg :pdf :postscript)
  "The kinds of file, by their contents, a LaTeX program shut in is shown when
the document includes them as pictures from outside the folders it sees.")

(defun latex-picture-files (tex trusted)
  "The files the LaTeX document TEX includes as pictures (\\includegraphics) by
a full name or one from your home folder (~/), written so in one of TRUSTED,
the texts of its template and setup files that are your own, and which are
pictures or PDFs by their contents (*LATEX-PICTURE-FILE-KINDS*): those a
LaTeX program shut in is shown, each alone and read only, wherever they are.
A note's own text, or a file that came with it, cannot have a file of yours
from elsewhere shown, however it writes its name."
  (let ((text (ignore-errors (tuatara.io:file-text tex :limit (* 64 1024 1024)))) (files '()))
    (when text
      (cl-ppcre:do-register-groups (name) ("\\\\includegraphics\\s*(?:\\[[^\\]]*\\])?\\s*\\{([^}\\n]+)\\}" text)
        (let ((native (and (or (uiop:string-prefix-p "/" name) (uiop:string-prefix-p "~/" name))
                           (some (lambda (own) (search name own)) trusted)
                           (ignore-errors (tuatara.io:native (tuatara.io:expand-path name))))))
          (when (and native (member (nth-value 1 (ignore-errors (file-kind native))) *latex-picture-file-kinds*))
            (pushnew native files :test #'string=)))))
    (nreverse files)))

(defun latex-sandbox-places (job tex path directory)
  "What the LaTeX program at PATH, making a PDF of TEX for JOB in DIRECTORY,
is shown, shut in, as a plist of SANDBOX-RUN's :DIRECTORIES, :WRITABLE and
:OVERLAYS. Read only: the folder of TEX and that of the file exported, and
the folders *EXPORT-TRUSTED-FOLDERS* names, each when it may be shown whole
\(SHOWN-WHOLE-P; else TEX alone), TeX's own folders (TEX-TREES) and your
fonts and their settings; to write in: DIRECTORY alone, and TeX's own cache
\(TEX-CACHE-OVERLAY). The files the note's pictures are drawn from (those an
export draws in latex/pictures/ among them), the pictures your own template
and setup files have TEX include from elsewhere (LATEX-PICTURE-FILES), or a
LaTeX file exported as it is when it stands in your own folders
\(EXPORT-TRUSTED-PATH-P), are shown too, each file alone, read only."
  (multiple-value-bind (trees var) (tex-trees path)
    (multiple-value-bind (overlays var-writable) (if var (tex-cache-overlay var) (values '() '()))
      (let* ((home (latex-home))
             (config (string-right-trim "/" (or (sb-posix:getenv "XDG_CONFIG_HOME") (format nil "~a/.config" home))))
             (cache (string-right-trim "/" (or (sb-posix:getenv "XDG_CACHE_HOME") (format nil "~a/.cache" home)))))
        (list :directories (append (let ((folders (list (tuatara.io:parent-path tex) (tuatara.io:parent-path (pdf-export-source job)))))
                                     (if (every #'shown-whole-p folders) folders (list tex)))
                                   (loop for folder in *export-trusted-folders*
                                         for native = (ignore-errors (tuatara.io:native (tuatara.io:expand-path folder)))
                                         when (and native (shown-whole-p native)) collect native)
                                   (pdf-export-pictures job)
                                   trees
                                   (latex-picture-files tex (if (eq (pdf-export-kind job) :latex)
                                                                ;; a LaTeX file of your own, in your own folders
                                                                (let ((text (and (export-trusted-path-p tex) (ignore-errors (tuatara.io:file-text tex :limit (* 64 1024 1024))))))
                                                                  (and text (list text)))
                                                                (pdf-export-trusted job)))
                                   (list (format nil "~a/fontconfig" config) (format nil "~a/fontconfig" cache))
                                   (tuatara.io:user-font-directories home))
              :writable (cons directory var-writable)
              :overlays overlays)))))

(defun latex-sandbox-environment (path tex)
  "What the LaTeX program at PATH, making a PDF of TEX, has in its
environment, shut in, each (NAME VALUE): its own folder first on the PATH,
for the programs it runs in turn (xdvipdfmx); your home folder, where TeX
and the fonts find your own; TEX's folder searched for what it names; what it
writes kept to where it runs; and *LATEX-PASSED-ENVIRONMENT* as the editor
has it."
  (append (list (list "PATH" (format nil "~a:~a" (string-right-trim "/" (tuatara.io:parent-path path)) tuatara.io:*sandbox-path*))
                (list "HOME" (latex-home))
                (list "TEXINPUTS" (format nil "~a:" (tuatara.io:parent-path tex)))
                (list "openout_any" "p"))
          (loop for setting in (sb-ext:posix-environ)
                for at = (position #\= setting)
                for name = (and at (subseq setting 0 at))
                when (and name (string/= name "TEXINPUTS")
                          (some (lambda (passed) (uiop:string-prefix-p passed name)) *latex-passed-environment*))
                  collect (list name (subseq setting (1+ at))))))

(defun run-shut-in (job tex path arguments directory)
  "Run the LaTeX program at PATH with ARGUMENTS for JOB, making a PDF of TEX
in DIRECTORY, shut in (TUATARA.IO:SANDBOX-RUN): it sees what
LATEX-SANDBOX-PLACES shows it, read only but for DIRECTORY and TeX's own
cache, has no network, and is held to *LATEX-SANDBOX-SECONDS*,
*LATEX-SANDBOX-MEMORY* and *LATEX-SANDBOX-FILE-LIMIT*; what it prints is let
go. Returns two values: true when it ended well, and else how it ended, as
a sentence naming it (TUATARA.IO:SANDBOX-OUTCOME-WORDS). JOB stopping ends it."
  (tuatara.runtime:job-check-stop)
  (multiple-value-bind (output outcome said)
      (apply #'tuatara.io:sandbox-run path arguments
             :seconds *latex-sandbox-seconds* :memory *latex-sandbox-memory* :file-limit *latex-sandbox-file-limit*
             :output-limit nil :cancel (pdf-export-cancel job) :directory directory
             :environment (latex-sandbox-environment path tex)
             (latex-sandbox-places job tex path directory))
    (declare (ignore output))
    (tuatara.runtime:job-check-stop)
    (values (eq outcome :done)
            (tuatara.io:sandbox-outcome-words outcome said :label (tuatara.io:native-file-name path)
                                                           :seconds-setting '*latex-sandbox-seconds*))))

(defun run-unconfined (job path arguments directory environment)
  "Run the LaTeX program at PATH with ARGUMENTS for JOB in DIRECTORY, with
ENVIRONMENT (each NAME=VALUE) in place of the editor's own of those names,
not shut in (RUN-FOR-EXPORT). Returns two values: true when it ended well,
and else how it ended, as a sentence naming it."
  (let ((code (run-for-export job path arguments
                              :directory directory
                              :environment (append environment
                                                   (remove-if (lambda (v)
                                                                (some (lambda (setting)
                                                                        (uiop:string-prefix-p (subseq setting 0 (1+ (position #\= setting))) v))
                                                                      environment))
                                                              (sb-ext:posix-environ))))))
    (values (eql code 0) (format nil "~a ended with exit code ~a" (tuatara.io:native-file-name path) code))))

(defun latex-unconfined-allowed-p ()
  "True when a LaTeX program may run for an export now: shut in by the
sandbox, or, where there is none (bwrap missing, or turned off by
*sandbox-program*), when you say yes to running it with your rights, as what
the document says could then read any of your files."
  (multiple-value-bind (status detail) (tuatara.io:sandbox-status)
    (or (eq status :ready)
        (yes-or-no-p* (format nil "~a. Run it all the same?" (latex-unconfined-words detail :would t))))))

(defun latex-unconfined-words (detail &key would)
  "What an export says when its LaTeX program ran without the sandbox, DETAIL
saying why; with WOULD, what it asks before it runs so."
  (format nil "LaTeX ~:[ran~;would run~] without the sandbox (~a), so what the document says could read any of your files"
          would detail))

(defun check-export-folder (path)
  "Signal an error saying so when the folder an export is to write PATH in
does not exist: an export never makes folders, wherever #+EXPORT_FILE_NAME
points."
  (unless (tuatara.io:path-stamp (tuatara.io:parent-path path))
    (error "~a does not exist (#+export_file_name)" (tuatara.io:parent-path path))))

(defun org-to-latex (job engine)
  "Write the Org text of JOB as LaTeX for the program ENGINE (ORG-LATEX-DOCUMENT),
beside its file or where its #+export_file_name says; returns the LaTeX
file's full name."
  (setf (pdf-export-step job) "writing LaTeX")
  (let ((source (pdf-export-source job))
        (text (let ((rope (pdf-export-rope job))) (if (stringp rope) rope (tuatara.text:rope-string rope)))))
    (multiple-value-bind (latex named map missing notes trusted pictures)
        (org-latex-document text :file source :engine engine :enhanced (pdf-export-enhanced job)
                                 :lines (pdf-export-origins job))
      (setf (pdf-export-text job) text (pdf-export-map job) map (pdf-export-missing job) missing
            (pdf-export-notes job) notes (pdf-export-trusted job) trusted (pdf-export-pictures job) pictures)
      (let ((tex (or named (replaced-type source "tex"))))
        (check-export-folder tex)
        (tuatara.runtime:job-check-stop)
        (tuatara.io:replace-file tex latex)
        tex))))

(defun latex-build-directory (tex engine)
  "Where ENGINE keeps its working files for the LaTeX file TEX, between one
export and the next: latex/ in the cache directory, a directory for each file
and program, for you alone. Those not used for a day are deleted."
  (let ((root (private-cache-directory "latex/")))
    (forget-old-copies root)
    (tuatara.io:ensure-private-directory
     (tuatara.io:path-join root (format nil "~a-~36r/~a/" (tuatara.io:native-file-name tex) (sxhash tex) engine)))))

(defparameter *latex-cross-reference-types* '("aux" "toc" "out" "lof" "lot" "nav" "snm")
  "The kinds of working file whose change after a LaTeX run calls for another.")

(defun cross-references (directory stem)
  "The working files of the LaTeX file STEM in DIRECTORY that a run changes
until its cross-references settle, each as its size and its SHA-256 sum
\(FILE-SHA256), so that two runs' are compared without holding them; NIL for
one not there."
  (mapcar (lambda (type)
            (let* ((path (tuatara.io:path-join directory (format nil "~a.~a" stem type)))
                   (stamp (tuatara.io:path-stamp path)))
              (and stamp (cons (tuatara.io:stamp-size stamp) (file-sha256 path)))))
          *latex-cross-reference-types*))

(defparameter *latex-log-tail* (* 1024 1024)
  "The most octets of a LaTeX program's log read after a run: its last ones,
where it says what went wrong and whether to run it again.")

(defun latex-log-text (log)
  "The end of LOG, a LaTeX program's log, as text: its last *LATEX-LOG-TAIL*
octets, from the first line that begins within them; an empty string when
there is none."
  (or (ignore-errors
       (tuatara.io:call-with-file
        log
        (lambda (fd path size)
          (let* ((from (max 0 (- size *latex-log-tail*)))
                 (octets (make-array (- size from) :element-type '(unsigned-byte 8)))
                 (text (tuatara.text:lenient-string octets :end (tuatara.io:read-fd-at fd octets from :path path))))
            (if (plusp from)
                (subseq text (1+ (or (position #\Newline text) (1- (length text)))))
                text)))))
      ""))

(defun latex-errors (log)
  "The errors in LOG, a LaTeX program's log, in order, at most
*LATEX-ERROR-LINES* of them: each a plist of its :MESSAGE, the :FILE and
:LINE it names (FILE:LINE: lines, or an l.LINE line after a ! line, for the
file being made), and the two lines after it, :CONTEXT; not those that only
say it stopped (Emergency stop)."
  (let* ((lines (coerce (uiop:split-string log :separator '(#\Newline)) 'vector))
         (found '()))
    (loop for i from 0 below (length lines)
          for line = (aref lines i)
          for context = (loop for j from (1+ i) below (min (length lines) (+ i 3)) collect (aref lines j))
          do (multiple-value-bind (match parts) (cl-ppcre:scan-to-strings "^(.*?\\.(?:tex|sty|cls|def|cfg)):([0-9]+): (.*)$" line)
               (cond (match
                      (push (list :file (aref parts 0) :line (parse-integer (aref parts 1)) :message (aref parts 2) :context context)
                            found))
                     ((and (plusp (length line)) (char= #\! (char line 0)))
                      (let ((at (loop for j from (1+ i) below (min (length lines) (+ i 12))
                                      for l = (aref lines j)
                                      when (cl-ppcre:scan "^l\\.[0-9]+" l)
                                        return (parse-integer l :start 2 :junk-allowed t))))
                        (push (list :file nil :line at :message (string-left-trim "! " line) :context context) found))))))
    ;; what only says that it stopped, after an error said already
    (last (remove-if (lambda (problem) (cl-ppcre:scan "^\\s*(?:==> Fatal error occurred|Emergency stop)" (getf problem :message)))
                     (nreverse found))
          *latex-error-lines*)))

(defun rerun-asked-p (log)
  "True when LOG, a LaTeX program's log, says it should be run again."
  (some (lambda (words) (search words log))
        '("Rerun to get" "Please rerun" "Label(s) may have changed" "Rerun LaTeX")))

(defun latex-program-place (path)
  "The LaTeX program at PATH named by where its folder really is, every link
to the folder followed, under the name it was found by (xelatex, which may
itself be a link to another program that reads its name): so that it, its
folder and the folders of TeX kpsewhich names agree. PATH as it is when its
folder is not there."
  (let ((folder (ignore-errors (tuatara.io:real-path (tuatara.io:parent-path path)))))
    (if folder (tuatara.io:path-join folder (tuatara.io:native-file-name path)) path)))

(defun run-latex (job tex engine path)
  "Make a PDF of the LaTeX file TEX with the program ENGINE at PATH (by where
its folder really is, LATEX-PROGRAM-PLACE), for JOB, running it until its
cross-references settle, at most *LATEX-RUNS* times, in
LATEX-BUILD-DIRECTORY, where its working files are kept: the files TEX names
\(its pictures, files it takes in) are found in TEX's directory, which it may
not write in, so that a document cannot change the files beside it. Tried once
more without them when it fails with some there, since those of an earlier
version can trip it. Returns the PDF made, in that directory, or NIL and what
went wrong, as LATEX-ERRORS gives it.
The program is shut in (RUN-SHUT-IN): what a document says, in LaTeX or in
Lua, can read only the folders it is shown and write only in that directory,
with no network. Where there is no sandbox (bwrap missing, or turned off by
*sandbox-program*), it is run not shut in only when you have said yes
\(LATEX-UNCONFINED-ALLOWED-P), and the export says so when it is done
\(LATEX-UNCONFINED-WORDS): it then has your rights, but for TeX's own limits
\(no programs run, nothing written outside that directory)."
  (let* ((path (latex-program-place path))
         (directory (latex-build-directory tex engine))
         (stem (tuatara.io:native-file-stem tex))
         (log (tuatara.io:path-join directory (concatenate 'string stem ".log")))
         (pdf (tuatara.io:path-join directory (concatenate 'string stem ".pdf")))
         (shut-in (multiple-value-bind (status detail) (tuatara.io:sandbox-status)
                    (or (eq status :ready)
                        (progn (pushnew (latex-unconfined-words detail) (pdf-export-notes job) :test #'string=)
                               nil))))
         ;; the program's own directory first on the PATH, for the programs it runs in turn
         ;; (xdvipdfmx); TEX's directory searched for what it names; what it writes kept to
         ;; where it runs, never a parent directory or a full name elsewhere
         (environment (list (format nil "PATH=~a:~a" (string-right-trim "/" (tuatara.io:parent-path path))
                                    (tuatara.io:program-search-path))
                            (format nil "TEXINPUTS=~a:" (tuatara.io:parent-path tex))
                            "openout_any=p"))
         (arguments (list "-interaction=nonstopmode" "-halt-on-error" "-file-line-error" "-no-shell-escape"
                          (concatenate 'string "-output-directory=" directory)
                          ;; its full name, which a name beginning with - cannot be taken for an option in
                          tex)))
    (flet ((attempt ()
             (ignore-errors (tuatara.io:remove-file pdf))
             (loop for run from 1 to *latex-runs*
                   do (setf (pdf-export-step job) (format nil "running ~a~:[~*~; (~:r time)~]" engine (> run 1) run))
                      (let ((before (cross-references directory stem)))
                        (multiple-value-bind (ended how)
                            (if shut-in
                                (run-shut-in job tex path arguments directory)
                                (run-unconfined job path arguments directory environment))
                          (let ((said (latex-log-text log)))
                            (unless (and ended (tuatara.io:path-stamp pdf))
                              (return (values nil (or (latex-errors said)
                                                      (list (list :message (format nil "~a~:[ and wrote no log~;~]"
                                                                                   (or how (format nil "~a made no PDF" engine))
                                                                                   (plusp (length said)))))))))
                            (unless (or (rerun-asked-p said) (not (equalp before (cross-references directory stem))))
                              (return pdf)))))
                   finally (return pdf))))
      (let ((had-files (some #'identity (cross-references directory stem))))
        (multiple-value-bind (made why) (attempt)
          (if (or made (not had-files))
              (values made why)
              (progn (dolist (entry (ignore-errors (tuatara.io:directory-entries directory)))
                       (ignore-errors (tuatara.io:delete-tree (tuatara.io:path-join directory (first entry)))))
                     (attempt))))))))

(defun put-pdf (made target)
  "Put the PDF MADE at TARGET, in place of what is there in one step: a view
of TARGET reads either the old PDF or the new, never half of one."
  (let ((temporary (tuatara.io:scratch-name (tuatara.io:parent-path target) "pdf")))
    (unless (tuatara.io:copy-regular-file made temporary)
      (ignore-errors (tuatara.io:remove-file temporary))
      (error "~a could not be written" target))
    (handler-case (tuatara.io:rename-path temporary target :replace t)
      (error (e) (ignore-errors (tuatara.io:remove-file temporary)) (error e)))))

(defun error-place (job problem)
  "Where PROBLEM, one of LATEX-ERRORS, is in what JOB was asked to make a PDF
of, as two values, the file and the line: for an Org file, the line its
LaTeX came from (its MAP: the element's first line, counted on as far as the
error is past the element's first line in the LaTeX, never past its last),
or line 1 for the preamble; for a LaTeX file, its own; NIL for one in
another file, or with no line."
  (let* ((line (getf problem :line))
         (file (getf problem :file))
         (tex (pdf-export-tex job))
         (ours (and line tex (or (null file) (string= (tuatara.io:native-file-name file) (tuatara.io:native-file-name tex))))))
    (cond ((not ours) nil)
          ((and (eq (pdf-export-kind job) :org) (pdf-export-map job))
           (let* ((map (pdf-export-map job))
                  (entry (find-if (lambda (e) (<= (first e) line)) map :from-end t))
                  (text (pdf-export-text job)))
             (flet ((line-at (position) (1+ (count #\Newline text :end (min (length text) position)))))
               (let ((at (if entry
                             ;; an element's lines go to LaTeX one for one, as near as can be told: counted on, within it
                             (destructuring-bind (marked begin end) entry
                               (min (line-at end) (+ (line-at begin) (- line marked))))
                             1))
                     (origins (pdf-export-origins job)))
                 ;; a part exported alone: the line of the file its line comes from
                 (values (pdf-export-source job)
                         (if origins (aref origins (min (1- (length origins)) (max 0 (1- at)))) at))))))
          (t (values tex line)))))

(defun missing-characters-words (job)
  "What to say of what JOB made besides that it is written: the characters no
font here has, in words with the place of each in the file written
\(ERROR-PLACE), and its notes (a picture not shown); NIL when there is none."
  (let ((missing (pdf-export-missing job))
        (notes (pdf-export-notes job)))
    (when (or missing notes)
      (format nil "~:[~*~;no font here has ~{~a~^, ~}~]~:[~;; ~]~{~a~^; ~}" missing
              (loop for (c . line) in missing
                    collect (multiple-value-bind (file at) (error-place job (list :line line))
                              (format nil "~c (U+~4,'0x~@[, ~a~]~@[:~d~])" c (char-code c)
                                      (and file (tuatara.io:native-file-name file)) (and file at))))
              (and missing notes) notes))))

(defun error-words (job problem)
  "PROBLEM, one of LATEX-ERRORS, in a line: where it is (ERROR-PLACE), then its message."
  (multiple-value-bind (file line) (error-place job problem)
    (format nil "~@[~a~]~@[:~d~]~:[~;: ~]~a" (and file (tuatara.io:native-file-name file)) (and file line) file
            (getf problem :message))))

(defun export-work (job)
  "What JOB's thread does: wait for the save of its buffer, write an Org
buffer as LaTeX, and make the PDF of the LaTeX with each of the programs for
it in turn until one does, put beside the LaTeX file."
  (let ((save (pdf-export-save job)))
    (when save
      (setf (pdf-export-step job) "waiting for the save")
      (sb-thread:join-thread (tuatara.io:save-job-thread save) :default nil)
      (unless (eq :done (tuatara.io:save-job-status save))
        (error "~a was not saved" (export-name job)))))
  (let* ((tex (if (eq (pdf-export-kind job) :org)
                  (org-to-latex job (or (car (first (latex-engines-found))) "xelatex"))
                  (pdf-export-source job)))
         (target (replaced-type tex "pdf")))
    (setf (pdf-export-tex job) tex)
    (when (eq (pdf-export-then job) :latex-only)
      (return-from export-work))
    (dolist (engine (engines-for tex))
      (multiple-value-bind (made errors) (run-latex job tex (car engine) (cdr engine))
        (when made
          (setf (pdf-export-step job) "writing the PDF")
          (put-pdf made target)
          (setf (pdf-export-pdf job) target (pdf-export-engine job) (car engine))
          (return-from export-work))
        (push (car engine) (pdf-export-failed job))
        (setf (pdf-export-errors job)
              (append (pdf-export-errors job) (list (cons (car engine) errors))))))
    ;; the first problem the first program met, where it is in what was written
    (let ((first (first (cdr (first (pdf-export-errors job))))))
      (error "~{~a~^ and ~} failed~@[ (~a)~]; *LaTeX errors* has what they said"
             (reverse (pdf-export-failed job)) (and first (error-words job first))))))

(defun show-buffer-place (buffer position)
  "Show BUFFER in another window, selected, point at POSITION, its line in the
middle; widened first, and that said, when POSITION is outside the part of it
shown."
  (pop-to-buffer buffer :other t)
  (when (eq buffer (tuatara.display:window-buffer (current-window)))
    (goto-shown buffer (min position (tuatara.buffer:buffer-size buffer)))
    (tuatara.display:window-recenter (current-window))))

(defun show-place-beside (buffer position stay)
  "Show BUFFER in another window, point at POSITION, its line in the middle:
that window selected, or with STAY the selected window kept."
  (let ((here (current-window)))
    (show-buffer-place buffer position)
    (when (and stay (member here (editor-windows)))
      (select-window here))))

(defun visit-problem (file line &key stay)
  "Show FILE at LINE in another window, selected, or with STAY the selected
window kept."
  (let ((buffer (visit-path file :show (lambda (buffer) (declare (ignore buffer))))))
    (when buffer
      (show-place-beside buffer (tuatara.buffer:buffer-goto-line buffer (1- line)) stay))))

(defun show-latex-errors (job)
  "Show what the programs of JOB that failed said, in *LaTeX errors*, a list
of the problems under the name of the program that met them: each where it
is in the file written (ERROR-PLACE), with the line's text, then the lines
LaTeX printed with it."
  (let ((org-lines (or (pdf-export-lines job)
                       (and (pdf-export-text job) (uiop:split-string (pdf-export-text job) :separator '(#\Newline))))))
    (multiple-value-bind (pieces entries)
        (entry-lines
         (lambda (put)
           (loop for (engine . errors) in (pdf-export-errors job)
                 do (funcall put "")
                    (funcall put (format nil "~a failed:" engine) :heading (list :engine engine))
                    (dolist (problem errors)
                      (multiple-value-bind (file line) (error-place job problem)
                        ;; one entry, the problem, on each of its lines
                        (let ((entry (list :error file line)))
                          (funcall put (format nil "  ~@[~a: ~]~a" (and file (format nil "~a:~d" (tuatara.io:native-file-name file) line))
                                               (getf problem :message))
                                   nil entry)
                          ;; the Org line it came from, as written
                          (when (and file (eq (pdf-export-kind job) :org) (equal file (pdf-export-source job)))
                            (funcall put (format nil "    | ~a" (or (nth (1- line) org-lines) "")) nil entry))
                          (dolist (context (getf problem :context))
                            (funcall put (format nil "    ~a" context) nil entry))))))))
      (let ((list (set-search-filtering (buffer-in-mode "*LaTeX errors*" 'latex-errors-mode)
                                        :nouns "errors" :heading (lambda (entry) (eq (first entry) :engine)))))
        (render-pieces list (cons (format nil "Making a PDF of ~a~@[, written as LaTeX in ~a~]~%"
                                          (abbreviated-path (pdf-export-source job))
                                          (and (eq (pdf-export-kind job) :org) (pdf-export-tex job)
                                               (abbreviated-path (pdf-export-tex job))))
                                  pieces)
                       :entries entries :first-line 1 :point :start)
        (display-buffer list)))))

(defun show-exported-pdf (pdf)
  "Show PDF in its view where the rule for :exported buffers puts it (beside,
or alone on a narrow screen), the focus staying where it is."
  (pdf-view-visit pdf (lambda (buffer)
                        (setf (tuatara.buffer:buffer-local buffer :kind) :exported)
                        (display-buffer buffer))))

(defmethod tuatara.runtime:job-take-in ((job pdf-export))
  "Say how the export went: the LaTeX written for an export to LaTeX alone;
else read again the views of the PDF open, show or open the PDF as asked,
show what the programs said when none made it."
  (let ((pdf (pdf-export-pdf job)))
    ;; an export to LaTeX alone is done once the LaTeX is written
    (when (and (eq (pdf-export-status job) :done) (eq (pdf-export-then job) :latex-only))
      (message "Wrote ~a~@[; ~a~]" (abbreviated-path (pdf-export-tex job)) (missing-characters-words job))
      (return-from tuatara.runtime:job-take-in))
    (case (pdf-export-status job)
      (:done
       (dolist (buffer (copy-list *pdf-views*))
         (let ((view (pdf-view-of buffer)))
           (when (and view (equal (pdf-view-path view) pdf))
             (pdf-view-reload buffer))))
       (message "Wrote ~a (~a~@[; ~{~a~^ and ~} failed~]) in ~,1f s~@[; ~a~]" (abbreviated-path pdf) (pdf-export-engine job)
                (reverse (pdf-export-failed job))
                (tuatara.runtime:seconds-since (pdf-export-started job))
                (missing-characters-words job))
       (case (pdf-export-then job)
         (:view (show-exported-pdf pdf))
         (:outside (open-externally-file pdf))))
      (:stopped (message "~a" (tuatara.runtime:job-ending-words job)))
      (:failed
       (message "~a" (tuatara.runtime:job-ending-words job))
       (when (pdf-export-errors job)
         (show-latex-errors job))))))

;;; The commands

(defun export-kind (buffer)
  "What BUFFER's file is to be exported as, by its major mode: :ORG or :LATEX; NIL for neither."
  (cond ((buffer-mode-named-p buffer 'org-mode) :org)
        ((buffer-mode-named-p buffer 'latex-mode) :latex)))

(defparameter *enhanced-commands*
  '((:file . org-export-latex-pdf-enhanced) (:view . org-export-latex-pdf-enhanced-buffer)
    (:outside . org-export-latex-pdf-enhanced-xdg) (:latex-only . org-export-latex-enhanced))
  "The enhanced export's command for each THEN of START-LATEX-EXPORT.")

(defun note-directory (buffer)
  "The folder of BUFFER's file (BUFFER-DIRECTORY), or NIL for a buffer of no file."
  (and (tuatara.buffer:buffer-file buffer) (buffer-directory buffer)))

(defun only-enhanced-macro (text &optional directory)
  "The name of a macro of TEXT, an Org document, that only the enhanced export
knows, redact or protect (*ORG-HIDING-MACROS*), used with no #+MACRO: line
for it, its setup files' read too (DIRECTORY the note's folder), as written;
NIL when none."
  (let ((defined nil) (read nil))
    (cl-ppcre:do-register-groups (name) (*org-hiding-call-re* text)
      (unless read
        (setf defined (with-org-note-keywords (text directory) (org-macro-definitions)) read t))
      (unless (assoc name defined :test #'string-equal)
        (return-from only-enhanced-macro name)))))

(defun org-export-part (buffer scope)
  "The part of BUFFER, an Org buffer, to export alone for SCOPE, :SELECTION
\(the lines the selection is on) or :SUBTREE (the subtree at point, its
headline the title, its EXPORT_TITLE and EXPORT_FILE_NAME properties used),
as four values: its text, the document's keyword lines put before it, as
whole lines of the file; the line of the file each of its lines comes from,
a vector; the file's lines; and words for it (the subtree \"Findings\").
NIL when there is no such part (no headline at point)."
  (multiple-value-bind (tree text) (org-buffer-tree buffer)
    (let* ((lines (coerce (uiop:split-string text :separator '(#\Newline)) 'vector))
           (starts (let ((v (make-array (length lines))) (at 0))
                     (dotimes (i (length lines) v)
                       (setf (aref v i) at)
                       (incf at (1+ (length (aref lines i)))))))
           (line-of (lambda (position) (max 0 (1- (or (position-if (lambda (start) (> start position)) starts) (length lines))))))
           ;; at the very end of the text, its last character's headline
           (point (min (org-byte->char buffer (tuatara.buffer:buffer-point buffer)) (max 0 (1- (length text)))))
           (headline (and (eq scope :subtree) (org-current-headline tree point)))
           (range (ecase scope
                    (:selection (multiple-value-bind (start end) (selection-range)
                                  (and start (cons (org-byte->char buffer start) (org-byte->char buffer (max start (1- end)))))))
                    (:subtree (and headline (cons (org-node-begin headline)
                                                  (max (org-node-begin headline)
                                                       (1- (- (org-node-end headline) (org-node-post-blank headline))))))))))
      (when range
        (let* ((first (funcall line-of (car range)))
               (last (max first (funcall line-of (cdr range))))
               (kept '()) (origins '()))
          (flet ((keep (line text) (push text kept) (push (1+ line) origins)))
            ;; the document's keywords, but for its title when the subtree gives one
            (org-walk-tree tree (lambda (node)
                                   (when (and (eq (org-node-type node) 'keyword)
                                              (not (and headline (string-equal (org-property node :key) "TITLE")))
                                              (not (<= first (funcall line-of (org-node-begin node)) last)))
                                     (let ((line (funcall line-of (org-node-begin node))))
                                       (keep line (aref lines line))))))
            (when headline
              (let ((line (funcall line-of (org-node-begin headline))))
                (keep line (format nil "#+title: ~a" (or (org-headline-property headline "EXPORT_TITLE")
                                                         (org-property headline :raw-value))))
                (let ((name (org-headline-property headline "EXPORT_FILE_NAME")))
                  (when name (keep line (format nil "#+export_file_name: ~a" name))))))
            (loop for line from first to last do (keep line (aref lines line))))
          (values (format nil "~{~a~%~}" (reverse kept))
                  (coerce (reverse origins) 'simple-vector)
                  lines
                  (if headline
                      (format nil "the subtree \"~a\"" (org-property headline :raw-value))
                      "the selection")))))))

(defun export-refused-p (buffer text &key wrong-kind running redactions)
  "True, having said why, when BUFFER, whose text is TEXT, is not to be
exported now. Looked at in turn: opened safely; WRONG-KIND, the words to say
when it is not of a kind the export takes; no file yet; RUNNING, the words
to say when an export of it is running already; and with REDACTIONS, a
hidden text shown elsewhere in it, unless you say to go on."
  (let ((name (tuatara.buffer:buffer-name buffer)))
    ;; a file opened safely has no major mode: said as what it is, before its kind
    (cond ((tuatara.io:safe-visit-p buffer)
           (message "~a was opened safely: exporting would run what it says (\\[end-safe-visit] opens it as usual)" name))
          (wrong-kind (message "~a" wrong-kind))
          ((null (tuatara.buffer:buffer-file buffer)) (message "~a has no file yet: save it first" name))
          (running (message "~a" running))
          ;; a hidden text shown elsewhere in the file: listed, and asked about, before anything is made
          ((and redactions (not (redactions-allow-export-p buffer text)))
           (message "Not exported: *Redactions* lists where hidden text shows"))
          (t (return-from export-refused-p nil)))
    t))

(defun start-latex-export (then &key enhanced)
  "Start making a PDF of the current buffer's Org text, or of its LaTeX file,
saved first when it has changes, and then do THEN with it (PDF-EXPORT), with
ENHANCED Tuatara's own improvements on Org's LaTeX (Org files only); or say
why not."
  (let* ((buffer (current-buffer))
         (name (tuatara.buffer:buffer-name buffer))
         (file (tuatara.buffer:buffer-file buffer))
         (kind (export-kind buffer))
         (text (and kind (tuatara.buffer:buffer-string buffer)))
         (running (find-if #'pdf-export-p (jobs-of buffer :running t))))
    (cond ((export-refused-p buffer text
                             :wrong-kind (and (null kind) (format nil "~a is neither an Org nor a LaTeX file: nothing to export" name))
                             :running (and running (format nil "~a is being ~(~a~) already (~a)" name
                                                           (if (eq kind :latex) "compiled" "exported") (pdf-export-step running)))
                             :redactions (or (eq kind :latex) enhanced)))
          ;; names to hide, which the export as Org makes it would refuse in the background: said at once
          ((and (eq kind :org) (not enhanced)
                (let ((macro (only-enhanced-macro text (note-directory buffer))))
                  (when macro
                    (message "~a hides text with {{{~a(...)}}}, which only the enhanced export does: M-x ~(~a~)"
                             name macro (cdr (assoc then *enhanced-commands*)))
                    t))))
          ((and (not (eq then :latex-only)) (null (latex-engines-found)))
           (message "No program makes PDFs here: none of ~{~a~^, ~} was found on the PATH or in TeX Live's folders (install TeX Live, or give their paths in *latex-engines*)"
                    (latex-engine-names)))
          ((and (not (eq then :latex-only)) (not (latex-unconfined-allowed-p)))
           (message "Not exported: LaTeX runs only shut in the sandbox unless you say yes"))
          (t
           ;; an Org buffer is written as LaTeX as it is; a LaTeX file is read from disk, so saved first
           (when (and (eq kind :latex) (tuatara.buffer:buffer-modified-p buffer))
             (save-buffer-with-care buffer))
           (let ((save (and (eq kind :latex) (tuatara.io:save-of buffer))))
             (if (and (eq kind :latex) (tuatara.buffer:buffer-modified-p buffer) (null save))
                 (message "Not exported: ~a has changes not saved" name)
                 (let* ((scope (and (eq kind :org)
                                    (cond ((region-active-p buffer) :selection)
                                          ((prefix-argument) :subtree))))
                        (job (make-pdf-export buffer (tuatara.io:native (tuatara.io:expand-path file)) kind then save
                                              (and (eq kind :org) (tuatara.buffer:buffer-text buffer)) enhanced)))
                   ;; a part alone: its text, and where each of its lines is in the file
                   (unless (or scope (export-target-allowed-p buffer '("tex" "pdf") :text text))
                     (message "Not exported")
                     (return-from start-latex-export))
                   (when scope
                     (multiple-value-bind (text origins lines words) (org-export-part buffer scope)
                       (unless text
                         (message "There is no headline here to export the subtree of")
                         (return-from start-latex-export))
                       ;; where the part says it is written: its headline's EXPORT_FILE_NAME too
                       (unless (export-target-allowed-p buffer '("tex" "pdf") :text text)
                         (message "Not exported")
                         (return-from start-latex-export))
                       (setf (pdf-export-rope job) text (pdf-export-origins job) origins
                             (pdf-export-lines job) (coerce lines 'list) (pdf-export-part job) words)))
                   (tuatara.runtime:start-job-thread
                    job (tuatara.runtime:carry-settings *latex-carried-settings* #'export-work)
                    :name "tuatara export" :details (format nil "~(~a~), then ~(~a~)~:[~;, enhanced~]" kind then enhanced))
                   (message "~a ~@[~a of ~]~a to ~a..." (export-doing kind) (pdf-export-part job) name
                            (if (eq then :latex-only) "LaTeX" "PDF")))))))))

(define-command org-export-latex-pdf ()
  "Make the Org file of the current buffer into a PDF beside it, in the
background: the plain export, the note set as standard LaTeX sets it, with
no template or layout of Tuatara's own. The buffer is written as
LaTeX first, as it is in the buffer, beside its file (or where its
#+export_file_name says), with its #+setupfile, #+latex_header, #+options
and the rest; its code blocks are never run. The LaTeX is made into a PDF by
xelatex, or by lualatex when xelatex fails (*latex-engines*), run as often as
its cross-references need. Their working files are kept apart, so the
directory gets only the .tex and the .pdf. When no program makes it,
*LaTeX errors* shows what they said, each error at the line of your file it
comes from, \\<latex-errors-mode>\\[list-visit] there showing that line. A
view of the PDF already open is read again; the PDF is not shown otherwise:
\\[org-export-latex-pdf-buffer] shows it, \\[org-export-latex-pdf-xdg] opens it
in the desktop's program. \\[org-export-latex-pdf-enhanced] makes it with
Tuatara's own improvements. With text selected, only the
lines it is on are exported, under the document's keywords; with
\\[universal-argument] first, only the subtree at point, its headline the
title (or its EXPORT_TITLE property), its EXPORT_FILE_NAME property, when it
has one, naming the file. So for every command making a PDF or LaTeX of an
Org file."
  (start-latex-export :file))

(define-command org-export-latex-pdf-buffer ()
  "Make the Org file of the current buffer into a PDF beside it, as
\\[org-export-latex-pdf] does, and show the PDF: down the right side of the
screen when it is wide enough (*split-width-threshold* columns), else alone
on it. The focus stays in the file, and making it again reads the PDF shown
again, at the same page. Where it shows is the rule for :exported buffers:
\(place-buffers :exported :right-or-alone) as standard."
  (start-latex-export :view))

(define-command org-export-latex-pdf-xdg ()
  "Make the Org file of the current buffer into a PDF beside it, as
\\[org-export-latex-pdf] does, and open the PDF in the desktop's program for
PDFs (xdg-open), or the one *open-externally-programs* gives
\"application/pdf\", asking first unless *open-externally-ask* is NIL."
  (start-latex-export :outside))

(define-command org-export-latex ()
  "Write the Org file of the current buffer as LaTeX beside it (or where its
#+export_file_name says), the plain export with no template or layout of
Tuatara's own, in the background, without making a PDF: the LaTeX file \\[org-export-latex-pdf]
makes its PDF from. No LaTeX program is needed for it."
  (start-latex-export :latex-only))

(define-command org-export-latex-enhanced ()
  "Write the Org file of the current buffer as LaTeX as
\\[org-export-latex] does, with Tuatara's own improvements (its
#+template, tables that fit, redaction): the LaTeX file
\\[org-export-latex-pdf-enhanced] makes its PDF from."
  (start-latex-export :latex-only :enhanced t))

(define-command org-export-latex-pdf-enhanced ()
  "Make the Org file of the current buffer into a PDF beside it, as
\\[org-export-latex-pdf] does, with Tuatara's own improvements: its
#+template, tables that fit and redaction (\\[org-export-latex-enhanced])."
  (start-latex-export :file :enhanced t))

(define-command org-export-latex-pdf-enhanced-buffer ()
  "Make the Org file of the current buffer into a PDF as
\\[org-export-latex-pdf-enhanced] does, and show it as
\\[org-export-latex-pdf-buffer] shows its PDF."
  (start-latex-export :view :enhanced t))

(define-command org-export-latex-pdf-enhanced-xdg ()
  "Make the Org file of the current buffer into a PDF as
\\[org-export-latex-pdf-enhanced] does, and open it in the desktop's program
as \\[org-export-latex-pdf-xdg] opens its PDF."
  (start-latex-export :outside :enhanced t))

(define-command latex-compile-pdf ()
  "Make the LaTeX file of the current buffer into a PDF beside it, in the
background, saving it first when it has changes: by xelatex, or by lualatex
when xelatex fails (*latex-engines*), or first by the one its first lines
name (\"% Intended LaTeX compiler: lualatex\", \"%!TEX program = lualatex\"),
run as often as its cross-references need. Their working files are kept
apart, so the directory gets only the .pdf. When no program makes it,
*LaTeX errors* shows what they said, each error at its line,
\\<latex-errors-mode>\\[list-visit] there showing that line. A view of the PDF
already open is read again; the PDF is not shown otherwise:
\\[latex-compile-pdf-buffer] shows it, \\[latex-compile-pdf-xdg] opens it in the
desktop's program."
  (start-latex-export :file))

(define-command latex-compile-pdf-buffer ()
  "Make the LaTeX file of the current buffer into a PDF beside it, as
\\[latex-compile-pdf] does, and show the PDF: down the right side of the
screen when it is wide enough (*split-width-threshold* columns), else alone
on it. The focus stays in the file, and making it again reads the PDF shown
again, at the same page. Where it shows is the rule for :exported buffers."
  (start-latex-export :view))

(define-command latex-compile-pdf-xdg ()
  "Make the LaTeX file of the current buffer into a PDF beside it, as
\\[latex-compile-pdf] does, and open the PDF in the desktop's program for
PDFs (xdg-open), or the one *open-externally-programs* gives
\"application/pdf\", asking first unless *open-externally-ask* is NIL."
  (start-latex-export :outside))

(define-command copy-latex-template ()
  "Copy a built-in template of the enhanced export, asked for with completion
\(article, its own look, or org, the plain export's document), into your
own templates, ~/.config/tuatara/latex-templates/NAME.tex, and visit it there
to change: from then on #+template: NAME takes your copy. A copy of yours
already there is visited as it is, never written over. In a template,
{{{KEY}}} is where the value of a note's #+KEY: line goes; the template's
first lines say which it takes. A template of your own for another kind of
document (a report) can start from one of these, kept there or anywhere a
note's #+template: names by its path."
  (copy-chosen-own-file "Copy the export template: "
                        (loop for (name . text) in *built-in-latex-templates*
                              collect (make-candidate name :value (list nil (concatenate 'string name ".tex") text)))
                        (list (latex-templates-directory))
                        "export template"))

;;; Word documents

(defstruct (docx-export (:include tuatara.runtime:job) (:constructor make-docx-export (buffer text file then)) (:copier nil))
  "An Org note being written as a Word document for BUFFER, a job: TEXT the
note's, FILE its file; THEN :FILE or :OUTSIDE (opened in the desktop's
program after); PATH, the .docx written; NOTES, what else to say of it."
  text file then path notes)

(setf (documentation 'docx-export-p 'function) "True when OBJECT is a DOCX-EXPORT.")

(tuatara.runtime:define-job-words docx-export
  :doing "Writing the Word document"
  :description (format nil "Writing ~a as a Word document" (tuatara.io:native-file-name (docx-export-file job)))
  :verb "write the Word document"
  :left-words "no Word document written")
(defmethod tuatara.runtime:job-holds-queue-p ((job docx-export))
  "A Word export holds back no other work on its buffer: it only reads the note."
  nil)

(defmethod tuatara.runtime:job-take-in ((job docx-export))
  "Say where the Word document went and what else there is to say of it; open
it in the desktop's program when asked."
  (case (docx-export-status job)
    (:done (let ((path (docx-export-path job)))
             (message "Wrote ~a~{; ~a~}" (abbreviated-path path) (docx-export-notes job))
             (when (eq (docx-export-then job) :outside) (open-externally-file path :also '(:docx)))))
    (t (message "~a" (tuatara.runtime:job-ending-words job)))))

(defun start-docx-export (then)
  "Start writing the current buffer's Org note as a Word document beside it
\(or where #+EXPORT_FILE_NAME says), then do THEN with it; or say why not."
  (let* ((buffer (current-buffer))
         (name (tuatara.buffer:buffer-name buffer))
         (file (tuatara.buffer:buffer-file buffer))
         (org (eq (export-kind buffer) :org))
         (text (and org (tuatara.buffer:buffer-string buffer))))
    (cond ((export-refused-p buffer text
                             :wrong-kind (and (not org)
                                              (format nil "~a is no Org note: nothing to write as a Word document" name))
                             :running (and (find-if #'docx-export-p (jobs-of buffer :running t))
                                           (format nil "~a is being written as a Word document already" name))
                             :redactions t))
          ((not (export-target-allowed-p buffer '("docx") :text text))
           (message "Not exported"))
          (t
           (let ((job (make-docx-export buffer text (tuatara.io:native (tuatara.io:expand-path file)) then)))
             (tuatara.runtime:start-job-thread
              job (lambda (job)
                    (multiple-value-bind (octets named notes) (org-docx-document (docx-export-text job) :file (docx-export-file job))
                      (let ((path (or named (replaced-type (docx-export-file job) "docx"))))
                        (check-export-folder path)
                        (tuatara.io:replace-file path octets)
                        (setf (docx-export-path job) path (docx-export-notes job) notes))))
              :name "tuatara docx export")
             (message "Writing ~a as a Word document..." name))))))

(define-command org-export-docx ()
  "Write the Org note of this buffer as a Word document (.docx) beside it, or
where #+EXPORT_FILE_NAME says (its .docx): read as the enhanced PDF export
reads it, its :EXPORT: drawers' settings and all, what redact and protect hide
a black bar, never its text, in the file; in the background."
  (start-docx-export :file))

(define-command org-export-docx-xdg ()
  "Write the Org note of this buffer as a Word document, as \\[org-export-docx]
does, and open it in the desktop's program for Word documents."
  (start-docx-export :outside))

(define-major-mode latex-errors-mode special-mode
  "*LaTeX errors*: what the programs making a PDF said when none made it,
each error at the line of your file it comes from, under the name of the
program that met it. \\<latex-errors-mode>\\[list-visit] on an error shows that
line in another window, \\[list-visit-other] the list staying selected;
\\[list-next-entry] and \\[list-previous-entry] go from one error to the next;
\\[search-filter-regex] narrows the list to the errors whose words match;
\\`q' closes."
  :lighter "LaTeX errors" :interactive nil)

(defmethod visit-entry ((mode latex-errors-mode) list entry other)
  "Show the line of the file the error on the line comes from in another
window, selected, or with OTHER the list kept selected."
  (declare (ignore list))
  (destructuring-bind (kind &optional file line) entry
    (cond ((not (eq kind :error)) (message "No error on this line"))
          ((null file) (message "LaTeX said nothing of where this error is"))
          (t (visit-problem file line :stay other)))))

(defun install-latex-export ()
  "Bind the keys of *LaTeX errors*; offer the commands making PDFs of
Org files in Org buffers, those of LaTeX files in LaTeX buffers; and make
the standard rule of where exported PDFs are shown: down the right side of
the screen when it is wide enough, else alone on it."
  (entry-list-keys 'latex-errors-mode "*LaTeX errors*")
  (commands-for-mode 'org-mode "Org files" 'org-export-latex 'org-export-latex-enhanced 'org-export-latex-pdf 'org-export-latex-pdf-buffer 'org-export-latex-pdf-xdg
                     'org-export-latex-pdf-enhanced 'org-export-latex-pdf-enhanced-buffer 'org-export-latex-pdf-enhanced-xdg)
  (commands-for-mode 'latex-mode "LaTeX files" 'latex-compile-pdf 'latex-compile-pdf-buffer 'latex-compile-pdf-xdg)
  (commands-for-mode 'org-mode "Org files" 'org-insert-export-drawer 'org-export-docx 'org-export-docx-xdg)
  (place-buffers :exported :right-or-alone))
