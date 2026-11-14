;;;; test/definitions.lisp --- Every function is defined once in its package.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test definitions
  :description "No function, macro or command is defined twice in one package, where the later would quietly replace the earlier; no two tests have one name; the core asks the host, never a variable holding one function; the octets of a mapped file are read through the text layer alone; no source file names a package of a higher layer; no source file holds a tab character or ends a line with a backslash; all of the editor's own Lisp parses.")

(defun source-files (top)
  "Every Lisp source file under the directory TOP of the repository (\"lisp\", \"src\")."
  (directory (merge-pathnames (make-pathname :directory (list :relative top :wild-inferiors) :name :wild :type "lisp")
                              (asdf:system-source-directory "tuatara"))))

(defun core-source-files ()
  "Every Lisp source file of the core: the directories src, lisp and frontends."
  (loop for top in '("src" "lisp" "frontends") append (source-files top)))

(defun source-definitions ()
  "Every (PACKAGE NAME FILE) the source files define with DEFUN, DEFMACRO,
DEFGENERIC or DEFINE-COMMAND, the package read from each file's IN-PACKAGE."
  (let ((found '()))
    (dolist (file (core-source-files))
      (let ((text (uiop:read-file-string file)) (package nil))
        (cl-ppcre:do-register-groups (name) ("(?m)^\\(in-package #:([^)\\s]+)\\)" text)
          (setf package name))
        (cl-ppcre:do-register-groups (name) ("(?m)^\\((?:defun|defmacro|defgeneric|define-command) ([^\\s()]+)" text)
          (push (list package (string-downcase name) (file-namestring file)) found))))
    found))

(define-test (definitions defined-once)
  (let ((seen (make-hash-table :test 'equal)) (twice '()) (definitions (source-definitions)))
    ;; the sources were found and read
    (true (> (length definitions) 1000))
    (true (find '("tuatara.defaults" "split-selection") definitions :test #'equal
                :key (lambda (d) (list (string-downcase (first d)) (second d)))))
    (dolist (definition definitions)
      (destructuring-bind (package name file) definition
        ;; by the symbol's own package: a name another package exports, used here, is that package's
        (let* ((symbol (let ((p (find-package (string-upcase package)))) (and p (find-symbol (string-upcase name) p))))
               (package (if symbol (string-downcase (package-name (symbol-package symbol))) package))
               (key (list package name)))
          (when (and (gethash key seen) (string/= (gethash key seen) file))
            (push (format nil "~a::~a in ~a and ~a" package name (gethash key seen) file) twice))
          (setf (gethash key seen) file))))
    (true (null twice) "Defined twice: ~{~a~^; ~}" twice)))

(defun structure-accessors ()
  "Every slot reader of the structures Tuatara's packages define, as (PACKAGE NAME),
the package's name and the reader's name in lower case."
  (let ((found '()))
    (dolist (package (list-all-packages))
      (when (uiop:string-prefix-p "TUATARA" (package-name package))
        (do-symbols (symbol package)
          (when (eq (symbol-package symbol) package)
            (let ((description (sb-kernel:find-defstruct-description symbol nil)))
              (when description
                (dolist (slot (sb-kernel:dd-slots description))
                  (let ((accessor (sb-kernel:dsd-accessor-name slot)))
                    (when accessor
                      (pushnew (list (string-downcase (package-name (symbol-package accessor)))
                                     (string-downcase (symbol-name accessor)))
                               found :test #'equal))))))))))
    found))

(define-test (definitions core-asks-the-host)
  ;; what only the features know the core asks the host for, never a variable holding one function
  (let ((slots '()))
    (dolist (file (source-files "src"))
      (cl-ppcre:do-register-groups (name) ("(?m)^\\(def(?:var|parameter) (\\*[^\\s()]+-function\\*)"
                                           (uiop:read-file-string file))
        (push (format nil "~(~a~) in ~a" name (file-namestring file)) slots)))
    (is equal '() slots)
    (true (typep tuatara.runtime:*host* 'tuatara.defaults::defaults-host))))

(define-test (definitions accessors-kept)
  ;; a function defined with a structure's slot reader's name replaces the reader
  ;; for every caller, without a word
  (let ((accessors (structure-accessors))
        (clashes '()))
    (true (> (length accessors) 200))
    (true (find '("tuatara.defaults" "split-buffer") accessors :test #'equal))
    (dolist (definition (source-definitions))
      (destructuring-bind (package name file) definition
        (when (find (list (string-downcase package) name) accessors :test #'equal)
          (push (format nil "~a::~a in ~a" package name file) clashes))))
    (true (null clashes) "Defined over a structure's slot reader: ~{~a~^; ~}" clashes)))

(define-test (definitions structures-are-records)
  ;; a structure is written with DEFINE-RECORD, which keeps each slot's
  ;; documentation with the slot, never with a plain DEFSTRUCT
  (let ((plain '()) (records 0))
    (dolist (top '("src" "lisp" "frontends" "tools"))
      (dolist (file (source-files top))
        (let ((text (uiop:read-file-string file)))
          (incf records (length (cl-ppcre:all-matches-as-strings "(?m)^\\(define-record " text)))
          (when (cl-ppcre:scan "(?m)^\\s*\\(defstruct [^,\\s]" text)
            (push (file-namestring file) plain)))))
    (true (> records 250))
    (is equal '() plain)))

(define-test (definitions tests-named-once)
  ;; a test's name is its own whatever its parent: one defined again replaces the
  ;; first, which then never runs
  (let ((seen (make-hash-table :test 'equal)) (twice '()) (count 0))
    (dolist (file (directory (merge-pathnames "test/*.lisp" (asdf:system-source-directory "tuatara"))))
      (cl-ppcre:do-register-groups (parent child top)
                                   ("(?m)^\\(define-(?:modal-)?test (?:\\(([^\\s()]+) ([^\\s()]+)\\)|([^\\s()]+))"
                                    (uiop:read-file-string file))
        (declare (ignore parent))
        (incf count)
        (let ((name (string-downcase (or child top))))
          (when (gethash name seen)
            (push (format nil "~a in ~a and ~a" name (gethash name seen) (file-namestring file)) twice))
          (setf (gethash name seen) (file-namestring file)))))
    (true (> count 300))
    (true (null twice) "Tests named twice: ~{~a~^; ~}" twice)))

(define-test (definitions long-lines-listed)
  ;; report only: the columns are targets, so a count never fails the suite
  (let* ((root (namestring (asdf:system-source-directory "tuatara")))
         (output (uiop:run-program (list "python3" (concatenate 'string root "tools/line-check.py") "--summary" root)
                                   :output :string))
         (total (car (last (uiop:split-string (string-trim '(#\Newline) output) :separator '(#\Newline))))))
    (format t "~&~a~%List: tools/line-check.py ~a (without --summary, a line each)~%" total root)
    (true (search "lines past the column" total))))

(define-test (definitions sources-without-tabs)
  ;; a tab in the text a program writes is written as #\Tab or ~c, never typed into the source
  (let ((tabbed (remove-if-not (lambda (file) (find #\Tab (uiop:read-file-string file))) (core-source-files))))
    (false tabbed "~{~a~^, ~}" (mapcar #'file-namestring tabbed))))

(define-test (definitions no-backslash-before-line-end)
  ;; a backslash before the line end in a string escapes the newline, which then stays in the string: a long string
  ;; is split with CONCATENATE, never as C and shell join lines
  (let ((found '()))
    (dolist (file (append (core-source-files) (source-files "test")))
      (with-open-file (in file :external-format :utf-8)
        (loop for line = (read-line in nil) while line for number from 1
              ;; backslashes ending the line, an odd count leaving the last one to escape the newline
              for run = (- (length line) (or (position #\\ line :from-end t :test-not #'char=) -1) 1)
              when (and (oddp run) (char/= #\; (char (string-left-trim " " line) 0)))
                do (push (format nil "~a:~d" (file-namestring file) number) found))))
    (false found "~{~a~^, ~}" (reverse found))))

(define-test (definitions own-lisp-parses-cleanly)
  ;; all of this editor's own Lisp parses with its Lisp grammar without errors
  (let ((broken '())
        ;; a pattern, which system-relative-pathname would take as a name
        (files (directory (merge-pathnames "**/*.lisp" (asdf:system-source-directory "tuatara")))))
    (true (> (length files) 100) "~d files" (length files))
    (dolist (file files)
      (unless (or (search "/build/" (namestring file)) (search "/.project/" (namestring file))
                  ;; the grammar takes the symbols ^ and \\ (Calc's operators) and #\; before a
                  ;; blank for no Lisp: a gap in the grammar, not in the file
                  (search "lisp/org/formulas.lisp" (namestring file)))
        (multiple-value-bind (tree rope parser) (parse-string "commonlisp" (uiop:read-file-string file))
          (declare (ignore rope))
          (when (tuatara.syntax:tree-has-error-p tree)
            (push (enough-namestring file (asdf:system-source-directory "tuatara")) broken))
          (release-parse tree parser))))
    (is equal '() broken)))

(defparameter *mapped-octets-read-elsewhere*
  '(("save.lisp" "a save writes what the kernel did not copy of a stretch it noted read (NOTE-READ) first")
    ("sort.lisp" "a sort merges its own files of sorted lines, which are taken away after the merge"))
  "The files outside src/text that read a mapping's octets themselves
\(MAPPING-SAP), each with why that leaves no file in memory.")

(define-test (definitions mapped-octets-read-through-the-text-layer)
  ;; the octets of a mapped file are read through the text layer (its spans, SPAN-SAP), which
  ;; notes what is read, so that a pass through a large file leaves memory as it found it
  ;; (WITH-READ-PASS); a reader of its own elsewhere would read the file into memory and leave it
  ;; there: only the few known ones, each for its reason, do
  (let ((found '()))
    (dolist (file (core-source-files))
      (unless (search "/src/text/" (namestring file))
        (when (cl-ppcre:scan "mapping-sap\\b" (uiop:read-file-string file))
          (push (file-namestring file) found))))
    (is equal (sort (mapcar #'first *mapped-octets-read-elsewhere*) #'string<) (sort found #'string<)
        "Reading mapped octets outside src/text: ~{~a~^, ~}" found)))

(defparameter *package-layers*
  '(("tuatara" . 0) ("tuatara-user" . 0) ("tuatara.runtime" . 0)
    ("tuatara.text" . 1)
    ("tuatara.buffer" . 2)
    ("tuatara.io" . 3)
    ("tuatara.dates" . 4) ("tuatara.syntax" . 4)
    ("tuatara.display" . 5) ("tuatara.frontend" . 5) ("tuatara.command" . 5) ("tuatara.modes" . 5)
    ("tuatara.defaults" . 6) ("tuatara.git" . 6) ("tuatara.lsp" . 6) ("tuatara.compile" . 6)
    ("tuatara.packages" . 6) ("tuatara.export" . 6) ("tuatara.terminal-emulator" . 6)
    ("tuatara.frontend.fake" . 7) ("tuatara.frontend.tui" . 7) ("tuatara.frontend.gtk" . 7)
    ("tuatara.gtk" . 7) ("tuatara.terminal" . 7))
  "The layers of Tuatara's packages, low to high: the runtime and the settings,
the text model, buffers, files and processes, dates (whose time zones are read
from files) and syntax, the display and
the command loop with its modes, the defaults and the features, and the
frontends. A source file's package may name packages of its own layer or a
lower one.")

(defparameter *layering-exceptions*
  '()
  "The source files that name a package of a higher layer, each with the package
it names and why that is allowed. Questions the lower layers ask what is loaded
above them go through the host's generic functions, which need no entry.")

(defun layer-references (file)
  "The packages of Tuatara that the source FILE names, as (NAME . LINE): the
packages its package definition uses or imports from, and every PACKAGE:SYMBOL
or PACKAGE::SYMBOL in its code; strings and comments are not read."
  (let* ((text (uiop:read-file-string file))
         (code (cl-ppcre:regex-replace-all
                "(?s)\"(?:[^\"\\\\]|\\\\.)*\"|;[^\\n]*|#\\\\."
                text
                (lambda (match &rest rest)
                  (declare (ignore rest))
                  (make-string (count #\Newline match) :initial-element #\Newline))
                :simple-calls t))
         (found '()))
    (flet ((note (name position)
             (push (cons (string-downcase name) (1+ (count #\Newline code :end position))) found)))
      (cl-ppcre:do-scans (start end starts ends "(?i)(?<![\\w.#:-])(tuatara[\\w.-]*):{1,2}(?=[^\\s:()])" code)
        (note (subseq code (aref starts 0) (aref ends 0)) start))
      (cl-ppcre:do-scans (start end starts ends "(?i)\\(:import-from\\s+#:(tuatara[\\w.-]*)" code)
        (note (subseq code (aref starts 0) (aref ends 0)) start))
      (cl-ppcre:do-scans (start end starts ends "(?i)\\(:use\\s([^()]*)\\)" code)
        (cl-ppcre:do-matches-as-strings (name "(?i)(?<=#:)tuatara[\\w.-]*" (subseq code (aref starts 0) (aref ends 0)))
          (note name start))))
    found))

(defun layer-violations (&optional (files (loop for top in '("src" "lisp") append (source-files top))))
  "Every (FILE PACKAGE LINE REFERENCED-PACKAGE) where one of FILES, by default the
sources under src/ and lisp/, names a package of a higher layer than its own, as
the exceptions allow."
  (let ((violations '()))
    (dolist (file files)
      (let* ((text (uiop:read-file-string file))
             (package (or (cl-ppcre:register-groups-bind (name) ("(?m)^\\(in-package #:([^)\\s]+)\\)" text) name)
                          (cl-ppcre:register-groups-bind (name) ("(?m)^\\((?:uiop:)?def(?:ine-)?package #:(\\S+)" text)
                            name)))
             (own (and package (cdr (assoc (string-downcase package) *package-layers* :test #'string=)))))
        (if (null own)
            (push (list (file-namestring file) package 0 "a package with no layer") violations)
            (dolist (reference (layer-references file))
              (let ((layer (cdr (assoc (car reference) *package-layers* :test #'string=))))
                (when (and layer (> layer own)
                           (not (find-if (lambda (e) (and (string= (first e) (file-namestring file))
                                                          (string= (second e) (car reference))))
                                         *layering-exceptions*)))
                  (push (list (file-namestring file) package (cdr reference) (car reference)) violations)))))))
    violations))

(define-test (definitions layering)
  ;; the layers are the packages' real order: a lower layer never names a higher one
  (let ((violations (layer-violations)))
    (is equal '() violations "A file names a package of a higher layer: ~{~a~^; ~}" violations))
  ;; the scan sees a package named before a symbol
  (true (find "tuatara.io" (layer-references (merge-pathnames "src/dates/zones.lisp"
                                                              (asdf:system-source-directory "tuatara")))
              :key #'car :test #'string=))
  ;; a call into the defaults from the core, in a file made for the purpose, is found
  (let ((file (merge-pathnames (format nil "layering-~36r.lisp" (random (expt 36 8) (make-random-state t)))
                               (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (out file :direction :output :if-exists :supersede)
             (format out "(in-package #:tuatara.io)~%~%(defun f ()~%  (tuatara.defaults::g))~%"))
           (is equal (list (list (file-namestring file) "tuatara.io" 4 "tuatara.defaults"))
               (layer-violations (list file))))
      (uiop:delete-file-if-exists file))))
