;;;; lisp/help/health.lisp --- M-x check-health: what each language and mode has here.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Helix's hx --health: the init file, where the grammars are looked for,
;;;; how the editor is shown and reaches the clipboard, whether the sandbox
;;;; that shuts in the programs decoding files works, and which programs decode
;;;; which files; then for each language
;;;; whether its tree-sitter grammar is found and whether each set of its rules
;;;; (colours, indentation, selection, other languages inside it, local names,
;;;; definitions) is there and works, with its formatter and language server;
;;;; then each major mode, with its language, comments, tabs and the files it is
;;;; chosen for. Compiling every language's rules takes a moment, so that is
;;;; done in the background and the report shown as help when it is ready.

(in-package #:tuatara.defaults)

(defparameter *health-rules*
  '((:colours "Colours" language-rules)
    (:indent "Indent" language-indents)
    (:select "Select" language-textobjects)
    (:inject "Inject" language-injections)
    (:locals "Locals" language-locals)
    (:definitions "Defuns" language-definitions))
  "The sets of rules \\[check-health] checks for each language: a keyword, the
heading of its column, and the function giving the rules' text.")

(defstruct (health-check (:include tuatara.runtime:job) (:constructor make-health-check (languages grammars)) (:copier nil))
  "The rules of LANGUAGES being checked, a job, GRAMMARS the grammar of each
(or NIL), found beforehand on the editor's thread. Once done, RESULTS holds for
each language a list of (KIND . STATUS), STATUS :GOOD, :NONE (no such rules),
:UNCHECKED (no grammar to check them with) or (:BAD . TEXT), SANDBOX what
trying the sandbox found, (STATUS DETAIL) as TUATARA.IO:SANDBOX-STATUS gives,
DECODERS what decodes files here, as sentences, and EXPORTERS what makes
files of others (PDFs of Org and LaTeX), as sentences; TOOLS, for each
language, its formatter and servers as LANGUAGE-TOOLS-WORDS says them;
DRAWING, for each way the window can be drawn, (RENDERER . FIGURES) as
TUATARA.FRONTEND:FRONTEND-MEASURE-DRAWING gives them, and RENDERER the
way the settings ask for (*GUI-RENDERER*)."
  (languages '() :read-only t)
  (grammars '())
  (results '())
  (sandbox '())
  (decoders '())
  (exporters '())
  (tools '())
  (drawing '())
  (renderer nil))

(setf (documentation 'health-check-p 'function) "True when OBJECT is a HEALTH-CHECK.")

(tuatara.runtime:define-job-words health-check
  :doing "Checking the languages' rules"
  :description "Checking what each language and mode has here"
  :verb "check them"
  :left-words "no report")

(defmethod tuatara.runtime:job-at-exit ((job health-check))
  "A check only shows something: stopped without asking."
  :quiet)

(defmethod tuatara.runtime:job-take-in ((job health-check))
  "Show the report of the check, or say how it ended when it did not end done."
  (if (eq (tuatara.runtime:job-status job) :done)
      (show-health-report job)
      (message "check-health: ~a" (tuatara.runtime:job-ending-words job))))

(defun health-check-running ()
  "The check \\[check-health] has running, or NIL."
  (find-if (lambda (job) (and (health-check-p job) (tuatara.runtime:job-running-p job))) tuatara.runtime:*jobs*))

(defvar *health-decoder-functions* '()
  "Functions of no arguments, each saying in a line what reads a kind of file
here, or NIL to say nothing: \\[check-health] shows their lines among the
decoders, after its own. A package that reads files adds one. Called on the
check's own thread.")

(defvar *health-wait* 0.3
  "The seconds \\[check-health] waits for its check before going on with it in
the background, so that a quick one shows at once.")

(defun check-language-rules (language grammar)
  "Each of LANGUAGE's sets of rules (*HEALTH-RULES*) compiled for GRAMMAR, as
a list of (KIND . STATUS) (see HEALTH-CHECK). The compiled rules are thrown
away: what the editor uses stays as it is."
  (loop for (kind nil text) in *health-rules*
        for source = (funcall text language)
        collect (cons kind
                      (cond ((null source) :none)
                            ((null grammar) :unchecked)
                            (t (handler-case (progn (tuatara.syntax:compile-query grammar source
                                                                                  (format nil "~(~a~) (health check)" kind))
                                                    :good)
                                 (error (e) (cons :bad (substitute #\Space #\Newline (princ-to-string e))))))))))

(defun start-health-check ()
  "Start checking every language's grammar and rules in the background, a
job, the grammars found there too, which may wait for the program loading
them to start, and in a window, how it draws each way it can
\(DRAWING-WORDS). Returns the check."
  (let* ((languages (sort (copy-list *languages*) #'string< :key (lambda (l) (symbol-name (language-name l)))))
         (frontend (and *editor* (editor-frontend *editor*))))
    (multiple-value-bind (renderers in-use) (and frontend (tuatara.frontend:frontend-renderers frontend))
    (tuatara.runtime:start-job-thread
     (make-health-check languages '())
     (lambda (check)
       (setf (health-check-grammars check)
             (mapcar (lambda (l) (tuatara.syntax:find-grammar (language-grammar l))) languages)
             (health-check-sandbox check)
             (multiple-value-list (tuatara.io:sandbox-status :again t))
             (health-check-decoders check)
             (append (list (pdf-decoder-words) (office-page-words) (image-decoder-words) (external-program-words) (notify-health-words) (encrypted-health-words) (git-health-words))
                     (remove nil (mapcar (lambda (f) (ignore-errors (funcall f))) *health-decoder-functions*)))
             (health-check-exporters check)
             (list (latex-export-words))
             (health-check-tools check)
             (mapcar (lambda (language) (multiple-value-list (language-tools-words language))) languages)
             (health-check-results check)
             (mapcar #'check-language-rules languages (health-check-grammars check))
             (health-check-drawing check)
             (loop for renderer in renderers
                   collect (cons renderer (ignore-errors (tuatara.frontend:frontend-measure-drawing frontend renderer))))
             (health-check-renderer check)
             in-use))
     :name "tuatara health"))))

;;; The report's text

(defvar *health-pieces* '()
  "The report being written: its pieces, newest first, each (TEXT . FACE).")

(defun health-put (text &optional face)
  "Add TEXT to the report being written, shown in FACE when given, the keys
it names (SUBSTITUTE-COMMAND-KEYS) in :help-key-binding."
  (push (cons text face) *health-pieces*))

(defun health-cell (text width &optional face)
  "Add TEXT to the report in FACE, padded with spaces to WIDTH columns of the
screen (COLUMN-TEXT) and one more."
  (health-put (column-text text width :elide nil) face)
  (health-put " "))

(defun health-heading (text)
  "Add TEXT to the report as a heading, a blank line before it."
  (health-put (format nil "~%"))
  (health-put text :heading)
  (health-put (format nil "~%")))

(defun directory-words (directories)
  "DIRECTORIES as the report names them: each once, short (ABBREVIATED-PATH), those
that are not there said so."
  (format nil "~{~a~^, ~}"
          (mapcar (lambda (d) (format nil "~a~:[ (not there)~;~]" (abbreviated-path d) (probe-file d)))
                  (remove-duplicates directories :test #'equal :from-end t
                                                 :key (lambda (d) (namestring (or (probe-file d) d)))))))

(defun health-put-words (words column width)
  "Add WORDS to the report, a space between each, the line broken before a
word that would pass WIDTH columns, the lines after the first starting at
COLUMN (TUATARA.DISPLAY:FILL-WORDS)."
  (loop for line in (tuatara.display:fill-words words width :start column :indent column)
        for first = t then nil
        do (unless first (health-put (format nil "~%~v@{ ~}" column nil)))
           (health-put line)))

(defun status-cell (status)
  "What a column of the languages' table shows for STATUS, and its face, as two values."
  (cond ((eq status :good) (values "✓" :list-good))
        ((eq status :none) (values "–" :list-dim))
        ((eq status :unchecked) (values "?" :list-dim))
        (t (values "✘" :list-bad))))

(defun lisp-language-p (language)
  "True when LANGUAGE is edited in Lisp mode, which indents by its own rules."
  (some (lambda (mode) (subtypep mode 'lisp-mode)) (language-modes language)))

(defun language-rule-cell (language kind status)
  "What the column of KIND in LANGUAGE's row shows for STATUS, and its face."
  (cond ((and (eq kind :indent) (eq status :none) (lisp-language-p language))
         (values "Lisp" :list-good))
        ((and (eq kind :definitions) (eq status :none))
         (values "brackets" :list-dim))
        ((and (eq kind :colours) (eq status :good))
         (values (multiple-value-bind (style level) (language-colouring language)
                   (case style
                     (:rich "✓ rich")
                     (:sparse (format nil "✓ sparse ~d" level))
                     (t "✓ off")))
                 :list-good))
        (t (status-cell status))))

(defun mode-tab-words (name)
  "How \\[indent-for-tab-command] and indentation fill in major mode NAME:
\"4 spaces\", \"8 tabs\"."
  (let* ((names (mode-precedence name))
         (width (loop for n in names thereis (nth-value 0 (gethash n *mode-tab-widths*))))
         (tabs (loop for n in names do (multiple-value-bind (value found) (gethash n *mode-indent-tabs*)
                                         (when found (return (list value))))
                     finally (return (list *indent-tabs-mode*)))))
    (format nil "~d ~:[spaces~;tabs~]" (or width *tab-width*) (first tabs))))

(defun mode-files-words (name)
  "The files major mode NAME is chosen for, as a list of words: .rs, Makefile,
#!bash, and the regular expressions paths are matched with."
  (multiple-value-bind (extensions names interpreters patterns) (tuatara.modes:mode-file-rules name)
    (append (mapcar (lambda (e) (concatenate 'string "." e)) extensions)
            names
            (mapcar (lambda (i) (concatenate 'string "#!" i)) interpreters)
            patterns)))

(defun special-mode-name-p (name)
  "True when major mode NAME is special-mode or one derived from it: the
editor's own buffers, never a file's."
  (subtypep name 'special-mode))

(defun health-problems (languages grammars results &optional sandbox)
  "What failed among LANGUAGES, GRAMMARS and RESULTS, and the SANDBOX tried
(as HEALTH-CHECK keeps it), as a list of sentences."
  (let ((problems '()))
    (destructuring-bind (&optional status detail &rest rest) sandbox
      (declare (ignore rest))
      (case status
        (:missing (push (format nil "No sandbox: ~a. Install bubblewrap for images and PDFs to be decoded safely, and for text to be coloured and parsed." detail) problems))
        (:broken (push (format nil "No sandbox: ~a." detail) problems))))
    (unless (tuatara.syntax:syntax-helper-program)
      (push (format nil "The program that runs the grammars, tuatara-syntax-helper, is in none of ~a: no language has a grammar."
                    (directory-words (tuatara.syntax:grammar-directories)))
            problems))
    (loop for language in languages
          for grammar in grammars
          for statuses in results
          do (when (and (null grammar) (tuatara.syntax:syntax-helper-program))
               (push (format nil "~(~a~): no grammar ~a.so in ~a, so no colours or structure." (language-name language)
                             (language-grammar language) (directory-words (tuatara.syntax:grammar-directories)))
                     problems))
             (loop for (kind . status) in statuses
                   do (when (consp status)
                        (push (format nil "~(~a~): its ~(~a~) rules do not compile: ~a" (language-name language)
                                      (second (assoc kind *health-rules*)) (cdr status))
                              problems))))
    (nreverse problems)))

(defun report-width ()
  "The columns of the window the report is made from, or 100 without one."
  (or (and *editor* (ignore-errors (tuatara.display:window-width (current-window)))) 100))

(defun write-health-report (check)
  "Write the report of CHECK, which is done, into *HEALTH-PIECES*."
  (let* ((languages (health-check-languages check))
         (grammars (health-check-grammars check))
         (results (health-check-results check))
         (tools (health-check-tools check))
         (frontend (and *editor* (editor-frontend *editor*))))
    (health-put "Tuatara health: what each language and mode has here" :heading)
    (health-put (format nil "~%~%"))
    (flet ((fact (label text &optional face)
             (health-cell label 10)
             (health-put text face)
             (health-put (format nil "~%"))))
      (fact "Init file" (format nil "~a, ~a" (abbreviated-path (config-file))
                                (let ((outcome *init-file-outcome*))
                                  (cond ((eq outcome :read) "read when the editor started")
                                        ((eq outcome :none) "none there")
                                        ((consp outcome) (format nil "stopped at an error: ~a" (cdr outcome)))
                                        (t "not read")))))
      (fact "Grammars" (format nil "~a: ~:[no program to run them~;the program that runs them and ~d of ~d grammars~]"
                               (directory-words (tuatara.syntax:grammar-directories))
                               (tuatara.syntax:syntax-helper-program) (count-if-not #'null grammars) (length grammars)))
      (fact "Parsing" (cond ((eq :ready (first (health-check-sandbox check)))
                             (format nil "grammars run shut in, in a program for the projects you trust and one for each other file or buffer, ~:[each held to its memory in its own address space only (no systemd-run scope here)~;each held to its memory with all it starts (a systemd-run scope)~]"
                                     (tuatara.io:sandbox-memory-scope-p)))
                            (*syntax-helper-unconfined*
                             "no sandbox (see above): grammars run, not shut in, for the projects you trust only; nothing else is parsed")
                            (t "no sandbox (see above): no text is parsed or coloured")))
      (loop for (label . text) in (and frontend (tuatara.frontend:frontend-health frontend))
            do (fact label text))
      (loop for words in (drawing-words (health-check-drawing check) (health-check-renderer check))
            for label = "Measured" then ""
            do (fact label words))
      (destructuring-bind (&optional status detail &rest rest) (health-check-sandbox check)
        (declare (ignore rest))
        (when status
          (fact "Sandbox" (if (eq status :ready)
                              (format nil "~a: programs decoding files run shut in, with no network and no file but the one they decode~:[~;, system calls they need not make refused (seccomp)~]"
                                      detail (tuatara.io:sandbox-filtered-p))
                              (format nil "~a: no file is decoded; images, PDFs and the like are shown as octets and text" detail))
                (unless (eq status :ready) :list-dim))))
      (loop for words in (health-check-decoders check)
            for label = "Decoders" then ""
            do (fact label words))
      (loop for words in (health-check-exporters check)
            for label = "Export" then ""
            do (fact label words)))
    ;; the languages
    (health-heading "Languages")
    (let ((widths (list* 14 7 (append (loop for (kind heading) in *health-rules*
                                            collect (max (length heading) (if (eq kind :colours) 10 (if (eq kind :definitions) 8 4))))
                                      (list 15 6)))))
      (loop for heading in (list* "Language" "Grammar" (append (mapcar #'second *health-rules*) (list "Formatter")))
            for width in widths
            do (health-cell heading width :header-line))
      (health-put "Server" :header-line)
      (health-put (format nil "~%"))
      (loop for language in languages
            for grammar in grammars
            for statuses in results
            for (formatter servers) in tools
            do (health-cell (string-downcase (language-name language)) (first widths))
               (multiple-value-bind (text face) (status-cell (if grammar :good :missing))
                 (health-cell text (second widths) face))
               (loop for (kind . status) in statuses
                     for width in (cddr widths)
                     do (multiple-value-bind (text face) (language-rule-cell language kind status)
                          (health-cell text width face)))
               (health-cell (or formatter "–") 15 (if (and formatter (search "✓" formatter)) nil :list-dim))
               (health-put (or servers "–") (if (and servers (search "✓" servers)) nil :list-dim))
               (health-put (format nil "~%"))))
    (health-put (substitute-command-keys (format nil "~%✓ there and working, ✘ failing (see below), – none, ? not checked without the grammar.
Indent Lisp: by Lisp mode's own rules. Defuns brackets: \\[beginning-of-defun] and \\[end-of-defun] go by opening
brackets at the start of a line. Formatter and Server: each language's usual programs, or *language-formatters* and
*language-servers*; ✓ after one installed, the others not used until they are.~%")
                                         :marks t))
    ;; the modes
    (health-heading "Major modes")
    (let* ((names (mode-names))
           (own (remove-if-not #'special-mode-name-p names))
           (files (remove-if #'special-mode-name-p names))
           (width (reduce #'max files :key (lambda (n) (length (symbol-name n))) :initial-value 4)))
      (loop for (heading w) in `(("Mode" ,width) ("Language" 13) ("Comment" 7) ("Tabs" 8))
            do (health-cell heading w :header-line))
      (health-put "Files" :header-line)
      (health-put (format nil "~%"))
      (dolist (name files)
        (let* ((mode (make-instance name))
               (language (mode-language mode)))
          (health-cell (string-downcase name) width)
          (health-cell (if language (string-downcase (language-name language)) "–") 13 (unless language :list-dim))
          (let ((comment (or (nth-value 1 (mode-comment-syntax mode))
                             ;; a comment with an end, as <!-- -->
                             (multiple-value-bind (opener closer) (mode-block-comment-syntax mode)
                               (and opener (format nil "~a ~a" opener closer))))))
            (health-cell (or comment "–") 7 (unless comment :list-dim)))
          (health-cell (mode-tab-words name) 8)
          (let ((words (mode-files-words name)))
            (if words
                (health-put-words words (+ width 1 14 8 9) (max 80 (report-width)))
                (health-put "–" :list-dim)))
          (health-put (format nil "~%"))))
      (health-put (format nil "~%The editor's own buffers: ~{~(~a~)~^, ~}.~%" own)))
    ;; what failed
    (health-heading "Problems")
    (let ((problems (health-problems languages grammars results (health-check-sandbox check))))
      (if problems
          (dolist (problem problems)
            (health-put "✘ " :list-bad)
            (health-put problem)
            (health-put (format nil "~%")))
          (health-put (format nil "None found.~%"))))))

(defun health-pieces (check)
  "The report of CHECK, which is done, as pieces for RENDER-PIECES, the keys
marked in them to be shown as keys."
  (let ((*health-pieces* '()))
    (write-health-report check)
    (reverse *health-pieces*)))

(defun health-report (check)
  "The text of the report of CHECK, which is done."
  (values (pieces-text (health-pieces check) :keys t)))

(defun drawing-choice (drawing)
  "The way of drawing the window to use, as *GUI-RENDERER* names it, from
DRAWING, a list of (RENDERER . FIGURES) the standard way first
\(FRONTEND-MEASURE-DRAWING): another way only when it showed a twentieth
more of the keys held, or as many, a key's frame a tenth sooner and a
millisecond at least, and the slowest of twenty no slower; else the
standard, so that ways alike within what a measure of seconds tells apart
keep the standard. A way GTK fell back from (DRAWN-INSTEAD) counts as not
measured."
  (let* ((measured (remove-if (lambda (way) (or (null (cdr way)) (drawn-instead (car way) (cdr way)))) drawing))
         (standard (first measured)))
    (if (null standard)
        (car (first drawing))
        (car (reduce (lambda (best other)
                       (let ((b (cdr best)) (o (cdr other)))
                         (if (or (> (getf o :frames) (* 21/20 (getf b :frames)))
                                 (and (>= (getf o :frames) (getf b :frames))
                                      (< (getf o :median) (min (* 9/10 (getf b :median)) (- (getf b :median) 1)))
                                      (<= (getf o :slowest) (getf b :slowest))))
                             other
                             best)))
                     (rest measured) :initial-value standard)))))

(defun drawn-instead (renderer figures)
  "The renderer GTK drew with in place of RENDERER, a value of
*GUI-RENDERER*, as FIGURES report it (:RENDERER, as GTK names its type:
GskBroadwayRenderer), when it fell back from the one asked for; NIL when it
drew as asked, or the name is not one of GTK's (Gsk...)."
  (let ((name (getf figures :renderer))
        (asked (case renderer (:gl "gl") (:vulkan "vulkan") (:cairo "cairo"))))
    (and (stringp name) asked (uiop:string-prefix-p "Gsk" name) (not (search asked name :test #'char-equal)) name)))

(defun renderer-words (renderer)
  "How the report names RENDERER, a value of *GUI-RENDERER*."
  (case renderer (:gl "GL") (:vulkan "Vulkan") (:cairo "the processor") (t "GTK's choice")))

(defun drawing-words (drawing in-use)
  "What the report says of DRAWING (see DRAWING-CHOICE): each way's figures,
and which to use, IN-USE being the way the settings ask for."
  (when drawing
    (let ((choice (drawing-choice drawing))
          (usable (some (lambda (way) (and (cdr way) (not (drawn-instead (car way) (cdr way))))) drawing)))
      (append
       (loop for (renderer . figures) in drawing
             for instead = (drawn-instead renderer figures)
             collect (if figures
                         (format nil "~a: ~@[not to be had here, GTK drew with ~a instead: ~]~,1f ms from a key to its frame (~,1f ms the slowest of twenty), ~d frames for ~d keys held"
                                 (renderer-words renderer) instead (getf figures :median) (getf figures :slowest)
                                 (getf figures :frames) (getf figures :keys))
                         (format nil "~a: could not be measured" (renderer-words renderer))))
       (list (cond ((not usable)
                    (format nil "None of these ways could be had here, so none is the faster"))
                   ((eq choice in-use)
                    (format nil "~a is the faster here, and the one in use" (renderer-words choice)))
                   (t (format nil "~a is the faster here: (setf *gui-renderer* ~(~s~)) in the init file"
                              (renderer-words choice) choice))))))))

(defun show-health-report (check)
  "Show the report of CHECK, which is done, as help in *Health*, from its
start; \\<help-mode>\\[revert-buffer] there checks again, the report shown
meanwhile until the new one is ready."
  (let ((pieces (health-pieces check)) (shown nil))
    (show-help-pieces (lambda ()
                        ;; made again: checked again, in the background
                        (if shown (check-health) (setf shown t))
                        pieces)
                      :name "*Health*")))

(define-command check-health ()
  "Show what each language and major mode has here.
For each language, whether its tree-sitter grammar is found, and whether each
set of its rules is there and works (colours, indentation, selection, other
languages inside it, local names, definitions), with its formatter and
language server; then each major mode with its language, comment marker, tabs
and the files it is chosen for; then what failed, and why. Where the init file
and grammars are, how the editor reaches the clipboard, and whether the
sandbox that shuts in the programs decoding files works (bubblewrap, bwrap:
no network, no file but the one decoded), and which programs decode which
files, come first. In a window, how fast it is drawn each way it can be
\(GL, Vulkan: *GUI-RENDERER*), measured by a copy of Tuatara opened for a
moment for each and holding a key down, and which way to use. In
the report, \\<help-mode>\\[revert-buffer] checks again and \\[quit-window]
closes it."
  (if (health-check-running)
      (message "Already checking; the report comes when it is ready")
      (take-in-soon (start-health-check) *health-wait* "Checking the languages' rules"
                    (format nil "~:[~;a window opening for a moment for each way of drawing; ~]the report comes when it is ready"
                            (and *editor* (tuatara.frontend:frontend-renderers (editor-frontend *editor*)))))))
