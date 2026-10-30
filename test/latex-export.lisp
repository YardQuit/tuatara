;;;; test/latex-export.lisp --- Org and LaTeX files made into PDFs.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(defun report-template ()
  "The path of the report template the tests use, a small one of their own
\(test/latex-export/report.tex), as #+template: names one."
  (namestring (asdf:system-relative-pathname :tuatara "test/latex-export/report.tex")))

(define-test latex-export
  :description "An Org document is written as LaTeX as Org writes it: its keywords, setup file, options,
sections, lists, tables as #+ATTR_LATEX lays them out, blocks, markup, links, footnotes and macros, what
is tagged noexport left out, nothing run. M-x org-export-latex-pdf makes the PDF beside the file with the
first LaTeX program that works, org-export-latex-pdf-buffer shows it beside the file or alone, read again
on the next export, org-export-latex-pdf-xdg opens it outside; what the programs said when none works is
in *LaTeX errors*; a file opened safely, or one that is no Org or LaTeX, is refused.")

(defun org-latex-of (text &rest keys)
  "TEXT, an Org document, as LaTeX (ORG-LATEX-DOCUMENT), with KEYS, and the file its #+EXPORT_FILE_NAME names."
  (apply #'tuatara.defaults::org-latex-document text keys))

(define-test (latex-export org-as-latex)
  (with-test-directory (dir)
    (alexandria:write-string-into-file (format nil "#+latex_header: \\usepackage{setupmark}~%#+options: toc:nil~%")
                                       (merge-pathnames "setup.org" dir))
    (let* ((file (namestring (merge-pathnames "notes.org" dir)))
           (latex (org-latex-of "#+title: My Notes
#+author: Ann
#+date: [2026-02-13 Fri 08:58]
#+setupfile: setup.org
#+macro: greet Hello $1
#+macro: code (eval (shell-command \"touch pwned\"))
#+latex_class_options: [a4paper]
* Intro
Some *bold*, /italic/, _under_, +gone+, =verb= and ~code~ text: 50% of $5 & #1 {braces}.
{{{greet(world)}}} {{{code}}} by {{{author}}}. LaTeX and TeX... \\alpha
- [X] done
- [ ] open
Ordered:
1. first
2. second
Terms:
- term :: meaning
* Hidden :noexport:
secret words
* Links
[[https://example.org/a_b%20c][the site]] <https://example.org> [[*Intro][back]] [[denote:20260101T000000][a note]]
Text with a footnote[fn:1].
#+attr_latex: :environment tabularx :width \\textwidth :align p{0.3\\textwidth}X
| Name | Value |
|------+-------|
| a    | 12    |
#+begin_src lisp
  (print 1)
#+end_src
#+begin_src lisp
#+end_src
#+begin_quote
Quoted.
#+end_quote
#+latex: \\newpage
[fn:1] The note.
** Deep
*** Deeper
**** Deepest
item text
" :file file)))
      ;; the preamble: the program meant, the class, the packages, the setup file's header, the keywords
      (true (search "% Intended LaTeX compiler: xelatex" latex))
      (true (search "\\documentclass[a4paper]{article}" latex))
      (true (search "\\usepackage{fontspec}" latex))
      (true (search "\\usepackage{setupmark}" latex))
      (true (search "\\title{My Notes}" latex))
      (true (search "\\author{Ann}" latex))
      (true (search "\\date{\\textit{[2026-02-13 Fri 08:58]}}" latex))
      (true (search "\\maketitle" latex))
      ;; toc:nil from the setup file
      (false (search "\\tableofcontents" latex))
      ;; sections, and what is past the third level as a list
      (true (search "\\section{Intro}" latex))
      (true (search "\\subsection{Deep}" latex))
      (true (search "\\subsubsection{Deeper}" latex))
      (true (search "\\item Deepest" latex))
      ;; noexport left out
      (false (search "Hidden" latex))
      (false (search "secret" latex))
      ;; markup and escapes
      (true (search "\\textbf{bold}, \\emph{italic}, \\uline{under}, \\sout{gone}, \\texttt{verb} and \\texttt{code}" latex))
      (true (search "50\\% of \\$5 \\& \\#1 \\{braces\\}" latex))
      (true (search "\\LaTeX{} and \\TeX{}\\ldots{}" latex))
      (true (search "\\(\\alpha\\)" latex))
      ;; macros: a value of its own and the author; one that would run code is left empty
      (true (search "Hello world  by Ann." latex))
      (false (probe-file (merge-pathnames "pwned" dir)))
      ;; lists, checkboxes as their marks
      (true (search "\\item[{$\\boxtimes$}] done" latex))
      (true (search "\\item[{$\\square$}] open" latex))
      (true (search "\\begin{enumerate}" latex))
      (true (search "\\item[{term}] meaning" latex))
      ;; links: the web, a headline, a note by its identifier
      (true (search "\\href{https://example.org/a_b\\%20c}{the site}" latex))
      (true (search "\\url{https://example.org}" latex))
      (true (search "]{back}" latex))
      (true (search "a note" latex))
      (true (search "footnote\\footnote{The note.}" latex))
      ;; the table as its #+ATTR_LATEX says
      (true (search "\\begin{tabularx}{\\textwidth}{p{0.3\\textwidth}X}" latex))
      (true (search "Name & Value\\\\" latex))
      (true (search "\\hline" latex))
      ;; code as written, its indentation taken away; an empty block left out
      (true (search (format nil "\\begin{verbatim}~%(print 1)~%\\end{verbatim}") latex))
      (is = 1 (count-matches "\\begin{verbatim}" latex))
      (true (search "\\begin{quote}" latex))
      (true (search "\\newpage" latex))
      (true (search "\\end{document}" latex)))
    ;; where #+export_file_name puts it, .tex added
    (is equal (namestring (merge-pathnames "out/report.tex" dir))
        (nth-value 1 (org-latex-of (format nil "#+export_file_name: out/report~%Text~%")
                                   :file (namestring (merge-pathnames "notes.org" dir)))))
    ;; a headline tagged export: only that one, with what is above it
    (let ((latex (org-latex-of (format nil "* One~%one text~%* Two~%** Kept :export:~%kept text~%"))))
      (false (search "one text" latex))
      (true (search "kept text" latex))
      (true (search "\\section{Two}" latex)))
    ;; #+latex_compiler names the program meant
    (true (search "% Intended LaTeX compiler: lualatex"
                  (org-latex-of (format nil "#+latex_compiler: lualatex~%Text~%"))))))

(defun count-matches (part text)
  "How many times PART is in TEXT."
  (loop with start = 0
        for at = (search part text :start2 start)
        while at count t do (setf start (1+ at))))

(defun stand-in-latex (directory name &key fail)
  "A program NAME in DIRECTORY that does as a LaTeX program does for an export:
writes the file's PDF and log into its -output-directory, or with FAIL a log
with an error and no PDF, ending 1. Returns its full name."
  (let ((script (namestring (merge-pathnames name directory)))
        (pdf (namestring (merge-pathnames "stand-in.pdf" directory))))
    (unless (probe-file pdf)
      (alexandria:write-byte-vector-into-file (pdf-octets '("Exported page one" "Exported page two")) pdf))
    (alexandria:write-string-into-file
     (format nil "#!/bin/sh
for a in \"$@\"; do case \"$a\" in -output-directory=*) out=\"${a#-output-directory=}\";; *.tex) tex=\"$a\";; esac; done
stem=$(basename \"$tex\" .tex)
echo \"~a ran\" >> '~a'
~:[cp '~a' \"$out/$stem.pdf\"; echo 'Output written' > \"$out/$stem.log\"; exit 0~;~*n=$(grep -n nonsense \"$tex\" | head -1 | cut -d: -f1); printf './%s.tex:%s: Undefined control sequence.\\nl.%s Some \\\\nonsense\\n' \"$stem\" \"${n:-1}\" \"${n:-1}\" > \"$out/$stem.log\"; exit 1~]
"
             name (namestring (merge-pathnames "ran.txt" directory)) fail pdf)
     script :if-exists :supersede)
    (sb-posix:chmod script #o700)
    script))

(defun export-session (file steps &key (width 120) (height 30) cell-pixels)
  "Visit FILE in an editor session and take STEPS in turn, each keys to type
or a function to call, the next once the exports in the background have
ended and the PDFs' views are drawn. Returns the frontend and the editor."
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width width :height height :cell-pixels cell-pixels)))
    ;; M-x lists the commands used lately first: none, as for RUN-KEYS
    (setf tuatara.defaults::*command-history* '())
    (labels ((busy ()
               (or (some #'tuatara.defaults::pdf-export-p tuatara.runtime:*jobs*)
                   (some #'tuatara.defaults::docx-export-p tuatara.runtime:*jobs*)
                   (some (lambda (b) (let ((view (tuatara.defaults::pdf-view-of b)))
                                       (and view (or (tuatara.defaults::pdf-view-job view) (tuatara.defaults::pdf-view-text-job view)))))
                         tuatara.defaults::*pdf-views*)))
             (take (steps start)
               (cond ((and (busy) (< (- (get-internal-real-time) start) (* 30 internal-time-units-per-second)))
                      (sleep 0.02)
                      (tuatara.frontend.fake:fake-queue-action f (lambda () (take steps start))))
                     (steps
                      (queue-test-keys f (list (first steps)))
                      ;; the work it starts begins before the next look
                      (tuatara.frontend.fake:fake-queue-action
                       f (lambda () (take (rest steps) (get-internal-real-time))))))))
      (tuatara.frontend.fake:fake-queue-action f (lambda () (take (if (listp steps) steps (list steps)) (get-internal-real-time)))))
    (values f (tuatara.command:run-editor f :files (list (namestring file)) :setup #'tuatara.defaults:install-defaults))))

(defun refusal-in (file command)
  "What COMMAND says instead of running, in a session visiting FILE, where it
is not offered by M-x either; NIL when it runs there."
  (let ((said nil))
    (export-session file (list (lambda () (setf said (tuatara.defaults::mode-command-refusal command)))))
    said))

(defun export-command (name)
  "The keys that run the command NAME by M-x."
  (format nil "M-x ~a RET" (rimu-typed name)))

(define-test (latex-export pdf-beside)
  (with-test-directory (dir)
    (let* ((file (merge-pathnames "notes.org" dir))
           (xelatex (stand-in-latex dir "xelatex"))
           (lualatex (stand-in-latex dir "lualatex"))
           (tuatara-user:*latex-engines* (list xelatex lualatex)))
      (alexandria:write-string-into-file (format nil "#+title: Notes~%* One~%Text~%") file)
      ;; the PDF and the LaTeX beside it, by the first program; the working files kept apart
      (pdf-forget)
      (let ((f (export-session file (export-command "org-export-latex-pdf"))))
        (true (tuatara.frontend.fake:fake-frame-showing f "Exporting notes.org to PDF..."))
        (true (tuatara.frontend.fake:fake-frame-showing f "notes.pdf (xelatex)"))
        (true (probe-file (merge-pathnames "notes.pdf" dir)))
        (true (search "\\section{One}" (alexandria:read-file-into-string (merge-pathnames "notes.tex" dir))))
        (false (probe-file (merge-pathnames "notes.aux" dir)))
        (false (probe-file (merge-pathnames "notes.log" dir)))
        ;; not shown
        (false (tuatara.frontend.fake:fake-frame-showing f "Page 1 of 2")))
      ;; the first program failing, the next makes it, and that is said
      (let ((tuatara-user:*latex-engines* (list (stand-in-latex dir "xelatex-broken" :fail t) lualatex)))
        (delete-file (merge-pathnames "notes.pdf" dir))
        (let ((f (export-session file (export-command "org-export-latex-pdf"))))
          (true (tuatara.frontend.fake:fake-frame-showing f "(lualatex; xelatex-broken failed)"))
          (true (probe-file (merge-pathnames "notes.pdf" dir)))))
      ;; every program failing: what they said, in *LaTeX errors*, each error at the Org line it comes from
      (let ((tuatara-user:*latex-engines* (list (stand-in-latex dir "xelatex-broken" :fail t)))
            (file (merge-pathnames "broken.org" dir)))
        (alexandria:write-string-into-file (format nil "#+title: Notes~%* One~%Text~%Some \\nonsense here~%") file)
        (multiple-value-bind (f)
            (export-session file (list (export-command "org-export-latex-pdf")
                                       (lambda () (tuatara.defaults::pop-to-buffer (tuatara.buffer:find-buffer "*LaTeX errors*")))
                                       "n")
                            :width 200)
          (true (tuatara.frontend.fake:fake-frame-showing
                 f "Could not export broken.org to PDF: xelatex-broken failed (broken.org:4: Undefined control sequence.)"))
          (let* ((errors (tuatara.buffer:find-buffer "*LaTeX errors*"))
                 (text (tuatara.buffer:buffer-string errors)))
            (true (search "  broken.org:4: Undefined control sequence." text))
            (true (search "    | Some \\nonsense here" text))
            ;; a list of the errors under the program's name, each one entry on all its lines; n goes to the first
            (is equal '(:engine "xelatex-broken") (tuatara.defaults::list-entry-at errors (tuatara.buffer:buffer-goto-line errors 2)))
            (let ((entry (tuatara.defaults::list-entry-at errors (tuatara.buffer:buffer-goto-line errors 3))))
              (is eq :error (first entry))
              (is eq entry (tuatara.defaults::list-entry-at errors (tuatara.buffer:buffer-goto-line errors 4))))
            (is = 3 (tuatara.buffer:buffer-line-number errors (tuatara.buffer:buffer-point errors)))
            (true (eq 'tuatara.defaults::list-visit (list-key-binding 'latex-errors-mode "RET")))))
        ;; RET on the error shows the line of the note it comes from
        (multiple-value-bind (f editor)
            (export-session file (list (export-command "org-export-latex-pdf")
                                       (lambda () (tuatara.defaults::pop-to-buffer (tuatara.buffer:find-buffer "*LaTeX errors*")))
                                       "n RET")
                            :width 200)
          (declare (ignore f))
          (is string= "broken.org" (current-name editor))
          (is = 3 (tuatara.buffer:buffer-line-number (tuatara.buffer:find-buffer "broken.org") (session-point editor)))))
      ;; no program at all: said, nothing started
      (let ((tuatara-user:*latex-engines* (list "no-such-latex-program")))
        (let ((f (export-session file (export-command "org-export-latex-pdf") :width 200)))
          (true (tuatara.frontend.fake:fake-frame-showing f "none of no-such-latex-program was found on the PATH or in TeX Live")))))))

(define-test (latex-export shown-and-read-again)
  (with-test-directory (dir)
    (let* ((file (merge-pathnames "notes.org" dir))
           (tuatara-user:*latex-engines* (list (stand-in-latex dir "xelatex"))))
      (alexandria:write-string-into-file (format nil "* One~%Text~%") file)
      ;; a wide screen: the PDF down the right side, the focus staying in the Org file
      (pdf-forget)
      (multiple-value-bind (f editor) (export-session file (export-command "org-export-latex-pdf-buffer") :width 200
                                                                                                      :cell-pixels '(10 20))
        (let ((windows (tuatara.command:editor-windows editor)))
          (is = 2 (length windows))
          (is string= "notes.org" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
          (is string= "notes.pdf" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (second windows))))
          (true (> (tuatara.display:area-left (second windows)) 0)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Page 1 of 2")))
      ;; a narrow screen: the PDF alone on it
      (pdf-forget)
      (multiple-value-bind (f editor) (export-session file (export-command "org-export-latex-pdf-buffer") :width 100
                                                                                                      :cell-pixels '(10 20))
        (declare (ignore f))
        (is = 1 (length (tuatara.command:editor-windows editor)))
        (is string= "notes.pdf" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
      ;; exported again: the view shown reads the new PDF, at the same page
      (pdf-forget)
      (let ((tuatara-user:*latex-engines* (list (stand-in-latex dir "xelatex"))))
        (multiple-value-bind (f editor)
            (export-session file (list (export-command "org-export-latex-pdf-buffer")
                                       (lambda ()
                                         ;; the stand-in's PDF made anew with three pages
                                         (alexandria:write-byte-vector-into-file
                                          (pdf-octets '("New one" "New two" "New three")) (merge-pathnames "stand-in.pdf" dir)
                                          :if-exists :supersede))
                                       (export-command "org-export-latex-pdf"))
                            :width 200 :cell-pixels '(10 20))
          (declare (ignore editor))
          (let ((view (some #'tuatara.defaults::pdf-view-of tuatara.defaults::*pdf-views*)))
            (true view)
            (true (tuatara.frontend.fake:fake-frame-showing f "Page 1 of 3"))))))))

(define-test (latex-export not-exported)
  (with-test-directory (dir)
    (let ((tuatara-user:*latex-engines* (list (stand-in-latex dir "xelatex"))))
      ;; a file that is neither Org nor LaTeX
      (let ((file (merge-pathnames "notes.txt" dir)))
        (alexandria:write-string-into-file "Text" file)
        (is equal "org-export-latex-pdf works in Org files only" (refusal-in file 'tuatara.defaults::org-export-latex-pdf))
        (is equal "latex-compile-pdf works in LaTeX files only" (refusal-in file 'tuatara.defaults::latex-compile-pdf)))
      ;; a file opened safely: nothing run
      (let ((file (merge-pathnames "evidence.org" dir))
            (tuatara-user:*safe-directories* (list (namestring dir))))
        (alexandria:write-string-into-file "* Evidence" file)
        (is equal "org-export-latex-pdf works in Org files only" (refusal-in file 'tuatara.defaults::org-export-latex-pdf))
        ;; and called all the same, nothing run
        (export-session file (list (lambda () (tuatara.defaults::start-latex-export :file))))
        (false (probe-file (merge-pathnames "ran.txt" dir))))
      ;; a LaTeX file with changes: saved first, then made into a PDF from what was saved
      (let ((file (merge-pathnames "paper.tex" dir)))
        (alexandria:write-string-into-file "\\documentclass{article}" file)
        (let ((f (export-session file (list "g e o x C-g" (export-command "latex-compile-pdf")))))
          (true (tuatara.frontend.fake:fake-frame-showing f "Compiling paper.tex to PDF..."))
          (true (tuatara.frontend.fake:fake-frame-showing f "paper.pdf (xelatex)"))
          (true (search "x" (alexandria:read-file-into-string file) :start2 20))
          (true (probe-file (merge-pathnames "paper.pdf" dir))))))))

(define-test (latex-export tex-live-folders)
  (with-test-directory (dir)
    ;; a release of TeX Live the PATH does not have: the newest one with the program is used
    (let* ((root (namestring (merge-pathnames "texlive/" dir)))
           (old (ensure-directories-exist (merge-pathnames "texlive/2024/bin/x86_64-linux/" dir)))
           (new (ensure-directories-exist (merge-pathnames "texlive/2026/bin/x86_64-linux/" dir)))
           (tuatara.defaults::*tex-live-directories* (list "/nonexistent/texlive/" root))
           (tuatara-user:*latex-engines* (list "tuatara-test-xelatex" "tuatara-test-lualatex")))
      (declare (ignore old))
      (stand-in-latex (merge-pathnames "texlive/2024/bin/x86_64-linux/" dir) "tuatara-test-xelatex")
      (stand-in-latex new "tuatara-test-xelatex")
      (stand-in-latex (merge-pathnames "texlive/2024/bin/x86_64-linux/" dir) "tuatara-test-lualatex")
      (is equal (list (cons "tuatara-test-xelatex" (namestring (merge-pathnames "tuatara-test-xelatex" new)))
                      (cons "tuatara-test-lualatex"
                            (namestring (merge-pathnames "texlive/2024/bin/x86_64-linux/tuatara-test-lualatex" dir))))
          (tuatara.defaults::latex-engines-found))
      ;; a full path is taken as it is, never looked for elsewhere
      (let ((tuatara-user:*latex-engines* (list "/nonexistent/tuatara-test-xelatex")))
        (is equal '() (tuatara.defaults::latex-engines-found))))))

(define-test (latex-export enhanced)
  (with-test-directory (dir)
    (let ((tuatara-user:*latex-engines* (list (stand-in-latex dir "xelatex"))))
      ;; an Org file: its PDF, as the compatible export makes it so far
      (let ((file (merge-pathnames "notes.org" dir)))
        (alexandria:write-string-into-file (format nil "* One~%Text~%") file)
        (let ((f (export-session file (export-command "org-export-latex-pdf-enhanced"))))
          (true (tuatara.frontend.fake:fake-frame-showing f "notes.pdf (xelatex)"))
          (true (probe-file (merge-pathnames "notes.pdf" dir)))))
      ;; a LaTeX file: for the compatible export only
      (let ((file (merge-pathnames "paper.tex" dir)))
        (alexandria:write-string-into-file "\\documentclass{article}" file)
        (is equal "org-export-latex-pdf-enhanced works in Org files only"
            (refusal-in file 'tuatara.defaults::org-export-latex-pdf-enhanced))
        (is equal nil (refusal-in file 'tuatara.defaults::latex-compile-pdf))
        (false (probe-file (merge-pathnames "paper.pdf" dir)))))))

(define-test (latex-export styles)
  ;; the places of a style: values, values as written, files' full names, parts kept or left out
  (let ((field (lambda (name way)
                 (cond ((string= name "title") (if (eq way :raw) "A & B" "A \\& B"))
                       ((string= name "logo") (and (eq way :path) "/full/logo.png"))
                       ((string= name "empty") "")
                       (t nil)))))
    (is string= "\\title{A \\& B} raw A & B {/full/logo.png}"
        (tuatara.defaults::fill-latex-template "\\title{{{{title}}}} raw {{{title:raw}}} {{{{logo:path}}}}" field))
    (is string= "[yes][none]" (tuatara.defaults::fill-latex-template
                               "{{{#title}}}[yes]{{{/title}}}{{{#empty}}}[empty]{{{/empty}}}{{{^missing}}}[none]{{{/missing}}}{{{#missing}}}[no]{{{/missing}}}"
                               field))
    ;; a brace before a place, and braces that are no place, stay braces
    (is string= "\\a{x}{ {{y}} }" (tuatara.defaults::fill-latex-template "\\a{x}{ {{y}} }" field)))
  (with-test-directory (dir)
    (let* ((file (namestring (merge-pathnames "survey.org" dir)))
           (text (format nil "#+title: Report~%#+template: ~a~%#+organisation: Example Ltd~%#+reference: REF-1~%#+status: Draft~%#+copy_to: Ann~%#+copy_to: Bo~%* Findings~%Text~%#+begin_signature~%Town 2026-10-04~%Ann - Lead~%#+end_signature~%* Appendix: Terms :appendix:~%Words~%"
                         (report-template))))
      ;; the enhanced export takes the style: cover, facts, signature, annexes, the tag not printed
      (let ((latex (tuatara.defaults::org-latex-document text :file file :enhanced t)))
        (true (search "Example Ltd" latex))
        (true (search "\\MakeUppercase{Report}" latex))
        (true (search "Title & Report REF-1 & \\textbf{Draft}" latex))
        (true (search "Copies to & Ann\\newline Bo" latex))
        (true (search "\\begin{signature}" latex))
        (true (search (format nil "\\appendix~%\\section{Appendix: Terms}") latex))
        (false (search "\\textsc{appendix}" latex))
        (false (search "includegraphics" latex)))
      ;; the export as Org makes it never takes a style, nor begins annexes
      (let ((latex (tuatara.defaults::org-latex-document text :file file)))
        (false (search "Example Ltd" latex))
        (false (search "\\appendix" latex))
        (true (search "\\textsc{appendix}" latex)))
      ;; a template of your own by name, in latex-templates/ of the configuration directory, before a built-in one;
      ;; #+latex_style: is the same keyword's older name
      (let ((old (uiop:getenv "XDG_CONFIG_HOME"))
            (named (format nil "#+organisation: Example Ltd~%~~a: report~%Text~%")))
        (unwind-protect
             (progn
               (sb-posix:setenv "XDG_CONFIG_HOME" (namestring dir) 1)
               (ensure-directories-exist (merge-pathnames "tuatara/latex-templates/" dir))
               (alexandria:write-string-into-file "MINE {{{organisation}}} {{{body}}}" (merge-pathnames "tuatara/latex-templates/report.tex" dir))
               (dolist (keyword '("#+template" "#+latex_template" "#+latex_style"))
                 (true (uiop:string-prefix-p "MINE Example Ltd"
                                             (tuatara.defaults::org-latex-document (format nil named keyword) :file file :enhanced t))))
               (alexandria:write-string-into-file "ARTICLE OF MINE [{{{latex_class:raw}}}] {{{body}}}" (merge-pathnames "tuatara/latex-templates/article.tex" dir))
               (true (uiop:string-prefix-p "ARTICLE OF MINE" (tuatara.defaults::org-latex-document (format nil "Text~%") :file file :enhanced t)))
               ;; by path: from the note's folder, or ~
               (ensure-directories-exist (merge-pathnames "templates/" dir))
               (alexandria:write-string-into-file "FORENSIC {{{reference}}} {{{body}}}" (merge-pathnames "templates/forensic.tex" dir))
               (true (uiop:string-prefix-p "FORENSIC R-9" (tuatara.defaults::org-latex-document
                                                           (format nil "#+template: ./templates/forensic.tex~%#+reference: R-9~%Text~%")
                                                           :file file :enhanced t)))
               (let ((home (sb-posix:getenv "HOME")))
                 (unwind-protect
                      (progn (sb-posix:setenv "HOME" (string-right-trim "/" (namestring dir)) 1)
                             (true (uiop:string-prefix-p "FORENSIC" (tuatara.defaults::org-latex-document
                                                                     (format nil "#+template: ~~/templates/forensic.tex~%Text~%")
                                                                     :enhanced t))))
                   (sb-posix:setenv "HOME" home 1)))
               ;; the export as Org makes it takes none of them
               (false (search "FORENSIC" (tuatara.defaults::org-latex-document
                                          (format nil "#+template: ./templates/forensic.tex~%Text~%") :file file)))
               ;; LaTeX's own kinds of document keep the enhanced export's look; a class of your own does not
               (let ((report (tuatara.defaults::org-latex-document (format nil "#+latex_class: report~%* Part~%Text~%") :file file :enhanced t)))
                 (true (search "[report]" report))
                 (true (uiop:string-prefix-p "ARTICLE OF MINE" report)))
               (false (search "ARTICLE OF MINE" (tuatara.defaults::org-latex-document (format nil "#+latex_class: myclass~%Text~%")
                                                                                      :file file :enhanced t)))
               ;; a template there is not, by name or by path
               (flet ((said (keyword-line)
                        (handler-case (progn (tuatara.defaults::org-latex-document (format nil "~a~%Text~%" keyword-line) :file file :enhanced t) "")
                          (error (e) (princ-to-string e)))))
                 (true (search "There is no LaTeX template called nosuch (#+template:)" (said "#+template: nosuch")))
                 (true (search "There is no LaTeX template at" (said "#+template: ./gone.tex")))))
          (if old (sb-posix:setenv "XDG_CONFIG_HOME" old 1) (sb-posix:unsetenv "XDG_CONFIG_HOME")))))))

(define-test (latex-export redaction)
  (let ((text (format nil "#+title: Notes on {{{redact(John Otago)}}}~%#+author: {{{protect(Ann Napier)}}}~%* About {{{redact(Bo Raglan)}}}~%Subject {{{redact(Otago, John)}}} met {{{protect(Ann Napier)}}}.~%| User | Host |~%|------+------|~%| {{{redact(user4711)}}} | {{{redact(wks1)}}} |~%")))
    ;; redacted as standard: neither in the LaTeX nor in the PDF's facts, a box as wide as the text in their place
    (let ((latex (tuatara.defaults::org-latex-document text :enhanced t)))
      (dolist (secret '("John Otago" "Bo Raglan" "Otago" "user4711" "wks1"))
        (false (search secret latex)))
      (true (search "\\texorpdfstring{\\redactedbox{nnnnnnnnnnn}}{[redacted]}" latex)) ; Otago, John, its comma and space
      (true (search "\\DeclareRobustCommand{\\redactedbox}" latex))
      (true (search "pdftitle={Notes on [redacted]}" latex))
      ;; protected text is shown as standard
      (true (search "Ann Napier" latex)))
    ;; protect:t hides it too; redact:nil shows what redact marks
    (let ((latex (tuatara.defaults::org-latex-document (format nil "#+options: protect:t redact:nil~%~a" text) :enhanced t)))
      (false (search "Ann Napier" latex))
      (true (search "John Otago" latex)))
    ;; the export as Org makes it does not know them: an error, never the text printed
    (true (search "hides text only in the enhanced export"
                  (handler-case (progn (tuatara.defaults::org-latex-document text) "")
                    (error (e) (princ-to-string e)))))
    ;; with a #+macro: line of your own there, as for Emacs, that is used
    (true (search "[John Otago]" (tuatara.defaults::org-latex-document (format nil "#+macro: redact [$1]~%#+macro: protect $1~%Subject {{{redact(John Otago)}}}~%"))))
    ;; in the enhanced export a #+macro: redact line never takes Tuatara's place
    (false (search "John Otago" (tuatara.defaults::org-latex-document (format nil "#+macro: redact $1~%Subject {{{redact(John Otago)}}}~%")
                                                                     :enhanced t)))))

(define-test (latex-export short-hiding-names)
  ;; R and P, in any case, are redact and protect
  (let* ((text (format nil "#+options: protect:t~%#+title: Notes on {{{R(John Otago)}}}~%Subject {{{r(Bo Raglan)}}} met {{{P(Ann Napier)}}}.~%"))
         (latex (tuatara.defaults::org-latex-document text :enhanced t)))
    (dolist (secret '("John Otago" "Bo Raglan" "Ann Napier"))
      (false (search secret latex)))
    (true (search "pdftitle={Notes on [redacted]}" latex))
    ;; elsewhere, an error, never the text printed
    (true (search "hides text only in the enhanced export"
                  (handler-case (progn (tuatara.defaults::org-latex-document text) "")
                    (error (e) (princ-to-string e)))))
    (is string= "R" (tuatara.defaults::only-enhanced-macro text))
    ;; found shown elsewhere as the long names are
    (is equal '("Bo Raglan") (mapcar #'second (tuatara.defaults::redaction-findings
                                           (format nil "Subject {{{R(Bo Raglan)}}}; later Bo Raglan.~%") :org)))
    ;; where Org reads no macros, refused as the long names are
    (true (search "on line 3 is where Org reads no macros"
                  (handler-case (progn (tuatara.defaults::org-latex-document
                                        (format nil "Text~%#+begin_example~%{{{R(Bo Raglan)}}}~%#+end_example~%") :enhanced t)
                                       "")
                    (error (e) (princ-to-string e)))))))

(define-test (latex-export fitted-tables)
  (flet ((enhanced (text) (tuatara.defaults::org-latex-document text :enhanced t)))
    (let ((wide (format nil "| Key | Command | Description |~%|-----+---------+-------------|~%| SPC , | switch-to-buffer | Switch to another buffer, chosen from the list of buffers open, the last used first in the list |~%| SPC h | scratch-buffer | Open or return to the scratch buffer, where Lisp can be tried out and notes taken |~%")))
      ;; wider than the text: as wide as the text, the column of words wrapping, the header bold and again on each page
      (let ((latex (enhanced wide)))
        (true (search "\\begin{xltabular}{\\textwidth}{@{}l l >{\\raggedright\\arraybackslash\\hsize=1.000\\hsize}X@{}}" latex))
        (true (search (format nil "\\toprule~%\\textbf{Key} & \\textbf{Command} & \\textbf{Description}\\\\~%\\midrule~%\\endhead") latex))
        (true (search (format nil "\\bottomrule~%\\end{xltabular}") latex))
        (true (search "\\usepackage{xltabular}" latex)))
      ;; laid out by its own #+attr_latex: as it says
      (let ((latex (enhanced (format nil "#+attr_latex: :environment tabularx :width \\textwidth :align lXX~%~a" wide))))
        (true (search "\\begin{tabularx}{\\textwidth}{lXX}" latex))
        (false (search "xltabular}{" latex)))
      ;; the export as Org makes it: as Org lays it out
      (true (search "\\begin{tabular}{lll}" (tuatara.defaults::org-latex-document wide))))
    ;; narrow enough: as wide as it is, numbers to the right, no header without a rule under it
    (let ((latex (enhanced (format nil "| Tool | 1.2 |~%| Other | 3.4 |~%"))))
      (true (search "\\begin{longtable}{@{}l r@{}}" latex))
      (false (search "\\endhead" latex)))
    ;; too wide with nothing to wrap: smaller type
    (let ((latex (enhanced (format nil "| ~a | ~a | ~a |~%" (make-string 30 :initial-element #\a)
                                   (make-string 30 :initial-element #\b) (make-string 30 :initial-element #\c)))))
      (true (search (format nil "{\\footnotesize~%\\begin{longtable}") latex)))))

(define-test (latex-export lists-blocks-underscores)
  (let ((text (format nil "Asked:~%a) Days from home.~%b) Days abroad.~%~%#+attr_latex: :columns 2 :font \\small~%#+begin_example~%Logon  08:07~%Logoff 17:32~%#+end_example~%~%Event login_success and x_{2}.~%")))
    ;; the enhanced export: a) b) as written, a block in two columns, an underscore in a word kept
    (let ((latex (tuatara.defaults::org-latex-document text :enhanced t)))
      (true (search (format nil "\\begin{enumerate}~%\\renewcommand{\\labelenumi}{\\alph{enumi})}") latex))
      (true (search "\\item Days from home." latex))
      (true (search (format nil "{\\small~%\\begin{multicols}{2}~%\\begin{Verbatim}") latex))
      (true (search "login\\_success" latex))
      (true (search "x\\textsubscript{2}" latex)))
    ;; the export as Org makes it: a) is text, the block as it is, _success a subscript, as Org has them
    (let ((latex (tuatara.defaults::org-latex-document text)))
      (false (search "\\begin{enumerate}" latex))
      (false (search "multicols" latex))
      (true (search "login\\textsubscript{success}" latex)))
    ;; #+options: ^:t in the enhanced export: subscripts as Org reads them
    (true (search "login\\textsubscript{success}"
                  (tuatara.defaults::org-latex-document (format nil "#+options: ^:t~%~a" text) :enhanced t)))))

(define-test (latex-export latex-only)
  (with-test-directory (dir)
    (let ((file (merge-pathnames "notes.org" dir))
          ;; no LaTeX program needed to write LaTeX
          (tuatara-user:*latex-engines* (list "no-such-latex-program")))
      (alexandria:write-string-into-file (format nil "#+template: ~a~%* One~%Text~%" (report-template)) file)
      (let ((f (export-session file (export-command "org-export-latex"))))
        (true (tuatara.frontend.fake:fake-frame-showing f "Exporting notes.org to LaTeX..."))
        (true (tuatara.frontend.fake:fake-frame-showing f "notes.tex"))
        (true (search "\\section{One}" (alexandria:read-file-into-string (merge-pathnames "notes.tex" dir))))
        ;; as Org writes it: no style
        (false (search "Appendices" (alexandria:read-file-into-string (merge-pathnames "notes.tex" dir))))
        (false (probe-file (merge-pathnames "notes.pdf" dir))))
      (export-session file (export-command "org-export-latex-enhanced"))
      (true (search "Appendices" (alexandria:read-file-into-string (merge-pathnames "notes.tex" dir))))
      (false (probe-file (merge-pathnames "notes.pdf" dir))))))

(define-test (latex-export enhanced-said)
  (with-test-directory (dir)
    (let ((file (merge-pathnames "survey.org" dir))
          (tuatara-user:*latex-engines* (list (stand-in-latex dir "xelatex"))))
      (alexandria:write-string-into-file (format nil "* One~%Subject {{{redact(Bo Raglan)}}}.~%") file)
      ;; the export as Org makes it, on a note hiding names: said at once, with the enhanced command for it
      (let ((f (export-session file (export-command "org-export-latex-pdf-buffer") :width 200)))
        (true (tuatara.frontend.fake:fake-frame-showing
               f "survey.org hides text with {{{redact(...)}}}, which only the enhanced export does: M-x org-export-latex-pdf-enhanced-buffer"))
        (false (probe-file (merge-pathnames "survey.tex" dir)))))))

(define-test (latex-export cover-title-and-copy)
  ;; the report's cover and facts take #+front_title:, the contents #+title:
  (let ((latex (tuatara.defaults::org-latex-document
                (format nil "#+title: Mock Report~%#+front_title: Report~%#+template: ~a~%#+reference: R-1~%* One~%" (report-template))
                :enhanced t)))
    (true (search "\\MakeUppercase{Report}" latex))
    (true (search "Title & Report R-1" latex))
    (true (search "\\contentsline{section}{Mock Report}" latex)))
  ;; M-x copy-latex-template: a built-in template copied into your own, and visited
  (with-test-directory (dir)
    (let ((old (uiop:getenv "XDG_CONFIG_HOME"))
          (file (merge-pathnames "note.org" dir)))
      (alexandria:write-string-into-file "Text" file)
      (unwind-protect
           (progn
             (sb-posix:setenv "XDG_CONFIG_HOME" (namestring dir) 1)
             (multiple-value-bind (f editor) (export-session file "M-x c o p y - l a t e x - t e m p l a t e RET a r t i c l e RET" :width 200)
               (let ((copy (merge-pathnames "tuatara/latex-templates/article.tex" dir)))
                 (true (probe-file copy))
                 (is string= (tuatara.defaults::find-latex-template "article" :built-in t) (alexandria:read-file-into-string copy))
                 (is string= "article.tex" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
                 (true (tuatara.frontend.fake:fake-frame-showing f "Copied the export template article.tex to"))
                 ;; a copy of yours is visited, never written over
                 (alexandria:write-string-into-file "mine" copy :if-exists :supersede)
                 (forget-file-buffers)
                 (true (tuatara.frontend.fake:fake-frame-showing
                        (export-session file "M-x c o p y - l a t e x - t e m p l a t e RET a r t i c l e RET" :width 200)
                        "is yours already"))
                 (is string= "mine" (alexandria:read-file-into-string copy)))))
        (if old (sb-posix:setenv "XDG_CONFIG_HOME" old 1) (sb-posix:unsetenv "XDG_CONFIG_HOME"))))))

(define-test (latex-export code-wrapped-and-coloured)
  (let ((text (format nil "#+begin_src emacs-lisp~%(defun hello () \"A string\")~%#+end_src~%~%#+begin_example~%path\\to{x}~%#+end_example~%")))
    ;; the enhanced export: lines that break at the margin, the code in its colours, the rest as written
    (let ((latex (tuatara.defaults::org-latex-document text :enhanced t)))
      (true (search "\\begin{Verbatim}[breaklines,breakautoindent=false,breakindent=1em,breakafter=/-.\\,;:=,commandchars=\\\\\\{\\}]" latex))
      (true (search "\\usepackage{fvextra}" latex))
      (true (search "\\color[HTML]{" latex))
      (true (search "defun" latex))
      (true (search "path\\char92{}to\\char123{}x\\char125{}" latex)))
    ;; the export as Org makes it: verbatim, no colour
    (let ((latex (tuatara.defaults::org-latex-document text)))
      (true (search (format nil "\\begin{verbatim}~%(defun hello () \"A string\")~%\\end{verbatim}") latex))
      (false (search "\\color[HTML]" latex))))
  ;; a face's colour under a theme not in force
  (true (tuatara.display:colour-hex (tuatara.display:face-attribute-in-theme :keyword :fg 'tuatara.defaults::ao-light)))
  (false (tuatara.display:face-attribute-in-theme :keyword :fg 'tuatara.defaults::plain)))

(define-test (latex-export fonts-and-fallbacks)
  (let* ((installed (loop for name being the hash-values of (tuatara.defaults::installed-font-families) collect name))
         (latin (or (find "Cantarell" installed :test #'string-equal) (first installed)))
         (tuatara.defaults::*latex-main-font* latin)
         (tuatara.defaults::*latex-sans-font* latin)
         (tuatara.defaults::*latex-mono-font* latin))
    (when latin
      ;; a note that sets no look of its own: the article style, its fonts chosen
      (multiple-value-bind (latex named map missing)
          (tuatara.defaults::org-latex-document (format nil "* One~%Done ✓ and a hieroglyph 𓀀.~%") :enhanced t)
        (declare (ignore named map))
        (true (search "\\usepackage{parskip}" latex))
        (true (search (format nil "\\setmainfont{~a}" latin) latex))
        ;; the code font's ligatures off: :: and // are copied from the PDF as written
        (true (search (format nil "\\setmonofont[Scale=MatchLowercase,Contextuals=NoAlternate,Ligatures=NoCommon]{~a}" latin) latex))
        ;; a character the fonts lack: in a font that has it, or named as missing; never both
        (dolist (c (list (code-char #x2713) (code-char #x13000)))
          (let ((wrapped (search (format nil "{}~c}" c) latex))
                (named (assoc c missing))
                (has (tuatara.defaults::characters-have-p (tuatara.defaults::font-characters latin) (char-code c))))
            ;; one the font has is left as it is; else it is in a font that has it, or named as missing
            (if has
                (false (or wrapped named))
                (progn (true (or wrapped named))
                       (false (and wrapped named))))))
        ;; the hieroglyph's place: the line of the LaTeX it is on
        (let ((entry (assoc (code-char #x13000) missing)))
          (when entry
            (true (search "hieroglyph" (nth (1- (cdr entry)) (uiop:split-string latex :separator '(#\Newline)))))))))
    ;; a note with a look of its own (#+latex_header): Org's layout, fonts as it sets them
    (let ((latex (tuatara.defaults::org-latex-document (format nil "#+latex_header: \\usepackage{x}~%Text~%") :enhanced t)))
      (false (search "\\usepackage{parskip}" latex))
      (false (search "\\setmainfont" latex)))
    ;; the export as Org makes it: no chosen fonts, no fallbacks
    (let ((latex (tuatara.defaults::org-latex-document (format nil "Done ✓~%"))))
      (false (search "\\setmainfont" latex))
      (false (search "tuataraFallback" latex)))))

(define-test (latex-export font-own-names)
  ;; a font by the name it gives itself, not one fontconfig's rules put before it
  (is string= "STIX Two Text" (tuatara.defaults::own-family "STIX,STIX Two Text" "en"))
  (is string= "STIX Two Text" (tuatara.defaults::own-family "STIX,STIX Two Text,STIX Two Text Medium" "en,en"))
  (is string= "DejaVu Math TeX Gyre" (tuatara.defaults::own-family "DejaVu Serif,DejaVu Math TeX Gyre" "en"))
  (is string= "Noto Sans" (tuatara.defaults::own-family "Noto Sans" "en"))
  (is string= "Noto Sans" (tuatara.defaults::own-family "Noto Sans" "")))

(define-test (latex-export pictures-made-png)
  ;; the PNG written: its signature, its header's size and kind, the chunks' checks
  (let* ((image (tuatara.frontend:make-image 2 1 3 (make-array 6 :element-type '(unsigned-byte 8) :initial-contents '(255 0 0 0 255 0))))
         (png (tuatara.defaults::png-octets image)))
    (is equalp #(137 80 78 71 13 10 26 10) (subseq png 0 8))
    (is equalp #(0 0 0 2 0 0 0 1 8 2) (subseq png 16 26))
    (is = (tuatara.defaults::crc32 png 12 29)
        (tuatara.io:octets-be png 29 4)))
  (with-test-directory (dir)
    (alexandria:write-string-into-file
     "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"40\" height=\"20\"><rect width=\"40\" height=\"20\" fill=\"#3a6ea5\"/></svg>"
     (merge-pathnames "drawing.svg" dir))
    (alexandria:write-string-into-file "not a picture" (merge-pathnames "broken.svg" dir))
    (alexandria:write-byte-vector-into-file *tiny-png* (merge-pathnames "photo.png" dir) :if-exists :supersede)
    (let ((file (namestring (merge-pathnames "notes.org" dir)))
          (text (format nil "[[file:drawing.svg]]~%~%[[file:photo.png]]~%~%[[file:broken.svg]]~%")))
      (multiple-value-bind (latex named map missing notes) (tuatara.defaults::org-latex-document text :file file :enhanced t)
        (declare (ignore named map missing))
        ;; a PNG or the like as it is; a drawing made into a PNG in the cache, drawn for print
        (true (search (format nil "{~a}" (namestring (merge-pathnames "photo.png" dir))) latex))
        (let* ((at (search "latex/pictures/" latex))
               (png (and at (subseq latex (position #\{ latex :end at :from-end t) (position #\} latex :start at)))))
          (true png)
          (when png
            (let ((octets (alexandria:read-file-into-byte-vector (subseq png 1))))
              (is equalp #(137 80 78 71) (subseq octets 0 4))
              ;; 40 pixels wide drawn at 300 dots an inch, not at its own size
              (true (> (tuatara.io:octets-be octets 18 2) 40)))))
        ;; one that is no picture: its name in a frame, and why said
        (true (search "\\fbox{\\texttt{broken.svg}}" latex))
        (true (some (lambda (n) (search "broken.svg is no picture" n)) notes)))
      ;; the export as Org makes it: a drawing is a link, as Org has it
      (false (search "latex/pictures/" (tuatara.defaults::org-latex-document text :file file))))))

(define-test (latex-export part-alone)
  (with-test-directory (dir)
    (let ((file (merge-pathnames "notes.org" dir))
          (tuatara-user:*latex-engines* (list "no-such-latex-program")))
      (alexandria:write-string-into-file
       (format nil "#+title: Whole~%#+options: toc:nil~%* First~%first text~%* Second~%:PROPERTIES:~%:EXPORT_FILE_NAME: second~%:END:~%second text~%Some \\nonsense~%") file)
      ;; C-u: the subtree at point alone, its headline the title, its file name its own
      (export-session file (list "M-> C-u" (export-command "org-export-latex")))
      (let ((latex (alexandria:read-file-into-string (merge-pathnames "second.tex" dir))))
        (true (search "\\title{Second}" latex))
        (true (search "second text" latex))
        (false (search "first text" latex))
        ;; the document's options still hold
        (false (search "\\tableofcontents" latex)))
      ;; an error in a part alone is told at the line of the file it is on
      (let ((tuatara-user:*latex-engines* (list (stand-in-latex dir "xelatex-broken" :fail t))))
        (let ((f (export-session file (list "M-> C-u" (export-command "org-export-latex-pdf")) :width 220)))
          (true (tuatara.frontend.fake:fake-frame-showing f "notes.org:10: Undefined control sequence."))))
      ;; a selection alone: the lines it is on, under the document's keywords
      (export-session file (list "g g j j C-SPC j j" (export-command "org-export-latex")))
      (let ((latex (alexandria:read-file-into-string (merge-pathnames "notes.tex" dir))))
        (true (search "first text" latex))
        (false (search "second text" latex))
        (true (search "\\title{Whole}" latex))))))

(define-test (latex-export hidden-where-it-cannot-be)
  (flet ((refusal (text &rest keys)
           (handler-case (progn (apply #'tuatara.defaults::org-latex-document text :enhanced t keys) nil)
             (error (e) (princ-to-string e)))))
    ;; where Org reads no macros: refused, the line named
    (true (search "{{{redact(...)}}} on line 3 is where Org reads no macros"
                  (refusal (format nil "* One~%#+begin_src sh~%echo {{{redact(John Otago)}}}~%#+end_src~%"))))
    (true (search "on line 2" (refusal (format nil "Text~%See =code {{{redact(John Otago)}}}= here.~%"))))
    (true (search "on line 1" (refusal (format nil "[[https://x.example/{{{redact(John Otago)}}}][a link]]~%"))))
    (true (search "on line 2" (refusal (format nil "Text~%#+latex: \\textbf{{{{redact(John Otago)}}}}~%"))))
    (true (search "on line 1" (refusal (format nil "#+latex_header: % {{{redact(John Otago)}}}~%Text~%"))))
    ;; a style printing a keyword as written (report's logo, a file's name); one it prints as Org text is let be
    (true (search "#+logo: holds {{{redact(...)}}}"
                  (refusal (format nil "#+template: ~a~%#+reference: {{{redact(R-1)}}}~%#+logo: {{{redact(x)}}}.png~%Text~%" (report-template)))))
    (false (refusal (format nil "#+template: ~a~%#+reference: {{{redact(R-1)}}}~%Text~%" (report-template))))
    ;; where it works: in text, tables, headlines, and a keyword printed as Org text
    (false (refusal (format nil "#+title: About {{{redact(Bo)}}}~%* {{{redact(Bo)}}}~%Text {{{redact(Bo)}}}~%| {{{redact(Bo)}}} |~%")))
    ;; protect shown (protect:nil, as standard): nothing hidden, nothing refused, in a block too
    (false (refusal (format nil "#+begin_src sh~%echo {{{protect(Ann)}}}~%#+end_src~%")))
    (true (search "on line 3" (refusal (format nil "#+options: protect:t~%#+begin_src sh~%echo {{{protect(Ann)}}}~%#+end_src~%"))))
    ;; a part alone: the line of the whole file
    (true (search "on line 12" (refusal (format nil "#+begin_example~%{{{redact(Bo)}}}~%#+end_example~%") :lines (vector 11 12 13))))))

(define-test (latex-export redactions-read-back)
  ;; hidden text is written one way and read back by the one pattern: as plain
  ;; words for the Word document's titles, as as many letters for a table's width
  (let* ((tuatara.defaults::*org-latex* (tuatara.defaults::make-org-latex "" nil nil))
         (hidden (tuatara.defaults::org-latex-redaction "redact" "Bo Raglan")))
    (false (search "Bo Raglan" hidden))
    (is equalp (list hidden #("9")) (multiple-value-list (cl-ppcre:scan-to-strings tuatara.defaults::*org-redaction-scanner* hidden)))
    (is string= "About [redacted]" (tuatara.defaults::docx-plain-title (concatenate 'string "About " hidden)))
    (is string= "About nnnnnnnnn" (tuatara.defaults::org-latex-visible-text (concatenate 'string "About " hidden)))
    ;; the Word document's part alone
    (is string= "[redacted]" (tuatara.defaults::docx-plain-title "@@docx:redacted:3@@"))
    ;; shown when not hidden
    (is string= "Ann Napier" (tuatara.defaults::org-latex-redaction "protect" "Ann Napier"))))

(define-test (latex-export redactions-checked)
  ;; found: the whole (any case), a word of it alone, a token a hidden part completes; never a time that only ends alike
  (let ((findings (tuatara.defaults::redaction-findings
                   (format nil "Subject {{{redact(John Otago)}}} used 90.233.1.{{{redact(40)}}}.~%Later john otago wrote; Otago again; from 90.233.1.40 at 08:12:40.~%")
                   :org)))
    (is equal '("john otago" "Otago" "90.233.1.40") (mapcar #'second findings)))
  ;; protect counts only when its text is hidden
  (is = 0 (length (tuatara.defaults::redaction-findings (format nil "{{{protect(Ann Nelson)}}} and Ann Nelson~%") :org)))
  (is = 1 (length (tuatara.defaults::redaction-findings (format nil "#+options: protect:t~%{{{protect(Ann Nelson)}}} and Ann Nelson~%") :org)))
  ;; a LaTeX file's own commands
  (is equal '("John Otago") (mapcar #'second (tuatara.defaults::redaction-findings (format nil "\\R{John Otago} met John Otago.~%") :latex)))
  (with-test-directory (dir)
    (let ((file (merge-pathnames "leak.org" dir))
          (tuatara-user:*latex-engines* (list (stand-in-latex dir "xelatex"))))
      (alexandria:write-string-into-file (format nil "* Survey~%Subject {{{redact(Bo Raglan)}}}.~%Later Bo Raglan wrote.~%") file)
      ;; M-x check-redactions: each place listed, n going to it, RET showing it
      (multiple-value-bind (f editor)
          (export-session file (list (export-command "check-redactions")
                                     (lambda () (tuatara.defaults::pop-to-buffer (tuatara.buffer:find-buffer "*Redactions*")))
                                     "n RET")
                          :width 200)
        (true (tuatara.frontend.fake:fake-frame-showing f "1 place in leak.org shows text hidden elsewhere in it: see *Redactions*"))
        (let ((list (tuatara.buffer:find-buffer "*Redactions*")))
          (true (search "Bo Raglan (redact), 1 place:" (tuatara.buffer:buffer-string list)))
          (true (search "| Later Bo Raglan wrote." (tuatara.buffer:buffer-string list)))
          ;; the place one entry on both its lines
          (let ((entry (tuatara.defaults::list-entry-at list (tuatara.buffer:buffer-goto-line list 3))))
            (is eq :place (first entry))
            (is eq entry (tuatara.defaults::list-entry-at list (tuatara.buffer:buffer-goto-line list 4)))))
        (is string= "leak.org" (current-name editor))
        (is = 2 (tuatara.buffer:buffer-line-number (tuatara.buffer:find-buffer "leak.org") (session-point editor))))
      ;; before the enhanced export: asked; no, and nothing is made
      (let ((f (export-session file (list (export-command "org-export-latex-pdf-enhanced") "n RET") :width 200)))
        (true (tuatara.frontend.fake:fake-frame-showing f "1 place shows text hidden elsewhere (see *Redactions*); make the PDF anyway?"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Not exported: *Redactions* lists where hidden text shows"))
        (false (probe-file (merge-pathnames "leak.pdf" dir))))
      ;; yes: made
      (export-session file (list (export-command "org-export-latex-pdf-enhanced") "y e s RET") :width 200)
      (true (probe-file (merge-pathnames "leak.pdf" dir))))))

(defparameter *export-drawer-note*
  (format nil "#+title: Settings~%* Plain~%:EXPORT:~%numbered: no~%columns: 2~%:END:~%Words.~%~
               :EXPORT:~%widths: 30% 5cm *~%align: left right center~%position: left~%repeat header: no~%:END:~%~
               | a | b | c |~%|---+---+---|~%| 1 | 2 | 3 |~%~%~
               :EXPORT:~%style: roman~%:END:~%- one~%- two~%~%~
               :EXPORT:~%style: none~%font: small~%:END:~%1. first~%~%~
               :EXPORT:~%columns: 2~%indent: 2em~%:END:~%#+begin_example~%x~%#+end_example~%~%~
               :EXPORT:~%width: 60%~%place: here~%:END:~%#+caption: A picture~%[[file:pic.png]]~%~%~
               * Annex :EXPORT:~%:EXPORT:~%annex: yes~%:END:~%Text.~%")
  "A note using an :EXPORT: drawer for each kind of thing it sets.")

(define-test (latex-export export-drawer)
  (with-test-directory (dir)
    (alexandria:write-byte-vector-into-file (coerce (png-octets 2 2) '(vector (unsigned-byte 8))) (merge-pathnames "pic.png" dir))
    (let ((file (namestring (merge-pathnames "note.org" dir))))
      (let ((latex (tuatara.defaults::org-latex-document *export-drawer-note* :file file :enhanced t)))
        ;; a heading: no number, what is under it in two columns
        (true (search (format nil "\\section*{Plain}") latex))
        (true (search "\\begin{multicols}{2}" latex))
        ;; a table: its widths and alignments, against the left margin, its header once
        (true (search ">{\\raggedright\\arraybackslash}p{0.300\\linewidth} >{\\raggedleft\\arraybackslash}p{5cm} >{\\centering\\arraybackslash}X" latex))
        ;; in the heading's two columns, kept whole in its column (position right is tested outside columns)
        (true (search "\\begin{table}[H]" latex))
        (false (search "\\endhead" latex))
        ;; lists: roman numbers; no bullets, in small type
        (true (search "\\renewcommand{\\labelenumi}{\\roman{enumi}.}" latex))
        (true (search "\\renewcommand{\\labelitemi}{}" latex))
        (true (search (format nil "{\\small~%\\begin{itemize}") latex))
        ;; a block: in from the left, in two columns
        (true (search "\\setlength{\\leftmargin}{2em}" latex))
        ;; a picture: 60% wide, exactly here
        (true (search "width=0.600\\linewidth" latex))
        (true (search "\\begin{figure}[H]" latex))
        ;; annexes begin at the heading set so
        (true (search (format nil "\\appendix~%") latex))
        ;; the drawers' lines are never printed, nor by the export as Org makes it
        (false (search "widths:" latex))
        (false (search "widths:" (tuatara.defaults::org-latex-document *export-drawer-note* :file file))))
      ;; a picture beside the paragraph after it, that paragraph written once
      (let ((latex (tuatara.defaults::org-latex-document
                    (format nil ":EXPORT:~%beside: right~%:END:~%[[file:pic.png]]~%~%The words beside it.~%") :file file :enhanced t)))
        (true (search "\\begin{minipage}[c]{0.47\\linewidth}" latex))
        (is = 1 (count-matches "The words beside it" latex))
        (true (< (search "The words beside it" latex) (search "includegraphics" latex))))))
  ;; what cannot be is said, with the drawer's line, and left out
  (let ((notes (nth-value 4 (tuatara.defaults::org-latex-document
                             (format nil "Text~%~%:EXPORT:~%colour: red~%widths: 1cm~%:END:~%- item~%") :enhanced t))))
    (true (find-if (lambda (n) (search "Line 3, an :EXPORT: drawer: colour is no setting" n)) notes))
    (true (find-if (lambda (n) (search "widths does not apply to list" n)) notes))))

(define-test (latex-export export-drawer-written)
  ;; M-x org-insert-export-drawer: the settings for what point is on, and Org's own exports told to leave it out
  (flet ((after (keys text)
           (multiple-value-bind (f editor) (run-keys (format nil "M-x o r g - m o d e RET ~a" keys) :text text :width 160 :height 10)
             (declare (ignore f))
             (session-text editor))))
    (let ((text (after "j j M-x o r g - i n s e r t - e x p o r t - d r a w e r RET"
                       (format nil "#+title: T~%* Head~%| a | b |~%"))))
      (true (search (format nil "#+title: T~%#+options: d:(not \"EXPORT\" \"LOGBOOK\")~%") text))
      (true (search (format nil ":EXPORT:~%new page: auto~%space before: auto~%space after: auto~%spacing: auto~%columns: auto~%page: auto~%font: auto~%indent: auto~%width: auto~%widths: auto~%align: auto~%position: auto~%repeat header: auto~%lines: auto~%:END:~%| a | b |") text)))
    (let ((text (after "j M-x o r g - i n s e r t - e x p o r t - d r a w e r RET"
                       (format nil "#+options: toc:nil~%* Head~%Text~%"))))
      (true (search "#+options: toc:nil d:(not \"EXPORT\" \"LOGBOOK\")" text))
      (true (search (format nil "* Head~%:EXPORT:~%numbered: auto~%annex: auto~%contents: auto~%new page: auto~%space before: auto~%space after: auto~%spacing: auto~%columns: auto~%page: auto~%font: auto~%:END:~%Text") text)))))

(define-test (latex-export links-in-the-document)
  (let ((text (format nil "* Findings~%See [[Logons]], [[Logons][the logons]], [[*Findings]], [[*Notes]], [[here]].~%~%~
                           #+name: Logons~%#+caption: Logons~%| a | b |~%~%* Notes~%:PROPERTIES:~%:UNNUMBERED: t~%:END:~%<<here>> Text.~%")))
    ;; with no description, as Org writes it: the number
    (let ((latex (tuatara.defaults::org-latex-document text)))
      (true (search "See \\ref{" latex))
      (true (search "\\hyperref[" latex)))
    (let ((latex (tuatara.defaults::org-latex-document text :enhanced t)))
      ;; the kind and number: Table 1, section 1
      (is = 2 (count-matches "\\autoref{" latex))
      ;; described: the words, linked
      (true (search "]{the logons}" latex))
      ;; a heading with no number by its title, a target by its words
      (true (search "]{Notes}" latex))
      (true (search "]{here}" latex)))))

(defun docx-text (octets)
  "The octets of a Word document Tuatara wrote, its files stored as they are, read as text."
  (sb-ext:octets-to-string (coerce octets '(vector (unsigned-byte 8))) :external-format '(:utf-8 :replacement #\?)))

(define-test (latex-export docx-written)
  (with-test-directory (dir)
    (alexandria:write-byte-vector-into-file (coerce (png-octets 2 2) '(vector (unsigned-byte 8))) (merge-pathnames "pic.png" dir))
    (let* ((file (namestring (merge-pathnames "note.org" dir)))
           (note (format nil "#+title: The Survey~%#+author: {{{P(Ann Nelson)}}}~%#+options: protect:t~%~
                              * Findings~%Subject {{{R(John Otago)}}} met *Bo*.[fn:1] See [[Logons]] and [[*Annex][the annex]].~%~%~
                              #+name: Logons~%#+caption: Logons~%| User | n |~%|------+---|~%| a | 1 |~%~%~
                              :EXPORT:~%style: roman~%:END:~%- one~%- two that goes~%  on a line of its own~%~%~
                              #+caption: A picture~%[[file:pic.png]]~%~%~
                              * Plain~%:EXPORT:~%numbered: no~%:END:~%Text.~%~%~
                              * Annex :appendix:~%Words.~%~%[fn:1] A note.~%")))
      (multiple-value-bind (octets named notes) (tuatara.defaults::org-docx-document note :file file)
        (declare (ignore named))
        (is equal '() notes)
        (let ((text (docx-text octets)))
          ;; a zip archive any reader takes
          (let ((path (merge-pathnames "note.docx" dir)))
            (alexandria:write-byte-vector-into-file octets path)
            (is = 0 (nth-value 2 (uiop:run-program (list "unzip" "-tq" (namestring path)) :ignore-error-status t))))
          ;; what is hidden is nowhere in it, a bar of as many letters in its place
          (false (search "John Otago" text))
          (false (search "Ann Nelson" text))
          (true (search ">nnnnnnnnnn<" text))
          ;; headings numbered as the PDF numbers them, the annex from a page of its own, numbered anew
          (true (search ">1  </w:t>" text))
          (true (search "<w:pageBreakBefore/>" text))
          ;; the annex lettered, as LaTeX letters it
          (true (search ">A  </w:t>" text))
          (true (search "Heading1" text))
          ;; a table's caption, its header row again on each page; a roman list; a footnote; links to bookmarks
          (true (search "Table 1: " text))
          (true (search "<w:tblHeader/>" text))
          (true (search "lowerRoman" text))
          (true (search "<w:footnoteReference w:id=\"2\"/>" text))
          (true (search "w:anchor=\"_Tuatara" text))
          (true (search ">Table 1</w:t>" text))
          ;; the picture in the file, captioned
          (true (search "word/media/image1.png" text))
          (true (search "Figure 1: " text))
          ;; a line end and the spaces setting in the next line one space; air under a table
          (true (search "two that goes on a line" text))
          (true (search "</w:tbl>
<w:p><w:pPr><w:spacing w:before=\"0\" w:after=\"0\" w:line=\"160\"" text))
          ;; the drawer's lines never written
          (false (search "numbered:" text)))))))

(define-test (latex-export docx-sizes)
  ;; a table too wide in 11 points set in 10 when that is enough, its code block in the text's size
  (let ((text (docx-text (tuatara.defaults::org-docx-document
                          (format nil "| ~a | ~a |~%~%#+begin_example~%short line~%#+end_example~%"
                                  (make-string 40 :initial-element #\a) (make-string 29 :initial-element #\b))))))
    (true (search "<w:sz w:val=\"20\"/>" text))
    (true (search "<w:pStyle w:val=\"SourceCode\"/></w:pPr><w:r><w:t xml:space=\"preserve\">short line" text))))

(define-test (latex-export docx-command)
  (with-test-directory (dir)
    (let ((file (merge-pathnames "survey.org" dir)))
      (alexandria:write-string-into-file (format nil "#+title: Survey~%* One~%Subject {{{R(Bo Raglan)}}}.~%") file)
      (let ((f (export-session file (export-command "org-export-docx") :width 200)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Wrote "))
        (true (probe-file (merge-pathnames "survey.docx" dir)))
        (false (search "Bo Raglan" (docx-text (alexandria:read-file-into-byte-vector (merge-pathnames "survey.docx" dir)))))))))

(defun write-docx-template (folder document &key (header nil) title-page size)
  "Write a Word template, unpacked, into FOLDER: DOCUMENT its word/document.xml
body (paragraphs and tables), HEADER, when given, a header's paragraphs, SIZE
its text's size in half points, when given."
  (flet ((put (name text)
           (let ((path (uiop:parse-native-namestring (concatenate 'string (namestring folder) name))))
             (ensure-directories-exist path)
             (alexandria:write-string-into-file text path :if-exists :supersede))))
    (let ((ns "xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\""))
      (put "[Content_Types].xml" "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\"><Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/><Default Extension=\"xml\" ContentType=\"application/xml\"/><Override PartName=\"/word/document.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml\"/><Override PartName=\"/word/styles.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml\"/><Override PartName=\"/word/header1.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.header+xml\"/></Types>")
      (put "_rels/.rels" "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument\" Target=\"word/document.xml\"/></Relationships>")
      (put "word/_rels/document.xml.rels" "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles\" Target=\"styles.xml\"/><Relationship Id=\"rId2\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/header\" Target=\"header1.xml\"/></Relationships>")
      (put "word/styles.xml" (format nil "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><w:styles ~a>~@[<w:docDefaults><w:rPrDefault><w:rPr><w:sz w:val=\"~d\"/></w:rPr></w:rPrDefault></w:docDefaults>~]<w:style w:type=\"paragraph\" w:default=\"1\" w:styleId=\"Normal\"><w:name w:val=\"Normal\"/><w:rPr><w:sz w:val=\"20\"/></w:rPr></w:style><w:style w:type=\"paragraph\" w:styleId=\"Heading1\"><w:name w:val=\"heading 1\"/><w:rPr><w:color w:val=\"C00000\"/></w:rPr></w:style></w:styles>" ns size))
      (put "word/header1.xml" (format nil "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><w:hdr ~a>~a</w:hdr>" ns (or header "<w:p/>")))
      (put "word/document.xml" (format nil "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><w:document ~a><w:body>~a<w:sectPr><w:headerReference w:type=\"default\" r:id=\"rId2\"/><w:pgSz w:w=\"12240\" w:h=\"15840\"/><w:pgMar w:top=\"1440\" w:right=\"1440\" w:bottom=\"1440\" w:left=\"1440\" w:header=\"720\" w:footer=\"720\" w:gutter=\"0\"/>~:[~;<w:titlePg/>~]</w:sectPr></w:body></w:document>" ns document title-page)))))

(define-test (latex-export docx-template-filled)
  (with-test-directory (dir)
    (let* ((templates (merge-pathnames "templates/" dir))
           (file (namestring (merge-pathnames "note.org" dir)))
           (text (format nil "#+title: The Survey~%#+template: ./templates/survey~%#+organisation: Example Ltd~%#+reference: R-7~%#+copy_to: Ann~%#+copy_to: Bo~%* Findings~%Subject {{{R(John Otago)}}}.~%")))
      (ensure-directories-exist templates)
      (alexandria:write-string-into-file "\\documentclass{article}\\begin{document}{{{organisation}}} {{{reference}}} {{{body}}}\\end{document}"
                                         (merge-pathnames "survey.tex" templates) :if-exists :supersede)
      (write-docx-template
       (merge-pathnames "survey/" templates)
       (concatenate 'string
                    ;; a place Word split among runs of different looks
                    "<w:p><w:r><w:rPr><w:b/></w:rPr><w:t>Organisation: {{{organi</w:t></w:r><w:proofErr w:type=\"spellStart\"/><w:r><w:t>sation}}}</w:t></w:r></w:p>"
                    "<w:p><w:r><w:t>{{{contents}}}</w:t></w:r></w:p>"
                    ;; a row kept, a row taken away
                    "<w:tbl><w:tr><w:tc><w:p><w:r><w:t>{{{#copy_to}}}Copy to</w:t></w:r></w:p></w:tc><w:tc><w:p><w:r><w:t>{{{copy_to}}}{{{/copy_to}}}</w:t></w:r></w:p></w:tc></w:tr>"
                    "<w:tr><w:tc><w:p><w:r><w:t>{{{#team}}}Team</w:t></w:r></w:p></w:tc><w:tc><w:p><w:r><w:t>{{{team}}}{{{/team}}}</w:t></w:r></w:p></w:tc></w:tr></w:tbl>"
                    ;; paragraphs kept only with no unit
                    "<w:p><w:r><w:t>{{{^unit}}}</w:t></w:r></w:p><w:p><w:r><w:t>No unit given.</w:t></w:r></w:p><w:p><w:r><w:t>{{{/unit}}}</w:t></w:r></w:p>"
                    "<w:p><w:r><w:t>{{{body}}}</w:t></w:r></w:p>")
       :header "<w:p><w:r><w:t>Reference {{{reference}}}</w:t></w:r></w:p>")
      (multiple-value-bind (octets named notes) (tuatara.defaults::org-docx-document text :file file)
        (declare (ignore named))
        (let ((all (docx-text octets)))
          ;; the split place filled in the look of the run it began in, the proofing mark kept
          (true (search "<w:r><w:rPr><w:b/></w:rPr><w:t xml:space=\"preserve\">Organisation: </w:t></w:r><w:r><w:rPr><w:b/></w:rPr><w:t xml:space=\"preserve\">Example Ltd</w:t></w:r>" all))
          (false (search "{{{" all))
          (true (search ">Ann</w:t></w:r><w:r><w:br/></w:r>" all))
          (false (search "Team" all))
          (true (search "No unit given." all))
          ;; the note's text where {{{body}}} was, its heading in the template's own style; the contents
          (true (search ">Findings</w:t>" all))
          (true (search "w:val=\"C00000\"" all))
          (true (search "TOC1" all))
          ;; the header filled; the template's page kept; what is hidden nowhere
          (true (search ">R-7</w:t>" all))
          (true (search "w:w=\"12240\"" all))
          (false (search "John Otago" all))
          ;; the two halves ask for the same places but these: said
          (true (find-if (lambda (n) (and (search "LaTeX half has no place for" n) (search "{{{copy_to}}}" n) (not (search "{{{contents}}}" n)))) notes))
          (let ((path (merge-pathnames "note.docx" dir)))
            (alexandria:write-byte-vector-into-file octets path)
            (is = 0 (nth-value 2 (uiop:run-program (list "unzip" "-tq" (namestring path)) :ignore-error-status t))))))
      ;; a place printed as written refuses a keyword holding a name to hide, as the LaTeX half does
      (alexandria:write-string-into-file "\\documentclass{article}\\begin{document}{{{body}}}\\end{document}"
                                         (merge-pathnames "raw.tex" templates) :if-exists :supersede)
      (write-docx-template (merge-pathnames "raw/" templates) "<w:p><w:r><w:t>{{{reference:raw}}}</w:t></w:r></w:p><w:p><w:r><w:t>{{{body}}}</w:t></w:r></w:p>")
      (true (search "#+reference: holds {{{redact(...)}}}"
                    (handler-case (progn (tuatara.defaults::org-docx-document
                                          (format nil "#+template: ./templates/raw~%#+reference: {{{redact(R-1)}}}~%Text~%") :file file)
                                         "")
                      (error (e) (princ-to-string e)))))
      ;; a template with no Word half: Tuatara's own look, and said
      (true (find-if (lambda (n) (search "has no Word half" n))
                     (nth-value 2 (tuatara.defaults::org-docx-document (format nil "#+template: nosuch~%Text~%") :file file)))))))

(define-test (latex-export docx-template-pages)
  ;; the note's contents begin a page when their paragraph did; with sections of columns in the
  ;; text, the first page (its own header and footer) stays the first section's
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "note.org" dir))))
      (write-docx-template (merge-pathnames "pages/" dir)
                           "<w:p><w:r><w:t>Cover</w:t></w:r></w:p><w:p><w:pPr><w:pageBreakBefore/></w:pPr><w:r><w:t>{{{contents}}}</w:t></w:r></w:p><w:p><w:r><w:t>{{{body}}}</w:t></w:r></w:p>"
                           :title-page t)
      (let ((all (docx-text (tuatara.defaults::org-docx-document
                             (format nil "#+template: ./pages~%* One~%:EXPORT:~%columns: 2~%:END:~%Text in two columns.~%")
                             :file file))))
        (true (< (search "<w:br w:type=\"page\"/>" all) (search "TOCHeading" all)))
        (let ((first-break (search "<w:sectPr>" all)) (last (search "<w:sectPr>" all :from-end t)))
          (true (search "<w:titlePg/>" all :start2 first-break :end2 (search "</w:sectPr>" all :start2 first-break)))
          (false (search "<w:titlePg/>" all :start2 last))
          ;; the text after the columns on their page, the columns balanced
          (true (search "<w:type w:val=\"continuous\"/><w:pgSz" all :start2 last)))))))

(define-test (latex-export docx-template-tables)
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "note.org" dir)))
          (note (format nil "#+template: ./left~%| ~a | ~a |~%" (make-string 50 :initial-element #\a) (make-string 30 :initial-element #\b))))
      (write-docx-template (merge-pathnames "left/" dir) "<w:p><w:r><w:t>{{{body}}}</w:t></w:r></w:p>" :size 20)
      ;; centred, as the template's LaTeX half sets tables
      (alexandria:write-string-into-file "\\documentclass{article}\\begin{document}{{{body}}}\\end{document}" (merge-pathnames "left.tex" dir))
      (true (search "<w:jc w:val=\"center\"/>" (docx-text (tuatara.defaults::org-docx-document note :file file))))
      ;; against the left margin, as its LaTeX half sets them
      (alexandria:write-string-into-file "\\documentclass{article}\\setlength{\\LTleft}{0pt}\\begin{document}{{{body}}}\\end{document}"
                                         (merge-pathnames "left.tex" dir) :if-exists :supersede)
      (let ((all (docx-text (tuatara.defaults::org-docx-document note :file file))))
        (true (search "<w:jc w:val=\"left\"/>" all))
        ;; the template's text 10 points: the table too wide at 10 set in 10/11 of it, 9 points
        (true (search "<w:sz w:val=\"18\"/>" all)))
      ;; a table's own position before the template's
      (true (search "<w:jc w:val=\"center\"/>"
                    (docx-text (tuatara.defaults::org-docx-document
                                (format nil "#+template: ./left~%:EXPORT:~%position: center~%:END:~%| a | b |~%") :file file)))))))

(define-test (latex-export docx-font-width)
  ;; a font not installed: none measured (Courier New's 0.6 taken); a monospaced one installed: its width
  (false (tuatara.defaults::font-character-ratio "No Such Font Tuatara"))
  (let* ((listed (tuatara.defaults::fontconfig-output "fc-list" ":spacing=mono" "family"))
         (family (and listed (first (uiop:split-string (first (uiop:split-string listed :separator '(#\Newline))) :separator ",")))))
    (when (and family (plusp (length family)))
      (let ((ratio (tuatara.defaults::font-character-ratio family)))
        (true (and ratio (< 2/5 ratio 7/10)) "~a measured ~a" family ratio)))))

(define-test (latex-export redactions-as-exported)
  ;; what the check calls hidden is what the export hides: protect:t in a setup file counts, redact:nil too
  (with-test-directory (dir)
    (alexandria:write-string-into-file (format nil "#+options: protect:t~%") (merge-pathnames "setup.org" dir))
    (let ((directory (namestring dir)))
      (flet ((hidden (text) (mapcar #'fourth (tuatara.defaults::hidden-spans text :org directory))))
        (is equal '("redact") (hidden (format nil "{{{R(Ann)}}} and {{{P(Bo)}}}~%")))
        (is equal '("redact" "protect") (hidden (format nil "#+setupfile: setup.org~%{{{R(Ann)}}} and {{{P(Bo)}}}~%")))
        (is equal '("protect") (hidden (format nil "#+setupfile: setup.org~%#+options: redact:nil~%{{{R(Ann)}}} and {{{P(Bo)}}}~%")))
        (is equal '() (hidden (format nil "#+options: redact:nil~%{{{R(Ann)}}} and {{{P(Bo)}}}~%")))))))

(define-test (latex-export table-width-and-position)
  ;; full: margin to margin, the columns sharing it as their text does; right: against the right margin
  (with-test-directory (dir)
    (let* ((file (namestring (merge-pathnames "note.org" dir)))
           (note (lambda (drawer) (format nil ":EXPORT:~%~a~%:END:~%| name | a longer column |~%|-~%| x | y |~%" drawer))))
      (let ((latex (tuatara.defaults::org-latex-document (funcall note "width: full") :file file :enhanced t)))
        (true (search "\\begin{xltabular}{\\textwidth}" latex))
        (true (search "\\hsize=0.421\\hsize}X" latex))
        (true (search "\\hsize=1.579\\hsize}X" latex)))
      (let ((latex (tuatara.defaults::org-latex-document (funcall note (format nil "width: 80%~%position: right")) :file file :enhanced t)))
        (true (search "\\begin{xltabular}{0.800\\linewidth}" latex))
        (true (search "\\setlength{\\LTleft}{\\fill}\\setlength{\\LTright}{0pt}" latex)))
      (let* ((all (docx-text (tuatara.defaults::org-docx-document (funcall note "width: full") :file file)))
             (columns (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "(?<=<w:gridCol w:w=\")\\d+" all)))
             (table (parse-integer (first (cl-ppcre:all-matches-as-strings "(?<=<w:tblW w:w=\")\\d+" all)))))
        (is = table (reduce #'+ columns))
        (true (< (first columns) (second columns)))
        (true (> table 8000)))
      (let ((all (docx-text (tuatara.defaults::org-docx-document (funcall note "position: right") :file file))))
        (true (search "<w:jc w:val=\"right\"/>" all))))))

(define-test (latex-export digests-within-their-columns)
  ;; a long digest goes on to the next line every *LATEX-CODE-RUN-LIMIT* characters
  (is = 16 (tuatara.defaults::org-latex-longest-piece (format nil "=~a=" (make-string 64 :initial-element #\a))))
  (true (search "\\texttt{d41d8cd98f00b204\\codebreak{}e9800998ecf8427e}"
                (tuatara.defaults::org-latex-document (format nil "=d41d8cd98f00b204e9800998ecf8427e=~%") :enhanced t)))
  ;; a table of five columns of code and prose, too wide for its longest parts at the normal
  ;; size, is printed smaller, and no column is narrower than its longest part
  (let* ((row "| =[[:sha1:]]= | a SHA-1 digest, 40 hex digits (FIPS 180-4) | =[[:sha1:]]= | =da39a3ee5e6b4b0d3255bfef95601890afd80709 and more= | =da39a3ee5e6b4b0d3255bfef95601890afd80709= |")
         (latex (tuatara.defaults::org-latex-document
                 (format nil "| Written | Means | Pattern | In | Finds |~%|-~%~a~%~a~%" row row) :enhanced t))
         (shares (mapcar (lambda (s) (let ((*read-eval* nil)) (read-from-string s)))
                         (cl-ppcre:all-matches-as-strings "(?<=\\\\hsize=)[0-9.]+(?=\\\\hsize)" latex))))
    (true (search (format nil "{\\footnotesize~%\\begin{xltabular}") latex))
    (true shares "the wrapping columns' shares: ~s" shares)
    (true (every (lambda (share) (> share 0.6)) shares) "shares ~s" shares)))

(define-test (latex-export code-within-the-margins)
  (let ((text (format nil "Run =M-x org-export-latex-pdf=, then =C-x= and =10.25=.~%"))
        (enhanced (lambda (text) (tuatara.defaults::org-latex-document text :enhanced t))))
    ;; long names may go on to the next line, keys and numbers are kept whole
    (let ((latex (funcall enhanced text)))
      (true (search "\\texttt{M-x org-\\codebreak{}export-\\codebreak{}latex-\\codebreak{}pdf}" latex))
      (true (search "\\texttt{C-x}" latex))
      (true (search "\\texttt{10.25}" latex))
      ;; the line after the break begins with an arrow, as a code block's lines do
      (true (search "\\DeclareRobustCommand{\\codebreak}{\\discretionary{}{\\breakmark}{}}" latex))
      (true (search "\\fvset{breaksymbolleft=\\breakmark}" latex))
      ;; the arrow is drawn, not a character: copying the text leaves it out
      (true (search "\\DeclareRobustCommand{\\breakmark}{\\hbox{\\setlength{\\unitlength}{0.07em}" latex))
      (false (search "hookrightarrow" latex))
      (true (search "\\setlength{\\emergencystretch}{3em}" latex)))
    ;; the export as Org makes it: as Org writes it
    (let ((latex (tuatara.defaults::org-latex-document text)))
      (true (search "\\texttt{M-x org-export-latex-pdf}" latex))
      (false (search "codebreak" latex))
      (false (search "emergencystretch" latex)))
    ;; a pattern breaks after | and @, its \ and ( kept with what follows
    (true (search "\\texttt{\\textbackslash{}d\\{4\\}|\\codebreak{}\\textbackslash{}bhello}"
                  (funcall enhanced (format nil "=\\d{4}|\\bhello=~%"))))
    ;; a repeat count is kept whole
    (true (search "\\texttt{\\textbackslash{}d\\{1,2\\}\\textbackslash{}/\\codebreak{}"
                  (funcall enhanced (format nil "=\\d{1,2}\\/\\d{4}=~%")))))
  ;; the longest part of a cell that cannot go on to the next line
  (is = 7 (tuatara.defaults::org-latex-longest-piece "=org-export-latex-pdf= C-x"))
  (is = 0 (tuatara.defaults::org-latex-longest-piece ""))
  ;; wrapping columns share in proportion to their text, each at least as wide as its longest part
  (is equal '(1/2 3/2) (tuatara.defaults::org-latex-column-shares '(1 3) '(1 1) 100))
  (is equal '(2/5 8/5) (tuatara.defaults::org-latex-column-shares '(5 95) '(10 10) 50))
  (is equal '(6/5 4/5) (tuatara.defaults::org-latex-column-shares '(1 1) '(60 40) 50))
  ;; a column of short values beside one of sentences is never narrower than its values
  (let* ((latex (tuatara.defaults::org-latex-document
                 (format nil "| Setting | Standard | What it does |~%|-~%~
                              | *lsp-languages* | '(rust go python typescriptreact) | The languages whose servers are used, every language with a server installed, or a list of those alone |~%~
                              | *lsp-idle-delay* | 0.5 | Seconds the cursor rests before the parameters of the call are shown in the echo row |~%")
                 :enhanced t))
         (share (let ((*read-eval* nil))
                  (read-from-string (cl-ppcre:scan-to-strings "(?<=hsize=)[0-9.]+(?=\\\\hsize}X >)" latex)))))
    (true (< 0.5 share))))

(define-test (latex-export table-columns-alike)
  ;; one table, the same columns wrapping and the same shares in the PDF and the Word document
  (with-test-directory (dir)
    (let* ((file (namestring (merge-pathnames "note.org" dir)))
           (note (lambda (key &optional drawer)
                   (format nil "~@[:EXPORT:~%~a~%:END:~%~]| Key | Command | Description |~%|-~%~
                                | ~a | switch-to-buffer | Switch to another buffer, chosen from the list of buffers open, the last used first in the list |~%~
                                | ~a | scratch-buffer | Open or return to the scratch buffer, where Lisp can be tried out and notes taken |~%"
                           drawer key key)))
           (latex (lambda (text) (tuatara.defaults::org-latex-document text :file file :enhanced t)))
           (columns (lambda (text)
                      (mapcar #'parse-integer
                              (cl-ppcre:all-matches-as-strings "(?<=<w:gridCol w:w=\")\\d+"
                                                               (docx-text (tuatara.defaults::org-docx-document text :file file)))))))
      ;; a narrow column with a space in its cells wraps in neither: as wide as its text, as without the space
      (dolist (key '("a b" "a-b"))
        (true (search "{@{}l l >{\\raggedright\\arraybackslash\\hsize=1.000\\hsize}X@{}}" (funcall latex (funcall note key)))))
      (is = (first (funcall columns (funcall note "a-b"))) (first (funcall columns (funcall note "a b"))))
      ;; widths: a length and two equal shares of what is left, in both
      (let ((given (funcall latex (funcall note "a b" "widths: 2cm * *"))))
        (true (search "p{2cm}" given))
        (is = 2 (count-matches ">{\\raggedright\\arraybackslash}X" given)))
      (destructuring-bind (given one other) (funcall columns (funcall note "a b" "widths: 2cm * *"))
        (is = 1134 given)
        (is = one other)))))

(define-test (latex-export table-lines)
  ;; lines: the lines printed, the same in the PDF and the Word document
  (with-test-directory (dir)
    (let* ((file (namestring (merge-pathnames "note.org" dir)))
           (note (lambda (drawer) (format nil "~@[:EXPORT:~%lines: ~a~%:END:~%~]| name | n |~%|-~%| x | 1 |~%| y | 2 |~%|-~%| z | 3 |~%" drawer)))
           (latex (lambda (drawer) (tuatara.defaults::org-latex-document (funcall note drawer) :file file :enhanced t)))
           (docx (lambda (drawer) (docx-text (tuatara.defaults::org-docx-document (funcall note drawer) :file file))))
           (table-rule (lambda (all) (count-matches "<w:tcBorders><w:bottom w:val=\"single\"" all))))
      ;; as standard: booktabs' rules above, under the header, at the table's own rule and under it
      (let ((standard (funcall latex nil)))
        (true (search "{@{}l r@{}}" standard))
        (is = 1 (count-matches "\\toprule" standard))
        (is = 2 (count-matches "\\midrule" standard))
        (is = 1 (count-matches "\\bottomrule" standard)))
      (let ((standard (funcall docx nil)))
        (true (search "<w:top w:val=\"single\" w:sz=\"12\"" standard))
        (true (search "<w:left w:val=\"nil\"/>" standard))
        (is = 4 (funcall table-rule standard)))
      ;; none: not a line
      (let ((none (funcall latex "none")))
        (false (cl-ppcre:scan "\\\\(?:top|mid|bottom)rule|\\\\hline" none))
        (true (search "\\endhead" none)))
      (let ((none (funcall docx "none")))
        (false (search "w:val=\"single\"" none)))
      ;; top and bottom only
      (let ((ends (funcall latex "top bottom")))
        (is = 1 (count-matches "\\toprule" ends))
        (is = 0 (count-matches "\\midrule" ends))
        (is = 1 (count-matches "\\bottomrule" ends)))
      (is = 0 (funcall table-rule (funcall docx "top bottom")))
      ;; under the header only: the table's own rule not printed
      (let ((header (funcall latex "header")))
        (is = 1 (count-matches "\\midrule" header))
        (false (search "\\toprule" header)))
      (is = 2 (funcall table-rule (funcall docx "header")))
      ;; lines down the table: rules that meet them, a line between the columns
      (let ((all (funcall latex "all")))
        (true (search "{|l | r|}" all))
        (false (search "\\toprule" all))
        ;; above, under the header, between each row and the next, under it
        (is = 5 (count-matches "\\hline" all)))
      (let ((all (funcall docx "all")))
        (true (search "<w:insideV w:val=\"single\"" all))
        (true (search "<w:insideH w:val=\"single\"" all))
        (true (search "<w:right w:val=\"single\"" all)))
      (true (search "{|l r|}" (funcall latex "box header")))
      ;; a word it does not take, said
      (true (find-if (lambda (n) (search "lines: dotted is none of top" n))
                     (nth-value 4 (tuatara.defaults::org-latex-document (funcall note "top dotted") :file file :enhanced t))))))
  ;; the tips: what works in both exports, what only in the PDF
  (let ((f (run-keys "M-x d e s c r i b e - e x p o r t - s e t t i n g s RET" :width 160 :height 60)))
    (declare (ignore f))
    (let ((help (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Help*"))))
      (true (search "lines: box header" help))
      (true (search "A block (code, example, quote)" help))
      (true (search "M-x org-insert-export-drawer" help))
      (false (search "\\[org-insert-export-drawer]" help))
      (true (search "In the PDF and the Word document alike" help))
      (true (search "  \\\\ at the end of a line" help))
      (true (search "  \\newline" help))
      ;; every line within 72 columns
      (is equal '() (remove-if (lambda (line) (<= (length line) 72)) (uiop:split-string help :separator '(#\Newline)))))))

(define-test (latex-export more-export-settings)
  ;; a new page, space around, the space between lines, a paragraph's alignment, a picture's position,
  ;; a heading out of the contents: the same in the PDF and the Word document
  (with-test-directory (dir)
    (alexandria:write-byte-vector-into-file (coerce (png-octets 2 2) '(vector (unsigned-byte 8))) (merge-pathnames "pic.png" dir))
    (let* ((file (namestring (merge-pathnames "note.org" dir)))
           (note (format nil "#+options: toc:t~%* Shown~%Text.~%* Hidden~%:EXPORT:~%contents: no~%new page: before~%:END:~%Intro.~%~%~
                              :EXPORT:~%align: right~%spacing: double~%space before: 1cm~%:END:~%Right words.~%~%~
                              :EXPORT:~%new page: after~%space after: 12pt~%:END:~%| a |~%~%~
                              :EXPORT:~%position: left~%:END:~%[[file:pic.png]]~%~%~
                              :EXPORT:~%align: justify~%spacing: 1.15~%:END:~%Even words.~%"))
           (latex (tuatara.defaults::org-latex-document note :file file :enhanced t))
           (docx (docx-text (tuatara.defaults::org-docx-document note :file file))))
      ;; the PDF
      (true (cl-ppcre:scan "(?s)\\\\clearpage\\s*(?:%[^\\n]*\\n)*\\{\\\\expandafter\\\\let\\\\expandafter\\\\addcontentsline\\\\csname @gobblethree\\\\endcsname\\s*\\\\section\\{Hidden\\}" latex))
      (true (search "{\\raggedleft" latex))
      (true (search "\\linespread{1.618}\\selectfont" latex))
      (true (search "\\par\\vspace{1cm}" latex))
      (true (search "\\par\\vspace{12pt}" latex))
      (true (cl-ppcre:scan "(?s)\\\\end\\{longtable\\}.*?\\\\clearpage" latex))
      (true (search "\\begin{flushleft}" latex))
      (true (search "\\linespread{1.064}\\selectfont" latex))
      ;; the Word document
      (is = 2 (count-matches "<w:br w:type=\"page\"/>" docx))
      (true (search "<w:spacing w:line=\"480\" w:lineRule=\"auto\"/><w:jc w:val=\"right\"/>" docx))
      (true (search "<w:spacing w:line=\"276\" w:lineRule=\"auto\"/><w:jc w:val=\"both\"/>" docx))
      (true (search "w:line=\"567\" w:lineRule=\"exact\"" docx))
      (true (search "w:line=\"240\" w:lineRule=\"exact\"" docx))
      (true (cl-ppcre:scan "<w:jc w:val=\"left\"/></w:pPr><w:r><w:drawing>" docx))
      ;; out of the contents, still a numbered heading
      (true (search ">1  Shown<" (cl-ppcre:regex-replace-all "</w:t></w:r><w:r><w:t[^>]*>" docx "")))
      (false (search "PAGEREF" (subseq docx (or (search "Hidden" docx) 0) (+ 400 (or (search "Hidden" docx) 0)))))
      (is = 1 (count-matches "PAGEREF" docx))))
  ;; a length that is none goes nowhere into the LaTeX, and is said
  (multiple-value-bind (latex a b c notes)
      (tuatara.defaults::org-latex-document (format nil ":EXPORT:~%space before: 0pt}\\input{x}\\vspace{0pt~%:END:~%Text.~%") :enhanced t)
    (declare (ignore a b c))
    (false (search "\\input{x}" latex))
    (true (find-if (lambda (n) (search "is no length" n)) notes))))

(define-test (latex-export pages-sideways)
  ;; page: landscape on a heading (it and all under it) and on elements side by side (one run of pages)
  (let ((note (format nil "* One~%Upright.~%* Wide~%:EXPORT:~%page: landscape~%:END:~%Sideways.~%* Two~%Upright before.~%~%:EXPORT:~%page: landscape~%:END:~%| a |~%~%:EXPORT:~%page: landscape~%:END:~%| b |~%~%After.~%")))
    (let ((latex (tuatara.defaults::org-latex-document note :enhanced t)))
      ;; the paper turned for them and back after, headers and footers with it
      (true (search "\\newenvironment{tuataralandscape}" latex))
      (is = 2 (count-matches "\\begin{tuataralandscape}" latex))
      (true (< (search "\\begin{tuataralandscape}" latex) (search "\\section{Wide}" latex) (search "\\end{tuataralandscape}" latex)))
      ;; the two tables share their run
      (true (cl-ppcre:scan "(?s)\\\\begin\\{tuataralandscape\\}\\s*\\\\begin\\{longtable\\}.*?a.*?\\\\end\\{longtable\\}\\s*\\\\begin\\{longtable\\}.*?b.*?\\\\end\\{longtable\\}\\s*\\\\end\\{tuataralandscape\\}" latex)))
    (with-test-directory (dir)
      (let* ((all (docx-text (tuatara.defaults::org-docx-document note :file (namestring (merge-pathnames "n.org" dir)))))
             (sections (let ((o '()))
                         (cl-ppcre:do-register-groups (type size) ("<w:sectPr>.*?(?:<w:type w:val=\"(\\w+)\"/>)?<w:pgSz([^>]*)/>" all)
                           (push (list (or type "nextPage") (if (search "landscape" size) :sideways :upright)) o))
                         (nreverse o))))
        ;; each turn on a page of its own; the section after pages turned sideways too; the last upright, the text after
        (is equal '(("continuous" :upright) ("nextPage" :sideways) ("nextPage" :upright) ("nextPage" :sideways) ("nextPage" :upright)) sections
            "~s" sections)
        ;; tables on sideways pages as wide as those pages let them be
        (true (search "<w:pgSz w:w=\"16838\" w:h=\"11906\" w:orient=\"landscape\"/>" all))))
    ;; Org's own export, which reads no :EXPORT: drawers, turns nothing
    (false (search "tuataralandscape}" (tuatara.defaults::org-latex-document note))))
  ;; within a heading turned sideways, a table turned too stays sideways: turned once, in both
  (let ((note (format nil "* Wide~%:EXPORT:~%page: landscape~%:END:~%Text.~%~%:EXPORT:~%page: landscape~%:END:~%| a |~%")))
    (let ((latex (tuatara.defaults::org-latex-document note :enhanced t)))
      (is = 1 (count-matches "\\begin{tuataralandscape}" latex))
      (is = 1 (count-matches "\\end{tuataralandscape}" latex)))
    (let ((all (docx-text (tuatara.defaults::org-docx-document note))))
      (is = 1 (count-matches "w:orient=\"landscape\"" all)))))

(define-test (latex-export redactions-checked-at-once)
  ;; hundreds of hidden names in a long note: looked through at once, each place found once
  (let* ((names (loop for i below 400 collect (format nil "Person~d Surname~d" i i)))
         (text (with-output-to-string (s)
                 (dolist (name names) (format s "Met {{{redact(~a)}}} today.~%" name))
                 (loop repeat 2000 do (format s "Plain words of a long note with nothing hidden in them at all.~%"))
                 (format s "Later Person7 Surname7 wrote, and person12 again.~%")))
         (start (get-internal-real-time))
         (findings (tuatara.defaults::redaction-findings text :org)))
    (is equal '("Person7 Surname7" "person12") (mapcar #'second findings))
    (true (< (- (get-internal-real-time) start) (* 2 internal-time-units-per-second))
          "~,2f s" (/ (- (get-internal-real-time) start) internal-time-units-per-second))))

(define-test (latex-export docx-hidden-heading-in-contents)
  ;; a heading hiding a name: in the contents as [redacted], never its markup nor the name
  (let ((all (docx-text (tuatara.defaults::org-docx-document
                         (format nil "#+title: T~%#+options: toc:t~%* Interview {{{redact(John)}}}~%Text.~%")))))
    (false (search "@@" all))
    (false (search "John" all))
    (true (search "Interview [redacted]" all) "~a" (subseq all 0 (min 3000 (length all))))))

(define-test (latex-export export-file-name-elsewhere)
  ;; #+EXPORT_FILE_NAME keeps the export's own ending, and writing over a file outside the note's
  ;; folder is asked first: no leaves it as it was, yes writes it
  (is string= "/tmp/x.tex" (tuatara.defaults::org-export-file-name (format nil "#+export_file_name: /tmp/x.conf~%") "/tmp/n.org"))
  (with-test-directory (dir)
    (let ((other (merge-pathnames "other/report.docx" dir))
          (file (merge-pathnames "notes/note.org" dir)))
      (ensure-directories-exist other)
      (ensure-directories-exist file)
      (alexandria:write-string-into-file "the real report" other)
      (alexandria:write-string-into-file (format nil "#+export_file_name: ../other/report~%Text.~%") file)
      (let ((f (export-session file (list (export-command "org-export-docx") "n o RET") :width (width-for dir))))
        (true (tuatara.frontend.fake:fake-frame-showing f "writes over "))
        (is string= "the real report" (alexandria:read-file-into-string other)))
      (export-session file (list (export-command "org-export-docx") "y e s RET"))
      (false (string= "the real report" (alexandria:read-file-into-string other :external-format :latin-1))))
    ;; a new file outside the folder is asked about too; inside a folder of the note's, not
    (let ((file (merge-pathnames "notes/new.org" dir)) (made (merge-pathnames "elsewhere/new.docx" dir)))
      (ensure-directories-exist made)
      (alexandria:write-string-into-file (format nil "#+export_file_name: ../elsewhere/new~%Text.~%") file)
      (export-session file (list (export-command "org-export-docx") "n o RET"))
      (false (probe-file made)))
    (let ((file (merge-pathnames "notes/kept.org" dir)) (made (merge-pathnames "notes/exports/kept.docx" dir)))
      (ensure-directories-exist made)
      (alexandria:write-string-into-file (format nil "#+export_file_name: exports/kept~%Text.~%") file)
      (export-session file (list (export-command "org-export-docx")))
      (true (probe-file made)))
    ;; a subtree exported alone, its headline's EXPORT_FILE_NAME outside: asked too
    (let ((file (merge-pathnames "notes/part.org" dir)) (made (merge-pathnames "elsewhere/part.tex" dir)))
      (alexandria:write-string-into-file
       (format nil "#+title: Part~%* Findings~%:PROPERTIES:~%:EXPORT_FILE_NAME: ../elsewhere/part~%:END:~%Text.~%") file)
      (let ((f (export-session file (list (concatenate 'string "j C-u " (export-command "org-export-latex-enhanced")) "n o RET")
                               :width (width-for dir))))
        (true (tuatara.frontend.fake:fake-frame-showing f "writes "))
        (false (probe-file made))))
    ;; in a folder of the note's that is one of your own (templates, logos, trusted): asked
    (let* ((file (merge-pathnames "notes/mine.org" dir)) (made (merge-pathnames "notes/setup/mine.docx" dir))
           (tuatara-user:*export-trusted-folders* (list (namestring (merge-pathnames "notes/setup/" dir)))))
      (ensure-directories-exist made)
      (alexandria:write-string-into-file (format nil "#+export_file_name: setup/mine~%Text.~%") file)
      (let ((f (export-session file (list (export-command "org-export-docx") "n o RET") :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "or in one of your own; go on?"))
        (false (probe-file made))))))

(define-test (latex-export tables-in-columns)
  ;; a table in columns is kept whole (longtable cannot be in columns): a float of its own, as wide as its column
  (let ((latex (tuatara.defaults::org-latex-document
                (format nil "* Two~%:EXPORT:~%columns: 2~%:END:~%#+caption: Counts~%| a | b |~%|---+---|~%| 1 | 2 |~%~%* After~%| c |~%")
                :enhanced t)))
    (true (cl-ppcre:scan "(?s)\\\\begin\\{multicols\\}.*\\\\begin\\{table\\}\\[H\\].*\\\\caption\\{Counts\\}.*\\\\begin\\{tabular" latex) "~a" latex)
    (false (cl-ppcre:scan "(?s)\\\\begin\\{multicols\\}.*\\\\begin\\{(?:long|xl)tab.*\\\\end\\{multicols\\}" latex))
    ;; outside the columns, as before
    (true (cl-ppcre:scan "(?s)\\\\end\\{multicols\\}.*\\\\begin\\{(?:longtable|xltabular)\\}" latex)))
  (true (tuatara.defaults::latex-template-body-in-columns-p "\\begin{multicols}{2}{{{body}}}\\end{multicols}"))
  (false (tuatara.defaults::latex-template-body-in-columns-p "\\begin{multicols}{2}x\\end{multicols}{{{body}}}")))

(define-test (latex-export names-with-brackets)
  ;; a note named with [ ] * ?, as files from elsewhere are: its PDF and Word document made all the same
  (is string= "survey [1]" (tuatara.io:native-file-stem "/x/survey [1].org"))
  (is string= "org" (tuatara.io:native-file-type "/x/survey [1].org"))
  (false (tuatara.io:native-file-type "/x/.bashrc"))
  (with-test-directory (dir)
    (let ((file (merge-pathnames "notes/" dir)))
      (ensure-directories-exist file)
      (let ((note (concatenate 'string (namestring file) "survey [1].org")))
        (with-open-file (o (uiop:parse-native-namestring note) :direction :output) (format o "#+title: T~%Text.~%"))
        (export-session note (list (export-command "org-export-docx")))
        (true (tuatara.io:path-stamp (concatenate 'string (namestring file) "survey [1].docx")))
        (when (tuatara.defaults::latex-engines-found)
          (export-session note (list (export-command "org-export-latex-pdf")))
          (true (tuatara.io:path-stamp (concatenate 'string (namestring file) "survey [1].pdf"))))))))

(define-test (latex-export lengths-read-alike)
  ;; one reading of a setting's length for both exports: a share with a point, a number alone in points
  (is string= "0.333\\linewidth" (tuatara.defaults::org-latex-length "33.3%"))
  (is string= "8pt" (tuatara.defaults::org-latex-length "8"))
  (is string= "2.5cm" (tuatara.defaults::org-latex-length "2.5cm"))
  (is string= "0.5\\textwidth" (tuatara.defaults::org-latex-length "0.5\\textwidth"))
  (is equal '(333/10 :share) (multiple-value-list (tuatara.defaults::setting-length "33.3%")))
  (is equal '(8 :pt) (multiple-value-list (tuatara.defaults::setting-length "8"))))

(define-test (latex-export number-columns-read-alike)
  ;; a column the editor sets to the right is set to the right in the export too: hex, NaN and inf are numbers
  (let ((table (format nil "| Name | Mask | Value |~%| a | 0xff | inf |~%| b | 0x1F | NaN |~%")))
    (is equal '(:left :right :right)
        (tuatara.defaults::org-table-alignments '(("Name" "Mask" "Value") ("a" "0xff" "inf") ("b" "0x1F" "NaN")) 3))
    (true (search "\\begin{tabular}{lrr}" (tuatara.defaults::org-latex-document table)))))

(define-test (latex-export code-indentation-as-tangled)
  ;; a tab reaches the next eighth column, so a line indented by a tab and one by eight spaces share it all
  (let ((text (format nil "#+begin_src sh~%~cfoo~%        bar~%#+end_src~%" #\Tab)))
    (true (search (format nil "\\begin{verbatim}~%foo~%bar~%\\end{verbatim}") (tuatara.defaults::org-latex-document text)))
    (is string= (format nil "foo~%bar~%")
        (tuatara.defaults::org-block-code (tuatara.defaults::org-src-block-at (tuatara.defaults::org-parse text) 16)))))

(define-test (latex-export template-places-read-alike)
  ;; both halves read a place the same way: :text given, a picture's size the Word half's alone
  (flet ((field (name way) (format nil "~a/~(~a~)" name way)))
    (is string= "A logo/path B reference/text C reference/raw"
        (tuatara.defaults::fill-latex-template "A {{{logo:path:height=1.8cm}}} B {{{reference:text}}} C {{{reference:raw}}}" #'field)))
  (is equal '(:name "logo" :condition nil :way :path :date nil :size (:height "1.8cm") :end 28)
      (tuatara.defaults::template-place "{{{logo:path:height=1.8cm}}} rest"))
  (is eq :text (nth-value 1 (tuatara.defaults::docx-place "{{{reference:text}}}")))
  (is equal '("copy_to" "logo" "reference")
      (tuatara.defaults::template-places "{{{logo:path:width=3cm}}} {{{#copy_to}}}x{{{/copy_to}}} {{{reference:text}}}")))

(define-test (latex-export docx-built-in-as-a-template)
  ;; Tuatara's own look is a template filled as one of yours is: what the note gives, and nothing for what it does not
  (let ((with (docx-text (tuatara.defaults::org-docx-document
                          (format nil "#+title: T~%#+subtitle: S~%#+author: A~%#+date: 2026-10-07~%* H~%Text.~%"))))
        (without (docx-text (tuatara.defaults::org-docx-document (format nil "#+options: toc:nil author:nil~%Text.~%")))))
    (true (search "<w:pStyle w:val=\"Title\"/>" with))
    (true (search ">S</w:t>" with))
    (true (search "<w:pStyle w:val=\"Date\"/>" with))
    (true (search "<w:pStyle w:val=\"TOCHeading\"/>" with))
    ;; no title, author or contents asked: no paragraph of theirs, no place left in the text
    (false (search "<w:pStyle w:val=\"Title\"/>" without))
    (false (search "<w:pStyle w:val=\"Author\"/>" without))
    (false (search "<w:pStyle w:val=\"TOCHeading\"/>" without))
    (false (search "{{{" without))
    ;; its parts' relations written whole, no stray characters between them
    (false (search "~" without)))
  ;; a keyword's value is words alone: A. is no list's bullet, * no heading
  (let ((text (docx-text (tuatara.defaults::org-docx-document (format nil "#+title: * Star~%#+author: A. Writer~%Text.~%")))))
    (true (search ">A. Writer<" text))
    (true (search ">* Star<" text)))
  (let ((latex (tuatara.defaults::org-latex-document (format nil "#+title: * Star~%#+author: A. Writer~%Text.~%") :enhanced t)))
    (true (search "A. Writer" latex))))

(define-test (latex-export files-from-the-note-folder)
  ;; setup files and pictures come from the note's folder and those in it, or from
  ;; *export-trusted-folders*; one elsewhere, a link leading out too, is left out and said
  (with-test-directory (dir)
    (let ((note-folder (merge-pathnames "survey/" dir))
          (outside (merge-pathnames "private/" dir)))
      (ensure-directories-exist (merge-pathnames "sub/" note-folder))
      (ensure-directories-exist outside)
      (dolist (folder (list (merge-pathnames "sub/" note-folder) outside))
        (alexandria:write-string-into-file (format nil "#+latex_header: \\usepackage{~amark}~%" (if (eq folder outside) "outside" "inside"))
                                           (merge-pathnames "setup.org" folder))
        (alexandria:write-byte-vector-into-file (coerce (png-octets 2 2) '(vector (unsigned-byte 8))) (merge-pathnames "pic.png" folder)))
      (sb-posix:symlink (namestring (merge-pathnames "pic.png" outside)) (namestring (merge-pathnames "link.png" note-folder)))
      (sb-posix:symlink (namestring outside) (namestring (merge-pathnames "linked" note-folder)))
      ;; a .. or a folder that is a link, leading out, is not in the note's folder (clock tables and archives ask so too)
      (let ((case (namestring note-folder)))
        (true (tuatara.defaults::org-path-beside-p (concatenate 'string case "sub/setup.org") case))
        (true (tuatara.defaults::org-path-beside-p (concatenate 'string case "sub/not-there.org") case))
        (false (tuatara.defaults::org-path-beside-p (concatenate 'string case "../private/setup.org") case))
        (false (tuatara.defaults::org-path-beside-p (concatenate 'string case "sub/../../private/setup.org") case))
        (false (tuatara.defaults::org-path-beside-p (concatenate 'string case "linked/setup.org") case))
        (false (tuatara.defaults::org-path-beside-p (concatenate 'string case "linked/not-there.org") case)))
      (let ((file (namestring (merge-pathnames "note.org" note-folder)))
            (text (format nil "#+setupfile: sub/setup.org~%#+setupfile: ../private/setup.org~%~%[[file:sub/pic.png]]~%~%[[file:../private/pic.png]]~%~%[[file:link.png]]~%")))
        (let ((tuatara-user:*export-trusted-folders* '()))
          (multiple-value-bind (latex named lines missing notes) (tuatara.defaults::org-latex-document text :file file)
            (declare (ignore named lines missing))
            (true (search "insidemark" latex))
            (false (search "outsidemark" latex))
            (true (search "sub/pic.png}" latex))
            (false (search "private/pic.png}" latex))
            (false (cl-ppcre:scan "includegraphics\\[[^]]*\\]\\{[^}]*link\\.png\\}" latex))
            (true (search "\\fbox{\\texttt{link.png}}" latex))
            (is = 1 (count-matches "\\includegraphics" latex))
            (is = 3 (count-if (lambda (n) (search "outside the note's folder" n)) notes) "~s" notes))
          (multiple-value-bind (octets named notes) (tuatara.defaults::org-docx-document text :file file)
            (declare (ignore named))
            (is = 1 (count-if (lambda (e) (search "media/" (getf e :name)))
                              (tuatara.defaults::zip-entries (coerce octets '(simple-array (unsigned-byte 8) (*))))))
            (is = 3 (count-if (lambda (n) (search "outside the note's folder" n)) notes) "~s" notes)))
        ;; a folder trusted: taken in from there too
        (let ((tuatara-user:*export-trusted-folders* (list (namestring outside))))
          (multiple-value-bind (latex named lines missing notes) (tuatara.defaults::org-latex-document text :file file)
            (declare (ignore named lines missing))
            (true (search "outsidemark" latex))
            (true (search "private/pic.png}" latex))
            (false (find-if (lambda (n) (search "outside the note's folder" n)) notes))))))))

(define-test (latex-export templates-from-the-note-folder)
  ;; a template named by its path is taken from the note's folder, not from elsewhere
  (with-test-directory (dir)
    (let ((notes (namestring (merge-pathnames "notes/" dir)))
          (inside (merge-pathnames "notes/mine.tex" dir))
          (outside (merge-pathnames "elsewhere/theirs.tex" dir)))
      (ensure-directories-exist inside)
      (ensure-directories-exist outside)
      (alexandria:write-string-into-file "inside template" inside)
      (alexandria:write-string-into-file "outside template" outside)
      (is string= "inside template" (tuatara.defaults::latex-template-for "./mine" notes))
      (fail (tuatara.defaults::latex-template-for "../elsewhere/theirs" notes))
      (let ((tuatara-user:*export-trusted-folders* (list (namestring (merge-pathnames "elsewhere/" dir)))))
        (is string= "outside template" (tuatara.defaults::latex-template-for "../elsewhere/theirs" notes))))))

(define-test (latex-export unconfined-asked-first)
  ;; with no sandbox, the export asks before LaTeX runs with your rights: n runs nothing, y runs it and says so
  (with-test-directory (dir)
    (let* ((file (merge-pathnames "notes.org" dir))
           (tuatara-user:*latex-engines* (list (stand-in-latex dir "xelatex")))
           (tuatara-user:*sandbox-program* nil))
      (alexandria:write-string-into-file (format nil "#+title: Notes~%* One~%Text~%") file)
      (pdf-forget)
      (let ((f (export-session file (list (export-command "org-export-latex-pdf") "n RET") :width 200)))
        (true (tuatara.frontend.fake:fake-frame-showing f "LaTeX would run without the sandbox"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Not exported: LaTeX runs only shut in the sandbox unless you say yes")))
      (false (probe-file (merge-pathnames "notes.pdf" dir)))
      (false (probe-file (merge-pathnames "notes.tex" dir)))
      (let ((f (export-session file (list (export-command "org-export-latex-pdf") "y RET") :width 200)))
        (true (tuatara.frontend.fake:fake-frame-showing f "LaTeX ran without the sandbox")))
      (true (probe-file (merge-pathnames "notes.pdf" dir)))
      ;; writing the LaTeX alone runs no LaTeX program, so nothing is asked
      (let ((f (export-session file (export-command "org-export-latex") :width 200)))
        (false (tuatara.frontend.fake:fake-frame-showing f "would run without the sandbox"))))))

(define-test (latex-export engine-shut-in)
  ;; LaTeX's own Lua, run by lualatex for a document, reads only the document's folder and TeX's own,
  ;; writes nowhere in it, and the PDF is made all the same; without a sandbox it runs as before, and that is said
  (let ((tuatara-user:*latex-engines* '("lualatex")))
    (when (and (tuatara.defaults::latex-engines-found) (eq :ready (tuatara.io:sandbox-status)))
      (with-test-directory (dir)
        (let* ((folder (merge-pathnames "survey/" dir))
               (home (merge-pathnames "home/" dir))
               (private (namestring (merge-pathnames ".bashrc" home)))
               (tex (namestring (merge-pathnames "probe.tex" folder))))
          (ensure-directories-exist folder)
          (ensure-directories-exist home)
          (alexandria:write-string-into-file "private words" private)
          (alexandria:write-string-into-file
           (format nil "\\documentclass{article}
\\begin{document}
\\directlua{
local f = io.open('~awritten.txt', 'w')
texio.write_nl('PROBE-WRITE:' .. tostring(not (f == nil)))
if f then f:write('x') f:close() end
local g = io.open('~a', 'r')
texio.write_nl('PROBE-READ:' .. tostring(not (g == nil)))
if g then texio.write_nl('PROBE-TEXT:' .. g:read('*l')) g:close() end
}
\\IfFileExists{~a}{\\typeout{PROBE-TEX-READ:true}}{\\typeout{PROBE-TEX-READ:false}}
Shut in.
\\end{document}
" (namestring folder) private private)
           tex)
          (flet ((said ()
                   (alexandria:read-file-into-string
                    (concatenate 'string (tuatara.defaults::latex-build-directory tex "lualatex") "probe.log"))))
            (let ((f (export-session tex (export-command "latex-compile-pdf") :width 200)))
              (true (tuatara.frontend.fake:fake-frame-showing f "probe.pdf (lualatex)"))
              (false (tuatara.frontend.fake:fake-frame-showing f "without the sandbox")))
            (true (probe-file (merge-pathnames "probe.pdf" folder)))
            (false (probe-file (merge-pathnames "written.txt" folder)))
            (let ((log (said)))
              (true (search "PROBE-WRITE:false" log) "~a" log)
              (true (search "PROBE-READ:false" log) "~a" log)
              (true (search "PROBE-TEX-READ:false" log) "~a" log)
              (false (search "private words" log)))
            ;; no sandbox here: asked first; refused, nothing is run; said yes to, made, said, and the
            ;; file outside read, as LaTeX alone allows
            (delete-file (merge-pathnames "probe.pdf" folder))
            (let ((tuatara-user:*sandbox-program* nil))
              (let ((f (export-session tex (list (export-command "latex-compile-pdf") "n RET") :width 200)))
                (true (tuatara.frontend.fake:fake-frame-showing f "LaTeX would run without the sandbox (turned off"))
                (true (tuatara.frontend.fake:fake-frame-showing f "Not exported: LaTeX runs only shut in the sandbox unless you say yes")))
              (false (probe-file (merge-pathnames "probe.pdf" folder)))
              (let ((f (export-session tex (list (export-command "latex-compile-pdf") "y RET") :width 200)))
                (true (tuatara.frontend.fake:fake-frame-showing f "LaTeX ran without the sandbox (turned off"))))
            (true (probe-file (merge-pathnames "probe.pdf" folder)))
            (false (probe-file (merge-pathnames "written.txt" folder)))
            (true (search "PROBE-TEXT:private words" (said)))
            (is eq :ready (tuatara.io:sandbox-status))))))))

(defparameter *one-pixel-png*
  (coerce (loop with hex = "89504e470d0a1a0a0000000d4948445200000001000000010802000000907753de0000000c49444154789c63f8cfc0000003010100c9fe92ef0000000049454e44ae426082"
                for i from 0 below (length hex) by 2 collect (parse-integer hex :start i :end (+ i 2) :radix 16))
          '(vector (unsigned-byte 8)))
  "A PNG picture of one pixel, for the tests.")

(define-test (latex-export pictures-from-elsewhere)
  ;; a picture the document includes from a folder it does not see: shown to LaTeX, that file alone, read
  ;; only, once its contents say it is a picture; a file named as one that is not, never
  (with-test-directory (dir)
    (let* ((elsewhere (ensure-directories-exist (merge-pathnames "Pictures/Misc/" dir)))
           (logo (namestring (merge-pathnames "logo.png" elsewhere)))
           (fake (namestring (merge-pathnames "secret.png" elsewhere)))
           (folder (ensure-directories-exist (merge-pathnames "notes/" dir)))
           (tex (namestring (merge-pathnames "report.tex" folder))))
      (alexandria:write-byte-vector-into-file *one-pixel-png* logo)
      (alexandria:write-string-into-file "private words" fake)
      (alexandria:write-string-into-file
       (format nil "\\documentclass{article}~%\\usepackage{graphicx}~%\\begin{document}~%\\includegraphics[width=1cm]{~a}~%\\includegraphics{~a}~%\\includegraphics{local.png}~%\\end{document}~%" logo fake)
       tex)
      ;; only when one's own template or setup file names it: never from the note's own text
      (is equal (list logo) (tuatara.defaults::latex-picture-files tex (list (format nil "\\includegraphics{~a}" logo) fake)))
      (false (tuatara.defaults::latex-picture-files tex '()))
      (false (tuatara.defaults::latex-picture-files tex (list "an unrelated template")))
      (let ((job (tuatara.defaults::make-pdf-export nil tex :org :file nil nil nil)))
        (setf (tuatara.defaults::pdf-export-trusted job) (list logo fake))
        (destructuring-bind (&key directories &allow-other-keys)
            (tuatara.defaults::latex-sandbox-places job tex (stand-in-latex dir "xelatex") "/build/")
          (true (member logo directories :test #'string=))
          (false (member fake directories :test #'string=))
          (false (member (string-right-trim "/" (namestring elsewhere)) directories :test #'string=))))
      ;; a LaTeX file exported as it is: its pictures from elsewhere only when it stands in one's own folders
      (let ((job (tuatara.defaults::make-pdf-export nil tex :latex :file nil nil nil)))
        (destructuring-bind (&key directories &allow-other-keys)
            (tuatara.defaults::latex-sandbox-places job tex (stand-in-latex dir "xelatex") "/build/")
          (false (member logo directories :test #'string=)))
        (let ((tuatara-user:*export-trusted-folders* (list (namestring folder))))
          (destructuring-bind (&key directories &allow-other-keys)
              (tuatara.defaults::latex-sandbox-places job tex (stand-in-latex dir "xelatex") "/build/")
            (true (member logo directories :test #'string=)))))
      ;; made into a PDF by a LaTeX program shut in, where there is one
      (let ((tuatara-user:*latex-engines* '("lualatex"))
            (tuatara-user:*export-trusted-folders* (list (namestring folder))))
        (when (and (tuatara.defaults::latex-engines-found) (eq :ready (tuatara.io:sandbox-status)))
          (alexandria:write-string-into-file
           (format nil "\\documentclass{article}~%\\usepackage{graphicx}~%\\begin{document}~%\\includegraphics[width=1cm]{~a}~%\\end{document}~%" logo)
           tex :if-exists :supersede)
          (let ((f (export-session tex (export-command "latex-compile-pdf") :width 200)))
            (true (tuatara.frontend.fake:fake-frame-showing f "report.pdf (lualatex)")))
          (true (probe-file (merge-pathnames "report.pdf" folder))))))))

(define-test (latex-export engine-shown-its-folders)
  ;; what a LaTeX program shut in is shown: the document's folder, the trusted ones, TeX's own and
  ;; your fonts, read only; its working directory to write in; TeX's own cache written apart
  (with-test-directory (dir)
    (let* ((folder (namestring (ensure-directories-exist (merge-pathnames "survey/" dir))))
           (engine (stand-in-latex dir "xelatex"))
           (tex (concatenate 'string folder "notes.tex"))
           (job (tuatara.defaults::make-pdf-export nil tex :latex :file nil nil nil))
           (tuatara-user:*export-trusted-folders* (list (namestring dir))))
      (destructuring-bind (&key directories writable overlays)
          (tuatara.defaults::latex-sandbox-places job tex engine "/build/")
        (true (member folder directories :test #'string=))
        (true (some (lambda (d) (string= (string-right-trim "/" d) (string-right-trim "/" (namestring dir)))) directories))
        (is equal '("/build/") writable)
        (is equal '() overlays)
        ;; the pictures an export draws are never shown as a folder
        (false (find-if (lambda (d) (search "latex/pictures" d)) directories)))
      ;; never the root, the home folder nor a folder holding it, whole: a note there shows only its LaTeX
      ;; and the files its pictures are drawn from (nothing is written: the paths are only worked out)
      (let* ((home (tuatara.defaults::latex-home)))
        (false (tuatara.defaults::shown-whole-p "/"))
        (false (tuatara.defaults::shown-whole-p home))
        (false (tuatara.defaults::shown-whole-p (tuatara.io:parent-path home)))
        (true (tuatara.defaults::shown-whole-p folder))
        (let* ((given (concatenate 'string home "/given.tex"))
               (job (tuatara.defaults::make-pdf-export nil (concatenate 'string home "/given.org") :org :file nil nil nil))
               (picture (concatenate 'string folder "drawn.png")))
          (setf (tuatara.defaults::pdf-export-pictures job) (list picture))
          (let ((directories (getf (tuatara.defaults::latex-sandbox-places job given engine "/build/") :directories)))
            (true (member given directories :test #'string=))
            (true (member picture directories :test #'string=))
            (false (find-if (lambda (d) (member (string-right-trim "/" d) (list home (string-right-trim "/" (tuatara.io:parent-path home))) :test #'string=))
                            directories)
                   "~s" directories))))
      ;; the folders texmf.cnf is looked for in (TEXMFCNF) shown too: a TeX Live in a hidden folder of
      ;; home, its settings found there, and the TEXMFCNF your shell may set
      (let* ((bin (namestring (ensure-directories-exist (merge-pathnames "texlive/bin/" dir))))
             (tex-engine (stand-in-latex bin "xelatex"))
             (kpsewhich (concatenate 'string bin "kpsewhich")))
        (alexandria:write-string-into-file
         (format nil "#!/bin/sh~%printf '%s\\n' '{~a.texlive/web2c,/usr/share/texmf/web2c,~a,/}' '~atexlive' '~atexlive/texmf-dist' '' '' '' '' '' '{}' '~atexlive/texmf-var'~%"
                 (namestring dir) (tuatara.defaults::latex-home) (namestring dir) (namestring dir) (namestring dir))
         kpsewhich)
        (sb-posix:chmod kpsewhich #o755)
        (multiple-value-bind (trees var) (tuatara.defaults::tex-trees tex-engine)
          (true (member (format nil "~a.texlive/web2c" (namestring dir)) trees :test #'string=) "~s" trees)
          (false (member "/usr/share/texmf/web2c" trees :test #'string=))
          ;; never the whole home or the root, whatever a setting says
          (false (member (tuatara.defaults::latex-home) trees :test #'string=))
          (false (member "" trees :test #'string=))
          (true (member (format nil "~atexlive/texmf-dist" (namestring dir)) trees :test #'string=))
          (is string= (format nil "~atexlive/texmf-var" (namestring dir)) var)))
      ;; TeX Live's folders as kpsewhich beside it tells them; its cache seen, written apart
      (let ((real (cdr (first (let ((tuatara-user:*latex-engines* '("lualatex"))) (tuatara.defaults::latex-engines-found))))))
        (when (and real (tuatara.io:find-program (concatenate 'string (tuatara.io:parent-path real) "kpsewhich")))
          (multiple-value-bind (trees var) (tuatara.defaults::tex-trees real)
            (true (member (string-right-trim "/" (tuatara.io:parent-path real)) trees :test #'string=))
            (true (some (lambda (tree) (search "texmf-dist" tree)) trees) "~s" trees)
            (when var
              (destructuring-bind (&key overlays writable &allow-other-keys)
                  (tuatara.defaults::latex-sandbox-places job tex real "/build/")
                (true (or (find var overlays :key #'first :test #'string=)
                          (find var writable :key (lambda (w) (and (consp w) (cdr w))) :test #'equal)))
                (false (find var (getf (tuatara.defaults::latex-sandbox-places job tex real "/build/") :directories)
                             :test #'string=))))))))))

(define-test (latex-export pictures-only-from-own-templates)
  ;; a note's own raw LaTeX cannot have a file from elsewhere shown to LaTeX: only its template's text is trusted
  (let ((trusted (nth-value 5 (tuatara.defaults::org-latex-document
                               (format nil "#+latex: \\includegraphics{/elsewhere/passport.pdf}~%Words.~%") :enhanced t))))
    (true trusted)
    (false (some (lambda (text) (search "/elsewhere/passport.pdf" text)) trusted))))

(define-test (latex-export docx-footnote-style)
  ;; a footnote's paragraphs take the footnotes' own style
  (let ((text (docx-text (tuatara.defaults::org-docx-document (format nil "Seen there.[fn:1]~%~%[fn:1] On the log.~%")))))
    (let* ((at (search "On the log." text))
           (paragraph (search "<w:p>" text :from-end t :end2 at)))
      (true (search "<w:pStyle w:val=\"FootnoteText\"/>" text :start2 paragraph :end2 at)))))

(define-test (latex-export brackets-after-a-break)
  ;; a line after a line break, or a table's first cell, beginning with [ is
  ;; text, never the length LaTeX's \\ can take: the break ends with \relax, the
  ;; cell begins with an empty group
  (let ((latex (org-latex-of (format nil "P — Period\\\\~%[nD] — Days\\\\~%[nH] — Hours~%~%| [x] | y |~%| z | w |~%"))))
    (true (search (format nil "Period\\\\\\relax~%[nD] — Days\\\\\\relax~%[nH]") latex))
    (true (search "{}[x] & y\\\\" latex))
    (true (search "z & w\\\\" latex))))

(define-test (latex-export setting-words-read-once)
  ;; one reading of the words a setting takes, for both exports
  (is eq :left (tuatara.defaults::export-side "Left"))
  (is eq :centre (tuatara.defaults::export-side "center"))
  (is eq :centre (tuatara.defaults::export-side "centre"))
  (is eq :justify (tuatara.defaults::export-side "justify"))
  (false (tuatara.defaults::export-side "sideways"))
  (false (tuatara.defaults::export-side nil))
  ;; a value a setting does not take is said; one it takes is not
  (true (search "spacing: wibble is none of" (tuatara.defaults::export-setting-fault "spacing" "wibble")))
  (false (tuatara.defaults::export-setting-fault "spacing" "1.15"))
  (true (search "align: sideways is none of" (tuatara.defaults::export-setting-fault "align" "left sideways right")))
  (false (tuatara.defaults::export-setting-fault "align" "left Center auto"))
  (false (tuatara.defaults::export-setting-fault "style" "Roman"))
  (false (tuatara.defaults::export-setting-fault "font" "\\small"))
  (true (tuatara.defaults::export-setting-fault "columns" "two"))
  (false (tuatara.defaults::export-setting-fault "width" "anything, read where it is used")))

(define-test (latex-export setting-faults-said)
  ;; spacing: wibble and align: sideways said after both exports, with the drawer's line
  (let* ((text (format nil "Intro.~%~%:EXPORT:~%spacing: wibble~%align: sideways~%:END:~%Words.~%"))
         (latex-notes (nth-value 4 (tuatara.defaults::org-latex-document text :enhanced t)))
         (docx-notes (nth-value 2 (tuatara.defaults::org-docx-document text))))
    (dolist (notes (list latex-notes docx-notes))
      (true (find-if (lambda (n) (search "Line 3, an :EXPORT: drawer: spacing: wibble is none of" n)) notes) "~s" notes)
      (true (find-if (lambda (n) (search "Line 3, an :EXPORT: drawer: align: sideways is none of" n)) notes) "~s" notes))))

(define-test (latex-export table-header-and-alignment)
  ;; the header is the rows above the first rule when rows follow it, in both exports
  (let* ((tuatara.defaults::*org-latex-text* (format nil "| a | 1 |~%|---+---|~%| b | 22 |~%"))
         (table (let ((found nil))
                  (tuatara.defaults::org-walk-tree (tuatara.defaults::org-parse tuatara.defaults::*org-latex-text*)
                                                    (lambda (node) (when (eq (tuatara.defaults::org-node-type node) 'tuatara.defaults::table)
                                                                     (setf found (or found node)))))
                  found)))
    (multiple-value-bind (rows special standard cookies) (tuatara.defaults::org-export-table-shape table)
      (is = 1 (tuatara.defaults::export-table-header rows))
      (false (tuatara.defaults::export-table-header (list (first rows) (second rows))))
      (is eq :left (tuatara.defaults::table-column-alignment 0 standard cookies))
      (is eq :right (tuatara.defaults::table-column-alignment 1 standard cookies))
      (false special))))

(define-test (latex-export picture-caption-at-its-side)
  ;; a picture against a margin has its caption under it, at that side, as the Word document has it
  (with-test-directory (dir)
    (alexandria:write-byte-vector-into-file (coerce (png-octets 2 2) '(vector (unsigned-byte 8))) (merge-pathnames "pic.png" dir))
    (let* ((file (namestring (merge-pathnames "note.org" dir)))
           (text (format nil ":EXPORT:~%position: right~%width: 30%~%:END:~%#+caption: A red box~%[[file:pic.png]]~%"))
           (latex (tuatara.defaults::org-latex-document text :file file :enhanced t))
           (docx (docx-text (tuatara.defaults::org-docx-document text :file file))))
      (true (search (format nil "\\raggedleft~%\\sbox0{\\includegraphics") latex) "~a" latex)
      (true (< (search "\\begin{minipage}{\\wd0}" latex) (search "\\caption{A red box}" latex) (search "\\end{minipage}" latex)))
      (true (search "<w:pStyle w:val=\"Caption\"/><w:jc w:val=\"right\"/>" docx)))))

(define-test (latex-export annexes-on-a-page-of-their-own)
  ;; the annexes begin on a new page in the PDF and in the Word document
  (let* ((text (format nil "* Main~%Text.~%* Logs :appendix:~%Logged.~%* Tools~%:EXPORT:~%annex: yes~%:END:~%More.~%"))
         (latex (tuatara.defaults::org-latex-document text :enhanced t))
         (docx (docx-text (tuatara.defaults::org-docx-document text))))
    (true (search (format nil "\\clearpage~%\\appendix~%") latex))
    (is = 1 (count-matches "\\appendix" latex))
    (let* ((at (search "Logs</w:t></w:r><w:bookmarkEnd" docx))
           (paragraph (search "<w:p>" docx :from-end t :end2 at)))
      (true (search "<w:pageBreakBefore/>" docx :start2 paragraph :end2 at)))
    (is = 1 (count-matches "<w:pageBreakBefore/>" docx))))

(define-test (latex-export program-by-its-real-folder)
  ;; a LaTeX program found through a link to its folder is run from the real folder, by the name it was found by
  (with-test-directory (dir)
    (let* ((real (namestring (merge-pathnames "real/bin/" dir)))
           (linked (namestring (merge-pathnames "linked" dir))))
      (ensure-directories-exist real)
      (alexandria:write-string-into-file "" (concatenate 'string real "xetex"))
      (sb-posix:symlink "xetex" (concatenate 'string real "xelatex"))
      (sb-posix:symlink (string-right-trim "/" real) linked)
      (is string= (concatenate 'string (tuatara.io:real-path real) "/xelatex")
          (tuatara.defaults::latex-program-place (concatenate 'string linked "/xelatex")))
      (is string= "/nowhere/xelatex" (tuatara.defaults::latex-program-place "/nowhere/xelatex")))))

(define-test (latex-export redactions-each-text-once)
  ;; the same name hidden twice, in another case too: looked for once, given away to the first hiding it
  (let ((findings (tuatara.defaults::redaction-findings
                   (format nil "{{{R(Ann Nelson)}}} then {{{R(ann nelson)}}}; Ann Nelson and NELSON.~%") :org)))
    (is equal '("Ann Nelson" "NELSON") (mapcar #'second findings))
    (is equal '("Ann Nelson" "Ann Nelson") (mapcar #'third findings))))

(define-test (latex-export log-read-from-its-end)
  ;; a LaTeX program's log made huge by its document: only its end read, where what went wrong is said
  (with-test-directory (dir)
    (let ((log (namestring (merge-pathnames "big.log" dir))))
      (with-open-file (out log :direction :output :if-exists :supersede)
        (loop repeat 100000 do (write-line "Overfull \\hbox in paragraph at lines 1--2" out))
        (write-line "! Undefined control sequence." out)
        (write-line "l.12 \\foo" out))
      (let ((text (tuatara.defaults::latex-log-text log)))
        (true (<= (length text) tuatara.defaults::*latex-log-tail*))
        (true (search "! Undefined control sequence." text))
        (is = 1 (length (tuatara.defaults::latex-errors text))))
      (is string= "" (tuatara.defaults::latex-log-text (namestring (merge-pathnames "none.log" dir)))))))

(define-test (latex-export link-to-a-part-left-out)
  ;; a link to a heading left out of the export is written as its words, never a reference to no label
  (let ((latex (tuatara.defaults::org-latex-document
                (format nil "* Kept~%See [[*Hidden][the hidden part]] and [[*Kept]].~%* Hidden :noexport:~%Secret.~%"))))
    (true (search "the hidden part" latex))
    (false (search "Secret" latex))
    (false (cl-ppcre:scan "hyperref\\[[^]]*\\]\\{the hidden part\\}" latex))
    (true (search "\\ref{" latex))))

(define-test (latex-export docx-makes-no-folders)
  ;; a Word export, as a LaTeX one, never makes the folder #+export_file_name names: it says so
  (with-test-directory (dir)
    (let ((file (merge-pathnames "survey.org" dir)))
      (alexandria:write-string-into-file (format nil "#+export_file_name: missing/survey~%Text.~%") file)
      (let ((f (export-session file (export-command "org-export-docx") :width 200)))
        (true (tuatara.frontend.fake:fake-frame-showing f "does not exist"))
        (false (probe-file (merge-pathnames "missing/" dir)))))))

(define-test (latex-export macros-held-to-a-limit)
  ;; macros calling ten of another, five deep, would grow the note 100,000-fold: the calls past the limit left as written
  (let* ((tuatara.defaults::*org-macro-limit* 10000)
         (text (with-output-to-string (s)
                 (loop for (name next) on '("a" "b" "c" "d" "e")
                       do (format s "#+macro: ~a ~{~a~}~%" name
                                  (make-list 10 :initial-element (if next (format nil "{{{~a}}}" next) "word "))))
                 (format s "{{{a}}}~%"))))
    (multiple-value-bind (latex named lines missing notes) (tuatara.defaults::org-latex-document text)
      (declare (ignore named lines missing))
      (true (< (length latex) 30000) "~:d characters" (length latex))
      (true (find-if (lambda (n) (search "*org-macro-limit*" n)) notes) "~s" notes))
    ;; one call whose value alone is past the limit: left as it is
    (let ((latex (tuatara.defaults::org-latex-document
                  (format nil "#+macro: m ~{~a~}~%{{{m(~a)}}}~%" (make-list 100 :initial-element "$1") (make-string 200 :initial-element #\x)))))
      (true (< (length latex) 30000) "~:d characters" (length latex))
      (true (search "m(xxxx" latex)))
    ;; within the limit, macros are put in place as before
    (true (search "word word" (tuatara.defaults::org-latex-document (format nil "#+macro: e word~%{{{e}}} {{{e}}}~%"))))))

(define-test (latex-export setup-file-read-within-a-limit)
  ;; a setup file larger than the limit is not read, and that is said
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "note.org" dir))))
      (with-open-file (out (merge-pathnames "setup.org" dir) :direction :output)
        (format out "#+title: Big setup~%~a~%" (make-string 5000 :initial-element #\x)))
      (let ((text (format nil "#+setupfile: setup.org~%Text.~%")))
        (let ((tuatara.defaults::*org-file-read-limit* 1000))
          (multiple-value-bind (latex named lines missing notes) (tuatara.defaults::org-latex-document text :file file)
            (declare (ignore named lines missing))
            (false (search "Big setup" latex))
            (true (find-if (lambda (n) (search "setup file larger than" n)) notes) "~s" notes)))
        (true (search "Big setup" (tuatara.defaults::org-latex-document text :file file)))))))

(define-test (latex-export templates-read-within-a-limit)
  ;; a template named by its path, LaTeX or Word, larger than the limit is refused rather than read whole
  (with-test-directory (dir)
    (let ((folder (namestring dir)))
      (with-open-file (out (merge-pathnames "big.tex" dir) :direction :output)
        (write-string (make-string 5000 :initial-element #\%) out))
      (ensure-directories-exist (merge-pathnames "big/word/" dir))
      (with-open-file (out (merge-pathnames "big/word/document.xml" dir) :direction :output)
        (write-string (make-string 5000 :initial-element #\x) out))
      (let ((tuatara.defaults::*org-file-read-limit* 1000)
            (tuatara.defaults::*docx-largest-unpacked* 1000))
        (fail (tuatara.defaults::latex-template-for "./big.tex" folder) 'tuatara.io:file-system-error)
        (fail (tuatara.defaults::docx-read-folder (concatenate 'string folder "big/")) 'tuatara.io:file-system-error))
      (is = 5000 (length (tuatara.defaults::latex-template-for "./big.tex" folder)))
      (is = 1 (length (tuatara.defaults::docx-read-folder (concatenate 'string folder "big/")))))))

(define-test (latex-export font-names-never-options)
  ;; a family a note or a template names, beginning with -, is given to fc-match as a name, never as an option
  (let ((calls '()))
    (sb-int:encapsulate 'tuatara.defaults::fontconfig-output 'record
                        (lambda (function program &rest arguments)
                          (declare (ignore function))
                          (push (cons program arguments) calls)
                          nil))
    (unwind-protect
         (progn (tuatara.defaults::font-characters "-V")
                (tuatara.defaults::font-character-ratio "-V"))
      (sb-int:unencapsulate 'tuatara.defaults::fontconfig-output 'record))
    (is = 2 (length calls))
    (dolist (call calls)
      (is equal "fc-match" (first call))
      (is equal '("--" "-V") (last call 2) "~s" call))))

(define-test (latex-export word-template-named-within-its-folder)
  ;; #+template: . or .. names no folder of the Word templates' folder, a template's name does
  (with-test-directory (dir)
    (let ((templates (concatenate 'string (namestring dir) "docx-templates/")))
      (dolist (folder (list (namestring dir) templates (concatenate 'string templates "report/")))
        (ensure-directories-exist (concatenate 'string folder "word/"))
        (with-open-file (out (concatenate 'string folder "word/document.xml") :direction :output)
          (write-string "<w:document/>" out)))
      (sb-int:encapsulate 'tuatara.defaults::docx-templates-directory 'test (lambda (function) (declare (ignore function)) templates))
      (unwind-protect
           (progn (false (tuatara.defaults::docx-template-folder ".." nil))
                  (false (tuatara.defaults::docx-template-folder "." nil))
                  (is equal (concatenate 'string templates "report/") (tuatara.defaults::docx-template-folder "report" nil)))
        (sb-int:unencapsulate 'tuatara.defaults::docx-templates-directory 'test)))))

(define-test (latex-export export-drawer-written-crlf)
  ;; in a note whose lines end with CR LF: the option goes before the line's end, a new line ends as the others do
  (flet ((after (keys text)
           (session-text (nth-value 1 (run-keys-in-turns (list keys) :text text :width 160 :height 10 :crlf t
                                                                     :mode 'tuatara.defaults::org-mode))))
         (crlf (text) (cl-ppcre:regex-replace-all "\\n" text (format nil "~C~%" #\Return))))
    (let ((text (after "j M-x o r g - i n s e r t - e x p o r t - d r a w e r RET"
                       (crlf (format nil "#+options: toc:nil~%* Head~%Text~%")))))
      (true (search (crlf (format nil "#+options: toc:nil d:(not \"EXPORT\" \"LOGBOOK\")~%* Head~%:EXPORT:~%")) text) "~s" text))
    (let ((text (after "j j M-x o r g - i n s e r t - e x p o r t - d r a w e r RET"
                       (crlf (format nil "#+title: T~%* Head~%| a | b |~%")))))
      (true (search (crlf (format nil "#+title: T~%#+options: d:(not \"EXPORT\" \"LOGBOOK\")~%* Head~%")) text) "~s" text)
      (false (cl-ppcre:scan "[^\\r]\\n" text) "~s" text))))

(define-test (latex-export docx-in-the-note-language)
  ;; the Word document marked in the language #+LANGUAGE: names, British English when it names none or no code
  (flet ((language (note)
           (let ((text (docx-text (tuatara.defaults::org-docx-document note))))
             (aref (nth-value 1 (cl-ppcre:scan-to-strings "<w:rPrDefault><w:rPr>.*?<w:lang w:val=\"([^\"]*)\"" text)) 0))))
    (is string= "de-CH" (language (format nil "#+language: de-CH~%Grüezi.~%")))
    (is string= "mi" (language (format nil "#+language: mi~%Kia ora.~%")))
    (is string= "en-GB" (language (format nil "Hello.~%")))
    (is string= "en-GB" (language (format nil "#+language: \"><w:x/>~%Hello.~%")))))

(define-test (latex-export png-written-once-in-place)
  ;; a picture's PNG: its rows read back as they were, in blocks of 65,535 octets, written with no garbage for each pixel
  (flet ((picture (side)
           (let ((pixels (make-array (* side side 4) :element-type '(unsigned-byte 8))))
             (dotimes (i (length pixels)) (setf (aref pixels i) (mod (* i 7) 256)))
             (tuatara.frontend:make-image side side 4 pixels))))
    (let* ((image (picture 200))
           (png (tuatara.defaults::png-octets image))
           (idat (search (map 'vector #'char-code "IDAT") png))
           (length (tuatara.io:octets-be png (- idat 4) 4))
           (raw (tuatara.io:inflate png :start (+ idat 6) :end (+ idat 4 length -4))))
      (is = (* 200 (1+ (* 200 4))) (length raw))
      (true (loop for y below 200
                  always (and (zerop (aref raw (* y 801)))
                              (equalp (subseq raw (1+ (* y 801)) (* (1+ y) 801))
                                      (subseq (tuatara.frontend:image-pixels image) (* y 800) (* (1+ y) 800))))))
      (is = (tuatara.defaults::crc32 png idat (+ idat 4 length)) (tuatara.io:octets-be png (+ idat 4 length) 4))
      (is = (+ idat 4 length 4 12) (length png)))
    (flet ((consed (side)
             (let ((image (picture side)))
               (loop repeat 2
                     minimize (let* ((before (sb-ext:get-bytes-consed))
                                     (png (tuatara.defaults::png-octets image)))
                                (- (sb-ext:get-bytes-consed) before (length png)))))))
      ;; beyond the file itself, octets for each pixel more: several when the file was grown a octet at a time
      (let ((per-pixel (/ (- (consed 600) (consed 300)) (- (* 600 600) (* 300 300)))))
        (true (< per-pixel 1/2) "~,2f octets a pixel" per-pixel)))))

(define-test (latex-export sixteen-bit-png-made-eight-bit)
  ;; a PNG of 16 bits a colour, which some PDF readers draw blank: made a PNG of 8 bits for the export; one of 8 taken as it is
  (with-test-directory (dir)
    (let* ((pixels (let ((p (make-array (* 4 2 4) :element-type '(unsigned-byte 8))))
                     (dotimes (i (length p) p) (setf (aref p i) (mod (* 37 i) 256)))))
           (eight (tuatara.defaults::png-octets (tuatara.frontend:make-image 4 2 4 pixels)))
           ;; the same rows read as 2 by 2 pixels of 16 bits a channel: its header changed, its check made again
           (sixteen (copy-seq eight))
           (deep (namestring (merge-pathnames "deep.png" dir)))
           (plain (namestring (merge-pathnames "plain.png" dir))))
      (tuatara.io:octets-put-integer sixteen 2 4 :at 16 :big-endian t)
      (setf (aref sixteen 24) 16)
      (tuatara.io:octets-put-integer sixteen (tuatara.defaults::crc32 sixteen 12 29) 4 :at 29 :big-endian t)
      (alexandria:write-byte-vector-into-file sixteen deep)
      (alexandria:write-byte-vector-into-file eight plain)
      (true (tuatara.defaults::png-sixteen-bit-p deep))
      (false (tuatara.defaults::png-sixteen-bit-p plain))
      (is string= plain (tuatara.defaults::latex-picture plain))
      (let ((made (tuatara.defaults::latex-picture deep)))
        (true (and made (string/= made deep)) "~s" made)
        (false (tuatara.defaults::png-sixteen-bit-p made))))))

(define-test (latex-export window-font-family-with-its-style)
  ;; the window's font named with a weight or style before its size: its family found, the
  ;; longest installed that begins its name; the fonts installed asked for once for the three parts
  (let ((installed (make-hash-table :test 'equalp)))
    (dolist (family '("Cascadia Code" "Cascadia Code NF" "Kauri Mono")) (setf (gethash family installed) family))
    (let ((tuatara.defaults::*latex-mono-font* nil) (tuatara.defaults::*latex-font-choices* '((:mono "Kauri Mono"))))
      (progv '(tuatara-user:*gui-font*) '("Cascadia Code NF Medium 12")
        (is equal "Cascadia Code NF" (tuatara.defaults::chosen-font :mono installed)))
      (progv '(tuatara-user:*gui-font*) '("Cascadia Code 11")
        (is equal "Cascadia Code" (tuatara.defaults::chosen-font :mono installed)))
      (progv '(tuatara-user:*gui-font*) '("Cascadia Codex 11")
        (is equal "Kauri Mono" (tuatara.defaults::chosen-font :mono installed)))))
  (let ((asked 0))
    (sb-int:encapsulate 'tuatara.defaults::installed-font-families 'count-asked
                        (lambda (function &rest arguments) (incf asked) (apply function arguments)))
    (unwind-protect (is = 3 (length (tuatara.defaults::chosen-fonts)))
      (sb-int:unencapsulate 'tuatara.defaults::installed-font-families 'count-asked))
    (is = 1 asked)))

(define-test (latex-export png-made-whole)
  ;; a picture made into PNG of more than one stored block, its rows across their edges: each
  ;; row its filter octet then its pixels, the Adler-32 sum of them all at the end
  (let* ((width 301) (height 250) (channels 3)
         (pixels (let ((p (make-array (* width height channels) :element-type '(unsigned-byte 8))))
                   (dotimes (i (length p) p) (setf (aref p i) (mod (* i 7) 256)))))
         (png (tuatara.defaults::png-octets (tuatara.frontend:make-image width height channels pixels)))
         (raw (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    ;; the zlib stream of IDAT after its two header octets: stored blocks, each five octets then its data
    (let ((at (+ 8 25 8 2)))
      (loop (let ((last (aref png at)) (length (logior (aref png (+ at 1)) (ash (aref png (+ at 2)) 8))))
              (loop for i from (+ at 5) below (+ at 5 length) do (vector-push-extend (aref png i) raw))
              (setf at (+ at 5 length))
              (when (= last 1) (return))))
      (let ((wanted (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)) (a 1) (b 0))
        (dotimes (y height)
          (vector-push-extend 0 wanted)
          (dotimes (x (* width channels)) (vector-push-extend (aref pixels (+ (* y width channels) x)) wanted)))
        (true (equalp raw wanted))
        (loop for octet across wanted do (setf a (mod (+ a octet) 65521) b (mod (+ b a) 65521)))
        (is = (logior (ash b 16) a) (tuatara.io:octets-integer png at 4 :big-endian t))))))

(define-test (latex-export docx-picture-stored-once)
  ;; a picture shown twice is stored in the Word document once, each showing its own drawing
  (with-test-directory (dir)
    (alexandria:write-byte-vector-into-file (coerce (png-octets 2 2) '(vector (unsigned-byte 8))) (merge-pathnames "pic.png" dir))
    (let* ((file (namestring (merge-pathnames "note.org" dir)))
           (text (docx-text (tuatara.defaults::org-docx-document (format nil "[[file:pic.png]]~%~%Text.~%~%[[file:pic.png]]~%") :file file))))
      (true (search "media/image1.png" text))
      (false (search "image2" text))
      (is = 2 (length (cl-ppcre:all-matches-as-strings "<a:blip r:embed=" text)))
      (true (search "<wp:docPr id=\"2\" name=\"Picture 2\"/>" text)))))

(defun latex-without-breaks (latex)
  "LATEX with the places a line may break in code taken out, as copying from the PDF leaves them."
  (cl-ppcre:regex-replace-all "\\\\codebreak\\{\\}|\\\\discretionary\\{\\}\\{\\}\\{\\}" latex ""))

(defun latex-longest-between-breaks (latex)
  "The most characters of LATEX between two places a line may break in code."
  (loop for piece in (cl-ppcre:split "\\\\codebreak\\{\\}|\\\\discretionary\\{\\}\\{\\}\\{\\}|\\\\texttt\\{|\\}|[ \\n&]|\\\\\\\\" latex)
        maximize (length piece)))

(define-test (latex-export long-code-breaks)
  (let ((enhanced (lambda (text) (tuatara.defaults::org-latex-document text :enhanced t)))
        (digest "d41d8cd98f00b2040000e9800998ecf8427e0000da39a3ee5e6b4b0d3255bfef95601890"))
    ;; a long span: places to break in a path and a message, and in a digest with none; with
    ;; them taken out, the span is what was written
    (dolist (span '("~/Projects/palimpsest-practice/hand.pdf"
                    "Wrote ~/Projects/palimpsest-practice/hand.pdf: 434 octets"
                    "tuatara-user:*org-latex-text-characters*"
                    "d41d8cd98f00b2040000e9800998ecf8427e0000da39a3ee5e6b4b0d"))
      (let ((latex (funcall enhanced (format nil "See =~a= here.~%" span))))
        (true (search "\\codebreak{}" latex) "no place to break in ~a" span)
        (true (search (format nil "\\texttt{~a}" (tuatara.defaults::latex-escape span :special-strings nil))
                      (latex-without-breaks latex))
              "the copied text differs for ~a" span)))
    ;; a long line of a block: its digest may break, and the line is what was written
    (let* ((line (format nil "SHA-256     ~a" digest))
           (latex (funcall enhanced (format nil "#+begin_example~%~a~%#+end_example~%" line))))
      (true (search "breakautoindent=false,breakindent=1em" latex))
      (true (search "\\discretionary{}{}{}" latex))
      (true (< (latex-longest-between-breaks (subseq latex (search "SHA-256" latex))) 17))
      (true (search line (latex-without-breaks latex))))))
