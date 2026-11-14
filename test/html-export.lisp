;;;; test/html-export.lisp --- Org documents written as web pages.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test html-export
  :description "An Org document is written as one web page by the reading the LaTeX export uses: headings,
numbers, lists, tables, blocks, markup, links, footnotes, every character escaped, raw HTML left out, nothing
run; a chapter of the manual as parts of a page. The stylesheet's colours come from the Ao theme; a
page loads nothing from elsewhere.")

(defun html-of (text &rest keys)
  "TEXT, an Org document, as a web page (ORG-HTML-DOCUMENT), with KEYS, and the notes it gives."
  (multiple-value-bind (html output notes) (apply #'tuatara.defaults::org-html-document text keys)
    (declare (ignore output))
    (values html notes)))

(defun html-parts-of (text &rest keys)
  "TEXT, an Org document, as the parts of a web page (ORG-HTML-PARTS)."
  (apply #'tuatara.defaults::org-html-parts text keys))

(defun html-body-of (text &rest keys)
  "The body of the web page made from the Org document TEXT."
  (getf (apply #'html-parts-of text keys) :body))

(define-test (html-export hostile-text-is-escaped)
  (let* ((hostile "<script>alert(1)</script>")
         (text (format nil "#+title: ~a
#+subtitle: ~a

* ~a
#+caption: cap ~a
| ~a |

Words ~a ~~~a~~ =~a=.[fn:1]

#+begin_try ~a
1. Step ~a
#+end_try

[[https://x.example/\"><script>y][~a]]

[fn:1] Note ~a
" hostile hostile hostile hostile hostile hostile hostile hostile hostile hostile hostile hostile))
         (html (html-of text)))
    (false (search hostile html))
    (false (search "<script" html))
    (true (search "&lt;script&gt;alert(1)&lt;/script&gt;" html))))

(define-test (html-export links-cannot-run-anything)
  (let ((body (html-body-of "[[javascript:alert(1)][run]] [[file:javascript:alert(1)][it]] [[data:text/html,hi][x]]")))
    (false (search "href=\"javascript:" body))
    (false (search "href=\"data:" body))
    (true (search "javascript%3Aalert" body))
    (true (search "run" body))))

(define-test (html-export raw-html-is-left-out-and-said)
  (multiple-value-bind (html notes)
      (html-of "A @@html:<b>snip</b>@@ word.

#+html: <i>line</i>

#+begin_export html
<script>block()</script>
#+end_export

#+begin_export latex
\\typeout{raw}
#+end_export
")
    (false (search "snip" html))
    (false (search "<i>line" html))
    (false (search "block()" html))
    (false (search "typeout" html))
    (true (member "A block of raw HTML left out of the web page" notes :test #'uiop:string-prefix-p))
    (true (member "A #+HTML: line left out of the web page" notes :test #'uiop:string-prefix-p))
    (true (member "A raw HTML snippet left out of the web page" notes :test #'uiop:string-prefix-p))))

(define-test (html-export nothing-is-run)
  (with-test-directory (dir)
    (let* ((mark (merge-pathnames "ran.txt" dir))
           (file (namestring (merge-pathnames "note.org" dir)))
           (text (format nil "#+macro: m (eval (with-open-file (s \"~a\" :direction :output) (print 1 s)))
{{{m}}}

#+begin_src lisp :exports results
(with-open-file (s \"~a\" :direction :output) (print 1 s))
#+end_src

#+call: x() =1
" (namestring mark) (namestring mark)))
           (html (html-of text :file file)))
      (false (probe-file mark))
      (true (search "with-open-file" html)))))

(define-test (html-export an-empty-document)
  (multiple-value-bind (html notes) (html-of "")
    (true (search "<!doctype html>" html))
    (true (search "<main>" html))
    (false notes)))

(define-test (html-export headings-and-numbers)
  (let* ((parts (html-parts-of (format nil "#+title: T~%~%* One~%** Two~%* Three :noexport:~%* Four~%")))
         (body (getf parts :body))
         (headings (getf parts :headings)))
    (is equal '(1 2 1) (mapcar #'first headings))
    (is equal '("1" "1.1" "2") (mapcar #'second headings))
    (true (search "<h2 id=\"one\"><span class=\"num\">1</span> One</h2>" body))
    (true (search "<h3 id=\"two\"><span class=\"num\">1.1</span> Two</h3>" body))
    (false (search "Three" body))
    (true (search "<nav class=\"contents\">" body) "the contents, as #+options: says by default")
    (true (search "<h1>T</h1>" body))))

(define-test (html-export chapter-parts)
  (let* ((parts (html-parts-of (format nil "* A chapter~%~%The lead.~%~%Then ~~C-x~~ and =name=.~%~%** Section~%")
                                 :chapter 7 :keys t))
         (body (getf parts :body)))
    (is equal '("7" "7.1") (mapcar #'second (getf parts :headings)))
    (true (search "<h1 id=\"a-chapter\">A chapter</h1>" body) "the number is left to the contents")
    (true (search "<h2 id=\"section\"><span class=\"num\">7.1</span> Section</h2>" body))
    (true (search "<p class=\"lead\">The lead.</p>" body))
    (true (search "<p>Then <kbd>C-x</kbd> and <code>name</code>.</p>" body))
    (false (search "<nav" body) "no contents of its own")))

(define-test (html-export contents-nest)
  (let ((list (tuatara.defaults::org-html-contents
               '((1 "1" "A" "a") (2 "1.1" "B" "b") (1 "2" "C" "c") (3 nil "D" "d")) :page "p.html")))
    (is string= "<ol><li><a href=\"p.html#a\">1 A</a><ol><li><a href=\"p.html#b\">1.1 B</a></li></ol></li><li><a href=\"p.html#c\">2 C</a><ol><li><ol><li><a href=\"p.html#d\">D</a></li></ol></li></ol></li></ol>"
        list))
  (is string= "" (tuatara.defaults::org-html-contents '()))
  (is string= "<ol><li><a href=\"#a\">A</a></li></ol>"
      (tuatara.defaults::org-html-contents '((1 nil "A" "a") (2 nil "B" "b")) :depth 1)))

(define-test (html-export ids-are-unique-and-plain)
  (let ((tuatara.defaults::*org-export* (tuatara.defaults::make-org-export "" nil nil))
        (tuatara.defaults::*org-html* (tuatara.defaults::make-org-html)))
    (is string= "the-screen" (tuatara.defaults::org-html-new-id "The Screen"))
    (is string= "the-screen-2" (tuatara.defaults::org-html-new-id "The Screen"))
    (is string= "place" (tuatara.defaults::org-html-new-id "?!"))))

(define-test (html-export blocks-of-the-manual)
  (let ((body (html-body-of (format nil "* C~%
#+begin_try Try it, with a letter
1. Press ~~C-x~~.
2. Then =this=.
#+end_try

#+name: fig-one
#+caption: The caption
#+begin_example
a < b
#+end_example

#+begin_example
plain
#+end_example

#+begin_src lisp
(+ 1 2)
#+end_src

#+begin_quote
Said.
#+end_quote

#+begin_center
Mid.
#+end_center

#+begin_verse
one
two
#+end_verse

-----
") :keys t :chapter 1)))
    (true (search "<div class=\"try\">
<h3>Try it, with a letter</h3>
<ol>
<li>Press <kbd>C-x</kbd>.</li>
<li>Then <code>this</code>.</li>
</ol>
</div>" body))
    (true (search "<figure id=\"fig-one\">
<pre class=\"ex\">a &lt; b
</pre>
<figcaption>The caption</figcaption>
</figure>" body))
    (true (search "<pre class=\"ex\">plain" body))
    (true (search "<pre class=\"ex\"><code class=\"language-lisp\">(+ 1 2)" body))
    (true (search "<blockquote>" body))
    (true (search "<div class=\"centre\">" body))
    (true (search "<p class=\"verse\">one<br>" body))
    (true (search "<hr>" body))))

(define-test (html-export a-special-block-class-is-plain)
  (let ((body (html-body-of (format nil "#+begin_a\"b<script>~%x~%#+end_a\"b<script>~%"))))
    (false (search "<script" body))
    (true (search "<div class=\"abscript\">" body))))

(define-test (html-export special-block-keeps-its-words)
  (let ((node (first (tuatara.defaults::org-node-children
                      (first (tuatara.defaults::org-node-children
                              (tuatara.defaults::org-parse (format nil "#+begin_try Some words~%x~%#+end_try~%"))))))))
    (is string= "Some words" (tuatara.defaults::org-property node :parameters))))

(define-test (html-export a-picture-of-the-screen)
  (let ((body (html-body-of (format nil "#+attr_html: :screen t :ml 2 :pr 3 :signs PD :lit P
#+caption: A screen
#+begin_example
Mix <the> batter
 TUATARA[N] PD [-] pancakes.txt
  2/3  Find file: ~~/notes/sh
#+end_example
"))))
    (true (search "<div class=\"screen\">" body))
    (true (search "Mix &lt;the&gt; batter" body))
    (true (search "<div class=\"ml\"> TUATARA[N] <span class=\"sign\">P</span><span class=\"faint\">D</span> [-] pancakes.txt</div>" body))
    (true (search "<div class=\"pr\">  2/3  <b>Find file:</b> ~/notes/sh█</div>" body))))

(define-test (html-export screen-lines-with-no-parts)
  (is string= "a  b" (tuatara.defaults::org-html-mode-line "a  b" nil nil))
  (is string= "a <span class=\"sign\">X</span>" (tuatara.defaults::org-html-mode-line "a X" "X" "X"))
  (is string= "no colon█" (tuatara.defaults::org-html-prompt-line "no colon"))
  (is string= "<b>Go:</b> &lt;x█" (tuatara.defaults::org-html-prompt-line "Go: <x")))

(define-test (html-export tables-markup-and-entities)
  (let ((body (html-body-of (format nil "| Keys  | Name   |
|-------+--------|
| ~~C-x~~ | a\\vert{}b |
| ~~C-y~~ | *bold* |

| one | 1 |
| two | 2 |

*b* /i/ _u_ +s+ x^{2} a_{1} line\\\\
next
") :keys t)))
    (true (search "<thead>" body))
    (true (search "<th>Keys</th><th>Name</th>" body))
    (true (search "<td><kbd>C-x</kbd></td><td>a|b</td>" body))
    (true (search "<td class=\"right\">1</td>" body) "a column of numbers is set to the right")
    (true (search "<td><strong>bold</strong></td>" body))
    (true (search "<strong>b</strong> <em>i</em> <u>u</u> <del>s</del>" body))
    (true (search "x<sup>2</sup> a<sub>1</sub>" body))
    (true (search "line<br>" body))))

(define-test (html-export every-link-type)
  (with-test-directory (dir)
    (let* ((file (namestring (merge-pathnames "note.org" dir)))
           (body (html-body-of (format nil "* A head
:PROPERTIES:
:CUSTOM_ID: spot
:END:

<<word>>Here.

[[https://orgmode.org/a?b=1&c=2][Org]] [[http://x.example]] [[mailto:a@b.example][mail]]
[[file:02-other.org][next]] [[file:pic dir/a b.txt]] [[#spot][custom]] [[*A head]] [[word][target]]
[[denote:20260101T000000][a note]] [[nowhere]]
") :file file)))
      (true (search "<a href=\"https://orgmode.org/a?b=1&amp;c=2\">Org</a>" body))
      (true (search "<a href=\"http://x.example\">http://x.example</a>" body))
      (true (search "<a href=\"mailto:a@b.example\">mail</a>" body))
      (true (search "<a href=\"02-other.html\">next</a>" body))
      (true (search "href=\"pic%20dir/a%20b.txt\"" body))
      (true (search "<a href=\"#spot\">custom</a>" body))
      (true (search "<a href=\"#spot\">A head</a>" body))
      (true (search "<a href=\"#word\">target</a>" body))
      (true (search "a note" body))
      (true (search "nowhere" body))
      (false (search "href=\"#nowhere" body)))))

(define-test (html-export a-file-address)
  (is string= "a%20b.org" (tuatara.defaults::org-html-file-address "a b.org"))
  (is string= "b.org" (tuatara.defaults::org-html-file-address "b.org::*Head"))
  (true (uiop:string-prefix-p "file:///" (tuatara.defaults::org-html-file-address "/tmp/x.png"))))

(define-test (html-export pictures-on-the-page)
  (with-test-directory (dir)
    (let* ((directory (merge-pathnames "notes/" dir))
           (file (namestring (merge-pathnames "note.org" directory)))
           (near (merge-pathnames "pic.png" directory))
           (outside (merge-pathnames "outside.png" dir)))
      (ensure-directories-exist near)
      (alexandria:write-string-into-file "x" near)
      (alexandria:write-string-into-file "x" outside)
      (let ((body (html-body-of (format nil "#+caption: Cap
#+attr_html: :alt <hostile> :width 50%
[[file:pic.png]]

[[file:~a]]

[[file:gone.svg]]

[[https://x.example/remote.png]]
" (namestring outside)) :file file)))
        (true (search "<figure>" body))
        (true (search "<img src=\"pic.png\" alt=\"&lt;hostile&gt;\" width=\"50%\">" body))
        (true (search "<figcaption>Cap</figcaption>" body))
        (true (search "<code>outside.png</code>" body) "from outside the note's directory, its name alone")
        (true (search "<code>gone.svg</code>" body) "not there, so its name alone")
        (false (search "<img src=\"https" body) "a page never loads from the network")
        (true (search "<a href=\"https://x.example/remote.png\">" body))))))

(define-test (html-export lists-and-footnotes)
  (let ((body (html-body-of (format nil "- [X] done
- [ ] open
  - inner


1. one
2. two


- term :: meaning


A note[fn:a] and again[fn:a] and inline[fn::written].

[fn:a] The definition.
"))))
    (true (search "<li>☒ done</li>" body))
    (true (search "<ul>
<li>inner</li>" body))
    (true (search "<ol>
<li>one</li>" body))
    (true (search "<dt>term</dt><dd>meaning</dd>" body))
    (is = 2 (cl-ppcre:count-matches "href=\"#fn-1\"" body))
    (true (search "<li id=\"fn-1\">" body))
    (true (search "The definition." body))
    (true (search "<li id=\"fn-2\">written</li>" body))))

(define-test (html-export an-empty-footnote-section-is-none)
  (false (search "footnotes" (html-body-of "No notes."))))

(define-test (html-export redaction-never-shows-the-text)
  (let ((html (html-of (format nil "#+macro: redact $1~%Hidden: {{{redact(topsecret)}}}.~%"))))
    (declare (ignorable html))
    (false (search "topsecret" (html-of (format nil "A {{{redact(topsecret)}}} word.~%"))))
    (true (search "<span class=\"redacted\">█████████</span>" (html-of (format nil "A {{{redact(topsecret)}}} word.~%"))))))

(define-test (html-export the-title-and-more)
  (let ((html (html-of (format nil "#+title: The <T>
#+subtitle: Sub
#+author: Ann
#+date: 2026
#+language: mi
#+options: toc:nil

Text.
"))))
    (true (search "<title>The &lt;T&gt;</title>" html))
    (true (search "<html lang=\"mi\">" html))
    (true (search "<h1>The &lt;T&gt;</h1>" html))
    (true (search "Sub" html))
    (true (search "Ann" html))
    (false (search "class=\"contents\"" html))))

(define-test (html-export options-are-honoured)
  (let ((body (html-body-of (format nil "#+options: num:nil -:nil toc:nil~%* TODO [#A] H :tag:~%a--b...~%"))))
    (false (search "class=\"num\"" body))
    (true (search "a--b..." body))
    (true (search "<span class=\"todo\">TODO</span>" body))
    (true (search "<span class=\"tag\">tag</span>" body)))
  (let ((body (html-body-of "a--b --- c...")))
    (true (search "a–b — c…" body)))
  (let ((body (html-body-of (format nil "#+options: toc:nil~%#+toc: headlines 1~%* One~%** Two~%"))))
    (is = 1 (cl-ppcre:count-matches "#one" body) "the contents placed, as asked, deep enough for one level")
    (false (search "#two" body))))

(define-test (html-export a-page-loads-nothing)
  (let* ((html (html-of (format nil "Text [[https://x.example][x]]~%")))
         (stylesheet (tuatara.defaults::org-html-stylesheet)))
    (false (search "<script" html))
    (false (search "http" stylesheet))
    (true (search "default-src 'none'" html))
    (false (cl-ppcre:scan "src=\"http" html))))

(define-test (html-export page-with-files-of-its-own)
  (let ((html (tuatara.defaults::org-html-page "T" "<p></p>" :stylesheet "a.css" :script "b.js")))
    (true (search "<link rel=\"stylesheet\" href=\"a.css\">" html))
    (true (search "<script src=\"b.js\" defer></script>" html))
    (true (search "script-src 'self'" html))
    (false (search "<style>" html))))

(define-test (html-export page-with-an-icon)
  (let ((html (tuatara.defaults::org-html-page "T" "<p></p>" :icon "a&b.png")))
    (true (search "<link rel=\"icon\" href=\"a&amp;b.png\">" html))
    (true (search "img-src 'self'" html)))
  (false (search "rel=\"icon\"" (tuatara.defaults::org-html-page "T" "<p></p>"))))

(define-test (html-export the-theme-gives-the-colours)
  (let ((light (tuatara.defaults::web-page-colours :light))
        (dark (tuatara.defaults::web-page-colours :dark))
        (stylesheet (tuatara.defaults::org-html-stylesheet)))
    (is string= "#ffffff" (cdr (assoc "bg" light :test #'string=)))
    (is string= "#080d15" (cdr (assoc "bg" dark :test #'string=)))
    (is string= "#1565c0" (cdr (assoc "accent" light :test #'string=)))
    (is string= "#99c7ff" (cdr (assoc "accent" dark :test #'string=)))
    (is string= "#f0f0f0" (cdr (assoc "panel" light :test #'string=)))
    (is string= "#0d1526" (cdr (assoc "panel" dark :test #'string=)))
    (is string= "#080d15" (cdr (assoc "screen" light :test #'string=)) "a picture of the screen is dark either way")
    (true (search "--bg: #080d15;" stylesheet))
    (true (search "@media (prefers-color-scheme: dark)" stylesheet))
    (true (search "color-scheme: dark" stylesheet))
    (false (cl-ppcre:scan "#[0-9a-fA-F]{3,8}\\b" tuatara.defaults::*org-html-style*) "the stylesheet's rules name no colour")
    (false (search "data-theme" stylesheet) "an exported page follows the system alone")))

(define-test (html-export the-reader-chooses-the-colours)
  (let ((chosen (tuatara.defaults::org-html-stylesheet :chosen t)))
    (true (search "@media screen and (prefers-color-scheme: dark) {
:root:not([data-theme=\"light\"]) {" chosen) "the system's dark unless light is chosen")
    (true (search "@media screen {
:root[data-theme=\"dark\"] {" chosen) "dark when chosen")
    (is = 2 (cl-ppcre:count-matches "--bg: #080d15;" chosen))
    (is = 1 (cl-ppcre:count-matches "--bg: #ffffff;" chosen) "paper has the light colours alone")))

(define-test (html-export written-beside-the-note)
  (with-test-directory (dir)
    (let ((file (merge-pathnames "note.org" dir)))
      (alexandria:write-string-into-file (format nil "#+title: Out~%~%Words <here>.~%") file)
      (export-session file (export-command "org-export-html"))
      (let ((page (alexandria:read-file-into-string (merge-pathnames "note.html" dir) :external-format :utf-8)))
        (true (search "Words &lt;here&gt;." page))
        (true (search "<style>" page))))))

(defun manual-tool (name)
  "The function NAME of the manual's build (tools/manual-html.lisp), loaded when first asked for."
  (unless (find-package "MANUAL-HTML")
    (load (asdf:system-relative-pathname :tuatara "tools/manual-html.lisp")))
  (find-symbol (string-upcase name) "MANUAL-HTML"))

(define-test (html-export a-docstring-naming-a-backslash-command)
  (unless (find-package "MANUAL-TABLES")
    (load (asdf:system-relative-pathname :tuatara "tools/manual-tables.lisp")))
  (is string= "a =\\p{Lu}= b and =\\d=" (funcall (find-symbol "PLAIN-BACKSLASHES" "MANUAL-TABLES") "a \\p{Lu} b and \\d")))

(define-test (html-export command-table-has-modal-and-non-modal-keys)
  (unless (find-package "MANUAL-TABLES")
    (load (asdf:system-relative-pathname :tuatara "tools/manual-tables.lisp")))
  (flet ((tool (name) (find-symbol name "MANUAL-TABLES")))
    (let ((tuatara.command:*editor* (nth-value 1 (run-keys ""))))
      (let* ((row (assoc "forward-char" (funcall (tool "COMMAND-ROWS")) :test #'string=))
             (cells (funcall (tool "CELLS") :keys-first row))
             (kind (assoc "commands" (symbol-value (tool "*KINDS*")) :test #'string=)))
        (is equal '("Modal" "Non-modal" "Command" "What it does") (third kind))
        (true (search "~l~" (first cells)) "the NORMAL state's key is in the Modal column")
        (false (search "~C-f~" (first cells)))
        (true (search "~C-f~" (second cells)) "the global key is in the Non-modal column")
        (false (search "~l~" (second cells)))))))

(define-test (html-export options-table-explains-the-choice)
  (unless (find-package "MANUAL-TABLES")
    (load (asdf:system-relative-pathname :tuatara "tools/manual-tables.lisp")))
  (let* ((rows (funcall (find-symbol "OPTION-ROWS" "MANUAL-TABLES")))
         (gui (find-if (lambda (row) (search "--gui" (first row))) rows))
         (version (find-if (lambda (row) (search "--version" (first row))) rows)))
    (true (search "Open a window of its own instead" (second gui)))
    (true (search "Use it when the terminal is the limit" (second gui)) "the reason to choose it follows")
    (false (search "  " (second gui)) "the paragraphs joined by one space")
    (false (search "Use it" (second version)) "an option with nothing to choose stays one line")))

(define-test (html-export a-long-table-split)
  (let* ((rows (loop for n from 1 to 7 collect (format nil "| ~d |" n)))
         (head (list "| H |" "|---|"))
         (text (format nil "before~%~{~a~%~}after" (append head rows))))
    (let ((split (let ((symbol (manual-tool "*longest-table*")))
                   (progv (list symbol) '(3)
                     (funcall (manual-tool "long-tables-split") text)))))
      (is = 3 (cl-ppcre:count-matches "\\| H \\|" split))
      (true (uiop:string-prefix-p "before" split))
      (true (search "after" split)))
    (is string= text (funcall (manual-tool "long-tables-split") text) "short tables are left as they are")))

(defun manual-tree (dir)
  "Make a manual of two chapters under DIR, as tools/manual-tables.sh leaves one."
  (let ((generated (merge-pathnames "doc/manual/generated/" dir)))
    (ensure-directories-exist generated)
    (alexandria:write-string-into-file "// filter" (merge-pathnames "doc/manual/manual.js" dir))
    (let ((icon (merge-pathnames "data/icons/hicolor/32x32/apps/tuatara.png" dir)))
      (ensure-directories-exist icon)
      (alexandria:write-string-into-file "png" icon))
    (alexandria:write-string-into-file "#+title: Not a chapter" (merge-pathnames "commands.org" generated))
    (alexandria:write-string-into-file
     (format nil "#+title: One~%#+language: en~%~%Next: [[file:02-two.org][2. Two]]~%~%* One~%~%A lead.~%~%** Part~%~%#+begin_try Try it~%1. Go to [[file:02-two.org][Two]].~%#+end_try~%~%Next: [[file:02-two.org][2. Two]]~%")
     (merge-pathnames "01-one.org" generated))
    (alexandria:write-string-into-file
     (format nil "#+title: Two~%~%Previous: [[file:01-one.org][1. One]]~%~%* Two~%~%Words.~%~%Previous: [[file:01-one.org][1. One]]~%")
     (merge-pathnames "02-two.org" generated))
    dir))

(define-test (html-export the-manual-as-pages)
  (with-test-directory (dir)
    (manual-tree dir)
    (funcall (manual-tool "run-html") dir)
    (let ((one (alexandria:read-file-into-string (merge-pathnames "build/manual/01-one.html" dir) :external-format :utf-8))
          (index (alexandria:read-file-into-string (merge-pathnames "build/manual/index.html" dir) :external-format :utf-8)))
      (true (search "<strong>Tuatara Manual</strong>" one))
      (true (search "<span>Next: <a href=\"02-two.html\">Two</a></span>" one))
      (false (search "Next: [[" one) "the chapter's own links are replaced by the bar")
      (true (search "<li id=\"nav-01-one\" class=\"here\">" one))
      (true (search "<div class=\"try\">" one))
      (true (search "<a href=\"02-two.html\">Two</a>." one))
      (true (search "<nav class=\"contents\">" index))
      (true (search "<li id=\"nav-index\" class=\"here\">" index) "the contents page marked as the one shown")
      (is = 1 (cl-ppcre:count-matches "0 Contents" index) "only in the column, not in the page's own list")
      (true (probe-file (merge-pathnames "build/manual/manual.css" dir)))
      (true (probe-file (merge-pathnames "build/manual/manual.js" dir)))
      (true (probe-file (merge-pathnames "build/manual/tuatara.png" dir)))
      (true (search "<link rel=\"icon\" href=\"tuatara.png\">" index))
      (true (search "monospace" (alexandria:read-file-into-string (merge-pathnames "build/manual/manual.css" dir))))
      (true (search "--head: var(--mono);" (alexandria:read-file-into-string (merge-pathnames "build/manual/manual.css" dir)))
            "the headings and the contents column monospaced too")
      (true (search "td:not(:last-child) kbd, main article td:not(:last-child) code { white-space: nowrap; }"
                    (alexandria:read-file-into-string (merge-pathnames "build/manual/manual.css" dir)))
            "keys and names in a table kept whole, a long description taking its own column's room")
      (true (search "main article { max-width: none; }"
                    (alexandria:read-file-into-string (merge-pathnames "build/manual/manual.css" dir)))
            "the text as wide as the window leaves it beside the contents column")
      (false (search "Not a chapter" index)))))

(define-test (html-export the-manual-filter-searches-the-text)
  (with-test-directory (dir)
    (manual-tree dir)
    (funcall (manual-tool "run-html") dir)
    (let* ((script (alexandria:read-file-into-string (merge-pathnames "build/manual/manual.js" dir)
                                                     :external-format :utf-8))
           (line (subseq script 0 (position #\Newline script)))
           (index (tuatara.text:json-read (subseq line (1+ (position #\= line)) (position #\; line :from-end t)))))
      (true (uiop:string-prefix-p "window.tuataraManualText = " script) "the text comes before the filter")
      (true (search "// filter" script) "and the filter follows it")
      (true (search "a lead." (gethash "01-one" index)) "a chapter's opening under the chapter's own key")
      (false (search "go to two" (gethash "01-one" index)))
      (true (search "go to two" (gethash "01-one#part" index)) "a section's text, in lower case, under its key")
      (true (search "words." (gethash "02-two" index))))
    (let ((deep (funcall (manual-tool "section-texts") '(:stem "03-three")
                         (list :headings '((1 nil "Three" "three") (2 "3.1" "Part" "part")
                                           (5 nil "Deep" "deep"))
                               :body "<h1 id=\"three\">Three</h1><p>Open.</p><h2 id=\"part\">Part</h2>
<p>Here &amp; there.</p><h5 id=\"deep\">Deep</h5><p>Far down.</p>"))))
      (is equal '("03-three" "03-three#part") (mapcar #'car deep) "a heading too deep for the column has no key")
      (is string= "part here & there. deep far down." (cdr (second deep)) "its text goes to the one above"))))

(define-test (html-export the-manual-on-one-page)
  (with-test-directory (dir)
    (manual-tree dir)
    (funcall (manual-tool "run-html") dir)
    (flet ((page (name)
             (alexandria:read-file-into-string (merge-pathnames (format nil "build/manual/~a" name) dir)
                                               :external-format :utf-8)))
      (let ((all (page "all.html"))
            (one (page "01-one.html"))
            (css (page "manual.css")))
        (true (search "<section class=\"chapter\" id=\"01-one\">" all))
        (true (search "<section class=\"chapter\" id=\"02-two\">" all))
        (true (< (search "id=\"01-one\"" all) (search "id=\"02-two\"" all)) "the chapters in order")
        (true (search "<a href=\"#02-two\">Two</a>." all) "a link to a chapter goes to its section")
        (true (search "id=\"01-one--part\"" all) "headings named after their chapter, as chapters share names")
        (true (search "href=\"#01-one--part\"" all) "the contents lead to the headings on the page")
        (false (search "href=\"02-two.html" all))
        (false (search "Next: <a" all) "no bars between chapters on one page")
        (true (search "<a href=\"index.html\">By chapter</a>" all))
        (true (search "<a href=\"all.html\">All on one page</a>" one))
        (dolist (html (list all one))
          (true (search "<nav class=\"toc\" id=\"contents\"" html))
          (let ((front (search "<li id=\"nav-index\"><a href=\"index.html\">0 Contents</a></li>" html)))
            (true front "the column opens with 0 Contents, a link to the contents page")
            (true (and front (< front (search "1 One</a>" html))) "0 Contents comes before chapter 1"))
          (false (search "to-contents" html) "no link in the corner")
          (true (search "<button type=\"button\" class=\"theme\" hidden>" html)))
        (true (search ":root[data-theme=\"dark\"]" css) "the reader may choose dark")
        (true (search "@media print" css))
        (true (search "nav.contents a, .node a { text-decoration: none; }" css))
        (true (search "nav.toc a.in-view {" css) "the section being read on the whole manual is marked")
        (true (search "main article h2 { border-bottom: 3px solid var(--accent);" css) "a section's heading stands out while scrolling")))
    ;; and the script the pages run never keeps a link from opening its page
    (let ((script (alexandria:read-file-into-string (asdf:system-relative-pathname :tuatara "doc/manual/manual.js"))))
      (false (search "preventDefault" script) "manual.js keeps no link from opening its page")
      (true (search "item.querySelector(\":scope > a\").classList.toggle(\"in-view\", on)" script)
            "the entries a marked one is under are marked too, so a folded section leaves its own lit")
      (true (search "var line = window.scrollY + window.innerHeight / 2;" script)
            "a heading counts as read from the middle of the window"))))

(define-test (html-export the-manual-as-one-document)
  (with-test-directory (dir)
    (manual-tree dir)
    (funcall (manual-tool "run-pdf") dir)
    (let ((latex (alexandria:read-file-into-string (merge-pathnames "build/manual/tuatara-manual.tex" dir) :external-format :utf-8)))
      (true (search "\\section{One}" latex))
      (true (search "\\section{Two}" latex))
      (true (search "\\begin{try}" latex))
      (false (search "02-two.org" latex) "a link between chapters is a link inside the document")
      (true (search "\\hyperref" latex))
      (false (search "Previous:" latex)))))

(defun html-export-names (text file)
  "Where the web page, Word and LaTeX exports of the Org document TEXT, of FILE, say to write it."
  (list (nth-value 1 (tuatara.defaults::org-html-document text :file file))
        (nth-value 1 (tuatara.defaults::org-docx-document text :file file))
        (nth-value 1 (tuatara.defaults::org-latex-document text :file file))))

(define-test (html-export output-name-from-a-macro-is-not-taken)
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "note.org" dir)))
          (text (format nil "#+MACRO: m #+EXPORT_FILE_NAME: ~aelsewhere/index~%~%{{{m}}}~%~%Words.~%"
                        (namestring dir))))
      (false (tuatara.defaults::org-export-file-name text file) "the #+ lines, which are checked, name nothing")
      (dolist (name (html-export-names text file))
        (false name)))))

(define-test (html-export output-name-is-the-one-checked)
  (with-test-directory (dir)
    (let* ((file (namestring (merge-pathnames "note.org" dir)))
           (text (format nil "#+EXPORT_FILE_NAME: ~aelsewhere/index~%\\begin{a}~%#+EXPORT_FILE_NAME: ok~%\\end{a}~%"
                         (namestring dir)))
           (checked (tuatara.defaults::org-export-file-name text file)))
      (true (uiop:string-suffix-p checked "ok.tex"))
      (loop for name in (html-export-names text file)
            for type in '("html" "docx" "tex")
            do (is string= (tuatara.defaults::replaced-type checked type) name)))))

(define-test (html-export contents-for-the-first-toc-line-alone)
  (multiple-value-bind (html notes)
      (html-of (format nil "#+options: toc:nil~%~{~a~}~%* One~%~%* Two~%"
                       (loop repeat 50 collect (format nil "#+TOC: headlines~%"))))
    (is = 1 (length (cl-ppcre:all-matches-as-strings "<nav class=\"contents\">" html)))
    (is = 1 (count-if (lambda (note) (uiop:string-prefix-p "A repeated #+TOC:" note)) notes))))

(define-test (html-export table-cells-read-once-for-alignment)
  (let ((calls 0) (body nil))
    (sb-int:encapsulate 'tuatara.defaults::org-export-cell-text 'cells-read
                        (lambda (function &rest arguments)
                          (incf calls)
                          (apply function arguments)))
    (unwind-protect
         (setf body (html-body-of (format nil "~{~a~}" (loop repeat 300 collect (format nil "| a | 1 | b |~%")))))
      (sb-int:unencapsulate 'tuatara.defaults::org-export-cell-text 'cells-read))
    (true (<= calls (* 5 300 3)) "each cell read a few times, not once for every cell written")
    (true (search "<td class=\"right\">1</td>" body))))

(define-test (html-export ids-for-many-places-of-one-name)
  (let ((tuatara.defaults::*org-html* (tuatara.defaults::make-org-html)))
    (multiple-value-bind (ids seconds)
        (measuring (loop repeat 10000 collect (tuatara.defaults::org-html-new-id "Same words")))
      (true (< seconds 2) "the next number of a name known, not searched for")
      (is string= "same-words" (first ids))
      (is string= "same-words-2" (second ids))
      (is string= "same-words-10000" (first (last ids)))
      (is = 10000 (hash-table-count (tuatara.defaults::org-html-ids tuatara.defaults::*org-html*)))))
  (let ((tuatara.defaults::*org-html* (tuatara.defaults::make-org-html)))
    (is equal '("a" "a-2" "a-2-2" "a-3")
        (mapcar #'tuatara.defaults::org-html-new-id '("a" "a" "a-2" "a")))))

(define-test (html-export footnotes-numbered-at-once)
  (tuatara.defaults::with-org-note-keywords ("" nil)
    (tuatara.defaults::org-export-read-options tuatara.defaults::*org-export*)
    (let* ((tuatara.defaults::*org-html* (tuatara.defaults::make-org-html))
           (out (make-string-output-stream))
           (labels (loop for n below 30000 collect (format nil "n~d" n))))
      (true (< (nth-value 1 (measuring
                              (dolist (label labels)
                                (tuatara.defaults::org-html-footnote
                                 (tuatara.defaults::make-org-node 'tuatara.defaults::footnote-reference
                                                                  :begin 0 :properties (list :label label))
                                 out))))
               1)
            "a label's number looked up, not searched for")
      (tuatara.defaults::org-html-footnote
       (tuatara.defaults::make-org-node 'tuatara.defaults::footnote-reference :begin 0 :properties (list :label "n7"))
       out)
      (let ((written (get-output-stream-string out)))
        (true (search "<a href=\"#fn-30000\">30000</a>" written))
        (true (uiop:string-suffix-p written "<a href=\"#fn-8\">8</a></sup>"))))))

(define-test (html-export policy-allows-no-data-addresses)
  (let ((html (tuatara.defaults::org-html-page "T" "<p></p>" :style "p {}")))
    (true (search "img-src 'self' file:" html))
    (false (search "data:" html))))

(define-test (html-export picture-on-a-page-written-elsewhere)
  (with-test-directory (dir)
    (let* ((directory (merge-pathnames "notes/" dir))
           (file (namestring (merge-pathnames "note.org" directory)))
           (picture (merge-pathnames "pic.png" directory)))
      (ensure-directories-exist picture)
      (alexandria:write-string-into-file "x" picture)
      (flet ((page (name) (html-parts-of (format nil "#+export_file_name: ~a~%~%[[file:pic.png]]~%" name) :file file)))
        (true (search "<img src=\"pic.png\"" (getf (page "beside") :body)) "beside its note, the picture beside it")
        (let ((parts (page "../out/index")))
          (false (search "<img" (getf parts :body)) "a relative address would be read from the other directory")
          (true (search "<code>pic.png</code>" (getf parts :body)))
          (true (member "pic.png: a page written away from its note" (getf parts :notes)
                        :test #'uiop:string-prefix-p)))))))

(define-test (html-export cells-wrap-to-the-page)
  ;; a table stays as wide as the page: long keys and names in its cells wrap, so its last
  ;; column is never pushed out of sight at a large font
  (let ((style (uiop:read-file-string (merge-pathnames "data/org-html.css" tuatara.runtime:*source-tree*))))
    (true (search "td { overflow-wrap: anywhere; }" style))
    (true (search "td kbd, td code { white-space: normal; }" style))))

(define-test (html-export every-listed-setting-explained-in-its-chapter)
  ;; a setting a chapter lists in a settings table is also named in the chapter's text, where its
  ;; effect and an example are written
  (let ((missing '()))
    (dolist (file (directory (merge-pathnames "doc/manual/[0-9][0-9]-*.org" tuatara.runtime:*source-tree*)))
      (let ((listed '()) (text '()))
        (dolist (line (uiop:read-file-lines file))
          (if (uiop:string-prefix-p "#+table:" line)
              (when (uiop:string-prefix-p "#+table: settings " line)
                (setf listed (append listed (cl-ppcre:split " +" (subseq line (length "#+table: settings "))))))
              (push line text)))
        (let ((prose (format nil "~{~a~^~%~}" text)))
          (dolist (name listed)
            (unless (search name prose)
              (push (format nil "~a: ~a" (pathname-name file) name) missing))))))
    (is equal '() (sort missing #'string<) "settings listed in a table but not named in the chapter's text")))

(define-test (html-export toc-lists-left-out-said)
  ;; a #+TOC: of tables or figures, which a web page does not make, is said to be left out
  (let ((notes (getf (html-parts-of (format nil "#+TOC: tables~%~%* Head~%Text~%")) :notes)))
    (true (find-if (lambda (n) (search "#+TOC: tables line left out" n)) notes) "~s" notes)))

(defun manual-tuatara-written ()
  "What the first column of the manual's table \"What Tuatara adds to Org\"
writes between = signs, each as written (=#+template: NAME= gives \"#+template:
NAME\"), in order."
  (let* ((manual (merge-pathnames "doc/manual/14-org.org" tuatara.runtime:*source-tree*))
         (lines (uiop:read-file-lines manual))
         (start (position "*** What Tuatara adds to Org" lines :test #'string=))
         (end (position-if (lambda (line) (uiop:string-prefix-p "*** " line)) lines :start (1+ start)))
         (written '()))
    (dolist (line (subseq lines start end))
      (when (uiop:string-prefix-p "| =" line)
        (let ((cell (string-trim " " (second (cl-ppcre:split "\\|" line)))))
          (cl-ppcre:do-register-groups (shown) ("=([^=]+)=" cell)
            (push shown written)))))
    (nreverse written)))

(defun manual-tuatara-names ()
  "The names the first column of the manual's table \"What Tuatara adds to
Org\" gives (MANUAL-TUATARA-WRITTEN), each once: the word in front of the first
space, colon or bracket, without its #+ or its colons."
  (let ((names '()))
    (dolist (shown (manual-tuatara-written))
      (let ((name (cl-ppcre:regex-replace "_[0-9].*$" (first (cl-ppcre:split "[ :(]" (string-left-trim "{#+:" shown)))
                                          ""))) ; :TOC_3_gh: names the :TOC: tag
        (when (> (length name) 2) (pushnew name names :test #'string=))))
    (nreverse names)))

(define-test (html-export names-only-tuatara-acts-on-are-in-the-source)
  ;; each name the manual's table "What Tuatara adds to Org" lists is found in the source: a name
  ;; renamed or dropped there is changed in the manual as well
  (let* ((names (manual-tuatara-names))
         (sources (format nil "~{~a~%~}"
                          (loop for pattern in '("lisp/**/*.lisp" "src/**/*.lisp")
                                append (loop for file in (directory (merge-pathnames pattern tuatara.runtime:*source-tree*))
                                             collect (alexandria:read-file-into-string file :external-format :utf-8)))))
         (missing '()))
    (true names "the table names something")
    (dolist (name names)
      (unless (search name sources :test #'char-equal) (push name missing)))
    (is equal '() (sort missing #'string<) "names in the manual's table but not in the source")))

(define-test (html-export the-org-chapter-of-the-manual-exports)
  ;; the chapter that tells of the redact and protect macros writes none of their calls, which an export
  ;; refuses where Org reads no macro, so that it is made as a page
  (let* ((file (merge-pathnames "doc/manual/14-org.org" tuatara.runtime:*source-tree*))
         (body (html-body-of (alexandria:read-file-into-string file :external-format :utf-8))))
    (true (search "Org differs" body))))
