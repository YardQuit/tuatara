;;;; lisp/org/keywords.lisp --- The #+KEYWORD lines an Org note may hold that Tuatara reads, in plain words.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; *ORG-KEYWORD-SETTINGS* lists every keyword line Tuatara acts on, with what
;;;; it does and whether Tuatara alone reads it; *ORG-OPTION-SETTINGS* does the
;;;; same for the words of #+OPTIONS:, and *ORG-STARTUP-WORDS* (fontify.lisp)
;;;; for those of #+STARTUP:. M-x describe-org-keyword-settings shows them.

(in-package #:tuatara.defaults)

(defparameter *org-keyword-settings*
  '(("#+TITLE: text" nil "the document's title, printed above it")
    ("#+SUBTITLE: text" nil "printed under the title")
    ("#+AUTHOR: name" nil "printed under the title unless #+OPTIONS: author:nil")
    ("#+DATE: text" nil "printed under the title unless #+OPTIONS: date:nil")
    ("#+DESCRIPTION: text" nil "the subject of a PDF's properties")
    ("#+KEYWORDS: words" nil "the keywords of a PDF's properties")
    ("#+LANGUAGE: en" nil "the language code of the document (en, de-DE): its hyphenation, and the language the
     Word document and web page say")
    ("#+EXPORT_FILE_NAME: name" nil "the name the export is written to, in place of the note's name")
    ("#+SETUPFILE: file" nil
     "the lines of another file are read in this one's place; from the note's directory or one of
      *EXPORT-TRUSTED-DIRECTORIES* only")
    ("#+MACRO: name text" nil "defines {{{name}}}; $1 to $9 in the text stand for the arguments")
    ("#+OPTIONS: word:value ..." nil "see the options below")
    ("#+STARTUP: word ..." nil "see the words below; the last word of each kind wins")
    ("#+TOC: headlines 2" nil "a table of contents here, 2 levels deep; tables or figures give a list of those")
    ("#+LATEX: text" nil "a line of LaTeX in the PDF; the web page and Word document leave it out")
    ("#+HTML: text" nil "a line of HTML Tuatara's web page leaves out, and says so")
    ("#+LATEX_CLASS: article" nil "the LaTeX document class of the PDF")
    ("#+LATEX_CLASS_OPTIONS: [11pt]" nil "the options of the document class")
    ("#+LATEX_HEADER: text" nil "a line added to the LaTeX preamble; read for formulas shown as pictures only in a
     trusted project")
    ("#+LATEX_HEADER_EXTRA: text" nil "the same, after those")
    ("#+LATEX_COMPILER: xelatex" nil "the program that makes the PDF")
    ("#+CITE_EXPORT: biblatex" nil "the way citations are made: biblatex, natbib or bibtex, then the bibliography
     style and the citation style")
    ("#+BIBLIOGRAPHY: file.bib" nil "a bibliography file; a name LaTeX cannot take is left out, and the export says so")
    ("#+PRINT_BIBLIOGRAPHY:" nil "the bibliography printed here")
    ("#+TEMPLATE: name" t "the template that gives a PDF or Word document its look")
    ("#+LATEX_TEMPLATE: name" t "an older name for #+TEMPLATE:")
    ("#+LATEX_STYLE: name" t "an older name for #+TEMPLATE:")
    ("#+LOGO: file" t "the picture a template's logo takes, from *LOGOS-DIRECTORY*")
    ("#+KEY: value" t "any other line a template names: its value is put where the template says")
    ("#+SELECT_TAGS: export" nil "only the headings with these tags are exported, with what is above and under them")
    ("#+EXCLUDE_TAGS: noexport" nil "the headings with these tags are left out; noexport is standard")
    ("#+EXPORT_SELECT_TAGS: tag" t "read as #+SELECT_TAGS: is")
    ("#+EXPORT_EXCLUDE_TAGS: tag" t "read as #+EXCLUDE_TAGS: is")
    ("#+TODO: TODO NEXT | DONE" nil
     "the keywords of tasks: those before the bar are open, those after it done. #+SEQ_TODO: and #+TYP_TODO: are
      read alike, and all the lines make one set; (t) and (t@/!) after a word are dropped")
    ("#+FILETAGS: :a:b:" nil "tags every heading of the note has, for the agenda and its matches")
    ("#+CATEGORY: name" nil "the category the agenda shows for the note's entries; the file's name when there is none")
    ("#+ARCHIVE: %s_archive::" nil "where the clock table looks for archived entries; %s stands for the file's name")
    ("#+PROPERTY: name value" nil "a property every heading of the note has")
    ("#+CAPTION: text" nil "the caption of the table, picture or block after it; #+NAME: names it for links")
    ("#+ATTR_ORG: :width 50%" nil "sizes and places the picture after it; see M-x describe-inline-picture-settings")
    ("#+TBLFM: $3=$1+$2" nil "the formulas of the table above it; see M-x describe-table-formula-settings")
    ("#+BEGIN: name" nil "a dynamic block, up to #+END:; the lists of formatted names and the clock table are such
     blocks"))
  "The keyword lines of an Org note that Tuatara acts on, each as it is
written, true when only Tuatara reads it (other Org programs ignore it), and
what it does.")

(defparameter *org-option-settings*
  '(("title:nil" "leave the title out of the export; the document class's title page too")
    ("author:nil" "leave the author out")
    ("date:nil" "leave the date out")
    ("toc:nil, toc:2" "no table of contents, or one only that many levels deep")
    ("num:nil" "no numbers before the headings")
    ("H:3" "how many levels of headings are sections; those under are lists")
    ("tags:nil" "do not print the headings' tags")
    ("todo:nil" "do not print the TODO keywords of headings")
    ("pri:t" "print the priorities of headings")
    ("d:(not \"EXPORT\" \"LOGBOOK\")" "which drawers print: t all but LOGBOOK, nil none, (not \"A\") all but those,
     (\"A\") only those")
    ("arch:t" "export archived headings too; headline exports their titles only")
    ("f:nil" "leave the footnotes out")
    ("e:nil" "print entities (\\alpha) as written")
    ("-:nil" "keep -- and --- and ... as written, not as dashes and an ellipsis")
    ("<:nil" "leave the timestamps out")
    ("\\n:t" "keep the line breaks of a paragraph")
    ("^:nil, ^:{}" "no sub and superscripts, or only those in braces: x_{2}")
    ("redact:nil" "print what redact(...) hides")
    ("protect:t" "hide what protect(...) holds"))
  "The words of an Org note's #+OPTIONS: line that Tuatara reads, each as it
is written and what it does. A word is NAME:VALUE, a value one word or a list
in parentheses.")

(define-command describe-org-keyword-settings ()
  "Show every #+KEYWORD line an Org note may hold that Tuatara reads, and the
words of #+OPTIONS: and #+STARTUP: lines: what each does, and which are
Tuatara's own, which other programs that read Org notes ignore. The lines
are filled to *HELP-COLUMN*."
  (show-help
   (with-output-to-string (s)
     (write-help-section s "Keyword lines of an Org note"
                         :intro "A line #+KEYWORD: value near the top of a note changes how it is shown and
                         exported. The name may be written in any case. Lines Tuatara does not read stay as
                         they are, and other programs may read them. #+TAGS: and #+PRIORITIES: are not read;
                         tags and priorities are those written on the headings."
                         :example "#+TITLE: Report
#+STARTUP: indent inlineimages
#+TODO: TODO NEXT | DONE
#+OPTIONS: toc:2 num:nil")
     (write-help-section s "The lines"
                         :rows (loop for (written own words) in *org-keyword-settings*
                                     append (list written (if own (format nil "(Tuatara's own) ~a" words) words)))
                         :width 30)
     (write-help-section s "Words of #+OPTIONS:"
                         :intro "Each is NAME:VALUE, apart by spaces. Not every export reads every one."
                         :rows (loop for (written words) in *org-option-settings* append (list written words))
                         :width 28)
     (write-help-section s "Words of #+STARTUP:"
                         :rows (loop for (word nil words) in *org-startup-words* append (list word words))
                         :width 18)
     (write-help-section s "Only Tuatara reads"
                         :intro "The lines marked (Tuatara's own), the :EXPORT: drawer
                         (\\[describe-export-settings]), the macros redact and protect, the options redact and
                         protect, the :appendix: and :TOC: tags, the NOTIFY_BEFORE property, #+ATTR_ORG: :float
                         and the startup words inlineimages and latexpreview (shown only when
                         *ORG-STARTUP-SHOWS-PICTURES* is t). Org's own programs ignore them or, for the macros,
                         stop with an error; the manual's chapter on Org says what each does there."))))
