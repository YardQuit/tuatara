;;;; lisp/org/latex-languages.lisp --- The language an Org note says it is written in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; #+LANGUAGE: sv in a note makes its PDF hyphenate, name its contents and
;;;; captions, and its Word document name them, in Swedish.  One table
;;;; (*EXPORT-LANGUAGES*) gives, for each language code, its name for
;;;; polyglossia (xelatex, lualatex) and for babel (pdflatex), the TeX Live
;;;; hyphenation patterns it needs, and the words Word's export prints.

(in-package #:tuatara.defaults)

(defparameter *export-languages*
  '(;; code  name   polyglossia   babel         patterns       TeX Live package    Table      Figure    Contents
    ("en"    "English"               "english"      "english"      "en-us"   "hyphen-english"    "Table"    "Figure"  "Contents")
    ("en-gb" "British English"       "british"      "british"      "en-gb"   "hyphen-english"    "Table"    "Figure"  "Contents")
    ("en-us" "American English"      "american"     "american"     "en-us"   "hyphen-english"    "Table"    "Figure"  "Contents")
    ("en-au" "Australian English"    "australian"   "australian"   "en-gb"   "hyphen-english"    "Table"    "Figure"  "Contents")
    ("en-nz" "New Zealand English"   "newzealand"   "newzealand"   "en-gb"   "hyphen-english"    "Table"    "Figure"  "Contents")
    ("en-ca" "Canadian English"      "canadian"     "canadian"     "en-us"   "hyphen-english"    "Table"    "Figure"  "Contents")
    ("sv"    "Swedish"               "swedish"      "swedish"      "sv"      "hyphen-swedish"    "Tabell"   "Figur"   "Innehåll")
    ("de"    "German"                "german"       "ngerman"      "de-1996" "hyphen-german"     "Tabelle"  "Abbildung" "Inhalt")
    ("de-at" "Austrian German"       "austrian"     "naustrian"    "de-1996" "hyphen-german"     "Tabelle"  "Abbildung" "Inhalt")
    ("de-ch" "Swiss German"          "german"       "ngerman"      "de-1996" "hyphen-german"     "Tabelle"  "Abbildung" "Inhalt")
    ("fr"    "French"                "french"       "french"       "fr"      "hyphen-french"     "Tableau"  "Figure"  "Table des matières")
    ("nb"    "Norwegian (Bokmål)"    "norwegian"    "norsk"        "nb"      "hyphen-norwegian"  "Tabell"   "Figur"   "Innhold")
    ("no"    "Norwegian (Bokmål)"    "norwegian"    "norsk"        "nb"      "hyphen-norwegian"  "Tabell"   "Figur"   "Innhold")
    ("nn"    "Norwegian (Nynorsk)"   "nynorsk"      "nynorsk"      "nn"      "hyphen-norwegian"  "Tabell"   "Figur"   "Innhald")
    ("da"    "Danish"                "danish"       "danish"       "da"      "hyphen-danish"     "Tabel"    "Figur"   "Indhold")
    ("fi"    "Finnish"               "finnish"      "finnish"      "fi"      "hyphen-finnish"    "Taulukko" "Kuva"    "Sisällys")
    ("es"    "Spanish"               "spanish"      "spanish"      "es"      "hyphen-spanish"    "Tabla"    "Figura"  "Índice")
    ("it"    "Italian"               "italian"      "italian"      "it"      "hyphen-italian"    "Tabella"  "Figura"  "Indice")
    ("nl"    "Dutch"                 "dutch"        "dutch"        "nl"      "hyphen-dutch"      "Tabel"    "Figuur"  "Inhoud")
    ("pt"    "Portuguese"            "portuguese"   "portuguese"   "pt"      "hyphen-portuguese" "Tabela"   "Figura"  "Índice")
    ("pt-br" "Brazilian Portuguese"  "brazil"       "brazilian"    "pt"      "hyphen-portuguese" "Tabela"   "Figura"  "Sumário")
    ("pl"    "Polish"                "polish"       "polish"       "pl"      "hyphen-polish"     "Tabela"   "Rysunek" "Spis treści")
    ("cs"    "Czech"                 "czech"        "czech"        "cs"      "hyphen-czech"      "Tabulka"  "Obrázek" "Obsah")
    ("is"    "Icelandic"             "icelandic"    "icelandic"    "is"      "hyphen-icelandic"  "Tafla"    "Mynd"    "Efnisyfirlit")
    ("et"    "Estonian"              "estonian"     "estonian"     "et"      "hyphen-estonian"   "Tabel"    "Joonis"  "Sisukord")
    ("hu"    "Hungarian"             "hungarian"    "magyar"       "hu"      "hyphen-hungarian"  "Táblázat" "Ábra"    "Tartalomjegyzék")
    ("tr"    "Turkish"               "turkish"      "turkish"      "tr"      "hyphen-turkish"    "Tablo"    "Şekil"   "İçindekiler")
    ("ca"    "Catalan"               "catalan"      "catalan"      "ca"      "hyphen-catalan"    "Taula"    "Figura"  "Taula de continguts"))
  "The languages a note's #+LANGUAGE: may name, each (CODE NAME POLYGLOSSIA
BABEL PATTERNS PACKAGE TABLE FIGURE CONTENTS): the ISO code, the language's
name, its name for polyglossia and for babel (NIL where babel cannot set it
with the fonts pdflatex has), the ending of the hyphenation patterns' file
\(loadhyph-PATTERNS.tex) and the TeX Live package holding it, and the words
Word's export prints for a table, a picture and the contents.")

(defun export-language (code)
  "The row of *EXPORT-LANGUAGES* for CODE, a language code as #+LANGUAGE: gives
it (sv, en-GB, pt_BR; the language alone when its country has no row of its
own); NIL when CODE is none of them."
  (let* ((code (string-downcase (substitute #\- #\_ (string-trim " " (or code "")))))
         (dash (position #\- code)))
    (or (assoc code *export-languages* :test #'string=)
        (and dash (assoc (subseq code 0 dash) *export-languages* :test #'string=)))))

(defun export-language-code ()
  "The code the note being written gives for its language (#+LANGUAGE:), or
NIL when it gives none."
  (let ((code (and *org-latex* (org-latex-keyword "LANGUAGE"))))
    (and code (plusp (length (string-trim " " code))) (string-trim " " code))))

(defun latex-loads-language-p (&rest texts)
  "True when one of TEXTS, LaTeX, already loads polyglossia or babel or sets the
document's language."
  (some (lambda (text)
          (cl-ppcre:scan "\\\\(?:usepackage|RequirePackage)(?:\\[[^]]*\\])?\\{[^}]*\\b(?:polyglossia|babel)\\b|\\\\(?:setmainlanguage|selectlanguage|babelprovide)\\b"
                         text))
        texts))

(defun org-latex-language-setup (engine)
  "The lines that set the language the note being written says it is in, for the
LaTeX program ENGINE: polyglossia's for xelatex and lualatex, babel's for the
rest. Empty when the note says none, or its template or #+LATEX_HEADER lines
set a language themselves; a code not in *EXPORT-LANGUAGES* is said so once
\(EXPORT-NOTE) and sets none."
  (let ((code (export-language-code)))
    (cond
      ((null code) "")
      ((latex-loads-language-p *org-latex-template*
                               (format nil "~{~a~%~}" (append (org-latex-keyword-all "LATEX_HEADER")
                                                              (org-latex-keyword-all "LATEX_HEADER_EXTRA")))
                               (or (org-latex-keyword "LATEX_CLASS_OPTIONS") ""))
       "")
      (t
       (let ((row (export-language code))
             (modern (member engine '("xelatex" "lualatex") :test #'string-equal)))
         (cond
           ((null row)
            (export-note "#+language: ~a is not a language the export knows; the document keeps LaTeX's own (~{~a~^, ~})"
                         code (mapcar #'first *export-languages*))
            "")
           (modern (format nil "\\usepackage{polyglossia}~%\\setmainlanguage{~a}~%" (third row)))
           ((fourth row) (format nil "\\usepackage[~a]{babel}~%" (fourth row)))
           (t (export-note "~a cannot be set by ~a; the document keeps LaTeX's own language (#+latex_compiler: xelatex)"
                           (second row) engine)
              "")))))))

(defun org-latex-language-row ()
  "The row of *EXPORT-LANGUAGES* for the note being written when it
names a language the export knows; else NIL."
  (let ((code (export-language-code)))
    (and code (export-language code))))

(defun export-language-word (which)
  "The word the Word export prints for WHICH, :TABLE, :FIGURE or :CONTENTS, in
the language the note being written says it is in; English when it says none or
one the export does not know."
  (let ((row (or (org-latex-language-row) (export-language "en"))))
    (ecase which
      (:table (seventh row))
      (:figure (eighth row))
      (:contents (ninth row)))))

(defparameter *english-quote-marks* '("“" "”" "‘" "’")
  "The quotation marks of English: opening and closing double, opening and
closing single.")

(defparameter *export-quote-marks*
  '(("sv" "”" "”" "’" "’")
    ("fi" "”" "”" "’" "’")
    ("de" "„" "“" "‚" "‘")
    ("fr" "«" "»" "‹" "›"))
  "How the enhanced export writes quotation marks for a language, by the
language alone: (CODE OPENING-DOUBLE CLOSING-DOUBLE OPENING-SINGLE
CLOSING-SINGLE). A language not here, or a note with none, gets
*ENGLISH-QUOTE-MARKS*.")

(defun export-quote-marks ()
  "The four quotation marks (opening double, closing double, opening single,
closing single) for the note being written, by its #+LANGUAGE:."
  (let* ((code (or (export-language-code) "en"))
         (language (string-downcase (subseq code 0 (position-if (lambda (c) (find c "-_")) code)))))
    (or (rest (assoc language *export-quote-marks* :test #'string=)) *english-quote-marks*)))
