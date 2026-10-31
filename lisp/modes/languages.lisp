;;;; lisp/modes/languages.lisp --- Programming languages parsed with tree-sitter.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A language joins a tree-sitter grammar to the major modes that edit it and
;;;; to the rules that colour it: Helix's, in syntax/rules/ of the source, read
;;;; into the program when it is built. Two styles colour with those rules:
;;;; rich, the default, which colours everything they name, as Helix does, and
;;;; sparse, which colours only some kinds of thing (comments and strings;
;;;; keywords and functions; types and constants), fewer or more as its level
;;;; says. (set-colouring 'rust :sparse) in the init file chooses a style for a
;;;; language, or for the language of a major mode ('rust-mode), and M-x
;;;; rotate-colouring goes from rich to sparse to none and round again. CSV and
;;;; TSV, coloured by csv.lisp without a grammar, take the same styles. A file
;;;; visited safely is never parsed.

(in-package #:tuatara.defaults)

(defun syntax-directory (kind)
  "The directory holding the grammars' rules of KIND (\"rules\" for colouring,
\"indents\", \"textobjects\", \"injections\", \"locals\", \"tags\"): in syntax/
beside the program, as a release has them, else the source tree's; where
it would be beside the program when neither is there."
  (let ((name (format nil "syntax/~a/" kind)))
    (or (first (tuatara.runtime:resource-directories name name :existing t))
        (first (tuatara.runtime:resource-directories name name)))))

(defparameter *query-inherits* (cl-ppcre:create-scanner ";+\\s*inherits\\s*:?\\s*([a-z_,()-]+)\\s*")
  "What a rules file says to take in another's rules of the same kind, a
comment such as ; inherits: ecma,_javascript.")

(defparameter *rules-largest* (* 1024 1024)
  "The largest set of rules read, in characters: a rules file larger, or one
whose ; inherits: lines make it larger, is not used, and that is said.")

(defun read-query (directory name &optional seen)
  "The text of the rules NAME.scm in DIRECTORY, or NIL when there is none;
each ; inherits: A,B in it replaced by the rules A and B of DIRECTORY, read
the same way. Rules larger than *RULES-LARGEST*, as a file or once what
they inherit is put in, are not used: NIL, and said."
  (let ((path (probe-file (merge-pathnames (format nil "~a.scm" name) directory))))
    (when path
      (let ((size (ignore-errors (with-open-file (in path :element-type '(unsigned-byte 8)) (file-length in)))))
        (if (or (null size) (> size *rules-largest*))
            (progn (rules-failed "The rules ~a are larger than ~:d characters: not used" (namestring path) *rules-largest*)
                   nil)
            (let ((text (cl-ppcre:regex-replace-all
                         *query-inherits* (uiop:read-file-string path :external-format :utf-8)
                         (lambda (match names)
                           (declare (ignore match))
                           (format nil "~{~%~a~%~}"
                                   (loop for other in (uiop:split-string names :separator ",")
                                         unless (member other (cons name seen) :test #'string=)
                                           collect (or (read-query directory other (cons name seen)) ""))))
                         :simple-calls t)))
              (if (> (length text) *rules-largest*)
                  (progn (rules-failed "The rules ~a, with those they take in, are larger than ~:d characters: not used"
                                       (namestring path) *rules-largest*)
                         nil)
                  text)))))))

(defun read-kind-query (kind name)
  "The text of the rules of KIND (as SYNTAX-DIRECTORY names kinds) called NAME:
Tuatara's own, syntax/KIND/NAME.scm, else those you added, KIND/NAME.scm in
the folder of grammars you add; NIL when there are none. Rules you added
for one of Tuatara's grammars are said as they are taken."
  (or (read-query (syntax-directory kind) name)
      (let ((text (read-query (merge-pathnames (format nil "~a/" kind) (tuatara.syntax:user-grammar-directory)) name)))
        (when (and text (not (tuatara.syntax:user-grammar-p name)))
          (message "Rules you added are used for ~a: ~a/~a.scm in ~a" name kind name
                        (abbreviated-path (namestring (tuatara.syntax:user-grammar-directory)))))
        text)))

(defun read-rules (name)
  "The text of the colouring rules NAME (a grammar's or a language's name), from
syntax/rules/NAME.scm (READ-KIND-QUERY), or NIL when there are none."
  (read-kind-query "rules" name))

(defun read-indents (name)
  "The text of the indentation rules for NAME, from indents/NAME.scm (READ-KIND-QUERY), or NIL when there are none."
  (read-kind-query "indents" name))

(defun read-textobjects (name)
  "The text of the rules for selecting functions, types, parameters and
comments in NAME, from textobjects/NAME.scm (READ-KIND-QUERY), or NIL when there are none."
  (read-kind-query "textobjects" name))

(defun read-injections (name)
  "The text of the rules for text in other languages in NAME, from
injections/NAME.scm (READ-KIND-QUERY), or NIL when there are none."
  (read-kind-query "injections" name))

(defun read-locals (name)
  "The text of the rules for local names in NAME, from locals/NAME.scm
\(READ-KIND-QUERY), or NIL when there are none."
  (read-kind-query "locals" name))

(defun read-tags (name)
  "The text of the rules naming the definitions and headings in NAME, from
tags/NAME.scm (READ-KIND-QUERY), or NIL when there are none."
  (read-kind-query "tags" name))

(defstruct (language (:constructor make-language (name doc grammar modes rules indents indent textobjects
                                                  injections injection-names locals definitions tags))
                     (:copier nil))
  "A language, of code or of text: its NAME (a symbol such as RUST), DOC, the GRAMMAR
it is parsed with (the name of its grammar file), the MODES that edit it, its
colouring RULES (their text), the STYLE set for it (:RICH, :SPARSE or :NONE;
NIL for the one set for its modes, LANGUAGE-COLOURING), the LEVEL set for its
sparse style (NIL for 3), and QUERY, the rules compiled when first
needed (:FAILED when they could not be); its indentation rules, INDENTS (their
text), the columns of one level of INDENT, and INDENT-QUERY, those rules
compiled when first needed; the rules for selecting its functions, types,
parameters and comments, TEXTOBJECTS (their text), with TEXTOBJECT-QUERY,
those compiled when first needed; and its rules for text in other languages,
INJECTIONS, with INJECTION-QUERY, and INJECTION-NAMES, a regular expression
for the names other languages' rules call it by; its rules for local names,
LOCALS, with LOCALS-QUERY; DEFINITIONS, the rules naming what
\\[beginning-of-defun] and \\[end-of-defun] move between, with DEFINITIONS-QUERY;
and TAGS, the rules naming its definitions and headings for \\[buffer-outline],
with TAGS-QUERY."
  (name nil :read-only t)
  (doc "" :read-only t)
  (grammar "" :read-only t)
  (modes '() :read-only t)
  (rules nil :read-only t)
  (style nil)
  (level nil)
  (query nil)
  (indents nil :read-only t)
  (indent 4)
  (indent-query nil)
  (textobjects nil :read-only t)
  (textobject-query nil)
  (injections nil :read-only t)
  (injection-names nil :read-only t)
  (injection-query nil)
  (locals nil :read-only t)
  (locals-query nil)
  (definitions nil :read-only t)
  (definitions-query nil)
  (tags nil :read-only t)
  (tags-query nil))

(setf (documentation 'make-language 'function) "A LANGUAGE from its parts; see DEFINE-LANGUAGE."
      (documentation 'language-p 'function) "True when OBJECT is a LANGUAGE."
      (documentation 'language-name 'function) "The language's name, a symbol."
      (documentation 'language-doc 'function) "What the language is."
      (documentation 'language-grammar 'function) "The name of the grammar the language is parsed with."
      (documentation 'language-modes 'function) "The major modes that edit the language."
      (documentation 'language-rules 'function) "The text of the language's colouring rules, or NIL."
      (documentation 'language-style 'function) "How the language is set to be coloured: :RICH, :SPARSE, :NONE, or NIL for as its modes are."
      (documentation 'language-level 'function) "How much the sparse style is set to colour, from 1 (least) to 4 (all, as rich), or NIL for 3."
      (documentation 'language-query 'function) "The compiled colouring rules, :FAILED, or NIL before they are needed."
      (documentation 'language-indents 'function) "The text of the language's indentation rules, or NIL."
      (documentation 'language-indent 'function) "The columns of one level of indentation."
      (documentation 'language-indent-query 'function) "The compiled indentation rules, :FAILED, or NIL before they are needed."
      (documentation 'language-textobjects 'function) "The text of the rules for selecting the language's functions, types, parameters and comments, or NIL."
      (documentation 'language-textobject-query 'function) "Those rules compiled, :FAILED, or NIL before they are needed."
      (documentation 'language-injections 'function) "The text of the language's rules for text in other languages, or NIL."
      (documentation 'language-injection-names 'function) "A regular expression for the names other languages' rules call this one by, or NIL."
      (documentation 'language-injection-query 'function) "The rules for text in other languages compiled, :FAILED, or NIL before they are needed."
      (documentation 'language-locals 'function) "The text of the language's rules for local names, or NIL."
      (documentation 'language-locals-query 'function) "The rules for local names compiled, :FAILED, or NIL before they are needed."
      (documentation 'language-definitions 'function) "The text of the rules naming the language's definitions, each captured as @definition, or NIL."
      (documentation 'language-definitions-query 'function) "Those rules compiled, :FAILED, or NIL before they are needed."
      (documentation 'language-tags 'function) "The text of the rules naming the language's definitions and headings, or NIL."
      (documentation 'language-tags-query 'function) "The rules naming definitions and headings compiled, :FAILED, or NIL before they are needed.")

(defvar *languages* '()
  "Every language, the most recently defined first.")

(defvar *injection-languages* (make-hash-table :test 'equalp :synchronized t)
  "The language each name that rules for text in other languages have used
means, as found so far (:NONE for none); emptied when a language is defined.")

(defun find-language (name)
  "The language called NAME (a symbol from any package, compared by name), or NIL."
  (find (string name) *languages* :key (lambda (l) (symbol-name (language-name l))) :test #'string-equal))

(defun textobject-definitions (textobjects)
  "Rules naming definitions made from TEXTOBJECTS, the text of a language's
selection rules: its functions and types (@function.around, @class.around),
each captured as @definition."
  (and textobjects
       (search "around" textobjects)
       (cl-ppcre:regex-replace-all "@(function|class)\\.around\\b" textobjects "@definition")))

(defun own-language-name-p (name)
  "True when NAME, a language's name, is one Tuatara's own definitions make:
a symbol of its defaults' package."
  (and (symbolp name) (eq (symbol-package name) (find-package '#:tuatara.defaults))))

(defun own-language-p (language)
  "True when LANGUAGE is one of Tuatara's own."
  (own-language-name-p (language-name language)))

(defun register-language (name doc grammar modes &key (indent 4) more-textobjects injection-names definitions queries)
  "Record language NAME with DOC, GRAMMAR and MODES, reading its colouring,
indentation, selection and injection rules, one level of indentation being
INDENT columns, MORE-TEXTOBJECTS the text of selection rules of its own,
added to those read, INJECTION-NAMES a regular expression for the names
other languages' rules call it by, and DEFINITIONS the text of the rules
naming its definitions; a language defined again keeps the style and level
chosen for it. One of Tuatara's own languages is never replaced from
elsewhere (an init file, a package): that is refused, said, and NIL
returned; a package adds languages of its own. This keeps a package from
replacing one by mistake; it is no guard against one meaning to, as a
package runs Lisp with all the rights Tuatara has."
  (let ((old (find-language name)))
    (when (and old (own-language-p old) (not (own-language-name-p name)))
      (message "~(~a~) is one of Tuatara's own languages: a package or an init file adds languages, it does not replace them (~(~a~) left as it is)"
               name name)
      (return-from register-language nil)))
  (when injection-names
    (handler-case (cl-ppcre:create-scanner (format nil "^(?i)(~a)$" injection-names))
      (error (e)
        (error "The injection names of ~(~a~), ~s, are not a regular expression: ~a" name injection-names e))))
  (let* ((old (find-language name))
         (queries (or queries grammar))
         (textobjects (let ((read (read-textobjects queries)))
                        (if (and read more-textobjects)
                            (format nil "~a~%~a" read more-textobjects)
                            (or read more-textobjects))))
         (definitions (if (eq definitions :textobjects) (textobject-definitions textobjects) definitions))
         (language (make-language name doc grammar modes (read-rules queries) (read-indents queries) indent textobjects
                                  (read-injections queries) injection-names (read-locals queries) definitions
                                  (read-tags queries))))
    (when old
      (setf (language-style language) (language-style old)
            (language-level language) (language-level old)))
    (setf *languages* (cons language (remove old *languages*)))
    (clrhash *injection-languages*)
    name))

(defmacro define-language (name doc &key grammar modes (indent 4) more-textobjects injection-names definitions queries)
  "Define language NAME (of code or of text, such as Markdown), documented by
DOC, parsed with the tree-sitter grammar GRAMMAR (the name of its file, rust
for rust.so: Tuatara's own, or one you added to the folder of grammars you
add, ~/.local/share/tuatara/grammars/, its rules beside it in rules/,
indents/ and the rest) and edited in the major modes MODES. Its colouring rules are
read from syntax/rules/GRAMMAR.scm, and its indentation rules, if it has
them, from syntax/indents/GRAMMAR.scm, one level being INDENT columns. The rules for
selecting its functions, types, parameters and comments are read from
syntax/textobjects/GRAMMAR.scm, with the text MORE-TEXTOBJECTS added, and
its rules for text in other languages from syntax/injections/GRAMMAR.scm;
INJECTION-NAMES is a regular expression for the names other languages' rules
call it by, besides its own name and its grammar's. DEFINITIONS is the text
of the rules naming its definitions, what \\[beginning-of-defun] and
\\[end-of-defun] move between, each captured as @definition, or :TEXTOBJECTS
for its functions and types as its selection rules find them; without them a
definition is a line that begins with an opening bracket, as in Lisp. The
rules naming its definitions and headings, which \\[buffer-outline] lists, are
read from syntax/tags/GRAMMAR.scm. QUERIES names the rules' files when they
are not GRAMMAR's (a language with rules of its own for a grammar it shares)."
  (check-type doc string "a docstring saying what the language is")
  `(register-language ',name ,doc ,grammar ',modes :indent ,indent :more-textobjects ,more-textobjects
                      :injection-names ,injection-names :definitions ,definitions :queries ,queries))

(defvar *mode-languages* (cons nil (make-hash-table :test 'eq))
  "The language each major mode edits, by the mode's name: (LANGUAGES .
TABLE), TABLE made from LANGUAGES, the *LANGUAGES* of then.")

(defun mode-languages ()
  "A table from each major mode's name to the language it edits, the first in
*LANGUAGES* to name it; made again when *LANGUAGES* is another list."
  (let ((made *mode-languages*))
    (if (eq (car made) *languages*)
        (cdr made)
        (let ((table (make-hash-table :test 'eq)))
          (dolist (language (reverse *languages*))
            (dolist (name (language-modes language))
              (setf (gethash name table) language)))
          (setf *mode-languages* (cons *languages* table))
          table))))

(defun mode-language (mode)
  "The language of major mode MODE (an instance): that of the nearest mode it
inherits from that edits one, or NIL."
  (let ((table (mode-languages)))
    (loop for name in (mode-precedence mode)
          thereis (values (gethash name table)))))

(defvar *rules-quiet* nil
  "True in the thread compiling a language's rules in the background, where
what fails is kept to be said by the editor's thread (*RULES-UNSAID*).")

(defvar *rules-unsaid* (sb-concurrency:make-queue :name "rules unsaid")
  "What compiling rules in the background found wrong, to be said in the
editor's thread.")

(defun rules-failed (control &rest arguments)
  "Say that rules do not work, as MESSAGE does, or keep it to be said when in
a thread of the background (*RULES-QUIET*)."
  (tuatara.runtime:debug-report :error "~?" control arguments)
  (if *rules-quiet*
      (sb-concurrency:enqueue (apply #'format nil control arguments) *rules-unsaid*)
      (apply #'message control arguments)))

(defparameter *language-queries*
  '((:colouring language-query language-rules "colouring"
     "The colouring rules of ~(~a~) do not work: ~a")
    (:injection language-injection-query language-injections "injection"
     "The rules of ~(~a~) for text in other languages do not work: ~a")
    (:locals language-locals-query language-locals "local names"
     "The rules of ~(~a~) for local names do not work: ~a")
    (:tags language-tags-query language-tags "outline"
     "The rules of ~(~a~) naming its definitions do not work: ~a")
    (:definitions language-definitions-query language-definitions "definition"
     "The definition rules of ~(~a~) do not work: ~a")
    (:indent language-indent-query language-indents "indentation"
     "The indentation rules of ~(~a~) do not work: ~a")
    (:text-objects language-textobject-query language-textobjects "selection"
     "The selection rules of ~(~a~) do not work: ~a" disable-unused-patterns))
  "The kinds of a language's rules that are compiled when first needed, each
\(KIND KEPT SOURCE LABEL FAILURE AFTER): KEPT reads the rules compiled (and,
with SETF, keeps them), SOURCE reads their text, LABEL names them to the
compiler, FAILURE says they do not work given the language's name and the
error, and AFTER, when given, is called with the rules once compiled.")

(defvar *rules-unanswered* (make-hash-table :test 'eq :synchronized t)
  "For each language whose rules the program compiling them did not answer
for (a busy moment), (WHEN WAIT TIMES): they are not asked for again before
the internal real time WHEN, WAIT the seconds waited, doubled each time it
does not answer again, up to *RULES-UNANSWERED-WAIT-MOST*; TIMES how many
times it has not answered.")

(defparameter *rules-unanswered-wait* 0.5
  "The seconds before rules the program compiling them did not answer for are
asked for again, the first time.")

(defparameter *rules-unanswered-wait-most* 30
  "The most seconds before rules the program compiling them did not answer
for are asked for again.")

(defun rules-backing-off-p (language)
  "True while LANGUAGE's rules are not to be asked for again yet, the program
compiling them not having answered (*RULES-UNANSWERED*)."
  (let ((entry (gethash language *rules-unanswered*)))
    (and entry (< (get-internal-real-time) (first entry)))))

(defparameter *rules-unanswered-most* 4
  "How many times the program compiling a language's rules may not answer for
them before they are given up on as failed until Tuatara starts again: each
time it is ended, and with it the rules of every other language being
compiled.")

(defun note-rules-unanswered (language)
  "Have LANGUAGE's rules asked for again later, the program compiling them not
having answered: after *RULES-UNANSWERED-WAIT*, twice as long as before when
it did not answer again. True when it has not answered
*RULES-UNANSWERED-MOST* times: they are not asked for again."
  (sb-ext:with-locked-hash-table (*rules-unanswered*)
    (let* ((before (gethash language *rules-unanswered*))
           (wait (if before (min *rules-unanswered-wait-most* (* 2 (second before))) *rules-unanswered-wait*))
           (times (1+ (if before (third before) 0))))
      (setf (gethash language *rules-unanswered*)
            (list (+ (get-internal-real-time) (round (* wait internal-time-units-per-second))) wait times))
      (>= times *rules-unanswered-most*))))

(defvar *rules-editor-wait* 2
  "The seconds the editor's thread waits for a language's rules to be compiled
when it needs them at once (to indent a line, select a function); past them
it carries on as for a language without such rules, and they are kept once
compiled in the background.")

(defvar *compiling* (make-hash-table :test 'equal :synchronized t)
  "The thread compiling each language's rules of a kind for the editor's
thread, (LANGUAGE . KIND), while it runs (LANGUAGE-COMPILED-QUERY).")

(defun language-compiled-query (language kind)
  "The compiled rules of LANGUAGE of KIND, one of *LANGUAGE-QUERIES* (such as
:COLOURING, :INDENT or :TEXT-OBJECTS), compiled when first needed; NIL when
it has none, its grammar is missing, or they do not compile, which is said
once in *Messages* (RULES-FAILED). Rules the program compiling them did not
answer for, at a busy moment, are not taken for failed: NIL now, and asked
for again a little later (NOTE-RULES-UNANSWERED), until it has not answered
*RULES-UNANSWERED-MOST* times. On the editor's thread they are compiled in
the background, waited for at most *RULES-EDITOR-WAIT*: NIL past it, the
editor carrying on as for a language without such rules, and the rules
kept once compiled."
  (let ((known (funcall (second (assoc kind *language-queries*)) language)))
    (cond (known (and (tuatara.syntax:query-p known) known))
          ((not (on-editor-thread-p)) (compile-language-query language kind))
          ((rules-backing-off-p language) nil)
          (t (let ((thread (thread-once *compiling* (cons language kind)
                                        (lambda () (let ((*rules-quiet* t)) (compile-language-query language kind))))))
               (sb-thread:join-thread thread :timeout *rules-editor-wait* :default nil)
               (let ((now (funcall (second (assoc kind *language-queries*)) language)))
                 (and (tuatara.syntax:query-p now) now)))))))

(defun compile-language-query (language kind)
  "The work of LANGUAGE-COMPILED-QUERY, on the thread it runs on: LANGUAGE's
rules of KIND compiled, and kept."
  (destructuring-bind (kept source label failure &optional after) (rest (assoc kind *language-queries*))
    (let ((known (funcall kept language)))
      (if known
          (and (tuatara.syntax:query-p known) known)
          (let* ((failed nil)
                 (grammar (and (not (rules-backing-off-p language))
                               (tuatara.syntax:find-grammar (language-grammar language))))
                 (source (funcall source language))
                 (query (when (and grammar source)
                          (handler-case (prog1 (tuatara.syntax:compile-query grammar source label)
                                          (remhash language *rules-unanswered*))
                            (tuatara.syntax:query-not-answered (e)
                              (when (note-rules-unanswered language)
                                (rules-failed failure (language-name language) e)
                                (setf failed t))
                              nil)
                            (error (e)
                              (rules-failed failure (language-name language) e)
                              (setf failed t)
                              nil)))))
            (when (and query after) (funcall after query))
            ;; not kept while the program compiling rules is still starting, nor
            ;; while it does not answer
            (unless (or (and (null query) (not failed)
                             (or (tuatara.syntax:grammar-pending-p (language-grammar language)) (and grammar source)))
                        (and (not failed) (rules-backing-off-p language)))
              (funcall (fdefinition (list 'setf kept)) (or query :failed) language))
            query)))))

;;; Choosing how a language is coloured

(defparameter *colouring-styles* '(:rich :sparse :none)
  "The colouring styles, in the order \\[rotate-colouring] goes through them.")

(defparameter *sparse-levels*
  '(("comment" "string" "diff" "markup.heading")
    ("keyword" "function" "tag" "markup" "punctuation.special.separator")
    ("type" "constructor" "constant" "attribute" "label" "namespace"))
  "What the sparse style colours at each level, as leading parts of the rules'
capture names (keyword stands for keyword.control.conditional and the rest):
level 1 colours what the first list names, level 2 what the first two name,
level 3 all three, and level 4 everything, as the rich style does. The rest,
such as variables, fields, operators and brackets, is left plain, but for
the marks between the columns of a table (LaTeX's &).")

(defun language-colouring (language &optional mode)
  "The style LANGUAGE is coloured in and the level of the sparse style, as two
values: those set for it (set-colouring, \\[rotate-colouring]), else those set
for the nearest major mode MODE (an instance; LANGUAGE's first mode when NIL,
if it has one) is made from, or for every buffer; else rich, and level 3."
  (if (language-style language)
      (values (language-style language) (or (language-level language) 3))
      (multiple-value-bind (style level)
          (colouring-rule (or mode (let ((first (first (language-modes language))))
                                     (and first (make-instance first)))))
        (values (or style :rich) (or level 3)))))

(defgeneric mode-own-colouring (mode)
  (:documentation "The name set-colouring and \\[rotate-colouring] give the
colouring of major mode MODE (an instance) by, for a mode that colours its
text by rules of its own at the sparse style's levels, as Org: the name its
style is set under (org-mode); NIL for any other mode.")
  (:method ((mode t)) nil))

(defun mode-colouring (mode)
  "The style a major mode that colours by rules of its own (MODE-OWN-COLOURING)
is coloured in, and the level of the sparse style, as two values: those set for
the nearest mode MODE is made from, or for every buffer (COLOURING-RULE); else
rich, and level 3."
  (multiple-value-bind (style level) (colouring-rule mode)
    (values (or style :rich) (or level 3))))

(defun colouring-of (name)
  "What (set-colouring NAME ...) colours: T for every buffer; :CSV for CSV and
TSV (csv, tsv, or a mode made from csv-mode); the language called NAME, or that
of the major mode called NAME; the name of a major mode that colours by rules
of its own (MODE-OWN-COLOURING), as org-mode; else, for a major mode that edits
no language itself, the mode's name when a language or CSV is edited in a mode
made from it, :NOTHING when none is; NIL for a name that is none of these."
  (let ((mode (and (not (eq name t)) (find (string name) (mode-names) :key #'symbol-name :test #'string-equal))))
    (cond ((eq name t) t)
          ((or (member (string name) '("csv" "tsv") :test #'string-equal)
               (and mode (subtypep mode 'csv-mode)))
           :csv)
          ((find-language name))
          ((null mode) nil)
          ((mode-language (make-instance mode)))
          ((mode-own-colouring (make-instance mode)) mode)
          ((or (subtypep 'csv-mode mode)
               (some (lambda (language) (some (lambda (m) (subtypep m mode)) (language-modes language))) *languages*))
           mode)
          (t :nothing))))

(defun tuatara-user:set-colouring (language style &key level)
  "Colour LANGUAGE (rust, bash, lisp), or the language of a major mode
(rust-mode), in STYLE: :rich, the default, colours everything its rules name;
:sparse colours only some kinds of thing, and LEVEL says how much: 1 comments and strings, 2 also keywords and functions, 3
(the default) also types and constants, 4 everything; :none colours nothing.
Every mode of the language goes with it. CSV and TSV (csv, csv-mode, tsv-mode)
are coloured their own way, without levels: :rich each column in its own
colour; :sparse only the separators, the quoted fields and the lines that
start with #; :none nothing.
Org (org-mode) colours by rules of its own, at the same styles and levels:
level 1 its headlines, comments and keyword lines, 2 also TODO keywords, tags,
emphasis, links, lists and code, 3 also dates. LANGUAGE may also be a major
mode the modes of languages are made from, as prog-mode or text-mode (Org
too), for all of them, or T for every buffer. What is set
for a language itself comes first, then what is set for the nearest mode its
mode is made from, then what is set for every buffer. For example, in the init
file: (set-colouring 'rust :sparse), (set-colouring 'csv-mode :sparse),
(set-colouring 'prog-mode :sparse), (set-colouring 'org-mode :sparse :level 2),
(set-colouring t :sparse)."
  (let ((found (colouring-of language)))
    (cond ((null found)
           (error "No language or major mode called ~(~a~); the languages are csv, ~{~(~a~)~^, ~}."
                  language (mapcar #'language-name *languages*)))
          ((eq found :nothing)
           (error "No language is coloured in ~(~a~) or a mode made from it." language)))
    (unless (member style *colouring-styles*)
      (error "The style must be one of ~{~s~^, ~}, not ~s." *colouring-styles* style))
    (when level
      (when (eq found :csv)
        (error "CSV and TSV are coloured without levels: no :level for ~(~a~)." language))
      (unless (and (integerp level) (<= 1 level 4))
        (error "The level must be from 1 to 4, not ~s." level)))
    (cond ((eq found :csv) (setf *csv-colouring* style))
          ((symbolp found)
           (setf *colouring-rules* (cons (list found style level) (remove found *colouring-rules* :key #'first))))
          (t (when level (setf (language-level found) level))
             (setf (language-style found) style)))
    style))

(defun tuatara-user:set-indentation (language columns)
  "Indent LANGUAGE (c, rust, bash) COLUMNS columns a level, where its rules
indent by levels: \\<normal>\\[indent-selection-or-line] and a new line's
indentation use it. For example, in the init file: (set-indentation 'c 2)."
  (let ((found (find-language language)))
    (unless found (error "No language called ~(~a~); there are ~{~(~a~)~^, ~}." language (mapcar #'language-name *languages*)))
    (unless (and (integerp columns) (<= 1 columns 16))
      (error "The columns must be a whole number from 1 to 16, not ~s." columns))
    (setf (language-indent found) columns)))

;;; Faces for captures

(defun rich-face (name)
  "The face for capture NAME: the face named by the longest leading part of
NAME, cut at dots, that is a face; :NONE for the capture none, which takes a
colour away; NIL when no part is a face."
  (if (string= name "none")
      :none
      (loop for end = (length name) then (position #\. name :end end :from-end t)
            while end
            do (let ((face (find-symbol (string-upcase (subseq name 0 end)) :keyword)))
                 (when (and face (tuatara.display:find-face face))
                   (return face))))))

(defun sparse-colours-p (name level)
  "True when the sparse style at LEVEL colours what capture NAME captures."
  (or (>= level 4)
      (loop for kinds in *sparse-levels*
            for l from 1 to level
            thereis (find-if (lambda (kind)
                               (and (string= kind name :end2 (min (length name) (length kind)))
                                    (or (= (length name) (length kind))
                                        (and (> (length name) (length kind)) (char= (char name (length kind)) #\.)))))
                             kinds))))

(defvar *capture-faces* (make-hash-table :test 'eq :weakness :key)
  "For each compiled query, the faces of its captures worked out so far, by
style, level and the number of faces there were.")

(defun query-faces (query style level)
  "The face of each capture of QUERY, by capture number, in STYLE (:RICH or
:SPARSE) at LEVEL. In the sparse style what the level leaves plain gets
:NONE: it still wins its text over other captures, as in the rich style, so
that the colours left are the rich style's, but it shows no colour."
  (let* ((key (list style (if (eq style :sparse) level 0) (hash-table-count tuatara.display:*faces*)))
         (known (assoc key (gethash query *capture-faces*) :test #'equal)))
    (if known
        (cdr known)
        (let ((faces (map 'vector (lambda (name)
                                    (let ((face (rich-face name)))
                                      (if (and face (eq style :sparse) (not (sparse-colours-p name level)))
                                          :none
                                          face)))
                          (tuatara.syntax:query-captures query))))
          (push (cons key faces) (gethash query *capture-faces*))
          faces))))

;;; Parsing the buffers of a language

(defun parse-afresh (buffer)
  "Forget what went wrong in BUFFER's text and parse it afresh in its mode's
language, when it has one, grammars that went wrong on it tried again."
  (tuatara.syntax:forget-text-faults buffer)
  (let ((language (mode-language (buffer-major-mode buffer))))
    (when (and language (let ((syntax (tuatara.syntax:buffer-syntax buffer)))
                          (or (null syntax) (tuatara.syntax:syntax-broken syntax))))
      (tuatara.syntax:detach-syntax buffer)
      (start-parsing buffer language))))

(defun start-parsing (buffer language)
  "Keep a syntax tree of BUFFER's text in LANGUAGE's grammar, unless its file
was visited safely (whose text no parser is given), the grammar is off
\(GRAMMAR-ON-P: BUFFER is then marked, for the question whether to turn it
on), or the grammar is missing."
  (when (setf (tuatara.buffer:buffer-local buffer :grammar-off)
              (and (not (tuatara.io:safe-visit-p buffer)) (not (grammar-on-p language)) language))
    (grammar-question-wanted buffer))
  (unless (or (tuatara.io:safe-visit-p buffer) (tuatara.buffer:buffer-local buffer :grammar-off))
    (let ((grammar (tuatara.syntax:find-grammar (language-grammar language))))
      (setf (tuatara.buffer:buffer-local buffer :syntax-pending)
            (and (null grammar) (tuatara.syntax:grammar-pending-p (language-grammar language)) language))
      (when grammar
        (tuatara.syntax:attach-syntax buffer grammar)))))

;; any mode with a language, whether code (prog-mode) or text (markdown-mode)
(defmethod mode-enter :after ((mode fundamental-mode) buffer)
  "Start parsing BUFFER as the language of MODE, when it has one."
  (let ((language (mode-language mode)))
    (when language (start-parsing buffer language))))

(defmethod mode-exit :after ((mode fundamental-mode) buffer)
  "Stop parsing BUFFER and forget its syntax tree."
  (setf (tuatara.buffer:buffer-local buffer :grammar-off) nil)
  (tuatara.syntax:detach-syntax buffer))

(defvar *rules-wait* 0.05
  "Seconds redisplay waits for a language's rules for colouring to be compiled
the first time they are needed, which for a few languages takes a third of a
second; past it the text shows without their colours, which come when the
compiling, in the background, is done.")

(defvar *warming* (make-hash-table :test 'eq :synchronized t)
  "The thread compiling each language's rules for colouring, while it runs.")

(defun rules-ready-p (language)
  "True when LANGUAGE's rules for colouring (its colours, its text in other
languages, its local names) are compiled, or found not to compile."
  (and (language-query language)
       (or (null (language-injections language)) (language-injection-query language))
       (or (null (language-locals language)) (language-locals-query language))))

(defun warm-rules (language)
  "Compile LANGUAGE's rules for colouring in a thread of its own, unless they
are compiled or being compiled, and have the editor redraw when they are.
Returns the thread, or NIL."
  (unless (or (rules-ready-p language) (rules-backing-off-p language))
    (thread-once *warming* language
                 (lambda ()
                   (unwind-protect
                        (let ((*rules-quiet* t))
                          (language-compiled-query language :colouring) (language-compiled-query language :injection) (language-compiled-query language :locals))
                     (sb-ext:atomic-incf (car tuatara.syntax:*rules-generation*))
                     (let ((wake tuatara.syntax:*syntax-wake-function*))
                       (when wake (ignore-errors (funcall wake)))))))))

(defun thread-once (table key function)
  "The thread running FUNCTION for KEY, kept in TABLE (a synchronized table)
while it runs: the one there when it is alive, else a new one, kept there
before it can end, and taken out as it ends only while the entry is still
its own."
  (sb-ext:with-locked-hash-table (table)
    (let ((running (gethash key table)))
      (if (and running (sb-thread:thread-alive-p running))
          running
          (setf (gethash key table)
                (tuatara.runtime:make-guarded-thread
                 (lambda ()
                   (unwind-protect (funcall function)
                     (sb-ext:with-locked-hash-table (table)
                       (when (eq (gethash key table) sb-thread:*current-thread*)
                         (remhash key table)))))
                 :name "tuatara rules"))))))

(defun rules-at-hand (language &key (wait *rules-wait*))
  "True when LANGUAGE's rules for colouring are compiled, compiling them in the
background when not, and waiting up to WAIT seconds for that; what failed
meanwhile is said."
  (prog1 (or (rules-ready-p language)
             (let ((thread (warm-rules language)))
               (when (and thread (plusp wait))
                 (sb-thread:join-thread thread :timeout wait :default nil))
               (rules-ready-p language)))
    (loop for text = (sb-concurrency:dequeue *rules-unsaid*)
          while text do (message "~a" text))))

(defun kind-face (kind style level)
  "The face for names defined as KIND (such as variable.parameter) in STYLE at
LEVEL, or NIL when that style leaves them plain."
  (let ((face (rich-face kind)))
    (and face (not (eq face :none))
         (or (not (eq style :sparse)) (sparse-colours-p kind level))
         face)))

(defun language-for-injection (name)
  "The language a rule for text in another language means by NAME: the one of
that name, else one with a grammar of that name (the grammar's own language
first), else the first whose
injection names match all of NAME; for (:SHEBANG LINE) the language of the
mode a file beginning with LINE gets, for (:FILENAME NAME) that of the mode a
file called NAME gets; NIL when this editor has none."
  (let ((known (gethash name *injection-languages*)))
    (if known
        (and (not (eq known :none)) known)
        (let ((language
                (if (consp name)
                    (let ((mode (if (eq (first name) :shebang)
                                    (tuatara.modes:mode-for-file #p"/" (second name))
                                    (tuatara.modes:mode-for-file (sb-ext:parse-native-namestring (second name))))))
                      (and (not (eq mode 'fundamental-mode)) (mode-language (make-instance mode))))
                (or (find-if (lambda (l) (string-equal name (symbol-name (language-name l)))) *languages*)
                    ;; a grammar's own language before another that borrows it (Bash's, not .env files')
                    (let ((users (remove-if-not (lambda (l) (string-equal name (language-grammar l))) *languages*)))
                      (or (find-if (lambda (l) (string-equal (language-grammar l) (symbol-name (language-name l)))) users)
                          (first users)))
                    (find-if (lambda (l) (let ((names (language-injection-names l)))
                                           (and names (ignore-errors (cl-ppcre:scan (format nil "^(?i)(~a)$" names) name)))))
                             *languages*)))))
          (setf (gethash name *injection-languages*) (or language :none))
          language))))

(defun injection-resolver (style level)
  "A function of a language's name, as rules for text in other languages name
it, returning the grammar, colouring query, faces in STYLE at LEVEL, injection
rules and rules for local names of the language meant, and the function giving
the face in STYLE at LEVEL of each kind of definition, or NIL. The rules of a language not yet
compiled are compiled in the background, the function waiting for them at
most *RULES-WAIT* from when it is made, for all the languages it is asked for
together. NIL for a language whose grammar is off (GRAMMAR-ON-P): its text is
left plain."
  (let ((deadline (tuatara.runtime:deadline-in *rules-wait*)))
    (lambda (name)
      (let ((other (language-for-injection name)))
        (when (and other (grammar-on-p other)
                   (rules-at-hand other :wait (max 0 (/ (- deadline (get-internal-real-time))
                                                        internal-time-units-per-second))))
          (let ((grammar (tuatara.syntax:find-grammar (language-grammar other)))
                (colours (language-compiled-query other :colouring)))
            (when (and grammar colours)
              (values grammar colours (query-faces colours style level) (language-compiled-query other :injection)
                      (language-compiled-query other :locals) (lambda (kind) (kind-face kind style level))))))))))

(defmethod mode-syntax-highlights ((mode fundamental-mode) buffer rows)
  "Colour what ROWS of BUFFER show by its language's rules, in the style and
level chosen for it."
  (let ((syntax (tuatara.syntax:buffer-syntax buffer))
        (language (mode-language mode)))
    (multiple-value-bind (style level) (and language (language-colouring language mode))
      (if (and syntax language (plusp (length rows)) (not (eq style :none))
               (rules-at-hand language))
          (let ((query (language-compiled-query language :colouring))
                (injections (language-compiled-query language :injection))
                (locals (language-compiled-query language :locals)))
            (when query
              (let ((faces (query-faces query style level))
                    ;; text in another language, coloured in this one's style
                    (injections (and injections (list injections (injection-resolver style level))))
                    ;; a local name coloured as its definition
                    (locals (and locals (list locals (lambda (kind) (kind-face kind style level))))))
                ;; only what the rows show: the rest of a very long line is not looked at
                (loop for (start . end) in (tuatara.display:shown-ranges rows :buffer buffer)
                      append (tuatara.syntax:syntax-highlights syntax query faces start end injections locals)))))
          (call-next-method)))))

(define-command rotate-colouring ()
  "Colour the current buffer's language in the next style: rich, then sparse,
then none, then rich again. Every buffer of the language changes with it, in
a CSV or TSV buffer every CSV and TSV buffer, and in an Org buffer every Org
buffer; the style each starts in is set in the init file with set-colouring."
  (let ((mode (buffer-major-mode (current-buffer))))
    (flet ((next (style) (or (second (member style *colouring-styles*)) (first *colouring-styles*))))
      (let ((language (mode-language mode)))
        (cond ((typep mode 'csv-mode)
               (setf *csv-colouring* (next (csv-colouring mode)))
               (message "CSV and TSV colouring: ~(~a~)" *csv-colouring*))
              ((mode-own-colouring mode)
               (let ((name (mode-own-colouring mode))
                     (style (next (mode-colouring mode)))
                     (level (nth-value 1 (colouring-rule mode))))
                 (setf *colouring-rules* (cons (list name style level) (remove name *colouring-rules* :key #'first)))
                 (message "~a colouring: ~(~a~)" (tuatara.modes:mode-lighter name) style)))
              ((null language)
               (message "~a is not in the mode of a language this editor colours" (tuatara.buffer:buffer-name (current-buffer))))
              (t
               (setf (language-style language) (next (language-colouring language mode)))
               (message "~@(~a~) colouring: ~(~a~)" (language-name language) (language-style language))))))))

(defun install-colouring (editor)
  "Have a parse that finishes in the background wake EDITOR, to show its
colours, and a grammar that fails, or a parse given up as too slow, say so; and
a text left alone be parsed afresh (SYNTAX-REFRESH-TICK)."
  (setf tuatara.syntax:*syntax-wake-function* (lambda () (wake-editor editor))
        tuatara.syntax:*syntax-fault-function* 'report-grammar-fault
        tuatara.syntax:*syntax-slow-function* 'report-slow-parse
        tuatara.syntax:*syntax-helper-shared-function* 'buffer-in-trusted-project-p
        tuatara.syntax:*helper-ended-function* 'report-helper-ended
        tuatara.syntax:*helper-refused-function* 'report-no-sandbox-for-grammars
        tuatara.syntax:*helper-in-use-function* 'buffer-shown-p
        tuatara.syntax:*must-not-wait-function* 'on-editor-thread-p
        tuatara.syntax:*user-grammar-function* 'user-grammar-allowed-p)
  ;; the helpers all share started now, in the background: the first start
  ;; looks for the sandbox, which may take a moment
  (sb-thread:make-thread (lambda () (ignore-errors (tuatara.syntax:rules-helper) (tuatara.syntax:shared-helper)))
                         :name "syntax helpers starting")
  (pushnew 'syntax-refresh-tick *tick-functions*))

(defun syntax-refresh-tick (editor)
  "Tick function: parse afresh, in the background, the text of each buffer
left alone long enough since parses after its edits
(TUATARA.SYNTAX:REFRESH-SYNTAX). Asks to be called when the next one is due,
or when rules not answered for may be asked for again (*RULES-UNANSWERED*)."
  (declare (ignore editor))
  (let ((soonest nil))
    ;; a buffer whose grammar was not to be had yet, its loader starting
    (dolist (buffer tuatara.buffer:*buffers*)
      (let ((language (tuatara.buffer:buffer-local buffer :syntax-pending))
            (next (or (tuatara.buffer:buffer-local buffer :syntax-pending-next) 0))
            (now (get-internal-real-time)))
        (when (and language (null (tuatara.syntax:buffer-syntax buffer)))
          (if (< now next)
              (setf soonest (/ (- next now) internal-time-units-per-second))
              (let ((tries (setf (tuatara.buffer:buffer-local buffer :syntax-pending-tries)
                                 (1+ (or (tuatara.buffer:buffer-local buffer :syntax-pending-tries) 0)))))
                (start-parsing buffer language)
                ;; tried less and less often while the grammar cannot be had yet
                (when (tuatara.buffer:buffer-local buffer :syntax-pending)
                  (let ((wait (min 10 (* 0.2 (expt 2 (min tries 6))))))
                    (setf (tuatara.buffer:buffer-local buffer :syntax-pending-next)
                          (+ now (round (* wait internal-time-units-per-second)))
                          soonest (min (or soonest wait) wait)))))))))
    ;; rules the program compiling them did not answer for: the screen drawn
    ;; again, and they asked for again, once their wait is over
    (sb-ext:with-locked-hash-table (*rules-unanswered*)
      (maphash (lambda (language entry)
                 (declare (ignore language))
                 (let ((wait (/ (- (first entry) (get-internal-real-time)) internal-time-units-per-second)))
                   (when (plusp wait)
                     (setf soonest (if soonest (min soonest wait) wait)))))
               *rules-unanswered*))
    (dolist (buffer tuatara.buffer:*buffers* soonest)
      (let* ((syntax (tuatara.syntax:buffer-syntax buffer))
             (wait (and syntax (tuatara.syntax:refresh-syntax syntax))))
        (when wait (setf soonest (if soonest (min soonest wait) wait)))))))

(defvar *memory-low-said* nil
  "True once the editor has said that memory is low and that parses stopped
half way keep what they took until it starts again.")

(defun report-slow-parse (syntax)
  "Say that parsing the text of SYNTAX's buffer took too long, or too much
memory, or that memory ran short, and it was given up."
  (let ((bytes (tuatara.text:rope-bytes (tuatara.syntax:syntax-rope syntax)))
        (grammar (tuatara.syntax:grammar-name (tuatara.syntax:syntax-grammar syntax)))
        (buffer (tuatara.buffer:buffer-name (tuatara.syntax:syntax-buffer syntax))))
    (case (tuatara.syntax:syntax-broken syntax)
      (:memory
       (message "The ~a grammar went wrong reading ~a and was stopped (it took over ~:d MB): its colours are off in that buffer."
                grammar buffer (floor (tuatara.syntax:parse-memory-limit bytes) (* 1024 1024))))
      (:short
       (message "Memory ran short while the ~a grammar read ~a: its colours are off there until memory is back and the text is edited.~:[~; Memory is low: parses stopped half way keep what they took until Tuatara starts again.~]"
                grammar buffer
                (and (plusp (tuatara.syntax:parsers-let-go)) (not (shiftf *memory-low-said* t)))))
      (t
       (message "The ~a grammar took too long to read ~a (over ~,0f seconds): its colours are off in that buffer."
                grammar buffer (tuatara.syntax:parse-time-limit bytes))))))

(defun on-editor-thread-p ()
  "True on the thread of the editor running, which never waits for a program to start."
  (and (running-editor) (not (away-from-editor-p))))

(defun buffer-shown-p (buffer)
  "True when BUFFER is shown in a window of the editor running, from any thread."
  (let ((editor (running-editor)))
    (and editor (buffer-windows buffer editor) t)))

(defun report-helper-ended (helper)
  "Say that HELPER, a program running the grammars, ended while no parse ran
in it (a parse that was running says so itself); its text is parsed again
in a new one when it is next shown or edited."
  (when (zerop (car (tuatara.syntax:helper-parsing helper)))
    (message "The program running the grammars ~(~a~); text is parsed again in a new one."
             (case (tuatara.syntax:helper-ended helper)
               (:hung "stopped answering and was ended")
               (:broken "answered what cannot be right and was ended")
               (t "ended")))))

(defun report-no-sandbox-for-grammars (trusted)
  "Say that text is not parsed, so not coloured, because there is no sandbox
to run the grammars in: TRUSTED when it is that of a project you trust."
  (message "~:[Files outside the projects you trust are not~;No text is~] coloured or parsed: there is no sandbox (bwrap) to run the grammars in. Install bubblewrap~:[~;, or set *syntax-helper-unconfined* to run them for the projects you trust without it~]; M-x check-health says more."
           trusted trusted))

(defun report-grammar-fault (syntax)
  "Say that the grammar of SYNTAX failed while reading its buffer and is off
until the editor starts again; and, when its code may have damaged the
editor's memory (not when it ran in the program parsing for the editor), to
save and restart."
  (let* ((grammar (tuatara.syntax:syntax-grammar syntax))
         (fault (tuatara.syntax:grammar-faulted grammar))
         ;; the program parsing ended, or the grammar only gave it a wrong value:
         ;; the editor's own memory is as it was
         (clean (or (eq fault :crashed) (typep fault 'tuatara.syntax:grammar-failure))))
    (tuatara.runtime:debug-report :error "the ~a grammar failed reading ~a~@[: ~a~]"
                                  (tuatara.syntax:grammar-name grammar)
                                  (tuatara.buffer:buffer-name (tuatara.syntax:syntax-buffer syntax))
                                  (and (typep fault 'condition) fault))
    (if fault
        (message "The ~a grammar failed while reading ~a, as it did on other texts: its colours and syntax are off until Tuatara starts again.~:[ Save your work and restart.~;~]"
                 (tuatara.syntax:grammar-name grammar)
                 (tuatara.buffer:buffer-name (tuatara.syntax:syntax-buffer syntax))
                 clean)
        (message "The ~a grammar failed while reading ~a: its colours and syntax are off in that buffer; other texts keep them."
                 (tuatara.syntax:grammar-name grammar)
                 (tuatara.buffer:buffer-name (tuatara.syntax:syntax-buffer syntax))))))

;;; The languages

(define-major-mode rust-mode prog-mode
  "Rust source."
  :lighter "Rust" :comment "//" :extensions ("rs") :interpreters ("rust-script" "cargo"))

(define-major-mode bash-mode prog-mode
  "Shell scripts: bash, and the POSIX shells whose scripts read alike."
  :lighter "Bash"
  :comment "#"
  :extensions ("sh" "bash" "ash" "dash" "ksh" "mksh" "zsh" "eclass" "ebuild")
  :file-names (".bashrc" ".bash_profile" ".bash_login" ".bash_logout" ".bash_aliases" ".profile"
               ".zshrc" ".zshenv" ".zprofile" ".zlogin" ".zlogout" ".xprofile" ".xinitrc" "xinitrc"
               ".xsession" ".xsessionrc" "PKGBUILD" "APKBUILD" "bash_completion" "direnvrc")
  :patterns ("/bash-completion/completions/[^/]+$" "/bash_completion\\.d/[^/]+$")
  :interpreters ("sh" "bash" "dash" "ash" "ksh" "mksh" "zsh"))

(define-major-mode containerfile-mode prog-mode
  "Containerfiles and Dockerfiles: the instructions Podman, Buildah and Docker
build container images from."
  :lighter "Containerfile"
  :comment "#"
  :extensions ("containerfile" "dockerfile")
  :file-names ("Containerfile" "containerfile" "Dockerfile" "dockerfile")
  :patterns ("/[Cc]ontainerfile\\.[^/]*$" "/[Dd]ockerfile\\.[^/]*$"))

(define-major-mode c-mode prog-mode
  "C source and its headers."
  :lighter "C" :comment "//" :extensions ("c" "h"))

(define-language rust
  "Rust, parsed with tree-sitter-rust."
  :grammar "rust" :modes (rust-mode) :indent 4 :injection-names "rs|rust"
  :definitions "[(function_item) (impl_item) (struct_item) (enum_item)] @definition")

(define-language bash
  "Shell scripts, parsed with tree-sitter-bash."
  :grammar "bash" :modes (bash-mode) :indent 2 :injection-names "shell|bash|zsh|sh"
  :definitions "(function_definition) @definition")

(define-language c
  "C, parsed with tree-sitter-c."
  :grammar "c" :modes (c-mode) :indent 4 :injection-names "c"
  :definitions "[(function_definition) (type_definition) (preproc_def) (preproc_function_def)] @definition
(translation_unit [(struct_specifier) (enum_specifier) (union_specifier)] @definition)")

(define-language containerfile
  "Containerfiles and Dockerfiles, parsed with tree-sitter-dockerfile."
  :grammar "dockerfile" :modes (containerfile-mode) :indent 2
  :injection-names "docker|dockerfile|containerfile")

(define-language lisp
  "Common Lisp, parsed with tree-sitter-commonlisp."
  :grammar "commonlisp" :modes (lisp-mode) :injection-names "common-lisp|commonlisp|lisp|cl"
  :more-textobjects "
; functions: defun, defmacro, defgeneric, defmethod and lambda forms, their
; bodies inside
(defun (defun_header) . (_)+ @function.inside) @function.around

; types: defclass, defstruct, define-condition and deftype forms, inside
; everything after the name
((list_lit . (sym_lit) @_definer . (_) . (_)+ @class.inside) @class.around
 (#match? @_definer \"^(?i)(defclass|defstruct|define-condition|deftype)$\"))

; parameters: each part of a lambda list, and each argument of a call
(defun_header lambda_list: (list_lit (_) @parameter.inside @parameter.around))
(list_lit . (sym_lit) (_) @parameter.inside @parameter.around)
")
