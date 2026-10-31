;;;; lisp/files/grammars.lisp --- Which grammars read text: the common ones, and those you turn on.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A grammar is code from outside Tuatara, run shut in, on the text of a file
;;;; and on the text in other languages inside it (a code block in a note or a
;;;; Markdown file). The grammars of the languages most people write, kept by
;;;; tree-sitter's own projects or by the languages' own, read text without
;;;; asking; a file in another language asks once whether to turn its grammar
;;;; on, and the answer is kept between sessions. Text in another language
;;;; inside a file is read only by a grammar that is on, so that no file can
;;;; choose which grammar's code runs on it.

(in-package #:tuatara.defaults)

(defparameter *common-grammar-languages*
  '(rust bash c containerfile lisp
    toml json jsonc json5 yaml xml ini markdown markdown-inline html css latex bibtex
    diff git-config git-ignore git-attributes git-rebase git-commit regex comment jsdoc log
    javascript jsx typescript tsx python go gomod java cpp c-sharp lua ruby php php-only perl sql
    make cmake haskell kotlin swift scala elixir erlang ocaml ocaml-interface r julia zig elisp
    clojure hcl powershell)
  "The languages whose grammars are on while *GRAMMARS-ON* holds :COMMON: those
most people write, each grammar kept by tree-sitter's own projects, by the
language's own, or used by many, and those Tuatara's own modes and lists use.")

(defvar *grammars-on* '(:common)
  "The languages whose grammars read text without asking: a file in such a
language is coloured and parsed as it opens, and so is the text in it inside
another file (a code block in a note or a Markdown file). A list of language
names, as \\[check-health] lists them (yaml, nix, awk), and :COMMON for the
languages most people write (the default, '(:common)). A grammar is code from
outside Tuatara, run shut in; a file in a language whose grammar is not on
asks whether to turn it on, and the answer is kept between sessions
\(\\[turn-on-grammar] and \\[turn-off-grammar] change it later); text in such
a language inside another file is left plain. A list without :COMMON keeps
off every grammar not named, the common ones too. A file opened safely is
never parsed, whatever this says.")

(defvar *grammar-answers-kept* 1000
  "How many answers to whether to turn a grammar on are remembered.")

(defun print-grammar-answer (answer)
  "The line of the list of grammar answers kept between sessions for ANSWER,
\(NAME . ON): the language's name, a tab, and on or off."
  (format nil "~a~c~:[off~;on~]" (car answer) #\Tab (cdr answer)))

(defun parse-grammar-answer (line)
  "The answer (NAME . ON) a LINE of the list of grammar answers writes, or NIL
for a line that is none."
  (let ((tab (position #\Tab line)))
    (and tab (plusp tab)
         (member (subseq line (1+ tab)) '("on" "off") :test #'string=)
         (cons (subseq line 0 tab) (string= (subseq line (1+ tab)) "on")))))

(define-state-list *grammar-answers* "grammars" 'parse-grammar-answer 'print-grammar-answer '*grammar-answers-kept*
  :path-of #'car)

(defvar *grammar-answers-read* nil
  "The answers to whether to turn a grammar on, as last read from the list kept
between sessions: a table from a language's name to :ON or :OFF.")

(defvar *grammars-on-table* nil
  "The grammars on, as last worked out (GRAMMARS-ON-TABLE): (SETTING LANGUAGES
ANSWERS . TABLE), TABLE holding the name of each grammar on, worked out from
*GRAMMARS-ON* as SETTING, *LANGUAGES* as LANGUAGES and ANSWERS, the
*GRAMMAR-ANSWERS-READ* of then.")

(defvar *grammars-not-now* '()
  "The names of the languages whose question was given up in this session
\(\\`C-g'): not asked again until Tuatara starts again.")

(defun grammar-answers (&key again)
  "The table of the answers to whether to turn a grammar on (see
*GRAMMAR-ANSWERS-READ*), read from the list kept between sessions the first
time, and again with AGAIN (another session may have answered meanwhile)."
  (or (and (not again) *grammar-answers-read*)
      (let ((table (make-hash-table :test 'equalp)))
        ;; the latest first: an older answer for the same language is passed over
        (dolist (answer (reverse (state-list-entries *grammar-answers*)) table)
          (setf (gethash (car answer) table) (if (cdr answer) :on :off)))
        (setf *grammar-answers-read* table))))

(defun grammars-on-table ()
  "A table holding the name of each grammar on: those of the languages
*GRAMMARS-ON* names (each language's own grammar, which the languages made
from it share, and a name that is a grammar's), and of those turned on when
asked. Worked out again only when the setting, the languages or the answers
change."
  (let ((seen *grammars-on-table*)
        (answers (grammar-answers)))
    (if (and seen (eq (first seen) *grammars-on*) (eq (second seen) *languages*) (eq (third seen) answers))
        (cdddr seen)
        (let ((table (make-hash-table :test 'equal))
              (setting *grammars-on*))
          (flet ((turn-on (name)
                   (let ((language (find-language name)))
                     (setf (gethash (string-downcase (string name)) table) t)
                     (when language (setf (gethash (language-grammar language) table) t)))))
            (dolist (entry (setting-list '*grammars-on* setting))
              (cond ((eq entry :common) (mapc #'turn-on *common-grammar-languages*))
                    ((or (symbolp entry) (stringp entry)) (turn-on entry))))
            (maphash (lambda (name answer) (when (eq answer :on) (turn-on name))) answers))
          (setf *grammars-on-table* (list* setting *languages* answers table))
          table))))

(defun grammar-on-p (language)
  "True when LANGUAGE's grammar may read text: it is on (*GRAMMARS-ON*, or
turned on when asked)."
  (and (gethash (language-grammar language) (grammars-on-table)) t))

(defun language-words (language)
  "LANGUAGE named for a person: the mode-line name of its first mode, else its own name."
  (let ((mode (first (language-modes language))))
    (or (and mode (ignore-errors (tuatara.modes:mode-lighter mode)))
        (string-downcase (symbol-name (language-name language))))))

(defun note-grammar-answer (language on)
  "Keep the answer ON (true for on) for LANGUAGE's grammar, between sessions as well."
  (let ((name (string-downcase (symbol-name (language-name language)))))
    (state-list-put *grammar-answers* (list (cons name on)))
    (let ((table (make-hash-table :test 'equalp)))
      (maphash (lambda (k v) (setf (gethash k table) v)) (grammar-answers))
      (setf (gethash name table) (if on :on :off)
            *grammar-answers-read* table))))

(defun grammars-changed ()
  "Parse the text of every buffer whose language's grammar was off and is on
now, stop parsing that of those whose grammar is off now, and have text in
other languages coloured again by the grammars on."
  (dolist (buffer tuatara.buffer:*buffers*)
    (let ((language (mode-language (buffer-major-mode buffer))))
      (when language
        (cond ((and (tuatara.buffer:buffer-local buffer :grammar-off) (grammar-on-p language))
               (start-parsing buffer language))
              ((and (tuatara.syntax:buffer-syntax buffer) (not (grammar-on-p language)))
               (tuatara.syntax:detach-syntax buffer)
               (setf (tuatara.buffer:buffer-local buffer :grammar-off) language))))))
  (sb-ext:atomic-incf (car tuatara.syntax:*rules-generation*)))

(defun grammar-stays-off-words (language)
  "What to say when LANGUAGE's grammar stays off: how to turn it on later."
  (format nil "The ~a grammar stays off: ~a turns it on, or ~(~a~) added to *grammars-on* in your init file"
          (language-words language) (substitute-command-keys "\\[turn-on-grammar]") (language-name language)))

(defun ask-to-turn-on-grammar (language)
  "Ask whether to turn LANGUAGE's grammar on, keep the answer, and act on it:
on, the buffers in it are parsed; off, say how to turn it on later. Given up
with \\`C-g', it is not asked again in this session."
  (handler-case
      (let ((on (yes-or-no-p* (format nil "Turn on the ~a grammar? It colours and parses ~a text with code from outside Tuatara, run shut in"
                                      (language-words language) (language-words language)))))
        (note-grammar-answer language on)
        (if on
            (progn (grammars-changed)
                   (message "The ~a grammar is on" (language-words language)))
            (message "~a" (grammar-stays-off-words language))))
    (quit ()
      (push (string-downcase (symbol-name (language-name language))) *grammars-not-now*)
      (message "~a" (grammar-stays-off-words language)))))

(defvar *grammar-asking* nil
  "True while the question whether to turn a grammar on is asked.")

(defun ask-about-shown-grammar (editor)
  "Post-command function: when the buffer of EDITOR's selected window is in a
language whose grammar is off and was never asked about, ask whether to turn
it on (ASK-TO-TURN-ON-GRAMMAR). Not while another question is open or
recorded keys play."
  (unless (or *grammar-asking* (editor-minibuffer-active editor) (keys-playing-p))
    (let* ((buffer (tuatara.display:window-buffer (editor-window editor)))
           (language (tuatara.buffer:buffer-local buffer :grammar-off)))
      (when language
        (let ((name (string-downcase (symbol-name (language-name language)))))
          (unless (member name *grammars-not-now* :test #'string=)
            ;; another session may have answered meanwhile
            (let ((answer (gethash name (grammar-answers :again t))))
              (cond ((grammar-on-p language) (grammars-changed))
                    ((null answer)
                     (let ((*grammar-asking* t))
                       (ask-to-turn-on-grammar language)))))))))))

(defun grammar-language-candidates (on)
  "The languages to choose among, as candidates of their names: those whose
grammar is on when ON, else those whose grammar is off; each language with a
grammar of its own, or with modes of its own."
  (sort (loop for language in *languages*
              when (and (or (language-modes language)
                            (string-equal (language-grammar language) (symbol-name (language-name language))))
                        (eq (and on t) (grammar-on-p language)))
                collect (string-downcase (symbol-name (language-name language))))
        #'string<))

(defun read-grammar-language (prompt on)
  "Ask with PROMPT for a language whose grammar is on (ON true) or off,
offering the current buffer's when it is one; the language chosen, or NIL."
  (let* ((own (mode-language (buffer-major-mode (current-buffer))))
         (names (grammar-language-candidates on))
         (default (and own (eq (and on t) (grammar-on-p own))
                       (string-downcase (symbol-name (language-name own))))))
    (if (null names)
        (progn (message "Every grammar is ~:[on~;off~]" on) nil)
        (find-language (completing-read prompt names :require-match t :default default)))))

(define-command turn-on-grammar ()
  "Turn on the grammar of a language, asked for (the current buffer's first
when its grammar is off): its files are coloured and parsed from now on, and
text in it inside other files, in later sessions too. A grammar is code from
outside Tuatara, run shut in. *grammars-on* names the grammars on without
asking; \\[turn-off-grammar] turns one off again."
  (let ((language (read-grammar-language "Turn on the grammar of: " nil)))
    (when language
      (note-grammar-answer language t)
      (setf *grammars-not-now* (remove (string-downcase (symbol-name (language-name language))) *grammars-not-now* :test #'string=))
      (grammars-changed)
      (message "The ~a grammar is on" (language-words language)))))

(define-command turn-off-grammar ()
  "Turn off the grammar of a language, asked for (the current buffer's first):
its files are no longer coloured or parsed, nor text in it inside other files,
in later sessions too, and no file asks to turn it on again;
\\[turn-on-grammar] turns it on again. A grammar *grammars-on* names stays on."
  (let ((language (read-grammar-language "Turn off the grammar of: " t)))
    (when language
      (note-grammar-answer language nil)
      (grammars-changed)
      (if (grammar-on-p language)
          (message "The ~a grammar stays on: *grammars-on* names it" (language-words language))
          (message "The ~a grammar is off" (language-words language))))))

(defun install-grammar-questions (editor)
  "Have EDITOR ask, as a file in a language whose grammar is off is shown,
whether to turn that grammar on."
  (pushnew 'ask-about-shown-grammar (editor-post-command-functions editor)))
