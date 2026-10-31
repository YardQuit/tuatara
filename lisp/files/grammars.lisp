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

(defvar *grammar-answers-entries* nil
  "The entries of *GRAMMAR-ANSWERS* *GRAMMAR-ANSWERS-READ* was made from.")

(defun grammar-answers (&key again)
  "The table of the answers to whether to turn a grammar on (see
*GRAMMAR-ANSWERS-READ*), read from the list kept between sessions the first
time, and with AGAIN looked at again (another session may have answered
meanwhile), made again only when the list has changed."
  (let ((table *grammar-answers-read*))
    (if (and table (not again))
        table
        (let ((entries (state-list-entries *grammar-answers*)))
          (if (and table (eq entries *grammar-answers-entries*))
              table
              (let ((table (make-hash-table :test 'equalp)))
                ;; the latest first: an older answer for the same language is passed over
                (dolist (answer (reverse entries))
                  (setf (gethash (car answer) table) (if (cdr answer) :on :off)))
                (setf *grammar-answers-entries* entries
                      *grammar-answers-read* table)))))))

(defun user-grammar-names ()
  "The names of the grammars you added (TUATARA.SYNTAX:USER-GRAMMAR-FILE): each
NAME.so in the folder of grammars you add whose name Tuatara has no grammar of."
  (loop for path in (ignore-errors (directory (merge-pathnames "*.so" (tuatara.syntax:user-grammar-directory))))
        for name = (pathname-name path)
        when (and (tuatara.syntax:grammar-name-p name) (tuatara.syntax:user-grammar-file name))
          collect name))

(defun grammars-on-table ()
  "A table holding the name of each grammar on: those of the languages
*GRAMMARS-ON* names (each language's own grammar, which the languages made
from it share, and a name that is a grammar's), and of those turned on when
asked; :USER for a grammar you added, on only while its file is the one
turned on (USER-GRAMMAR-TRUSTED-P), and never among the common ones. Worked
out again only when the setting, the languages or the answers change."
  (let ((seen *grammars-on-table*)
        (answers (grammar-answers)))
    (if (and seen (eq (first seen) *grammars-on*) (eq (second seen) *languages*) (eq (third seen) answers))
        (cdddr seen)
        (let ((table (make-hash-table :test 'equal))
              (setting *grammars-on*)
              (added (user-grammar-names)))
          (flet ((turn-on (name &optional common)
                   (let* ((language (find-language name))
                          (grammar (if language (language-grammar language) (string-downcase (string name))))
                          (user (member grammar added :test #'string=)))
                     (unless (and common user)
                       (setf (gethash (string-downcase (string name)) table) (if user :user t)
                             (gethash grammar table) (if user :user t))))))
            (dolist (entry (setting-list '*grammars-on* setting))
              (cond ((eq entry :common) (dolist (name *common-grammar-languages*) (turn-on name t)))
                    ((or (symbolp entry) (stringp entry)) (turn-on entry))))
            (maphash (lambda (name answer) (when (eq answer :on) (turn-on name))) answers))
          (setf *grammars-on-table* (list* setting *languages* answers table))
          table))))

(defun grammar-on-p (language)
  "True when LANGUAGE's grammar may read text: it is on (*GRAMMARS-ON*, or
turned on when asked), and when it is one you added, its file is the one
turned on (USER-GRAMMAR-TRUSTED-P)."
  (let* ((grammar (language-grammar language))
         (on (gethash grammar (grammars-on-table))))
    (if (eq on :user)
        (user-grammar-trusted-p grammar)
        (and on t))))

(defvar *user-grammar-sums-kept* 200
  "How many sums of grammars you added, as they were turned on, are remembered.")

(defun print-user-grammar-sum (entry)
  "The line of the list of sums of grammars you added for ENTRY, (NAME . SUM)."
  (format nil "~a~c~a" (car entry) #\Tab (cdr entry)))

(defun parse-user-grammar-sum (line)
  "The entry (NAME . SUM) a LINE of the list of sums of grammars you added
writes, or NIL for a line that is none."
  (let ((tab (position #\Tab line)))
    (and tab (plusp tab) (= 64 (- (length line) tab 1))
         (cons (subseq line 0 tab) (subseq line (1+ tab))))))

(define-state-list *user-grammar-sums* "user-grammars" 'parse-user-grammar-sum 'print-user-grammar-sum
  '*user-grammar-sums-kept* :path-of #'car)

(defvar *user-grammar-seen* (make-hash-table :test 'equal :synchronized t)
  "For each grammar you added, (STAMP SUM SAID): its file as last looked at,
its SHA-256 then, and whether its change was said.")

(defun user-grammar-sum (name)
  "The SHA-256 of the file of NAME, a grammar you added, summed again only when
the file has changed; NIL when it cannot be read."
  (let* ((path (tuatara.syntax:user-grammar-file name))
         (stamp (and path (state-file-stamp (tuatara.io:native path))))
         (seen (gethash name *user-grammar-seen*)))
    (cond ((null stamp) nil)
          ((and seen (equal (first seen) stamp)) (second seen))
          (t (let ((sum (file-sha256 (tuatara.io:native path))))
               (setf (gethash name *user-grammar-seen*) (list stamp sum nil))
               sum)))))

(defun record-user-grammar (name)
  "Keep the SHA-256 of the file of NAME, a grammar you added, as the one
turned on: from now on it is used while its file is that one."
  (let ((sum (user-grammar-sum name)))
    (when sum (state-list-put *user-grammar-sums* (list (cons name sum))))
    sum))

(defun user-grammar-trusted-p (name)
  "True when the file of NAME, a grammar you added, is the one turned on: its
SHA-256 as when it was first turned on (kept now, when it never was).
Changed since, it is refused, said once, until it is turned on again."
  (let ((sum (user-grammar-sum name))
        (kept (cdr (state-list-find *user-grammar-sums* name))))
    (cond ((null sum) nil)
          ((null kept) (record-user-grammar name) t)
          ((string= sum kept) t)
          (t (let ((seen (gethash name *user-grammar-seen*)))
               (unless (third seen)
                 (setf (third seen) t)
                 (message "The ~a grammar in ~a has changed since you turned it on: it stays off until you turn it on again (~a)"
                          name (abbreviated-path (namestring (tuatara.syntax:user-grammar-directory)))
                          (substitute-command-keys "\\[turn-on-grammar]"))))
             nil))))

(defun language-words (language)
  "LANGUAGE named for a person: the mode-line name of its first mode, else its own name."
  (let ((mode (first (language-modes language))))
    (or (and mode (ignore-errors (tuatara.modes:mode-lighter mode)))
        (string-downcase (symbol-name (language-name language))))))

(defun note-grammar-answer (language on)
  "Keep the answer ON (true for on) for LANGUAGE's grammar, between sessions as
well; turned on, a grammar you added is kept as its file is now."
  (let ((name (string-downcase (symbol-name (language-name language)))))
    (when (and on (tuatara.syntax:user-grammar-file (language-grammar language)))
      (record-user-grammar (language-grammar language))
      (let ((seen (gethash (language-grammar language) *user-grammar-seen*)))
        (when seen (setf (third seen) nil))))
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
  "Ask whether to turn LANGUAGE's grammar on, answered by one key pressed
once the question is shown (keys typed before are thrown away): y keeps it
on and parses the buffers in it, n keeps it off and says how to turn it on
later; any other key, or \\`C-g', is \"not now\": not asked again in this
session, and nothing kept."
  (let* ((words (language-words language))
         (key (handler-case (read-key (format nil "Turn on the ~a grammar? (y or n) " words) :fresh t)
                (quit () nil)))
         (answer (and key (not (tuatara.frontend:key-ctrl key)) (not (tuatara.frontend:key-meta key))
                      (characterp (tuatara.frontend:key-base key))
                      (char-downcase (tuatara.frontend:key-base key)))))
    (case answer
      (#\y (note-grammar-answer language t)
       (grammars-changed)
       (message "The ~a grammar is on" words))
      (#\n (note-grammar-answer language nil)
       (message "~a" (grammar-stays-off-words language)))
      (t (push (string-downcase (symbol-name (language-name language))) *grammars-not-now*)
       (message "~a; asked again when Tuatara starts again" (grammar-stays-off-words language))))))

(defvar *grammar-asking* nil
  "True while the question whether to turn a grammar on is asked.")

(defun grammar-question-pending-p (buffer)
  "The language of BUFFER whose grammar is off and was never asked about in
this session nor answered, or NIL."
  (let ((language (and (tuatara.buffer:live-buffer-p buffer) (tuatara.buffer:buffer-local buffer :grammar-off))))
    (and language
         (let ((name (string-downcase (symbol-name (language-name language)))))
           (and (not (member name *grammars-not-now* :test #'string=))
                (null (gethash name (grammar-answers)))
                (not (grammar-on-p language))))
         language)))

(defun ask-about-shown-grammar (editor)
  "Post-command function while a buffer's grammar question is pending: when
the buffer of EDITOR's selected window is in a language whose grammar is off
and was never asked about, ask whether to turn it on
\(ASK-TO-TURN-ON-GRAMMAR); not while another question is open or recorded
keys play. Once no buffer has a question pending, it takes itself away, so
that the commands after cost nothing more."
  (unless (or *grammar-asking* (editor-minibuffer-active editor) (keys-playing-p))
    (let ((buffer (tuatara.display:window-buffer (editor-window editor))))
      (when (tuatara.buffer:buffer-local buffer :grammar-off)
        ;; another session may have answered meanwhile
        (grammar-answers :again t)
        (let ((language (grammar-question-pending-p buffer)))
          (cond (language
                 (let ((*grammar-asking* t))
                   (ask-to-turn-on-grammar language)))
                ((grammar-on-p (tuatara.buffer:buffer-local buffer :grammar-off))
                 (grammars-changed))))))
    (unless (some #'grammar-question-pending-p tuatara.buffer:*buffers*)
      (setf (editor-post-command-functions editor)
            (remove 'ask-about-shown-grammar (editor-post-command-functions editor))))))

(defun grammar-question-wanted (buffer)
  "Have the editor ask, as BUFFER is shown, whether to turn its language's
grammar on (ASK-ABOUT-SHOWN-GRAMMAR), when that question is pending; from
any thread."
  (when (and (grammar-question-pending-p buffer) (running-editor))
    (defer-action (lambda ()
                    (let ((editor (or *editor* (running-editor))))
                      (when editor
                        (pushnew 'ask-about-shown-grammar (editor-post-command-functions editor))
                        (ask-about-shown-grammar editor)))))))

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

(defun user-grammar-allowed-p (name)
  "True when NAME, a grammar you added, may be loaded: it is on, and its file
is the one turned on (TUATARA.SYNTAX:*USER-GRAMMAR-FUNCTION*)."
  (and (eq (gethash name (grammars-on-table)) :user) (user-grammar-trusted-p name)))
