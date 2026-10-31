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
  "The names of the grammars you added (TUATARA.SYNTAX:USER-GRAMMAR-P): each
NAME.so in the folder you add them to, or kept from it, whose name Tuatara
has no grammar of."
  (remove-duplicates
   (loop for folder in (list (tuatara.syntax:user-grammar-directory) (tuatara.syntax:user-grammar-store))
         append (loop for path in (ignore-errors (directory (merge-pathnames "*.so" folder) :resolve-symlinks nil))
                      for name = (pathname-name path)
                      when (tuatara.syntax:user-grammar-p name) collect name))
   :test #'string=))

(defun grammars-on-table ()
  "A table holding the name of each grammar on: those of the languages
*GRAMMARS-ON* names (each language's own grammar, which the languages made
from it share, and a name that is a grammar's), and of those turned on when
asked. A grammar you added is never among the common ones: :NAMED when the
setting names it, :USER when it was turned on when asked; either is on only
as USER-GRAMMAR-ON-P says. Worked out again only when the setting, the
languages or the answers change."
  (let ((seen *grammars-on-table*)
        (answers (grammar-answers)))
    (if (and seen (eq (first seen) *grammars-on*) (eq (second seen) *languages*) (eq (third seen) answers))
        (cdddr seen)
        (let ((table (make-hash-table :test 'equal))
              (setting *grammars-on*)
              (added (user-grammar-names)))
          (flet ((turn-on (name how)
                   (let* ((language (find-language name))
                          (grammar (if language (language-grammar language) (string-downcase (string name))))
                          (user (member grammar added :test #'string=))
                          (value (cond ((not user) t) ((eq how :named) :named) (t :user))))
                     (unless (and (eq how :common) user)
                       (dolist (key (list (string-downcase (string name)) grammar))
                         (unless (eq (gethash key table) :named)
                           (setf (gethash key table) value)))))))
            (dolist (entry (setting-list '*grammars-on* setting))
              (cond ((eq entry :common) (dolist (name *common-grammar-languages*) (turn-on name :common)))
                    ((or (symbolp entry) (stringp entry)) (turn-on entry :named))))
            (maphash (lambda (name answer) (when (eq answer :on) (turn-on name :answer))) answers))
          (setf *grammars-on-table* (list* setting *languages* answers table))
          table))))

(defun grammar-on-p (language)
  "True when LANGUAGE's grammar may read text: it is on (*GRAMMARS-ON*, or
turned on when asked), and when it is one you added, USER-GRAMMAR-ON-P says
so. Nothing is read from the disk."
  (let* ((grammar (language-grammar language))
         (on (gethash grammar (grammars-on-table))))
    (if (member on '(:user :named))
        (user-grammar-on-p grammar on)
        (and on t))))

;;; Grammars you add

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

(defvar *user-grammar-records* nil
  "The sums kept of the grammars you added and turned on, as read from
*USER-GRAMMAR-SUMS*: a table from a grammar's name to its SHA-256.")

(defvar *user-grammar-verdicts* (make-hash-table :test 'equal :synchronized t)
  "For each grammar you added looked at when loaded in this session, what was
found: :TRUSTED, the copy kept is what was turned on and its file has not
changed; :CHANGED, its file has; :GONE, its file or the copy is gone; or
:UNTRUSTED, it was never turned on as it is.")

(defparameter *user-grammar-largest* (* 256 1024 1024)
  "The largest file of a grammar you add that is taken, in octets.")

(defun user-grammar-records ()
  "The table of *USER-GRAMMAR-RECORDS*, read once."
  (or *user-grammar-records*
      (let ((table (make-hash-table :test 'equal :synchronized t)))
        (dolist (entry (reverse (state-list-entries *user-grammar-sums*)) table)
          (setf (gethash (car entry) table) (cdr entry)))
        (setf *user-grammar-records* table))))

(defun user-grammar-on-p (grammar how)
  "True when GRAMMAR, one you added, on as HOW says (:NAMED by *GRAMMARS-ON*,
:USER when asked), may read text: not found changed or gone when loaded,
and either turned on as it is (its sum kept) or named in *GRAMMARS-ON*,
which keeps it as it is the first time it is loaded. Nothing is read from
the disk: the file is looked at when the grammar is loaded."
  (let ((verdict (gethash grammar *user-grammar-verdicts*)))
    (case verdict
      (:trusted t)
      ((:changed :gone :untrusted) nil)
      (t (or (eq how :named) (and (gethash grammar (user-grammar-records)) t))))))

(defun open-user-grammar (name)
  "A descriptor open for reading on NAME.so in the folder of grammars you add,
and its size: a plain file there, not a link (never followed); NIL when
there is none such."
  (let* ((path (uiop:native-namestring (merge-pathnames (format nil "~a.so" name) (tuatara.syntax:user-grammar-directory))))
         (fd (ignore-errors (sb-posix:open path (logior sb-posix:o-rdonly sb-posix:o-nofollow #o2000000)))))
    (when fd
      (let ((stat (ignore-errors (sb-posix:fstat fd))))
        (if (and stat (= (logand (sb-posix:stat-mode stat) sb-posix:s-ifmt) sb-posix:s-ifreg))
            (values fd (sb-posix:stat-size stat))
            (progn (sb-posix:close fd) nil))))))

(defun user-grammar-file-sum (name)
  "The SHA-256 of NAME.so in the folder of grammars you add, read through a
descriptor on the file itself (OPEN-USER-GRAMMAR); NIL when there is none."
  (let ((fd (open-user-grammar name)))
    (when fd
      (unwind-protect (file-sha256 (format nil "/proc/self/fd/~d" fd))
        (sb-posix:close fd)))))

(defun stored-grammar-path (name)
  "The path of the copy kept of NAME, a grammar you added (USER-GRAMMAR-STORE)."
  (merge-pathnames (format nil "~a.so" name) (tuatara.syntax:user-grammar-store)))

(defun store-user-grammar (name)
  "Keep NAME.so of the folder of grammars you add as turned on: its octets,
read through a descriptor on the file itself, written as a new file of yours
alone in USER-GRAMMAR-STORE, which the program running grammars loads it
from, and its SHA-256 kept between sessions. The sum, or NIL when the file
is not a plain file there or is larger than *USER-GRAMMAR-LARGEST*."
  (multiple-value-bind (fd size) (open-user-grammar name)
    (when fd
      (let ((octets (unwind-protect (ignore-errors (tuatara.io:fd-octets fd (uiop:native-namestring (stored-grammar-path name))
                                                                           size *user-grammar-largest*))
                      (sb-posix:close fd))))
        (when octets
          (let* ((store (tuatara.syntax:user-grammar-store))
                 (path (stored-grammar-path name))
                 (new (merge-pathnames (format nil "~a.so.~a" name (tuatara.io:random-name)) store)))
            (tuatara.io:write-new-file new octets)
            (sb-posix:rename (uiop:native-namestring new) (uiop:native-namestring path))
            (let ((sum (file-sha256 (uiop:native-namestring path))))
              (when sum
                (state-list-put *user-grammar-sums* (list (cons name sum)))
                (setf (gethash name (user-grammar-records)) sum)
                (remhash name *user-grammar-verdicts*))
              sum)))))))

(defun user-grammar-allowed-p (name)
  "True when NAME, a grammar you added, may be loaded now (from its copy
kept): it is on, and either turned on as its file is now (that copy's sum
kept, and the file's the same), or named in *GRAMMARS-ON* and kept as it is
now, the first time. Its file changed or gone, it is refused, said once,
until it is turned on again. The verdict is kept for the session, so that
nothing is read again for it (*USER-GRAMMAR-VERDICTS*). Asked when the
grammar is loaded (TUATARA.SYNTAX:*USER-GRAMMAR-FUNCTION*), on the thread
loading it."
  (let ((how (gethash name (grammars-on-table))))
    (when (member how '(:user :named))
      (let ((verdict (gethash name *user-grammar-verdicts*)))
        (unless verdict
          (let ((kept (gethash name (user-grammar-records))))
            (when (and (null kept) (eq how :named) (store-user-grammar name))
              (setf kept (gethash name (user-grammar-records))))
            (setf verdict
                  (cond ((null kept) :untrusted)
                        ((not (equal kept (file-sha256 (uiop:native-namestring (stored-grammar-path name))))) :gone)
                        (t (let ((now (user-grammar-file-sum name)))
                             (cond ((null now) :gone)
                                   ((string= now kept) :trusted)
                                   (t :changed)))))
                  (gethash name *user-grammar-verdicts*) verdict)
            (when (member verdict '(:changed :gone))
              (message "The ~a grammar you added has ~:[changed~;gone, or its copy kept~] since you turned it on: it stays off until you turn it on again (~a)"
                       name (eq verdict :gone) (substitute-command-keys "\\[turn-on-grammar]")))))
        (eq verdict :trusted)))))

(defun language-words (language)
  "LANGUAGE named for a person: the mode-line name of its first mode, else its own name."
  (let ((mode (first (language-modes language))))
    (or (and mode (ignore-errors (tuatara.modes:mode-lighter mode)))
        (string-downcase (symbol-name (language-name language))))))

(defun note-grammar-answer (language on)
  "Keep the answer ON (true for on) for LANGUAGE's grammar, between sessions as
well; turned on, a grammar you added is kept as its file is now
\(STORE-USER-GRAMMAR)."
  (let ((name (string-downcase (symbol-name (language-name language)))))
    (when (and on (tuatara.syntax:user-grammar-file (language-grammar language)))
      (store-user-grammar (language-grammar language)))
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
this session nor answered (a grammar you added, never turned on as it is, is
asked about again), or NIL."
  (let ((language (and (tuatara.buffer:live-buffer-p buffer) (tuatara.buffer:buffer-local buffer :grammar-off))))
    (and language
         (let ((name (string-downcase (symbol-name (language-name language)))))
           (and (not (member name *grammars-not-now* :test #'string=))
                (not (grammar-on-p language))
                ;; an answer of before, for a grammar now one you added and
                ;; never turned on as it is, is asked again
                (or (null (gethash name (grammar-answers)))
                    (and (tuatara.syntax:user-grammar-p (language-grammar language))
                         (null (gethash (language-grammar language) (user-grammar-records)))
                         (eq (gethash name (grammar-answers)) :on)))))
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
