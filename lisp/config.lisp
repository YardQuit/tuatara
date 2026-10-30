;;;; lisp/config.lisp --- Your init file and the configuration API it uses.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; ~/.config/tuatara/init.lisp is read after the defaults are installed, in the
;;;; TUATARA-USER package, so that BIND-KEY and friends are available unqualified.
;;;; An error in it becomes a message; the editor still starts. The first time
;;;; the editor runs without one, it writes a starter there, every setting in it
;;;; commented out (but the tabs some languages' tools require): the settings
;;;; there are to be found, with how to use them.

(in-package #:tuatara.defaults)

(defvar *config-file* nil
  "The init file to load, or NIL for the default ~/.config/tuatara/init.lisp (XDG_CONFIG_HOME respected).")

(defun config-path (name)
  "The path of NAME, a file or a folder ending in /, in Tuatara's
configuration folder, ~/.config/tuatara/ (tuatara/ in XDG_CONFIG_HOME when it
is set), as text: (config-path \"snippets/\"). Worked out when asked, so
that it follows the environment the editor runs in."
  (namestring (uiop:xdg-config-home (concatenate 'string "tuatara/" name))))

(defun config-file ()
  "The init file path: *CONFIG-FILE* or init.lisp in the configuration
folder (CONFIG-PATH)."
  (or *config-file* (pathname (config-path "init.lisp"))))

;;; Settings read as they were meant

(defvar *settings-read-as* (make-hash-table :test 'eq :synchronized t)
  "Each list setting whose value was read as other than it is, by name: (VALUE
. READ-AS), the value last seen and how it was read, said once in *Messages*.")

(defun setting-list (name value &key pairs)
  "VALUE, the setting NAME holds, read as the list it is meant to be: a list as
it is; a dotted list, (A B . C), with its tail as one more element, (A B C);
any other value but NIL as a list of that one value. With PAIRS, for a
setting made of pairs (A . B): a single pair whose A is not itself a pair, as
(#\\( . #\\)), is a list of that one pair, and an element that is no pair is
left out. The first time a value is read as other than it is, *Messages*
says how it was read."
  (flet ((proper-p (list) (and (listp list) (null (cdr (last list))))))
    (if (and (proper-p value)
             (or (not pairs)
                 (every #'listp value)))
        value
        (let ((known (gethash name *settings-read-as*)))
          (if (and known (eq (car known) value))
              (cdr known)
              (let* ((items (cond ((and pairs (consp value) (atom (car value)) (atom (cdr value))) (list value))
                                  ((consp value) (append (loop for cell on value collect (car cell))
                                                         (let ((tail (cdr (last value)))) (and tail (list tail)))))
                                  (t (list value))))
                     (read (if pairs (remove-if-not #'listp items) items)))
                (setf (gethash name *settings-read-as*) (cons value read))
                (let ((*print-pretty* nil))
                  (message "~(~a~) is ~s, not a list~:[~; of pairs~]: read as ~s" name value pairs read))
                read))))))

(defun keymap-registry ()
  "The keymaps an init file can name, as an alist of (KEYWORD . KEYMAP)."
  `((:global . ,*global-keymap*)
    (:normal . ,*tuatara-normal-keymap*)
    (:insert . ,*tuatara-insert-keymap*)
    (:support . ,*tuatara-support-keymap*)
    (:leader . ,*tuatara-leader-keymap*)
    (:buffers . ,*buffers-keymap*)
    (:help . ,*help-keymap*)
    (:search . ,*search-keymap*)
    (:pattern . ,*pattern-keymap*)
    (:pattern-answer . ,*pattern-answer-keymap*)
    (:path-question . ,*path-question-keymap*)
    (:popup . ,*popup-keymap*)
    ,@(when *editor* `((:minibuffer . ,(editor-minibuffer-keymap *editor*))))))

(defun keymap-named (map)
  "The keymap MAP designates: a keymap object, or a keyword from
KEYMAP-REGISTRY."
  (cond ((keymap-p map) map)
        ((cdr (assoc map (keymap-registry))))
        (t (error "Unknown keymap ~s; use a major mode's name or one of ~{~s~^ ~}." map (mapcar #'car (keymap-registry))))))

(defun mode-keymaps (map mode)
  "The keymaps bind-key's MAP and MODE stand for: MAP's (KEYMAP-NAMED) without
MODE; with MODE, a major mode's name or a list of them (MODE-NAMED, as org for
org-mode), the keys bound for each of those modes (and the modes made from
them) in MAP, which is then :global (the mode's own keys), :normal, :insert,
:support or :leader (MODE-LAYER). MAP a major mode's name stands for that
mode's own keys, as with MAP :global and MODE that mode."
  (cond ((and (null mode) map (symbolp map) (not (keywordp map)))
         (mode-keymaps :global map))
        ((null mode) (list (keymap-named map)))
        (t (loop for given in (if (listp mode) mode (list mode))
                 for name = (if (and given (symbolp given)) (mode-named given) (error "No major mode called ~s." given))
                 collect (case map
                           (:global (mode-keymap name))
                           ((:normal :insert :support :leader) (mode-layer name map))
                           (t (error "With :mode, :map is :global, :normal, :insert, :support or :leader, not ~s." map)))))))

(defun tuatara-user:bind-key (keys command &key (map :global) mode label)
  "Bind KEYS (keys apart by spaces, C- for Control, M- for Meta and S- for
Shift, as help writes them: \"C-c t\", \"SPC ,\") to COMMAND (a command name
from any package, matched by name) in MAP: :global (both states), :normal,
:insert, :support, :leader (keys under the leader key, \\`SPC' or
*LEADER-KEY*), a special buffer's map such as :buffers, :help, :search,
:pattern (every question that takes a pattern, a search of files as you type
among them) or :minibuffer, :popup (the list of completions beside the
cursor), or a major mode's name ('csv-mode)
for keys only in that mode and the modes derived from it. The main
configuration call of an init file.

MODE, a major mode's name or a list of them, binds KEYS only in those modes
and the modes made from them, MAP then :global, :normal, :insert, :support or
:leader: (bind-key \"o l\" 'org-store-link :map :leader :mode '(org-mode
markdown-mode)) makes \\`SPC o l' that in Org and Markdown, and the same key
bound without MODE stays as it is everywhere else.

LABEL, a string, is what the key hints show for the key instead of the
command's name: (bind-key \"t h\" 'hl-line-mode :map :leader :label \"toggle
hl-line\"). COMMAND may also be the label and the command together: '(\"toggle
hl-line\" . hl-line-mode). A prefix key is labelled with COMMAND NIL, its
binding left as it is: (bind-key \"t\" nil :map :leader :label \"toggle\") shows
+toggle beside \\`t' after \\`SPC'."
  (let* ((label (or label (and (consp command) (car command))))
         (command (if (consp command) (cdr command) command))
         (symbol (and (or command (null label))
                      (or (command-symbol command) (error "No command named ~(~a~)." command)))))
    (dolist (keymap (mode-keymaps map mode))
      (cond ((and (null command) label) (set-key-label keymap keys label))
            (t (define-key keymap keys symbol)
               (when label (set-key-label keymap keys label)))))
    symbol))

(defun tuatara-user:unbind-key (keys &key (map :global) mode)
  "Remove the binding of KEYS in MAP, or in MAP for MODE (see BIND-KEY)."
  (dolist (keymap (mode-keymaps map mode))
    (define-key keymap keys nil)))

(defun tuatara-user:set-face (name &rest attributes)
  "Change face NAME (a keyword such as :comment, :keyword or :csv-column-1)
with ATTRIBUTES: :fg and :bg take a colour (:red, :bright-blue ..., a number
0-255, or \"#rrggbb\"), :bold :italic :reverse :dim take T or NIL,
:underline T (a straight line), :wave, :dotted, :box (a box round each
character; a terminal draws a line) or NIL, :underline-colour a
colour (NIL for the text's), :inherit names a face; in a window of its own (tuatara --gui), :font names a
font and its weight (\"Adwaita Mono Bold\"), drawn at the window's size,
and :height 1.25, 1.5, 2 (up to 3) draws a line beginning in the face that
many rows tall and its letters as wide (an Org headline level's face,
:org-level-1). Attributes not given keep their
value. Your settings stay on top of any theme. Examples: (set-face :comment :fg :cyan),
\(set-face :spellcheck :underline :dotted)."
  (apply #'tuatara.display:set-user-face name attributes))

(defun tuatara-user:load-theme (name)
  "Make theme NAME the one in force, for example (load-theme 'ao-light); the
themes are listed by \\[load-theme]. Your own set-face settings stay on top.
The desktop's light or dark style is not followed after it."
  (setf *theme-chosen* t)
  (tuatara.display:apply-theme name))

(defmacro tuatara-user:define-theme (name doc &body settings)
  "Define a theme called NAME, said by DOC, that \\[load-theme] lists and
(load-theme 'NAME) puts in force. The simplest starts from a theme and gives
some of its roles colours of their own: (define-theme my-ao \"Ao on black.\"
:faces-from ao-dark :palette ((ground \"#000000\"))). :palette gives roles
their colours (a role may name another role); :faces-from takes another
theme's faces, coloured by this palette, the roles it does not give from that
theme's; then face settings such as (:comment :fg comment :italic t), :fg and
:bg taking a role or a colour. The roles of ao-dark and ao-light are listed in
lisp/theme.lisp."
  `(tuatara.display:define-theme ,name ,doc ,@settings))

(defmacro starter-config-text ()
  "The starter init file as data/init.lisp has it, read when this file is
compiled and kept in the program."
  (data-file-text "init.lisp"))

(defparameter *starter-config* (starter-config-text)
  "The init file written the first time the editor runs without one: every
setting commented out, each with what it does, but for the tabs some languages'
own tools require (Make, gofmt), set there so that taking them away is taking
a line away.")

(defun write-starter-config (path)
  "Write *STARTER-CONFIG* to PATH as a new file, making its directory; a file
there already, or a link, is never touched. True when written; NIL when
not, and why as a second value when it could not be."
  (handler-case
      (unless (tuatara.io:path-kind path :follow nil)
        (ensure-directories-exist path)
        ;; made new or not at all: a link there, even one leading nowhere, is never written through
        (tuatara.io:write-new-file path *starter-config* :mode #o600)
        t)
    (error (e) (values nil (princ-to-string e)))))

(defun load-user-config ()
  "Load the init file if it exists, in the TUATARA-USER package, form by form
\(READ-INIT-FILE), the packages it loads said with their times
\(CALL-WITH-START-PACKAGES); an error in it is said and the rest is read. The first time the editor runs
without an init file of its own (*CONFIG-FILE* NIL: not a test's), a starter
is written there first (WRITE-STARTER-CONFIG) and said."
  (let ((file (config-file)))
    (when (and (null *config-file*) (not (probe-file file)))
      (multiple-value-bind (written why) (write-starter-config file)
        (cond (written (message "Wrote a starter init file: ~a" (abbreviated-path file)))
              (why (message "No starter init file written at ~a: ~a" (abbreviated-path file) why)))))
    (if (probe-file file)
        (let ((*package* (find-package :tuatara-user)))
          (setf *init-file-outcome* :read)
          (call-with-start-packages (lambda () (read-init-file file)))
          (eq *init-file-outcome* :read))
        (progn (setf *init-file-outcome* :none) nil))))

(defun next-form-start (text at)
  "Where the next form of TEXT, Lisp, begins at or after AT: past spaces,
line ends, ; comments and #| |# comments."
  (loop with i = at
        while (< i (length text))
        do (case (char text i)
             ((#\Space #\Tab #\Newline #\Return #\Page) (incf i))
             (#\; (setf i (or (position #\Newline text :start i) (length text))))
             (#\# (if (and (< (1+ i) (length text)) (char= (char text (1+ i)) #\|))
                      (setf i (nested-comment-end (lambda (j) (if (< j (length text)) (char-code (char text j)) 0))
                                                  i (length text)))
                      (return i)))
             (t (return i)))
        finally (return i)))

(defun map-text-forms (text function)
  "Read the forms of TEXT, Lisp, one after another, in the package and with
the reader as they are, calling FUNCTION with each and the line it begins on,
from 1. NIL when the whole of TEXT was read; else where reading stopped, as
\(LINE . CONDITION)."
  (let ((at 0) (counted 0) (line 1))
    (loop
      ;; the lines counted on from the form before, not from the start each time
      (let ((start (next-form-start text at)))
        (incf line (count #\Newline text :start counted :end start))
        (setf counted start))
      (multiple-value-bind (form next)
          (handler-case (read-from-string text nil text :start at)
            (error (e) (return (cons line e))))
        (when (eq form text) (return nil))
        (funcall function form line)
        (setf at next)))))

(defun read-init-file (file)
  "Read and evaluate the forms of the init file FILE one by one, in the package
it is read in (MAP-TEXT-FORMS): one that fails is said, with its line and why,
the first such kept in *INIT-FILE-OUTCOME*, and the forms after it are read
all the same; text that cannot be read at all ends the reading, said so.
The functions, commands and recorded keys its forms define are noted as
defined in FILE (NOTE-INIT-FILE-DEFINITION), where help shows them."
  (let ((text (tuatara.io:file-text file))
        (*load-truename* (truename file))
        (*load-pathname* (pathname file)))
    (flet ((fail (line e)
             (when (eq *init-file-outcome* :read)
               (setf *init-file-outcome* (cons :error (princ-to-string e))))
             (message "Error in ~a, line ~d: ~a" (namestring file) line e)))
      (let* ((path (namestring (truename file)))
             (stopped (map-text-forms text (lambda (form line)
                                             (handler-case (progn (eval form) (note-init-file-definition form path line))
                                               (error (e) (fail line e)))))))
        (when stopped (fail (car stopped) (cdr stopped)))))))
