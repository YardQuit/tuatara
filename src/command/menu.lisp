;;;; src/command/menu.lisp --- Menus of keys: actions and the switches they take, shown in the panel.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A menu is opened by a key and shown in the panel under the prompt, as
;;;; the keys under a prefix are: its actions, each a key and a command, and
;;;; its switches and options, each a key and an argument that the actions
;;;; read (MENU-ARGUMENTS). A switch or an option typed keeps the menu open,
;;;; flipping or setting it; an action runs and closes it, as does any other
;;;; key, C-g among them, unless the menu holds: then a key not in it is
;;;; said so and the menu stays, and C-g asks before what was set is lost. C-h lists all of the menu in *Help*. Magit's
;;;; transient menus work this way; the actions here are commands of their
;;;; own, run as well from M-x without the menu.

(in-package #:tuatara.command)

(defstruct (menu-entry (:constructor make-menu-entry (key label &key command switch option reader replace)) (:copier nil))
  "One line of a menu: KEY, as keys are written (- f), and LABEL, what it
says; then either COMMAND, the action the key runs, or SWITCH, an argument
the key turns on and off (--force), or OPTION, an argument given a value
\(--depth=), which READER, a function of a prompt, reads. An option with
REPLACE true is asked again when it has a value, READER then given that
value as well, and what it reads takes its place; an empty answer takes it
away."
  (key "" :type string :read-only t)
  (label "" :type string :read-only t)
  (command nil :read-only t)
  (switch nil :read-only t)
  (option nil :read-only t)
  (reader nil :read-only t)
  (replace nil :read-only t))

(setf (documentation 'make-menu-entry 'function) "A MENU-ENTRY of KEY and LABEL, with a COMMAND, a SWITCH or an OPTION, its READER and whether it is REPLACEd."
      (documentation 'menu-entry-replace 'function) "True when the option is asked again, its value given, rather than taken away."
      (documentation 'menu-entry-p 'function) "True when OBJECT is a MENU-ENTRY."
      (documentation 'menu-entry-key 'function) "The keys of the entry, as keys are written."
      (documentation 'menu-entry-label 'function) "What the entry says in the menu."
      (documentation 'menu-entry-command 'function) "The command the entry runs, or NIL for a switch or an option."
      (documentation 'menu-entry-switch 'function) "The argument the entry turns on and off, or NIL."
      (documentation 'menu-entry-option 'function) "The argument the entry gives a value, ending in =, or NIL."
      (documentation 'menu-entry-reader 'function) "The function of a prompt reading an option's value.")

(defstruct (menu (:constructor make-menu (name title groups &key show note summary hold)) (:copier nil))
  "A menu called NAME, the command opening it, shown under TITLE: GROUPS, each
\(TITLE . ENTRIES), ENTRIES MENU-ENTRYs. SHOW, when given, a function of an
entry, says whether the menu has it now (the key of one it leaves out says
so); NOTE, when given, a function of the menu, gives a line \\`C-h'
says first (which entries are left out and how to see them); SUMMARY, when
given, a function of the arguments on, gives lines shown under the entries,
what they come to. With HOLD true, a key not in the menu leaves it open,
saying so, and \\`C-g' asks first when an argument was changed."
  (name nil :read-only t)
  (title "" :read-only t)
  (groups '() :read-only t)
  (show nil :read-only t)
  (note nil :read-only t)
  (summary nil :read-only t)
  (hold nil :read-only t))

(setf (documentation 'make-menu 'function) "A MENU called NAME under TITLE with GROUPS, SHOW, NOTE, SUMMARY and HOLD."
      (documentation 'menu-hold 'function) "True when a key not in the menu leaves it open and \\`C-g' asks before leaving."
      (documentation 'menu-summary 'function) "The function giving the lines under the entries, or NIL."
      (documentation 'menu-p 'function) "True when OBJECT is a MENU."
      (documentation 'menu-name 'function) "The name of the menu, the command opening it."
      (documentation 'menu-title 'function) "What the menu is called at its top."
      (documentation 'menu-groups 'function) "The groups of the menu, each (TITLE . ENTRIES)."
      (documentation 'menu-show 'function) "The function saying whether the menu has an entry now, or NIL for all."
      (documentation 'menu-note 'function) "The function giving the line C-h says first, or NIL.")

(defvar *menus* (make-hash-table :test 'eq)
  "Every menu by its name, as DEFINE-MENU makes them.")

(defun menu-named (name)
  "The menu DEFINE-MENU made under NAME, the command opening it, or NIL."
  (values (gethash name *menus*)))

(defvar *menu-help-function* nil
  "A function of a text showing it in *Help*: what \\`C-h' in a menu lists.")

(defun menu-shows-p (menu entry)
  "True when MENU has ENTRY now (its SHOW function, when it has one)."
  (or (null (menu-show menu)) (funcall (menu-show menu) entry)))

(defun menu-entries (menu &key all)
  "The entries of MENU it has now, or with ALL every one, in order."
  (loop for (nil . entries) in (menu-groups menu)
        append (if all entries (remove-if-not (lambda (e) (menu-shows-p menu e)) entries))))

(defun parse-menu-entry (spec)
  "The form making a MENU-ENTRY from SPEC, as DEFINE-MENU takes one:
\(KEY LABEL COMMAND), (KEY LABEL :SWITCH ARGUMENT) or (KEY LABEL :OPTION
ARGUMENT :READ READER), :REPLACE T after it for one asked again."
  (destructuring-bind (key label &rest more) spec
    (if (keywordp (first more))
        (destructuring-bind (&key switch option (read 'read-string) replace) more
          `(make-menu-entry ,key ,label :switch ,switch :option ,option :reader ',(and option read) :replace ,replace))
        `(make-menu-entry ,key ,label :command ',(first more)))))

(defmacro define-menu (name (&key title show note summary hold) doc &body groups)
  "Define the command NAME, documented by DOC, that opens a menu under TITLE
of GROUPS, each (GROUP-TITLE ENTRY...), an entry (KEY LABEL COMMAND) for an
action, (KEY LABEL :SWITCH \"--argument\") for a switch, or (KEY LABEL
:OPTION \"--argument=\" :READ READER) for an option, READER a function of a
prompt (READ-STRING when not given), :REPLACE T after it to ask it again
with its value. SHOW, NOTE and SUMMARY are as MAKE-MENU takes them,
function names; HOLD too, true or false."
  (check-type doc string "a docstring; every command documents itself")
  `(progn
     (setf (gethash ',name *menus*)
           (make-menu ',name ,title
                      (list ,@(loop for (group-title . specs) in groups
                                    collect `(cons ,group-title (list ,@(mapcar #'parse-menu-entry specs)))))
                      :show ,(and show `',show) :note ,(and note `',note) :summary ,(and summary `',summary)
                      :hold ,hold))
     (define-command ,name () ,doc (open-menu (gethash ',name *menus*)))))

(defparameter *menu-keeping-commands* '(menu-toggle-switch menu-set-option menu-entry-left-out menu-key-not-here menu-leave)
  "The commands that keep a menu open: those of its switches and options, the
one saying an entry is left out, and in a menu that holds, the one saying a
key is not in it and the one asking whether to leave (which closes it itself).")

(defun open-menu (menu &key arguments)
  "Open MENU: its keys before every other while its switches and options are
typed, its entries in the panel, its arguments ARGUMENTS to begin with (none
unless given, in the order given). The key of an entry it leaves out says so
and keeps it open, never reaching the buffer behind it: an action's as the
menu opens, a switch's or an option's as its key is typed. A menu that holds
\(MENU-HOLD) takes every other key too, and \\`C-g' (MENU-LEAVE)."
  (let ((keymap (make-keymap :name (string-downcase (symbol-name (menu-name menu)))))
        (editor *editor*))
    (dolist (entry (menu-entries menu :all t))
      (define-key keymap (menu-entry-key entry)
        ;; a switch or an option looks whether the menu has it as its key is typed,
        ;; for one may come and go with the others' values
        (cond ((menu-entry-switch entry) 'menu-toggle-switch)
              ((menu-entry-option entry) 'menu-set-option)
              ((not (menu-shows-p menu entry)) 'menu-entry-left-out)
              (t (menu-entry-command entry)))))
    (define-key keymap *prefix-help-key* 'menu-describe)
    (when (menu-hold menu)
      (setf (keymap-default keymap) 'menu-key-not-here)
      (define-key keymap "C-g" 'menu-leave))
    (setf (editor-menu editor) (list menu (reverse arguments) (reverse arguments)))
    (set-transient-map editor keymap
                       (lambda (command) (member command *menu-keeping-commands*))
                       (lambda () (setf (editor-menu editor) nil)))))

(defun menu-arguments ()
  "The arguments the menu open now has on, its switches' and its options'
with their values (--force, --depth=3), in the order they were given; NIL
with no menu open, as when its action is run by M-x."
  (let ((open (and *editor* (editor-menu *editor*))))
    (and open (reverse (second open)))))

(defun menu-given (entry arguments)
  "The one of ARGUMENTS that ENTRY gives: its switch as it is (--force, and
never --force-with-lease), or its option with a value (--depth=3); NIL when
none does."
  (let ((switch (menu-entry-switch entry)) (option (menu-entry-option entry)))
    (cond (switch (find switch arguments :test #'string=))
          (option (find-if (lambda (a) (uiop:string-prefix-p option a)) arguments)))))

(defun menu-entry-argument (entry)
  "The switch or the option ENTRY gives, or NIL for an action."
  (or (menu-entry-switch entry) (menu-entry-option entry)))

(defun menu-entry-typed (&key all)
  "The entry of the open menu whose keys were just typed, among those it has
now, or with ALL among every one; NIL when there is none."
  (let ((open (editor-menu *editor*))
        (typed (tuatara.frontend:key-sequence-description *this-command-keys*)))
    (and open (find typed (menu-entries (first open) :all all) :key #'menu-entry-key :test #'string=))))

(defun menu-open-p (buffer)
  "True while a menu is open, whatever BUFFER is."
  (declare (ignore buffer))
  (and *editor* (editor-menu *editor*) t))

(define-command menu-toggle-switch ()
  "Turn the switch of the open menu whose key was typed on, or off, or say it
is left out of the menu now; the menu stays open."
  (let ((entry (menu-entry-typed)) (open (editor-menu *editor*)))
    (unless entry (menu-entry-left-out))
    (when entry
      (let ((argument (menu-entry-switch entry)))
        (setf (second open) (if (member argument (second open) :test #'string=)
                                (remove argument (second open) :test #'string=)
                                (cons argument (second open))))))))

(define-command menu-set-option ()
  "Give the option of the open menu whose key was typed a value, asked for,
or take its value away when it has one, or ask it again with that value when
the option says so (MENU-ENTRY-REPLACE), or say it is left out of the menu
now; the menu stays open."
  (let ((entry (menu-entry-typed)) (open (editor-menu *editor*)))
    (unless entry (menu-entry-left-out))
    (when entry
      (let* ((argument (menu-entry-option entry))
             (given (menu-given entry (second open))))
        (cond
          ((and given (menu-entry-replace entry))
           (let ((value (funcall (menu-entry-reader entry) (format nil "~a " (menu-entry-label entry))
                                 (subseq given (length argument)))))
             (setf (second open) (remove given (second open) :test #'string=))
             (when (plusp (length value))
               (push (concatenate 'string argument value) (second open)))))
          (given
            (setf (second open) (remove given (second open) :test #'string=)))
          (t
            (let ((value (funcall (menu-entry-reader entry) (format nil "~a " (menu-entry-label entry)))))
              (when (plusp (length value))
                (push (concatenate 'string argument value) (second open))))))))))

(defun menu-changed-p (open)
  "True when the menu OPEN, as EDITOR-MENU has it, has arguments on other than
those it was opened with."
  (let ((now (second open)) (opened (third open)))
    (not (and (subsetp now opened :test #'string=) (subsetp opened now :test #'string=)))))

(define-command menu-key-not-here ()
  "Say that the key just typed is not in the open menu, which stays open, and
that \\`C-g' leaves it."
  (message "~a is not in this menu: C-g leaves it" (tuatara.frontend:key-sequence-description *this-command-keys*)))

(define-command menu-leave ()
  "Leave the open menu, asked first when anything in it was changed; it stays
open unless the answer is yes."
  (let ((open (editor-menu *editor*)))
    (when open
      (if (or (not (menu-changed-p open))
              (handler-case (y-or-n-p* (format nil "Leave ~a, losing what you set?" (menu-title (first open))))
                (quit () nil)))
          (progn (clear-transient-map *editor*) (message "Quit"))
          (message "Still in ~a" (menu-title (first open)))))))

(define-command menu-entry-left-out ()
  "Say that the entry of the open menu whose key was typed is left out of it
now, and how to see it; the menu stays open."
  (let ((open (editor-menu *editor*))
        (entry (menu-entry-typed :all t)))
    (when entry
      (message "~a (~a) is left out of this menu now~@[; ~a~]" (menu-entry-label entry) (menu-entry-key entry)
               (and (menu-note (first open)) (funcall (menu-note (first open)) (first open)))))))

(defun menu-text (menu)
  "What \\`C-h' in MENU lists: its note, then each group's entries, every
one, those it leaves out now marked so, with the command or argument of each."
  (with-output-to-string (out)
    (format out "~a~%" (menu-title menu))
    (let ((note (and (menu-note menu) (funcall (menu-note menu) menu))))
      (when note (format out "~a~%" note)))
    (loop for (title . entries) in (menu-groups menu)
          do (format out "~%~a~%" title)
             (dolist (e entries)
               (format out "  ~8a ~32a ~(~a~)~:[  (not shown now)~;~]~%" (menu-entry-key e) (menu-entry-label e)
                       (or (menu-entry-command e) (menu-entry-argument e))
                       (menu-shows-p menu e))))))

(define-command menu-describe ()
  "List in *Help* every entry of the open menu: its keys, what it says and
the command or argument behind it, those not shown now too."
  (let ((open (editor-menu *editor*)))
    (when (and open *menu-help-function*)
      (funcall *menu-help-function* (menu-text (first open))))))

(defun menu-entry-text (entry arguments)
  "ENTRY as the panel shows it, given the ARGUMENTS on: its label, and for a
switch or an option its argument in brackets, an option's with its value."
  (let ((argument (menu-entry-argument entry)))
    (if argument
        (format nil "~a (~a)" (menu-entry-label entry) (or (menu-given entry arguments) argument))
        (menu-entry-label entry))))

(defun menu-panel-rows (editor width room)
  "The panel rows of EDITOR's open menu, at most ROOM: its title, then each
group with entries shown, its title and its entries in columns, an argument
on in :menu-argument-on and one off in :menu-argument-off, then the lines of
its summary; where they do not all fit, the last row says that \\`C-h' lists
them all."
  (destructuring-bind (menu arguments &rest opened) (editor-menu editor)
    (declare (ignore opened))
    (let* ((summary (and (menu-summary menu)
                         (mapcar (lambda (line) (list (subseq line 0 (min (length line) width)) nil '()))
                                 (funcall (menu-summary menu) (reverse arguments)))))
           (room (max 1 (- room (length summary))))
           (rows (list (group-title-row (menu-title menu) width)))
           ;; one column width for every group, so that they line up
           (column (apply #'key-hint-column-width
                          (loop for (nil . all) in (menu-groups menu)
                                collect (loop for e in all
                                              when (menu-shows-p menu e)
                                                collect (cons (menu-entry-key e) (menu-entry-text e arguments)))))))
      (loop for (title . all) in (menu-groups menu)
            for entries = (remove-if-not (lambda (e) (menu-shows-p menu e)) all)
            when entries
              do (setf rows (append rows (list (list title nil (list (list 0 (length title) :completion-group-title))))
                                    (mapcar (lambda (row) (menu-row-arguments row entries arguments))
                                            (key-hint-rows (mapcar (lambda (e) (cons (menu-entry-key e) (menu-entry-text e arguments)))
                                                                   entries)
                                                           width (max 1 (- room (length rows) 1))
                                                           :column-width column)))))
      (append (subseq rows 0 (min (length rows) room)) summary))))

(defun menu-keys-pending-p (editor)
  "True when the keys EDITOR has pending, if any, begin keys of its open menu's
entries (- before - f), so that the panel goes on showing the menu."
  (let ((pending (editor-pending editor)))
    (or (null pending)
        (let ((typed (format nil "~a " (tuatara.frontend:key-sequence-description pending))))
          (some (lambda (entry) (uiop:string-prefix-p typed (menu-entry-key entry)))
                (menu-entries (first (editor-menu editor)) :all t))))))

(defun menu-row-arguments (row entries arguments)
  "ROW, a panel row of ENTRIES, with the arguments in it marked on or off as ARGUMENTS has them."
  (destructuring-bind (string face spans) row
    (let ((more '()))
      (dolist (e entries)
        (let ((argument (menu-entry-argument e)))
          (when argument
            (let* ((given (menu-given e arguments))
                   (shown (format nil "(~a)" (or given argument)))
                   (at (search (format nil "~a ~a" (menu-entry-label e) shown) string)))
              (when at
                (let ((start (+ at (length (menu-entry-label e)) 1)))
                  (push (list start (+ start (length shown)) (if given :menu-argument-on :menu-argument-off))
                        more)))))))
      (list string face (flatten-spans (append spans more) (length string))))))
