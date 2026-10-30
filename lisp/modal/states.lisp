;;;; lisp/modal/states.lisp --- The machinery of modal editing: states, their keymaps, the buffers' classes.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A buffer is in NORMAL or INSERT state; its major mode's class says how
;;;; modal editing treats it (editing, support, excluded). modal-sync gives the
;;;; editor the keymap of the current buffer's state. Tuatara's own keys for the
;;;; states are the modal layer's (lisp/modal/); *modal-editing* NIL turns the
;;;; states off, every buffer taking its mode's and the global keys.

(in-package #:tuatara.defaults)

(defvar *tuatara-normal-keymap* (make-keymap :name "TUATARA NORMAL")
  "Keymap of the NORMAL state: letters are editing commands.")

(defvar *tuatara-insert-keymap* (make-keymap :name "TUATARA INSERT")
  "Keymap of the INSERT state: empty, so letters insert themselves.")

(defvar *tuatara-support-keymap* (make-keymap :name "TUATARA SUPPORT")
  "Keymap of support buffers (read-only lists, help): the reading half of
NORMAL state (*TUATARA-PROSE-KEYS*) and the \\`SPC' leader, under the buffer's
own keys, which win.")

(defvar *tuatara-list-keymap* (make-keymap :name "TUATARA LIST")
  "Keymap of support buffers that are lists of lines, as the agenda is (their
:TUATARA-SUPPORT-KEYMAP local): only NORMAL's keys that move from line to
line and select and copy whole lines (*TUATARA-LIST-KEYS*) and the \\`SPC'
leader, under the buffer's own keys, which win.")

(defvar *tuatara-calendar-keymap* (make-keymap :name "TUATARA CALENDAR")
  "Keymap of the calendar in the modal editing (its :TUATARA-SUPPORT-KEYMAP
local): moving by days, weeks and months, scrolling, today and back where it
was, % and y to select and copy its text, and the \\`SPC' leader, under the
calendar's own keys; nothing else of NORMAL state.")

(defvar *tuatara-leader-keymap* (make-keymap :name "leader")
  "The \\`SPC' leader map, in NORMAL state: yours alone, empty until your init
file gives it keys (bind-key with :map :leader); the starter init file lists
some, commented out, each a command with a key of its own under C-x or C-c too.")

(defvar *modal-editing* t
  "True for Tuatara's modal editing: NORMAL and INSERT states in editing
buffers, NORMAL's reading keys in support buffers, the leader on \\`SPC'. NIL
turns it off: every buffer then takes the keys of its mode and the global
keys, typing inserts, and every command is still there to bind
or run with \\[execute-extended-command].")

(defvar *mode-line-tuatara* t
  "True to write TUATARA before the modal state in the mode line, TUATARA[N];
NIL for the state alone, [N]. \\[toggle-mode-line-tuatara] switches it.")

(defun buffer-modal-class (&optional (buffer (current-buffer)))
  "How the modal editing treats BUFFER: :editing (NORMAL and INSERT), :support
(NORMAL's keys that read and select, and the leader, under the buffer's own
keys) or :excluded (it holds no keys), :excluded for every buffer while
*MODAL-EDITING* is off. A buffer-local :modal-class overrides what its major
mode inherits."
  (if (not *modal-editing*)
      :excluded
      (or (tuatara.buffer:buffer-local buffer :modal-class)
          (mode-modal-class (buffer-major-mode buffer)))))

;;; State

(defun modal-state (&optional (buffer (current-buffer)))
  "The modal state of BUFFER, :normal or :insert."
  (tuatara.buffer:buffer-local buffer 'modal-state :normal))

(defun set-modal-state (state &optional (buffer (current-buffer)))
  "Put BUFFER into STATE and make the editor consult that state's keymap. What
an INSERT state did, from the command that entered it (\\<normal>\\[tuatara-change]'s
deletion, \\[tuatara-open-below]'s new line) to the one that leaves it, is one
step to undo."
  (let ((before (modal-state buffer)))
    (cond ((and (eq state :insert) (not (eq before :insert)))
           (setf (tuatara.buffer:buffer-local buffer 'insert-undo-since)
                 (if (eq buffer (car *command-undo-since*))
                     (cdr *command-undo-since*)
                     (tuatara.buffer:buffer-undo-stack buffer))))
          ((and (eq before :insert) (not (eq state :insert)))
           (let ((since (tuatara.buffer:buffer-local buffer 'insert-undo-since)))
             (setf (tuatara.buffer:buffer-local buffer 'insert-undo-since) nil)
             (tuatara.buffer:merge-undo-groups buffer since)))))
  (setf (tuatara.buffer:buffer-local buffer 'modal-state) state)
  (modal-sync *editor*))

(defun insert-undo-break (buffer)
  "Before BUFFER is read again from its file: what was typed in its INSERT
state so far one step to undo, the reading again one of its own."
  (when (eq (modal-state buffer) :insert)
    (tuatara.buffer:merge-undo-groups buffer (tuatara.buffer:buffer-local buffer 'insert-undo-since)))
  nil)

(defun insert-undo-restart (buffer)
  "After BUFFER was read again from its file: what is typed in its INSERT state
from now on one step to undo."
  (when (eq (modal-state buffer) :insert)
    (setf (tuatara.buffer:buffer-local buffer 'insert-undo-since) (tuatara.buffer:buffer-undo-stack buffer))))

(defun end-insert-state (buffer)
  "Put BUFFER, a file open already just visited from another buffer, in
NORMAL state when it was left in INSERT state, as \\<insert>\\[tuatara-normal] there
would: what was typed is one step to undo, and a line begun and left blank
loses its blanks."
  (when (eq (modal-state buffer) :insert)
    (when (eq buffer (current-buffer))
      (forget-typed-indentation buffer))
    (set-modal-state :normal buffer)))

(defun modal-sync (editor)
  "Make EDITOR's state keymap and self-insert behaviour match the current buffer's
class and state. In NORMAL state a buffer's own keys come first, when it has
some (its :TUATARA-NORMAL-KEYMAP local, as a quick edit's \\`q'), made to fall
through to NORMAL's. Does nothing until the editor has a window (during setup)."
  (let* ((buffer (and (editor-window editor) (tuatara.display:window-buffer (editor-window editor))))
         (state (and buffer (modal-state buffer))))
    (unless buffer (return-from modal-sync nil))
    (case (buffer-modal-class buffer)
      (:excluded
       ;; modal editing holds no keys: the buffer's mode and the global map decide, letters insert
       (setf (editor-state-keymap editor) nil
             (editor-fallback-keymap editor) nil
             (editor-self-insert-command editor) 'self-insert-command))
      (:support
       ;; the mode's keys first, then NORMAL's reading keys (or the buffer's choice of them) and SPC, then the global map
       (setf (editor-state-keymap editor) nil
             (editor-fallback-keymap editor) (or (tuatara.buffer:buffer-local buffer :tuatara-support-keymap)
                                                 *tuatara-support-keymap*)
             (editor-self-insert-command editor) nil))
      (t (setf (editor-state-keymap editor)
               (if (eq state :insert)
                   *tuatara-insert-keymap*
                   (let ((own (tuatara.buffer:buffer-local buffer :tuatara-normal-keymap)))
                     (if own
                         (progn (setf (keymap-parent own) *tuatara-normal-keymap*) own)
                         *tuatara-normal-keymap*)))
               (editor-fallback-keymap editor) nil
               (editor-self-insert-command editor) (if (eq state :insert) 'self-insert-command nil))))))

