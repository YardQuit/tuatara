;;;; lisp/terminal/emulator.lisp --- The terminal under M-x terminal: a program on a pseudo-terminal, its screen kept by the terminal library.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A program (your shell) runs on a pseudo-terminal; what it prints is read
;;;; by a thread of its own and given to the terminal library (libghostty-vt,
;;;; from Ghostty), which keeps the screen, the scrollback and the program's
;;;; modes. The editor's thread asks for what changed when it draws a frame
;;;; (EMULATOR-FRAME): the reader steps aside while it does, so that however
;;;; much the program prints, the editor never waits more than a few
;;;; milliseconds. What is typed goes to the program through a writer thread
;;;; of its own, so a program that reads nothing never holds the editor up.
;;;; The library is loaded when a terminal is first started, from terminal/
;;;; beside the program or the source tree's build/terminal/
;;;; (tools/terminal-library.sh builds it); layout.sexp beside it says what its
;;;; C headers say of the values and structures read here.

(in-package #:tuatara.defaults)

;;; The library

(defvar *terminal-library-directories* '()
  "Directories searched in order for the terminal library (libghostty-vt.so and
its layout.sexp); when empty, terminal/ beside the program, then the source
tree's build/terminal/.")

(defvar *terminal-layout* nil
  "What the terminal library's headers say (layout.sexp), a property list of
values and offsets, once the library is loaded; NIL before. Set it with
SET-TERMINAL-LAYOUT, which keeps VT's cells in step.")

(defvar *layout-cells* (make-hash-table :test 'eq :synchronized t)
  "A cell (KEY . VALUE) for each key of the layout VT has been asked for, its
value as *TERMINAL-LAYOUT* gives it, so that a key named in the code is read
without looking through the layout.")

(defun layout-cell (key)
  "The cell of *LAYOUT-CELLS* for KEY, made the first time."
  (or (gethash key *layout-cells*)
      (setf (gethash key *layout-cells*) (cons key (getf *terminal-layout* key)))))

(defun set-terminal-layout (layout)
  "Make LAYOUT, a property list read from layout.sexp, the terminal library's
layout, and every cell of *LAYOUT-CELLS* hold its value there."
  (setf *terminal-layout* layout)
  (maphash (lambda (key cell) (setf (cdr cell) (getf layout key))) *layout-cells*)
  layout)

(defun terminal-library-directories ()
  "The directories searched for the terminal library."
  (or *terminal-library-directories* (tuatara.runtime:resource-directories "terminal/" "build/terminal/")))

(defun terminal-library-p ()
  "True when the terminal library is loaded, loading it and reading its
layout the first time it is needed."
  (or (and *terminal-layout* t)
      (let ((directory (find-if (lambda (d) (and (probe-file (merge-pathnames "libghostty-vt.so" d))
                                                 (probe-file (merge-pathnames "layout.sexp" d))))
                                (terminal-library-directories))))
        (when (and directory (foreign-library-p (namestring (merge-pathnames "libghostty-vt.so" directory)))
                   (foreign-library-p (namestring (merge-pathnames "libtuatara-terminal.so" directory))))
          (set-terminal-layout
           (with-open-file (in (merge-pathnames "layout.sexp" directory))
             (let ((*read-eval* nil) (*package* (find-package :keyword))) (read in))))
          t))))

(declaim (inline layout-cell-value))
(defun layout-cell-value (cell)
  "The value CELL, one of *LAYOUT-CELLS*, holds; an error when the layout has none."
  (or (cdr cell) (error "The terminal library's layout says nothing of ~s" (car cell))))

(defun vt (key)
  "The value KEY has in the terminal library's layout (*TERMINAL-LAYOUT*)."
  (layout-cell-value (layout-cell key)))

(define-compiler-macro vt (&whole form key)
  "A key written as a keyword read from its cell, found once when the code is loaded."
  (if (keywordp key)
      `(layout-cell-value (load-time-value (layout-cell ,key)))
      form))

;;; An emulator

(defstruct (emulator (:constructor %make-emulator) (:copier nil))
  "A program running on a pseudo-terminal, its screen kept by the terminal
library: TERMINAL, RENDER-STATE, ITERATOR and CELLS are the library's handles;
ENCODER and EVENT turn keys into what the program reads, MOUSE-ENCODER and
MOUSE-EVENT what the mouse does; PROCESS the program,
FD the pseudo-terminal's side the editor holds. LOCK
guards the library's state; WANTED is set while the editor's thread wants a
frame, the reader waiting on TURN meanwhile; CHANGED is set when the program
printed since the last frame; CLOSED once the program's side is gone. WAKE,
when given, is called when a frame is wanted after quiet. LAST-OUTPUT is when
the program last printed (internal real time); TO-COMPRESS is set while its
scrollback has not been compressed since. CELL-WIDTH and CELL-HEIGHT are a
cell's pixels where known (0 else); PLACEMENTS the library's iterator over the
pictures shown, when pictures are on, PICTURE-MEMORY the most octets of
pictures it keeps, GRAPHICS-GENERATION what the library said of its pictures
when they were last counted (KEEP-PLACEMENTS-WITHIN-BOUNDS), PICTURES-DROPPED
true once they were dropped, PICTURES the pictures made of them for the
editor, by what they were made from, and PICTURES-SHOWN those of them a frame
shows; PLACEHOLDERS, for each row of the screen, the cells standing for a
part of a picture (ROW-PLACEHOLDERS), and PLACEHOLDER-ENDS the table a frame
joins their runs with (PLACEHOLDER-AREAS).
INPUT-LOCK guards the encoders and their events, which hold what the program
last asked of keys and the mouse (SYNC-INPUT-STATE), so that typing never
waits for the library; BRACKETED and SCROLLS-BY-KEYS say, as of the
program's last output, whether it asks for pastes marked and whether the
wheel is to send it the up and down keys. REPLIES counts the octets of the
library's answers to the program waiting in OUTBOX (*TERMINAL-REPLY-LIMIT*)."
  (id 0 :type fixnum) terminal render-state iterator cells encoder event mouse-encoder mouse-event process (fd -1 :type fixnum)
  (last-output 0 :type integer) to-compress
  (cell-width 0 :type fixnum) (cell-height 0 :type fixnum) placements placeholders
  (picture-memory 0 :type (unsigned-byte 64)) (graphics-generation 0 :type (unsigned-byte 64)) pictures-dropped
  (pictures (make-hash-table :test 'equal)) (pictures-shown (make-hash-table :test 'equal))
  (placeholder-ends (make-hash-table))
  (lock (sb-thread:make-mutex :name "terminal"))
  (input-lock (sb-thread:make-mutex :name "terminal input"))
  (turn (sb-thread:make-waitqueue :name "terminal turn"))
  wanted changed closed reader writer bracketed scrolls-by-keys
  (outbox (sb-concurrency:make-mailbox :name "terminal input"))
  (replies 0 :type sb-ext:word)
  wake (columns 80 :type fixnum) (rows 24 :type fixnum))

(defvar *emulators* (make-hash-table :synchronized t)
  "The emulators running, by their number, for the library's callbacks.")

(defvar *emulator-count* (list 0)
  "How many emulators have been started, in its car: the next one's number.")

(defparameter *terminal-environment* '("TERM=xterm-256color" "COLORTERM=truecolor")
  "Variables a terminal's program gets in place of the editor's own of the same names.")

(defparameter *terminal-read-size* 65536
  "The most octets of a program's output read, and given to the library, at a time:
the longest the editor's thread can wait for a frame.")

(defparameter *terminal-reply-limit* (* 1024 1024)
  "The most octets of the terminal's answers to its program (to its question
what the terminal is, or where the cursor is) waiting for the program to read
them. Answers past it are dropped, so that text asking over and over while
the program reads nothing (a file shown with cat) cannot fill the memory;
what you type is never dropped.")

(defun out-pointer (function &rest arguments)
  "Call FUNCTION with ARGUMENTS and a place for a pointer, which it fills;
return the pointer, or signal an error when it returns other than success."
  (sb-alien:with-alien ((place sb-sys:system-area-pointer (tuatara.io:null-pointer)))
    (let ((result (apply function (append arguments (list (sb-alien:alien-sap (sb-alien:addr place)))))))
      (unless (eql result (vt :success)) (error "The terminal library refused (~d)" result)))
    place))

(defmacro vt-new (constructor &rest arguments)
  "Make one of the terminal library's objects with its C function CONSTRUCTOR,
which takes the library's own allocator, a place for the object, then
ARGUMENTS (each a list of an alien type and a value); return the object, or
signal an error when the library refuses."
  `(out-pointer (lambda (place)
                  (foreign-call ,constructor sb-alien:int (sb-sys:system-area-pointer (tuatara.io:null-pointer))
                                (sb-sys:system-area-pointer place) ,@arguments))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *library-objects*
    '((emulator-terminal nil "ghostty_terminal_free")
      (emulator-placements nil "ghostty_kitty_graphics_placement_iterator_free")
      (emulator-render-state "ghostty_render_state_new" "ghostty_render_state_free")
      (emulator-iterator "ghostty_render_state_row_iterator_new" "ghostty_render_state_row_iterator_free")
      (emulator-cells "ghostty_render_state_row_cells_new" "ghostty_render_state_row_cells_free")
      (emulator-encoder "ghostty_key_encoder_new" "ghostty_key_encoder_free")
      (emulator-event "ghostty_key_event_new" "ghostty_key_event_free")
      (emulator-mouse-encoder "ghostty_mouse_encoder_new" "ghostty_mouse_encoder_free")
      (emulator-mouse-event "ghostty_mouse_event_new" "ghostty_mouse_event_free"))
    "The terminal library's objects an emulator holds: each its slot's reader,
the C function that makes it from no more than a place for it (NIL for one
made otherwise: the terminal, which takes its size, and the pictures'
iterator, made only when pictures are on), and the one that frees it."))

(defmacro make-library-objects (emulator)
  "Make EMULATOR's objects of *LIBRARY-OBJECTS* that need no more than a place."
  `(setf ,@(loop for (slot constructor) in *library-objects*
                 when constructor append `((,slot ,emulator) (vt-new ,constructor)))))

(defmacro free-library-objects (emulator)
  "Free each of EMULATOR's objects of *LIBRARY-OBJECTS* it has, the terminal
last, and forget them."
  `(progn ,@(loop for (slot nil free) in (reverse *library-objects*)
                  collect `(let ((object (,slot ,emulator)))
                             (when object
                               (foreign-call ,free sb-alien:void (sb-sys:system-area-pointer object))
                               (setf (,slot ,emulator) nil))))))

(defun set-size-option (terminal option value)
  "Set the terminal library's OPTION of TERMINAL to VALUE, a size."
  (sb-alien:with-alien ((place (sb-alien:unsigned 64) value))
    (foreign-call "ghostty_terminal_set" sb-alien:int (sb-sys:system-area-pointer terminal) (sb-alien:int option)
                  (sb-sys:system-area-pointer (sb-alien:alien-sap (sb-alien:addr place))))))

(defun library-size (terminal what)
  "The size the terminal library keeps of TERMINAL as WHAT (a :data- value of
its layout), or NIL when it gives none."
  (sb-alien:with-alien ((place (sb-alien:unsigned 64) 0))
    (and (eql (vt :success) (foreign-call "ghostty_terminal_get" sb-alien:int (sb-sys:system-area-pointer terminal)
                                          (sb-alien:int what) (sb-sys:system-area-pointer (sb-alien:alien-sap (sb-alien:addr place)))))
         place)))

(sb-alien:define-alien-callable tuatara-terminal-write-pty sb-alien:void
    ((terminal sb-sys:system-area-pointer) (userdata sb-sys:system-area-pointer)
     (data sb-sys:system-area-pointer) (size (sb-alien:unsigned 64)))
  terminal                              ; not used: a callable's body takes no declarations
  (let ((emulator (gethash (sb-sys:sap-int userdata) *emulators*)))
    ;; an answer past the limit dropped: the program reads none of them meanwhile
    (when (and emulator (<= (+ (emulator-replies emulator) size) *terminal-reply-limit*))
      (sb-ext:atomic-incf (emulator-replies emulator) size)
      (sb-concurrency:send-message (emulator-outbox emulator) (cons :reply (tuatara.io:sap-octets data size))))))

(defun set-window-size (fd columns rows &optional (cell-width 0) (cell-height 0))
  "Tell the pseudo-terminal FD its size, COLUMNS by ROWS of cells CELL-WIDTH by
CELL-HEIGHT pixels (0 when not known), which the program reads (TIOCSWINSZ):
a program sending pictures sizes them by it."
  (tuatara.io:set-terminal-window-size fd rows columns (* columns cell-width) (* rows cell-height)))

(defun resize-library (emulator columns rows cell-width cell-height)
  "Make the screen the library keeps for EMULATOR COLUMNS by ROWS, of cells
CELL-WIDTH by CELL-HEIGHT pixels (0 when not known), by which it sizes
pictures given no size in cells. To be called holding the library, or
before its reader starts."
  (foreign-call "ghostty_terminal_resize" sb-alien:int (sb-sys:system-area-pointer (emulator-terminal emulator))
                ((sb-alien:unsigned 16) columns) ((sb-alien:unsigned 16) rows)
                ((sb-alien:unsigned 32) cell-width) ((sb-alien:unsigned 32) cell-height)))

(defun terminal-environment (environment)
  "The variables a terminal's program gets: ENVIRONMENT (NAME=VALUE strings),
or the editor's own, with *TERMINAL-ENVIRONMENT* in place of those it names."
  (let ((names (mapcar (lambda (v) (subseq v 0 (1+ (position #\= v)))) *terminal-environment*)))
    (append *terminal-environment*
            (remove-if (lambda (v) (some (lambda (name) (uiop:string-prefix-p name v)) names))
                       (or environment (sb-ext:posix-environ))))))

(defun start-emulator (program arguments &key (columns 80) (rows 24) directory environment wake
                                            (scrollback-lines 100000) (scrollback-bytes (* 256 1024 1024))
                                            pictures (cell-width 0) (cell-height 0))
  "Run PROGRAM with ARGUMENTS on a pseudo-terminal of COLUMNS by ROWS, in a
session of its own whose controlling terminal it is (setsid), the terminal
set as terminals are (echo on, UTF-8: stty sane iutf8), in
DIRECTORY, ENVIRONMENT its variables (TERMINAL-ENVIRONMENT), its screen kept by the terminal library with
SCROLLBACK-LINES lines of scrollback (SCROLLBACK-BYTES at most), cells of
CELL-WIDTH by CELL-HEIGHT pixels where known. With PICTURES, the pictures the
program sends (the kitty graphics protocol) are kept, *TERMINAL-PICTURE-MEMORY*
octets at most, for EMULATOR-FRAME to give; without, none is kept. WAKE, a
function of no arguments, is called on the reader's thread when the program
printed after quiet, so that a frame is drawn. Returns the EMULATOR; an error
when the library cannot be loaded."
  (unless (terminal-library-p)
    (error "The terminal library is not there (tools/terminal-library.sh builds it)"))
  (let* ((id (1+ (sb-ext:atomic-incf (car *emulator-count*))))
         (terminal (vt-new "ghostty_terminal_new" ((sb-alien:unsigned 16) columns) ((sb-alien:unsigned 16) rows)))
         (emulator (%make-emulator :id id :terminal terminal :columns columns :rows rows :wake wake
                                   :cell-width cell-width :cell-height cell-height)))
    (set-size-option terminal (vt :opt-scrollback-max-lines) scrollback-lines)
    (if pictures
        (allow-pictures emulator)
        (set-size-option terminal (vt :opt-kitty-storage-limit) 0))
    ;; a cell's pixels, by which the library sizes pictures not given a size in cells
    (when (and (plusp cell-width) (plusp cell-height))
      (resize-library emulator columns rows cell-width cell-height))
    (set-size-option terminal (vt :opt-scrollback-max-bytes) scrollback-bytes)
    (foreign-call "ghostty_terminal_set" sb-alien:int (sb-sys:system-area-pointer terminal) (sb-alien:int (vt :opt-userdata))
                  (sb-sys:system-area-pointer (sb-sys:int-sap id)))
    (foreign-call "ghostty_terminal_set" sb-alien:int (sb-sys:system-area-pointer terminal) (sb-alien:int (vt :opt-write-pty))
                  (sb-sys:system-area-pointer (sb-alien:alien-sap (sb-alien:alien-callable-function 'tuatara-terminal-write-pty))))
    (make-library-objects emulator)
    (sync-input-state emulator)
    (setf (gethash id *emulators*) emulator)
    ;; a session of its own, the pseudo-terminal its controlling terminal, for job control (C-z, fg);
    ;; the terminal's usual settings before the program starts (echo on, UTF-8), which a
    ;; program's line editor reads as it starts
    (let ((process (multiple-value-call #'sb-ext:run-program
                     (tuatara.io:setsid-command "/bin/sh" (list* "-c" "stty sane iutf8 2>/dev/null; exec \"$0\" \"$@\"" program arguments)
                                                "-w" "-c")
                     :pty t :wait nil :search t
                     :directory directory
                     :environment (terminal-environment environment))))
      (setf (emulator-process emulator) process
            (emulator-fd emulator) (sb-sys:fd-stream-fd (sb-ext:process-pty process)))
      (set-window-size (emulator-fd emulator) columns rows cell-width cell-height))
    (setf (emulator-reader emulator)
          (tuatara.runtime:make-guarded-thread (lambda () (read-program-output emulator)) :name "tuatara terminal reader" :niceness 0)
          (emulator-writer emulator)
          (tuatara.runtime:make-guarded-thread (lambda () (write-program-input emulator)) :name "tuatara terminal writer" :niceness 0))
    emulator))

(defun read-program-output (emulator)
  "What EMULATOR's reader thread does: read the program's output a block at a
time and give it to the library, stepping aside while the editor wants a
frame, and keep what the program asks of keys, the mouse and pastes
(SYNC-INPUT-STATE); wake the editor when something changed after quiet. Ends
when the program's side of the pseudo-terminal is gone."
  (let ((fd (emulator-fd emulator)))
    (unwind-protect
         (tuatara.io:with-foreign-memory (buffer *terminal-read-size*)
           (loop
             (let ((got (loop (multiple-value-bind (got errno) (sb-unix:unix-read fd buffer *terminal-read-size*)
                                ;; a signal (a program the sandbox ran ending) is no end
                                (unless (and (null got) (eql errno sb-unix:eintr)) (return got))))))
               (when (or (null got) (<= got 0)) (return))
               (sb-thread:with-mutex ((emulator-lock emulator))
                 (loop while (emulator-wanted emulator)
                       do (sb-thread:condition-wait (emulator-turn emulator) (emulator-lock emulator)))
                 (foreign-call "ghostty_terminal_vt_write" sb-alien:void (sb-sys:system-area-pointer (emulator-terminal emulator))
                               (sb-sys:system-area-pointer buffer) ((sb-alien:unsigned 64) got))
                 (when (emulator-placements emulator) (keep-placements-within-bounds emulator))
                 (sync-input-state emulator))
               (setf (emulator-last-output emulator) (get-internal-real-time))
               (unless (emulator-to-compress emulator)
                 (setf (emulator-to-compress emulator) t)
                 (wake-compressor))
               (unless (emulator-changed emulator)
                 (setf (emulator-changed emulator) t)
                 (when (emulator-wake emulator) (ignore-errors (funcall (emulator-wake emulator))))))))
      (setf (emulator-closed emulator) t)
      (when (emulator-wake emulator) (ignore-errors (funcall (emulator-wake emulator)))))))

(defun write-program-input (emulator)
  "What EMULATOR's writer thread does: write what is sent to its outbox to the
program, all of it, until :STOP comes or the program's side is gone; the
library's answers, sent as (:REPLY . OCTETS), counted off its REPLIES as
they are written."
  (let ((fd (emulator-fd emulator)))
    (loop
      (let* ((message (sb-concurrency:receive-message (emulator-outbox emulator)))
             (octets (if (consp message) (cdr message) message)))
        (when (eq message :stop) (return))
        (handler-case (tuatara.io:write-octets fd octets :path "a terminal")
          (tuatara.io:file-system-error () (return)))
        (when (consp message)
          (sb-ext:atomic-decf (emulator-replies emulator) (length octets)))))))

(defun emulator-input (emulator octets)
  "Send OCTETS (or a string, as UTF-8) to EMULATOR's program, as typed; the
editor never waits for the program to read them."
  (unless (emulator-closed emulator)
    (sb-concurrency:send-message (emulator-outbox emulator)
                                 (if (stringp octets) (tuatara.text:string-octets octets) (copy-seq octets)))))

(defmacro with-emulator-turn ((emulator &key (wait t)) &body body)
  "Run BODY holding EMULATOR's library state, the reader stepping aside. With
WAIT false, only when the library is free at once (the reader may hold it a
while, a picture being drawn): else BODY is not run and the value is :BUSY."
  (let ((e (gensym "EMULATOR")) (run (gensym "BODY")))
    `(let ((,e ,emulator))
       (flet ((,run () ,@body))
         (if (not ,wait)
             ;; WANTED left alone: set and cleared without the lock, the reader could miss its wake
             (if (sb-thread:grab-mutex (emulator-lock ,e) :waitp nil)
                 (unwind-protect (,run) (sb-thread:release-mutex (emulator-lock ,e)))
                 :busy)
             (progn
               (setf (emulator-wanted ,e) t)
               (unwind-protect
                    (sb-thread:with-mutex ((emulator-lock ,e))
                      (unwind-protect (,run)
                        (setf (emulator-wanted ,e) nil)
                        (sb-thread:condition-broadcast (emulator-turn ,e))))
                 (setf (emulator-wanted ,e) nil))))))))

(defmacro with-emulator-input ((emulator) &body body)
  "Run BODY holding EMULATOR's encoders and their events (its INPUT-LOCK),
while it has them; NIL when it has ended. Never waits for the library, which
the reader may hold a while (a picture being drawn): the encoders hold what
the program last asked (SYNC-INPUT-STATE)."
  (let ((e (gensym "EMULATOR")))
    `(let ((,e ,emulator))
       (sb-thread:with-mutex ((emulator-input-lock ,e))
         (when (emulator-terminal ,e) ,@body)))))

(defun emulator-mode-p (emulator mode)
  "True when EMULATOR's program has MODE on, a mode as the terminal library
numbers it (:mode-bracketed-paste of its layout, say). To be called holding
the library (WITH-EMULATOR-TURN)."
  (tuatara.io:with-foreign-memory (sap (vt :mode-config-size) :zero t)
    (setf (sb-sys:sap-ref-16 sap (vt :mode-config-mode)) mode)
    (and (eql (vt :success) (foreign-call "ghostty_terminal_get" sb-alien:int (sb-sys:system-area-pointer (emulator-terminal emulator))
                                          (sb-alien:int (vt :data-mode)) (sb-sys:system-area-pointer sap)))
         (/= 0 (sb-sys:sap-ref-8 sap (vt :mode-config-value))))))

(defun sync-input-state (emulator)
  "Have EMULATOR's encoders follow what its program asks of keys and the
mouse now (its cursor keys' mode, the kitty keyboard protocol, its mouse
tracking and the rest), and keep in its BRACKETED whether it asks for pastes
marked, and in its SCROLLS-BY-KEYS whether the wheel is to send it the up
and down keys: a full-screen program (on the alternate screen) that does not
track the mouse, while it leaves alternate scrolling on (mode 1007), such as
the pager less. To be called holding the library, once its output has been
given to it, so that typing never waits for the library."
  (let ((terminal (emulator-terminal emulator)))
    (sb-thread:with-mutex ((emulator-input-lock emulator))
      (foreign-call "ghostty_key_encoder_setopt_from_terminal" sb-alien:void
                    (sb-sys:system-area-pointer (emulator-encoder emulator)) (sb-sys:system-area-pointer terminal))
      (foreign-call "ghostty_mouse_encoder_setopt_from_terminal" sb-alien:void
                    (sb-sys:system-area-pointer (emulator-mouse-encoder emulator)) (sb-sys:system-area-pointer terminal)))
    (setf (emulator-bracketed emulator) (emulator-mode-p emulator (vt :mode-bracketed-paste))
          (emulator-scrolls-by-keys emulator) (and (eql (library-size terminal (vt :data-active-screen)) (vt :screen-alternate))
                                                   (emulator-mode-p emulator (vt :mode-alternate-scroll))))))

(defun library-mods (ctrl meta shift)
  "The terminal library's bits for the keys held with a key or the mouse:
CTRL, META (Alt) and SHIFT, each true when held."
  (logior (if ctrl (vt :mods-ctrl) 0) (if meta (vt :mods-alt) 0) (if shift (vt :mods-shift) 0)))

(defun send-encoded (emulator size encode)
  "Send EMULATOR's program what ENCODE makes: ENCODE, a function of a place
for SIZE octets and a place for how many of them it wrote, returns the
library's result. True when it made any octets."
  (let ((octets (tuatara.io:with-foreign-memory (out size)
                  (sb-alien:with-alien ((length (sb-alien:unsigned 64) 0))
                    (and (eql (vt :success) (funcall encode out (sb-alien:alien-sap (sb-alien:addr length))))
                         (plusp length)
                         (tuatara.io:sap-octets out (min length size)))))))
    (when octets
      (emulator-input emulator octets)
      t)))

(defun physical-key (key)
  "The terminal library's physical key for KEY (a Tuatara key), by its base: a
letter, digit or punctuation of a US keyboard, or a named key; 0 (none) for
another character, which then goes by its text alone."
  (let ((base (tuatara.frontend:key-base key)))
    (or (cdr (assoc (if (characterp base) (string (char-downcase base)) (format nil "~(~s~)" base))
                    (vt :keys) :test #'string=))
        0)))

(defun emulator-key (emulator key)
  "Send EMULATOR's program KEY, a Tuatara key, as the program asks keys to come
now (its cursor keys' mode, the kitty keyboard protocol and the rest, as of
its last output), never waiting for the library. Returns true when the key
means something to a terminal."
  (let* ((base (tuatara.frontend:key-base key))
         (text (cond ((and (characterp base) (not (tuatara.frontend:key-ctrl key))) (string base))
                     ((eq base :enter) (string #\Return))
                     ((eq base :tab) (string #\Tab))))
         (physical (physical-key key)))
    (when (or text (plusp physical))
      (with-emulator-input (emulator)
        (let ((event (emulator-event emulator))
              (utf8 (and text (sb-ext:string-to-octets text :external-format :utf-8 :null-terminate t))))
          (foreign-call "ghostty_key_event_set_action" sb-alien:void (sb-sys:system-area-pointer event) (sb-alien:int (vt :key-press)))
          (foreign-call "ghostty_key_event_set_key" sb-alien:void (sb-sys:system-area-pointer event) (sb-alien:int physical))
          (foreign-call "ghostty_key_event_set_mods" sb-alien:void (sb-sys:system-area-pointer event)
                        ((sb-alien:unsigned 16) (library-mods (tuatara.frontend:key-ctrl key) (tuatara.frontend:key-meta key)
                                                              (or (tuatara.frontend:key-shift key)
                                                                  (and (characterp base) (upper-case-p base))))))
          (foreign-call "ghostty_key_event_set_unshifted_codepoint" sb-alien:void (sb-sys:system-area-pointer event)
                        ((sb-alien:unsigned 32) (if (characterp base) (char-code (char-downcase base)) 0)))
          (sb-sys:with-pinned-objects (utf8)
            (foreign-call "ghostty_key_event_set_utf8" sb-alien:void (sb-sys:system-area-pointer event)
                          (sb-sys:system-area-pointer (if utf8 (sb-sys:vector-sap utf8) (tuatara.io:null-pointer)))
                          ((sb-alien:unsigned 64) (if utf8 (1- (length utf8)) 0)))
            (send-encoded emulator 128
                          (lambda (out length)
                            (foreign-call "ghostty_key_encoder_encode" sb-alien:int
                                          (sb-sys:system-area-pointer (emulator-encoder emulator)) (sb-sys:system-area-pointer event)
                                          (sb-sys:system-area-pointer out) ((sb-alien:unsigned 64) 128)
                                          (sb-sys:system-area-pointer length))))))))))

(defun emulator-bracketed-paste-p (emulator)
  "True when EMULATOR's program asks for pastes marked as pasted (bracketed
paste), as of its last output, so that a shell runs none of a paste's lines
until you press RET; never waits for the library."
  (emulator-bracketed emulator))

(defun emulator-paste (emulator text)
  "Paste TEXT into EMULATOR's program as terminals paste: marked as pasted when
the program asks for that (EMULATOR-BRACKETED-PASTE-P, so that a shell runs
none of its lines until you press RET), its control characters (Escape among
them) made spaces so that pasted text cannot work the terminal, its line ends
carriage returns when not marked. The editor never waits for the program to
read it, nor for the library. Returns true when it was marked."
  (let* ((data (tuatara.text:string-octets text))
         (room (+ (length data) 64))
         (out (make-array room :element-type '(unsigned-byte 8)))
         (bracketed (emulator-bracketed-paste-p emulator)))
    (sb-alien:with-alien ((written (sb-alien:unsigned 64) 0))
      (sb-sys:with-pinned-objects (data out)
        (unless (eql (vt :success)
                     (foreign-call "ghostty_paste_encode" sb-alien:int
                                   (sb-sys:system-area-pointer (sb-sys:vector-sap data)) ((sb-alien:unsigned 64) (length data))
                                   ((sb-alien:boolean 8) bracketed)
                                   (sb-sys:system-area-pointer (sb-sys:vector-sap out)) ((sb-alien:unsigned 64) room)
                                   (sb-sys:system-area-pointer (sb-alien:alien-sap (sb-alien:addr written)))))
          (error "The terminal library could not make the paste")))
      (emulator-input emulator (subseq out 0 written)))
    bracketed))

(defun emulator-mouse (emulator action button column row &key ctrl meta shift pressed)
  "Tell EMULATOR's program what the mouse did at cell COLUMN, ROW of its screen
\(from 0), when it has asked to hear of the mouse, never waiting for the
library. ACTION is :press, :release or :motion; BUTTON :left,
:middle, :wheel-up, :wheel-down or NIL; CTRL, META and SHIFT the keys held; PRESSED
true while a button is held. True when the program was told; NIL when it
does not track the mouse."
  (with-emulator-input (emulator)
    (send-encoded emulator 64
                  (lambda (out length)
                    ;; the library's arithmetic on positions, done as C does it
                    (sb-int:with-float-traps-masked (:overflow :invalid :divide-by-zero :inexact :underflow)
                      (foreign-call "tuatara_terminal_mouse" sb-alien:int
                                    (sb-sys:system-area-pointer (emulator-mouse-encoder emulator))
                                    (sb-sys:system-area-pointer (emulator-mouse-event emulator))
                                    (sb-alien:int (vt (ecase action (:press :mouse-press) (:release :mouse-release) (:motion :mouse-motion))))
                                    (sb-alien:int (if button (vt (ecase button (:left :mouse-left) (:middle :mouse-middle) (:wheel-up :mouse-wheel-up) (:wheel-down :mouse-wheel-down))) 0))
                                    (sb-alien:int (library-mods ctrl meta shift))
                                    (sb-alien:int column) (sb-alien:int row)
                                    (sb-alien:int (emulator-columns emulator)) (sb-alien:int (emulator-rows emulator))
                                    (sb-alien:int (if pressed 1 0))
                                    (sb-sys:system-area-pointer out) ((sb-alien:unsigned 64) 64)
                                    (sb-sys:system-area-pointer length)))))))

(defun emulator-link (emulator column row &key history)
  "The address of the link (OSC 8, as ls --hyperlink and many programs write
them) on the cell at COLUMN, ROW of EMULATOR's screen as it shows, or with
HISTORY at ROW of its whole history, scrollback first; NIL when it has none."
  (with-emulator-turn (emulator)
    (when (emulator-terminal emulator)
      (sb-alien:with-alien ((length (sb-alien:unsigned 64) 0))
        (flet ((link (out size)
                 (foreign-call "tuatara_terminal_link" sb-alien:int (sb-sys:system-area-pointer (emulator-terminal emulator))
                               (sb-alien:int (if history 1 0)) (sb-alien:int column) (sb-alien:int row)
                               (sb-sys:system-area-pointer out) ((sb-alien:unsigned 64) size)
                               (sb-sys:system-area-pointer (sb-alien:alien-sap (sb-alien:addr length))))))
          (let ((result (link (tuatara.io:null-pointer) 0)))
            (when (and (member result (list (vt :success) (vt :out-of-space))) (plusp length))
              (let ((octets (make-array length :element-type '(unsigned-byte 8))))
                (when (eql (vt :success) (tuatara.io:with-octets-sap (sap octets) (link sap (length octets))))
                  (tuatara.text:octets-string (subseq octets 0 (min length (length octets)))))))))))))

(defun emulator-scrolls-by-keys-p (emulator)
  "True when the wheel over EMULATOR's screen is to send its program the
up and down keys: a full-screen program (on the alternate screen) that does
not track the mouse, while it leaves alternate scrolling on (mode 1007),
such as the pager less; as of its last output, never waiting for the library."
  (emulator-scrolls-by-keys emulator))

(defun emulator-resize (emulator columns rows &optional (cell-width 0) (cell-height 0))
  "Make EMULATOR's screen COLUMNS by ROWS (cells of CELL-WIDTH by CELL-HEIGHT
pixels, when known), and tell its program. True when that changed it;
NIL, nothing done, when it is that size already; :BUSY, nothing done, when
the reader holds the library (a picture being drawn): never waited for, to be
tried again."
  (unless (and (= columns (emulator-columns emulator)) (= rows (emulator-rows emulator))
               (= cell-width (emulator-cell-width emulator)) (= cell-height (emulator-cell-height emulator)))
    (when (eq :busy (with-emulator-turn (emulator :wait nil)
                      (resize-library emulator columns rows cell-width cell-height)
                      t))
      (return-from emulator-resize :busy))
    (setf (emulator-columns emulator) columns (emulator-rows emulator) rows
          (emulator-cell-width emulator) cell-width (emulator-cell-height emulator) cell-height)
    (unless (emulator-closed emulator) (set-window-size (emulator-fd emulator) columns rows cell-width cell-height))
    t))

;;; A cell's look, as a number

(defconstant +colour-given+ #x2000000
  "The bit of a colour code set when it gives a colour at all: 0 is the
terminal's own colour.")

(defconstant +colour-palette+ #x1000000
  "The bit of a colour code set when its low octet is an index of the
256-colour palette; clear when its low three octets are red, green and blue.")

(defconstant +look-bg-shift+ 26
  "Where a look's ground colour starts among its bits, its text's colour the bits below.")

(defconstant +look-attributes-shift+ 52
  "Where a look's attributes start among its bits, each its bit by its place
in *LOOK-ATTRIBUTES*.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *look-attributes* '(:bold :italic :faint :inverse :invisible :strike :underline :link)
    "The attributes a cell's look may have, in the order of their bits."))

(defmacro look-bit (attribute)
  "The bit of a look standing for ATTRIBUTE, one of *LOOK-ATTRIBUTES*."
  (ash 1 (+ +look-attributes-shift+ (or (position attribute *look-attributes*)
                                         (error "~s is not an attribute of a look" attribute)))))

(declaim (inline rgb-code palette-code sap-rgb-code style-colour))
(defun rgb-code (red green blue)
  "The colour code of RED, GREEN and BLUE, each 0 to 255."
  (logior +colour-given+ (ash red 16) (ash green 8) blue))

(defun palette-code (index)
  "The colour code of INDEX of the 256-colour palette."
  (logior +colour-given+ +colour-palette+ (logand index #xff)))

(defun code-colour (code)
  "The colour CODE stands for: an index of the 256-colour palette, #rrggbb, or
NIL for the terminal's own."
  (cond ((zerop code) nil)
        ((logtest code +colour-palette+) (logand code #xff))
        (t (tuatara.display:colour-hex (list (ldb (byte 8 16) code) (ldb (byte 8 8) code) (ldb (byte 8 0) code))))))

(declaim (inline make-look))
(defun make-look (fg bg &optional (attributes 0))
  "A cell's look as a number: FG and BG colour codes, the text's and the
ground's (RGB-CODE, PALETTE-CODE, 0 for none), ATTRIBUTES their bits
\(LOOK-BIT); 0 is no look."
  (logior fg (ash bg +look-bg-shift+) attributes))

(defun look-list (look)
  "LOOK, a number (MAKE-LOOK), as a list (FG BG . ATTRIBUTES): FG and BG an
index of the palette, #rrggbb, or NIL for the terminal's own, ATTRIBUTES among
*LOOK-ATTRIBUTES*; NIL for no look."
  (unless (zerop look)
    (list* (code-colour (ldb (byte +look-bg-shift+ 0) look))
           (code-colour (ldb (byte +look-bg-shift+ +look-bg-shift+) look))
           (loop for attribute in *look-attributes*
                 for bit from +look-attributes-shift+
                 when (logbitp bit look) collect attribute))))

(defun sap-rgb-code (sap)
  "The colour code of three octets at SAP, red, green and blue."
  (rgb-code (sb-sys:sap-ref-8 sap 0) (sb-sys:sap-ref-8 sap 1) (sb-sys:sap-ref-8 sap 2)))

(defun style-colour (style offset)
  "The colour code of the style at STYLE whose colour starts at OFFSET: of an
index of the 256-colour palette (the terminal's own, as the frontend shows it),
of red, green and blue, or 0 for none."
  (let ((tag (sb-sys:signed-sap-ref-32 style (+ offset (vt :color-tag))))
        (value (sb-sys:sap+ style (+ offset (vt :color-value)))))
    (cond ((= tag (vt :color-palette)) (palette-code (sb-sys:sap-ref-8 value 0)))
          ((= tag (vt :color-rgb)) (sap-rgb-code value))
          (t 0))))

(declaim (inline cells-get cell-look))
(defun cells-get (cells what place)
  "Have the terminal library put in PLACE what WHAT (a :cell- value of its
layout) is of the cell CELLS, its iterator over a row's cells, stands at;
true when it did."
  (eql (vt :success) (foreign-call "ghostty_render_state_row_cells_get" sb-alien:int (sb-sys:system-area-pointer cells)
                                   (sb-alien:int what) (sb-sys:system-area-pointer place))))

(defun cell-look (cells style colour)
  "The look of the cell CELLS stands at, as a number (MAKE-LOOK, LOOK-LIST):
its colours and its attributes among :bold :italic :faint :inverse :invisible
:strike and :underline (EMULATOR-FRAME adds :link for a cell with a link); 0
for a cell of no look. STYLE and COLOUR are places for the library to fill."
  (let ((fg 0) (bg 0) (attributes 0))
    (setf (sb-sys:sap-ref-8 colour 0) 0)
    (when (and (cells-get cells (vt :cell-has-styling) colour) (/= 0 (sb-sys:sap-ref-8 colour 0)))
      (setf (sb-sys:sap-ref-64 style 0) (vt :style-size))
      (when (cells-get cells (vt :cell-style) style)
        (setf fg (style-colour style (vt :style-fg))
              bg (style-colour style (vt :style-bg)))
        (macrolet ((on (offset attribute)
                     `(when (/= 0 (sb-sys:sap-ref-8 style ,offset)) (setf attributes (logior attributes (look-bit ,attribute))))))
          (on (vt :style-bold) :bold)
          (on (vt :style-italic) :italic)
          (on (vt :style-faint) :faint)
          (on (vt :style-inverse) :inverse)
          (on (vt :style-invisible) :invisible)
          (on (vt :style-strikethrough) :strike))
        (when (/= 0 (sb-sys:signed-sap-ref-32 style (vt :style-underline)))
          (setf attributes (logior attributes (look-bit :underline))))))
    ;; a ground given to the cell itself, as erasing with a colour leaves it
    (when (and (zerop bg) (cells-get cells (vt :cell-bg-color) colour))
      (setf bg (sap-rgb-code colour)))
    (make-look fg bg attributes)))

(defconstant +cell-codes-room+ 128
  "The most code points of one cell the frame reads: a character and the marks
on it. The library keeps fewer than this for a cell.")

(defconstant +cell-codes-shown+ 32
  "The most code points of one cell shown: a character and its first marks.")

(defun emulator-frame (emulator function &key all (wait t))
  "Take what changed on EMULATOR's screen since the last frame, or with ALL the
whole screen (when it is first shown, say): call FUNCTION
with each changed row's number from 0, its text (a wide character once, as
the editor shows it, two columns wide) and its looks, a list of (START END
LOOK) by characters, LOOK as CELL-LOOK gives it, cells of no look left out.
Returns the cursor: its column and row, whether it is shown, and its shape
\(:bar :block :underline or :hollow), as four values (NIL for the column and
row when it is off the screen), and as a fifth the pictures on the screen
\(FRAME-PICTURES) when they are kept. Holds the library a moment, the reader
stepping aside meanwhile; with WAIT false, only when the library is free at
once, else nothing is taken and the value is :BUSY."
  (with-emulator-turn (emulator :wait wait)
    (setf (emulator-changed emulator) nil)
    (unless (and (emulator-placeholders emulator) (= (length (emulator-placeholders emulator)) (emulator-rows emulator)))
      (setf (emulator-placeholders emulator) (make-array (emulator-rows emulator) :initial-element '())
            all t))
    (let* ((rs (emulator-render-state emulator))
           (iterator (emulator-iterator emulator))
           (cells (emulator-cells emulator)))
      (tuatara.io:with-foreign-memory (scratch (+ 64 (vt :style-size) 64))
        (tuatara.io:with-foreign-memory (codes (* 4 +cell-codes-room+))
          (let ((style (sb-sys:sap+ scratch 16)) (colour (sb-sys:sap+ scratch 32)) (text (make-string-output-stream)))
             (foreign-call "ghostty_render_state_update" sb-alien:int (sb-sys:system-area-pointer rs)
                      (sb-sys:system-area-pointer (emulator-terminal emulator)))
             (setf (sb-sys:sap-ref-sap scratch 0) iterator)
             (foreign-call "ghostty_render_state_get" sb-alien:int (sb-sys:system-area-pointer rs) (sb-alien:int (vt :rs-row-iterator))
                           (sb-sys:system-area-pointer scratch))
             (sb-alien:with-alien ((y (sb-alien:unsigned 16) 0))
              (let ((row -1))
                    (loop while (if all
                                    (foreign-call "ghostty_render_state_row_iterator_next" (sb-alien:boolean 8)
                                             (sb-sys:system-area-pointer iterator))
                                    (foreign-call "ghostty_render_state_row_iterator_next_dirty" (sb-alien:boolean 8)
                                             (sb-sys:system-area-pointer iterator) (sb-sys:system-area-pointer (sb-alien:alien-sap (sb-alien:addr y)))))
                          do (incf row)
                             (let* ((links (progn
                                             ;; whether the row has links (OSC 8), asked of each cell only then
                                             (setf (sb-sys:sap-ref-64 scratch 0) 0 (sb-sys:sap-ref-8 scratch 8) 0)
                                             (and (eql (vt :success)
                                                       (foreign-call "ghostty_render_state_row_get" sb-alien:int (sb-sys:system-area-pointer iterator)
                                                                (sb-alien:int (vt :row-raw)) (sb-sys:system-area-pointer scratch)))
                                                  (eql (vt :success)
                                                       (foreign-call "ghostty_row_get" sb-alien:int ((sb-alien:unsigned 64) (sb-sys:sap-ref-64 scratch 0))
                                                                (sb-alien:int (vt :raw-row-link)) (sb-sys:system-area-pointer (sb-sys:sap+ scratch 8))))
                                                  (/= 0 (sb-sys:sap-ref-8 scratch 8)))))
                                    (_ (progn (setf (sb-sys:sap-ref-sap scratch 0) cells)
                                              (foreign-call "ghostty_render_state_row_get" sb-alien:int (sb-sys:system-area-pointer iterator)
                                                       (sb-alien:int (vt :row-cells)) (sb-sys:system-area-pointer scratch))))
                                    (looks '()) (column 0) (look 0) (from 0)
                                    (at (if all row y)) (placeholders '()) (marks nil) (cell -1) (shown 1))
                               (declare (ignore _))
                               (loop while (foreign-call "ghostty_render_state_row_cells_next" (sb-alien:boolean 8) (sb-sys:system-area-pointer cells))
                                     do (setf (sb-sys:sap-ref-32 scratch 0) 0)
                                        (incf cell)
                                        (cells-get cells (vt :cell-graphemes-len) scratch)
                                        (let ((n (sb-sys:sap-ref-32 scratch 0)))
                                          (unless (and (zerop n)
                                                       ;; the second cell of a wide character: it is drawn by the first
                                                       (progn (setf (sb-sys:sap-ref-64 scratch 0) 0)
                                                              (cells-get cells (vt :cell-raw) scratch)
                                                              (let ((raw (sb-sys:sap-ref-64 scratch 0)))
                                                                (setf (sb-sys:sap-ref-32 scratch 8) 0)
                                                                (foreign-call "ghostty_cell_get" sb-alien:int ((sb-alien:unsigned 64) raw)
                                                                         (sb-alien:int (vt :raw-wide)) (sb-sys:system-area-pointer (sb-sys:sap+ scratch 8)))
                                                                (let ((wide (sb-sys:sap-ref-32 scratch 8)))
                                                                  (or (= wide (vt :wide-spacer-tail)) (= wide (vt :wide-spacer-head)))))))
                                            (setf shown 1)
                                            (cond
                                              ((zerop n) (write-char #\Space text))
                                              ;; more marks on one character than room for them (text made to
                                              ;; garble screens): one stand-in, never more than the room written
                                              ((> n +cell-codes-room+) (write-char (code-char #xfffd) text))
                                              (t
                                                  (cells-get cells (vt :cell-graphemes-buf) codes)
                                                  (setf marks nil)
                                                  (if (= (sb-sys:sap-ref-32 codes 0) tuatara.frontend:+kitty-placeholder+)
                                                      ;; a cell of a picture in text: a space, the picture drawn over it
                                                      (progn (write-char #\Space text)
                                                             (setf marks (cons :placeholder
                                                                               (loop for i from 1 below (min n 4)
                                                                                     collect (sb-sys:sap-ref-32 codes (* 4 i))))))
                                                      (progn
                                                        (setf shown (min n +cell-codes-shown+))
                                                        (dotimes (i shown)
                                                          (let ((code (sb-sys:sap-ref-32 codes (* 4 i))))
                                                            (write-char (if (< code char-code-limit) (code-char code) (code-char #xfffd)) text)))))))
                                            (let ((now (cell-look cells style colour))
                                                  (end (+ column shown)))
                                              (when (and links
                                                         (progn (setf (sb-sys:sap-ref-64 scratch 0) 0 (sb-sys:sap-ref-8 scratch 8) 0)
                                                                (cells-get cells (vt :cell-raw) scratch)
                                                                (foreign-call "ghostty_cell_get" sb-alien:int ((sb-alien:unsigned 64) (sb-sys:sap-ref-64 scratch 0))
                                                                         (sb-alien:int (vt :raw-has-link)) (sb-sys:system-area-pointer (sb-sys:sap+ scratch 8)))
                                                                (/= 0 (sb-sys:sap-ref-8 scratch 8))))
                                                (setf now (logior now (look-bit :link))))
                                              (when marks
                                                (push (list cell (ldb (byte +look-bg-shift+ 0) now) (rest marks)) placeholders)
                                                (setf marks nil))
                                              (unless (= now look)
                                                (when (/= look 0) (push (list from column look) looks))
                                                (setf look now from column))
                                              (setf column end)))))
                               (when (/= look 0) (push (list from column look) looks))
                               (let ((kept (emulator-placeholders emulator)))
                                 (when (and kept (< -1 at (length kept)))
                                   (setf (aref kept at) (row-placeholders (nreverse placeholders)))))
                               (funcall function at (get-output-stream-string text) (nreverse looks))))))
             (foreign-call "ghostty_render_state_clean" sb-alien:int (sb-sys:system-area-pointer rs))
             (multiple-value-bind (x y visible shape) (render-cursor emulator)
               (values x y visible shape (and (emulator-placements emulator) (frame-pictures emulator))))))))))

(defun render-cursor (emulator)
  "Where the cursor of EMULATOR's screen is, as its render state was last
updated: its column and row (NIL for both when it is off the screen),
whether it is shown, and its shape (:bar :block :underline or :hollow), as
four values. To be called holding the library."
  (tuatara.io:with-foreign-memory (cursor (vt :cursor-size) :zero t)
    (setf (sb-sys:sap-ref-64 cursor 0) (vt :cursor-size))
    (foreign-call "ghostty_render_state_get" sb-alien:int (sb-sys:system-area-pointer (emulator-render-state emulator))
                  (sb-alien:int (vt :rs-cursor)) (sb-sys:system-area-pointer cursor))
    (let ((on (/= 0 (sb-sys:sap-ref-8 cursor (vt :cursor-has-value)))))
      (values (and on (sb-sys:sap-ref-16 cursor (vt :cursor-x)))
              (and on (sb-sys:sap-ref-16 cursor (vt :cursor-y)))
              (/= 0 (sb-sys:sap-ref-8 cursor (vt :cursor-visible)))
              (let ((shape (sb-sys:signed-sap-ref-32 cursor (vt :cursor-style))))
                (cond ((= shape (vt :cursor-bar)) :bar) ((= shape (vt :cursor-underline)) :underline)
                      ((= shape (vt :cursor-hollow)) :hollow) (t :block)))))))

(defun emulator-text (emulator &key styled)
  "The whole of EMULATOR's screen as text, its scrollback first, a line a row,
the spaces at their ends left out; with STYLED its colours and looks as the
escape sequences that make them (ANSI-PIECES reads them). Three values: the
text, how many rows of it are scrollback, and the row of the screen the
cursor is on. Holds the library while it is made, the reader stepping aside,
so to be called away from the editor's thread for a long scrollback."
  (with-emulator-turn (emulator)
    (let ((terminal (emulator-terminal emulator)))
      (sb-alien:with-alien ((out sb-sys:system-area-pointer (tuatara.io:null-pointer))
                            (length (sb-alien:unsigned 64) 0))
        (unless (eql (vt :success)
                     (foreign-call "tuatara_terminal_text" sb-alien:int (sb-sys:system-area-pointer terminal)
                                   (sb-alien:int (if styled 1 0))
                                   (sb-sys:system-area-pointer (sb-alien:alien-sap (sb-alien:addr out)))
                                   (sb-sys:system-area-pointer (sb-alien:alien-sap (sb-alien:addr length)))))
          (error "The terminal's text could not be made"))
        (let ((octets (if (zerop length)
                          (make-array 0 :element-type '(unsigned-byte 8))
                          (prog1 (tuatara.io:sap-octets out length)
                            (foreign-call "tuatara_terminal_text_free" sb-alien:void (sb-sys:system-area-pointer out)
                                          ((sb-alien:unsigned 64) length))))))
          (foreign-call "ghostty_render_state_update" sb-alien:int
                        (sb-sys:system-area-pointer (emulator-render-state emulator)) (sb-sys:system-area-pointer terminal))
          (values (tuatara.text:octets-string octets)
                  (or (library-size terminal (vt :data-scrollback-rows)) 0)
                  (nth-value 1 (render-cursor emulator))))))))

(defun emulator-title (emulator)
  "The title EMULATOR's program gave its terminal (as a shell names its
window, with OSC 0 or 2), or NIL when it gave none; :BUSY when the library
could not be had at once."
  (with-emulator-turn (emulator :wait nil)
    (when (emulator-terminal emulator)
      (tuatara.io:with-foreign-memory (sap (vt :string-size) :zero t)
        (when (eql (vt :success) (foreign-call "ghostty_terminal_get" sb-alien:int (sb-sys:system-area-pointer (emulator-terminal emulator))
                                               (sb-alien:int (vt :data-title)) (sb-sys:system-area-pointer sap)))
          (let ((n (sb-sys:sap-ref-64 sap (vt :string-length))))
            (when (plusp n)
              (tuatara.text:octets-string (tuatara.io:sap-octets (sb-sys:sap-ref-sap sap (vt :string-pointer)) n)))))))))

(defun emulator-directory (emulator)
  "The directory EMULATOR's program (your shell) is in now, ending in /, or NIL
when it cannot be told: what the system says of the program's own process,
so that it follows cd whatever the shell is and however it is set up."
  (let ((process (emulator-process emulator)))
    (when (and process (not (emulator-closed emulator)))
      (let ((directory (ignore-errors (sb-posix:readlink (format nil "/proc/~d/cwd" (program-group process))))))
        (and directory (char= (char directory 0) #\/) (tuatara.io:slash-ended directory))))))

;;; Pictures programs send (kitty's graphics protocol)

(defvar tuatara-user:*terminal-picture-memory* (* 64 1024 1024)
  "The most octets of pictures a terminal keeps for the programs in it
that draw them; 0 takes none, programs then drawing with text.")

(defparameter *terminal-picture-largest* 4096
  "The most pixels across or down a PNG picture a terminal's program sends is
drawn with, in the sandbox.")

(defvar *png-decoder-set* nil
  "True once the terminal library draws PNG pictures with DECODE-TERMINAL-PNG.")

(sb-alien:define-alien-callable tuatara-terminal-decode-png (sb-alien:unsigned 8)
    ((userdata sb-sys:system-area-pointer) (allocator sb-sys:system-area-pointer)
     (data sb-sys:system-area-pointer) (size (sb-alien:unsigned 64)) (out sb-sys:system-area-pointer))
  userdata                              ; not used: a callable's body takes no declarations
  (if (decode-terminal-png allocator data size out) 1 0))

(defun decode-terminal-png (allocator data size out)
  "Turn the PNG picture of SIZE octets at DATA, which a terminal's program sent,
into the pixels the library keeps (red, green, blue and alpha), in memory
from ALLOCATOR, described at OUT (a GhosttySysImage): drawn by the program
the sandbox shuts in, as every picture from outside is (DECODE-IMAGE), never
by the editor itself (DECODE-PICTURE). True when it was drawn."
  (handler-case
      (let ((image (decode-picture (tuatara.io:sap-octets data size) :png *terminal-picture-largest* *terminal-picture-largest*)))
        (when image
          (let* ((width (tuatara.frontend:image-width image)) (height (tuatara.frontend:image-height image))
                 (channels (tuatara.frontend:image-channels image)) (pixels (tuatara.frontend:image-pixels image))
                 (length (* width height 4))
                 (buffer (foreign-call "ghostty_alloc" sb-sys:system-area-pointer (sb-sys:system-area-pointer allocator)
                                  ((sb-alien:unsigned 64) length))))
            (unless (tuatara.io:null-pointer-p buffer)
              (dotimes (i (* width height))
                (dotimes (c 3) (setf (sb-sys:sap-ref-8 buffer (+ (* 4 i) c)) (aref pixels (+ (* channels i) c))))
                (setf (sb-sys:sap-ref-8 buffer (+ (* 4 i) 3)) (if (= channels 4) (aref pixels (+ (* channels i) 3)) 255)))
              (setf (sb-sys:sap-ref-32 out (vt :sys-image-width)) width
                    (sb-sys:sap-ref-32 out (vt :sys-image-height)) height
                    (sb-sys:sap-ref-sap out (vt :sys-image-data)) buffer
                    (sb-sys:sap-ref-64 out (vt :sys-image-length)) length)
              t))))
    (error () nil)))

(defun allow-pictures (emulator)
  "Have EMULATOR keep the pictures its program sends, *TERMINAL-PICTURE-MEMORY*
octets at most, sent in the escape sequences alone: never read from a file or
shared memory the program names, so that text shown (a file read with cat)
cannot make the terminal read other files."
  (let ((terminal (emulator-terminal emulator)))
    (unless *png-decoder-set*
      (foreign-call "ghostty_sys_set" sb-alien:int (sb-alien:int (vt :sys-decode-png))
                    (sb-sys:system-area-pointer (sb-alien:alien-sap (sb-alien:alien-callable-function 'tuatara-terminal-decode-png))))
      (setf *png-decoder-set* t))
    (setf (emulator-picture-memory emulator) tuatara-user:*terminal-picture-memory*)
    (set-size-option terminal (vt :opt-kitty-storage-limit) (emulator-picture-memory emulator))
    (sb-alien:with-alien ((off (sb-alien:unsigned 8) 0))
      (dolist (option (list (vt :opt-kitty-medium-file) (vt :opt-kitty-medium-shared-mem)))
        (foreign-call "ghostty_terminal_set" sb-alien:int (sb-sys:system-area-pointer terminal) (sb-alien:int option)
                      (sb-sys:system-area-pointer (sb-alien:alien-sap (sb-alien:addr off))))))
    (foreign-call "ghostty_terminal_set" sb-alien:int (sb-sys:system-area-pointer terminal) (sb-alien:int (vt :opt-kitty-medium-temp-file))
                  (sb-sys:system-area-pointer (tuatara.io:null-pointer)))
    (when (plusp (emulator-picture-memory emulator))
      (setf (emulator-placements emulator) (vt-new "ghostty_kitty_graphics_placement_iterator_new")))))

(defparameter *terminal-placement-limit* 1024
  "The most places a terminal's program may show its pictures at; past it
the terminal drops every picture the program sent
\(KEEP-PLACEMENTS-WITHIN-BOUNDS), and the program may send more.")

(defun keep-placements-within-bounds (emulator)
  "Drop every picture EMULATOR's program sent when it has placed them more
than *TERMINAL-PLACEMENT-LIMIT* times, counted only when its pictures
changed since they were last counted: the library's room for pictures is
made none, which deletes them and their places, then what it was, and the
debug log says so the first time. To be called holding the library,
pictures on."
  (sb-alien:with-alien ((graphics sb-sys:system-area-pointer (tuatara.io:null-pointer))
                        (generation (sb-alien:unsigned 64) 0)
                        (iterator sb-sys:system-area-pointer (emulator-placements emulator)))
    (let ((terminal (emulator-terminal emulator)))
      (flet ((graphics-get (what place)
               (eql (vt :success) (foreign-call "ghostty_kitty_graphics_get" sb-alien:int (sb-sys:system-area-pointer graphics)
                                                (sb-alien:int what) (sb-sys:system-area-pointer place)))))
        (when (and (eql (vt :success) (foreign-call "ghostty_terminal_get" sb-alien:int (sb-sys:system-area-pointer terminal)
                                                    (sb-alien:int (vt :data-kitty-graphics))
                                                    (sb-sys:system-area-pointer (sb-alien:alien-sap (sb-alien:addr graphics)))))
                   (not (tuatara.io:null-pointer-p graphics))
                   (graphics-get (vt :graphics-generation) (sb-alien:alien-sap (sb-alien:addr generation)))
                   (/= generation (emulator-graphics-generation emulator)))
          (setf (emulator-graphics-generation emulator) generation)
          (when (and (graphics-get (vt :graphics-placement-iterator) (sb-alien:alien-sap (sb-alien:addr iterator)))
                     (loop for placements from 0
                           while (foreign-call "ghostty_kitty_graphics_placement_next" (sb-alien:boolean 8) (sb-sys:system-area-pointer iterator))
                           thereis (>= placements *terminal-placement-limit*)))
            (set-size-option terminal (vt :opt-kitty-storage-limit) 0)
            (set-size-option terminal (vt :opt-kitty-storage-limit) (emulator-picture-memory emulator))
            (unless (shiftf (emulator-pictures-dropped emulator) t)
              (tuatara.runtime:queue-debug-report
               :terminal (format nil "terminal ~d: pictures dropped, placed more than ~:d times"
                                 (emulator-id emulator) *terminal-placement-limit*)))))))))

(defun image-value (image-handle what &optional (bits 32))
  "What WHAT (an :image- value of the library's layout) is of the picture the
library keeps as IMAGE-HANDLE, a number of BITS bits (32 or 64); NIL when it
gives none. To be called holding the library."
  (sb-alien:with-alien ((place (sb-alien:unsigned 64) 0))
    (and (eql (vt :success) (foreign-call "ghostty_kitty_graphics_image_get" sb-alien:int (sb-sys:system-area-pointer image-handle)
                                          (sb-alien:int what) (sb-sys:system-area-pointer (sb-alien:alien-sap (sb-alien:addr place)))))
         (if (= bits 32) (ldb (byte 32 0) place) place))))

(defun terminal-picture-most-pixels ()
  "The most pixels a picture a terminal's program sends may have to be shown:
as many as a PNG picture it sends is drawn with at most
\(*TERMINAL-PICTURE-LARGEST* across and down). A larger one shows nothing."
  (* *terminal-picture-largest* *terminal-picture-largest*))

(defun library-picture (image-handle)
  "The picture the library keeps as IMAGE-HANDLE, a TUATARA.FRONTEND:IMAGE
copied from its pixels, or NIL while they are on their way or when it has
more pixels than are shown (TERMINAL-PICTURE-MOST-PIXELS); and its
generation, which changes when the picture does. To be called holding the
library."
  (let ((width (image-value image-handle (vt :image-width))) (height (image-value image-handle (vt :image-height)))
        (format (image-value image-handle (vt :image-format)))
        (generation (image-value image-handle (vt :image-generation) 64))
        (pointer (let ((address (image-value image-handle (vt :image-data-pointer) 64))) (and address (sb-sys:int-sap address)))))
    (values (when (and pointer width height (plusp width) (plusp height)
                       (not (tuatara.io:null-pointer-p pointer))
                       (<= (* width height) (terminal-picture-most-pixels)))
              (let* ((in (cond ((= format (vt :image-format-rgb)) 3) ((= format (vt :image-format-rgba)) 4)
                               ((= format (vt :image-format-gray-alpha)) 2) (t 1)))
                     (out (if (evenp in) 4 3)))
                (tuatara.frontend:make-image
                 width height out
                 (if (= in out)
                     (tuatara.io:sap-octets pointer (* width height out))
                     ;; grey made red, green and blue alike
                     (let ((pixels (make-array (* width height out) :element-type '(unsigned-byte 8))))
                       (dotimes (i (* width height) pixels)
                         (let ((grey (sb-sys:sap-ref-8 pointer (* in i))))
                           (setf (aref pixels (* out i)) grey (aref pixels (+ (* out i) 1)) grey (aref pixels (+ (* out i) 2)) grey)
                           (when (= in 2) (setf (aref pixels (+ (* out i) 3)) (sb-sys:sap-ref-8 pointer (1+ (* in i))))))))))))
            generation)))

(defun whole-picture (kept picture-id image-handle)
  "The picture PICTURE-ID, kept by the library as IMAGE-HANDLE, as a
TUATARA.FRONTEND:IMAGE, and its generation, as two values: from KEPT (an
emulator's PICTURES) while its generation is the same, so that each
generation is copied once; else copied anew (LIBRARY-PICTURE) and kept.
NIL when there is none to show. To be called holding the library."
  (let ((entry (gethash (list :whole picture-id) kept))
        (generation (image-value image-handle (vt :image-generation) 64)))
    (if (and entry (eql generation (cdr entry)))
        (values (car entry) generation)
        (multiple-value-bind (image generation) (library-picture image-handle)
          (if image
              (setf (gethash (list :whole picture-id) kept) (cons image generation))
              (remhash (list :whole picture-id) kept))
          (values image generation)))))

(defun picture-part (image x y width height)
  "The part of IMAGE WIDTH by HEIGHT pixels from X, Y, as a picture of its own;
IMAGE itself when that is all of it."
  (if (and (zerop x) (zerop y) (= width (tuatara.frontend:image-width image)) (= height (tuatara.frontend:image-height image)))
      image
      (let* ((channels (tuatara.frontend:image-channels image))
             (from (tuatara.frontend:image-pixels image))
             (stride (* channels (tuatara.frontend:image-width image)))
             (pixels (make-array (* width height channels) :element-type '(unsigned-byte 8))))
        (dotimes (row height)
          (replace pixels from :start1 (* row width channels)
                               :start2 (+ (* (+ y row) stride) (* x channels)) :end2 (+ (* (+ y row) stride) (* (+ x width) channels))))
        (tuatara.frontend:make-image width height channels pixels))))

(defun frame-pictures (emulator)
  "The pictures EMULATOR's program has placed on its screen, as the editor
shows them over its cells: each (IMAGE ROW COLUMN COLUMNS ROWS), ROW and
COLUMN of the screen from 0, the part of a picture partly off the screen
alone. Pictures are made once for each picture the program sends and kept
\(EMULATOR's PICTURES) while it shows them. To be called holding the library."
  (let ((kept (emulator-pictures emulator))
        (used (setf (emulator-pictures-shown emulator) (emptied-table (emulator-pictures-shown emulator))))
        (shown '()) (virtual (make-hash-table)))
    (sb-alien:with-alien ((graphics sb-sys:system-area-pointer (tuatara.io:null-pointer))
                          (id (sb-alien:unsigned 32) 0))
     (tuatara.io:with-foreign-memory (info (vt :render-info-size))
         (let ((terminal (emulator-terminal emulator)) (iterator (emulator-placements emulator)))
           (when (and (eql (vt :success) (foreign-call "ghostty_terminal_get" sb-alien:int (sb-sys:system-area-pointer terminal)
                                                  (sb-alien:int (vt :data-kitty-graphics)) (sb-sys:system-area-pointer (sb-alien:alien-sap (sb-alien:addr graphics)))))
                      (not (tuatara.io:null-pointer-p graphics))
                      (progn (setf (sb-sys:sap-ref-sap info 0) iterator)
                             (eql (vt :success) (foreign-call "ghostty_kitty_graphics_get" sb-alien:int (sb-sys:system-area-pointer graphics)
                                                         (sb-alien:int (vt :graphics-placement-iterator)) (sb-sys:system-area-pointer info)))))
             (loop while (foreign-call "ghostty_kitty_graphics_placement_next" (sb-alien:boolean 8) (sb-sys:system-area-pointer iterator))
                   do (setf id 0)
                      (foreign-call "ghostty_kitty_graphics_placement_get" sb-alien:int (sb-sys:system-area-pointer iterator)
                               (sb-alien:int (vt :placement-image-id)) (sb-sys:system-area-pointer (sb-alien:alien-sap (sb-alien:addr id))))
                      (let ((handle (foreign-call "ghostty_kitty_graphics_image" sb-sys:system-area-pointer
                                             (sb-sys:system-area-pointer graphics) ((sb-alien:unsigned 32) id)))
                            (sap info))
                        ;; a placement for pictures in text (kitty's Unicode placeholders): its cells, for them
                        (flet ((placement-value (what)
                                 (setf (sb-sys:sap-ref-64 sap 0) 0)
                                 (foreign-call "ghostty_kitty_graphics_placement_get" sb-alien:int (sb-sys:system-area-pointer iterator)
                                          (sb-alien:int what) (sb-sys:system-area-pointer sap))
                                 (sb-sys:sap-ref-32 sap 0)))
                          (when (and (not (tuatara.io:null-pointer-p handle)) (/= 0 (logand #xff (placement-value (vt :placement-virtual)))))
                            (setf (gethash id virtual)
                                  (list handle (placement-value (vt :placement-columns)) (placement-value (vt :placement-rows))))))
                        (unless (tuatara.io:null-pointer-p handle)
                          (dotimes (i (vt :render-info-size)) (setf (sb-sys:sap-ref-8 sap i) 0))
                          (setf (sb-sys:sap-ref-64 sap 0) (vt :render-info-size))
                          (when (and (eql (vt :success) (foreign-call "ghostty_kitty_graphics_placement_render_info" sb-alien:int
                                                                 (sb-sys:system-area-pointer iterator) (sb-sys:system-area-pointer handle)
                                                                 (sb-sys:system-area-pointer terminal) (sb-sys:system-area-pointer sap)))
                                     (/= 0 (sb-sys:sap-ref-8 sap (vt :render-info-visible))))
                            (let* ((columns (sb-sys:sap-ref-32 sap (vt :render-info-columns)))
                                   (rows (sb-sys:sap-ref-32 sap (vt :render-info-rows)))
                                   (column (sb-sys:signed-sap-ref-32 sap (vt :render-info-column)))
                                   (row (sb-sys:signed-sap-ref-32 sap (vt :render-info-row)))
                                   (sx (sb-sys:sap-ref-32 sap (vt :render-info-source-x)))
                                   (sy (sb-sys:sap-ref-32 sap (vt :render-info-source-y)))
                                   (sw (sb-sys:sap-ref-32 sap (vt :render-info-source-width)))
                                   (sh (sb-sys:sap-ref-32 sap (vt :render-info-source-height)))
                                   ;; rows and columns off the screen, cut off
                                   (above (max 0 (- row))) (left (max 0 (- column)))
                                   (below (max 0 (- (+ row rows) (emulator-rows emulator))))
                                   (right (max 0 (- (+ column columns) (emulator-columns emulator))))
                                   (shown-rows (- rows above below)) (shown-columns (- columns left right)))
                              (when (and (plusp shown-rows) (plusp shown-columns) (plusp sw) (plusp sh))
                                (multiple-value-bind (whole generation) (whole-picture kept id handle)
                                  (when whole
                                    (let* ((key (list :part id generation sx sy sw sh rows columns above below left right))
                                           (part (or (car (gethash key kept))
                                                     (let* ((y (+ sy (floor (* above sh) rows))) (x (+ sx (floor (* left sw) columns)))
                                                            (h (max 1 (- (+ sy (floor (* (- rows below) sh) rows)) y)))
                                                            (w (max 1 (- (+ sx (floor (* (- columns right) sw) columns)) x))))
                                                       (picture-part whole (min x (1- (tuatara.frontend:image-width whole)))
                                                                     (min y (1- (tuatara.frontend:image-height whole)))
                                                                     (min w (- (tuatara.frontend:image-width whole) x))
                                                                     (min h (- (tuatara.frontend:image-height whole) y)))))))
                                      (setf (gethash key kept) (list part))
                                      (setf (gethash key used) t (gethash (list :whole id) used) t)
                                      (push (list part (+ row above) (+ column left) shown-columns shown-rows) shown)))))))))))
           ;; pictures in text: the areas of cells standing for them, each drawn at its cells' size
           (when (and (emulator-placeholders emulator) (plusp (hash-table-count virtual))
                      (plusp (emulator-cell-width emulator)) (plusp (emulator-cell-height emulator)))
             (dolist (area (placeholder-areas (emulator-placeholders emulator)
                                              (setf (emulator-placeholder-ends emulator)
                                                    (emptied-table (emulator-placeholder-ends emulator)))))
               (let ((placement (gethash (first area) virtual)))
                 (when placement
                   (destructuring-bind (handle columns rows) placement
                     (multiple-value-bind (whole generation) (whole-picture kept (first area) handle)
                       (when whole
                         (let* ((cw (emulator-cell-width emulator)) (ch (emulator-cell-height emulator))
                                (columns (if (plusp columns) columns (ceiling (tuatara.frontend:image-width whole) cw)))
                                (rows (if (plusp rows) rows (ceiling (tuatara.frontend:image-height whole) ch)))
                                ;; the area's own place first: keys alike in their first few parts hash alike
                                (key (list* (second area) (third area) (first area) :text generation columns rows cw ch area))
                                (part (or (car (gethash key kept))
                                          (placeholder-picture whole area columns rows cw ch))))
                           (setf (gethash key kept) (list part))
                           (setf (gethash key used) t (gethash (list :whole (first area)) used) t)
                           (destructuring-bind (id row column area-columns area-rows &rest more) area
                             (declare (ignore id more))
                             (push (list part row column area-columns area-rows) shown))))))))))
           ;; let go of what no longer shows
           (loop for key being the hash-keys of kept
                 unless (gethash key used) do (remhash key kept))
           (nreverse shown))))))

(defvar *kitty-mark-numbers*
  (let ((table (make-hash-table)))
    (loop for code across tuatara.frontend:*kitty-marks* for n from 0 do (setf (gethash code table) n))
    table)
  "The number each of kitty's marks (TUATARA.FRONTEND:*KITTY-MARKS*) stands for.")

(defun row-placeholders (cells)
  "The cells of a row standing for parts of pictures, as the frame found them,
each (COLUMN COLOUR MARKS): COLOUR the code of the cell's colour (STYLE-COLOUR), MARKS the code points after U+10EEEE. As a list of (COLUMN PICTURE
ROW PART-COLUMN): PICTURE the picture's number (its colour, the third mark its
highest octet), ROW and PART-COLUMN the cell's of the picture, from its
marks, or, those left out, from the cell before it: its row, and the column
after its column."
  (let ((out '()) (before nil))
    (dolist (cell cells (nreverse out))
      (destructuring-bind (column colour marks) cell
        (let* ((numbers (mapcar (lambda (code) (gethash code *kitty-mark-numbers*)) marks))
               (low (logand colour #xffffff))
               (id (logior low (ash (or (third numbers) 0) 24)))
               ;; the cell before, of the same picture, just left of it
               (follows (and before (= (first before) (1- column)) (= (second before) id)))
               (row (or (first numbers) (if follows (third before) 0)))
               (part-column (or (second numbers) (if (and follows (= row (third before))) (1+ (fourth before)) 0)))
               (this (list column id row part-column)))
          (push this out)
          (setf before this))))))

(defun emptied-table (table)
  "TABLE emptied for the next frame; when the last frame filled a quarter of
it at most, a new, smaller table of its kind instead."
  (let ((size (hash-table-size table)) (count (hash-table-count table)))
    (if (and (> size 256) (< (* 4 count) size))
        (make-hash-table :test (hash-table-test table) :size (max 64 (* 2 count)))
        (clrhash table))))

(defun placeholder-areas (rows &optional (ends (make-hash-table)))
  "The parts of pictures ROWS (EMULATOR's PLACEHOLDERS, a vector of
ROW-PLACEHOLDERS lists) stand for, each (PICTURE ROW COLUMN COLUMNS ROWS
PART-ROW PART-COLUMN): the screen's cells from ROW and COLUMN, COLUMNS by
ROWS, showing that much of the picture from its cell PART-ROW, PART-COLUMN;
the cells of a row in a run, and the runs of the rows below that carry on
from it, one area. ENDS, an empty EQL table, is used to join them. Takes
time in step with the cells."
  (let ((runs '()))
    ;; the runs of each row: cells one after another, of one picture and row, its columns in turn
    (loop for cells across rows
          for screen-row from 0
          do (let ((run nil))
               (dolist (cell cells)
                 (destructuring-bind (column id row part-column) cell
                   (if (and run (= column (+ (third run) (fourth run))) (= id (first run))
                            (= row (sixth run)) (= part-column (+ (seventh run) (fourth run))))
                       (incf (fourth run))
                       (progn (when run (push run runs))
                              (setf run (list id screen-row column 1 1 row part-column))))))
               (when run (push run runs))))
    ;; a run joined to the area above it when it carries on from it; each area kept in
    ;; ENDS by the row below its last and its column, the only place a run carrying it on
    ;; can be, as areas of one row never share a cell
    (let ((areas '()))
      (dolist (run (nreverse runs) (nreverse areas))
        (destructuring-bind (id screen-row column columns rows row part-column) run
          (declare (ignore rows))
          (let* ((place (+ (* screen-row 65536) column))
                 (above (gethash place ends))
                 (area (if (and above (= (first above) id) (= (fourth above) columns)
                                (= (+ (sixth above) (fifth above)) row) (= (seventh above) part-column))
                           (progn (incf (fifth above)) above)
                           (car (push run areas)))))
            (setf (gethash (+ place 65536) ends) area)))))))

(defun placeholder-picture (whole area columns rows cell-width cell-height)
  "The part of the picture WHOLE that AREA (PLACEHOLDER-AREAS') shows, drawn
at the size of its cells (CELL-WIDTH by CELL-HEIGHT pixels each): WHOLE fit
into COLUMNS by ROWS cells, its proportions kept and centred, what of its
cells it leaves empty clear."
  (destructuring-bind (id row column area-columns area-rows part-row part-column) area
    (declare (ignore id row column))
    (let* ((iw (tuatara.frontend:image-width whole)) (ih (tuatara.frontend:image-height whole))
           (channels (tuatara.frontend:image-channels whole)) (from (tuatara.frontend:image-pixels whole))
           (box-width (* columns cell-width)) (box-height (* rows cell-height))
           (scale (float (min (/ box-width iw) (/ box-height ih)) 1d0))
           (left (/ (- box-width (* iw scale)) 2)) (top (/ (- box-height (* ih scale)) 2))
           (width (* area-columns cell-width)) (height (* area-rows cell-height))
           (x0 (* part-column cell-width)) (y0 (* part-row cell-height))
           ;; the picture's column for each column of the part, and its row for each row
           (xs (let ((v (make-array width)))
                 (dotimes (x width v) (setf (aref v x) (floor (- (+ x0 x 0.5d0) left) scale)))))
           (ys (let ((v (make-array height)))
                 (dotimes (y height v) (setf (aref v y) (floor (- (+ y0 y 0.5d0) top) scale)))))
           (pixels (make-array (* width height 4) :element-type '(unsigned-byte 8) :initial-element 0)))
      (dotimes (y height)
        (let ((iy (aref ys y)))
          (when (< -1 iy ih)
            (dotimes (x width)
              (let ((ix (aref xs x)))
                (when (< -1 ix iw)
                  (let ((at (* channels (+ ix (* iy iw)))) (to (* 4 (+ x (* y width)))))
                    (setf (aref pixels to) (aref from at) (aref pixels (+ to 1)) (aref from (+ at 1))
                          (aref pixels (+ to 2)) (aref from (+ at 2))
                          (aref pixels (+ to 3)) (if (= channels 4) (aref from (+ at 3)) 255)))))))))
      (tuatara.frontend:make-image width height 4 pixels))))

;;; Compressing a quiet terminal's scrollback

(defvar tuatara-user:*terminal-compress-after* 10
  "Seconds a terminal's program has printed nothing after which its scrollback
is compressed, in the background, a little at a time, so that a terminal
with a long history holds a fraction of the memory; reading the history
\(its history view) undoes it as it goes, unseen. NIL never compresses.")

(defvar *compressor* nil
  "The thread that compresses quiet terminals' scrollback (COMPRESS-QUIET-TERMINALS),
started with the first terminal.")

(defvar *compressor-lock* (sb-thread:make-mutex :name "terminal compressor")
  "Guards waking the compressor.")

(defvar *compressor-wake* (sb-thread:make-waitqueue :name "terminal compressor")
  "Where the compressor waits for a terminal to have printed.")

(defvar *compressor-woken* nil
  "Set when a terminal printed since the compressor last looked.")

(defun wake-compressor ()
  "Tell the compressor a terminal printed after its scrollback was compressed
\(or for the first time), starting the compressor the first time."
  (sb-thread:with-mutex (*compressor-lock*)
    (unless (and *compressor* (sb-thread:thread-alive-p *compressor*))
      (setf *compressor* (tuatara.runtime:make-guarded-thread #'compress-quiet-terminals :name "tuatara terminal compressor")))
    (setf *compressor-woken* t)
    (sb-thread:condition-broadcast *compressor-wake*)))

(defun compress-emulator (emulator)
  "Compress EMULATOR's scrollback a step at a time, holding the library for a
step only, so that neither the reader nor the editor waits more than a
moment. True when it is done (or nothing can be compressed); NIL when the
program printed meanwhile or the terminal ended, to be tried again later."
  (let ((printed (emulator-last-output emulator)))
    (sb-alien:with-alien ((result sb-alien:int 0))
      (loop
        (when (or (emulator-closed emulator) (/= printed (emulator-last-output emulator)))
          (return nil))
        (let ((step (with-emulator-turn (emulator)
                      (when (emulator-terminal emulator)
                        (setf result 0)
                        (and (eql (vt :success)
                                  (foreign-call "ghostty_terminal_compress" sb-alien:int (sb-sys:system-area-pointer (emulator-terminal emulator))
                                                (sb-alien:int (vt :compress-incremental))
                                                (sb-sys:system-area-pointer (sb-alien:alien-sap (sb-alien:addr result)))))
                             result)))))
          (unless (eql step (vt :compress-pending))
            (return (and step t)))
          (sb-thread:thread-yield))))))

(defun compress-quiet-terminals ()
  "What the compressor thread does: compress the scrollback of each terminal
that has printed nothing for *TERMINAL-COMPRESS-AFTER* seconds and has
printed since it was last compressed, and sleep until the next is due, or
until one prints, never waking for nothing."
  (loop
    (let ((after tuatara-user:*terminal-compress-after*)
          (soonest nil)
          (now (get-internal-real-time)))
      (when after
        (dolist (emulator (loop for e being the hash-values of *emulators* collect e))
          (when (and (emulator-to-compress emulator) (not (emulator-closed emulator)))
            (let ((left (- (* after internal-time-units-per-second) (- now (emulator-last-output emulator)))))
              (if (plusp left)
                  (setf soonest (min left (or soonest left)))
                  (progn
                    ;; set before, so that output meanwhile sets it again
                    (setf (emulator-to-compress emulator) nil)
                    (unless (compress-emulator emulator)
                      (setf (emulator-to-compress emulator) t
                            soonest (min (* after internal-time-units-per-second)
                                         (or soonest (* after internal-time-units-per-second)))))))))))
      ;; a wake meanwhile is not lost: it set *COMPRESSOR-WOKEN*; after a timeout
      ;; the lock is not held
      (sb-thread:grab-mutex *compressor-lock*)
      (let ((held (or *compressor-woken*
                      (if soonest
                          (sb-thread:condition-wait *compressor-wake* *compressor-lock*
                                                    :timeout (max 0.05 (/ soonest internal-time-units-per-second)))
                          (sb-thread:condition-wait *compressor-wake* *compressor-lock*)))))
        (setf *compressor-woken* nil)
        (when held (sb-thread:release-mutex *compressor-lock*))))))

(defun emulator-memory (emulator)
  "How much memory EMULATOR's screen and scrollback hold, a property list:
:resident, octets in use; :pages and :compressed-pages of its scrollback;
:compression, true when compressing can free memory here."
  (with-emulator-turn (emulator)
    (when (emulator-terminal emulator)
      (tuatara.io:with-foreign-memory (sap (vt :memory-size) :zero t)
        (setf (sb-sys:sap-ref-64 sap 0) (vt :memory-size))
        (when (eql (vt :success) (foreign-call "ghostty_terminal_get" sb-alien:int (sb-sys:system-area-pointer (emulator-terminal emulator))
                                               (sb-alien:int (vt :data-memory-usage)) (sb-sys:system-area-pointer sap)))
          (list :resident (+ (sb-sys:sap-ref-64 sap (vt :memory-primary-resident))
                             (sb-sys:sap-ref-64 sap (vt :memory-alternate-resident)))
                :pages (sb-sys:sap-ref-64 sap (vt :memory-primary-pages))
                :compressed-pages (sb-sys:sap-ref-64 sap (vt :memory-primary-compressed-pages))
                :compression (/= 0 (sb-sys:sap-ref-8 sap (vt :memory-compression-supported)))))))))

(defun emulator-alive-p (emulator)
  "True while EMULATOR's program has not ended."
  (not (emulator-closed emulator)))

(defun program-group (process)
  "The process group of the program PROCESS runs on its terminal: the child
setsid started, which leads a session and a group of its own; PROCESS's own
when it has none."
  (let* ((pid (sb-ext:process-pid process))
         (children (ignore-errors
                    (with-open-file (in (format nil "/proc/~d/task/~d/children" pid pid))
                      (read-line in nil "")))))
    (or (and children (ignore-errors (parse-integer children :junk-allowed t))) pid)))

(defun group-alive-p (group)
  "True while a process of the process group GROUP is there."
  (handler-case (progn (sb-posix:kill (- group) 0) t)
    (sb-posix:syscall-error () nil)))

(defun stop-emulator (emulator &key background)
  "End EMULATOR: the processes of its program's group told the terminal is gone
\(SIGHUP), killed when they have not ended two seconds later, its threads
ended, and the library's state let go of. With BACKGROUND, only the telling
is done at once, the waiting and the rest on a thread of its own."
  (let* ((process (emulator-process emulator))
         (group (and process (sb-ext:process-alive-p process) (program-group process))))
    (when group
      (ignore-errors (sb-posix:kill (- group) sb-posix:sighup)))
    (flet ((finish ()
             (when group
               (loop repeat 100 while (group-alive-p group) do (sleep 0.02))
               (when (group-alive-p group)
                 (ignore-errors (sb-posix:kill (- group) sb-posix:sigkill))
                 (loop repeat 50 while (group-alive-p group) do (sleep 0.02)))
               ;; setsid, waiting for its program, ends with it
               (loop repeat 50 while (sb-ext:process-alive-p process) do (sleep 0.02))
               (when (sb-ext:process-alive-p process) (ignore-errors (sb-ext:process-kill process sb-posix:sigkill))))
             (sb-concurrency:send-message (emulator-outbox emulator) :stop)
             (when process (ignore-errors (close (sb-ext:process-pty process))))
             (dolist (thread (list (emulator-reader emulator) (emulator-writer emulator)))
               (when thread (ignore-errors (sb-thread:join-thread thread :timeout 2 :default nil))))
             (when process (ignore-errors (sb-ext:process-close process)))
             ;; the encoders too, which typing uses holding only their own lock
             (sb-thread:with-mutex ((emulator-lock emulator))
               (sb-thread:with-mutex ((emulator-input-lock emulator))
                 (free-library-objects emulator)))
             (remhash (emulator-id emulator) *emulators*)
             (setf (emulator-closed emulator) t)))
      (if background
          (tuatara.runtime:make-guarded-thread #'finish :name "tuatara terminal end")
          (finish))
      nil)))
