;;;; frontends/tui/tui.lisp --- The ncurses terminal frontend.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(defpackage #:tuatara.frontend.tui
  (:use #:cl)
  (:import-from #:tuatara.io #:define-c-function #:null-pointer #:null-pointer-p)
  (:import-from #:tuatara-user #:*tui-padding* #:*tui-pictures* #:*tui-mouse* #:*tui-colours* #:*kitty-keyboard*)
  (:documentation "The ncurses frontend: paints rows into the terminal and turns key presses into events.")
  (:export #:tui-frontend #:make-tui-frontend))

(in-package #:tuatara.frontend.tui)

(defvar *tui-padding* 0
  "The room, in whole cells, between the edges of the terminal and Tuatara's
text: one number for all four sides, or a list: of two, the top and bottom,
then the right and left ((0 2) is none above and below, two columns at the
sides); of three, the top, the right and left, the bottom; of four, the top,
right, bottom and left, clockwise. A terminal may have padding of its own,
in pixels, which is finer. Set it in the init file, or try values with
\\[eval-expression] (setf *tui-padding* 1), which the terminal follows at once.")

(defvar *tui-pictures* :auto
  "Whether pictures (a preview of an image, a PDF's pages) are shown in the
terminal: :auto, where the terminal says, when asked at the start, that it speaks
the kitty graphics protocol (such as kitty or Ghostty), never inside tmux or
screen; T, in any terminal,
for one that speaks it but does not say; NIL, never, the file's octets shown instead.")

(defvar *tui-mouse* t
  "Whether the mouse works in Tuatara in a terminal as in its window: the first
button puts point, drags select (past the text, scrolling), twice selects a
word and three times a line, the wheel scrolls, and a terminal's program in
\\[terminal] gets them. NIL leaves the mouse to the terminal, its own
selection and copying. With it on, Shift held selects as the terminal does,
in most terminals (kitty, Ghostty, WezTerm, GNOME's).")

(defvar *tui-colours* :auto
  "How many colours Tuatara uses in a terminal. :AUTO (the standard) goes by
what the terminal says: 24-bit colour, the theme's colours exactly, where
COLORTERM is truecolor or 24bit (as kitty, Ghostty, WezTerm, foot, Konsole and
GNOME's terminal set it), outside the Linux console; else as many as TERM's description gives; none where NO_COLOR is
set. :24-BIT, 256, 16 or 8 say it outright, for a terminal that does not say
(COLORTERM is not passed on by ssh); NIL, none, the plain theme in their place.")

(defvar *kitty-keyboard* :auto
  "Whether the terminal frontend reads keys by the kitty keyboard protocol, so
that Escape, Alt and Control keys arrive whole, nothing waiting 25 ms to tell
Escape then a key from Meta and that key, and Tab from C-i, RET from C-m:
:AUTO (the standard) asks the terminal (kitty, foot, WezTerm, Ghostty and
others speak it), T uses it, NIL never.")

(defun load-ncurses ()
  "Load ncurses, the library the terminal is drawn with, once; signals when it is missing."
  (unless (some #'tuatara.runtime:foreign-library-p '("libncursesw.so.6" "libncursesw.so"))
    (error "The library libncursesw.so.6 is not installed")))

;; setlocale comes from the C library already linked into the SBCL runtime;
;; "libc.so" on Linux is a linker script, not a loadable object.

(define-c-function ("setlocale" %setlocale) :string (category :int) (locale :string))
(define-c-function ("newterm" %newterm) :pointer (type :string) (out :pointer) (in :pointer))
(define-c-function ("set_term" %set-term) :pointer (screen :pointer))
(define-c-function ("delscreen" %delscreen) :void (screen :pointer))
(define-c-function ("setupterm" %setupterm) :int (term :string) (fd :int) (error :pointer))
(define-c-function ("tigetnum" %tigetnum) :int (name :string))
(define-c-function ("del_curterm" %del-curterm) :int (terminal :pointer))
(define-c-function ("endwin" %endwin) :int)
(define-c-function ("raw" %raw) :int)
(define-c-function ("noecho" %noecho) :int)
(define-c-function ("nonl" %nonl) :int)
(define-c-function ("keypad" %keypad) :int (window :pointer) (enable :bool))
(define-c-function ("nodelay" %nodelay) :int (window :pointer) (enable :bool))
(define-c-function ("wtimeout" %wtimeout) :void (window :pointer) (delay :int))
(define-c-function ("set_escdelay" %set-escdelay) :int (ms :int))
(define-c-function ("wget_wch" %wget-wch) :int (window :pointer) (wch :pointer))
(define-c-function ("werase" %werase) :int (window :pointer))
(define-c-function ("wmove" %wmove) :int (window :pointer) (y :int) (x :int))
(define-c-function ("waddstr" %waddstr) :int (window :pointer) (string :string))
(define-c-function ("waddstr" %waddstr-octets) :int (window :pointer) (octets :pointer))
(define-c-function ("wclrtoeol" %wclrtoeol) :int (window :pointer))
(define-c-function ("wrefresh" %wrefresh) :int (window :pointer))
(define-c-function ("clearok" %clearok) :int (window :pointer) (enable :bool))
(define-c-function ("ttyname" %ttyname) :string (fd :int))
(define-c-function ("wredrawln" %wredrawln) :int (window :pointer) (start :int) (count :int))
(define-c-function ("wattr_set" %wattr-set) :int (window :pointer) (attrs :uint32) (pair :short) (opts :pointer))
(define-c-function ("curs_set" %curs-set) :int (visibility :int))
(define-c-function ("getmaxx" %getmaxx) :int (window :pointer))
(define-c-function ("getmaxy" %getmaxy) :int (window :pointer))
(define-c-function ("start_color" %start-color) :int)
(define-c-function ("use_default_colors" %use-default-colors) :int)
(define-c-function ("has_colors" %has-colors) :bool)
(define-c-function ("init_pair" %init-pair) :int (pair :short) (fg :short) (bg :short))
(define-c-function ("init_extended_pair" %init-extended-pair) :int (pair :int) (fg :int) (bg :int))
(define-c-function ("wbkgd" %wbkgd) :int (window :pointer) (ch :uint32))
(define-c-function ("getcurx" %getcurx) :int (window :pointer))
(define-c-function ("keyname" %keyname) :string (code :int))
(define-c-function ("typeahead" %typeahead) :int (fd :int))
(define-c-function ("ungetch" %ungetch) :int (ch :int))

(defmacro c-variable (name type)
  "The value of the C library's variable NAME, of the alien TYPE."
  `(sb-alien:extern-alien ,name ,type))

(defconstant +a-reverse+ (ash 1 18) "The ncurses A_REVERSE attribute bit.")
(defconstant +a-bold+ (ash 1 21) "The ncurses A_BOLD attribute bit.")
(defconstant +a-underline+ (ash 1 17) "The ncurses A_UNDERLINE attribute bit.")
(defconstant +a-dim+ (ash 1 20) "The ncurses A_DIM attribute bit.")
(defconstant +a-italic+ (ash 1 31) "The ncurses A_ITALIC attribute bit.")

(defvar *colours* nil "True when the terminal supports colours and they are initialised.")
(defvar *terminal-colours* 0 "Number of colours the terminal reports (8, 16, 256, or 16777216 for 24-bit colour).")
(defvar *screen* nil "The ncurses screen Tuatara draws on, NIL before it starts.")
(defvar *colours-wanted* :auto
  "The colours the screen was started for (WANTED-COLOURS), to tell when
*TUI-COLOURS* asks for others.")
(defvar *colours-why* ""
  "What decided the terminal's colours, as M-x check-health says it: from
COLORTERM, from TERM, NO_COLOR or *TUI-COLOURS*.")
(defvar *direct-colour* nil
  "True when the terminal shows 24-bit colour: a theme's #rrggbb colours are sent
exactly, rather than as the nearest of 256.")
(defvar *terminal-pairs* 0 "Number of colour pairs the terminal reports.")
(defvar *pairs* (make-hash-table :test 'equal) "Colour pairs allocated so far: (FG . BG) to pair number.")
(defvar *next-pair* 1 "The next colour pair number to allocate.")
(defvar *spec-cache* (tuatara.display:make-face-cache) "Face spec to attribute bits, kept until a face changes.")

(defun colour-pair (n)
  "Colour pair N as attributes take it here: above ncurses's own bits, given
to it apart (ATTRIBUTES-ON), so that pairs past 255 can be used."
  (ash n 32))

(defun attributes-on (window attrs)
  "Write what follows in WINDOW with ATTRS, the bits and the colour pair of
SPEC-ATTRIBUTES, the pair given as ncurses's extended one."
  (sb-alien:with-alien ((pair sb-alien:int (ash attrs -32)))
    (%wattr-set window (ldb (byte 32 0) attrs) 0 (sb-alien:alien-sap (sb-alien:addr pair)))))

(defun attributes-off (window)
  "Write what follows in WINDOW plain again."
  (%wattr-set window 0 0 (load-time-value (null-pointer) t)))

(defun background-character (attrs)
  "ATTRS, as SPEC-ATTRIBUTES gives them, as a blank character carrying them
for the window's background (its pair, one of the first made, in its bits)."
  (logior (ldb (byte 32 0) attrs) (ash (min 255 (ash attrs -32)) 8) 32))

(defun terminal-colour (colour)
  "Two values for COLOUR (a name, an integer, a string \"#rrggbb\" or NIL): the
terminal colour number (-1 for the terminal's default) and extra attribute bits
that stand in for a bright colour the terminal lacks (bright black becomes dim,
other bright colours become bold). A #rrggbb colour is sent exactly where the
terminal shows 24-bit colour, else it becomes the nearest of the terminal's
colours (256, or the basic 16 or 8)."
  (let ((n (cond ((null colour) -1)
                 ((integerp colour) colour)
                 ((stringp colour)
                  (multiple-value-bind (r g b) (tuatara.display:parse-hex-colour colour)
                    (cond ((null r) -1)
                          (*direct-colour* (direct-colour-number r g b))
                          (t (tuatara.display:nearest-palette-index
                              colour (cond ((>= *terminal-colours* 256) 256) ((>= *terminal-colours* 16) 16) (t 8)))))))
                 (t (or (tuatara.display:basic-colour-index colour) -1)))))
    (cond ((< n 0) (values -1 0))
          ((< n *terminal-colours*) (values n 0))
          ((= n 8) (values -1 +a-dim+))
          ((< n 16) (values (- n 8) +a-bold+))
          (t (values (mod n 8) 0)))))

(defun pair-for (fg bg)
  "The colour pair for terminal colours FG and BG, allocated on first use; 0
when the terminal has no colours or its pairs are used up. With 24-bit colour FG and BG may be colours of 24 bits,
which only ncurses's extended pairs take."
  (cond ((and (= fg -1) (= bg -1)) 0)
        ((not *colours*) 0)
        (t (let ((key (cons fg bg)))
             (or (gethash key *pairs*)
                 (if (< *next-pair* (min 32767 *terminal-pairs*))
                     (let ((pair *next-pair*))
                       (if *direct-colour*
                           (%init-extended-pair pair fg bg)
                           (%init-pair pair fg bg))
                       (incf *next-pair*)
                       (setf (gethash key *pairs*) pair))
                     0))))))

(defun direct-colour-number (r g b)
  "The colour number of R, G and B (0-255) for a terminal started with 24-bit
colour (xterm-direct256): the 24-bit value, except that values below 256, which
that terminal takes for its own palette, become 256 (#000100, no colour the eye
tells from its neighbours)."
  (max 256 (+ (ash r 16) (ash g 8) b)))

(defparameter *terminfo-directories*
  '("/etc/terminfo/" "/lib/terminfo/" "/usr/share/terminfo/" "/usr/lib/terminfo/" "/usr/share/lib/terminfo/")
  "Where terminal descriptions are installed, after $TERMINFO, ~/.terminfo and $TERMINFO_DIRS.")

(defun terminfo-installed-p (name)
  "True when a description of terminal type NAME is installed where ncurses looks for one."
  (let ((dirs (append (let ((dir (uiop:getenv "TERMINFO"))) (and (plusp (length dir)) (list dir)))
                      (list (namestring (merge-pathnames ".terminfo/" (user-homedir-pathname))))
                      (tuatara.text:split-words (or (uiop:getenv "TERMINFO_DIRS") "") ":")
                      *terminfo-directories*)))
    (some (lambda (dir)
            (let ((dir (string-right-trim "/" dir)))
              (or (probe-file (format nil "~a/~a/~a" dir (char name 0) name))
                  (probe-file (format nil "~a/~(~2,'0x~)/~a" dir (char-code (char name 0)) name)))))
          dirs)))

(defun terminfo-colours (term)
  "How many colours the installed description of terminal type TERM gives (0
for none), or NIL when there is none installed."
  (when (plusp (length term))
    (sb-alien:with-alien ((failure sb-alien:int 0))
      (when (zerop (%setupterm term 1 (sb-alien:alien-sap (sb-alien:addr failure))))
        (prog1 (max 0 (%tigetnum "colors"))
          (%del-curterm (c-variable "cur_term" sb-sys:system-area-pointer)))))))

(defun wanted-colours (&key (setting *tui-colours*) (no-color (uiop:getenv "NO_COLOR")))
  "The colours asked for: *TUI-COLOURS* when it says (:24-BIT, 256, 16, 8 or
NIL), else NIL where NO_COLOR is set (not empty), else :AUTO."
  (cond ((not (eq setting :auto)) setting)
        ((plusp (length no-color)) nil)
        (t :auto)))

(defun direct-terminal-type (&key (term (uiop:getenv "TERM")) (colorterm (uiop:getenv "COLORTERM"))
                                  (wanted (wanted-colours)) (term-colours (terminfo-colours term)))
  "The terminal type to start ncurses with for 24-bit colour, or NIL to go on
with TERM: xterm-direct256 (xterm-256color with 24-bit colour, whose numbers
below 256 stay the palette's) when WANTED is :24-BIT, or :AUTO and the
terminal says it shows 24-bit colour (COLORTERM truecolor or 24bit) and is
no Linux console, whose colours are its own, and TERM-COLOURS, the colours of
TERM's description, are 8 or more or there is no description of it installed;
and that description is installed."
  (and (case wanted
         (:24-bit t)
         (:auto (and (member colorterm '("truecolor" "24bit") :test #'string-equal)
                     (not (uiop:string-prefix-p "linux" term))
                     (or (null term-colours) (>= term-colours 8))))
         (t nil))
       (terminfo-installed-p "xterm-direct256")
       "xterm-direct256"))

(defun colours-used (wanted colours direct)
  "How many colours to use, and what decided it in words, as two values: of the
COLOURS the terminal description gives, as WANTED asks (WANTED-COLOURS), DIRECT
true when ncurses was started for 24-bit colour."
  (cond ((null wanted)
         (values 0 (if (eq *tui-colours* :auto) "NO_COLOR is set" "*tui-colours* is nil")))
        ((integerp wanted) (values (min wanted colours) (format nil "*tui-colours* ~d" wanted)))
        (direct (values colours (if (eq wanted :24-bit) "*tui-colours* :24-bit" "COLORTERM says so")))
        (t (values colours "from TERM"))))

(defun spec-attributes (spec)
  "The ncurses attribute bits (flags and colour pair) for face SPEC, cached until
a face changes. A change of theme starts the colour pairs afresh, so that
switching themes back and forth never uses the pairs up."
  (multiple-value-bind (table emptied) (tuatara.display:face-cache-table *spec-cache*)
    (when emptied
      (clrhash *pairs*)
      (setf *next-pair* 1))
    (or (gethash spec table)
        (setf (gethash spec table)
              (multiple-value-bind (fg bg flags) (tuatara.display:resolve-face-spec spec)
                (multiple-value-bind (fg-number fg-extra) (terminal-colour fg)
                  (multiple-value-bind (bg-number bg-extra) (terminal-colour bg)
                    (declare (ignore bg-extra))
                    (logior (colour-pair (pair-for fg-number bg-number))
                            fg-extra
                            (if (member :bold flags) +a-bold+ 0)
                            (if (member :italic flags) +a-italic+ 0)
                            (if (member :underline flags) +a-underline+ 0)
                            (if (member :reverse flags) +a-reverse+ 0)
                            (if (member :dim flags) +a-dim+ 0)))))))))

(defconstant +key-code-yes+ 256 "wget_wch result meaning the value is a function-key code.")

(defparameter *key-codes*
  '((258 . :down) (259 . :up) (260 . :left) (261 . :right) (262 . :home) (263 . :backspace)
    (265 . :f1) (266 . :f2) (267 . :f3) (268 . :f4) (269 . :f5) (270 . :f6) (271 . :f7) (272 . :f8)
    (273 . :f9) (274 . :f10) (275 . :f11) (276 . :f12)
    (330 . :delete) (331 . :insert) (338 . :page-down) (339 . :page-up) (343 . :enter) (360 . :end)
    (353 . :tab))                        ; KEY_BTAB: shift-tab, reported as tab with shift below
  "ncurses key codes and the special keys they stand for.")

(defparameter *shifted-key-codes*
  '((393 . :left) (402 . :right) (337 . :up) (336 . :down) (391 . :home) (386 . :end)
    (383 . :delete) (396 . :insert) (398 . :page-up) (400 . :page-down))
  "ncurses key codes of special keys with Shift, and the keys they stand for.")

(defparameter *key-name-bases*
  '(("UP" . :up) ("DN" . :down) ("LFT" . :left) ("RIT" . :right) ("HOM" . :home) ("END" . :end)
    ("NXT" . :page-down) ("PRV" . :page-up) ("DC" . :delete) ("IC" . :insert))
  "The special keys by the names terminal descriptions give them with modifiers
(kUP5: Control and up).")

(defun modifier-bits (field)
  "The modifiers a terminal's modifier FIELD stands for (one more than the sum
of 1 Shift, 2 Alt, 4 Control, as xterm and kitty send it; NIL for none), as
three values: shift, meta and control."
  (let* ((number (or (and field (ignore-errors (parse-integer (first (uiop:split-string field :separator ":"))))) 1))
         (bits (max 0 (1- number))))
    (values (logtest bits 1) (logtest bits 2) (logtest bits 4))))

(defun key-name-key (name)
  "The KEY a terminal description's NAME for a special key with modifiers
stands for (kUP5, kLFT3, kDC2), or NIL."
  (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^k([A-Z]+)([2-8])$" (or name ""))
    (let ((base (and match (cdr (assoc (aref groups 0) *key-name-bases* :test #'string=)))))
      (when base
        (multiple-value-bind (shift meta ctrl) (modifier-bits (aref groups 1))
          (tuatara.frontend:make-key base :ctrl ctrl :meta meta :shift shift))))))

(defun ncurses-key (code)
  "The KEY an ncurses key CODE stands for, or NIL for one of no use here: the
special keys, with Shift, F13 to F60 as F1 to F12 with Shift, Control, both or
Alt (as xterm numbers them), and those with modifiers the terminal's
description names (KEY-NAME-KEY)."
  (let ((base (cdr (assoc code *key-codes*))) (shifted (cdr (assoc code *shifted-key-codes*))))
    (cond (base (tuatara.frontend:make-key base))
          (shifted (tuatara.frontend:make-key shifted :shift t))
          ((<= 277 code 324)
           (multiple-value-bind (group n) (floor (- code 265) 12)
             (tuatara.frontend:make-key (intern (format nil "F~d" (1+ n)) :keyword)
                                        :shift (and (member group '(1 3)) t) :ctrl (and (member group '(2 3)) t) :meta (= group 4))))
          (t (key-name-key (ignore-errors (%keyname code)))))))

(defvar *kitty-keyboard-decided* nil
  "True once the terminal frontend has decided on the kitty keyboard protocol,
at the first key read (after the init file has had its say).")

(defvar *kitty-keyboard-on* nil
  "True while the terminal sends keys by the kitty keyboard protocol, switched on
by FRONTEND-START (KITTY-KEYBOARD-SUPPORTED-P): Escape, Alt and Control keys
then come as CSI ... u sequences, read without a timer.")

(defvar *terminal-answers* :unasked
  "What the terminal said when asked at the start (ASK-TERMINAL), a list of
:KEYBOARD, :PICTURES and :SHARED with whether it does each; :UNASKED before.")

(defvar *shown-cursor-shape* nil
  "The cursor shape last sent to the terminal, so that it is sent only when it changes.")

(defvar *shown-cursor-colour* nil
  "The cursor colour last sent to the terminal (#rrggbb, or :DEFAULT for the
terminal's own), so that it is sent only when it changes.")

(defclass tui-frontend ()
  ((window :initform nil :accessor tui-window
           :documentation "The ncurses stdscr pointer while started.")
   (wake :initform nil :accessor tui-wake
         :documentation "The wake pipe (TUATARA.IO:MAKE-WAKE-PIPE) reading keys waits on besides the terminal, while started.")
   (mouse :initform nil :accessor tui-mouse :documentation "True while the terminal reports the mouse (*TUI-MOUSE*).")
   (clicks :initform nil :accessor tui-clicks
           :documentation "The first button's last press: (COLUMN ROW TIME COUNT), to tell a second and third press there.")
   (padding :initform '(0 0 0 0) :accessor tui-padding
            :documentation "The room in cells round the text, as last taken from *TUI-PADDING*: top, right, bottom and left.")
   (pending :initform '() :accessor tui-pending
            :documentation "Events to hand the editor before reading the terminal, oldest first: a resize the padding made.")
   (images :initform (make-hash-table) :accessor tui-images
           :documentation "The pictures the terminal has been sent, by their number there: each a list of the IMAGE, the columns and rows it was last placed in, and whether the frame being painted has it.")
   (placing :initform '() :accessor tui-placing
            :documentation "The pictures the frame being painted places at their cells, where the terminal draws no picture in place of text (PICTURES-IN-TEXT-P): each a list of the picture's number, its placement's number, the row and column, and the control keys of its size.")
   (placed :initform '() :accessor tui-placed
           :documentation "The pictures the frame painted last placed at their cells, as TUI-PLACING has them.")
   (next-image :initform 0 :accessor tui-next-image
               :documentation "How many pictures have been sent to the terminal: the next one's number there is made from it (KITTY-IMAGE-ID), the same again every 4,096 pictures."))
  (:documentation "The ncurses terminal frontend."))

(defun make-tui-frontend ()
  "Return a terminal frontend."
  (make-instance 'tui-frontend))

(defun stdscr ()
  "The ncurses standard screen."
  (c-variable "stdscr" sb-sys:system-area-pointer))

(defvar *saved-stderr* nil
  "The terminal's standard error, a descriptor, while DIVERT-STDERR has it sent to a log; else NIL.")

(defun divert-stderr ()
  "Send standard error to stderr.log in the cache directory while the screen
is the editor's: what SBCL's runtime, a library or a program the editor runs
writes there would land in the middle of the screen. The log holds the
current session's (and that of another session still writing it); the
sessions before are kept beside it (TUATARA.IO:SESSION-LOG)."
  (unless *saved-stderr*
    (let ((fd (tuatara.io:session-log "the terminal")))
      (when fd
        (setf *saved-stderr* (sb-posix:fcntl 2 1030 10)) ; F_DUPFD_CLOEXEC: kept from programs the editor runs
        (sb-posix:dup2 fd 2)
        (sb-posix:close fd)))))

(defun restore-stderr ()
  "Give standard error back to the terminal (DIVERT-STDERR)."
  (when *saved-stderr*
    (ignore-errors (sb-posix:dup2 *saved-stderr* 2))
    (ignore-errors (sb-posix:close *saved-stderr*))
    (setf *saved-stderr* nil)))

(defun set-terminal-modes (modes on)
  "Switch the terminal's MODES, a list of their numbers (CSI ? number h), on
when ON is true, else off."
  (dolist (mode modes)
    (terminal-control "[?~d~a" mode (if on "h" "l"))))

(defun tell-terminal-mouse (on)
  "Ask the terminal to report the mouse to Tuatara (ON true: its buttons, its
moves while a button is held, the wheel, in SGR's form), or to stop."
  (set-terminal-modes '(1002 1006) on))

(defun tell-terminal-pastes (on)
  "Ask the terminal to mark text pasted into it (ON true), so that Tuatara
takes it in as text and never as keys (READ-PASTE), or to stop."
  (set-terminal-modes '(2004) on))

(defmethod tuatara.frontend:frontend-start ((f tui-frontend))
  (load-ncurses)
  (%setlocale 6 "")                     ; LC_ALL: let ncurses handle UTF-8
  (setf *screen* nil)
  (start-screen f)
  (divert-stderr)
  (setf *kitty-keyboard-decided* nil
        *terminal-answers* :unasked)
  (when *tui-mouse* (tell-terminal-mouse t))
  (tell-terminal-pastes t)
  (setf (tui-wake f) (tuatara.io:make-wake-pipe)))

(defun start-screen (f)
  "Start ncurses on the terminal for the colours asked for (WANTED-COLOURS):
the first time, or again in place of the screen before, which has ended."
  ;; for 24-bit colour ncurses needs a terminal description that has it; TERM
  ;; says which only while ncurses starts, so that programs run later see TERM
  ;; as it was
  (let* ((wanted (wanted-colours))
         (direct (direct-terminal-type :wanted wanted))
         (term (uiop:getenv "TERM"))
         (before *screen*)
         (screen (progn (when direct (sb-posix:setenv "TERM" direct 1))
                        (unwind-protect
                             (%newterm (or direct term) (c-variable "stdout" sb-sys:system-area-pointer)
                                       (c-variable "stdin" sb-sys:system-area-pointer))
                          (when direct (sb-posix:setenv "TERM" term 1))))))
    (when (null-pointer-p screen)
      (error "The terminal ~a is not known (TERM)" (or term "not set")))
    (%set-term screen)
    (when before (%delscreen before))
    (setf *screen* screen *colours-wanted* wanted)
    (setf *colours* (and wanted (%has-colors) (zerop (%start-color))))
    (multiple-value-bind (count why)
        (colours-used wanted (if *colours* (c-variable "COLORS" sb-alien:int) 0) direct)
      (setf *terminal-colours* count *colours-why* why)))
  (setf (tui-window f) (stdscr))
  ;; nonl: the return key arrives as itself (13), not made a newline, so that
  ;; C-j (10) is a key of its own
  (%raw) (%noecho) (%nonl)
  (when *colours*
    (%use-default-colors)
    (setf *terminal-pairs* (c-variable "COLOR_PAIRS" sb-alien:int)
          *direct-colour* (>= *terminal-colours* #x1000000)
          *next-pair* 1)
    (clrhash *pairs*)
    (clrhash (tuatara.display:face-cache-table *spec-cache*)))
  (%keypad (tui-window f) t)
  ;; frames painted whole, keys waiting or not: the editor decides when to skip one
  (%typeahead -1)
  (%set-escdelay 25)
  (%curs-set 1)
  (setf (tui-mouse f) *tui-mouse*))

(defun kitty-keyboard (on)
  "Switch the kitty keyboard protocol on (ON true) or back off: on, keys are
told apart (1) and a key with Shift is reported with what it types (4)."
  (terminal-control (if on "[>5u" "[<u")))

(defun give-terminal-back (f)
  "Leave F's terminal as it was before the editor: its pictures gone, and its
cursor, keys, mouse and pastes its own again; ncurses ended."
  (forget-images f t)
  (terminal-control "[0 q")
  (terminal-control "]112~c" (code-char 7))
  (when *kitty-keyboard-on* (kitty-keyboard nil))
  (when (tui-mouse f) (tell-terminal-mouse nil))
  (tell-terminal-pastes nil)
  (setf *shown-cursor-shape* nil *shown-cursor-colour* nil)
  (%endwin))

(defun take-terminal-again (f kitty mouse)
  "Take F's terminal back after GIVE-TERMINAL-BACK: the kitty keyboard
protocol on again when KITTY, the mouse reported when MOUSE, pastes marked,
and the screen painted whole, as what was drawn on it meanwhile is gone."
  (when kitty (kitty-keyboard t))
  (when mouse (tell-terminal-mouse t))
  (tell-terminal-pastes t)
  (%clearok (tui-window f) t)
  (%wrefresh (tui-window f)))

(defmethod tuatara.frontend:frontend-stop ((f tui-frontend))
  (stop-clipboard-watch)
  (forget-shared-pictures)
  (let ((wake (tui-wake f)))
    (when wake
      (setf (tui-wake f) nil)
      (tuatara.io:close-wake-pipe wake)))
  (when (tui-window f)
    (give-terminal-back f)
    (setf *kitty-keyboard-on* nil (tui-mouse f) nil (tui-window f) nil))
  (restore-stderr))

(defmethod tuatara.frontend:frontend-terminal-device ((f tui-frontend))
  (and (tui-window f) (ignore-errors (%ttyname 0))))

(defmethod tuatara.frontend:frontend-lend-terminal ((f tui-frontend) function)
  (if (null (tui-window f))
      (funcall function)
      (let ((kitty *kitty-keyboard-on*) (mouse (tui-mouse f)))
        (give-terminal-back f)
        (unwind-protect (funcall function)
          (take-terminal-again f kitty mouse)))))

(defmethod tuatara.frontend:frontend-colour-count ((f tui-frontend))
  (if *colours* *terminal-colours* 0))

(defun terminal-name ()
  "The terminal Tuatara runs in, as far as its environment says: kitty, the
TERM_PROGRAM a terminal sets, else TERM."
  (cond ((uiop:getenv "KITTY_WINDOW_ID") "kitty")
        ((let ((program (uiop:getenv "TERM_PROGRAM"))) (and program (plusp (length program)) program)))
        (t (or (uiop:getenv "TERM") "a terminal"))))

(defmethod tuatara.frontend:frontend-description ((f tui-frontend))
  (format nil "in the terminal (~a)" (terminal-name)))

(defmethod tuatara.frontend:frontend-health ((f tui-frontend))
  (let ((colours (tuatara.frontend:frontend-colour-count f))
        (programs (clipboard-programs)))
    (list (cons "Display" (format nil "this terminal (TERM ~a), ~a (~a), ~a" (or (uiop:getenv "TERM") "not set")
                                  (cond ((>= colours 16777216) "24-bit colour")
                                        ((plusp colours) (format nil "~d colours" colours))
                                        (t "no colours"))
                                  *colours-why*
                                  (cond ((tuatara.frontend:frontend-images-p f) "pictures (kitty graphics)")
                                        ((null *tui-pictures*) "no pictures (*tui-pictures* is nil)")
                                        ((multiplexer-p) "no pictures (inside tmux or screen; *tui-pictures* t sends them)")
                                        (t "no pictures (the terminal does not say it draws kitty's)"))))
          (cons "Clipboard" (if programs
                                (format nil "~a and ~a" (file-namestring (first (first programs))) (file-namestring (first (second programs))))
                                "no program for it found (wl-copy, xclip or xsel): kills stay in the editor")))))

(defun inner-size (f)
  "Two values: the columns and rows of F's terminal inside the room round the
text (TUI-PADDING), which is what the editor has to lay out."
  (destructuring-bind (top right bottom left) (tui-padding f)
    (values (max 1 (- (%getmaxx (tui-window f)) left right))
            (max 1 (- (%getmaxy (tui-window f)) top bottom)))))

(defmethod tuatara.frontend:frontend-size ((f tui-frontend))
  (inner-size f))

(defun resize-event (f)
  "An event saying F's terminal is now the size it is (INNER-SIZE)."
  (multiple-value-bind (width height) (inner-size f)
    (tuatara.frontend:make-event :resize :width width :height height)))

(defun queue-resize (f)
  "Have F give the editor a RESIZE-EVENT after the events already waiting."
  (setf (tui-pending f) (append (tui-pending f) (list (resize-event f)))))

(defmethod tuatara.frontend:frontend-settings-changed ((f tui-frontend))
  ;; other colours asked for: the screen started again for them, and drawn whole
  (when (and (tui-window f) (not (eql (wanted-colours) *colours-wanted*)))
    (%endwin)
    (start-screen f)
    (setf *shown-cursor-shape* nil *shown-cursor-colour* nil)
    (%clearok (tui-window f) t)
    (queue-resize f))
  (when (and (tui-window f) (not (eq (and *tui-mouse* t) (tui-mouse f))))
    (setf (tui-mouse f) (and *tui-mouse* t))
    (tell-terminal-mouse (tui-mouse f)))
  (when (tui-window f)
    (let ((sides (multiple-value-list (tuatara.frontend:padding-sides *tui-padding*))))
      (unless (equal sides (tui-padding f))
        (setf (tui-padding f) sides)
        (queue-resize f)))))

(defun read-wide-char (window)
  "Two values from wget_wch: the status (0 for a character, 256 for a key code, -1 for none) and the value."
  (sb-alien:with-alien ((wch (sb-alien:unsigned 32) 0))
    (let ((status (%wget-wch window (sb-alien:alien-sap (sb-alien:addr wch)))))
      (values status wch))))

(defun char-key (code &key meta)
  "The KEY for a character CODE from the terminal, decoding control characters:
13 is the return key, 10 C-j, 28 to 31 C-\\, C-], C-^ and C-_ (what C-/ sends)."
  (cond ((= code 27) (tuatara.frontend:make-key :escape :meta meta))
        ((= code 13) (tuatara.frontend:make-key :enter :meta meta))
        ((= code 9) (tuatara.frontend:make-key :tab :meta meta))
        ((= code 127) (tuatara.frontend:make-key :backspace :meta meta))
        ((= code 0) (tuatara.frontend:make-key #\Space :ctrl t :meta meta))
        ((< code 27) (tuatara.frontend:make-key (code-char (+ code 96)) :ctrl t :meta meta))
        ((< code 32) (tuatara.frontend:make-key (code-char (+ code 64)) :ctrl t :meta meta))
        (t (tuatara.frontend:make-key (code-char code) :meta meta))))

(defun decide-kitty-keyboard ()
  "Switch the kitty keyboard protocol on where *KITTY-KEYBOARD* and the terminal
say so (KITTY-KEYBOARD-SUPPORTED-P), once."
  (unless *kitty-keyboard-decided*
    (setf *kitty-keyboard-decided* t
          *kitty-keyboard-on* (case *kitty-keyboard*
                                ((nil) nil)
                                ((t) t)
                                (t (terminal-answer :keyboard))))
    (when *kitty-keyboard-on*
      (kitty-keyboard t))))

(defun read-terminal-reply (seconds done)
  "The octets the terminal sends on standard input within SECONDS, read
straight from it (before ncurses reads any), until DONE, a function of the
octets so far, is true."
  (let ((octets (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (buffer (make-array 256 :element-type '(unsigned-byte 8)))
        (deadline (tuatara.runtime:deadline-in seconds)))
    (loop
      (let ((left (tuatara.runtime:seconds-left deadline)))
        (when (or (<= left 0) (funcall done octets)) (return octets))
        (let ((ready (tuatara.io:wait-for-fds (list 0) left)))
          (when (and (consp ready) (member 0 ready))
            (let ((n (sb-unix:unix-read 0 (sb-sys:vector-sap buffer) (length buffer))))
              (if (and n (plusp n))
                  (loop for i below n do (vector-push-extend (aref buffer i) octets))
                  (return octets)))))))))

(defun terminal-answer (key)
  "Whether the terminal does KEY, as it said when asked (ASK-TERMINAL, the
first time anything wants to know): :KEYBOARD, the kitty keyboard protocol;
:PICTURES, kitty's pictures; :SHARED, pictures read from shared memory."
  (when (eq *terminal-answers* :unasked)
    (setf *terminal-answers* (or (ignore-errors (ask-terminal)) '())))
  (getf *terminal-answers* key))

(defun ask-terminal ()
  "Ask the terminal, in one go, whether it speaks the kitty keyboard protocol
\(CSI ? u), whether it draws kitty's pictures (a picture of one pixel offered
to it, not kept), and, on this machine, whether it reads them from shared
memory (one more such picture, put there), pictures not asked about inside
tmux or screen (MULTIPLEXER-P); then for its primary device
attributes (CSI c), which every terminal answers after the rest, so that one
that knows none of these answers only that, and nothing waits longer than a
quarter of a second. Keys typed meanwhile are handed back. Returns the
answers as TERMINAL-ANSWERS-IN reads them."
  (let* ((pictures (not (multiplexer-p)))
         (shared (and pictures (local-terminal-p) (shared-probe))))
    (unwind-protect
         (progn
           (when pictures
             (kitty-command "a=q,i=31,s=1,v=1,t=d,f=24" "AAAA"))
           (when shared
             (kitty-command "a=q,i=32,s=1,v=1,t=s,f=24,S=3" (shared-name-payload shared)))
           (terminal-control "[?u")
           (terminal-control "[c")
           (let* ((reply (read-terminal-reply 1/4 (lambda (octets)
                                                    (cl-ppcre:scan (format nil "~c\\[\\?[0-9;]*c" #\Escape)
                                                                   (sb-ext:octets-to-string octets :external-format :latin-1)))))
                  (text (sb-ext:octets-to-string reply :external-format :latin-1)))
             (give-back-keys (cl-ppcre:regex-replace-all (format nil "~c\\[\\?[0-9;]*[uc]|~c_G[^~c]*~c\\\\" #\Escape #\Escape #\Escape #\Escape)
                                                         text ""))
             (terminal-answers-in text)))
      (when shared (ignore-errors (sb-posix:unlink (shared-picture-path shared)))))))

(defun multiplexer-p ()
  "True when Tuatara runs inside tmux or screen, which take a picture's
commands for something else (the window's title), so that pictures are asked
about and shown there only when *TUI-PICTURES* is T."
  (let ((term (or (uiop:getenv "TERM") "")))
    (or (uiop:getenv "TMUX") (uiop:getenv "STY")
        (uiop:string-prefix-p "screen" term) (uiop:string-prefix-p "tmux" term))))

(defun shared-probe ()
  "The name of a new shared memory object holding one black pixel, for
ASK-TERMINAL to offer the terminal; NIL when none could be made."
  (make-shared-picture (make-array 3 :element-type '(unsigned-byte 8) :initial-element 0)))

(defun shared-picture-path (name)
  "Where the shared memory object NAME is in the file system."
  (concatenate 'string "/dev/shm/" name))

(defun shared-name-payload (name)
  "The shared memory object NAME as a graphics command names it: in base 64."
  (let ((octets (sb-ext:string-to-octets (concatenate 'string "/" name) :external-format :latin-1)))
    (tuatara.io:base64-string octets)))

(defun shared-picture-name ()
  "A new name for a shared memory object holding a picture: tuatara-, this
process's number, and a random part no one can guess, new at every start."
  (format nil "tuatara-~d-~a" (sb-posix:getpid) (tuatara.io:random-name)))

(defun make-shared-picture (octets)
  "The name of a new shared memory object holding OCTETS, readable by this
user alone; NIL when none could be made."
  (let* ((name (shared-picture-name))
         (path (shared-picture-path name))
         (fd (handler-case (sb-posix:open path (logior sb-posix:o-creat sb-posix:o-excl sb-posix:o-rdwr) #o600)
               (error () nil))))
    (when fd
      (unwind-protect
           (handler-case (progn (tuatara.io:write-octets fd octets :path name) name)
             (error () (ignore-errors (sb-posix:unlink path)) nil))
        (sb-posix:close fd)))))

(defun terminal-answers-in (text)
  "What TEXT, the terminal's answer to ASK-TERMINAL, says it does, as
TERMINAL-ANSWER has it: :KEYBOARD, :PICTURES and :SHARED, each true or NIL."
  (flet ((ok-p (id) (and (cl-ppcre:scan (format nil "~c_Gi=~d;OK~c\\\\" #\Escape id #\Escape) text) t)))
    (list :keyboard (kitty-keyboard-reply-p text) :pictures (ok-p 31) :shared (ok-p 32))))

(defvar *keys-given-back* nil
  "True from when keys are handed back to ncurses (GIVE-BACK-KEYS) until reading
finds none left: keys are waiting that the terminal no longer has to send.")

(defun give-back-keys (text)
  "Hand ncurses back TEXT, octets as characters, typed while the terminal's
answer was read, so that they are read as keys in their order."
  (when (plusp (length text)) (setf *keys-given-back* t))
  (loop for i from (1- (length text)) downto 0
        do (%ungetch (char-code (char text i)))))

(defun kitty-keyboard-reply-p (text)
  "True when TEXT, what the terminal answered, holds a reply to CSI ? u (CSI ?
flags u): it speaks the kitty keyboard protocol."
  (and (cl-ppcre:scan (format nil "~c\\[\\?[0-9]*u" #\Escape) text) t))

(defparameter *kitty-key-codes*
  '((27 . :escape) (13 . :enter) (9 . :tab) (127 . :backspace)
    (57414 . :enter) (57417 . :left) (57418 . :right) (57419 . :up) (57420 . :down)
    (57421 . :page-up) (57422 . :page-down) (57423 . :home) (57424 . :end)
    (57425 . :insert) (57426 . :delete)
    (57399 . #\0) (57400 . #\1) (57401 . #\2) (57402 . #\3) (57403 . #\4) (57404 . #\5)
    (57405 . #\6) (57406 . #\7) (57407 . #\8) (57408 . #\9) (57409 . #\.) (57410 . #\/)
    (57411 . #\*) (57412 . #\-) (57413 . #\+) (57415 . #\=) (57416 . #\,))
  "Key codes of the kitty keyboard protocol that stand for another key: Escape,
return, Tab, backspace, and the keypad's keys.")

(defparameter *us-shifted-characters*
  (let ((table (make-hash-table)))
    (loop for plain across "`1234567890-=[]\\;',./" for shifted across "~!@#$%^&*()_+{}|:\"<>?"
          do (setf (gethash plain table) shifted))
    table)
  "What each key of a US keyboard types with Shift, by what it types without:
the guess for a key kitty's protocol reports with Shift but without what it
types then.")

(defun kitty-key (parameters)
  "The KEY a CSI u sequence of the kitty keyboard protocol stands for, its
PARAMETERS the text between CSI and u (code[:shifted[:base]];modifiers[;text]);
NIL for a key of no use here (a lock key alone, say). A key with Shift is
what it types with Shift (M-> rather than M-S-.), as the window frontend has
it (by a US keyboard when the terminal does not say); Shift stays only on
keys that type nothing."
  (let* ((fields (uiop:split-string parameters :separator ";"))
         (codes (uiop:split-string (first fields) :separator ":"))
         (code (ignore-errors (parse-integer (first codes))))
         (shifted-code (and (second codes) (plusp (length (second codes))) (ignore-errors (parse-integer (second codes))))))
    (multiple-value-bind (shift meta ctrl) (modifier-bits (second fields))
      (when code
        (let ((special (cdr (assoc code *kitty-key-codes*))))
          (cond ((keywordp special) (tuatara.frontend:make-key special :ctrl ctrl :meta meta :shift shift))
                ((or (< code 32) (and (null special) (<= 57344 code 63743))) nil)
                (t (let ((char (or special (code-char code))))
                     (cond ((and shift shifted-code (>= shifted-code 32))
                            (tuatara.frontend:make-key (code-char shifted-code) :ctrl ctrl :meta meta))
                           ((and shift (not shifted-code) (gethash char *us-shifted-characters*))
                            (tuatara.frontend:make-key (gethash char *us-shifted-characters*) :ctrl ctrl :meta meta))
                           ((and shift (alpha-char-p char))
                            (tuatara.frontend:make-key (char-upcase char) :ctrl ctrl :meta meta))
                           (t (tuatara.frontend:make-key char :ctrl ctrl :meta meta
                                                              :shift (and shift (not (graphic-char-p char))))))))))))))

(defparameter *csi-tilde-keys*
  '((2 . :insert) (3 . :delete) (5 . :page-up) (6 . :page-down) (7 . :home) (8 . :end)
    (11 . :f1) (12 . :f2) (13 . :f3) (14 . :f4) (15 . :f5) (17 . :f6) (18 . :f7) (19 . :f8)
    (20 . :f9) (21 . :f10) (23 . :f11) (24 . :f12))
  "The keys of CSI number ~ sequences, by their number.")

(defun csi-key (parameters final)
  "The KEY a CSI sequence stands for, its PARAMETERS the text between CSI and
its FINAL character: kitty's CSI u, and the special keys with modifiers as
xterm sends them (CSI 1;5A, CSI 3;3~); NIL for any other, a terminal's
answer among them."
  (unless (find #\? parameters)
    (let ((fields (uiop:split-string parameters :separator ";")))
      (multiple-value-bind (shift meta ctrl) (modifier-bits (second fields))
        (let ((base (case final
                      (#\u (return-from csi-key (kitty-key parameters)))
                      (#\Z (return-from csi-key (tuatara.frontend:make-key :tab :shift t)))
                      (#\A :up) (#\B :down) (#\C :right) (#\D :left) (#\H :home) (#\F :end)
                      (#\P :f1) (#\Q :f2) (#\S :f4)
                      (#\~ (cdr (assoc (ignore-errors (parse-integer (first fields))) *csi-tilde-keys*))))))
          (and base (tuatara.frontend:make-key base :ctrl ctrl :meta meta :shift shift)))))))

(defconstant +key-mouse+ 409 "ncurses' code for the start of a report of the mouse.")

(defparameter *click-seconds* 1/2
  "The most time between presses of the first button on one cell for them to
count as a double or triple click.")

(defun mouse-key (f parameters final)
  "The mouse key of F's terminal's report, PARAMETERS the text between CSI and
FINAL in SGR's form (<BUTTON;COLUMN;ROW, M pressed or moved, m let go): the
first button's press (a second or third on the cell soon after, a double or
triple), its moves while held, its letting go, the middle button's press,
the wheel; at the cell of the editor's screen (the padding taken off), with
Control, Meta and Shift as held. NIL for the other buttons."
  (let ((fields (mapcar (lambda (field) (ignore-errors (parse-integer field))) (uiop:split-string (subseq parameters 1) :separator ";"))))
    (destructuring-bind (&optional button x y &rest more) fields
      (declare (ignore more))
      (when (and button x y)
        (let* ((column (- x 1 (fourth (tui-padding f)))) (row (- y 1 (first (tui-padding f))))
               (shift (logtest button 4)) (meta (logtest button 8)) (ctrl (logtest button 16))
               (moved (logtest button 32)) (wheel (logtest button 64)) (which (logand button 3))
               (base (cond (wheel (and (char= final #\M) (case which (0 :wheel-up) (1 :wheel-down))))
                           ((= which 1) (and (char= final #\M) (not moved) :mouse-2))
                           ((/= which 0) nil)
                           ((char= final #\m) :up-mouse-1)
                           (moved :drag-mouse-1)
                           (t (let* ((now (get-internal-real-time)) (last (tui-clicks f))
                                     (count (if (and last (= column (first last)) (= row (second last))
                                                     (< (- now (third last)) (* *click-seconds* internal-time-units-per-second)))
                                                (min 3 (1+ (fourth last)))
                                                1)))
                                (setf (tui-clicks f) (list column row now count))
                                (case count (1 :down-mouse-1) (2 :double-mouse-1) (t :triple-mouse-1)))))))
          (and base (tuatara.frontend:make-key base :ctrl ctrl :meta meta :shift shift :column column :row row)))))))

(defun read-csi (window &optional first)
  "The parameters and the final character of a CSI sequence whose ESC [ has
been read from WINDOW, FIRST its next character's code when read already;
the rest waited for at most 1/10 s, as from a busy terminal or over a slow
connection it may come in parts. NIL when it breaks off."
  (let ((parameters (make-string-output-stream))
        (deadline (tuatara.runtime:deadline-in 1/10)))
    (%nodelay window t)
    (unwind-protect
         (flet ((next ()
                  (loop
                    (multiple-value-bind (status value) (read-wide-char window)
                      (cond ((= status 0) (return value))
                            ((= status +key-code-yes+) (return nil))
                            (t (let ((left (tuatara.runtime:seconds-left deadline)))
                                 (unless (and (plusp left) (consp (tuatara.io:wait-for-fds (list 0) left)))
                                   (return nil)))))))))
           (loop for value = (or (shiftf first nil) (next))
                 do (cond ((null value) (return nil))
                          ((<= #x40 value #x7e) (return (values (get-output-stream-string parameters) (code-char value))))
                          ((<= #x20 value #x3f) (write-char (code-char value) parameters))
                          (t (return nil)))))
      (%nodelay window nil))))

(defparameter *paste-end* (coerce (list #\Escape #\[ #\2 #\0 #\1 #\~) 'string)
  "What the terminal sends after text pasted into it.")

(defparameter *paste-limit* (* 8 1024 1024)
  "The most characters of one paste taken in: the rest is read and left out.")

(defparameter *paste-pause-seconds* 1
  "How long the rest of a paste may keep the terminal waiting before what has
come is taken as all of it, for a paste whose end never comes.")

(defparameter *paste-most-seconds* 60
  "The longest one paste is read for, however steadily it comes.")

(defstruct (paste (:constructor make-paste (&optional (limit *paste-limit*))) (:copier nil))
  "A paste being read from the terminal (PASTE-TAKE): its TEXT so far, at most
LIMIT characters, carriage returns made newlines; how many characters of the
paste's end (*PASTE-END*) have just come, MATCHED, held back until it is
known whether they are its end; AFTER-RETURN, true after a carriage return,
whose newline is then left out; and CUT, true once characters were left out
past LIMIT."
  (text (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
  (limit *paste-limit*)
  (matched 0)
  (after-return nil)
  (cut nil))

(defun paste-add (paste char)
  "Add CHAR to PASTE's text: a carriage return as a newline, a newline right
after one left out, nothing once the text has its limit."
  (let ((after-return (paste-after-return paste)))
    (setf (paste-after-return paste) (char= char #\Return))
    (unless (and after-return (char= char #\Newline))
      (if (< (fill-pointer (paste-text paste)) (paste-limit paste))
          (vector-push-extend (if (char= char #\Return) #\Newline char) (paste-text paste))
          (setf (paste-cut paste) t)))))

(defun paste-take (paste code)
  "Take the character of CODE, read from the terminal, into PASTE; true once
it ended the paste (*PASTE-END*). The characters of the end are not part of
the text; any that only began like it are."
  (let ((char (or (code-char code) (code-char #xFFFD))) (end *paste-end*))
    (loop
      (let ((matched (paste-matched paste)))
        (cond ((char= char (char end matched))
               (setf (paste-matched paste) (1+ matched))
               (return (= (1+ matched) (length end))))
              ((zerop matched)
               (paste-add paste char)
               (return nil))
              (t
               ;; what looked like the end was text after all: it goes in, and CHAR is looked at afresh
               (loop for i below matched do (paste-add paste (char end i)))
               (setf (paste-matched paste) 0)))))))

(defun paste-string (paste)
  "PASTE's text as a simple string."
  (coerce (paste-text paste) 'simple-string))

(defun read-paste (f window)
  "Read the text pasted into F's terminal, whose start has been read from
WINDOW, up to its end (*PASTE-END*): every character as itself, never as a
key, at most *PASTE-LIMIT* of them kept (saying so when more came). It ends early when nothing more
comes for *PASTE-PAUSE-SECONDS* or after *PASTE-MOST-SECONDS*, so that a
paste that never ends cannot hold the editor. A resize meanwhile waits in
F's pending events. Returns the :PASTE event."
  (let ((paste (make-paste))
        (most (tuatara.runtime:deadline-in *paste-most-seconds*))
        (pause (tuatara.runtime:deadline-in *paste-pause-seconds*)))
    ;; without the keypad ncurses takes no characters together as a key
    (%keypad window nil)
    (%nodelay window t)
    (unwind-protect
         (loop
           (multiple-value-bind (status value) (read-wide-char window)
             (cond ((= status 0)
                    (when (paste-take paste value) (return))
                    (setf pause (tuatara.runtime:deadline-in *paste-pause-seconds*)))
                   ((= status +key-code-yes+)
                    (when (= value 410)
                      (queue-resize f)))
                   (t (let ((left (min (tuatara.runtime:seconds-left pause) (tuatara.runtime:seconds-left most))))
                        (unless (and (plusp left) (consp (tuatara.io:wait-for-fds (list 0) left)))
                          (return)))))))
      (%nodelay window nil)
      (%keypad window t))
    (when (paste-cut paste)
      (tuatara-user:message "Only the first ~:d characters of the paste were taken in" (paste-limit paste)))
    (tuatara.frontend:make-event :paste :text (paste-string paste))))

(defun paste-start-p (parameters final)
  "True when the CSI sequence of PARAMETERS and FINAL is the start of text
pasted into the terminal (CSI 200 ~)."
  (and (char= final #\~) (string= parameters "200")))

(defun read-wide-char-soon (window seconds)
  "As READ-WIDE-CHAR, from WINDOW in no-delay mode, but waiting up to SECONDS
for a character not there yet: the rest of a key's sequence that comes in a
second part."
  (multiple-value-bind (status value) (read-wide-char window)
    (if (and (/= status 0) (/= status +key-code-yes+)
             (consp (tuatara.io:wait-for-fds (list 0) seconds)))
        (read-wide-char window)
        (values status value))))

(defun wide-char-event (f window status value)
  "The event for what wget_wch read from F's WINDOW: STATUS 0 and a character
VALUE, or a key code, or nothing at all (the end of input)."
  (cond ((= status +key-code-yes+)
         (cond ((= value 410)
                (resize-event f))
               ((= value 353) (tuatara.frontend:make-event :key :key (tuatara.frontend:make-key :tab :shift t)))
               ;; the mouse's report, its start (ESC [ <) taken by ncurses as the terminal's
               ;; description says: the rest read here
               ((= value +key-mouse+)
                (multiple-value-bind (parameters final) (read-csi window)
                  (let ((key (and final (mouse-key f (concatenate 'string "<" parameters) final))))
                    (if key
                        (tuatara.frontend:make-event :key :key key)
                        (tuatara.frontend:make-event :tick)))))
               (t (let ((key (ncurses-key value)))
                    ;; a key of no use here: as no key at all
                    (if key
                        (tuatara.frontend:make-event :key :key key)
                        (tuatara.frontend:make-event :tick))))))
        ((/= status 0) (tuatara.frontend:make-event :quit))
        ((= value 27)
         ;; ESC: a key in a CSI sequence (kitty's protocol, or one ncurses does not
         ;; know), or a terminal's late answer; else Meta's prefix if another key
         ;; follows at once, or a lone escape
         ;; by kitty's protocol a lone ESC is never a key (Escape is CSI 27 u): the
         ;; rest of its sequence is waited for, never taken for keys typed; after
         ;; ESC [ the rest is waited for in every mode, as a sequence may come in
         ;; two parts (over a network), and a paste must never be read as keys
         (%nodelay window t)
         (multiple-value-bind (status2 value2 status3 value3)
             (multiple-value-bind (status2 value2) (if *kitty-keyboard-on*
                                                       (read-wide-char-soon window 1/10)
                                                       (read-wide-char window))
               (if (and (= status2 0) (= value2 (char-code #\[)))
                   (multiple-value-bind (status3 value3) (read-wide-char-soon window 1/10)
                     (values status2 value2 status3 value3))
                   (values status2 value2 -1 0)))
           (%nodelay window nil)
           (cond ((and (= status2 0) (= value2 (char-code #\[))
                       (or *kitty-keyboard-on* (= status3 0)))
                  (multiple-value-bind (parameters final) (read-csi window (and (= status3 0) value3))
                    (if (and final (paste-start-p parameters final))
                        ;; text pasted: read whole, to go in as text and never as keys
                        (read-paste f window)
                        (let ((key (and final (if (and (plusp (length parameters)) (char= (char parameters 0) #\<))
                                                  (mouse-key f parameters final)
                                                  (csi-key parameters final)))))
                          (if key
                              (tuatara.frontend:make-event :key :key key)
                              ;; a sequence of no key, or one not read: as no key at all
                              (tuatara.frontend:make-event :tick))))))
                 ((= status2 0)
                  (tuatara.frontend:make-event :key :key (char-key value2 :meta t)))
                 (t (tuatara.frontend:make-event :key :key (tuatara.frontend:make-key :escape))))))
        (t (tuatara.frontend:make-event :key :key (char-key value)))))

(defun next-event (f seconds)
  "The next event from the terminal of F, waiting SECONDS at most (NIL: as long
as it takes): a key, a resize, or a :TICK when the time ran out or another
thread called FRONTEND-WAKE; :QUIT once the terminal is gone. What ncurses has
read ahead, and a resize it was told of, come first; then the terminal and the
wake pipe are waited on together, a second at a time, since the resize signal
may reach another thread and so not end the wait."
  (let ((pending (pop (tui-pending f))))
    (when pending (return-from next-event pending)))
  (decide-kitty-keyboard)
  (let ((window (tui-window f))
        (wake (tui-wake f))
        (deadline (and seconds (tuatara.runtime:deadline-in seconds)))
        (empty 0))                     ; the terminal said it had input, and ncurses found none
    (loop
      (multiple-value-bind (status value)
          (progn (%nodelay window t)
                 (unwind-protect (read-wide-char window) (%nodelay window nil)))
        (when (or (= status 0) (= status +key-code-yes+))
          (return (wide-char-event f window status value)))
        (setf *keys-given-back* nil))
      (let* ((left (and deadline (tuatara.runtime:seconds-left deadline)))
             (ready (if (and left (<= left 0))
                        '()
                        (tuatara.io:wait-for-fds (list 0 (car wake)) (if left (min left 1) 1)))))
        (cond ((and (consp ready) (member (car wake) ready))
               (tuatara.io:drain-wake-pipe wake)
               (return (tuatara.frontend:make-event :tick)))
              ((and (consp ready) (member 0 ready))
               ;; readable again and again with nothing to read: the terminal hung up
               (when (> (incf empty) 50) (return (tuatara.frontend:make-event :quit))))
              ((and left (<= left 0))
               (return (tuatara.frontend:make-event :tick))))))))

(defmethod tuatara.frontend:frontend-read-event ((f tui-frontend))
  (next-event f nil))

(defmethod tuatara.frontend:frontend-read-event-timeout ((f tui-frontend) seconds)
  (next-event f seconds))

(defmethod tuatara.frontend:frontend-wake ((f tui-frontend))
  (let ((wake (tui-wake f)))
    (when wake (tuatara.io:wake wake))))

(defmethod tuatara.frontend:frontend-input-pending-p ((f tui-frontend))
  ;; ncurses reads the terminal a byte at a time as keys are asked for, so
  ;; typed-ahead keys wait in the terminal's queue on standard input; besides
  ;; those, the events kept to hand over first and the keys handed back
  (and (or (tui-pending f) *keys-given-back* (sb-sys:wait-until-fd-usable 0 :input 0)) t))

(defvar *background-generation* -1
  "The face generation the window's background was last painted for.")

(defmethod tuatara.frontend:frontend-begin-frame ((f tui-frontend))
  (let ((window (tui-window f)))
    ;; the :default face (a theme's text colour and background) becomes the
    ;; window's background, so erased cells and unfaced text show it
    (unless (= *background-generation* tuatara.display:*face-generation*)
      (%wbkgd window (background-character (spec-attributes :default)))
      (setf *background-generation* tuatara.display:*face-generation*))
    (%werase window)))

(defparameter *spaces* (make-string 256 :initial-element #\Space)
  "Spaces, for a row's face to be painted past its text.")

(defmethod tuatara.frontend:frontend-put-row ((f tui-frontend) row string &key face spans scales over parts)
  (declare (ignore scales over parts))     ; a terminal's rows are all one size, painted as ncurses keeps them
  ;; nothing the terminal cannot be sent or that would move its cursor
  (setf string (tuatara.display:displayable string))
  ;; a mark beginning the row would be drawn on the cell before it: on a dotted circle
  (when (and (plusp (length string)) (tuatara.display:mark-code-p (char-code (char string 0))))
    (setf string (concatenate 'string (string tuatara.display:*mark-base*) string)
          spans (loop for (start end span-face) in spans
                      collect (list (if (zerop start) 0 (1+ start)) (1+ end) span-face))))
  (let* ((window (tui-window f))
         (row-attrs (if face (spec-attributes face) 0)))
    (destructuring-bind (top right bottom left) (tui-padding f)
      ;; a row past the window's last is not drawn: it would be written over the end of the last
      (when (< (+ row top) (- (%getmaxy window) bottom))
        (let* ((edge (- (%getmaxx window) right))
               (string (clip-to-columns string (- edge left))))
          (flet ((put (text start end attrs)
                   (when (< start end)
                     (when (plusp attrs) (attributes-on window attrs))
                     (tuatara.io:with-c-string (octets text :start start :end end)
                       (%waddstr-octets window octets))
                     (when (plusp attrs) (attributes-off window)))))
            (%wmove window (+ row top) left)
            (let ((pos 0) (length (length string)))
              (loop for (span-start end span-face) in spans
                    ;; spans overlapping one before: only what is past it, so that no column is written twice
                    for start = (max span-start pos)
                    while (< start length)
                    when (< start end)
                      do (put string pos start row-attrs)
                         (put string start (min end length) (spec-attributes (tuatara.display:combine-faces span-face face)))
                         (setf pos (min end length)))
              (put string pos length row-attrs))
            (if face
                ;; a row face covers the whole row: pad from where the text ended to the edge
                (let ((x (%getcurx window)))
                  ;; x is 0 after exactly the width's characters (the cursor wrapped): nothing to pad
                  (when (and (or (zerop (length string)) (> x 0)) (< x edge))
                    (loop for at from x below edge by (length *spaces*)
                          do (put *spaces* 0 (min (length *spaces*) (- edge at)) row-attrs))))
                (%wclrtoeol window))))))))

(defun clip-to-columns (string columns)
  "STRING cut so as to take up no more than COLUMNS columns of the terminal."
  (let ((kept (tuatara.display:chars-in-columns string columns)))
    (if (= kept (length string)) string (subseq string 0 kept))))

;;; The system clipboard, through the programs that serve it

(defvar *clipboard-programs* :unknown
  "The programs that write and read the system clipboard here, as (WRITE READ),
each a program's full name and its arguments, found once: wl-copy and
wl-paste under Wayland, else xclip or xsel under X; NIL when there are none.")

(defun clipboard-programs ()
  "The programs of *CLIPBOARD-PROGRAMS*, found the first time asked."
  (when (eq *clipboard-programs* :unknown)
    (setf *clipboard-programs*
          (flet ((found (write read)
                   (let ((w (tuatara.io:find-program (first write))) (r (tuatara.io:find-program (first read))))
                     (and w r (list (cons w (rest write)) (cons r (rest read)))))))
            (or (and (uiop:getenv "WAYLAND_DISPLAY")
                     (found '("wl-copy" "--type" "text/plain;charset=utf-8") '("wl-paste" "--no-newline" "--type" "text")))
                (and (uiop:getenv "DISPLAY")
                     (or (found '("xclip" "-selection" "clipboard" "-in") '("xclip" "-selection" "clipboard" "-out" "-target" "UTF8_STRING"))
                         (found '("xsel" "--clipboard" "--input") '("xsel" "--clipboard" "--output"))))))))
  *clipboard-programs*)

(defstruct (clipboard-watch (:copier nil))
  "The clipboard kept up to date in the background: PROCESS, wl-paste --watch,
writes a line each time the clipboard changes, which THREAD reads and then
reads the clipboard's text into TEXT; STATE is :READING from a change until its
text is in, then :FRESH; LOCK guards them, CHANGED is told when they change."
  process thread (text nil) (state :reading)
  (lock (sb-thread:make-mutex :name "clipboard"))
  (changed (sb-thread:make-waitqueue)))

(defvar *clipboard-watch* nil
  "The clipboard watched in the background (CLIPBOARD-WATCH), once the first
paste has asked for it, under Wayland; NIL when it is not.")

(defun start-clipboard-watch ()
  "Watch the clipboard in the background, under Wayland with wl-paste: a
CLIPBOARD-WATCH, or NIL when it cannot be watched here (xclip and xsel
cannot say when it changes)."
  (let* ((programs (clipboard-programs))
         (read (second programs))
         (echo (tuatara.io:find-program "echo")))
    (when (and read echo (search "wl-paste" (first read)))
      (let ((process (ignore-errors
                      (sb-ext:run-program (first read) (list "--watch" echo "changed")
                                          :input nil :output :stream :error nil :wait nil
                                          :external-format :utf-8))))
        (when process
          (let ((watch (make-clipboard-watch :process process)))
            (setf (clipboard-watch-thread watch)
                  (tuatara.runtime:make-guarded-thread
                   (lambda ()
                       (unwind-protect
                            (loop for line = (ignore-errors (read-line (sb-ext:process-output process) nil))
                                  while line
                                  do (sb-thread:with-mutex ((clipboard-watch-lock watch))
                                       (setf (clipboard-watch-state watch) :reading))
                                     (multiple-value-bind (text ok) (tuatara.io:program-output read)
                                       (sb-thread:with-mutex ((clipboard-watch-lock watch))
                                         ;; a change noted meanwhile is read again, by its own line
                                         (setf (clipboard-watch-text watch) (and ok text)
                                               (clipboard-watch-state watch) :fresh)
                                         (sb-thread:condition-broadcast (clipboard-watch-changed watch)))))
                         (sb-thread:with-mutex ((clipboard-watch-lock watch))
                           (setf (clipboard-watch-state watch) :ended)
                           (sb-thread:condition-broadcast (clipboard-watch-changed watch)))
                         (ignore-errors (sb-ext:process-close process))))
                   :name "clipboard watch"))
            watch))))))

(defun stop-clipboard-watch ()
  "Stop watching the clipboard."
  (let ((watch (shiftf *clipboard-watch* nil)))
    (when (and watch (not (eq watch :none)))
      (ignore-errors (sb-ext:process-kill (clipboard-watch-process watch) 15)))))

(defun watched-clipboard ()
  "The clipboard's text as the watch has it, waiting a moment for a change
being read; two values, the text and true, or NIL and NIL when there is no
watch to ask (the clipboard is then read at once)."
  (when (null *clipboard-watch*)
    (setf *clipboard-watch* (or (start-clipboard-watch) :none)))
  (let ((watch *clipboard-watch*))
    (when (clipboard-watch-p watch)
      (sb-thread:with-mutex ((clipboard-watch-lock watch))
        (tuatara.runtime:wait-on-until (clipboard-watch-changed watch) (clipboard-watch-lock watch)
                                       (lambda () (not (eq (clipboard-watch-state watch) :reading))) 1)
        (when (eq (clipboard-watch-state watch) :fresh)
          (values (clipboard-watch-text watch) t))))))

(defmethod tuatara.frontend:frontend-set-clipboard ((f tui-frontend) text)
  (let ((programs (clipboard-programs)))
    (and programs (nth-value 1 (tuatara.io:program-output (first programs) :input text :capture nil)))))

(defmethod tuatara.frontend:frontend-clipboard ((f tui-frontend))
  (multiple-value-bind (text watched) (watched-clipboard)
    (if watched
        text
        (let ((programs (clipboard-programs)))
          (when programs
            (multiple-value-bind (text ok) (tuatara.io:program-output (second programs))
              (and ok text)))))))

(defmethod tuatara.frontend:frontend-primary-selection ((f tui-frontend))
  (let ((read (second (clipboard-programs))))
    (when read
      (multiple-value-bind (text ok)
          (tuatara.io:program-output
           ;; the clipboard's program, asked for the primary selection
           (if (string= (file-namestring (first read)) "wl-paste")
               (list* (first read) "--primary" (rest read))
               (substitute "--primary" "--clipboard" (substitute "primary" "clipboard" read :test #'equal) :test #'equal)))
        (and ok text)))))

(defvar *cursor* '(0 . 0) "Cursor position to apply at the end of the frame.")
(defvar *frame-cursor-shape* :block "Cursor shape to apply at the end of the frame.")
(defvar *frame-cursor-colour* nil
  "Cursor colour to apply at the end of the frame, as faces take colours; NIL for the text's.")

(defmethod tuatara.frontend:frontend-set-cursor ((f tui-frontend) row column shape &optional colour)
  (setf *cursor* (cons row column) *frame-cursor-shape* shape *frame-cursor-colour* colour))

(defun cursor-shape-code (shape)
  "The DECSCUSR number of SHAPE, steady: 2 a block, 4 an underline, 6 a bar."
  (case shape (:bar 6) (:underline 4) (t 2)))

(defun cursor-colour-code (colour)
  "The colour the terminal's cursor is given for COLOUR, as faces take colours:
its #rrggbb, for NIL the :cursor face's (TUATARA.DISPLAY:CURSOR-COLOUR), and
:DEFAULT when that is the terminal's own."
  (or (tuatara.display:colour-hex (tuatara.display:cursor-colour colour)) :default))

(defun terminal-control (control &rest arguments)
  "Send the terminal an escape sequence now: ESC, then CONTROL, a format string
given ARGUMENTS (written as it is when there are none)."
  (write-char #\Escape *standard-output*)
  (if arguments
      (apply #'format *standard-output* control arguments)
      (write-string control *standard-output*))
  (finish-output))

(defmethod tuatara.frontend:frontend-end-frame ((f tui-frontend))
  (let ((window (tui-window f)))
    (%wmove window (+ (first (tui-padding f)) (car *cursor*)) (+ (fourth (tui-padding f)) (cdr *cursor*)))
    ;; synchronized output (DEC mode 2026): the terminal shows the frame only
    ;; once it is all there, so that the cursor's trips to the cells being
    ;; changed and back never flicker; terminals without it ignore the mode
    (terminal-control "[?2026h")
    ;; no cursor over a picture (ncurses sends the change only when there is one);
    ;; shown again, the terminal's own cursor may come back blinking: its shape is sent again
    (when (eq *frame-cursor-shape* :hidden) (setf *shown-cursor-shape* nil))
    (%curs-set (if (eq *frame-cursor-shape* :hidden) 0 1))
    (redraw-unplaced f)
    (%wrefresh window)
    (place-pictures f)
    (terminal-control "[?2026l")
    ;; DECSCUSR: 2 = steady block, 4 = underline, 6 = bar; sent again only when
    ;; it changes, since some terminals redraw the cursor on every one, a flicker
    (unless (or (eq *frame-cursor-shape* :hidden) (eq *frame-cursor-shape* *shown-cursor-shape*))
      (terminal-control "[~a q" (cursor-shape-code *frame-cursor-shape*))
      (setf *shown-cursor-shape* *frame-cursor-shape*))
    (forget-images f nil)
    ;; the cursor's colour, OSC 12 (OSC 112 gives the terminal its own back)
    (let ((colour (cursor-colour-code *frame-cursor-colour*)))
      (unless (equal colour *shown-cursor-colour*)
        (if (eq colour :default)
            (terminal-control "]112~c" (code-char 7))
            (terminal-control "]12;~a~c" colour (code-char 7)))
        (setf *shown-cursor-colour* colour)))))

;;; Pictures, as kitty draws them: sent once, then shown in cells written as
;;; text. Each cell of a picture is U+10EEEE, the picture's number the cell's
;;; colour, the first of each row marked with its row; the terminal draws that
;;; part of the picture there. So ncurses paints and clears pictures as it does
;;; text, and nothing else of the screen changes.

(defun terminal-pictures-p ()
  "True when the terminal said it draws kitty's pictures when asked (TERMINAL-ANSWER)."
  (terminal-answer :pictures))

(defun pictures-in-text-p ()
  "True when the terminal draws a picture in place of the characters standing
for it (U+10EEEE): kitty and Ghostty. Not WezTerm or Konsole, as their
environment says, which draw a picture only where it is put (PLACE-PICTURES)."
  (not (or (equal (uiop:getenv "TERM_PROGRAM") "WezTerm")
           (uiop:getenv "KONSOLE_VERSION"))))

(defmethod tuatara.frontend:frontend-images-p ((f tui-frontend))
  (and (tui-window f)
       (case *tui-pictures* ((nil) nil) ((t) t) (t (terminal-pictures-p)))
       (tuatara.frontend:frontend-cell-pixels f)
       t))

(defmethod tuatara.frontend:frontend-cell-pixels ((f tui-frontend))
  ;; the width and height in pixels, which kitty fills in
  (multiple-value-bind (rows columns width height) (tuatara.io:terminal-window-size 1)
    (when (and rows (plusp rows) (plusp columns) (plusp width) (plusp height))
      (values (max 1 (floor width columns)) (max 1 (floor height rows))))))

(defun kitty-command (control &optional (payload ""))
  "Send the terminal a graphics command: CONTROL, its keys and values, and PAYLOAD."
  (format *standard-output* "~c_G~a;~a~c\\" #\Escape control payload #\Escape))

(defvar *shared-pictures* '()
  "The names of the shared memory objects sent to the terminal that it may not
have read (and so not removed) yet, removed when the terminal is let go of.")

(defun local-terminal-p ()
  "True when the terminal may run on this machine, not at the other end of SSH,
so that it is worth asking whether it reads what is put in shared memory here."
  (not (or (uiop:getenv "SSH_CONNECTION") (uiop:getenv "SSH_TTY") (uiop:getenv "SSH_CLIENT"))))

(defun send-image-shared (image number)
  "Send the terminal IMAGE's pixels, as its picture NUMBER, through shared
memory: written in an object of /dev/shm, whose name alone goes through the
terminal, which reads it and removes it. NIL when no object could be made."
  (let ((name (make-shared-picture (tuatara.frontend:image-pixels image))))
    (when name
      ;; those the terminal read and removed are let go of here too
      (when (> (length *shared-pictures*) 64)
        (setf *shared-pictures* (remove-if-not (lambda (old) (probe-file (shared-picture-path old))) *shared-pictures*)))
      (push name *shared-pictures*)
      (kitty-command (format nil "a=t,t=s,f=~d,s=~d,v=~d,i=~d,S=~d,q=2"
                             (if (= 4 (tuatara.frontend:image-channels image)) 32 24)
                             (tuatara.frontend:image-width image) (tuatara.frontend:image-height image)
                             number (length (tuatara.frontend:image-pixels image)))
                     (shared-name-payload name))
      (finish-output *standard-output*)
      t)))

(defun forget-shared-pictures ()
  "Remove the shared memory objects of pictures the terminal has not read."
  (dolist (name *shared-pictures*)
    (ignore-errors (sb-posix:unlink (shared-picture-path name))))
  (setf *shared-pictures* '()))

(defun send-image (image number)
  "Send the terminal IMAGE's pixels, as its picture NUMBER: through shared
memory when the terminal said it reads them from there (TERMINAL-ANSWER), at
once however large; else written to it in pieces of 4096 digits. It answers
nothing."
  (unless (and (terminal-answer :shared) (send-image-shared image number))
    (send-image-written image number)))

(defun send-image-written (image number)
  "Send the terminal IMAGE's pixels, as its picture NUMBER, written to it in
pieces of 4096 digits of base 64."
  (let* ((pixels (tuatara.frontend:image-pixels image))
         (piece 3072)
         (total (length pixels)))
    (loop for start from 0 below (max 1 total) by piece
          for end = (min total (+ start piece))
          for more = (< end total)
          do (kitty-command (if (zerop start)
                                (format nil "a=t,f=~d,s=~d,v=~d,i=~d,q=2,m=~d"
                                        (if (= 4 (tuatara.frontend:image-channels image)) 32 24)
                                        (tuatara.frontend:image-width image) (tuatara.frontend:image-height image)
                                        number (if more 1 0))
                                (format nil "m=~d" (if more 1 0)))
                            (tuatara.io:base64-string pixels :start start :end end)))
    (finish-output *standard-output*)))

(defun image-cells (image columns rows width height)
  "The columns and rows IMAGE takes shown in COLUMNS by ROWS cells of WIDTH by
HEIGHT pixels: as large as fits, its proportions kept, never larger than it is."
  (let* ((w (tuatara.frontend:image-width image))
         (h (tuatara.frontend:image-height image))
         (scale (tuatara.frontend:image-fit-scale image (* columns width) (* rows height))))
    (values (max 1 (min columns 297 (ceiling (* w scale) width)))
            (max 1 (min rows 297 (ceiling (* h scale) height))))))

(defparameter *kitty-image-colours* 16
  "How many colours the cells of pictures are written in, each taking one of
the terminal's colour pairs, of which the faces need the most.")

(defun kitty-image-id (count)
  "The number the terminal knows the picture sent COUNT-th (from 0) by: its
lowest octet, 1 to *KITTY-IMAGE-COLOURS*, is the colour its cells are written
in, so that pictures never use up the colour pairs; its highest octet the
mark each row's first cell has as well; 4,096 numbers before one comes again,
so that the terminal never shows a picture of long ago in place of a new one
of the same number."
  (let ((count (mod count (* *kitty-image-colours* 256))))
    (logior (ash (floor count *kitty-image-colours*) 24) (1+ (mod count *kitty-image-colours*)))))

(defun image-row-text (row columns &optional (from 0) (id 1))
  "The text of ROW of the cells of the picture numbered ID, COLUMNS of them
from its column FROM: the first marked with its row, its column and, when not
0, the highest octet of ID; the rest following on from it."
  (let ((cell (code-char tuatara.frontend:+kitty-placeholder+))
        (high (ldb (byte 8 24) id)))
    (concatenate 'string
                 (list* cell (code-char (aref tuatara.frontend:*kitty-marks* row)) (code-char (aref tuatara.frontend:*kitty-marks* from))
                        (and (plusp high) (list (code-char (aref tuatara.frontend:*kitty-marks* high)))))
                 (make-string (1- columns) :initial-element cell))))

(defmethod tuatara.frontend:frontend-put-image ((f tui-frontend) image row column columns rows &optional offset)
  (multiple-value-bind (width height) (tuatara.frontend:frontend-cell-pixels f)
    (when width
      (multiple-value-bind (across down)
          (if offset
              ;; at its own size: all its cells placed, the part from the offset on shown
              (values (max 1 (min 297 (ceiling (tuatara.frontend:image-width image) width)))
                      (max 1 (min 297 (ceiling (tuatara.frontend:image-height image) height))))
              (image-cells image columns rows width height))
        (let* ((images (tui-images f))
               ;; a picture has a number for each size it is shown at: two windows of
               ;; different sizes showing one picture each place their own
               (number (loop for n being the hash-keys of images using (hash-value entry)
                             when (and (eq (first entry) image) (eql (second entry) across) (eql (third entry) down))
                               return n)))
          (unless number
            (setf number (kitty-image-id (tui-next-image f)))
            (incf (tui-next-image f))
            (tuatara.runtime:debug-report :redisplay "kitty: picture ~d sent, ~dx~d pixels (image ~d), placed ~dx~d cells~:[ through the terminal~; through shared memory~]"
                                          number (tuatara.frontend:image-width image) (tuatara.frontend:image-height image)
                                          (tuatara.frontend:image-id image) across down (terminal-answer :shared))
            ;; a picture of long ago had this number: the terminal forgets it
            (when (gethash number images)
              (kitty-command (format nil "a=d,d=I,i=~d,q=2" number)))
            (send-image image number)
            (when (pictures-in-text-p)
              (kitty-command (format nil "a=p,U=1,i=~d,p=1,c=~d,r=~d,q=2" number across down)))
            (finish-output *standard-output*)
            (setf (gethash number images) (list image across down nil)))
          (let ((entry (gethash number images)))
            (setf (fourth entry) t)
            (let* ((window (tui-window f))
                   (attrs (colour-pair (pair-for (ldb (byte 8 0) number)
                                                 (terminal-colour (nth-value 1 (tuatara.display:resolve-face-spec :default)))))))
              (destructuring-bind (top right bottom left) (tui-padding f)
                (declare (ignore right bottom))
                (let* ((from-row (min (1- down) (if offset (first offset) 0)))
                       (from-column (min (1- across) (if offset (second offset) 0)))
                       (shown-rows (min rows (- down from-row)))
                       (shown-columns (min columns (- across from-column))))
                  (if (pictures-in-text-p)
                      (dotimes (r shown-rows)
                        (%wmove window (+ top row r) (+ left column))
                        (attributes-on window attrs)
                        (%waddstr window (image-row-text (+ from-row r) shown-columns from-column number))
                        (attributes-off window))
                      ;; blank cells, the picture put over them once they are on the screen
                      (let ((blank (make-string shown-columns :initial-element #\Space)))
                        (dotimes (r shown-rows)
                          (%wmove window (+ top row r) (+ left column))
                          (%waddstr window blank))
                        (push (list number
                                    (1+ (count number (tui-placing f) :key #'first))
                                    (+ top row) (+ left column)
                                    (placement-size image across down offset from-row from-column
                                                    shown-rows shown-columns width height)
                                    shown-rows)
                              (tui-placing f)))))))))))))

(defun placement-size (image across down offset from-row from-column rows columns width height)
  "The keys of the kitty graphics protocol that size a picture put at its cells
\(PLACE-PICTURES), IMAGE shown ACROSS by DOWN cells of WIDTH by HEIGHT pixels:
with OFFSET, at its own size, the part from FROM-ROW and FROM-COLUMN on,
ROWS by COLUMNS cells of it; else the whole of it, as large as fits, its
proportions kept: the columns given where its width decides, the rows where
its height does, neither where it is shown at its own size."
  (let ((w (tuatara.frontend:image-width image))
        (h (tuatara.frontend:image-height image)))
    (if offset
        (let ((x (* from-column width)) (y (* from-row height)))
          (format nil "x=~d,y=~d,w=~d,h=~d" x y
                  (max 1 (min (* columns width) (- w x))) (max 1 (min (* rows height) (- h y)))))
        (let ((by-width (/ (* across width) (max 1 w)))
              (by-height (/ (* down height) (max 1 h))))
          (cond ((and (>= by-width 1) (>= by-height 1)) "")
                ((<= by-width by-height) (format nil "c=~d" across))
                (t (format nil "r=~d" down)))))))

(defun place-pictures (f)
  "Put the pictures of the frame F has just painted at their cells, in a
terminal that draws no picture in place of text (PICTURES-IN-TEXT-P): each
from its top left cell, the cursor kept where it is; and take away those the
frame before placed that this one does not."
  (let ((placing (tui-placing f)))
    (dolist (old (tui-placed f))
      (unless (find-if (lambda (new) (and (eql (first new) (first old)) (eql (second new) (second old)))) placing)
        (kitty-command (format nil "a=d,d=i,i=~d,p=~d,q=2" (first old) (second old)))))
    (when placing
      (terminal-control "7")
      (dolist (place placing)
        (destructuring-bind (number id row column size rows) place
          (declare (ignore rows))
          (terminal-control "[~d;~dH" (1+ row) (1+ column))
          (kitty-command (format nil "a=p,i=~d,p=~d,C=1,q=2~:[~;,~a~]" number id (plusp (length size)) size))))
      (terminal-control "8"))
    (setf (tui-placed f) placing
          (tui-placing f) '())))

(defun redraw-unplaced (f)
  "Have ncurses write again, in the frame about to be shown, the rows where
the frame before put a picture at its cells that this one does not put the
same: the terminal may keep a picture's part in a cell until it is written."
  (let ((window (tui-window f)) (placing (tui-placing f)))
    (dolist (old (tui-placed f))
      (unless (find-if (lambda (new) (equal (subseq new 0 5) (subseq old 0 5))) placing)
        (%wredrawln window (third old) (sixth old))))))

(defun forget-images (f all)
  "Have the terminal let go of the pictures F sent that the frame just
painted did not show (with ALL, of every one), and start the next frame."
  (let ((images (tui-images f)) (forgot nil))
    (loop for number being the hash-keys of images using (hash-value entry)
          do (if (and (fourth entry) (not all))
                 (setf (fourth entry) nil)
                 (progn (kitty-command (format nil "a=d,d=I,i=~d,q=2" number))
                        (tuatara.runtime:debug-report :redisplay "kitty: picture ~d let go of" number)
                        (remhash number images)
                        (setf forgot t))))
    (when forgot (finish-output *standard-output*))))
