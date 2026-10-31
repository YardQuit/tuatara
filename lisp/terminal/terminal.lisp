;;;; lisp/terminal/terminal.lisp --- M-x terminal: a terminal inside a buffer.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A terminal buffer shows the screen of a program run on a pseudo-terminal
;;;; (EMULATOR, emulator.lisp), your shell unless told otherwise: its rows as
;;;; the buffer's text, their colours as faces, point where the program's
;;;; cursor is. While you type there (INSERT state, or always with modal
;;;; editing off) every key goes to the program, Escape and C-c too, but for
;;;; the keys *TERMINAL-EDITOR-KEYS* keep for the editor (C-x and its windows
;;;; and buffers, M-x, C-g and C-\ to stop typing there, C-q to send the next
;;;; key whatever it is); C-V pastes, marked as pasted (TERMINAL-PASTE). In NORMAL state the
;;;; screen stands still as text to move through, select and copy, the
;;;; program going on meanwhile; i or a types into it again.

(in-package #:tuatara.defaults)

(defvar tuatara-user:*terminal-editor-keys* '("C-x" "M-x" "C-g" "C-\\" "C-q" "C-s")
  "Keys that stay the editor's while you type into a terminal (\\[terminal]).
Every other key goes to the program in it. C-x (and the keys after it, for
windows and buffers) and M-x work as anywhere. C-g and C-\\ stop typing there:
NORMAL state, the screen as text. C-q sends the next key to the program
whatever it is, so C-q C-g gives the program its C-g. C-s searches the
terminal's history (\\[terminal-search]). Each is written as keys are written
for bind-key, \"C-x\", \"M-x\". Escape goes to the program unless
*ESCAPE-QUITS* is on; then it stops typing there as C-g does, and C-q ESC
gives the program its Escape. For a program that needs C-g itself, such as
another editor, put (setf *terminal-editor-keys* (remove \"C-g\"
*terminal-editor-keys* :test #'string=)) in the init file: C-g then goes to
the program, and C-\\ alone stops typing.")

(defvar tuatara-user:*terminal-paste-keys* '("C-V" "S-<insert>")
  "Keys that paste into a terminal while you type there (\\[terminal-paste]): the
text copied last, in Tuatara or another program, marked as pasted for a
program that asks (a shell then runs none of its lines until you press RET).
C-V is Control, Shift and v together, as most terminals paste. Each as keys
are written for bind-key.")

(defvar tuatara-user:*terminal-program* nil
  "The program \\[terminal] runs, a list of it and its arguments, (\"bash\" \"-l\"); NIL
for your shell ($SHELL, else /bin/sh).")

(defvar tuatara-user:*terminal-place* :bottom
  "Where a terminal shows (\\[terminal], \\[terminal-toggle]): :bottom or :top,
a window across that whole side of the screen *TERMINAL-SIZE* high; :left or
:right, one down that whole side *TERMINAL-WIDTH* wide; or :full, the
selected window.")

(defvar tuatara-user:*terminal-size* 0.3
  "How high a terminal's window is at the bottom or top of the screen
\(*TERMINAL-PLACE*, \\[terminal-bottom], \\[terminal-top]): a fraction of the
screen's height (0.3), or a number of rows (15).")

(defvar tuatara-user:*terminal-width* 0.5
  "How wide a terminal's window is at the left or right of the screen
\(*TERMINAL-PLACE*, \\[terminal-left], \\[terminal-right]): a fraction of the
screen's width (0.5, half of it), or a number of columns (80).")

(defun terminal-place-size (place)
  "How big a terminal's new window at PLACE is: *TERMINAL-WIDTH* at :left and
:right, *TERMINAL-SIZE* at :bottom and :top, none at :full."
  (case place
    ((:left :right) tuatara-user:*terminal-width*)
    ((:bottom :top) tuatara-user:*terminal-size*)))

(defvar tuatara-user:*terminal-scrollback* 100000
  "How many lines a terminal (\\[terminal]) keeps that have gone up off its screen.")

(define-major-mode terminal-mode special-mode
  "A terminal: the screen of a program run on a pseudo-terminal, your shell
unless *TERMINAL-PROGRAM* says otherwise. Typing there goes to the program,
every key but those *TERMINAL-EDITOR-KEYS* keep: C-g or C-\\ stops
typing there for its history view, its whole history as text to move through,
select and copy (\\<terminal-mode>\\[terminal-start-typing], or C-\\ again, types into it again; \\[terminal-copy-and-type] copies what is
selected first); \\[terminal-send-key] sends the next key to the program whatever it is;
C-V (*TERMINAL-PASTE-KEYS*) pastes, as \\[terminal-paste] in the history view does; C-x
and its keys work as anywhere."
  :lighter "Terminal" :interactive nil)

(defvar *terminal-buffers* '()
  "The terminal buffers there are, newest first.")

(defvar *terminal-look-faces* (cons nil (make-hash-table))
  "The faces of each look shown so far (TERMINAL-FACES), by the look, in its
cdr; its car the face generation (TUATARA.DISPLAY:*FACE-GENERATION*) they
were made for.")

(defun terminal-faces (look)
  "The faces a cell of LOOK (EMULATOR-FRAME's, a number: LOOK-LIST) is shown
in: its colours, then bold, italic, underlined, dim and reversed. Made once
for each look while the faces stay as they are."
  (let ((known *terminal-look-faces*))
    (unless (eql (car known) tuatara.display:*face-generation*)
      ;; a fresh table, never one cleared: another thread may be reading the old one
      (setf known (cons tuatara.display:*face-generation* (make-hash-table :synchronized t))
            *terminal-look-faces* known))
    (multiple-value-bind (faces found) (gethash look (cdr known))
      (if found
          faces
          (setf (gethash look (cdr known)) (look-faces look))))))

(defun look-faces (look)
  "The faces a cell of LOOK is shown in, made afresh (TERMINAL-FACES)."
  (destructuring-bind (&optional fg bg &rest attributes) (look-list look)
    (remove nil (list* (and fg (terminal-colour-face :fg fg))
                       (and bg (terminal-colour-face :bg bg))
                       (mapcar (lambda (attribute)
                                 (case attribute
                                   (:bold :terminal-bold) (:italic :terminal-italic) (:underline :terminal-underline)
                                   (:faint :terminal-dim) (:inverse :terminal-reverse) (:link :link)))
                               attributes)))))

(defvar *terminal-colour-faces* (make-hash-table :test 'eq :synchronized t)
  "The faces made for colours programs in terminals chose (TERMINAL-COLOUR-FACE),
each name with (KIND . COLOUR), so that they can be coloured again for the
theme in force (RECOLOUR-TERMINAL-FACES).")

(defun recolour-terminal-faces ()
  "Colour again every face made for a program's colour, as the theme in force
gives them: a theme put in force leaves them bare."
  (loop for (kind . colour) being the hash-values of *terminal-colour-faces*
        do (terminal-colour-face kind colour)))

(defun terminal-colour-face (kind colour)
  "A face of KIND, :fg or :bg, in COLOUR as a program in a terminal chose it
\(an index of the 256-colour palette, or #rrggbb, taken to the nearest of
4,096), made the first time, and again after a theme left it bare. The sixteen
basic colours are the theme's (THEME-TERMINAL-COLOUR), else the terminal's own."
  (let* ((colour (multiple-value-bind (red green blue) (tuatara.display:parse-hex-colour colour)
                   (if red
                       ;; to the nearest of 4,096 colours, so that a program's thousands of shades
                       ;; make no more faces than that
                       (tuatara.display:colour-hex (mapcar (lambda (c) (* 17 (round c 17))) (list red green blue)))
                       colour)))
         (name (intern (string-upcase (format nil "TERMINAL-~a-~a" kind colour)) :keyword))
         (shown (or (and (integerp colour) (theme-terminal-colour colour)) colour)))
    ;; made again when a theme put in force since has left it without its colour
    (unless (and (tuatara.display:find-face name) (equal shown (tuatara.display:face-attribute name kind)))
      (setf (gethash name *terminal-colour-faces*) (cons kind colour))
      (if (eq kind :fg)
          (tuatara.display:set-face-attributes name :doc "A colour a program in a terminal chose for its text." :fg shown)
          (tuatara.display:set-face-attributes name :doc "A colour a program in a terminal chose for its ground." :bg shown)))
    name))

(defvar *terminal-face-generation* nil
  "The face generation (TUATARA.DISPLAY:*FACE-GENERATION*) the faces of
programs' colours were last coloured for.")

;;; A terminal's text with its escape sequences, its colours made faces

(defun sgr-colour (codes)
  "The colour code (MAKE-LOOK) the SGR CODES (a list, from just after 38 or 48)
begin with, and the codes after it: 5;N an index of the 256, 2;R;G;B red,
green and blue; 0 for none."
  (flet ((part (c) (min 255 (max 0 (or c 0)))))
    (case (first codes)
      (5 (values (palette-code (part (second codes))) (cddr codes)))
      (2 (values (rgb-code (part (second codes)) (part (third codes)) (part (fourth codes))) (nthcdr 4 codes)))
      (t (values 0 (rest codes))))))

(defun sgr-colour-parts (parts)
  "The parts of a colour written with colons (2::R:G:B, 2:R:G:B or 5:N, after
38: or 48:) as SGR-COLOUR takes them: the colour space a 2:: may hold left out."
  (if (and (eql (first parts) 2) (= (length parts) 5)) (list* 2 (cddr parts)) parts))

(defun ansi-pieces (text)
  "TEXT, a terminal's text with its escape sequences, as pieces (TEXT FACES),
FACES a list, or plain strings:
SGR colours, bold, italic and underline as faces, every other sequence (a
cursor's move, a shell's marks) left out."
  (let ((pieces '()) (fg 0) (bg 0) (bold nil) (italic nil) (underline nil)
        (out (make-string-output-stream)) (i 0) (n (length text)))
    (flet ((flush ()
             (let ((s (get-output-stream-string out)))
               (when (plusp (length s))
                 (let ((faces (terminal-faces (make-look fg bg (logior (if bold (look-bit :bold) 0) (if italic (look-bit :italic) 0)
                                                                       (if underline (look-bit :underline) 0))))))
                   (push (if faces (list s faces) s) pieces))))))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (cond
                   ((and (char= c #\Escape) (< (1+ i) n) (char= (char text (1+ i)) #\[))
                    ;; CSI: its parameters, then its final character
                    (let* ((end (or (position-if (lambda (ch) (<= #x40 (char-code ch) #x7e)) text :start (+ i 2)) (1- n)))
                           (final (char text end)))
                      (when (char= final #\m)
                        (flush)
                        ;; each parameter a code, its parts after colons its own (4:3 a curly
                        ;; underline, 58:2::R:G:B an underline's colour); 38 and 48 take theirs
                        ;; either way: 38;2;R;G;B or 38:2::R:G:B
                        (let ((codes (mapcar (lambda (p)
                                               (let ((parts (mapcar (lambda (x) (or (ignore-errors (parse-integer x)) 0))
                                                                    (uiop:split-string p :separator ":"))))
                                                 (if (rest parts) parts (first parts))))
                                             (uiop:split-string (subseq text (+ i 2) end) :separator ";"))))
                          (loop while codes
                                do (let ((code (pop codes)))
                                     (when (consp code)
                                       (setf code (case (first code)
                                                    ;; an underline's style: 0 none, else one
                                                    (4 (if (eql 0 (second code)) 24 4))
                                                    ((38 48) (multiple-value-bind (colour) (sgr-colour (sgr-colour-parts (rest code)))
                                                               (if (= (first code) 38) (setf fg colour) (setf bg colour)))
                                                     nil)
                                                    (t nil))))
                                     (case (or code :none)
                                       (:none nil)
                                       (0 (setf fg 0 bg 0 bold nil italic nil underline nil))
                                       (1 (setf bold t)) (3 (setf italic t)) (4 (setf underline t))
                                       ((21 22) (setf bold nil)) (23 (setf italic nil)) (24 (setf underline nil))
                                       (39 (setf fg 0)) (49 (setf bg 0))
                                       (38 (multiple-value-setq (fg codes) (sgr-colour codes)))
                                       (48 (multiple-value-setq (bg codes) (sgr-colour codes)))
                                       ;; the sixteen colours by their numbers in the palette
                                       (t (cond ((<= 30 code 37) (setf fg (palette-code (- code 30))))
                                                ((<= 40 code 47) (setf bg (palette-code (- code 40))))
                                                ((<= 90 code 97) (setf fg (palette-code (+ 8 (- code 90)))))
                                                ((<= 100 code 107) (setf bg (palette-code (+ 8 (- code 100))))))))))))
                      (setf i (1+ end))))
                   ((and (char= c #\Escape) (< (1+ i) n) (char= (char text (1+ i)) #\]))
                    ;; OSC: up to its BEL or ST (ESC \)
                    (let ((end (loop for j from (+ i 2) below n
                                     for ch = (char text j)
                                     when (char= ch (code-char 7)) return (1+ j)
                                     when (and (char= ch #\Escape) (< (1+ j) n) (char= (char text (1+ j)) #\\))
                                       return (+ j 2))))
                      (setf i (or end n))))
                   ((char= c #\Escape) (incf i 2))
                   ((char= c #\Return) (incf i))
                   ((and (< (char-code c) 32) (not (member c '(#\Newline #\Tab)))) (incf i))
                   (t (write-char c out) (incf i)))))
      (flush))
    (nreverse pieces)))

(defun key-among-p (key keys)
  "True when KEY is one of KEYS, each written as keys are written for
bind-key (\"C-x\", \"S-<insert>\")."
  (let ((spec (tuatara.frontend:key-spec key)))
    (some (lambda (keys) (equal spec (first (ignore-errors (tuatara.frontend:parse-key-sequence keys))))) keys)))

(defun terminal-editor-key-p (key)
  "True when KEY stays the editor's in a terminal: one of
*TERMINAL-EDITOR-KEYS*, or Escape while *ESCAPE-QUITS* is on."
  (or (and tuatara-user:*escape-quits* (equal (tuatara.frontend:key-spec key) '(:escape)))
      (key-among-p key tuatara-user:*terminal-editor-keys*)))

(defun terminal-typing-p (buffer)
  "True while what is typed in BUFFER, a terminal, goes to its program: not in
its history view (TERMINAL-STOP-TYPING)."
  (not (tuatara.buffer:buffer-local buffer 'terminal-history-view)))

(defun set-terminal-history-view (buffer on)
  "Put BUFFER, a terminal, in its history view (ON true) or back to typing,
forgetting a search asked for while its history was being read
\(TERMINAL-SEARCH): the mode line says which; with modal editing on, the history read in SUPPORT
state and typed into in INSERT state, as a modal layer has them."
  (setf (tuatara.buffer:buffer-local buffer 'terminal-history-view) on
        (tuatara.buffer:buffer-local buffer :mode-name) (if on "Terminal history" "Terminal"))
  ;; typing again: a search asked for before the history was in is asked no more
  (unless on (setf (tuatara.buffer:buffer-local buffer 'terminal-search-wanted) nil))
  (setf (tuatara.buffer:buffer-local buffer :modal-class) (if on :support :editing))
  (if on
      (modal-sync *editor*)
      (set-modal-state :insert buffer)))

(defun terminal-paste-key-p (key)
  "True when KEY pastes into a terminal while typing there (*TERMINAL-PASTE-KEYS*)."
  (key-among-p key tuatara-user:*terminal-paste-keys*))

(defun terminal-stop-key-p (key)
  "True when KEY, one the editor keeps in a terminal, stops typing there: C-g
and C-\\, and Escape too while *ESCAPE-QUITS* is on."
  (or (quit-key-p key) (equal (tuatara.frontend:key-spec key) '(:ctrl #\\))))

(defun terminal-cell (buffer key)
  "The cell of BUFFER's terminal screen the mouse KEY happened on, as two
values, its column and row from 0; NIL when it was not on the screen of a
window showing BUFFER."
  (let ((column (tuatara.frontend:key-column key)) (row (tuatara.frontend:key-row key))
        (emulator (tuatara.buffer:buffer-local buffer :emulator)))
    (let ((window (and column row emulator (window-at-cell column row))))
      (when (and window (eq (tuatara.display:window-buffer window) buffer))
        (multiple-value-bind (x y) (window-text-cell window column row)
          (when (and (< -1 x (emulator-columns emulator)) (< -1 y (min (emulator-rows emulator) (tuatara.display:window-height window))))
            (values x y)))))))

(defun terminal-mouse (buffer emulator key)
  "Give the mouse KEY over BUFFER's terminal to its program, while typing
there: clicks, drags and the wheel to a program that tracks the mouse (Shift
held keeps them the editor's); the wheel to a full-screen program that does
not as the up and down keys, which scroll a pager such as less; else the wheel up shows the
history view, to scroll through; the middle button pastes what is selected
anywhere (the primary selection) to a program that does not track it; the
button let go with nothing bound to it, as after a click on a link, is
dropped. True when it took KEY."
  (let ((base (tuatara.frontend:key-base key)))
    (when (member base '(:down-mouse-1 :double-mouse-1 :triple-mouse-1 :drag-mouse-1 :up-mouse-1 :mouse-2 :wheel-up :wheel-down))
      (multiple-value-bind (x y) (terminal-cell buffer key)
        (when x
          (flet ((tell (action button &optional pressed)
                   (and (not (tuatara.frontend:key-shift key))
                        (emulator-mouse emulator action button x y :ctrl (tuatara.frontend:key-ctrl key)
                                                                   :meta (tuatara.frontend:key-meta key) :pressed pressed))))
            (case base
              ((:wheel-up :wheel-down)
               (cond ((tell :press base))
                     ((emulator-scrolls-by-keys-p emulator)
                      (loop repeat 3 do (emulator-key emulator (tuatara.frontend:make-key (if (eq base :wheel-up) :up :down))))
                      t)
                     ((eq base :wheel-up) (terminal-stop-typing) t)
                     (t t)))
              (:down-mouse-1
               (let ((link (and (tuatara.frontend:key-ctrl key) (terminal-link-at buffer emulator x y))))
                 (if link
                     (progn (open-terminal-link link) t)
                     (tell :press :left t))))
              ((:double-mouse-1 :triple-mouse-1) (tell :press :left t))
              (:drag-mouse-1 (tell :motion :left t))
              ;; let go: the program's, or nobody's when no key is bound to it (a click with Control held)
              (:up-mouse-1 (or (tell :release :left)
                               (null (resolve-keys *editor* (list (tuatara.frontend:key-spec key))))))
              ;; the middle button: the program's, or a paste of what is selected anywhere
              (:mouse-2 (or (and (tell :press :middle t) (tell :release :middle))
                            (progn (paste-into-terminal buffer emulator (tuatara.frontend:frontend-primary-selection (editor-frontend *editor*)))
                                   t))))))))))

;;; Links

(defparameter *terminal-url-pattern* "(?i)\\b(?:https?|ftp|file)://[^\\s<>\"'`]+"
  "Where a web or file address starts in a terminal's text, and as far as it may go.")

(defun text-urls (text)
  "The web and file addresses written in TEXT, as a list of (START END), by
characters: an address's last full stop, comma, colon or quotation mark left
out, and a closing bracket it does not open."
  (let ((found '()))
    (when (search "://" text)
      (cl-ppcre:do-matches (start end *terminal-url-pattern* text)
        (loop while (and (> end start)
                         (let ((c (char text (1- end))))
                           (or (find c ".,;:!?'\"")
                               (and (find c tuatara.text:*closers*)
                                    (> (count c text :start start :end end)
                                       (count (char tuatara.text:*openers* (position c tuatara.text:*closers*)) text :start start :end end))))))
              do (decf end))
        (push (list start end) found)))
    (nreverse found)))

(defun terminal-link-at (buffer emulator column row &key history)
  "The address of the link on the cell at COLUMN, ROW of BUFFER's terminal: its
screen as it shows, or with HISTORY the history view's lines. A link a
program wrote (OSC 8) first, else a web or file address written there; NIL."
  (or (emulator-link emulator column row :history history)
      (let ((text (if history
                      (tuatara.buffer:buffer-line-text buffer (tuatara.buffer:buffer-goto-line buffer row))
                      (let ((rows (tuatara.buffer:buffer-local buffer 'terminal-rows)))
                        (and rows (< row (length rows)) (aref rows row))))))
        (when text
          (let ((at (tuatara.display:chars-in-columns text column)))
            (loop for (start end) in (text-urls text)
                  when (and (<= start at) (< at end)) return (subseq text start end)))))))


(defun open-terminal-link (address)
  "Open ADDRESS, a link in a terminal: a file of this computer (file://) in
Tuatara, in another window than the terminal's, after asking, the file it
leads to named (what a link shows may differ from where it leads), safely
when it is in one of *SAFE-DIRECTORIES*; a web or mail address in the
desktop's program after asking (OPEN-ADDRESS)."
  (let ((scheme (address-scheme address)))
    (cond ((string= scheme "file")
           (let ((path (uri-path address :hosts (list "localhost" (machine-instance)))))
             (cond ((null path)
                    (message "~a is no file of this computer" (escape-controls address)))
                   ((not (yes-or-no-p* (format nil "Open ~a in Tuatara?" (escape-controls path))))
                    (message "Not opened"))
                   (t (visit-path path :safe (and (safe-directory-of path) t)
                                       :show (lambda (buffer)
                                               (let ((window (display-buffer buffer :other t)))
                                                 (when window (select-window window)))))))))
          (t (open-address address)))))

(define-command terminal-open-link ()
  "Open the link under the mouse (C-click) or at point in a terminal: a link a
program wrote (ls --hyperlink, OSC 8) or a web or file address written
there. A file of this computer opens in Tuatara, a web or mail address in
the desktop's program, each after asking, the question naming where the
link leads (OPEN-TERMINAL-LINK). Web and file addresses in a terminal are
underlined."
  (let* ((buffer (current-buffer))
         (emulator (live-emulator buffer))
         (key (editor-this-key *editor*))
         (mouse (and key (tuatara.frontend:key-column key)))
         (typing (terminal-typing-p buffer)))
    (let ((address
            ;; typing, the cell clicked on the screen; else at point, in the history view where it was clicked
            (multiple-value-bind (x y) (and typing mouse (terminal-cell buffer key))
              (if x
                  (terminal-link-at buffer emulator x y)
                  (progn
                    (when (and mouse (not typing)) (mouse-set-point))
                    (let ((point (point)))
                      (terminal-link-at buffer emulator (tuatara.display:buffer-screen-column buffer point)
                                        (tuatara.buffer:buffer-line-number buffer point)
                                        :history (not typing))))))))
      (if address
          (open-terminal-link address)
          (message "No link here")))))

(defun terminal-key-handler (editor key)
  "Send KEY to the program of the current buffer's terminal while typing there
\(TERMINAL-TYPING-P), but for the keys that stay the editor's; Escape among
those stops typing there, as C-g does. True when it took KEY."
  (declare (ignore editor))
  (let* ((buffer (current-buffer))
         (emulator (tuatara.buffer:buffer-local buffer :emulator)))
    (when (and emulator (emulator-alive-p emulator) (terminal-typing-p buffer))
      (cond ((terminal-mouse buffer emulator key))
            ((terminal-paste-key-p key) (terminal-paste) t)
            ((terminal-editor-key-p key)
             (cond ((terminal-stop-key-p key) (terminal-stop-typing) t)
                   ((equal (tuatara.frontend:key-spec key) '(:ctrl #\s)) (terminal-search) t)))
            (t (emulator-key emulator key))))))

(defun terminal-cell-pixels ()
  "The width and height in whole pixels of a cell where the frontend shows
pictures (PICTURE-CELL-PIXELS), as two values; 0 and 0 elsewhere."
  (multiple-value-bind (width height) (picture-cell-pixels)
    (if (and width height) (values (round width) (round height)) (values 0 0))))

(defun terminal-window-size (window)
  "The columns and rows of WINDOW's text, as two values: the size of the
terminal shown there."
  (values (max 2 (tuatara.display:window-text-width window))
          (max 1 (tuatara.display:window-height window))))

(defun terminal-row-shown (row row-looks keep)
  "What a terminal buffer shows of ROW, a row of its screen whose looks are
ROW-LOOKS (EMULATOR-FRAME's): a list (KEEP OCTETS SPANS), OCTETS the row in
UTF-8, the spaces at its end left out but for its first KEEP characters, SPANS
each (START END FACES) in those octets, its addresses as links and its looks
as faces."
  (let* ((last (position #\Space row :from-end t :test-not #'char=))
         (end (min (length row) (max keep (if last (1+ last) 0))))
         (spans '()))
    (flet ((octets (to) (tuatara.text:string-octet-length row :end to)))
      (loop for (start stop) in (text-urls (subseq row 0 end))
            do (push (list (octets start) (octets stop) '(:link)) spans))
      (loop for (from to look) in row-looks
            when (< from end)
              do (let ((start (octets from)) (stop (octets (min to end))))
                   (when (< start stop) (push (list start stop (terminal-faces look)) spans))))
      (list keep (tuatara.text:string-octets row :end end) (nreverse spans)))))

(defun terminal-shown-text (rows looks cursor-row cursor-column &optional shown)
  "The text a terminal buffer shows for ROWS, the screen's rows, as UTF-8
octets, and the parts of it in faces, as two values: the rows a line each,
the spaces at their ends left out but those before the cursor (at
CURSOR-COLUMN of CURSOR-ROW), and each (START END FACES) in octets, from
LOOKS. SHOWN, when given, a vector as long as ROWS, keeps what each row shows
\(TERMINAL-ROW-SHOWN), made again only for a row whose place in it is NIL or
whose spaces kept for the cursor changed."
  (let ((entries (make-array (length rows))) (spans '()) (octets 0))
    (loop for row across rows
          for row-looks across looks
          for r from 0
          do (let ((keep (if (eql r cursor-row) (tuatara.display:chars-in-columns row cursor-column) 0))
                   (entry (and shown (aref shown r))))
               (unless (and entry (= keep (first entry)))
                 (setf entry (terminal-row-shown row row-looks keep))
                 (when shown (setf (aref shown r) entry)))
               (setf (aref entries r) entry)))
    (let ((text (make-array (max 0 (+ (1- (length rows)) (loop for entry across entries sum (length (second entry)))))
                            :element-type '(unsigned-byte 8))))
      (loop for (nil row-octets row-spans) across entries
            for r from 0
            do (when (plusp r) (setf (aref text octets) 10) (incf octets))
               (replace text row-octets :start1 octets)
               (loop for (start end faces) in row-spans
                     do (push (list (+ octets start) (+ octets end) faces) spans))
               (incf octets (length row-octets)))
      (values text (nreverse spans)))))

(defvar tuatara-user:*terminal-title-max* 40
  "The most columns of a terminal's title (as the shell gives it, user@host:~/dir
in many) the mode line shows; NIL shows none.")

(defun follow-terminal-program (buffer emulator)
  "Have BUFFER, a terminal, stand in the directory its program is in now, so
that \\[find-file] and the rest start there and the mode line says it, and keep
the title the program gave its terminal for the mode line."
  (let ((directory (emulator-directory emulator)))
    (when (and directory (not (equal directory (tuatara.buffer:buffer-local buffer :directory))))
      (setf (tuatara.buffer:buffer-local buffer :directory) directory)))
  (let ((title (emulator-title emulator)))
    (unless (eq title :busy)
      (setf (tuatara.buffer:buffer-local buffer 'terminal-title) title))))

(defun terminal-mode-line-title (editor buffer)
  "Mode line function: the title the program in BUFFER, a terminal, gave it,
cut to *TERMINAL-TITLE-MAX* columns."
  (declare (ignore editor))
  (let ((title (and tuatara-user:*terminal-title-max* (tuatara.buffer:buffer-local buffer 'terminal-title))))
    (when (and title (plusp (length title)))
      (values (tuatara.display:fit-columns (escape-controls title) '() tuatara-user:*terminal-title-max*)))))

(defun terminal-refresh (buffer emulator &key all)
  "Show in BUFFER what changed on EMULATOR's screen, or with ALL the whole of
it, point where the program's cursor is, and the pictures its program placed
over it (BUFFER's :pictures). :BUSY when the library could not be had at
once, nothing shown: to be tried again."
  (let ((rows (tuatara.buffer:buffer-local buffer 'terminal-rows))
        (looks (tuatara.buffer:buffer-local buffer 'terminal-looks))
        (shown (tuatara.buffer:buffer-local buffer 'terminal-shown))
        ;; the whole screen still owed by a refresh that could not be made
        (all (or all (tuatara.buffer:buffer-local buffer 'terminal-owed-all))))
    (unless (and rows (= (length rows) (emulator-rows emulator)))
      (setf rows (make-array (emulator-rows emulator) :initial-element "")
            looks (make-array (emulator-rows emulator) :initial-element '())
            shown (make-array (emulator-rows emulator) :initial-element nil)
            all t
            (tuatara.buffer:buffer-local buffer 'terminal-rows) rows
            (tuatara.buffer:buffer-local buffer 'terminal-looks) looks
            (tuatara.buffer:buffer-local buffer 'terminal-shown) shown))
    (multiple-value-bind (x y visible shape pictures)
        (emulator-frame emulator (lambda (row text row-looks)
                                   (when (< row (length rows))
                                     (setf (aref rows row) text (aref looks row) row-looks (aref shown row) nil)))
                        :all all :wait nil)
      (declare (ignore visible shape))
      ;; the reader holds the library a while (a picture being drawn): not waited for, tried again soon
      (when (eq x :busy)
        (setf (tuatara.buffer:buffer-local buffer 'terminal-owed-all) all)
        (return-from terminal-refresh :busy))
      (setf (tuatara.buffer:buffer-local buffer 'terminal-owed-all) nil)
      (setf (tuatara.buffer:buffer-local buffer :pictures) pictures)
      (follow-terminal-program buffer emulator)
      (multiple-value-bind (text spans) (terminal-shown-text rows looks y (or x 0) shown)
        (set-made-text buffer text :spans spans :key nil))
      (when y
        (let ((line (tuatara.buffer:buffer-goto-line buffer (min y (1- (length rows))))))
          (setf (tuatara.buffer:buffer-point buffer)
                (min (tuatara.buffer:buffer-line-end buffer line)
                     (+ line (tuatara.text:string-octet-length
                              (aref rows y) :end (min (length (aref rows y)) (tuatara.display:chars-in-columns (aref rows y) x)))))))))))

(defun start-terminal-history (buffer emulator)
  "Make the whole of EMULATOR's screen, scrollback and all, with its colours,
the text of BUFFER, in the background: the text is read and its colours made
faces on a thread of its own, and put in BUFFER by INSTALL-TERMINAL-HISTORY
once it is there, the editor answering keys meanwhile."
  (setf (tuatara.buffer:buffer-local buffer 'terminal-history)
        (tuatara.runtime:start-background-result
         (lambda ()
           (multiple-value-bind (text scrollback cursor-row) (emulator-text emulator :styled t)
             (multiple-value-bind (plain spans) (pieces-text (ansi-pieces text))
               (list :text plain :spans spans :line (+ scrollback (or cursor-row 0))))))
         "tuatara terminal history" (editor-job-waker))))

(defun install-terminal-history (buffer)
  "Put in BUFFER, a terminal no longer typed into, its history once
START-TERMINAL-HISTORY has made it: point on the line the program's cursor
is on."
  (let* ((cell (tuatara.buffer:buffer-local buffer 'terminal-history))
         (history (and cell (car cell))))
    (when history
      (setf (tuatara.buffer:buffer-local buffer 'terminal-history) nil)
      (if (getf history :error)
          (message "The terminal's history could not be read: ~a" (getf history :error))
          (progn
            (set-made-text buffer (getf history :text) :spans (getf history :spans) :key nil)
            (let ((lines (tuatara.buffer:buffer-line-count buffer)))
              (setf (tuatara.buffer:buffer-point buffer)
                    (tuatara.buffer:buffer-goto-line buffer (min (getf history :line) (max 0 (1- lines))))))
            (dolist (window (buffer-windows buffer))
              (setf (tuatara.display:window-point window) (tuatara.buffer:buffer-point buffer)))
            (message "~a: its history as text, ~:d line~:p" (tuatara.buffer:buffer-name buffer)
                     (tuatara.buffer:buffer-line-count buffer))
            ;; a search asked for while it was being read: begun now, back from the cursor
            (when (shiftf (tuatara.buffer:buffer-local buffer 'terminal-search-wanted) nil)
              (setf (editor-unread *editor*)
                    (append (editor-unread *editor*)
                            (list (tuatara.frontend:make-event :key :key (tuatara.frontend:make-key #\r :ctrl t)))))))))))

(defun terminal-tick (editor)
  "Tick function: colour programs' colours again when a theme has been put in
force, fit each terminal shown to its window, and show what its program
changed while it is typed into; a terminal whose program has ended is
killed, saying how it ended. Waits for the next wake, which a program's
output brings, but for a terminal whose library was held (a picture being
drawn), looked at again a moment later."
  (unless (eql *terminal-face-generation* tuatara.display:*face-generation*)
    (recolour-terminal-faces)
    (setf *terminal-face-generation* tuatara.display:*face-generation*))
  (let ((again nil))
  (dolist (buffer (copy-list *terminal-buffers*))
    (let ((emulator (tuatara.buffer:buffer-local buffer :emulator))
          (window (buffer-window buffer editor)))
      (cond ((or (null emulator) (not (live-buffer-p buffer)))
             (setf *terminal-buffers* (remove buffer *terminal-buffers*)))
            (t
             (let ((all nil))
               (when window
                 (multiple-value-bind (columns rows) (terminal-window-size window)
                   (multiple-value-bind (cell-width cell-height) (terminal-cell-pixels)
                     (case (emulator-resize emulator columns rows cell-width cell-height)
                       ((nil))
                       (:busy (setf again t))
                       (t (setf all t))))))
               ;; typing stopped: the history as text, made in the background; typing
               ;; again: the screen as it is now
               (let ((typing (terminal-typing-p buffer)))
                 (unless (eq typing (tuatara.buffer:buffer-local buffer 'terminal-was-typing))
                   (setf (tuatara.buffer:buffer-local buffer 'terminal-was-typing) typing
                         all (or all typing))
                   (if typing
                       (setf (tuatara.buffer:buffer-local buffer 'terminal-history) nil)
                       (start-terminal-history buffer emulator)))
                 (unless typing
                   (setf (tuatara.buffer:buffer-local buffer :pictures) nil)
                   (install-terminal-history buffer)))
               (when (and (terminal-typing-p buffer)
                          (or all (emulator-changed emulator) (tuatara.buffer:buffer-local buffer 'terminal-owed-all)))
                 (when (eq (terminal-refresh buffer emulator :all all) :busy)
                   (setf again t))))
             (when (emulator-closed emulator)
               (let ((name (tuatara.buffer:buffer-name buffer))
                     (process (emulator-process emulator)))
                 (tuatara.buffer:kill-buffer buffer)
                 (message "~a ended~@[ (exit ~d)~]" name
                          (and process (eq (sb-ext:process-status process) :exited) (sb-ext:process-exit-code process)))))))))
  (and again 1/20)))

(defun place-terminals ()
  "Have terminals shown where *TERMINAL-PLACE* and *TERMINAL-SIZE* say, their
window selected and kept for them (PLACE-BUFFERS)."
  (let ((place tuatara-user:*terminal-place*))
    (unless (member place '(:bottom :top :left :right :full))
      (error "*terminal-place* is ~s: use :bottom, :top, :left, :right or :full" place))
    (place-buffers 'terminal-mode (if (eq place :full) :here place)
                   :size (terminal-place-size place) :keep t :select t)))

(defun show-terminal (buffer &optional place)
  "Show BUFFER, a terminal, where terminals are shown (PLACE-TERMINALS), or
with PLACE (:bottom, :top, :left, :right or :full) in a new window there,
*TERMINAL-SIZE* big, whatever *TERMINAL-PLACE* says; its window selected and
typing there. Returns the window."
  (let ((window (if place
                    (display-buffer-at buffer (if (eq place :full) :here place)
                                       :size (terminal-place-size place))
                    (progn (place-terminals) (display-buffer buffer)))))
    (when window (select-window window))
    (set-terminal-history-view buffer nil)
    window))

(define-command terminal ()
  "Open a terminal in a buffer of its own, where *TERMINAL-PLACE* says, as
OPEN-TERMINAL does."
  (open-terminal))

(define-command terminal-left ()
  "Open a new terminal in a window down the whole left side, whatever
*TERMINAL-PLACE* says, *TERMINAL-WIDTH* wide, as \\[terminal] opens one."
  (open-terminal :left))

(define-command terminal-right ()
  "Open a new terminal in a window down the whole right side, whatever
*TERMINAL-PLACE* says, *TERMINAL-WIDTH* wide, as \\[terminal] opens one."
  (open-terminal :right))

(define-command terminal-top ()
  "Open a new terminal in a window across the whole top, whatever
*TERMINAL-PLACE* says, *TERMINAL-SIZE* high, as \\[terminal] opens one."
  (open-terminal :top))

(define-command terminal-bottom ()
  "Open a new terminal in a window across the whole bottom, whatever
*TERMINAL-PLACE* says, *TERMINAL-SIZE* high, as \\[terminal] opens one."
  (open-terminal :bottom))

(define-command terminal-full ()
  "Open a new terminal in the selected window, whatever *TERMINAL-PLACE*
says, as \\[terminal] opens one; quitting it brings back what the window showed."
  (open-terminal :full))

(defun open-terminal (&optional place)
  "Open a terminal in a buffer of its own, where *TERMINAL-PLACE* says or, with
PLACE, in a new window there (SHOW-TERMINAL), in the current buffer's directory
\(in your home directory instead when that buffer was opened safely or its
directory is in one of *SAFE-DIRECTORIES* or *EVIDENCE-FOLDERS*, so that your shell's prompt and
its helpers read nothing a folder of evidence holds, such as its .git or
.envrc; the echo area says so): your shell (or *TERMINAL-PROGRAM*) on a
screen as large as the window, which follows the window's size. Everything typed there goes to the program, Escape
and C-c too, but for the keys *TERMINAL-EDITOR-KEYS* keep: C-x and its
windows and buffers, M-x, and C-g or C-\\, which stop typing there,
the screen standing still as text to move through, select and copy (i or a
types into it again); \\[terminal-send-key] sends the next key to the program
whatever it is. Each \\[terminal] opens another; killing the buffer ends its
program, and the program ending (exit) kills the buffer; a terminal that
cannot be shown or started leaves no buffer behind. The sixteen basic
colours a program picks are the theme's (*TERMINAL-PALETTE-ROLES*)."
  (unless (terminal-library-p)
    (error "The terminal library is not there (tools/terminal-library.sh builds it)"))
  (let* ((editor *editor*)
         (here (tuatara.io:native (buffer-directory (current-buffer))))
         (careful (cond ((tuatara.io:safe-visit-p (current-buffer))
                         (format nil "~a is opened safely" (tuatara.buffer:buffer-name (current-buffer))))
                        ((safe-directory-of here) (format nil "~a is in *safe-directories*" here))
                        ((evidence-path-p here) (evidence-visit-reason here))))
         (directory (if careful (tuatara.io:home-directory) here))
         (program (or tuatara-user:*terminal-program*
                      (list (tuatara.io:user-shell))))
         (buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "*terminal*"))))
    (set-major-mode buffer 'terminal-mode)
    (setf (tuatara.buffer:buffer-local buffer :modal-class) :editing
          (tuatara.buffer:buffer-local buffer :keys-as-typed) 'terminal-typing-p
          (tuatara.buffer:buffer-local buffer :key-handler) 'terminal-key-handler
          (tuatara.buffer:buffer-local buffer :paste-handler) 'terminal-paste-handler
          (tuatara.buffer:buffer-local buffer :directory) directory)
    (dolist (toggle '(line-numbers hl-line cursor-column soft-wrap spellcheck visual-line centred-text))
      (ignore-errors (choose-display buffer toggle nil)))
    ;; shown where *terminal-place* says, and its program started, or the buffer killed again
    (let ((started nil))
      (unwind-protect
           (progn
             (unless (show-terminal buffer place)
               (error "There is no room for a terminal there"))
             (multiple-value-bind (columns rows) (terminal-window-size (editor-window editor))
               (setf (tuatara.buffer:buffer-local buffer :emulator)
                     (multiple-value-bind (cell-width cell-height) (terminal-cell-pixels)
                       (start-emulator (first program) (rest program) :columns columns :rows rows :directory directory
                                                                      :wake (editor-job-waker)
                                                                      :scrollback-lines tuatara-user:*terminal-scrollback*
                                                                      :pictures (plusp cell-width)
                                                                      :cell-width cell-width :cell-height cell-height))))
             (setf started t))
        (unless started (tuatara.buffer:kill-buffer buffer))))
    (push buffer *terminal-buffers*)
    (set-modal-state :insert buffer)
    (when careful (message "The terminal starts in your home directory: ~a" careful))
    (terminal-refresh buffer (tuatara.buffer:buffer-local buffer :emulator) :all t)))

(define-command terminal-toggle ()
  "Show the terminal and type into it, or hide it again: the terminal opened
last, where *TERMINAL-PLACE* says (a new one when there is none); selected
when it shows already; hidden when you are in it, its window closed (or, at
:full, the buffer before it back). The terminal goes on while hidden, its
program and screen as they were."
  (let* ((editor *editor*)
         (buffer (find-if #'live-buffer-p *terminal-buffers*))
         (window (and buffer (buffer-window buffer editor))))
    (cond ((null buffer) (terminal))
          ((eq window (editor-window editor)) (leave-window window))
          (window (select-window window) (set-modal-state :insert buffer))
          (t (show-terminal buffer)))))

(define-command terminal-stop-typing ()
  "Stop typing into the terminal: its history view, its whole history (what
went up off its screen too) standing still as text with its colours, to move
through, search, select and copy while its program goes on; with modal
editing on in SUPPORT state (NORMAL's keys that read and select, and the
leader), else with the keys of any text. \\<terminal-mode>\\[terminal-start-typing] types into it again, the screen
as it is by then; \\[terminal-copy-and-type] copies what is selected and types into it again."
  (set-terminal-history-view (current-buffer) t)
  (message "Terminal history: ~a types into it again"
           (substitute-command-keys "\\<terminal-mode>\\[terminal-start-typing]")))

(define-command terminal-start-typing ()
  "Type into the terminal again after its history view (\\[terminal-stop-typing]):
everything typed goes to its program, the screen shown as it is now."
  (set-terminal-history-view (current-buffer) nil))

(define-command terminal-copy-and-type ()
  "In a terminal's history view, copy what is selected (as \\[kill-ring-save]) and
type into the terminal again; with nothing selected, type into it again."
  (let ((buffer (current-buffer)))
    (when (and (tuatara.buffer:buffer-mark-active-p buffer)
               (multiple-value-bind (start end) (tuatara.buffer:buffer-region buffer) (and start (< start end))))
      (kill-ring-save)
      (setf (tuatara.buffer:buffer-mark-active-p buffer) nil))
    (terminal-start-typing)))

(define-command terminal-paste ()
  "Paste into the terminal's program the text copied last, in Tuatara or
another program (as \\[yank] takes it), and type into the terminal: marked
as pasted for a program that asks, so that a shell runs none of its lines
until you press RET, and its control characters made spaces, so that pasted
text cannot work the terminal. While you type there *TERMINAL-PASTE-KEYS*
paste."
  (let* ((buffer (current-buffer))
         (emulator (live-emulator buffer)))
    (take-in-clipboard)
    (let ((kill (first (editor-kill-ring *editor*))))
      (unless kill (error "Nothing has been copied"))
      (paste-into-terminal buffer emulator (if (stringp kill) kill (tuatara.text:rope-string kill))))))

(define-command terminal-paste-primary ()
  "Paste into the terminal's program the text selected last in any program
\(the primary selection), as the middle button does, and type into it."
  (let ((buffer (current-buffer)))
    (paste-into-terminal buffer (live-emulator buffer) (tuatara.frontend:frontend-primary-selection (editor-frontend *editor*)))))

(defun terminal-paste-handler (buffer text)
  "Paste TEXT, pasted into the terminal Tuatara runs in while BUFFER, a
terminal, was shown, into BUFFER's program (PASTE-INTO-TERMINAL)."
  (paste-into-terminal buffer (live-emulator buffer) text))

(defun live-emulator (buffer)
  "The emulator of BUFFER, a terminal, while its program runs; else an error
saying no program runs there."
  (let ((emulator (tuatara.buffer:buffer-local buffer :emulator)))
    (unless (and emulator (emulator-alive-p emulator))
      (error "No program runs in this buffer"))
    emulator))

(defun paste-into-terminal (buffer emulator text)
  "Paste TEXT into EMULATOR's program, BUFFER's terminal, typing into it again
\(EMULATOR-PASTE); nothing when TEXT is NIL or empty. Text of more than one
line, or ending a line, for a program that does not ask for pastes marked
\(EMULATOR-BRACKETED-PASTE-P), which then runs each line as it comes, is
pasted only after asking."
  (when (and text (plusp (length text)))
    (let* ((text (lf-newlines text))
           (ends (count-if (lambda (c) (member c '(#\Newline #\Return))) text))
           (lines (if (member (char text (1- (length text))) '(#\Newline #\Return)) ends (1+ ends))))
      (if (and (plusp ends) (not (emulator-bracketed-paste-p emulator))
               (not (yes-or-no-p* (format nil "Paste ~:d line~:p; the program runs each as it comes; go on?" lines))))
          (message "Not pasted")
          (progn
            (unless (terminal-typing-p buffer) (set-terminal-history-view buffer nil))
            (emulator-paste emulator text))))))

(define-command terminal-search ()
  "Search the terminal's whole history, from where its program's cursor is
back (C-r as anywhere): typing there, its history view is shown first and
the search starts once it is in; in the history view, search on from point
\(\\[regex-search-forward])."
  (let ((buffer (current-buffer)))
    (if (terminal-typing-p buffer)
        (progn (setf (tuatara.buffer:buffer-local buffer 'terminal-search-wanted) t)
               (set-terminal-history-view buffer t)
               (message "Terminal history: reading it to search"))
        (regex-search-forward))))

(define-command terminal-send-key ()
  "Send the next key typed to the program in this terminal, whatever it is: one
of those that stay the editor's (C-g, C-x, M-x, C-q), say."
  (let ((emulator (live-emulator (current-buffer))))
    (let ((key (let ((*keys-as-typed* t)) (read-key "Key for the program: "))))
      (echo "")
      (unless (emulator-key emulator key)
        (message "~a means nothing to a terminal" (tuatara.frontend:key-description (tuatara.frontend:key-spec key)))))))

(defun forget-terminal (buffer)
  "End the program of BUFFER, a terminal being killed (*KILL-BUFFER-FUNCTIONS*)."
  (let ((emulator (tuatara.buffer:buffer-local buffer :emulator)))
    (when emulator
      (setf (tuatara.buffer:buffer-local buffer :emulator) nil)
      (stop-emulator emulator :background t))
    (setf *terminal-buffers* (remove buffer *terminal-buffers*))))

(defun install-terminal ()
  "Bind the terminal's keys and have its screens kept up to date."
  ;; in the history view (typing, the key handler takes them first)
  (define-key (mode-keymap 'terminal-mode) "C-\\" 'terminal-start-typing)
  (define-key (mode-keymap 'terminal-mode) "RET" 'terminal-copy-and-type)
  (define-key (mode-keymap 'terminal-mode) "C-q" 'terminal-send-key)
  (define-key (mode-keymap 'terminal-mode) "C-y" 'terminal-paste)
  (define-key (mode-keymap 'terminal-mode) "C-c C-o" 'terminal-open-link)
  (define-key (mode-keymap 'terminal-mode) "<mouse-2>" 'terminal-paste-primary)
  (define-key (mode-keymap 'terminal-mode) "C-<down-mouse-1>" 'terminal-open-link)
  (define-key (mode-keymap 'terminal-mode) "p" 'terminal-paste)
  (define-key (mode-keymap 'terminal-mode) "i" 'terminal-start-typing)
  (define-key (mode-keymap 'terminal-mode) "a" 'terminal-start-typing)
  (pushnew 'forget-terminal tuatara.buffer:*kill-buffer-functions*)
  (pushnew 'terminal-mode-line-title *mode-line-functions*)
  (pushnew 'terminal-tick *tick-functions*))
