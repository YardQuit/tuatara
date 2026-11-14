;;;; src/frontend/messages.lisp --- What a remote frontend and its client say to each other.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The painter interface carried over a local socket. Each message is a
;;;; 4-octet length, a 1-octet kind and its fields in a fixed order: numbers
;;;; least significant octet first, text as UTF-8 after its length in octets.
;;;; This file is the one place the format is written and read. Nothing read
;;;; is trusted: every length, count and number is held to its bound before
;;;; it is used, and a message that breaks one is a MALFORMED-MESSAGE, which
;;;; ends that connection and nothing else.

(in-package #:tuatara.frontend)

;;; Kinds and bounds

(defconstant +protocol+ 6
  "The number of the messages' format, said first both ways: a client and an
editor of different numbers, or of different builds, do not talk.")

(defparameter *message-kinds*
  '((:hello 1 1024 1024)
    ;; from a client to the editor
    (:attach 2 1048576 nil) (:facts 3 65536 nil) (:event 4 65536 nil) (:answer 5 65536 nil) (:detach 6 64 nil)
    (:no-window 7 2048 nil) (:picture-taken 8 64 nil)
    ;; from the editor to a client
    (:frame 16 nil 16777216) (:names 17 nil 1048576) (:faces 18 nil 1048576) (:picture 19 nil 64)
    (:title 20 nil 65536) (:selection 21 nil 16777216) (:toggle 22 nil 64) (:raise 23 nil 64)
    (:ask 24 nil 65536) (:exit 25 nil 2048) (:refused 26 nil 4096) (:take-back 27 nil 64) (:settings 28 nil 4096)
    (:another 29 nil 256) (:picture-begin 30 nil 64) (:picture-piece 31 nil 32864) (:picture-forget 32 nil 64))
  "Each kind of message as (NAME CODE FROM-CLIENT FROM-EDITOR): its name, its
number on the wire, and the most octets it may take as the editor reads it
from a client and as a client reads it from the editor, NIL where it is never
sent that way.")

(defconstant +most-columns+ 4096 "The most columns a remote screen may have.")
(defconstant +most-rows+ 2048 "The most rows a remote screen may have.")
(defconstant +most-names+ 4096 "The most face names one connection numbers.")
(defconstant +most-characters+ 1048576
  "The most octets of a row's text, and so the most characters its spans reach into.")
(defconstant +most-place+ 65535 "The most a cell of a picture, or the cursor, is from the screen's or picture's first.")
(defconstant +most-text+ 16777216 "The most octets of text pasted, or of a clipboard, carried at once.")
(defconstant +text-piece+ 60000 "The most octets of text one message from a client carries; more goes in pieces.")
(defconstant +most-pictures+ 16 "The most pictures a client keeps; both ends let go of the oldest first.")
(defconstant +most-picture-octets+ 268435456 "The most octets of pixels of one picture.")
(defconstant +most-far-picture-octets+ 134217728
  "The most octets of pixels of one picture sent to a client on another machine.")
(defconstant +most-far-pictures-kept+ 268435456
  "The most octets of pixels of the pictures a client on another machine keeps at once.")
(defconstant +most-far-picture-side+ 16384
  "The most pixels either way of a picture sent to a client on another machine.")
(defconstant +far-picture-piece+ 32768
  "The most octets of a picture one message to a client on another machine carries.")
(defconstant +far-pictures-in-flight+ 131072
  "How many octets of a picture a client on another machine may have not yet
taken when another piece is sent: with that piece, at most this and
+FAR-PICTURE-PIECE+ less one stand between a key and what it shows.")
(defconstant +most-pictures-unpacking+ 4
  "The most pictures a client on another machine unpacks at once: the one
coming, and those it was told to forget that are not done yet. Past it, reading
what the editor sends waits for one to be done.")

(define-condition malformed-message (error)
  ((reason :initarg :reason :reader malformed-message-reason
           :documentation "What was wrong with the message, in words."))
  (:report (lambda (condition stream)
             (format stream "A malformed message: ~a" (malformed-message-reason condition))))
  (:documentation "Signalled when what the other end of a connection sent breaks the format or one of its
bounds; the connection is ended."))

(setf (documentation 'malformed-message-reason 'function) "What was wrong with the message of CONDITION, in words.")

(defun malformed (control &rest arguments)
  "Signal a MALFORMED-MESSAGE whose reason FORMAT makes of CONTROL and ARGUMENTS."
  (error 'malformed-message :reason (apply #'format nil control arguments)))

(defun kind-code (kind)
  "The number on the wire of the message kind KIND (*MESSAGE-KINDS*)."
  (second (assoc kind *message-kinds*)))

;;; Writing

(define-record (outbox (:constructor make-outbox (&optional (size 4096) &aux (octets (tuatara.text:make-octets size))))
                      (:copier nil))
  "Messages written and not yet sent: the first FILL octets of OCTETS, and
DESCRIPTORS, each (AT . FD), the open file descriptor (FD) passed with the
octet at AT."
  (octets nil :type tuatara.text:octets
          :documentation "The octets of BOX's messages, the first OUTBOX-FILL of them written.")
  (fill 0 :type fixnum :documentation "How many octets of messages BOX holds.")
  (descriptors '() :type list :documentation "The descriptors BOX passes, each (AT . FD), newest first."))

(setf (documentation 'make-outbox 'function) "An empty OUTBOX with room for SIZE octets before it grows.")

(declaim (inline outbox-room))
(defun outbox-room (box count)
  "Where the next COUNT octets go in BOX, room made for them."
  (declare (type outbox box) (type fixnum count))
  (let ((at (outbox-fill box)) (octets (outbox-octets box)))
    (when (> (+ at count) (length octets))
      (let ((larger (tuatara.text:make-octets (max (+ at count) (* 2 (length octets))))))
        (replace larger octets :end2 at)
        (setf (outbox-octets box) larger)))
    (setf (outbox-fill box) (+ at count))
    at))

(defconstant +roomy-box+ (* 1024 1024)
  "Octets past which the room of a box grown for one large message (a frame,
a paste) is given back once it is empty.")

(defun empty-outbox (box)
  "Make BOX empty, giving back the room a large message made it take
\(+ROOMY-BOX+)."
  (setf (outbox-fill box) 0)
  (when (> (length (outbox-octets box)) +roomy-box+)
    (setf (outbox-octets box) (tuatara.text:make-octets 65536))))

(defun put-number (box value count)
  "Put the integer VALUE in COUNT octets of BOX, least significant first, a
negative one as two's complement."
  (declare (type outbox box) (type integer value) (type (integer 1 4) count))
  (let* ((at (outbox-room box count)) (octets (outbox-octets box)))
    (dotimes (k count)
      (setf (aref octets (+ at k)) (ldb (byte 8 (* 8 k)) value)))))

(defun put-octets (box octets &optional (start 0) (end (length octets)))
  "Put OCTETS from START below END in BOX as they are."
  (let ((at (outbox-room box (- end start))))
    (replace (outbox-octets box) octets :start1 at :start2 start :end2 end)))

(defun put-bounded (box value most)
  "Put VALUE, an integer, in 4 octets of BOX, as 0 when less and as MOST when
more: a place on the screen or in a row, which the reading end holds to MOST."
  (put-number box (max 0 (min most value)) 4))

(defun patch-length (box at)
  "Write over the 4 octets of BOX at AT how many octets BOX holds after them."
  (let ((length (- (outbox-fill box) at 4)) (octets (outbox-octets box)))
    (dotimes (k 4)
      (setf (aref octets (+ at k)) (ldb (byte 8 (* 8 k)) length)))))

(defun put-text (box string &optional most)
  "Put STRING in BOX as UTF-8 after its length in octets, in 4 octets; with
MOST, only as much of it as takes at most MOST octets, cut at a character, as
the reading end takes no more."
  (let ((at (outbox-room box 4)))
    (multiple-value-bind (octets end) (tuatara.text:put-string-octets string (outbox-octets box) (+ at 4))
      (setf (outbox-octets box) octets
            (outbox-fill box) (if (and most (> (- end at 4) most))
                                  (loop for cut downfrom (+ at 4 most) above (+ at 4)
                                        unless (tuatara.text:continuation-byte-p (aref octets cut)) return cut
                                        finally (return (+ at 4)))
                                  end))
      (patch-length box at))))

(defun begin-message (box kind)
  "Begin a message of KIND (*MESSAGE-KINDS*) at the end of BOX; where it
begins, for END-MESSAGE."
  (let ((at (outbox-fill box)))
    (put-number box 0 4)
    (put-number box (kind-code kind) 1)
    at))

(defun end-message (box at)
  "End the message of BOX begun at AT (BEGIN-MESSAGE), its length written."
  (patch-length box at))

(defun outbox-cut (box start end)
  "Take the octets of BOX from START below END out of it, those after moved
down, and the descriptors to be passed with them, closed."
  (let ((octets (outbox-octets box)) (gone (- end start)))
    (replace octets octets :start1 start :start2 end :end2 (outbox-fill box))
    (decf (outbox-fill box) gone)
    (when (outbox-descriptors box)
      (close-descriptors (loop for (at . fd) in (outbox-descriptors box) when (<= start at (1- end)) collect fd))
      (setf (outbox-descriptors box)
            (loop for (at . fd) in (outbox-descriptors box)
                  unless (<= start at (1- end)) collect (cons (if (>= at end) (- at gone) at) fd))))))

;;; Reading

(define-record (inbox (:constructor make-inbox (fd side &key (octets (tuatara.text:make-octets 65536))
                                                         (end 0) deadline))
                     (:copier nil))
  "What the other end of a connection has sent, read from FD (NIL when
OCTETS hold all there is) by SIDE, :EDITOR for the editor reading a client
and :CLIENT for a client reading the editor: OCTETS from START below END not
yet read as messages; the message being read has its next field at AT and
ends at STOP. DESCRIPTORS are those passed and not yet taken, at most
MOST-DESCRIPTORS of them (NIL: as SIDE takes them); DEADLINE, an internal
real time past which waiting for more is given up (NIL: none)."
  (fd nil :documentation "The socket INBOX reads, or NIL when its octets are all there is.")
  (side :editor :type (member :editor :client)
        :documentation "Who reads INBOX: :EDITOR, from a client, or :CLIENT, from the editor.")
  (octets (tuatara.text:make-octets 65536) :type tuatara.text:octets :documentation "The octets INBOX has read.")
  (start 0 :type fixnum :documentation "Where INBOX's octets not yet read as messages begin.")
  (end 0 :type fixnum :documentation "Where INBOX's octets read so far end.")
  (at 0 :type fixnum :documentation "Where the next field of the message being read is in INBOX's octets.")
  (stop 0 :type fixnum :documentation "Where the message being read ends in INBOX's octets.")
  (descriptors '() :type list :documentation "The descriptors passed to INBOX and not yet taken, oldest first.")
  (most-descriptors nil
                    :documentation "The most descriptors INBOX takes passed and not yet
taken, or NIL for as many as its side takes.")
  (deadline nil :documentation "When INBOX gives up waiting for more, an internal real time, or NIL."))

(setf (documentation 'make-inbox 'function)
      "An INBOX reading what FD sends, for SIDE (:EDITOR or :CLIENT); with FD NIL, OCTETS below END are all there is.")

(defun close-descriptors (descriptors)
  "Close each of DESCRIPTORS, open file descriptors."
  (dolist (fd descriptors)
    (tuatara.runtime:ignoring-errors-reported ("closing descriptor ~d" fd) (sb-posix:close fd))))

(defun fill-inbox (inbox wanted)
  "Read into INBOX until at least WANTED octets are there not yet read as
messages, waiting for them; NIL when the other end has gone first. More
descriptors passed than its side takes are malformed."
  (loop while (< (- (inbox-end inbox) (inbox-start inbox)) wanted)
        do (let ((fd (inbox-fd inbox)) (octets (inbox-octets inbox)))
             (unless fd (return-from fill-inbox nil))
             ;; what is not yet read moved to the front, and room made for the rest; the room a large
             ;; message took given back once it is read
             (when (plusp (inbox-start inbox))
               (let ((left (- (inbox-end inbox) (inbox-start inbox))))
                 (when (and (> (length octets) +roomy-box+) (<= (max left wanted) 65536))
                   (setf octets (setf (inbox-octets inbox) (replace (tuatara.text:make-octets 65536) octets
                                                                     :start2 (inbox-start inbox)
                                                                     :end2 (inbox-end inbox)))
                         (inbox-start inbox) 0 (inbox-end inbox) left))
                 (replace octets octets :start2 (inbox-start inbox) :end2 (inbox-end inbox))
                 (setf (inbox-end inbox) left (inbox-start inbox) 0)))
             (when (< (length octets) wanted)
               (let ((larger (tuatara.text:make-octets wanted)))
                 (replace larger octets :end2 (inbox-end inbox))
                 (setf (inbox-octets inbox) (setf octets larger))))
             (multiple-value-bind (got descriptors cut errno)
                 (tuatara.io:with-octets-sap (sap octets)
                   (tuatara.io:receive-with-descriptors fd (sb-sys:sap+ sap (inbox-end inbox))
                                                        (- (length octets) (inbox-end inbox))))
               (when descriptors
                 (setf (inbox-descriptors inbox) (append (inbox-descriptors inbox) descriptors)))
               ;; the editor takes none from a client but with its attach (standard input); a client one
               ;; for each picture, a few at a time: past that, every one passed is closed at once
               (when (or cut (> (length (inbox-descriptors inbox))
                                (or (inbox-most-descriptors inbox)
                                    (if (eq (inbox-side inbox) :editor) 0 +most-pictures+))))
                 (close-descriptors (shiftf (inbox-descriptors inbox) '()))
                 (malformed "descriptors passed, more than are taken"))
               (cond ((plusp got) (incf (inbox-end inbox) got))
                     ((zerop got) (return-from fill-inbox nil))
                     ((= errno sb-posix:eintr))
                     ((= errno sb-posix:eagain)
                      (let ((deadline (inbox-deadline inbox)))
                        (when (and deadline (tuatara.runtime:deadline-passed-p deadline))
                          (malformed "nothing said in time"))
                        ;; with no deadline, woken only by what comes or by the socket shut
                        (tuatara.io:poll-fd fd 1 (and deadline
                                                      (max 1 (ceiling (* 1000 (tuatara.runtime:seconds-left
                                                                               deadline))))))))
                     (t (return-from fill-inbox nil))))))
  t)

(defun read-message (inbox)
  "The kind of the next message INBOX has, read whole, its fields to be taken
in order with the TAKE- functions; NIL when the other end has gone. A message
of an unknown kind, of a kind never sent this way, or longer than its kind
may be, is malformed."
  (unless (fill-inbox inbox 5) (return-from read-message nil))
  (let* ((octets (inbox-octets inbox)) (start (inbox-start inbox))
         (length (loop for k below 4 sum (ash (aref octets (+ start k)) (* 8 k))))
         (code (aref octets (+ start 4)))
         (entry (find code *message-kinds* :key #'second))
         (most (and entry (if (eq (inbox-side inbox) :editor) (third entry) (fourth entry)))))
    (unless most (malformed "a message of kind ~d, which is not taken here" code))
    (unless (<= 1 length most) (malformed "a ~(~a~) message of ~d octets, past its ~d" (first entry) length most))
    (unless (fill-inbox inbox (+ 4 length)) (return-from read-message nil))
    (let ((start (inbox-start inbox)))
      (setf (inbox-at inbox) (+ start 5) (inbox-stop inbox) (+ start 4 length) (inbox-start inbox) (+ start 4 length)))
    (first entry)))

(defun take-place (inbox count)
  "Where the next COUNT octets of the message being read begin in INBOX's
octets, taken; malformed when the message ends before them."
  (let ((at (inbox-at inbox)))
    (unless (<= (+ at count) (inbox-stop inbox)) (malformed "a message cut short"))
    (setf (inbox-at inbox) (+ at count))
    at))

(defun take-number (inbox count)
  "The unsigned integer in the next COUNT octets of the message being read
from INBOX, least significant first."
  (let ((at (take-place inbox count)) (octets (inbox-octets inbox)))
    (loop for k below count sum (ash (aref octets (+ at k)) (* 8 k)))))

(defun take-signed (inbox count)
  "The signed integer in the next COUNT octets of the message being read from
INBOX, as two's complement."
  (let ((value (take-number inbox count)))
    (if (logbitp (1- (* 8 count)) value) (- value (ash 1 (* 8 count))) value)))

(defun take-bounded (inbox count least most what)
  "The unsigned integer in the next COUNT octets from INBOX, malformed unless
from LEAST to MOST; WHAT names it in the reason."
  (let ((value (take-number inbox count)))
    (unless (<= least value most) (malformed "~a ~d, past its bounds ~d to ~d" what value least most))
    value))

(defun take-text (inbox most)
  "The text next in the message being read from INBOX, at most MOST octets;
what is not UTF-8 in it read as raw bytes."
  (let* ((length (take-bounded inbox 4 0 most "a text of"))
         (at (take-place inbox length)))
    (tuatara.text:octets-string (inbox-octets inbox) :start at :end (+ at length))))

(defun take-count (inbox count most each)
  "A count in the next COUNT octets from INBOX, at most MOST, of things each
taking at least EACH octets of what is left of the message."
  (let ((value (take-bounded inbox count 0 most "a count of")))
    (unless (<= (* value each) (- (inbox-stop inbox) (inbox-at inbox)))
      (malformed "a count of ~d, more than the message holds" value))
    value))

(defun message-read (inbox)
  "Malformed unless the message being read from INBOX has no octets left."
  (unless (= (inbox-at inbox) (inbox-stop inbox))
    (malformed "~d octets left over at a message's end" (- (inbox-stop inbox) (inbox-at inbox)))))

;;; Saying hello

(defun put-hello (box)
  "Put a hello in BOX: the format's number and this build's version line."
  (let ((at (begin-message box :hello)))
    (put-number box +protocol+ 4)
    (put-text box (tuatara:version-line) 512)
    (end-message box at)))

(defun take-hello (inbox)
  "Read a hello from INBOX, whose message has begun; the other end's version
line, or NIL when its format or build differs from this one's, with what it
runs in words as a second value."
  (let ((protocol (take-number inbox 4))
        (line (take-text inbox 512)))
    (message-read inbox)
    (cond ((/= protocol +protocol+) (values nil (format nil "~a, with messages of format ~d" line protocol)))
          ((string/= line (tuatara:version-line)) (values nil line))
          (t line))))

;;; Facts: what a client's screen is, for the editor's questions

(define-record (facts (:constructor make-facts ()) (:copier copy-facts))
  "What a client said of its screen, from which the editor's questions to a
remote frontend are answered. WINDOW is true for a window of its own, else a
terminal. WIDTH and HEIGHT are in cells. COLOURS is what it can show.
CELL-WIDTH and CELL-HEIGHT are in pixels, or NIL. IMAGES is true when it shows
pictures. REORDERS is true when it turns right-to-left text round itself.
STYLE is the desktop's, :dark, :light or NIL. FONT-STEPS is a number, or NIL.
DESCRIPTION is words for where it shows the editor. HEALTH is (LABEL . TEXT)
pairs. FONTS and FONT-FACES are as FRONTEND-FONTS and FRONTEND-FONT-FACES give
them."
  (window nil :documentation "True when the client's screen is a window of its own.")
  (width 80 :type fixnum :documentation "The client's screen's width in cells.")
  (height 24 :type fixnum :documentation "The client's screen's height in cells.")
  (colours 256 :documentation "How many colours the client can show.")
  (cell-width nil :documentation "A cell's width in pixels, or NIL.")
  (cell-height nil :documentation "A cell's height in pixels, or NIL.")
  (images nil :documentation "True when the client shows pictures.")
  (reorders nil :documentation "True when the client turns right-to-left text round itself.")
  (style nil :documentation "The desktop's style the client knows, :dark, :light or NIL.")
  (font-steps nil :documentation "The steps the client's font is scaled by, or NIL.")
  (description nil :documentation "Where the client shows the editor, in words, or NIL.")
  (health '() :documentation "What the client says of itself for the health check,
(LABEL . TEXT) pairs.")
  (fonts '() :documentation "The fixed-width font families the client can show.")
  (font-faces '() :documentation "The font families and their variants the client can show."))

(setf (documentation 'make-facts 'function) "FACTS of an 80 by 24 terminal of 256 colours, as standard."
      (documentation 'copy-facts 'function) "A copy of FACTS, to be changed where the original is not."
      (documentation 'facts-p 'function) "True when OBJECT is FACTS.")

(defparameter *styles* #(nil :dark :light)
  "The desktop's styles, by their numbers in a message.")

(defun put-facts (box frontend &key whole)
  "Put what FRONTEND is (FACTS) in BOX, as fields of a message; with WHOLE,
its health and its fonts too."
  (multiple-value-bind (width height) (frontend-size frontend)
    (multiple-value-bind (cell-width cell-height) (frontend-cell-pixels frontend)
      (let ((steps (frontend-font-steps frontend)))
        (put-number box (if steps 1 0) 1)
        (put-number box (min +most-columns+ (max 1 width)) 2)
        (put-number box (min +most-rows+ (max 1 height)) 2)
        (put-number box (max 0 (min (frontend-colour-count frontend) 16777216)) 4)
        (put-number box (if cell-width (min 65535 (max 1 (round cell-width))) 0) 2)
        (put-number box (if cell-height (min 65535 (max 1 (round cell-height))) 0) 2)
        (put-number box (logior (if (frontend-images-p frontend) 1 0) (if (frontend-reorders-text-p frontend) 2 0)) 1)
        (put-number box (or (position (frontend-desktop-style frontend) *styles*) 0) 1)
        (put-number box (if steps (max -64 (min 64 steps)) 0) 2)
        (put-text box (or (frontend-description frontend) "") 1024)
        (when whole
          (let ((health (frontend-health frontend)))
            (put-number box (min 64 (length health)) 2)
            (loop for (label . text) in health repeat 64
                  do (put-text box (princ-to-string label) 1024) (put-text box (princ-to-string text) 4096)))
          (let ((fonts (frontend-fonts frontend)))
            (put-number box (min 8192 (length fonts)) 4)
            (loop for font in fonts repeat 8192 do (put-text box font 256)))
          (let ((families (frontend-font-faces frontend)))
            (put-number box (min 8192 (length families)) 4)
            (loop for (family fixed variants) in families repeat 8192
                  do (put-text box family 256)
                     (put-number box (if fixed 1 0) 1)
                     (put-number box (min 256 (length variants)) 2)
                     (loop for (name font) in variants repeat 256
                           do (put-text box name 256) (put-text box font 512)))))))))

(defun take-facts (inbox &key whole)
  "Read FACTS from INBOX, as PUT-FACTS put them, WHOLE as given there."
  (let ((facts (make-facts)))
    (setf (facts-window facts) (= 1 (take-bounded inbox 1 0 1 "a kind of screen"))
          (facts-width facts) (take-bounded inbox 2 1 +most-columns+ "a width")
          (facts-height facts) (take-bounded inbox 2 1 +most-rows+ "a height")
          (facts-colours facts) (take-bounded inbox 4 0 16777216 "a count of colours"))
    (let ((cell-width (take-number inbox 2)) (cell-height (take-number inbox 2)))
      (when (and (plusp cell-width) (plusp cell-height))
        (setf (facts-cell-width facts) cell-width (facts-cell-height facts) cell-height)))
    (let ((flags (take-bounded inbox 1 0 3 "flags")))
      (setf (facts-images facts) (logtest 1 flags) (facts-reorders facts) (logtest 2 flags)))
    (setf (facts-style facts) (svref *styles* (take-bounded inbox 1 0 (1- (length *styles*)) "a style")))
    (let ((steps (take-signed inbox 2)))
      (unless (<= -64 steps 64) (malformed "font steps ~d" steps))
      (when (facts-window facts) (setf (facts-font-steps facts) steps)))
    (let ((description (take-text inbox 1024)))
      (setf (facts-description facts) (and (plusp (length description)) description)))
    (when whole
      (setf (facts-health facts)
            (loop repeat (take-count inbox 2 64 8)
                  collect (cons (take-text inbox 1024) (take-text inbox 4096)))
            (facts-fonts facts)
            (loop repeat (take-count inbox 4 8192 4) collect (take-text inbox 256))
            (facts-font-faces facts)
            (loop repeat (take-count inbox 4 8192 7)
                  collect (list (take-text inbox 256) (= 1 (take-bounded inbox 1 0 1 "a fixed width"))
                                (loop repeat (take-count inbox 2 256 8)
                                      collect (list (take-text inbox 256) (take-text inbox 512)))))))
    facts))

;;; Settings: how the editor's init file has a client's screen shown

(defparameter *display-settings*
  '((tuatara-user:*gui-font* :font) (tuatara-user:*gui-padding* :padding)
    (tuatara-user:*window-colours-from-desktop* :boolean)
    (tuatara-user:*gui-renderer* (:gl :vulkan :cairo nil) :at-start)
    (tuatara-user:*tui-padding* :padding) (tuatara-user:*tui-pictures* (:auto t nil))
    (tuatara-user:*tui-mouse* :boolean) (tuatara-user:*tui-colours* (:auto :24-bit 256 16 8 nil))
    (tuatara-user:*kitty-keyboard* (:auto t nil) :at-start) (tuatara-user:*altgr-as-meta* :boolean))
  "The settings a client's window or terminal is shown by, which the editor
reads from its init file and tells its clients, each (NAME KIND [:AT-START]):
KIND, :FONT for a font's name or NIL, :PADDING for room round the text,
:BOOLEAN, or the list of values it may take, its standard first. One said
:AT-START is taken as the client's screen opens, and not changed after.")

(defun font-name-p (name)
  "True when NAME may name a client's font: text of at most 256 octets with
no control character in it."
  (and (stringp name) (<= (tuatara.text:string-octet-length name) 256)
       (notany (lambda (c) (tuatara.text:control-code-p (char-code c))) name)))

(defun put-settings (box)
  "Put in BOX, as fields of a message, the values of *DISPLAY-SETTINGS* here;
one unset, or of a value it may not take, is left as the client has it."
  (loop for (name kind) in *display-settings*
        for value = (if (boundp name) (symbol-value name) :unset)
        do (case kind
             (:font (let ((given (or (null value) (font-name-p value))))
                      (put-number box (if given 1 0) 1)
                      (when given (put-text box (or value "")))))
             (:padding (put-number box (if (eq value :unset) 0 1) 1)
              (unless (eq value :unset)
                (dolist (side (multiple-value-list (padding-sides value)))
                  (put-number box (min 999 side) 2))))
             (:boolean (put-number box (cond ((eq value :unset) 0) (value 2) (t 1)) 1))
             (t (put-number box (let ((n (position value kind))) (if n (1+ n) 0)) 1)))))

(defun take-settings (inbox)
  "Read the settings of a message from INBOX, as PUT-SETTINGS put them: each
given as (NAME . VALUE), in the order of *DISPLAY-SETTINGS*."
  (prog1 (loop for (name kind) in *display-settings*
               for value = (case kind
                             (:font (if (zerop (take-bounded inbox 1 0 1 "a setting's flag"))
                                        :unset
                                        (let ((font (take-text inbox 256)))
                                          (unless (font-name-p font)
                                            (malformed "a font named with a control character"))
                                          (and (plusp (length font)) font))))
                             (:padding (if (zerop (take-bounded inbox 1 0 1 "a setting's flag"))
                                           :unset
                                           (loop repeat 4 collect (take-bounded inbox 2 0 999 "padding"))))
                             (:boolean (case (take-bounded inbox 1 0 2 "a setting") (0 :unset) (1 nil) (t t)))
                             (t (let ((n (take-bounded inbox 1 0 (length kind) "a setting")))
                                  (if (zerop n) :unset (nth (1- n) kind)))))
               unless (eq value :unset)
                 collect (cons name value))
    (message-read inbox)))

;;; What a client asks for as it attaches

(define-record (request (:constructor make-request (&key directory terminal environment files safe hex standard-input
                                                         diff merge (frames 1) wait stop joins far agent notes))
                       (:copier nil))
  "What a client asks of the editor as it attaches, beside what its screen
is. DIRECTORY is the directory it was started in, an absolute path. TERMINAL is
the device of its terminal (/dev/pts/3), or NIL. ENVIRONMENT is its variables
the editor keeps for it, each (NAME . VALUE). FILES is each (PATH LINE COLUMN),
an absolute path and the line and column to go to, or NIL. SAFE and HEX are
true to open them so. STANDARD-INPUT is true when its standard input is passed
with the attach to be read into a buffer (on the editor's side, the
descriptor). DIFF is the two paths to compare, MERGE the four to merge, or
NIL. FRAMES is how many frames to open. WAIT is true when the program that
started it waits for the files to be done with. STOP is true when it asks the
editor to stop. JOINS is the word the editor gave a client asking it for
another window, which this connection then is (FRONTEND-ANOTHER), or NIL.
FAR is true when the client is on another machine, its connection carried by
ssh. Such a client passes no descriptor and names no terminal, and its paths
are the editor's machine's. AGENT, for such a client, is the socket on the
editor's machine its ssh connection brings the desk's agent to, for the
programs of its frames alone, or NIL. NOTES is what its connection said, at
most eight lines, to be shown in its frame."
  (directory nil :documentation "The directory the client of REQUEST was started in.")
  (terminal nil :documentation "The device of the client's terminal, or NIL.")
  (environment '() :documentation "The client's variables kept for it, each (NAME . VALUE).")
  (files '() :documentation "The files the client opens, each (PATH LINE COLUMN).")
  (safe nil :documentation "True when the client's files open safely.")
  (hex nil :documentation "True when the client's files open in the hex view.")
  (standard-input nil
                  :documentation "True when the client's standard input is read into a
buffer; on the editor's side, its descriptor.")
  (diff nil :documentation "The two paths the client compares, or NIL.")
  (merge nil :documentation "The four paths the client merges, or NIL.")
  (frames 1 :type fixnum :documentation "How many frames the client opens.")
  (wait nil :documentation "True when the client waits for its files to be done with.")
  (stop nil :documentation "True when the client asks the editor to stop.")
  (joins nil :documentation "The word of the window the editor asked the client for that this connection is, or NIL.")
  (far nil :documentation "True when the client is on another machine, reached over ssh.")
  (agent nil :documentation "The socket of the desk's agent for a far client's frames, or NIL.")
  (notes '() :documentation "What the client's connection said, to show in its frame."))

(setf (documentation 'make-request 'function) "A REQUEST of a client, as its keywords give it.")

(defconstant +most-path+ 4096 "The most octets of a path a client names.")

(defun put-request (box request)
  "Put REQUEST in BOX, as fields of a message: its files, paths and variables
past their bounds are the client's to refuse before it is sent."
  (put-number box (logior (if (request-wait request) 1 0) (if (request-stop request) 2 0)
                          (if (request-safe request) 4 0) (if (request-hex request) 8 0)
                          (if (request-standard-input request) 16 0) (if (request-joins request) 32 0)
                          (if (request-far request) 64 0))
              2)
  (put-number box (max 1 (min 16 (request-frames request))) 1)
  (put-text box (or (request-directory request) "/") +most-path+)
  (put-text box (or (request-terminal request) "") 256)
  (put-number box (length (request-environment request)) 2)
  (loop for (name . value) in (request-environment request)
        do (put-text box name 128) (put-text box value +most-path+))
  (put-number box (length (request-files request)) 2)
  (loop for (path line column) in (request-files request)
        do (put-text box path +most-path+)
           (put-number box (or line 0) 4)
           (put-number box (if column (1+ column) 0) 4))
  (dolist (paths (list (request-diff request) (request-merge request)))
    (put-number box (length paths) 1)
    (dolist (path paths) (put-text box path +most-path+)))
  (when (request-joins request)
    (put-text box (request-joins request) 64))
  (put-text box (or (request-agent request) "") 256)
  (let ((notes (subseq (request-notes request) 0 (min 8 (length (request-notes request))))))
    (put-number box (length notes) 1)
    (dolist (note notes) (put-text box note 1024))))

(defun put-no-window (box word words)
  "Put a message in BOX telling the editor the window asked for by WORD is not
opened, why being WORDS."
  (let ((at (begin-message box :no-window)))
    (put-text box word 64)
    (put-text box words 1024)
    (end-message box at)))

(defun window-word-p (word)
  "True when WORD may be the word a window asked for is known by: one to 64
ASCII letters and digits."
  (and (stringp word) (<= 1 (length word) 64) (every (lambda (c) (and (< (char-code c) 128) (alphanumericp c))) word)))

(defun take-path (inbox what)
  "An absolute path read from INBOX, of at most +MOST-PATH+ octets with no
NUL in it; malformed otherwise, WHAT naming it in the reason."
  (let ((path (take-text inbox +most-path+)))
    (unless (and (plusp (length path)) (char= (char path 0) #\/) (not (find (code-char 0) path)))
      (malformed "~a that is no absolute path" what))
    path))

(defun environment-name-p (name)
  "True when NAME is written as an environment variable's name: capitals,
digits and underscores, not first a digit."
  (and (plusp (length name))
       (every (lambda (c) (or (char<= #\A c #\Z) (char<= #\0 c #\9) (char= c #\_))) name)
       (not (digit-char-p (char name 0)))))

(defun take-request (inbox)
  "Read a REQUEST from INBOX, as PUT-REQUEST put it; malformed when a client
on another machine (REQUEST-FAR) names a terminal or passes its standard
input."
  (let* ((flags (take-bounded inbox 2 0 127 "a request's flags"))
         (frames (take-bounded inbox 1 1 16 "a count of frames"))
         (directory (take-path inbox "a directory"))
         (terminal (let ((device (take-text inbox 256)))
                     (cond ((zerop (length device)) nil)
                           ((and (uiop:string-prefix-p "/dev/" device) (not (find (code-char 0) device))
                                 (not (search "/.." device)))
                            device)
                           (t (malformed "a terminal that is no device")))))
         (environment (loop repeat (take-count inbox 2 128 8)
                            collect (let ((name (take-text inbox 128)) (value (take-text inbox +most-path+)))
                                      (unless (environment-name-p name) (malformed "a variable named ~s" name))
                                      (when (find (code-char 0) value) (malformed "a variable holding a NUL"))
                                      (cons name value))))
         (files (loop repeat (take-count inbox 2 4096 13)
                      collect (let ((path (take-path inbox "a file")) (line (take-number inbox 4))
                                    (column (take-number inbox 4)))
                                (list path (and (plusp line) line) (and (plusp column) (1- column))))))
         (diff (loop repeat (take-bounded inbox 1 0 2 "a count of files to compare")
                     collect (take-path inbox "a file to compare")))
         (merge (loop repeat (take-bounded inbox 1 0 4 "a count of files to merge")
                      collect (take-path inbox "a file to merge")))
         (joins (and (logtest 32 flags)
                     (let ((word (take-text inbox 64)))
                       (unless (window-word-p word) (malformed "a window asked for by a word of other characters"))
                       word)))
         (agent (let ((path (take-text inbox 256)))
                  (cond ((zerop (length path)) nil)
                        ((and (uiop:string-prefix-p "/" path) (not (find (code-char 0) path))) path)
                        (t (malformed "an agent's socket not named from the root")))))
         (notes (loop repeat (take-bounded inbox 1 0 8 "a count of notes") collect (take-text inbox 1024))))
    (unless (member (length diff) '(0 2)) (malformed "~d files to compare" (length diff)))
    (unless (member (length merge) '(0 4)) (malformed "~d files to merge" (length merge)))
    ;; nothing but octets crosses ssh, and a terminal there is no terminal here
    (when (and (logtest 64 flags) (or terminal (logtest 16 flags)))
      (malformed "a client on another machine passing a terminal or its standard input"))
    (when (and agent (not (logtest 64 flags)))
      (malformed "a client on this machine naming an agent"))
    (make-request :directory directory :terminal terminal :environment environment :files files
                  :safe (logtest 4 flags) :hex (logtest 8 flags) :standard-input (logtest 16 flags)
                  :diff diff :merge merge :frames frames :wait (logtest 1 flags) :stop (logtest 2 flags)
                  :joins joins :far (logtest 64 flags) :agent agent :notes notes)))

;;; Events, from a client

(defparameter *special-keys*
  (coerce (remove-duplicates (remove-if-not #'keywordp (mapcar #'cdr *key-names*)) :from-end t) 'simple-vector)
  "The keys that are no character (KEY-BASE), by their numbers in a message.")

(defparameter *event-kinds* #(:key :resize :paste :drop :close :focus)
  "The kinds of event a client sends, by their numbers in a message.")

(defun sendable-event-p (event)
  "True when EVENT is of a kind a client sends (*EVENT-KINDS*), a key's base
a character or one of *SPECIAL-KEYS*."
  (and (find (event-kind event) *event-kinds*)
       (or (not (eq (event-kind event) :key))
           (characterp (key-base (event-key event)))
           (find (key-base (event-key event)) *special-keys*))
       t))

(defun put-event (box event &key text more)
  "Put an event message of EVENT in BOX, which SENDABLE-EVENT-P says can be
sent; a paste's TEXT, when given, in its place, MORE true when more of it
follows in the next."
  (let ((at (begin-message box :event)))
    (put-number box (position (event-kind event) *event-kinds*) 1)
    (ecase (event-kind event)
      (:key (let* ((key (event-key event)) (base (key-base key))
                   (column (key-column key)) (row (key-row key)) (place (and column row t)))
              (put-number box (logior (if (key-ctrl key) 1 0) (if (key-meta key) 2 0) (if (key-shift key) 4 0)
                                      (if (characterp base) 0 8) (if place 16 0))
                          1)
              (if (characterp base)
                  (put-number box (char-code base) 4)
                  (put-number box (position base *special-keys*) 1))
              (when place
                (put-number box (max 0 (min 65535 column)) 2)
                (put-number box (max 0 (min 65535 row)) 2))))
      (:resize (put-number box (min +most-columns+ (max 1 (event-width event))) 2)
               (put-number box (min +most-rows+ (max 1 (event-height event))) 2))
      (:paste (put-number box (if more 1 0) 1)
              (put-text box (or text (event-text event)) +text-piece+))
      (:drop (put-number box (max 0 (min 65535 (or (event-column event) 0))) 2)
             (put-number box (max 0 (min 65535 (or (event-row event) 0))) 2)
             ;; only names that can be opened, whole: the editor refuses any other
             (let ((names (loop for file in (event-files event)
                                for name = (if (pathnamep file) (namestring file) file)
                                when (and (dropped-name-p name) (<= (tuatara.text:string-octet-length name) 4096))
                                  collect name)))
               (put-number box (min 1024 (length names)) 2)
               (loop for name in names repeat 1024 do (put-text box name))))
      ((:close :focus)))
    (end-message box at)))

(defun take-event (inbox frame)
  "Read an event from INBOX, as PUT-EVENT put it, naming FRAME; a paste as
two values, its text's piece and true when more of it follows, the event
then made by the caller once it has all."
  (let ((kind (svref *event-kinds* (take-bounded inbox 1 0 (1- (length *event-kinds*)) "an event kind"))))
    (multiple-value-prog1
        (ecase kind
          (:key (let* ((flags (take-bounded inbox 1 0 31 "a key's flags"))
                       (base (if (logtest 8 flags)
                                 (svref *special-keys* (take-bounded inbox 1 0 (1- (length *special-keys*)) "a key"))
                                 (let ((code (take-bounded inbox 4 0 (1- char-code-limit) "a character")))
                                   (or (code-char code) (malformed "no character ~d" code)))))
                       (column (and (logtest 16 flags) (take-number inbox 2)))
                       (row (and (logtest 16 flags) (take-number inbox 2))))
                  (make-event :key :key (make-key base :ctrl (logtest 1 flags) :meta (logtest 2 flags)
                                                       :shift (logtest 4 flags) :column column :row row)
                                   :frame frame)))
          (:resize (make-event :resize :width (take-bounded inbox 2 1 +most-columns+ "a width")
                                       :height (take-bounded inbox 2 1 +most-rows+ "a height")
                                       :frame frame))
          (:paste (let ((more (= 1 (take-bounded inbox 1 0 1 "a paste's flag"))))
                    (values (take-text inbox +text-piece+) more)))
          (:drop (let* ((column (take-number inbox 2)) (row (take-number inbox 2))
                        (files (loop repeat (take-count inbox 2 1024 4) collect (take-text inbox 4096))))
                   (unless (every #'dropped-name-p files)
                     (malformed "a file dropped whose name is empty or holds a control character"))
                   (make-event :drop :files files :column column :row row :frame frame)))
          ((:close :focus) (make-event kind :frame frame)))
      (message-read inbox))))

;;; Answers, from a client

(defparameter *questions* #(:clipboard :primary :font :font-steps :lend)
  "What the editor asks a client, by their numbers in a message: the text of
its clipboard or primary selection, the font it chose given a name, the font
steps it took given a number, and whether it gave its terminal back to be
lent to a program, words to show there given (1 when it did).")

(defun put-answer (box request answer &key more)
  "Put the answer to request number REQUEST in BOX: ANSWER, NIL, a text (a
piece of it, MORE true when more follows) or an integer."
  (let ((at (begin-message box :answer)))
    (put-number box request 4)
    (etypecase answer
      (null (put-number box 0 1))
      (string (put-number box 1 1) (put-number box (if more 1 0) 1) (put-text box answer +text-piece+))
      (integer (put-number box 2 1) (put-number box (max -64 (min 64 answer)) 2)))
    (end-message box at)))

(defun take-answer (inbox)
  "Read an answer from INBOX, as PUT-ANSWER put it: its request's number, the
answer, and true when it is a piece of text with more to follow."
  (let ((request (take-number inbox 4)))
    (multiple-value-prog1
        (ecase (take-bounded inbox 1 0 2 "an answer's kind")
          (0 (values request nil nil))
          (1 (let ((more (= 1 (take-bounded inbox 1 0 1 "an answer's flag"))))
               (values request (take-text inbox +text-piece+) more)))
          (2 (values request (take-signed inbox 2) nil)))
      (message-read inbox))))

;;; Face names, numbered for each connection

(define-record (face-names (:constructor make-face-names ()) (:copier nil))
  "The numbers the messages of one connection give face names by: TABLE from
each name to its number, from 1 in the order the names were first given one
\(0 is no face); FRESH, the names numbered since they were last sent, newest
first."
  (table (make-hash-table :test 'eq) :read-only t :documentation "The number of each face name of NAMES, by name.")
  (count 0 :type fixnum :documentation "How many face names NAMES has numbered.")
  (fresh '() :type list :documentation "The face names NAMES numbered since last sent, newest first."))

(setf (documentation 'make-face-names 'function) "FACE-NAMES with no name numbered yet."
      (documentation 'face-names-p 'function) "True when OBJECT is FACE-NAMES.")

(defun face-name-number (names name)
  "The number NAME, a face name, has in NAMES, given one when it has none; 0
for NIL, and for any name past the +MOST-NAMES+ first."
  (cond ((null name) 0)
        ((gethash name (face-names-table names)))
        ((not (and (symbolp name) (<= (tuatara.text:string-octet-length (symbol-name name)) 128))) 0)
        ((< (face-names-count names) +most-names+)
         (push name (face-names-fresh names))
         (setf (gethash name (face-names-table names)) (incf (face-names-count names))))
        (t 0)))

(defun put-names (box names)
  "Put a message in BOX naming the face names NAMES numbered since it last
did, if any."
  (when (face-names-fresh names)
    (let ((at (begin-message box :names)) (fresh (reverse (face-names-fresh names))))
      (put-number box (length fresh) 2)
      (dolist (name fresh)
        (put-number box (gethash name (face-names-table names)) 2)
        (put-text box (symbol-name name) 128))
      (end-message box at)
      (setf (face-names-fresh names) '()))))

(sb-ext:defglobal **face-names-made** (list 0)
  "How many face names from the editors at the other end of connections this
process has made keywords of, which last as long as it does, in a list.")

(defun received-face-name (name)
  "The face name NAME, as the editor at the other end gave it, stands for here:
the keyword of that name there is, or a new one while this process has made
fewer than +MOST-NAMES+ of them; else NAME itself, a string, which no face
has."
  (or (find-symbol name :keyword)
      (and (< (car **face-names-made**) +most-names+)
           (progn (sb-ext:atomic-incf (car **face-names-made**))
                  (intern name :keyword)))
      name))

(defun take-names (inbox names)
  "Read the face names of a message from INBOX into NAMES, a vector of them
by number; each number is given a name once."
  (loop repeat (take-count inbox 2 +most-names+ 6)
        do (let ((number (take-bounded inbox 2 1 +most-names+ "a face name's number"))
                 (name (take-text inbox 128)))
             (when (svref names number) (malformed "the face name ~d given twice" number))
             (setf (svref names number) (received-face-name name))))
  (message-read inbox))

(defun put-face-spec (box names spec)
  "Put the face spec SPEC, a face name, a list of them or NIL, in BOX, its
names as their NAMES numbers."
  (cond ((null spec) (put-number box 0 1))
        ((atom spec) (put-number box 1 1) (put-number box (face-name-number names spec) 2))
        (t (let ((count (min 255 (length spec))))
             (put-number box 2 1)
             (put-number box count 1)
             (loop for name in spec repeat count do (put-number box (face-name-number names name) 2))))))

(defun take-face-name (inbox names)
  "A face name read from INBOX by its number in NAMES, a vector of names."
  (let ((number (take-bounded inbox 2 0 +most-names+ "a face name's number")))
    (if (zerop number)
        nil
        (or (svref names number) (malformed "the face name ~d, never given" number)))))

(defun take-face-spec (inbox names)
  "A face spec read from INBOX, as PUT-FACE-SPEC put it."
  (ecase (take-bounded inbox 1 0 2 "a face spec")
    (0 nil)
    (1 (take-face-name inbox names))
    (2 (loop repeat (take-count inbox 1 255 2) collect (take-face-name inbox names)))))

;;; Colours, proportions and spans

(defun put-colour (box colour)
  "Put COLOUR, as faces take colours, in BOX; anything else as none."
  (let ((basic (and (keywordp colour) (position colour tuatara.display:*basic-colour-names*))))
    (cond (basic (put-number box 1 1) (put-number box basic 1))
          ((and (integerp colour) (<= 0 colour 255)) (put-number box 2 1) (put-number box colour 1))
          ((and (stringp colour) (<= (length colour) 64)) (put-number box 3 1) (put-text box colour))
          (t (put-number box 0 1)))))

(defun take-colour (inbox)
  "A colour read from INBOX, as PUT-COLOUR put it."
  (ecase (take-bounded inbox 1 0 3 "a colour")
    (0 nil)
    (1 (svref tuatara.display:*basic-colour-names*
              (take-bounded inbox 1 0 (1- (length tuatara.display:*basic-colour-names*)) "a colour")))
    (2 (take-number inbox 1))
    (3 (take-text inbox 256))))

(defun put-proportion (box value most)
  "Put VALUE, a rational from 0 to MOST whose denominator is at most 65535 (a
scale, a shift in rows, a picture's place or size), in BOX; anything else as 0."
  (let ((value (if (and (rationalp value) (<= 0 value most) (<= (denominator value) 65535)) value 0)))
    (put-number box (numerator value) 4)
    (put-number box (denominator value) 2)))

(defun take-proportion (inbox most)
  "A rational read from INBOX, as PUT-PROPORTION put it, at most MOST."
  (let* ((numerator (take-number inbox 4))
         (denominator (take-bounded inbox 2 1 65535 "a denominator"))
         (value (/ numerator denominator)))
    (unless (<= value most) (malformed "a proportion of ~a, past ~d" value most))
    value))

(defun put-spans (box names spans)
  "Put SPANS, each (START END FACE), in BOX."
  (put-number box (min +most-characters+ (length spans)) 4)
  (loop for (start end face) in spans repeat +most-characters+
        do (put-bounded box start +most-characters+) (put-bounded box end +most-characters+)
           (put-face-spec box names face)))

(defun take-spans (inbox names)
  "Spans read from INBOX, as PUT-SPANS put them."
  (loop repeat (take-count inbox 4 +most-characters+ 9)
        collect (list (take-bounded inbox 4 0 +most-characters+ "a span's start")
                      (take-bounded inbox 4 0 +most-characters+ "a span's end")
                      (take-face-spec inbox names))))

;;; Faces, as the editor has them

(defparameter *underlines* #(nil t :wave :dotted :box)
  "The ways a face is underlined, by their numbers in a message.")

(defun put-face (box names face)
  "Put FACE, a face of TUATARA.DISPLAY, in BOX: its name, colours, flags,
underline, font, height and the face it inherits from, names as their NAMES
numbers."
  (put-number box (face-name-number names (tuatara.display:face-name face)) 2)
  (put-colour box (tuatara.display:face-fg face))
  (put-colour box (tuatara.display:face-bg face))
  (put-number box (logior (if (tuatara.display:face-bold face) 1 0) (if (tuatara.display:face-italic face) 2 0)
                          (if (tuatara.display:face-reverse face) 4 0) (if (tuatara.display:face-dim face) 8 0)
                          (if (tuatara.display:face-extend face) 16 0))
              1)
  (put-number box (or (position (tuatara.display:face-underline face) *underlines*) 1) 1)
  (put-colour box (tuatara.display:face-underline-colour face))
  (let ((font (tuatara.display:face-font face)))
    (put-text box (if (and (stringp font) (<= (tuatara.text:string-octet-length font) 256)) font "")))
  (put-proportion box (or (tuatara.display:face-height face) 0) 3)
  (put-number box (face-name-number names (tuatara.display:face-inherit face)) 2))

(defun take-face (inbox names)
  "A face read from INBOX, as PUT-FACE put it, as a list: its name, then its
attributes as SET-FACE-ATTRIBUTES takes them."
  (let ((name (or (take-face-name inbox names) (malformed "a face with no name")))
        (fg (take-colour inbox)) (bg (take-colour inbox))
        (flags (take-bounded inbox 1 0 31 "a face's flags"))
        (underline (svref *underlines* (take-bounded inbox 1 0 (1- (length *underlines*)) "an underline")))
        (underline-colour (take-colour inbox))
        (font (take-text inbox 256))
        (height (take-proportion inbox 3))
        (inherit (take-face-name inbox names)))
    (list name :fg fg :bg bg :bold (logtest 1 flags) :italic (logtest 2 flags) :reverse (logtest 4 flags)
               :dim (logtest 8 flags) :extend (logtest 16 flags) :underline underline :underline-colour underline-colour
               :font (and (plusp (length font)) font) :height (and (> height 1) (min height 3))
               :inherit (and (symbolp inherit) inherit))))

;;; A frame, painted: what the painter interface was called with between its beginning and its end

(defparameter *cursor-shapes* #(:block :bar :underline :hidden)
  "The cursor's shapes, by their numbers in a message.")

(defun put-row-call (box names row string face spans scales over parts)
  "Put in BOX the painting of ROW (FRONTEND-PUT-ROW with STRING, FACE, SPANS,
SCALES, OVER and PARTS)."
  (put-number box 1 1)
  (put-number box row 2)
  (put-text box string +most-characters+)
  (put-face-spec box names face)
  (put-spans box names spans)
  (put-number box (min 4096 (length scales)) 2)
  (loop for (start end scale shift height) in scales repeat 4096
        do (put-bounded box start +most-columns+) (put-bounded box end +most-columns+)
           (put-proportion box scale 3) (put-proportion box shift +most-rows+) (put-proportion box height 3))
  (put-number box (min 4096 (length over)) 2)
  (loop for (left text over-spans) in over repeat 4096
        do (put-bounded box left +most-columns+) (put-text box text +most-characters+) (put-spans box names over-spans))
  (put-number box (min 4096 (length parts)) 2)
  (loop for part in parts repeat 4096 do (put-bounded box part +most-columns+)))

(defun put-kept-row-call (box row)
  "Put in BOX the painting of ROW as the frame before painted it."
  (put-number box 2 1)
  (put-number box row 2))

(defun put-cursor-call (box row column shape colour)
  "Put in BOX the cursor placed (FRONTEND-SET-CURSOR with ROW, COLUMN, SHAPE and COLOUR)."
  (put-number box 3 1)
  (put-number box (max (- +most-place+) (min +most-place+ row)) 4)
  (put-number box (max (- +most-place+) (min +most-place+ column)) 4)
  (put-number box (or (position shape *cursor-shapes*) 0) 1)
  (put-colour box colour))

(defun put-picture-call (box id row column columns rows offset)
  "Put in BOX the picture numbered ID shown (FRONTEND-PUT-IMAGE with ROW,
COLUMN, COLUMNS, ROWS and OFFSET, each a whole number of cells or, placed
exactly in a window, parts of one); its pixels go in messages of their own
\(PUT-PICTURE, or for a client on another machine PUT-PICTURE-BEGIN)."
  (put-number box 4 1)
  (put-number box id 4)
  (put-proportion box row +most-rows+)
  (put-proportion box column +most-columns+)
  (put-proportion box columns +most-columns+)
  (put-proportion box rows +most-rows+)
  (put-number box (if offset 1 0) 1)
  (when offset
    (put-proportion box (first offset) +most-place+)
    (put-proportion box (second offset) +most-place+)))

(defun take-row-call (inbox names)
  "A row's painting read from INBOX, as PUT-ROW-CALL put it: (ROW STRING
FACE SPANS SCALES OVER PARTS)."
  (list (take-bounded inbox 2 0 (1- +most-rows+) "a row")
        (take-text inbox +most-characters+)
        (take-face-spec inbox names)
        (take-spans inbox names)
        (loop repeat (take-count inbox 2 4096 26)
              collect (list (take-bounded inbox 4 0 +most-columns+ "a column")
                            (take-bounded inbox 4 0 +most-columns+ "a column")
                            (take-proportion inbox 3) (take-proportion inbox +most-rows+) (take-proportion inbox 3)))
        (loop repeat (take-count inbox 2 4096 12)
              collect (list (take-bounded inbox 4 0 +most-columns+ "a column")
                            (take-text inbox +most-characters+) (take-spans inbox names)))
        (loop repeat (take-count inbox 2 4096 4) collect (take-bounded inbox 4 0 +most-columns+ "a column"))))

(defun take-place-signed (inbox)
  "A signed place read from INBOX, as the cursor's row or column, held to
+MOST-PLACE+ either way."
  (let ((value (take-signed inbox 4)))
    (unless (<= (- +most-place+) value +most-place+) (malformed "a place of ~d" value))
    value))

(defun take-frame (inbox names kept pictures)
  "The calls of a frame read from INBOX, in order, each (:ROW ROW STRING FACE
SPANS SCALES OVER PARTS), (:CURSOR ROW COLUMN SHAPE COLOUR) or (:PICTURE
IMAGE ROW COLUMN COLUMNS ROWS OFFSET). KEPT, a vector of the rows painted
last by row, is brought up to date and gives each row painted as before;
PICTURES, a list of the pictures sent, gives each picture by its number."
  (loop until (= (inbox-at inbox) (inbox-stop inbox))
        collect (ecase (take-bounded inbox 1 1 4 "a call")
                  (1 (let ((call (take-row-call inbox names)))
                       (setf (svref kept (first call)) call)
                       (cons :row call)))
                  (2 (let ((row (take-bounded inbox 2 0 (1- +most-rows+) "a row")))
                       (cons :row (or (svref kept row) (malformed "row ~d painted as before, never painted" row)))))
                  (3 (list :cursor (take-place-signed inbox) (take-place-signed inbox)
                           (svref *cursor-shapes* (take-bounded inbox 1 0 (1- (length *cursor-shapes*)) "a shape"))
                           (take-colour inbox)))
                  (4 (let* ((id (take-number inbox 4))
                            (image (or (find id pictures :key #'image-id) (malformed "picture ~d, never sent" id)))
                            (row (take-proportion inbox +most-rows+))
                            (column (take-proportion inbox +most-columns+))
                            (columns (take-proportion inbox +most-columns+))
                            (rows (take-proportion inbox +most-rows+))
                            (offset (and (= 1 (take-bounded inbox 1 0 1 "an offset's flag"))
                                         (list (take-proportion inbox +most-place+)
                                               (take-proportion inbox +most-place+)))))
                       (list :picture image row column columns rows offset))))))

;;; Pictures

(defun put-picture (box image let-go)
  "Put a message in BOX sending IMAGE: its number, size and channels, its
pixels passed as a sealed file in memory with it, and the number of the
picture the client lets go of to keep it, LET-GO, or NIL for none."
  (let* ((pixels (image-pixels image))
         (octets (if (typep pixels 'tuatara.text:octets) pixels (coerce pixels 'tuatara.text:octets)))
         (fd (tuatara.io:memory-file "tuatara picture" octets :kept nil))
         (at (begin-message box :picture)))
    (push (cons at fd) (outbox-descriptors box))
    (put-number box (image-id image) 4)
    (put-number box (image-width image) 4)
    (put-number box (image-height image) 4)
    (put-number box (image-channels image) 1)
    (put-number box (if let-go 1 0) 1)
    (put-number box (or let-go 0) 4)
    (end-message box at)))

(defun take-picture (inbox)
  "A picture read from INBOX, as PUT-PICTURE put it, its pixels from the
first descriptor passed, and as a second value the number of the picture to
let go of, or NIL; malformed unless that file is sealed and holds the pixels
its size says, no more and no less."
  (let* ((id (take-number inbox 4))
         (width (take-bounded inbox 4 1 65535 "a picture's width"))
         (height (take-bounded inbox 4 1 65535 "a picture's height"))
         (channels (take-bounded inbox 1 3 4 "a picture's channels"))
         (letting-go (= 1 (take-bounded inbox 1 0 1 "a flag")))
         (let-go (let ((number (take-number inbox 4))) (and letting-go number)))
         (count (* width height channels)))
    (message-read inbox)
    (unless (<= count +most-picture-octets+) (malformed "a picture of ~d octets" count))
    (let ((fd (or (pop (inbox-descriptors inbox)) (malformed "a picture with no pixels passed"))))
      (unwind-protect
           (let ((size (tuatara.io:sealed-file-size fd)))
             (unless (eql size count) (malformed "a picture's pixels, ~a octets for ~d" size count))
             (let ((pixels (tuatara.text:make-octets count)))
               (unless (= count (tuatara.io:read-fd-at fd pixels 0)) (malformed "a picture's pixels cut short"))
               (values (%make-image width height channels pixels id) let-go)))
        (close-descriptors (list fd))))))

;;; Pictures for a client on another machine: a descriptor does not cross ssh, so the pixels go as bytes.
;;; The pixels are packed by DEFLATE; what a client unpacks is only ever rows of pixels,
;;; no picture format and nothing that is drawn by a program.

(defun rows-against-above (pixels stride &key back)
  "PIXELS, a picture's rows of STRIDE octets, in a vector of their own, each
octet less the octet above it (the first row as it is), which packs much
smaller; with BACK, changed in place from that again to the pixels."
  (declare (type tuatara.text:octets pixels) (type (and fixnum (integer 1)) stride))
  (let* ((length (length pixels))
         (out (if back pixels (tuatara.text:make-octets length))))
    (declare (type tuatara.text:octets out) (type fixnum length))
    (if back
        (loop for at of-type fixnum from stride below length
              do (setf (aref out at) (logand 255 (+ (aref out at) (aref out (- at stride))))))
        (progn
          (replace out pixels :end2 (min stride length))
          (loop for at of-type fixnum from stride below length
                do (setf (aref out at) (logand 255 (- (aref pixels at) (aref pixels (- at stride))))))))
    out))

(defun packed-picture (image)
  "The octets IMAGE is sent as to a client on another machine: its rows
against those above (ROWS-AGAINST-ABOVE), deflated."
  (let ((pixels (image-pixels image)))
    (tuatara.io:deflate (rows-against-above (if (typep pixels 'tuatara.text:octets)
                                                pixels
                                                (coerce pixels 'tuatara.text:octets))
                                            (* (image-width image) (image-channels image))))))

(defconstant +first-picture-room+ 65536
  "How many octets of room a picture coming from another machine is given at
first: the room grows only as its pieces come.")

(define-record (arriving-picture (:constructor make-arriving-picture
                                    (id width height channels total
                                     &aux (octets (tuatara.text:make-octets (min total +first-picture-room+)))))
                                (:copier nil))
  "A picture coming to a client from an editor on another machine: its ID,
size and CHANNELS, the TOTAL of its packed octets, and OCTETS, the first
FILLED of them come so far, in order. Once all have come it is unpacked by
THREAD, unless CANCELLED, told to forget it meanwhile."
  (id 0 :type (unsigned-byte 32) :read-only t :documentation "The number the picture arriving is sent by.")
  (width 0 :type fixnum :read-only t :documentation "The picture arriving's width in pixels.")
  (height 0 :type fixnum :read-only t :documentation "The picture arriving's height in pixels.")
  (channels 0 :type fixnum :read-only t :documentation "The octets of each pixel of the picture arriving.")
  (total 0 :type fixnum :read-only t :documentation "How many packed octets the picture arriving goes in.")
  (octets nil :type tuatara.text:octets :documentation "The packed octets of the picture arriving
come so far, and room.")
  (filled 0 :type fixnum :documentation "How many of the picture arriving's packed octets have come.")
  (cancelled nil :documentation "True when the client was told to forget
the picture unpacking.")
  (thread nil :documentation "The thread unpacking the picture arriving, once it is whole."))

(setf (documentation 'make-arriving-picture 'function)
      "An ARRIVING-PICTURE of TOTAL packed octets, given room for the first of them.")

(defun put-picture-begin (box id width height channels total)
  "Begin sending in BOX the picture numbered ID, WIDTH by HEIGHT pixels of
CHANNELS octets each, packed in TOTAL octets to follow (PUT-PICTURE-PIECE-OF)."
  (let ((at (begin-message box :picture-begin)))
    (put-number box id 4)
    (put-number box width 4)
    (put-number box height 4)
    (put-number box channels 1)
    (put-number box total 4)
    (end-message box at)))

(defun take-picture-begin (inbox)
  "An ARRIVING-PICTURE read from INBOX, as PUT-PICTURE-BEGIN put it, held to
the size and octets a client takes from another machine before any room is
made for it. Malformed when past them."
  (let* ((id (take-number inbox 4))
         (width (take-bounded inbox 4 1 +most-far-picture-side+ "a picture's width"))
         (height (take-bounded inbox 4 1 +most-far-picture-side+ "a picture's height"))
         (channels (take-bounded inbox 1 3 4 "a picture's channels"))
         (count (* width height channels))
         ;; packed octets may pass the pixels a little, a block's words stored for what does not pack
         (total (take-bounded inbox 4 1 (+ count (ash count -12) 4096) "a picture's packed octets")))
    (message-read inbox)
    (unless (<= count +most-far-picture-octets+) (malformed "a picture of ~d octets" count))
    (make-arriving-picture id width height channels total)))

(defun put-picture-piece-of (box id octets start end)
  "Put in BOX the fields of the next octets of the picture numbered ID: OCTETS
from START below END, where START is how many were sent before."
  (put-number box id 4)
  (put-number box start 4)
  (put-octets box octets start end))

(defun take-picture-piece (inbox arriving)
  "Read the next octets of the picture ARRIVING from INBOX, as
PUT-PICTURE-PIECE-OF put them, in order and within its octets; true when they
were the last. Malformed for a piece of any other picture or place."
  (let ((id (take-number inbox 4)) (start (take-number inbox 4)))
    (unless (= id (arriving-picture-id arriving)) (malformed "a piece of picture ~d, while another comes" id))
    (unless (= start (arriving-picture-filled arriving)) (malformed "a piece of a picture from octet ~d" start))
    (let* ((count (- (inbox-stop inbox) (inbox-at inbox)))
           (total (arriving-picture-total arriving))
           (end (+ start count)))
      (unless (and (plusp count) (<= end total)) (malformed "a piece of ~d octets, past its picture" count))
      (when (> end (length (arriving-picture-octets arriving)))
        (setf (arriving-picture-octets arriving)
              (replace (tuatara.text:make-octets (min total
                                                      (max end (* 2 (length (arriving-picture-octets arriving))))))
                       (arriving-picture-octets arriving) :end2 start)))
      (replace (arriving-picture-octets arriving) (inbox-octets inbox)
               :start1 start :start2 (take-place inbox count) :end2 (inbox-stop inbox))
      (setf (arriving-picture-filled arriving) end)
      (= end total))))

(defun arrived-picture (arriving)
  "The picture ARRIVING, whole, as the IMAGE it stands for: unpacked to
exactly the pixels its size says and no more. Malformed when it does not
unpack to that."
  (let* ((stride (* (arriving-picture-width arriving) (arriving-picture-channels arriving)))
         (count (* stride (arriving-picture-height arriving)))
         (pixels (handler-case (tuatara.io:inflate (arriving-picture-octets arriving)
                                                   :end (arriving-picture-filled arriving) :limit count :size count)
                   (tuatara.io:zip-error (condition) (malformed "a picture that does not unpack: ~a" condition)))))
    (unless (= (length pixels) count) (malformed "a picture of ~d octets, for ~d" (length pixels) count))
    (%make-image (arriving-picture-width arriving) (arriving-picture-height arriving)
                 (arriving-picture-channels arriving)
                 (rows-against-above pixels stride :back t) (arriving-picture-id arriving))))
