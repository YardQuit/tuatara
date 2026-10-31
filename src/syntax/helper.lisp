;;;; src/syntax/helper.lisp --- Speaking to the program that parses, shut in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Grammars are not run in the editor: tuatara-syntax-helper, built beside
;;;; them (syntax/helper.c), loads them and holds the syntax trees, shut in as
;;;; programs decoding files are (src/io/sandbox.lisp), and the editor asks it
;;;; over a local socket for what it wants to know of a tree. One such helper
;;;; parses the files of projects you trust; each other file or buffer has one
;;;; of its own, so that its text, whatever it sets going in a grammar,
;;;; reaches nothing of any other; one more loads the grammars and compiles
;;;; their rules, and is never given text. Without the sandbox none is started
;;;; for text outside the projects you trust. A helper that ends, or does not
;;;; answer in time, is ended and its trees are gone; the next that is needed
;;;; is started afresh.
;;;;
;;;; A call is made of messages as syntax/helper.c says, over a connection of
;;;; its own (CALL), with foreign memory for what is sent and what comes back,
;;;; kept from one call to the next. The editor's end of each connection never
;;;; waits: the whole of a call, the request written, the text its parse asks
;;;; for given, and the reply read, has one deadline, past which the helper is
;;;; ended. A parse asks for its text over its own connection, while it runs;
;;;; anything else it asks ends it, as does a reply that cannot be right.

(in-package #:tuatara.syntax)

(defvar *grammar-directories* '()
  "Directories searched in order for the program that parses and the
grammars; when empty, grammars/ beside the program, then the source tree's
build/grammars/.")

(defun user-grammar-directory ()
  "The folder of the grammars you add yourself: tuatara/grammars/ in your data
folder ($XDG_DATA_HOME, ~/.local/share/ unless set), each NAME.so with its
rules in rules/, indents/, textobjects/, injections/, locals/ and tags/ beside
it, as Tuatara's own syntax/ has them. Grammars are never loaded from it:
turned on, one is copied into USER-GRAMMAR-STORE."
  (uiop:xdg-data-home "tuatara/grammars/"))

(defun user-grammar-store ()
  "The folder the grammars you added and turned on are kept in, as they were
turned on: grammar-store/ in Tuatara's state folder, made if needed, for you
alone. The program running grammars loads them from here."
  (let ((folder (merge-pathnames "grammar-store/" (tuatara.io:state-directory))))
    (unless (probe-file folder)
      (ignore-errors (sb-posix:mkdir (string-right-trim "/" (uiop:native-namestring folder)) #o700)))
    folder))

(defun own-grammar-directories ()
  "The directories of Tuatara's own grammars and the program that runs them:
*GRAMMAR-DIRECTORIES* when set, else grammars/ beside the program, then the
source tree's build/grammars/."
  (or *grammar-directories* (tuatara.runtime:resource-directories "grammars/" "build/grammars/")))

(defun grammar-directories ()
  "The directories searched, in order, for the program that parses and the
grammars: Tuatara's own (OWN-GRAMMAR-DIRECTORIES), then the copies of the
grammars you added and turned on (USER-GRAMMAR-STORE), never taken in place
of one of Tuatara's."
  (let ((own (own-grammar-directories))
        (store (user-grammar-store)))
    (if (member store own :test #'equal) own (append own (list store)))))

(defun foreign-string (pointer &optional length)
  "The UTF-8 text of LENGTH octets at the foreign POINTER, as a string; without
LENGTH, the octets up to the first zero."
  (tuatara.text:lenient-string (sap-octets pointer length)))

(defun physical-memory ()
  "The octets of memory the machine has, or 0 when the system does not say."
  (flet ((conf (name) (sb-alien:alien-funcall (sb-alien:extern-alien "sysconf" (function sb-alien:long sb-alien:int)) name)))
    (let ((pages (conf 85)) (size (conf 30)))      ; _SC_PHYS_PAGES, _SC_PAGESIZE
      (if (and (plusp pages) (plusp size)) (* pages size) 0))))

(defvar *syntax-helper-memory* (* 4 1024 1024 1024)
  "The octets of memory each program parsing for the editor (its syntax
helper, which runs the grammars) may take, 4 GB unless set; NIL for no limit.
A helper that runs out stops its parses, as a parse over its budget is
stopped (*SYNTAX-MEMORY-LIMIT*).")

(defvar *syntax-helper-wait* 2
  "The seconds a program parsing for the editor (its syntax helper) may take
to answer, over the time a parse may take: one that takes longer, a grammar's
code looping in it, is ended, and another started when it is next needed.")

(defvar *syntax-helper-shared-function* nil
  "A function of a buffer, true when its text may be parsed by the helper
parsing the files of projects you trust; NIL for none: each buffer then has
a helper of its own.")

(defvar *syntax-helper-unconfined* nil
  "True to have the grammars run, where there is no sandbox, for the files of
projects you trust, in a program of their own that is not shut in. Files
outside them are never parsed without the sandbox, whatever this says. NIL
unless set: without the sandbox no text is parsed.")

(defvar *helper-refused-function* nil
  "A function of one argument called, once, when text is not parsed because
there is no sandbox: T when the text is that of a project you trust (see
*SYNTAX-HELPER-UNCONFINED*), NIL for other text; NIL for none.")

(defvar *syntax-helpers-most* 16
  "The most programs parsing for the editor that buffers have of their own at
once: past them, the one used longest ago is ended, its buffer's text parsed
again, in one of its own, when it is next shown or edited.")

(defparameter *syntax-helper-processes* 4096
  "The processes and threads a program parsing for the editor may have: one
for each parser it holds, and its own.")

(defparameter *helper-program* "tuatara-syntax-helper"
  "The name of the program that parses for the editor, beside the grammars.")

(defconstant +top-bit+ #x80000000
  "The bit a request of the helper's own has set in its number.")

;;; Operations, as syntax/helper.c numbers them

(defconstant +op-grammar+ 1) (defconstant +op-parser+ 2) (defconstant +op-parser-delete+ 3)
(defconstant +op-parse+ 4) (defconstant +op-cancel+ 5) (defconstant +op-edit+ 6) (defconstant +op-copy+ 7)
(defconstant +op-tree-delete+ 8) (defconstant +op-query+ 9) (defconstant +op-query-delete+ 10)
(defconstant +op-disable+ 11) (defconstant +op-matches+ 12) (defconstant +op-ancestry+ 13)
(defconstant +op-select+ 14) (defconstant +op-children+ 15) (defconstant +op-has-error+ 16)
(defconstant +op-reset+ 17) (defconstant +op-memory+ 18) (defconstant +op-connect+ 19)
(defconstant +op-parse-ranges+ 20) (defconstant +op-matches-many+ 21) (defconstant +op-children-many+ 22)
(defconstant +op-read+ 64)

;;; A helper

(defstruct (helper (:constructor %make-helper (process fd directories kind)) (:copier nil))
  "A program parsing for the editor: its PROCESS, FD its first connection
(over which the others are handed to it, and parses are stopped), the
grammar DIRECTORIES it was given, and its KIND: :OWN, parsing one buffer's
text, :SHARED, parsing the files of projects you trust, or :RULES, loading
the grammars and compiling their rules, never given any text. FAULTED holds
the names of the grammars that crashed in it, or in the helpers before it
for the same buffer. IDLE holds its connections no call uses now, CONNECTIONS
all it has; WRITE-LOCK is held while the first connection is written to, and
while IDLE and CONNECTIONS change. QUERIES holds the numbers of the queries
it has been given. ALIVE is set until it ends, ENDED says what ended it,
USED when it was last asked something."
  (process nil :read-only t)
  (fd -1 :type fixnum :read-only t)
  (directories '() :read-only t)
  (kind :own :read-only t)
  (faulted '())
  (parsing (list 0))
  (kept nil)
  (buffer nil)
  (idle '())
  (connections '())
  (write-lock (sb-thread:make-mutex :name "syntax helper") :read-only t)
  (queries (make-hash-table :synchronized t) :read-only t)
  (alive t)
  (ended nil)
  (used (get-internal-real-time)))

(setf (documentation 'helper-p 'function) "True when OBJECT is a HELPER."
      (documentation 'helper-process 'function) "The helper's process."
      (documentation 'helper-fd 'function) "The helper's first connection."
      (documentation 'helper-directories 'function) "The grammar directories the helper was given."
      (documentation 'helper-kind 'function) "What the helper is for: :OWN, :SHARED or :RULES."
      (documentation 'helper-parsing 'function) "How many parses run in the helper now, in a cons for atomic counting."
      (documentation 'helper-buffer 'function) "The buffer whose text the helper parses, for one of its own."
      (documentation 'helper-kept 'function) "True while the helper is in use for work that must not lose it (an export), never ended for room."
      (documentation 'helper-faulted 'function) "The names of the grammars that crashed in the helper, or in those before it for its buffer."
      (documentation 'helper-idle 'function) "The helper's connections no call uses now."
      (documentation 'helper-connections 'function) "Every connection to the helper."
      (documentation 'helper-write-lock 'function) "Held while the first connection is written to, and the connections change."
      (documentation 'helper-queries 'function) "The numbers of the queries the helper has been given."
      (documentation 'helper-alive 'function) "True until the helper ends."
      (documentation 'helper-ended 'function) "What ended the helper: :CRASHED, :HUNG, :SLOW (honest work past its time), :BROKEN (a reply that cannot be right) or :STOPPED; NIL while it runs."
      (documentation 'helper-used 'function) "When the helper was last asked something, in internal time units.")

(defun helper-shared (helper)
  "True when HELPER parses the files of projects you trust."
  (eq (helper-kind helper) :shared))

(defmethod print-object ((helper helper) stream)
  (print-unreadable-object (helper stream :type t :identity t)
    (format stream "~(~a~)~:[ ended~;~]" (helper-kind helper) (helper-alive helper))))

(sb-ext:defglobal **helpers** '()
  "Every helper running.")

(sb-ext:defglobal **helpers-lock** (sb-thread:make-mutex :name "syntax helpers")
  "Held while **HELPERS** and the shared helpers are changed.")

(sb-ext:defglobal **kept-helpers** '()
  "The helpers kept for all to use, the one parsing the files of projects you
trust and the one compiling rules, for the list of grammar directories they
load from, as ((KIND . DIRECTORIES) . HELPER).")

;;; Calls: a connection to a helper, used by one thread at a time

(defconstant +read-size+ 65536
  "The most octets a parse is given at a time.")

(defstruct (call (:constructor %make-call (helper fd)) (:copier nil))
  "A connection to HELPER over the socket FD, used by one call at a time:
OUT, foreign memory the request is written in (its header first), OUT-SIZE
octets of it used of OUT-ROOM; IN, where the reply's body is put, IN-SIZE
octets of IN-ROOM; HEADER, where a message's header is read; TEXT, where the
text a parse reads is given from; STATUS what the reply's header said, :DEAD
when the helper ended first."
  (helper nil :read-only t)
  (fd -1 :type fixnum :read-only t)
  (out (null-pointer) :type sb-sys:system-area-pointer)
  (out-size 0 :type fixnum)
  (out-room 0 :type fixnum)
  (in (null-pointer) :type sb-sys:system-area-pointer)
  (in-size 0 :type fixnum)
  (in-room 0 :type fixnum)
  (header (null-pointer) :type sb-sys:system-area-pointer)
  (text (null-pointer) :type sb-sys:system-area-pointer)
  (status nil))

(setf (documentation 'call-p 'function) "True when OBJECT is a CALL."
      (documentation 'call-helper 'function) "The helper the connection is to."
      (documentation 'call-fd 'function) "The connection's socket."
      (documentation 'call-out 'function) "Foreign memory the request is written in."
      (documentation 'call-out-size 'function) "The octets of the request written."
      (documentation 'call-out-room 'function) "The octets there is room for in the request."
      (documentation 'call-in 'function) "Foreign memory the reply's body is put in."
      (documentation 'call-in-size 'function) "The octets of the reply's body."
      (documentation 'call-in-room 'function) "The octets there is room for in the reply."
      (documentation 'call-header 'function) "Foreign memory a message's header is read into."
      (documentation 'call-text 'function) "Foreign memory the text a parse reads is given through."
      (documentation 'call-status 'function) "How the request went: its reply's third number, or :DEAD.")

(defun foreign-grow (sap room wanted)
  "Foreign memory of at least WANTED octets holding what SAP's ROOM octets
held (SAP freed), and its room, as two values."
  (let* ((new (max wanted (* 2 room) 4096))
         (got (sb-alien:alien-funcall
               (sb-alien:extern-alien "realloc" (function sb-alien:unsigned-long sb-alien:unsigned-long
                                                          sb-alien:unsigned-long))
               (sb-sys:sap-int sap) new)))
    (when (zerop got)
      (error 'storage-condition))
    (values (sb-sys:int-sap got) new)))

(defun foreign-free (sap)
  "Free the foreign memory at SAP, from FOREIGN-GROW."
  (sb-alien:alien-funcall (sb-alien:extern-alien "free" (function sb-alien:void sb-alien:unsigned-long))
                          (sb-sys:sap-int sap)))

(defun poll-fd (fd events milliseconds)
  "Wait up to MILLISECONDS (a second at most) for FD to have any of EVENTS
\(POLLIN 1, POLLOUT 4); the events it has, 0 when none came."
  (declare (type fixnum milliseconds))
  (sb-alien:with-alien ((poll (array (sb-alien:unsigned 8) 8)))
    (let ((sap (sb-alien:alien-sap poll)))
      (setf (sb-sys:signed-sap-ref-32 sap 0) fd
            (sb-sys:sap-ref-16 sap 4) events
            (sb-sys:sap-ref-16 sap 6) 0)
      (if (plusp (sb-alien:alien-funcall
                  (sb-alien:extern-alien "poll" (function sb-alien:int sb-sys:system-area-pointer sb-alien:unsigned-long
                                                          sb-alien:int))
                  sap 1 (max 0 (min 1000 milliseconds))))
          (sb-sys:sap-ref-16 sap 6)
          0))))

(defun milliseconds-to (deadline)
  "The milliseconds until DEADLINE (an internal real time), 0 once it has passed."
  (max 0 (ceiling (- deadline (get-internal-real-time)) (floor internal-time-units-per-second 1000))))

(defun transfer (fd sap size direction deadline)
  "Read (DIRECTION :READ) or write (:WRITE) SIZE octets at SAP over FD by
DEADLINE, an internal real time (NIL for none): T when done, :ENDED when FD
ended or failed first, :LATE when the deadline came first. FD may be one
that does not wait (O_NONBLOCK)."
  (let ((done 0))
    (loop while (< done size)
          do (let ((moved (if (eq direction :read)
                              (sb-alien:alien-funcall
                               (sb-alien:extern-alien "read" (function sb-alien:long sb-alien:int sb-sys:system-area-pointer
                                                                       sb-alien:unsigned-long))
                               fd (sb-sys:sap+ sap done) (- size done))
                              (sb-alien:alien-funcall
                               (sb-alien:extern-alien "send" (function sb-alien:long sb-alien:int sb-sys:system-area-pointer
                                                                       sb-alien:unsigned-long sb-alien:int))
                               fd (sb-sys:sap+ sap done) (- size done) 16384)))) ; MSG_NOSIGNAL
               (cond ((plusp moved) (incf done moved))
                     ((zerop moved) (return-from transfer :ended))
                     (t (let ((errno (sb-alien:get-errno)))
                          (cond ((= errno sb-posix:eintr))
                                ((= errno sb-posix:eagain)
                                 (when (and deadline (tuatara.runtime:deadline-passed-p deadline))
                                   (return-from transfer :late))
                                 (poll-fd fd (if (eq direction :read) 1 4)
                                          (if deadline (milliseconds-to deadline) 1000)))
                                (t (return-from transfer :ended))))))))
    t))

(defun send-descriptor (fd descriptor)
  "Ask the helper over its first connection FD to take the socket DESCRIPTOR
as a new connection (+OP-CONNECT+); NIL when it cannot be sent."
  (sb-alien:with-alien ((header (array (sb-alien:unsigned 32) 3))
                        (part (array (sb-alien:unsigned 64) 2))
                        (control (array (sb-alien:unsigned 8) 24))
                        (message (array (sb-alien:unsigned 8) 56)))
    (let ((h (sb-alien:alien-sap header)) (p (sb-alien:alien-sap part))
          (c (sb-alien:alien-sap control)) (m (sb-alien:alien-sap message)))
      (setf (sb-sys:sap-ref-32 h 0) 0 (sb-sys:sap-ref-32 h 4) 0 (sb-sys:sap-ref-32 h 8) +op-connect+)
      ;; struct iovec, struct cmsghdr with SCM_RIGHTS, struct msghdr (x86-64 and arm64 lay them out alike)
      (setf (sb-sys:sap-ref-64 p 0) (sb-sys:sap-int h) (sb-sys:sap-ref-64 p 8) 12)
      (setf (sb-sys:sap-ref-64 c 0) 20 (sb-sys:sap-ref-32 c 8) 1 (sb-sys:sap-ref-32 c 12) 1
            (sb-sys:signed-sap-ref-32 c 16) descriptor (sb-sys:sap-ref-32 c 20) 0)
      (dotimes (i 56) (setf (sb-sys:sap-ref-8 m i) 0))
      (setf (sb-sys:sap-ref-64 m 16) (sb-sys:sap-int p) (sb-sys:sap-ref-64 m 24) 1
            (sb-sys:sap-ref-64 m 32) (sb-sys:sap-int c) (sb-sys:sap-ref-64 m 40) 24)
      (= 12 (sb-alien:alien-funcall
             (sb-alien:extern-alien "sendmsg" (function sb-alien:long sb-alien:int sb-sys:system-area-pointer sb-alien:int))
             fd m 16384)))))                            ; MSG_NOSIGNAL

(defun take-call (helper)
  "A connection to HELPER no other call uses, made when none is idle; NIL
when HELPER has ended or no connection can be made."
  (or (sb-thread:with-mutex ((helper-write-lock helper))
        (and (helper-alive helper) (pop (helper-idle helper))))
      (and (helper-alive helper)
           (multiple-value-bind (mine theirs) (ignore-errors (socket-pair))
             (when mine
               (let ((sent (sb-thread:with-mutex ((helper-write-lock helper))
                             (and (helper-alive helper) (send-descriptor (helper-fd helper) theirs)))))
                 (sb-posix:close theirs)
                 (if (not sent)
                     (progn (sb-posix:close mine) nil)
                     (make-call helper mine))))))))

(defun make-call (helper fd)
  "A connection to HELPER over the socket FD, one of its connections."
  (let ((call (%make-call helper fd)))
    (multiple-value-bind (out room) (foreign-grow (null-pointer) 0 4096)
      (setf (call-out call) out (call-out-room call) room))
    (multiple-value-bind (in room) (foreign-grow (null-pointer) 0 4096)
      (setf (call-in call) in (call-in-room call) room))
    (setf (call-header call) (foreign-grow (null-pointer) 0 16)
          (call-text call) (foreign-grow (null-pointer) 0 (+ 12 +read-size+)))
    (sb-thread:with-mutex ((helper-write-lock helper))
      (push call (helper-connections helper)))
    call))

(defun close-call (call)
  "Close the connection CALL and let go of its memory."
  (ignore-errors (sb-posix:close (call-fd call)))
  (dolist (sap (list (call-out call) (call-in call) (call-header call) (call-text call)))
    (foreign-free sap)))

(defconstant +kept-room+ (* 1024 1024)
  "The most octets of memory a connection keeps for its requests and replies
between calls: more, taken for a large one, is let go.")

(defun give-back-call (call)
  "Keep the connection CALL to be used again, its memory for requests and
replies no more than +KEPT-ROOM+, or close it when its helper has ended."
  (when (> (call-in-room call) +kept-room+)
    (foreign-free (call-in call))
    (multiple-value-bind (in room) (foreign-grow (null-pointer) 0 4096)
      (setf (call-in call) in (call-in-room call) room (call-in-size call) 0)))
  (when (> (call-out-room call) +kept-room+)
    (foreign-free (call-out call))
    (multiple-value-bind (out room) (foreign-grow (null-pointer) 0 4096)
      (setf (call-out call) out (call-out-room call) room (call-out-size call) 0)))
  (let ((helper (call-helper call)))
    (when (sb-thread:with-mutex ((helper-write-lock helper))
            (if (helper-alive helper)
                (progn (push call (helper-idle helper)) nil)
                (progn (setf (helper-connections helper) (remove call (helper-connections helper))) t)))
      (close-call call))))

(defun call-room (call octets)
  "Make room in CALL's request for OCTETS more."
  (when (> (+ (call-out-size call) octets) (call-out-room call))
    (multiple-value-bind (out room) (foreign-grow (call-out call) (call-out-room call) (+ (call-out-size call) octets))
      (setf (call-out call) out (call-out-room call) room))))

(defun call-begin (call op)
  "Start in CALL a request asking OP."
  (setf (call-out-size call) 12
        (sb-sys:sap-ref-32 (call-out call) 8) op)
  call)

(defun call-u32 (call value)
  "Add the 32-bit number VALUE to CALL's request."
  (call-room call 4)
  (setf (sb-sys:sap-ref-32 (call-out call) (call-out-size call)) (ldb (byte 32 0) value))
  (incf (call-out-size call) 4)
  call)

(defun call-u64 (call value)
  "Add the 64-bit number VALUE to CALL's request."
  (call-room call 8)
  (setf (sb-sys:sap-ref-64 (call-out call) (call-out-size call)) (ldb (byte 64 0) value))
  (incf (call-out-size call) 8)
  call)

(defun call-octets (call octets &key pad)
  "Add OCTETS (an octet vector) to CALL's request, with PAD, made up to a
multiple of four octets."
  (let* ((n (length octets)) (total (if pad (* 4 (ceiling n 4)) n)))
    (call-room call total)
    (sb-sys:with-pinned-objects (octets)
      (sb-kernel:system-area-ub8-copy (sb-sys:vector-sap octets) 0 (call-out call) (call-out-size call) n))
    (loop for i from n below total do (setf (sb-sys:sap-ref-8 (call-out call) (+ (call-out-size call) i)) 0))
    (incf (call-out-size call) total)
    call))

(defun reply-overrun (call)
  "End the helper of CALL, whose reply cannot be right, and leave the
WITH-CALL that CALL belongs to, which gives NIL."
  (end-helper (call-helper call) :broken)
  (throw 'reply-malformed nil))

(declaim (inline reply-has))
(defun reply-has (call at size)
  "Check that CALL's reply holds SIZE octets from octet AT (REPLY-OVERRUN)."
  (unless (and (<= 0 at) (<= 0 size) (<= (+ at size) (call-in-size call)))
    (reply-overrun call)))

(defun reply-u32 (call i)
  "The I-th 32-bit number of CALL's reply."
  (reply-has call (* 4 i) 4)
  (sb-sys:sap-ref-32 (call-in call) (* 4 i)))

(defun reply-u32-at (call at)
  "The 32-bit number of CALL's reply at octet AT."
  (reply-has call at 4)
  (sb-sys:sap-ref-32 (call-in call) at))

(defun reply-i64-at (call at)
  "The signed 64-bit number of CALL's reply at octet AT."
  (reply-has call at 8)
  (sb-sys:signed-sap-ref-64 (call-in call) at))

(defun reply-count (call at each)
  "The 32-bit number of CALL's reply at octet AT, a count of things that take
at least EACH octets each after it: more than the reply could hold ends the
helper (REPLY-OVERRUN)."
  (let ((count (reply-u32-at call at)))
    (reply-has call (+ at 4) (* count each))
    count))

(defun reply-string (call at length)
  "The text of LENGTH octets of CALL's reply from octet AT."
  (reply-has call at length)
  (foreign-string (sb-sys:sap+ (call-in call) at) length))

(defstruct (parse-input (:constructor make-parse-input (rope &optional (end (tuatara.text:rope-bytes rope)) (number 0)
                                                                       (size end)))
                        (:copier nil))
  "The text a parse reads: ROPE, up to octet END (the end of the last range of
it read, for text in another language), by the parser NUMBER (0 for those
parsing text in other languages), SIZE octets of text in all. FAILED is set
when reading it went wrong, and the parse is then thrown away; OVER when it
asked for its text more times over than a parse does (READ-BACK-MOST), and
was given no more of it."
  (rope nil :read-only t)
  (end 0 :type fixnum :read-only t)
  (number 0 :type fixnum :read-only t)
  (size 0 :type fixnum :read-only t)
  (given 0 :type fixnum)
  (failed nil)
  (over nil))

(setf (documentation 'make-parse-input 'function) "A PARSE-INPUT reading ROPE up to END (its end unless given) by the parser NUMBER."
      (documentation 'parse-input-end 'function) "The octet the parse reads no further than."
      (documentation 'parse-input-number 'function) "The number of the parser reading the text."
      (documentation 'parse-input-p 'function) "True when OBJECT is a PARSE-INPUT."
      (documentation 'parse-input-rope 'function) "The rope being parsed."
      (documentation 'parse-input-failed 'function) "True when reading the text went wrong."
      (documentation 'parse-input-given 'function) "The octets of text given to the parse so far."
      (documentation 'parse-input-size 'function) "The octets of text the parse is of, in all."
      (documentation 'parse-input-over 'function) "True when the parse asked for more of its text than it may have.")

(defparameter *read-back-times* 64
  "How many times over a parse may read its text: a grammar reads text again
as it tries one way of reading it and then another, nested brackets most.")

(defparameter *read-back-more* (* 16 1024 1024)
  "The octets a parse may read besides *READ-BACK-TIMES* its text.")

(defun read-back-most (input)
  "The most octets of text a parse of INPUT may be given in all, past which
it is given no more, and given up as too slow: its time is held by its
deadline, this is for a parse that would read without end."
  (+ (* *read-back-times* (parse-input-size input)) *read-back-more*))

(defconstant +reply-most+ (* 256 1024 1024)
  "The most octets a helper's reply may hold.")

(defun give-text (call input id byte deadline)
  "Answer the helper's asking, numbered ID, for the text from octet BYTE on of
INPUT, which the parse asked on CALL reads: up to +READ-SIZE+ octets of it,
none past its end, by DEADLINE; as TRANSFER says."
  (let ((out (call-text call)) (size 0))
    (handler-case
        (let* ((rope (parse-input-rope input))
               (total (min (parse-input-end input) (tuatara.text:rope-bytes rope))))
          (when (and (< byte total) (not (parse-input-over input)))
            (setf size (tuatara.text:rope-copy-to-sap rope byte (min total (+ byte +read-size+)) (sb-sys:sap+ out 12)))))
      (serious-condition ()
        (setf (parse-input-failed input) t size 0)))
    (setf (sb-sys:sap-ref-32 out 0) size (sb-sys:sap-ref-32 out 4) id (sb-sys:sap-ref-32 out 8) 0)
    (incf (parse-input-given input) size)
    (transfer (call-fd call) out (+ 12 size) :write deadline)))

(defun op-reply-most (op)
  "The most octets a reply to a request asking OP may hold."
  (cond ((member op (list +op-has-error+ +op-copy+ +op-parser+)) 4)
        ((= op +op-parse+) 8)
        ((= op +op-select+) 12)
        ;; a grammar's node types, names of a few hundred octets at most
        ((= op +op-grammar+) (* 16 1024 1024))
        ((= op +op-memory+) 24)
        ((= op +op-ancestry+) (* 20 4096))
        ((= op +op-children+) (+ 4 (* 8 4096)))
        ((member op (list +op-parser-delete+ +op-tree-delete+ +op-edit+ +op-reset+ +op-disable+ +op-query-delete+)) 0)
        (t +reply-most+)))

(defun call-send (call &key seconds cancel input most (late :hung))
  "Send CALL's request and wait for its reply: its status (0 when it went
well, a number when not, :DEAD when the helper ended first). The helper is
ended when the whole call, its request, the text its parse asks for and its
reply, takes more than SECONDS (*SYNTAX-HELPER-WAIT* unless given), or its
reply is longer than MOST octets (unless given, the most for what it asked,
OP-REPLY-MOST); ended as LATE when late (:HUNG unless given: :SLOW for work
that may honestly take that long). CANCEL, a function of no arguments, is called now and then
while waiting, to say a parse is to stop; INPUT is the text a parse asked
reads."
  (let* ((helper (call-helper call))
         (fd (call-fd call))
         (sap (call-out call))
         (header (call-header call))
         (deadline (tuatara.runtime:deadline-in (or seconds *syntax-helper-wait*)))
         (most (min +reply-most+ (or most (op-reply-most (sb-sys:sap-ref-32 (call-out call) 8))))))
    (setf (sb-sys:sap-ref-32 sap 0) (- (call-out-size call) 12)
          (sb-sys:sap-ref-32 sap 4) 1
          (helper-used helper) (get-internal-real-time))
    (flet ((dead (why)
             (end-helper helper why)
             (return-from call-send (setf (call-status call) :dead)))
           (moved (result)
             (case result
               (:late (end-helper helper late) (return-from call-send (setf (call-status call) :dead)))
               (:ended (end-helper helper :crashed) (return-from call-send (setf (call-status call) :dead))))))
      (moved (transfer fd sap (call-out-size call) :write deadline))
      (loop
        (cond ((not (helper-alive helper)) (return (setf (call-status call) :dead)))
              ((tuatara.runtime:deadline-passed-p deadline) (dead late))
              ((zerop (poll-fd fd 1 (min 5 (milliseconds-to deadline))))
               (when cancel (funcall cancel)))
              (t
               (moved (transfer fd header 12 :read deadline))
               (let ((size (sb-sys:sap-ref-32 header 0))
                     (id (sb-sys:sap-ref-32 header 4))
                     (status (sb-sys:sap-ref-32 header 8)))
                 (if (logtest id +top-bit+)
                     ;; the parse asked on this connection, while it runs, asks for its
                     ;; text: anything else is not answered, and ends the helper
                     (progn
                       (unless (and input (= size 8) (= status +op-read+)
                                    (= id (logior +top-bit+ (parse-input-number input))))
                         (dead :broken))
                       (moved (transfer fd (call-in call) 8 :read deadline))
                       (let ((number (sb-sys:sap-ref-32 (call-in call) 0))
                             (byte (sb-sys:sap-ref-32 (call-in call) 4)))
                         (unless (and (= number (parse-input-number input))
                                      (<= byte (min (parse-input-end input)
                                                    (tuatara.text:rope-bytes (parse-input-rope input)))))
                           (dead :broken))
                         ;; past what a parse reads: no more text, its end taken as the text's
                         (when (> (parse-input-given input) (read-back-most input))
                           (setf (parse-input-over input) t))
                         (moved (give-text call input id byte deadline))))
                     (progn
                       (unless (and (= id 1) (<= size most)) (dead :broken))
                       (when (> size (call-in-room call))
                         (multiple-value-bind (in room) (foreign-grow (call-in call) (call-in-room call) size)
                           (setf (call-in call) in (call-in-room call) room)))
                       (moved (transfer fd (call-in call) size :read deadline))
                       (setf (call-in-size call) size)
                       (return (setf (call-status call) status)))))))))))

(defmacro with-call ((call helper op) &body body)
  "Run BODY with CALL a connection to HELPER (CALL-SEND) no other call uses,
begun as a request of OP, given back after; nothing when HELPER has ended,
NIL then. A reply that cannot be right (REPLY-OVERRUN) leaves BODY with NIL."
  (let ((h (gensym "HELPER")))
    `(let* ((,h ,helper)
            (,call (and ,h (helper-alive ,h) (take-call ,h))))
       (when ,call
         (catch 'reply-malformed
           (unwind-protect (progn (call-begin ,call ,op) ,@body)
             (give-back-call ,call)))))))

(defun send-cancel (helper parser-number)
  "Have HELPER stop the parse of its parser PARSER-NUMBER, over its first connection."
  (sb-alien:with-alien ((message (array (sb-alien:unsigned 32) 4)))
    (let ((sap (sb-alien:alien-sap message)))
      (setf (sb-sys:sap-ref-32 sap 0) 4 (sb-sys:sap-ref-32 sap 4) 0 (sb-sys:sap-ref-32 sap 8) +op-cancel+
            (sb-sys:sap-ref-32 sap 12) parser-number)
      ;; a helper not reading its first connection is not waited for
      (let ((result (sb-thread:with-mutex ((helper-write-lock helper))
                      (and (helper-alive helper)
                           (transfer (helper-fd helper) sap 16 :write (tuatara.runtime:deadline-in 0.1))))))
        (case result
          (:late (end-helper helper :hung) nil)
          (:ended (end-helper helper :crashed) nil)
          (t result))))))

;;; Starting and ending helpers

(defun socket-pair ()
  "Two ends of a new local socket, as two descriptors, neither left open in
programs started (a program is given its end as its input); the first, the
editor's, does not wait when it cannot be read or written at once."
  (sb-alien:with-alien ((fds (array sb-alien:int 2)))
    (unless (zerop (sb-alien:alien-funcall
                    (sb-alien:extern-alien "socketpair" (function sb-alien:int sb-alien:int sb-alien:int sb-alien:int
                                                                  (* (array sb-alien:int 2))))
                    1 (logior 1 #x80000) 0 (sb-alien:addr fds)))     ; AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC
      (error "No socket for the syntax helper."))
    (let ((mine (sb-alien:deref fds 0)))
      ;; the editor's end never waits: each call has a deadline of its own
      (sb-alien:alien-funcall (sb-alien:extern-alien "fcntl" (function sb-alien:int sb-alien:int sb-alien:int sb-alien:long))
                              mine 4 #x800)                                     ; F_SETFL O_NONBLOCK
      (values mine (sb-alien:deref fds 1)))))

(defun syntax-helper-program (&optional (directories (grammar-directories)))
  "The path of the program that parses for the editor, in the first of
DIRECTORIES that has it, or NIL: without it, no text is parsed."
  (loop for directory in directories
        for path = (probe-file (merge-pathnames *helper-program* directory))
        when path return path))

(sb-ext:defglobal **starter** nil
  "The thread helpers are started from, kept as long as the editor runs: a
helper, and the sandbox it is shut in, end when the thread that started them
ends (PR_SET_PDEATHSIG), so none is started from a thread that may end first.")

(sb-ext:defglobal **starter-lock** (sb-thread:make-mutex :name "syntax helper starting")
  "Held while work is handed to the starting thread, or taken from it.")

(sb-ext:defglobal **starter-wake** (sb-thread:make-waitqueue :name "syntax helper starting")
  "Signalled when the starting thread is given work, and when it has done it.")

(sb-ext:defglobal **starter-work** nil
  "The work handed to the starting thread, (TOKEN . FUNCTION), FUNCTION of no
arguments, until it takes it.")

(sb-ext:defglobal **starter-done** (make-hash-table :test 'eq)
  "What each work given to a starting thread returned, by its token, as
\(RESULT CONDITION), until it is taken; under **STARTER-LOCK**.")

(sb-ext:defglobal **starters** '()
  "Every thread helpers have been started from, the one in use and those
given up on (RETIRED), which take no more work and are left to end their
helpers' lives with the editor's; under **STARTER-LOCK**.")

(sb-ext:defglobal **retired** '()
  "The starting threads given up on, as one that did not answer in time.")

(defun starter-loop ()
  "What the starting thread does: run each function handed to it, keeping what
it returns, or the condition that stopped it, by the work's token, unless
that work was given up on; once itself given up on, nothing more, waiting
to be ended with the rest."
  (loop
    (let ((work (sb-thread:with-mutex (**starter-lock**)
                  (loop until (and **starter-work** (not (member sb-thread:*current-thread* **retired**)))
                        do (sb-thread:condition-wait **starter-wake** **starter-lock**))
                  (shiftf **starter-work** nil))))
      (let ((result (handler-case (list (funcall (cdr work))) (serious-condition (e) (list nil e)))))
        (sb-thread:with-mutex (**starter-lock**)
          (if (eq (car (car work)) :abandoned)
              ;; given up on: a program it started meanwhile is ended, not left behind
              (when (sb-ext:process-p (first result))
                (ignore-errors (sb-ext:process-kill (first result) sb-unix:sigkill))
                (ignore-errors (sb-ext:process-wait (first result)))
                (ignore-errors (sb-ext:process-close (first result))))
              (setf (gethash (car work) **starter-done**) result))
          (sb-thread:condition-broadcast **starter-wake**))))))

(sb-ext:defglobal **starting** (sb-thread:make-mutex :name "syntax helper start")
  "Held by the thread having a helper started, so that one is started at a time.")

(defparameter *starter-wait* 30
  "The seconds a helper's start may take, the sandbox looked for first, before
it is given up and another thread made to start helpers from.")

(defparameter *at-once-wait* 0.05
  "The seconds a start on a thread that must not wait may take, past which
it is left to finish in the background (IN-STARTER :AT-ONCE).")

(defun in-starter (function &key at-once held untaken)
  "What FUNCTION returns, called in the thread helpers are started from (made
the first time, and again when it has died or not done its last work within
*STARTER-WAIT*, that one then retired, its helpers left running), and the
condition that stopped it, if one did, as a second value. FUNCTION owns what
it opens: given up on, it may still run later. AT-ONCE: when another start
is under way, :BUSY at once, FUNCTION not called; and given up on after
*AT-ONCE-WAIT* (:BUSY), the thread not retired, what FUNCTION started then
ended. HELD when the caller holds **STARTING** already. UNTAKEN, a function,
is called when FUNCTION was given up on before it was ever called."
  (unless (or held (sb-thread:grab-mutex **starting** :waitp (not at-once)))
    (when untaken (ignore-errors (funcall untaken)))
    (return-from in-starter :busy))
  (unwind-protect (in-starter-now function (if at-once *at-once-wait* *starter-wait*) (not at-once) untaken)
    (unless held (sb-thread:release-mutex **starting**))))

(defun in-starter-now (function wait retire untaken)
  "The work of IN-STARTER, **STARTING** held: FUNCTION waited for WAIT
seconds; the starting thread RETIRE'd when it did not answer in them."
  (sb-thread:with-mutex (**starter-lock**)
    (unless (and **starter** (sb-thread:thread-alive-p **starter**))
      (setf **starter** (sb-thread:make-thread #'starter-loop :name "tuatara syntax helper starting"))
      (push **starter** **starters**))
    (let ((token (list :work))
          (starter **starter**)
          (deadline (tuatara.runtime:deadline-in wait)))
      (setf **starter-work** (cons token function))
      (sb-thread:condition-broadcast **starter-wake**)
      (loop until (or (nth-value 1 (gethash token **starter-done**))
                      (not (sb-thread:thread-alive-p starter))
                      (tuatara.runtime:deadline-passed-p deadline))
            do (sb-thread:condition-wait **starter-wake** **starter-lock**
                                         :timeout (max 0.005 (min 1 (/ (milliseconds-to deadline) 1000)))))
      (multiple-value-bind (result done) (gethash token **starter-done**)
        (remhash token **starter-done**)
        (cond (done (values-list result))
              (t
               (setf (car token) :abandoned)
               ;; never taken: what it would have closed is closed here
               (when (eq (car **starter-work**) token)
                 (setf **starter-work** nil)
                 (when untaken (ignore-errors (funcall untaken))))
               (cond (retire
                      ;; dead or stuck: retired, the next start made from another
                      (push starter **retired**)
                      (setf **starter** nil)
                      (values nil (make-condition 'simple-error
                                                  :format-control "the thread starting helpers did not answer")))
                     (t :busy))))))))

(defparameter *syntax-helper-descriptors* 256
  "The most files a program parsing for the editor may have open: its
connections, and a grammar's file while it is loaded.")

(sb-ext:defglobal **refused-said** nil
  "True once text has not been parsed for want of the sandbox, and that was said.")

(defun refuse-unconfined (trusted)
  "Say, once, that text is not parsed because there is no sandbox: TRUSTED
when it is that of a project you trust. NIL."
  (unless (shiftf **refused-said** t)
    (tuatara.runtime:debug-report :error "no syntax helper started: there is no sandbox~:[~; (*syntax-helper-unconfined* not set)~]"
                                  trusted)
    (when *helper-refused-function*
      (ignore-errors (funcall *helper-refused-function* trusted))))
  nil)

(defun helpers-refused-p (kind)
  "True when no helper of KIND is to be started, as is known already: there
is no sandbox, and KIND is :OWN, or none runs without one
\(*SYNTAX-HELPER-UNCONFINED*)."
  (and (member (tuatara.io:sandbox-status-now) '(:missing :broken :off))
       (or (eq kind :own) (not *syntax-helper-unconfined*))
       t))

(defun start-helper (directories kind &key at-once held)
  "A new helper of KIND (see HELPER) loading grammars from DIRECTORIES, shut
in by the sandbox; NIL when its program is not found, or when there is no
sandbox: then a helper parsing a file outside the projects you trust is
never started, and another only when *SYNTAX-HELPER-UNCONFINED* says so, a
process of its own not shut in. AT-ONCE, when another start is under way,
or this one takes more than a moment, gives :BUSY (IN-STARTER), which HELD
says the caller holds already."
  (let ((program (syntax-helper-program directories)))
    (when program
      (multiple-value-bind (mine theirs) (socket-pair)
        (let* ((places (mapcar #'uiop:native-namestring (remove-if-not #'uiop:directory-exists-p directories)))
               (memory *syntax-helper-memory*)
               (processes *syntax-helper-processes*)
               (descriptors *syntax-helper-descriptors*)
               (unconfined (and *syntax-helper-unconfined* (not (eq kind :own))))
               (how nil))
          (multiple-value-bind (process error)
              (in-starter
               ;; the helper's end of the socket is this work's own, closed by
               ;; it once given: run late, given up on, it touches nothing else
               (lambda ()
                 (let ((stream (sb-sys:make-fd-stream theirs :input t :output t :buffering :none :auto-close nil)))
                   (unwind-protect
                        (if (eq :ready (tuatara.io:sandbox-status))
                            (progn (setf how :shut-in)
                                   (tuatara.io:sandbox-start (uiop:native-namestring program) places stream
                                                             :memory memory :directories places
                                                             :processes processes :descriptors descriptors))
                            (when unconfined
                              (setf how :unconfined)
                              (sb-ext:run-program (uiop:native-namestring program) places
                                                  :search nil :wait nil :input stream :output nil :error nil
                                                  :environment '())))
                     (sb-posix:close theirs))))
               :at-once at-once :held held
               ;; the work never run: its end of the socket closed here
               :untaken (lambda () (sb-posix:close theirs)))
            (cond ((eq process :busy)
                   (sb-posix:close mine)
                   :busy)
                  (process
                   (let ((helper (%make-helper process mine directories kind)))
                     (sb-thread:with-mutex (**helpers-lock**) (push helper **helpers**))
                     (tuatara.runtime:debug-report :parse "syntax helper (~(~a~)) started, ~:[not shut in: no sandbox, *syntax-helper-unconfined* set~;shut in~]"
                                                   kind (eq how :shut-in))
                     helper))
                  (t
                   (sb-posix:close mine)
                   (cond (error
                          (tuatara.runtime:debug-report :error "syntax helper (~(~a~)) not started: ~a" kind error)
                          nil)
                         ((null how) (refuse-unconfined (not (eq kind :own)))))))))))))

(defvar *helper-ended-function* nil
  "A function of a HELPER called, once, when a helper ends or is ended without
being asked to, to say so; NIL for none. HELPER-PARSING says whether a parse
was running in it, which says so itself.")

(defmacro while-parsing ((helper) &body body)
  "Run BODY, a parse in HELPER, counted in HELPER-PARSING."
  (let ((h (gensym "HELPER")))
    `(let ((,h ,helper))
       (sb-ext:atomic-incf (car (helper-parsing ,h)))
       (unwind-protect (progn ,@body)
         (sb-ext:atomic-decf (car (helper-parsing ,h)))))))

(defvar *parsing-buffer* nil
  "What is being parsed or coloured now, to which what goes wrong is counted:
a buffer, or what an export of a text is known by (TEXT-KEY); NIL for none.")

(defun octets-digest (rope)
  "A 64-bit digest (FNV-1a) of the octets of ROPE, the same for the same text."
  (let ((hash #xcbf29ce484222325)
        (window (make-array 65536 :element-type '(unsigned-byte 8)))
        (bytes (tuatara.text:rope-bytes rope)))
    (declare (type (unsigned-byte 64) hash))
    (loop for from from 0 below bytes by 65536
          for to = (min bytes (+ from 65536))
          do (tuatara.text:rope-copy-into rope from to window)
             (dotimes (i (- to from))
               (setf hash (ldb (byte 64 0) (* (logxor hash (aref window i)) #x100000001b3)))))
    hash))

(defun text-key (&key file text)
  "What a text is known by when what goes wrong in it is counted: the real
path of FILE, every link followed, else a digest of TEXT (a rope or a
string), the same however often it is opened or exported."
  (if file
      (or (ignore-errors (tuatara.io:real-path (tuatara.io:native (tuatara.io:expand-path file)))) file)
      (list :text (octets-digest (if (stringp text) (tuatara.text:string->rope text) text)))))

(sb-ext:defglobal **faults-by-key** (make-hash-table :test 'equal :synchronized t)
  "What went wrong in each text known by a key (TEXT-KEY) and no buffer, as an export's.")

(defun fault-holder (where)
  "Who holds what went wrong for WHERE: a buffer; the buffer a helper of its
own parses; a helper with none; or a text's key (TEXT-KEY)."
  (if (and (helper-p where) (helper-buffer where)) (helper-buffer where) where))

(defun faulted-names (where)
  "The names of the grammars gone wrong for WHERE (FAULT-HOLDER), :ALL among
them once anything went wrong there that no one grammar answers for."
  (let ((holder (fault-holder where)))
    (cond ((helper-p holder) (helper-faulted holder))
          ((typep holder 'tuatara.buffer:buffer) (tuatara.buffer:buffer-local holder :syntax-faulted))
          (holder (gethash holder **faults-by-key**)))))

(defun note-faulted (where name)
  "Add NAME, a grammar's name or :ALL, to what went wrong for WHERE (FAULTED-NAMES)."
  (let ((holder (fault-holder where)))
    (cond ((helper-p holder) (pushnew name (helper-faulted holder) :test #'equal))
          ((typep holder 'tuatara.buffer:buffer)
           (pushnew name (tuatara.buffer:buffer-local holder :syntax-faulted) :test #'equal))
          (holder (sb-ext:with-locked-hash-table (**faults-by-key**)
                    (pushnew name (gethash holder **faults-by-key**) :test #'equal))))))

(defun forget-text-faults (buffer)
  "Forget what went wrong in BUFFER's text, its grammars tried on it again,
as when it is read again (revert-buffer): with the buffer, and with what
its text is known by (TEXT-IDENTITY), as an export of its file has it."
  (setf (tuatara.buffer:buffer-local buffer :syntax-faulted) nil)
  (remhash (text-identity buffer) **faults-by-key**))

(defun text-identity (where)
  "What one text is known by, for counting the texts a grammar went wrong on:
its key (TEXT-KEY), from the file of the buffer WHERE stands for
\(FAULT-HOLDER), or the buffer's text when it has none; a helper parsing no
buffer's text is its own. Three different texts are three, by design."
  (let ((holder (fault-holder where)))
    (cond ((helper-p holder) holder)
          ((typep holder 'tuatara.buffer:buffer)
           (let ((file (tuatara.buffer:buffer-file holder)))
             (if file (text-key :file file) (text-key :text (tuatara.buffer:buffer-text holder)))))
          (t holder))))

(defun end-helper (helper why &key (wait t))
  "Mark HELPER as ended, WHY being :CRASHED, :HUNG, :SLOW, :BROKEN or
:STOPPED: its process is ended, its end waited for here, or in another
thread unless WAIT; the calls waiting for it find it gone, and the next
helper wanted in its place is started afresh."
  (let ((first (sb-thread:with-mutex ((helper-write-lock helper))
                 (when (helper-alive helper)
                   (setf (helper-alive helper) nil
                         (helper-ended helper) why)
                   t))))
    (when first
      (sb-thread:with-mutex (**helpers-lock**)
        (setf **helpers** (remove helper **helpers**)
              **kept-helpers** (remove helper **kept-helpers** :key #'cdr)))
      (let ((process (helper-process helper)))
        (when process
          (when (sb-ext:process-alive-p process)
            (ignore-errors (sb-ext:process-kill process sb-unix:sigkill)))
          ;; its end waited for here, or, on a thread that must not wait, in another
          (flet ((reap ()
                   (ignore-errors (sb-ext:process-wait process))
                   (ignore-errors (sb-ext:process-close process))))
            (if wait
                (reap)
                (sb-thread:make-thread #'reap :name "syntax helper ending")))))
      (flet ((shut (fd)
               (sb-alien:alien-funcall (sb-alien:extern-alien "shutdown" (function sb-alien:int sb-alien:int sb-alien:int))
                                       fd 2)))
        (shut (helper-fd helper))
        (ignore-errors (sb-posix:close (helper-fd helper)))
        ;; the connections in use end for their callers, closed when given back
        (let ((idle (sb-thread:with-mutex ((helper-write-lock helper))
                      (dolist (call (helper-connections helper)) (shut (call-fd call)))
                      (setf (helper-connections helper) (set-difference (helper-connections helper) (helper-idle helper)))
                      (shiftf (helper-idle helper) '()))))
          (mapc #'close-call idle)))
      ;; one that stopped answering, or answered wrongly, while not parsing:
      ;; nothing more of its buffer is parsed, so that it does not hang again
      (when (and (member why '(:hung :broken)) (helper-buffer helper) (zerop (car (helper-parsing helper))))
        (note-faulted helper :all))
      (unless (eq why :stopped)
        (tuatara.runtime:debug-report :error "syntax helper (~(~a~)) ~(~a~)" (helper-kind helper) why)
        (when *helper-ended-function*
          (ignore-errors (funcall *helper-ended-function* helper)))))))

(defun reply-malformed (helper)
  "End HELPER, whose reply cannot be right: what it holds is not trusted, and
the texts it parsed are parsed again by another. NIL."
  (end-helper helper :broken)
  nil)

(defun stop-helper (helper &key (wait t))
  "End HELPER, no longer needed; its process's end waited for unless WAIT is NIL."
  (when (helper-alive helper)
    (end-helper helper :stopped :wait wait)))

(defun stop-helpers ()
  "End every helper, and every thread they were started from, as the program does
before a saved image is made and when it ends."
  (dolist (helper (sb-thread:with-mutex (**helpers-lock**) (copy-list **helpers**)))
    (stop-helper helper))
  (dolist (starter (sb-thread:with-mutex (**starter-lock**)
                     (prog1 **starters** (setf **starters** '() **retired** '() **starter** nil))))
    (when (sb-thread:thread-alive-p starter)
      (ignore-errors (sb-thread:terminate-thread starter))
      (ignore-errors (sb-thread:join-thread starter :default nil :timeout 2)))))

(pushnew 'forget-buffer-helper tuatara.buffer:*kill-buffer-functions*)
(pushnew 'stop-helpers sb-ext:*save-hooks*)
(pushnew 'stop-helpers sb-ext:*exit-hooks*)

(defvar *must-not-wait-function* nil
  "A function of no arguments, true on a thread that must never wait for a
helper to start (the editor's own); NIL for none.")

(defun waiting-allowed-p ()
  "True when this thread may wait for a helper to start (*MUST-NOT-WAIT-FUNCTION*)."
  (not (and *must-not-wait-function* (ignore-errors (funcall *must-not-wait-function*)))))

(sb-ext:defglobal **kept-locks** (list (cons :shared (sb-thread:make-mutex :name "shared syntax helper"))
                                       (cons :rules (sb-thread:make-mutex :name "rules syntax helper")))
  "For each kind of helper kept for all, the lock held while one is started.")

(sb-ext:defglobal **kept-starting** (list (cons :shared nil) (cons :rules nil))
  "For each kind of helper kept for all, true while one is started in the background.")

(defun known-kept-helper (key)
  "The helper kept for KEY, (KIND . DIRECTORIES), when it runs; else NIL."
  (sb-thread:with-mutex (**helpers-lock**)
    (let ((known (cdr (assoc key **kept-helpers** :test #'equal))))
      (and known (helper-alive known) known))))

(defun kept-helper (kind)
  "The helper of KIND kept for all to use (:SHARED or :RULES) for the grammar
directories there are now, started when there is none, one at a time, those
kept for other directories ended; NIL when it cannot be. On a thread that
must not wait (WAITING-ALLOWED-P), NIL while it is not running, and it is
started in the background, the editor woken once it is."
  (let ((key (cons kind (grammar-directories))))
    (when (helpers-refused-p kind)
      (return-from kept-helper (refuse-unconfined t)))
    (or (known-kept-helper key)
        (if (waiting-allowed-p)
            (sb-thread:with-mutex ((cdr (assoc kind **kept-locks**)))
              ;; started meanwhile by another thread
              (or (known-kept-helper key)
                  (let ((helper (start-helper (cdr key) kind)))
                    (when helper
                      (let ((old (sb-thread:with-mutex (**helpers-lock**)
                                   (prog1 (loop for (k . h) in **kept-helpers**
                                                when (eq (car k) kind) collect h)
                                     (setf **kept-helpers** (cons (cons key helper)
                                                                  (remove-if (lambda (entry) (eq (caar entry) kind))
                                                                             **kept-helpers**)))))))
                        (mapc #'stop-helper old)))
                    helper)))
            (let ((cell (assoc kind **kept-starting**)))
              (unless (shiftf (cdr cell) t)
                (start-in-background (lambda ()
                                       (unwind-protect (kept-helper kind)
                                         (setf (cdr cell) nil)))))
              nil)))))

(defun shared-helper ()
  "The helper parsing the files of projects you trust (KEPT-HELPER)."
  (kept-helper :shared))

(defun rules-helper ()
  "The helper loading grammars and compiling rules, never given any text (KEPT-HELPER)."
  (kept-helper :rules))

(defvar *helper* :none
  "The helper parsing the text being worked on (a buffer's, its syntax's),
for the parsers made meanwhile, as of text in another language in it: a
HELPER; :OWN for one of its own, started when first wanted
\(WITH-OWN-HELPER); :SHARED for the text of projects you trust, parsed in
the shared one; :NONE, as it is unless bound, when the text is not to be
parsed: what parses text says which.")

(defun current-helper ()
  "The helper for what is parsed now (*HELPER*): NIL when there is none to be had."
  (let ((helper *helper*))
    (cond ((eq helper :shared) (shared-helper))
          ((eq helper :own) (setf *helper* (or (start-own-helper :kept t) :none))
                            (current-helper))
          ((and (helper-p helper) (helper-alive helper)) helper))))

(defmacro with-own-helper (() &body body)
  "Run BODY with text it parses parsed in a helper of its own, started when
first wanted, never ended for room meanwhile, and ended after."
  `(let ((*helper* :own))
     (unwind-protect (progn ,@body)
       (when (helper-p *helper*) (stop-helper *helper*)))))

(sb-ext:defglobal **room-lock** (sb-thread:make-mutex :name "syntax helpers of their own")
  "Held while a helper of its own is started, room made for it first.")

(defun start-own-helper (&key kept buffer at-once)
  "Start a helper of its own (:OWN), room made for it first; KEPT when it is
never to be ended for room, BUFFER the one whose text it parses. When BUFFER
has a helper running by then, started meanwhile by another thread, that one;
NIL when none can be had, or BUFFER is killed, before or while it starts.
AT-ONCE, for a thread that must not wait: :BUSY at once when another start
or making room is under way, room made only once the start is had, helpers
ended for room not waited for, and the start given up on (:BUSY) when it
takes more than a moment."
  (when (helpers-refused-p :own)
    (return-from start-own-helper (refuse-unconfined nil)))
  (unless (sb-thread:grab-mutex **room-lock** :waitp (not at-once))
    (return-from start-own-helper :busy))
  (unwind-protect
       (let ((running (and buffer (tuatara.buffer:buffer-local buffer :syntax-helper))))
         (cond ((and running (helper-alive running)) running)
               ((and buffer (not (tuatara.buffer:live-buffer-p buffer))) nil)
               ;; at once: the start had first, so that a busy one ends no helper for room
               ((and at-once (not (sb-thread:grab-mutex **starting** :waitp nil))) :busy)
               (t (unwind-protect
                       (progn
                         (make-room-for-helper :wait (not at-once))
                         (start-own-helper-now kept buffer at-once))
                    (when at-once (sb-thread:release-mutex **starting**))))))
    (sb-thread:release-mutex **room-lock**)))

(defun start-own-helper-now (kept buffer at-once)
  "The work of START-OWN-HELPER, room made: the start, AT-ONCE holding
**STARTING**; a helper started for a buffer killed meanwhile ended, ended
in the background AT-ONCE."
  (let ((helper (start-helper (grammar-directories) :own :at-once at-once :held at-once)))
    (cond ((member helper '(nil :busy)) helper)
          ;; killed while it started: none left behind for it
          ((and buffer (not (tuatara.buffer:live-buffer-p buffer)))
           (stop-helper helper :wait (not at-once))
           nil)
          (t (setf (helper-kept helper) kept
                   (helper-buffer helper) buffer)
             (when buffer
               (setf (tuatara.buffer:buffer-local buffer :syntax-helper) helper))
             helper))))

(defun buffer-text-trusted-p (buffer)
  "True when BUFFER's text is that of a project you trust
\(*SYNTAX-HELPER-SHARED-FUNCTION*)."
  (and *syntax-helper-shared-function*
       (ignore-errors (funcall *syntax-helper-shared-function* buffer))
       t))

(defun buffer-helper (buffer)
  "The helper to parse BUFFER's text: the shared one when
*SYNTAX-HELPER-SHARED-FUNCTION* says so (a file of a project you trust), its
own helper ended then; else its own, the one it has running or one started
now, one at a time for a buffer whatever thread asks. NIL when none can be
had (no sandbox). Starting a helper takes a moment: see BUFFER-HELPER-SOON
for a thread that must not wait. What went wrong in its text is kept with
the buffer (FAULTED-NAMES), whichever helper parses it."
  (let ((own (tuatara.buffer:buffer-local buffer :syntax-helper)))
    (cond ((buffer-text-trusted-p buffer)
           (when (and own (helper-alive own)) (forget-buffer-helper buffer))
           (shared-helper))
          ((and own (helper-alive own)) own)
          (t (start-own-helper :buffer buffer)))))

(defun buffer-helper-soon (buffer)
  "BUFFER's helper (BUFFER-HELPER) when it is had at once: its own running;
on a thread that may wait (WAITING-ALLOWED-P), one started now; on one that
may not, as the editor's own, one set going now when the sandbox is known
and no other start holds it up, which waits for nothing, the program's
readiness waited for by the first parse in the background. Else NIL, and
it is started in the background, the editor woken (*SYNTAX-WAKE-FUNCTION*)
once it is, the text shown uncoloured meanwhile."
  (let ((own (tuatara.buffer:buffer-local buffer :syntax-helper)))
    (cond ((and own (helper-alive own) (not (buffer-text-trusted-p buffer))) own)
          ((waiting-allowed-p) (buffer-helper buffer))
          ((buffer-text-trusted-p buffer) (shared-helper))
          ((helpers-refused-p :own) (refuse-unconfined nil))
          ;; the sandbox known and no other start under way: set going now,
          ;; which takes no waiting for the program to be ready
          ((and (not (eq :unknown (tuatara.io:sandbox-status-now)))
                (let ((started (start-own-helper :buffer buffer :at-once t)))
                  (and (helper-p started) started))))
          (t (unless (tuatara.buffer:buffer-local buffer :syntax-helper-starting)
               (setf (tuatara.buffer:buffer-local buffer :syntax-helper-starting) t)
               (start-in-background (lambda ()
                                      (unwind-protect (buffer-helper buffer)
                                        (setf (tuatara.buffer:buffer-local buffer :syntax-helper-starting) nil)))))
             nil))))

(defparameter *helper-setting-names*
  '(*grammar-directories* *syntax-helper-memory* *syntax-helpers-most* *syntax-helper-shared-function*
    *syntax-helper-unconfined* *syntax-helper-wait*)
  "The settings that say how a helper is started and chosen, given to the
thread a parse runs in as they are where it was asked for.")

(defun helper-settings ()
  "The values of *HELPER-SETTING-NAMES* now, as a list."
  (mapcar #'symbol-value *helper-setting-names*))

(defun start-in-background (function)
  "Call FUNCTION, which starts a helper, in a thread of its own, then wake the
editor (*SYNTAX-WAKE-FUNCTION*) to show what it now parses."
  (let ((settings (helper-settings)))
    (sb-thread:make-thread (lambda ()
                             (progv *helper-setting-names* settings
                               (ignore-errors (funcall function)))
                             (let ((wake (and (boundp '*syntax-wake-function*) (symbol-value '*syntax-wake-function*))))
                               (when wake (ignore-errors (funcall wake)))))
                           :name "syntax helper starting in the background")))

(defvar *helper-in-use-function* nil
  "A function of a buffer, true while its text is shown; NIL for none. A
helper of a buffer shown, or parsing, is not ended for room.")

(defun helper-in-use-p (helper)
  "True when HELPER is not to be ended for room: kept (an export's), parsing,
or parsing for a buffer that is shown (*HELPER-IN-USE-FUNCTION*)."
  (or (helper-kept helper)
      (plusp (car (helper-parsing helper)))
      (let ((buffer (helper-buffer helper)))
        (and buffer *helper-in-use-function* (ignore-errors (funcall *helper-in-use-function* buffer))))))

(defun make-room-for-helper (&key (wait t))
  "End the helpers buffers have of their own that were used longest ago, so
that one more makes no more than *SYNTAX-HELPERS-MOST*; those in use
\(HELPER-IN-USE-P) are left running, and do not count when kept, the
helpers then going past the most, which the debug log says. Their
processes' ends waited for unless WAIT is NIL."
  (let* ((own (sort (remove-if-not (lambda (helper) (and (eq (helper-kind helper) :own) (not (helper-kept helper))))
                                   (sb-thread:with-mutex (**helpers-lock**) (copy-list **helpers**)))
                    #'< :key #'helper-used))
         (count (length own))
         (most (max 1 *syntax-helpers-most*)))
    (dolist (helper own)
      (when (< count most) (return))
      (unless (helper-in-use-p helper)
        (stop-helper helper :wait wait)
        (decf count)))
    (when (>= count most)
      (tuatara.runtime:debug-report :parse "~d syntax helpers of buffers in use, past *syntax-helpers-most* (~d)"
                                    (1+ count) most))))

(defun forget-buffer-helper (buffer)
  "End BUFFER's own helper, if it has one: its text is parsed no more. Taken
under the lock a helper of its own is started under, so that one starting
for BUFFER meanwhile sees it killed."
  (let ((own (sb-thread:with-mutex (**room-lock**)
               (shiftf (tuatara.buffer:buffer-local buffer :syntax-helper) nil))))
    (when own
      (stop-helper own))))

(defun helper-memory (helper)
  "What HELPER says of its memory: the octets its trees and grammars hold,
whether the memory it keeps for running short is kept, and the octets it may
still take (-1: no limit), as three values; NIL when it has ended."
  (with-call (call helper +op-memory+)
    (when (eql 0 (call-send call))
      (values (reply-i64-at call 0) (= 1 (reply-u32 call 2)) (reply-i64-at call 16)))))
