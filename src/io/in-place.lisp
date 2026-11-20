;;;; src/io/in-place.lisp --- Saving a large file by writing only what changed, in place, after a journal.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Where the file system cannot share blocks between files (ext4, and every
;;;; one but btrfs, XFS, bcachefs and ZFS), a new file renamed over the old
;;;; costs a copy of every octet: a 10 GB file written whole for one line. A
;;;; large file the buffer reads where it is (mapped) holds most of the new
;;;; text already where it belongs, so with *SAVE-IN-PLACE*, where the changes,
;;;; with the journal below, come to less than two thirds of the file, it is
;;;; saved in place:
;;;;
;;;;   1. The octets the save overwrites or cuts off, in whole pages, are
;;;;      copied into a file with no name beside the file, after a header
;;;;      naming the file, its length and where they came from; that journal
;;;;      is flushed, named .NAME.tuatara-journal and the directory flushed.
;;;;   2. The buffer's text, and its undo history, read those octets from the
;;;;      journal from then on.
;;;;   3. The changes are written into the file, its length set and the file
;;;;      flushed.
;;;;   4. The journal's name is taken away and the directory flushed.
;;;;
;;;; The file on disk is the old one until step 1 is on disk and the new one
;;;; once step 4 is. In between, after a crash or a power cut, it may be part
;;;; old and part new, with the journal beside it: the next time Tuatara opens
;;;; the file, or saves or reverts it, it puts the old file back exactly from
;;;; the journal before reading it (RECOVER-CUT-SHORT-SAVE), whatever
;;;; *SAVE-IN-PLACE* is by then, and a save waits until it has. Other programs
;;;; that read the file before then see it part saved. A save that fails, or
;;;; is stopped, while it writes the file puts the old one back at once.
;;;;
;;;; A journal is used only when it is a file of its own (one name, written by
;;;; its owner alone, who owns the file and runs Tuatara), no save or putting
;;;; back holds it (each holds a lock on it throughout), and it fits the file:
;;;; the same file, born when it was, its length between the old and the new,
;;;; last changed by the save. While a save writes, it marks its journal with the
;;;; time every few seconds, so that a change to the file after the save was
;;;; cut short is told from one the save made. Anything else, such as
;;;; another program having written the file since, is said and nothing
;;;; written, and the file is not saved while the journal is there. A journal
;;;; held by a save going on, or a putting back, is waited for a little; the
;;;; file is not opened meanwhile.
;;;; FALLOC_FL_INSERT_RANGE and COLLAPSE_RANGE, which open or close a gap of
;;;; whole blocks, are not used: an edit of text is almost never a whole
;;;; number of blocks long and at a block boundary.

(in-package #:tuatara.io)

(define-setting *ext4-save-in-place* nil
  "Whether a large file on ext4, or another file system that cannot share
blocks between files, is saved by writing only what changed into the file
itself, where that is quicker than writing it whole.
- t: it is saved so.
- nil: every save is written as a new file put in the old one's place
  (the standard).

It is used on ext4 and the other file systems that cannot share blocks
between files (all but btrfs, XFS, bcachefs and ZFS, where a new file costs
no more than the change anyway). It is used for a file of over 64 MB on a
local drive. It is used when the edit keeps the length of the text or lies in
about the last third of the file. A 10 GB file with one line changed is then
saved in a few hundredths of a second, where writing it whole takes about a
minute. An edit nearer the start moves all that follows it, so it is saved as
a new file either way.

What such a save will overwrite is first copied into .NAME.tuatara-journal
beside the file and put on the disk. The journal goes once the changes are
on the disk too. The trade-off: should the computer crash or lose power
during such a save, another program that reads the file before Tuatara
opens it again may find it half saved. Tuatara itself always gets the old
file or the new one. The next time it opens, saves or reverts the file, it
puts the old file back exactly from the journal, whatever this setting is
then. It does so only when the journal is surely that save's. One the file
has been changed after is left, said, and the file not saved until you
remove it.
On a file system that keeps no birth time for files (ext4 made with 128-octet
inodes), a new file may be made under the old name and be given the old one's
number by the system. It is told apart from the old file only by its times.
With NIL, every program finds the old file or the new one. A file opened
safely, or in an evidence directory, is never written, so never put back
either."
  :values boolean)

(defvar *in-place-most* 2/3
  "The most a save in place writes, journal and changes together, as a share
of the file's new length; a save that would write more writes a new file
renamed over the old instead. Written twice, once to the journal and once in
place, an octet costs more than one written once to a new file, so a save
in place is the quicker only below about this share.")

(defvar *saving-step* nil
  "NIL, or a function called with each step of a save in place as it is
reached, in order: :JOURNAL-WRITTEN, :JOURNAL-FLUSHED, :JOURNAL-NAMED,
:DIRECTORY-FLUSHED, :TEXT-MOVED, :WRITING (after each piece of the file
written), :FILE-WRITTEN, :FILE-FLUSHED, :JOURNAL-REMOVED and
:DIRECTORY-FLUSHED again. Tests stop a save at each, as a crash would.")

(defconstant +journal-version+ 2
  "The version of the journal's header this release writes and reads.")

(defconstant +journal-header-most+ (* 1024 1024)
  "The most octets a journal's header takes; a save that would need more is
not made in place.")

(defconstant +journal-heartbeat+ 2
  "Seconds between the marks a save in place makes on its journal, with the
time, while it writes.")

(defvar *journal-busy-wait* 10
  "Seconds opening, saving or reverting a file waits for another save of it,
or a putting back, to let its journal go, before it is refused.")

(define-condition journal-busy (file-system-error) ()
  (:documentation "A file that another save in place, or a putting back, is writing now: it is not
opened or saved until that has ended."))

(defvar *journal-owner* nil
  "The account, by its user id (UID), a journal must belong to, with the file
it is of, to be used: NIL for the one Tuatara runs as. Tests name another.")

(defparameter *journal-heading* "Tuatara save journal"
  "The words a journal's header begins with, before its version.")

(defun saving-step (step)
  "Tell *SAVING-STEP* that a save in place has reached STEP."
  (when *saving-step* (funcall *saving-step* step)))

(defun journal-path (target)
  "Where the journal of a save in place of the file TARGET is: beside it,
named .NAME.tuatara-journal."
  (format nil "~a/.~a.tuatara-journal" (string-right-trim "/" (native (parent-path target))) (native-file-name target)))

;;; What a save changes

(defun reads-file-p (fd existing)
  "True when the file open as file descriptor (FD) is the one EXISTING describes."
  (let ((stamp (tuatara.runtime:ignoring-errors-reported ("examining a mapped file") (fd-stamp fd "a mapped file"))))
    (same-stamped-file-p stamp existing)))

(defun in-place-test (mappings existing)
  "A function telling of a stretch of text from a mapped file, as
WRITE-ROPE's KEPT does, whether the file EXISTING describes holds its first
octets already where they go, and how many from there are alike: those of
MAPPINGS read from that very file at the offset they go to are."
  (let ((known (make-hash-table)))
    (lambda (mapping offset length position)
      (if (member mapping mappings)
          (multiple-value-bind (fd at end) (tuatara.text:mapping-source mapping offset)
            (values (and (= offset position) (= at offset)
                         (multiple-value-bind (answer found) (gethash fd known)
                           (if found answer (setf (gethash fd known) (reads-file-p fd existing)))))
                    (min length (- end offset))))
          (values nil length)))))

(defun read-elsewhere-p (mappings existing)
  "True when a mapping other than MAPPINGS, of any buffer or text kept, reads
the file EXISTING describes, which a write in place could change under it."
  (some (lambda (mapping)
          (and (not (member mapping mappings))
               (some (lambda (source) (reads-file-p (third source) existing)) (tuatara.text:mapping-sources mapping))))
        (tuatara.text:live-mappings)))

(defun rope-changes (rope kept)
  "The stretches (START . END) of ROPE, in order and apart, a file must be
given for it to hold ROPE: all but what KEPT (see IN-PLACE-TEST) says it
holds already."
  (let ((changes '()) (position 0))
    (flet ((changed (n)
             (if (and changes (= (cdar changes) position))
                 (incf (cdar changes) n)
                 (push (cons position (+ position n)) changes))
             (incf position n)))
      (tuatara.text:map-rope-sources
       (lambda (kind a b c)
         (ecase kind
           (:memory (changed (- c b)))
           (:file (let ((offset b) (length c))
                    (loop while (plusp length)
                          do (multiple-value-bind (in-place n) (funcall kept a offset length position)
                               (if in-place (incf position n) (changed n))
                               (incf offset n) (decf length n)))))))
       rope))
    (nreverse changes)))

(defun changes-test (changes)
  "A function as WRITE-ROPE's KEPT that says a stretch is in place but where
CHANGES (from ROPE-CHANGES, in order) has it; asked of stretches in order."
  (lambda (mapping offset length position)
    (declare (ignore mapping offset))
    (loop while (and changes (<= (cdar changes) position)) do (pop changes))
    (let ((change (first changes)))
      (cond ((null change) (values t length))
            ((< position (car change)) (values t (min length (- (car change) position))))
            (t (values nil (min length (- (cdr change) position))))))))

(defun journal-ranges (changes old-size new-size)
  "The stretches (START . END) of a file of OLD-SIZE octets that a save of
CHANGES, leaving it NEW-SIZE long, overwrites or cuts off, in whole pages,
in order and apart."
  (let ((page (sb-posix:getpagesize)) (ranges '()))
    (dolist (change (append changes (and (< new-size old-size) (list (cons new-size old-size)))))
      (let ((start (* page (floor (car change) page)))
            (end (* page (ceiling (cdr change) page))))
        (when (< start old-size)
          (if (and ranges (<= start (cdar ranges)))
              (setf (cdar ranges) (max end (cdar ranges)))
              (push (cons start end) ranges)))))
    (nreverse ranges)))

(defun ranges-octets (ranges)
  "The octets the stretches RANGES (START . END) cover."
  (loop for (start . end) in ranges sum (- end start)))

;;; The journal

(defun journal-header (existing born made new-size ranges data)
  "The header of the journal of a save in place of the file EXISTING
describes, born at BORN (NIL where the file system keeps no birth time), made
at MADE (in nanoseconds), to be NEW-SIZE long, keeping RANGES of it from
offset DATA on, as octets."
  (tuatara.text:string-octets
   (format nil "~a ~d~%data ~d~%file ~d ~d~%born ~d~%made ~d~%size ~d~%new ~d~%time ~d~%~:{range ~d ~d~%~}end~%"
           *journal-heading* +journal-version+ data (stamp-dev existing) (stamp-ino existing) (or born 0) made
           (stamp-size existing) new-size (stamp-mtime existing)
           (mapcar (lambda (range) (list (car range) (cdr range))) ranges))))

(defun journal-header-lines (fd path)
  "The lines of the header of the journal open as file descriptor (FD): its
second line says how long the header is (\"data N\"), up to
+JOURNAL-HEADER-MOST+ octets. PATH names it in messages."
  (flet ((head (octets)
           (let* ((head (make-array octets :element-type '(unsigned-byte 8)))
                  (got (read-fd-at fd head 0 :path path)))
             (uiop:split-string (tuatara.text:octets-string (subseq head 0 got)) :separator '(#\Newline)))))
    (let* ((start (head 4096))
           (data (second start))
           (octets (and data (uiop:string-prefix-p "data " data)
                        (every #'digit-char-p (subseq data 5)) (< 5 (length data) 15)
                        (parse-integer data :start 5))))
      (if (and octets (< 4096 octets) (<= octets +journal-header-most+)) (head octets) start))))

(defun read-journal-header (fd path)
  "What the journal open as file descriptor (FD) says, as a property list:
:DEV, :INO, :BORN (0 for none), :SIZE and :MTIME of the file it is of, :MADE,
when the journal was made, :NEW, the length the save gave it, :DATA, where
its octets begin, and :RANGES, the
stretches (START . END) of the file they are; NIL when it is not a journal
this release reads. PATH names it in messages."
  (let* ((lines (journal-header-lines fd path))
         (fields '()) (ranges '()))
    (flet ((numbers (line words)
             (let ((parts (uiop:split-string line :separator " ")))
               (and (= (length parts) (1+ words))
                    (every (lambda (part) (and (plusp (length part)) (every #'digit-char-p part))) (rest parts))
                    (mapcar #'parse-integer (rest parts))))))
      (unless (equal (first lines) (format nil "~a ~d" *journal-heading* +journal-version+))
        (return-from read-journal-header nil))
      (dolist (line (rest lines) nil)
        (let* ((word (subseq line 0 (or (position #\Space line) (length line))))
               (key (cdr (assoc word
                                '(("born" . :born) ("made" . :made) ("size" . :size) ("new" . :new) ("data" . :data)
                                  ("time" . :mtime))
                                :test #'string=))))
          (cond ((string= word "end")
                 (return (and (every (lambda (key) (getf fields key)) '(:dev :born :made :size :new :mtime :data))
                              (list* :ranges (nreverse ranges) fields))))
                ((string= word "file")
                 (let ((n (numbers line 2)))
                   (unless n (return nil))
                   (setf (getf fields :dev) (first n) (getf fields :ino) (second n))))
                (key
                 (let ((n (numbers line 1)))
                   (unless n (return nil))
                   (setf (getf fields key) (first n))))
                ((string= word "range")
                 (let ((n (numbers line 2)))
                   (unless (and n (< (first n) (second n))) (return nil))
                   (push (cons (first n) (second n)) ranges)))
                (t (return nil))))))))

(defun journal-refusal (journal path target-stamp)
  "Why the journal open as file descriptor JOURNAL, at PATH, is not to be used
for the file TARGET-STAMP describes, in words; NIL when it may be: a plain
file of one name that its owner alone may write, who owns that file too and
is the account Tuatara runs as (*JOURNAL-OWNER*)."
  (let ((stamp (fd-stamp journal path))
        (owner (or *journal-owner* (sb-posix:geteuid))))
    (cond ((not (eq (stamp-kind stamp) :regular)) (format nil "it is ~a" (kind-words (stamp-kind stamp))))
          ((/= (stamp-nlink stamp) 1) "it has other names")
          ((/= (stamp-uid stamp) owner) "it belongs to another user")
          ((/= (stamp-uid target-stamp) owner) "the file belongs to another user")
          ((logtest (stamp-mode stamp) #o022) "others may write it"))))

(defun journal-fits-p (header journal-stamp stamp born)
  "True when the journal whose header is HEADER, last marked by its save at
JOURNAL-STAMP's time, is of the file STAMP describes, born at BORN, as a save
cut short left it. That means the same file, its length between the old and
the new, not changed since the save began, or changed while the save went on,
no later than a few seconds after its last mark."
  (let ((old (getf header :size)) (new (getf header :new)) (changed (stamp-mtime stamp))
        (second 1000000000))
    (and (= (getf header :dev) (stamp-dev stamp)) (= (getf header :ino) (stamp-ino stamp))
         (= (getf header :born) (or born 0))
         (<= (min old new) (stamp-size stamp) (max old new))
         (or (= changed (getf header :mtime))
             (<= (- (getf header :made) (* 2 second)) changed
                 (+ (stamp-mtime journal-stamp) (* (+ +journal-heartbeat+ 3) second)))))))

(defun now-nanoseconds ()
  "The time now, in nanoseconds since 1970, as a stamp has it."
  (multiple-value-bind (seconds microseconds) (sb-ext:get-time-of-day)
    (+ (* seconds 1000000000) (* microseconds 1000))))

(defun start-heartbeat (journal target)
  "Mark JOURNAL, the journal of a save in place of TARGET, with the time now,
and again every +JOURNAL-HEARTBEAT+ seconds until the function returned is
called."
  (flet ((mark ()
           (tuatara.runtime:ignoring-errors-reported ("marking the journal of ~a" target)
             (set-fd-modification-time journal :now))))
    (mark)
    (let* ((stop (sb-thread:make-semaphore))
           (thread (tuatara.runtime:make-guarded-thread
                    (lambda ()
                      (loop until (sb-thread:wait-on-semaphore stop :timeout +journal-heartbeat+)
                            do (mark)))
                    :name "tuatara journal marks" :niceness 0)))
      (lambda ()
        (sb-thread:signal-semaphore stop)
        (when thread (sb-thread:join-thread thread :default nil))))))

(defun write-journal (journal file existing new-size ranges target cancel progress)
  "Copy RANGES of the file open as FILE, which EXISTING describes and a save
leaves NEW-SIZE long, into JOURNAL, a new file with no name, after its
header, and flush it. Returns where its octets begin; NIL when CANCEL's car
turned true first. PROGRESS is called with the octets copied so far."
  (let* ((born (fd-birth-time file))
         (made (now-nanoseconds))
         (header (journal-header existing born made new-size ranges 0))
         (data (let ((page (sb-posix:getpagesize))) (* page (ceiling (+ (length header) 64) page))))
         (done 0))
    (write-octets journal (journal-header existing born made new-size ranges data) :path target)
    (loop for (start . end) in ranges
          for length = (- (min end (stamp-size existing)) start)
          do (sb-posix:lseek journal (+ data start) sb-posix:seek-set)
             (unless (copy-fd-contents file journal length :from start :path target :cancel cancel
                                       :progress (and progress (lambda (n) (funcall progress (+ done n)))))
               (return-from write-journal nil))
             (incf done length))
    ;; whole pages, the last too, for the text to read them
    (sb-posix:ftruncate journal (+ data (if ranges (cdar (last ranges)) 0)))
    (saving-step :journal-written)
    (flush-fd journal target)
    (saving-step :journal-flushed)
    data))

(defun name-journal (journal directory target)
  "Name JOURNAL, a descriptor of an unnamed file in DIRECTORY, as TARGET's
journal and flush the directory. NIL when the system will not name it or a
journal of TARGET is there already."
  (call-with-directory directory
                       (lambda (dirfd)
                         (when (link-unnamed-at journal dirfd (native-file-name (journal-path target)))
                           (saving-step :journal-named)
                           (sync-directory directory)
                           (saving-step :directory-flushed)
                           t))
                       "save in"))

(defun remove-journal (directory target)
  "Take away TARGET's journal's name and flush the directory: what it kept is
not needed any more."
  (call-with-directory directory
                    (lambda (dirfd)
                      (unless (unlink-entry-at dirfd (native-file-name (journal-path target)))
                        (fail "remove the journal of" target)))
                    "save in")
  (saving-step :journal-removed)
  (sync-directory directory)
  (saving-step :directory-flushed))

(defun journal-there-p (target)
  "True when something is named as TARGET's journal beside it."
  (and (path-stamp (journal-path target) :follow nil :failure nil) t))

(defun put-back (file journal header target)
  "Put the octets JOURNAL kept back into FILE, open for writing, and give it
its old length and modification time again: the file before the save,
exactly. HEADER says what JOURNAL keeps (READ-JOURNAL-HEADER). Flushes FILE."
  (let ((size (getf header :size)) (data (getf header :data)))
    (loop for (start . end) in (getf header :ranges)
          for length = (- (min end size) start)
          when (plusp length)
            do (sb-posix:lseek file start sb-posix:seek-set)
               (copy-fd-contents journal file length :from (+ data start) :path target))
    (sb-posix:ftruncate file size)
    (set-fd-modification-time file (getf header :mtime))
    (flush-fd file target)))

(defun fd-content-same-p (fd stamp path)
  "True when the file open as file descriptor (FD), at PATH, is the one STAMP
describes, with the same content as far as the system tells."
  (same-content-p stamp (fd-stamp fd path)))

;;; Saving in place

(defun move-text-onto-journal (mappings existing journal data ranges)
  "Make what MAPPINGS read of the file EXISTING describes within RANGES read
from JOURNAL instead, whose octets begin at DATA. Signals when the system
will not, each stretch then read as it was."
  (dolist (mapping mappings)
    (let ((fd nil) (taken nil))
      (unwind-protect
           (loop for (start end source delta) in (tuatara.text:mapping-sources mapping)
                 when (and (zerop delta) (reads-file-p source existing))
                   do (loop for (from . to) in ranges
                            for low = (max start from) and high = (min end to)
                            when (< low high)
                              do (unless fd (setf fd (sb-posix:dup journal)))
                                 (if (tuatara.text:retarget-mapping-range mapping fd low high data)
                                     (setf taken t)
                                     ;; closed meanwhile: nothing reads it
                                     (return))))
        (when (and fd (not taken))
          (sb-posix:close fd))))))

(defun in-place-plan (rope existing mappings)
  "How a save in place of ROPE over the file EXISTING describes would go, as a
property list (:CHANGES and :RANGES, the stretches written and journalled),
when that writes less than *IN-PLACE-MOST* of the file and nothing but
MAPPINGS reads it; else NIL. MAPPINGS are those reading the file's own
octets."
  (unless (read-elsewhere-p mappings existing)
    (let* ((changes (rope-changes rope (in-place-test mappings existing)))
           (size (tuatara.text:rope-octet-length rope))
           (ranges (journal-ranges changes (stamp-size existing) size)))
      (when (and (< (+ (ranges-octets changes) (ranges-octets ranges)) (* *in-place-most* (max 1 size)))
                 (< (+ 4096 (length (journal-header existing 0 0 size ranges 0))) +journal-header-most+))
        (list :changes changes :ranges ranges)))))

(defun write-changes-in-place (rope target existing plan &key mappings cancel progress)
  "Save ROPE over TARGET, which EXISTING describes, by writing only the
changes PLAN (from IN-PLACE-PLAN) gives, in place, after a journal of what
they overwrite (this file's header). MAPPINGS read TARGET's own octets and
read the journal for those after.
Returns :DONE; :STOPPED with the file as it was; or NIL, the file untouched,
for a new file to be written instead. NIL comes when the file system takes no
file without a name or no lock, a journal of it is there already, the text
cannot be moved onto the journal, or the file was replaced, changed or about
to be written by another program meanwhile. A failure puts the file back as it
was before it is signalled."
  (let* ((directory (parent-path target))
         (changes (getf plan :changes)) (ranges (getf plan :ranges))
         (size (tuatara.text:rope-octet-length rope))
         (journalled (ranges-octets ranges))
         (changed (ranges-octets changes))
         (file (open-for-reading target t :accept '(:regular) :operation "save"))
         (journal nil) (data 0) (named nil) (writing nil) (out nil) (leases '()) (heartbeat nil))
    (labels ((report (octets)
               ;; counted in the text's octets, as every save's progress is
               (when progress (funcall progress (floor (* size octets) (max 1 (+ journalled changed))) size)))
             (give-back ()
               ;; the file as it was and no journal; said when that cannot be
               (when named
                 (setf named nil)
                 (cond ((not (or (not writing)
                                 (tuatara.runtime:ignoring-errors-reported ("putting back ~a from its journal" target)
                                   (put-back out journal (list :size (stamp-size existing) :mtime (stamp-mtime existing)
                                                               :data data :ranges ranges)
                                             target)
                                   t)))
                        (tell-writing (format nil "The save of ~a failed part way; its old content is kept in ~a and ~
                                                   put back when it is next opened"
                                              (native target) (journal-path target))))
                       ((tuatara.runtime:ignoring-errors-reported ("removing the journal of ~a" target)
                          (remove-journal directory target)
                          t))
                       ((journal-there-p target)
                        (tell-writing (format nil "The save of ~a failed; the file is as it was, but its journal ~a ~
                                                   could not be removed: it is used, and removed, when the file is ~
                                                   next opened"
                                              (native target) (journal-path target))))
                       (t (tell-writing (format nil "The save of ~a failed; the file is as it was, but its directory ~
                                                     could not be flushed: after a power cut now, its journal may be ~
                                                     beside it again, and is used when the file is next opened"
                                                (native target))))))))
      (unwind-protect
           (handler-bind ((error (lambda (e) (declare (ignore e)) (give-back))))
             (unless (fd-content-same-p file existing target)
               (error 'file-changed-on-disk :path target))
             (setf journal (open-unnamed-file directory #o600))
             ;; held throughout, so that no other save or putting back uses the journal meanwhile
             (unless (and journal (tuatara.runtime:lock-fd journal))
               (return-from write-changes-in-place nil))
             (setf data (write-journal journal file existing size ranges target cancel #'report))
             (unless data
               (return-from write-changes-in-place :stopped))
             ;; a journal there already is another save's: this one writes a new file instead
             (unless (name-journal journal directory target)
               (return-from write-changes-in-place nil))
             (setf named t
                   heartbeat (start-heartbeat journal target))
             (unless (handler-case (progn (move-text-onto-journal mappings existing journal data ranges) t)
                       (sb-posix:syscall-error () nil))
               (give-back)
               (return-from write-changes-in-place nil))
             (saving-step :text-moved)
             (multiple-value-bind (stood broken) (step-aside-leases mappings)
               (setf leases stood
                     out (handler-case (sb-posix:open (native target)
                                                      (logior sb-posix:o-wronly +o-nofollow+ +o-cloexec+))
                           (sb-posix:syscall-error (e) (fail "open for writing" target (sb-posix:syscall-errno e)))))
               ;; another program about to write it, or the file replaced since it was read: a new file instead
               (when (or broken (not (fd-content-same-p out existing target)))
                 (give-back)
                 (return-from write-changes-in-place nil)))
             (check-writing target "save" :fd out)
             (setf writing t)
             (unless (write-rope out rope :path target :cancel cancel :kept (changes-test changes)
                                          :progress (lambda (done total)
                                                      (saving-step :writing)
                                                      (report (+ journalled (floor (* changed done) (max 1 total))))))
               (give-back)
               (return-from write-changes-in-place :stopped))
             (sb-posix:ftruncate out size)
             (saving-step :file-written)
             (flush-fd out target)
             (saving-step :file-flushed)
             (remove-journal directory target)
             (setf named nil)
             (report (+ journalled changed))
             :done)
        (when heartbeat (funcall heartbeat))
        (when out (tuatara.runtime:close-fd-quietly out "~a" target))
        (take-back-leases leases)
        (when journal
          ;; the text may keep the journal open: the lock goes now
          (tuatara.runtime:unlock-fd journal)
          (tuatara.runtime:close-fd-quietly journal "the journal of ~a" target))
        (sb-posix:close file)))))

;;; A save cut short

(defun recover-from-journal (fd path &key safe (operation "open"))
  "Put back the file open as file descriptor (FD), at PATH, as it was before
a save in place that was cut short (by a crash or a power cut), when its
journal is beside it. The file is then exactly the old one, and the journal
gone. This is said in words (TELL-WRITING).
Nothing is written, and that is said, in these cases. The file is opened SAFE
\(the journal not even read). Nothing may be written (WRITING-REFUSAL). The
system refuses. The journal may not be used (JOURNAL-REFUSAL). The journal is
one of another file or does not fit the file as a save cut short left it
\(JOURNAL-FITS-P). This release cannot read the journal. The file is replaced
meanwhile.
A journal another save or putting back holds is waited for, *JOURNAL-BUSY-WAIT*
seconds at most, else JOURNAL-BUSY is signalled for OPERATION (\"open\"). True
when the file was put back, or changed by the one waited for."
  (let* ((target (or (fd-real-path fd) (native path)))
         (journal-path (journal-path target)))
    (when (path-stamp journal-path :follow nil :failure nil)
      (let ((refusal (if safe "it is opened safely" (writing-refusal target :fd fd))))
        (when refusal
          (tell-writing (format nil "~a may be part saved: a save of it was cut short, and its journal ~a is not used ~
                                     here, as ~a"
                                target journal-path refusal))
          (return-from recover-from-journal nil)))
      (flet ((left-alone (why)
               (tell-writing (format nil "~a may be part saved: a save of it was cut short, and its journal ~a is ~
                                          left as it is, as ~a; once you have made sure the file is right, remove the ~
                                          journal"
                                     target journal-path why))
               (return-from recover-from-journal nil)))
        (handler-case
            (let ((journal (handler-case (sb-posix:open journal-path (logior sb-posix:o-rdonly sb-posix:o-nonblock
                                                                             +o-nofollow+ +o-cloexec+))
                             (sb-posix:syscall-error (e) (fail "read" journal-path (sb-posix:syscall-errno e))))))
              (unwind-protect
                   (let ((refusal (journal-refusal journal journal-path (fd-stamp fd path))))
                     (when refusal (left-alone refusal))
                     (unless (tuatara.runtime:lock-fd journal :wait *journal-busy-wait*)
                       (error 'journal-busy :operation operation :path target
                                            :reason
                                            "a save of it, or a putting back, is going on now; try again shortly"))
                     ;; the one waited for has ended, its journal gone: the file is as it left it
                     (let ((named (path-stamp journal-path :follow nil :failure nil)))
                       (unless (and named (= (stamp-ino named) (stamp-ino (fd-stamp journal journal-path))))
                         (return-from recover-from-journal t)))
                     (let ((stamp (fd-stamp fd path))
                           (header (read-journal-header journal journal-path)))
                       (unless header (left-alone "this release cannot read it"))
                       (unless (journal-fits-p header (fd-stamp journal journal-path) stamp (fd-birth-time fd))
                         (left-alone "the file has been changed or replaced since the save was cut short"))
                       (let ((out (handler-case (sb-posix:open target
                                                               (logior sb-posix:o-wronly +o-nofollow+ +o-cloexec+))
                                    (sb-posix:syscall-error (e)
                                      (fail "open for writing" target (sb-posix:syscall-errno e))))))
                         (unwind-protect
                              (progn (unless (fd-content-same-p out stamp target)
                                       (left-alone "the file was replaced while it was being put back"))
                                     (check-writing target "put back" :fd out)
                                     (put-back out journal header target))
                           (sb-posix:close out))
                         (remove-journal (parent-path target) target)
                         (tell-writing (format nil "A save of ~a was cut short: the file is put back as it was before ~
                                                    it" target))
                         t)))
                (sb-posix:close journal)))
          (journal-busy (e) (error e))
          (file-system-error (e)
            (tell-writing (format nil "~a may be part saved: a save of it was cut short, and it could not be put back ~
                                       from ~a: ~a"
                                  target journal-path (file-system-error-reason e)))
            nil))))))

(define-record (recovery (:include tuatara.runtime:job) (:constructor %make-recovery) (:copier nil))
  "A file being put back from the journal of a save in place cut short, in
the background, a job (TUATARA.RUNTIME:JOB): PATH, through the file
descriptor (FD) of its own; PUT-BACK true once it was, and REFUSAL the
JOURNAL-BUSY signalled when it was not, as another save or putting back held
the journal."
  (path nil :documentation "The file RECOVERY puts back.")
  (fd nil :documentation "The descriptor RECOVERY opened its file through.")
  (put-back nil :documentation "True once RECOVERY put its file back, NIL before or when it was refused.")
  (refusal nil :documentation "The JOURNAL-BUSY condition RECOVERY was refused with, or NIL."))

(tuatara.runtime:define-job-words recovery
  :doing "Putting back"
  :description (format nil "Putting back ~a as it was before a save of it was cut short" (native (recovery-path job)))
  :verb "put the file back"
  :left-words "the file is put back when it is next opened"
  :at-exit :wait)
(defmethod tuatara.runtime:job-holds-queue-p ((job recovery)) nil)
(defmethod tuatara.runtime:job-shows-processing-p ((job recovery)) nil)

(defun recover-cut-short-save (fd path &key safe (operation "open"))
  "Put back the file open as file descriptor (FD), at PATH, from the journal
of a save in place of it that was cut short, when one is beside it
\(RECOVER-FROM-JOURNAL), with SAFE and OPERATION as there: in the background,
a job, which what asked waits for (HOST-WAIT-FOR-JOB); already in a job,
there and then. True when the file was put back, or changed meanwhile by a
save waited for; JOURNAL-BUSY when another save or putting back held it too
long."
  (cond ((not (journal-there-p (or (fd-real-path fd) (native path)))) nil)
        (tuatara.runtime:*job* (recover-from-journal fd path :safe safe :operation operation))
        (t (let ((job (%make-recovery :path path :fd (sb-posix:dup fd))))
             (tuatara.runtime:start-job-thread
              job
              (lambda (job)
                (unwind-protect
                     (handler-case
                         (setf (recovery-put-back job)
                               (recover-from-journal (recovery-fd job) path :safe safe :operation operation))
                       (journal-busy (e) (setf (recovery-refusal job) e)))
                  (sb-posix:close (recovery-fd job))))
              :name "tuatara putting back")
             (if (tuatara.runtime:job-thread job)
                 (tuatara.runtime:host-wait-for-job tuatara.runtime:*host* job
                                                    (format nil "Putting back ~a as it was before a save of it was ~
                                                                 cut short..."
                                                            (native-file-name path)))
                 (sb-posix:close (recovery-fd job)))
             (tuatara.runtime:take-in-jobs (lambda (other) (eq other job)))
             (when (recovery-refusal job)
               (error (recovery-refusal job)))
             (recovery-put-back job)))))

(defun recover-before-saving (target existing)
  "Put TARGET, which EXISTING describes, back as it was before a save in
place of it that was cut short, when its journal is beside it, before it is
saved again (RECOVER-CUT-SHORT-SAVE): its stamp then, or NIL when there was
no journal. While a journal stays beside it, it is not saved (an error
saying why)."
  (when (and existing (eq (stamp-kind existing) :regular) (journal-there-p target))
    (let ((fd (open-for-reading target t :accept '(:regular) :operation "save")))
      (unwind-protect
           (progn
             (recover-cut-short-save fd target :operation "save")
             (when (journal-there-p target)
               (error 'file-system-error
                      :operation "save" :path target
                      :reason (format nil "a save of it was cut short and its journal ~a is still beside it (see ~
                                           above); once you have made sure the file is right, remove the journal"
                                      (journal-path target))))
             (fd-stamp fd target))
        (sb-posix:close fd)))))
