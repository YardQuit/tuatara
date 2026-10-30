;;;; test/emulator.lisp --- The terminal under M-x terminal: a program on a pseudo-terminal, its screen kept by the terminal library.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test emulator
  :description "A program run on a pseudo-terminal, its screen kept by the terminal library: text, colours and wide characters read row by row, typing, resizing, the library answering the program's questions, the editor's thread never kept waiting while the program floods it, and a clean end.")

(defun emulator-screen (emulator &key (until (constantly t)) (seconds 5))
  "EMULATOR's screen as a vector of its rows' texts and a vector of their
looks, read whole, then frame after frame until UNTIL, a function of the
texts, is true or SECONDS have passed; the cursor's column and row as third
and fourth values."
  (let ((texts (make-array (tuatara.defaults::emulator-rows emulator) :initial-element ""))
        (looks (make-array (tuatara.defaults::emulator-rows emulator) :initial-element '()))
        (deadline (tuatara.runtime:deadline-in seconds))
        (x nil) (y nil) (all t))
    (loop
      (multiple-value-setq (x y)
        (tuatara.defaults::emulator-frame emulator (lambda (row text spans)
                                                     (when (< row (length texts))
                                                       (setf (aref texts row) text (aref looks row) spans)))
                                          :all all))
      (setf all nil)
      (when (or (funcall until texts) (<= (tuatara.runtime:seconds-left deadline) 0)) (return))
      (sleep 1/50))
    (values texts looks x y)))

(defun screen-has (text)
  "A function of a screen's row texts: true when one of them holds TEXT."
  (lambda (texts) (some (lambda (row) (search text row)) texts)))

(defmacro with-emulator ((var program arguments &rest keys) &body body)
  "Run BODY with VAR an emulator running PROGRAM with ARGUMENTS, stopped after."
  `(let ((,var (tuatara.defaults::start-emulator ,program ,arguments ,@keys)))
     (unwind-protect (progn ,@body)
       (tuatara.defaults::stop-emulator ,var))))

(define-test (emulator text-colours-and-wide)
  (true (tuatara.defaults::terminal-library-p))
  (with-emulator (e "sh" (list "-c" "printf 'hello \\033[1;31mred\\033[0m 漢字 end\\n'; sleep 5") :columns 40 :rows 6)
    (multiple-value-bind (texts looks) (emulator-screen e :until (screen-has "end"))
      (let ((row (position-if (lambda (r) (search "hello" r)) texts)))
        (true row)
        ;; a wide character once, as the editor shows it
        (is string= "hello red 漢字 end" (string-right-trim " " (aref texts row)))
        ;; red and bold on its three characters only
        (let ((red (find 6 (aref looks row) :key #'first)))
          (true red)
          (is = 9 (second red))
          (true (member :bold (cddr (tuatara.defaults::look-list (third red)))))
          (is eql 1 (first (tuatara.defaults::look-list (third red)))))))))

(define-test (emulator typing)
  ;; what is typed reaches the program, which answers in capitals
  (with-emulator (e "tr" '("a-z" "A-Z") :columns 40 :rows 6)
    (tuatara.defaults::emulator-input e (format nil "typed-~d~c" 42 #\Return))
    (true (find-if (lambda (r) (search "TYPED-42" r)) (emulator-screen e :until (screen-has "TYPED-42"))))))

(define-test (emulator pastes-raw-octets)
  ;; a pasted raw byte's character reaches the program as its octet
  (with-emulator (e "sh" (list "-c" "stty raw -echo; head -c 3 | od -An -tx1; sleep 5") :columns 40 :rows 6)
    (sleep 0.3)
    (tuatara.defaults::emulator-paste e (format nil "a~cb" (code-char #xDCFF)))
    (true (find-if (lambda (r) (search "61 ff 62" r)) (emulator-screen e :until (screen-has "61 ff 62"))))))

(define-test (emulator resized)
  (with-emulator (e "sh" (list "-c" "sleep 0.5; stty size; sleep 5") :columns 80 :rows 24)
    (tuatara.defaults::emulator-resize e 40 10)
    (true (find-if (lambda (r) (search "10 40" r)) (emulator-screen e :until (screen-has "10 40"))))))

(define-test (emulator answers-the-program)
  ;; a program asking the terminal what it is gets the library's answer
  (with-emulator (e "bash" (list "-c" "stty -echo; printf '\\033[c'; IFS= read -r -s -t 3 -d c reply; echo \"answer:${reply#?}\"; sleep 5")
                 :columns 60 :rows 6)
    (let ((row (find-if (lambda (r) (search "answer:" r)) (emulator-screen e :until (screen-has "answer:")))))
      (true row)
      (true (search "answer:[?" row)))))

(define-test (emulator never-keeps-the-editor-waiting)
  ;; a program printing as fast as it can: frames taken all the while, none waiting long
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "flood.txt" dir))))
      (with-open-file (out file :direction :output :if-exists :supersede)
        (dotimes (i 400000) (format out "line ~d of a program printing as fast as it can~c~%" i #\Return)))
      (with-emulator (e "cat" (list file) :columns 120 :rows 40)
        (let ((longest 0) (frames 0))
          (loop until (tuatara.defaults::emulator-closed e)
                do (let ((start (get-internal-real-time)))
                     (tuatara.defaults::emulator-frame e (lambda (&rest row) (declare (ignore row))))
                     (setf longest (max longest (- (get-internal-real-time) start)))
                     (incf frames))
                   (sleep 1/100))
          (true (plusp frames))
          ;; a frame, the wait for the reader to step aside included
          (true (< (/ longest internal-time-units-per-second) 1/10) "the longest frame took ~,3f s"
                (/ longest internal-time-units-per-second))
          (true (find-if (lambda (r) (search "line 399999 of" r)) (emulator-screen e))))))))

(define-test (emulator set-as-terminals-are)
  ;; echo on and UTF-8, as a program's line editor expects, and its own session
  (with-emulator (e "sh" (list "-c" "stty -a; ps -o stat= -p $$; sleep 5") :columns 160 :rows 12)
    (let ((texts (emulator-screen e :until (screen-has "eol"))))
      (true (find-if (lambda (r) (search " echo " r)) texts))
      (false (find-if (lambda (r) (search "-echo " r)) texts))
      (true (find-if (lambda (r) (search " iutf8" r)) texts)))))

(define-test (emulator ends-a-program-deaf-to-hangups)
  ;; a program that ignores the hangup is killed, within a few seconds
  (let ((e (tuatara.defaults::start-emulator "sh" (list "-c" "trap '' HUP; sleep 60") :columns 40 :rows 6))
        (start (get-internal-real-time)))
    (sleep 0.3)
    (tuatara.defaults::stop-emulator e)
    (false (sb-ext:process-alive-p (tuatara.defaults::emulator-process e)))
    (true (< (- (get-internal-real-time) start) (* 5 internal-time-units-per-second)))))

(define-test (emulator ends-cleanly)
  (let ((e (tuatara.defaults::start-emulator "sh" '() :columns 40 :rows 6)))
    (true (tuatara.defaults::emulator-alive-p e))
    (tuatara.defaults::stop-emulator e)
    (false (tuatara.defaults::emulator-alive-p e))
    (false (sb-ext:process-alive-p (tuatara.defaults::emulator-process e)))
    (false (sb-thread:thread-alive-p (tuatara.defaults::emulator-reader e)))
    (false (sb-thread:thread-alive-p (tuatara.defaults::emulator-writer e)))
    (false (gethash (tuatara.defaults::emulator-id e) tuatara.defaults::*emulators*))))

(define-test (emulator compressed-when-quiet)
  ;; a long scrollback, quiet a moment, is compressed in the background and reads back whole
  (let ((was tuatara-user:*terminal-compress-after*))
    ;; set, not bound: the compressor's thread reads it
    (setf tuatara-user:*terminal-compress-after* 0.3)
    (unwind-protect
         (with-emulator (e "sh" (list "-c" "seq 1 300000; sleep 30") :columns 80 :rows 24)
           (true (find-if (lambda (r) (search "300000" r)) (emulator-screen e :until (screen-has "300000") :seconds 20)))
           (let ((before (tuatara.defaults::emulator-memory e))
                 (deadline (tuatara.runtime:deadline-in 10))
                 (after nil)
                 (longest 0))
             ;; the library asked for meanwhile, as a frame does: never kept waiting long
             (loop do (let ((start (get-internal-real-time)))
                        (setf after (tuatara.defaults::emulator-memory e))
                        (setf longest (max longest (- (get-internal-real-time) start))))
                   until (or (plusp (getf after :compressed-pages)) (<= (tuatara.runtime:seconds-left deadline) 0))
                   do (sleep 0.005))
             (true (< (/ longest internal-time-units-per-second) 1/10) "the library was held ~,3f s"
                   (/ longest internal-time-units-per-second))
             (true (getf after :compression))
             (true (plusp (getf after :compressed-pages)))
             (true (< (getf after :resident) (getf before :resident)) "~:d octets before, ~:d after"
                   (getf before :resident) (getf after :resident))
             (let ((text (tuatara.defaults::emulator-text e)))
               (true (search (format nil "~%250000~%") text))
               (true (search (format nil "~%299999~%") text)))))
      (setf tuatara-user:*terminal-compress-after* was))))

(define-test (emulator many-marks-on-one-character)
  ;; a character with more combining marks than a cell keeps (text made to garble
  ;; screens): read safely, the rest of the row where it belongs
  (with-emulator (e "sh" (list "-c" (format nil "printf 'a~a|end\\n'; sleep 5"
                                           (with-output-to-string (out) (dotimes (i 70) (write-string "\\314\\201" out)))))
                 :columns 40 :rows 6)
    (let* ((texts (emulator-screen e :until (screen-has "end")))
           (row (find-if (lambda (r) (search "end" r)) texts)))
      (true row)
      (true (search "|end" row)))))

(define-test (emulator answers-kept-within-bounds)
  ;; text asking what the terminal is over and over, the program reading nothing a while:
  ;; the answers waiting stay within the limit, and what is typed reaches it after
  (let ((was tuatara.defaults::*terminal-reply-limit*))
    ;; set, not bound: the reader's thread reads it
    (setf tuatara.defaults::*terminal-reply-limit* 65536)
    (unwind-protect
         (with-emulator (e "sh" (list "-c" "stty -echo -icanon; printf '\\033[c%.0s' $(seq 1 60000); echo flooded; sleep 1; exec cat -v")
                        :columns 80 :rows 10)
           (let ((most 0) (deadline (tuatara.runtime:deadline-in 20)))
             (loop until (or (find-if (lambda (r) (search "flooded" r)) (emulator-screen e :seconds 0))
                             (<= (tuatara.runtime:seconds-left deadline) 0))
                   do (setf most (max most (tuatara.defaults::emulator-replies e)))
                      (sleep 1/100))
             (true (<= most 65536) "~:d octets of answers waited" most)
             (true (plusp most))
             (tuatara.defaults::emulator-input e (format nil "typed-ok~c" #\Return))
             (true (find-if (lambda (r) (search "typed-ok" r)) (emulator-screen e :until (screen-has "typed-ok") :seconds 20)))))
      (setf tuatara.defaults::*terminal-reply-limit* was))))

(define-test (emulator pictures-off-keeps-none)
  ;; without pictures the library keeps none of them; with them, as much as is allowed
  (flet ((limit (e)
           (tuatara.defaults::with-emulator-turn (e)
             (tuatara.defaults::library-size (tuatara.defaults::emulator-terminal e) (tuatara.defaults::vt :data-kitty-storage-limit)))))
    (with-emulator (e "sh" (list "-c" "sleep 5") :columns 40 :rows 6)
      (is eql 0 (limit e)))
    (with-emulator (e "sh" (list "-c" "sleep 5") :columns 40 :rows 6 :pictures t :cell-width 10 :cell-height 20)
      (is eql tuatara-user:*terminal-picture-memory* (limit e)))))

(define-test (emulator typing-never-waits-for-the-library)
  ;; the library held a while (as while a picture is drawn): keys and pastes still go at once
  (with-emulator (e "tr" '("a-z" "A-Z") :columns 40 :rows 6)
    (let ((held (sb-thread:make-semaphore)) (release (sb-thread:make-semaphore)))
      (let ((holder (sb-thread:make-thread
                     (lambda ()
                       (tuatara.defaults::with-emulator-turn (e)
                         (sb-thread:signal-semaphore held)
                         (sb-thread:wait-on-semaphore release :timeout 10))))))
        (sb-thread:wait-on-semaphore held)
        (let ((start (get-internal-real-time)))
          (true (tuatara.defaults::emulator-key e (tuatara.frontend:make-key #\k)))
          (tuatara.defaults::emulator-paste e "pasted")
          (tuatara.defaults::emulator-key e (tuatara.frontend:make-key :enter))
          (true (< (- (get-internal-real-time) start) (/ internal-time-units-per-second 10))))
        (sb-thread:signal-semaphore release)
        (sb-thread:join-thread holder)
        (true (find-if (lambda (r) (search "KPASTED" r)) (emulator-screen e :until (screen-has "KPASTED"))))))))

(define-test (emulator resize-never-waits)
  ;; while the reader holds the library (a picture being drawn), a resize is not waited for:
  ;; :BUSY, the size kept, and made once the library is free
  (with-emulator (e "sh" (list "-c" "sleep 5") :columns 40 :rows 6)
    (let* ((held (sb-thread:make-semaphore)) (free (sb-thread:make-semaphore))
           (holder (sb-thread:make-thread
                    (lambda ()
                      (sb-thread:with-mutex ((tuatara.defaults::emulator-lock e))
                        (sb-thread:signal-semaphore held)
                        (sb-thread:wait-on-semaphore free :timeout 5))))))
      (sb-thread:wait-on-semaphore held :timeout 5)
      (let ((start (get-internal-real-time)))
        (is eq :busy (tuatara.defaults::emulator-resize e 50 8))
        (true (< (- (get-internal-real-time) start) (/ internal-time-units-per-second 10))))
      (is = 40 (tuatara.defaults::emulator-columns e))
      (sb-thread:signal-semaphore free)
      (sb-thread:join-thread holder)
      (is eq t (tuatara.defaults::emulator-resize e 50 8))
      (is = 50 (tuatara.defaults::emulator-columns e)))))

(defun least-of-three (function &key bytes)
  "The least of three runs of FUNCTION: its internal real time, or with BYTES
the octets it consed."
  (loop repeat 3
        minimize (if bytes
                     (let ((before (sb-ext:get-bytes-consed))) (funcall function) (- (sb-ext:get-bytes-consed) before))
                     (let ((before (get-internal-real-time))) (funcall function) (- (get-internal-real-time) before)))))

(define-test (emulator layout-read-at-once)
  ;; a key of the library's layout named in the code costs the same wherever it is in the
  ;; layout: 0.4 µs for one near its end when each was looked for through the layout
  (true (tuatara.defaults::terminal-library-p))
  (let* ((layout tuatara.defaults::*terminal-layout*)
         (time (lambda (key)
                 (least-of-three (compile nil `(lambda () (dotimes (i 1000000) (tuatara.defaults::vt ,key))))))))
    (let ((first (funcall time (first layout))) (last (funcall time (car (last layout 2)))))
      (true (< (max last 1000) (* 3 (max first 1000))) "the last key ~d, the first ~d" last first))))

(define-test (emulator frame-garbage-per-cell)
  ;; a whole frame of 200 by 50 cells in colours a program chose, eight cells a run, or blank:
  ;; a few octets a cell, the row's text and its runs of looks; 1,050 octets a cell of colours
  ;; and 42 a blank one when each cell's look was made a list and its colours strings
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "screen.txt" dir))))
      (dolist (coloured '(t nil))
        (with-open-file (out file :direction :output :if-exists :supersede)
          (dotimes (row 49)
            (dotimes (column 200)
              (if coloured
                  (format out "~c[1;38;2;~d;~d;~dm~c[48;2;~d;~d;7mx" #\Escape (mod row 256) row (floor column 8) #\Escape (floor column 8) row)
                  (write-char #\Space out)))
            (format out "~c[0m~c~%" #\Escape #\Return)))
        (with-emulator (e "sh" (list "-c" (format nil "cat '~a'; echo; echo shown; sleep 30" file)) :columns 200 :rows 50)
          (true (find-if (lambda (r) (search "shown" r)) (emulator-screen e :until (screen-has "shown"))))
          (let ((each (/ (least-of-three (lambda () (tuatara.defaults::emulator-frame e (lambda (&rest row) (declare (ignore row))) :all t))
                                         :bytes t)
                         (* 200 50))))
            (true (< each 24) "~:[blank~;coloured~] cells: ~,1f octets a cell" coloured (float each))))))))

;;; Files of octets a terminal shows

(defun write-octets-file (file &rest parts)
  "Write FILE of PARTS, each a string (its characters' codes, all below 256,
the octets) or a list of a count and a string written that many times."
  (with-open-file (out file :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (flet ((put (string) (loop for c across string do (write-byte (char-code c) out))))
      (dolist (part parts)
        (if (consp part) (loop repeat (first part) do (put (second part))) (put part))))))

;;; Pictures in text

(defun picture-screen-file (file runs &key (placements 0))
  "Write FILE of two pictures in text (kitty's Unicode placeholders), RUNS
pairs of cells standing for them in turn (each cell its own run), and
PLACEMENTS more places of the first beside its place in text; then a line
\"shown\"."
  (let ((cell (map 'string #'code-char '(#xf4 #x8e #xbb #xae))))
    (apply #'write-octets-file file
           (append (list (format nil "~c_Ga=t,f=24,s=1,v=1,i=1,q=2;AAAA~c\\~:*~:*~c_Ga=p,i=1,p=1,U=1,c=1,r=1,q=2~c\\" #\Escape #\Escape)
                         (format nil "~c_Ga=t,f=24,s=1,v=1,i=2,q=2;AAAA~c\\~:*~:*~c_Ga=p,i=2,p=1,U=1,c=1,r=1,q=2~c\\" #\Escape #\Escape))
                   (loop for k from 2 below (+ 2 placements)
                         collect (format nil "~c_Ga=p,i=1,p=~d,P=1,Q=1,q=2~c\\" #\Escape k #\Escape))
                   (list (format nil "~c[H" #\Escape)
                         (list runs (format nil "~c[38;5;1m~a~:*~:*~c[38;5;2m~a" #\Escape cell))
                         (format nil "~c[0m~c~cshown" #\Escape #\Return #\Newline))))))

(defun picture-frame-time (file)
  "The least internal real time of three whole frames of a terminal 200 by 50
showing FILE (PICTURE-SCREEN-FILE's), its pictures on, a row of it changed
before each by its program."
  (with-emulator (e "sh" (list "-c" (format nil "cat '~a'; while :; do printf '\\033[50;10Hz'; sleep 0.02; done" file))
                 :columns 200 :rows 50 :pictures t :cell-width 10 :cell-height 20)
    (true (find-if (lambda (r) (search "shown" r)) (emulator-screen e :until (screen-has "shown") :seconds 30)))
    (least-of-three (lambda () (tuatara.defaults::emulator-frame e (lambda (&rest row) (declare (ignore row))) :all t)))))

(define-test (emulator picture-runs-framed-in-linear-time)
  ;; two pictures in text, the screen's cells standing for them in turn, 1,200 runs and
  ;; then 4,900: what each run more adds to a frame is a few microseconds
  (with-test-directory (dir)
    (let ((few (namestring (merge-pathnames "few.txt" dir))) (many (namestring (merge-pathnames "many.txt" dir))))
      (picture-screen-file few 600)
      (picture-screen-file many 2450)
      (let* ((a (picture-frame-time few)) (b (picture-frame-time many))
             (each (/ (* 1e6 (/ (- b a) internal-time-units-per-second)) 3700)))
        (true (< each 20) "~,1f µs a run more (~,3f s and ~,3f s a frame)" each
              (/ a internal-time-units-per-second) (/ b internal-time-units-per-second))))))

(define-test (emulator many-placements-dropped)
  ;; one picture placed 20,000 times beside a place in text, the screen full of cells
  ;; standing for two pictures: the pictures dropped once placed more than the limit, a
  ;; frame quick
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "placements.txt" dir))))
      (picture-screen-file file 2450 :placements 20000)
      (let ((each (picture-frame-time file)))
        (true (< each (/ internal-time-units-per-second 20)) "~,3f s a frame" (/ each internal-time-units-per-second))))))

;;; Output the terminal library must survive

(defparameter *survival-seconds* 120
  "The longest a process of SURVIVES-IN-CHILD may take before it is ended and its
test fails.")

(defun survives-in-child (case)
  "Show the output CASE names (test/emulator/survives.lisp) in a terminal in
a process of its own, so that output ending that process fails a test
rather than ending the tests: what the terminal showed after, a property
list; the process's exit code, or :TIMEOUT when it was ended after
*SURVIVAL-SECONDS*; and what it printed, its errors among it."
  (with-test-directory (dir)
    (let* ((out (merge-pathnames "shown" dir))
           (printed (merge-pathnames "printed" dir))
           (process (sb-ext:run-program sb-ext:*runtime-pathname*
                                        (list "--dynamic-space-size" "2GB" "--noinform" "--disable-ldb" "--end-runtime-options" "--script"
                                              (namestring (asdf:system-relative-pathname :tuatara "test/emulator/survives.lisp"))
                                              case (namestring out) (namestring dir))
                                        :wait nil :output (namestring printed) :if-output-exists :supersede :error :output))
           (deadline (tuatara.runtime:deadline-in *survival-seconds*)))
      (loop while (and (sb-ext:process-alive-p process) (plusp (tuatara.runtime:seconds-left deadline)))
            do (sleep 1/10))
      (let ((code (if (sb-ext:process-alive-p process)
                      (progn (sb-ext:process-kill process 9) (sb-ext:process-wait process) :timeout)
                      (sb-ext:process-exit-code process))))
        (sb-ext:process-close process)
        (values (ignore-errors (with-open-file (in out) (with-standard-io-syntax (let ((*read-eval* nil)) (read in)))))
                code
                (or (ignore-errors (uiop:read-file-string printed)) ""))))))

(defmacro surviving ((shown case) &body body)
  "Run BODY with SHOWN what the terminal showed of the output CASE names, in a
process of its own (SURVIVES-IN-CHILD), once that process ended well; the
tests skipped when they run in Tuatara's own program, which runs no
scripts as SBCL does."
  (let ((code (gensym "CODE")) (printed (gensym "PRINTED")))
    `(if (string-equal "sbcl" (pathname-name sb-ext:*runtime-pathname*))
         (multiple-value-bind (,shown ,code ,printed) (survives-in-child ,case)
           (is eql 0 ,code "the process ended with ~a; it printed:~%~a" ,code ,printed)
           ,@body)
         (skip "these tests run in Tuatara's own program, not SBCL, which they start a process of" (true t)))))

(define-test (emulator clearing-keeps-the-cursor-look)
  ;; screens cleared into a scrollback of no lines (CSI 22 J) while the cursor is bold and
  ;; in a link on the oldest page: the process lives on, the next character bold and linked
  (surviving (shown "clear")
    (is string= "y" (string-right-trim " " (or (getf shown :text) "")))
    (true (getf shown :bold))
    (is equal "http://example.com" (getf shown :link))))

(define-test (emulator line-limit-keeps-the-cursor-page)
  ;; a terminal of one row and no scrollback, a bold link reflowed wider, then line feeds
  ;; letting go of the page the cursor is on: the process lives on, the next character
  ;; bold and linked
  (surviving (shown "line-limit")
    (true (search "b" (or (getf shown :text) "")))
    (true (getf shown :bold))
    (is equal "http://example.com" (getf shown :link))))

(define-test (emulator wide-page-narrowed)
  ;; a screen 65,535 columns wide holding "hello" made 2 wide and 1 high: the process
  ;; lives on, the screen 2 wide, its text all there, its rows read in turn
  (surviving (shown "narrowed")
    (is eql 2 (getf shown :columns))
    (is string= "hello" (remove-if (lambda (c) (member c '(#\Newline #\Space))) (or (getf shown :history) "")))))

(define-test (emulator pictures-out-of-bounds-harmless)
  ;; a picture placed at the largest sizes and offsets the protocol can write, then deleted
  ;; by them: nothing shown of it, the text after it drawn
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "pictures.txt" dir)))
          (big "4294967295"))
      (write-octets-file file
                         (format nil "~c_Ga=t,f=24,s=1,v=1,i=1,q=2;AAAA~c\\" #\Escape #\Escape)
                         (format nil "~c_Ga=p,i=1,q=2,c=~a,r=~:*~a,X=~:*~a,Y=~:*~a,x=~:*~a,y=~:*~a,w=~:*~a,h=~:*~a,H=-2147483648,V=-2147483648,z=-2147483648~c\\"
                                 #\Escape big #\Escape)
                         (format nil "~c_Ga=p,i=1,p=2,q=2,c=65535,r=65535~c\\after" #\Escape #\Escape)
                         (format nil "~c_Ga=d,d=r,x=1,y=~a~c\\ shown" #\Escape big #\Escape))
      (with-emulator (e "sh" (list "-c" (format nil "cat '~a'; sleep 30" file)) :columns 80 :rows 24
                     :pictures t :cell-width 10 :cell-height 20)
        (true (find-if (lambda (r) (search "shown" r)) (emulator-screen e :until (screen-has "shown"))))
        (true (tuatara.defaults::emulator-alive-p e))))))
