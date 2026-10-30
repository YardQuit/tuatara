;;;; test/emulator/survives.lisp --- Output a terminal must survive, shown in a process of its own.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Run by the tests of test/emulator.lisp that feed a terminal output that
;;;; could end the process showing it, with sbcl --script: CASE OUT DIRECTORY.
;;;; Starts a terminal from these sources, has its program show what CASE
;;;; names (clear, line-limit or narrowed), written as a file in DIRECTORY,
;;;; and writes to OUT what the terminal shows after, a property list: :text
;;;; the first row of the screen, :bold whether the character the case looks
;;;; at is bold, :link the address of its link, :history the whole of its
;;;; text, :columns the terminal's columns. Nothing is written when the
;;;; process ends before.

(load (merge-pathnames "../../tools/quicklisp.lisp" *load-truename*))
(push (merge-pathnames "../../" (make-pathname :name nil :type nil :defaults *load-truename*)) asdf:*central-registry*)
(ql:quickload :tuatara/defaults :silent t)

(defun first-row (emulator text &optional (seconds 20))
  "The first row of EMULATOR's screen and its looks, once a row of it holds
TEXT or SECONDS have passed."
  (let ((rows (make-array (tuatara.defaults::emulator-rows emulator) :initial-element ""))
        (looks (make-array (tuatara.defaults::emulator-rows emulator) :initial-element '()))
        (deadline (tuatara.runtime:deadline-in seconds)))
    (loop (tuatara.defaults::emulator-frame emulator (lambda (row string spans)
                                                       (when (< row (length rows))
                                                         (setf (aref rows row) string (aref looks row) spans)))
                                            :all t)
          (when (or (some (lambda (row) (search text row)) rows) (<= (tuatara.runtime:seconds-left deadline) 0))
            (return (values (aref rows 0) (aref looks 0))))
          (sleep 1/50))))

(defun bold-at-p (looks column)
  "True when the run of LOOKS (a row's) over COLUMN is bold."
  (some (lambda (span)
          (and (<= (first span) column) (< column (+ (first span) (second span)))
               (member :bold (cddr (tuatara.defaults::look-list (third span))))))
        looks))

(defun write-case-file (file &rest parts)
  "Write FILE of PARTS, each a string of characters below 256 (the octets),
or a list of a count and such a string, written that many times."
  (with-open-file (out file :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (dolist (part parts)
      (loop repeat (if (consp part) (first part) 1)
            do (loop for c across (if (consp part) (second part) part) do (write-byte (char-code c) out))))))

(defun resize-when-free (emulator columns rows)
  "Resize EMULATOR to COLUMNS by ROWS, again while its library is busy."
  (loop repeat 100 until (eq t (tuatara.defaults::emulator-resize emulator columns rows)) do (sleep 1/20)))

(defvar *started* '()
  "The terminals RUN-CASE started, stopped before the process ends.")

(defun run-case (case directory)
  "Show the output CASE names in a terminal; what it shows after, as OUT takes it."
  (let ((esc (string #\Escape)) (file (namestring (merge-pathnames "case.txt" directory))))
    (flet ((run (program &rest keys)
             (car (push (apply #'tuatara.defaults::start-emulator "sh" (list "-c" program) keys) *started*))))
      (cond
        ((string= case "clear")
         ;; screens cleared into the scrollback (CSI 22 J) while the cursor is bold and in a
         ;; link on the oldest page, which a scrollback of no lines lets go
         (apply #'write-case-file file
                (append (loop for lines in (append (loop for n from 4250 below 4280 collect n) '(4263))
                              append (list (format nil "~ac" esc) (list lines (format nil "x~c~c" #\Return #\Newline))
                                           (format nil "x~a[H~:*~a[1m~:*~a]8;;http://example.com~:*~a\\~:*~a[22Jy~:*~a]8;;~:*~a\\~:*~a[0m" esc)))
                        (list (format nil "~c~cshown" #\Return #\Newline))))
         (let ((e (run (format nil "cat '~a'; sleep 30" file) :columns 10 :rows 3 :scrollback-lines 0)))
           (multiple-value-bind (text looks) (first-row e "shown")
             (list :text text :bold (bold-at-p looks 0) :link (tuatara.defaults::emulator-link e 0 0)))))
        ((string= case "line-limit")
         ;; one row and no scrollback: a bold link reflowed wider, then line feeds letting go
         ;; of the page the cursor is on
         (let ((e (run "stty -echo -onlcr; printf '\\033[1m\\033]8;;http://example.com\\033\\\\'; printf 'a%.0s' $(seq 103); read -r go; awk 'BEGIN { for (i = 0; i < 2000; i++) printf \"\\n\" }'; printf b; sleep 30"
                       :columns 3 :rows 1 :scrollback-lines 0)))
           (first-row e "a")
           (sleep 0.5)
           (resize-when-free e 80 1)
           (tuatara.defaults::emulator-input e (format nil "go~c" #\Return))
           (multiple-value-bind (text looks) (first-row e "b")
             (let ((column (or (search "b" text) 0)))
               (list :text text :bold (bold-at-p looks column) :link (tuatara.defaults::emulator-link e column 0))))))
        ((string= case "narrowed")
         ;; a screen 65,535 columns wide made 2 wide and 1 high, its text read rather than
         ;; its frames, which would each draw all 65,535 cells
         (let ((e (run "printf hello; sleep 30" :columns 65535 :rows 1)))
           (flet ((text-shows (text)
                    (let ((deadline (tuatara.runtime:deadline-in 20)))
                      (loop for shown = (tuatara.defaults::emulator-text e)
                            until (or (search text (remove #\Newline shown)) (<= (tuatara.runtime:seconds-left deadline) 0))
                            do (sleep 1/50)
                            finally (return shown)))))
             (text-shows "hello")
             (resize-when-free e 2 1)
             (list :history (text-shows "hello") :columns (tuatara.defaults::emulator-columns e)))))))))

(destructuring-bind (case out directory) (rest sb-ext:*posix-argv*)
  (let ((result (run-case case (uiop:ensure-directory-pathname directory))))
    (mapc #'tuatara.defaults::stop-emulator *started*)
    (with-open-file (o out :direction :output :if-exists :supersede)
      (with-standard-io-syntax (prin1 result o))))
  (sb-ext:exit :code 0 :abort t))
