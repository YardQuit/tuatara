;;;; test/compile.lisp --- Running a build or test command (M-x compile) and going from place to place (M-g n, M-g p).
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test compiling
  :description "M-x compile runs a command from the project's root, in a session of its own, its output shown as it comes in *compile*, kept to *compile-output-most*, with how it ended and how long it took; the lines naming a file and line are places RET opens, error, warning and note lines in faces of their own; r runs it again, C-c C-k stops all it started; a second waits for the first; only for a project trusted; M-g n and M-g p go through the list of places used last: a compile's, a search of files', a buffer's diagnostics.")

(defun compile-buffer ()
  "The buffer of a compile's output, when there is one."
  (tuatara.buffer:find-buffer "*compile*"))

(defun compile-output ()
  "The text of the compile's output, empty when there is none."
  (let ((b (compile-buffer)))
    (if b (tuatara.buffer:buffer-string b) "")))

(defun compile-running-p ()
  "True while a compile's command runs."
  (let ((b (compile-buffer)))
    (and b (tuatara.runtime:running-job b 'tuatara.defaults::compile-job) t)))

(defun compile-ended-p ()
  "True once the compile has ended, all its output in, and said how at its end."
  (let ((b (compile-buffer)))
    (and b (not (compile-running-p))
         (not (find-if #'tuatara.defaults::compile-job-p tuatara.runtime:*jobs*))
         (null tuatara.defaults::*compiles-ending*)
         (let* ((size (tuatara.buffer:buffer-size b))
                (tail (tuatara.buffer:buffer-string b :start (max 0 (- size 300)))))
           (or (search "Compile " tail :start2 (if (< size 300) 10 0))
               (search "Could not" tail))))))

(defun shown-buffer-and-line ()
  "The name of the buffer in the selected window, the line of its point (from 0) and the column."
  (let* ((b (tuatara.display:window-buffer (tuatara.command:editor-window tuatara.command:*editor*)))
         (point (tuatara.buffer:buffer-point b)))
    (list (tuatara.buffer:buffer-name b) (tuatara.buffer:buffer-line-number b point) (tuatara.buffer:buffer-column b point))))

(defvar *places-reached* '()
  "The places a session's steps waited for and reached, the latest first (REACHED).")

(defun reached (place)
  "A step waiting until the selected window shows PLACE, (BUFFER-NAME LINE COLUMN), noted in *PLACES-REACHED* once it does."
  (list :until (lambda () (when (equal place (shown-buffer-and-line))
                            (push place *places-reached*)
                            t))))

(defun said (words)
  "A step waiting until the echo area says something holding WORDS, noted in *PLACES-REACHED* as WORDS once it does."
  (list :until (lambda () (let ((message (tuatara.command:editor-message tuatara.command:*editor*)))
                            (when (and message (search words message))
                              (push words *places-reached*)
                              t)))))

(defun row-shown-in-face (frontend text face)
  "True when some frame FRONTEND painted had a row holding TEXT with some of it in FACE."
  (loop for screen in (tuatara.frontend.fake:fake-history frontend)
        for (spans) in (tuatara.frontend.fake:fake-face-history frontend)
          thereis (loop for row across screen
                        for row-spans across spans
                          thereis (and (search text row)
                                       (loop for (nil nil f) in row-spans
                                               thereis (member face (if (listp f) f (list f))))))))

(defun compile-project (dir script)
  "Make DIR a project (.git) holding a.c, sub/b.txt and out.sh, SCRIPT, the commands the tests run."
  (ensure-directories-exist (merge-pathnames ".git/" dir))
  (ensure-directories-exist (merge-pathnames "sub/" dir))
  (alexandria:write-string-into-file (format nil "int one;~%int two = bad;~%int three;~%") (merge-pathnames "a.c" dir))
  (alexandria:write-string-into-file (format nil "beta first~%beta second~%") (merge-pathnames "sub/b.txt" dir))
  (alexandria:write-string-into-file script (merge-pathnames "out.sh" dir)))

(defun compile-steps (dir &rest steps)
  "The steps of a session that visits a.c in DIR, runs sh out.sh with M-x compile, then takes STEPS."
  (list* (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (namestring (merge-pathnames "a.c" dir))))
         "M-x c o m p i l e RET RET"
         steps))

(defmacro with-compile-session ((dir script &key (trusted t)) &body body)
  "Run BODY with DIR a new project holding SCRIPT as out.sh (COMPILE-PROJECT),
trusted unless TRUSTED is NIL, sh out.sh the command offered, no places in
force; *compile* killed afterwards."
  `(with-test-directory (,dir)
     (compile-project ,dir ,script)
     (let ((tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*lsp-trusted-projects* (if ,trusted (list (namestring ,dir)) '()))
           (tuatara.defaults::*compile-history* (list "sh out.sh"))
           (tuatara.defaults::*places-in-force* nil)
           (*places-reached* '()))
       (unwind-protect (progn ,@body)
         (when (compile-buffer) (tuatara.buffer:kill-buffer (compile-buffer)))))))

(defun pid-alive-p (pid)
  "True when the process numbered PID is there and not a zombie."
  (let ((stat (ignore-errors (uiop:read-file-string (format nil "/proc/~d/stat" pid)))))
    (and stat (not (search ") Z " stat)))))

(defun numbers-after (word text)
  "The whole number after WORD and a space in TEXT, NIL when there is none."
  (let ((at (search word text)))
    (and at (parse-integer text :start (+ at (length word) 1) :junk-allowed t))))

(define-test (compiling lines-name-places)
  ;; what each kind of line names: its kind, file, line and column
  (flet ((place (line &key before (exists (lambda (file) (member file '("notes/todo.org" "x.lisp" "Makefile") :test #'string=))))
           (multiple-value-list (tuatara.defaults::output-line-place line :kind-before before :exists exists))))
    (is equal '(:error "src/a.c" 12 5) (place "src/a.c:12:5: error: expected ';' before '}' token"))
    (is equal '(:error "a.c" 3 nil) (place "a.c:3: fatal error: x.h: No such file or directory"))
    (is equal '(:warning "lib/b.cpp" 7 1) (place "lib/b.cpp:7:1: warning: unused variable 'x' [-Wunused-variable]"))
    (is equal '(:note "a.c" 2 9) (place "a.c:2:9: note: declared here"))
    ;; rustc: the kind of the line above
    (is equal '(:error) (place "error[E0308]: mismatched types"))
    (is equal '(:warning) (place "warning: unused variable: `x`"))
    (is equal '(:warning "src/main.rs" 4 9) (place "  --> src/main.rs:4:9" :before :warning))
    (is equal '(:error "src/main.rs" 4 9) (place "  --> src/main.rs:4:9"))
    ;; Python's traceback, and the error under it; <string> and <frozen ...> name no file
    (is equal '(:error "/tmp/x/run.py" 10 nil) (place "  File \"/tmp/x/run.py\", line 10, in <module>"))
    (is equal '(nil) (place "  File \"<frozen importlib._bootstrap>\", line 241, in _call"))
    (is equal '(nil) (place "  File \"<string>\", line 1, in <module>"))
    (is equal '(:error) (place "ValueError: invalid literal"))
    ;; grep -n, no kind: a place when the file is there, Makefile too
    (is equal '(nil "notes/todo.org" 42 nil) (place "notes/todo.org:42:* TODO call"))
    (is equal '(nil "x.lisp" 3 14) (place "x.lisp:3:14: something"))
    (is equal '(nil "Makefile" 12 nil) (place "Makefile:12: *** missing separator"))
    (is equal '(nil) (place "other.lisp:3: not there"))
    (is equal '(nil) (place "x.lisp:3: no way to tell" :exists nil))
    ;; SBCL's compiler notes and make's failures: a kind, no place
    (is equal '(:warning) (place "; caught WARNING:"))
    (is equal '(:note) (place "; caught STYLE-WARNING:"))
    (is equal '(:error) (place "make: *** [Makefile:3: all] Error 1"))
    (is equal '(:error) (place "make: *** No rule to make target 'x'.  Stop."))
    (is equal '(:error) (place "*** No targets specified and no makefile found.  Stop."))
    ;; no place: a time, a date, an address and port, a web address, words, an empty line
    (is equal '(nil) (place "12:30:45: started"))
    (is equal '(nil) (place "2018-08-11T13:42:35Z,2018-08-11T13:55:01Z,1533995701,The quick brown fox"))
    (is equal '(nil) (place "10.0.0.1:8080: connection refused"))
    (is equal '(nil) (place "see https://example.org:8080/x"))
    (is equal '(nil) (place "a.c: In function 'main':"))
    (is equal '(nil) (place "y"))
    (is equal '(nil) (place ""))
    ;; a line number of more than nine digits names no place
    (is equal '(nil) (place "a.c:12345678901234567890:1: error: far"))
    ;; make going into a directory and out of it
    (let ((in "make[1]: Entering directory '/tmp/x/sub'") (out "make[1]: Leaving directory '/tmp/x/sub'"))
      (is equal '(:entering "/tmp/x/sub") (multiple-value-list (tuatara.defaults::make-directory-line in (length in))))
      (is equal '(:leaving "/tmp/x/sub") (multiple-value-list (tuatara.defaults::make-directory-line out (length out)))))))

(define-test (compiling test-failures-name-places)
  ;; a Rust test failing names where it panicked, as cargo test prints it now and before 1.73;
  ;; tsc, C# and MSVC name a place as file(line,col): error
  (flet ((place (line) (multiple-value-list (tuatara.defaults::output-line-place line :exists (constantly nil)))))
    (is equal '(:error "src/tide.rs" 12 9) (place "thread 'tide::tests::rises' panicked at src/tide.rs:12:9:"))
    (is equal '(:error "src/main.rs" 4 37)
        (place "thread 'main' panicked at 'called `Option::unwrap()` on a `None` value', src/main.rs:4:37"))
    (is equal '(:error "tests/it's.rs" 3 5) (place "thread 'it' panicked at tests/it's.rs:3:5:"))
    (is equal '(:error "src/app.ts" 3 7) (place "src/app.ts(3,7): error TS2322: Type 'string' is not assignable to type 'number'."))
    (is equal '(:error "Program.cs" 5 13)
        (place "Program.cs(5,13): error CS0103: The name 'x' does not exist in the current context [/tmp/p/p.csproj]"))
    (is equal '(:warning "a.cpp" 10 nil) (place "a.cpp(10): warning C4996: 'strcpy': This function or variable may be unsafe."))
    (is equal '(:error "C:\\src\\b.cpp" 7 nil) (place "C:\\src\\b.cpp(7) : error C2065: 'x': undeclared identifier"))
    ;; words of the same shape that name no place
    (is equal '(nil) (place "thread 'main' has overflowed its stack"))
    (is equal '(nil) (place "f(3): errors are words here"))
    (is equal '(nil) (place "see (3,7): error"))
    (is equal '(nil) (place "worker(42): error: the queue is full"))
    (is equal '(:error "src/a.ts" 2 1) (place "src/a.ts(2,1):error TS1005: ';' expected."))
    (is equal '(:warning "b.cpp" 4 nil) (place "b.cpp(4) :warning C4101: 'y': unreferenced local variable"))))

(define-test (compiling panic-reached-by-next-place)
  ;; M-g n goes to where a Rust test panicked
  (with-compile-session (dir (format nil "printf 'running 1 test\\n'~%printf \"thread 'tide::tests::rises' panicked at sub/b.txt:2:6:\\n\"~%printf 'assertion failed\\n'~%exit 101~%"))
    (auto-save-session (compile-steps dir
                                      (list :until #'compile-ended-p)
                                      "M-g n" (reached '("b.txt" 1 5))
                                      "M-g n" (said "No more places in *compile*"))
                       :width 100 :height 30)
    (is equal '(("b.txt" 1 5) "No more places in *compile*") (reverse *places-reached*))))

(define-test (compiling output-shown-and-places-visited)
  ;; the output in *compile*, how it ended; M-g n and M-g p go through its places, opening each
  ;; at its line and column; error and warning lines in their faces
  (with-compile-session (dir (format nil "printf 'a.c:2:11: error: bad is not declared\\n'~%printf 'warning: unused thing\\n'~%printf '  --> sub/b.txt:2:6\\n'~%printf 'plain words\\n'~%exit 3~%"))
    (let ((f (auto-save-session
              (compile-steps dir
                             (list :until #'compile-ended-p)
                             "M-g n" (reached '("a.c" 1 10))
                             "M-g n" (reached '("b.txt" 1 5))
                             "M-g n" (said "No more places in *compile*")
                             "M-g p" (reached '("a.c" 1 10)))
              :width 100 :height 30)))
      (is equal '(("a.c" 1 10) ("b.txt" 1 5) "No more places in *compile*" ("a.c" 1 10)) (reverse *places-reached*))
      (true (search "sh out.sh" (compile-output)))
      (true (search "a.c:2:11: error: bad is not declared" (compile-output)))
      (true (search "Compile failed, exit code 3, after" (compile-output)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Compile failed, exit code 3"))
      (true (row-shown-in-face f "error: bad is not declared" :compile-error))
      (true (row-shown-in-face f "warning: unused thing" :compile-warning))
      (true (row-shown-in-face f "--> sub/b.txt:2:6" :compile-warning))
      (false (row-shown-in-face f "plain words" :compile-error))
      (true (row-shown-in-face f "Compile failed, exit code 3" :compile-error)))))

(define-test (compiling run-only-when-trusted)
  ;; a project not trusted: asked first, the question saying all a yes allows, and with no nothing runs
  (with-compile-session (dir (format nil "touch ran~%") :trusted nil)
    (let ((f (auto-save-session (compile-steps dir (list :wait 0.1) "n RET" (said "Not run:")) :width 200)))
      (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "its language servers start by themselves"))
      (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "git runs the programs it names"))
      (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "its build commands run"))
      (false (probe-file (merge-pathnames "ran" dir)))
      (false (compile-running-p)))))

(define-test (compiling output-as-it-comes-and-stopped)
  ;; what the command prints is shown while it runs, the editor answering keys meanwhile;
  ;; C-c C-k in its output stops it, and every program it started, one that will not end killed
  (with-compile-session (dir (format nil "printf 'first line\\n'~%sh -c 'trap \"\" TERM; echo child $$; exec sleep 30' &~%trap '' TERM~%sleep 30~%printf 'second line\\n'~%"))
    (let ((shown-while-running nil)
          (began (get-internal-real-time)))
      (let ((f (auto-save-session
                (compile-steps dir
                               (list :until (lambda () (and (search "child " (compile-output)) (compile-running-p)
                                                            (setf shown-while-running t))))
                               ;; keys answered while it runs
                               "M-> i n t SPC f o u r ;"
                               "C-x o C-c C-k"
                               (list :until #'compile-ended-p)
                               ;; the child that will not end is killed once the grace is over
                               (list :until (lambda () (let ((child (numbers-after "child" (compile-output))))
                                                         (and child (not (pid-alive-p child)))))))
                :width 100 :height 30)))
        (let ((child (numbers-after "child" (compile-output))))
          (true shown-while-running)
          (true (tuatara.frontend.fake:fake-frame-showing f "first line"))
          (true (search "int four;" (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "a.c"))))
          (true (search "Compile stopped" (compile-output)))
          (false (search "held its output open" (compile-output)))
          (false (search "second line" (compile-output)))
          (true child)
          (false (and child (pid-alive-p child)))
          (true (< (- (get-internal-real-time) began) (* 9 internal-time-units-per-second))))))))

(define-test (compiling session-of-its-own)
  ;; the command runs in a session of its own, so that nothing it runs can reach the editor's
  ;; terminal
  (with-compile-session (dir (format nil "echo session $(cut -d' ' -f6 /proc/$$/stat)~%"))
    (auto-save-session (compile-steps dir (list :until #'compile-ended-p)) :width 100 :height 30)
    (let* ((session (numbers-after "session" (compile-output)))
           (stat (uiop:read-file-string "/proc/self/stat"))
           (ours (parse-integer (fourth (uiop:split-string (subseq stat (+ 2 (position #\) stat :from-end t))) :separator " "))
                                :junk-allowed t)))
      (true session)
      (false (eql session ours)))))

(define-test (compiling ends-when-its-command-does)
  ;; a program the command leaves running with the output open does not keep the compile going:
  ;; it ends soon after the command, saying so
  (with-compile-session (dir (format nil "sleep 30 &~%echo left $!~%"))
    (let ((began (get-internal-real-time)))
      (unwind-protect
           (progn
             (auto-save-session (compile-steps dir (list :until #'compile-ended-p)) :width 100 :height 30)
             (true (< (- (get-internal-real-time) began) (* 8 internal-time-units-per-second)))
             (true (search "Compile finished, exit code 0" (compile-output)))
             (true (search "held its output open" (compile-output))))
        (let ((left (numbers-after "left" (compile-output))))
          (when left (ignore-errors (sb-posix:kill left sb-posix:sigkill))))))))

(define-test (compiling one-at-a-time)
  ;; a second compile asked for while one runs waits for it to have ended before it starts
  (with-compile-session (dir (format nil "if [ -e second ]; then test -e first-gone && echo after-the-first || echo before-the-first; exit 0; fi~%trap 'touch first-gone; exit 1' TERM~%echo first~%sleep 30 & wait~%"))
    (auto-save-session
     (compile-steps dir
                    (list :until (lambda () (search (format nil "~%first~%") (compile-output))))
                    (lambda () (alexandria:write-string-into-file "" (merge-pathnames "second" dir)))
                    "M-x c o m p i l e RET RET y RET"
                    (list :until (lambda () (and (search "-the-first" (compile-output)) (compile-ended-p)))))
     :width 100 :height 30)
    (true (search "after-the-first" (compile-output)))
    (false (search "before-the-first" (compile-output)))))

(define-test (compiling output-kept-whole-and-to-its-most)
  ;; output of a few megabytes, many-octet characters among it, comes into the buffer as it was
  ;; printed; past *compile-output-most* it is left out, and the end says how much
  (with-compile-session (dir (format nil "cat big.txt~%"))
    (let ((text (with-output-to-string (out)
                  (loop for i below 60000 do (format out "line ~d of the output, kōwhai and tūī ~a~%" i (make-string (mod i 7) :initial-element #\x))))))
      (alexandria:write-string-into-file text (merge-pathnames "big.txt" dir) :external-format :utf-8)
      (auto-save-session (compile-steps dir (list :until #'compile-ended-p)) :width 100 :height 30)
      (let* ((output (compile-output))
             (start (search "line 0 of" output)))
        (true start)
        (is string= text (subseq output start (min (length output) (+ start (length text))))))
      (let ((tuatara-user:*compile-output-most* 100000))
        (auto-save-session (compile-steps dir (list :until #'compile-ended-p)) :width 100 :height 30)
        (true (< (tuatara.buffer:buffer-size (compile-buffer)) (+ 100000 70000)))
        (true (search "more output left out" (compile-output)))))))

(define-test (compiling again-and-opened-from-its-line)
  ;; r in the output runs the command again, in the same directory; RET on a line naming a
  ;; place opens it in the other window
  (with-compile-session (dir (format nil "echo once >> count~%echo 'sub/b.txt:2: beta second'~%"))
    (flet ((runs () (let ((count (probe-file (merge-pathnames "count" dir))))
                      (if count (count #\Newline (alexandria:read-file-into-string count)) 0))))
      (auto-save-session
       (compile-steps dir
                      (list :until #'compile-ended-p)
                      "C-x o r" (list :until (lambda () (and (= 2 (runs)) (compile-ended-p))))
                      "M-< C-n C-n C-n RET" (reached '("b.txt" 1 0)))
       :width 100 :height 30)
      (is = 2 (runs))
      (is equal '(("b.txt" 1 0)) *places-reached*))))

(define-test (compiling places-where-make-went)
  ;; the names after make's Entering directory are taken from there; a Makefile is a place
  ;; once it is there
  (with-compile-session (dir (format nil "echo \"make[1]: Entering directory '$(pwd)/sub'\"~%echo 'b.txt:2: beta second'~%echo \"make[1]: Leaving directory '$(pwd)/sub'\"~%echo 'Makefile:1: here'~%"))
    (alexandria:write-string-into-file (format nil "all:~%") (merge-pathnames "Makefile" dir))
    (auto-save-session
     (compile-steps dir (list :until #'compile-ended-p)
                    "M-g n" (reached '("b.txt" 1 0))
                    "M-g n" (reached '("Makefile" 0 0)))
     :width 100 :height 30)
    (is equal '(("b.txt" 1 0) ("Makefile" 0 0)) (reverse *places-reached*))))

(define-test (compiling places-in-the-file-typed-in-keep-insert-state)
  ;; with modal editing, M-g n to a place in the file being typed in leaves it in INSERT
  ;; state, as going to another place in it does
  (with-compile-session (dir (format nil "echo 'a.c:1: one'~%echo 'a.c:3: three'~%"))
    (let ((tuatara.defaults::*modal-editing* t) (state nil))
      (auto-save-session
       (compile-steps dir (list :until #'compile-ended-p)
                      "M-g n" (reached '("a.c" 0 0))
                      "i" "M-g n" (reached '("a.c" 2 0))
                      (lambda () (setf state (tuatara.defaults::modal-state
                                              (tuatara.buffer:find-buffer "a.c")))))
       :width 100 :height 30)
      (is eq :insert state))))

(define-test (compiling killed-output-no-places)
  ;; killing *compile* drops its places from those in force
  (with-compile-session (dir (format nil "echo 'a.c:1: x'~%"))
    (auto-save-session (compile-steps dir (list :until #'compile-ended-p)) :width 100 :height 30)
    (true (typep tuatara.defaults::*places-in-force* 'tuatara.defaults::compile-run))
    (tuatara.buffer:kill-buffer (compile-buffer))
    (false tuatara.defaults::*places-in-force*)))

(define-test (compiling file-line-shown-within-it)
  ;; a place opens on its line, at a character's start, never past the line's end; a file no
  ;; longer there is said and not made
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "k.txt" dir)))
          (gone (namestring (merge-pathnames "gone.txt" dir)))
          (tuatara.defaults::*modal-editing* nil)
          (*places-reached* '()))
      (alexandria:write-string-into-file (format nil "first~%kōwhai tūī~%third~%") file :external-format :utf-8)
      (auto-save-session
       (list (lambda () (tuatara.defaults::show-file-line file 2 :offset 4)) (reached '("k.txt" 1 3))
             (lambda () (tuatara.defaults::show-file-line file 2 :offset 400)) (reached '("k.txt" 1 10))
             (lambda () (tuatara.defaults::show-file-line file 2 :column 8)) (reached '("k.txt" 1 7))
             (lambda () (tuatara.defaults::show-file-line gone 1)) (said "is not there"))
       :width 100 :height 20)
      (is equal '(("k.txt" 1 3) ("k.txt" 1 10) ("k.txt" 1 7) "is not there") (reverse *places-reached*))
      (false (tuatara.buffer:find-buffer "gone.txt"))
      (false (probe-file gone))
      (kill-buffers-visiting dir))))

(define-test (compiling found-lines-are-places)
  ;; after a search of files, M-g n and M-g p go on to the lines it found after and before the
  ;; one chosen, as choosing them does
  (with-test-directory (dir)
    (compile-project dir "")
    (let ((tuatara.defaults::*modal-editing* nil)
          (tuatara.defaults::*places-in-force* nil)
          (*places-reached* '()))
      (unwind-protect
           (auto-save-session
            (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (namestring (merge-pathnames "a.c" dir))))
                  "M-s g b e t a RET" (reached '("b.txt" 0 0))
                  "M-g n" (reached '("b.txt" 1 0))
                  "M-g p" (reached '("b.txt" 0 0)))
            :width 100 :height 20)
        (kill-buffers-visiting dir))
      (is equal '(("b.txt" 0 0) ("b.txt" 1 0) ("b.txt" 0 0)) (reverse *places-reached*)))))

(defun lines-diagnosed ()
  "Once d.txt is visited, give it a diagnostic on its lines 1, 3 and 5 (from
0), as a language server would; true then."
  (let ((b (tuatara.buffer:find-buffer "d.txt")))
    (when b
      (setf (tuatara.buffer:buffer-local b 'tuatara.defaults::lsp-diagnostics)
            (list (cons :one (tuatara.defaults::make-placed
                              (tuatara.buffer:buffer-version b)
                              (loop for line in '(1 3 5)
                                    for at = (tuatara.buffer:buffer-goto-line b line)
                                    collect (tuatara.defaults::make-diagnostic at (+ at 2) 1 "wrong" "test"))))))
      t)))

(define-test (compiling diagnostics-are-places)
  ;; once a language server's diagnostic has been gone to, M-g n and M-g p go through that
  ;; buffer's diagnostics, each press one further
  (with-test-directory (dir)
    (let ((file (merge-pathnames "d.txt" dir))
          (tuatara.defaults::*modal-editing* nil)
          (tuatara.defaults::*places-in-force* nil)
          (*places-reached* '()))
      (alexandria:write-string-into-file (format nil "~{line ~d~%~}" (loop for i below 8 collect i)) file)
      (unwind-protect
           (auto-save-session
            (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (namestring file)))
                  (list :until #'lines-diagnosed)
                  "M-< M-x l s p - n e x t - d i a g n o s t i c RET" (reached '("d.txt" 1 0))
                  "M-g n" (reached '("d.txt" 3 0))
                  "M-g n" (reached '("d.txt" 5 0))
                  "M-g p" (reached '("d.txt" 3 0))
                  "M-g p" (reached '("d.txt" 1 0)))
            :width 100 :height 20)
        (kill-buffers-visiting dir))
      (is equal '(("d.txt" 1 0) ("d.txt" 3 0) ("d.txt" 5 0) ("d.txt" 3 0) ("d.txt" 1 0)) (reverse *places-reached*)))))

(define-test (compiling ends-of-places-said-alike)
  ;; at either end of a list of places, M-g n and M-g p say so in the same words, naming the
  ;; list: a search of files' lines, a buffer's diagnostics (a compile's: output-shown-and-places-visited)
  (with-test-directory (dir)
    (compile-project dir "")
    (let ((tuatara.defaults::*modal-editing* nil)
          (tuatara.defaults::*places-in-force* nil)
          (*places-reached* '()))
      (unwind-protect
           (auto-save-session
            (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (namestring (merge-pathnames "a.c" dir))))
                  "M-s g b e t a RET" (reached '("b.txt" 0 0))
                  "M-g p" (said "No place before this one in the search for beta")
                  "M-g n" (reached '("b.txt" 1 0))
                  "M-g n" (said "No more places in the search for beta"))
            :width 100 :height 20)
        (kill-buffers-visiting dir))
      (is equal '(("b.txt" 0 0) "No place before this one in the search for beta" ("b.txt" 1 0)
                  "No more places in the search for beta")
          (reverse *places-reached*))))
  (with-test-directory (dir)
    (let ((file (merge-pathnames "d.txt" dir))
          (tuatara.defaults::*modal-editing* nil)
          (tuatara.defaults::*places-in-force* nil)
          (*places-reached* '()))
      (alexandria:write-string-into-file (format nil "~{line ~d~%~}" (loop for i below 8 collect i)) file)
      (unwind-protect
           (auto-save-session
            (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (namestring file)))
                  (list :until #'lines-diagnosed)
                  "M-< M-x l s p - n e x t - d i a g n o s t i c RET" (reached '("d.txt" 1 0))
                  "M-g p" (said "No place before this one in the diagnostics of d.txt")
                  "M-g n M-g n" (reached '("d.txt" 5 0))
                  "M-g n" (said "No more places in the diagnostics of d.txt"))
            :width 100 :height 20)
        (kill-buffers-visiting dir))
      (is equal '(("d.txt" 1 0) "No place before this one in the diagnostics of d.txt" ("d.txt" 5 0)
                  "No more places in the diagnostics of d.txt")
          (reverse *places-reached*)))))

(defun kill-pids-after (word text)
  "Kill (SIGKILL) the process whose number follows WORD in TEXT, a test's leftover, when there is one."
  (let ((pid (numbers-after word text)))
    (when pid (ignore-errors (sb-posix:kill pid sb-posix:sigkill)))))

(define-test (compiling ends-though-a-left-program-writes)
  ;; a program the command leaves behind writing on for ever does not keep the compile going
  (with-compile-session (dir (format nil "(while :; do echo x; sleep 0.05; done) &~%echo writer $!~%"))
    (let ((began (get-internal-real-time)))
      (unwind-protect
           (progn
             (auto-save-session (compile-steps dir (list :until #'compile-ended-p)) :width 100 :height 30)
             (true (< (- (get-internal-real-time) began) (* 9 internal-time-units-per-second)))
             (true (search "held its output open" (compile-output))))
        (kill-pids-after "writer" (compile-output))))))

(define-test (compiling stopped-though-an-outside-program-writes)
  ;; stopped, a compile ends with its own session, whatever outside it still writes to the output;
  ;; the compile asked for after it then runs
  (with-compile-session (dir (format nil "if [ -e second ]; then echo the-second-ran; exit 0; fi~%setsid sh -c 'echo writer $$; while :; do echo x; sleep 0.05; done' &~%sleep 30~%"))
    (let ((began (get-internal-real-time)) (first-output "")
          (marker (format nil "stopped-though-an-outside-program-writes begins ~d" (get-universal-time))))
      ;; a full *Messages* drops its oldest lines as new ones come, so what the compile said is
      ;; found after a line of the test's own, not after the size the log had
      (loop repeat tuatara.command:*message-log-max* for n from 0
            do (tuatara.command:log-message (format nil "filler ~d ~a" n (make-string 200 :initial-element #\x))))
      (tuatara.command:log-message marker)
      (unwind-protect
           (progn
             (auto-save-session
              (compile-steps dir
                             (list :until (lambda () (setf first-output (if (search "writer " (compile-output)) (compile-output) ""))
                                            (plusp (length first-output))))
                             (lambda () (alexandria:write-string-into-file "" (merge-pathnames "second" dir)))
                             "M-x c o m p i l e RET RET y RET"
                             (list :until (lambda () (and (search "the-second-ran" (compile-output)) (compile-ended-p)))))
              :width 100 :height 30)
             (true (search "the-second-ran" (compile-output)))
             (true (< (- (get-internal-real-time) began) (* 9 internal-time-units-per-second)))
             ;; the first said it was stopped, not that something was left running
             (let* ((log (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Messages*")))
                    (said (subseq log (or (search marker log :from-end t) 0))))
               (true (search marker log))
               (true (search "Compile stopped" said))
               (false (search "left running" said))))
        (kill-pids-after "writer" first-output)))))

(define-test (compiling killed-on-leaving)
  ;; leaving the editor while a compile is being stopped kills at once what will not end
  (with-compile-session (dir (format nil "sh -c 'trap \"\" TERM; echo child $$; exec sleep 30' &~%trap '' TERM~%sleep 30~%"))
    (let ((child nil) (gone-at nil) (began nil))
      (auto-save-session
       (compile-steps dir
                      (list :until (lambda () (setf child (numbers-after "child" (compile-output)))))
                      "C-x o C-c C-k"
                      (lambda () (setf began (get-internal-real-time)) (tuatara.defaults::kill-compiles-at-exit nil))
                      (list :until (lambda () (when (not (pid-alive-p child)) (setf gone-at (get-internal-real-time)))))
                      (list :until #'compile-ended-p))
       :width 100 :height 30)
      (true child)
      (true gone-at)
      ;; well before the stop's own grace of 2 seconds is over
      (true (and gone-at began (< (- gone-at began) (* internal-time-units-per-second (machine-slowdown))))))))

(define-test (compiling stopped-whatever-its-name)
  ;; a program of the compile whose name is not UTF-8 is found and stopped with the rest
  (with-compile-session (dir (format nil "cp \"$(command -v sleep)\" \"$(printf 'sl\\377p')\"~%trap '' TERM~%\"./$(printf 'sl\\377p')\" 30 &~%echo odd $!~%sleep 0.2~%rm \"$(printf 'sl\\377p')\"~%wait~%"))
    (let ((odd nil) (stopped-at nil) (gone-at nil))
      (unwind-protect
           (progn
             (auto-save-session
              (compile-steps dir
                             (list :until (lambda () (setf odd (numbers-after "odd" (compile-output)))))
                             "C-x o C-c C-k"
                             (lambda () (setf stopped-at (get-internal-real-time)))
                             (list :until (lambda () (unless (pid-alive-p odd) (setf gone-at (get-internal-real-time)))))
                             (list :until #'compile-ended-p))
              :width 100 :height 30)
             (true odd)
             ;; killed once the stop's grace is over, not left to end by itself
             (true (and gone-at stopped-at (< (- gone-at stopped-at) (* 5 internal-time-units-per-second)))))
        (when odd (ignore-errors (sb-posix:kill odd sb-posix:sigkill)))))))

(define-test (compiling small-pieces-joined)
  ;; output come in many small writes (10,000 lines of 99 octets) goes into the buffer as few
  ;; pieces of text, not one for each
  (let ((b (tuatara.buffer:make-buffer "*compile-pieces*" :text "heading"))
        (leaves 0))
    (unwind-protect
         (let ((pieces (loop for i below 10000
                             collect (tuatara.text:string-octets (format nil "~98,,,'-a~%" i)))))
           (auto-save-session (list (lambda () (tuatara.defaults::add-compile-output b pieces))) :width 100 :height 20)
           (is = (+ 7 (* 99 10000)) (tuatara.buffer:buffer-size b))
           (tuatara.text::map-leaves (lambda (leaf) (declare (ignore leaf)) (incf leaves)) (tuatara.buffer:buffer-text b))
           (true (< leaves 400) "~d pieces of text for 1 MB" leaves))
      (tuatara.buffer:kill-buffer b))))

(define-test (compiling waiting-compile-said-when-killed)
  ;; killing *compile* while a second compile waits for the first says that it did not run
  (with-compile-session (dir (format nil "echo first~%sleep 30~%"))
    (let ((f (auto-save-session
              (compile-steps dir
                             (list :until (lambda () (search (format nil "~%first~%") (compile-output))))
                             "M-x c o m p i l e RET RET y RET"
                             (lambda () (tuatara.buffer:kill-buffer (compile-buffer)))
                             (said "was killed"))
              :width 100 :height 30)))
      (declare (ignore f))
      (is equal '("was killed") *places-reached*))))

(define-test (compiling setsid-missing-said)
  ;; without setsid the compile still runs, and says once that it has no session of its own
  (with-compile-session (dir (format nil "echo ran~%"))
    (let ((tuatara.defaults::*setsid-missing-said* nil)
          (find (fdefinition 'tuatara.io:find-program)))
      (with-functions-replaced ((tuatara.io:find-program (lambda (name &rest more)
                                                            (unless (string= name "setsid") (apply find name more)))))
        (auto-save-session (compile-steps dir (said "setsid is not installed") (list :until #'compile-ended-p))
                           :width 100 :height 30))
      (is equal '("setsid is not installed") *places-reached*)
      (true (search (format nil "~%ran~%") (compile-output))))))

(defun output-as-a-terminal-shows-it (pieces)
  "The text a compile's buffer holds after the output PIECES (strings) come
in, one after another, through OUTPUT-PAST-RETURNS."
  (let ((returns (tuatara.defaults::make-output-returns)) (shown (tuatara.text:make-octets 0)))
    (dolist (piece pieces (tuatara.text:octets-string shown))
      (multiple-value-bind (octets back) (tuatara.defaults::output-past-returns returns (tuatara.text:string-octets piece))
        (setf shown (concatenate 'tuatara.text:octets (subseq shown 0 (- (length shown) back)) octets))))))

(define-test (compiling progress-redrawn-on-one-line)
  ;; a carriage return alone in a program's output goes back to the line's start and what
  ;; follows writes over it, as a terminal shows it, however the output comes in pieces
  (let ((output (format nil "start~%10%~c50%~c100%~%5%~cdone~c~%é中~c😀~cX" #\Return #\Return #\Return #\Return #\Return #\Return)))
    (is string= (format nil "start~%100%~%done~c~%X中" #\Return)
        (output-as-a-terminal-shows-it (list output)))
    (dolist (size '(1 2 3 5))
      (is string= (output-as-a-terminal-shows-it (list output))
          (output-as-a-terminal-shows-it (loop for i from 0 below (length output) by size
                                               collect (subseq output i (min (length output) (+ i size)))))
          "in pieces of ~d" size)))
  ;; X written over the first character after the last return: shorter text leaves the rest
  (is string= "X00%" (output-as-a-terminal-shows-it (list (format nil "100%~cX" #\Return))))
  ;; a line too long to follow goes in as it came to its newline, and what follows is
  ;; followed again; a carriage return making one too long gives it up too
  (let ((tuatara.defaults::*compile-return-line-most* 4))
    (is string= (format nil "abcdefABC~%Zy") (output-as-a-terminal-shows-it (list "abcdefAB" (format nil "C~%xy~c" #\Return) "Z")))
    (is string= (format nil "xyzwv~cQ" #\Return) (output-as-a-terminal-shows-it (list (format nil "ab~cxyzwv" #\Return) (format nil "~cQ" #\Return)))))
  ;; in *compile*: the progress shows as one line, the places after it found where they are
  (with-compile-session (dir (format nil "printf '10%%\\r50%%\\r100%%\\n'~%printf 'a.c:2:11: error: bad\\n'~%"))
    (auto-save-session (compile-steps dir (list :until #'compile-ended-p) "M-g n" (reached '("a.c" 1 10)))
                       :width 100 :height 30)
    (is equal '(("a.c" 1 10)) *places-reached*)
    (true (search (format nil "~%100%~%a.c:2:11: error: bad~%") (compile-output)))
    (false (find #\Return (compile-output)))))
