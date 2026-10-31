;;;; test/grammars.lisp --- Which grammars read text: the common ones, those turned on, the question.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test grammars-on
  :description "Only the grammars on read text: the common ones and those turned on. A file in a
language whose grammar is off asks once whether to turn it on, the answer kept between sessions;
text in such a language inside another file is left plain; a file opened safely asks nothing.")

(defmacro with-grammar-answers-forgotten ((&rest names) &body body)
  "Run BODY with no answer kept for the grammars of the languages NAMES (strings),
none given up in this session, and *GRAMMARS-ON* as it is by default; the same
afterwards."
  `(flet ((forget ()
            (tuatara.defaults::state-list-drop tuatara.defaults::*grammar-answers* (list ,@names))
            (setf tuatara.defaults::*grammar-answers-read* nil
                  tuatara.defaults::*grammars-not-now* '())))
     (forget)
     ;; the grammars loaded first, as a session started earlier would have them
     (tuatara.syntax:rules-helper)
     (dolist (grammar '("awk" "markdown" "markdown_inline" "python")) (tuatara.syntax:find-grammar grammar))
     (let ((tuatara-user:*grammars-on* '(:common))
           (*grammar-questions-wanted* t))
       (unwind-protect (progn ,@body)
         (forget)))))

(defun row-faces (f row)
  "The faces of the spans of ROW in the last frame of the fake frontend F, each once."
  (remove-duplicates (alexandria:flatten (mapcar #'third (aref (tuatara.frontend.fake:fake-spans f) row)))))

(defparameter *awk-in-markdown* (format nil "# Notes~%~%```awk~%BEGIN { print \"x\" }~%```~%")
  "A Markdown file holding a code block in AWK, whose grammar is not a common one.")

(define-test (grammars-on text-inside-left-plain-when-off)
  ;; the AWK in a Markdown code block is coloured by AWK's grammar only once it is on
  (with-grammar-answers-forgotten ("awk")
    (let ((off (row-faces (colour-session "" *awk-in-markdown* "md") 3)))
      (true (member :markup.raw.block off) "the code block is coloured as Markdown's")
      (false (member :namespace off) "AWK's BEGIN not coloured while its grammar is off"))
    (let ((tuatara-user:*grammars-on* '(:common awk)))
      (true (member :namespace (row-faces (colour-session "" *awk-in-markdown* "md") 3))
            "AWK's BEGIN coloured once its grammar is on"))))

(define-test (grammars-on file-asks-and-yes-turns-on)
  ;; a file in AWK asks; y turns the grammar on, the file is coloured, and the
  ;; answer is kept: the next file in AWK asks nothing
  (with-grammar-answers-forgotten ("awk")
    (with-file-session ((f editor) "y" (format nil "BEGIN { print 1 }~%") "awk")
      (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "Turn on the AWK grammar"))
      (true (member :keyword (row-faces f 0)) "the file is coloured once its grammar is on"))
    (is eq :on (gethash "awk" (tuatara.defaults::grammar-answers :again t)))
    (with-file-session ((f editor) "" (format nil "BEGIN { print 1 }~%") "awk")
      (false (tuatara.frontend.fake:fake-frame-showing-across-rows f "Turn on the AWK grammar") "asked once only")
      (true (member :keyword (row-faces f 0))))))

(define-test (grammars-on file-asks-and-no-says-how-later)
  ;; n keeps the grammar off, says how to turn it on later, and is kept: the
  ;; next file asks nothing; turn-on-grammar turns it on after all
  (with-grammar-answers-forgotten ("awk")
    (with-file-session ((f editor) "n" (format nil "BEGIN { print 1 }~%") "awk")
      (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "Turn on the AWK grammar"))
      (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "The AWK grammar stays off: M-x turn-on-grammar turns it on"))
      (false (member :keyword (row-faces f 0)) "not coloured while its grammar is off"))
    (with-file-session ((f editor) (format nil "M-x ~a RET RET" (key-string "turn-on-grammar"))
                        (format nil "BEGIN { print 1 }~%") "awk")
      (false (tuatara.frontend.fake:fake-frame-showing-across-rows f "Turn on the AWK grammar") "asked once only")
      (true (tuatara.frontend.fake:fake-frame-showing f "The AWK grammar is on"))
      (true (member :keyword (row-faces f 0)) "coloured once turned on by the command"))))

(define-test (grammars-on common-grammar-asks-nothing)
  ;; a file in a common language is parsed as it opens, without a question
  (with-grammar-answers-forgotten ()
    (with-file-session ((f editor) "" (format nil "def f(x):~%    return x~%") "py")
      (false (tuatara.frontend.fake:fake-frame-showing f "grammar?"))
      (true (row-faces f 0)))))

(define-test (grammars-on safe-visit-asks-nothing)
  ;; a file opened safely is never parsed, and asks nothing
  (with-grammar-answers-forgotten ("awk")
    (with-test-directory (directory)
      (let ((path (merge-pathnames "evidence.awk" directory)))
        (with-open-file (o path :direction :output) (format o "BEGIN { print 1 }~%"))
        (let ((tuatara-user:*safe-directories* (list (namestring directory))))
          (multiple-value-bind (f editor) (run-keys "" :file path :width 60)
            (unwind-protect
                 (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
                   (true (tuatara.io:safe-visit-p buffer))
                   (false (tuatara.frontend.fake:fake-frame-showing f "grammar?"))
                   (false (tuatara.syntax:buffer-syntax buffer)))
              (tuatara.buffer:kill-buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))))))))

(define-test (grammars-on nothing-done-after-commands-unless-asking)
  ;; nothing is looked at after each command while no question is pending,
  ;; nor once the question is answered
  (with-grammar-answers-forgotten ("awk")
    (multiple-value-bind (f editor) (run-keys "a b c" :text "abc")
      (declare (ignore f))
      (false (member 'tuatara.defaults::ask-about-shown-grammar (tuatara.command:editor-post-command-functions editor))))
    (with-file-session ((f editor) "n a b" (format nil "BEGIN { print 1 }~%") "awk")
      (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "Turn on the AWK grammar"))
      (false (member 'tuatara.defaults::ask-about-shown-grammar (tuatara.command:editor-post-command-functions editor))))))

(defun typed-ahead-session (keys text extension)
  "Run KEYS (as QUEUE-TEST-KEYS takes them) on a file with EXTENSION holding
TEXT, every key queued counting as typed ahead, waiting in the frontend;
return the fake frontend."
  (with-test-directory (directory)
    (let ((path (merge-pathnames (format nil "sample.~a" extension) directory))
          (f (tuatara.frontend.fake:make-fake-frontend :width 60 :height 8)))
      (with-open-file (o path :direction :output) (write-string text o))
      (setf (tuatara.frontend.fake:fake-pending f) (lambda (f) (tuatara.frontend.fake::fake-events f)))
      (queue-test-keys f keys)
      (let ((editor (tuatara.command:run-editor f :files (list path) :setup #'tuatara.defaults:install-defaults)))
        (tuatara.buffer:kill-buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
      f)))

(define-test (grammars-on keys-typed-ahead-never-answer)
  ;; a key typed before the question shows never answers it: one pressed
  ;; after does, one key alone; with none, it is "not now", nothing kept
  (with-grammar-answers-forgotten ("awk")
    (let ((f (typed-ahead-session (list "y" :tick "n") (format nil "BEGIN { print 1 }~%") "awk")))
      (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "The AWK grammar stays off"))
      (is eq :off (gethash "awk" (tuatara.defaults::grammar-answers :again t)) "the n pressed after")))
  (with-grammar-answers-forgotten ("awk")
    (let ((f (typed-ahead-session "y" (format nil "BEGIN { print 1 }~%") "awk")))
      (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "asked again when Tuatara starts again"))
      (false (gethash "awk" (tuatara.defaults::grammar-answers :again t)) "nothing kept")
      (true (member "awk" tuatara.defaults::*grammars-not-now* :test #'string=) "not now"))))

(define-test (grammars-on other-key-is-not-now)
  ;; a key other than y or n is "not now" for this session, and nothing is kept
  (with-grammar-answers-forgotten ("awk")
    (with-file-session ((f editor) "x" (format nil "BEGIN { print 1 }~%") "awk")
      (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "asked again when Tuatara starts again")))
    (false (gethash "awk" (tuatara.defaults::grammar-answers :again t)))
    (true (member "awk" tuatara.defaults::*grammars-not-now* :test #'string=))))

(define-test (grammars-on answers-read-again-only-when-changed)
  ;; looking at the answers again, as each command does while a question is
  ;; pending, makes nothing new while no answer has changed
  (with-grammar-answers-forgotten ("awk")
    (let ((table (tuatara.defaults::grammar-answers :again t))
          (on (tuatara.defaults::grammars-on-table)))
      (is eq table (tuatara.defaults::grammar-answers :again t))
      (is eq on (tuatara.defaults::grammars-on-table)))))

(tuatara.modes:define-major-mode user-grammar-test-mode tuatara.defaults::prog-mode
  "Files read with a grammar the tests add as yours."
  :lighter "TestJSON" :extensions ("testjson"))

(defun add-test-grammar (folder)
  "Compile JSON's grammar as testjson.so into FOLDER, the folder of grammars
you add, with JSON's colouring rules beside it; true when it could."
  (let ((src (asdf:system-relative-pathname :tuatara "build/grammar-sources/json/src/")))
    (ensure-directories-exist (merge-pathnames "rules/" folder))
    (uiop:copy-file (asdf:system-relative-pathname :tuatara "syntax/rules/json.scm") (merge-pathnames "rules/testjson.scm" folder))
    (zerop (nth-value 2 (uiop:run-program (list "cc" "-O1" "-shared" "-fPIC" "-std=c11" "-Dtree_sitter_json=tree_sitter_testjson"
                                                (format nil "-I~a" (uiop:native-namestring src))
                                                (uiop:native-namestring (merge-pathnames "parser.c" src))
                                                "-o" (uiop:native-namestring (merge-pathnames "testjson.so" folder)))
                                          :ignore-error-status t :error-output nil)))))

(defmacro with-test-grammar-added ((folder) &body body)
  "Run BODY with FOLDER the folder of grammars you add, holding testjson.so
(ADD-TEST-GRAMMAR), the language USER-GRAMMAR-TEST defined for it, and nothing
kept of it nor of any answer about it; all taken away afterwards."
  `(let ((,folder (tuatara.syntax:user-grammar-directory)))
     (flet ((forget-it ()
              (tuatara.defaults::state-list-drop tuatara.defaults::*user-grammar-sums* (list "testjson"))
              (setf tuatara.defaults::*user-grammar-records* nil)
              (clrhash tuatara.defaults::*user-grammar-verdicts*)
              (uiop:delete-directory-tree (tuatara.syntax:user-grammar-store) :validate t :if-does-not-exist :ignore)
              (tuatara.syntax:forget-grammars)))
       (unwind-protect
            (progn
              (true (add-test-grammar ,folder) "compiled")
              (forget-it)
              (tuatara.syntax:stop-helpers)
              (tuatara.defaults::define-language user-grammar-test
                "JSON read by a grammar the tests add as yours."
                :grammar "testjson" :modes (user-grammar-test-mode))
              (with-grammar-answers-forgotten ("user-grammar-test")
                (let ((tuatara.defaults::*grammars-on-table* nil))
                  ,@body)))
         (setf tuatara.defaults::*languages* (remove 'user-grammar-test tuatara.defaults::*languages*
                                                     :key #'tuatara.defaults::language-name))
         (uiop:delete-directory-tree ,folder :validate t :if-does-not-exist :ignore)
         (forget-it)
         (tuatara.syntax:stop-helpers)))))

(defun test-language-on-p ()
  "True when the grammar of the language the tests add as yours is on."
  (tuatara.defaults::grammar-on-p (tuatara.defaults::find-language 'user-grammar-test)))

(defun new-session-for-grammars ()
  "Forget what was found of the grammars you added and the grammars loaded, as
a new session would."
  (clrhash tuatara.defaults::*user-grammar-verdicts*)
  (setf tuatara.defaults::*grammars-on-table* nil)
  (tuatara.syntax:forget-grammars))

(define-test (grammars-on grammar-you-add)
  ;; a grammar you add: off until you turn it on, never one of the common
  ;; ones; turned on, a copy of it is kept and loaded, and it colours; its file
  ;; changed, it is refused in the next session until turned on again
  (with-test-grammar-added (folder)
    (let ((tuatara.defaults::*common-grammar-languages* '(user-grammar-test json)))
      (false (test-language-on-p) "not one of the common ones")
      (false (tuatara.syntax:find-grammar "testjson") "not loaded while off")
      (with-file-session ((f editor) "y" (format nil "{\"a\": 1}~%") "testjson")
        (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "Turn on the TestJSON grammar"))
        (true (row-faces f 0) "coloured once on"))
      (true (probe-file (merge-pathnames "testjson.so" (tuatara.syntax:user-grammar-store))) "a copy kept")
      (true (test-language-on-p))
      ;; changed, the same size, its times put back: still found changed
      (let ((path (uiop:native-namestring (merge-pathnames "testjson.so" folder))))
        (let ((octets (alexandria:read-file-into-byte-vector path))
              (stat (sb-posix:stat path)))
          (setf (aref octets (1- (length octets))) (logxor 1 (aref octets (1- (length octets)))))
          (alexandria:write-byte-vector-into-file octets path :if-exists :overwrite)
          (sb-posix:utimes path (sb-posix:stat-atime stat) (sb-posix:stat-mtime stat))))
      (new-session-for-grammars)
      (false (tuatara.syntax:find-grammar "testjson") "refused once changed")
      (false (test-language-on-p) "off once found changed")
      (tuatara.defaults::note-grammar-answer (tuatara.defaults::find-language 'user-grammar-test) t)
      (new-session-for-grammars)
      (true (tuatara.syntax:find-grammar "testjson") "on again once turned on")
      (true (test-language-on-p)))))

(define-test (grammars-on old-yes-is-no-trust)
  ;; a yes given while a name was one of Tuatara's own grammars trusts nothing
  ;; added later under that name: it is asked about again
  (with-test-grammar-added (folder)
    (tuatara.defaults::state-list-put tuatara.defaults::*grammar-answers* (list (cons "user-grammar-test" t)))
    (setf tuatara.defaults::*grammar-answers-read* nil)
    (new-session-for-grammars)
    (false (test-language-on-p) "not on")
    (false (tuatara.syntax:find-grammar "testjson") "not loaded")
    (with-file-session ((f editor) "n" (format nil "{\"a\": 1}~%") "testjson")
      (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "Turn on the TestJSON grammar") "asked again"))))

(define-test (grammars-on named-grammar-kept-when-loaded)
  ;; a grammar you added that *grammars-on* names is kept as it is the first
  ;; time it is loaded, and loaded from that copy
  (with-test-grammar-added (folder)
    (let ((tuatara-user:*grammars-on* '(:common user-grammar-test)))
      (true (test-language-on-p))
      (true (tuatara.syntax:find-grammar "testjson"))
      (true (gethash "testjson" (tuatara.defaults::user-grammar-records)) "kept"))))

(define-test (grammars-on linked-grammar-never-loaded)
  ;; a link in the folder of grammars you add, to a file elsewhere, is no
  ;; grammar of yours to load, nor one of Tuatara's: never loaded, never kept
  (with-test-grammar-added (folder)
    (let ((elsewhere (merge-pathnames "elsewhere.so" (tuatara.io:cache-directory))))
      (unwind-protect
           (progn
             (rename-file (merge-pathnames "testjson.so" folder) elsewhere)
             (sb-posix:symlink (uiop:native-namestring elsewhere) (uiop:native-namestring (merge-pathnames "testjson.so" folder)))
             (new-session-for-grammars)
             (true (tuatara.syntax:user-grammar-p "testjson") "taken as yours")
             (false (tuatara.syntax:user-grammar-file "testjson") "no plain file")
             (tuatara.defaults::note-grammar-answer (tuatara.defaults::find-language 'user-grammar-test) t)
             (false (tuatara.syntax:find-grammar "testjson") "not loaded")
             (false (gethash "testjson" (tuatara.defaults::user-grammar-records)) "nothing kept"))
        (ignore-errors (delete-file elsewhere))))))

(define-test (grammars-on own-grammar-never-passed-over)
  ;; when one of Tuatara's grammars cannot be loaded, the program running
  ;; grammars takes no file of the same name from a folder after it
  (with-test-directory (directory)
    (let* ((own (merge-pathnames "own/" directory))
           (later (merge-pathnames "later/" directory)))
      (ensure-directories-exist own)
      (true (add-test-grammar later) "compiled")
      (write-test-file own "testjson.so" "not a library")
      (let ((tuatara.syntax:*grammar-directories*
              (list (namestring own) (namestring later)
                    (namestring (asdf:system-relative-pathname :tuatara "build/grammars/")))))
        (tuatara.syntax:forget-grammars)
        (unwind-protect (false (loop repeat 3 thereis (tuatara.syntax:find-grammar "testjson") do (sleep 0.2)))
          (tuatara.syntax:stop-helpers)
          (tuatara.syntax:forget-grammars))))))

(define-test (grammars-on own-languages-not-replaced)
  ;; an init file or a package adds languages; one of Tuatara's own is not
  ;; replaced, and that is said
  (let ((before (tuatara.defaults::find-language 'python)))
    (unwind-protect
         (progn
           (false (eval '(tuatara.defaults::define-language tuatara-user::python "Python read as Nix." :grammar "nix")))
           (is eq before (tuatara.defaults::find-language 'python))
           (is string= "python" (tuatara.defaults::language-grammar (tuatara.defaults::find-language 'python))))
      (unless (eq before (tuatara.defaults::find-language 'python))
        (setf tuatara.defaults::*languages* (cons before (remove (tuatara.defaults::find-language 'python)
                                                                 tuatara.defaults::*languages*)))))))

(define-test (grammars-on rules-held-to-their-size)
  ;; rules over *rules-largest*, as a file or with what they take in, are
  ;; not used, and that is said
  (with-test-directory (directory)
    (let ((tuatara.defaults::*rules-largest* 1000))
      (write-test-file directory "big.scm" (make-string 2000 :initial-element #\;))
      (write-test-file directory "a.scm" (format nil "; inherits: b,b,b,b~%(x) @y~%"))
      (write-test-file directory "b.scm" (format nil "; inherits: c,c,c,c~%~a~%" (make-string 90 :initial-element #\;)))
      (write-test-file directory "c.scm" (make-string 90 :initial-element #\;))
      (false (tuatara.defaults::read-query directory "big"))
      (false (tuatara.defaults::read-query directory "a") "grown past it by what it takes in")
      (true (tuatara.defaults::read-query directory "c")))))

(define-test (grammars-on rules-threads-kept-once)
  ;; a thread compiling rules that ends at once is not left kept: asked
  ;; again, a new one runs
  (let ((table (make-hash-table :test 'equal :synchronized t)) (runs 0))
    (sb-thread:join-thread (tuatara.defaults::thread-once table :k (lambda () (incf runs))))
    (loop repeat 100 while (gethash :k table) do (sleep 0.01))
    (false (gethash :k table) "taken out as it ended")
    (sb-thread:join-thread (tuatara.defaults::thread-once table :k (lambda () (incf runs))))
    (is = 2 runs)
    ;; a dead thread left in the table counts as none
    (setf (gethash :k table) (sb-thread:make-thread (lambda ())))
    (sb-thread:join-thread (gethash :k table))
    (sb-thread:join-thread (tuatara.defaults::thread-once table :k (lambda () (incf runs))))
    (is = 3 runs)))

(define-test (grammars-on grammar-store-yours-alone)
  ;; the folder copies of grammars you added are kept in: one that others may
  ;; read is made yours alone; a link in its place is not used, and a copy is
  ;; then not kept, said
  (let* ((folder (merge-pathnames "grammar-store/" (tuatara.io:state-directory)))
         (name (string-right-trim "/" (uiop:native-namestring folder)))
         (elsewhere (merge-pathnames "elsewhere-store/" (tuatara.io:cache-directory))))
    (unwind-protect
         (progn
           (uiop:delete-directory-tree folder :validate t :if-does-not-exist :ignore)
           (sb-posix:mkdir name #o755)
           (sb-posix:chmod name #o755)
           (true (tuatara.syntax:user-grammar-store))
           (is = #o700 (logand #o777 (sb-posix:stat-mode (sb-posix:stat name))) "made yours alone")
           (sb-posix:rmdir name)
           (ensure-directories-exist elsewhere)
           (sb-posix:symlink (string-right-trim "/" (uiop:native-namestring elsewhere)) name)
           (false (tuatara.syntax:user-grammar-store) "a link is not used")
           (true (search "a link" (tuatara.syntax:store-refused-why)))
           (false (member nil (tuatara.syntax:grammar-directories)))
           (false (tuatara.defaults::stored-grammar-path "testjson")))
      (ignore-errors (sb-posix:unlink name))
      (uiop:delete-directory-tree elsewhere :validate t :if-does-not-exist :ignore)
      (tuatara.syntax:user-grammar-store))))
