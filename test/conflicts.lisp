;;;; test/conflicts.lisp --- Conflicts a merge left in a file, shown and put right under C-c ^.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test conflicts
  :description "A file shown that holds whole conflicts (<<<<<<<, ||||||| and a base where there
is one, =======, >>>>>>>) has them painted, a face for each side, the base and the marker lines,
and Conflicts N in the mode line; C-c ^ n and p go between them, u keeps the first side, l the
second, a both, b the base, = shows the sides' differences; each one step to undo, point going
on to the next, and when none is left it says how to stage the file. A lone ======= is no
conflict. The git status lists the files in conflict under Unmerged files; RET there goes to the
first conflict.")

(defparameter *two-conflicts*
  (format nil "top~%<<<<<<< HEAD~%ours one~%=======~%theirs one~%>>>>>>> feature~%middle~%<<<<<<< HEAD~%ours two~%||||||| base~%base two~%=======~%theirs two~%>>>>>>> feature~%end~%")
  "A text holding two conflicts, the second written with its base.")

(defun conflicts-of (name)
  "True when the buffer called NAME shows conflicts."
  (let ((buffer (tuatara.buffer:find-buffer name)))
    (and buffer (tuatara.defaults::conflict-markers-on-p buffer))))

(defun conflicts-looked-through (name)
  "True once the buffer called NAME has been looked through for conflicts and none is being looked for."
  (let ((buffer (tuatara.buffer:find-buffer name)))
    (and buffer (tuatara.buffer:buffer-local buffer 'tuatara.defaults::conflict-checked)
         (null (tuatara.buffer:buffer-local buffer 'tuatara.defaults::conflict-job)))))

(defun face-on-row-with (frontend text face)
  "True when some frame FRONTEND painted had a row holding TEXT with that text in FACE."
  (loop for screen in (tuatara.frontend.fake:fake-history frontend)
        for (spans) in (tuatara.frontend.fake:fake-face-history frontend)
          thereis (loop for row across screen
                        for row-spans across spans
                        for at = (search text row)
                          thereis (and at (loop for (start end f) in row-spans
                                                  thereis (and (<= start at) (< at end) (member face (if (listp f) f (list f)))))))))

(defun conflict-session (dir text steps &key (name "a.txt"))
  "Run STEPS (as AUTO-SAVE-SESSION takes them) once a file NAME holding TEXT
in DIR is visited and its conflicts found, or it is looked through. Returns
the frontend and the editor."
  (let ((path (namestring (merge-pathnames name dir)))
        (tuatara.defaults::*modal-editing* nil))
    (put-file path text)
    (auto-save-session (list* (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed path))
                              (list :until (lambda () (conflicts-looked-through name)))
                              steps))))

(define-test (conflicts found-whole-only)
  ;; only markers in order make a conflict: start, a base or none, the separator, the end
  (flet ((found (text) (tuatara.defaults::conflict-find-all (tuatara.text:string->rope text)))
         (at (text part &optional (from 0)) (search part text :start2 from)))
    (let ((two (format nil "<<<<<<< a~%x~%=======~%y~%>>>>>>> b~%")))
      (is equal (list (list 0 nil (at two "=======") (at two ">>>>>>>"))) (found two)))
    (let ((three (format nil "<<<<<<< a~%x~%||||||| o~%z~%=======~%y~%>>>>>>> b")))
      (is equal (list (list 0 (at three "|||||||") (at three "=======") (at three ">>>>>>>"))) (found three)))
    (let ((crlf (format nil "<<<<<<< a~c~%x~c~%=======~c~%y~c~%>>>>>>> b~c~%" #\Return #\Return #\Return #\Return #\Return)))
      (is = 1 (length (found crlf))))
    (dolist (none (list (format nil "a~%=======~%b~%")
                        (format nil ">>>>>>> b~%=======~%<<<<<<< a~%")
                        (format nil "<<<<<<< a~%x~%>>>>>>> b~%")
                        (format nil "<<<<<<<< a~%x~%=======~%y~%>>>>>>> b~%")
                        (format nil "<<<<<<< a~%x~%=======x~%y~%>>>>>>> b~%")
                        (format nil "<<<<<<<a~%x~%=======~%y~%>>>>>>> b~%")
                        (format nil " <<<<<<< a~%x~%=======~%y~%>>>>>>> b~%")))
      (is equal '() (found none) "Found in ~s" none))
    ;; a <<<<<<< line again begins again; a marker astride the 64 KB the text is read in
    (let ((again (format nil "<<<<<<< a~%<<<<<<< c~%x~%=======~%y~%>>>>>>> b~%")))
      (is equal (list (list (at again "<<<<<<< c") nil (at again "=======") (at again ">>>>>>>"))) (found again)))
    (let* ((pad (make-string (- 65536 3) :initial-element #\x))
           (astride (format nil "~a~%<<<<<<< a~%x~%=======~%y~%>>>>>>> b~%" pad)))
      (is equal (list (list (at astride "<<<<<<<") nil (at astride "=======") (at astride ">>>>>>>"))) (found astride)))))

(define-test (conflicts looked-through-without-garbage)
  ;; looking through a text makes no garbage for each line: one block on the stack, whatever its
  ;; size; the least of five runs, so that work on other threads meanwhile does not count
  (flet ((consed (megabytes)
           (let ((rope (tuatara.text:string->rope
                        (with-output-to-string (out)
                          (loop repeat (* megabytes 1024 1024 1/16) do (write-line "a,b,c,d,e,f,g,h" out))))))
             (tuatara.defaults::conflict-find-all rope)
             (loop repeat 5
                   minimize (let ((before (sb-ext:get-bytes-consed)))
                              (tuatara.defaults::conflict-find-all rope)
                              (- (sb-ext:get-bytes-consed) before))))))
    (let ((small (consed 1)) (large (consed 9)))
      (true (< (/ (- large small) 8) 1024) "~:d octets for each megabyte more" (round (- large small) 8)))))

(define-test (conflicts shown-and-kept)
  ;; painted a face for each part; n goes to the first, u keeps the first side and goes on,
  ;; l keeps the second and says none is left; undo brings the conflict back
  (with-test-directory (dir)
    (let ((after-first nil) (gone nil))
      (flet ((buffer () (tuatara.buffer:find-buffer "a.txt")))
        (multiple-value-bind (f editor)
            (conflict-session dir *two-conflicts*
                              (list "M-< C-c ^ n" "C-c ^ u"
                                    (list :until (lambda () (setf after-first (tuatara.buffer:buffer-point (buffer)))))
                                    "C-c ^ l" (list :until (lambda () (setf gone (not (conflicts-of "a.txt")))))
                                    "C-/" (list :until (lambda () (conflicts-of "a.txt")))))
          (declare (ignore editor))
          (true (face-on-row-with f "<<<<<<< HEAD" :conflict-marker))
          (true (face-on-row-with f "ours one" :conflict-first))
          (true (face-on-row-with f "theirs one" :conflict-second))
          (true (face-on-row-with f "base two" :conflict-base))
          (false (face-on-row-with f "middle" :conflict-first))
          (true (tuatara.frontend.fake:fake-frame-showing f "Conflicts 2"))
          (true (tuatara.frontend.fake:fake-frame-showing f "Conflict 1 of 2"))
          (true (tuatara.frontend.fake:fake-frame-showing f "Kept the first side; 1 conflict left"))
          (true (tuatara.frontend.fake:fake-frame-showing f "No conflicts left in a.txt"))
          (is = (search "<<<<<<<" (format nil "top~%ours one~%middle~%<<<<<<<")) after-first)
          (true gone)
          ;; the second conflict back, as one step, and shown again
          (is string= (format nil "top~%ours one~%middle~%<<<<<<< HEAD~%ours two~%||||||| base~%base two~%=======~%theirs two~%>>>>>>> feature~%end~%")
              (tuatara.buffer:buffer-string (buffer)))
          (true (conflicts-of "a.txt")))))))

(define-test (conflicts base-and-both)
  ;; b keeps the base where there is one, and says there is none in a conflict without;
  ;; a keeps both sides, the first then the second
  (with-test-directory (dir)
    (let ((text (format nil "<<<<<<< HEAD~%ours one~%||||||| base~%base one~%=======~%theirs one~%>>>>>>> feature~%<<<<<<< HEAD~%ours two~%=======~%theirs two~%>>>>>>> feature~%")))
      (let ((f (conflict-session dir text (list "M-< C-c ^ b" "C-c ^ b" "C-c ^ a"
                                                (list :until (lambda () (not (conflicts-of "a.txt"))))))))
        (true (tuatara.frontend.fake:fake-frame-showing f "This conflict has no base"))
        (is string= (format nil "base one~%ours two~%theirs two~%")
            (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "a.txt")))))))

(define-test (conflicts lone-markers-left-alone)
  ;; a line of ======= alone, markers out of order: no conflict, nothing painted, no keys
  (with-test-directory (dir)
    (let ((text (format nil "Title~%=======~%~%>>>>>>> quoted~%<<<<<<< reply~%")))
      (let ((f (conflict-session dir text (list "M-<"))))
        (false (conflicts-of "a.txt"))
        (false (tuatara.frontend.fake:fake-frame-showing f "Conflicts"))
        (false (tuatara.buffer:buffer-local (tuatara.buffer:find-buffer "a.txt") :override-keymap))))))

(define-test (conflicts sides-compared)
  ;; = shows how the two sides differ, named by their marker lines, as a diff shows changes,
  ;; worked out in the background; the sides' lines exactly as they are, the names without a
  ;; carriage return
  (is string= (format nil "--- old~%+++ new~%@@ -1,3 +1,3 @@~% a~%-b~%+x~% c~%")
      (tuatara.defaults::unified-diff '("a" "b" "c") '("a" "x" "c")))
  (is string= (format nil "--- old~%+++ new~%@@ -0,0 +1,1 @@~%+a~%")
      (tuatara.defaults::unified-diff '() '("a")))
  (is string= "" (tuatara.defaults::unified-diff '("a") '("a")))
  (is equal '("a" "" "") (tuatara.defaults::conflict-side-lines (format nil "a~%~%~%")))
  (is equal '("a") (tuatara.defaults::conflict-side-lines (format nil "a~%")))
  (let ((buffer (tuatara.buffer:make-buffer "*conflict label test*" :text (format nil "<<<<<<< HEAD~c~%x~%" #\Return))))
    (unwind-protect (is equal "HEAD" (tuatara.defaults::conflict-marker-label buffer 0))
      (tuatara.buffer:kill-buffer buffer)))
  (with-test-directory (dir)
    (flet ((shown () (tuatara.buffer:find-buffer "*Conflict: a.txt*")))
      (conflict-session dir *two-conflicts* (list "M-< C-c ^ n C-c ^ ="
                                                  (list :until (lambda () (and (shown) (search "---" (tuatara.buffer:buffer-string (shown))))))))
      (let ((text (tuatara.buffer:buffer-string (shown))))
        (tuatara.buffer:kill-buffer (shown))
        (true (search (format nil "--- HEAD~%+++ feature~%") text))
        (true (search (format nil "-ours one~%+theirs one~%") text))))))

(define-test (conflicts from-the-git-status)
  ;; a merge left a.txt in conflict: listed under Unmerged files; RET there goes to its
  ;; conflict, l keeps the second side and says how to stage it; s in the status stages it
  (with-test-directory (dir)
    (scratch-repository dir)
    (flet ((git (&rest arguments) (apply #'git-output dir arguments)))
      (put-file (merge-pathnames "a.txt" dir) (format nil "zero~%one~%"))
      (git "commit" "-q" "-am" "Zero")
      (git "checkout" "-q" "-b" "other")
      (put-file (merge-pathnames "a.txt" dir) (format nil "zero~%two~%"))
      (git "commit" "-q" "-am" "Two")
      (git "checkout" "-q" "main")
      (put-file (merge-pathnames "a.txt" dir) (format nil "zero~%three~%"))
      (git "commit" "-q" "-am" "Three")
      (git "merge" "-q" "other"))
    (let ((at-conflict nil))
      (let ((f (git-session dir "a.txt"
                            (list (list :until (lambda () (tuatara.defaults::git-state-unmerged (git-state-here))))
                                  "M-< C-s b o t h SPC c h RET RET"
                                  (list :until (lambda () (and (conflicts-of "a.txt")
                                                               (eql 5 (tuatara.buffer:buffer-point (tuatara.buffer:find-buffer "a.txt")))
                                                               (setf at-conflict t))))
                                  "C-c ^ l" "C-x C-s" "C-x g" (list :until #'git-state-here)
                                  "M-< C-s b o t h SPC c h RET s"
                                  (list :until (lambda () (null (tuatara.defaults::git-state-unmerged (git-state-here)))))))))
        (true (tuatara.frontend.fake:fake-frame-showing f "Unmerged files (1)"))
        (true (tuatara.frontend.fake:fake-frame-showing f "both changed" "a.txt"))
        (true at-conflict)
        (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "No conflicts left in a.txt: save it (C-x C-s), then C-x g and s on its line stage it"))
        (is string= (format nil "zero~%two~%") (alexandria:read-file-into-string (merge-pathnames "a.txt" dir)))
        (is equal '("a.txt") (mapcar #'first (tuatara.defaults::git-state-staged (git-state-here))))))))

(defun conflict-count-of (name)
  "How many conflicts stand in the buffer called NAME."
  (length (tuatara.defaults::conflict-list (tuatara.buffer:find-buffer name))))

(define-test (conflicts edits-followed-not-looked-through)
  ;; typing in a side looks nothing through again; a separator broken takes its conflict out at
  ;; once, counted so in the mode line; typed back, the marker left over has the file looked
  ;; through again once the typing pauses
  (with-test-directory (dir)
    (let ((started 0) (shown nil) (typed nil) (broken nil))
      (sb-int:encapsulate 'tuatara.runtime:start-job-thread 'conflicts-counted
                          (lambda (function job &rest arguments)
                            (when (search "CONFLICT" (symbol-name (type-of job))) (incf started))
                            (apply function job arguments)))
      (unwind-protect
           (let ((f (conflict-session dir *two-conflicts*
                                      (list (list :until (lambda () (conflicts-of "a.txt")))
                                            (lambda () (setf shown started))
                                            "M-< C-n C-n C-e SPC x y z"
                                            (list :until (lambda () (search "ours one xyz" (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "a.txt")))))
                                            (lambda () (setf typed started))
                                            "M-< C-s = = = = = = = RET DEL"
                                            (list :until (lambda () (setf broken (= 1 (conflict-count-of "a.txt")))))
                                            "="
                                            (list :until (lambda () (= 2 (conflict-count-of "a.txt"))))))))
             (is = shown typed)
             (true broken)
             (true (tuatara.frontend.fake:fake-frame-showing f "Conflicts 1"))
             (is = 2 (conflict-count-of "a.txt")))
        (sb-int:unencapsulate 'tuatara.runtime:start-job-thread 'conflicts-counted)))))

(define-test (conflicts edits-make-no-garbage-for-each-conflict)
  ;; following an edit costs the same however many conflicts the text holds: the octets made for
  ;; each character typed in a side with the edits followed, less those made without, for two
  ;; sizes; the least of five runs, so that work on other threads meanwhile does not count
  (flet ((following (count)
           (let* ((text (with-output-to-string (out)
                          (dotimes (i count)
                            (format out "line~%<<<<<<< HEAD~%ours ~d~%=======~%theirs ~d~%>>>>>>> other~%" i i))))
                  (at (search (format nil "ours ~d" (floor count 2)) text)))
             (flet ((consed (watched)
                      (let ((buffer (tuatara.buffer:make-buffer (format nil "*conflicts ~d*" count) :text text)))
                        (unwind-protect
                             (progn
                               (when watched
                                 (tuatara.defaults::conflict-put buffer (tuatara.buffer:buffer-version buffer)
                                                                 (tuatara.defaults::conflict-find-all (tuatara.buffer:buffer-text buffer)))
                                 (tuatara.defaults::conflict-watch buffer)
                                 (assert (= count (length (tuatara.defaults::conflict-list buffer)))))
                               (loop repeat 5
                                     minimize (let ((before (sb-ext:get-bytes-consed)))
                                                (loop repeat 1000 do (tuatara.buffer:buffer-replace buffer at at "x"))
                                                (round (- (sb-ext:get-bytes-consed) before) 1000))))
                          (tuatara.buffer:kill-buffer buffer)))))
               (- (consed t) (consed nil))))))
    (let ((few (following 500)) (many (following 4000)))
      (true (< few 4096) "~:d octets a key" few)
      (true (< (- many few) 512) "~:d octets a key with 500 conflicts, ~:d with 4000" few many))))

(define-test (conflicts old-look-brings-none-back)
  ;; a look started before a conflict was put right, taken in after, does not bring it back
  (let ((buffer (tuatara.buffer:make-buffer "*conflicts old look*" :text *two-conflicts*)))
    (unwind-protect
         (let* ((version (tuatara.buffer:buffer-version buffer))
                (found (tuatara.defaults::conflict-find-all (tuatara.buffer:buffer-text buffer))))
           (tuatara.defaults::conflict-put buffer version found)
           (tuatara.defaults::conflict-watch buffer)
           (let ((start (search "<<<<<<<" *two-conflicts*)) (after (+ (search ">>>>>>> feature" *two-conflicts*) 16)))
             (tuatara.buffer:buffer-replace buffer start after (format nil "ours one~%")))
           (is = 1 (length (tuatara.defaults::conflict-list buffer)))
           (tuatara.defaults::conflict-put buffer version found)
           (is = 1 (length (tuatara.defaults::conflict-list buffer))))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (conflicts decrypted-left-alone)
  ;; a buffer of decrypted text is never looked through by itself
  (with-test-directory (dir)
    (let ((secret (namestring (merge-pathnames "secret.txt" dir))))
      (put-file secret *two-conflicts*)
      (let ((buffer (tuatara.io:visit-file secret)))
        (setf (tuatara.buffer:buffer-local buffer :plaintext) t)
        (conflict-session dir *two-conflicts* (list "C-x b s e c r e t . t x t RET" "C-x b a . t x t RET"
                                                    (list :until (lambda () (conflicts-of "a.txt")))))
        (false (tuatara.defaults::conflict-markers-on-p buffer))
        (false (tuatara.buffer:buffer-local buffer 'tuatara.defaults::conflict-job))))))

(define-test (conflicts looked-through-once-read-again)
  ;; a file shown without conflicts, read again once a merge has left some in it: found
  (with-test-directory (dir)
    (let ((path (merge-pathnames "a.txt" dir)))
      (conflict-session dir (format nil "plain~%")
                        (list (lambda () (put-file path *two-conflicts*))
                              "M-x r e v e r t - b u f f e r RET"
                              (list :until (lambda () (conflicts-of "a.txt")))))
      (true (conflicts-of "a.txt")))))

(define-test (conflicts put-right-then-back-on-disk)
  ;; a file whose conflicts were put right and saved, put back in conflict on disk (git
  ;; checkout -m): read again, its conflicts are shown and C-c ^ n goes to them; undoing the
  ;; reading takes them away; read again as a file changed on disk is, shown again
  (with-test-directory (dir)
    (let ((path (merge-pathnames "a.txt" dir))
          (resolved nil) (back nil) (gone nil) (again nil) (first-at nil))
      (flet ((buffer () (tuatara.buffer:find-buffer "a.txt")))
        (conflict-session dir *two-conflicts*
                          (list (list :until (lambda () (conflicts-of "a.txt")))
                                "M-< C-c ^ n C-c ^ u" "C-c ^ u"
                                (list :until (lambda () (setf resolved (not (conflicts-of "a.txt")))))
                                "C-x C-s"
                                (lambda () (put-file path *two-conflicts*))
                                "M-x r e v e r t - b u f f e r RET"
                                (list :until (lambda () (setf back (conflicts-of "a.txt"))))
                                "M-< C-c ^ n"
                                (lambda () (setf first-at (tuatara.buffer:buffer-point (buffer))))
                                "C-/"
                                (list :until (lambda () (setf gone (not (conflicts-of "a.txt")))))
                                "C-x C-s"
                                (lambda ()
                                  (put-file path *two-conflicts*)
                                  (tuatara.defaults::reload-buffer (buffer)))
                                (list :until (lambda () (setf again (conflicts-of "a.txt"))))))
        (true resolved)
        (true back)
        (is eql (search "<<<<<<<" *two-conflicts*) first-at)
        (true gone)
        (true again)
        (is = 2 (conflict-count-of "a.txt"))))))

(define-test (conflicts keys-over-other-keys)
  ;; a buffer with keys of its own keeps them under the conflicts' keys, and gets them back as
  ;; they were once its conflicts are put right
  (with-test-directory (dir)
    (let ((own (tuatara.command:make-keymap :name "own keys"))
          (ends nil))
      (tuatara.command:define-key own "C-c z" 'tuatara.defaults::end-of-buffer)
      (conflict-session dir *two-conflicts*
                        (list (list :until (lambda () (conflicts-of "a.txt")))
                              (lambda () (setf (tuatara.buffer:buffer-local (tuatara.buffer:find-buffer "a.txt") :override-keymap) own))
                              "M-<" "C-c z"
                              (list :until (lambda () (setf ends (= (tuatara.buffer:buffer-point (tuatara.buffer:find-buffer "a.txt"))
                                                                    (tuatara.buffer:buffer-size (tuatara.buffer:find-buffer "a.txt"))))))
                              "M-< C-c ^ n C-c ^ u" "C-c ^ u"
                              (list :until (lambda () (not (conflicts-of "a.txt"))))))
      (true ends)
      (is string= (format nil "top~%ours one~%middle~%ours two~%end~%") (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "a.txt")))
      (is eq own (tuatara.buffer:buffer-local (tuatara.buffer:find-buffer "a.txt") :override-keymap)))))

(define-test (conflicts at-the-end-of-the-text)
  ;; a last conflict ending the text without a newline is put right from its end too, point
  ;; left where it was
  (with-test-directory (dir)
    (let ((f (conflict-session dir (format nil "x~%<<<<<<< a~%one~%=======~%two~%>>>>>>> b")
                               (list (list :until (lambda () (conflicts-of "a.txt")))
                                     "M-> C-c ^ l" (list :until (lambda () (not (conflicts-of "a.txt"))))))))
      (true (tuatara.frontend.fake:fake-frame-showing f "No conflicts left in a.txt"))
      (let ((buffer (tuatara.buffer:find-buffer "a.txt")))
        (is string= (format nil "x~%two~%") (tuatara.buffer:buffer-string buffer))
        (is = 2 (tuatara.buffer:buffer-point buffer))))))

(define-test (conflicts sides-shown-from-the-file-folder)
  ;; the buffer showing how the sides differ stands in the conflicted file's folder, as the
  ;; mode line shows
  (with-test-directory (dir)
    (flet ((shown () (tuatara.buffer:find-buffer "*Conflict: a.txt*")))
      (conflict-session dir *two-conflicts* (list "M-< C-c ^ n C-c ^ ="
                                                  (list :until (lambda () (and (shown) (search "---" (tuatara.buffer:buffer-string (shown))))))))
      (let ((shown (shown)))
        (unwind-protect
             (progn
               (is string= (tuatara.command:buffer-directory (tuatara.buffer:find-buffer "a.txt"))
                   (tuatara.command:buffer-directory shown))
               (is string= (tuatara.command::buffer-directory-name (tuatara.buffer:find-buffer "a.txt"))
                   (tuatara.command::buffer-directory-name shown)))
          (tuatara.buffer:kill-buffer shown))))))
