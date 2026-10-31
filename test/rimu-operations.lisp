;;;; test/rimu-operations.lisp --- Rimu's marks and operations, the trash, and rename-formatted.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test rimu-operations
  :description "In a rimu list, m u U t mark, d marks for deletion and x puts those in the trash; C copies and
R moves the marked or the one on the line to the other list's directory or a name asked for, asking
before replacing (the old to the trash); D puts in the trash (the freedesktop.org one, with its
.trashinfo), C-u D deletes for good; + makes directories. rename-formatted gives files formatted
names, their headers written and saved, visiting buffers following.")

(defmacro with-operations-tree ((directory) &body body)
  "Run BODY with DIRECTORY a new directory holding a.txt, b.txt, sub/ with
c.txt, and link to a.txt, and a trash of its own (XDG_DATA_HOME in it), no
drive's trash seen (no drive mounted, as far as the trash knows), so that
emptying the trash never reaches a real one."
  (let ((old (gensym "OLD")))
    `(with-test-directory (,directory)
       (alexandria:write-string-into-file "alpha" (merge-pathnames "a.txt" ,directory))
       (alexandria:write-string-into-file "beta" (merge-pathnames "b.txt" ,directory))
       (ensure-directories-exist (merge-pathnames "sub/" ,directory))
       (alexandria:write-string-into-file "gamma" (merge-pathnames "sub/c.txt" ,directory))
       (sb-posix:chmod (namestring (merge-pathnames "b.txt" ,directory)) #o640)
       (sb-posix:symlink "a.txt" (namestring (merge-pathnames "link" ,directory)))
       (let ((,old (sb-posix:getenv "XDG_DATA_HOME")))
         (sb-posix:setenv "XDG_DATA_HOME" (namestring (merge-pathnames "data/" ,directory)) 1)
         (unwind-protect (let ((tuatara.defaults::*mounts-file* (namestring (merge-pathnames "no-drives" ,directory))))
                           ,@body)
           (if ,old (sb-posix:setenv "XDG_DATA_HOME" ,old 1) (sb-posix:unsetenv "XDG_DATA_HOME")))))))

(defun ops-session (directory keys &key (width 120) (height 20))
  "A session listing DIRECTORY with the preview off, typing KEYS, and waiting
for rimu's operations to finish. Returns the frontend and the editor."
  (let ((tuatara-user:*rimu-preview* nil))
    (multiple-value-prog1 (rimu-session directory keys :width width :height height)
      (loop repeat 200 while (find-if #'tuatara.defaults::rimu-operation-p tuatara.runtime:*jobs*) do (sleep 0.02)))))

(defun here (directory name)
  "The file NAME in DIRECTORY, a pathname."
  (merge-pathnames name directory))

(define-test (rimu-operations marks)
  (with-operations-tree (dir)
    ;; m marks and goes on, u unmarks, d marks for deletion; the first column says which
    (multiple-value-bind (f editor) (ops-session dir "g f a . t x t RET m m d")
      (declare (ignore f))
      (let ((lines (rimu-lines editor)))
        (true (find-if (lambda (l) (and (uiop:string-prefix-p "> " l) (search "a.txt" l))) lines))
        (true (find-if (lambda (l) (and (uiop:string-prefix-p "> " l) (search "b.txt" l))) lines))
        (true (find-if (lambda (l) (and (uiop:string-prefix-p "D " l) (search "link" l))) lines))))
    ;; the marking keys are those of every list: a count marks as many, a negative one going up
    (is eq 'tuatara.defaults::list-mark-entry
        (tuatara.command:lookup-key tuatara.defaults::*rimu-keymap* (tuatara.frontend:parse-key-sequence "m")))
    (multiple-value-bind (f editor) (ops-session dir "g f a . t x t RET C-u 2 m C-u - 1 u")
      (declare (ignore f))
      (let ((lines (rimu-lines editor)))
        (true (find-if (lambda (l) (and (uiop:string-prefix-p "> " l) (search "a.txt" l))) lines))
        (false (find-if (lambda (l) (and (uiop:string-prefix-p "> " l) (search "b.txt" l))) lines))))
    ;; t turns marks about, U takes them all away; marks stay while the list is read again
    (multiple-value-bind (f editor) (ops-session dir "g f a . t x t RET m t")
      (declare (ignore f))
      (let ((lines (rimu-lines editor)))
        (false (find-if (lambda (l) (and (uiop:string-prefix-p "> " l) (search "a.txt" l))) lines))
        (true (find-if (lambda (l) (and (uiop:string-prefix-p "> " l) (search "b.txt" l))) lines))))
    (multiple-value-bind (f editor) (ops-session dir "g f a . t x t RET m r")
      (declare (ignore f))
      (true (find-if (lambda (l) (and (uiop:string-prefix-p "> " l) (search "a.txt" l))) (rimu-lines editor))))
    ;; marked for deletion, read again (r lays it out anew), then unmarked: no longer in that mark's colour
    (let ((f (ops-session dir "g f a . t x t RET d r k u")))
      (let ((row (loop for row from 0 below 20
                       when (search "a.txt" (tuatara.frontend.fake:fake-screen-line f row)) return row)))
        (true row)
        (false (find :list-deletion (aref (tuatara.frontend.fake:fake-spans f) row)
                     :key (lambda (span) (let ((face (third span))) (if (listp face) (first face) face)))))))
    (multiple-value-bind (f editor) (ops-session dir "g f a . t x t RET m m U")
      (true (tuatara.frontend.fake:fake-frame-showing f "2 marks taken away"))
      (false (find-if (lambda (l) (uiop:string-prefix-p "> " l)) (rimu-lines editor))))
    ;; a mark of another kind a command gives (a package's M): its own colour, passed over by t,
    ;; and the entries chosen by it
    (multiple-value-bind (f editor)
        (ops-session dir (list "g f a . t x t RET" (lambda () (tuatara.defaults::rimu-mark-and-go-on #\M)) "t"))
      (let ((lines (rimu-lines editor))
            (row (loop for row from 0 below 20 when (search "a.txt" (tuatara.frontend.fake:fake-screen-line f row)) return row)))
        (true (find-if (lambda (l) (and (uiop:string-prefix-p "M " l) (search "a.txt" l))) lines))
        (true (find-if (lambda (l) (and (uiop:string-prefix-p "> " l) (search "b.txt" l))) lines))
        (true (find :list-marked-other (aref (tuatara.frontend.fake:fake-spans f) row)
                    :key (lambda (span) (let ((face (third span))) (if (listp face) (first face) face)))))
        (is equal (list (tuatara.io:native (merge-pathnames "a.txt" dir)))
            (tuatara.defaults::rimu-chosen-paths (session-buffer editor) :mark #\M))))))

(define-test (rimu-operations marks-across-directories)
  (with-operations-tree (dir)
    ;; a mark stays with its file while the list goes elsewhere; the header says how many
    ;; are marked in other directories; the operations take them all, saying where
    (multiple-value-bind (f editor)
        (ops-session dir "g f a . t x t RET m g f s u b RET RET g f c . t x t RET m" :width 140)
      (true (tuatara.frontend.fake:fake-frame-showing f "1 marked in other directories"))
      (is equal (list (tuatara.io:native (merge-pathnames "sub/c.txt" dir)) (tuatara.io:native (merge-pathnames "a.txt" dir)))
          (tuatara.defaults::rimu-chosen-paths (session-buffer editor)))
      ;; or only those of the directory shown
      (is equal (list (tuatara.io:native (merge-pathnames "sub/c.txt" dir)))
          (tuatara.defaults::rimu-chosen-paths (session-buffer editor) :here-only t)))
    (multiple-value-bind (f editor)
        (ops-session dir "g f a . t x t RET m g f s u b RET RET g f c . t x t RET m h" :width 140)
      (declare (ignore f))
      (true (find-if (lambda (l) (and (uiop:string-prefix-p "> " l) (search "a.txt" l))) (rimu-lines editor))))
    (let ((f (ops-session dir "g f a . t x t RET m g f s u b RET RET g f c . t x t RET m C" :width 400)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Copy 2 files in 2 directories to:")))
    ;; U takes away every mark, here and in the other directories, and the line saying so goes
    (multiple-value-bind (f editor)
        (ops-session dir "g f a . t x t RET m g f s u b RET RET U" :width 140)
      (true (tuatara.frontend.fake:fake-frame-showing f "1 mark taken away"))
      (false (search "marked in other directories" (session-text editor)))
      (is equal nil (tuatara.defaults::rimu-marked-elsewhere (session-buffer editor))))))

(define-test (rimu-operations marks-elsewhere-said)
  (with-operations-tree (dir)
    (alexandria:write-string-into-file "sub beta" (here dir "sub/b.txt"))
    ;; the line counts only the files marked (>) still there: one marked for deletion is not counted
    (multiple-value-bind (f editor) (ops-session dir "g f a . t x t RET d g f s u b RET RET" :width 140)
      (declare (ignore f))
      (false (search "marked in other directories" (session-text editor))))
    ;; a file marked gone meanwhile: no longer counted, and C says so, never taking the line's entry instead
    (multiple-value-bind (f editor)
        (ops-session dir (list "g f a . t x t RET m g f s u b RET RET"
                               (lambda () (delete-file (here dir "a.txt")))
                               "r C")
                     :width 200)
      (false (search "marked in other directories" (session-text editor)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Nothing to copy: the files marked in other directories are gone")))
    (alexandria:write-string-into-file "alpha" (here dir "a.txt"))
    ;; K is about this list's lines: with marks only elsewhere it takes out the line's entry
    (multiple-value-bind (f editor) (ops-session dir "g f a . t x t RET m g f s u b RET RET g f c . t x t RET K" :width 140)
      (true (tuatara.frontend.fake:fake-frame-showing f "1 line taken out of the list"))
      (false (find-if (lambda (l) (search "c.txt" l)) (rimu-lines editor)))
      (true (probe-file (here dir "sub/c.txt"))))
    ;; R and g w name where a file marked in another directory is
    (let ((f (ops-session dir "g f b . t x t RET m g f s u b RET RET g f c . t x t RET R C-g" :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f (format nil "Rename ~ab.txt to:" (tuatara.command::abbreviated-path (namestring dir))))))
    (let ((f (ops-session dir "g f b . t x t RET m g f s u b RET RET g f c . t x t RET g w n RET" :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f (format nil "Wipe ~ab.txt for good?" (tuatara.command::abbreviated-path (namestring dir)))))
      (true (probe-file (here dir "b.txt"))))
    (let ((f (ops-session dir "g f b . t x t RET m g f s u b RET RET m g w n RET" :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Wipe 2 files in 2 directories for good?")))
    ;; files of one name from two directories copied into one: asked first, only the first copied
    (let ((there (namestring (here dir "there/"))))
      (ensure-directories-exist there)
      (let ((f (ops-session dir (format nil "g f b . t x t RET m g f s u b RET RET g f b . t x t RET m C C-a C-k ~a C-j n RET"
                                        (rimu-typed there))
                            :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "2 files named b.txt: only one can be there; copy the one in"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Nothing copied"))
        (false (probe-file (here there "b.txt"))))
      (let ((f (ops-session dir (format nil "g f b . t x t RET m g f s u b RET RET g f b . t x t RET m C C-a C-k ~a C-j y RET"
                                        (rimu-typed there))
                            :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Copied 1 file"))
        (true (tuatara.frontend.fake:fake-frame-showing f "b.txt (another of its name goes there)"))
        (false (tuatara.frontend.fake:fake-frame-showing f ".tuatara-copy"))
        ;; the list's own first
        (is string= "sub beta" (alexandria:read-file-into-string (here there "b.txt")))))))

(define-test (rimu-operations put-back-only-where-it-may)
  (with-operations-tree (dir)
    (let ((trash (here dir "data/Trash/")))
      (ensure-directories-exist (here trash "info/"))
      (ensure-directories-exist (here trash "files/"))
      (flet ((trashed (name path)
               (alexandria:write-string-into-file "x" (here trash (format nil "files/~a" name)))
               (alexandria:write-string-into-file (format nil "[Trash Info]~%Path=~a~%DeletionDate=2026-10-07T10:00:00~%" path)
                                                  (here trash (format nil "info/~a.trashinfo" name)))))
        ;; a path that is not whole, or climbs with .., is refused, said why, the thing left in the trash
        (trashed "rel" "relput/x")
        (let ((f (ops-session dir "g t R" :width 300)))
          (true (tuatara.frontend.fake:fake-frame-showing f "Not put back: x: its .trashinfo gives no whole path"))
          (true (probe-file (here trash "files/rel"))))
        (delete-file (here trash "info/rel.trashinfo"))
        (trashed "up" (format nil "~asub/../../escaped" (tuatara.io:url-path (namestring dir))))
        (let ((f (ops-session dir "g t R" :width 300)))
          (true (tuatara.frontend.fake:fake-frame-showing f "Not put back: escaped: its .trashinfo's path climbs with .."))
          (true (probe-file (here trash "files/up"))))
        (delete-file (here trash "info/up.trashinfo"))
        ;; several: where they go is said
        (trashed "one" (format nil "~aone" (tuatara.io:url-path (namestring dir))))
        (trashed "two" (format nil "~asub/two" (tuatara.io:url-path (namestring dir))))
        (let ((f (ops-session dir "g t m m R n RET" :width 300)))
          (true (tuatara.frontend.fake:fake-frame-showing f "Put back the 2 things to "))
          (true (tuatara.frontend.fake:fake-frame-showing f (format nil "~asub/" (tuatara.command::abbreviated-path (namestring dir)))))))))
  ;; a drive's: within the drive, its folders as they really are
  (with-operations-tree (dir)
    (let* ((top (namestring (ensure-directories-exist (here dir "drive/"))))
           (outside (namestring (ensure-directories-exist (here dir "home/")))))
      (sb-posix:symlink outside (namestring (here dir "drive/link")))
      (flet ((refusal (path)
               (tuatara.defaults::trash-restore-refusal (tuatara.defaults::make-trashed "/t/" "n" top path "" nil))))
        (false (refusal (concatenate 'string top "a/b")))
        (true (refusal (concatenate 'string top "../home/x")))
        (true (refusal (concatenate 'string outside "x")))
        (true (refusal (concatenate 'string top "link/x")))
        (true (refusal (concatenate 'string top "link/new/x")))
        ;; a NUL or another control character, which a call to the system would cut the path at
        (is string= "its .trashinfo's path holds a control character"
            (refusal (format nil "~aa~cb" top (code-char 0))))
        (true (refusal (format nil "~aa~cb" top #\Newline)))))
    (is string= "its .trashinfo gives no whole path"
        (tuatara.defaults::trash-restore-refusal (tuatara.defaults::make-trashed "/t/" "n" nil "a/b" "" nil)))))

(define-test (rimu-operations questions-and-order)
  ;; what is asked names one file, or how many, and where they are when that is not here
  (is string= "Move a to the trash" (tuatara.defaults::rimu-question "Move" '("/d/a") :here "/d/" :after "to the trash"))
  (is string= "Move /e/a to the trash" (tuatara.defaults::rimu-question "Move" '("/e/a") :here "/d/" :after "to the trash"))
  (is string= "Copy 2 files to:" (tuatara.defaults::rimu-question "Copy" '("/d/a" "/d/b") :here "/d/" :after "to:"))
  (true (search "Copy 2 files in /e" (tuatara.defaults::rimu-question "Copy" '("/e/a" "/e/b") :here "/d/" :after "to:")))
  (is string= "Copy 2 files in 2 directories to:"
      (tuatara.defaults::rimu-question "Copy" '("/d/a" "/e/b") :here "/d/" :after "to:"))
  (is string= "2 files" (tuatara.defaults::rimu-question nil '("/d/a" "/e/b")))
  ;; the work is done in the order the files were chosen
  (is equal '(("/a" . 2) ("/b" . 3) ("/c" . 1))
      (tuatara.defaults::rimu-sort-pairs '(("/c" . 1) ("/a" . 2) ("/b" . 3)) '("/a" "/b" "/c")))
  ;; a mark is of the directory shown when its file is in it, or anywhere under it in a list of files found
  (true (tuatara.defaults::rimu-mark-here-p "/d/a" "/d/"))
  (false (tuatara.defaults::rimu-mark-here-p "/d/e/a" "/d/"))
  (true (tuatara.defaults::rimu-mark-here-p "/d/e/a" "/d/" t))
  (false (tuatara.defaults::rimu-mark-here-p "/dx/a" "/d/"))
  (false (tuatara.defaults::rimu-mark-here-p "/d/" "/d/")))

(define-test (rimu-operations take-out-lines)
  (with-operations-tree (dir)
    ;; K takes the line out of the list, the file left as it is
    (multiple-value-bind (f editor) (ops-session dir "g f a . t x t RET K")
      (let ((lines (rimu-lines editor)))
        (false (find-if (lambda (l) (search "a.txt" l)) lines))
        (true (find-if (lambda (l) (search "b.txt" l)) lines)))
      (true (tuatara.frontend.fake:fake-frame-showing f "1 line taken out of the list"))
      (true (probe-file (merge-pathnames "a.txt" dir))))
    ;; the marked ones, their marks gone with them; reading the list again shows them
    (multiple-value-bind (f editor) (ops-session dir "g f a . t x t RET m m K")
      (declare (ignore f))
      (let ((lines (rimu-lines editor)))
        (false (find-if (lambda (l) (or (search "a.txt" l) (search "b.txt" l))) lines))
        (true (find-if (lambda (l) (search "sub" l)) lines))))
    (multiple-value-bind (f editor) (ops-session dir "g f a . t x t RET m m K r")
      (declare (ignore f))
      (let ((lines (rimu-lines editor)))
        (true (find-if (lambda (l) (and (uiop:string-prefix-p "  " l) (search "a.txt" l))) lines))
        (true (find-if (lambda (l) (and (uiop:string-prefix-p "  " l) (search "b.txt" l))) lines))))))

(define-test (rimu-operations copy-and-move)
  (with-operations-tree (dir)
    (let ((there (namestring (merge-pathnames "there/" dir))))
      (ensure-directories-exist there)
      ;; C: into the directory given, modes and links kept, a directory with all in it
      (let ((f (ops-session dir (format nil "g f b . t x t RET m g f s u b RET m g f l i n k RET m C C-a C-k ~a C-j"
                                        (rimu-typed there)))))
        (true (tuatara.frontend.fake:fake-frame-showing f "Copied 3 files to"))
        (is string= "beta" (alexandria:read-file-into-string (here there "b.txt")))
        (is = #o640 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring (here there "b.txt"))))))
        (is string= "gamma" (alexandria:read-file-into-string (here there "sub/c.txt")))
        (is string= "a.txt" (sb-posix:readlink (namestring (here there "link"))))
        (true (probe-file (here dir "b.txt"))))
      ;; the question completes on directories, as C-x C-f does; the first line takes what is typed
      (let ((f (ops-session dir (format nil "g f b . t x t RET C C-a C-k ~a t h" (rimu-typed (namestring dir))) :height 30)))
        (true (tuatara.frontend.fake:fake-frame-showing f "there/")))
      ;; TAB on the line typed (selected, a directory) leaves it as it is, again and again; C-j then takes there/ (b.txt is in it)
      (let ((f (ops-session dir (format nil "g f b . t x t RET C C-a C-k ~a TAB TAB C-j" (rimu-typed (namestring (here dir "there/"))))
                            :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "b.txt is there already")))
      ;; again: asked whether to replace; no leaves them; a directory there is not merged
      (let ((f (ops-session dir (format nil "g f b . t x t RET m g f s u b RET m C C-a C-k ~a C-j n RET" (rimu-typed there)))))
        (true (tuatara.frontend.fake:fake-frame-showing f "b.txt is there already: replace, the old to the trash?"))
        (true (tuatara.frontend.fake:fake-frame-showing f "sub (a directory is there)")))
      (alexandria:write-string-into-file "old beta" (here there "b.txt") :if-exists :supersede)
      (let ((f (ops-session dir (format nil "g f b . t x t RET C C-a C-k ~a C-j y RET" (rimu-typed there)))))
        (true (tuatara.frontend.fake:fake-frame-showing f "1 replaced, the old in the trash"))
        (is string= "beta" (alexandria:read-file-into-string (here there "b.txt")))
        (true (probe-file (here dir "data/Trash/files/b.txt"))))
      ;; R: one entry given a new name; a buffer visiting it follows
      (let ((visiting (tuatara.io:visit-file (namestring (here dir "a.txt")))))
        (ops-session dir "g f a . t x t RET R C-a C-k r e n a m e d . t x t RET")
        (false (probe-file (here dir "a.txt")))
        (is string= "alpha" (alexandria:read-file-into-string (here dir "renamed.txt")))
        (is equal (tuatara.io:expand-path (namestring (here dir "renamed.txt"))) (tuatara.buffer:buffer-file visiting))
        (tuatara.buffer:kill-buffer visiting))
      ;; R: a name only; many marked, or a directory given, point elsewhere
      (let ((f (ops-session dir "g f r e n a m e d . t x t RET R C-a C-k s u b / x RET")))
        (true (tuatara.frontend.fake:fake-frame-showing f "sub/x is no name: a name only"))
        (true (probe-file (here dir "renamed.txt"))))
      (let ((f (ops-session dir "g f b . t x t RET m m R")))
        (true (tuatara.frontend.fake:fake-frame-showing f "2 marked: R renames one")))
      ;; M: into a directory, navigated to; what is no directory is refused
      (let ((f (ops-session dir (format nil "g f r e n a m e d . t x t RET M C-a C-k ~a C-j" (rimu-typed there)) :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Moved 1 file to"))
        (true (probe-file (here there "renamed.txt"))))
      (let ((f (ops-session dir (format nil "g f b . t x t RET M C-a C-k ~a C-j" (rimu-typed (namestring (here dir "nowhere.txt")))) :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "is no directory: nothing moved"))
        (true (probe-file (here dir "b.txt"))))
      ;; a directory is not moved into itself
      (let ((f (ops-session dir (format nil "g f s u b RET M C-a C-k ~a C-j" (rimu-typed (namestring (here dir "sub/")))))))
        (true (tuatara.frontend.fake:fake-frame-showing f "sub (itself)"))
        (true (probe-file (here dir "sub/c.txt")))))))

(define-test (rimu-operations trash-and-delete)
  (with-operations-tree (dir)
    ;; D: to the trash, asked first, with a .trashinfo saying where it was
    (let ((f (ops-session dir "g f b . t x t RET D y")))
      (true (tuatara.frontend.fake:fake-frame-showing f "Move b.txt to the trash? (y or n)"))
      (true (tuatara.frontend.fake:fake-frame-showing f "Moved to the trash 1 file"))
      (false (probe-file (here dir "b.txt")))
      (is string= "beta" (alexandria:read-file-into-string (here dir "data/Trash/files/b.txt")))
      (let ((info (alexandria:read-file-into-string (here dir "data/Trash/info/b.txt.trashinfo"))))
        (true (search "[Trash Info]" info))
        (true (search (format nil "Path=~a" (tuatara.io:url-path (namestring (here dir "b.txt")))) info))
        (true (search "DeletionDate=" info))))
    ;; the same name again: a name of its own in the trash
    (alexandria:write-string-into-file "beta 2" (here dir "b.txt"))
    (ops-session dir "g f b . t x t RET D y")
    (is string= "beta 2" (alexandria:read-file-into-string (here dir "data/Trash/files/b.txt.2")))
    ;; d and x: those marked for deletion, a directory too
    (let ((f (ops-session dir "g f s u b RET d x y RET")))
      (true (tuatara.frontend.fake:fake-frame-showing f "Move sub to the trash?"))
      (false (probe-file (here dir "sub/")))
      (true (probe-file (here dir "data/Trash/files/sub/c.txt"))))
    ;; n: nothing done
    (ops-session dir "g f a . t x t RET D n RET")
    (true (probe-file (here dir "a.txt")))
    ;; C-u D: for good, asked twice
    (let ((f (ops-session dir "g f a . t x t RET C-u D y RET y RET")))
      (true (tuatara.frontend.fake:fake-frame-showing f "Really delete a.txt for good? It cannot be put back."))
      (true (tuatara.frontend.fake:fake-frame-showing f "Deleted for good 1 file"))
      (false (probe-file (here dir "a.txt")))
      (false (probe-file (here dir "data/Trash/files/a.txt"))))
    ;; the link: the link itself goes, not where it leads
    (alexandria:write-string-into-file "alpha" (here dir "a.txt"))
    (ops-session dir "g f l i n k RET D y")
    (true (probe-file (here dir "a.txt")))
    (true (sb-posix:readlink (namestring (here dir "data/Trash/files/link"))))))

(define-test (rimu-operations trash-info-read-safely)
  ;; a trash on a drive not to be trusted: a .trashinfo that is a named pipe, a link to an endless
  ;; device, or very large, is left out without holding the list up or filling memory; a line end
  ;; in where a thing was is shown as ?, one line for each thing
  (with-operations-tree (dir)
    (let ((info (here dir "data/Trash/info/"))
          (fifo (namestring (here dir "data/Trash/info/pipe.trashinfo"))))
      (ensure-directories-exist info)
      (ensure-directories-exist (here dir "data/Trash/files/"))
      (alexandria:write-string-into-file
       (format nil "[Trash Info]~%Path=~aline%0Abreak.txt~%DeletionDate=2026-10-07T10:00:00~%"
               (tuatara.io:url-path (namestring dir)))
       (here dir "data/Trash/info/good.trashinfo"))
      (sb-posix:mkfifo fifo #o600)
      (sb-posix:symlink "/dev/zero" (namestring (here dir "data/Trash/info/zero.trashinfo")))
      (alexandria:write-string-into-file
       (format nil "[Trash Info]~%Path=/big~%DeletionDate=2026-10-07T10:00:00~%~a" (make-string 70000 :initial-element #\x))
       (here dir "data/Trash/info/big.trashinfo"))
      (let* ((reader (sb-thread:make-thread (lambda () (tuatara.defaults::trash-contents)) :name "tuatara test trash reader"))
             (things (sb-thread:join-thread reader :default :hung :timeout 20)))
        (when (eq things :hung)
          ;; a reader held on the pipe let go: opened for writing and closed, it reads its end
          (ignore-errors (sb-posix:close (sb-posix:open fifo (logior sb-posix:o-wronly sb-posix:o-nonblock))))
          (sb-thread:join-thread reader :default nil :timeout 5))
        (false (eq things :hung))
        (is = 1 (length things))
        (is string= (format nil "~aline~%break.txt" (namestring dir)) (tuatara.defaults::trashed-path (first things))))
      (ops-session dir "M-x r i m u - t r a s h RET")
      (let ((list (tuatara.buffer:find-buffer "*Trash*")))
        (true list)
        (let ((text (tuatara.buffer:buffer-string list)))
          ;; the titles, the dashes, the section's heading, the one line of the thing, an empty
          ;; line and the count
          (is = 6 (count #\Newline text))
          (true (search "line^Jbreak.txt" text)))
        (tuatara.buffer:kill-buffer list)))))

(define-test (rimu-operations make-directory)
  (with-operations-tree (dir)
    (let ((f (ops-session dir (format nil "+ C-a C-k ~a C-j" (rimu-typed (namestring (here dir "new/deeper/")))))))
      (true (tuatara.frontend.fake:fake-frame-showing f "Made"))
      (true (probe-file (here dir "new/deeper/"))))
    (let ((f (ops-session dir (format nil "+ C-a C-k ~a C-j" (rimu-typed (namestring (here dir "sub/")))))))
      (true (tuatara.frontend.fake:fake-frame-showing f "is there already")))
    ;; a name with a [ and a * in it is made as it is named, those above it too
    (let* ((path (concatenate 'string (tuatara.io:native dir) "case [1]*/deeper/"))
           (f (ops-session dir (format nil "+ C-a C-k ~a C-j" (rimu-typed path)))))
      (true (tuatara.frontend.fake:fake-frame-showing f "Made"))
      (is eq :directory (tuatara.io:path-kind path)))))

(define-test (rimu-operations rename-formatted)
  (with-operations-tree (dir)
    (alexandria:write-string-into-file (format nil "Some notes.~%") (here dir "notes.org"))
    (alexandria:write-string-into-file (format nil "#+title: Old~%#+filetags: :old:~%~%Body.~%")
                                       (here dir "20260101T101010--old__old.org"))
    ;; a file with no formatted name: its identifier from when it was changed, the header put in and saved
    (let ((f (ops-session dir "g f n o t e s . o r g RET SPC d r C-a C-k M y SPC n o t e s RET p r o j e c t RET RET")))
      (true (tuatara.frontend.fake:fake-frame-showing f "Renamed to"))
      (let ((renamed (find-if (lambda (name) (search "--my-notes__project.org" name))
                              (mapcar #'first (tuatara.io:directory-entries dir)))))
        (true renamed)
        (true (tuatara.defaults::identifier-at-start-p renamed))
        (let ((text (alexandria:read-file-into-string (here dir renamed))))
          (true (uiop:string-prefix-p "#+title: My notes" text))
          (true (search "#+filetags: :project:" text))
          (true (search "Some notes." text)))))
    ;; a formatted one: its identifier kept, the header's title and keywords written anew
    (ops-session dir "g f 2 0 2 6 0 1 0 1 RET SPC d r C-a C-k N e w SPC t i t l e RET g a r d e n RET RET")
    (let ((text (alexandria:read-file-into-string (here dir "20260101T101010--new-title__garden.org"))))
      (true (uiop:string-prefix-p (format nil "#+title: New title~%#+filetags: :garden:~%") text))
      (true (search "Body." text)))
    ;; keywords only, for the marked files: the header's title kept as it is written
    (ops-session dir "g f 2 0 2 6 0 1 0 1 RET SPC d k a l p h a RET RET")
    (true (probe-file (here dir "20260101T101010--new-title__alpha.org")))
    (true (uiop:string-prefix-p (format nil "#+title: New title~%#+filetags: :alpha:~%")
                                (alexandria:read-file-into-string (here dir "20260101T101010--new-title__alpha.org"))))
    ;; and offered when it is renamed again
    (ops-session dir "g f 2 0 2 6 0 1 0 1 RET SPC d r C-e SPC t w o RET RET")
    (true (uiop:string-prefix-p "#+title: New title two"
                                (alexandria:read-file-into-string (here dir "20260101T101010--new-title-two__alpha.org"))))))

(define-test (rimu-operations formatted-rename-waits-for-a-save)
  ;; a file whose buffer is being saved keeps its name: what the save writes
  ;; would land at the old one
  (with-test-directory (dir)
    (alexandria:write-string-into-file "notes" (here dir "notes.txt"))
    (let* ((path (tuatara.io:native (here dir "notes.txt")))
           (buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "notes.txt"))))
      (setf (tuatara.buffer:buffer-file buffer) (tuatara.io:expand-path path))
      (sb-int:encapsulate 'tuatara.io:save-of 'saving-now (lambda (save-of b) (or (eq b buffer) (funcall save-of b))))
      (unwind-protect
           (fail (tuatara.defaults::rename-file-formatted path "My notes" '("work")))
        (sb-int:unencapsulate 'tuatara.io:save-of 'saving-now)
        (tuatara.buffer:kill-buffer buffer))
      (true (probe-file (here dir "notes.txt"))))))

(define-test (rimu-operations copies-whole-or-said)
  (with-operations-tree (dir)
    ;; a directory into itself through a symbolic link to it: left alone, never copied on and on
    (sb-posix:symlink "sub" (namestring (here dir "sublink")))
    (ensure-directories-exist (here dir "sub/inner/"))
    (let ((f (ops-session dir (format nil "g f s u b RET C C-a C-k ~a C-j" (rimu-typed (namestring (here dir "sublink/inner/"))))
                          :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f "sub (itself)"))
      (false (probe-file (here dir "sub/inner/sub/"))))
    ;; a link itself copied into the directory it leads to: a copy of the link, not itself
    (let ((f (ops-session dir (format nil "g f s u b l i n k RET C C-a C-k ~a C-j" (rimu-typed (namestring (here dir "sub/"))))
                          :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Copied 1 file"))
      (is string= "sub" (sb-posix:readlink (namestring (here dir "sub/sublink")))))
    ;; a named pipe in a directory copied: left out, and said; the copy not counted whole
    (sb-posix:mkfifo (namestring (here dir "sub/pipe")) #o600)
    (false (tuatara.io:copy-path-tree (namestring (here dir "sub/")) (namestring (here dir "copy"))))
    (true (probe-file (here dir "copy/c.txt")))
    (let ((f (ops-session dir (format nil "g f s u b RET C C-a C-k ~a C-j" (rimu-typed (namestring (here dir "there/"))))
                          :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f "left out: pipe (a named pipe)")))
    ;; a move that fails: the buffer visiting the file stays with it, where it still is
    (ensure-directories-exist (here dir "shut/"))
    (sb-posix:chmod (namestring (here dir "shut/")) #o500)
    (let ((visiting (tuatara.io:visit-file (namestring (here dir "a.txt")))))
      (unwind-protect
           (let ((f (ops-session dir (format nil "g f a . t x t RET M C-a C-k ~a C-j" (rimu-typed (namestring (here dir "shut/"))))
                                 :width 300)))
             (true (tuatara.frontend.fake:fake-frame-showing f "not done: a.txt"))
             (true (probe-file (here dir "a.txt")))
             (is equal (tuatara.io:expand-path (namestring (here dir "a.txt"))) (tuatara.buffer:buffer-file visiting)))
        (sb-posix:chmod (namestring (here dir "shut/")) #o700)
        (tuatara.buffer:kill-buffer visiting)))
    ;; a copy stopped part of the way: not counted whole
    (false (tuatara.io:copy-path-tree (namestring (here dir "a.txt")) (namestring (here dir "a2.txt")) :cancel (list t)))
    ;; stopped half-way (on tmpfs, where a copy goes piece by piece rather than shared at once):
    ;; nothing under its name, and no hidden part left beside it
    (when (probe-file "/dev/shm/")
      (let ((shm (format nil "/dev/shm/tuatara-test-copy-~d/" (random 1000000))) (cancel (list nil)))
        (ensure-directories-exist shm)
        (unwind-protect
             (progn
               (alexandria:write-byte-vector-into-file (make-array (* 3 1024 1024) :element-type '(unsigned-byte 8) :initial-element 1)
                                                       (concatenate 'string shm "half.bin"))
               (false (tuatara.io:copy-path-tree (concatenate 'string shm "half.bin") (concatenate 'string shm "half-copy.bin")
                                                 :cancel cancel :progress (lambda (n) (declare (ignore n)) (setf (car cancel) t))))
               (is equal (list (concatenate 'string shm "half.bin")) (mapcar #'namestring (directory (concatenate 'string shm "*.*")))))
          (ignore-errors (tuatara.io:delete-tree shm)))))
    ;; a move across file systems that cannot be whole: nothing of it left there, the file where it was
    (when (and (probe-file "/dev/shm/") (not (eql (tuatara.io:stamp-dev (tuatara.io:path-stamp "/dev/shm/"))
                                                  (tuatara.io:stamp-dev (tuatara.io:path-stamp (namestring dir))))))
      (let ((there (format nil "/dev/shm/tuatara-test-move-~d/" (random 1000000))))
        (ensure-directories-exist (here dir "withpipe/"))
        (alexandria:write-string-into-file "x" (here dir "withpipe/f.txt"))
        (sb-posix:mkfifo (namestring (here dir "withpipe/pipe")) #o600)
        (ensure-directories-exist there)
        (unwind-protect
             (progn
               (fail (tuatara.io:move-path (namestring (here dir "withpipe/")) (concatenate 'string there "withpipe")))
               (true (probe-file (here dir "withpipe/f.txt")))
               (false (tuatara.io:path-stamp (concatenate 'string there "withpipe") :follow nil))
               (is equal '() (directory (concatenate 'string there "*.*"))))
          (ignore-errors (tuatara.io:delete-tree there)))))
    ;; and in a list, taken away, never left holding part of the file
    (let ((from (namestring (here dir "big.bin"))) (to (namestring (here dir "big-copy.bin"))))
      (alexandria:write-byte-vector-into-file (make-array (* 4 1024 1024) :element-type '(unsigned-byte 8) :initial-element 7) from)
      (true (search "stopped" (handler-case (progn (tuatara.defaults::rimu-work :copy (cons from to) (list t)) "")
                                (error (e) (princ-to-string e)))))
      (false (probe-file to))))
  ;; the operations under way are asked about before exiting, and stopped only once the exit is certain
  (let ((tuatara.runtime:*jobs*
          (list (tuatara.defaults::make-rimu-operation :copy (list (cons "/a" "/b")) nil nil))))
    (sb-int:encapsulate 'tuatara.command:y-or-n-p* 'test (lambda (f prompt &rest how) (declare (ignore f prompt how)) t))
    (unwind-protect
         (let ((answer (tuatara.defaults::jobs-quit-query)))
           (true (functionp answer))
           (false (car (tuatara.defaults::rimu-operation-cancel (first tuatara.runtime:*jobs*)))))
      (sb-int:unencapsulate 'tuatara.command:y-or-n-p* 'test))))

(define-test (rimu-operations trash-on-other-drives)
  ;; Path= on another drive is counted from its top, read back from it
  (is string= "a/b c" (tuatara.io::trash-info-path "/run/media/u/USB/a/b c" "/run/media/u/USB/"))
  (is string= "srv/x" (tuatara.io::trash-info-path "/srv/x" "/"))
  (is string= "/home/u/x" (tuatara.io::trash-info-path "/home/u/x" nil))
  ;; a mount point with a space, a tab or a backslash in its name, as /proc/self/mounts writes it
  (is string= (format nil "/run/media/u/My Disk~c\\x" #\Tab) (tuatara.defaults::mount-field "/run/media/u/My\\040Disk\\011\\134x"))
  ;; a drive's trash is used only when it is made of directories themselves, yours: a link in it could lead
  ;; deleting or putting back anywhere; a shared .Trash needs the sticky bit
  (with-operations-tree (dir)
    (let* ((top (namestring (here dir "drive/")))
           (uid (sb-posix:getuid))
           (trash (format nil "~a.Trash-~d/" top uid))
           (decoy (namestring (ensure-directories-exist (here dir "home/Documents/")))))
      (ensure-directories-exist (format nil "~afiles/" trash))
      (ensure-directories-exist (format nil "~ainfo/" trash))
      (true (tuatara.io:drive-trash-sound-p trash top))
      (sb-posix:rmdir (format nil "~afiles" trash))
      (sb-posix:symlink decoy (format nil "~afiles" trash))
      (false (tuatara.io:drive-trash-sound-p trash top))
      (let ((shared (format nil "~a.Trash/~d/" top uid)))
        (ensure-directories-exist (format nil "~afiles/" shared))
        (ensure-directories-exist (format nil "~ainfo/" shared))
        (false (tuatara.io:drive-trash-sound-p shared top))
        (sb-posix:chmod (format nil "~a.Trash" top) #o1777)
        (true (tuatara.io:drive-trash-sound-p shared top)))))
  ;; where a drive's thing was: within the drive, its . and .. taken as written
  (true (tuatara.io:path-within-p "/run/media/u/USB/a/b" "/run/media/u/USB/"))
  (false (tuatara.io:path-within-p "/run/media/u/USB/../../../home/u/.bashrc" "/run/media/u/USB/"))
  (false (tuatara.io:path-within-p "/home/u/.bashrc" "/run/media/u/USB/"))
  ;; a name that starts with the drive's, another drive
  (false (tuatara.io:path-within-p "/run/media/u/USBX/a" "/run/media/u/USB"))
  (true (tuatara.io:path-within-p "/run/media/u/USB" "/run/media/u/USB/")))

(define-test (rimu-operations formatted-headers-written-anew)
  (flet ((anew (style text)
           (tuatara.defaults::text-with-header text (assoc style tuatara.defaults::*formatted-file-types*)
                                               "New" '("a" "b") "20260101T101010" 0)))
    ;; Org's keywords in capitals, kept in capitals
    (is string= (format nil "#+TITLE: New~%#+FILETAGS: :a:b:~%~%Body.~%")
        (anew :org (format nil "#+TITLE: Old~%#+FILETAGS: :old:~%~%Body.~%")))
    ;; lines ending in CR LF keep those ends, the new line too
    (is string= (format nil "#+title: New~c~%#+filetags: :a:b:~c~%~c~%Body.~c~%" #\Return #\Return #\Return #\Return)
        (anew :org (format nil "#+title: Old~c~%~c~%Body.~c~%" #\Return #\Return #\Return)))
    ;; lines ending both ways: left alone
    (false (anew :org (format nil "#+title: Old~c~%~%Body.~%" #\Return)))
    ;; a text file's header ends where its fields do: a tags: line in the body is the body's
    (is string= (format nil "title: New~%tags: a  b~%Body.~%tags: mine~%")
        (anew :text (format nil "title: Old~%Body.~%tags: mine~%")))
    ;; Markdown's keywords a line each, replaced with the new ones
    (is string= (format nil "---~%title: \"New\"~%tags: [\"a\", \"b\"]~%date: x~%---~%Body.~%")
        (anew :markdown (format nil "---~%title: \"Old\"~%tags:~%  - old~%  - older~%date: x~%---~%Body.~%"))))
  ;; a file that is not all text: renamed, its octets untouched, and said
  (with-operations-tree (dir)
    (let ((octets (concatenate '(vector (unsigned-byte 8)) (tuatara.text:string-octets (format nil "#+title: Old~%"))
                               #(233 116 233 10))))
      (alexandria:write-byte-vector-into-file octets (here dir "latin.org"))
      (let ((f (ops-session dir "g f l a t i n RET SPC d r C-a C-k L a t i n RET RET" :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "is not all text"))
        (let ((renamed (find-if (lambda (name) (search "--latin.org" name)) (mapcar #'first (tuatara.io:directory-entries dir)))))
          (true renamed)
          (is equalp octets (alexandria:read-file-into-byte-vector (here dir renamed))))))))

(define-test (rimu-operations home-and-back)
  (with-operations-tree (dir)
    ;; g h: the home directory; g l: back to the one before, point on the entry it was on; again, forth
    (multiple-value-bind (f editor) (ops-session dir "g f b . t x t RET g h")
      (declare (ignore f))
      (is string= (tuatara.io:directory-name (tuatara.io:native (tuatara.io:home-directory)) :clean t)
          (tuatara.buffer:buffer-local (tuatara.display:window-buffer (tuatara.command:editor-window editor))
                                       'tuatara.defaults::rimu-directory)))
    (multiple-value-bind (f editor) (ops-session dir "g f b . t x t RET g h g l")
      (declare (ignore f))
      (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
        (is string= (namestring dir) (tuatara.buffer:buffer-local buffer 'tuatara.defaults::rimu-directory))
        (is string= "b.txt" (tuatara.io:listed-name (tuatara.defaults::rimu-entry-at buffer (tuatara.buffer:buffer-point buffer))))))
    (multiple-value-bind (f editor) (ops-session dir "g h g l g l")
      (declare (ignore f))
      (is string= (tuatara.io:directory-name (tuatara.io:native (tuatara.io:home-directory)) :clean t)
          (tuatara.buffer:buffer-local (tuatara.display:window-buffer (tuatara.command:editor-window editor))
                                       'tuatara.defaults::rimu-directory)))
    ;; nothing before: said
    (let ((f (ops-session dir "g l")))
      (true (tuatara.frontend.fake:fake-frame-showing f "No directory listed before in this list")))
    ;; marks stay with their files through the directories visited
    (multiple-value-bind (f editor) (ops-session dir "g f b . t x t RET m g h g l")
      (declare (ignore f))
      (true (find-if (lambda (l) (and (uiop:string-prefix-p "> " l) (search "b.txt" l))) (rimu-lines editor))))))

(defun disk-change-session (directory change shown &key (follow t) before)
  "List DIRECTORY, type the keys BEFORE when given, make CHANGE (a function) to
it as another program would, and wait up to 10 seconds, the editor going on,
for the list to hold the text SHOWN; the frontend and the editor, and whether
it did."
  (rimu-forget)
  (let ((tuatara-user:*rimu-preview* nil)
        (tuatara-user:*rimu-follow-changes* follow)
        (f (tuatara.frontend.fake:make-fake-frontend :width 120 :height 20))
        (start nil) (seen nil))
    (tuatara.frontend.fake:fake-queue-keys f (format nil "C-x d C-a C-k ~a C-j~@[ ~a~]" (rimu-typed (namestring directory)) before))
    (labels ((check ()
               (let ((list (find-if (lambda (b) (and (tuatara.defaults::rimu-list-p b)
                                                     (equal (tuatara.buffer:buffer-local b 'tuatara.defaults::rimu-directory)
                                                            (namestring directory))))
                                    tuatara.buffer:*buffers*)))
                 (cond ((null start)
                        (setf start (get-internal-real-time))
                        (sleep 0.3)                      ; the watch in place
                        (funcall change)
                        (tuatara.frontend.fake:fake-queue-action f #'check))
                       ((and list (search shown (tuatara.buffer:buffer-string list)))
                        (setf seen t))
                       ((< (- (get-internal-real-time) start) (* 10 internal-time-units-per-second))
                        (sleep 0.05)
                        (tuatara.frontend.fake:fake-queue-action f #'check))))))
      (tuatara.frontend.fake:fake-queue-action f #'check))
    (let ((editor (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults)))
      (values f editor seen))))

(define-test (rimu-operations follows-the-disk)
  (with-operations-tree (dir)
    ;; a file made, one deleted, one renamed by another program: the list shows it by itself
    (true (nth-value 2 (disk-change-session dir (lambda () (alexandria:write-string-into-file "x" (here dir "made-elsewhere.txt")))
                                            "made-elsewhere.txt")))
    (false (nth-value 2 (disk-change-session dir (lambda () (delete-file (here dir "b.txt"))) "no-such-name")))
    (multiple-value-bind (f editor) (disk-change-session dir (lambda () (rename-file (here dir "a.txt") (here dir "renamed-elsewhere.txt")))
                                                         "renamed-elsewhere.txt")
      (declare (ignore f))
      (let ((text (tuatara.buffer:buffer-string (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
        (true (search "renamed-elsewhere.txt" text))
        (false (search "a.txt" text))
        (false (search "b.txt" text))))
    ;; with *rimu-follow-changes* NIL, the list stays as it was read
    (false (nth-value 2 (disk-change-session dir (lambda () (alexandria:write-string-into-file "x" (here dir "quiet.txt")))
                                             "quiet.txt" :follow nil)))))

(define-test (rimu-operations symbolic-and-hard-links)
  (with-operations-tree (dir)
    (let ((there (namestring (merge-pathnames "there/" dir))))
      (ensure-directories-exist there)
      ;; S: links to the marked entries in the directory given, each to its entry's whole name
      (let ((f (ops-session dir (format nil "g f b . t x t RET m g f s u b RET m Y C-a C-k ~a C-j" (rimu-typed there)) :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Symlink 2 files to:"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Symlinked 2 files to"))
        (is string= (namestring (here dir "b.txt")) (sb-posix:readlink (namestring (here there "b.txt"))))
        (is string= (namestring (here dir "sub")) (sb-posix:readlink (namestring (here there "sub"))))
        (is string= "gamma" (alexandria:read-file-into-string (here there "sub/c.txt"))))
      ;; one link given a new name
      (ops-session dir (format nil "g f a . t x t RET Y C-a C-k ~a C-j" (rimu-typed (namestring (here there "alias.txt")))))
      (is string= "alpha" (alexandria:read-file-into-string (here there "alias.txt")))
      ;; L: a second name of one file; writing to one is seen by the other
      (let ((f (ops-session dir (format nil "g f a . t x t RET L C-a C-k ~a C-j" (rimu-typed (namestring (here there "second.txt"))))
                            :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Hard linked 1 file to")))
      (is = (sb-posix:stat-ino (sb-posix:stat (namestring (here dir "a.txt"))))
          (sb-posix:stat-ino (sb-posix:stat (namestring (here there "second.txt")))))
      ;; a directory has no second name: said, nothing made
      (let ((f (ops-session dir (format nil "g f s u b RET L C-a C-k ~a C-j" (rimu-typed (namestring (here there "sub2")))) :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "not done: sub"))
        (false (tuatara.io:path-stamp (namestring (here there "sub2")) :follow nil)))
      ;; a name there: asked whether to replace, as a copy asks
      (let ((f (ops-session dir (format nil "g f b . t x t RET Y C-a C-k ~a C-j n RET" (rimu-typed there)) :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "b.txt is there already: replace, the old to the trash?"))))))

(define-test (rimu-operations archives-made-and-extracted)
  (with-operations-tree (dir)
    (let ((there (namestring (merge-pathnames "there/" dir))))
      (ensure-directories-exist there)
      ;; c: entries marked in two directories into one archive, kept below the directory both are in
      (let ((f (ops-session dir (format nil "g f b . t x t RET m g f s u b RET l g f c . t x t RET m c C-a C-k ~a C-j"
                                        (rimu-typed (namestring (here there "both.tar.gz"))))
                            :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Archive 2 files in 2 directories as:"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Archived 2 files to")))
      (is eq :gzip (nth-value 1 (tuatara.defaults::file-kind (namestring (here there "both.tar.gz")))))
      (false (remove-if-not (lambda (p) (search ".part" (namestring p))) (uiop:directory-files there)))
      ;; Z: extracted into a directory of its name, offered beside it
      (let ((f (ops-session there "g f b o t h . t a r . g z RET Z C-j" :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Extracted 1 archive to")))
      (is string= "beta" (alexandria:read-file-into-string (here there "both/b.txt")))
      (is string= "gamma" (alexandria:read-file-into-string (here there "both/sub/c.txt")))
      ;; C-c e: what it holds, a line each, nothing unpacked
      (let ((listing (tuatara.defaults::archive-listing (namestring (here there "both.tar.gz")))))
        (true (search " b.txt" listing))
        (true (search " sub/c.txt" listing))
        (true (search "-rw" listing)))
      (ops-session there "g f b o t h . t a r . g z RET C-c e")
      (let ((shown (tuatara.buffer:find-buffer "*Archive: both.tar.gz*")))
        (true shown)
        (when shown
          (loop repeat 200 until (search "sub/c.txt" (tuatara.buffer:buffer-string shown)) do (sleep 0.02))
          (true (search "sub/c.txt" (tuatara.buffer:buffer-string shown)))
          (true (tuatara.buffer:buffer-local shown :read-only))
          (tuatara.buffer:kill-buffer shown)))
      (false (probe-file (here there "b.txt")))
      ;; the name says how it is compressed; the contents say how it is extracted
      (ops-session dir (format nil "g f a . t x t RET c C-a C-k ~a C-j" (rimu-typed (namestring (here there "one.tar.bz2")))))
      (is eq :bzip2 (nth-value 1 (tuatara.defaults::file-kind (namestring (here there "one.tar.bz2")))))
      (rename-file (here there "one.tar.bz2") (here there "renamed.tar"))
      (ops-session there (format nil "g f r e n a m e d . t a r RET Z C-a C-k ~a C-j" (rimu-typed (namestring (here there "x/")))))
      (is string= "alpha" (alexandria:read-file-into-string (here there "x/a.txt")))
      ;; a name of no archive: said, nothing made
      (let ((f (ops-session dir (format nil "g f a . t x t RET c C-a C-k ~a C-j" (rimu-typed (namestring (here there "a.zip"))))
                            :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Name the archive .tar, .tar.gz"))
        (false (probe-file (here there "a.zip"))))
      ;; an archive naming a place above where it is extracted writes nothing there
      (ensure-directories-exist (here dir "evil/inner/"))
      (alexandria:write-string-into-file "evil" (here dir "evil/victim.txt"))
      (uiop:run-program (list "tar" "-C" (namestring (here dir "evil/inner/")) "-cPf" (namestring (here dir "evil.tar"))
                              "../victim.txt"))
      (delete-file (here dir "evil/victim.txt"))
      (ops-session dir (format nil "g f e v i l . t a r RET Z C-a C-k ~a C-j" (rimu-typed (namestring (here dir "evil/inner/")))))
      (false (probe-file (here dir "evil/victim.txt"))))))

(define-test (rimu-operations differences)
  (with-operations-tree (dir)
    (flet ((shown (name text)
             ;; the buffer NAME once it shows TEXT, or NIL; killed after
             (let ((buffer (tuatara.buffer:find-buffer name)))
               (when buffer
                 (loop repeat 200 until (search text (tuatara.buffer:buffer-string buffer)) do (sleep 0.02))
                 (prog1 (search text (tuatara.buffer:buffer-string buffer))
                   (tuatara.buffer:kill-buffer buffer))))))
      ;; =: the two marked, the first shown the old
      (ops-session dir "g f a . t x t RET m m =")
      (true (shown "*Diff: a.txt and b.txt*" (format nil "-alpha~%")))
      ;; one marked compared with another asked for: the same says so
      (alexandria:write-string-into-file "alpha" (here dir "sub/same.txt"))
      (ops-session dir (format nil "g f a . t x t RET = C-a C-k ~a C-j" (rimu-typed (namestring (here dir "sub/same.txt")))))
      (true (shown "*Diff: a.txt and same.txt*" "No differences:"))
      ;; M-x diff-files: the entry on the line with another asked for
      (ops-session dir (format nil "g f b . t x t RET M-x d i f f - f i l e s RET C-a C-k ~a C-j"
                               (rimu-typed (namestring (here dir "sub/same.txt")))))
      (true (shown "*Diff: b.txt and same.txt*" "+alpha"))
      ;; two directories with all in them, a file only one holds against none; other/ is shown first
      (ensure-directories-exist (here dir "other/"))
      (alexandria:write-string-into-file "delta" (here dir "other/c.txt"))
      (alexandria:write-string-into-file "new" (here dir "other/only.txt"))
      (ops-session dir "g f s u b RET m g f o t h e r RET m =")
      (let ((buffer (tuatara.buffer:find-buffer "*Diff: other and sub*")))
        (true buffer)
        (when buffer
          (loop repeat 200 until (search "only.txt" (tuatara.buffer:buffer-string buffer)) do (sleep 0.02))
          (let ((text (tuatara.buffer:buffer-string buffer)))
            (true (search "-delta" text))
            (true (search "+gamma" text))
            (true (search "-new" text)))))
      ;; a file against a folder: with the file of its name there, none made up when there is none
      (ops-session dir "g f a . t x t RET m g f o t h e r RET m =")
      (true (or (shown "*Diff: a.txt and other*" "could not be compared") (shown "*Diff: other and a.txt*" "could not be compared")))
      ;; two comparisons of files of the same names: the second leaves the first be
      (dolist (folder '("p/" "q/" "r/" "s/"))
        (ensure-directories-exist (here dir folder))
        (alexandria:write-string-into-file folder (here dir (concatenate 'string folder "x.txt"))))
      (ops-session (here dir "p/") (format nil "g f x . t x t RET = C-a C-k ~a C-j" (rimu-typed (namestring (here dir "q/x.txt")))))
      (ops-session (here dir "r/") (format nil "g f x . t x t RET = C-a C-k ~a C-j" (rimu-typed (namestring (here dir "s/x.txt")))))
      (let ((both (remove-if-not (lambda (b) (search "*Diff: x.txt and x.txt*" (tuatara.buffer:buffer-name b))) tuatara.buffer:*buffers*)))
        (is = 2 (length both))
        (mapc #'tuatara.buffer:kill-buffer both))
      ;; three marked: said, nothing compared
      (let ((f (ops-session dir "g f a . t x t RET m m m =" :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "3 entries are marked: mark two"))))))

(define-test (rimu-operations link-onto-what-it-leads-to)
  ;; a link copied, or linked to, onto the very file it leads to is refused:
  ;; replacing that file would leave a link to nothing, its contents in the trash
  (with-operations-tree (dir)
    (sb-posix:symlink (namestring (here dir "b.txt")) (namestring (here dir "sub/b.txt")))
    (dolist (key '("C" "Y"))
      (let ((f (ops-session (here dir "sub/") (format nil "g f b . t x t RET ~a C-a C-k ~a C-j y RET" key (rimu-typed (namestring dir)))
                            :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "the very same file") "~a" key))
      (is string= "beta" (alexandria:read-file-into-string (here dir "b.txt")))
      (is eq :regular (tuatara.io:path-kind (namestring (here dir "b.txt")) :follow nil)))))

(define-test (rimu-operations archives-held-in)
  ;; an archive holding far more than it takes up stops before the drive is
  ;; full, said so; nothing is left of a folder made for one that fails; a file
  ;; in a folder also packed goes in once; xz is held to the memory there is,
  ;; and times are written in your zone
  (with-operations-tree (dir)
    (ensure-directories-exist (here dir "zeros/"))
    (with-open-file (out (here dir "zeros/zero") :direction :output :element-type '(unsigned-byte 8))
      (let ((block (make-array (* 1024 1024) :element-type '(unsigned-byte 8) :initial-element 0)))
        (loop repeat 24 do (write-sequence block out))))
    (uiop:run-program (list "tar" "-C" (namestring (here dir "zeros/")) "-czf" (namestring (here dir "zeros.tar.gz")) "zero"))
    (delete-file (here dir "zeros/zero"))
    (let* ((free (tuatara.io:free-bytes (namestring dir)))
           (tuatara.defaults::*archive-free-reserve* (- free (* 4 1024 1024)))
           (said (handler-case (progn (tuatara.defaults::extract-archive (namestring (here dir "zeros.tar.gz"))
                                                                         (namestring (here dir "out/")))
                                      nil)
                   (error (e) (princ-to-string e)))))
      (true (search "*archive-free-reserve*" (or said "")) "~a" said)
      (let ((size (ignore-errors (tuatara.io:stamp-size (tuatara.io:path-stamp (namestring (here dir "out/zero")))))))
        (true (or (null size) (< size (* 24 1024 1024))))))
    ;; a broken archive: the folder made for it taken away again
    (alexandria:write-string-into-file "not an archive at all" (here dir "broken.tar"))
    (false (ignore-errors (tuatara.defaults::extract-archive (namestring (here dir "broken.tar")) (namestring (here dir "never/")))))
    (false (tuatara.io:path-stamp (namestring (here dir "never/"))))
    ;; sub/ and sub/c.txt marked: c.txt packed once
    (tuatara.defaults::write-archive (list (namestring (here dir "sub/")) (namestring (here dir "sub/c.txt")))
                                     (namestring (here dir "once.tar")))
    (let ((listing (tuatara.defaults::archive-listing (namestring (here dir "once.tar")))))
      (is = 1 (count-matches "sub/c.txt" listing)))
    (let ((environment (tuatara.defaults::tar-environment)))
      (true (assoc "XZ_DEFAULTS" environment :test #'string=)))
    (is string= "a: b" (tuatara.defaults::tar-failure-words
                        (format nil "tar: Removing leading `../' from member names~%a: b~%tar: Exiting with failure status due to previous errors")))))

(define-test (rimu-operations keys-listed)
  ;; ? lists the keys a list takes away as doing nothing there, never as their commands; nor do the key hints
  (with-operations-tree (dir)
    (ops-session dir "?")
    (let ((help (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Help*"))))
      (true (search "rimu-copy" help))
      (false (search "undefined" help))
      ;; w, which a list takes away, shows as (none here) under every keymap, the modal ones' too
      (let ((lines (remove-if-not (lambda (line) (uiop:string-prefix-p "  w " line))
                                  (uiop:split-string help :separator '(#\Newline)))))
        (true lines)
        (true (every (lambda (line) (search "(none here)" line)) lines))))
    (let ((f (ops-session dir "g" :height 30)))
      (true (tuatara.frontend.fake:fake-frame-showing f "rimu-home"))
      ;; with the g keys a list's parent mode gives it: g r reads the list again
      (true (tuatara.frontend.fake:fake-frame-showing f "revert-buffer"))
      (true (tuatara.frontend.fake:fake-frame-showing f "rimu-trash"))
      (false (tuatara.frontend.fake:fake-frame-showing f "undefined")))))

(define-test (rimu-operations names-as-text)
  (with-operations-tree (dir)
    ;; C-x C-q: the names as text, a line each (sub, a.txt, b.txt, link), point on a.txt's;
    ;; a.txt and b.txt swapped by editing their lines, in two steps; a buffer on a.txt follows it
    (let ((visiting (tuatara.io:visit-file (namestring (here dir "a.txt")))))
      (multiple-value-bind (f editor)
          (ops-session dir "g f a . t x t RET C-x C-q i C-a C-k b . t x t C-g j i C-a C-k a . t x t C-g C-c C-c")
        (true (tuatara.frontend.fake:fake-frame-showing f "Rename: C-c C-c renames the lines changed"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Renamed 2"))
        (true (tuatara.defaults::rimu-list-p (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
      (is string= "beta" (alexandria:read-file-into-string (here dir "a.txt")))
      (is string= "alpha" (alexandria:read-file-into-string (here dir "b.txt")))
      (is equal (tuatara.io:expand-path (namestring (here dir "b.txt"))) (tuatara.buffer:buffer-file visiting))
      (tuatara.buffer:kill-buffer visiting))
    ;; refused, nothing renamed: a name twice, a name another file has, a line taken away
    (let ((f (ops-session dir "g f a . t x t RET C-x C-q i C-a C-k s u b C-g C-c C-c")))
      (true (tuatara.frontend.fake:fake-frame-showing f "Nothing renamed: sub is given twice")))
    (alexandria:write-string-into-file "z" (here dir "other.txt"))
    (let ((f (ops-session dir "g f a . t x t RET C-x C-q i C-a C-k C-k C-g C-c C-c" :width 200)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Nothing renamed: a line was added, taken away or joined (4 lines for 5 names)")))
    (true (probe-file (here dir "a.txt")))
    ;; a line taken away and another added: as many lines as names, but none renamed in a cascade
    (let ((f (ops-session dir "g f a . t x t RET C-x C-q i C-a C-k C-k M-> n e w C-g C-c C-c" :width 200)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Nothing renamed: a line was added, taken away or joined")))
    (true (probe-file (here dir "a.txt")))
    (true (probe-file (here dir "other.txt")))
    (false (probe-file (here dir "new")))
    ;; a name with a line break in it is left out, said in the header; the others pair as they are
    (alexandria:write-string-into-file "nl" (here dir (format nil "nl~%name")))
    (let ((f (ops-session dir "g f a . t x t RET C-x C-q i C-a C-k a 2 . t x t C-g C-c C-c" :width 200)))
      (true (tuatara.frontend.fake:fake-frame-showing f "1 name with a control or invisible character left out"))
      (true (tuatara.frontend.fake:fake-frame-showing f "Renamed 1")))
    (true (probe-file (here dir "a2.txt")))
    (true (probe-file (here dir (format nil "nl~%name"))))
    (true (probe-file (here dir "other.txt")))
    (rename-file (here dir "a2.txt") (here dir "a.txt"))
    (delete-file (here dir (format nil "nl~%name")))
    ;; C-c C-k: nothing renamed, back to the list
    (let ((f (ops-session dir "g f a . t x t RET C-x C-q i C-a C-k z z z C-g C-c C-k")))
      (true (tuatara.frontend.fake:fake-frame-showing f "No name changed")))
    (true (probe-file (here dir "a.txt")))
    ;; a name with a slash is refused: renaming never moves
    (let ((f (ops-session dir "g f a . t x t RET C-x C-q i C-a C-k s u b / a . t x t C-g C-c C-c" :width 160)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Nothing renamed: sub/a.txt is no name in this directory")))
    ;; a mark goes with its file renamed: a.txt (beta) marked for deletion, a and b swapped, x trashes it
    (ops-session dir "g f a . t x t RET d g f a . t x t RET C-x C-q i C-a C-k b . t x t C-g j i C-a C-k a . t x t C-g C-c C-c x y RET")
    (is string= "alpha" (alexandria:read-file-into-string (here dir "a.txt")))
    (false (probe-file (here dir "b.txt")))
    ;; the edits refused are left open; they go, so as not to stand in later tests' way
    (dolist (buffer (copy-list tuatara.buffer:*buffers*))
      (when (uiop:string-prefix-p "*Rename: " (tuatara.buffer:buffer-name buffer))
        (tuatara.buffer:kill-buffer buffer)))))

(define-test (rimu-operations names-as-text-checked-in-linear-time)
  ;; checking the names of a folder renamed as text costs the same for each
  ;; name, however many there are: four times the names, at most about four
  ;; times the time
  (with-test-directory (dir)
    (flet ((seconds-each (count)
             (let* ((names (loop for i below count collect (format nil "photo-~d.jpg" i)))
                    (finals (loop for i below count collect (format nil "renamed-~d.jpg" i)))
                    (changes (mapcar #'cons names finals))
                    (start (thread-cpu-time)))
               (loop repeat 3
                     do (false (tuatara.defaults::rimu-names-refusal (tuatara.io:native dir) names finals changes)))
               (/ (- (thread-cpu-time) start) internal-time-units-per-second count))))
      (seconds-each 500)
      (let ((small (seconds-each 2000)) (large (seconds-each 8000)))
        (true (< large (* 2.5 (max small 1/1000000))) "each name: ~,2e s of 2000, ~,2e s of 8000" small large)))
    ;; a name given twice, and one another file has, are still refused
    (alexandria:write-string-into-file "x" (merge-pathnames "kept.jpg" dir))
    (true (search "given twice" (tuatara.defaults::rimu-names-refusal (tuatara.io:native dir) '("a" "b") '("c" "c") '(("a" . "c") ("b" . "c")))))
    (true (search "there already" (tuatara.defaults::rimu-names-refusal (tuatara.io:native dir) '("a") '("kept.jpg") '(("a" . "kept.jpg")))))))

(define-test (rimu-operations new-files)
  (with-operations-tree (dir)
    ;; a: an empty file made there, the list on it
    (multiple-value-bind (f editor) (ops-session dir "a f r e s h . t x t RET" :width 300)
      (true (tuatara.frontend.fake:fake-frame-showing f "Touch in"))
      (true (tuatara.frontend.fake:fake-frame-showing f "Made"))
      (is = 0 (length (alexandria:read-file-into-string (here dir "fresh.txt"))))
      (let ((list (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
        (is string= "fresh.txt" (tuatara.io:listed-name (tuatara.defaults::rimu-entry-at list (tuatara.buffer:buffer-point list))))))
    ;; a name there already, a file or a directory, is touched: its times now, its text kept
    (flet ((age (name)
             (let ((path (here dir name)))
               (sb-posix:utimes path 1000000000 1000000000)
               path))
           (modified (path) (sb-posix:stat-mtime (sb-posix:lstat path))))
      (let* ((path (age "a.txt"))
             (text (alexandria:read-file-into-string path))
             (f (ops-session dir "a a . t x t RET" :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Touched a.txt"))
        (true (> (modified path) 1000000000))
        (is string= text (alexandria:read-file-into-string path)))
      (let ((path (age "sub")))
        (ops-session dir "a s u b RET" :width 300)
        (true (> (modified path) 1000000000)))
      ;; RET alone touches the entry on the line, or those marked
      (let ((path (age "a.txt")))
        (let ((f (ops-session dir "g f a . t x t RET a RET" :width 300)))
          (true (tuatara.frontend.fake:fake-frame-showing f "(RET: a.txt)")))
        (true (> (modified path) 1000000000)))
      (let ((one (age "a.txt")) (two (age "b.txt")))
        (let ((f (ops-session dir "g f a . t x t RET m g f b . t x t RET m a RET" :width 300)))
          (true (tuatara.frontend.fake:fake-frame-showing f "(RET: 2 files)"))
          (true (tuatara.frontend.fake:fake-frame-showing f "Touched 2 entries")))
        (true (> (modified one) 1000000000))
        (true (> (modified two) 1000000000))))
    ;; only a name: a / is refused
    (let ((f (ops-session dir "a s u b / x RET" :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f "sub/x is no name of a file in"))
      (false (probe-file (here dir "sub/x"))))
    ;; a plain name is one the directory can hold as it is: no /, no control character, not . nor ..
    (dolist (name '("notes.txt" ".hidden" "a b" "ü"))
      (true (tuatara.defaults::plain-file-name-p name) "~s" name))
    (dolist (name (list "" "." ".." "sub/x" "/" (format nil "a~cb" #\Tab) (format nil "a~c" (code-char 127))))
      (false (tuatara.defaults::plain-file-name-p name) "~s" name))
    ;; A: a formatted file, its header written and saved at once
    (let ((tuatara.defaults::*formatted-file-prompts* '(:title :keywords)))
      (ops-session dir "A M y SPC n o t e RET RET"))
    (let ((made (find-if (lambda (name) (search "--my-note" name)) (mapcar #'first (tuatara.io:directory-entries dir)))))
      (true made)
      (true (search "#+title: My note" (alexandria:read-file-into-string (here dir made)))))))

(define-test (rimu-operations tests-see-no-real-trash)
  ;; the tests' trashes are their own: neither the home one nor a drive's (a USB stick's)
  (with-operations-tree (dir)
    (is equal (list (cons (tuatara.io:home-trash) nil)) (tuatara.defaults::trash-directories))
    (true (search (namestring dir) (tuatara.io:home-trash))))
  ;; and the runner's own: a data home of the tests', no drive mounted
  (false (search "/.local/share/" (tuatara.io:home-trash)))
  (is equal (list (cons (tuatara.io:home-trash) nil)) (tuatara.defaults::trash-directories)))

(define-test (rimu-operations trash-list)
  (with-operations-tree (dir)
    ;; g t lists the trash; R puts the one on the line back where it was, after asking
    (ops-session dir "g f b . t x t RET D y")
    (let ((f (ops-session dir "g t" :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f "1 thing in the trash"))
      (true (tuatara.frontend.fake:fake-frame-showing f "b.txt")))
    ;; answered no: left in the trash
    (let ((f (ops-session dir "g t R n" :width 300)))
      (false (tuatara.frontend.fake:fake-frame-showing f "Put back:"))
      (false (probe-file (here dir "b.txt")))
      (true (probe-file (here dir "data/Trash/info/b.txt.trashinfo"))))
    (let ((f (ops-session dir "g t R" :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Put back the thing"))
      (false (probe-file (here dir "b.txt"))))
    (let ((f (ops-session dir "g t R y" :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Put back:"))
      (is string= "beta" (alexandria:read-file-into-string (here dir "b.txt")))
      (false (probe-file (here dir "data/Trash/info/b.txt.trashinfo"))))
    ;; taken: another name asked for
    (ops-session dir "g f b . t x t RET D y")
    (alexandria:write-string-into-file "new beta" (here dir "b.txt"))
    (let ((f (ops-session dir (format nil "g t R y C-a C-k ~a C-j" (rimu-typed (namestring (here dir "b-old.txt")))))))
      (true (tuatara.frontend.fake:fake-frame-showing f "is taken; put it back as:"))
      (is string= "beta" (alexandria:read-file-into-string (here dir "b-old.txt"))))
    ;; D for good, X empties, asked twice; each thing counted once, its .trashinfo going with it
    (ops-session dir "g f b - o l d . t x t RET D y g f s u b RET D y")
    (ops-session dir "g f a . t x t RET D y")
    (let ((f (ops-session dir "g t D y RET" :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Deleted for good 1 file"))
      (false (tuatara.frontend.fake:fake-frame-showing f "Deleted for good 2 files")))
    (let ((f (ops-session dir "g t" :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f "2 things in the trash")))
    ;; back from the list of the trash, the cursor is on an entry of the list
    (multiple-value-bind (f editor) (ops-session dir "g e g t q")
      (declare (ignore f))
      (true (tuatara.defaults::rimu-entry-at (tuatara.display:window-buffer (tuatara.command:editor-window editor))
                                             (session-point editor))))
    (is = 2 (length (uiop:directory-files (here dir "data/Trash/info/"))))
    (let ((f (ops-session dir "g t X y RET y RET" :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Deleted for good 2 files"))
      (false (tuatara.frontend.fake:fake-frame-showing f "Deleted for good 4 files")))
    (is eq nil (uiop:directory-files (here dir "data/Trash/info/")))
    (let ((f (ops-session dir "g t" :width 300)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Nothing in the trash")))))

(define-test (rimu-operations trash-drawn-from-what-was-read)
  (with-operations-tree (dir)
    (ops-session dir "g f b . t x t RET D y")
    ;; marks drawn from what was read: a thing put in the trash meanwhile shows only once it is read again
    (let ((f (ops-session dir (list "g t" (lambda () (tuatara.io:trash-path (namestring (here dir "a.txt")))) "m u TAB TAB")
                          :width 200)))
      (true (tuatara.frontend.fake:fake-frame-showing f "1 thing in the trash"))
      (false (tuatara.frontend.fake:fake-frame-showing f "2 things in the trash")))
    ;; rimu having changed the trash, the list shown reads it again by itself
    (let ((f (ops-session dir (list "g t" (lambda ()
                                            (tuatara.io:trash-path (namestring (here dir "sub")))
                                            (tuatara.defaults::trash-changed))
                                    :tick)
                          :width 200)))
      (true (tuatara.frontend.fake:fake-frame-showing f "3 things in the trash")))
    ;; deleted for good in the background, the list read again after
    (let ((f (ops-session dir "g t D y RET" :width 200)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Deleted for good"))
      (true (tuatara.frontend.fake:fake-frame-showing f "2 things in the trash")))))

(define-test (rimu-operations restored-into-odd-names)
  ;; put back as a name in a directory not there, whose name holds [ ] and *: the
  ;; directory made again, characters, not patterns
  (with-operations-tree (dir)
    (let ((target (concatenate 'string (tuatara.io:native dir) "gone [1]*/b.txt")))
      (ops-session dir "g f b . t x t RET D y")
      (alexandria:write-string-into-file "new beta" (here dir "b.txt"))
      (let ((f (ops-session dir (format nil "g t R y C-a C-k ~a C-j" (rimu-typed target)) :width 300)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Put back:")))
      (is string= "beta" (file-text (sb-ext:parse-native-namestring target))))))

(define-test (rimu-operations trash-refresh)
  ;; r and g r list the trash again, what went there since shown; nothing put back
  (with-operations-tree (dir)
    (ops-session dir "g f b . t x t RET D y")
    (dolist (keys '("r" "g r"))
      (let ((name (if (string= keys "r") "a.txt" "sub")))
        (let ((f (ops-session dir (list "g t" (lambda () (tuatara.io:trash-path (namestring (here dir name)))) keys))))
          (true (tuatara.frontend.fake:fake-frame-showing f (if (string= keys "r") "2 things in the trash" "3 things in the trash")))
          (false (tuatara.frontend.fake:fake-frame-showing f "Put back:"))
          (false (probe-file (here dir "b.txt"))))))))

(define-test (rimu-operations typed-line-selected)
  ;; as Vertico does: in C-x d the line typed is selected while it names a directory, RET takes it;
  ;; C-n goes to the first directory, RET into it, RET again takes it
  (with-operations-tree (dir)
    (rimu-forget)
    (let ((tuatara-user:*rimu-preview* nil))
      (multiple-value-bind (f editor) (run-keys (format nil "C-x d C-a C-k ~a RET" (rimu-typed (namestring dir))) :width 300 :height 20)
        (true (tuatara.frontend.fake:fake-frame-showing f "*/"))
        (is string= (namestring dir)
            (tuatara.buffer:buffer-local (tuatara.display:window-buffer (tuatara.command:editor-window editor))
                                         'tuatara.defaults::rimu-directory)))
      (rimu-forget)
      (multiple-value-bind (f editor) (run-keys (format nil "C-x d C-a C-k ~a C-n RET RET" (rimu-typed (namestring dir))) :width 300 :height 20)
        (declare (ignore f))
        (is string= (namestring (here dir "sub/"))
            (tuatara.buffer:buffer-local (tuatara.display:window-buffer (tuatara.command:editor-window editor))
                                         'tuatara.defaults::rimu-directory)))
      ;; + with a name begun that a directory begins with (s, sub/ there): sub/ selected; C-p RET makes s
      (ops-session dir (format nil "+ C-a C-k ~a s C-p RET" (rimu-typed (namestring dir))) :width 300)
      (true (probe-file (here dir "s/")))
      (false (probe-file (here dir "sub/s/"))))
    ;; C-x C-f on a directory, the line typed selected: RET lists it, as Emacs's dired does
    (rimu-forget)
    (let ((tuatara-user:*rimu-preview* nil))
      (multiple-value-bind (f editor) (run-keys (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (namestring (here dir "sub/"))))
                                                :width 300 :height 20)
        (declare (ignore f))
        (is string= (namestring (here dir "sub/"))
            (tuatara.buffer:buffer-local (tuatara.display:window-buffer (tuatara.command:editor-window editor))
                                         'tuatara.defaults::rimu-directory))))
    ;; C-j with nothing typed: this directory, the one the editor is in
    (rimu-forget)
    (let ((tuatara-user:*rimu-preview* nil)
          (*default-pathname-defaults* (pathname (namestring dir))))
      (multiple-value-bind (f editor) (run-keys "C-x d C-a C-k C-j" :width 300 :height 20)
        (declare (ignore f))
        (true (tuatara.buffer:buffer-local (tuatara.display:window-buffer (tuatara.command:editor-window editor))
                                           'tuatara.defaults::rimu-directory))))
    ;; M the same: su, sub/ selected, RET into it, RET moves there
    (ops-session dir (format nil "g f b . t x t RET M C-a C-k ~a s u RET RET" (rimu-typed (namestring dir))) :width 300)
    (is string= "beta" (alexandria:read-file-into-string (here dir "sub/b.txt")))))
(define-test (rimu-operations last-used-first)
  ;; what was chosen lately in a directory, or something in it, comes first there: RET goes that way
  (with-operations-tree (dir)
    (ensure-directories-exist (here dir "zeta/"))
    (tuatara.defaults::note-chosen-path (namestring (here dir "zeta/notes.txt")))
    (let ((tuatara-user:*last-used-first* t))
      (rimu-forget)
      (multiple-value-bind (f editor) (run-keys (format nil "C-x d C-a C-k ~a C-n RET RET" (rimu-typed (namestring dir))) :width 300 :height 20)
        (declare (ignore f))
        (is string= (namestring (here dir "zeta/"))
            (tuatara.buffer:buffer-local (tuatara.display:window-buffer (tuatara.command:editor-window editor))
                                         'tuatara.defaults::rimu-directory)))
      ;; and what is chosen is remembered, the latest first
      (is string= (namestring (here dir "zeta/")) (first (tuatara.defaults::chosen-paths))))
    ;; with *last-used-first* NIL: the plain order, sub/ before zeta/
    (tuatara.defaults::note-chosen-path (namestring (here dir "zeta/notes.txt")))
    (let ((tuatara-user:*last-used-first* nil))
      (rimu-forget)
      (multiple-value-bind (f editor) (run-keys (format nil "C-x d C-a C-k ~a C-n RET RET" (rimu-typed (namestring dir))) :width 300 :height 20)
        (declare (ignore f))
        (is string= (namestring (here dir "sub/"))
            (tuatara.buffer:buffer-local (tuatara.display:window-buffer (tuatara.command:editor-window editor))
                                         'tuatara.defaults::rimu-directory))))))

(define-test (rimu-operations links-never-followed)
  (with-test-directory (dir)
    (let ((outside (merge-pathnames "outside/" dir))
          (tree (merge-pathnames "tree/" dir)))
      (ensure-directories-exist outside)
      (alexandria:write-string-into-file "keep" (merge-pathnames "precious.txt" outside))
      (flet ((outside-whole () (and (probe-file (merge-pathnames "precious.txt" outside))
                                    (equal '("precious.txt") (mapcar #'first (tuatara.io:directory-entries (namestring outside))))))
             (fill-tree ()
               (ensure-directories-exist (merge-pathnames "sub/deeper/" tree))
               (dotimes (i 20)
                 (alexandria:write-string-into-file "x" (merge-pathnames (format nil "sub/f~d" i) tree) :if-exists :supersede)
                 (alexandria:write-string-into-file "y" (merge-pathnames (format nil "sub/deeper/g~d" i) tree) :if-exists :supersede))))
        ;; a directory swapped for a link to another between being examined and opened: not opened through it
        (sb-posix:symlink (namestring outside) (namestring (merge-pathnames "swapped" dir)))
        (tuatara.io::call-with-parent (namestring (merge-pathnames "swapped" dir))
          (lambda (dirfd name)
            (fail (tuatara.io::open-directory-entry dirfd name "swapped") 'tuatara.io:file-system-error)
            (fail (tuatara.io::copy-file-at dirfd name dirfd (tuatara.text:string-octets "copied") "swapped" "copied")
                'tuatara.io:file-system-error)))
        (false (probe-file (merge-pathnames "copied" dir)))
        ;; deleted and copied while another program swaps a directory in it for a link to OUTSIDE, again and again
        (let* ((sub (namestring (merge-pathnames "tree/sub" dir)))
               (aside (namestring (merge-pathnames "tree/aside" dir)))
               (stop nil)
               ;; the tree filled again only while SUB is a directory, never written through the link
               (lock (sb-thread:make-mutex :name "swap"))
               (swapper (sb-thread:make-thread
                         (lambda ()
                           (loop until stop
                                 do (sb-thread:with-mutex (lock)
                                      (ignore-errors
                                       (when (probe-file sub)
                                         (sb-posix:rename sub aside)
                                         (sb-posix:symlink (namestring outside) sub)
                                         (sb-posix:unlink sub)
                                         (sb-posix:rename aside sub))))))
                         :name "swapper")))
          (unwind-protect
               (dotimes (round 60)
                 (sb-thread:with-mutex (lock) (ignore-errors (fill-tree)))
                 (ignore-errors (tuatara.io:copy-path-tree (namestring tree) (namestring (merge-pathnames "copy" dir))))
                 (ignore-errors (tuatara.io:delete-tree (namestring (merge-pathnames "copy" dir))))
                 (ignore-errors (tuatara.io:delete-tree (namestring tree))))
            (setf stop t)
            (sb-thread:join-thread swapper)))
        (true (outside-whole) "what the links led to is untouched")))))

(define-test (rimu-operations deep-trees-refused)
  ;; a tree nested deeper than *DEEPEST-FOLDER-TREE*: refused whole, never left half-copied, nothing hidden beside
  (with-test-directory (dir)
    (let ((tuatara.io::*deepest-folder-tree* 4)
          (deep (merge-pathnames "deep/a/b/c/d/e/f/" dir))
          (shallow (merge-pathnames "shallow/a/b/" dir)))
      (ensure-directories-exist deep)
      (ensure-directories-exist shallow)
      (alexandria:write-string-into-file "x" (merge-pathnames "file" deep))
      (fail (tuatara.io:copy-path-tree (namestring (merge-pathnames "deep" dir)) (namestring (merge-pathnames "copy" dir)))
          'tuatara.io:file-system-error)
      (is equal '("deep" "shallow") (sort (mapcar #'first (tuatara.io:directory-entries (namestring dir))) #'string<))
      (fail (tuatara.io:delete-tree (namestring (merge-pathnames "deep" dir))) 'tuatara.io:file-system-error)
      (true (probe-file (merge-pathnames "file" deep)))
      ;; within the depth, both are done
      (true (tuatara.io:copy-path-tree (namestring (merge-pathnames "shallow" dir)) (namestring (merge-pathnames "copy" dir))))
      (true (tuatara.io:delete-tree (namestring (merge-pathnames "copy" dir))))
      (let ((tuatara.io::*deepest-folder-tree* 20))
        (true (tuatara.io:delete-tree (namestring (merge-pathnames "deep" dir))))))))

(define-test (rimu-operations trash-as-a-list)
  ;; the trash as the buffer list is: a section for your trash, marks, two put back at once,
  ;; one marked for deletion deleted for good with x
  (with-operations-tree (dir)
    (ops-session dir "g f a . t x t RET D y g f b . t x t RET D y g f s u b RET D y")
    (let ((f (ops-session dir "g t" :width 200)))
      (true (tuatara.frontend.fake:fake-frame-showing f "▾ Your trash (3)"))
      (true (tuatara.frontend.fake:fake-frame-showing f "sub/"))
      (true (tuatara.frontend.fake:fake-frame-showing f "3 things in the trash")))
    ;; sub, then b, marked (the last to go first), put back after one question
    (let ((f (ops-session dir "g t m m R y" :width 200)))
      (true (tuatara.frontend.fake:fake-frame-showing f (format nil "Put back the 2 things to ~a?" (tuatara.command::abbreviated-path (namestring dir)))))
      (true (probe-file (here dir "b.txt")))
      (true (probe-file (here dir "sub/c.txt")))
      (true (tuatara.frontend.fake:fake-frame-showing f "1 thing in the trash")))
    ;; a.txt marked for deletion and deleted for good
    (let ((f (ops-session dir "g t d x y RET" :width 200)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Delete the thing a.txt for good?"))
      (false (probe-file (here dir "a.txt")))
      (true (tuatara.frontend.fake:fake-frame-showing f "Nothing in the trash")))))

(define-test (rimu-operations trash-filtered-questions)
  ;; in the trash list narrowed by a filter, R and D on the marked say how many the filter hides;
  ;; answered no, nothing is put back or deleted
  (with-operations-tree (dir)
    (ops-session dir "g f a . t x t RET m m D y")
    (unwind-protect
         (loop for (keys question) in '(("R n" "Put back the 2 things (1 hidden by the filter) to ")
                                        ("D n" "Delete the 2 things (1 hidden by the filter) for good?"))
               do (let ((f (ops-session dir (format nil "g t C-x / r M-< C-n C-n C-n m m C-x / s a . t x t RET ~a" keys)
                                        :width (width-for dir 200))))
                    (true (tuatara.frontend.fake:fake-frame-showing f question))))
      (let ((list (tuatara.buffer:find-buffer "*Trash*")))
        (when list (tuatara.buffer:kill-buffer list))))
    (false (probe-file (here dir "a.txt")))
    (false (probe-file (here dir "b.txt")))))

(defun filter-line (editor)
  "The line saying how the current list is narrowed by search filters, or NIL."
  (find-if (lambda (line) (search "iltered by" line)) (rimu-lines editor)))

(define-test (rimu-operations narrowed-by-filters)
  ;; C-x / s narrows a directory's list to the entries whose lines match, as in every list;
  ;; filters stack, and C-x / r takes them away
  (with-operations-tree (dir)
    (multiple-value-bind (f editor) (ops-session dir "C-x / s t x t RET")
      (declare (ignore f))
      (let ((lines (rimu-lines editor)))
        (true (find-if (lambda (l) (search "a.txt" l)) lines))
        (true (find-if (lambda (l) (search "b.txt" l)) lines))
        (false (find-if (lambda (l) (search "sub" l)) lines))
        (false (find-if (lambda (l) (search "link" l)) lines))
        (true (search "Filtered by txt: 2 of 4 entries" (filter-line editor)))
        ;; the directory's name stays above the entries
        (true (find-if (lambda (l) (search (string-right-trim "/" (namestring dir)) l)) lines))))
    (multiple-value-bind (f editor) (ops-session dir "C-x / s t x t RET C-x / s a RET")
      (declare (ignore f))
      (true (search "Filtered by txt, a: 1 of 4 entries" (filter-line editor)))
      (false (find-if (lambda (l) (search "b.txt" l)) (rimu-lines editor))))
    (multiple-value-bind (f editor) (ops-session dir "C-x / s t x t RET C-x / s a RET C-x / r")
      (true (tuatara.frontend.fake:fake-frame-showing f "2 filters taken away"))
      (false (filter-line editor))
      (true (find-if (lambda (l) (search "link" l)) (rimu-lines editor))))
    ;; each line still acts on the entry it shows
    (multiple-value-bind (f editor) (ops-session dir "C-x / s b RET g f b . t x t RET")
      (declare (ignore f))
      (let ((list (session-buffer editor)))
        (is string= "b.txt" (tuatara.io:listed-name (tuatara.defaults::rimu-entry-at list (tuatara.buffer:buffer-point list))))
        (is equal (list (tuatara.io:native (here dir "b.txt"))) (tuatara.defaults::rimu-chosen-paths list))))
    ;; read again, or laid out anew, the list is narrowed again
    (dolist (keys '("g r" "I" "s" "H"))
      (multiple-value-bind (f editor) (ops-session dir (format nil "C-x / s t x t RET ~a" keys))
        (declare (ignore f))
        (true (search "Filtered by txt: " (filter-line editor)) "~a" keys)
        (false (find-if (lambda (l) (search "sub" l)) (rimu-lines editor)))))))

(define-test (rimu-operations filters-by-column)
  ;; with the details shown, C-x / f asks for a column, the name among them, and tries that cell
  ;; alone: ^b finds b.txt by its name, which the whole line, starting with its mark and
  ;; details, never begins with; with the names alone there is no column to ask for
  (with-operations-tree (dir)
    (multiple-value-bind (f editor) (ops-session dir "I C-x / f n a m e RET ^ b RET")
      (let ((lines (rimu-lines editor)) (list (session-buffer editor)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Filter by the column:"))
        (true (find-if (lambda (l) (search "b.txt" l)) lines))
        (false (find-if (lambda (l) (search "a.txt" l)) lines))
        (false (find-if (lambda (l) (search "sub" l)) lines))
        (is equal '("inode" "permissions" "links" "owner" "group" "size" "time" "name")
            (mapcar #'first (tuatara.defaults::list-table-columns list)))))
    (multiple-value-bind (f editor) (ops-session dir "")
      (declare (ignore f))
      (false (tuatara.defaults::list-table-columns (session-buffer editor))))))

(define-test (rimu-operations marks-under-filters)
  ;; marks stay on the entries a filter hides, and the operations take them, their questions
  ;; saying how many the filter hides; a file marked in another directory is not one of them
  (with-operations-tree (dir)
    (multiple-value-bind (f editor) (ops-session dir "g f a . t x t RET m m C-x / s b RET")
      (declare (ignore f))
      (let ((list (session-buffer editor)))
        (false (find-if (lambda (l) (search "a.txt" l)) (rimu-lines editor)))
        (true (find-if (lambda (l) (and (uiop:string-prefix-p "> " l) (search "b.txt" l))) (rimu-lines editor)))
        (is equal (list (tuatara.io:native (here dir "a.txt")) (tuatara.io:native (here dir "b.txt")))
            (tuatara.defaults::rimu-chosen-paths list))))
    (let ((f (ops-session dir "g f a . t x t RET m m C-x / s b RET D n" :width 200)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Move 2 files (1 hidden by the filter) to the trash?")))
    (let ((f (ops-session dir "g f a . t x t RET m m C-x / s b RET C" :width 200)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Copy 2 files (1 hidden by the filter) to:")))
    (true (probe-file (here dir "a.txt")))
    ;; marked while narrowed, the mark is still there once the filter is taken away; t turns only
    ;; the entries shown about
    (multiple-value-bind (f editor) (ops-session dir "C-x / s b RET g f b . t x t RET m C-x / r")
      (declare (ignore f))
      (true (find-if (lambda (l) (and (uiop:string-prefix-p "> " l) (search "b.txt" l))) (rimu-lines editor))))
    (multiple-value-bind (f editor) (ops-session dir "C-x / s t x t RET t C-x / r")
      (declare (ignore f))
      (let ((lines (rimu-lines editor)))
        (true (find-if (lambda (l) (and (uiop:string-prefix-p "> " l) (search "a.txt" l))) lines))
        (true (find-if (lambda (l) (and (uiop:string-prefix-p "  " l) (search "sub" l))) lines))))
    ;; one marked in another directory is not hidden by this list's filter
    (let ((f (ops-session dir "g f a . t x t RET m g f s u b RET RET g f c . t x t RET m C-x / s z z RET C" :width 400)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Copy 2 files (1 hidden by the filter) in 2 directories to:")))))

(define-test (rimu-operations filters-and-folders)
  ;; going into a folder lists it whole, the filters dropped; going up too
  (with-operations-tree (dir)
    (multiple-value-bind (f editor) (ops-session dir "C-x / s s u b RET g f s u b RET RET")
      (declare (ignore f))
      (let ((list (session-buffer editor)))
        (false (tuatara:search-filters list))
        (false (filter-line editor))
        (true (find-if (lambda (l) (search "c.txt" l)) (rimu-lines editor)))))
    (multiple-value-bind (f editor) (ops-session dir "g f s u b RET RET C-x / s z z RET h")
      (declare (ignore f))
      (false (filter-line editor))
      (true (find-if (lambda (l) (search "a.txt" l)) (rimu-lines editor))))
    ;; a file made on disk while the list is narrowed shows when it matches, the filter kept
    (multiple-value-bind (f editor seen)
        (disk-change-session dir (lambda ()
                                   (alexandria:write-string-into-file "x" (here dir "made.dat"))
                                   (alexandria:write-string-into-file "x" (here dir "made.txt")))
                             "made.txt" :before "C-x / s t x t RET")
      (declare (ignore f))
      (true seen)
      (let ((text (tuatara.buffer:buffer-string (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
        (false (search "made.dat" text))
        (true (search "Filtered by txt: 3 of" text))))))

(define-test (rimu-operations filters-beside-the-preview)
  ;; the preview shows the entry on the line of the narrowed list
  (with-operations-tree (dir)
    (rimu-forget)
    (let ((tuatara-user:*rimu-preview* t) (tuatara-user:*rimu-preview-delay* 0))
      (let ((f (run-keys (list (format nil "C-x d C-a C-k ~a C-j C-x / s b . t x t RET" (rimu-typed (namestring dir)))
                               :tick :tick)
                         :width 120 :height 20)))
        (true (tuatara.frontend.fake:fake-frame-showing f "beta"))
        (false (tuatara.frontend.fake:fake-frame-showing f "alpha"))))
    (rimu-forget)))

(define-test (rimu-operations filters-in-a-safe-list)
  ;; a folder opened safely is narrowed as any list is, and still changes nothing
  (with-operations-tree (dir)
    (rimu-forget)
    (multiple-value-bind (f editor)
        (run-keys (format nil "M-x f i n d - f i l e - s a f e l y RET C-a C-k ~a C-j C-x / s a . t x t RET D"
                          (rimu-typed (namestring dir)))
                  :width 120 :height 20)
      (true (tuatara.io:safe-visit-p (session-buffer editor)))
      (true (search "Filtered by a.txt: 1 of 4 entries" (filter-line editor)))
      (false (find-if (lambda (l) (search "b.txt" l)) (rimu-lines editor)))
      (true (tuatara.frontend.fake:fake-frame-showing f "changes files, and"))
      (true (probe-file (here dir "a.txt"))))
    (rimu-forget)))

(define-test (rimu-operations masters-marked-apart)
  ;; C-c f m marks a master (M) and goes on; the masters marked anywhere and the files marked here
  ;; come apart, a master never among those compared with it
  (with-operations-tree (dir)
    (multiple-value-bind (f editor) (ops-session dir "g f a . t x t RET C-c f m g f b . t x t RET m")
      (declare (ignore f))
      (let ((buffer (session-buffer editor)))
        (true (find-if (lambda (l) (and (uiop:string-prefix-p "M " l) (search "a.txt" l))) (rimu-lines editor)))
        (multiple-value-bind (masters chosen) (tuatara.defaults::rimu-paths-marked-apart #\M :files-only t :buffer buffer)
          (is equal (list (tuatara.io:native (here dir "a.txt"))) masters)
          (is equal (list (tuatara.io:native (here dir "b.txt"))) chosen)))))
  (true (tuatara.defaults::fraction-p 1))
  (true (tuatara.defaults::fraction-p 0.8))
  (false (tuatara.defaults::fraction-p 0))
  (false (tuatara.defaults::fraction-p 1.5))
  (false (tuatara.defaults::fraction-p "0.5")))

(define-test (rimu-operations formatted-file-asked-alike)
  ;; A in a list asks what new-file-formatted asks but the subdirectory, and makes the same text
  (with-operations-tree (dir)
    (let ((tuatara-user:*formatted-file-prompts* '(:title :subdirectory :file-type))
          (tuatara-user:*formatted-file-type* :org)
          (tuatara-user:*formatted-file-directory* (namestring dir)))
      (let ((f (ops-session dir "A K e a SPC w a l k RET RET" :width 200)))
        (declare (ignore f))
        (let ((made (find-if (lambda (p) (search "--kea-walk" (file-namestring p))) (uiop:directory-files dir))))
          (true made)
          (when made
            (let ((text (alexandria:read-file-into-string made)))
              (true (uiop:string-prefix-p "#+title: Kea walk" text))
              (is string= text
                  (tuatara.defaults::formatted-file-start-text
                   (tuatara.defaults::formatted-file-type-for ".org") "Kea walk" '()
                   (subseq (file-namestring made) 0 15)
                   (tuatara.defaults::identifier-time (subseq (file-namestring made) 0 15))
                   ""))))))))
  (true (search "SKIP" (documentation 'tuatara.defaults::ask-formatted-file-answers 'function))))
