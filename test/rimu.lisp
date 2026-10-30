;;;; test/rimu.lisp --- Rimu: directories listed, moved through, previewed.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test rimu
  :description "Rimu lists a directory as Emacs's dired does, never opening what is in it; moves through directories; shows the details, the names starting with a dot and another order on request; previews the entry on the line beside the list; takes columns of other information.")

(defun rimu-raw-name ()
  "The octets of a name that is not UTF-8: raw, the octets FF and FE, name."
  (coerce #(114 97 119 255 254 110 97 109 101) '(vector (unsigned-byte 8))))

(defun rimu-raw-string ()
  "RIMU-RAW-NAME as a listing decodes it: its two octets as their raw bytes' characters."
  (concatenate 'string "raw" (string (code-char #xDCFF)) (string (code-char #xDCFE)) "name"))

(defun rimu-at-raw-name (directory function)
  "Call FUNCTION with a descriptor of DIRECTORY and RIMU-RAW-NAME as octets ending in NUL."
  (let ((dirfd (sb-posix:open (namestring directory) sb-posix:o-rdonly))
        (name (make-array 10 :element-type '(unsigned-byte 8) :initial-element 0)))
    (replace name (rimu-raw-name))
    (unwind-protect (sb-sys:with-pinned-objects (name) (funcall function dirfd (sb-sys:vector-sap name)))
      (sb-posix:close dirfd))))

(defun rimu-fill-tree (directory)
  "Fill DIRECTORY with what rimu lists in the tests: the directories sub (with
deeper in it) and .hidden-dir; the files text.txt, data.csv, .dotfile, bin.dat
(binary, set-user-id) and one whose name is not UTF-8; links to text.txt, to
sub and to nowhere; a named pipe."
  (ensure-directories-exist (merge-pathnames "sub/deeper/" directory))
  (ensure-directories-exist (merge-pathnames ".hidden-dir/" directory))
  (put-file (merge-pathnames "text.txt" directory) (format nil "hello~%"))
  (put-file (merge-pathnames "data.csv" directory) (format nil "a,b~%1,2~%"))
  (put-file (merge-pathnames ".dotfile" directory) "dot")
  (alexandria:write-byte-vector-into-file (coerce #(0 1 2 98 105 110) '(vector (unsigned-byte 8)))
                                          (merge-pathnames "bin.dat" directory))
  (sb-posix:chmod (namestring (merge-pathnames "bin.dat" directory)) #o4755)
  (sb-posix:symlink "text.txt" (namestring (merge-pathnames "link-to-text" directory)))
  (sb-posix:symlink "sub" (namestring (merge-pathnames "link-to-dir" directory)))
  (sb-posix:symlink "nowhere" (namestring (merge-pathnames "broken-link" directory)))
  (sb-posix:mkfifo (namestring (merge-pathnames "pipe" directory)) #o644)
  (rimu-at-raw-name directory (lambda (dirfd name)
                                (let ((fd (sb-alien:alien-funcall
                                           (sb-alien:extern-alien "openat" (function sb-alien:int sb-alien:int sb-sys:system-area-pointer
                                                                                     sb-alien:int sb-alien:int))
                                           dirfd name (logior sb-posix:o-wronly sb-posix:o-creat) #o644)))
                                  (when (>= fd 0) (sb-posix:close fd))))))

(defmacro with-rimu-tree ((directory) &body body)
  "Run BODY with DIRECTORY bound to a new directory (WITH-TEST-DIRECTORY) that
RIMU-FILL-TREE filled."
  `(with-test-directory (,directory)
     (rimu-fill-tree ,directory)
     (unwind-protect (progn ,@body)
       ;; a name that is not UTF-8 is taken away by its octets, which the removal of the tree cannot
       (rimu-at-raw-name ,directory (lambda (dirfd name)
                                      (sb-alien:alien-funcall
                                       (sb-alien:extern-alien "unlinkat" (function sb-alien:int sb-alien:int sb-sys:system-area-pointer sb-alien:int))
                                       dirfd name 0))))))

(defun rimu-typed (text)
  "TEXT as the keys that type it."
  (format nil "~{~a~^ ~}" (map 'list (lambda (c) (if (char= c #\Space) "SPC" (string c))) text)))

(defun rimu-forget ()
  "Kill the rimu lists and previews earlier sessions left."
  (dolist (b (copy-list tuatara.buffer:*buffers*))
    (when (or (tuatara.defaults::rimu-list-p b) (search "(preview" (tuatara.buffer:buffer-name b)))
      (tuatara.buffer:kill-buffer b))))

(defun rimu-session (directory keys &key (width 100) (height 20) preview (file nil) cell-pixels details)
  "Run an editor session that lists DIRECTORY with C-x d, then takes KEYS (a
string, or a list as QUEUE-TEST-KEYS takes), with no rimu list left from
earlier sessions, the settings as they are by default but the preview, on only with PREVIEW (and then shown at once), and the
columns of sizes and dates, shown only with DETAILS; in a
session on FILE, with KEYS only; CELL-PIXELS has the frontend show pictures
(RUN-KEYS). Returns the frontend and the editor."
  (rimu-forget)
  (let ((tuatara-user:*rimu-preview* preview)
        (tuatara-user:*rimu-preview-delay* 0)
        (tuatara-user:*rimu-details* details)
        (tuatara-user:*rimu-dotfiles* nil)
        (tuatara-user:*rimu-sort* :name))
    (run-keys (cond (file keys)
                    ((listp keys) (cons (format nil "C-x d C-a C-k ~a C-j" (rimu-typed (namestring directory))) keys))
                    (t (format nil "C-x d C-a C-k ~a C-j~@[ ~a~]" (rimu-typed (namestring directory))
                               (and (plusp (length keys)) keys))))
              :width width :height height :file file :cell-pixels cell-pixels)))

(defun rimu-lines (editor)
  "The lines of the editor's current buffer."
  (uiop:split-string (session-text editor) :separator '(#\Newline)))

(defun rimu-list-name (directory)
  "The name of the rimu list of DIRECTORY, a pathname."
  (format nil "~a/" (car (last (pathname-directory directory)))))

(defun rimu-faces-on (frontend name)
  "The faces the last frame of FRONTEND painted over the first cell of NAME,
shown at the end of a row after a space."
  (loop for row from 0 below (length (tuatara.frontend.fake:fake-screen frontend))
        for line = (tuatara.frontend.fake:fake-screen-line frontend row)
        for column = (let ((at (- (length line) (length name))))
                       (and (> at 0) (string= name line :start2 at) (char= (char line (1- at)) #\Space) at))
        when column
          return (loop for (start end face) in (aref (tuatara.frontend.fake:fake-spans frontend) row)
                       when (and (<= start column) (< column end))
                         append (if (listp face) face (list face)))))

(define-test (rimu listing)
  (with-rimu-tree (dir)
    (multiple-value-bind (f editor) (rimu-session dir "")
      (let ((header (format nil "  ~a:" (string-right-trim "/" (namestring dir)))))
        ;; the directory's name, then directories and links to them, then the rest, by
        ;; name octet by octet; no names starting with a dot
        (is equal (list header "  link-to-dir" "  sub" "  bin.dat" "  broken-link" "  data.csv" "  link-to-text"
                        "  pipe" (concatenate 'string "  " (rimu-raw-string)) "  text.txt" "")
            (rimu-lines editor))
        ;; named after the directory, point on the first name; the mode line says the order
        (is string= (rimu-list-name dir) (current-name editor))
        (is = (+ (length header) 1 2) (session-point editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "Rimu  by name"))
        ;; a name that is not UTF-8 shows its octets
        (true (tuatara.frontend.fake:fake-frame-showing f "  raw\\xFF\\xFEname"))
        ;; the colours of dired: directories, links, links leading nowhere, pipes
        (true (member :heading (loop for (start end face) in (aref (tuatara.frontend.fake:fake-spans f) 0)
                                         when (= start 2) collect face)))
        (true (member :list-directory (rimu-faces-on f "sub")))
        (true (member :rimu-symlink (rimu-faces-on f "link-to-dir")))
        (true (member :rimu-broken-symlink (rimu-faces-on f "broken-link")))
        (true (member :rimu-special (rimu-faces-on f "pipe")))
        (false (intersection '(:list-directory :rimu-symlink :rimu-special :list-dim) (rimu-faces-on f "text.txt")))))
    ;; a compiled file dimmed
    (put-file (merge-pathnames "made.fasl" dir) "x")
    (multiple-value-bind (f editor) (rimu-session dir "")
      (declare (ignore editor))
      (true (member :list-dim (rimu-faces-on f "made.fasl"))))))

(define-test (rimu details)
  (with-rimu-tree (dir)
    (multiple-value-bind (f editor) (rimu-session dir "I")
      (let* ((lines (rimu-lines editor))
             (bin (find " bin.dat" lines :test (lambda (end line) (uiop:string-suffix-p line end))))
             (pipe (find " pipe" lines :test (lambda (end line) (uiop:string-suffix-p line end)))))
        (true (tuatara.frontend.fake:fake-frame-showing f "Details shown"))
        ;; the octets used and those free under the directory's name
        (true (cl-ppcre:scan "^  total used in directory [0-9.]+[KMG]? available [0-9.]+[KMGTP]?$" (second lines)))
        ;; the inode number as ls -i writes it, then permissions, links, owner, group, size, time and the
        ;; name, as ls -l writes them
        (true (cl-ppcre:scan (format nil "^ +~d -rwsr-xr-x[.+]? 1 \\S+ \\S+ +6 [A-Z][a-z][a-z] [ 1-3][0-9] [0-2][0-9]:[0-5][0-9] bin.dat$"
                                     (sb-posix:stat-ino (sb-posix:lstat (namestring (merge-pathnames "bin.dat" dir)))))
                             bin))
        (true (cl-ppcre:scan "^ +[0-9]+ prw-r--r--[.+]? 1 " pipe))
        ;; the inode numbers line up on their right
        (is = (search " -rws" bin) (search " prw-" pipe))
        ;; where a link leads
        (true (find "link-to-text -> text.txt" lines :test (lambda (end line) (uiop:string-suffix-p line end))))
        (true (find "broken-link -> nowhere" lines :test (lambda (end line) (uiop:string-suffix-p line end))))
        ;; the names line up
        (is = (search "bin.dat" bin) (search "pipe" pipe))
        ;; the set-user letter in its colour
        (let ((row (loop for row from 0 below (length (tuatara.frontend.fake:fake-screen f))
                         when (uiop:string-suffix-p (tuatara.frontend.fake:fake-screen-line f row) " bin.dat") return row)))
          (let ((letter (+ 3 (search "-rws" bin))))
            (true (find :rimu-set-id (aref (tuatara.frontend.fake:fake-spans f) row)
                        :test (lambda (face span) (and (<= (first span) letter) (< letter (second span)) (eq face (third span))))))))))
    ;; columns left out by the setting: no inode numbers, no groups
    (let ((tuatara-user:*rimu-hidden-columns* '(:inode :group)))
      (multiple-value-bind (f editor) (rimu-session dir "I")
        (declare (ignore f))
        (let ((bin (find " bin.dat" (rimu-lines editor) :test (lambda (end line) (uiop:string-suffix-p line end)))))
          (true (cl-ppcre:scan "^  -rwsr-xr-x[.+]? 1 \\S+ +6 [A-Z][a-z][a-z] [ 1-3][0-9] [0-2][0-9]:[0-5][0-9] bin.dat$" bin) "~s" bin))))
    ;; permissions in octal, the set-user bit's digit in its colour
    (let ((tuatara-user:*rimu-permissions* :octal))
      (multiple-value-bind (f editor) (rimu-session dir "I")
        (let* ((lines (rimu-lines editor))
               (bin (find " bin.dat" lines :test (lambda (end line) (uiop:string-suffix-p line end))))
               (sub (find-if (lambda (line) (cl-ppcre:scan "[0-9] sub$" line)) lines)))
          (true (cl-ppcre:scan "^ +[0-9]+ -4755[.+]? 1 \\S+ \\S+ +6 " bin) "~s" bin)
          (true (cl-ppcre:scan "^ +[0-9]+ d[0-7]{3}[.+]? " sub) "~s" sub)
          (let ((row (loop for row from 0 below (length (tuatara.frontend.fake:fake-screen f))
                           when (uiop:string-suffix-p (tuatara.frontend.fake:fake-screen-line f row) " bin.dat") return row))
                (digit (1+ (search " -4755" bin))))
            (true (find :rimu-set-id (aref (tuatara.frontend.fake:fake-spans f) row)
                        :test (lambda (face span) (and (<= (first span) (1+ digit)) (< (1+ digit) (second span)) (eq face (third span))))))))))
    (is string= "d755" (tuatara.defaults::mode-octal-text #o40755))
    (is string= "-644" (tuatara.defaults::mode-octal-text #o100644))
    (is string= "-4755" (tuatara.defaults::mode-octal-text #o104755))
    (is string= "d1777" (tuatara.defaults::mode-octal-text #o41777))
    ;; I again hides them
    (multiple-value-bind (f editor) (rimu-session dir "I I")
      (true (tuatara.frontend.fake:fake-frame-showing f "Details hidden"))
      (is string= "  link-to-dir" (second (rimu-lines editor))))
    ;; the setting shows them in new lists
    (let ((tuatara-user:*rimu-details* t))
      (rimu-forget)
      (multiple-value-bind (f editor) (run-keys (format nil "C-x d C-a C-k ~a C-j" (rimu-typed (namestring dir))) :width 100 :height 20)
        (declare (ignore f))
        (true (uiop:string-prefix-p "  total used in directory " (second (rimu-lines editor))))))))

(define-test (rimu dotfiles-and-order)
  (with-rimu-tree (dir)
    ;; H shows the names starting with a dot, . and .. first
    (multiple-value-bind (f editor) (rimu-session dir "H")
      (true (tuatara.frontend.fake:fake-frame-showing f "Names starting with a dot shown"))
      (is equal '("  ." "  .." "  .hidden-dir" "  link-to-dir" "  sub" "  .dotfile" "  bin.dat")
          (subseq (rimu-lines editor) 1 8)))
    (multiple-value-bind (f editor) (rimu-session dir "H H")
      (declare (ignore f))
      (is string= "  link-to-dir" (second (rimu-lines editor))))
    ;; s orders by time, the latest first, directories still first
    (let ((now (- (get-universal-time) tuatara.dates:+unix-epoch+)))
      (sb-posix:utimes (namestring (merge-pathnames "text.txt" dir)) (+ now 100) (+ now 100))
      (sb-posix:utimes (namestring (merge-pathnames "data.csv" dir)) 1000000000 1000000000))
    (multiple-value-bind (f editor) (rimu-session dir "s")
      (let ((lines (rimu-lines editor)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Rimu  by time"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Sorted by time, the latest first"))
        (is equal '("  link-to-dir" "  sub") (sort (subseq lines 1 3) #'string<))
        (is string= "  text.txt" (fourth lines))
        (is string= "  data.csv" (nth (- (length lines) 2) lines))))
    ;; a date over half a year old shows its year, as ls -l does
    (multiple-value-bind (f editor) (rimu-session dir "I")
      (declare (ignore f))
      (true (find-if (lambda (line) (cl-ppcre:scan "Sep [ 0-9][0-9]  2001 data\\.csv$" line)) (rimu-lines editor))))
    (multiple-value-bind (f editor) (rimu-session dir "s s")
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "Rimu  by name")))))

(define-test (rimu moving)
  (with-rimu-tree (dir)
    (let ((name (rimu-list-name dir)))
      ;; l lists a directory in the same buffer, named after it
      (multiple-value-bind (f editor) (rimu-session dir "j l")
        (declare (ignore f))
        (is string= "sub/" (current-name editor))
        (is equal (list (format nil "  ~asub:" (namestring dir)) "  deeper" "") (rimu-lines editor))
        (is eq nil (tuatara.buffer:find-buffer name)))
      ;; h goes up, point on the directory left
      (multiple-value-bind (f editor) (rimu-session dir "j l h")
        (declare (ignore f))
        (is string= name (current-name editor))
        (let ((point (session-point editor)) (text (session-text editor)))
          (is string= "sub" (subseq text point (+ point 3)))))
      ;; a link to a directory is listed by the link's name
      (multiple-value-bind (f editor) (rimu-session dir "l")
        (declare (ignore f))
        (is string= (format nil "  ~alink-to-dir:" (namestring dir)) (first (rimu-lines editor))))
      ;; RET visits a file; with a prefix argument, safely
      (multiple-value-bind (f editor) (rimu-session dir "g f t e x t . t x t RET RET")
        (declare (ignore f))
        (is string= "text.txt" (current-name editor))
        (is string= (format nil "hello~%") (session-text editor)))
      (let ((visiting (find (tuatara.io:expand-path (merge-pathnames "text.txt" dir)) tuatara.buffer:*buffers*
                            :key #'tuatara.buffer:buffer-file :test #'equal)))
        (when visiting (tuatara.buffer:kill-buffer visiting)))
      (multiple-value-bind (f editor) (rimu-session dir "g f t e x t . t x t RET C-u RET")
        (true (tuatara.frontend.fake:fake-frame-showing f "[%] Safe text.txt"))
        (true (tuatara.io:safe-visit-p (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
      ;; a named pipe and a link leading nowhere are not opened
      (multiple-value-bind (f editor) (rimu-session dir "g f p i p e RET l")
        (true (tuatara.frontend.fake:fake-frame-showing f "pipe is a named pipe; rimu does not open it"))
        (is string= name (current-name editor)))
      (multiple-value-bind (f editor) (rimu-session dir "g f b r o k e n RET l")
        (declare (ignore editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "broken-link leads nowhere: nowhere")))
      ;; a name that is not UTF-8 opens, and saves under the very same octets
      (multiple-value-bind (f editor) (rimu-session dir "g f r a w RET l i h i C-g C-x C-s")
        (declare (ignore f))
        (is string= "hi" (session-text editor))
        (let ((entry (find (rimu-raw-name) (tuatara.io:list-directory (namestring dir))
                           :key #'tuatara.io:listed-octets :test #'equalp)))
          (is = 2 (tuatara.io:stamp-size (tuatara.io:listed-stamp entry))))
        (is = 1 (count-if (lambda (e) (search "raw" (tuatara.io:listed-name e))) (tuatara.io:list-directory (namestring dir)))))
      ;; NORMAL state's motions by word and expression and its other selections are free keys:
      ;; they say they are undefined and leave point where it is
      (dolist (key tuatara.defaults::*list-free-keys*)
        (multiple-value-bind (f editor) (rimu-session dir (format nil "g f t e x t . t x t RET ~a" key))
          (true (tuatara.frontend.fake:fake-frame-showing f (format nil "~a is undefined" key)))
          (let ((point (session-point editor)))
            (is string= "text.txt" (subseq (session-text editor) point (+ point 8))))))
      ;; M-x show-file-name says the full name of the entry on the line; on the directory's name, the directory
      (multiple-value-bind (f editor) (rimu-session dir "g f t e x t . t x t RET M-x s h o w - f i l e - n a m e RET")
        (declare (ignore editor))
        (true (tuatara.frontend.fake:fake-frame-showing f (format nil "~atext.txt" (namestring dir)))))
      (multiple-value-bind (f editor) (rimu-session dir "g g M-x s h o w - f i l e - n a m e RET")
        (declare (ignore f))
        (is string= (namestring dir) (tuatara.command:editor-message editor)))
      ;; V selects lines, J and K taking in more
      (multiple-value-bind (f editor) (rimu-session dir "V J")
        (declare (ignore editor))
        (flet ((selected-p (row) (find :region (aref (tuatara.frontend.fake:fake-spans f) row)
                                       :key (lambda (span) (let ((face (third span))) (if (listp face) (first face) face))))))
          (true (selected-p 1))
          (true (selected-p 2))
          (false (selected-p 3))))
      ;; C-o shows the entry in another window and stays in the list
      (multiple-value-bind (f editor) (rimu-session dir "g f s u b RET C-o")
        (declare (ignore f))
        (is = 2 (length (tuatara.command:editor-windows editor)))
        (is string= name (current-name editor))
        (true (find "sub/" (tuatara.command:editor-windows editor)
                    :key (lambda (w) (tuatara.buffer:buffer-name (tuatara.display:window-buffer w))) :test #'string=)))
      ;; > and < go to the next and the previous directory
      (multiple-value-bind (f editor) (rimu-session dir ">")
        (declare (ignore f))
        (let ((point (session-point editor)))
          (is string= "sub" (subseq (session-text editor) point (+ point 3)))))
      (multiple-value-bind (f editor) (rimu-session dir "> >")
        (declare (ignore editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "No directory below")))
      (multiple-value-bind (f editor) (rimu-session dir "> <")
        (declare (ignore f))
        (let ((point (session-point editor)))
          (is string= "link-to-dir" (subseq (session-text editor) point (+ point 11)))))
      ;; r reads the directory again, point staying on its entry
      (multiple-value-bind (f editor) (rimu-session dir "g f t e x t . t x t RET r")
        (declare (ignore f))
        (let ((point (session-point editor)))
          (is string= "text.txt" (subseq (session-text editor) point (+ point 8)))))
      ;; q leaves the list
      (multiple-value-bind (f editor) (rimu-session dir "q")
        (declare (ignore f))
        (is string= "*scratch*" (current-name editor))))))

(define-test (rimu directory-to-directory)
  ;; > goes to the next directory of the list, < to the one before, the
  ;; cursor on its name; none further that way leaves it where it is
  (with-test-directory (dir)
    (dolist (name '("a.txt" "n.txt" "z.txt")) (alexandria:write-string-into-file name (merge-pathnames name dir)))
    (dolist (name '("m-dir/" "y-dir/")) (ensure-directories-exist (merge-pathnames name dir)))
    (flet ((name-at (keys)
             (multiple-value-bind (f editor) (rimu-session dir keys)
               (declare (ignore f))
               (let* ((buffer (session-buffer editor))
                      (entry (tuatara.defaults::list-entry-at buffer)))
                 (and (tuatara.io:listed-p entry) (tuatara.io:listed-name entry))))))
      (is equal "y-dir" (name-at "M-< > >"))
      (is equal "y-dir" (name-at "M-> <"))
      (is equal "m-dir" (name-at "M-> < <")))))

(define-test (rimu jump)
  (with-rimu-tree (dir)
    (let ((file (namestring (merge-pathnames "text.txt" dir))))
      ;; C-x C-j and SPC d d list the file's directory, point on the file
      (dolist (keys '("C-x C-j" "SPC d d"))
        (multiple-value-bind (f editor) (rimu-session dir keys :file file)
          (declare (ignore f))
          (is string= (rimu-list-name dir) (current-name editor))
          (let ((point (session-point editor)))
            (is string= "text.txt" (subseq (session-text editor) point (+ point 8))))))
      ;; in a list, it goes up
      (multiple-value-bind (f editor) (rimu-session dir "C-x C-j C-x C-j" :file file)
        (declare (ignore f))
        (is string= "tuatara/" (current-name editor))
        (let ((point (session-point editor)) (below (car (last (pathname-directory dir)))))
          (is string= below (subseq (session-text editor) point (+ point (length below)))))))))

(define-test (rimu stands-in-its-directory)
  (with-rimu-tree (dir)
    (multiple-value-bind (f editor) (rimu-session dir "C-x C-f" :width (width-for dir))
      (declare (ignore editor))
      (let ((list (find-if #'tuatara.defaults::rimu-list-p tuatara.buffer:*buffers*)))
        (is string= (namestring dir) (tuatara.command:buffer-directory list))
        ;; a file is asked for from the directory listed
        (true (tuatara.frontend.fake:fake-frame-showing f (format nil "Find file: ~a"
                                                                  (tuatara.defaults::abbreviated-path (namestring dir)))))))
    ;; the directory prompt lists a directory with a name that is not UTF-8
    (multiple-value-bind (f editor) (run-keys (format nil "C-x C-f C-a C-k ~a r a w" (rimu-typed (namestring dir))) :width 100 :height 20)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "raw\\xFF\\xFEname"))
      (false (tuatara.frontend.fake:fake-frame-showing f "decoding error")))
    (true (find (rimu-raw-string) (tuatara.io:directory-entries dir) :key #'first :test #'string=))))

(define-test (rimu preview-leaves-a-hidden-entry)
  ;; a search filter hiding the entry the preview shows empties the preview
  ;; when no entry is left at point
  (with-rimu-tree (dir)
    (flet ((previews () (mapcar #'tuatara.buffer:buffer-name
                                (remove-if-not (lambda (b) (search "(preview" (tuatara.buffer:buffer-name b))) tuatara.buffer:*buffers*))))
      (multiple-value-bind (f editor) (rimu-session-then dir "g f t e x t . t x t RET" "C-x / s z q z q RET")
        (declare (ignore editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "text.txt (preview)"))
        (false (member "text.txt (preview)" (previews) :test #'string=))
        (true (tuatara.frontend.fake:fake-frame-showing f "No entry on this line."))))))

(define-test (rimu preview)
  (with-rimu-tree (dir)
    (flet ((previews () (remove-if-not (lambda (b) (search "(preview" (tuatara.buffer:buffer-name b))) tuatara.buffer:*buffers*))
           (windows (editor) (length (tuatara.command:editor-windows editor))))
      ;; the file on the line, beside the list
      (multiple-value-bind (f editor) (rimu-session dir "g f t e x t . t x t RET" :preview t)
        (is = 2 (windows editor))
        (is string= (rimu-list-name dir) (current-name editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "text.txt (preview)"))
        (true (tuatara.frontend.fake:fake-frame-showing f "hello"))
        (is equal '("text.txt (preview)") (mapcar #'tuatara.buffer:buffer-name (previews)))
        ;; finding a file from it starts in the file's folder
        (is string= (tuatara.io:native dir) (tuatara.buffer:buffer-local (first (previews)) :directory)))
      ;; the one before goes when the next is shown
      (multiple-value-bind (f editor) (rimu-session dir "g f t e x t . t x t RET g f d a t a RET" :preview t)
        (declare (ignore f editor))
        (is equal '("data.csv (preview)") (mapcar #'tuatara.buffer:buffer-name (previews))))
      ;; a binary file in the hex view, read-only
      (multiple-value-bind (f editor) (rimu-session dir "g f b i n RET" :preview t)
        (declare (ignore f editor))
        (let ((preview (first (previews))))
          (true (tuatara.display:hex-view-p preview))
          (true (tuatara.buffer:buffer-local preview :read-only))))
      ;; a directory's own list, with no preview of its own
      (multiple-value-bind (f editor) (rimu-session dir "g f s u b RET" :preview t)
        (declare (ignore editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "sub/ (preview)"))
        (true (tuatara.frontend.fake:fake-frame-showing f "  deeper")))
      ;; a named pipe is not opened: the preview says what it is
      (multiple-value-bind (f editor) (rimu-session dir "g f p i p e RET" :preview t)
        (declare (ignore editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "pipe is a named pipe; rimu does not open it.")))
      ;; P closes it, and opens it again
      (multiple-value-bind (f editor) (rimu-session dir "g f t e x t . t x t RET P" :preview t)
        (true (tuatara.frontend.fake:fake-frame-showing f "Preview off"))
        (is = 1 (windows editor))
        (is eq nil (previews)))
      (multiple-value-bind (f editor) (rimu-session dir "g f t e x t . t x t RET P P" :preview t)
        (true (tuatara.frontend.fake:fake-frame-showing f "Preview on"))
        (is = 2 (windows editor)))
      ;; leaving the list closes it; so does opening a file in the list's window
      (multiple-value-bind (f editor) (rimu-session dir "g f t e x t . t x t RET q" :preview t)
        (declare (ignore f))
        (is = 1 (windows editor))
        (is eq nil (previews)))
      (multiple-value-bind (f editor) (rimu-session dir "g f d a t a RET l" :preview t)
        (declare (ignore f))
        (is string= "data.csv" (current-name editor))
        (is = 1 (windows editor))
        (is eq nil (previews)))
      ;; a file visited already is shown as it is, and stays
      (let* ((file (namestring (merge-pathnames "text.txt" dir)))
             (path (tuatara.io:expand-path file)))
        (flet ((visiting () (find path tuatara.buffer:*buffers* :key #'tuatara.buffer:buffer-file :test #'equal)))
          (multiple-value-bind (f editor) (rimu-session dir "C-x C-j" :file file :preview t)
            (declare (ignore f))
            (is = 2 (windows editor))
            (is eq nil (previews))
            (true (find (visiting) (tuatara.command:editor-windows editor) :key #'tuatara.display:window-buffer)))
          (multiple-value-bind (f editor) (rimu-session dir "C-x C-j q" :file file :preview t)
            (declare (ignore f editor))
            (true (visiting))))))))

(define-test (rimu preview-window-reused)
  (with-rimu-tree (dir)
    (flet ((previews () (remove-if-not (lambda (b) (search "(preview" (tuatara.buffer:buffer-name b))) tuatara.buffer:*buffers*))
           (shown (editor) (mapcar (lambda (w) (tuatara.buffer:buffer-name (tuatara.display:window-buffer w)))
                                   (tuatara.command:editor-windows editor))))
      (unwind-protect
           (progn
             ;; C-o shows the file in the preview's window: the list and that one window, no preview left
             (multiple-value-bind (f editor) (rimu-session dir "g f t e x t . t x t RET C-o" :preview t)
               (declare (ignore f))
               (is equal (list (rimu-list-name dir) "text.txt") (shown editor))
               (is string= (rimu-list-name dir) (current-name editor))
               (is eq nil (previews)))
             ;; moving on, the preview takes that window again, the file staying open
             (multiple-value-bind (f editor) (rimu-session dir "g f t e x t . t x t RET C-o g f d a t a RET" :preview t)
               (declare (ignore f))
               (is equal (list (rimu-list-name dir) "data.csv (preview)") (shown editor))
               (true (tuatara.buffer:find-buffer "text.txt")))
             ;; a directory: its own list, in the preview's window
             (multiple-value-bind (f editor) (rimu-session dir "g f s u b RET C-o" :preview t)
               (declare (ignore f))
               (is equal (list (rimu-list-name dir) "sub/") (shown editor))))
        (kill-buffers-under dir)
        (rimu-forget)))))

(define-test (rimu preview-keeps-its-name)
  ;; the list read again, the preview made again keeps its name, not NAME<2>
  (with-rimu-tree (dir)
    (dolist (keys '("g f t e x t . t x t RET r" "g f s u b RET r"))
      (multiple-value-bind (f editor) (rimu-session dir (list keys :tick :tick) :preview t)
        (declare (ignore f editor))
        (is equal (list (if (search "s u b" keys) "sub/ (preview)" "text.txt (preview)"))
            (mapcar #'tuatara.buffer:buffer-name
                    (remove-if-not (lambda (b) (search "(preview" (tuatara.buffer:buffer-name b))) tuatara.buffer:*buffers*)))))
    (rimu-forget)))

(define-test (rimu point-on-entries)
  ;; the cursor stays on the entries: g e and j on the last entry put it on the last one's name
  (with-rimu-tree (dir)
    (dolist (keys '("g e" "g e j" "g e k j j"))
      (multiple-value-bind (f editor) (rimu-session dir keys)
        (declare (ignore f))
        (let* ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor)))
               (lines (tuatara.defaults::rimu-layout-lines (tuatara.buffer:buffer-local buffer 'tuatara.defaults::rimu-layout)))
               (last (position-if #'identity lines :from-end t)))
          (is = (tuatara.defaults::rimu-line-position buffer last) (session-point editor)))))))

(define-test (rimu metadata-left)
  ;; g i shows the file's metadata in the list's place: C-x k and q both go back to the list, the preview with it
  (with-rimu-tree (dir)
    (flet ((read-p ()
             (notany (lambda (job) (and (tuatara.defaults::metadata-job-p job) (eq (tuatara.runtime:job-status job) :running)))
                     tuatara.runtime:*jobs*)))
      (dolist (leave '("C-x k" "q"))
        (multiple-value-bind (f editor)
            ;; the metadata read first: killing its buffer then asks nothing of a job
            (rimu-session dir (list "g f t e x t . t x t RET g i" (lambda () (loop repeat 250 until (read-p) do (sleep 0.02)))
                                    :tick leave :tick)
                          :preview t)
          (false (tuatara.frontend.fake:fake-frame-showing f "q is undefined"))
          (is string= (rimu-list-name dir) (current-name editor))
          (is eq nil (find "text.txt" tuatara.buffer:*buffers* :key #'tuatara.buffer:buffer-name :test #'string=))
          (let ((metadata (tuatara.buffer:find-buffer "*Metadata: text.txt*")))
            (when metadata (tuatara.buffer:kill-buffer metadata))))))
    (rimu-forget)))

(define-test (rimu preview-tiff-size)
  ;; a TIFF whose first directory follows its pixels: its size read there
  (with-test-directory (dir)
    (flet ((le (n k) (loop for i below k collect (ldb (byte 8 (* 8 i)) n))))
      (alexandria:write-byte-vector-into-file
       (kind-octets (list 73 73 42 0) (le 24 4) (make-list 16 :initial-element 0)
                    (le 2 2) (le 256 2) (le 3 2) (le 1 4) (le 6000 4) (le 257 2) (le 3 2) (le 1 4) (le 6000 4) (le 0 4))
       (merge-pathnames "big.tif" dir)))
    (let ((f (rimu-session dir "" :preview t :width 160)))
      (true (tuatara.frontend.fake:fake-frame-showing f "TIFF image, 6000 × 6000")))
    (rimu-forget)))

(define-test (rimu quick-edit)
  (with-rimu-tree (dir)
    (let* ((path (tuatara.io:expand-path (merge-pathnames "text.txt" dir)))
           (name (rimu-list-name dir)))
      (flet ((visiting () (find path tuatara.buffer:*buffers* :key #'tuatara.buffer:buffer-file :test #'equal))
             (previews () (remove-if-not (lambda (b) (search "(preview" (tuatara.buffer:buffer-name b))) tuatara.buffer:*buffers*))
             (windows (editor) (length (tuatara.command:editor-windows editor)))
             (file () (uiop:read-file-string path)))
        (flet ((fresh () (let ((b (visiting))) (when b (tuatara.buffer:kill-buffer b)))))
          ;; o: the preview becomes the file itself, editable, the cursor there
          (fresh)
          (multiple-value-bind (f editor) (rimu-session dir "g f t e x t . t x t RET o" :preview t :width 200)
            (is = 2 (windows editor))
            (is eq (visiting) (tuatara.display:window-buffer (tuatara.command:editor-window editor)))
            (false (tuatara.buffer:buffer-local (visiting) :read-only))
            (is eq nil (previews))
            (true (tuatara.frontend.fake:fake-frame-showing f "Text  Quick edit"))
            (false (tuatara.frontend.fake:fake-frame-showing f "preview  "))
            (true (tuatara.frontend.fake:fake-frame-showing f "Quick edit: q goes back to the preview")))
          ;; C-c C-c after a change asks; y saves, and the preview follows the list again
          (fresh)
          (multiple-value-bind (f editor) (rimu-session dir "g f t e x t . t x t RET o i X C-g C-c C-c y RET" :preview t)
            (is string= name (current-name editor))
            (is string= (format nil "Xhello~%") (file))
            (is eq nil (visiting))
            (is equal '("text.txt (preview)") (mapcar #'tuatara.buffer:buffer-name (previews)))
            (true (tuatara.frontend.fake:fake-frame-showing f "Xhello")))
          ;; n lets the change go
          (fresh)
          (multiple-value-bind (f editor) (rimu-session dir "g f t e x t . t x t RET o i Y C-g C-c C-c n RET" :preview t)
            (declare (ignore f))
            (is string= name (current-name editor))
            (is string= (format nil "Xhello~%") (file))
            (is eq nil (visiting)))
          ;; nothing changed: no question
          (fresh)
          (multiple-value-bind (f editor) (rimu-session dir "g f t e x t . t x t RET o C-c C-c" :preview t)
            (false (tuatara.frontend.fake:fake-frame-showing f "Save text.txt?"))
            (is string= name (current-name editor)))
          ;; C-g at the question goes on with the quick edit
          (fresh)
          (multiple-value-bind (f editor) (rimu-session dir "g f t e x t . t x t RET o i Z C-g C-c C-c C-g" :preview t)
            (declare (ignore f))
            (is eq (visiting) (tuatara.display:window-buffer (tuatara.command:editor-window editor)))
            (true (tuatara.buffer:buffer-modified-p (visiting))))
          ;; q in NORMAL goes back too; in INSERT it is a letter typed
          (fresh)
          (multiple-value-bind (f editor) (rimu-session dir "g f t e x t . t x t RET o q" :preview t)
            (declare (ignore f))
            (is string= name (current-name editor))
            (is eq nil (visiting)))
          (fresh)
          (multiple-value-bind (f editor) (rimu-session dir "g f t e x t . t x t RET o i q C-g" :preview t)
            (declare (ignore f))
            (is eq (visiting) (tuatara.display:window-buffer (tuatara.command:editor-window editor)))
            (is string= (format nil "qXhello~%") (session-text editor)))
          (fresh)
          (multiple-value-bind (f editor) (rimu-session dir "g f t e x t . t x t RET o i Q C-g q n RET" :preview t)
            (declare (ignore f))
            (is string= name (current-name editor))
            (is string= (format nil "Xhello~%") (file))
            (is eq nil (visiting)))
          ;; the cursor going back to the list goes back to the preview, asking about changes
          (fresh)
          (multiple-value-bind (f editor) (rimu-session dir "g f t e x t . t x t RET o C-x o" :preview t)
            (declare (ignore f))
            (is string= name (current-name editor))
            (is eq nil (visiting))
            (is equal '("text.txt (preview)") (mapcar #'tuatara.buffer:buffer-name (previews))))
          (fresh)
          (multiple-value-bind (f editor) (rimu-session dir "g f t e x t . t x t RET o i Q C-g C-x o y RET" :preview t)
            (declare (ignore f))
            (is string= name (current-name editor))
            (is string= (format nil "QXhello~%") (file))
            (is eq nil (visiting)))
          ;; C-g at that question takes the cursor back into the quick edit
          (fresh)
          (multiple-value-bind (f editor) (rimu-session dir "g f t e x t . t x t RET o i R C-g C-x o C-g" :preview t)
            (true (tuatara.frontend.fake:fake-frame-showing f "The quick edit goes on"))
            (is eq (visiting) (tuatara.display:window-buffer (tuatara.command:editor-window editor)))
            (true (tuatara.buffer:buffer-modified-p (visiting))))
          ;; its keys and NORMAL's are all listed by C-h b there
          (fresh)
          (multiple-value-bind (f editor) (rimu-session dir "g f t e x t . t x t RET o C-h b" :preview t :height 40)
            (declare (ignore f editor))
            (let ((help (tuatara.buffer:find-buffer "*Help*")))
              (true (search "rimu-quick-edit-done" (tuatara.buffer:buffer-string help)))
              (true (search "forward-word" (tuatara.buffer:buffer-string help)))))
          ;; a buffer open before keeps its changes when they are let go, and stays
          (fresh)
          (multiple-value-bind (f editor) (rimu-session dir "C-x C-j o i W C-g C-c C-c n RET" :preview t
                                                                                         :file (namestring path))
            (true (tuatara.frontend.fake:fake-frame-showing f "keeps its changes, not saved"))
            (is string= name (current-name editor))
            (true (tuatara.buffer:buffer-modified-p (visiting))))
          ;; a binary file stays in the hex view its preview had, read-only as it opens safely, and says so
          (let ((bin (tuatara.io:expand-path (merge-pathnames "bin.dat" dir))))
            (flet ((bin-buffer () (find bin tuatara.buffer:*buffers* :key #'tuatara.buffer:buffer-file :test #'equal)))
              (let ((b (bin-buffer))) (when b (tuatara.buffer:kill-buffer b)))
              (multiple-value-bind (f editor) (rimu-session dir "g f b i n RET o" :preview t)
                (true (tuatara.frontend.fake:fake-frame-showing f "bin.dat is binary: shown in the hex view, read-only"))
                (is eq (bin-buffer) (tuatara.display:window-buffer (tuatara.command:editor-window editor)))
                (true (tuatara.display:hex-view-p (bin-buffer)))
                (true (tuatara.buffer:buffer-local (bin-buffer) :read-only)))
              (let ((b (bin-buffer))) (when b (tuatara.buffer:kill-buffer b)))
              (multiple-value-bind (f editor) (rimu-session dir "g f b i n RET o q" :preview t)
                (declare (ignore f))
                (is string= name (current-name editor))
                (is eq nil (bin-buffer)))))
          ;; with the preview off, the quick edit makes a window beside the list, gone with it
          (fresh)
          (multiple-value-bind (f editor) (rimu-session dir "g f t e x t . t x t RET o" :preview nil)
            (declare (ignore f))
            (is = 2 (windows editor))
            (is eq (visiting) (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
          (fresh)
          (multiple-value-bind (f editor) (rimu-session dir "g f t e x t . t x t RET o C-c C-c" :preview nil)
            (declare (ignore f))
            (is = 1 (windows editor))
            (is string= name (current-name editor)))
          ;; on a directory, the cursor goes into its list beside
          (multiple-value-bind (f editor) (rimu-session dir "g f s u b RET o" :preview t)
            (declare (ignore f))
            (is string= "sub/ (preview)" (current-name editor))
            (is = 2 (windows editor)))
          ;; M-x offers going back from a quick edit in a quick edit only
          (fresh)
          (multiple-value-bind (f editor) (rimu-session dir "g f t e x t . t x t RET o M-x r i m u - q u i c k - e d i t - d"
                                                        :preview t :width 200)
            (declare (ignore editor))
            (true (tuatara.frontend.fake:fake-frame-showing f "rimu-quick-edit-done")))
          (multiple-value-bind (f editor) (rimu-session dir "M-x r i m u - q u i c k - e d i t - d" :preview t :width 200)
            (declare (ignore editor))
            (false (tuatara.frontend.fake:fake-frame-showing f "rimu-quick-edit-done")))
          ;; a named pipe is not opened
          (multiple-value-bind (f editor) (rimu-session dir "g f p i p e RET o" :preview t)
            (true (tuatara.frontend.fake:fake-frame-showing f "pipe is a named pipe; rimu does not open it"))
            (is string= name (current-name editor))))))))

(defun kind-octets (&rest parts)
  "PARTS, strings of ASCII and lists of octets, as one vector of octets."
  (coerce (loop for part in parts
                append (if (stringp part) (map 'list #'char-code part) part))
          '(vector (unsigned-byte 8))))

(defun png-octets (width height)
  "The start of a PNG image WIDTH by HEIGHT, 8-bit RGBA: its mark and header."
  (kind-octets (list #x89 80 78 71 13 10 26 10 0 0 0 13) "IHDR"
               (list (ldb (byte 8 24) width) (ldb (byte 8 16) width) (ldb (byte 8 8) width) (ldb (byte 8 0) width)
                     (ldb (byte 8 24) height) (ldb (byte 8 16) height) (ldb (byte 8 8) height) (ldb (byte 8 0) height)
                     8 6 0 0 0)))

(defun tiff-octets (entries &key (at-8 '(8 0 0 0)) make)
  "The start of a little-endian TIFF whose first directory, at octet 8 (unless
AT-8 says CR2's), holds ENTRIES, each (TAG TYPE COUNT VALUE), and, with MAKE,
a Make tag naming it, its text after the directory."
  (let* ((all (append entries (and make (list (list 271 2 (1+ (length make)) :make)))))
         (ifd (if (equal at-8 '(8 0 0 0)) 8 16))
         (text-at (+ ifd 2 (* 12 (length all)) 4)))
    (flet ((le (n k) (loop for i below k collect (ldb (byte 8 (* 8 i)) n))))
      (kind-octets (list 73 73 42 0) (le ifd 4)
                   (if (= ifd 16) (append (list 67 82 2 0) (le 0 4)) '())
                   (le (length all) 2)
                   (loop for (tag type count value) in all
                         append (append (le tag 2) (le type 2) (le count 4) (le (if (eq value :make) text-at value) 4)))
                   (le 0 4)
                   (if make (append (map 'list #'char-code make) (list 0)) '())))))

(define-test (rimu kinds)
  (flet ((kind (&rest parts) (multiple-value-list (tuatara.io:octets-kind (apply #'kind-octets parts)))))
    ;; told by the octets, with what they say of size and make
    (is equal '("PNG image, 640 × 480, 8-bit RGBA" :png) (kind (coerce (png-octets 640 480) 'list)))
    ;; the PNG signature with no image header after it is a damaged PNG, no size made up
    (is equal '("damaged PNG image: its image header is missing or wrong" :png)
        (kind (list #x89 80 78 71 13 10 26 10) "garbagegarbagegarbagegarbage"))
    (is equal '("JPEG image, 640 × 480" :jpeg)
        (kind (list #xFF #xD8 #xFF #xE0 0 16) "JFIF" (list 0 1 1 0 0 1 0 1 0 0)
              (list #xFF #xC0 0 17 8 1 #xE0 2 #x80 3 1 #x22 0 2 #x11 1 3 #x11 1)))
    (is equal '("GIF image, 640 × 480" :gif) (kind "GIF89a" (list #x80 2 #xE0 1 0 0 0)))
    ;; a TIFF with its size; a camera's raw told from it by its marks, of each maker's shape
    (is equal '("TIFF image, 640 × 480" :tiff) (kind (coerce (tiff-octets '((256 3 1 640) (257 3 1 480))) 'list)))
    (is equal '("Camera raw, Canon CR2" :raw) (kind (coerce (tiff-octets '((256 3 1 160)) :at-8 '(16 0 0 0)) 'list)))
    (is equal '("Camera raw, DNG" :raw) (kind (coerce (tiff-octets '((256 3 1 256) (50706 1 4 1))) 'list)))
    (is equal '("Camera raw, Nikon NEF" :raw)
        (kind (coerce (tiff-octets '((256 3 1 160) (34665 4 1 0)) :make "NIKON CORPORATION") 'list)))
    ;; a maker's scanner TIFF, with no camera's EXIF: a TIFF still
    (is equal '("TIFF image, 160 × 120" :tiff)
        (kind (coerce (tiff-octets '((256 3 1 160) (257 3 1 120)) :make "NIKON") 'list)))
    (is equal '("Camera raw, Fujifilm RAF" :raw) (kind "FUJIFILMCCD-RAW 0201FF383501"))
    (is equal '("Camera raw, Canon CR3" :raw) (kind (list 0 0 0 24) "ftypcrx " (list 0 0 0 1) "crx isom"))
    ;; an AVIF and a HEIF with their size: the largest of their pictures, not a thumbnail's, from
    ;; the item properties alone (meta, iprp, ipco), the same four letters in other boxes not taken
    (labels ((box (type &rest parts)
               (let ((body (coerce (apply #'kind-octets parts) 'list)))
                 (append (list 0 0 (ash (+ 8 (length body)) -8) (logand (+ 8 (length body)) 255))
                         (map 'list #'char-code type) body)))
             (ispe (width height)
               (box "ispe" (list 0 0 0 0 0 0 (ash width -8) (logand width 255) 0 0 (ash height -8) (logand height 255))))
             (heif (brand &rest sizes)
               (kind (list 0 0 0 24) "ftyp" brand (list 0 0 0 0) "mif1" brand
                     (box "meta" (list 0 0 0 0) (box "iprp" (box "ipco" (apply #'append sizes))))
                     (box "mdat" (ispe 30000 30000)))))
      (is equal '("AVIF image, 8000 × 6000" :avif) (heif "avif" (ispe 320 240) (ispe 8000 6000)))
      (is equal '("HEIF image, 4032 × 3024" :heif) (heif "heic" (ispe 4032 3024) (ispe 512 384)))
      (is equal '(4032 3024) (multiple-value-list (tuatara.io:octets-picture-size
                                                   (apply #'kind-octets (list 0 0 0 24) "ftypheic" (list 0 0 0 0) "mif1heic"
                                                          (box "meta" (list 0 0 0 0) (box "iprp" (box "ipco" (ispe 4032 3024))))
                                                          nil))))
      (is equal '("HEIF image" :heif) (kind (list 0 0 0 24) "ftypheic" (list 0 0 0 0) "mif1heic" (ispe 4032 3024))))
    ;; a JPEG with fill octets and a restart marker before its frame header still says its size
    (is equal '("JPEG image, 640 × 480" :jpeg)
        (kind (list #xFF #xD8 #xFF #xFF #xFF #xD0 #xFF #xE0 0 16) "JFIF" (list 0 1 1 0 0 1 0 1 0 0)
              (list #xFF #xC0 0 17 8 1 #xE0 2 #x80 3 1 #x22 0 2 #x11 1 3 #x11 1)))
    ;; the size alone, as the words give it; none for what is no picture or a camera's raw
    (is equal '(640 480) (multiple-value-list (tuatara.io:octets-picture-size (png-octets 640 480))))
    (is equal '(640 480) (multiple-value-list (tuatara.io:octets-picture-size (tiff-octets '((256 3 1 640) (257 3 1 480))))))
    (is equal '(nil nil) (multiple-value-list (tuatara.io:octets-picture-size (tiff-octets '((256 3 1 256) (50706 1 4 1))))))
    (is equal '(nil nil) (multiple-value-list (tuatara.io:octets-picture-size (kind-octets "%PDF-1.4"))))
    (is equal '("Camera raw, Canon CRW" :raw) (kind (list 73 73 26 0 0 0) "HEAPCCDR"))
    (is equal '("Camera raw, Olympus ORF" :raw) (kind (list 73 73 82 79 8 0 0 0)))
    (is equal '("Camera raw, Panasonic or Leica RW2" :raw) (kind (list 73 73 85 0 8 0 0 0)))
    (is equal '("Camera raw, Minolta MRW" :raw) (kind (list 0 77 82 77 0 0 0 0)))
    (false (tuatara.io:kind-name-disagrees-p :raw "IMG_0001.CR3"))
    (true (tuatara.io:kind-name-disagrees-p :raw "IMG_0001.jpg"))
    (is equal '("PDF document, version 1.4" :pdf) (kind "%PDF-1.4" (list 10)))
    (is equal '("ELF 64-bit executable, x86-64" :elf) (kind (list #x7F 69 76 70 2 1 1 0 0 0 0 0 0 0 0 0 2 0 62 0)))
    (is equal '("gzip compressed data, was notes.txt" :gzip) (kind (list #x1F #x8B 8 8 0 0 0 0 0 3) "notes.txt" (list 0)))
    (is equal '("Word document (OOXML)" :docx) (kind (coerce (tuatara.io:zip-octets '(("[Content_Types].xml" . "") ("word/document.xml" . ""))) 'list)))
    (is equal '("tar archive" :tar) (kind (make-list 257 :initial-element 0) "ustar"))
    (is equal '("SQLite database" :sqlite) (kind "SQLite format 3" (list 0)))
    (is equal '("script, run by /bin/sh" :script) (kind "#!/bin/sh" (list 10) "echo hi"))
    (is equal '("desktop launcher (a program to start)" :desktop) (kind "[Desktop Entry]" (list 10)))
    (is equal '("PEM openssh private key" :pem) (kind "-----BEGIN OPENSSH PRIVATE KEY-----"))
    (is equal '("UTF-16 text, little-endian" :utf-16) (kind (list #xFF #xFE 104 0 105 0)))
    ;; plain text, and what is not told, are nothing
    (is eq nil (first (kind "hello, world")))
    (is eq nil (first (kind (list 1 2 3 4))))
    ;; a name disagrees only with an extension its kind is not known by
    (true (tuatara.io:kind-name-disagrees-p :png "photo.jpg"))
    (false (tuatara.io:kind-name-disagrees-p :png "photo.PNG"))
    (false (tuatara.io:kind-name-disagrees-p :elf "program"))
    (false (tuatara.io:kind-name-disagrees-p :png ".hidden"))
    (false (tuatara.io:kind-name-disagrees-p :script "run.whatever")))
  ;; the preview says what a file is, over its hex dump too, and a name that disagrees in the warning colour
  (with-rimu-tree (dir)
    (alexandria:write-byte-vector-into-file (png-octets 640 480) (merge-pathnames "photo.jpg" dir))
    (multiple-value-bind (f editor) (rimu-session dir "g f p h o t o RET" :preview t :width 150)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "PNG image, 640 × 480, 8-bit RGBA  (named .jpg)"))
      (true (tuatara.frontend.fake:fake-frame-showing f "00000000  89 50 4e 47"))
      (let ((row (loop for row from 0 below (length (tuatara.frontend.fake:fake-screen f))
                       when (search "(named .jpg)" (tuatara.frontend.fake:fake-screen-line f row)) return row)))
        (true (find :warning (aref (tuatara.frontend.fake:fake-spans f) row)
                    :key (lambda (span) (let ((face (third span))) (if (listp face) (first face) face)))))))
    (delete-file (merge-pathnames "photo.jpg" dir))))

(defun pdf-octets (pages &key javascript attachment attachments)
  "A PDF of PAGES, one string of text each (an empty one for a page with no
text), in Helvetica; with JAVASCRIPT, a script run when it opens; with
ATTACHMENT, (NAME TEXT), a file of that name and text embedded in it, and
with ATTACHMENTS, a list of such, each of them."
  (let* ((attachments (if attachment (cons attachment attachments) attachments))
         (n (length pages))
         (font (+ 3 (* 2 n)))
         (spec (+ font (if javascript 2 1)))
         (objects (append (list (format nil "<< /Type /Catalog /Pages 2 0 R~@[ /OpenAction ~d 0 R~]~@[~a~] >>"
                                        (and javascript (1+ font))
                                        (and attachments
                                             (format nil " /Names << /EmbeddedFiles << /Names [~{(~a) ~d 0 R~^ ~}] >> >>"
                                                     (loop for (name) in attachments
                                                           for i from 0
                                                           append (list name (+ spec (* 2 i)))))))
                                (format nil "<< /Type /Pages /Kids [~{~d 0 R~^ ~}] /Count ~d >>"
                                        (loop for i below n collect (+ 3 (* 2 i))) n))
                          (loop for text in pages
                                for i from 0
                                for stream = (if (string= text "") "" (format nil "BT /F1 18 Tf 20 100 Td (~a) Tj ET" text))
                                append (list (format nil "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 300 200] /Contents ~d 0 R /Resources << /Font << /F1 ~d 0 R >> >> >>"
                                                     (+ 4 (* 2 i)) font)
                                             (format nil "<< /Length ~d >>~%stream~%~a~%endstream" (length stream) stream)))
                          (list "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
                          (and javascript (list "<< /Type /Action /S /JavaScript /JS (app.alert\\(1\\)) >>"))
                          (loop for (name text) in attachments
                                for i from 0
                                append (list (format nil "<< /Type /Filespec /F (~a) /EF << /F ~d 0 R >> >>" name (+ spec (* 2 i) 1))
                                             (format nil "<< /Type /EmbeddedFile /Length ~d >>~%stream~%~a~%endstream"
                                                     (length text) text)))))
         (offsets '())
         (text (with-output-to-string (out)
                 (format out "%PDF-1.4~%")
                 (loop for object in objects
                       for i from 1
                       do (push (file-position out) offsets)
                          (format out "~d 0 obj~%~a~%endobj~%" i object))
                 (let ((xref (file-position out)))
                   (format out "xref~%0 ~d~%0000000000 65535 f ~%" (1+ (length objects)))
                   (dolist (offset (reverse offsets)) (format out "~10,'0d 00000 n ~%" offset))
                   (format out "trailer~%<< /Size ~d /Root 1 0 R >>~%startxref~%~d~%%%EOF~%" (1+ (length objects)) xref)))))
    (map '(vector (unsigned-byte 8)) #'char-code text)))

(defun pdf-preview (dir name octets &rest settings)
  "Write OCTETS as the file NAME in DIR and preview it with SETTINGS, pairs of
a special variable and its value bound around the session: the frontend."
  (alexandria:write-byte-vector-into-file octets (merge-pathnames name dir) :if-exists :supersede)
  (progv (loop for (variable) on settings by #'cddr collect variable)
      (loop for (nil value) on settings by #'cddr collect value)
    (values (rimu-session dir (format nil "g f ~a RET" (rimu-typed name)) :preview t :width 250))))

(defun streamed-zip-octets (entries)
  "A zip archive's first octets holding ENTRIES, each (NAME . STRING), as a
program writing as it goes makes them: each entry's sizes after its data."
  (flet ((le (n count) (loop for i below count collect (ldb (byte 8 (* 8 i)) n))))
    (coerce (loop for (name . text) in entries
                  for data = (map 'list #'char-code text)
                  append (append '(80 75 3 4) (le 20 2) (le 8 2) (le 0 2) (le 0 4) (le 0 4) (le 0 4) (le 0 4)
                                 (le (length name) 2) (le 0 2) (map 'list #'char-code name) data
                                 '(80 75 7 8) (le 0 4) (le (length data) 4) (le (length data) 4)))
            '(vector (unsigned-byte 8)))))

(define-test (rimu zip-kinds-by-entry-names)
  ;; a zip archive is told by the names of its entries, never by words in names or contents
  (flet ((kind (entries) (nth-value 1 (tuatara.io:octets-kind (tuatara.io:zip-octets entries)))))
    (is eq :zip (kind '(("password/x.txt" . "secret") ("keyword/crossword/y.txt" . "ppt/ xl/ word/"))))
    (is eq :zip (kind '(("notes.txt" . "AndroidManifest.xml META-INF/MANIFEST.MF [Content_Types].xml"))))
    (is eq :zip (kind '(("[Content_Types].xml" . "") ("password/x.txt" . ""))))
    (is eq :docx (kind '(("[Content_Types].xml" . "") ("_rels/.rels" . "") ("word/document.xml" . "<w:document/>"))))
    (is eq :docx (kind '(("[Content_Types].xml" . "") ("word/media/image1.png" . "") ("word/document.xml" . ""))))
    (is eq :xlsx (kind '(("[Content_Types].xml" . "") ("xl/workbook.xml" . ""))))
    (is eq :pptx (kind '(("[Content_Types].xml" . "") ("ppt/presentation.xml" . ""))))
    (is eq :apk (kind '(("AndroidManifest.xml" . "") ("classes.dex" . "") ("META-INF/MANIFEST.MF" . ""))))
    (is eq :jar (kind '(("META-INF/MANIFEST.MF" . "Manifest-Version: 1.0") ("a/B.class" . ""))))
    (is eq :zip (kind '(("res/AndroidManifest.xml" . ""))))
    ;; entries whose sizes come after their data are followed all the same
    (is eq :docx (nth-value 1 (tuatara.io:octets-kind
                               (streamed-zip-octets '(("[Content_Types].xml" . "<Types/>") ("word/document.xml" . "<w:document/>"))))))
    (is eq :zip (nth-value 1 (tuatara.io:octets-kind (streamed-zip-octets '(("password/x.txt" . "word/document.xml"))))))))

(define-test (rimu kinds-of-short-files)
  ;; each mark followed by any octets, cut short anywhere: told or not, never an error
  (let ((marks (list (list 0 #x89 80 78 71 13 10 26 10) (list 0 #xFF #xD8 #xFF) (list 0 "GIF89a") (list 0 "RIFF" "????" "WEBPVP8X")
                     (list 0 "RIFF" "????" "WEBPVP8L") (list 0 "RIFF" "????" "WEBPVP8 ") (list 0 "BM") (list 0 73 73 42 0)
                     (list 0 77 77 0 42) (list 0 0 0 1 0) (list 0 "8BPS") (list 0 "gimp xcf") (list 0 "%PDF-")
                     (list 0 80 75 3 4) (list 0 #x1F #x8B 8 8) (list 0 #x1F #x8B) (list 0 "BZh9") (list 257 "ustar")
                     (list 0 "070701") (list 0 #x7F 69 76 70) (list 0 #x7F 69 76 70 2 1) (list 0 "MZ")
                     (list 0 #xCA #xFE #xBA #xBE) (list 4 "ftyp3") (list 4 "ftyp") (list 4 "ftypqt") (list 0 #x1A #x45 #xDF #xA3)
                     (list 0 "RIFF" "????" "AVI ") (list 0 "RIFF" "????" "WAVE") (list 0 "FORM" "????" "AIFF") (list 0 "ID3")
                     (list 0 "OggS") (list 0 "fLaC") (list 0 "MThd") (list 0 #xD4 #xC3 #xB2 #xA1) (list 0 76 0 0 0 1 20 2 0)
                     (list 0 "regf") (list 0 "MDMP") (list 0 0 1 0 0 0) (list 0 "OTTO") (list 0 "wOFF") (list 0 #xFF #xFE)
                     (list 0 #xEF #xBB #xBF) (list 0 "<svg") (list 0 "<?xml") (list 0 "#!") (list 0 "{\rtf")))
        (failures '()))
    (dolist (mark marks)
      (let ((start (apply #'kind-octets (make-list (first mark) :initial-element 0)
                                  (mapcar (lambda (part) (if (numberp part) (list part) part)) (rest mark)))))
        (dolist (fill '(0 #xFF 7 random))
          (loop for extra from 0 to 48
                for octets = (concatenate '(vector (unsigned-byte 8)) start
                                          (loop repeat extra collect (if (eq fill 'random) (random 256) fill)))
                do (handler-case (tuatara.io:octets-kind octets)
                     (error (e) (pushnew (list (rest mark) (type-of e)) failures :test (function equal))))))))
    (is equal '() (reverse failures))))

(define-test (rimu pdf-text)
  (with-rimu-tree (dir)
    ;; its text, taken out in the sandbox, each page after the first marked; its pages over it
    (let ((f (pdf-preview dir "report.pdf" (pdf-octets '("Hello from page one" "And page two")))))
      (true (tuatara.frontend.fake:fake-frame-showing f "PDF document, version 1.4, 2 pages"))
      (true (tuatara.frontend.fake:fake-frame-showing f "Hello from page one"))
      (true (tuatara.frontend.fake:fake-frame-showing f "── Page 2 ──"))
      (true (tuatara.frontend.fake:fake-frame-showing f "And page two"))
      (true (tuatara.frontend.fake:fake-frame-showing f "report.pdf (preview, text)")))
    ;; a PDF by another name is still one, the name marked
    (let ((f (pdf-preview dir "notes.txt" (pdf-octets '("Hidden in a text file")))))
      (true (tuatara.frontend.fake:fake-frame-showing f "PDF document, version 1.4, 1 page  (named .txt)"))
      (true (tuatara.frontend.fake:fake-frame-showing f "Hidden in a text file")))
    ;; JavaScript in it, in the warning colour
    (let* ((f (pdf-preview dir "script.pdf" (pdf-octets '("Open me") :javascript t)))
           (row (loop for row from 0 below (length (tuatara.frontend.fake:fake-screen f))
                      when (search ", with JavaScript" (tuatara.frontend.fake:fake-screen-line f row)) return row)))
      (true row)
      (true (find :warning (aref (tuatara.frontend.fake:fake-spans f) row)
                  :key (lambda (span) (let ((face (third span))) (if (listp face) (first face) face))))))
    ;; pages with no text: the file as it is, and why
    (let ((f (pdf-preview dir "scan.pdf" (pdf-octets '("")))))
      (true (tuatara.frontend.fake:fake-frame-showing f "1 page, no text in it (pages of pictures?)"))
      (true (tuatara.frontend.fake:fake-frame-showing f "%PDF-1.4")))
    ;; not a PDF inside: the program's words
    (let ((f (pdf-preview dir "broken.pdf" (map '(vector (unsigned-byte 8)) #'char-code
                                                (format nil "%PDF-1.4~%nothing of a PDF here~%")))))
      (true (or (tuatara.frontend.fake:fake-frame-showing f "not decoded: pdftotext failed")
                (tuatara.frontend.fake:fake-frame-showing f "no text in it")))
      (true (tuatara.frontend.fake:fake-frame-showing f "%PDF-1.4")))
    ;; no program for it, and decoding turned off: the octets, and why
    (let ((f (pdf-preview dir "report.pdf" (pdf-octets '("Hello"))
                          'tuatara.defaults::*pdf-decoders* '((:name "nopdf" :text ("no-such-pdftotext" "fd://0" "-"))))))
      (true (tuatara.frontend.fake:fake-frame-showing f "not decoded: no program for its text (install nopdf: no-such-pdftotext)"))
      (true (tuatara.frontend.fake:fake-frame-showing f "%PDF-1.4")))
    (let ((f (pdf-preview dir "report.pdf" (pdf-octets '("Hello")) 'tuatara-user:*sandbox-program* nil)))
      (true (tuatara.frontend.fake:fake-frame-showing f "not decoded: decoding is off (*sandbox-program*)"))
      (false (tuatara.frontend.fake:fake-frame-showing f "(preview, text)")))
    ;; a PDF with octets of its own, as most have, not decoded: in the hex view
    (let ((f (pdf-preview dir "binary.pdf" (concatenate '(vector (unsigned-byte 8)) (pdf-octets '("Hello")) #(0 1 2 3))
                          'tuatara-user:*sandbox-program* nil)))
      (true (tuatara.frontend.fake:fake-frame-showing f "00000000  25 50 44 46")))
    (tuatara.io:sandbox-status :again t)
    (dolist (name '("report.pdf" "notes.txt" "script.pdf" "scan.pdf" "broken.pdf" "binary.pdf"))
      (delete-file (merge-pathnames name dir)))))

(define-test (rimu pdf-decoding-stopped)
  ;; moving on while a PDF is decoded stops its program, and the next entry is shown
  (with-rimu-tree (dir)
    (alexandria:write-byte-vector-into-file (pdf-octets '("Slow")) (merge-pathnames "slow.pdf" dir))
    (rimu-forget)
    (let* ((mark (format nil "30.~d" (+ 100000 (random 900000))))
           (tuatara.defaults::*pdf-decoders* `((:name "slow" :text ("sleep" ,mark))))
          (tuatara-user:*sandbox-seconds* 60)
          (tuatara-user:*rimu-preview* t)
          (tuatara-user:*rimu-preview-delay* 0)
          (tuatara-user:*rimu-details* nil)
          (tuatara-user:*rimu-dotfiles* nil)
          (tuatara-user:*rimu-sort* :name)
          (f (tuatara.frontend.fake:make-fake-frontend :width 150 :height 20))
          (started nil)
          (start (get-internal-real-time)))
      (tuatara.frontend.fake:fake-queue-keys
       f (format nil "C-x d C-a C-k ~a C-j g f s l o w . p d f RET" (rimu-typed (namestring dir))))
      ;; once the slow program runs, move on: checked again and again, the editor going on between
      (labels ((check ()
                 (cond ((plusp (processes-running mark))
                        (setf started t)
                        (tuatara.frontend.fake:fake-queue-keys f "k"))
                       ((< (- (get-internal-real-time) start) (* 10 internal-time-units-per-second))
                        (sleep 0.02)
                        (tuatara.frontend.fake:fake-queue-action f #'check)))))
        (tuatara.frontend.fake:fake-queue-action f #'check))
      (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults)
      (true started)
      (true (< (- (get-internal-real-time) start) (* 15 internal-time-units-per-second)))
      (true (loop repeat 100 thereis (zerop (processes-running mark)) do (sleep 0.05))))
    (delete-file (merge-pathnames "slow.pdf" dir))))

(defparameter *tiny-png*
  (coerce '(137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0 2 0 0 0 2 2 3 0 0 0 15 216 229 183 0 0 0 12 80 76
            84 69 255 0 0 0 255 0 0 0 255 255 255 255 251 0 96 246 0 0 0 12 73 68 65 84 8 215 99 16 96 216 0 0 0 228 0
            193 246 139 247 8 0 0 0 0 73 69 78 68 174 66 96 130)
          '(vector (unsigned-byte 8)))
  "A PNG image of 2 by 2 pixels: red, green, then blue, white.")

(define-test (rimu pictures-read-back)
  (flet ((octets (&rest parts) (apply #'kind-octets parts))
         (pixels (image) (coerce (tuatara.frontend:image-pixels image) 'list)))
    ;; a PAM image with alpha, and a PPM one without
    (let ((image (tuatara.defaults::decoded-image
                  (octets (format nil "P7~%WIDTH 2~%HEIGHT 1~%DEPTH 4~%MAXVAL 255~%TUPLTYPE RGB_ALPHA~%ENDHDR~%")
                          (list 255 0 0 255 0 255 0 128)))))
      (is = 2 (tuatara.frontend:image-width image))
      (is = 1 (tuatara.frontend:image-height image))
      (is = 4 (tuatara.frontend:image-channels image))
      (is equal '(255 0 0 255 0 255 0 128) (pixels image)))
    (let ((image (tuatara.defaults::decoded-image (octets (format nil "P6~%1 2~%255~%") (list 1 2 3 4 5 6)))))
      (is = 3 (tuatara.frontend:image-channels image))
      (is equal '(1 2 3 4 5 6) (pixels image)))
    ;; what says more pixels than it has, or pixels of another kind, is no picture
    (false (tuatara.defaults::decoded-image (octets (format nil "P6~%100 100~%255~%") (list 1 2 3))))
    (false (tuatara.defaults::decoded-image (octets (format nil "P7~%WIDTH 1~%HEIGHT 1~%DEPTH 4~%MAXVAL 65535~%ENDHDR~%")
                                                    (make-list 8 :initial-element 0))))
    ;; grey and alpha (a black-and-white photo) made the three colours alike, the alpha kept
    (let ((grey (tuatara.defaults::decoded-image (octets (format nil "P7~%WIDTH 1~%HEIGHT 1~%DEPTH 2~%MAXVAL 255~%ENDHDR~%") (list 90 200)))))
      (true grey)
      (is equal '(90 90 90 200) (coerce (tuatara.frontend:image-pixels grey) 'list)))
    (false (tuatara.defaults::decoded-image (octets (format nil "P7~%WIDTH 99999~%HEIGHT 1~%DEPTH 3~%MAXVAL 255~%ENDHDR~%"))))
    (false (tuatara.defaults::decoded-image (octets "GIF89a")))))

(define-test (rimu pictures-in-kitty)
  ;; base 64, as the terminal reads it
  (flet ((b64 (text) (let ((o (map '(simple-array (unsigned-byte 8) (*)) #'char-code text)))
                       (tuatara.io:base64-string o))))
    (is string= "TWFu" (b64 "Man"))
    (is string= "TWE=" (b64 "Ma"))
    (is string= "TQ==" (b64 "M"))
    (is string= "" (b64 "")))
  ;; and read back as written, every octet of every length
  (dotimes (length 7)
    (let ((o (make-array length :element-type '(unsigned-byte 8) :initial-contents (loop for k below length collect (- 255 (* 37 k))))))
      (is equalp o (tuatara.io:base64-octets (map '(vector (unsigned-byte 8)) #'char-code (tuatara.io:base64-string o))))))
  (is string= "YW4" (subseq (tuatara.io:base64-string (map '(vector (unsigned-byte 8)) #'char-code "xan") :start 1) 0 3))
  ;; a picture sent: its size and kind, in pieces of 4096 digits, the terminal to answer nothing
  (let* ((image (tuatara.frontend:make-image 40 40 4 (make-array 6400 :element-type '(unsigned-byte 8) :initial-element 7)))
         (sent (with-output-to-string (*standard-output*) (tuatara.frontend.tui::send-image-written image 7)))
         (escape (string #\Escape)))
    (true (uiop:string-prefix-p (format nil "~a_Ga=t,f=32,s=40,v=40,i=7,q=2,m=1;" escape) sent))
    (is = 3 (loop with mark = (format nil "~a_G" escape)
                  for at = (search mark sent) then (search mark sent :start2 (1+ at))
                  while at count t))
    (true (search (format nil "~a_Gm=0;" escape) sent)))
  ;; a terminal that did not say it reads shared memory: written to it
  (let ((tuatara.frontend.tui::*terminal-answers* '(:pictures t :shared nil))
        (image (tuatara.frontend:make-image 2 2 3 (make-array 12 :element-type '(unsigned-byte 8) :initial-element 7))))
    (true (uiop:string-prefix-p (format nil "~c_Ga=t,f=24,s=2,v=2,i=9,q=2,m=0;" #\Escape)
                                (with-output-to-string (*standard-output*) (tuatara.frontend.tui::send-image image 9)))))
  ;; one that said it does: through shared memory, its name alone sent, the pixels in /dev/shm for the terminal
  (let* ((image (tuatara.frontend:make-image 40 40 4 (make-array 6400 :element-type '(unsigned-byte 8) :initial-element 7)))
         (sent (let ((tuatara.frontend.tui::*terminal-answers* '(:pictures t :shared t)))
                 (with-output-to-string (*standard-output*) (tuatara.frontend.tui::send-image image 8))))
         (escape (string #\Escape))
         (name (first tuatara.frontend.tui::*shared-pictures*)))
    (true (uiop:string-prefix-p (format nil "~a_Ga=t,t=s,f=32,s=40,v=40,i=8,S=6400,q=2;" escape) sent))
    (is = 1 (count #\; sent))
    (is equalp (make-array 6400 :element-type '(unsigned-byte 8) :initial-element 7)
        (alexandria:read-file-into-byte-vector (concatenate 'string "/dev/shm/" name)))
    (is = #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (concatenate 'string "/dev/shm/" name)))))
    ;; not read by a terminal: removed when it is let go of
    (tuatara.frontend.tui::forget-shared-pictures)
    (false (probe-file (concatenate 'string "/dev/shm/" name))))
  ;; how many cells it takes: no larger than it is, its proportions kept
  (let ((image (tuatara.frontend:make-image 200 100 3 nil)))
    (is equal '(20 5) (multiple-value-list (tuatara.frontend.tui::image-cells image 60 30 10 20)))
    (is equal '(10 3) (multiple-value-list (tuatara.frontend.tui::image-cells image 10 30 10 20))))
  ;; a row of its cells: the first marked with its row and column 0
  (let ((text (tuatara.frontend.tui::image-row-text 2 3)))
    (is = 5 (length text))
    (is equal (list #x10EEEE #x030E #x0305 #x10EEEE #x10EEEE) (map 'list #'char-code text)))
  ;; numbers not given again for 4,096 pictures: the colour 1 to 16 only (a colour pair each, never used up),
  ;; the highest octet marked too
  (is = 1 (tuatara.frontend.tui::kitty-image-id 0))
  (is = 16 (tuatara.frontend.tui::kitty-image-id 15))
  (is = (logior (ash 1 24) 1) (tuatara.frontend.tui::kitty-image-id 16))
  (is = 4096 (length (remove-duplicates (loop for i below 4096 collect (tuatara.frontend.tui::kitty-image-id i)))))
  (true (loop for i below 10000 always (<= 1 (ldb (byte 8 0) (tuatara.frontend.tui::kitty-image-id i)) 16)))
  (is = 1 (tuatara.frontend.tui::kitty-image-id 4096))
  ;; a completion list over a picture's cells: the picture left out while it is there
  (let ((box (list 5 10 (list (list "abcdef" nil) (list "gh" nil)))))
    (true (tuatara.command::box-over-area-p box 0 0 20 10))
    (true (tuatara.command::box-over-area-p box 6 15 5 5))
    (false (tuatara.command::box-over-area-p box 7 0 40 10))
    (false (tuatara.command::box-over-area-p box 0 16 10 10))
    (false (tuatara.command::box-over-area-p box 0 0 10 5)))
  ;; pictures where the terminal says it draws kitty's (kitty, Ghostty), whatever its name;
  ;; none where it answers only its device attributes (tmux, gnome-terminal)
  (flet ((answers (&rest replies)
           (tuatara.frontend.tui::terminal-answers-in
            (apply #'concatenate 'string (mapcar (lambda (reply) (format nil reply #\Escape #\Escape)) replies)))))
    (is equal '(:keyboard t :pictures t :shared t)
        (answers "~c_Gi=31;OK~c\\" "~c_Gi=32;OK~c\\" "~c[?0u" "~c[?62;22c"))
    (is equal '(:keyboard nil :pictures t :shared nil)
        (answers "~c_Gi=31;OK~c\\" "~c_Gi=32;EBADF:shared memory not readable~c\\" "~c[?62;c"))
    (is equal '(:keyboard nil :pictures nil :shared nil) (answers "~c[?65;1;9c")))
  (let ((program (uiop:getenv "TERM_PROGRAM")) (konsole (uiop:getenv "KONSOLE_VERSION"))
        (tuatara.frontend.tui::*terminal-answers* '(:pictures t)))
    (unwind-protect
         (progn (sb-posix:unsetenv "TERM_PROGRAM") (sb-posix:unsetenv "KONSOLE_VERSION")
                (true (tuatara.frontend.tui::terminal-pictures-p))
                (true (tuatara.frontend.tui::pictures-in-text-p))
                ;; WezTerm draws kitty's pictures, but not in place of the characters standing for them:
                ;; they are put at their cells instead
                (sb-posix:setenv "TERM_PROGRAM" "WezTerm" 1)
                (true (tuatara.frontend.tui::terminal-pictures-p))
                (false (tuatara.frontend.tui::pictures-in-text-p))
                (sb-posix:unsetenv "TERM_PROGRAM")
                (sb-posix:setenv "KONSOLE_VERSION" "250401" 1)
                (false (tuatara.frontend.tui::pictures-in-text-p))
                (sb-posix:unsetenv "KONSOLE_VERSION")
                (let ((tuatara.frontend.tui::*terminal-answers* '(:keyboard t)))
                  (false (tuatara.frontend.tui::terminal-pictures-p))))
      (if program (sb-posix:setenv "TERM_PROGRAM" program 1) (sb-posix:unsetenv "TERM_PROGRAM"))
      (if konsole (sb-posix:setenv "KONSOLE_VERSION" konsole 1) (sb-posix:unsetenv "KONSOLE_VERSION"))))
  ;; put at its cells: its proportions kept, the side that decides its size given, none at its own size;
  ;; a part of one at its own size by its pixels
  (let ((image (tuatara.frontend:make-image 400 100 3 (make-array 120000 :element-type '(unsigned-byte 8)))))
    (is string= "c=20" (tuatara.frontend.tui::placement-size image 20 3 nil 0 0 3 20 10 20))
    (is string= "r=2" (tuatara.frontend.tui::placement-size image 20 2 nil 0 0 2 20 20 20))
    (is string= "" (tuatara.frontend.tui::placement-size image 40 5 nil 0 0 5 40 10 20))
    (is string= "x=100,y=40,w=200,h=60" (tuatara.frontend.tui::placement-size image 40 5 '(2 10) 2 10 3 20 10 20)))
  ;; placed after the frame, the cursor kept; one no longer shown taken away
  (let ((f (tuatara.frontend.tui::make-tui-frontend)))
    (setf (tuatara.frontend.tui::tui-placed f) (list (list 7 1 2 3 "c=20" 3) (list 9 1 10 0 "" 2))
          (tuatara.frontend.tui::tui-placing f) (list (list 7 1 4 3 "c=20" 3)))
    (let ((sent (with-output-to-string (*standard-output*) (tuatara.frontend.tui::place-pictures f))))
      (true (search (format nil "~c_Ga=d,d=i,i=9,p=1,q=2;~c\\" #\Escape #\Escape) sent))
      (false (search "i=7,p=1,q=2" sent))
      (true (search (format nil "~c7~c[5;4H~c_Ga=p,i=7,p=1,C=1,q=2,c=20;~c\\~c8" #\Escape #\Escape #\Escape #\Escape #\Escape) sent)))
    (is equal (list (list 7 1 4 3 "c=20" 3)) (tuatara.frontend.tui::tui-placed f))
    (is eq nil (tuatara.frontend.tui::tui-placing f)))
  (let ((text (tuatara.frontend.tui::image-row-text 0 2 0 (logior (ash 3 24) 7))))
    (is equal (list #x10EEEE #x0305 #x0305 #x0310 #x10EEEE) (map 'list #'char-code text))))

(define-test (rimu picture-preview)
  (with-rimu-tree (dir)
    (alexandria:write-byte-vector-into-file *tiny-png* (merge-pathnames "tiny.png" dir) :if-exists :supersede)
    ;; where the frontend shows pictures: the picture, over the text of the preview's window
    (multiple-value-bind (f editor) (rimu-session dir "g f t i n y . p n g RET" :preview t :width 160 :cell-pixels '(10 20))
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "PNG image, 2 × 2"))
      (true (tuatara.frontend.fake:fake-frame-showing f "tiny.png (preview, image)"))
      (true (tuatara.frontend.fake:fake-frame-showing f "Sandboxed"))
      (let ((put (first (tuatara.frontend.fake:fake-images f))))
        (true put)
        (destructuring-bind (image row column columns rows &optional offset) put
          (declare (ignore offset))
          (is = 2 (tuatara.frontend:image-width image))
          (is = 2 (tuatara.frontend:image-height image))
          ;; red, green, blue, white, whatever the channels
          (let ((pixels (tuatara.frontend:image-pixels image)) (n (tuatara.frontend:image-channels image)))
            (is equal '(255 0 0) (coerce (subseq pixels 0 3) 'list))
            (is equal '(0 255 0) (coerce (subseq pixels n (+ n 3)) 'list))
            (is equal '(255 255 255) (coerce (subseq pixels (* 3 n) (+ (* 3 n) 3)) 'list)))
          ;; the preview's window, beside the list, its header line over it
          (is = 1 row)
          (true (> column 70))
          (true (> columns 50))
          (true (> rows 10))
          ;; the octets are not painted under it
          (false (search "89 50 4e 47" (tuatara.frontend.fake:fake-screen-line f (+ row 1)))))))
    ;; M-x hex-mode in the preview: its octets in place of the picture, and again the picture
    (multiple-value-bind (f editor)
        (rimu-session-then dir "g f t i n y . p n g RET" '("C-x o" "M-x h e x - m o d e RET") :cell-pixels '(10 20))
      (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
        (true (tuatara.buffer:buffer-local buffer :image-hidden))
        (true (tuatara.display:hex-view-p buffer))
        (true (tuatara.frontend.fake:fake-frame-showing f "Hex view on: the file's octets in place of the picture"))
        (false (tuatara.frontend.fake:fake-images f))))
    (multiple-value-bind (f editor)
        (rimu-session-then dir "g f t i n y . p n g RET" '("C-x o" "M-x h e x - m o d e RET" "M-x h e x - m o d e RET") :cell-pixels '(10 20))
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "The picture again"))
      (true (tuatara.frontend.fake:fake-images f)))
    ;; an SVG drawn in the sandbox, its text under the picture
    (alexandria:write-string-into-file
     "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"120\" height=\"80\"><rect width=\"120\" height=\"80\" fill=\"#2c5484\"/></svg>"
     (merge-pathnames "shape.svg" dir) :if-exists :supersede)
    (multiple-value-bind (f editor) (rimu-session dir "g f s h a p e . s v g RET" :preview t :width 160 :cell-pixels '(10 20))
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "SVG image"))
      (let ((put (first (tuatara.frontend.fake:fake-images f))))
        (true put)
        (when put
          (is = 120 (tuatara.frontend:image-width (first put)))
          (is = 80 (tuatara.frontend:image-height (first put)))
          (is equal '(#x2c #x54 #x84) (coerce (subseq (tuatara.frontend:image-pixels (first put)) 0 3) 'list)))))
    (delete-file (merge-pathnames "shape.svg" dir))
    ;; where it shows none, the octets, and no picture made
    (let ((f (rimu-session dir "g f t i n y . p n g RET" :preview t :width 160)))
      (true (tuatara.frontend.fake:fake-frame-showing f "00000000  89 50 4e 47"))
      (false (tuatara.frontend.fake:fake-frame-showing f "(preview, image)")))
    ;; no program for pictures: the octets, and why
    (let ((tuatara.defaults::*image-decoders* '((:name "NoMagick" :program ("no-such-magick" :input :size "PAM:-")))))
      (let ((f (rimu-session dir "g f t i n y . p n g RET" :preview t :width 280 :cell-pixels '(10 20))))
        (true (tuatara.frontend.fake:fake-frame-showing f "not shown: no program for pictures (install NoMagick: no-such-magick)"))
        (true (tuatara.frontend.fake:fake-frame-showing f "00000000  89 50 4e 47"))
        (false (tuatara.frontend.fake:fake-images f))))
    (delete-file (merge-pathnames "tiny.png" dir))))

(defvar *evaluated* nil
  "Set by text that must never be evaluated, when it is.")

(define-test (rimu pdf-page-picture)
  ;; the first page's size in pixels, from what pdfinfo says, fitted and turned
  (is equal '(612 792) (tuatara.defaults::page-pixels (format nil "Pages: 3~%Page size:      612 x 792 pts (letter)~%Page rot: 0~%") 1000 792))
  (is equal '(400 300) (tuatara.defaults::page-pixels (format nil "Page size: 300 x 400 pts~%Page rot: 90~%") 400 400))
  (false (tuatara.defaults::page-pixels "Pages: 3" 100 100))
  (is equal '(595 842) (tuatara.defaults::page-pixels (format nil "Page size: 595.276 x 841.89 pts (A4)~%") 1000 842))
  ;; what the decoder prints is the file's word, never read as Lisp
  (let ((tuatara.tests::*evaluated* nil))
    (false (tuatara.defaults::page-pixels (format nil "Page size: #.(setf tuatara.tests::*evaluated* t) x 1 pts~%") 100 100))
    (false tuatara.tests::*evaluated*))
  (loop for size in '("0 x 0 pts" "-3 x 4 pts" "1e999 x 2 pts" "1d-320 x 1d-320 pts" "x" "" "3 x" "1.2.3 x 4 pts" ". x . pts")
        do (false (tuatara.defaults::page-pixels (format nil "Page size: ~a~%" size) 100 100)))
  ;; a title with lines of its own in it cannot stand in for what the program found
  (let ((info (format nil "Title: x~%JavaScript: no~%Encrypted: no~%Pages: 1~%Page size: 1 x 1 pts~%Pages: 7~%Encrypted: yes~%JavaScript: yes~%Page size: 300 x 400 pts~%")))
    (is equal "7" (tuatara.defaults::info-field info "Pages"))
    (is equal "yes" (tuatara.defaults::info-field info "JavaScript"))
    (is equal '(300 400) (tuatara.defaults::page-pixels info 300 400)))
  (with-rimu-tree (dir)
    (alexandria:write-byte-vector-into-file (pdf-octets '("Hello from page one" "And page two")) (merge-pathnames "report.pdf" dir)
                                            :if-exists :supersede)
    (alexandria:write-byte-vector-into-file (pdf-octets '("Second file")) (merge-pathnames "second.pdf" dir)
                                            :if-exists :supersede)
    ;; where pictures show: the first page drawn, its proportions (300 by 200 points) kept
    (multiple-value-bind (f editor) (rimu-session dir "g f r e p o r t . p d f RET" :preview t :width 160 :cell-pixels '(10 20))
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "PDF document, version 1.4, 2 pages"))
      (let ((image (first (first (tuatara.frontend.fake:fake-images f)))))
        (true image)
        (is = 3/2 (/ (tuatara.frontend:image-width image) (tuatara.frontend:image-height image)))
        (true (> (tuatara.frontend:image-width image) 300)))
      (false (tuatara.frontend.fake:fake-frame-showing f "Hello from page one")))
    ;; T switches to the text, and the next file shows its text too; T again, pictures
    (multiple-value-bind (f editor) (rimu-session dir "g f r e p o r t . p d f RET T" :preview t :width 160 :cell-pixels '(10 20))
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "Preview: text"))
      (true (tuatara.frontend.fake:fake-frame-showing f "Hello from page one"))
      (false (tuatara.frontend.fake:fake-images f)))
    (multiple-value-bind (f editor) (rimu-session dir "g f r e p o r t . p d f RET T g f s e c o n d . p d f RET" :preview t :width 160 :cell-pixels '(10 20))
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "Second file"))
      (false (tuatara.frontend.fake:fake-images f)))
    (multiple-value-bind (f editor) (rimu-session dir "g f r e p o r t . p d f RET T T" :preview t :width 160 :cell-pixels '(10 20))
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "Preview: pictures"))
      (true (tuatara.frontend.fake:fake-images f)))
    (delete-file (merge-pathnames "report.pdf" dir))
    (delete-file (merge-pathnames "second.pdf" dir))))

(defun rimu-session-then (directory first then &key (width 160) (height 20) cell-pixels details)
  "Run an editor session that lists DIRECTORY with the preview on, types
FIRST, and THEN, a string of keys or a list of them, each once the preview and
any PDF's view have done their work in the background. Returns the frontend
and the editor."
  (rimu-forget)
  (let ((tuatara-user:*rimu-preview* t)
        (tuatara-user:*rimu-preview-delay* 0)
        (tuatara-user:*rimu-details* details)
        (tuatara-user:*rimu-dotfiles* nil)
        (tuatara-user:*rimu-sort* :name)
        (f (tuatara.frontend.fake:make-fake-frontend :width width :height height :cell-pixels cell-pixels))
        (start (get-internal-real-time)))
    (tuatara.frontend.fake:fake-queue-keys f (format nil "C-x d C-a C-k ~a C-j ~a" (rimu-typed (namestring directory)) first))
    (labels ((busy ()
               (let ((state tuatara.defaults::*rimu-preview-state*))
                 (or (null state) (tuatara.defaults::rimu-preview-job state) (null (tuatara.defaults::rimu-preview-buffer state))
                     (some (lambda (b) (let ((view (tuatara.defaults::pdf-view-of b)))
                                         (and view (or (tuatara.defaults::pdf-view-job view) (tuatara.defaults::pdf-view-text-job view)))))
                           tuatara.defaults::*pdf-views*))))
             (check ()
               (if (and (busy) (< (- (get-internal-real-time) start) (* 20 internal-time-units-per-second)))
                   (progn (sleep 0.02) (tuatara.frontend.fake:fake-queue-action f #'check))
                   (let ((turn (pop then)))
                     (when turn
                       (tuatara.frontend.fake:fake-queue-keys f turn)
                       (when then (tuatara.frontend.fake:fake-queue-action f #'check)))))))
      (unless (listp then) (setf then (list then)))
      (tuatara.frontend.fake:fake-queue-action f #'check))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
      (when scratch (tuatara.buffer:kill-buffer scratch)))
    (tuatara.buffer:make-buffer "*scratch*" :text "")
    (values f (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                            :buffer (tuatara.buffer:find-buffer "*scratch*")))))

(define-test (rimu pdf-preview-entered)
  ;; the cursor gone into a PDF's preview: all of it there as its pages; back in the list, its preview again
  (with-rimu-tree (dir)
    (alexandria:write-byte-vector-into-file (pdf-octets '("One" "Two" "Three")) (merge-pathnames "report.pdf" dir)
                                            :if-exists :supersede)
    (progn (tuatara.defaults::finish-state-writes) (uiop:delete-file-if-exists (tuatara.defaults::state-list-path tuatara.defaults::*pdf-places*)))
    (multiple-value-bind (f editor)
        (rimu-session-then dir "g f r e p o r t . p d f RET" '("C-x o" "l") :cell-pixels '(10 20))
      (true (tuatara.frontend.fake:fake-frame-showing f "report.pdf, all of it"))
      (true (tuatara.frontend.fake:fake-frame-showing f "Page 2 of 3"))
      (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
        (true (tuatara.defaults::pdf-view-of buffer))
        ;; its C-s still its own, under the quick edit's keys
        (is eq 'tuatara.defaults::pdf-view-search
            (tuatara.command:lookup-key (tuatara.buffer:buffer-local buffer :override-keymap)
                                        (tuatara.frontend:parse-key-sequence "C-s")))))
    (progn (tuatara.defaults::finish-state-writes) (uiop:delete-file-if-exists (tuatara.defaults::state-list-path tuatara.defaults::*pdf-places*)))
    (multiple-value-bind (f editor)
        (rimu-session-then dir "g f r e p o r t . p d f RET" '("C-x o" "l" "C-x o") :cell-pixels '(10 20))
      (true (tuatara.frontend.fake:fake-frame-showing f "report.pdf (preview"))
      (true (tuatara.defaults::rimu-list-p (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
    ;; no sandbox: the preview stays a preview, the file never opened for the cursor going in
    (let ((tuatara.io::*sandbox-program* "no-such-sandbox-program"))
      (multiple-value-bind (f editor)
          (rimu-session-then dir "g f r e p o r t . p d f RET" '("C-x o") :cell-pixels '(10 20))
        (declare (ignore f))
        (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
          (false (tuatara.defaults::pdf-view-of buffer))
          (false (tuatara.buffer:buffer-file buffer)))))
    (dolist (b (copy-list tuatara.defaults::*pdf-views*))
      (when (tuatara.buffer:live-buffer-p b) (tuatara.buffer:kill-buffer b)))
    (progn (tuatara.defaults::finish-state-writes) (uiop:delete-file-if-exists (tuatara.defaults::state-list-path tuatara.defaults::*pdf-places*)))
    (delete-file (merge-pathnames "report.pdf" dir))))

(define-test (rimu columns)
  (let ((columns tuatara.defaults::*rimu-columns*))
    (unwind-protect
         (with-rimu-tree (dir)
           ;; a column after the names, lined up after the longest
           (tuatara.defaults::set-rimu-column :test-note (tuatara.defaults::rimu-cells-by-entry
                                                          (lambda (entry) (format nil "note-~a" (tuatara.io:listed-name entry))))
                                              :place :after :shown (constantly t))
           (multiple-value-bind (f editor) (rimu-session dir "")
             (declare (ignore f))
             (let ((lines (rimu-lines editor)))
               (true (find "  text.txt" lines :test (lambda (start line) (uiop:string-prefix-p start line))))
               (is = (search "note-text.txt" (find-if (lambda (l) (search "note-text.txt" l)) lines))
                   (search "note-link-to-dir" (find-if (lambda (l) (search "note-link-to-dir" l)) lines)))))
           ;; one found in the background shows once it is given
           (tuatara.defaults::set-rimu-column :test-later
                                              (lambda (directory entries)
                                                (declare (ignore directory))
                                                (sb-thread:make-thread
                                                 (lambda ()
                                                   (sleep 0.1)
                                                   (tuatara.defaults::rimu-column-ready
                                                    :test-later entries
                                                    (map 'simple-vector (lambda (entry) (format nil "later-~a" (tuatara.io:listed-name entry)))
                                                         entries))))
                                                :pending)
                                              :place :after :shown (constantly t))
           (multiple-value-bind (f editor) (rimu-session dir "")
             (declare (ignore f))
             (true (find-if (lambda (line) (and (search "note-text.txt" line) (uiop:string-suffix-p line "later-text.txt")))
                            (rimu-lines editor)))))
      (setf tuatara.defaults::*rimu-columns* columns))))

(define-test (rimu reading-in-the-background)
  (let ((columns tuatara.defaults::*rimu-columns*))
    (unwind-protect
         (with-rimu-tree (dir)
           ;; a directory that takes a while says so, then fills in
           (tuatara.defaults::set-rimu-column :test-slow (lambda (directory entries)
                                                           (declare (ignore directory))
                                                           (sleep 0.3)
                                                           (make-array (length entries) :initial-element nil))
                                              :shown (constantly t))
           (let ((tuatara.defaults::*rimu-read-wait* 0))
             (multiple-value-bind (f editor) (rimu-session dir "")
               (true (tuatara.frontend.fake:fake-frame-showing f "  Reading..."))
               (true (find "text.txt" (rimu-lines editor) :test (lambda (name line) (uiop:string-suffix-p line name)))))))
      (setf tuatara.defaults::*rimu-columns* columns))))

(define-test (rimu texts)
  ;; sizes as ls -h writes them, every figure rounded up
  (is equal '("0" "1023" "1.0K" "4.1K" "10K" "24K" "1.0M" "1.5M" "12G")
      (mapcar (lambda (n) (tuatara.io:octets-words n :compact t))
              (list 0 1023 1024 4097 10239 24576 1048575 1572864 (* 12 1024 1024 1024))))
  ;; the room left on a disk in units of 1,024
  (is string= "954.0 MiB" (tuatara.io:octets-words (* 954 1024 1024) :binary t))
  (is string= "1.2 TiB" (tuatara.io:octets-words (round (* 1.2 (expt 1024 4))) :binary t))
  (is string= "12.3 MB" (tuatara.io:octets-words 12300000))
  ;; the time within half a year, else the year
  (let* ((now (encode-universal-time 0 0 12 30 9 2026))
         (nanoseconds (lambda (universal) (* (- universal tuatara.dates:+unix-epoch+) 1000000000))))
    (is string= "Sep 29 12:00" (tuatara.defaults::rimu-time-text (funcall nanoseconds (encode-universal-time 0 0 12 29 9 2026)) now))
    (is string= "Sep  3  2025" (tuatara.defaults::rimu-time-text (funcall nanoseconds (encode-universal-time 0 0 12 3 9 2025)) now))
    (is string= "Oct  1  2026" (tuatara.defaults::rimu-time-text (funcall nanoseconds (encode-universal-time 0 0 12 1 10 2026)) now)))
  ;; names octet by octet: capitals first, a name before those it starts
  (flet ((octets (text) (tuatara.text:string-octets text)))
    (true (tuatara.defaults::rimu-octets< (octets "Zebra") (octets "apple")))
    (true (tuatara.defaults::rimu-octets< (octets "ab") (octets "abc")))
    (false (tuatara.defaults::rimu-octets< (octets "abc") (octets "ab")))
    (false (tuatara.defaults::rimu-octets< (octets "ab") (octets "ab"))))
  ;; a directory's name kept absolute, . and .. taken away
  (is string= "/a/c/" (tuatara.io:directory-name "/a/b/../c/./" :clean t))
  (is string= "/" (tuatara.io:directory-name "/a/.." :clean t))
  (is string= "c/" (tuatara.defaults::rimu-buffer-name "/a/b/c/"))
  (is string= "/" (tuatara.defaults::rimu-buffer-name "/"))
  (is string= "/a/b" (tuatara.defaults::rimu-header-name "/a/b/")))

(define-test (rimu reader)
  (with-rimu-tree (dir)
    (let* ((entries (tuatara.io:list-directory (namestring dir)))
           (by-name (lambda (name) (find name entries :key #'tuatara.io:listed-name :test #'string=))))
      ;; every entry, . and .. too, examined without being opened
      (true (funcall by-name "."))
      (true (funcall by-name ".."))
      (is eq :fifo (tuatara.io:stamp-kind (tuatara.io:listed-stamp (funcall by-name "pipe"))))
      (is eq :symlink (tuatara.io:stamp-kind (tuatara.io:listed-stamp (funcall by-name "link-to-dir"))))
      (is eq :directory (tuatara.io:stamp-kind (tuatara.io:listed-target-stamp (funcall by-name "link-to-dir"))))
      (is string= "sub" (tuatara.io:listed-target (funcall by-name "link-to-dir")))
      (is eq nil (tuatara.io:listed-target-stamp (funcall by-name "broken-link")))
      (is equalp (rimu-raw-name) (tuatara.io:listed-octets (funcall by-name (rimu-raw-string))))
      (is eq nil (tuatara.io:listed-device (funcall by-name "text.txt"))))
    ;; the start of a file, a link followed; what is not a file is refused unopened
    (let ((directory (namestring dir)))
      (multiple-value-bind (octets size) (tuatara.io:read-entry-start directory (tuatara.text:string-octets "link-to-text") 3)
        (is equalp (tuatara.text:string-octets "hel") octets)
        (is = 6 size))
      (fail (tuatara.io:read-entry-start directory (tuatara.text:string-octets "pipe") 10) 'tuatara.io:file-system-error)
      (fail (tuatara.io:read-entry-start directory (tuatara.text:string-octets "sub") 10) 'tuatara.io:file-system-error)))
  ;; a directory not there
  (multiple-value-bind (entries reason) (tuatara.io:list-directory "/nonexistent-rimu/")
    (is eq nil entries)
    (true (search "No such file" reason))))

(defun git-in (directory &rest arguments)
  "Run git in DIRECTORY with ARGUMENTS, as a test's author; true when it succeeded."
  (eql 0 (sb-ext:process-exit-code
          (sb-ext:run-program "git" (append (list "-C" (namestring directory) "-c" "user.name=Test" "-c" "user.email=test@example.org"
                                                  "-c" "commit.gpgSign=false")
                                            arguments)
                              :search t :output nil :error nil))))

(define-test (rimu git-column)
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames "docs/" dir))
    (put-file (merge-pathnames "a.txt" dir) (format nil "a~%"))
    (put-file (merge-pathnames "docs/b.txt" dir) (format nil "b~%"))
    (true (git-in dir "init" "-q"))
    (true (git-in dir "add" "a.txt"))
    (true (git-in dir "commit" "-q" "-m" "The first file"))
    (true (git-in dir "add" "docs/b.txt"))
    (true (git-in dir "commit" "-q" "-m" "Docs added"))
    (put-file (merge-pathnames "untracked.txt" dir) (format nil "u~%"))
    ;; G: each entry's last commit after its name, a directory's that of what is in it
    (multiple-value-bind (f editor) (rimu-session dir "G")
      (declare (ignore f))
      (let ((lines (rimu-lines editor)))
        (true (find-if (lambda (l) (and (search "a.txt" l) (cl-ppcre:scan "ago +The first file" l))) lines))
        (true (find-if (lambda (l) (and (search "docs" l) (cl-ppcre:scan "ago +Docs added" l))) lines))
        (true (find-if (lambda (l) (and (search "untracked.txt" l) (not (search "ago" l)))) lines))))
    ;; hidden again with G; shown from the start with *rimu-git-info*
    (multiple-value-bind (f editor) (rimu-session dir "G G")
      (declare (ignore f))
      (false (find-if (lambda (l) (search "The first file" l)) (rimu-lines editor))))
    (let ((tuatara-user:*rimu-git-info* t))
      (multiple-value-bind (f editor) (rimu-session dir "")
        (true (find-if (lambda (l) (search "Docs added" l)) (rimu-lines editor)))
        (multiple-value-bind (screen spans) (tuatara.frontend.fake:fake-frame-with f "Docs added")
          (let ((row (position-if (lambda (r) (search "Docs added" r)) screen)))
            (true (find :list-note (aref spans row) :key #'third))))))
    ;; outside a repository: nothing, and no complaint
    (is eq nil (tuatara.defaults::rimu-git-commits "/" '("tmp")))))

(define-test (rimu git-column-in-odd-directories)
  ;; the git column of a directory whose name holds [ ] and *, characters, not patterns
  (with-test-directory (dir)
    (let* ((directory (concatenate 'string (tuatara.io:native dir) "repo [1]*/"))
           (git (lambda (&rest arguments)
                  (eql 0 (sb-ext:process-exit-code
                          (sb-ext:run-program "git" (append (list "-C" directory "-c" "user.name=Test" "-c" "user.email=test@example.org"
                                                                  "-c" "commit.gpgSign=false")
                                                            arguments)
                                              :search t :output nil :error nil))))))
      (sb-posix:mkdir directory #o700)
      (put-file (sb-ext:parse-native-namestring (concatenate 'string directory "a.txt")) (format nil "a~%"))
      (true (funcall git "init" "-q"))
      (true (funcall git "add" "a.txt"))
      (true (funcall git "commit" "-q" "-m" "The first file"))
      (let ((commits (tuatara.defaults::rimu-git-commits directory '("a.txt"))))
        (true commits)
        (true (and commits (search "The first file" (format nil "~a" (gethash "a.txt" commits)))))))))

(define-test (rimu bindings-none-here)
  (with-rimu-tree (dir)
    (rimu-session dir "?" :width 120 :height 40)
    (let* ((help (tuatara.buffer:find-buffer "*Help*"))
           (text (and help (tuatara.buffer:buffer-string help))))
      (true text)
      ;; a key the list takes away (w, a word's motion elsewhere) is said to do nothing here, in every keymap listing it
      (let ((lines (remove-if-not (lambda (l) (cl-ppcre:scan "^  w +\\S" l)) (uiop:split-string text :separator '(#\Newline)))))
        (true lines)
        (true (every (lambda (l) (search "(none here)" l)) lines))))))

(define-test (rimu sizes-column)
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames "docs/deep/" dir))
    (put-file (merge-pathnames "a.txt" dir) (make-string 1500 :initial-element #\a))
    (put-file (merge-pathnames "docs/b.txt" dir) (make-string 2000 :initial-element #\b))
    (put-file (merge-pathnames "docs/deep/.hidden" dir) (make-string 500 :initial-element #\c))
    ;; a second name for b.txt, and a link to a.txt: neither counted again
    (sb-posix:link (namestring (merge-pathnames "docs/b.txt" dir)) (namestring (merge-pathnames "docs/b-again.txt" dir)))
    (sb-posix:symlink "../a.txt" (namestring (merge-pathnames "docs/link" dir)))
    ;; S: each entry's size, a directory's with all under it, hidden files too
    (multiple-value-bind (f editor) (rimu-session dir "S")
      (declare (ignore f))
      (let ((lines (rimu-lines editor)))
        ;; in the units of the size column and the first lines
        (true (find-if (lambda (l) (and (search "docs" l) (search "2.5K" l))) lines))
        (true (find-if (lambda (l) (and (search "a.txt" l) (search "1.5K" l))) lines))))
    (is = 2500 (tuatara.defaults::rimu-tree-size (namestring (merge-pathnames "docs/" dir))))
    ;; hidden again with S; shown from the start with *rimu-directory-sizes*
    (multiple-value-bind (f editor) (rimu-session dir "S S")
      (declare (ignore f))
      (false (find-if (lambda (l) (search "2.5K" l)) (rimu-lines editor))))
    (let ((tuatara-user:*rimu-directory-sizes* t))
      (multiple-value-bind (f editor) (rimu-session dir "")
        (declare (ignore f))
        (true (find-if (lambda (l) (search "2.5K" l)) (rimu-lines editor)))))))

(define-test (rimu started-on-a-directory)
  ;; tuatara DIRECTORY lists it, as C-x C-f does, rather than stopping at the start
  (with-rimu-tree (dir)
    (multiple-value-bind (f editor) (rimu-session dir "" :file (namestring dir))
      (declare (ignore f))
      (true (search (string-right-trim "/" (namestring dir)) (first (rimu-lines editor))))
      (true (find-if (lambda (l) (search "link-to-dir" l)) (rimu-lines editor))))))

(define-test (rimu permissions-label)
  ;; after the permissions, what ls -l puts there: . for a security context, + for an access control list
  (with-test-directory (dir)
    (put-file (merge-pathnames "plain.txt" dir) "x")
    (let* ((ls (uiop:run-program (list "ls" "-ld" (namestring (merge-pathnames "plain.txt" dir))) :output :string))
           (label (let ((c (char ls 10))) (and (find c ".+") c)))
           (entry (find "plain.txt" (tuatara.io:list-directory (namestring dir) :labels t)
                        :key #'tuatara.io:listed-name :test #'string=)))
      (is eql label (tuatara.io:listed-label entry))
      (false (tuatara.io:listed-label (find "plain.txt" (tuatara.io:list-directory (namestring dir))
                                            :key #'tuatara.io:listed-name :test #'string=)))
      (multiple-value-bind (f editor) (rimu-session dir "I")
        (declare (ignore f))
        (true (find-if (lambda (l) (and (search "plain.txt" l) (search (format nil "-rw-r--r--~@[~c~]" label) l)))
                       (rimu-lines editor)))))))

(define-test (rimu stays-in-its-project)
  ;; going up out of a project lists the folder above in a list of its own, the project's
  ;; list left where it was; g l goes back to it; within the project, the same list
  (with-test-directory (dir)
    (let ((project (merge-pathnames "proj/" dir)))
      (ensure-directories-exist (merge-pathnames ".git/" project))
      (alexandria:write-string-into-file "x" (ensure-directories-exist (merge-pathnames "sub/x.txt" project)))
      (flet ((lists ()
               (sort (mapcar (lambda (b) (tuatara.buffer:buffer-local b 'tuatara.defaults::rimu-directory))
                             (remove-if-not #'tuatara.defaults::rimu-list-p tuatara.buffer:*buffers*))
                     #'string<))
             (listed (editor) (tuatara.buffer:buffer-local (session-buffer editor) 'tuatara.defaults::rimu-directory)))
        (multiple-value-bind (f editor) (rimu-session (merge-pathnames "sub/" project) "h")
          (declare (ignore f))
          (is equal (list (tuatara.io:native project)) (lists))
          (is string= (tuatara.io:native project) (listed editor)))
        (multiple-value-bind (f editor) (rimu-session (merge-pathnames "sub/" project) "h h")
          (declare (ignore f))
          (is equal (list (tuatara.io:native dir) (tuatara.io:native project)) (lists))
          (is string= (tuatara.io:native dir) (listed editor)))
        (multiple-value-bind (f editor) (rimu-session (merge-pathnames "sub/" project) "h h g l")
          (declare (ignore f))
          (is equal (list (tuatara.io:native dir) (tuatara.io:native project)) (lists))
          (is string= (tuatara.io:native project) (listed editor)))))
    (rimu-forget)))

(define-test (rimu bookmarks-reuse-project-lists)
  ;; a folder bookmark moves the project's list there; one of no project, with no list of
  ;; none, gets a list of its own; with *rimu-reuse-lists* nil every one gets its own
  (with-test-directory (dir)
    (let ((project (merge-pathnames "proj/" dir)))
      (ensure-directories-exist (merge-pathnames ".project/" project))
      (dolist (folder '("proj/one/" "proj/two/" "elsewhere/"))
        (ensure-directories-exist (merge-pathnames folder dir)))
      (flet ((lists ()
               (sort (mapcar (lambda (b) (tuatara.buffer:buffer-local b 'tuatara.defaults::rimu-directory))
                             (remove-if-not #'tuatara.defaults::rimu-list-p tuatara.buffer:*buffers*))
                     #'string<))
             (native (folder) (tuatara.io:native (merge-pathnames folder dir))))
        (tuatara.defaults::state-list-put tuatara.defaults::*bookmarks*
                                          (list (list "tbtwo" (native "proj/two/") 0 "" nil nil)
                                                (list "tbelse" (native "elsewhere/") 0 "" nil nil)))
        (unwind-protect
             (progn
               (rimu-session (merge-pathnames "proj/one/" dir) "C-x m b t b t w o RET")
               (is equal (list (native "proj/two/")) (lists))
               (rimu-session (merge-pathnames "proj/one/" dir) "C-x m b t b e l s e RET")
               (is equal (list (native "elsewhere/") (native "proj/one/")) (lists))
               (let ((tuatara-user:*rimu-reuse-lists* nil))
                 (rimu-session (merge-pathnames "proj/one/" dir) "C-x m b t b t w o RET")
                 (is equal (list (native "proj/one/") (native "proj/two/")) (lists))))
          (tuatara.defaults::state-list-drop tuatara.defaults::*bookmarks* '("tbtwo" "tbelse"))
          (rimu-forget))))))

(define-test (rimu folder-opened-safely)
  ;; a folder opened safely is listed, as one opened as usual is, never read as a file
  (with-test-directory (dir)
    (alexandria:write-string-into-file "x" (merge-pathnames "inside.txt" dir))
    (rimu-forget)
    (multiple-value-bind (f editor)
        (run-keys (format nil "M-x f i n d - f i l e - s a f e l y RET C-a C-k ~a C-j" (rimu-typed (namestring dir)))
                  :width 100 :height 20)
      (true (tuatara.defaults::rimu-list-p (session-buffer editor)))
      (is string= (tuatara.io:native dir)
          (tuatara.buffer:buffer-local (session-buffer editor) 'tuatara.defaults::rimu-directory))
      (true (tuatara.frontend.fake:fake-frame-showing f "inside.txt"))
      (false (tuatara.frontend.fake:fake-frame-showing f "not a file of text"))
      (true (tuatara.io:safe-visit-p (session-buffer editor))))
    ;; its preview shows a file as one opened safely is shown: its text, no major mode;
    ;; a folder's preview list is opened safely too
    (alexandria:write-string-into-file "(defun shown-in-preview ())" (merge-pathnames "code.lisp" dir))
    (ensure-directories-exist (merge-pathnames "sub/" dir))
    (rimu-forget)
    (let ((tuatara-user:*rimu-preview* t) (tuatara-user:*rimu-preview-delay* 0))
      (multiple-value-bind (f editor)
          (run-keys (list (format nil "M-x f i n d - f i l e - s a f e l y RET C-a C-k ~a C-j C-s c o d e RET" (rimu-typed (namestring dir)))
                          :tick :tick)
                    :width 120 :height 20)
        (true (tuatara.defaults::rimu-previewing-p (session-buffer editor)))
        (true (tuatara.frontend.fake:fake-frame-showing f "(defun shown-in-preview ())"))
        (let ((preview (find-if (lambda (b) (search "code.lisp (preview" (tuatara.buffer:buffer-name b))) tuatara.buffer:*buffers*)))
          (true preview)
          (false (search "lisp" (string-downcase (princ-to-string (type-of (tuatara.defaults::buffer-major-mode preview)))))))))
    (rimu-forget)
    (let ((tuatara-user:*rimu-preview* t) (tuatara-user:*rimu-preview-delay* 0))
      (run-keys (list (format nil "M-x f i n d - f i l e - s a f e l y RET C-a C-k ~a C-j C-s s u b RET" (rimu-typed (namestring dir)))
                      :tick :tick)
                :width 120 :height 20)
      (let ((preview (find-if (lambda (b) (and (tuatara.defaults::rimu-list-p b) (tuatara.buffer:buffer-local b 'tuatara.defaults::rimu-preview-buffer)))
                              tuatara.buffer:*buffers*)))
        (true preview)
        (true (and preview (tuatara.io:safe-visit-p preview)))))
    ;; what is opened from it opens safely, and nothing in it is changed
    (let ((tuatara-user:*rimu-preview* t))
      (multiple-value-bind (f editor)
          (run-keys (format nil "M-x f i n d - f i l e - s a f e l y RET C-a C-k ~a C-j C-s i n s i d e RET D"
                            (rimu-typed (namestring dir)))
                    :width 120 :height 20)
        (declare (ignore editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "list-delete-marked changes files, and"))
        (true (probe-file (merge-pathnames "inside.txt" dir))))
      ;; nor packed into an archive or unpacked beside it, nor are its files given
      ;; formatted names
      (dolist (keys '("c" "Z" "C-x n r" "C-x n k"))
        (rimu-forget)
        (let ((f (run-keys (format nil "M-x f i n d - f i l e - s a f e l y RET C-a C-k ~a C-j C-s i n s i d e RET ~a"
                                   (rimu-typed (namestring dir)) keys)
                           :width 120 :height 20)))
          (true (tuatara.frontend.fake:fake-frame-showing f "changes files, and"))
          (true (probe-file (merge-pathnames "inside.txt" dir)))))
      ;; nor wiped, nor staged, renamed, deleted or put back by git, which act on the entry on the line
      (let ((refusals '()))
        (rimu-forget)
        (run-keys (list (format nil "M-x f i n d - f i l e - s a f e l y RET C-a C-k ~a C-j C-s i n s i d e RET"
                                (rimu-typed (namestring dir)))
                        (lambda ()
                          (dolist (command '(tuatara.defaults::wipe-file tuatara.defaults::git-stage-file
                                             tuatara.defaults::git-delete-file tuatara.defaults::git-rename-file
                                             tuatara.defaults::git-reset-file))
                            (push (tuatara.defaults::mode-command-refusal command) refusals))))
                  :width 120 :height 20)
        (is = 5 (length refusals))
        (true (every (lambda (said) (search "changes files, and" (or said ""))) refusals) "~s" refusals)))
    (rimu-forget)
    (let ((editor (nth-value 1 (run-keys (format nil "M-x f i n d - f i l e - s a f e l y RET C-a C-k ~a C-j C-s i n s i d e RET RET"
                                                  (rimu-typed (namestring dir)))
                                          :width 120 :height 20))))
      (is string= "inside.txt" (tuatara.buffer:buffer-name (session-buffer editor)))
      (true (tuatara.io:safe-visit-p (session-buffer editor))))
    (rimu-forget)))

(define-test (rimu folder-name-with-a-line-break)
  ;; a folder whose name holds a line break is listed on one title line, so that each
  ;; line acts on the entry it shows
  (with-test-directory (dir)
    (let ((odd (merge-pathnames (format nil "dir~%nl/") dir)))
      (ensure-directories-exist odd)
      (alexandria:write-string-into-file "A" (merge-pathnames "a.txt" odd))
      (alexandria:write-string-into-file "B" (merge-pathnames "b.txt" odd))
      (multiple-value-bind (f editor) (rimu-session dir "C-s d i r RET RET C-s a . t x t RET")
        (declare (ignore f))
        (let* ((list (session-buffer editor))
               (lines (uiop:split-string (tuatara.buffer:buffer-string list) :separator '(#\Newline))))
          (true (search "dir^Jnl:" (first lines)))
          (is equal "a.txt" (tuatara.io:listed-name
                             (tuatara.defaults::rimu-entry-at list (tuatara.buffer:buffer-point list))))))
      (rimu-forget))))

(define-test (rimu place-kept-while-away)
  ;; a file opened from the list; while it is shown, the list is read again for files made and
  ;; deleted meanwhile; back in the list once the file is closed, point is on that file's entry
  (with-test-directory (dir)
    (dolist (name '("a.txt" "m.txt" "z.txt"))
      (alexandria:write-string-into-file "x" (merge-pathnames name dir)))
    (rimu-forget)
    (let* ((tuatara-user:*rimu-preview* t)
           (tuatara-user:*rimu-preview-delay* 0)
           (tuatara-user:*rimu-sort* :name)
           (generation 0)
           (reread 0))
      (flet ((list-buffer () (tuatara.buffer:find-buffer (rimu-list-name dir)))
             (read-again ()
               (let ((now (tuatara.buffer:buffer-local (tuatara.buffer:find-buffer (rimu-list-name dir)) 'tuatara.defaults::rimu-generation)))
                 (when (and (not (eql now generation))
                            (null (tuatara.buffer:buffer-local (tuatara.buffer:find-buffer (rimu-list-name dir)) 'tuatara.defaults::rimu-job)))
                   (setf generation now)
                   (incf reread)))))
        (multiple-value-bind (f editor)
            (auto-save-session (list (format nil "C-x d C-a C-k ~a C-j" (rimu-typed (namestring dir)))
                                     (list :until (lambda () (list-buffer)))
                                     "g f m . t x t RET l"
                                     (list :until (lambda () (setf generation (tuatara.buffer:buffer-local (list-buffer) 'tuatara.defaults::rimu-generation))
                                                    (alexandria:write-string-into-file "y" (merge-pathnames "n.txt" dir))
                                                    t))
                                     (list :until #'read-again)
                                     (list :until (lambda () (delete-file (merge-pathnames "n.txt" dir)) t))
                                     (list :until #'read-again)
                                     "C-x k" (list :wait 0.1))
                               :width 100)
          (declare (ignore f))
          (is string= (rimu-list-name dir) (current-name editor))
          (let ((entry (tuatara.defaults::rimu-entry-at (list-buffer) (session-point editor))))
            (is = 2 reread "the list read again each time")
            (true entry)
            (is equalp (tuatara.text:string-octets "m.txt") (and entry (tuatara.io:listed-octets entry)))))))
    (rimu-forget)))

(define-test (rimu columns-kept-when-read-again)
  ;; a list read again keeps its git column, git not run, while the same names are listed and
  ;; the repository has the same commit and staging; the sizes show their old values while
  ;; they are worked out again
  (with-test-directory (dir)
    (scratch-repository dir)
    ;; the columns as a session has them
    (tuatara.defaults::install-rimu-git)
    (tuatara.defaults::install-rimu-sizes)
    (let* ((directory (namestring dir))
           (columns (mapcar (lambda (name) (find name tuatara.defaults::*rimu-columns* :key #'tuatara.defaults::rimu-column-name))
                            '(:git :sizes)))
           (view (tuatara.defaults::make-rimu-view nil nil :name nil nil columns nil nil nil))
           (old (tuatara.defaults::rimu-carry-cells nil (tuatara.defaults::rimu-read directory) view))
           (runs 0))
      (setf (gethash :git (tuatara.defaults::rimu-listing-cells old))
            (map 'vector (lambda (entry) (tuatara.io:listed-name entry)) (tuatara.defaults::rimu-listing-entries old))
            (gethash :sizes (tuatara.defaults::rimu-listing-cells old))
            (map 'vector (lambda (entry) (declare (ignore entry)) "old") (tuatara.defaults::rimu-listing-entries old)))
      (sb-int:encapsulate 'tuatara.defaults::run-git-program 'count-runs
                          (lambda (function &rest arguments) (incf runs) (apply function arguments)))
      (unwind-protect
           (let ((again (tuatara.defaults::rimu-carry-cells old (tuatara.defaults::rimu-read directory) view)))
             (is eq (gethash :git (tuatara.defaults::rimu-listing-cells old)) (gethash :git (tuatara.defaults::rimu-listing-cells again)))
             (is = 0 runs)
             (true (find "old" (gethash :sizes (tuatara.defaults::rimu-listing-cells again)) :test #'equal))
             ;; a commit made: the git column read again
             (put-file (merge-pathnames "b.txt" dir) "b")
             (git-output dir "add" "b.txt")
             (git-output dir "commit" "-q" "-m" "Second")
             (let ((later (tuatara.defaults::rimu-carry-cells again (tuatara.defaults::rimu-read directory) view)))
               (false (gethash :git (tuatara.defaults::rimu-listing-cells later)))))
        (sb-int:unencapsulate 'tuatara.defaults::run-git-program 'count-runs)))))

(define-test (rimu marks-found-without-garbage)
  ;; the mark of each entry shown is found by a key made once for the entry, not again at each
  ;; redisplay
  (with-rimu-tree (dir)
    (multiple-value-bind (f editor) (rimu-session dir "m m m")
      (declare (ignore f editor))
      (let* ((buffer (find-if #'tuatara.defaults::rimu-list-p tuatara.buffer:*buffers*))
             (marks (tuatara.defaults::rimu-marks buffer))
             (entries (tuatara.defaults::rimu-listing-entries (tuatara.buffer:buffer-local buffer 'tuatara.defaults::rimu-listing)))
             (directory (tuatara.buffer:buffer-local buffer 'tuatara.defaults::rimu-directory)))
        (is = 3 (count-if (lambda (entry) (tuatara.defaults::rimu-entry-mark buffer entry marks)) entries))
        (loop for entry across entries
              do (is eql (gethash (tuatara.defaults::rimu-mark-key directory entry) marks)
                     (tuatara.defaults::rimu-entry-mark buffer entry marks)))
        ;; a key made for each look was some 100 octets
        (let ((consed (loop repeat 3
                            minimize (let ((before (sb-ext:get-bytes-consed)))
                                       (dotimes (i 100000)
                                         (loop for entry across entries do (tuatara.defaults::rimu-entry-mark buffer entry marks)))
                                       (- (sb-ext:get-bytes-consed) before)))))
          (true (< consed (* 100000 (length entries) 8)) "~:d octets for ~:d looks" consed (* 100000 (length entries))))))))
