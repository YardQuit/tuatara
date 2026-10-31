;;;; test/org-parts.lisp --- An Org buffer in parts: kept up with each change, read as the whole parse reads.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(defun org-parts-corpus ()
  "The Org texts of test/org/, each once as written, once with letters of
more than one octet in it, and once with its lines ending in CR LF."
  (loop for name in '("headlines" "blocks" "lists" "tables" "objects" "edge" "found")
        for text = (uiop:read-file-string (asdf:system-relative-pathname :tuatara (format nil "test/org/~a.org" name)))
        collect (cons text nil)
        collect (cons (substitute #\å #\a (substitute #\ö #\o text)) nil)
        collect (cons (cl-ppcre:regex-replace-all (string #\Newline) text (format nil "~c~%" #\Return)) t)))

(defun org-parts-buffer (text crlf)
  "A new buffer holding TEXT, its lines ending in CR LF when CRLF is true."
  (let ((buffer (org-internals-buffer text)))
    (when crlf (setf (tuatara.buffer:buffer-local buffer :line-ending) :crlf))
    buffer))

(defun org-parts-summary (buffer)
  "BUFFER's parts as data: (START LEVEL TODO-LINES) for each, and the TODO keywords."
  (let ((parts (tuatara.defaults::org-buffer-parts buffer)))
    (list (loop for i below (tuatara.defaults::org-parts-count parts)
                for part = (tuatara.defaults::org-part-at parts i)
                collect (list (tuatara.defaults::org-part-start parts i) (tuatara.defaults::org-part-level part)
                              (tuatara.defaults::org-part-todo-lines part)))
          (tuatara.defaults::org-parts-todo-keywords parts))))

(defun org-parts-afresh (buffer)
  "BUFFER's parts as ORG-PARTS-SUMMARY has them, found again from nothing."
  (let ((kept (tuatara.buffer:buffer-local buffer 'tuatara.defaults::org-parts)))
    (setf (tuatara.buffer:buffer-local buffer 'tuatara.defaults::org-parts) nil)
    (prog1 (org-parts-summary buffer)
      (setf (tuatara.buffer:buffer-local buffer 'tuatara.defaults::org-parts) kept))))

(defun org-parts-whole-view (buffer)
  "An ORG-VIEW of all of BUFFER from its whole parse, as ORG-PARSE reads it:
the carriage returns before newlines left out only where lines end in CR LF."
  (multiple-value-bind (text returns) (if (tuatara.defaults::buffer-crlf-p buffer)
                                          (tuatara.defaults::org-strip-returns (tuatara.buffer:buffer-string buffer))
                                          (values (tuatara.buffer:buffer-string buffer) nil))
    (tuatara.defaults::make-org-view (tuatara.defaults::org-parse text) text 0 (tuatara.buffer:buffer-size buffer) returns t)))

(defun org-parts-colours (buffer views)
  "The colours of all of BUFFER from VIEWS, in order of where they begin (code in its language's colours left out)."
  (let ((size (tuatara.buffer:buffer-size buffer)))
    (with-functions-replaced ((tuatara.defaults::org-src-cached-highlights (lambda (&rest arguments) (declare (ignore arguments)) nil)))
      (stable-sort (loop for view in views nconc (tuatara.defaults::org-view-colours buffer view 0 size nil))
                   #'< :key #'first))))

(define-test (org-internals parts-colour-as-the-whole-parse)
  ;; the colours and hidden parts of links read from the parts are those the parse of the whole text gives,
  ;; letters of more octets than one and lines ending in CR LF too
  (dolist (entry (org-parts-corpus))
    (destructuring-bind (text . crlf) entry
      (let ((buffer (org-parts-buffer text crlf)))
        (unwind-protect
             (let* ((size (tuatara.buffer:buffer-size buffer))
                    (whole (org-parts-whole-view buffer))
                    (parts (tuatara.defaults::org-views buffer 0 size))
                    (expected (org-parts-colours buffer (list whole))))
               (true expected)
               (is equal expected (org-parts-colours buffer parts))
               ;; and read from the whole parse, once one is made
               (let ((tuatara.defaults::*org-parse-at-once* most-positive-fixnum))
                 (tuatara.defaults::org-buffer-tree buffer))
               (let ((from-whole (tuatara.defaults::org-views buffer 0 size)))
                 (true (every #'tuatara.defaults::org-view-root from-whole))
                 (is equal expected (org-parts-colours buffer from-whole)))
               (is equal (loop for (s . e) in (tuatara.defaults::org-hidden-chars (tuatara.defaults::org-view-tree whole) 0 (length (tuatara.defaults::org-view-text whole)))
                               collect (if (consp e)
                                           (list (tuatara.defaults::org-view-byte buffer whole s) (tuatara.defaults::org-view-byte buffer whole (first e)) (second e) nil)
                                           (list (tuatara.defaults::org-view-byte buffer whole s) (tuatara.defaults::org-view-byte buffer whole e) "" nil)))
                   (tuatara.defaults::org-hidden-ranges buffer 0 size :cache nil)))
          (tuatara.buffer:kill-buffer buffer))))))

(define-test (org-internals parts-kept-as-made-afresh)
  ;; after each of many changes, the parts kept up are those found again from nothing, and each part's parse
  ;; that of its text: stars typed and taken away, lines joined and split, a #+TODO: line written, many changes at once
  (let ((*random-state* (sb-ext:seed-random-state 2026))
        (pieces (list "* " "** " "*" " " (string #\Newline) (format nil "~%* New~%") (format nil "#+TODO: NEXT | DONE~%")
                      "NEXT " "é" "x" (format nil "~%~%") ":END:" "#+begin_src")))
    (dolist (entry (org-parts-corpus))
      (destructuring-bind (text . crlf) entry
        (unless crlf
          (let ((buffer (org-parts-buffer text nil)))
            (unwind-protect
                 (dotimes (step 120)
                   (let* ((rope (tuatara.buffer:buffer-text buffer))
                          (size (tuatara.text:rope-bytes rope))
                          (edits (sort (loop repeat (1+ (random 3))
                                             collect (let* ((start (tuatara.text:rope-char-start rope (random (1+ size))))
                                                            (end (if (zerop (random 3))
                                                                     (tuatara.text:rope-char-start rope (min size (+ start (random 12))))
                                                                     start)))
                                                       (tuatara.text:make-edit start end (tuatara.text:string-octets (nth (random (length pieces)) pieces)))))
                                       #'< :key #'tuatara.text:edit-start))
                          ;; apart, as a batch takes them
                          (edits (loop with last = -1
                                       for e in edits
                                       when (> (tuatara.text:edit-start e) last)
                                         collect e and do (setf last (tuatara.text:edit-end e)))))
                     (tuatara.buffer:buffer-edit buffer (coerce edits 'vector))
                     ;; every other change taken in at once, the rest two at a time
                     (when (or (evenp step) (= step 119))
                       (let ((kept (org-parts-summary buffer)))
                         (is equal (org-parts-afresh buffer) kept)
                         ;; each part's parse of its text as it is now
                         (dolist (view (tuatara.defaults::org-views buffer 0 (tuatara.buffer:buffer-size buffer)))
                           (is string= (tuatara.buffer:buffer-string buffer :start (tuatara.defaults::org-view-from view)
                                                                            :end (tuatara.defaults::org-view-to view))
                               (tuatara.defaults::org-view-text view))
                           (is equalp (org-tree-data (tuatara.defaults::org-parse (tuatara.defaults::org-view-text view)
                                                                                  :todo-keywords (second kept)))
                               (org-tree-data (tuatara.defaults::org-view-tree view))))))))
              (tuatara.buffer:kill-buffer buffer))))))))

(defun org-tree-data (node)
  "NODE and what is under it as data: type, positions and children, to compare two parses."
  (list (tuatara.defaults::org-node-type node) (tuatara.defaults::org-node-begin node) (tuatara.defaults::org-node-end node)
        (tuatara.defaults::org-node-contents-begin node) (tuatara.defaults::org-node-contents-end node)
        (tuatara.defaults::org-property node :todo-keyword)
        (mapcar #'org-tree-data (tuatara.defaults::org-node-children node))))

(defun org-typing-garbage (octets keys)
  "The octets consed while KEYS letters are typed in the middle of an Org text OCTETS long and drawn after each."
  (let* ((one (format nil "* TODO Heading~%Some *bold* text, a [[https://example.org][link]] and =code=.~%- an item~%- [ ] a box~%~%"))
         (text (with-output-to-string (out) (loop repeat (ceiling octets (length one)) do (write-string one out))))
         (middle (1+ (position #\Newline text :start (floor (length text) 2))))
         (before 0) (consed 0))
    ;; each change's colours made before the next key, however large the text: no parse in the background to wait for
    (let ((tuatara.defaults::*org-parse-at-once* most-positive-fixnum))
      (run-keys (list "M-x o r g - m o d e RET"
                      (lambda () (setf (tuatara.buffer:buffer-point (session-buffer tuatara.command:*editor*)) middle))
                      "i x" :tick
                      (lambda () (setf before (tuatara.bench::bytes-consed)))
                      (format nil "~{~a~^ ~}" (make-list keys :initial-element "y"))
                      (lambda () (setf consed (- (tuatara.bench::bytes-consed) before)))
                      "C-g")
                :text text :width 100 :height 30))
    consed))

(define-test (org-internals typing-costs-the-part-not-the-file)
  ;; a key typed in a large Org file makes the garbage of the part it is typed in, not of the file:
  ;; the whole file was parsed again for each, 2.5 MB of garbage for each key more for each 100 KB more of file
  (flet ((per-key (octets)
           (/ (- (loop repeat 2 minimize (org-typing-garbage octets 30))
                 (loop repeat 2 minimize (org-typing-garbage octets 10)))
              20)))
    (let ((per-100-kb (/ (- (per-key 400000) (per-key 100000)) 3)))
      (true (< per-100-kb 20000) "~:d octets for each key, for each 100 KB more of file" (round per-100-kb)))))

(define-test (org typed-headlines-coloured-at-once)
  ;; a headline typed in the middle of a note is coloured as one at once; its star taken away, the line is text again
  (let ((text (with-output-to-string (out)
                (dotimes (i 40) (format out "* Heading ~d~%Text of ~d.~%" i i)))))
    (flet ((after (at keys)
             (let ((tuatara-user:*org-bullets* nil) (tuatara-user:*org-startup-indented* nil))
               (run-keys (list "M-x o r g - m o d e RET"
                               (lambda () (setf (tuatara.buffer:buffer-point (session-buffer tuatara.command:*editor*)) (search at text)))
                               keys)
                         :text text :width 80 :height 12)))
           (row-of (f words) (loop for row below 11 when (search words (tuatara.frontend.fake:fake-screen-line f row)) return row)))
      (let* ((f (after "Text of 20." "i * * SPC N e w RET C-g"))
             (row (row-of f "** New")))
        (true row)
        (true (org-faces-include f row "** New" :org-level-2)))
      (let* ((f (after "* Heading 20" "d"))
             (row (row-of f "Heading 20")))
        (true row)
        (true (loop for (nil face) in (org-row-faces f row) never (eq (if (listp face) (first face) face) :org-level-1)))
        (true (org-faces-include f (- row 2) "* Heading 19" :org-level-1))))))

(defun org-node-full-data (node)
  "NODE and everything under it as data, every slot and property (a
headline's title objects too), to tell two parses apart."
  (list (tuatara.defaults::org-node-type node) (tuatara.defaults::org-node-begin node) (tuatara.defaults::org-node-end node)
        (tuatara.defaults::org-node-contents-begin node) (tuatara.defaults::org-node-contents-end node)
        (tuatara.defaults::org-node-post-blank node) (tuatara.defaults::org-node-post-affiliated node)
        (loop for (key value) on (tuatara.defaults::org-node-properties node) by #'cddr
              unless (eq key :children-vector)
                collect key and collect (if (eq key :title) (mapcar #'org-node-full-data value) value))
        (mapcar #'org-node-full-data (tuatara.defaults::org-node-children node))))

(defun org-parts-random-edits (buffer pieces)
  "Make one batch of one to three random edits in BUFFER, each putting one of PIECES somewhere."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (size (tuatara.text:rope-bytes rope))
         (edits (sort (loop repeat (1+ (random 3))
                            collect (let* ((start (tuatara.text:rope-char-start rope (random (1+ size))))
                                           (end (if (zerop (random 3))
                                                    (tuatara.text:rope-char-start rope (min size (+ start (random 12))))
                                                    start)))
                                      (tuatara.text:make-edit start end (tuatara.text:string-octets (nth (random (length pieces)) pieces)))))
                      #'< :key #'tuatara.text:edit-start)))
    (tuatara.buffer:buffer-edit buffer (coerce (loop with last = -1
                                                     for e in edits
                                                     when (> (tuatara.text:edit-start e) last)
                                                       collect e and do (setf last (tuatara.text:edit-end e)))
                                               'vector))))

(define-test (org-internals whole-parse-made-of-parts)
  ;; the whole parse commands read, made of the parts' parses, is the parse of the whole text, node for node,
  ;; and where its text lost its carriage returns as they are: after each of many changes, CR LF too
  (let ((*random-state* (sb-ext:seed-random-state 2027))
        (tuatara.defaults::*org-parse-at-once* most-positive-fixnum)
        (pieces (list "* " "** " "*" "*	" " " (string #\Newline) (format nil "~%* New~%") (format nil "#+TODO: NEXT | DONE~%")
                      "NEXT " "COMMENT " "é" "x" (format nil "~%~%") ":END:" (format nil "~%:PROPERTIES:~%:ID: 1~%:END:~%")
                      "#+begin_src" (format nil "~%#+end_src~%") "[[a][b]] *bold* "))
        (compared 0))
    (dolist (entry (org-parts-corpus))
      (destructuring-bind (text . crlf) entry
        (let ((buffer (org-parts-buffer text crlf)))
          (unwind-protect
               (dotimes (step 60)
                 (when (plusp step)
                   (if crlf
                       ;; lines kept ending in CR LF: no newline typed alone
                       (org-parts-random-edits buffer (list "* " "x" "é" "*" " " (format nil "~c~%* N~c~%" #\Return #\Return)))
                       (org-parts-random-edits buffer pieces)))
                 (when (or (evenp step) (= step 59))
                   (multiple-value-bind (tree tree-text) (tuatara.defaults::org-buffer-tree buffer)
                     (multiple-value-bind (whole returns) (tuatara.defaults::org-strip-returns (tuatara.buffer:buffer-string buffer))
                       (incf compared)
                       (is string= whole tree-text)
                       (is equal (org-node-full-data (tuatara.defaults::org-parse whole)) (org-node-full-data tree))
                       (when crlf
                         (is equalp returns (tuatara.defaults::org-state-returns (tuatara.defaults::org-buffer-state buffer)))
                         (is equalp returns (tuatara.defaults::org-parts-returns buffer)))))))
            (tuatara.buffer:kill-buffer buffer)))))
    (true (< 600 compared))))

(define-test (org-internals blocks-key-follows-the-blocks)
  ;; what language servers see of a note is worked out again when its blocks change, not for text typed elsewhere
  (let ((buffer (org-parts-buffer (format nil "* A~%Prose here.~%#+begin_src python~%x = 1~%#+end_src~%* B~%More prose.~%") nil)))
    (unwind-protect
         (flet ((key () (tuatara.defaults::org-blocks-key buffer))
                (insert (at text) (tuatara.buffer:buffer-edit buffer (vector (tuatara.text:make-edit at at (tuatara.text:string-octets text))))))
           (let ((before (key)))
             (insert 6 "Words ")
             (insert (search "More" (tuatara.buffer:buffer-string buffer)) "Even ")
             (is equal before (key))
             (insert (search "python" (tuatara.buffer:buffer-string buffer)) "r")
             (false (equal before (key)))
             (let ((again (key)))
               (insert (search "#+end_src" (tuatara.buffer:buffer-string buffer)) "x")
               (false (equal again (key))))))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (org-internals large-part-typed-in-without-lag)
  ;; typing in a note of one large part (no headlines): its parse in the background is put off while typing goes
  ;; on, not made again for each key, so that the garbage of each key stays small; it is parsed once typing pauses
  (let* ((text (with-output-to-string (out)
                 (dotimes (i 6000) (format out "- item ~d with *bold* and a [[https://x.org][link]]~%" i))))
         (middle (1+ (position #\Newline text :start (floor (length text) 2))))
         (before 0) (consed 0) (view nil))
    (labels ((buffer () (session-buffer tuatara.command:*editor*))
             (settled () (loop repeat 400 until (let ((v (tuatara.defaults::org-view-at (buffer) middle))) (and v (tuatara.defaults::org-view-current v)))
                               do (sleep 0.01))))
      (run-keys (append (list "M-x o r g - m o d e RET"
                              (lambda () (setf (tuatara.buffer:buffer-point (buffer)) middle))
                              "i x" :tick #'settled
                              (lambda () (setf before (tuatara.bench::bytes-consed))))
                        ;; fifty keys a second
                        (loop repeat 40 append (list "y" (lambda () (sleep 0.02))))
                        (list (lambda () (setf consed (- (tuatara.bench::bytes-consed) before)))
                              (lambda () (sleep 0.3)) :tick #'settled
                              (lambda () (setf view (tuatara.defaults::org-view-at (buffer) middle)))
                              "C-g"))
                :text text :width 100 :height 30))
    ;; each key's parse was 6 MB of garbage
    (true (< (/ consed 40) 300000) "~:d octets for each key" (round consed 40))
    (true (tuatara.defaults::org-view-current view))))

(define-test (org-internals whole-parse-after-a-change-parses-the-part)
  ;; the whole parse a command reads, after a key typed in a large note, parses only the part typed in:
  ;; the whole text was parsed again, 1.9 s for 10 MB
  (let* ((one (format nil "* Heading~%Some *bold* text and a [[https://x.org][link]].~%- an item~%~%"))
         (text (with-output-to-string (out) (loop repeat 6000 do (write-string one out))))
         (buffer (org-parts-buffer text nil))
         (parsed 0)
         (parse (fdefinition 'tuatara.defaults::org-parse)))
    (unwind-protect
         (let ((tuatara.defaults::*org-parse-at-once* most-positive-fixnum))
           (tuatara.defaults::org-buffer-tree buffer)
           (tuatara.buffer:buffer-edit buffer (vector (tuatara.text:make-edit 200000 200000 (tuatara.text:string-octets "x"))))
           (with-functions-replaced ((tuatara.defaults::org-parse (lambda (text &rest arguments)
                                                                     (incf parsed (length text))
                                                                     (apply parse text arguments))))
             (multiple-value-bind (tree whole) (tuatara.defaults::org-buffer-tree buffer)
               (true tree)
               (is = (1+ (length text)) (length whole))))
           (true (< 0 parsed 1000) "~:d characters parsed" parsed))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (org-internals returns-found-again-for-the-part-changed)
  ;; where a CR LF note's text lost its carriage returns, after a change: found again in the part changed, not
  ;; in the whole text
  (let* ((one (format nil "* Heading~c~%Some text.~c~%~c~%" #\Return #\Return #\Return))
         (text (with-output-to-string (out) (loop repeat 20000 do (write-string one out))))
         (buffer (org-parts-buffer text t))
         (read 0)
         (find (fdefinition 'tuatara.defaults::org-returns-in-rope)))
    (unwind-protect
         (progn
           (tuatara.defaults::org-returns-of buffer)
           (tuatara.buffer:buffer-edit buffer (vector (tuatara.text:make-edit 200000 200000 (tuatara.text:string-octets "x"))))
           (with-functions-replaced ((tuatara.defaults::org-returns-in-rope
                                      (lambda (rope &rest arguments &key (start 0) (end (tuatara.text:rope-bytes rope)))
                                        (incf read (- end start))
                                        (apply find rope arguments))))
             (is equalp (nth-value 1 (tuatara.defaults::org-strip-returns (tuatara.buffer:buffer-string buffer)))
                 (tuatara.defaults::org-returns-of buffer)))
           (true (< read 1000) "~:d octets read" read))
      (tuatara.buffer:kill-buffer buffer))))

(defun org-parts-many (count)
  "A text of COUNT short parts, a headline and a line under each."
  (with-output-to-string (out)
    (dotimes (i count) (format out "* H~d~%text~%" i))))

(define-test (org-internals edits-in-a-batch-cost-alike-however-many-parts)
  ;; a batch of many edits (a replacement of every match) is taken in at once: each edit costs alike whatever
  ;; the number of parts, where each moved every part after it (110 µs an edit at 100,000 parts)
  (flet ((seconds (parts edits)
           (let* ((text (org-parts-many parts))
                  (buffer (org-parts-buffer text nil)))
             (unwind-protect
                  (progn
                    (tuatara.defaults::org-buffer-parts buffer)
                    (let ((step (floor (length text) edits)))
                      (tuatara.buffer:buffer-edit buffer (coerce (loop for i below edits
                                                                       collect (tuatara.text:make-edit (+ 3 (* i step)) (+ 3 (* i step))
                                                                                                       (tuatara.text:string-octets "y")))
                                                                 'vector)))
                    (let ((start (get-internal-real-time)))
                      (tuatara.defaults::org-buffer-parts buffer)
                      (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
               (tuatara.buffer:kill-buffer buffer)))))
    (let ((per-edit (/ (- (seconds 80000 8000) (seconds 80000 2000)) 6000)))
      (true (< per-edit 2/100000) "~,1f µs for each edit more, at 80,000 parts" (* 1000000 per-edit)))))

(define-test (org-internals whole-parse-holds-the-parts)
  ;; once the whole parse is made, the parts are read from it: no part keeps a parse of its own, nothing is
  ;; held twice, and a large part is not parsed again
  (let* ((big (with-output-to-string (out) (dotimes (i 600) (format out "- item ~d with *bold* text~%" i))))
         (text (concatenate 'string (format nil "* A~%") big (org-parts-many 50)))
         (buffer (org-parts-buffer text nil))
         (parsed 0)
         (parse (fdefinition 'tuatara.defaults::org-parse)))
    (unwind-protect
         (let ((tuatara.defaults::*org-parse-at-once* 8192))
           (tuatara.defaults::org-views buffer 0 (tuatara.buffer:buffer-size buffer))
           (let ((tuatara.defaults::*org-parse-at-once* most-positive-fixnum))
             (tuatara.defaults::org-buffer-tree buffer))
           (let ((parts (tuatara.defaults::org-buffer-parts buffer)))
             (true (loop for i below (tuatara.defaults::org-parts-count parts)
                         never (tuatara.defaults::org-part-parse (tuatara.defaults::org-part-at parts i)))))
           (with-functions-replaced ((tuatara.defaults::org-parse (lambda (text &rest arguments)
                                                                     (incf parsed (length text))
                                                                     (apply parse text arguments))))
             (let ((views (tuatara.defaults::org-views buffer 0 (tuatara.buffer:buffer-size buffer))))
               (true (every #'tuatara.defaults::org-view-current views))
               (true (every #'tuatara.defaults::org-view-root views))))
           (is = 0 parsed))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (org-internals block-specs-follow-the-files-open)
  ;; what language servers see of a note is worked out again when a file is opened, a block written to a
  ;; file open in a buffer being seen by the note's own name
  (let ((note (org-parts-buffer (format nil "#+begin_src python :tangle x.py~%x = 1~%#+end_src~%") nil)))
    (unwind-protect
         (let ((before (tuatara.defaults::org-block-specs-key note))
               (other (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "x.py"))))
           (unwind-protect
                (progn (setf (tuatara.buffer:buffer-file other) #p"/tmp/x.py")
                       (false (equal before (tuatara.defaults::org-block-specs-key note))))
             (tuatara.buffer:kill-buffer other)))
      (tuatara.buffer:kill-buffer note))))

(define-test (org-internals prose-ranges-kept-for-part-and-whole)
  ;; telling prose from the whole parse and from a part's, one after the other, works each out once
  (let* ((buffer (org-parts-buffer (org-parts-many 30) nil))
         (worked 0)
         (laid-out (fdefinition 'tuatara.defaults::org-laid-out-ranges)))
    (unwind-protect
         (progn
           (tuatara.defaults::set-major-mode buffer 'tuatara.defaults::org-mode)
           (with-functions-replaced ((tuatara.defaults::org-laid-out-ranges (lambda (&rest arguments)
                                                                               (incf worked)
                                                                               (apply laid-out arguments))))
             (dotimes (i 3)
               (funcall (tuatara.defaults::prose-test buffer) 20)
               (tuatara.defaults::org-prose-p buffer 20)))
           (true (<= worked 2) "worked out ~d times" worked))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (org-internals parts-found-in-time-with-their-number)
  ;; the parts of a note with many headlines and blocks are found in time growing with them alone: each part
  ;; looked through all the block lines of the text, 0.9 s for a note of 10 MB
  (flet ((seconds (count)
           (let ((buffer (org-parts-buffer (with-output-to-string (out)
                                             (dotimes (i count) (format out "* H~d~%#+begin_src lisp~%(f)~%#+end_src~%" i)))
                                           nil)))
             (unwind-protect
                  (loop repeat 2
                        minimize (progn
                                   (setf (tuatara.buffer:buffer-local buffer 'tuatara.defaults::org-parts) nil)
                                   (let ((start (get-internal-real-time)))
                                     (tuatara.defaults::org-buffer-parts buffer)
                                     (/ (- (get-internal-real-time) start) internal-time-units-per-second))))
               (tuatara.buffer:kill-buffer buffer)))))
    (let ((per-thousand (/ (- (seconds 32000) (seconds 8000)) 24)))
      (true (< per-thousand 5/1000) "~,2f ms for each thousand parts more" (* 1000 per-thousand)))))
