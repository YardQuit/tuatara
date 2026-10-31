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
  "An ORG-VIEW of all of BUFFER from its whole parse, as ORG-PARSE reads it."
  (multiple-value-bind (text returns) (tuatara.defaults::org-strip-returns (tuatara.buffer:buffer-string buffer))
    (tuatara.defaults::make-org-view (tuatara.defaults::org-parse text) text 0 (tuatara.buffer:buffer-size buffer)
                                     (if (tuatara.defaults::buffer-crlf-p buffer) returns nil) t)))

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
