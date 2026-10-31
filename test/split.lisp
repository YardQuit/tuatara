;;;; test/split.lisp --- Split (f) and cursors in a column (t T).
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test split
  :description "f holds every match of a regexp in the selection and i a c d w act at all of them, what is typed at one written at all; t and T make cursors in a column, where motions, selections, typing, deleting, copying and pasting happen at every cursor.")

(defun split-text (keys text)
  "The text after KEYS are typed on TEXT."
  (session-text (nth-value 1 (run-keys keys :text text :width 60 :height 10))))

(defun split-echo (keys text)
  "The echo row after KEYS are typed on TEXT."
  (tuatara.frontend.fake:fake-screen-line (run-keys keys :text text :width 60 :height 10) 9))

(define-test (split split-verbs)
  ;; i types before every match, a after, c empties them first, d deletes them
  (is string= "Xfoo bar Xfoo" (split-text "f f o o RET i X C-g" "foo bar foo"))
  (is string= "foo! bar foo!" (split-text "f f o o RET a ! C-g" "foo bar foo"))
  (is string= "baz bar baz" (split-text "f f o o RET c b a z C-g" "foo bar foo"))
  (is string= " bar " (split-text "f f o o RET d" "foo bar foo"))
  ;; what c and d take is one kill: one copy where the matches agree, one a line where not
  (is string= "foo" (first (tuatara.command:editor-kill-ring (nth-value 1 (run-keys-in-turns '("f f o o RET" "d") :text "foo bar foo")))))
  (is string= (format nil "1~%22~%333")
      (first (tuatara.command:editor-kill-ring (nth-value 1 (run-keys-in-turns '("f [ 0 - 9 ] + RET" "d") :text "1 22 333")))))
  ;; matches that differ: a types at each one's end
  (is string= "1x 22x 333x" (split-text "f [ 0 - 9 ] + RET a x C-g" "1 22 333"))
  ;; the report says what was done
  (true (search "Split: typed before 2 places" (split-echo "f f o o RET i X C-g" "foo bar foo")))
  (true (search "Split: deleted 2 places" (split-echo "f f o o RET d" "foo bar foo")))
  ;; the writing is one change for undo
  (is string= "foo bar foo" (split-text "f f o o RET i X Y C-g u" "foo bar foo"))
  ;; DEL at a place's start takes the character before every place, the same one
  (is string= "1 1" (split-text "f 1 RET i DEL C-g" "a1 a1")))

(define-test (split escape-in-a-split)
  ;; Escape does in a split what it does in INSERT state elsewhere: with
  ;; *escape-quits* it ends the split as C-g does, while typing or waiting for a verb
  (let ((tuatara-user:*escape-quits* t))
    (is string= "N bar N" (split-text "f f o o RET c N ESC" "foo bar foo"))
    (true (search "Split: changed 2 places" (split-echo "f f o o RET c N ESC" "foo bar foo")))
    (true (search "Split ended: 2 places left alone" (split-echo "f f o o RET ESC" "foo bar foo"))))
  ;; as standard Escape is itself there as in INSERT state anywhere: the typing goes on
  (let ((tuatara-user:*escape-quits* nil))
    (is string= "NM bar NM" (split-text "f f o o RET c N ESC M C-g" "foo bar foo"))
    (is string= (run-keys-text "i N ESC M C-g") (run-keys-text "i N M C-g"))))

(define-test (split deletions-beside-places)
  ;; a deletion past a place's edge is made at the others only where the very
  ;; same text is there; else the split ends, the deletion made at the one place
  (let ((text (format nil "a foo bar~%c foo bazooka")))
    (is string= (format nil "a foo~%c foo bazooka") (split-text "% f f o o RET a C-k C-g" text))
    (true (search "Split ended: the text deleted beside this place differs at another"
                  (split-echo "% f f o o RET a C-k" text)))
    (is string= (format nil "a foor~%c foozooka") (split-text "% f f o o RET a M-z r C-g" text))
    ;; each C-d deleting the same character at both, until one does not
    (is string= (format nil "a foo~%c foozooka") (split-text "% f f o o RET a C-d C-d C-d C-d C-g" text))
    (is string= (format nil "a foobar~%c foo b  zooka") (split-text "% f f o o RET a M-\\ C-g" (format nil "a foo   bar~%c foo b  zooka")))
    (is string= (format nil "a foo~%c bazookafoo") (split-text "% f f o o RET i DEL DEL DEL DEL C-g" (format nil "a bar foo~%c bazooka foo")))
    (is string= "1 b1" (split-text "f 1 RET i DEL C-g" "a1 b1"))
    ;; what came before the deletion stays written at every place, one change for undo with it
    (is string= (format nil "a fooX~%c fooX bazooka") (split-text "% f f o o RET a X C-k C-g" text))
    (is string= text (split-text "% f f o o RET a X C-k C-g u" text))
    ;; the same text there: deleted at every place
    (is string= (format nil "a foo~%c foo") (split-text "% f f o o RET a C-k C-g" (format nil "a foo bar~%c foo bar")))
    (is string= (format nil "a foobar~%c foobar") (split-text "% f f o o RET a M-\\ C-g" (format nil "a foo   bar~%c foo   bar"))))
  ;; past the limit, a match out of sight without the same text beside it is left as it is
  (multiple-value-bind (f editor)
      (let ((tuatara-user:*split-limit* 2))
        (run-keys-in-turns '("% f a RET i DEL X C-g" "") :text "xa xa ya xa" :width 120 :height 10))
    (is string= "Xa Xa ya Xa" (session-text editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "1 left as they were, the text beside them not the same")))
  ;; and one come into sight while typing is let go, the others written
  (let ((tuatara-user:*split-limit* 4)
        (tuatara.defaults::*split-sight-octets* 16)
        (filler (loop for i below 300 collect i)))
    (let ((lines (uiop:split-string (session-text (nth-value 1 (run-keys-in-turns '("% f a RET" "i DEL X M->" "C-g" "")
                                                                                   :text (format nil "xa xa~%~{z~d~%~}xa~%~{z~d~%~}xa ya" filler filler)
                                                                                   :width 80 :height 10)))
                                    :separator '(#\Newline))))
      (is string= "Xa Xa" (first lines))
      (is string= "Xa" (nth 301 lines))
      (is string= "Xa ya" (car (last lines))))))

(defun split-file-session (keys text &key (name "split.txt"))
  "Type KEYS on a file holding TEXT; return the buffer's text, the file's, and
whether the buffer has changes not saved; the frontend fourth."
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames name dir) text)))
      (multiple-value-bind (f editor) (run-keys keys :file path :width 100 :height 10)
        (let ((b (session-buffer editor)))
          (unwind-protect
               (values (session-text editor) (file-text path) (tuatara.buffer:buffer-modified-p b) f)
            (tuatara.buffer:kill-buffer b)))))))

(define-test (split split-read-again)
  ;; the file read again while typing at a split: the split ends first, what was
  ;; typed one step for undo and the reading again one of its own
  (let ((text (format nil "foo bar foo baz~%"))
        (keys "f f o o RET a X M-x r e v e r t - b u f f e r RET y RET Y"))
    (multiple-value-bind (now file modified f) (split-file-session keys text)
      (declare (ignore file modified))
      (is = 1 (count #\Y now))
      (false (find #\X now))
      (true (tuatara.frontend.fake:fake-frame-showing f "Read split.txt again; the split typed at ended first")))
    (is string= (format nil "fooX bar fooX baz~%") (split-file-session (format nil "~a C-g u u" keys) text))
    (is string= text (split-file-session (format nil "~a C-g u u u" keys) text))))

(define-test (split split-saved-whole)
  ;; not saved while typing at a split, nor while it writes in the background
  (let ((text (format nil "foo bar foo baz~%")))
    (multiple-value-bind (now file modified f) (split-file-session "f f o o RET a X C-x C-s" text)
      (is string= (format nil "fooX bar fooX baz~%") now)
      (is string= text file)
      (true modified)
      (true (tuatara.frontend.fake:fake-frame-showing f "Not saved: the split is still being typed at")))
    (multiple-value-bind (now file modified) (split-file-session "f f o o RET a X C-g C-x C-s" text)
      (is string= now file)
      (false modified)))
  (with-test-directory (dir)
    (let* ((text (format nil "~{one two ~d~%~}" (loop for i below 20 collect i)))
           (path (put-file (merge-pathnames "s.txt" dir) text))
           (f (tuatara.frontend.fake:make-fake-frontend :width 100 :height 10))
           (release nil)
           (during '()))
      (let ((tuatara-user:*split-limit* 2)
            (tuatara.defaults::*rewrite-piece* 16)
            (tuatara.defaults::*rewrite-piece-hook* (lambda () (loop until release do (sleep 0.01)))))
        (let ((scratch (tuatara.buffer:find-buffer "*scratch*"))) (when scratch (tuatara.buffer:kill-buffer scratch)))
        (tuatara.buffer:make-buffer "*scratch*" :text "")
        (tuatara.frontend.fake:fake-queue-keys f "% f o RET i X C-g C-x C-s")
        (tuatara.frontend.fake:fake-queue-action
         f (lambda ()
             (setf during (list (file-text path) (tuatara.buffer:buffer-modified-p (tuatara.command:current-buffer)))
                   release t)
             (loop while (find-if #'tuatara.runtime:job-running-p tuatara.runtime:*jobs*)
                   do (sleep 0.01))))
        (tuatara.frontend.fake:fake-queue-keys f "")
        (tuatara.frontend.fake:fake-queue-keys f "C-x C-s")
        (let* ((editor (tuatara.command:run-editor f :files (list path) :setup #'tuatara.defaults:install-defaults
                                                     :buffer (tuatara.buffer:find-buffer "*scratch*")))
               (b (session-buffer editor)))
          (unwind-protect
               (progn
                 (true (tuatara.frontend.fake:fake-frame-showing f "Not saved: the split is still writing at the other matches of o"))
                 (is string= text (first during))
                 (true (second during))
                 ;; once done, saved whole
                 (loop while (tuatara.io:saves-running) do (sleep 0.01))
                 (is = 20 (count-if (lambda (line) (search "Xone twXo" line))
                                    (uiop:split-string (file-text path) :separator '(#\Newline)))))
            (tuatara.buffer:kill-buffer b)))))))

(define-test (split split-wrapping)
  ;; a delimiter wraps every place, the split staying for more
  (is string= "(foo) bar (foo)" (split-text "f f o o RET ( C-g" "foo bar foo"))
  (is string= "([foo]) bar ([foo])" (split-text "f f o o RET ( [ C-g" "foo bar foo"))
  ;; and takes the pair off where it stands outside every place
  (is string= "foo bar foo" (split-text "f f o o RET ( ( C-g" "foo bar foo"))
  ;; then a verb
  (is string= "(foo!) bar (foo!)" (split-text "f f o o RET ( a ! C-g" "foo bar foo"))
  ;; asked for the delimiter, C-g gives up: nothing wrapped
  (is string= "foo bar foo" (split-text "f f o o RET w C-g C-g" "foo bar foo")))

(define-test (split split-scope)
  ;; with nothing selected, the line at point
  (is string= (format nil "Xa Xa~%a a") (split-text "f a RET i X C-g" (format nil "a a~%a a")))
  ;; the selection
  (is string= (format nil "Xa Xa~%Xa a") (split-text "v j l f a RET i X C-g" (format nil "a a~%a a")))
  ;; %regexp: the whole buffer
  (is string= (format nil "Xa Xa~%Xa Xa") (split-text "f % a RET i X C-g" (format nil "a a~%a a")))
  ;; empty matches: ^ holds the start of every line
  (is string= (format nil "#a~%#b~%#c") (split-text "% f ^ RET i # C-g" (format nil "a~%b~%c")))
  ;; matches that touch, none, or nothing to split on
  (true (search "Matches touch on line 1 (aa): a split needs a character between places" (split-echo "f a RET" "aaa")))
  (true (search "Matches touch on line 2 (xyxy)" (split-echo "% f x y RET" (format nil "ab~%xyxy"))))
  (true (search "Nothing matched" (split-echo "f z RET" "aaa")))
  ;; moving about keeps it waiting for its verb; any other key ends it, doing its own job
  (is string= (format nil "Xfoo Xfoo~%x") (split-text "f f o o RET j k l i X C-g" (format nil "foo foo~%x")))
  (true (search "Split ended: 2 places left alone" (split-echo "f f o o RET C-d" (format nil "foo foo~%x"))))
  (is string= (format nil "oo foo~%x") (split-text "f f o o RET C-d" (format nil "foo foo~%x")))
  (true (search "Split ended: 2 places left alone" (split-echo "f f o o RET v" (format nil "foo foo~%x"))))
  ;; point moving into another place writes there
  (is string= "fooXY bar fooXY" (split-text "f f o o RET a X C-e Y C-g" "foo bar foo")))

(define-test (split split-typing-in-sight)
  ;; typing at a split of matches writes each key where it can be seen, the
  ;; places out of sight once the typing ends: one change for undo all the same
  ;; (the keys after f are typed once its search in the background is done, as a person's would be)
  (let ((tuatara.defaults::*split-sight-octets* 16)
        (text (format nil "~{foo ~d~%~}" (loop for i below 300 collect i))))
    (flet ((lines (editor) (uiop:split-string (session-text editor) :separator '(#\Newline))))
      ;; still typing: those in sight written, the far ones not yet
      (multiple-value-bind (f editor) (run-keys-in-turns '("% f f o o RET" "i X Y") :text text :width 80 :height 10)
        (is string= "XYfoo 0" (first (lines editor)))
        (is string= "foo 299" (nth 299 (lines editor)))
        (true (tuatara.frontend.fake:fake-frame-showing f "those out of sight as you finish")))
      ;; the typing over: every place
      (let ((editor (nth-value 1 (run-keys-in-turns '("% f f o o RET" "i X Y C-g") :text text :width 80 :height 10))))
        (true (every (lambda (line) (or (string= line "") (string= "XYfoo" line :end2 5))) (lines editor))))
      ;; and one u takes it all back
      (is string= text (session-text (nth-value 1 (run-keys-in-turns '("% f f o o RET" "i X Y C-g u") :text text :width 80 :height 10))))
      ;; a deletion past the places' edges is written at every place at once
      (let ((editor (nth-value 1 (run-keys-in-turns '("% f o o SPC RET" "i DEL C-g") :text text :width 80 :height 10))))
        (is string= "oo 0" (first (lines editor)))
        (is string= "oo 299" (nth 299 (lines editor))))
      (let ((editor (nth-value 1 (run-keys-in-turns '("% f [ 0 - 9 ] + RET" "i DEL Z C-g") :text text :width 80 :height 10))))
        (is string= "fooZ0" (first (lines editor)))
        (is string= "fooZ299" (nth 299 (lines editor))))
      ;; moving into another place while typing: every place brought up first
      (let* ((text (format nil "foo foo~%~{foo ~d~%~}" (loop for i below 300 collect i)))
             (editor (nth-value 1 (run-keys-in-turns '("% f f o o RET" "a X C-e Y C-g") :text text :width 80 :height 10))))
        (is string= "fooXY fooXY" (first (lines editor)))
        (is string= "fooXY 299" (nth 300 (lines editor)))))))

(define-test (split split-limit)
  ;; more matches than *split-limit*: those in sight held, as many as it allows,
  ;; counted as far as one past it, as the cursors are
  (let ((tuatara-user:*split-limit* 2))
    (true (search "Split: 2 places of 3 in the selection, those out of sight written in the background"
                  (split-echo "% f o RET" "one two three four")))
    (true (search "Split: 2 places of more than 2 in the selection" (split-echo "% f o RET" "one two three four o")))
    ;; the empty matches count too
    (true (search "Split: 2 places of 3" (split-echo "% f ^ RET" (format nil "a~%b~%c"))))
    ;; as many as it allows are held, as ever
    (is string= "Xone twXo" (split-text "% f o RET i X C-g" "one two")))
  ;; NIL: no limit but the memory's
  (let ((tuatara-user:*split-limit* nil))
    (is string= "Xa Xa Xa" (split-text "% f a RET i X C-g" "a a a"))
    (let ((tuatara.defaults::*split-budget* (* 2 tuatara.defaults::+split-place-octets+)))
      (true (search "Split: 2 places of 3 in the selection" (split-echo "% f a RET" "a a a"))))))

(define-test (split sight-looked-at-once)
  ;; past its limit, a split looks for the places in sight again only when the text, its
  ;; places or what is in sight changed, not after every command
  (let ((tuatara-user:*split-limit* 5) (looks nil))
    (run-keys (list "% f o RET"
                    (lambda ()
                      (sb-int:encapsulate 'tuatara.defaults::virtual-spans 'counted
                                          (lambda (function &rest arguments) (push t looks) (apply function arguments)))
                      (unwind-protect
                           (dotimes (i 3) (tuatara.defaults::virtual-fill tuatara.defaults::*split*))
                        (sb-int:unencapsulate 'tuatara.defaults::virtual-spans 'counted))))
              :text (format nil "o~%o~%o~%~a~%o o o~%" (make-string 400000 :initial-element #\x)) :width 100 :height 10)
    (is = 0 (length looks))))

(define-test (split spans-met)
  ;; a stretch meets a span it touches at either end, whether a point or longer
  (let ((spans (vector '(2 . 4) '(8 . 10))))
    (is equal '(nil t t t nil nil)
        (mapcar (lambda (stretch) (tuatara.defaults::spans-meet-p spans (car stretch) (cdr stretch)))
                '((5 . 7) (4 . 4) (7 . 8) (0 . 20) (11 . 12) (0 . 1))))
    (false (tuatara.defaults::spans-meet-p #() 0 5))))

(defun background-split (turns text)
  "Type TURNS on TEXT, each once the work in the background the one before
began is done, with every split's search going to the background in pieces of
8 octets; return the frontend and the editor."
  (let ((tuatara.defaults::*rewrite-threshold* 0)
        (tuatara.defaults::*rewrite-piece* 8))
    (run-keys-in-turns turns :text text :width 100 :height 10)))

(defun every-match-text (turns text &key (limit 2) background)
  "The text after TURNS typed on TEXT, each once the work in the background the
one before began is done, a split holding at most LIMIT places; with
BACKGROUND, the search too goes to the background."
  (let ((tuatara-user:*split-limit* limit)
        (tuatara.defaults::*rewrite-threshold* (if background 0 tuatara.defaults::*rewrite-threshold*)))
    (session-text (nth-value 1 (run-keys-in-turns (append turns '("")) :text text :width 120 :height 10)))))

(define-test (split every-match)
  ;; past what a split holds, the places in sight are typed at and the others
  ;; written in the background as the verb is done
  (let ((text "one two three four"))
    (is string= "Xone twXo three fXour" (every-match-text '("% f o RET i X C-g") text))
    (is string= "oXne twoX three foXur" (every-match-text '("% f o RET a X C-g") text))
    (is string= "0ne tw0 three f0ur" (every-match-text '("% f o RET c 0 C-g") text))
    (is string= "ne tw three fur" (every-match-text '("% f o RET d") text))
    (is string= "(o)ne tw(o) three f(o)ur" (every-match-text '("% f o RET w (" "C-g") text))
    (is string= "[o]ne tw[o] three f[o]ur" (every-match-text '("% f o RET [" "C-g") text))
    ;; a wrap key again takes the pair off, out of sight too
    (is string= text (every-match-text '("% f o RET (" "(" "C-g") text))
    ;; a deletion just before every place, and after
    (is string= "Xne twX three fXur" (every-match-text '("% f o RET c DEL X C-g") "-one tw-o three f-our"))
    (is string= "o+ne two+ three fo+ur" (every-match-text '("% f o RET a C-d + C-g") "o-ne two- three fo-ur"))
    ;; one change for undo, in sight and out of it
    (is string= text (every-match-text '("% f o RET d" "u") text))
    (is string= text (every-match-text '("% f o RET i X C-g" "u") text))
    ;; moving about keeps it waiting; any other key ends it, doing its own job
    (is string= "one two three four" (every-match-text '("% f o RET j") text))
    ;; the empty matches, not on the empty line after the last newline
    (is string= (format nil "#a~%#b~%#c~%") (every-match-text '("% f ^ RET i # C-g") (format nil "a~%b~%c~%")))
    ;; the search in the background holds those in sight as it ends
    (is string= "ne tw three fur" (every-match-text '("% f o RET" "d") text :background t))))

(define-test (split rectangles-read-as-they-come)
  ;; a rectangle over more than *rewrite-threshold* is searched a row at a time,
  ;; in the background: the matches in its columns only
  (let ((text (format nil "ab ab~%ab ab~%ab ab~%")))
    (let ((tuatara.defaults::*rewrite-threshold* 0))
      (is string= (format nil "ab Xab~%ab Xab~%ab Xab~%")
          (session-text (nth-value 1 (run-keys-in-turns '("l l l m v j j l f a RET" "i X C-g") :text text :width 80 :height 10)))))
    ;; and past what a split holds, every match of them
    (is string= (format nil "ab b~%ab b~%ab b~%")
        (every-match-text '("l l l m v j j l f a RET" "y d") text :background t))
    ;; over more than a thousand rows, the job asked now and then whether to stop,
    ;; and walked again from the middle of a row as it is taken in
    (let ((many (format nil "~{~a~%~}" (loop repeat 1100 collect "ab ab"))))
      (is string= (format nil "~{~a~%~}" (loop repeat 1100 collect "ab b"))
          (every-match-text '("l l l m v g e k l l l l f a RET" "y d") many :background t)))))

(define-test (split pieces-between-stretches)
  ;; the pieces of a scope of several stretches cover the text between them too,
  ;; searched for nothing, so that the text written in their place is whole
  (let* ((rope (tuatara.text:octets->rope (tuatara.text:string-octets "foo foo foo foo")))
         (groups (tuatara.defaults::scope-groups rope '((0 . 3) (8 . 11))))
         (pieces (loop for group in groups append (cdr group))))
    (is equal '((0 . 3) nil (8 . 11)) (mapcar #'car groups))
    (is = 0 (tuatara.defaults::rewrite-piece-start (first pieces)))
    (is = 11 (tuatara.defaults::rewrite-piece-end (car (last pieces))))
    (true (loop for (a b) on pieces while b
                always (= (tuatara.defaults::rewrite-piece-end a) (tuatara.defaults::rewrite-piece-start b))))
    (let ((found '()))
      (dolist (piece pieces)
        (tuatara.defaults::piece-matches (lambda (s e m g) (declare (ignore m g)) (push (cons s e) found) nil)
                                         rope (tuatara.defaults::make-pattern-matcher "foo" :regex t :compiled t) '((0 . 3) (8 . 11)) groups
                                         (tuatara.defaults::rewrite-piece-start piece) (tuatara.defaults::rewrite-piece-end piece)
                                         (constantly nil)))
      (is equal '((0 . 3) (8 . 11)) (reverse found)))))

(define-test (split seams-in-the-background)
  ;; in pieces of 8 octets, a pattern whose matches run over line ends and past a piece's end
  ;; holds the same places, and writes at every other match the same, as one scan finds them
  (let ((text (format nil "a1  b~%~%~%2c d   ~%  e~%~%f3~%")))
    (dolist (pattern '("\\s+" "\\n\\n+" "[^a]+" "\\D" "b[\\s\\S]*?e"))
      (let ((turns (list (format nil "% f ~a RET" (key-string pattern)) "i X C-g")))
        (is string= (session-text (nth-value 1 (run-keys-in-turns turns :text text :width 100 :height 10)))
            (session-text (nth-value 1 (background-split turns text)))
            "~a held" pattern))
      (let ((tuatara.defaults::*rewrite-piece* 8))
        (is string= (cl-ppcre:regex-replace-all pattern text "<\\&>")
            (every-match-text (list (format nil "% f ~a RET" (key-string pattern)) "w <" "C-g") text :limit 1 :background t)
            "~a written" pattern)))))

(define-test (split split-in-the-background)
  (let ((text (format nil "foo bar~%foo baz~%foo~%")))
    ;; a large text is searched in the background, the mode line saying how far,
    ;; and the split stands when the search ends; each piece takes a moment, so that
    ;; a frame is drawn while it goes on
    (multiple-value-bind (f editor) (let ((tuatara.defaults::*rewrite-piece-hook* (lambda () (sleep 0.05))))
                                      (background-split '("% f f o o RET" "i X C-g") text))
      (true (tuatara.frontend.fake:fake-frame-showing f "Searching the selection for foo in the background"))
      (true (processing-shown f "TUATARA["))
      (false (tuatara.frontend.fake:fake-frame-showing f "TUATARA[" "Searching"))
      (true (tuatara.frontend.fake:fake-frame-showing f "Split: 3 places in the selection"))
      (is string= (format nil "Xfoo bar~%Xfoo baz~%Xfoo~%") (session-text editor)))
    ;; the empty matches, found by the pieces at their starts and at the end
    (is string= (format nil "#foo bar~%#foo baz~%#foo~%")
        (session-text (nth-value 1 (background-split '("% f ^ RET" "i # C-g") text))))
    (is string= (format nil "foo bar;~%foo baz;~%foo;~%")
        (session-text (nth-value 1 (background-split '("% f $ RET" "i ; C-g") text))))
    ;; nothing found, and too many
    (true (tuatara.frontend.fake:fake-frame-showing (background-split '("% f q u x RET" "") text) "Nothing matched qux"))
    (let ((tuatara-user:*split-limit* 2))
      (multiple-value-bind (f editor) (background-split '("% f f o o RET" "i X C-g" "") text)
        (true (tuatara.frontend.fake:fake-frame-showing f "Split: 2 places of more than 2 in the selection"))
        (is string= (format nil "Xfoo bar~%Xfoo baz~%Xfoo~%") (session-text editor))))
    ;; with no limit, still no more than the memory has room for
    (let ((tuatara-user:*split-limit* nil)
          (tuatara.defaults::*split-budget* (* 2 tuatara.defaults::+split-place-octets+)))
      (true (tuatara.frontend.fake:fake-frame-showing (background-split '("% f f o o RET" "") text)
                                                      "Split: 2 places of more than 2 in the selection")))
    ;; the text changed while it searched, or the buffer was left: nothing held
    (let ((tuatara.defaults::*rewrite-piece-hook* (lambda () (sleep 0.2))))
      (multiple-value-bind (f editor) (background-split '("% f f o o RET i Y C-g" "j") text)
        (true (tuatara.frontend.fake:fake-frame-showing f "Not held: *scratch* changed while f searched it"))
        (is string= (format nil "Yfoo bar~%foo baz~%foo~%") (session-text editor)))
      (true (tuatara.frontend.fake:fake-frame-showing
             (background-split '("% f f o o RET C-x b o t h e r C-j" "") text)
             "Not held: *scratch* was left while f searched it"))
      ;; a verb typed while it searches acts at the cursor, never on the whole selection searched
      (multiple-value-bind (f editor) (background-split '("% f f o o RET c Q W C-g" "") text)
        (declare (ignore f))
        (true (search "foo baz" (session-text editor)) "~s" (session-text editor)))
      ;; M-x stop-job stops it, after asking
      (true (tuatara.frontend.fake:fake-frame-showing
             (background-split '("% f f o o RET M-x s t o p - j o b RET y RET" "") text)
             "Stopped: nothing held")))
    ;; moving about meanwhile: the split comes all the same, point left where it
    ;; was moved, and stays through more moving about; the verbs go to the places
    (multiple-value-bind (f editor) (background-split '("% f f o o RET j l" "j i X C-g") text)
      (true (tuatara.frontend.fake:fake-frame-showing f "Split: 3 places in the selection"))
      (is string= (format nil "Xfoo bar~%Xfoo baz~%Xfoo~%") (session-text editor)))
    (multiple-value-bind (f editor) (background-split '("% f f o o RET j l" "") text)
      (declare (ignore f))
      (is = 9 (session-point editor)))
    ;; a split armed between commands ends with the first key that is no move and not its own
    (multiple-value-bind (f editor) (background-split '("% f f o o RET" "C-d") text)
      (true (tuatara.frontend.fake:fake-frame-showing f "Split ended: 3 places left alone"))
      (is string= (format nil "oo bar~%foo baz~%foo~%") (session-text editor)))))

(define-test (split split-cursors)
  (let ((three (format nil "abc~%abc~%abc")))
    ;; t adds a cursor below at the column, and keys act at every cursor
    (is string= (format nil "ac~%ac~%ac") (split-text "l t t d" three))
    (is string= (format nil "aXbc~%aXbc~%aXbc") (split-text "l t t i X C-g" three))
    (is string= (format nil "abc!~%abc!~%abc!") (split-text "t t A ! C-g" three))
    ;; T adds above
    (is string= (format nil "Xabc~%Xabc~%abc") (split-text "j T I X C-g" three))
    ;; the column is the one on the screen, as moving a line down keeps it: a wide
    ;; character two, a tab to its stop; never inside a character's combining marks
    (is string= (format nil "あいWう~%abcdWefgh~%") (split-text "l l t i W C-g" (format nil "あいう~%abcdefgh~%")))
    (is string= (format nil "a~aWb~%abcdWefghijk~%" #\Tab) (split-text "l l t i W C-g" (format nil "a~ab~%abcdefghijk~%" #\Tab)))
    (let ((marks (format nil "e~a~a" (code-char #x301) (code-char #x301))))
      (is string= (format nil "~axyWz~%abcWdefgh~%abcWdefgh~%" marks)
          (split-text "j j l l l T T i W C-g" (format nil "~axyz~%abcdefgh~%abcdefgh~%" marks)))
      (is string= (format nil "~aWxyz~%aWbcdefgh~%" marks)
          (split-text "l t i W C-g" (format nil "~axyz~%abcdefgh~%" marks))))
    ;; lines selected take the screen column too
    (is string= (format nil "あいWう~%abcdWefgh~%") (split-text "l l v j t i W C-g" (format nil "あいう~%abcdefgh~%")))
    ;; DEL takes back the cursor added last
    (is string= (format nil "bc~%bc~%abc") (split-text "t t DEL d" three))
    ;; a V selection gives a cursor on every line
    (is string= (format nil "#abc~%#abc~%#abc") (split-text "V j j t i # C-g" three))
    ;; selections at every cursor, and one kill for what they take
    (multiple-value-bind (f editor) (run-keys "t t v l l d" :text three :width 60 :height 10)
      (declare (ignore f))
      (is string= (format nil "c~%c~%c") (session-text editor))
      (is string= "ab" (first (tuatara.command:editor-kill-ring editor))))
    ;; y then p puts back at each cursor
    (is string= (format nil "abcabc~%abcabc~%abcabc") (split-text "t t m w y g l p" three))
    ;; o opens a line below every cursor
    (is string= (format nil "abc~%X~%abc~%X") (split-text "t o X C-g" (format nil "abc~%abc")))
    ;; u keeps the cursors
    (is string= (format nil "bc~%bc~%bc") (split-text "t t d d u" three))
    ;; = lines the cursors up under the real one
    (is string= (format nil "aXbc~%aXbc") (split-text "t h l = i X C-g" (format nil "abc~%abc")))
    ;; f from the cursors searches each cursor's line
    (is string= (format nil "ac~%ac~%ac") (split-text "t t f b RET d" three))
    ;; a key the cursors do not have ends them, and does its own job
    (multiple-value-bind (f editor) (run-keys "t j d" :text three :width 60 :height 10)
      (declare (ignore f))
      (is string= (format nil "abc~%bc~%abc") (session-text editor)))
    ;; not past the text
    (true (search "Only 2 lines below" (split-echo "C-u 3 t" three))))
  ;; the case keys at every cursor
  (is string= (format nil "ABC~%ABC") (split-text "t M-u" (format nil "abc~%abc")))
  (is string= (format nil "Don't go~%Don't go") (split-text "t M-c" (format nil "don't go~%don't go")))
  ;; a mark run at every cursor
  (let ((words (format nil "ab cd ef~%ab cd ef")))
    (is string= (format nil " ef~% ef") (split-text "t M w d" words))
    ;; u and U step every selection back and forward, as in a run at one cursor
    (is string= (format nil " cd ef~% cd ef") (split-text "t M w u d" words))
    (is string= (format nil " ef~% ef") (split-text "t M w u U d" words))
    ;; v refuses in a run, as at one cursor, keeping the selections
    (is string= (format nil " ef~% ef") (split-text "t M w v d" words))
    ;; C-g ends the run, keeping the cursors with their own keys: V selects
    ;; their lines, and u undoes rather than stepping a run
    (false (search "not run at the cursors" (split-echo "t M w C-g V" words)))
    (true (search "selecting" (split-echo "t M w C-g V" words)))
    (is string= (format nil "AB cd ef~%AB cd ef") (split-text "t M w C-g M-u" words))
    (is string= words (split-text "t M w C-g M-u u" words)))
  ;; the other cursors are drawn
  (let ((f (run-keys "l t" :text (format nil "abc~%abc") :width 60 :height 10)))
    (true (find :split-cursor (aref (tuatara.frontend.fake:fake-spans f) 1) :key #'third))))

(defun every-line-text (turns text &key (limit 2) crlf)
  "The text after TURNS typed on TEXT, each once the work in the background the
one before began is done, no more than LIMIT cursors made; CRLF has the buffer
end its lines with a carriage return and a newline."
  (let ((tuatara-user:*cursors-limit* limit))
    (session-text (nth-value 1 (run-keys-in-turns (append turns '("")) :text text :width 150 :height 10 :crlf crlf)))))

(define-test (split cursors-paste-pieces)
  (let ((four (format nil "aaa~%bbb~%ccc~%ddd~%")))
    ;; p over a line selected at every cursor replaces it as whole lines, as at one cursor
    (is string= (format nil "ddd~%ddd~%ddd~%ddd~%") (split-text "j j j V y g g t t V p" four))
    (is string= (format nil "aaa~%bbb~%aaa~%bbb~%aaa~%bbb~%ddd~%") (split-text "t V y C-g g g t t V p" four))
    ;; lines copied at the cursors go back one each
    (is string= four (split-text "t t V y V p" four))
    ;; a kill made at as many cursors gives each its own piece back, an empty one too
    (is string= four (split-text "t t D p" four))
    (is string= (format nil "aaa~%bbb~%~%ddd~%") (split-text "t t D p" (format nil "aaa~%bbb~%~%ddd~%")))
    (is string= (format nil "aaa~%~%ccc~%ddd~%") (split-text "t t D p" (format nil "aaa~%~%ccc~%ddd~%")))
    (is string= (format nil "zz~%zz~%zz~%ddd~%") (split-text "t t D p" (format nil "zz~%zz~%zz~%ddd~%")))
    ;; another kill with as many lines as there are cursors: a line to each, as DONKEY pastes
    (is string= four (split-text "V j j y g g t t V p" four))
    ;; with another number of lines, the whole kill at every cursor
    (is string= (format nil "aaa~%bbb~%aaa~%bbb~%aaa~%bbb~%ddd~%")
        (split-text "V j y g g t t V p" four))))

(define-test (split cursors-past-the-limit)
  ;; more lines selected than cursors are made for: the cursors in sight stand,
  ;; as many as the limit, and the others are written in the background as each
  ;; verb is done, at the cursors' column, a shorter line at its end
  (let ((text (format nil "abc~%abc~%a~%abc~%")))
    (is string= (format nil "aXbc~%aXbc~%aX~%aXbc~%") (every-line-text '("l v j j j t i X C-g") text))
    (is string= (format nil "abXc~%abXc~%aX~%abXc~%") (every-line-text '("l v j j j t a X C-g") text))
    (is string= (format nil "Xabc~%Xabc~%Xa~%Xabc~%") (every-line-text '("l v j j j t I X C-g") text))
    (is string= (format nil "abcX~%abcX~%aX~%abcX~%") (every-line-text '("l v j j j t A X C-g") text))
    (is string= (format nil "aXc~%aXc~%aX~%aXc~%") (every-line-text '("l v j j j t c X C-g") text))
    (is string= (format nil "ac~%ac~%a~%ac~%") (every-line-text '("l v j j j t d") text))
    ;; T the same; a visual-line selection at each line's start
    (is string= (format nil "aXbc~%aXbc~%aX~%aXbc~%") (every-line-text '("l v j j j T i X C-g") text))
    (is string= (format nil "#abc~%#abc~%#a~%#abc~%") (every-line-text '("V j j j t i # C-g") text))
    ;; a deletion just before the cursors, written out of sight too
    (is string= (format nil "Xbc~%Xbc~%X~%Xbc~%") (every-line-text '("l v j j j t i DEL X C-g") text))
    ;; the cursors stay for the next verb, at the column the typing left them
    (is string= (format nil "aXYbc~%aXYbc~%aXY~%aXYbc~%") (every-line-text '("l v j j j t i X C-g" "i Y C-g") text))
    ;; one change for undo, in sight and out of it
    (is string= text (every-line-text '("l v j j j t d" "u") text))
    (is string= text (every-line-text '("l v j j j t i X C-g" "u") text))
    ;; lines ending with a carriage return and a newline
    (let ((crlf (format nil "abc~c~%a~c~%abc~c~%" #\Return #\Return #\Return)))
      (is string= (format nil "abcX~c~%aX~c~%abcX~c~%" #\Return #\Return #\Return)
          (every-line-text '("V j j t A X C-g") crlf :crlf t))
      (is string= (format nil "ac~c~%a~c~%ac~c~%" #\Return #\Return #\Return)
          (every-line-text '("l v j j t d") crlf :crlf t))
      ;; a deletion back over the line end at every cursor: the carriage return and newline one step
      (is string= (format nil "aabbccdd~c~%" #\Return)
          (every-line-text '("j t t I DEL C-g") (format nil "aa~c~%bb~c~%cc~c~%dd~c~%" #\Return #\Return #\Return #\Return)
                           :crlf t :limit 5)))
    ;; what the echo area says; any key the cursors past the limit do not have ends them
    (let ((tuatara-user:*cursors-limit* 2))
      (true (tuatara.frontend.fake:fake-frame-showing (run-keys "l v j j j t" :text text :width 150 :height 10)
                                                      "Split: 2 cursors of 4, those out of sight written in the background")))
    (is string= text (every-line-text '("l v j j j t j") text))
    ;; a rectangle whose corner is at a line's start: that line is one of its rows, and gets a cursor
    (is string= (format nil "Xabcd~%Xabcd~%Xabcd~%abcd") (split-text "l l l m v j j g h t i X C-g" (format nil "abcd~%abcd~%abcd~%abcd")))
    ;; a negative count: moves out of sight as in sight; refused where those out of sight would differ
    (is string= (format nil "Xabc~%Xabc~%Xa~%Xabc~%") (every-line-text '("l l v j j j t C-u - 2 l i X C-g") text))
    (is string= text (every-line-text '("l v j j j t C-u - 1 d") text))
    ;; no more lines than the limit: cursors, as ever
    (is string= (format nil "aXbc~%aXbc~%a~%abc~%") (every-line-text '("l v j t i X C-g") text))
    ;; a count past the limit holds those in sight too, below or above, taking
    ;; in the cursors already made when they stand alone on lines one after another
    (is string= (format nil "aXbc~%aXbc~%aX~%aXbc~%") (every-line-text '("l C-u 3 t i X C-g") text))
    (is string= (format nil "aXbc~%aXbc~%aX~%aXbc~%") (every-line-text '("j j j l C-u 3 T i X C-g") text))
    (is string= (format nil "aXbc~%aXbc~%aX~%aXbc~%") (every-line-text '("l t C-u 2 t i X C-g") text))
    (is string= (format nil "abc~%aXbc~%aX~%aXbc~%") (every-line-text '("j l C-u 2 t i X C-g") text))
    (let ((tuatara-user:*cursors-limit* 2))
      (true (tuatara.frontend.fake:fake-frame-showing (run-keys "l C-u 3 t" :text text :width 150 :height 10)
                                                      "Split: 2 cursors of 4, those out of sight written in the background"))
      ;; more than there are lines, and cursors that are selections, are refused
      (true (search "Only 3 lines below" (split-echo "C-u 5 t" text)))
      (true (search "No line above" (split-echo "C-u 5 T" text)))
      (true (search "4 cursors would pass *cursors-limit* (2)" (split-echo "l t m w C-u 2 t" text))))
    ;; with no limit, no more than the memory has room for
    (let ((tuatara-user:*cursors-limit* nil)
          (tuatara.defaults::*split-budget* (* 2 tuatara.defaults::+split-place-octets+)))
      (true (tuatara.frontend.fake:fake-frame-showing (run-keys "l v j j j t" :text text :width 150 :height 10)
                                                      "Split: 2 cursors of 4"))
      (true (tuatara.frontend.fake:fake-frame-showing (run-keys "l C-u 3 t" :text text :width 150 :height 10)
                                                      "Split: 2 cursors of 4")))
    ;; asked, not held in sight only: y makes every cursor
    (let ((tuatara-user:*split-limit-ask* t))
      (is string= (format nil "aXbc~%aXbc~%aX~%aXbc~%") (every-line-text '("l v j j j t y i X C-g") text))
      (is string= (format nil "aXbc~%aXbc~%aX~%aXbc~%") (every-line-text '("l v j j j t n i X C-g") text))
      (is string= (format nil "aXbc~%aXbc~%aX~%aXbc~%") (every-line-text '("l C-u 3 t y i X C-g") text))
      (is string= (format nil "aXbc~%aXbc~%aX~%aXbc~%") (every-line-text '("l C-u 3 t n i X C-g") text)))))

(define-test (split split-thorough)
  ;; f misses a match that needs text more than 4 KB past where the scan looks;
  ;; split-selection-thorough holds it, and writes there
  (let* ((filler (with-output-to-string (o) (dotimes (i 3000) (format o "y~%"))))
         (block (with-output-to-string (o) (dotimes (i 6000) (format o "x~%"))))
         (text (concatenate 'string filler "BEGIN" (string #\Newline) block "END" (string #\Newline)))
         (pattern (key-string "BEGIN[\\s\\S]*?END")))
    (true (search "Nothing matched" (split-echo (format nil "% f ~a RET" pattern) text)))
    (multiple-value-bind (f editor)
        (run-keys (format nil "% M-x ~a RET ~a RET c Z C-g" (key-string "split-selection-thorough") pattern) :text text :width 80 :height 10)
      (true (tuatara.frontend.fake:fake-frame-showing f "Thorough regex split on: "))
      (is string= (format nil "~aZ~%" filler) (session-text editor))))
  ;; how many matches the regexp has in what is split, before the prompt, as it is typed
  (dolist (case '(("% f o" "6      Regex split on: o") ("f o" "4      Regex split on: o") ("f % o" "6      Regex split on: %o")))
    (multiple-value-bind (f editor)
        (run-keys (list (first case) (lambda () (sleep 0.2)) :tick "C-g") :text (format nil "foo foo~%foo") :width 80)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f (second case)) "~a" (first case)))))

(define-test (split cursor-typing-in-sight)
  ;; typing at cursors writes each key where it can be seen, the cursors out
  ;; of sight once the typing ends: one change for undo all the same
  (let ((tuatara.defaults::*split-sight-octets* 16)
        (tuatara-user:*cursors-limit* 1000)
        (text (format nil "~{foo ~d~%~}" (loop for i below 300 collect i))))
    (flet ((lines (editor) (uiop:split-string (session-text editor) :separator '(#\Newline))))
      (multiple-value-bind (f editor) (run-keys "V g e t i X Y" :text text :width 80 :height 10)
        (is string= "XYfoo 299" (nth 299 (lines editor)))
        (is string= "foo 0" (first (lines editor)))
        (true (tuatara.frontend.fake:fake-frame-showing f "those out of sight as you finish")))
      (let ((editor (nth-value 1 (run-keys "V g e t i X Y C-g" :text text :width 80 :height 10))))
        (true (every (lambda (line) (or (string= line "") (string= "XYfoo" line :end2 5))) (lines editor))))
      (is string= text (session-text (nth-value 1 (run-keys "V g e t i X Y C-g u" :text text :width 80 :height 10)))))))

(define-test (split changing-case)
  ;; M-u M-l M-c change the case of words, moving over them
  (multiple-value-bind (f editor) (run-keys "M-u" :text "hello world")
    (declare (ignore f))
    (is string= "HELLO world" (session-text editor))
    (is = 5 (session-point editor)))
  (is string= "HELLO WORLD" (split-text "C-u 2 M-u" "hello world"))
  (is string= "hello World" (split-text "w M-c" "hello WORLD"))
  ;; a word stops at an apostrophe
  (is string= "Don'T stop" (split-text "M-c" "DON'T stop"))
  ;; a negative count changes the words before point, which stays
  (multiple-value-bind (f editor) (run-keys "w w C-u - 1 M-u" :text "hello world")
    (declare (ignore f))
    (is string= "hello WORLD" (session-text editor))
    (is = 11 (session-point editor)))
  ;; the region
  (is string= "HElLO" (split-text "v l l C-x C-u" "helLO"))
  (is string= "hello" (split-text "% C-x C-l" "HELLO")))

(define-test (split tutor)
  ;; every command the tutor names is a command
  (let ((names (cl-ppcre:all-matches-as-strings "\\\\\\[[a-z-]+\\]" tuatara.defaults::*tuatara-tutor-text*)))
    (true (plusp (length names)))
    (dolist (name (remove-duplicates names :test #'string=))
      (true (tuatara.command:find-command (subseq name 2 (1- (length name)))) "No command ~a" name)))
  (multiple-value-bind (f editor) (run-keys "M-x t u a t a r a - t u t o r RET" :text "" :width 100 :height 40)
    (let* ((b (session-buffer editor))
           (text (session-text editor)))
      (is string= "*Tutor*" (tuatara.buffer:buffer-name b))
      (is = 0 (search "Tuatara tutor" text))
      ;; the keys are the ones you have
      (true (search "h left    j down    k up    l right" text))
      (true (search "d   delete the character" text))
      (true (search "SPC i & asks for two keys" text))
      ;; nothing left to substitute, and nothing to undo or save
      (false (search "\\[" text))
      (false (search "\\`" text))
      (false (tuatara.buffer:buffer-modified-p b))
      (is eq nil (tuatara.buffer:buffer-undo-stack b))
      ;; keys, states and arrows in faces of their own
      (true (some (lambda (spans) (find :tutor-key spans :key #'third)) (tuatara.frontend.fake:fake-spans f)))
      (true (some (lambda (spans) (find :tutor-state spans :key #'third)) (tuatara.frontend.fake:fake-spans f)))))
  ;; coming back finds it as it was left
  (multiple-value-bind (f editor) (run-keys "M-x t u a t a r a - t u t o r RET d C-x b RET M-x t u a t a r a - t u t o r RET"
                                            :text "" :width 100 :height 40)
    (declare (ignore f))
    (is = 0 (search "uatara tutor" (session-text editor)))))

(define-test (split tutor-exercises)
  ;; M over a rectangle lets it go and marks the word at point
  (is string= "four" (selection-after "m v j l M" (format nil "one two three~%four five six")))
  ;; % in a mark run selects the whole text, and the run goes on
  (multiple-value-bind (f editor) (run-keys "M %" :text "one two")
    (declare (ignore f))
    (is eq tuatara.defaults::*mark-run-keymap* (tuatara.command:editor-transient-map editor)))
  ;; a run adopts a v selection, and w grows it to the end of its word
  (is string= "beta" (selection-after "w l v l l l M w" "alpha beta gamma delta"))
  ;; at the cursors a delimiter wraps each selection, and again takes it off
  (let ((flowers (format nil "rose~%iris~%lily")))
    (is string= (format nil "\"rose\"~%\"iris\"~%\"lily\"") (split-text "t t m w \"" flowers))
    (is string= flowers (split-text "t t m w \" \"" flowers)))
  ;; lesson 9: a suffix on every row from a block of no width at the line's end
  (is string= (format nil "aaaaa ;~%bbbbb ;~%ccccc ;")
      (split-text "g l m v j j c SPC ; C-g C-g" (format nil "aaaaa~%bbbbb~%ccccc")))
  ;; lesson 16: M at the cursors, then c
  (is string= (format nil "fresh apple~%fresh pear~%fresh plum")
      (split-text "t t M w c f r e s h C-g C-g" (format nil "old red apple~%old green pear~%old blue plum"))))

(define-test (split opening-lines)
  ;; o with a count: the cursor on the first new line, the rest blank below it
  (multiple-value-bind (f editor) (run-keys "C-u 3 o x C-g" :text (format nil "a~%b"))
    (declare (ignore f))
    (is string= (format nil "a~%x~%~%~%b") (session-text editor)))
  ;; O with a count: the blanks above, the cursor beside the line it came from
  (is string= (format nil "a~%~%~%x~%b") (split-text "j C-u 3 O x C-g" (format nil "a~%b")))
  ;; one undo takes them all back
  (is string= (format nil "a~%b") (split-text "C-u 3 o C-g u" (format nil "a~%b"))))

(define-test (split cursors-past-the-limit-move-alike)
  ;; % t on more lines than there may be cursors, then motions that move every cursor alike:
  ;; those out of sight follow, a shorter line done at its end
  (let ((text (format nil "abcd~%abcd~%ab~%abcd~%abcd~%")))
    (is string= (format nil "aXbcd~%aXbcd~%aXb~%aXbcd~%aXbcd~%") (every-line-text '("% t l i X C-g") text))
    (is string= (format nil "abXcd~%abXcd~%abX~%abXcd~%abXcd~%") (every-line-text '("% t l l l h i X C-g") text))
    (is string= (format nil "abcdX~%abcdX~%abX~%abcdX~%abcdX~%") (every-line-text '("% t g l i X C-g") text))
    (is string= (format nil "Xabcd~%Xabcd~%Xab~%Xabcd~%Xabcd~%") (every-line-text '("% t l l g h i X C-g") text))
    ;; a count
    (is string= (format nil "abXcd~%abXcd~%abX~%abXcd~%abXcd~%") (every-line-text '("% t C-u 2 l i X C-g") text))
    ;; a motion that would take each its own way is refused, the cursors kept, all of them
    (is string= (format nil "Yabcd~%Yabcd~%Yab~%Yabcd~%Yabcd~%") (every-line-text '("% t w i Y C-g") text))
    (let ((tuatara-user:*cursors-limit* 2))
      (let ((f (run-keys (format nil "% t w") :text text :width 150 :height 10)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Cursors past the limit move only alike"))))))

(define-test (split delimiter-keys-where-free)
  (flet ((bound (keymap char) (tuatara.command:lookup-key keymap (list (list char)))))
    (let ((tuatara-user:*pair-delimiters* '((#\( . #\)) (#\< . #\>) (#\" . #\")))
          (keymap (tuatara.command:make-keymap :name "delimiters")))
      (tuatara.command:define-key keymap (list (list #\<)) 'ignore)
      (tuatara.defaults::bind-free-delimiter-keys keymap 'wrap-it)
      (is eq 'wrap-it (bound keymap #\())
      (is eq 'wrap-it (bound keymap #\)))
      (is eq 'wrap-it (bound keymap #\"))
      (is eq 'wrap-it (bound keymap #\>))
      (is eq 'ignore (bound keymap #\<)))
    ;; the split's verbs stay theirs, the delimiters wrap
    (tuatara.defaults::build-split-chooser-keymap)
    (is eq 'tuatara.defaults::split-wrap-key (bound tuatara.defaults::*split-chooser-keymap* #\())
    (is eq 'tuatara.defaults::split-delete (bound tuatara.defaults::*split-chooser-keymap* #\d))))

(define-test (split cursors-below-and-above-alike)
  (flet ((typed (keys) (session-text (nth-value 1 (run-keys keys :text (format nil "a~%b~%c~%d~%e"))))))
    (is string= (format nil "a~%Xb~%Xc~%Xd~%e") (typed "j j t T i X C-g"))
    (is string= (format nil "a~%b~%Xc~%Xd~%Xe") (typed "j j C-u 2 t i X C-g"))
    (is string= (format nil "Xa~%Xb~%Xc~%d~%e") (typed "j j C-u 2 T i X C-g"))
    ;; a negative count goes the other way
    (is string= (format nil "a~%Xb~%Xc~%d~%e") (typed "j j C-u - 1 t i X C-g"))
    (is string= (format nil "a~%b~%Xc~%Xd~%e") (typed "j j C-u - 1 T i X C-g"))
    ;; more added below the last and above the first
    (is string= (format nil "Xa~%Xb~%Xc~%Xd~%Xe") (typed "j j t t T T i X C-g"))))

(define-test (split cursor-typing-said-where)
  ;; as the cursors end, the echo row says where the text went
  (let ((three (format nil "abc~%abc~%abc")))
    (flet ((said (keys) (string-trim " " (split-echo keys three))))
      (is string= "Split: typed before 3 cursors" (said "l t t i X C-g C-g"))
      (is string= "Split: typed after 3 cursors" (said "l t t a X C-g C-g"))
      (is string= "Split: typed at the line starts of 3 cursors" (said "l t t I X C-g C-g"))
      (is string= "Split: typed at the line ends of 3 cursors" (said "l t t A X C-g C-g"))
      (is string= "Split: typed on new lines below 3 cursors" (said "t t o X C-g C-g"))
      (is string= "Split: typed on new lines above 3 cursors" (said "t t O X C-g C-g")))))
