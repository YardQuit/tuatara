;;;; test/narrowing.lisp --- A buffer narrowed to a part of it: what commands reach, what is shown, what is saved.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test narrowing
  :description "Narrowing to the selection and to the section at point, widening; motion, search, replacement and editing kept to the part shown, its edges moving with edits, undo across narrowing and widening, saving the whole file, the mode line's Narrow.")

(defparameter *narrowing-text* (format nil "one~%two~%three~%four~%five~%")
  "Five lines, the second and third of which the tests narrow to.")

(defparameter *narrow-to-two-and-three* "C-n C-SPC C-n C-n C-x x n"
  "Keys selecting the second and third lines of *NARROWING-TEXT* and narrowing to them.")

(defun last-screen-rows (frontend)
  "The rows of FRONTEND's last frame, right-trimmed."
  (loop for row below (length (tuatara.frontend.fake::fake-screen frontend))
        collect (tuatara.frontend.fake:fake-screen-line frontend row)))

(defun shown-now-p (frontend text)
  "True when a row of FRONTEND's last frame holds TEXT."
  (some (lambda (row) (search text row)) (last-screen-rows frontend)))

(defun session-buffer (editor)
  "The buffer of the editor's selected window."
  (tuatara.display:window-buffer (tuatara.command:editor-window editor)))

(defparameter *narrowing-session-seconds* 20
  "How long a session of the narrowing tests may take before it is taken to
have hung: it is stopped then, and the test fails.")

(defparameter *narrowing-carried*
  '(tuatara.defaults::*rewrite-threshold* tuatara.defaults::*rewrite-piece* tuatara.defaults::*rewrite-checkpoint*
    tuatara.defaults::*rewrite-piece-hook* tuatara.defaults::*global-mark-ring* tuatara.defaults::*places-in-force*)
  "The settings a narrowing test may bind around a session, carried into the
thread the session runs on.")

(defun bounded-call (function)
  "Call FUNCTION in a thread of its own, the settings of *NARROWING-CARRIED*
as they are here: what it returns, the error it signalled, or :HUNG when it
has not returned within *NARROWING-SESSION-SECONDS* (the thread stopped
then)."
  (let* ((values (mapcar #'symbol-value *narrowing-carried*))
         (thread (sb-thread:make-thread
                  (lambda () (progv *narrowing-carried* values (handler-case (funcall function) (error (e) e))))
                  :name "narrowing session"))
         (result (sb-thread:join-thread thread :timeout *narrowing-session-seconds* :default :hung)))
    (when (eq result :hung)
      (sb-thread:terminate-thread thread)
      (sb-thread:join-thread thread :timeout 5 :default nil))
    result))

(defun bounded-session (keys arguments)
  "Run KEYS as RUN-KEYS does, with ARGUMENTS, in a thread of its own, the
settings of *NARROWING-CARRIED* as they are here: a list of the frontend and
the editor, the error that ended the session, or :HUNG when it has not ended
within *NARROWING-SESSION-SECONDS*. A session stopped so leaves nothing
behind: the buffers it made are killed, and the clock is as it was."
  (let* ((buffers (copy-list tuatara.buffer:*buffers*))
         (clock tuatara.defaults::*org-clock*)
         (result (bounded-call (lambda () (multiple-value-list (apply #'run-keys keys arguments))))))
    (when (eq result :hung)
      (setf tuatara.defaults::*org-clock* clock)
      (dolist (b (copy-list tuatara.buffer:*buffers*))
        (unless (member b buffers) (ignore-errors (tuatara.buffer:kill-buffer b)))))
    result))

(defun narrowing-keys (keys &rest arguments)
  "RUN-KEYS for the narrowing tests: the frontend and the editor, the session
bounded in time (BOUNDED-SESSION); an error when it hung or failed, so that
the test fails rather than waits."
  (let ((result (bounded-session keys arguments)))
    (cond ((eq result :hung) (error "The session did not end within ~d seconds" *narrowing-session-seconds*))
          ((typep result 'error) (error result))
          (t (values-list result)))))

(defun keys-finish-within (keys &rest arguments)
  "The editor after KEYS (BOUNDED-SESSION), the error that ended the session,
or :HUNG, so that each of several sessions in a test says how it ended."
  (let ((result (bounded-session keys arguments)))
    (if (listp result) (second result) result)))

(define-test (narrowing to-the-selection-and-back)
  ;; only the lines narrowed to are shown, the mode line says Narrow; widening shows all again
  (multiple-value-bind (f editor) (narrowing-keys *narrow-to-two-and-three* :text *narrowing-text* :width 60)
    (let ((b (session-buffer editor)))
      (true (tuatara.buffer:buffer-narrowed-p b))
      (is = 4 (tuatara.buffer:buffer-start b))
      (is = 14 (tuatara.buffer:buffer-end b))
      (false (tuatara.buffer:buffer-mark-active-p b))
      (is string= "two" (tuatara.frontend.fake:fake-screen-line f 0))
      (is string= "three" (tuatara.frontend.fake:fake-screen-line f 1))
      (false (shown-now-p f "one"))
      (false (shown-now-p f "four"))
      (true (shown-now-p f " Narrow "))
      ;; the text itself is whole
      (is string= *narrowing-text* (session-text editor))))
  (multiple-value-bind (f editor) (narrowing-keys (list *narrow-to-two-and-three* "C-x x w") :text *narrowing-text* :width 60)
    (false (tuatara.buffer:buffer-narrowed-p (session-buffer editor)))
    (true (shown-now-p f "four"))
    (true (shown-now-p f "five"))
    (false (shown-now-p f " Narrow ")))
  ;; without a selection nothing is narrowed, and that is said
  (multiple-value-bind (f editor) (narrowing-keys "C-x x n" :text *narrowing-text* :width 60)
    (false (tuatara.buffer:buffer-narrowed-p (session-buffer editor)))
    (true (tuatara.frontend.fake:fake-frame-showing f "No selection: narrow-to-region was not run"))))

(define-test (narrowing mode-line-says-narrow)
  ;; Narrow in its own face, before the buffer's name
  (let ((f (narrowing-keys *narrow-to-two-and-three* :text *narrowing-text* :width 60)))
    (multiple-value-bind (screen spans) (tuatara.frontend.fake:fake-frame-with f " Narrow ")
      (let* ((row (position-if (lambda (r) (search " Narrow " r)) screen))
             (at (search " Narrow " (aref screen row))))
        (true (find-if (lambda (span) (and (= (first span) at) (member :mode-line-narrowed (alexandria:ensure-list (third span)))))
                       (aref spans row)))
        (true (< at (search "*scratch*" (aref screen row))))))))

(define-test (narrowing motion-stops-at-the-edges)
  (flet ((point-after (keys)
           (session-point (nth-value 1 (narrowing-keys (list *narrow-to-two-and-three* keys) :text *narrowing-text* :width 60)))))
    ;; the ends of the buffer are those of the part shown
    (is = 4 (point-after "M-<"))
    (is = 14 (point-after "M->"))
    ;; lines and characters stop there too, however many are asked for
    (is = 4 (point-after "M-< C-p C-p"))
    (is = 4 (point-after "M-< C-b C-b"))
    (is = 14 (point-after "M-> C-n C-n"))
    (is = 14 (point-after "M-> C-f C-f"))
    (is = 14 (point-after "C-u 9 C-n"))
    (is = 4 (point-after "M-> C-u 9 C-p"))
    ;; words, and the line's ends
    (is = 14 (point-after "M-< M-f M-f M-f M-f"))
    (is = 4 (point-after "M-> M-b M-b M-b"))
    (is = 14 (point-after "M-> C-e"))
    (is = 8 (point-after "M-> C-b C-a"))
    (is = 14 (point-after "M-> C-a")))
  ;; narrowed inside lines: the line's ends are the edges
  (flet ((point-after (keys)
           (session-point (nth-value 1 (narrowing-keys (list "C-f C-SPC C-n C-f C-x x n" keys) :text *narrowing-text* :width 60)))))
    (is = 1 (point-after "M-> C-a C-a C-p C-a"))
    (is = 6 (point-after "M-< C-e C-n C-e"))))

(define-test (narrowing goto-line-counts-the-file)
  ;; line numbers are the file's, in the mode line and for goto-line; one outside widens, and says so
  (multiple-value-bind (f editor) (narrowing-keys (list *narrow-to-two-and-three* "M-g g 3 RET") :text *narrowing-text* :width 60)
    (is = 8 (session-point editor))
    (true (tuatara.buffer:buffer-narrowed-p (session-buffer editor)))
    (true (tuatara.frontend.fake:fake-frame-showing f "L3 C0")))
  (dolist (case '(("M-g g 5 RET" 19 "line 5") ("M-> M-g g 1 RET" 0 "line 1")))
    (destructuring-bind (keys point line) case
      (multiple-value-bind (f editor) (narrowing-keys (list *narrow-to-two-and-three* keys) :text *narrowing-text* :width 60)
        (is = point (session-point editor))
        (false (tuatara.buffer:buffer-narrowed-p (session-buffer editor)))
        (true (tuatara.frontend.fake:fake-frame-showing f (format nil "Widened *scratch* to show ~a" line)))))))

(define-test (narrowing jumps-outside-widen)
  ;; going back to a place kept before narrowing widens, saying so, never landing at the edge
  (multiple-value-bind (f editor)
      (let ((tuatara.defaults::*global-mark-ring* '()))
        (narrowing-keys (list *narrow-to-two-and-three*
                        (lambda () (let ((b (tuatara.command:current-buffer)))
                                     (setf tuatara.defaults::*global-mark-ring*
                                           (list (cons b (tuatara.text:make-anchor (tuatara.buffer:buffer-version b) 20))))))
                        "C-x C-SPC")
                  :text *narrowing-text* :width 80))
    (is = 20 (session-point editor))
    (false (tuatara.buffer:buffer-narrowed-p (session-buffer editor)))
    (true (tuatara.frontend.fake:fake-frame-showing f "Widened *scratch* to show line 5"))))

(define-test (narrowing search-finds-nothing-outside)
  (let ((text (format nil "needle one~%two~%three~%needle four~%")))
    (flet ((narrowed (keys) (narrowing-keys (list "C-n C-SPC C-n C-n C-x x n M-<" keys) :text text :width 60)))
      ;; forward and back, wrapping included: the needles outside are not found
      (multiple-value-bind (f editor) (narrowed "C-s n e e d l e RET")
        (is = 11 (session-point editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "No match for needle")))
      (is = 21 (session-point (nth-value 1 (narrowed "M-> C-r n e e d l e C-r C-r RET"))))
      (is = 11 (session-point (nth-value 1 (narrowed "C-s n e e d l e C-s C-s RET"))))
      ;; literal and regex alike, a match inside found
      (is = 20 (session-point (nth-value 1 (narrowed "C-s t h r e e RET"))))
      (is = 20 (session-point (nth-value 1 (narrowed "C-s t h . e e RET"))))
      (is = 11 (session-point (nth-value 1 (narrowed "C-s M-r n e e d l e RET"))))
      ;; widened again, it is found
      (is = 6 (session-point (nth-value 1 (narrowing-keys (list "C-n C-SPC C-n C-n C-x x n C-x x w M-< C-s n e e d l e RET")
                                                     :text text :width 60)))))))

(define-test (narrowing replace-all-leaves-outside-alone)
  (let ((text (format nil "foo~%foo two~%foo three~%foo~%")))
    (multiple-value-bind (f editor) (narrowing-keys (list "C-n C-SPC C-n C-n C-x x n" "M-< M-% f o o RET b a r RET !")
                                              :text text :width 80)
      (declare (ignore f))
      (is string= (format nil "foo~%bar two~%bar three~%foo~%") (session-text editor)))
    ;; a regular expression replaced everywhere: only in the part shown too
    (is string= (format nil "foo~%baz two~%baz three~%foo~%")
        (session-text (nth-value 1 (narrowing-keys (list "C-n C-SPC C-n C-n C-x x n" "M-< r r % f o o / b a z RET")
                                             :text text :width 80))))))

(define-test (narrowing jobs-keep-the-reach-they-began-with)
  ;; a replacement in the background begun while narrowed changes only the part shown; one begun
  ;; before narrowing reaches the whole text still, point kept inside when it is taken in
  (let ((text (format nil "foo~%foo two~%foo three~%foo~%"))
        (tuatara.defaults::*rewrite-piece-hook* (lambda () (sleep 0.02))))
    (flet ((after (&rest keys)
             (let ((b nil))
               (narrowing-keys (append (list (lambda () (setf b (tuatara.command:current-buffer)))) keys
                                 (list (lambda () (loop while (tuatara.runtime:jobs-of b :running t) do (sleep 0.02)))
                                       :tick))
                         :text text :width 100 :height 3)
               b)))
      (with-background ()
        (let ((b (after "C-n C-SPC C-n C-n C-x x n M-< r r % f o o / b a z RET")))
          (is string= (format nil "foo~%baz two~%baz three~%foo~%") (tuatara.buffer:buffer-string b)))
        (let ((b (after "r r % f o o / b a z RET"
                        (lambda () (let ((b (tuatara.command:current-buffer))) (tuatara.buffer:narrow-buffer b 4 19))))))
          (is string= (format nil "baz~%baz two~%baz three~%baz~%") (tuatara.buffer:buffer-string b))
          (true (tuatara.buffer:buffer-narrowed-p b))
          (true (<= (tuatara.buffer:buffer-start b) (tuatara.buffer:buffer-point b) (tuatara.buffer:buffer-end b))))))))

(define-test (narrowing records-kept-in-the-part-shown)
  ;; keeping a file's records without a selection filters those shown, the others left as they are
  (let ((editor (nth-value 1 (narrowing-keys (list "M-x c s v - m o d e RET"
                                             (lambda () (tuatara.buffer:narrow-buffer (tuatara.command:current-buffer) 13 25))
                                             "C-x / k n a m e RET t u i RET")
                                       :text (format nil "name,n~%kea,1~%kea,2~%tui,3~%kea,4~%") :width 120))))
    (is string= (format nil "name,n~%kea,1~%tui,3~%kea,4~%") (session-text editor))))

(define-test (narrowing counts-the-part-shown)
  ;; counting words without a selection counts what is shown
  (let ((f (narrowing-keys (list *narrow-to-two-and-three* "M-x c o u n t - w o r d s RET") :text *narrowing-text* :width 120)))
    (true (tuatara.frontend.fake:fake-frame-showing f "The part shown has 2 lines"))))

(define-test (narrowing edits-at-the-edges-move-them)
  ;; typed at either edge it goes inside; the part shown grows and shrinks with what is edited
  (let ((editor (nth-value 1 (narrowing-keys (list *narrow-to-two-and-three* "M-< i A C-g M-> i Z C-g") :text *narrowing-text* :width 60))))
    (let ((b (session-buffer editor)))
      ;; the part shown ends after the newline of three, where four begins
      (is string= (format nil "one~%Atwo~%three~%Zfour~%five~%") (session-text editor))
      (is = 4 (tuatara.buffer:buffer-start b))
      (is = 16 (tuatara.buffer:buffer-end b))))
  (let ((editor (nth-value 1 (narrowing-keys (list *narrow-to-two-and-three* "M-< C-k C-k") :text *narrowing-text* :width 60))))
    (is string= (format nil "one~%three~%four~%five~%") (session-text editor))
    (is = 10 (tuatara.buffer:buffer-end (session-buffer editor))))
  ;; deleting back from the start, or on from the end, reaches nothing outside
  (is string= *narrowing-text* (session-text (nth-value 1 (narrowing-keys (list *narrow-to-two-and-three* "M-< i DEL DEL C-g M-> C-d C-d")
                                                                    :text *narrowing-text* :width 60))))
  ;; nor does deleting the whole buffer's trailing blanks or every line's
  (let ((text (format nil "a  ~%b  ~%c  ~%~%~%")))
    (is string= (format nil "a  ~%b~%c  ~%~%~%")
        (session-text (nth-value 1 (narrowing-keys (list "C-n C-SPC C-n C-x x n" "M-x d e l e t e - t r a i l i n g - w h i t e s p a c e RET")
                                             :text text :width 80)))))
  ;; an edit reaching outside is refused, and said
  (let ((f (narrowing-keys (list *narrow-to-two-and-three* "M-> C-d") :text *narrowing-text* :width 120)))
    (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "outside the part of *scratch* shown"))))

(define-test (narrowing undo-across-narrow-and-widen)
  ;; a change made before narrowing, outside, is undone as any other; point stays inside
  (multiple-value-bind (f editor) (narrowing-keys (list "M-> i e n d C-g M-<" *narrow-to-two-and-three* "u") :text *narrowing-text* :width 60)
    (is string= *narrowing-text* (session-text editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "Undid a change outside the part shown"))
    (let ((b (session-buffer editor)))
      (true (tuatara.buffer:buffer-narrowed-p b))
      (true (<= (tuatara.buffer:buffer-start b) (session-point editor) (tuatara.buffer:buffer-end b)))))
  ;; a change made while narrowed is undone after widening, and redone
  (let ((editor (nth-value 1 (narrowing-keys (list *narrow-to-two-and-three* "M-< i X C-g C-x x w u") :text *narrowing-text* :width 60))))
    (is string= *narrowing-text* (session-text editor)))
  (let ((editor (nth-value 1 (narrowing-keys (list *narrow-to-two-and-three* "M-< i X C-g C-x x w u U") :text *narrowing-text* :width 60))))
    (is string= (format nil "one~%Xtwo~%three~%four~%five~%") (session-text editor))))

(define-test (narrowing saving-writes-the-whole-file)
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "kept.txt" dir) *narrowing-text*)))
      (multiple-value-bind (f editor) (narrowing-keys (list *narrow-to-two-and-three* "M-< i X C-g C-x C-s") :file path :width 120)
        (declare (ignore f))
        (true (tuatara.buffer:buffer-narrowed-p (session-buffer editor)))
        (is string= (format nil "one~%Xtwo~%three~%four~%five~%") (file-text path))))))

(define-test (narrowing to-the-section)
  ;; an Org subtree, its headline with it, the next heading of the same level left out
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "notes.org" dir)
                          (format nil "* Kererū~%seen in the bush~%* Tūī~%** Song~%two voices~%* Kea~%cheeky~%"))))
      (multiple-value-bind (f editor) (narrowing-keys "C-n C-n C-x x s" :file path :width 60)
        (let ((b (session-buffer editor)))
          (true (tuatara.buffer:buffer-narrowed-p b))
          (is string= (format nil "* Tūī~%** Song~%two voices~%")
              (tuatara.buffer:buffer-string b :start (tuatara.buffer:buffer-start b) :end (tuatara.buffer:buffer-end b)))
          (false (shown-now-p f "Kea"))
          (false (shown-now-p f "Kerer"))))))
  ;; a Lisp definition, the one point is in
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "birds.lisp" dir)
                          (format nil "(defun kiwi ()~%  1)~%~%(defun weka ()~%  2)~%~%(defun takahe ()~%  3)~%"))))
      (multiple-value-bind (f editor) (narrowing-keys "C-n C-n C-n C-n C-x x s" :file path :width 60)
        (let ((b (session-buffer editor)))
          (true (tuatara.buffer:buffer-narrowed-p b))
          (true (search (format nil "(defun weka ()~%  2)")
                        (tuatara.buffer:buffer-string b :start (tuatara.buffer:buffer-start b) :end (tuatara.buffer:buffer-end b))))
          (false (shown-now-p f "kiwi"))
          (false (shown-now-p f "takahe")))))))

(define-test (narrowing selecting-all-selects-the-part-shown)
  ;; the whole buffer selected is the part shown
  (let ((b (session-buffer (nth-value 1 (narrowing-keys (list *narrow-to-two-and-three* "C-x h") :text *narrowing-text* :width 60)))))
    (is equal '(4 14) (multiple-value-list (tuatara.buffer:buffer-region b)))))

(define-test (narrowing search-reads-only-the-part-shown)
  ;; a search of a large buffer narrowed to a few lines is bounded by them, never by the whole text
  (let* ((line (format nil "~a~%" (make-string 99 :initial-element #\x)))
         (b (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "narrowed search")
                                        :text (with-output-to-string (out) (loop repeat 80000 do (write-string line out))))))
    (unwind-protect
         (let ((state (tuatara.defaults::make-search-state 4000000 t t)))
           (is equal (list 0 8000000 nil) (multiple-value-list (tuatara.defaults::search-bounds state b)))
           (tuatara.buffer:narrow-buffer b 4000000 4000500)
           (is equal (list 4000000 4000500 nil) (multiple-value-list (tuatara.defaults::search-bounds state b))))
      (tuatara.buffer:kill-buffer b))))

(define-test (narrowing buffer-start-and-end-whole-unless-narrowed)
  ;; the API's bounds: the whole text unless narrowed, the part shown while it is
  (let ((b (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "narrowing bounds") :text *narrowing-text*)))
    (unwind-protect
         (progn
           (is = 0 (tuatara.buffer:buffer-start b))
           (is = 24 (tuatara.buffer:buffer-end b))
           (setf (tuatara.buffer:buffer-mark b) 20)
           (tuatara.buffer:narrow-buffer b 8 4)
           (is = 4 (tuatara.buffer:buffer-start b))
           (is = 8 (tuatara.buffer:buffer-end b))
           ;; point and the mark are kept inside
           (is = 4 (tuatara.buffer:buffer-point b))
           (is = 8 (tuatara.buffer:buffer-mark b))
           (setf (tuatara.buffer:buffer-point b) 20)
           (is = 8 (tuatara.buffer:buffer-point b))
           ;; an edit outside, not by a command, moves the edges with the text
           (tuatara.buffer:buffer-insert b 0 "zero ")
           (is = 9 (tuatara.buffer:buffer-start b))
           (is = 13 (tuatara.buffer:buffer-end b))
           ;; a command's edit outside is refused; lines and characters are the text's still
           (let ((tuatara.buffer:*narrowing-in-force* b))
             (fail (tuatara.buffer:buffer-delete b 0 1))
             (is = 18 (tuatara.buffer:buffer-line-end b 14))
             (is = 17 (tuatara.buffer:buffer-forward-char b 12 5)))
           ;; the whole text replaced does not widen it by itself
           (tuatara.buffer:buffer-replace b 0 (tuatara.buffer:buffer-size b) "new")
           (true (tuatara.buffer:buffer-narrowed-p b))
           (tuatara.buffer:widen-buffer b)
           (is = 3 (tuatara.buffer:buffer-end b)))
      (tuatara.buffer:kill-buffer b))))

(define-test (narrowing next-place-outside-widens)
  ;; M-g n to a place of the list in force outside the part shown of its file widens it, the
  ;; echo area saying so with the place it went to
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "found.txt" dir) *narrowing-text*))
          (tuatara.defaults::*places-in-force* nil))
      (multiple-value-bind (f editor)
          (narrowing-keys (list *narrow-to-two-and-three*
                                (lambda () (tuatara.defaults::use-places
                                            (tuatara.defaults::make-found-lines (vector (list (namestring path) 5 0)) -1 "five")))
                                "M-g n")
                          :file path :width 100)
        (let ((b (session-buffer editor)))
          (false (tuatara.buffer:buffer-narrowed-p b))
          (is = 19 (tuatara.buffer:buffer-point b))
          (true (search "Line 1 of 1 found; widened found.txt to show line 5"
                        (tuatara.frontend.fake:fake-screen-line f (1- (length (last-screen-rows f)))))))))))

(define-test (narrowing kept-to-the-buffer-the-command-runs-in)
  ;; a command's edits are refused outside the part shown of its own buffer only: another
  ;; buffer it changes is changed wherever it is asked
  (let ((b (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "narrowing other") :text *narrowing-text*))
        (here (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "narrowing here") :text "")))
    (unwind-protect
         (progn
           (tuatara.buffer:narrow-buffer b 4 14)
           (let ((tuatara.buffer:*narrowing-in-force* here))
             (tuatara.buffer:buffer-insert b 0 "zero "))
           (is string= (format nil "zero ~a" *narrowing-text*) (tuatara.buffer:buffer-string b))
           (let ((tuatara.buffer:*narrowing-in-force* b))
             (fail (tuatara.buffer:buffer-insert b 0 "x"))))
      (tuatara.buffer:kill-buffer b)
      (tuatara.buffer:kill-buffer here))))

(define-test (narrowing lifted-for-a-change-asked-outside)
  ;; a change asked for outside the part shown is made there, the part narrowed again after, its
  ;; edges moved by it, point where it was
  (let ((b (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "narrowing lifted") :text *narrowing-text*)))
    (unwind-protect
         (progn
           (tuatara.buffer:narrow-buffer b 4 14)
           (setf (tuatara.buffer:buffer-point b) 9)
           (let ((tuatara.buffer:*narrowing-in-force* b))
             (tuatara.buffer:with-narrowing-lifted (b)
               (setf (tuatara.buffer:buffer-point b) 0)
               (tuatara.buffer:buffer-insert b 0 "zero ")))
           (is string= (format nil "zero ~a" *narrowing-text*) (tuatara.buffer:buffer-string b))
           (is equal '(9 19) (multiple-value-list (tuatara.buffer:buffer-bounds b)))
           (is = 14 (tuatara.buffer:buffer-point b))
           ;; text put in at point goes after it, as point was before it
           (tuatara.buffer:with-narrowing-lifted (b)
             (tuatara.buffer:buffer-insert b 14 "X"))
           (is = 14 (tuatara.buffer:buffer-point b))
           ;; a jump outside made meanwhile has the buffer stay widened, there
           (tuatara.buffer:with-narrowing-lifted (b)
             (tuatara.defaults::goto-shown b 2))
           (false (tuatara.buffer:buffer-narrowed-p b))
           (is = 2 (tuatara.buffer:buffer-point b)))
      (tuatara.buffer:kill-buffer b))))

(define-test (narrowing init-file-lisp-sees-the-part)
  ;; the Lisp of an init file and M-: sees the part shown as the whole buffer: lines and
  ;; characters end at its edges, and a loop over its lines ends
  (let ((b (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "narrowing lisp") :text *narrowing-text*)))
    (unwind-protect
         (let ((result
                 (bounded-call
                  (lambda ()
                    (let ((tuatara.command:*current-buffer* b))
                      (tuatara.buffer:narrow-buffer b 4 14)
                      (list (progn (tuatara-user:goto-char (tuatara-user:point-min))
                                   (loop while (zerop (tuatara-user:forward-line 1)) count t))
                            (progn (tuatara-user:goto-char (tuatara-user:point-min)) (tuatara-user:forward-line -1))
                            (progn (tuatara-user:goto-char (tuatara-user:point-max))
                                   (handler-case (progn (tuatara-user:forward-char 1) :moved) (error (e) (princ-to-string e))))
                            (tuatara-user:char-after (tuatara-user:point-max))
                            (progn (tuatara-user:goto-char (tuatara-user:point-min)) (tuatara-user:char-before))
                            (list (tuatara-user:point-min) (tuatara-user:point-max))
                            (progn (tuatara.buffer:narrow-buffer b 5 12)
                                   (tuatara-user:goto-char 7)
                                   (list (tuatara-user:line-beginning-position) (tuatara-user:line-end-position)
                                         (tuatara-user:line-number-at-pos) (tuatara-user:line-number-at-pos nil t)))))))))
           (is equal (list 2 -1 "End of buffer" nil nil '(5 15) '(6 8 1 2)) result))
      (tuatara.buffer:kill-buffer b))))

(define-test (narrowing jobs-taken-in-at-once-or-later)
  ;; a job begun while narrowed is taken in alike, done at once or later: making every dynamic
  ;; block again reaches those outside the part shown, as asked
  (with-test-directory (dir)
    (let* ((path (put-file (merge-pathnames "blocks.org" dir)
                           (format nil "* A~%#+BEGIN: clocktable~%old~%#+END:~%* B~%body~%")))
           (editor (nth-value 1 (narrowing-keys (list (lambda () (let ((b (tuatara.command:current-buffer)))
                                                                    (tuatara.buffer:narrow-buffer b (- (tuatara.buffer:buffer-size b) 9)
                                                                                                  (tuatara.buffer:buffer-size b))))
                                                      "M-x o r g - u p d a t e - a l l - d b l o c k s RET")
                                                :file path :width 100))))
      (false (search (format nil "~%old~%") (session-text editor))))))

(define-test (narrowing format-buffer-lays-out-the-part)
  ;; with no formatter, the lines of the part shown are indented, those outside left as they are
  (with-test-directory (dir)
    (let* ((path (put-file (merge-pathnames "two.lisp" dir) (format nil "(defun a ()~%1)~%(defun b ()~%2)~%")))
           (editor (nth-value 1 (narrowing-keys (list (lambda () (tuatara.buffer:narrow-buffer (tuatara.command:current-buffer) 15 30))
                                                      "M-x f o r m a t - b u f f e r RET")
                                                :file path :width 100))))
      (is string= (format nil "(defun a ()~%1)~%(defun b ()~%  2)~%") (session-text editor)))))

(define-test (narrowing outline-lists-the-part)
  ;; the headings offered are those of the part shown
  (with-test-directory (dir)
    (let* ((path (put-file (merge-pathnames "heads.org" dir) (format nil "* Kea~%x~%* Tui~%y~%* Weka~%z~%")))
           (f (narrowing-keys (list (lambda () (tuatara.buffer:narrow-buffer (tuatara.command:current-buffer) 8 16))
                                    "M-g o" :wait :tick "C-g")
                              :file path :width 100 :height 12))
           (screen (tuatara.frontend.fake:fake-frame-with f "Go to heading")))
      (true screen)
      (true (some (lambda (row) (search "Tui" row)) screen))
      (false (some (lambda (row) (or (search "Kea" row) (search "Weka" row))) screen)))))

(define-test (narrowing joins-and-kills-stop-at-the-edges)
  ;; joining the part's first line to the one above does nothing; killing a line at the part's
  ;; end says so, as at the buffer's end
  (is string= *narrowing-text*
      (session-text (nth-value 1 (narrowing-keys (list *narrow-to-two-and-three* "M-< M-x d e l e t e - i n d e n t a t i o n RET")
                                                 :text *narrowing-text* :width 80))))
  (multiple-value-bind (f editor) (narrowing-keys (list *narrow-to-two-and-three* "M-> M-x k i l l - w h o l e - l i n e RET")
                                                  :text *narrowing-text* :width 80)
    (is string= *narrowing-text* (session-text editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "End of the part shown"))))

(define-test (narrowing commands-at-the-edges-end)
  ;; commands that look at the lines before or after one end at the edge of the part shown, never
  ;; going round at it: filling a paragraph in text, Org and Markdown, a balanced expression
  ;; swapped at the edge, going to the running clock's task
  (let ((text (format nil "one two~%three four~%~%five six~%seven eight~%")))
    (let ((editor (keys-finish-within (list (lambda () (tuatara.buffer:narrow-buffer (tuatara.command:current-buffer) 20 41))
                                               "M-< M-q RET")
                                      :text text :width 60)))
      (true (typep editor 'tuatara.command:editor) "~a" editor)
      (when (typep editor 'tuatara.command:editor)
        (is string= (format nil "one two~%three four~%~%five six seven eight~%") (session-text editor)))))
  (with-test-directory (dir)
    (dolist (case (list (list "notes.org" (format nil "* A~%text~%* B~%alpha beta~%gamma delta~%") 13 36)
                        (list "notes.md" (format nil "# T~%~%alpha beta~%gamma delta~%") 5 28)))
      (destructuring-bind (name text start end) case
        (let* ((path (put-file (merge-pathnames name dir) text))
               (editor (keys-finish-within (list (lambda () (tuatara.buffer:narrow-buffer (tuatara.command:current-buffer) start end))
                                                    "M-< M-q RET")
                                           :file path :width 60)))
          (true (typep editor 'tuatara.command:editor) "~a: ~a" name editor)
          (when (typep editor 'tuatara.command:editor)
            (true (search "alpha beta gamma delta" (session-text editor)) "~a" name))))))
  (with-test-directory (dir)
    (let* ((text (format nil "(a) (b) ; c~%(d)~%"))
           (path (put-file (merge-pathnames "swap.lisp" dir) text))
           (editor (keys-finish-within (list (lambda () (tuatara.buffer:narrow-buffer (tuatara.command:current-buffer) 0 8))
                                                "M-< C-f C-f C-f C-f C-f C-f C-f C-M-t")
                                       :file path :width 60)))
      (true (typep editor 'tuatara.command:editor) "~a" editor)
      (when (typep editor 'tuatara.command:editor) (is string= text (session-text editor)))))
  (with-test-directory (dir)
    (let* ((path (put-file (merge-pathnames "clock.org" dir) (format nil "* Task~%body~%more~%")))
           (editor (keys-finish-within (list "C-c C-x C-i"
                                                (lambda () (let ((b (tuatara.command:current-buffer)))
                                                             (tuatara.buffer:narrow-buffer b (- (tuatara.buffer:buffer-size b) 5)
                                                                                           (tuatara.buffer:buffer-size b))))
                                                "C-c C-x C-j C-c C-x C-o")
                                       :file path :width 80)))
      (true (typep editor 'tuatara.command:editor) "~a" editor)
      (when (typep editor 'tuatara.command:editor)
        (is = 0 (session-point editor))
        (false (tuatara.buffer:buffer-narrowed-p (session-buffer editor)))))))

(define-test (narrowing clock-out-reaches-its-task)
  ;; clocking out of a task whose line is outside the part shown finishes the line there, point
  ;; staying where it was; a change that fails leaves the clock running
  (with-test-directory (dir)
    (let* ((path (put-file (merge-pathnames "clock.org" dir) (format nil "* Task~%body~%more~%")))
           (editor (nth-value 1 (narrowing-keys (list "C-c C-x C-i"
                                   (lambda () (let ((b (tuatara.command:current-buffer)))
                                                (tuatara.buffer:narrow-buffer b (- (tuatara.buffer:buffer-size b) 5) (tuatara.buffer:buffer-size b))
                                                (setf (tuatara.buffer:buffer-point b) (tuatara.buffer:buffer-size b))))
                                   "C-c C-x C-o")
                             :file path :width 80))))
      (let ((b (session-buffer editor)))
        (true (search "=>" (tuatara.buffer:buffer-string b)))
        (true (tuatara.buffer:buffer-narrowed-p b))
        (is = (tuatara.buffer:buffer-size b) (tuatara.buffer:buffer-point b))
        (false tuatara.defaults::*org-clock*))))
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "kept.org" dir) (format nil "* Task~%body~%"))))
      (unwind-protect
           (progn
             (narrowing-keys (list "C-c C-x C-i"
                             (lambda () (push (lambda (buffer edits) (declare (ignore buffer edits)) (error "Refused for the test"))
                                              tuatara.buffer:*before-edit-functions*))
                             "C-c C-x C-o"
                             (lambda () (pop tuatara.buffer:*before-edit-functions*)))
                       :file path :width 80)
             (true tuatara.defaults::*org-clock*))
        (when tuatara.defaults::*org-clock* (tuatara.defaults::org-clock-out-now))))))

(define-test (narrowing edits-at-the-end-stay-inside)
  ;; what a command puts after the last line of the part shown stays in it, or is refused: never
  ;; on the hidden line after it. A count comments as many lines, the last one too
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "count.lisp" dir) (format nil "a~%b~%c~%d~%"))))
      (is string= (format nil ";; a~%;; b~%;; c~%d~%")
          (session-text (nth-value 1 (narrowing-keys "C-u 3 C-x C-;" :file path :width 80))))))
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "lines.lisp" dir) (format nil "a~%b~%c~%d~%"))))
      (is string= (format nil "a~%;; b~%;; c~%d~%")
          (session-text (nth-value 1 (narrowing-keys (list (lambda () (tuatara.buffer:narrow-buffer (tuatara.command:current-buffer) 2 6))
                                                     "M-< C-u 5 C-x C-;")
                                               :file path :width 80))))))
  ;; the last line of a part ending inside a line swapped with the one before, within the part
  (let ((editor (nth-value 1 (narrowing-keys (list (lambda () (tuatara.buffer:narrow-buffer (tuatara.command:current-buffer) 4 11))
                                             "M-> C-x C-t")
                                       :text *narrowing-text* :width 80))))
    (let ((text (session-text editor)))
      (false (string= text *narrowing-text*))
      (true (uiop:string-prefix-p (format nil "one~%") text))
      (true (uiop:string-suffix-p text (format nil "ee~%four~%five~%")))
      (true (search "thr" text :end2 (search "two" text)))))
  (multiple-value-bind (f editor) (narrowing-keys (list "C-f C-x SPC C-n C-n C-f C-x r M-w C-g"
                                                  (lambda () (tuatara.buffer:narrow-buffer (tuatara.command:current-buffer) 4 14))
                                                  "M-< C-x r y")
                                            :text *narrowing-text* :width 120)
    (is string= *narrowing-text* (session-text editor))
    (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "outside the part of *scratch* shown"))))

(define-test (narrowing whole-part-edits-keep-it-narrowed)
  ;; the part shown edited whole stays narrowed, an empty one too; reading the file again widens
  (let ((editor (nth-value 1 (narrowing-keys (list (lambda () (let ((b (tuatara.command:current-buffer)))
                                                          (tuatara.buffer:narrow-buffer b 0 (tuatara.buffer:buffer-size b))))
                                             "C-x h C-w")
                                       :text *narrowing-text* :width 60))))
    (is string= "" (session-text editor))
    (true (tuatara.buffer:buffer-narrowed-p (session-buffer editor))))
  (let ((editor (nth-value 1 (narrowing-keys (list (lambda () (tuatara.buffer:narrow-buffer (tuatara.command:current-buffer) 0 0)) "i x C-g")
                                       :text "" :width 60))))
    (is string= "x" (session-text editor))
    (true (tuatara.buffer:buffer-narrowed-p (session-buffer editor))))
  (with-test-directory (dir)
    (let* ((path (put-file (merge-pathnames "read.txt" dir) *narrowing-text*))
           (editor (nth-value 1 (narrowing-keys (list *narrow-to-two-and-three* (lambda () (put-file path (format nil "new~%~a" *narrowing-text*)))
                                                "M-x r e v e r t - b u f f e r RET")
                                          :file path :width 80))))
      (is string= (format nil "new~%~a" *narrowing-text*) (session-text editor))
      (false (tuatara.buffer:buffer-narrowed-p (session-buffer editor))))))

(define-test (narrowing window-keeps-its-layout)
  ;; a narrowed window's rows are kept from one frame to the next as any window's are
  (let ((kept nil))
    (narrowing-keys (list *narrow-to-two-and-three* "C-n" :tick
                    (lambda () (setf kept (and (tuatara.display::window-laid-out (tuatara.command:current-window)) t))))
              :text *narrowing-text* :width 60)
    (true kept)))

(define-test (narrowing hex-view-refused)
  ;; the hex view, which shows every octet, is refused while the buffer is narrowed
  (multiple-value-bind (f editor) (narrowing-keys (list *narrow-to-two-and-three* "M-x h e x - m o d e RET") :text *narrowing-text* :width 120)
    (false (tuatara.display:hex-view-p (session-buffer editor)))
    (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "is narrowed, and the hex view shows every octet"))))

(define-test (narrowing nothing-drawn-past-the-end)
  ;; an Org section narrowed to: the next headline's sign is not drawn on the row after the part
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "notes.org" dir)
                          (format nil "* Kererū~%seen in the bush~%* Tūī~%two voices~%* Kea~%cheeky~%"))))
      (let* ((f (narrowing-keys "C-n C-n C-x x s" :file path :width 60))
             (rows (last-screen-rows f))
             (body (position-if (lambda (row) (search "two voices" row)) rows)))
        (true body)
        (when body
          (is string= "" (string-trim " " (nth (1+ body) rows))))
        (is = 1 (count-if (lambda (row) (search "◉" row)) rows))))))

(define-test (narrowing refusal-names-the-widen-key)
  ;; an edit refused at the edge names the key that widens, as narrowing does
  (let ((f (narrowing-keys (list *narrow-to-two-and-three* "M-> C-d") :text *narrowing-text* :width 160)))
    (true (tuatara.frontend.fake:fake-frame-showing f "Narrowed to lines 2 to 3: C-x x w shows all of it again"))
    (true (tuatara.frontend.fake:fake-frame-showing
           f "That would change text outside the part of *scratch* shown: C-x x w shows all of it again"))))
