;;;; test/tui.lisp --- The terminal frontend and the program's start: names for shared pictures, the random numbers of each start.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test tui
  :description "The terminal frontend and the program's start, as frontends/tui has them.")

(define-test (tui random-numbers-new-at-each-start)
  ;; two starts of one saved executable begin with the same state; each draws its own numbers
  (let ((saved (make-random-state nil)))
    (flet ((first-number ()
             (let ((*random-state* (make-random-state saved)))
               (tuatara.terminal::fresh-random-state)
               (random (expt 2 64)))))
      (isnt = (first-number) (first-number)))))

(define-test (tui picture-names-new-at-each-start)
  ;; the names of shared pictures do not repeat a state saved in the executable
  (let ((saved (make-random-state nil)))
    (flet ((name () (let ((*random-state* (make-random-state saved))) (tuatara.frontend.tui::shared-picture-name))))
      (let ((one (name)) (two (name)))
        (isnt string= one two)
        (true (uiop:string-prefix-p (format nil "tuatara-~d-" (sb-posix:getpid)) one))))))

(define-test (tui window-only-without-a-terminal)
  ;; a window of its own only when neither the input nor the output is a terminal, on a desktop
  (flet ((window (input output desktop)
           (tuatara.terminal::window-wanted-p :input input :output output :desktop desktop)))
    (true (window nil nil ":0"))
    (false (window nil t ":0"))
    (false (window t nil ":0"))
    (false (window t t ":0"))
    (false (window nil nil nil))))

(define-test (tui input-pending-counts-what-is-kept)
  ;; with nothing on the terminal's input, a resize kept to hand over and keys handed
  ;; back to ncurses are input waiting all the same
  (multiple-value-bind (empty writer) (sb-posix:pipe)
    (let ((saved (sb-posix:dup 0))
          (f (tuatara.frontend.tui:make-tui-frontend)))
      (unwind-protect
           (progn
             (sb-posix:dup2 empty 0)
             (let ((tuatara.frontend.tui::*keys-given-back* nil))
               (false (tuatara.frontend:frontend-input-pending-p f))
               (push (tuatara.frontend:make-event :resize :width 80 :height 24) (tuatara.frontend.tui::tui-pending f))
               (true (tuatara.frontend:frontend-input-pending-p f))
               (setf (tuatara.frontend.tui::tui-pending f) '()
                     tuatara.frontend.tui::*keys-given-back* t)
               (true (tuatara.frontend:frontend-input-pending-p f))))
        (sb-posix:dup2 saved 0)
        (mapc #'sb-posix:close (list saved empty writer))))))

(define-test (tui file-names-not-utf-8-on-the-command-line)
  ;; a file name that is not UTF-8 on the command line: every argument kept, that
  ;; name's octets as its raw bytes' characters, the runtime's options left out
  (with-test-directory (directory)
    (let ((cmdline (merge-pathnames "cmdline" directory)))
      (with-open-file (o cmdline :direction :output :element-type '(unsigned-byte 8))
        (write-sequence (concatenate '(vector (unsigned-byte 8))
                                     (sb-ext:string-to-octets "tuatara") #(0)
                                     (sb-ext:string-to-octets "--dynamic-space-size") #(0) (sb-ext:string-to-octets "3GB") #(0)
                                     (sb-ext:string-to-octets "--gui") #(0)
                                     (sb-ext:string-to-octets "bad") #(255 254) (sb-ext:string-to-octets "name.txt") #(0)
                                     (sb-ext:string-to-octets "a.txt") #(0))
                        o))
      (is equal (list "tuatara" "--gui" (format nil "bad~c~cname.txt" (code-char #xDCFF) (code-char #xDCFE)) "a.txt")
          (tuatara.terminal::command-line-arguments :argv nil :from (uiop:native-namestring cmdline)))
      (is equal '("tuatara" "x") (tuatara.terminal::command-line-arguments :argv '("tuatara" "x") :from "/nonexistent")))))

(define-test (tui heap-restart-with-a-name-not-utf-8)
  ;; such a name is handed on as its octets; with no program there to run, this
  ;; start goes on with its heap
  (let ((sb-ext:*posix-argv* (list "tuatara" (format nil "bad~cname" (code-char #xDCFF))))
        (tuatara.io:*saved-program* "/nonexistent/tuatara"))
    (unwind-protect (finish (tuatara.terminal::restart-with-heap 100))
      (sb-posix:unsetenv "TUATARA_HEAP_DECIDED"))))

(define-test (tui terminal-given-back-alike)
  ;; stopping and lending the terminal give it back the same way, and a lent terminal is taken
  ;; again as the editor had it: the kitty keyboard, the mouse, pastes, the screen painted whole
  (let ((calls '()))
    (with-functions-replaced ((tuatara.frontend.tui::%endwin (lambda () (push :endwin calls)))
                              (tuatara.frontend.tui::%clearok (lambda (window on) (declare (ignore window on)) (push :clearok calls)))
                              (tuatara.frontend.tui::%wrefresh (lambda (window) (declare (ignore window)) (push :refresh calls)))
                              (tuatara.frontend.tui::forget-images (lambda (f all) (declare (ignore f)) (push (list :forget all) calls))))
      (flet ((sent (function)
               (let ((f (tuatara.frontend.tui:make-tui-frontend))
                     (tuatara.frontend.tui::*kitty-keyboard-on* t)
                     (tuatara.frontend.tui::*shown-cursor-shape* :bar)
                     (tuatara.frontend.tui::*shown-cursor-colour* "#123456")
                     (tuatara.frontend.tui::*saved-stderr* nil)
                     (tuatara.frontend.tui::*shared-pictures* '()))
                 (setf (tuatara.frontend.tui::tui-window f) :window (tuatara.frontend.tui::tui-mouse f) t
                       calls '())
                 (values (with-output-to-string (*standard-output*) (funcall function f))
                         (reverse calls)
                         tuatara.frontend.tui::*kitty-keyboard-on*
                         tuatara.frontend.tui::*shown-cursor-shape*))))
        (let* ((esc (string #\Escape))
               (given-back (concatenate 'string esc "[0 q" esc "]112" (string (code-char 7)) esc "[<u"
                                        esc "[?1002l" esc "[?1006l" esc "[?2004l")))
          (multiple-value-bind (out calls kitty shape) (sent #'tuatara.frontend:frontend-stop)
            (is string= given-back out)
            (is equal '((:forget t) :endwin) calls)
            (false kitty)
            (false shape))
          (multiple-value-bind (out calls kitty shape)
              (sent (lambda (f) (tuatara.frontend:frontend-lend-terminal f (lambda () (write-string "program")))))
            (is string= (concatenate 'string given-back "program" esc "[>5u" esc "[?1002h" esc "[?1006h" esc "[?2004h") out)
            (is equal '((:forget t) :endwin :clearok :refresh) calls)
            (true kitty)
            (false shape)))))))

(define-test (tui row-written-without-garbage)
  ;; a row's spans and the face painted past its text are written to the terminal from the row itself,
  ;; no piece of it copied: little garbage for each span more (faces combined as they always are)
  (let ((written (make-string-output-stream)) (recording t))
    (with-functions-replaced ((tuatara.frontend.tui::%wmove (lambda (window y x) (declare (ignore window y x)) 0))
                              (tuatara.frontend.tui::%getmaxx (lambda (window) (declare (ignore window)) 300))
                              (tuatara.frontend.tui::%getmaxy (lambda (window) (declare (ignore window)) 50))
                              (tuatara.frontend.tui::%getcurx (lambda (window) (declare (ignore window)) 200))
                              (tuatara.frontend.tui::%wclrtoeol (lambda (window) (declare (ignore window)) 0))
                              (tuatara.frontend.tui::attributes-on (lambda (window attrs) (declare (ignore window attrs))))
                              (tuatara.frontend.tui::attributes-off (lambda (window) (declare (ignore window))))
                              (tuatara.frontend.tui::spec-attributes (lambda (spec) (declare (ignore spec)) 1))
                              (tuatara.display:combine-faces (lambda (face base) (declare (ignore base)) face))
                              (tuatara.frontend.tui::%waddstr-octets
                               (lambda (window octets) (declare (ignore window))
                                 (when recording (write-string (tuatara.io:sap-string octets) written))
                                 0)))
      (let ((f (tuatara.frontend.tui:make-tui-frontend))
            (text (make-string 200 :initial-element #\x)))
        (setf (tuatara.frontend.tui::tui-window f) :window)
        (flet ((consed (count)
                 (let ((spans (loop for k below count collect (list (* k 5) (+ (* k 5) 3) :bold))))
                   (tuatara.frontend:frontend-put-row f 0 text :face :mode-line :spans spans)
                   (let ((before (tuatara.bench::bytes-consed)))
                     (dotimes (k 100) (tuatara.frontend:frontend-put-row f 0 text :face :mode-line :spans spans))
                     (/ (- (tuatara.bench::bytes-consed) before) 100)))))
          (tuatara.frontend:frontend-put-row f 0 text :face :mode-line :spans '((0 3 :bold)))
          ;; the row's text, then spaces to the edge
          (is string= (concatenate 'string text (make-string 100 :initial-element #\Space)) (get-output-stream-string written))
          (setf recording nil)
          (let ((few (consed 2)) (many (consed 40)))
            (true (< (/ (- many few) 38) 48) "~d octets of garbage for a row of 40 spans, ~d for one of 2" many few)))))))
