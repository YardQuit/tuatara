;;;; test/garbage.lisp --- When garbage is collected, and the room the heap has.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test garbage)

(defun collecting (function)
  "Call FUNCTION, which collects garbage, and return what it returned once
the editor has noted the collection (TUATARA.DEFAULTS::REPORT-GC), which a
busy machine may do a while after the collection. A note of an earlier
collection, made late, does not count: the note of FUNCTION's own has come
when the time collecting it noted (TUATARA.DEFAULTS::*GC-TIME-SEEN*) takes in
that collection."
  (multiple-value-prog1 (funcall function)
    (let ((collected sb-ext:*gc-real-time*))
      (tuatara.runtime:wait-until (lambda () (>= tuatara.defaults::*gc-time-seen* collected)) 60))))

(define-test (garbage collection-not-counted-as-work)
  ;; a collection of a full heap in the middle of timed work does not count against the work's bound
  (let ((kept (loop repeat 2000000 collect (make-string 50)))
        (before-collecting sb-ext:*gc-run-time*)
        (start (thread-cpu-time)))
    ;; noted by the editor before the test ends, so that the next test's collection is its own
    (collecting (lambda () (sb-ext:gc :full t)))
    (let ((collecting (- sb-ext:*gc-run-time* before-collecting))
          (counted (- (thread-cpu-time) start)))
      (true (plusp collecting))
      (true (< counted (/ collecting 4)) "~d counted of ~d collecting" counted collecting))
    (is = 2000000 (length kept))))

(define-test (garbage collected-when-idle)
  ;; started with the editor, as a tick
  (run-keys "")
  (true (member 'tuatara.defaults::idle-garbage-tick tuatara.command:*tick-functions*))
  ;; a nursery large against what the threads a session starts make meanwhile, which the count of octets
  ;; takes in, and small against Steel Bank Common Lisp (SBCL)'s own, so that none is collected unasked
  (let ((tuatara.defaults::*nursery-octets* (* 128 1024 1024))
        (tuatara-user:*idle-collection-seconds* 5)
        (garbage nil))
    (collecting #'sb-ext:gc)
    ;; little made since the last collection: nothing to do, not called again before a key
    (false (tuatara.defaults::idle-garbage-tick nil))
    (setf garbage (loop repeat 80 collect (make-array (* 1024 1024) :element-type '(unsigned-byte 8))))
    (is = 80 (length garbage))
    (setf garbage nil)
    (true (>= (tuatara.defaults::consed-since-gc) (* 64 1024 1024)))
    ;; a key just typed: not now, looked at again when the quiet time is up
    (let ((tuatara.command:*last-input-time* (get-internal-real-time))
          (before sb-ext:*gc-run-time*))
      (let ((again (tuatara.defaults::idle-garbage-tick nil)))
        (true (and (realp again) (< 0 again 5.01)) "~a" again))
      (is = before sb-ext:*gc-run-time*))
    ;; left alone for longer: collected now, and nothing waits after it
    (let ((tuatara.command:*last-input-time* (- (get-internal-real-time) (* 10 internal-time-units-per-second)))
          (before sb-ext:*gc-run-time*))
      (false (collecting (lambda () (tuatara.defaults::idle-garbage-tick nil))))
      (true (> sb-ext:*gc-run-time* before))
      (true (< (tuatara.defaults::consed-since-gc) (* 64 1024 1024)))
      (false (tuatara.defaults::idle-garbage-tick nil)))
    ;; off: never
    (let ((tuatara-user:*idle-collection-seconds* nil))
      (setf garbage (loop repeat 80 collect (make-array (* 1024 1024) :element-type '(unsigned-byte 8))))
      (is = 80 (length garbage))
      (setf garbage nil)
      (false (tuatara.defaults::idle-garbage-tick nil)))))

(define-test (garbage none-collected-while-a-key-waits)
  ;; a key waiting to be answered is answered first: neither the start-up collection nor an idle one comes before it
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width 80 :height 8))
        (results nil))
    (setf (tuatara.frontend.fake:fake-pending f) t)
    (queue-test-keys
     f (list (lambda ()
               (let ((tuatara.defaults::*start-up-garbage-collected* nil)
                     (tuatara.defaults::*start-up-tick-time*
                       (- (get-internal-real-time) (* 2 internal-time-units-per-second)))
                     (tuatara.defaults::*nursery-octets* (* 4 1024 1024))
                     (tuatara-user:*idle-collection-seconds* 0.3)
                     (tuatara.command:*last-input-time* (- (get-internal-real-time) internal-time-units-per-second))
                     (garbage (loop repeat 4096 collect (make-array 1024 :element-type '(unsigned-byte 8))))
                     (before sb-ext:*gc-run-time*))
                 (setf results (list (tuatara.defaults::start-up-garbage-tick nil)
                                     tuatara.defaults::*start-up-garbage-collected*
                                     (tuatara.defaults::idle-garbage-tick nil)
                                     (- sb-ext:*gc-run-time* before)
                                     (length garbage)))))))
    (call-without-grammar-questions
     (lambda () (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults)))
    (destructuring-bind (start-up collected idle collecting made) results
      (is = 4096 made)
      (is eql 1 start-up)
      (false collected)
      (true (realp idle) "~a" idle)
      (is = 0 collecting))))

(define-test (garbage room-in-the-heap)
  ;; room asked for: there, or after collecting; never more than the heap
  (true (tuatara.defaults::heap-room-p 1024))
  (false (tuatara.defaults::heap-room-p (* 2 (sb-ext:dynamic-space-size))))
  (let ((budget (tuatara.defaults::heap-budget nil)))
    (true (< 0 budget (tuatara.defaults::heap-free))))
  (is = 12345 (tuatara.defaults::heap-budget 12345)))
