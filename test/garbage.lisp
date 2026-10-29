;;;; test/garbage.lisp --- When garbage is collected, and the room the heap has.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test garbage)

(define-test (garbage collected-when-idle)
  ;; started with the editor, as a tick
  (run-keys "")
  (true (member 'tuatara.defaults::idle-garbage-tick tuatara.command:*tick-functions*))
  (let ((tuatara.defaults::*nursery-octets* (* 4 1024 1024))
        (tuatara-user:*idle-collection-seconds* 0.3)
        (garbage nil))
    (sb-ext:gc)
    ;; little made since the last collection: nothing to do, not called again before a key
    (false (tuatara.defaults::idle-garbage-tick nil))
    (setf garbage (loop repeat 4096 collect (make-array 1024 :element-type '(unsigned-byte 8))))
    (is = 4096 (length garbage))
    (setf garbage nil)
    (true (>= (tuatara.defaults::consed-since-gc) (* 2 1024 1024)))
    ;; a key just typed: not now, looked at again when the quiet time is up
    (let ((tuatara.command:*last-input-time* (get-internal-real-time))
          (before sb-ext:*gc-run-time*))
      (let ((again (tuatara.defaults::idle-garbage-tick nil)))
        (true (and (realp again) (< 0 again 0.31)) "~a" again))
      (is = before sb-ext:*gc-run-time*))
    ;; left alone for longer: collected now, and nothing waits after it
    (let ((tuatara.command:*last-input-time* (- (get-internal-real-time) internal-time-units-per-second))
          (before sb-ext:*gc-run-time*))
      (false (tuatara.defaults::idle-garbage-tick nil))
      (true (> sb-ext:*gc-run-time* before))
      (true (< (tuatara.defaults::consed-since-gc) (* 2 1024 1024)))
      (false (tuatara.defaults::idle-garbage-tick nil)))
    ;; off: never
    (let ((tuatara-user:*idle-collection-seconds* nil))
      (setf garbage (loop repeat 4096 collect (make-array 1024 :element-type '(unsigned-byte 8))))
      (is = 4096 (length garbage))
      (setf garbage nil)
      (false (tuatara.defaults::idle-garbage-tick nil)))))

(define-test (garbage room-in-the-heap)
  ;; room asked for: there, or after collecting; never more than the heap
  (true (tuatara.defaults::heap-room-p 1024))
  (false (tuatara.defaults::heap-room-p (* 2 (sb-ext:dynamic-space-size))))
  (let ((budget (tuatara.defaults::heap-budget nil)))
    (true (< 0 budget (tuatara.defaults::heap-free))))
  (is = 12345 (tuatara.defaults::heap-budget 12345)))
