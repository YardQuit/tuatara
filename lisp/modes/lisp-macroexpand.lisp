;;;; lisp/modes/lisp-macroexpand.lisp --- What a macro call expands to: C-c C-m, C-c M-m.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Emacs's pp-macroexpand-last-sexp, with SLIME's keys for Common Lisp:
;;;; C-c C-m expands the macro call at point once (MACROEXPAND-1), C-c M-m all
;;;; the way down (every macro inside too), and the expansion is shown
;;;; pretty-printed in *Pp Macroexpand Output*, beside; with C-u it goes in
;;;; after point instead. The call is the expression beginning at point when
;;;; an opening bracket is there, else the one before point. The expanding runs
;;;; as an evaluation does, on a thread of its own that C-g stops.

(in-package #:tuatara.defaults)

(defun expression-at-or-before (buffer position)
  "The start and end, as two values, of the Lisp expression at POSITION of
BUFFER: the one beginning there when an opening bracket is at POSITION, else
the one that ends last before it (EXPRESSION-BEFORE); NIL when there is none."
  (multiple-value-bind (start end) (bracket-form-bounds (tuatara.buffer:buffer-text buffer) position)
    (if start
        (values start end)
        (expression-before buffer position))))

(defun expansion-text (expansion package width)
  "EXPANSION pretty-printed as Lisp text read in PACKAGE writes it, in lower
case, lines at most WIDTH columns where it can; a structure met again inside
itself written #1= ... #1#, so that printing it ends."
  (let ((*package* package) (*print-case* :downcase) (*print-pretty* t) (*print-circle* t)
        (*print-right-margin* (max 40 width)) (*print-length* nil) (*print-level* nil) (*print-readably* nil))
    (handler-case (with-output-to-string (out)
                    (pprint expansion out)
                    (terpri out))
      (error (e) (format nil "~%;; the expansion cannot be printed: ~a~%" (tuatara.runtime:condition-words e))))))

(defun show-expansion (text)
  "Show TEXT in *Pp Macroexpand Output*, in Lisp mode, beside the window in use."
  (let ((b (find-or-make-buffer "*Pp Macroexpand Output*" :mode 'lisp-mode)))
    (setf (tuatara.buffer:buffer-local b :kind) :macroexpansion)
    (tuatara.buffer:widen-buffer b)
    (tuatara.buffer:buffer-replace b 0 (tuatara.buffer:buffer-size b) (string-left-trim '(#\Newline) text))
    (setf (tuatara.buffer:buffer-undo-stack b) '() (tuatara.buffer:buffer-redo-stack b) '()
          (tuatara.buffer:buffer-point b) 0)
    (tuatara.buffer:buffer-mark-saved b)
    (display-buffer b :other t :at :start)))

(defun read-expansion-call (text package)
  "The macro call TEXT read in PACKAGE, and as a second value why it is not
expanded, or NIL when it may be: expanding only looks, so nothing in the text
may run. The call is read once, here, with #. refused, since #. would run code
as it is read; the text must be that one expression and nothing after it; and a
MACROLET's macros run their own code as they expand, so one is refused."
  (handler-case
      (multiple-value-bind (form end)
          (let ((*package* package) (*read-eval* nil)) (read-from-string text))
        (labels ((holds-macrolet-p (x seen)
                   (cond ((eq x 'macrolet) t)
                         ((and (consp x) (not (gethash x seen)))
                          (setf (gethash x seen) t)
                          (or (holds-macrolet-p (car x) seen) (holds-macrolet-p (cdr x) seen))))))
          (cond ((find-if-not (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return #\Page))) text :start end)
                 (values nil "the text holds more than the one expression"))
                ((holds-macrolet-p form (make-hash-table :test 'eq))
                 (values nil "it holds a macrolet, whose macros would run as it expands"))
                (t (values form nil)))))
    (reader-error (e)
      (values nil (if (search "*READ-EVAL*" (string-upcase (tuatara.runtime:condition-words e)))
                      "#. in it would run code as it is read"
                      (format nil "it cannot be read: ~a" (tuatara.runtime:condition-words e)))))
    (error (e) (values nil (format nil "it cannot be read: ~a" (tuatara.runtime:condition-words e))))))

(defun macroexpand-at-point (all)
  "Expand the macro call at point (EXPRESSION-AT-OR-BEFORE) once, or with ALL
every macro in it too, and show the expansion (SHOW-EXPANSION), or with a
prefix argument put it in after the call."
  (let* ((b (current-buffer))
         (insert (prefix-argument)))
    (multiple-value-bind (start end) (expression-at-or-before b (point))
      (if (null start)
          (message "No expression at point to expand")
          (multiple-value-bind (form refusal)
              (read-expansion-call (tuatara.buffer:buffer-string b :start start :end end) (lisp-package b))
            (let* ((result (and (not refusal)
                                (run-evaluation (list (list (if all 'sb-cltl2:macroexpand-all 'macroexpand-1) (list 'quote form)))
                                                b :typed t)))
                   (outcome (getf result :outcome)))
              (cond (refusal (message "Not expanded: ~a" refusal))
                    ((eq outcome :stopped) (message "Expanding stopped"))
                    ((eq (first outcome) :error)
                     (message "Error: ~a" (tuatara.runtime:condition-words (second outcome)))
                     (when (and (getf result :backtrace) *backtrace-function*)
                       (funcall *backtrace-function* (second outcome) (getf result :backtrace))))
                    (t (let ((expansion (expansion-text (first (second outcome)) (getf result :package)
                                                        (tuatara.display:window-width (current-window)))))
                         (if insert
                             (goto (tuatara.buffer:buffer-insert b end (format nil "~%~a" (string-right-trim '(#\Newline) (string-left-trim '(#\Newline) expansion)))))
                             (show-expansion expansion)))))))))))

(define-command pp-macroexpand-last-expression ()
  "Show what the macro call at point expands to, once (MACROEXPAND-1),
pretty-printed in *Pp Macroexpand Output*: the call beginning at point when an
opening bracket is there, else the one before point. With a prefix argument
(\\[universal-argument]), put the expansion in after the call instead.
\\<lisp-mode>\\[pp-macroexpand-last-expression] in Lisp buffers."
  (macroexpand-at-point nil))

(define-command pp-macroexpand-all-last-expression ()
  "Show what the macro call at point expands to, every macro in it expanded
too (SB-CLTL2:MACROEXPAND-ALL), pretty-printed in *Pp Macroexpand Output*;
with a prefix argument (\\[universal-argument]), put it in after the call
instead. \\<lisp-mode>\\[pp-macroexpand-all-last-expression] in Lisp buffers."
  (macroexpand-at-point t))

(defun install-macroexpand ()
  "Bind \\`C-c C-m' and \\`C-c M-m' in Lisp buffers."
  (let ((map (mode-keymap 'lisp-mode)))
    (define-key map "C-c C-m" 'pp-macroexpand-last-expression)
    (define-key map "C-c RET" 'pp-macroexpand-last-expression)
    (define-key map "C-c M-m" 'pp-macroexpand-all-last-expression)))
