;;;; lisp/lsp/hints.lisp --- Inlay hints: what a language server shows within the code, the types and the parameters' names.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A server that gives inlay hints shows, faint within a line, what the code
;;;; leaves unsaid: a binding's type (let total: i32 = ...), the name of the
;;;; parameter an argument goes to (add(a: 1, b: 2)). They are no text of the
;;;; buffer: the cursor passes them by, a copy leaves them out, a save never
;;;; writes them. Asked for again each time the text is sent to the server;
;;;; meanwhile they move with the text typed, so that the line never jumps.
;;;; M-x lsp-inlay-hints-mode turns them off and on. Code lenses, what a server
;;;; says of a line (3 references, Run test), are shown so at the line's end
;;;; when M-x lsp-code-lenses-mode is on, M-x lsp-code-lens running one.

(in-package #:tuatara.defaults)

(defvar *lsp-inlay-hints* t
  "True for language servers' inlay hints to be shown within the code: the
types of bindings, the names of the parameters arguments go to.")

(defvar *lsp-most-hints* 5000
  "The most inlay hints kept for one buffer.")

(defun hint-label (hint)
  "The text HINT, an inlay hint a server sent, shows: its label, with the
spaces it asks for either side."
  (format nil "~:[~; ~]~a~:[~; ~]"
          (tuatara.text:json-true-p (tuatara.text:json-get hint "paddingLeft"))
          (or (tuatara.text:json-text hint "label")
              (format nil "~{~a~}" (map 'list (lambda (part) (or (tuatara.text:json-text part "value") "")) (tuatara.text:json-items hint "label"))))
          (tuatara.text:json-true-p (tuatara.text:json-get hint "paddingRight"))))

(defun lsp-ask-hints (doc)
  "Ask DOC's server that gives inlay hints for those of all it shows, kept
to be shown once they come while its buffer's text is still as asked of."
  (let ((server (and *lsp-inlay-hints* (doc-server-offers doc "inlayHintProvider" "inlay-hints")))
        (buffer (lsp-doc-buffer doc)))
    (when server
      (let ((version (lsp-doc-version doc))
            ;; all of the text as the server has it, changes not yet sent left out
            (shown (if (lsp-doc-shadow doc) (lsp-doc-rope doc) (lsp-doc-sent-rope doc))))
        (lsp-request server "textDocument/inlayHint"
                     (tuatara.text:json-object "textDocument" (doc-identifier doc)
                                               "range" (lsp-range server shown 0 (tuatara.text:rope-bytes shown)))
                     (lambda (result error)
                       (when (and (null error) (doc-unchanged-p doc version))
                         (let ((hints (loop for hint across (tuatara.text:json-items result)
                                            for i below *lsp-most-hints*
                                            for at = (doc-byte doc server (tuatara.text:json-get hint "position"))
                                            when at
                                              collect (list at at (hint-label hint)))))
                           (keep-placed buffer 'lsp-hints doc (and hints (coerce (stable-sort hints #'< :key #'car) 'simple-vector)))
                           (show-hints-in buffer)))))))))

(defun buffer-hints (buffer)
  "BUFFER's inlay hints and code lenses, of all its documents, in order of
place, where the text as it is now has them: each (AT TEXT FACE)."
  (stable-sort (loop for (nil . placed) in (tuatara.buffer:buffer-local buffer 'lsp-hints)
                     append (loop for (at nil text face) across (placed-items-now placed buffer)
                                  collect (list at text (or face :inlay-hint))))
               #'< :key #'car))

(defun hint-replacements (buffer start end)
  "The inlay hints and code lenses of BUFFER from START below END as the
display shows text standing where nothing is hidden: each (AT AT TEXT FACE),
in order of place. Complexity: O(log hints + those there)."
  (let ((found '()))
    (loop for (nil . placed) in (tuatara.buffer:buffer-local buffer 'lsp-hints)
          do (let ((these (loop for i from (placed-first placed buffer (lambda (hint) (>= (first hint) start)))
                                  below (length (placed-items placed))
                                for (at nil text face) = (placed-item placed i buffer)
                                while (< at end)
                                collect (list at at text (or face :inlay-hint)))))
               (setf found (if found (merge 'list found these #'< :key #'car) these))))
    found))

(defun show-hints-in (buffer)
  "Have BUFFER's display show its inlay hints, beside what its mode shows
otherwise (its :display-replacements-function, kept)."
  (unless (tuatara.buffer:buffer-local buffer 'lsp-hints-shown)
    (let ((before (tuatara.buffer:buffer-local buffer :display-replacements-function)))
      (setf (tuatara.buffer:buffer-local buffer 'lsp-hints-shown) t
            (tuatara.buffer:buffer-local buffer 'lsp-replacements-before) before
            (tuatara.buffer:buffer-local buffer :display-replacements-function)
            (if before
                (lambda (b start end)
                  (merge 'list (copy-list (funcall before b start end)) (hint-replacements b start end) #'< :key #'car))
                'hint-replacements)))))

(defun hints-doc (key)
  "The document a key of a buffer's hints is for: the key, or for code lenses its first."
  (if (consp key) (car key) key))

(defun forget-hints (buffer &optional doc)
  "BUFFER's inlay hints and code lenses let go, those of DOC alone when given;
its display as its mode has it once none are left."
  (setf (tuatara.buffer:buffer-local buffer 'lsp-hints)
        (and doc (remove doc (tuatara.buffer:buffer-local buffer 'lsp-hints) :key #'hints-doc)))
  (when (and (tuatara.buffer:buffer-local buffer 'lsp-hints-shown) (null (tuatara.buffer:buffer-local buffer 'lsp-hints)))
    (setf (tuatara.buffer:buffer-local buffer :display-replacements-function)
          (tuatara.buffer:buffer-local buffer 'lsp-replacements-before)
          (tuatara.buffer:buffer-local buffer 'lsp-hints-shown) nil)))

(defun toggle-server-showing (setting ask forget words)
  "Turn SETTING (a variable's symbol), what servers show in buffers, on or
off: on, each buffer's documents asked for it by ASK, a function of a
document; off, each buffer's let go by FORGET, a function of a buffer. Say
it, WORDS naming what it is."
  (let ((on (setf (symbol-value setting) (not (symbol-value setting)))))
    (dolist (buffer tuatara.buffer:*buffers*)
      (if on
          (mapc ask (buffer-docs buffer))
          (funcall forget buffer)))
    (message "~a ~:[off~;on~]" words on)))

(define-command lsp-inlay-hints-mode ()
  "Show the language servers' inlay hints within the code, or no longer:
the types of bindings, the names of the parameters arguments go to."
  (toggle-server-showing '*lsp-inlay-hints* #'lsp-ask-hints #'forget-hints "Inlay hints"))

;;; Code lenses

(defvar *lsp-code-lenses* nil
  "True for what language servers say of lines (3 references, Run test) to be
shown faint at each line's end.")

(defun lens-title (lens)
  "What LENS, a code lens, says: its command's title."
  (tuatara.text:json-text lens "command" "title"))

(defun show-lenses (buffer doc server lenses)
  "Show LENSES, SERVER's code lenses for DOC, at the ends of their lines in
BUFFER, those of a line joined, those with no title yet left out."
  (let ((rope (tuatara.buffer:buffer-text buffer))
        (by-line (make-hash-table)))
    (dolist (lens lenses)
      (let ((line (lens-line doc server lens)))
        (when (and line (lens-title lens))
          (push lens (gethash line by-line)))))
    (let ((items (loop for line being the hash-keys of by-line using (hash-value here)
                       collect (let ((end (line-end-byte rope line)))
                                 (list end end (format nil "  ~{~a~^ | ~}" (mapcar #'lens-title (reverse here)))
                                       :code-lens)))))
      (setf (tuatara.buffer:buffer-local buffer 'lsp-lenses) (cons server lenses))
      (keep-placed buffer 'lsp-hints (cons doc :lenses) (and items (coerce (sort items #'< :key #'car) 'simple-vector)))
      (show-hints-in buffer))))

(defun lens-line (doc server lens)
  "The 0-based line of DOC's buffer LENS, SERVER's code lens, is of, or NIL when it says none."
  (let ((at (doc-byte doc server (tuatara.text:json-get lens "range" "start"))))
    (and at (tuatara.text:byte->line (tuatara.buffer:buffer-text (lsp-doc-buffer doc)) at))))

(defun lsp-ask-lenses (doc)
  "Ask DOC's server that gives code lenses for those of its whole text, shown
once they come while the text is still as asked of; those it gives without
their title asked for it, shown as it comes."
  (let ((server (and *lsp-code-lenses* (null (lsp-doc-shadow doc))
                     (doc-server-offers doc "codeLensProvider" "code-lens"))))
    (when server
      (let ((buffer (lsp-doc-buffer doc))
            (version (lsp-doc-version doc)))
        (lsp-request server "textDocument/codeLens" (tuatara.text:json-object "textDocument" (doc-identifier doc))
                     (lambda (result error)
                       (when (and (null error) (doc-unchanged-p doc version))
                         (let* ((all (remove-if-not #'hash-table-p (tuatara.text:json-items result)))
                                (lenses (coerce (subseq all 0 (min (length all) *lsp-most-hints*)) 'list)))
                           (show-lenses buffer doc server lenses)
                           (when (tuatara.text:json-true-p (tuatara.text:json-get (lsp-server-capabilities server) "codeLensProvider" "resolveProvider"))
                             (dolist (lens lenses)
                               (unless (lens-title lens)
                                 (lsp-request server "codeLens/resolve" lens
                                              (lambda (resolved error)
                                                (when (and (null error) (hash-table-p resolved) (doc-unchanged-p doc version))
                                                  (setf (gethash "command" lens) (tuatara.text:json-get resolved "command"))
                                                  (show-lenses buffer doc server lenses)))))))))))))))

(defun forget-lenses (buffer)
  "BUFFER's code lenses let go."
  (setf (tuatara.buffer:buffer-local buffer 'lsp-hints)
        (remove-if #'consp (tuatara.buffer:buffer-local buffer 'lsp-hints) :key #'car)
        (tuatara.buffer:buffer-local buffer 'lsp-lenses) nil))

(define-command lsp-code-lenses-mode ()
  "Show what language servers say of lines (3 references, Run test) faint at
each line's end, or no longer; \\[lsp-code-lens] runs one."
  (toggle-server-showing '*lsp-code-lenses* #'lsp-ask-lenses #'forget-lenses "Code lenses"))

(define-command lsp-code-lens ()
  "Run what a language server says of the current line (a code lens: Run
test, 3 references), chosen when there are several, as the server's own
command."
  (let* ((buffer (current-buffer))
         (shown (tuatara.buffer:buffer-local buffer 'lsp-lenses))
         (server (car shown))
         (rope (tuatara.buffer:buffer-text buffer))
         (line (tuatara.text:byte->line rope (point)))
         (doc (find-if-not #'lsp-doc-shadow (buffer-docs buffer)))
         (here (and doc server
                    (remove-if-not (lambda (lens) (and (lens-title lens) (eql line (lens-line doc server lens))))
                                   (cdr shown)))))
    (cond ((null shown) (message "No code lenses shown here (~a)" (substitute-command-keys "\\[lsp-code-lenses-mode]")))
          ((null here) (message "No code lens on this line"))
          (t (let ((lens (if (rest here)
                             (completing-read-value "Run: " (mapcar (lambda (l) (make-candidate (lens-title l) :value l)) here)
                                                    :require-match t :keep-order t)
                             (first here))))
               (when (hash-table-p lens)
                 (lsp-do-action server (tuatara.text:json-get lens "command"))))))))
