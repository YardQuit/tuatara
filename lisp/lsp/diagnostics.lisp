;;;; lisp/lsp/diagnostics.lisp --- What language servers find wrong: underlined, marked beside the line, listed.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A server says what it finds wrong in a file as it changes: errors,
;;;; warnings, information and hints, each a range of the text and words. They
;;;; are underlined in the text (:diagnostic-error and the rest), marked beside
;;;; their lines (E, W, i, ·), counted in the mode line, shown in a box under
;;;; the cursor as it rests on one (*LSP-DIAGNOSTICS-DISPLAY*), and listed by
;;;; M-x lsp-diagnostics across the bottom, with what servers find in files not
;;;; open; M-x lsp-next-diagnostic and
;;;; M-x lsp-previous-diagnostic go from one to the next, and so do M-g n and
;;;; M-g p once they have (the list of places in force). Each moves with the
;;;; text typed before the server has looked again.

(in-package #:tuatara.defaults)

(defvar *lsp-diagnostics-display* :box
  "How what a language server found where the cursor rests is said: :box, in a
box under the cursor; :echo, in the echo area; NIL, not at all (the list of
\\[lsp-diagnostics] still has it).")

(defvar *lsp-diagnostics-style* :wave
  "How the text a language server finds something wrong in is marked: :wave,
errors and warnings underlined wavy, information and hints dotted; :dotted,
all dotted; :line, all with a straight line; :background, the text on a
ground of its kind's colour (the theme's error, warning and information
grounds), not underlined. The colours stay the theme's (:diagnostic-error
and the rest, set-face changing them).")

(defvar *diagnostics-style-shown* :wave
  "The *LSP-DIAGNOSTICS-STYLE* the diagnostic faces are underlined in now.")

(defun show-diagnostics-style ()
  "Underline the diagnostic faces as *LSP-DIAGNOSTICS-STYLE* says, when it
changed since they were: all in its kind of line, or with :wave as standard."
  (let ((style *lsp-diagnostics-style*))
    (unless (or (eq style *diagnostics-style-shown*) (not (member style '(:wave :dotted :line :background))))
      (unless (eq style :background)
        (loop for face in '(:diagnostic-error :diagnostic-warning :diagnostic-info :diagnostic-hint)
              for standard in '(:wave :wave :dotted :dotted)
              do (tuatara.display:set-user-face face :underline (case style (:wave standard) (:dotted :dotted) (t t)))))
      (setf *diagnostics-style-shown* style))))

(defstruct (diagnostic (:type list) (:constructor make-diagnostic (start end severity message source)) (:copier nil))
  "What a language server found wrong: from byte START to END of its buffer;
SEVERITY 1 an error, 2 a warning, 3 information, 4 a hint; MESSAGE, its
words; SOURCE, what found it (the server, or a checker of its own)."
  start end severity message source)

(defun diagnostic-table (buffer)
  "What BUFFER's language servers found wrong in it, where the text as it is
now has them, as three values: a list in order of place, the same as a
vector, and a vector giving for each the furthest any up to it reaches (its
end, or the octet after its start when that is further). Worked out once for
each text. Complexity: O(found) when the text changed, else O(1)."
  (let ((found (tuatara.buffer:buffer-local buffer 'lsp-diagnostics))
        (version (tuatara.buffer:buffer-version buffer))
        (kept (tuatara.buffer:buffer-local buffer 'lsp-diagnostics-sorted)))
    (unless (and kept (eq (first kept) found) (eq (second kept) version))
      (let* ((all (loop for (nil . placed) in found append (copy-list (placed-items-now placed buffer))))
             (sorted (if (loop for (a b) on all while b always (<= (diagnostic-start a) (diagnostic-start b)))
                         all
                         (stable-sort all #'< :key #'diagnostic-start)))
             (vector (coerce sorted 'simple-vector))
             (reach (make-array (length vector) :element-type 'fixnum))
             (furthest 0))
        (loop for d across vector
              for i from 0
              do (setf furthest (max furthest (diagnostic-end d) (1+ (diagnostic-start d)))
                       (aref reach i) furthest))
        (setf kept (list found version sorted vector reach)
              (tuatara.buffer:buffer-local buffer 'lsp-diagnostics-sorted) kept)))
    (values (third kept) (fourth kept) (fifth kept))))

(defun buffer-diagnostics (buffer)
  "What BUFFER's language servers found wrong in it, in order of place, where
the text as it is now has them."
  (values (diagnostic-table buffer)))

(defun diagnostics-near (buffer from to)
  "Those of what BUFFER's servers found that start at or before TO and may
reach FROM, in order of place: all that touch FROM to TO among them.
Complexity: O(log found + those between)."
  (multiple-value-bind (all vector reach) (diagnostic-table buffer)
    (declare (ignore all) (type (simple-array fixnum (*)) reach))
    (loop for i from (tuatara.runtime:bisect (mid 0 (length reach)) (< (aref reach mid) from)) below (length vector)
          for d = (svref vector i)
          while (<= (diagnostic-start d) to)
          collect d)))

(defparameter *diagnostic-severities*
  '((1 "error" "E" :error :diagnostic-error :diagnostic-error-ground)
    (2 "warning" "W" :warning :diagnostic-warning :diagnostic-warning-ground)
    (3 "information" "i" :info :diagnostic-info :diagnostic-info-ground)
    (4 "hint" "·" :hint :diagnostic-hint :diagnostic-hint-ground))
  "How each severity of what a language server finds is shown, the worst
first: its number, its word, its mark beside the line, the face of that mark
\(and of its line in the list, and of its kind in the box under the cursor),
and the faces of the text it is found in, underlined or on a ground.")

(defun severity-entry (severity)
  "The entry of SEVERITY in *DIAGNOSTIC-SEVERITIES*: a hint's for any severity not there."
  (or (assoc severity *diagnostic-severities*) (car (last *diagnostic-severities*))))

(defun diagnostic-severity-words (severity)
  "SEVERITY, a diagnostic's, as a word."
  (second (severity-entry severity)))

(defun json-diagnostic (server item start end)
  "ITEM, a Diagnostic SERVER sent, as a DIAGNOSTIC from START to END: an error
when it says no severity known, what found it the server when it says not."
  (make-diagnostic start end (first (severity-entry (or (tuatara.text:json-integer item "severity") 1)))
                   (string-trim '(#\Space #\Newline) (or (tuatara.text:json-text item "message") ""))
                   (or (tuatara.text:json-text item "source") (lsp-server-name server))))

(defvar *lsp-file-diagnostics* (make-hash-table :test 'equal)
  "What language servers found wrong in files not open, by the file's native
name: each a list of (SERVER . DIAGNOSTICS), DIAGNOSTICS as the server sent
them.")

(defun keep-file-diagnostics (server uri items)
  "Keep ITEMS, what SERVER found wrong in the file URI names, not open, when
the file is in its project; the list of them shown again."
  (let ((path (uri-path uri)))
    (when (and path (tuatara.io:really-within-p path (lsp-server-root server)))
      (let ((others (remove server (gethash path *lsp-file-diagnostics*) :key #'car)))
        (if (plusp (length (tuatara.text:json-items items)))
            (setf (gethash path *lsp-file-diagnostics*) (acons server items others))
            (if others (setf (gethash path *lsp-file-diagnostics*) others) (remhash path *lsp-file-diagnostics*))))
      (refill-open-list "*Diagnostics*" 'diagnostics-mode))))

(defun file-diagnostic-entries ()
  "What running servers found wrong in files not open, for the list: each
\(PATH SERVER DIAGNOSTIC-JSON), by file."
  (let ((entries '()))
    (maphash (lambda (path found)
               (unless (buffer-visiting path)
                 (loop for (server . items) in found
                       when (member server *lsp-servers*)
                         do (loop for d across items do (push (list path server d) entries)))))
             *lsp-file-diagnostics*)
    (stable-sort (nreverse entries) #'string< :key #'first)))

(define-lsp-notification "textDocument/publishDiagnostics" (server params)
  (let* ((uri (tuatara.text:json-text params "uri"))
         (doc (and uri (lsp-doc-of-uri uri)))
         (buffer (and doc (lsp-doc-buffer doc)))
         (version (tuatara.text:json-integer params "version")))
    (when (and uri (null doc) (lsp-feature-allowed-p server "diagnostics"))
      (keep-file-diagnostics server uri (tuatara.text:json-items params "diagnostics")))
    ;; in place of all it said of the file before, found in the text of VERSION (that last sent
    ;; when it names none), and moved on with what was typed since it was sent
    (when (and doc (member server (lsp-doc-servers doc)) (lsp-feature-allowed-p server "diagnostics"))
      (let ((found-in (doc-as-sent doc version)))
        (when found-in
          (keep-diagnostics buffer (cons doc server) (diagnostic-entries found-in server (tuatara.text:json-items params "diagnostics")))
          (note-heard doc server version)
          ;; the server has looked at the text: its hints too, when none came before
          (unless (tuatara.buffer:buffer-local buffer 'lsp-hints)
            (lsp-ask-hints doc)))))))

(defun diagnostic-entries (doc server items)
  "ITEMS, a vector of the Diagnostics SERVER gave for DOC, as DIAGNOSTICs;
any that says not where it is left out."
  (loop for d across items
        for (start end) = (multiple-value-list (doc-range-bytes doc server (tuatara.text:json-get d "range")))
        when start
          collect (json-diagnostic server d start end)))

(defun keep-diagnostics (buffer key entries)
  "Keep ENTRIES, DIAGNOSTICs of BUFFER, under KEY (a document and its server,
and how they came), in place of those kept so before; the list of them shown
again."
  (keep-placed buffer 'lsp-diagnostics key entries)
  (refill-open-list "*Diagnostics*" 'diagnostics-mode))

(defun lsp-pull-diagnostics (doc)
  "Ask each of DOC's servers that gives what it finds wrong only when asked
\(diagnosticProvider) for it, kept once it comes while the text is still as
asked of; what it found unchanged since it was last asked kept as it was."
  (let ((buffer (lsp-doc-buffer doc)))
    (dolist (server (lsp-doc-servers doc))
      (when (server-offers-p server "diagnosticProvider" "diagnostics")
        (let* ((version (lsp-doc-version doc))
               (key (list doc server :asked))
               (ids (or (tuatara.buffer:buffer-local buffer 'lsp-diagnostic-ids)
                        (setf (tuatara.buffer:buffer-local buffer 'lsp-diagnostic-ids) (make-hash-table :test 'equal)))))
          (lsp-request server "textDocument/diagnostic"
                       (tuatara.text:json-object "textDocument" (doc-identifier doc)
                                                 "previousResultId" (or (gethash key ids) :omit))
                       (lambda (result error)
                         (when (and (null error) (hash-table-p result) (doc-unchanged-p doc version))
                           (setf (gethash key ids) (tuatara.text:json-text result "resultId"))
                           (when (equal (tuatara.text:json-text result "kind") "full")
                             (keep-diagnostics buffer key (diagnostic-entries doc server (tuatara.text:json-items result "items"))))))))))))

(defun lsp-forget-diagnostics (buffer doc)
  "DOC, of BUFFER, is no longer open in a server: what servers found in it let go."
  (setf (tuatara.buffer:buffer-local buffer 'lsp-diagnostics)
        (remove doc (tuatara.buffer:buffer-local buffer 'lsp-diagnostics) :key #'caar)))

;;; Shown

(defun diagnostic-mark-face (severity)
  "The face of the mark beside a line, of the line in the list, and of the
kind said in the box under the cursor, for what is of SEVERITY."
  (fourth (severity-entry severity)))

(defun diagnostic-face (severity)
  "The face text of SEVERITY is marked in, as *LSP-DIAGNOSTICS-STYLE* has it."
  (let ((entry (severity-entry severity)))
    (if (eq *lsp-diagnostics-style* :background) (sixth entry) (fifth entry))))

(defun diagnostic-highlights (editor window rows)
  "The ranges of the text in ROWS of WINDOW its servers found wrong, underlined."
  (declare (ignore editor))
  (let ((buffer (tuatara.display:window-buffer window)))
    (when (and (tuatara.buffer:buffer-local buffer 'lsp-diagnostics) (plusp (length rows)))
      (show-diagnostics-style)
      (let ((from (tuatara.display:row-start (aref rows 0)))
            (to (tuatara.display:row-end (aref rows (1- (length rows)))))
            (size (tuatara.buffer:buffer-size buffer)))
        (loop for d in (diagnostics-near buffer from to)
              for start = (min (diagnostic-start d) size)
              ;; one of no width marks the character it stands before
              for end = (min size (max (diagnostic-end d) (1+ start)))
              when (and (< start to) (> end from))
                collect (list start end (diagnostic-face (diagnostic-severity d))))))))

(defun diagnostic-gutter-width (editor window)
  "One column for marks while the buffer has something its servers found wrong."
  (declare (ignore editor))
  (if (tuatara.buffer:buffer-local (tuatara.display:window-buffer window) 'lsp-diagnostics) 1 0))

(defun diagnostic-lines (buffer from to)
  "The worst of what BUFFER's servers found on each line whose start is from
byte FROM to byte TO, by where each starts: a list of (LINE . SEVERITY).
Complexity: O(log found + those there)."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (size (tuatara.text:rope-bytes rope))
         (vector (nth-value 1 (diagnostic-table buffer)))
         (lines '()))
    (loop for i from (tuatara.runtime:bisect (mid 0 (length vector)) (< (min size (diagnostic-start (svref vector mid))) from))
            below (length vector)
          for d = (svref vector i)
          for start = (min size (diagnostic-start d))
          while (<= start to)
          do (let* ((line (tuatara.text:byte->line rope start))
                    (entry (assoc line lines)))
               (if entry
                   (setf (cdr entry) (min (cdr entry) (diagnostic-severity d)))
                   (push (cons line (diagnostic-severity d)) lines))))
    lines))

(defun diagnostic-gutter-prepare (editor window rows)
  "The worst found on each line of ROWS of WINDOW, worked out once a frame (DIAGNOSTIC-LINES)."
  (declare (ignore editor))
  (let ((buffer (tuatara.display:window-buffer window)))
    (when (and (plusp (length rows)) (tuatara.buffer:buffer-local buffer 'lsp-diagnostics))
      (diagnostic-lines buffer (tuatara.buffer:buffer-line-start buffer (tuatara.display:row-start (aref rows 0)))
                        (tuatara.buffer:buffer-line-end buffer (tuatara.display:row-end (aref rows (1- (length rows)))))))))

(defun diagnostic-gutter-cell (editor window row line &optional (lines nil prepared))
  "The mark beside ROW of the worst its servers found on LINE: E, W, i or ·;
from LINES, as DIAGNOSTIC-GUTTER-PREPARE gives them for the frame, when given."
  (declare (ignore editor row))
  (let ((buffer (tuatara.display:window-buffer window)))
    (if (null line)
        (values " " nil)
        (let ((worst (cdr (assoc line (if prepared
                                          lines
                                          (let ((start (tuatara.text:line->byte (tuatara.buffer:buffer-text buffer) line)))
                                            (diagnostic-lines buffer start (tuatara.buffer:buffer-line-end buffer start))))))))
          (if (assoc worst *diagnostic-severities*)
              (values (third (severity-entry worst)) (diagnostic-mark-face worst))
              (values " " nil))))))

(defun diagnostic-mode-line (editor buffer)
  "How many errors and warnings BUFFER's servers found, for the mode line."
  (declare (ignore editor))
  (let ((all (buffer-diagnostics buffer)))
    (when all
      (let ((errors (count 1 all :key #'diagnostic-severity))
            (warnings (count 2 all :key #'diagnostic-severity)))
        (when (plusp (+ errors warnings))
          (list (format nil "~:[~*~;✘~d~]~:[~*~;~:*~:[~; ~]~*~]~:[~*~;⚠~d~]" (plusp errors) errors
                        (plusp warnings) (and (plusp errors) (plusp warnings)) (plusp warnings) warnings)
                (if (plusp errors) :error :warning)))))))

(defun diagnostic-words (diagnostic)
  "DIAGNOSTIC as a line of words: its kind, what found it, and what it says."
  (format nil "~a (~a): ~a" (diagnostic-severity-words (diagnostic-severity diagnostic)) (diagnostic-source diagnostic)
          (tuatara.text:one-line (diagnostic-message diagnostic))))

(defun diagnostics-in (buffer start end)
  "All that BUFFER's servers found from START to END, touching either, the worst first."
  (stable-sort (remove-if-not (lambda (d) (and (<= (diagnostic-start d) end) (<= start (max (diagnostic-end d) (diagnostic-start d)))))
                              (diagnostics-near buffer start end))
               #'< :key #'diagnostic-severity))

(defun diagnostics-at (buffer position)
  "All that BUFFER's servers found at POSITION, the worst first."
  (diagnostics-in buffer position position))

(defun diagnostic-say (editor)
  "With the cursor resting on something a server found, say it as
*LSP-DIAGNOSTICS-DISPLAY* has it: each in the box under the cursor, its kind
and what found it over what it says; or the worst in the echo area when
nothing else is said there. True when something was said."
  (let* ((buffer (current-buffer))
         (here (and *lsp-diagnostics-display* (tuatara.buffer:buffer-local buffer 'lsp-diagnostics)
                    (not (editor-minibuffer-active editor))
                    (diagnostics-at buffer (point)))))
    (when here
      (ecase *lsp-diagnostics-display*
        (:box (show-note editor buffer (point)
                         (loop for d in here
                               collect (list (format nil "~:(~a~) (~a)" (diagnostic-severity-words (diagnostic-severity d))
                                                     (diagnostic-source d))
                                             (diagnostic-mark-face (diagnostic-severity d)))
                               collect (list (diagnostic-message d) nil))))
        (:echo (when (null (editor-message editor))
                 (echo "~a" (diagnostic-words (first here))))))
      t)))

;;; Moving and listing

(defstruct (diagnostic-places (:constructor make-diagnostic-places (buffer)) (:copier nil))
  "What the language servers found wrong in BUFFER, as a list of places
\(STEP-PLACE): \\[next-place] and \\[previous-place] go from one to the next."
  (buffer nil :read-only t))

(setf (documentation 'diagnostic-places-p 'function) "True when OBJECT is a DIAGNOSTIC-PLACES."
      (documentation 'diagnostic-places-buffer 'function) "The buffer whose diagnostics a DIAGNOSTIC-PLACES goes through.")

(defmethod places-buffer ((places diagnostic-places))
  "The buffer whose diagnostics PLACES goes through."
  (diagnostic-places-buffer places))

(defmethod places-name ((places diagnostic-places))
  "What the language servers found wrong in its buffer: the diagnostics of notes.rs."
  (format nil "the diagnostics of ~a" (tuatara.buffer:buffer-name (diagnostic-places-buffer places))))

(defmethod step-place ((places diagnostic-places) direction)
  "Go to the next place after point the servers found something wrong in the
buffer (DIRECTION 1), or before it (-1), that buffer shown here, and say what."
  (let ((buffer (diagnostic-places-buffer places)))
    (if (live-buffer-p buffer)
        (progn (unless (eq buffer (current-buffer)) (show-buffer buffer))
               (goto-diagnostic (plusp direction)))
        (message "~a has been killed" (tuatara.buffer:buffer-name buffer)))))

(defun goto-diagnostic (forward)
  "Go to the next of what the servers found after point, FORWARD, else before
it, and say it; what they found in this buffer is the list of places in force
then (USE-PLACES)."
  (unless (let ((places *places-in-force*))
            (and (diagnostic-places-p places) (eq (diagnostic-places-buffer places) (current-buffer))))
    (use-places (make-diagnostic-places (current-buffer))))
  (let* ((all (buffer-diagnostics (current-buffer)))
         (here (point))
         (d (if forward
                (find-if (lambda (d) (> (diagnostic-start d) here)) all)
                (find-if (lambda (d) (< (diagnostic-start d) here)) all :from-end t))))
    (cond ((null all) (message "Nothing found wrong here~@[ (no language server)~]" (null (buffer-servers (current-buffer)))))
          ((null d) (say-places-end *places-in-force* (if forward 1 -1)))
          (t (goto-shown (current-buffer) (diagnostic-start d))
             (unless (diagnostic-say *editor*)
               (message "~a" (diagnostic-words d)))))))

(define-command lsp-next-diagnostic ()
  "Go to the next place the language servers found something wrong, and say what."
  (goto-diagnostic t))

(define-command lsp-previous-diagnostic ()
  "Go to the place before point where the language servers found something wrong, and say what."
  (goto-diagnostic nil))

(define-major-mode diagnostics-mode special-mode
  "What the language servers found wrong in the open files and the files of
their projects (\\[lsp-diagnostics]), a line each: where, how bad, what found it and what it says. \\<diagnostics-mode>\\[list-visit] goes there, in another window,
\\[revert-buffer] lists them again (they are listed again as servers find more); \\[quit-window] closes."
  :lighter "Diagnostics" :interactive nil)

(defvar *diagnostics-keymap* (mode-keymap 'diagnostics-mode)
  "Keys of the list of diagnostics (diagnostics-mode's keymap).")

(defun file-diagnostic-piece (path server item)
  "The line of the list of diagnostics for ITEM, a Diagnostic SERVER sent of
the file at PATH, not open: where the server says, as it counts."
  (let ((d (json-diagnostic server item 0 0)))
    (list (format nil "  ~a:~d:~d  ~a~%" (abbreviated-path path)
                  (1+ (or (tuatara.text:json-integer item "range" "start" "line") 0))
                  (1+ (or (tuatara.text:json-integer item "range" "start" "character") 0))
                  (diagnostic-words d))
          (diagnostic-mark-face (diagnostic-severity d)))))

(defun buffer-diagnostic-piece (buffer d)
  "The line of the list of diagnostics for D, a DIAGNOSTIC of BUFFER."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (start (min (diagnostic-start d) (tuatara.text:rope-bytes rope)))
         (line (tuatara.text:byte->line rope start)))
    (list (format nil "  ~a:~d:~d  ~a~%" (tuatara.buffer:buffer-name buffer) (1+ line)
                  (1+ (- start (tuatara.text:line->byte rope line))) (diagnostic-words d))
          (diagnostic-mark-face (diagnostic-severity d)))))

(defun diagnostics-show (buffer &optional (point 1))
  "Fill BUFFER, the list of what the servers found, point on line POINT, or with :keep where it was."
  (let ((entries (append (loop for b in tuatara.buffer:*buffers*
                               append (loop for d in (buffer-diagnostics b) collect (cons b d)))
                         (file-diagnostic-entries))))
    (render-pieces buffer
                   (cons (format nil "  Found by the language servers: ~d~%" (length entries))
                         (loop for entry in entries
                               collect (if (stringp (first entry))
                                           (apply #'file-diagnostic-piece entry)
                                           (buffer-diagnostic-piece (car entry) (cdr entry)))))
                   :point point :entries (coerce entries 'vector) :first-line 1
                   :refill (lambda (buffer) (diagnostics-show buffer :keep)))))

(define-command lsp-diagnostics ()
  "List what the language servers found wrong in the open files, a line each,
then in the files of their projects not open, in a window across the bottom;
RET there goes to it, opening the file."
  (let ((buffer (buffer-in-mode "*Diagnostics*" 'diagnostics-mode)))
    (diagnostics-show buffer)
    (display-buffer buffer)))

(defmethod list-entry-noun ((mode diagnostics-mode))
  "A diagnostic, what a language server found."
  "diagnostic")

(defmethod visit-entry ((mode diagnostics-mode) list entry other)
  "Go to the place ENTRY, a line of the list of diagnostics, names, in another
window always."
  (declare (ignore list other))
  (cond ((stringp (first entry))
         ;; a file not open: opened, and the place found as its server counts
         (destructuring-bind (path server d) entry
           (multiple-value-bind (buffer at) (uri-place server (path-uri path) (tuatara.text:json-get d "range" "start") :visit t)
             (if (null buffer)
                 (message "~a cannot be opened" (abbreviated-path path))
                 (progn (pop-to-buffer buffer :other t)
                        (goto-shown buffer at))))))
        ((live-buffer-p (car entry))
         (use-places (make-diagnostic-places (car entry)))
         (pop-to-buffer (car entry) :other t)
         (goto-shown (car entry) (min (diagnostic-start (cdr entry)) (tuatara.buffer:buffer-size (car entry))))
         (message "~a" (diagnostic-words (cdr entry))))
        (t (message "~a has been killed" (tuatara.buffer:buffer-name (car entry))))))

(defun install-diagnostics ()
  "Show what the language servers find: underlined, beside the lines, in the mode line."
  (pushnew 'diagnostic-highlights *highlight-functions*)
  (pushnew 'diagnostic-mode-line *mode-line-functions*)
  (add-gutter-provider :diagnostics 'diagnostic-gutter-width 'diagnostic-gutter-cell :prepare 'diagnostic-gutter-prepare)
  (define-key *diagnostics-keymap* "RET" 'list-visit)
  (commands-for-mode 'diagnostics-mode "the list of diagnostics (\\[lsp-diagnostics])" 'list-visit))

(defun install-diagnostics-place ()
  "Show the list of what the language servers found in a window across the
bottom, 30% of the height, kept for it and selected."
  (place-buffers 'diagnostics-mode :bottom :size 0.3 :keep t :select t))

(defun install-lsp (editor)
  "Have files opened in their projects' language servers, kept in step, what
the servers find shown, and the servers ended as the editor stops."
  (declare (ignore editor))
  (pushnew 'lsp-tick *tick-functions*)
  (setf tuatara.buffer:*find-file-functions* (append (remove 'lsp-visit tuatara.buffer:*find-file-functions*) (list 'lsp-visit)))
  (pushnew 'lsp-killed tuatara.buffer:*kill-buffer-functions*)
  (pushnew 'lsp-saved *after-save-functions*)
  (pushnew 'lsp-stop-all *exit-functions*)
  (pushnew 'lsp-outside-work *outside-work-functions*)
  (install-diagnostics)
  (install-semantic-tokens)
  (install-lsp-watching)
  (pushnew 'lsp-selection-ranges *selection-ranges-functions*)
  (install-org-blocks))
