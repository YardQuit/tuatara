;;;; test/keys.lisp --- Keys named in text: \[command] and the rest.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test keys-in-text
  :description "Help, documentation and messages name a command's keys as \\[COMMAND]: shown as the
keys bound at the time, a rebinding included, in :help-key-binding; \\<KEYMAP> names the keymap to
look in, \\{KEYMAP} gives its summary, \\`KEYS' a key sequence as it is, \\= quotes what follows;
every docstring names real commands and keymaps.")

(defmacro with-editor-session ((editor &rest keys) &body body)
  "Run BODY with EDITOR, a session run through KEYS (as RUN-KEYS takes them),
the editor TUATARA.COMMAND:*EDITOR* is bound to."
  (let ((f (gensym "FRONTEND")))
    `(multiple-value-bind (,f ,editor) (run-keys ,@(or keys (list "")))
       (declare (ignorable ,f))
       (let ((tuatara.command:*editor* ,editor))
         ,@body))))

(define-test (keys-in-text substituted)
  (with-editor-session (editor)
    (flet ((keys (text &rest options) (apply #'tuatara.command:substitute-command-keys text options)))
      (is string= "visit with C-x C-f" (keys "visit with \\[find-file]"))
      ;; no key: M-x and the command
      (is string= "run M-x mark-defun" (keys "run \\[mark-defun]"))
      ;; a keymap named first: the NORMAL state's, a major mode's, then the global one after it
      (is string= "y copies" (keys "\\<*tuatara-normal-keymap*>\\[copy-selection-or-char] copies"))
      (is string= "r refreshes" (keys "\\<buffers-mode>\\[revert-buffer] refreshes"))
      (is string= "C-x C-f" (keys "\\<buffers-mode>\\[find-file]"))
      ;; a major mode's key only where its keymap is named: it works in its buffers alone
      (is string= "M-x list-mark-entry" (keys "\\[list-mark-entry]"))
      (is string= "m" (keys "\\<buffers-mode>\\[list-mark-entry]"))
      ;; the keys every list has, from special-mode's keymap under its own
      (is string= "r" (keys "\\<help-mode>\\[revert-buffer]"))
      ;; the fewest keys when there are several
      (is string= "C-/" (keys "\\[undo]"))
      ;; a key sequence as it is, and quoting
      (is string= "type C-u 3 M-u" (keys "type \\`C-u 3 M-u'"))
      (is string= "not \\`a key!' here" (keys "not \\`a key!' here"))
      (is string= "the \\[find-file] syntax" (keys "the \\=\\[find-file] syntax"))
      (is string= "no backslash" (keys "no backslash"))
      ;; a keymap's summary
      (let ((summary (keys "\\{buffers-mode}")))
        (true (cl-ppcre:scan "(?m)^r +revert-buffer" summary))
        (true (cl-ppcre:scan "(?m)^q +quit-window" summary)))
      ;; the keys' places, kept for the text; marked for FORMAT, a ~ doubled
      (multiple-value-bind (text spans) (tuatara.command:unmark-keys (keys "a \\[find-file] b" :marks t))
        (is string= "a C-x C-f b" text)
        (is equal '((2 9 :help-key-binding)) spans)
        (is equal spans (tuatara.command:text-spans text)))
      (is string= "tilde ~ as is" (format nil (tuatara.command:unmark-keys (keys "tilde \\`~' as is" :marks t :for-format t)))))))

(define-test (keys-in-text follow-rebinding)
  ;; a command bound anew is named by its new keys
  (with-editor-session (editor)
    (let ((global (tuatara.command:editor-global-keymap editor)))
      (unwind-protect
           (progn
             (tuatara.command:define-key global "C-c t" 'tuatara.defaults::mark-defun)
             (is string= "C-c t marks" (tuatara.command:substitute-command-keys "\\[mark-defun] marks")))
        (tuatara.command:define-key global "C-c t" nil))
      (is string= "M-x mark-defun marks" (tuatara.command:substitute-command-keys "\\[mark-defun] marks")))))

(define-test (keys-in-text coloured-where-shown)
  ;; a message's keys, in the echo area
  (with-editor-session (editor "" :width 80 :height 8)
    (let ((text (tuatara.command:message "Visit with \\[find-file], ~a" "then")))
      (is string= "Visit with C-x C-f, then" text)
      (is equal '((11 18 :help-key-binding)) (tuatara.command:text-spans (tuatara.command:editor-message editor)))
      (is equal '((11 18 :help-key-binding))
          (third (first (last (tuatara.command:panel-rows editor)))))))
  ;; help shows a docstring's keys as keys, coloured
  (multiple-value-bind (f editor) (run-keys "C-h x f i l l - p a r a g r a p h RET" :text "" :width 100 :height 30)
    (declare (ignore editor))
    (let* ((b (tuatara.buffer:find-buffer "*Help*"))
           (text (tuatara.buffer:buffer-string b))
           (spans (coerce (car (tuatara.buffer:buffer-local b 'tuatara.defaults::made-spans)) 'list)))
      (true (search "M-q" text))
      (true (find :help-key-binding spans :key #'third))
      (true (some (lambda (span) (string= "M-q" (subseq text (first span) (second span)))) spans))
      (true (tuatara.frontend.fake:fake-frame-showing f "M-q")))))

(defun docstring-references (text)
  "The keys TEXT, a docstring, names: a list of (KIND NAME), KIND :COMMAND for
\\[NAME], :KEYMAP for \\<NAME> and \\{NAME}, :KEYS for \\`NAME'."
  (let ((out '()) (i 0) (n (length text)))
    (loop while (< i n)
          do (let ((c (char text i)) (next (and (< (1+ i) n) (char text (1+ i)))))
               (cond ((not (and (char= c #\\) next)) (incf i))
                     ((char= next #\=) (incf i 3))
                     ((find next "[<{`")
                      (let ((close (position (case next (#\[ #\]) (#\< #\>) (#\{ #\}) (#\` #\')) text :start (+ i 2))))
                        (when close
                          (push (list (case next (#\[ :command) (#\` :keys) (t :keymap)) (subseq text (+ i 2) close)) out))
                        (setf i (if close (1+ close) (+ i 2)))))
                     (t (incf i 2)))))
    (nreverse out)))

(define-test (keys-in-text docstrings-name-real-commands)
  ;; every \[COMMAND] in the editor's documentation (its modes' and faces' too) names a
  ;; command, every \<KEYMAP> a keymap
  (with-editor-session (editor)
    (let ((bad '()))
      (dolist (package (tuatara-packages))
        (do-symbols (symbol package)
          (when (eq (symbol-package symbol) package)
            (dolist (kind '(function variable type))
              (let ((doc (ignore-errors (documentation symbol kind))))
                (when doc
                  (loop for (what name) in (docstring-references doc)
                        do (unless (ecase what
                                     (:command (tuatara.command:command-symbol name))
                                     (:keymap (tuatara.command:keymap-called name))
                                     (:keys (tuatara.command:valid-key-text-p name)))
                             (push (format nil "~s names ~(~a~) ~s" symbol what name) bad)))))))))
      ;; and the faces' documentation
      (maphash (lambda (name face)
                 (let ((doc (tuatara.display:face-doc face)))
                   (when doc
                     (loop for (what reference) in (docstring-references doc)
                           do (unless (ecase what
                                        (:command (tuatara.command:command-symbol reference))
                                        (:keymap (tuatara.command:keymap-called reference))
                                        (:keys (tuatara.command:valid-key-text-p reference)))
                                (push (format nil "face ~s names ~(~a~) ~s" name what reference) bad))))))
               tuatara.display:*faces*)
      (true (null bad) "Docstrings naming what is not there: ~{~a~^; ~}" (sort bad #'string<)))))

(define-test (keys-in-text standard-keys-without-the-leader)
  ;; the leader is empty as standard; what it once held has keys of its own under C-x, C-c and M-g
  (true (null tuatara.defaults::*tuatara-leader-bindings*))
  (with-editor-session (editor)
    (let ((global (tuatara.command:editor-global-keymap editor)))
      (loop for (keys command) in '(("C-x b" switch-to-buffer) ("C-x x b" switch-other-buffer) ("C-x C-b" buffers) ("C-x C-r" find-recent-file)
                                    ("M-g o" buffer-outline) ("C-x C-j" rimu-jump) ("C-x n f" find-file-formatted) ("C-x n o" open-file-formatted)
                                    ("C-x n n" new-file-formatted) ("C-x n g" search-in-formatted) ("C-x n j" journal-new-or-existing-entry)
                                    ("C-x n a" calendar) ("C-x n e" new-empty-buffer) ("C-x a a" org-agenda) ("C-x a c" org-capture)
                                    ("C-x a l" org-store-link) ("C-x a o" org-clock-out) ("C-x T h" hl-line-mode) ("C-x T v" visual-line-mode)
                                    ("C-x w h" swap-window-left) ("C-x w t" transpose-window-layout) ("C-x 4 0" kill-buffer-and-window)
                                    ("C-x 8 &" insert-digraph) ("C-M-h" select-window-left))
            do (is eq (find-symbol (string-upcase (symbol-name command)) :tuatara.defaults)
                   (tuatara.command:lookup-key global (tuatara.frontend:parse-key-sequence keys))
                   "~a" keys)))))

(define-test (keys-in-text org-keys-first-for-point)
  ;; C-c's hints show first, under a title, the keys for what the cursor is on in Org
  (let ((tuatara.defaults::*modal-editing* nil)
        (text (format nil "* A heading~%See [[https://example.org][a link]] here.~%| a | b |~%| 1 | 2 |~%")))
    (flet ((hints (keys)
             (run-keys (format nil "M-x o r g - m o d e RET M-< ~a C-c" keys) :text text :width 120 :height 24)))
      (let ((f (hints "C-n C-f C-f C-f C-f C-f C-f")))
        (true (tuatara.frontend.fake:fake-frame-showing f "link at point"))
        (true (tuatara.frontend.fake:fake-frame-showing f "C-o org-open-at-point")))
      (true (tuatara.frontend.fake:fake-frame-showing (hints "C-n C-n C-f C-f") "table at point"))
      (true (tuatara.frontend.fake:fake-frame-showing (hints "") "heading at point"))
      ;; on plain text: the hints as before, no title
      (false (tuatara.frontend.fake:fake-frame-showing (hints "C-n C-e") (format nil " at point ~c" (code-char #x2500)))))))

(define-test (keys-in-text one-order-of-keys)
  ;; the fewest keys first, then the shortest to read, then by name
  (is equal '("M-q" "S-SPC" "g q" "C-x f" "C-x C-f")
      (sort (list "C-x C-f" "g q" "S-SPC" "C-x f" "M-q") #'tuatara.command:key-description<))
  ;; the tutor names the keys a command's documentation names in NORMAL state
  (run-keys "C-g")
  (multiple-value-bind (text spans) (tuatara.defaults::tutor-text)
    (let* ((octets (tuatara.text:string-octets text))
           (keys (mapcar (lambda (span) (tuatara.text:octets-string octets :start (car span) :end (cdr span))) spans))
           (names (cl-ppcre:all-matches-as-strings "\\\\\\[[a-z-]+\\]" tuatara.defaults::*tuatara-tutor-text*)))
      (dolist (name (remove-duplicates names :test #'string=))
        (let* ((command (find-symbol (string-upcase (subseq name 2 (1- (length name)))) :tuatara.defaults))
               (key (tuatara.command:command-key command (list tuatara.defaults:*tuatara-normal-keymap*))))
          (when key
            (true (member key keys :test #'string=) "~a" name)))))))

(define-test (keys-in-text child-keymap-shadows-its-parent)
  ;; a key the child binds again runs the child's command; one it takes away runs nothing
  (let* ((parent (tuatara.command:make-keymap :name "parent"))
         (child (tuatara.command:make-keymap :name "child" :parent parent)))
    (tuatara.command:define-keys parent '(("C-c a" parent-command) ("C-c b" other-parent-command) ("C-c d" kept-command)))
    (tuatara.command:define-keys child '(("C-c a" child-command) ("C-c b" :undefined)))
    (let ((summary (tuatara.command:keymap-summary child)))
      (true (search "child-command" summary))
      (false (search "parent-command" summary))
      (true (search "kept-command" summary)))
    (is equal '() (tuatara.command:where-is 'parent-command (list child)))
    (is equal '() (tuatara.command:where-is 'other-parent-command (list child)))
    (is equal '("C-c a") (tuatara.command:where-is 'child-command (list child)))
    (is equal '("C-c d") (tuatara.command:where-is 'kept-command (list child)))
    ;; an earlier keymap in the list shadows a later one
    (let ((later (tuatara.command:make-keymap :name "later")))
      (tuatara.command:define-key later "C-c a" 'later-command)
      (is equal '() (tuatara.command:where-is 'later-command (list child later))))))

(define-test (keys-in-text keymap-entries-own-and-inherited)
  ;; a keymap's entries with its parents', the child's shadowing, or its own alone
  (let* ((parent (tuatara.command:make-keymap :name "parent"))
         (child (tuatara.command:make-keymap :name "child" :parent parent)))
    (tuatara.command:define-keys parent '(("a" parent-command) ("b" kept-command)))
    (tuatara.command:define-key child "a" 'child-command)
    (flet ((commands (entries) (sort (mapcar #'cdr entries) #'string<)))
      (is equal '(child-command kept-command) (commands (tuatara.command:keymap-entries child)))
      (is equal '(child-command) (commands (tuatara.command:keymap-entries child :own t))))))

(define-test (keys-in-text default-takes-every-other-key)
  ;; a keymap with a default takes every key it does not bind, prefixes of later keymaps too
  (let ((menu (tuatara.command:make-keymap :name "menu"))
        (later (tuatara.command:make-keymap :name "later")))
    (tuatara.command:define-key menu "a" 'menu-command)
    (tuatara.command:define-keys later '(("b" later-command) ("C-x C-s" saving-command)))
    (setf (tuatara.command::keymap-default menu) 'caught-command)
    (flet ((runs (keys) (tuatara.command::keymaps-lookup (list menu later) (tuatara.frontend:parse-key-sequence keys))))
      (is eq 'menu-command (runs "a"))
      (is eq 'caught-command (runs "b"))
      (is eq 'caught-command (runs "C-x")))
    ;; without one, the later keymap's key
    (setf (tuatara.command::keymap-default menu) nil)
    (is eq 'later-command (tuatara.command::keymaps-lookup (list menu later) (tuatara.frontend:parse-key-sequence "b")))))

(define-test (keys-in-text bindings-held-while-walked)
  ;; a key bound on another thread waits until a walk of the keymaps has ended
  (let ((map (tuatara.command:make-keymap :name "walked"))
        (bound-meanwhile :unknown))
    (tuatara.command:define-key map "C-c a" 'walked-command)
    (tuatara.command:map-keymap-bindings
     (lambda (keys command)
       (declare (ignore keys command))
       (let ((binder (sb-thread:make-thread (lambda () (tuatara.command:define-key map "C-c b" 'later-command)))))
         (sleep 0.2)
         (setf bound-meanwhile (sb-thread:thread-alive-p binder))))
     (list map))
    (true bound-meanwhile "the binding waited")
    (loop repeat 100 until (tuatara.command:lookup-key map (tuatara.frontend:parse-key-sequence "C-c b")) do (sleep 0.01))
    (is eq 'later-command (tuatara.command:lookup-key map (tuatara.frontend:parse-key-sequence "C-c b")))))

(define-test (keys-in-text new-keymap-keeps-what-was-worked-out)
  ;; a keymap made for a menu or a question changes no binding: the keys found stay found
  (let ((map (tuatara.command:make-keymap :name "kept")))
    (tuatara.command:define-key map "C-c k" 'kept-command)
    (let ((table (tuatara.command::where-is-table (list map))))
      (tuatara.command:make-keymap :name "another")
      (is eq table (tuatara.command::where-is-table (list map))))))

(define-test (keys-in-text recorded-keys-help-names-the-quit-key)
  ;; the help of recording and playing keys names the key that really gives them up, never a
  ;; command no key runs
  (with-editor-session (editor)
    (dolist (command '(tuatara.defaults::start-recording-keys tuatara.defaults::play-recorded-keys))
      (let ((help (tuatara.command:substitute-command-keys (documentation command 'function))))
        (false (search "M-x keyboard-quit" help) "~(~a~)" command)
        (true (search "the quit key" help :test #'char-equal) "~(~a~)" command)
        (true (search "C-g" help) "~(~a~)" command)))))
