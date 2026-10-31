;;;; test/lsp.lisp --- Language servers and formatters: JSON, which programs serve which language.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test lsp
  :description "JSON read and written as language servers send it, what is not JSON refused with where;
which servers and formatter each language has, from what Helix knows or as chosen in the init file;
what marks a project; check-health saying which are installed.")

(define-test (lsp json-read-and-written)
  (let ((value (tuatara.text:json-read "{\"id\": 3, \"result\": {\"items\": [1, -2.5, 1e3, \"a\\u00e9\\n\", true, false, null]}}")))
    (is = 3 (tuatara.text:json-get value "id"))
    (let ((items (tuatara.text:json-get value "result" "items")))
      (is = 7 (length items))
      (is = 1 (aref items 0))
      (is = -2.5d0 (aref items 1))
      (is = 1000d0 (aref items 2))
      (is string= (format nil "aé~%") (aref items 3))
      (is eq t (aref items 4))
      (is eq :false (aref items 5))
      (is eq :null (aref items 6))
      (false (tuatara.text:json-get value "result" "items" 6))
      (false (tuatara.text:json-get value "nothing" "here"))))
  ;; a character beyond the first plane, written as two escapes; one alone stands as U+FFFD
  (is string= (string (code-char #x1F600)) (tuatara.text:json-read "\"\\ud83d\\ude00\""))
  (is string= (string (code-char #xFFFD)) (tuatara.text:json-read "\"\\ud83d\""))
  ;; written and read back the same
  (let* ((object (tuatara.text:json-object "jsonrpc" "2.0" "id" 1 "params" (tuatara.text:json-object "text" (format nil "a \"b\"~%	c~c" (code-char 1))
                                                                                                      "list" #(1 2 3) "flag" :false "none" nil
                                                                                                      "left out" :omit)))
         (text (tuatara.text:json-write object))
         (back (tuatara.text:json-read text)))
    (is string= "2.0" (tuatara.text:json-get back "jsonrpc"))
    (is string= (format nil "a \"b\"~%	c~c" (code-char 1)) (tuatara.text:json-get back "params" "text"))
    (is equalp #(1 2 3) (tuatara.text:json-get back "params" "list"))
    (is eq :false (tuatara.text:json-get back "params" "flag"))
    (is eq :null (gethash "none" (tuatara.text:json-get back "params")))
    (false (nth-value 1 (gethash "left out" (tuatara.text:json-get back "params")))))
  (is string= "[1.5,true,false,null,[]]" (tuatara.text:json-write (list 1.5d0 t :false :null #()))))

(define-test (lsp json-refused)
  ;; what another program sends is never trusted: anything not JSON is refused, saying where
  (dolist (text '("" "{" "[1,]" "{\"a\" 1}" "tru" "01x" "\"a" "[1] 2" "{\"a\":1,}" "\"\\x\"" "-" "1." "1e"))
    (fail (tuatara.text:json-read text) tuatara.text:json-error))
  (is = 3 (handler-case (tuatara.text:json-read "[1, ?]") (tuatara.text:json-error (e) (- (tuatara.text:json-error-position e) 1))))
  ;; nesting past the bound refused, not followed down until the stack runs out
  (fail (tuatara.text:json-read (concatenate 'string (make-string 100000 :initial-element #\[) (make-string 100000 :initial-element #\])))
      tuatara.text:json-error)
  ;; a number's exponent bounded: neither a hang nor an overflow
  (is = most-positive-double-float (tuatara.text:json-read "1e999999999"))
  (is = 0d0 (tuatara.text:json-read "1e-999999999"))
  (is = 12345678901234567890123 (tuatara.text:json-read "12345678901234567890123")))

(define-test (lsp tools-of-languages)
  ;; from what Helix knows: Rust's server and the files that mark its project
  (let ((rust (tuatara.defaults::language-server-commands 'rust)))
    (is equal '("rust-analyzer") (mapcar (lambda (s) (getf s :name)) rust))
    (is equal '("rust-analyzer") (getf (first rust) :command)))
  (true (member "Cargo.toml" (tuatara.defaults::language-roots 'rust) :test #'string=))
  ;; a language Tuatara names otherwise than Helix: Lisp
  (true (tuatara.defaults::language-tools 'lisp))
  ;; chosen in the init file: by a known server's name, by a program of one's own, or none
  (let ((tuatara-user:*language-servers* '((python "basedpyright" ("my-server" "--stdio")) (markdown)))
        (tuatara-user:*language-formatters* '((python "ruff" "format" "-") (rust))))
    (is equal '(("basedpyright-langserver" "--stdio") ("my-server" "--stdio"))
        (mapcar (lambda (s) (getf s :command)) (tuatara.defaults::language-server-commands 'python)))
    (false (tuatara.defaults::language-server-commands 'markdown))
    (is equal '("ruff" "format" "-") (tuatara.defaults::language-formatter-command 'python))
    (false (tuatara.defaults::language-formatter-command 'rust)))
  ;; the usual formatters of languages whose known tools leave it to their servers
  (is equal "rustfmt" (first (tuatara.defaults::language-formatter-command 'rust)))
  (is equal '("shfmt") (tuatara.defaults::language-formatter-command 'bash))
  (is equal '("clang-format") (tuatara.defaults::language-formatter-command 'c))
  ;; said by check-health: a program not installed marked
  (let ((tuatara-user:*language-servers* '((python ("tuatara-no-such-server" "--stdio")) (rust ("sh"))))
        (tuatara-user:*language-formatters* '((python "tuatara-no-such-formatter"))))
    (multiple-value-bind (formatter servers) (tuatara.defaults::language-tools-words 'python)
      (is string= "tuatara-no-such-formatter" formatter)
      (is string= "tuatara-no-such-server" servers))
    (is string= "sh ✓" (nth-value 1 (tuatara.defaults::language-tools-words 'rust)))))

(defun stand-in-formatter (dir &key (script "exec sed 's/  */ /g'") (name "fmt.sh"))
  "A formatter for the tests in DIR: a shell script called NAME running SCRIPT on its standard input; its full name."
  (let ((path (namestring (merge-pathnames name dir))))
    (alexandria:write-string-into-file (format nil "#!/bin/sh~%~a~%" script) path :if-exists :supersede)
    (sb-posix:chmod path #o755)
    path))

(defun formatting-done-p ()
  "True when no formatter is at work."
  (notany #'tuatara.defaults::format-job-p tuatara.runtime:*jobs*))

(define-test (lsp format-buffer)
  (if (not (eq :ready (tuatara.io:sandbox-status)))
      (skip "no sandbox here" (true nil))
      (with-test-directory (dir)
        (let* ((formatter (stand-in-formatter dir))
               (file (namestring (merge-pathnames "a.py" dir)))
               (tuatara.defaults::*modal-editing* nil)
               (tuatara-user:*language-formatters* (list (list 'python formatter))))
          (alexandria:write-string-into-file (format nil "keep = 1~%x  =   2~%keep = 3~%") file)
          ;; laid out by the formatter: only the line that differs replaced, point kept on its text, one undo
          (multiple-value-bind (f editor)
              (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file)) "M-> C-p" "M-x f o r m a t - b u f f e r RET"
                                       (list :until #'formatting-done-p) (list :wait 0.1)))
            (declare (ignore editor))
            (true (tuatara.frontend.fake:fake-frame-showing f "a.py formatted by fmt.sh, 1 place changed"))
            (let ((buffer (tuatara.buffer:find-buffer "a.py")))
              (is string= (format nil "keep = 1~%x = 2~%keep = 3~%") (tuatara.buffer:buffer-string buffer))
              (is = (search "keep = 3" (tuatara.buffer:buffer-string buffer)) (tuatara.buffer:buffer-point buffer))
              (tuatara.buffer:kill-buffer buffer)))
          ;; formatted before each save, when its language is named
          (let ((tuatara-user:*format-on-save* '(python)))
            (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file)) "M-> x SPC SPC SPC = SPC SPC 4" "C-x C-s"
                                     (list :until #'formatting-done-p) (list :wait 0.3)))
            (is string= (format nil "keep = 1~%x = 2~%keep = 3~%x = 4") (alexandria:read-file-into-string file))
            (let ((buffer (tuatara.buffer:find-buffer "a.py"))) (when buffer (tuatara.buffer:kill-buffer buffer))))
          ;; a formatter that fails changes nothing, and says what it said
          (let ((tuatara-user:*language-formatters* (list (list 'python (stand-in-formatter dir :script "echo bad >&2; exit 3")))))
            (multiple-value-bind (f editor)
                (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file)) "M-x f o r m a t - b u f f e r RET"
                                         (list :until #'formatting-done-p) (list :wait 0.1)))
              (declare (ignore editor))
              (true (tuatara.frontend.fake:fake-frame-showing f "fmt.sh could not format a.py: bad"))
              (let ((buffer (tuatara.buffer:find-buffer "a.py"))) (when buffer (tuatara.buffer:kill-buffer buffer)))))
          ;; a formatter that gives nothing back, or what is not text, changes nothing, said so
          (loop for (script words) in '(("cat >/dev/null" "it gave nothing back")
                                        ("cat >/dev/null; printf '\\377\\376x'" "is not text"))
                do (let ((tuatara-user:*language-formatters* (list (list 'python (stand-in-formatter dir :script script)))))
                     (multiple-value-bind (f editor)
                         (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file)) "M-x f o r m a t - b u f f e r RET"
                                                  (list :until #'formatting-done-p) (list :wait 0.1))
                                            :width 200)
                       (declare (ignore editor))
                       (true (tuatara.frontend.fake:fake-frame-showing f words) "~a" script)
                       (let ((buffer (tuatara.buffer:find-buffer "a.py")))
                         (is string= (alexandria:read-file-into-string file) (tuatara.buffer:buffer-string buffer))
                         (tuatara.buffer:kill-buffer buffer)))))
          ;; opened safely: never given to a formatter
          (multiple-value-bind (f editor)
              (auto-save-session (list (format nil "C-u C-x C-f C-a C-k ~a RET" (rimu-typed file)) "M-x f o r m a t - b u f f e r RET"))
            (declare (ignore editor))
            (true (tuatara.frontend.fake:fake-frame-showing f "a.py is opened safely: not formatted"))
            (let ((buffer (tuatara.buffer:find-buffer "a.py"))) (when buffer (tuatara.buffer:kill-buffer buffer))))))))

(define-test (lsp usual-formatter-installed)
  ;; a language whose usual formatter is installed is laid out by it: shfmt for a shell script
  (if (not (eq :ready (tuatara.io:sandbox-status)))
      (skip "no sandbox here" (true nil))
      (with-test-directory (dir)
        (let ((bin (merge-pathnames "bin/" dir))
              (file (namestring (merge-pathnames "a.sh" dir)))
              (path (sb-posix:getenv "PATH"))
              (tuatara.defaults::*modal-editing* nil))
          (ensure-directories-exist bin)
          (stand-in-formatter bin :name "shfmt")
          (alexandria:write-string-into-file (format nil "echo  hi~%") file)
          (unwind-protect
               (progn
                 (sb-posix:setenv "PATH" (format nil "~a:~a" (namestring bin) path) 1)
                 (multiple-value-bind (f editor)
                     (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file)) "M-x f o r m a t - b u f f e r RET"
                                              (list :until #'formatting-done-p) (list :wait 0.1)))
                   (declare (ignore editor))
                   (true (tuatara.frontend.fake:fake-frame-showing f "a.sh formatted by shfmt, 1 place changed"))
                   (is string= (format nil "echo hi~%") (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "a.sh")))))
            (sb-posix:setenv "PATH" path 1)
            (let ((buffer (tuatara.buffer:find-buffer "a.sh"))) (when buffer (tuatara.buffer:kill-buffer buffer))))))))

(define-test (lsp format-by-indentation)
  ;; no formatter: the lines indented as the mode indents them
  (let ((tuatara-user:*language-formatters* '((lisp))))
    (multiple-value-bind (f editor)
        (run-keys "M-x l i s p - m o d e RET M-x f o r m a t - b u f f e r RET" :text (format nil "(defun f ()~%1)") :width 120)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "indented by Tuatara"))
      (is string= (format nil "(defun f ()~%  1)") (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*scratch*"))))))

(define-test (lsp format-refused)
  ;; an encrypted file's text is kept from other programs: never a formatter's
  (let ((buffer (tuatara.buffer:make-buffer "secret-format.py")))
    (unwind-protect
         (progn (false (tuatara.defaults::outside-program-refusal buffer))
                (setf (tuatara.buffer:buffer-local buffer :plaintext) t)
                (true (search "encrypted" (tuatara.defaults::outside-program-refusal buffer))))
      (tuatara.buffer:kill-buffer buffer))))

(defun stand-in-server-setting (log)
  "*LANGUAGE-SERVERS* naming the tests' stand-in language server for Python, writing what it is told to LOG."
  (list (list 'python (list "python3" (namestring (merge-pathnames "test/stand-in-language-server.py" (asdf:system-source-directory :tuatara)))
                            (namestring log)))))

(defun told (log)
  "What the stand-in language server was told, the methods in order."
  (if (probe-file log) (uiop:read-file-lines log) '()))

(define-test (lsp diagnostics-from-a-server)
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames ".git/" dir))
    (let* ((log (merge-pathnames "told.log" dir))
           (file (namestring (merge-pathnames "a.py" dir)))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers* (stand-in-server-setting log))
           (tuatara-user:*lsp-sync-delay* 0.05))
      (alexandria:write-string-into-file (format nil "x = 1~%y = bad~%") file)
      (unwind-protect
           (progn
             ;; a project not trusted: the server offered, not started
             (multiple-value-bind (f editor)
                 (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file)) (list :wait 0.3)) :width 140)
               (declare (ignore editor))
               (true (tuatara.frontend.fake:fake-frame-showing f "can serve"))
               (false (told log))
               (tuatara.buffer:kill-buffer (tuatara.buffer:find-buffer "a.py")))
             ;; trusted: started as the file opens, what it finds underlined, marked beside the line, counted
             (let ((tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
                   (seen nil) (text nil))
               (multiple-value-bind (f editor)
                   (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                                            (list :until (lambda () (let ((b (tuatara.buffer:find-buffer "a.py")))
                                                                      (and b (tuatara.defaults::buffer-diagnostics b)))))
                                            "M-> m e h"
                                            (list :until (lambda () (= 2 (length (tuatara.defaults::buffer-diagnostics (tuatara.buffer:find-buffer "a.py"))))))
                                            "M-< M-x l s p - n e x t - d i a g n o s t i c RET" (list :wait 0.1)
                                            ;; what was found, kept before the session ends and its servers with it
                                            (list :until (lambda () (let ((b (tuatara.buffer:find-buffer "a.py")))
                                                                      (setf seen (tuatara.defaults::buffer-diagnostics b)
                                                                            text (tuatara.buffer:buffer-string b))))))
                                      :width 140)
                 (declare (ignore editor))
                 (let ((buffer (tuatara.buffer:find-buffer "a.py")))
                   (is equal '(1 2) (mapcar #'tuatara.defaults::diagnostic-severity seen))
                   (is = (search "bad" text) (tuatara.defaults::diagnostic-start (first seen)))
                   ;; said in a box under the cursor
                   (true (tuatara.frontend.fake:fake-frame-showing f "Error (stand-in)"))
                   (true (tuatara.frontend.fake:fake-frame-showing f "bad found"))
                   (true (tuatara.frontend.fake:fake-frame-showing f "✘1 ⚠1"))
                   (true (tuatara.frontend.fake:fake-frame-showing f "E"))
                   (tuatara.buffer:kill-buffer buffer)))
               (true (member "initialize" (told log) :test #'string=))
               (true (member "textDocument/didChange" (told log) :test #'string=))
               (true (member "textDocument/didClose" (told log) :test #'string=))))
           ;; opened safely: never given to a server
           (let ((tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
                 (before (count "textDocument/didOpen" (told log) :test #'string=)))
             (auto-save-session (list (format nil "C-u C-x C-f C-a C-k ~a RET" (rimu-typed file)) (list :wait 0.3)))
             (let ((buffer (tuatara.buffer:find-buffer "a.py")))
               (false (tuatara.defaults::buffer-servers buffer))
               (tuatara.buffer:kill-buffer buffer))
             (is = before (count "textDocument/didOpen" (told log) :test #'string=) "no new document opened"))
        (tuatara.defaults::lsp-stop-all)))))

(define-test (lsp what-servers-do)
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames ".git/" dir))
    (let* ((log (merge-pathnames "told.log" dir))
           (file (namestring (merge-pathnames "b.py" dir)))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers* (stand-in-server-setting log))
           (tuatara-user:*language-formatters* '((python)))
           (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
           (tuatara-user:*lsp-sync-delay* 0.05)
           (tuatara-user:*lsp-idle-delay* 0.1)
           (seen '()))
      (alexandria:write-string-into-file (format nil "def add(a, b):~%    return a  +  b~%~%total = add(1, 2)~%bad = 0~%") file)
      (flet ((note (key)
               ;; what the buffer and point are now, kept before the session ends
               (list :until (lambda () (let ((b (tuatara.buffer:find-buffer "b.py")))
                                         (push (list key (tuatara.buffer:buffer-string b) (tuatara.buffer:buffer-point b)) seen)
                                         t))))
             (ready () (list :until (lambda () (let ((b (tuatara.buffer:find-buffer "b.py")))
                                                 (and b (tuatara.defaults::buffer-diagnostics b))))))
             (typed (command) (format nil "M-x ~{~a~^ ~} RET" (map 'list #'string command))))
        (unwind-protect
             (multiple-value-bind (f editor)
                 (auto-save-session
                  (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file)) (ready)
                        ;; on add in its use: its definition, its uses, what it is
                        "M-< C-n C-n C-n C-f C-f C-f C-f C-f C-f C-f C-f C-f" (list :wait 0.3)
                        (typed "xref-find-definitions") (note :definition)
                        "C-x b b . p y RET M-< C-n C-n C-n C-f C-f C-f C-f C-f C-f C-f C-f C-f"
                        (typed "lsp-find-references") (list :wait 0.2) "q"
                        (typed "lsp-describe-thing-at-point") (list :wait 0.2)
                        ;; the call's parameters said as the cursor rests in it
                        "C-f C-f C-f C-f C-f C-f" (list :wait 0.5)
                        ;; out of a call: what the thing is
                        "M-< C-n C-f C-f C-f C-f" (list :wait 0.5)
                        ;; completed from the server's words
                        "M-> t o" (typed "completion-at-point") (list :wait 0.3) (note :completed)
                        ;; renamed everywhere
                        "M-< C-n C-n C-n" (typed "lsp-rename") "C-a C-k s u m RET" (list :wait 0.3) (note :renamed)
                        ;; the files it changed saved, asked once
                        "C-x s" "y RET" (list :wait 0.3)
                        ;; laid out by the server, there being no formatter
                        (typed "format-buffer") (list :wait 0.5) (note :formatted)
                        ;; the server's fix for what it found
                        "M-< C-s b a d RET" (typed "lsp-code-actions") (list :wait 0.3) "RET" (list :wait 0.3) (note :fixed))
                  :width 150 :height 30)
               (declare (ignore editor))
               (flet ((at (key) (rest (assoc key seen))))
                 (is = 4 (second (at :definition)) "at add in its definition")
                 (true (tuatara.frontend.fake:fake-frame-showing f "add(a, b)  — b"))
                 (true (tuatara.frontend.fake:fake-frame-showing f "return: int"))
                 (true (tuatara.frontend.fake:fake-frame-showing f "add: int"))
                 (true (tuatara.frontend.fake:fake-frame-showing f "A number of the stand-in."))
                 (false (tuatara.frontend.fake:fake-frame-showing f "```"))
                 (true (search (format nil "bad = 0~%total") (first (at :completed))))
                 (true (search "sum = add(1, 2)" (first (at :renamed))))
                 (true (search (format nil "~%sum") (first (at :renamed))))
                 (true (tuatara.frontend.fake:fake-frame-showing f "Save 1 changed file: b.py?"))
                 (true (search "return a + b" (first (at :formatted))))
                 (true (search "good = 0" (first (at :fixed))))
                 (true (tuatara.frontend.fake:fake-frame-showing f "Renamed in 1 file; not saved"))
                 (true (tuatara.frontend.fake:fake-frame-showing f "b.py formatted by python3"))))
          (tuatara.defaults::lsp-stop-all))))))

(define-test (lsp inlay-hints)
  ;; a server's hints shown faint within the code, never text of the buffer, asked for again after typing
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames ".git/" dir))
    (let* ((file (namestring (merge-pathnames "c.py" dir)))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers* (stand-in-server-setting (merge-pathnames "told.log" dir)))
           (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
           (tuatara-user:*lsp-sync-delay* 0.05)
           (text nil))
      (alexandria:write-string-into-file (format nil "total = 1~%") file)
      (unwind-protect
           (multiple-value-bind (f editor)
               (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                                        (list :until (lambda () (let ((b (tuatara.buffer:find-buffer "c.py")))
                                                                  (and b (tuatara.buffer:buffer-local b 'tuatara.defaults::lsp-hints)))))
                                        (list :wait 0.1)
                                        "M-> c o u n t SPC = SPC 2"
                                        (list :until (lambda () (= 2 (length (tuatara.defaults::buffer-hints (tuatara.buffer:find-buffer "c.py"))))))
                                        (list :wait 0.1)
                                        (list :until (lambda () (setf text (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "c.py"))))))
                                  :width 100)
             (declare (ignore editor))
             (true (tuatara.frontend.fake:fake-frame-showing f "total: int = 1"))
             (true (tuatara.frontend.fake:fake-frame-showing f "count: int = 2"))
             (is string= (format nil "total = 1~%count = 2") text))
        (tuatara.defaults::lsp-stop-all)))))

(define-test (lsp source-blocks-of-a-note)
  ;; a note's Python blocks seen by a Python server as one document: what it finds and hints at shown in the note
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames ".git/" dir))
    (let* ((file (namestring (merge-pathnames "n.org" dir)))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers* (stand-in-server-setting (merge-pathnames "told.log" dir)))
           (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
           (tuatara-user:*lsp-sync-delay* 0.05)
           (text (format nil "#+title: Blocks~%~%#+begin_src python~%  x = bad~%  ,* kept~%#+end_src~%~%Words, bad ones too.~%~%#+begin_src python~%  y = meh~%#+end_src~%"))
           (seen nil) (hints nil) (shadow nil))
      (alexandria:write-string-into-file text file)
      (unwind-protect
           (multiple-value-bind (f editor)
               (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                                        (list :until (lambda () (let ((b (tuatara.buffer:find-buffer "n.org")))
                                                                  (and b (= 2 (length (tuatara.defaults::buffer-diagnostics b)))
                                                                       (tuatara.defaults::buffer-hints b)))))
                                        (list :until (lambda () (let ((b (tuatara.buffer:find-buffer "n.org")))
                                                                  (setf seen (tuatara.defaults::buffer-diagnostics b)
                                                                        hints (tuatara.defaults::buffer-hints b)
                                                                        shadow (tuatara.defaults::org-blocks-shadow
                                                                                b (tuatara.defaults::find-language 'python)))))))
                                  :width 100 :height 20)
             (declare (ignore editor))
             ;; the code, line for line, its indentation and Org's comma left out
             (is string= (format nil "~%~%~%x = bad~%* kept~%~%~%~%~%~%y = meh~%~%") shadow)
             ;; found in the code only, at its place in the note: not the "bad" of the prose
             (is equal (list (search "bad" text) (search "meh" text)) (mapcar #'tuatara.defaults::diagnostic-start seen))
             (is equal '(1 2) (mapcar #'tuatara.defaults::diagnostic-severity seen))
             ;; a hint after each name given a value, within the note
             (is equal (list (+ 1 (search "x = bad" text)) (+ 1 (search "y = meh" text))) (mapcar #'car hints))
             (true (tuatara.frontend.fake:fake-frame-showing f "x: int = bad")))
        (tuatara.defaults::lsp-stop-all)))))

(define-test (lsp block-changes-sent-as-ranges)
  ;; typing in a note's Python block sends its server what changed in the block's code, not the
  ;; whole document again; what it finds then stands where the note has it
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames ".git/" dir))
    (let* ((log (merge-pathnames "told.log" dir))
           (file (namestring (merge-pathnames "r.org" dir)))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers* (stand-in-server-setting log))
           (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
           (tuatara-user:*lsp-sync-delay* 0.05)
           (text (format nil "* Notes~%~%#+begin_src python~%  x = 1~%#+end_src~%~%Prose.~%~%#+begin_src python~%  y = meh~%#+end_src~%"))
           (seen nil) (now nil))
      (alexandria:write-string-into-file text file)
      (flet ((buffer () (tuatara.buffer:find-buffer "r.org")))
        (unwind-protect
             (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                                      (list :until (lambda () (and (buffer) (= 1 (length (tuatara.defaults::buffer-diagnostics (buffer)))))))
                                      ;; the first block's line: x = 1 made x = 1 + bad
                                      "M-< C-n C-n C-n C-e SPC + SPC b a d"
                                      (list :until (lambda () (= 2 (length (tuatara.defaults::buffer-diagnostics (buffer))))))
                                      (list :until (lambda () (setf seen (tuatara.defaults::buffer-diagnostics (buffer))
                                                                    now (tuatara.buffer:buffer-string (buffer))))))
                                :width 100 :height 20)
          (tuatara.defaults::lsp-stop-all))
        (is equal (list (search "bad" now) (search "meh" now)) (mapcar #'tuatara.defaults::diagnostic-start seen))
        (true (member "ranges" (told log) :test #'string=))
        (false (member "whole" (told log) :test #'string=))))))

(define-test (lsp context-keys)
  ;; C-c in code: the language server's keys, named; in a list: its own keys too; the terminal's C-c its program's
  (multiple-value-bind (f editor) (run-keys "M-x r u s t - m o d e RET C-c" :width 120 :height 24)
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "d definition"))
    (true (tuatara.frontend.fake:fake-frame-showing f "R rename")))
  (let ((buffers (tuatara.defaults::mode-keymap 'tuatara.defaults::buffers-mode)))
    (is eq 'tuatara.defaults::list-mark-entry (tuatara.command:lookup-key buffers (tuatara.frontend:parse-key-sequence "C-c m")))
    (false (tuatara.command:lookup-key buffers (tuatara.frontend:parse-key-sequence "C-c x"))))
  (let ((rimu tuatara.defaults::*rimu-keymap*))
    ;; x stays rimu's alone: C-c x is left for your own keys
    (false (tuatara.command:lookup-key rimu (tuatara.frontend:parse-key-sequence "C-c x")))
    (true (tuatara.command:lookup-key rimu (tuatara.frontend:parse-key-sequence "C-c m"))))
  (true (member (tuatara.command:lookup-key (tuatara.defaults::mode-keymap 'tuatara.defaults::terminal-mode)
                                            (tuatara.frontend:parse-key-sequence "C-c q"))
                '(nil :undefined))))

(define-test (lsp places-follow-the-text)
  ;; typed before what a server marked, and not yet sent: the marks move with the text, none hidden
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames ".git/" dir))
    (let* ((file (namestring (merge-pathnames "d.py" dir)))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers* (stand-in-server-setting (merge-pathnames "told.log" dir)))
           (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
           (tuatara-user:*lsp-sync-delay* 60)
           (tuatara-user:*lsp-idle-delay* 60)
           (seen nil) (hints nil) (text nil))
      (alexandria:write-string-into-file (format nil "x = 1~%y = bad~%") file)
      (unwind-protect
           (multiple-value-bind (f editor)
               (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                                        (list :until (lambda () (let ((b (tuatara.buffer:find-buffer "d.py")))
                                                                  (and b (tuatara.defaults::buffer-diagnostics b)
                                                                       (= 2 (length (tuatara.defaults::buffer-hints b)))))))
                                        "M-< x x x" (list :wait 0.1)
                                        (list :until (lambda () (let ((b (tuatara.buffer:find-buffer "d.py")))
                                                                  (setf seen (tuatara.defaults::buffer-diagnostics b)
                                                                        hints (tuatara.defaults::buffer-hints b)
                                                                        text (tuatara.buffer:buffer-string b))))))
                                  :width 100)
             (declare (ignore editor))
             (is string= (format nil "xxxx = 1~%y = bad~%") text)
             (is = (search "bad" text) (tuatara.defaults::diagnostic-start (first seen)))
             (is equal (list 4 (1+ (search "y =" text))) (mapcar #'car hints))
             (true (tuatara.frontend.fake:fake-frame-showing f "xxxx: int = 1")))
        (tuatara.defaults::lsp-stop-all)))))

(define-test (lsp calls-symbols-and-colours)
  ;; who calls a function, what it calls, a project's names, and the code coloured as the server sees it
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames ".git/" dir))
    (let* ((file (namestring (merge-pathnames "e.py" dir)))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers* (stand-in-server-setting (merge-pathnames "told.log" dir)))
           (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
           (tuatara-user:*lsp-sync-delay* 0.05)
           (tuatara-user:*lsp-semantic-tokens* t)
           (tokens nil) (point nil))
      (alexandria:write-string-into-file (format nil "def add(a, b):~%    return a + b~%~%def twice(x):~%    return add(x, x)~%~%print(twice(2))~%") file)
      (flet ((typed (command) (format nil "M-x ~{~a~^ ~} RET" (map 'list #'string command))))
        (unwind-protect
             (multiple-value-bind (f editor)
                 (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                                          (list :until (lambda () (let ((b (tuatara.buffer:find-buffer "e.py")))
                                                                    (and b (tuatara.buffer:buffer-local b 'tuatara.defaults::lsp-tokens)))))
                                          (list :until (lambda () (let ((b (tuatara.buffer:find-buffer "e.py")))
                                                                    (setf tokens (loop for (nil . placed) in (tuatara.buffer:buffer-local b 'tuatara.defaults::lsp-tokens)
                                                                                       append (coerce (tuatara.defaults::placed-items-now placed b) 'list))))))
                                          ;; on add: where it is called from
                                          "M-< C-f C-f C-f C-f" (typed "lsp-incoming-calls") (list :wait 0.3) "q"
                                          ;; on twice: what it calls
                                          "C-x b e . p y RET M-< C-n C-n C-n C-f C-f C-f C-f" (typed "lsp-outgoing-calls") (list :wait 0.3) "q"
                                          ;; a name of the project, gone to
                                          "C-x b e . p y RET" (typed "lsp-find-symbol") "a d d RET" (list :wait 0.2) "RET" (list :wait 0.2)
                                          (list :until (lambda () (setf point (tuatara.buffer:buffer-point (tuatara.buffer:find-buffer "e.py"))))))
                                    :width 120 :height 30)
               (declare (ignore editor))
               (true (tuatara.frontend.fake:fake-frame-showing f "5: return add(x, x)"))
               (true (tuatara.frontend.fake:fake-frame-showing f "1: def add(a, b):"))
               (is = 4 point "at add's definition")
               (true (member '(4 7 :function) tokens :test #'equal))
               (true (member '(8 9 :variable) tokens :test #'equal)))
          (tuatara.defaults::lsp-stop-all))))))

(define-test (lsp uses-outline-and-progress)
  ;; the name the cursor rests on and its uses lit, the file's names in the outline, the server's work as a job
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames ".git/" dir))
    (let* ((file (namestring (merge-pathnames "f.py" dir)))
           (text (format nil "def add(a, b):~%    total = a + b~%    return total~%~%print(add(1, 2))~%"))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers* (stand-in-server-setting (merge-pathnames "told.log" dir)))
           (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
           (tuatara-user:*lsp-sync-delay* 0.05)
           (tuatara-user:*lsp-idle-delay* 0.1)
           (lit nil) (point nil) (working :unknown))
      (alexandria:write-string-into-file text file)
      (flet ((typed (command) (format nil "M-x ~{~a~^ ~} RET" (map 'list #'string command)))
             (buffer () (tuatara.buffer:find-buffer "f.py")))
        (unwind-protect
             (multiple-value-bind (f editor)
                 (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                                          (list :until (lambda () (tuatara.defaults::server-work (tuatara.defaults::buffer-servers (buffer)))))
                                          (list :wait 0.1)
                                          (typed "list-jobs") (list :wait 0.1) "q"
                                          ;; resting on total: its uses lit
                                          "C-x b f . p y RET M-< C-n C-f C-f C-f C-f C-f"
                                          (list :until (lambda () (tuatara.buffer:buffer-local (buffer) 'tuatara.defaults::lsp-symbol-highlights)))
                                          (list :until (lambda () (setf lit (mapcar (lambda (h) (subseq h 0 2))
                                                                                    (tuatara.defaults::placed-items-now
                                                                                     (tuatara.buffer:buffer-local (buffer) 'tuatara.defaults::lsp-symbol-highlights)
                                                                                     (buffer))))))
                                          ;; the outline: the server's names, total among add's
                                          "M-<" "M-g o" (list :wait 0.2) "t o t a l RET" (list :wait 0.1)
                                          (list :until (lambda () (setf point (tuatara.buffer:buffer-point (buffer)))))
                                          ;; typed: the server's indexing over
                                          "M-> x" (list :until (lambda () (null (tuatara.defaults::server-work (tuatara.defaults::buffer-servers (buffer))))))
                                          (list :until (lambda () (setf working (tuatara.defaults::server-work (tuatara.defaults::buffer-servers (buffer)))) t)))
                                    :width 120 :height 30)
               (declare (ignore editor))
               (true (processing-shown f))
               (true (tuatara.frontend.fake:fake-frame-showing f "Indexing"))
               (is equal (list (list (search "total" text) (+ 5 (search "total" text)))
                               (list (search "total" text :from-end t) (+ 5 (search "total" text :from-end t))))
                   lit)
               (true (tuatara.frontend.fake:fake-frame-showing f "def add(a, b):"))
               (is = (search "total" text) point)
               (false working))
          (tuatara.defaults::lsp-stop-all))))))

(define-test (lsp diagnostics-style)
  ;; the marks of what servers find: wavy as standard, dotted, a line, or a ground of their colour
  (let ((tuatara.display:*user-face-settings* tuatara.display:*user-face-settings*)
        (tuatara.defaults::*diagnostics-style-shown* :wave))
    (unwind-protect
         (progn
           (let ((tuatara-user:*lsp-diagnostics-style* :dotted))
             (tuatara.defaults::show-diagnostics-style)
             (is eq :dotted (tuatara.display:face-attribute :diagnostic-error :underline))
             (is eq :diagnostic-error (tuatara.defaults::diagnostic-face 1)))
           (let ((tuatara-user:*lsp-diagnostics-style* :line))
             (tuatara.defaults::show-diagnostics-style)
             (is eq t (tuatara.display:face-attribute :diagnostic-warning :underline)))
           (let ((tuatara-user:*lsp-diagnostics-style* :background))
             (is eq :diagnostic-error-ground (tuatara.defaults::diagnostic-face 1))
             (is eq :diagnostic-hint-ground (tuatara.defaults::diagnostic-face 4))))
      ;; as standard again: errors wavy, hints dotted
      (let ((tuatara-user:*lsp-diagnostics-style* :wave))
        (tuatara.defaults::show-diagnostics-style)
        (is eq :wave (tuatara.display:face-attribute :diagnostic-error :underline))
        (is eq :dotted (tuatara.display:face-attribute :diagnostic-hint :underline))))))

(define-test (lsp languages-on-and-off)
  ;; servers for every language, for those named alone, or for all but those named
  (let ((python (tuatara.defaults::find-language 'python))
        (rust (tuatara.defaults::find-language 'rust)))
    (let ((tuatara-user:*lsp-languages* t))
      (true (tuatara.defaults::lsp-language-on-p python)))
    (let ((tuatara-user:*lsp-languages* '(rust go)))
      (true (tuatara.defaults::lsp-language-on-p rust))
      (false (tuatara.defaults::lsp-language-on-p python)))
    (let ((tuatara-user:*lsp-languages* '(:not python)))
      (true (tuatara.defaults::lsp-language-on-p rust))
      (false (tuatara.defaults::lsp-language-on-p python))))
  ;; a file of a language left out: never given to a server, M-x lsp says why
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames ".git/" dir))
    (let* ((log (merge-pathnames "told.log" dir))
           (file (namestring (merge-pathnames "g.py" dir)))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers* (stand-in-server-setting log))
           (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
           (tuatara-user:*lsp-languages* '(:not python)))
      (alexandria:write-string-into-file (format nil "x = bad~%") file)
      (unwind-protect
           (multiple-value-bind (f editor)
               (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file)) (list :wait 0.3)
                                        "C-c L" (list :wait 0.1))
                                  :width 120)
             (declare (ignore editor))
             (true (tuatara.frontend.fake:fake-frame-showing f "Language servers are off for python (*lsp-languages*)"))
             (false (told log)))
        (tuatara.defaults::lsp-stop-all)))))

(define-test (lsp implementations-types-imports-and-events)
  ;; C-c I a name's implementations, C-c T its type, C-c o the imports in order, M-x lsp-events what was said
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames ".git/" dir))
    (let* ((file (namestring (merge-pathnames "h.py" dir)))
           (text (format nil "import sys~%import abc~%~%class Shape:~%    def area(self):~%        return 0~%~%class Square(Shape):~%    def area(self):~%        return 4~%~%s = Square()~%"))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers* (stand-in-server-setting (merge-pathnames "told.log" dir)))
           (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
           (tuatara-user:*lsp-sync-delay* 0.05)
           (typed nil) (organised nil) (events nil))
      (alexandria:write-string-into-file text file)
      (flet ((buffer () (tuatara.buffer:find-buffer "h.py")))
        (unwind-protect
             (multiple-value-bind (f editor)
                 (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                                          (list :until (lambda () (tuatara.defaults::buffer-servers (buffer))))
                                          (list :wait 0.2)
                                          ;; on area: its two implementations, listed
                                          "M-< C-n C-n C-n C-n C-f C-f C-f C-f C-f C-f C-f C-f C-f" "C-c I" (list :wait 0.3) "q"
                                          ;; on s: the type's definition, gone to
                                          "C-x b h . p y RET M->" "C-p C-a" "C-c T" (list :wait 0.3)
                                          (list :until (lambda () (setf typed (tuatara.buffer:buffer-point (buffer)))))
                                          ;; the imports in order
                                          "C-c o" (list :wait 0.3)
                                          (list :until (lambda () (setf organised (tuatara.buffer:buffer-string (buffer)))))
                                          "M-x l s p - e v e n t s RET" (list :wait 0.2)
                                          (list :until (lambda () (setf events (tuatara.buffer:buffer-string
                                                                                (tuatara.buffer:find-buffer "*Language server events*"))))))
                                    :width 120 :height 30)
               (declare (ignore editor))
               (true (tuatara.frontend.fake:fake-frame-showing f "5: def area(self):"))
               (true (tuatara.frontend.fake:fake-frame-showing f "9: def area(self):"))
               (is = (search "Shape:" text) typed)
               (true (uiop:string-prefix-p (format nil "import abc~%import sys~%") organised))
               (true (tuatara.frontend.fake:fake-frame-showing f "→ {\"jsonrpc\":\"2.0\""))
               (true (search "→ {\"jsonrpc\":\"2.0\",\"method\":\"textDocument/didOpen\"" events)))
          (tuatara.defaults::lsp-stop-all))))))

(define-test (lsp only-changes-sent)
  ;; typed, deleted and joined lines go to a server that takes them as ranges, never the whole text,
  ;; and what it finds stands where the text has it
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames ".git/" dir))
    (let* ((log (merge-pathnames "told.log" dir))
           (file (namestring (merge-pathnames "i.py" dir)))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers* (stand-in-server-setting log))
           (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
           (tuatara-user:*lsp-sync-delay* 0.05)
           (seen nil) (text nil))
      (alexandria:write-string-into-file (format nil "x = 1~%y = 2~%z = bad~%") file)
      (flet ((buffer () (tuatara.buffer:find-buffer "i.py")))
        (unwind-protect
             (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                                      (list :until (lambda () (tuatara.defaults::buffer-diagnostics (buffer))))
                                      ;; letters of two octets, a line joined to the one before, a word deleted
                                      "M-< é é SPC" (list :wait 0.2) "C-n C-a DEL" (list :wait 0.2) "M-> C-p C-e DEL DEL DEL m e h" (list :wait 0.3)
                                      (list :until (lambda () (let ((d (tuatara.defaults::buffer-diagnostics (buffer))))
                                                                (and d (= 1 (length d)) (= 2 (tuatara.defaults::diagnostic-severity (first d)))))))
                                      (list :until (lambda () (setf seen (tuatara.defaults::buffer-diagnostics (buffer))
                                                                    text (tuatara.buffer:buffer-string (buffer))))))
                                :width 100)
          (tuatara.defaults::lsp-stop-all))
        (is string= (format nil "éé x = 1y = 2~%z = meh~%") text)
        (is = (length (tuatara.text:string-octets (subseq text 0 (search "meh" text)))) (tuatara.defaults::diagnostic-start (first seen)))
        (true (member "ranges" (told log) :test #'string=))
        (false (member "whole" (told log) :test #'string=))))))

(define-test (lsp changes-reach-a-server-before-another-opens)
  ;; changes not yet sent to one server reach it when a second opens the file, not lost to it
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames ".git/" dir))
    (let* ((first-log (merge-pathnames "first.log" dir))
           (second-log (merge-pathnames "second.log" dir))
           (script (namestring (merge-pathnames "test/stand-in-language-server.py" (asdf:system-source-directory :tuatara))))
           (file (namestring (merge-pathnames "two.py" dir)))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers*
             (list (list 'python (list "python3" script (namestring first-log))
                         (list "env" "STAND_IN_SLOW_START=1" "python3" script (namestring second-log)))))
           (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
           (tuatara-user:*lsp-sync-delay* 4)
           ;; nothing asked of the server where the cursor rests, which would send the change
           (tuatara-user:*lsp-idle-delay* 30)
           (seen nil))
      (alexandria:write-string-into-file (format nil "x = 1~%") file)
      (flet ((buffer () (tuatara.buffer:find-buffer "two.py")))
        (unwind-protect
             (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                                      (list :until (lambda () (member "textDocument/didOpen" (told first-log) :test #'string=)))
                                      "M-> y SPC = SPC b a d"
                                      (list :until (lambda () (member "textDocument/didOpen" (told second-log) :test #'string=)))
                                      (list :until (lambda () (= 2 (length (tuatara.defaults::buffer-diagnostics (buffer))))))
                                      (list :until (lambda () (setf seen (tuatara.defaults::buffer-diagnostics (buffer))) t)))
                                :width 100)
          (tuatara.defaults::lsp-stop-all)
          (when (buffer) (tuatara.buffer:kill-buffer (buffer))))
        (true (member "textDocument/didOpen" (told second-log) :test #'string=))
        (true (member "ranges" (told first-log) :test #'string=))
        (is equal '("bad found" "bad found") (mapcar #'tuatara.defaults::diagnostic-message seen))))))

(define-test (lsp diagnostics-asked-for)
  ;; a server that gives some of what it finds only when asked: asked after each change, kept beside what it sends
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames ".git/" dir))
    (let* ((log (merge-pathnames "told.log" dir))
           (file (namestring (merge-pathnames "l.py" dir)))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers* (stand-in-server-setting log))
           (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
           (tuatara-user:*lsp-sync-delay* 0.05)
           (kinds nil))
      (alexandria:write-string-into-file (format nil "x = bad~%") file)
      (flet ((buffer () (tuatara.buffer:find-buffer "l.py")))
        (unwind-protect
             (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                                      (list :until (lambda () (tuatara.defaults::buffer-diagnostics (buffer))))
                                      "M-> u g h"
                                      (list :until (lambda () (= 2 (length (tuatara.defaults::buffer-diagnostics (buffer))))))
                                      (list :until (lambda () (setf kinds (mapcar #'tuatara.defaults::diagnostic-severity
                                                                                  (tuatara.defaults::buffer-diagnostics (buffer)))))))
                                :width 100)
          (tuatara.defaults::lsp-stop-all))
        (is equal '(1 3) kinds)
        (true (member "textDocument/diagnostic" (told log) :test #'string=))))))

(defun found-on-save (buffer)
  "What BUFFER's servers found in it, each (MESSAGE START), in order of place."
  (mapcar (lambda (d) (list (tuatara.defaults::diagnostic-message d) (tuatara.defaults::diagnostic-start d)))
          (tuatara.defaults::buffer-diagnostics buffer)))

(define-test (lsp saved-at-once-checked-in-its-text)
  ;; saved before the change was due to be sent: it is sent before the save is told, so what the
  ;; check of the file saved finds is where the text has it, not a line below
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames ".git/" dir))
    (let* ((log (merge-pathnames "told.log" dir))
           (file (namestring (merge-pathnames "saved.py" dir)))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers* (stand-in-server-setting log))
           (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
           (tuatara-user:*lsp-sync-delay* 4)
           (seen nil) (text nil))
      (alexandria:write-string-into-file (format nil "a = 1~%b = oops~%") file)
      (flet ((buffer () (tuatara.buffer:find-buffer "saved.py")))
        (unwind-protect
             (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                                      (list :until (lambda () (member "textDocument/didOpen" (told log) :test #'string=)))
                                      "M-< n e w SPC = SPC 0 RET C-x C-s"
                                      (list :until (lambda () (assoc "oops found" (found-on-save (buffer)) :test #'string=)))
                                      (list :until (lambda () (setf seen (found-on-save (buffer))
                                                                    text (tuatara.buffer:buffer-string (buffer))))))
                                :width 100)
          (tuatara.defaults::lsp-stop-all)
          (when (buffer) (tuatara.buffer:kill-buffer (buffer))))
        (is equal (list (list "oops found" (search "oops" text))) seen)
        (let ((told (told log)))
          (true (< (position "textDocument/didChange" told :test #'string=)
                   (position "textDocument/didSave" told :test #'string=))
                "~a" told))))))

(define-test (lsp diagnostics-of-an-older-text-taken)
  ;; what a server found in a text older than the one last sent is taken all the same, put
  ;; where that text had it and moved on: the error of a line deleted and saved goes, though
  ;; more was typed before the check of the save was said
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames ".git/" dir))
    (let* ((log (merge-pathnames "told.log" dir))
           (file (namestring (merge-pathnames "older.py" dir)))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers* (stand-in-server-setting log))
           (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
           (tuatara-user:*lsp-sync-delay* 0.1)
           (first-seen nil) (seen :none))
      (alexandria:write-string-into-file (format nil "a = 1~%b = oops~%") file)
      (flet ((buffer () (tuatara.buffer:find-buffer "older.py"))
             (checks () (count "checked" (told log) :test #'string=)))
        (unwind-protect
             (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                                      (list :until (lambda () (member "textDocument/didOpen" (told log) :test #'string=)))
                                      "M-> c SPC = SPC 2 C-x C-s"
                                      (list :until (lambda () (setf first-seen (found-on-save (buffer)))))
                                      ;; the line deleted and saved at once, then more typed and sent
                                      "M-< C-n C-a C-k C-k C-x C-s M-> z"
                                      (list :until (lambda () (= 2 (checks))))
                                      (list :wait 0.5)
                                      (list :until (lambda () (setf seen (found-on-save (buffer))) t)))
                                :width 100)
          (tuatara.defaults::lsp-stop-all)
          (when (buffer) (tuatara.buffer:kill-buffer (buffer))))
        (is equal '("oops found") (mapcar #'first first-seen))
        (is equal '() seen)))))

(define-test (lsp format-endings-said)
  ;; a server's formatting that changes nothing says why, each way in its own words: its failure
  ;; in short, no answer in time, nothing to change; and with no server, why it was only indented
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames ".git/" dir))
    (let* ((log (merge-pathnames "told.log" dir))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers* (stand-in-server-setting log))
           (tuatara-user:*language-formatters* '((python)))
           (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
           (tuatara-user:*format-seconds* 1))
      (unwind-protect
           (loop for (name text words) in '(("fails.py" "x = unformattable" "python3 could not format fails.py: Failed to spawn rustfmt")
                                            ("slow.py" "x = slowformat" "python3 could not format slow.py: no answer within 1 second (*format-seconds*)")
                                            ("done.py" "x = 1" "done.py formatted by python3: it was laid out already"))
                 for opened from 1
                 do (let ((file (namestring (merge-pathnames name dir))))
                      (alexandria:write-string-into-file (format nil "~a~%" text) file)
                      (let ((f (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                                                        (list :until (lambda () (= opened (count "textDocument/didOpen" (told log) :test #'string=))))
                                                        "M-x f o r m a t - b u f f e r RET"
                                                        (list :until (lambda () (search words (or (tuatara.command:editor-message tuatara.command::*editor*) ""))))
                                                        (list :wait 0.1))
                                                  :width 160)))
                        (true (tuatara.frontend.fake:fake-frame-showing f words) "~a" name)
                        (is string= (format nil "~a~%" text) (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer name)))
                        (tuatara.buffer:kill-buffer (tuatara.buffer:find-buffer name)))))
        (tuatara.defaults::lsp-stop-all))
      ;; no server: indented, and why said
      (let ((tuatara-user:*language-servers* '()))
        (multiple-value-bind (f editor)
            (run-keys "M-x p y t h o n - m o d e RET M-x f o r m a t - b u f f e r RET" :text (format nil "x = 1~%") :width 160)
          (declare (ignore editor))
          (true (tuatara.frontend.fake:fake-frame-showing f "no formatter is known for it, and no language server formats it")))))))

(define-test (lsp failure-words-short)
  ;; why a question came to nothing, in one short way: a command line by its program alone,
  ;; other words as they are, the first line only, no answer in time with its setting
  (flet ((words (message) (tuatara.defaults::lsp-error-words (tuatara.text:json-object "message" message))))
    (is string= "Failed to spawn rustfmt" (words "Failed to spawn cd \"/x/src\" && RUSTUP_AUTO_INSTALL=\"0\" \"rustfmt\""))
    (is string= "Failed to spawn rustfmt" (words "Failed to spawn \"/usr/bin/rustfmt\" \"--edition\" \"2021\""))
    (is string= "unresolved import \"a/b\"" (words (format nil "unresolved import \"a/b\"~%more")))
    (is string= "it failed" (words ""))
    (is string= "no answer within 2 seconds (*lsp-wait*)" (tuatara.defaults::lsp-error-words :timeout :seconds 2))
    (is string= "no answer within 1 second (*format-seconds*)"
        (tuatara.defaults::lsp-error-words :timeout :seconds 1 :setting 'tuatara-user:*format-seconds*))
    (is string= "a key stopped the wait" (tuatara.defaults::lsp-error-words :stopped))
    (is string= "no answer within 0.5 seconds (*lsp-wait*)" (tuatara.defaults::lsp-error-words :timeout :seconds 1/2))
    (is string= "no answer within 3 seconds (*lsp-wait*)" (tuatara.defaults::lsp-error-words :timeout :seconds 3.0))
    ;; a formatter's error output: its first line that says anything
    (is string= "error: expected item" (tuatara.defaults::short-failure-words (format nil "~%error: expected item~% --> a.rs:1:1~%")))))

(defun stand-in-server (name status &rest capabilities)
  "A language server named NAME of STATUS offering CAPABILITIES, for tests that need none running."
  (let ((server (tuatara.defaults::make-lsp-server name '("true") "/tmp/" 'python nil nil)))
    (setf (tuatara.defaults::lsp-server-status server) status
          (tuatara.defaults::lsp-server-capabilities server)
          (apply #'tuatara.text:json-object (loop for c in capabilities append (list c t))))
    server))

(define-test (lsp not-formatting-said-of-each-server)
  ;; why no server laid the code out, said of each: starting, not formatting, or not a part
  (let ((buffer (tuatara.buffer:make-buffer "unformatted.py")))
    (unwind-protect
         (flet ((servers (&rest servers)
                  (let ((doc (tuatara.defaults::make-lsp-doc buffer nil "file:///tmp/unformatted.py")))
                    (setf (tuatara.defaults::lsp-doc-servers doc) servers
                          (tuatara.buffer:buffer-local buffer 'tuatara.defaults::lsp-docs) (list doc)))))
           (is string= "no language server formats it" (tuatara.defaults::servers-not-formatting-words buffer))
           (servers (stand-in-server "kea" :ready "documentFormattingProvider"))
           (is string= "kea formats whole files only" (tuatara.defaults::servers-not-formatting-words buffer t))
           (servers (stand-in-server "kea" :ready "hoverProvider") (stand-in-server "tui" :starting))
           (is string= "kea does not format code, tui is still starting" (tuatara.defaults::servers-not-formatting-words buffer))
           ;; a note whose source blocks alone are seen
           (setf (tuatara.defaults::lsp-doc-shadow (first (tuatara.buffer:buffer-local buffer 'tuatara.defaults::lsp-docs)))
                 (constantly ""))
           (is string= "its language servers see only its source blocks" (tuatara.defaults::servers-not-formatting-words buffer)))
      (setf (tuatara.buffer:buffer-local buffer 'tuatara.defaults::lsp-docs) nil)
      (tuatara.buffer:kill-buffer buffer))))

(define-test (lsp texts-kept-until-spoken-of)
  ;; the texts sent are kept, eight at most, until every server has said what it found in a
  ;; newer one, but for that of the last save; none once the document is forgotten
  (let* ((buffer (tuatara.buffer:make-buffer "kept.py"))
         (kea (stand-in-server "kea" :ready)) (tui (stand-in-server "tui" :ready))
         (doc (tuatara.defaults::make-lsp-doc buffer nil "file:///tmp/kept.py")))
    (unwind-protect
         (flet ((kept () (mapcar #'first (tuatara.defaults::lsp-doc-kept doc))))
           (setf (tuatara.defaults::lsp-doc-servers doc) (list kea tui)
                 (tuatara.buffer:buffer-local buffer 'tuatara.defaults::lsp-docs) (list doc))
           (dotimes (i 10)
             (incf (tuatara.defaults::lsp-doc-version doc))
             (tuatara.defaults::note-sent doc))
           (is equal '(10 9 8 7 6 5 4 3) (kept))
           ;; the text of the last save stays for a check of the file saved
           (setf (tuatara.defaults::lsp-doc-saved doc) (find 4 (tuatara.defaults::lsp-doc-kept doc) :key #'first))
           (tuatara.defaults::note-heard doc kea 9)
           (is equal '(10 9 8 7 6 5 4 3) (kept) "one server has not spoken")
           (is eql 9 (cdr (assoc kea (tuatara.defaults::lsp-doc-heard doc))))
           (tuatara.defaults::note-heard doc tui 6)
           (is equal '(10 9 8 7 6 4) (kept))
           (true (tuatara.defaults::doc-as-sent doc 6))
           (false (tuatara.defaults::doc-as-sent doc 5))
           (true (tuatara.defaults::doc-as-sent doc 4))
           ;; that of the last save too once eight newer have been sent
           (dotimes (i 8)
             (incf (tuatara.defaults::lsp-doc-version doc))
             (tuatara.defaults::note-sent doc))
           (is equal '(18 17 16 15 14 13 12 11) (kept))
           (false (tuatara.defaults::lsp-doc-saved doc))
           (tuatara.defaults::lsp-forget-document kea doc)
           (tuatara.defaults::lsp-forget-document tui doc)
           (is equal '() (kept)))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (lsp texts-let-go-where-none-will-speak)
  ;; a server that gives what it finds only when asked keeps no texts; one that says it with
  ;; no number has spoken of the text last sent
  (let* ((buffer (tuatara.buffer:make-buffer "unspoken.py"))
         (asked (stand-in-server "kea" :ready "diagnosticProvider"))
         (tells (stand-in-server "tui" :ready))
         (doc (tuatara.defaults::make-lsp-doc buffer nil "file:///tmp/unspoken.py")))
    (unwind-protect
         (flet ((kept () (mapcar #'first (tuatara.defaults::lsp-doc-kept doc)))
                (send (n) (dotimes (i n)
                            (incf (tuatara.defaults::lsp-doc-version doc))
                            (tuatara.defaults::note-sent doc))))
           (setf (tuatara.defaults::lsp-doc-servers doc) (list asked)
                 (tuatara.buffer:buffer-local buffer 'tuatara.defaults::lsp-docs) (list doc))
           (send 5)
           (is equal '(5) (kept) "asked only: none spoken of")
           (setf (tuatara.defaults::lsp-doc-servers doc) (list asked tells))
           (send 3)
           (is equal '(8 7 6 5) (kept) "one that tells has not spoken")
           (tuatara.defaults::note-heard doc tells nil)
           (is eql 8 (cdr (assoc tells (tuatara.defaults::lsp-doc-heard doc))))
           (is equal '(8) (kept)))
      (setf (tuatara.buffer:buffer-local buffer 'tuatara.defaults::lsp-docs) nil)
      (tuatara.buffer:kill-buffer buffer))))

(define-test (lsp titles-in-a-sentence)
  ;; an action's title inside a sentence: its first letter small, an abbreviation kept
  (is string= "import `HashMap` from std::collections" (tuatara.defaults::title-in-a-sentence "Import `HashMap` from std::collections"))
  (is string= "CSS: sort properties" (tuatara.defaults::title-in-a-sentence "CSS: sort properties"))
  (is string= "inline variable" (tuatara.defaults::title-in-a-sentence "inline variable"))
  (is string= "" (tuatara.defaults::title-in-a-sentence "")))

(define-test (lsp completion-documentation-asked-for)
  ;; a server that leaves a completion's documentation until it is chosen: asked then, shown beside the list
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames ".git/" dir))
    (let* ((log (merge-pathnames "told.log" dir))
           (file (namestring (merge-pathnames "j.py" dir)))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers* (stand-in-server-setting log))
           (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
           (tuatara-user:*lsp-sync-delay* 0.05))
      (alexandria:write-string-into-file (format nil "alpha = 1~%alpine = 2~%") file)
      (unwind-protect
           (multiple-value-bind (f editor)
               (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                                        (list :until (lambda () (tuatara.defaults::buffer-servers (tuatara.buffer:find-buffer "j.py"))))
                                        (list :wait 0.2)
                                        "M-> a l" "M-x c o m p l e t i o n - a t - p o i n t RET" (list :wait 0.5) "C-n" (list :wait 0.5) "C-g")
                                  :width 120 :height 20)
             (declare (ignore editor))
             (true (tuatara.frontend.fake:fake-frame-showing f "The word `alpha`."))
             (true (tuatara.frontend.fake:fake-frame-showing f "The word `alpine`."))
             (true (member "completionItem/resolve" (told log) :test #'string=)))
        (tuatara.defaults::lsp-stop-all)))))

(define-test (lsp completion-key-in-code)
  ;; C-M-i, and M-TAB as a terminal sends it, completes from the server's words in any language's buffer
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames ".git/" dir))
    (let* ((log (merge-pathnames "told.log" dir))
           (file (namestring (merge-pathnames "k.py" dir)))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers* (stand-in-server-setting log))
           (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
           (tuatara-user:*lsp-sync-delay* 0.05)
           (texts '()))
      (alexandria:write-string-into-file (format nil "total = 1~%") file)
      (flet ((note () (list :until (lambda () (push (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "k.py")) texts) t))))
        (unwind-protect
             (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                                      (list :until (lambda () (tuatara.defaults::buffer-servers (tuatara.buffer:find-buffer "k.py"))))
                                      (list :wait 0.2)
                                      "M-> t o" "C-M-i" (list :wait 0.3) (note)
                                      "RET t o" "M-TAB" (list :wait 0.3) (note))
                                :width 120 :height 20)
          (tuatara.defaults::lsp-stop-all)))
      (is equal (format nil "total = 1~%total") (second texts))
      (is equal (format nil "total = 1~%total~%total") (first texts)))))

(define-test (lsp servers-told-of-files-on-disk)
  ;; a server that asks is told of its project's files made, changed and deleted by other programs,
  ;; those its patterns match, in folders found in the background; folders a build makes never watched
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames ".git/" dir))
    (ensure-directories-exist (merge-pathnames "pkg/" dir))
    (ensure-directories-exist (merge-pathnames "target/" dir))
    (let* ((log (merge-pathnames "told.log" dir))
           (file (namestring (merge-pathnames "k.py" dir)))
           (root (namestring (truename dir)))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers* (stand-in-server-setting log))
           (tuatara-user:*lsp-trusted-projects* (list (namestring dir))))
      (alexandria:write-string-into-file (format nil "x = 1~%") file)
      (alexandria:write-string-into-file (format nil "y = 1~%") (merge-pathnames "pkg/old.py" dir))
      (alexandria:write-string-into-file (format nil "z = 1~%") (merge-pathnames "pkg/gone.py" dir))
      (flet ((told-p (line) (lambda () (member line (told log) :test #'string=))))
        (unwind-protect
             (auto-save-session
              (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                    (list :until (lambda () (let ((tree (gethash root tuatara.defaults::*lsp-watched-trees*)))
                                              (and tree (tuatara.defaults::watched-tree-ready tree)))))
                    (list :wait 0.3)
                    (list :until (lambda ()
                                   (alexandria:write-string-into-file (format nil "w = 2~%") (merge-pathnames "pkg/new.py" dir))
                                   (alexandria:write-string-into-file (format nil "y = 22~%") (merge-pathnames "pkg/old.py" dir) :if-exists :supersede)
                                   (delete-file (merge-pathnames "pkg/gone.py" dir))
                                   (alexandria:write-string-into-file "built" (merge-pathnames "target/made.py" dir))
                                   (alexandria:write-string-into-file "notes" (merge-pathnames "pkg/notes.txt" dir))
                                   t))
                    (list :until (told-p "watched new.py 1"))
                    (list :until (told-p "watched old.py 2"))
                    (list :until (told-p "watched gone.py 3"))
                    (list :wait 0.3)))
          (tuatara.defaults::lsp-stop-all))
        (true (funcall (told-p "watched new.py 1")))
        (true (funcall (told-p "watched old.py 2")))
        (true (funcall (told-p "watched gone.py 3")))
        (false (funcall (told-p "watched made.py 1")) "a build's folder is not watched")
        (false (find-if (lambda (line) (search "notes.txt" line)) (told log)) "a file the patterns do not match is not told")))))

(define-test (lsp folds-lenses-selections-and-files-not-open)
  ;; C-c z hides a block, C-c Z all of them; code lenses at the lines' ends, C-c k runs one;
  ;; the server's pieces of the code for the selection; what it finds in a file not open listed
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames ".git/" dir))
    (let* ((log (merge-pathnames "told.log" dir))
           (file (namestring (merge-pathnames "m.py" dir)))
           (text (format nil "def one():~%    a = 1~%    return a~%~%def two():~%    return one()~%"))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers* (stand-in-server-setting log))
           (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
           (tuatara-user:*lsp-sync-delay* 0.05)
           (tuatara-user:*lsp-code-lenses* t)
           (folded nil) (all nil) (ranges nil) (opened nil))
      (alexandria:write-string-into-file text file)
      (alexandria:write-string-into-file (format nil "# a file of the project~%x = 2~%") (merge-pathnames "elsewhere.py" dir))
      (flet ((buffer () (tuatara.buffer:find-buffer "m.py")))
        (unwind-protect
             (multiple-value-bind (f editor)
                 (auto-save-session
                  (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                        (list :until (lambda () (tuatara.buffer:buffer-local (buffer) 'tuatara.defaults::lsp-lenses)))
                        (list :until (lambda () (find "  1 calls" (tuatara.defaults::buffer-hints (buffer)) :key #'second :test #'string=)))
                        ;; in one's body: its block hidden under its first line
                        "M-< C-n C-n" "C-c z"
                        (list :until (lambda () (setf folded (copy-list (tuatara.display:buffer-folds (buffer))))))
                        "C-c z" "C-c Z"
                        (list :until (lambda () (setf all (copy-list (tuatara.display:buffer-folds (buffer))))))
                        "C-c Z"
                        ;; on one's line: its lens run
                        "M-< C-c k" (list :wait 0.2)
                        ;; the pieces the server sees around a
                        (list :until (lambda () (setf ranges (tuatara.defaults::lsp-selection-ranges (buffer) (search "a = 1" text)))))
                        ;; what it found in a file not open, listed and gone to
                        "C-c l" (list :wait 0.2) "RET" (list :wait 0.3)
                        (list :until (lambda () (setf opened (let ((b (tuatara.buffer:find-buffer "elsewhere.py")))
                                                               (and b (tuatara.buffer:buffer-point b)))))))
                  :width 250 :height 30)
               (declare (ignore editor))
               (true (tuatara.frontend.fake:fake-frame-showing f "def one():  2 calls"))
               (is equal (list (cons (search (format nil "~%    a") text) (search (format nil "~%~%def two") text))) folded)
               (is = 2 (length all))
               (true (member "ran one" (told log) :test #'string=))
               (is equal (cons (search "a = 1" text) (1+ (search "a = 1" text))) (first ranges))
               (is = 3 (length ranges))
               (true (tuatara.frontend.fake:fake-frame-showing f "elsewhere.py:2:5  warning (stand-in): elsewhere found"))
               (is = (+ 4 (length (format nil "# a file of the project~%"))) opened))
          (tuatara.defaults::lsp-stop-all))))))

(define-test (lsp selection-grown-by-ranges)
  ;; with no syntax tree, the selection grows through the pieces a server gives, and shrinks back
  (let ((tuatara.defaults::*modal-editing* nil)
        (tuatara.defaults::*selection-ranges-functions*
          (list (lambda (buffer position) (declare (ignore buffer position)) (list (cons 4 7) (cons 0 11))))))
    (flet ((region (keys)
             ;; text mode: no syntax tree
             (let* ((editor (nth-value 1 (run-keys (format nil "M-x t e x t - m o d e RET C-f C-f C-f C-f C-f ~a" keys)
                                                   :text "abc def ghi" :width 80)))
                    (buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
               (multiple-value-list (tuatara.buffer:buffer-region buffer)))))
      (is equal '(4 7) (region "M-x e x p a n d - s e l e c t i o n RET"))
      (is equal '(0 11) (region "M-x e x p a n d - s e l e c t i o n RET M-x e x p a n d - s e l e c t i o n RET"))
      (is equal '(0 11) (region "C-u 2 M-x e x p a n d - s e l e c t i o n RET"))
      (is equal '(4 7) (region "C-u 2 M-x e x p a n d - s e l e c t i o n RET M-x s h r i n k - s e l e c t i o n RET"))
      (is equal '(4 7) (region "M-x e x p a n d - s e l e c t i o n RET M-x e x p a n d - s e l e c t i o n RET M-x s h r i n k - s e l e c t i o n RET")))))

(define-test (lsp code-keys-first-for-point)
  ;; in code, C-c's hints show first the keys for what the cursor is on: a problem, a name
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames ".git/" dir))
    (let* ((file (namestring (merge-pathnames "n.py" dir)))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers* (stand-in-server-setting (merge-pathnames "told.log" dir)))
           (tuatara-user:*lsp-trusted-projects* (list (namestring dir))))
      (alexandria:write-string-into-file (format nil "total = bad~%") file)
      (unwind-protect
           (multiple-value-bind (f editor)
               (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                                        (list :until (lambda () (tuatara.defaults::buffer-diagnostics (tuatara.buffer:find-buffer "n.py"))))
                                        "M-< C-e C-b C-c" (list :wait 0.2) "C-g" "M-< C-c" (list :wait 0.2) "C-g")
                                  :width 120 :height 24)
             (declare (ignore editor))
             (true (tuatara.frontend.fake:fake-frame-showing f "problem at point"))
             (true (tuatara.frontend.fake:fake-frame-showing f "name at point")))
        (tuatara.defaults::lsp-stop-all)))))

(define-test (lsp snippet-completions)
  ;; a server's completion that is a snippet: put in with its fields, gone through with TAB
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames ".git/" dir))
    (let* ((file (namestring (merge-pathnames "o.py" dir)))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers* (stand-in-server-setting (merge-pathnames "told.log" dir)))
           (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
           (text nil))
      (alexandria:write-string-into-file (format nil "x = 1~%") file)
      (unwind-protect
           (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                                    (list :until (lambda () (tuatara.defaults::buffer-servers (tuatara.buffer:find-buffer "o.py"))))
                                    (list :wait 0.2)
                                    "M-> a d d c a l" "M-x c o m p l e t i o n - a t - p o i n t RET" (list :wait 0.3)
                                    "1 TAB 2 TAB"
                                    (list :until (lambda () (setf text (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "o.py"))))))
                              :width 100)
        (tuatara.defaults::lsp-stop-all))
      (is string= (format nil "x = 1~%add(1, 2)") text))))

(define-test (lsp json-taken-with-care)
  ;; half of a pair alone stands as U+FFFD, what follows it read as itself
  (is string= (format nil "~cA" (code-char #xFFFD)) (tuatara.text:json-read "\"\\ud800A\""))
  (is string= (format nil "~cA" (code-char #xFFFD)) (tuatara.text:json-read "\"\\ud800\\u0041\""))
  (is string= (format nil "~c~c" (code-char #xFFFD) (code-char #x1F600)) (tuatara.text:json-read "\"\\ud800\\ud83d\\ude00\""))
  ;; a number beginning with 0, or written with other scripts' digits, is no JSON
  (dolist (text (list "01" "-01" "00" (format nil "1~c" (code-char #x0662))))
    (fail (tuatara.text:json-read text) tuatara.text:json-error))
  (is = 0 (tuatara.text:json-read "0"))
  (is = -0.5d0 (tuatara.text:json-read "-0.5"))
  ;; a number too long refused as it is read, not after
  (fail (tuatara.text:json-read (make-string 1000000 :initial-element #\1)) tuatara.text:json-error)
  ;; an end past the text refused as not JSON
  (fail (tuatara.text:json-read "[1]" :end 10) tuatara.text:json-error)
  (is equalp #(1) (tuatara.text:json-read "[1] junk" :end 3)))

(defun fake-server (root &optional (encoding :utf-8))
  "A language server that is not running, for the project at ROOT, counting positions in ENCODING."
  (let ((server (tuatara.defaults::make-lsp-server "fake" '("fake") (namestring root) nil nil nil)))
    (setf (tuatara.defaults::lsp-server-encoding server) encoding)
    server))

(define-test (lsp server-values-checked)
  ;; values of the wrong kind taken as none, never failing all a server sent
  (let ((value (tuatara.text:json-read "{\"line\": 1.0, \"items\": {\"a\": 1}, \"message\": 42, \"n\": 3}")))
    (false (tuatara.text:json-integer value "line"))
    (is = 3 (tuatara.text:json-integer value "n"))
    (false (tuatara.text:json-text value "message"))
    (is equalp #() (tuatara.text:json-items value "items"))
    (false (tuatara.defaults::position-line-character (tuatara.text:json-read "{\"line\": 1.0, \"character\": 0}"))))
  ;; a signature asked for by a number out of bounds: the nearest
  (is string= "f(a)  — a"
      (tuatara.defaults::lsp-signature-words
       (tuatara.text:json-read "{\"signatures\": [{\"label\": \"f(a)\", \"parameters\": [{\"label\": \"a\"}]}], \"activeSignature\": -1, \"activeParameter\": 0}")))
  ;; edits that say not where they go, or that overlap, are not made at all
  (let* ((server (fake-server "/nowhere/"))
         (rope (tuatara.text:string->rope (format nil "abc~%def~%")))
         (bytes (lambda (range) (tuatara.defaults::lsp-range-bytes server rope range))))
    (flet ((edits (text) (tuatara.defaults::text-edits (tuatara.text:json-read text) bytes)))
      (is = 2 (length (edits "[{\"range\": {\"start\": {\"line\": 1, \"character\": 0}, \"end\": {\"line\": 1, \"character\": 1}}, \"newText\": \"D\"},
                               {\"range\": {\"start\": {\"line\": 0, \"character\": 0}, \"end\": {\"line\": 0, \"character\": 0}}, \"newText\": \"x\"}]")))
      (false (edits "[{\"newText\": \"x\"}]"))
      (false (edits "[{\"range\": {\"start\": {\"line\": null, \"character\": 0}, \"end\": {\"line\": 0, \"character\": 0}}, \"newText\": \"x\"}]"))
      (false (edits "[{\"range\": {\"start\": {\"line\": 0, \"character\": 0}, \"end\": {\"line\": 0, \"character\": 2}}, \"newText\": \"x\"},
                      {\"range\": {\"start\": {\"line\": 0, \"character\": 1}, \"end\": {\"line\": 0, \"character\": 3}}, \"newText\": \"y\"}]")))))

(define-test (lsp positions-at-characters)
  ;; a place a server names is never within a character, nor past its line
  (let ((rope (tuatara.text:string->rope (format nil "aé€~%b~a" (string (code-char #x1F600)))))
        (utf-8 (fake-server "/nowhere/" :utf-8))
        (utf-16 (fake-server "/nowhere/" :utf-16)))
    (flet ((at (server line character) (tuatara.defaults::lsp-line-byte server rope line character)))
      (is = 1 (at utf-8 0 2) "within é: its start")
      (is = 3 (at utf-8 0 4) "within €: its start")
      (is = 6 (at utf-8 0 99) "past the line: its end")
      (is = 7 (at utf-8 99 0) "past the text: the last line")
      (is = 8 (at utf-16 1 2) "within the pair of 😀: its start")
      (is = 12 (at utf-16 1 3)))))

(define-test (lsp addresses-and-places)
  ;; only a file of this machine, from the root, its .. taken as they lead
  (false (tuatara.defaults::uri-path "file://../../etc/passwd"))
  (false (tuatara.defaults::uri-path "file://otherhost/x"))
  (false (tuatara.defaults::uri-path "file:///a%00b"))
  (false (tuatara.defaults::uri-path "https://example.org/a"))
  (is string= "/a/c" (tuatara.defaults::uri-path "file:///a/b/../c"))
  (is string= "/x y" (tuatara.defaults::uri-path "file://localhost/x%20y"))
  (with-test-directory (dir)
    (let* ((root (namestring (ensure-directories-exist (merge-pathnames "proj/" dir))))
           (server (fake-server root))
           (inside (namestring (merge-pathnames "a.py" root)))
           (outside (namestring (merge-pathnames "proj/../escape.py" dir))))
      (alexandria:write-string-into-file (format nil "first~%second~%") inside)
      (alexandria:write-string-into-file (format nil "secret~%") (merge-pathnames "escape.py" dir))
      (sb-posix:symlink (namestring dir) (concatenate 'string root "out"))
      (true (tuatara.io:really-within-p inside root))
      (false (tuatara.io:really-within-p outside root))
      (false (tuatara.io:really-within-p (concatenate 'string root "out/escape.py") root) "a link out of the project")
      ;; what a server finds wrong outside its project is not kept
      (tuatara.defaults::keep-file-diagnostics server (tuatara.defaults::path-uri outside)
                                               (tuatara.text:json-read "[{\"range\": {\"start\": {\"line\": 0, \"character\": 0}, \"end\": {\"line\": 0, \"character\": 1}}, \"message\": \"x\"}]"))
      (false (gethash (namestring (merge-pathnames "escape.py" dir)) tuatara.defaults::*lsp-file-diagnostics*))
      ;; the line beside a place: read from a file of the project only, never one in *safe-directories*
      (let ((tuatara.defaults::*location-files* (make-hash-table :test 'equal)))
        (is string= "second" (tuatara.defaults::location-line server inside 1))
        (is string= "" (tuatara.defaults::location-line server (tuatara.defaults::uri-path (tuatara.defaults::path-uri outside)) 0)))
      (let ((tuatara-user:*safe-directories* (list root)))
        (is string= "" (tuatara.defaults::location-line server inside 1))))))

(define-test (lsp refused-not-utf-8)
  ;; octets that are not UTF-8 cannot go in a message: the file is not given to a server
  (with-test-directory (dir)
    (let ((buffer (tuatara.buffer:make-buffer "raw-octets.py" :text (coerce #(120 32 61 32 255 10) 'tuatara.text:octets)
                                                               :file (namestring (merge-pathnames "raw.py" dir)))))
      (unwind-protect (is string= "it is not valid UTF-8" (tuatara.defaults::lsp-refusal buffer))
        (tuatara.buffer:kill-buffer buffer)))))

(define-test (lsp server-questions-and-edits)
  ;; a question no function answers is answered with an error; a change the server asks for of
  ;; itself is asked about, and never makes a file, reaches another machine, or goes on unasked
  (with-test-directory (dir)
    (let* ((root (merge-pathnames "proj/" dir))
           (log (merge-pathnames "told.log" dir))
           (file (namestring (merge-pathnames "p.py" root)))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers* (stand-in-server-setting log))
           (tuatara-user:*lsp-trusted-projects* (list (namestring root))))
      (ensure-directories-exist (merge-pathnames ".git/" root))
      (alexandria:write-string-into-file (format nil "editme = 1~%") file)
      (alexandria:write-string-into-file (format nil "one~%") (merge-pathnames "inside.txt" root))
      (alexandria:write-string-into-file (format nil "two~%") (merge-pathnames "outside.txt" dir))
      (flet ((applied () (remove-if-not (lambda (line) (uiop:string-prefix-p "applied" line)) (told log))))
        (unwind-protect
             (multiple-value-bind (f editor)
                 (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                                          (list :until (lambda () (member "answered unknown -32601" (told log) :test #'string=)))
                                          (list :wait 0.3) "n RET"
                                          (list :until (lambda () (= 1 (length (applied)))))
                                          "C-x k RET" (list :wait 0.2)
                                          (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                                          (list :wait 0.3) "y RET"
                                          (list :until (lambda () (= 2 (length (applied))))))
                                    :width 200)
               (declare (ignore editor))
               (true (tuatara.frontend.fake:fake-frame-showing f "python3 asks to change 3 files (1 outside"))
               (true (search "missing.txt is not there" (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Messages*")))))
          (tuatara.defaults::lsp-stop-all))
        (true (member "answered unknown -32601" (told log) :test #'string=))
        ;; refused, then made where it could be: never a new file
        (is equal '("applied false" "applied false") (applied))
        (let ((inside (tuatara.buffer:find-buffer "inside.txt"))
              (outside (tuatara.buffer:find-buffer "outside.txt")))
          (true (and inside (string= (format nil "edited one~%") (tuatara.buffer:buffer-string inside))))
          (true (and outside (string= (format nil "edited two~%") (tuatara.buffer:buffer-string outside)))))
        (is string= (format nil "two~%") (alexandria:read-file-into-string (merge-pathnames "outside.txt" dir)) "nothing saved")
        (false (probe-file (merge-pathnames "missing.txt" root)))
        (false (tuatara.buffer:find-buffer "missing.txt"))))))

(define-test (lsp servers-ending-and-stalling)
  ;; a server that ends mid-question is no longer waited for; one that sends too much is let go
  ;; unread; one that stops reading is ended, the editor never held up
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames ".git/" dir))
    (let* ((log (merge-pathnames "told.log" dir))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers* (stand-in-server-setting log))
           (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
           (largest tuatara.defaults::*lsp-largest-message*)
           (most-unsent tuatara.defaults::*lsp-most-unsent*)
           (took nil) (sending nil) (status nil))
      (flet ((file (name text)
               (let ((path (namestring (merge-pathnames name dir))))
                 (alexandria:write-string-into-file text path)
                 path))
             (buffer (name) (tuatara.buffer:find-buffer name)))
        (unwind-protect
             (progn
               (setf tuatara.defaults::*lsp-largest-message* 100000
                     tuatara.defaults::*lsp-most-unsent* 100000)
               (multiple-value-bind (f editor)
                   (auto-save-session
                    (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (file "huge.py" (format nil "huge = 1~%"))))
                          (list :until (lambda () (tuatara.defaults::buffer-servers (buffer "huge.py"))))
                          (list :wait 0.2)
                          "M-x l s p - d e s c r i b e - t h i n g - a t - p o i n t RET" (list :wait 0.2)
                          (list :until (lambda () (setf status (tuatara.defaults::lsp-server-status
                                                                (first (tuatara.defaults::buffer-servers (buffer "huge.py")))))))
                          (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (file "die.py" (format nil "die = 1~%"))))
                          (list :until (lambda () (tuatara.defaults::buffer-servers (buffer "die.py"))))
                          (list :wait 0.2)
                          (list :until (lambda () (setf took (get-internal-real-time))))
                          "M-x l s p - d e s c r i b e - t h i n g - a t - p o i n t RET"
                          (list :until (lambda () (setf took (/ (- (get-internal-real-time) took) internal-time-units-per-second))))
                          (list :until (lambda () (tuatara.defaults::lsp-stop-all) t))
                          (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (file "stall.py" (format nil "stall = 1~%"))))
                          (list :until (lambda () (tuatara.defaults::buffer-servers (buffer "stall.py"))))
                          (list :wait 0.3)
                          (list :until (lambda ()
                                         (let ((server (first (tuatara.defaults::buffer-servers (buffer "stall.py"))))
                                               (start (get-internal-real-time)))
                                           (loop repeat 50
                                                 do (tuatara.defaults::lsp-notify server "tuatara/filler"
                                                                                  (tuatara.text:json-object "text" (make-string 10000 :initial-element #\x))))
                                           (setf sending (/ (- (get-internal-real-time) start) internal-time-units-per-second))
                                           (setf status (list status (tuatara.defaults::lsp-server-status server))))))
                          (list :wait 0.1))
                    :width 160)
                 (declare (ignore editor))
                 (true (search "sent a message of" (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Messages*"))))
                 (true (tuatara.frontend.fake:fake-frame-showing f "stopped (it stopped reading what it is sent)"))))
          (setf tuatara.defaults::*lsp-largest-message* largest
                tuatara.defaults::*lsp-most-unsent* most-unsent)
          (tuatara.defaults::lsp-stop-all))
        (is equal '(:ready :stopped) status "the server that sent too much still runs; the one that stopped reading ended")
        (true (< took 1.5) "not waited for once it ended")
        (true (< sending 0.5) "the editor never waits for a server to read")))))

(define-test (lsp documents-closed-before-shutdown)
  ;; a server ended with a file open is told it is closed before it is asked to end
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames ".git/" dir))
    (let* ((log (merge-pathnames "told.log" dir))
           (file (namestring (merge-pathnames "q.py" dir)))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers* (stand-in-server-setting log))
           (tuatara-user:*lsp-trusted-projects* (list (namestring dir))))
      (alexandria:write-string-into-file (format nil "x = 1~%") file)
      (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                               (list :until (lambda () (member "textDocument/didOpen" (told log) :test #'string=)))
                               (list :until (lambda () (tuatara.defaults::lsp-stop-all) t))
                               (list :until (lambda () (member "exit" (told log) :test #'string=)))))
      (let ((told (told log)))
        (true (< (or (position "textDocument/didClose" told :test #'string=) 1000)
                 (or (position "shutdown" told :test #'string=) 0)))))))

(define-test (lsp restart-only-when-trusted)
  ;; M-x lsp-restart in a project not trusted starts no server: its code would run unasked
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames ".git/" dir))
    (let* ((log (merge-pathnames "told.log" dir))
           (file (namestring (merge-pathnames "r.py" dir)))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers* (stand-in-server-setting log))
           (tuatara-user:*lsp-trusted-projects* '()))
      (alexandria:write-string-into-file (format nil "x = 1~%") file)
      (unwind-protect
           (let ((f (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file)) (list :wait 0.3)
                                             "M-x l s p - r e s t a r t RET" (list :wait 0.5))
                                       :width 140)))
             (false (told log))
             (true (tuatara.frontend.fake:fake-frame-showing f "No language server started"))
             (tuatara.buffer:kill-buffer (tuatara.buffer:find-buffer "r.py")))
        (tuatara.defaults::lsp-stop-all)))))

(define-test (lsp blocks-not-served-asked-once)
  ;; a note's blocks in a project not trusted: no server takes them, and typing does not
  ;; ask again at each pause, only when its blocks change
  (with-test-directory (dir)
    (ensure-directories-exist (merge-pathnames ".git/" dir))
    (let* ((file (namestring (merge-pathnames "u.org" dir)))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers* (stand-in-server-setting (merge-pathnames "told.log" dir)))
           (tuatara-user:*lsp-trusted-projects* '())
           (tuatara-user:*lsp-sync-delay* 0.05)
           (calls 0))
      (alexandria:write-string-into-file (format nil "Words.~%~%#+begin_src python~%  x = 1~%#+end_src~%") file)
      (sb-int:encapsulate 'tuatara.defaults::lsp-connect 'count-calls
                          (lambda (f &rest arguments) (incf calls) (apply f arguments)))
      (unwind-protect
           (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file)) (list :wait 0.3)
                                    (list :until (lambda () (setf calls 0) t))
                                    "a" (list :wait 0.2) "b" (list :wait 0.2) "c" (list :wait 0.2)))
        (sb-int:unencapsulate 'tuatara.defaults::lsp-connect 'count-calls)
        (tuatara.defaults::lsp-stop-all))
      (true (<= calls 1) "asked ~d times" calls))))

(define-test (lsp tick-rests-while-nothing-waits)
  ;; with servers running and nothing to send, the editor is not woken to look again;
  ;; a change waiting is looked at when it is due
  (let* ((buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "*lsp tick*") :text "x"))
         (tuatara.defaults::*lsp-servers* (list :running))
         (tuatara.defaults::*lsp-changed* '())
         (tuatara-user:*lsp-sync-delay* 0.5))
    (unwind-protect
         (progn
           (false (tuatara.defaults::lsp-tick nil))
           (setf tuatara.defaults::*lsp-changed* (list (cons buffer (get-internal-real-time))))
           (let ((due (tuatara.defaults::lsp-tick nil)))
             (true (and (realp due) (< 0.3 due 0.51)) "due in ~a" due)))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (lsp tangled-blocks-stay-in-the-project)
  ;; a note's blocks written to a file of its project are seen as that file; to one outside it, as the note
  (with-test-directory (dir)
    (let* ((root (merge-pathnames "proj/" dir))
           (tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*language-servers* (stand-in-server-setting (merge-pathnames "told.log" dir)))
           (uris '()))
      (ensure-directories-exist (merge-pathnames ".git/" root))
      (loop for (name tangle) in '(("in.org" "code.py") ("out.org" "../../escape.py"))
            do (alexandria:write-string-into-file (format nil "#+begin_src python :tangle ~a~%x = 1~%#+end_src~%" tangle)
                                                  (merge-pathnames name root)))
      (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (namestring (merge-pathnames "in.org" root))))
                               (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (namestring (merge-pathnames "out.org" root))))
                               (list :until (lambda ()
                                              (setf uris (mapcar (lambda (name) (second (first (tuatara.defaults::lsp-doc-specs
                                                                                                (tuatara.buffer:find-buffer name)))))
                                                                 '("in.org" "out.org")))))))
      (true (and (first uris) (uiop:string-suffix-p (first uris) "/proj/code.py")))
      (true (and (second uris) (uiop:string-suffix-p (second uris) "/proj/out.org.py"))))))

(define-test (lsp placed-items-moved-when-wanted)
  ;; what servers said of places: those looked at moved alone, the same as all moved at once;
  ;; the marks beside lines and the hints found by halves, the same as looked for one by one
  (let* ((text (format nil "~{word~d ~}~%" (loop for i below 2000 collect i)))
         (b (tuatara.buffer:make-buffer "placed-items" :text (format nil "~a~a~a" text text text))))
    (unwind-protect
         (flet ((items () (coerce (loop for at from 0 below (- (tuatara.buffer:buffer-size b) 4) by 7
                                        collect (list at (+ at 4) :token))
                                  'simple-vector)))
           (tuatara.buffer::ensure-indexed b)
           (let ((lazy (tuatara.defaults::make-placed (tuatara.buffer:buffer-version b) (items)))
                 (eager (tuatara.defaults::make-placed (tuatara.buffer:buffer-version b) (items))))
             (dotimes (i 5)
               (tuatara.buffer:buffer-insert b (* 997 (1+ i)) "inserted")
               (tuatara.buffer:buffer-delete b (* 1301 (1+ i)) (+ 9 (* 1301 (1+ i))))
               ;; a few looked at in the middle, by halves
               (let ((from (* 1000 (1+ i))))
                 (loop for k from (tuatara.defaults::placed-first lazy b (lambda (item) (> (second item) from)))
                       repeat 20
                       do (tuatara.defaults::placed-item lazy k b))))
             (is equalp (tuatara.defaults::placed-items-now eager b) (tuatara.defaults::placed-items-now lazy b))))
      (tuatara.buffer:kill-buffer b)))
  (let ((b (tuatara.buffer:make-buffer "placed-diagnostics" :text (format nil "~{line ~d~%~}" (loop for i below 300 collect i)))))
    (unwind-protect
         (progn
           (tuatara.buffer::ensure-indexed b)
           (setf (tuatara.buffer:buffer-local b 'tuatara.defaults::lsp-diagnostics)
                 (list (cons :one (tuatara.defaults::make-placed
                                   (tuatara.buffer:buffer-version b)
                                   (loop for line from 0 below 300 by 3
                                         for at = (tuatara.buffer:buffer-goto-line b line)
                                         collect (tuatara.defaults::make-diagnostic at (+ at 2) (1+ (mod line 4)) "wrong" "test")
                                         when (zerop (mod line 9))
                                           collect (tuatara.defaults::make-diagnostic (1+ at) (+ at 3) 1 "worse" "test"))))))
           (let* ((w (tuatara.display:make-window b :width 40 :height 30))
                  (rows (tuatara.display:window-layout-rows w 0 30))
                  (ready (tuatara.defaults::diagnostic-gutter-prepare nil w rows)))
             (loop for line from 0 below 25
                   do (is equal (multiple-value-list (tuatara.defaults::diagnostic-gutter-cell nil w nil line))
                          (multiple-value-list (tuatara.defaults::diagnostic-gutter-cell nil w nil line ready))))
             (is equal '("E" " " " " "·") (loop for line below 4 collect (tuatara.defaults::diagnostic-gutter-cell nil w nil line ready)))
             (is = 2 (length (tuatara.defaults::diagnostics-in b 0 (tuatara.buffer:buffer-goto-line b 1))))))
      (tuatara.buffer:kill-buffer b))))

(define-test (lsp outermost-blocks-in-one-pass)
  ;; the blocks a fold of all hides: those inside no other, found after one sort, not by
  ;; comparing every pair
  (is equal '((0 10) (12 20)) (tuatara.defaults::outermost-ranges '((2 5) (0 10) (12 20) (0 10) (3 4) (12 15) (0 7))))
  (is equal '((0 10) (5 15)) (tuatara.defaults::outermost-ranges '((5 15) (0 10))))
  (flet ((seconds (n)
           (let ((ranges (loop for i below n append (list (list (* 10 i) (+ (* 10 i) 9)) (list (+ (* 10 i) 1) (+ (* 10 i) 5)))))
                 (start (get-internal-real-time)))
             (tuatara.defaults::outermost-ranges ranges)
             (/ (- (get-internal-real-time) start) internal-time-units-per-second))))
    ;; a few microseconds a block; a millisecond and more when every pair was compared
    (let ((each (/ (- (seconds 8000) (seconds 2000)) 12000)))
      (true (< each 0.00005) "~,2f µs for each more block" (* each 1000000)))))

(define-test (lsp capabilities-claimed-are-asked)
  ;; what the editor tells a server it can do, it does: every request it claims is made
  ;; somewhere in the client, and the kinds of names it takes are those it colours
  (let* ((source (with-output-to-string (out)
                   (dolist (file (directory (merge-pathnames "lisp/lsp/*.lisp" (asdf:system-source-directory :tuatara))))
                     (write-string (uiop:read-file-string file) out))))
         (capabilities (tuatara.defaults::lsp-capabilities))
         (text-document (tuatara.text:json-get capabilities "textDocument"))
         ;; a capability named apart from the method that uses it
         (methods '(("synchronization" . "didOpen") ("callHierarchy" . "prepareCallHierarchy")
                    ("semanticTokens" . "semanticTokens/full"))))
    (loop for capability being the hash-keys of text-document
          do (let ((method (or (cdr (assoc capability methods :test #'string=)) capability)))
               (true (search (format nil "\"textDocument/~a\"" method) source) "~a is claimed but never asked for" capability)))
    (false (tuatara.text:json-get text-document "rename" "prepareSupport") "rename is claimed to be prepared")
    (is equalp (map 'vector #'car tuatara.defaults::*lsp-semantic-faces*)
        (tuatara.text:json-get text-document "semanticTokens" "tokenTypes"))))

(define-test (lsp messages-read-with-little-garbage)
  ;; a message of 2 MB (a long hover) read and decoded once: some 19 octets made for each of
  ;; its octets went to decoding it alone, as text that took what was not UTF-8 for a ?
  (with-test-directory (dir)
    (let* ((path (namestring (merge-pathnames "message" dir)))
           (body (tuatara.text:string-octets (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"contents\":\"~a\"}}"
                                                     (make-string (* 2 1024 1024) :initial-element #\x)))))
      (with-open-file (out path :direction :output :element-type '(unsigned-byte 8))
        (write-sequence (tuatara.text:string-octets (format nil "Content-Length: ~d~c~c~c~c" (length body) #\Return #\Newline #\Return #\Newline)) out)
        (write-sequence body out))
      (let ((consed (loop repeat 3
                          minimize (with-open-file (in path :element-type :default :external-format :latin-1)
                                     (let ((before (sb-ext:get-bytes-consed)))
                                       (is equal (* 2 1024 1024)
                                           (length (tuatara.text:json-get (tuatara.defaults::lsp-read-message in) "result" "contents")))
                                       (- (sb-ext:get-bytes-consed) before))))))
        (true (< consed (* 16 (length body))) "~,1f octets made for each octet" (/ consed (length body)))))))

;;; What a server makes of the changes sent: the text it has, kept beside the buffer's

(defun lsp-line-start (text line)
  "Where the 0-based LINE of TEXT begins, lines ended by CR LF, LF or CR alone
as a language server ends them; NIL past its last."
  (let ((at 0))
    (loop repeat line
          do (let ((end (position-if (lambda (c) (member c '(#\Return #\Newline))) text :start at)))
               (unless end (return-from lsp-line-start nil))
               (setf at (if (and (char= (char text end) #\Return) (< (1+ end) (length text))
                                 (char= (char text (1+ end)) #\Newline))
                            (+ end 2)
                            (1+ end)))))
    at))

(defun lsp-applied-index (text position encoding)
  "The index into TEXT, a string, of POSITION as a language server reads it,
counting ENCODING's units (:UTF-8 octets or :UTF-16 units) along its line;
NIL when it is not exactly at a character within the line."
  (let ((at (lsp-line-start text (tuatara.text:json-get position "line")))
        (character (tuatara.text:json-get position "character"))
        (units 0))
    (when at
      (loop while (< units character)
            do (when (or (>= at (length text)) (member (char text at) '(#\Return #\Newline)))
                 (return-from lsp-applied-index nil))
               (incf units (if (eq encoding :utf-8)
                               (tuatara.text:char-octet-length (char text at))
                               (if (> (char-code (char text at)) #xFFFF) 2 1)))
               (incf at))
      (and (= units character) at))))

(defun lsp-applied (text changes encoding)
  "TEXT with CHANGES, the contentChanges of a didChange, made one after
another as a language server makes them; NIL when a range is not exactly at
characters of the text it is made in."
  (loop for change across changes
        for range = (tuatara.text:json-get change "range")
        do (if (null range)
               (setf text (tuatara.text:json-get change "text"))
               (let ((start (lsp-applied-index text (tuatara.text:json-get range "start") encoding))
                     (end (lsp-applied-index text (tuatara.text:json-get range "end") encoding)))
                 (unless (and start end (<= start end))
                   (return nil))
                 (setf text (concatenate 'string (subseq text 0 start) (tuatara.text:json-get change "text") (subseq text end))))))
  text)

(defun messages-written (server)
  "The messages written to SERVER, a stand-in with no program, since last
asked: each a JSON object, in order."
  (loop for octets = (sb-concurrency:receive-message-no-hang (tuatara.defaults::lsp-server-outbox server))
        while octets
        collect (tuatara.text:json-read (tuatara.text:octets-string octets))))

(defun server-taking-changes (encoding)
  "A ready stand-in language server with no program that takes changes as ranges, counting ENCODING's units."
  (let ((server (tuatara.defaults::make-lsp-server "kea" '("true") "/tmp/" 'python nil nil)))
    (setf (tuatara.defaults::lsp-server-status server) :ready
          (tuatara.defaults::lsp-server-encoding server) encoding
          (tuatara.defaults::lsp-server-capabilities server) (tuatara.text:json-object "textDocumentSync" 2))
    server))

(defun note-shadow-slowly (text language)
  "What a note's LANGUAGE blocks' document holds, TEXT the note: worked out
line by line from the whole text, an oracle for ORG-BLOCKS-SHADOW."
  (let* ((lines (coerce (uiop:split-string text :separator '(#\Newline)) 'vector))
         (shown (make-array (length lines) :initial-element ""))
         (open nil))
    (loop for line across lines
          for n from 0
          do (if open
                 (when (cl-ppcre:scan "(?i)^[ \\t]*#\\+end_src\\b" line)
                   (when (eq (tuatara.defaults::org-src-language (first open)) language)
                     (let* ((code (subseq lines (second open) n))
                            (indent (loop for l across code
                                          unless (zerop (length (string-trim '(#\Space #\Tab) l)))
                                            minimize (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) l) 0))))
                       (loop for l across code
                             for k from (second open)
                             for cut = (min (or indent 0) (length l))
                             for rest = (subseq l cut)
                             do (setf (aref shown k)
                                      (if (or (uiop:string-prefix-p ",*" rest) (uiop:string-prefix-p ",#+" rest)) (subseq rest 1) rest)))))
                   (setf open nil))
                 (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "(?i)^[ \\t]*#\\+begin_src[ \\t]+([^ \\t]+)" line)
                   (when match (setf open (list (aref groups 0) (1+ n)))))))
    (format nil "~{~a~^~%~}" (coerce shown 'list))))

(defparameter *note-pieces*
  (vector "a" "x = 1" " " "  " "é" "😀" "中" (string #\Newline) (string #\Newline) (format nil "~c~c" #\Return #\Newline)
          (format nil "#+begin_src python~%") (format nil "  #+BEGIN_SRC python :tangle no~%") (format nil "#+end_src~%")
          (format nil "#+begin_src rust~%") (format nil "* Heading~%") ",*" ",#+" "#+end_src" "#+begin_src python")
  "Pieces random notes are built from: code, Org's block lines, headlines, escaping commas and line ends of each kind.")

(defun random-sync-session (encoding text shadow-language)
  "Edit a buffer holding TEXT at random, its changes sent to a stand-in
server counting ENCODING's units now and then, and check after each send that
the server's text, made from what it was sent, is the buffer's (or, with
SHADOW-LANGUAGE, the document of the note's blocks of that language); undo,
redo and a whole text swapped among the edits. Returns how many changes went
as ranges and how many whole."
  (let* ((buffer (tuatara.buffer:make-buffer (format nil "sync-~a" (random 1000000)) :text text))
         (server (server-taking-changes encoding))
         (doc (tuatara.defaults::make-lsp-doc buffer (or shadow-language 'python) "file:///tmp/sync.py"
                                              (and shadow-language
                                                   (lambda (b &optional doc stretches)
                                                     (tuatara.defaults::org-blocks-shadow b shadow-language doc stretches)))))
         (tuatara.defaults::*lsp-doc-spec-functions* '())
         (model nil) (ranges 0) (whole 0))
    (flet ((expected () (let ((now (tuatara.buffer:buffer-string buffer)))
                          (if shadow-language (note-shadow-slowly now shadow-language) now)))
           (take (messages)
             (dolist (message messages)
               (let ((params (tuatara.text:json-get message "params")))
                 (cond ((equal (tuatara.text:json-get message "method") "textDocument/didOpen")
                        (setf model (tuatara.text:json-get params "textDocument" "text")))
                       ((equal (tuatara.text:json-get message "method") "textDocument/didChange")
                        (let ((changes (tuatara.text:json-get params "contentChanges")))
                          (if (every (lambda (c) (tuatara.text:json-get c "range")) changes) (incf ranges) (incf whole))
                          (setf model (and model (lsp-applied model changes encoding))))))))))
      (unwind-protect
           (progn
             (setf (tuatara.defaults::lsp-doc-servers doc) (list server)
                   (tuatara.buffer:buffer-local buffer 'tuatara.defaults::lsp-docs) (list doc))
             (tuatara.defaults::lsp-open-document server doc)
             (take (messages-written server))
             (is string= (expected) model)
             (dotimes (step 40)
               (let ((now (tuatara.buffer:buffer-string buffer)))
                 (case (random 12)
                   ((0 1) (tuatara.buffer:buffer-undo buffer))
                   (2 (tuatara.buffer:buffer-redo buffer))
                   (3 (when (zerop (random 4))
                        (let ((rope (tuatara.text:string->rope (concatenate 'string now "x")))
                              (size (tuatara.buffer:buffer-size buffer)))
                          (tuatara.buffer:buffer-swap-text buffer rope (lambda (p bias) (declare (ignore bias)) p)
                                                           (lambda (p bias) (declare (ignore bias)) (min p size))))))
                   (t (tuatara.buffer:buffer-edit buffer (random-batch now)))))
               (when (or (zerop (random 3)) (= step 39))
                 (tuatara.defaults::lsp-send-text buffer)
                 (take (messages-written server))
                 (let ((expected (expected)))
                   (unless (string= expected model)
                     (is string= expected model "after step ~a, the server counting ~a" step encoding)
                     (return))))))
        (setf (tuatara.buffer:buffer-local buffer 'tuatara.defaults::lsp-docs) nil)
        (tuatara.buffer:kill-buffer buffer)))
    (values ranges whole)))

(define-test (lsp changes-sent-make-the-text)
  ;; a server counting octets or UTF-16 units, given what changed at each pause, has the buffer's
  ;; text: random edits of letters of up to four octets, CR LF and LF, undone and done again
  (let ((ranges 0) (whole 0))
    (dotimes (round 40)
      (let ((*alphabet* (if (< round 20) *note-pieces* (remove #\Return *note-pieces* :test #'find))))
        (multiple-value-bind (r w) (random-sync-session (if (evenp round) :utf-16 :utf-8) (random-text 1000) nil)
          ;; with no carriage returns, a stray one sending all whole from then on
          (when (>= round 20) (incf ranges r) (incf whole w)))))
    (true (> ranges (* 3 whole)) "~a sent as ranges, ~a whole" ranges whole)))

(define-test (lsp block-changes-make-the-document)
  ;; the same for a note's Python blocks: blocks begun, ended, indented and escaped at random, the
  ;; document of their code made by the server from what changed is the one the note has
  (let ((ranges 0) (whole 0)
        (python (tuatara.defaults::org-src-language "python")))
    (dotimes (round 40)
      (let ((*alphabet* (if (< round 20) *note-pieces* (remove #\Return *note-pieces* :test #'find))))
        (multiple-value-bind (r w) (random-sync-session (if (evenp round) :utf-16 :utf-8) (random-text 1000) python)
          (when (>= round 20) (incf ranges r) (incf whole w)))))
    (true (> ranges (* 3 whole)) "~a sent as ranges, ~a whole" ranges whole)))

(defun note-of-blocks (count)
  "An Org note of COUNT headlines, each with prose and a Python block."
  (with-output-to-string (out)
    (dotimes (n count)
      (format out "* Heading ~d~%~%Some prose ~d.~%~%#+begin_src python~%  def f_~d(x):~%      return x + ~d~%#+end_src~%~%" n n n n))))

(define-test (lsp block-changes-cost-little-for-each-block)
  ;; a letter typed in a block of a long note: the pause sends that letter, and what it makes
  ;; grows by little for each block more in the note: some 33 octets, where making the whole
  ;; code again and sending it took some 4,600
  (flet ((pause (count)
           (let* ((text (note-of-blocks count))
                  (buffer (tuatara.buffer:make-buffer (format nil "blocks-~a-~a" count (random 1000000)) :text text))
                  (server (server-taking-changes :utf-16))
                  (python (tuatara.defaults::org-src-language "python"))
                  (doc (tuatara.defaults::make-lsp-doc buffer python "file:///tmp/blocks.org.py"
                                                       (lambda (b &optional doc stretches)
                                                         (tuatara.defaults::org-blocks-shadow b python doc stretches))))
                  (tuatara.defaults::*lsp-doc-spec-functions* '()))
             (unwind-protect
                  (progn
                    (setf (tuatara.defaults::lsp-doc-servers doc) (list server)
                          (tuatara.buffer:buffer-local buffer 'tuatara.defaults::lsp-docs) (list doc))
                    (tuatara.defaults::lsp-open-document server doc)
                    (messages-written server)
                    (loop repeat 3
                          for at = (+ 8 (search "return x" text :start2 (floor (length text) 2))) then (1+ at)
                          do (tuatara.buffer:buffer-insert buffer at "y")
                          minimize (let ((before (sb-ext:get-bytes-consed)))
                                     (tuatara.defaults::lsp-send-text buffer)
                                     (prog1 (- (sb-ext:get-bytes-consed) before)
                                       (let ((sent (messages-written server)))
                                         (is equal "y" (tuatara.text:json-get (aref (tuatara.text:json-get (first sent) "params" "contentChanges") 0)
                                                                              "text")))))))
               (setf (tuatara.buffer:buffer-local buffer 'tuatara.defaults::lsp-docs) nil)
               (tuatara.buffer:kill-buffer buffer)))))
    (let* ((small (pause 2000)) (large (pause 4000))
           (each (/ (- large small) 2000.0)))
      (true (< each 300) "~,1f octets made for each block more" each))))

(define-test (lsp size-of-what-is-sent-bounded)
  ;; *lsp-largest-document* bounds what a server is sent: a long note with a small block is
  ;; served, its blocks larger than it are not, nor a file larger than it
  (with-test-directory (dir)
    (let ((tuatara.defaults::*lsp-largest-document* 4000)
          (tuatara.defaults::*lsp-doc-spec-functions* (list 'tuatara.defaults::org-block-doc-specs))
          (prose (with-output-to-string (out) (loop repeat 150 do (format out "Words of prose, no code at all.~%"))))
          (buffers '()))
      (flet ((visiting (name text mode)
               (let ((buffer (tuatara.buffer:make-buffer name :text text :file (namestring (merge-pathnames name dir)))))
                 (push buffer buffers)
                 (tuatara.defaults::set-major-mode buffer mode)
                 buffer))
             (languages (buffer) (mapcar (lambda (spec) (tuatara.defaults::language-name (first spec)))
                                         (tuatara.defaults::lsp-doc-specs buffer))))
        (unwind-protect
             (let ((small (visiting "small.org" (format nil "~a#+begin_src python~%x = 1~%#+end_src~%" prose) 'tuatara.defaults::org-mode))
                   (large (visiting "large.org" (format nil "#+begin_src python~%~a#+end_src~%" prose) 'tuatara.defaults::org-mode))
                   (file (visiting "large.py" prose 'tuatara.defaults::python-mode)))
               (true (> (tuatara.buffer:buffer-size small) 4000))
               (is equal '(tuatara.defaults::python) (languages small))
               (is equal '() (languages large))
               (is equal '() (languages file))
               (true (search "larger than *lsp-largest-document*" (tuatara.defaults::lsp-unserved-words file '(t)))))
          (mapc #'tuatara.buffer:kill-buffer buffers))))))
