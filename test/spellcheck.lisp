;;;; test/spellcheck.lisp --- Words spelt wrong marked as shown, corrected, kept.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test spellcheck
  :description "The words found to spell, what is checked by the text's colours, the marks in a window once the speller answers, going to and correcting a word, and word lists kept by enchant in the tests' own directory.")

(defun speller-idle ()
  "Wait until every word asked about is answered."
  (loop repeat 500
        until (loop for table being the hash-values of tuatara.defaults::*spelling-answers*
                    never (loop for v being the hash-values of table thereis (eq v :asked)))
        do (sleep 1/100)))

(defun spellcheck-keys (&rest keys)
  "KEYS with spellcheck turned on first, and the speller waited for before
each of them and at the end, the editor woken to redraw."
  (append (list "M-x s p e l l c h e c k - m o d e RET" #'speller-idle :tick)
          (loop for k in keys append (list k #'speller-idle :tick))
          (list #'speller-idle :tick)))

(defun spelt-wrong (frontend)
  "The words FRONTEND's last frame shows in the :spellcheck face."
  (let ((screen (tuatara.frontend.fake:fake-screen frontend))
        (spans (tuatara.frontend.fake:fake-spans frontend))
        (words '()))
    (loop for row across screen
          for row-spans across spans
          do (loop for (start end face) in row-spans
                   when (if (listp face) (member :spellcheck face) (eq face :spellcheck))
                     do (push (subseq row start end) words)))
    (nreverse words)))

(define-test (spellcheck words-found)
  (let ((words (mapcar #'third (tuatara.defaults::spelling-words
                                "Teh fox's den, http://x.org/wrnog a_b me@x.org x86abc don't I e-mail" 0 :text))))
    (is equal '("Teh" "fox's" "den" "don't" "mail") words))
  ;; octets counted: a word after a two-octet letter starts where its octets do
  (is equal '((0 3 "éa") (4 6 "bc")) (mapcar (lambda (w) (subseq w 0 3)) (tuatara.defaults::spelling-words "éa bc" 0 :text)))
  ;; in code, camelCase is a name, not a word to spell
  (is equal '("plain") (mapcar #'third (tuatara.defaults::spelling-words "fooBar plain" 0 :code)))
  (true (tuatara.defaults::face-among-p :markup.raw.block '(:markup.raw)))
  (true (tuatara.defaults::face-among-p '(:bold :org-code) '(:org-code)))
  (false (tuatara.defaults::face-among-p :markup.rawish '(:markup.raw))))

(define-test (spellcheck marked-as-shown)
  (if (not (tuatara.defaults::enchant-p))
      (skip "enchant is not installed" (true nil))
      (let ((tuatara.defaults::*spellcheck-languages* '("en_US")))
        ;; text: the word spelt wrong underlined, the others not
        (let ((f (run-keys (spellcheck-keys) :text (format nil "The cat sat on teh mat.~%") :width 60 :height 6)))
          (is equal '("teh") (spelt-wrong f)))
        ;; Org: code and links left alone
        (let ((f (run-keys (append (list "M-x o r g - m o d e RET") (spellcheck-keys))
                           :text (format nil "* Hedding~%Some ~~wrnogcode~~ and [[https://x.org][a lnik]] wrod.~%")
                           :width 70 :height 6)))
          (is equal '("Hedding" "wrod") (spelt-wrong f)))
        ;; code: only comments and strings
        (let ((f (run-keys (append (list "M-x l i s p - m o d e RET") (spellcheck-keys))
                           :text (format nil "(defun badnaem () \"A strnig.\") ; a comentt~%") :width 70 :height 6)))
          (is equal '("strnig" "comentt") (spelt-wrong f))))))

(define-test (spellcheck by-mode)
  ;; *spellcheck-modes* turns it on by major mode, and says what in code: comments, strings, both, or all
  (if (not (tuatara.defaults::enchant-p))
      (skip "enchant is not installed" (true nil))
      (let ((tuatara.defaults::*spellcheck-languages* '("en_US"))
            (text (format nil "(defun badnaem () \"A strnig.\") ; a comentt~%")))
        (flet ((marked (modes &optional (keys "M-x l i s p - m o d e RET"))
                 (let ((tuatara.defaults::*spellcheck-modes* modes))
                   (spelt-wrong (run-keys (list keys #'speller-idle :tick #'speller-idle :tick) :text text :width 70 :height 6)))))
          (is equal '("strnig" "comentt") (marked '((lisp-mode . t))))
          (is equal '("comentt") (marked '((prog-mode . :comments))))
          (is equal '("strnig") (marked '((lisp-mode . :strings))))
          (is equal '("defun" "badnaem" "strnig" "comentt") (marked '((lisp-mode . :all))))
          ;; off for the mode: nothing, though on for the modes it is made from
          (is equal '() (marked '((prog-mode . t) (lisp-mode . nil))))
          ;; a mode of text takes any value as on
          (is equal '("teh") (let ((text (format nil "Not teh same.~%")))
                               (declare (ignorable text))
                               (let ((tuatara.defaults::*spellcheck-modes* '((text-mode . :comments))))
                                 (spelt-wrong (run-keys (list "M-x t e x t - m o d e RET" #'speller-idle :tick #'speller-idle :tick)
                                                        :text (format nil "Not teh same.~%") :width 70 :height 6)))))))))

(define-test (spellcheck said-when-on)
  ;; turned on, it says what it checks: every word in text, comments and strings in code
  (let ((tuatara.defaults::*spellcheck-languages* '("en_US")))
    (flet ((said (mode words)
             (true (tuatara.frontend.fake:fake-frame-showing
                    (run-keys (list (format nil "M-x ~{~a~^ ~} RET M-x s p e l l c h e c k - m o d e RET" (coerce mode 'list))
                                    #'speller-idle :tick)
                              :text (format nil "Some words.~%") :width 70 :height 6)
                    words))))
      (said "text-mode" "Spellcheck every word")
      (said "lisp-mode" "Spellcheck comments and strings"))))

(define-test (spellcheck going-and-correcting)
  (if (not (tuatara.defaults::enchant-p))
      (skip "enchant is not installed" (true nil))
      (let ((tuatara.defaults::*spellcheck-languages* '("en_US")))
        ;; z n goes to the next word spelt wrong, also past the window; z p back
        (multiple-value-bind (f editor)
            (run-keys (spellcheck-keys "z n")
                      :text (format nil "Good words here.~%~{~a~%~}A mistaek at the end.~%" (loop repeat 30 collect "Fine line."))
                      :width 60 :height 6)
          (declare (ignore f))
          (is = (search "mistaek" (session-text editor)) (session-point editor)))
        (multiple-value-bind (f editor) (run-keys (spellcheck-keys "g e z p") :text (format nil "A wrnog one.~%Right.~%") :width 60 :height 6)
          (declare (ignore f))
          (is = 2 (session-point editor)))
        ;; z = with the first suggestion taken
        (multiple-value-bind (f editor) (run-keys (spellcheck-keys "z =" "RET") :text (format nil "The cat sat on teh mat.~%") :width 60 :height 12)
          (declare (ignore f))
          (is string= (format nil "The cat sat on the mat.~%") (session-text editor)))
        ;; or kept while the editor runs: the mark goes
        (multiple-value-bind (f editor)
            (run-keys (spellcheck-keys "z =" "K e e p RET") :text (format nil "A word: qwzxyk.~%") :width 60 :height 12)
          (is string= (format nil "A word: qwzxyk.~%") (session-text editor))
          (true (tuatara.frontend.fake:fake-frame-showing f "is taken as right while the editor runs"))
          (is equal '() (spelt-wrong f))))))

(define-test (spellcheck word-lists)
  (if (not (tuatara.defaults::enchant-p))
      (skip "enchant is not installed" (true nil))
      (let ((tuatara.defaults::*spellcheck-languages* '("en_US"))
            (directory (uiop:getenv "ENCHANT_CONFIG_DIR")))
        ;; the tests never write to the developer's own lists
        (true (and directory (search "test" directory)))
        ;; z g adds the word at point to the language's list, kept on disk
        (multiple-value-bind (f editor) (run-keys (spellcheck-keys "z g") :text (format nil "zqxwvlist here~%") :width 60 :height 6)
          (declare (ignore editor))
          (true (tuatara.frontend.fake:fake-frame-showing f "Added “zqxwvlist” to the en_US word list"))
          (is equal '() (spelt-wrong f)))
        (true (search "zqxwvlist" (alexandria:read-file-into-string (merge-pathnames "en_US.dic" (uiop:ensure-directory-pathname directory))))))))

(define-test (spellcheck never-safe)
  ;; a file opened safely is never checked, even with spellcheck on everywhere
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "evidence.txt" dir))))
      (alexandria:write-string-into-file (format nil "teh wrnog~%") file)
      (let ((tuatara.defaults::*global-spellcheck-mode* t))
        (multiple-value-bind (f editor) (run-keys (list (format nil "C-u C-x C-f C-a C-k ~a RET" (key-string file)) #'speller-idle :tick)
                                                  :width 60 :height 6)
          (true (tuatara.io:safe-visit-p (session-buffer editor)))
          (is equal '() (spelt-wrong f)))))))

(define-test (spellcheck newest-first-and-bounded)
  (if (not (tuatara.defaults::enchant-p))
      (skip "enchant is not installed" (true nil))
      (let ((languages '("en_US")) (tuatara.defaults::*spelling-backlog* 1))
        ;; the speller busy: of the lots asked meanwhile only the newest waits; the older are dropped,
        ;; to be asked again when shown again
        (speller-idle)
        (clrhash tuatara.defaults::*spelling-answers*)
        (tuatara.defaults::speller-run (lambda () (sleep 3/10)))
        (tuatara.defaults::spelling-ask languages '("olda" "oldb"))
        (tuatara.defaults::spelling-ask languages '("newer"))
        (tuatara.defaults::spelling-ask languages '("newest" "teh"))
        (speller-idle)
        (sleep 1/2)
        (is eq :wrong (tuatara.defaults::spelling-known languages "teh"))
        (false (gethash "olda" (tuatara.defaults::spelling-table languages)))
        (false (gethash "newer" (tuatara.defaults::spelling-table languages)))
        ;; asked again, answered
        (tuatara.defaults::spelling-ask languages '("olda"))
        (sleep 1/2)
        (true (tuatara.defaults::spelling-known languages "olda"))
        ;; the answers kept are bounded: past the limit they are dropped, the words asked again
        (let ((tuatara.defaults::*spelling-answers-limit* 2))
          (tuatara.defaults::spelling-ask languages '("cat" "dog" "cow"))
          (speller-idle)
          (tuatara.defaults::spelling-ask languages '("hen"))
          (true (<= (hash-table-count (tuatara.defaults::spelling-table languages)) 2))))))

(define-test (spellcheck file-word-list)
  (if (not (tuatara.defaults::enchant-p))
      (skip "enchant is not installed" (true nil))
      (let ((tuatara.defaults::*spellcheck-languages* '("en_US")))
        ;; the file's own list, a LocalWords: line, takes its words as right there
        (let ((f (run-keys (append (list "M-x o r g - m o d e RET") (spellcheck-keys))
                           :text (format nil "Qwzxk and Vrbltz met teh team.~%~%# LocalWords: Qwzxk Vrbltz~%")
                           :width 70 :height 6)))
          (is equal '("teh") (spelt-wrong f)))
        ;; z = offers to keep the word in the file: written at its end as a comment of the language
        (multiple-value-bind (f editor)
            (run-keys (append (list "M-x o r g - m o d e RET") (spellcheck-keys "z =" "t h i s SPC f i l e RET"))
                      :text (format nil "Ask Qwzxk.~%") :width 70 :height 12)
          (is string= (format nil "Ask Qwzxk.~%# LocalWords: Qwzxk~%") (session-text editor))
          (true (tuatara.frontend.fake:fake-frame-showing f "Kept “Qwzxk” in this file's word list"))
          (is equal '() (spelt-wrong f)))
        ;; z f adds the word at point to the list there, on its line while it fits; in Lisp after ;;
        (multiple-value-bind (f editor)
            (run-keys (spellcheck-keys "z f")
                      :text (format nil "Vrbltz here.~%LocalWords: Qwzxk~%") :width 70 :height 6)
          (declare (ignore f))
          (is string= (format nil "Vrbltz here.~%LocalWords: Qwzxk Vrbltz~%") (session-text editor)))
        (multiple-value-bind (f editor)
            (run-keys (append (list "M-x l i s p - m o d e RET") (spellcheck-keys "l l l l z f"))
                      :text (format nil "; a Vrbltz~%(x)") :width 70 :height 6)
          (declare (ignore f))
          (is string= (format nil "; a Vrbltz~%(x)~%;; LocalWords: Vrbltz~%") (session-text editor)))
        ;; a word the list has already is not added again
        (multiple-value-bind (f editor)
            (run-keys (spellcheck-keys "z f" "z f")
                      :text (format nil "Vrbltz here.~%") :width 70 :height 6)
          (is string= (format nil "Vrbltz here.~%LocalWords: Vrbltz~%") (session-text editor))
          (true (tuatara.frontend.fake:fake-frame-showing f "Already kept “Vrbltz” in this file's word list")))
        ;; where the language has no comments to hold the list, nothing is written into the data
        (multiple-value-bind (f editor)
            (run-keys (append (list "M-x c s v - m o d e RET") (spellcheck-keys "z f"))
                      :text (format nil "Vrbltz,here~%") :width 120 :height 6)
          (is string= (format nil "Vrbltz,here~%") (session-text editor))
          (true (tuatara.frontend.fake:fake-frame-showing f "has no comments to keep a word list in"))))))

(define-test (spellcheck file-word-list-not-exported)
  ;; the list is a comment: neither export prints it
  (let ((note (format nil "#+title: N~%Ask Qwzxk.~%~%# LocalWords: Qwzxk~%")))
    (false (search "LocalWords" (tuatara.defaults::org-latex-document note :enhanced t)))
    (with-test-directory (dir)
      (false (search "LocalWords" (docx-text (tuatara.defaults::org-docx-document note :file (namestring (merge-pathnames "n.org" dir)))))))))

(define-test (spellcheck indented-view)
  ;; an Org file shown indented: the columns before the text under a headline stand for no
  ;; character, and are left out (an empty line there shows nothing else); the words after them
  ;; are checked, at the end of the file (g e) as at its start
  (if (not (tuatara.defaults::enchant-p))
      (skip "enchant is not installed" (true nil))
      (with-test-directory (dir)
        (let ((file (namestring (merge-pathnames "note.org" dir)))
              (tuatara.defaults::*spellcheck-languages* '("en_US")))
          (alexandria:write-string-into-file (format nil "* Head~%Some teh text.~%~%** Sub~%  More wrnog here.~%~%") file)
          (let ((said-before (tuatara.buffer:buffer-size (tuatara.command::messages-buffer))))
            (multiple-value-bind (f editor) (run-keys (spellcheck-keys "g e" "k" "g g") :file file :width 70 :height 8)
              (declare (ignore editor))
              (true (search "  Some teh" (tuatara.frontend.fake:fake-screen-line f 1)) "shown indented")
              (is equal '("teh" "wrnog") (spelt-wrong f))
              ;; nothing failed to draw in this session, whatever sessions before it said
              (false (search "could not be drawn"
                             (let ((messages (tuatara.command::messages-buffer)))
                               (tuatara.buffer:buffer-string messages :start (min said-before (tuatara.buffer:buffer-size messages))))))))
          ;; the window's first row an empty line under a headline, as after g e in a longer note
          (multiple-value-bind (f editor)
              (run-keys (list (lambda ()
                                (let* ((w (tuatara.command:editor-window tuatara.command:*editor*))
                                       (b (tuatara.display:window-buffer w))
                                       (blank (search (format nil "~%~%") (tuatara.buffer:buffer-string b))))
                                  (unless (tuatara.defaults::spellcheck-p b) (tuatara.defaults::spellcheck-mode))
                                  (setf (tuatara.display:window-start w) (1+ blank)
                                        (tuatara.buffer:buffer-point b) (search "More" (tuatara.buffer:buffer-string b)))))
                              #'speller-idle :tick #'speller-idle :tick)
                        :file file :width 70 :height 8)
            (declare (ignore editor))
            (true (search "    More wrnog" (tuatara.frontend.fake:fake-screen-line f 2)) "the empty line first")
            (is equal '("wrnog") (spelt-wrong f)))))))

(define-test (spellcheck speller-failing)
  ;; a lot of words the speller fails on: taken as right, the next lot answered, spelling still on
  (let ((tuatara.defaults::*spelling-wanted* '())
        (tuatara.defaults::*spelling-draining* t)
        (tuatara.defaults::*spelling-wake-function* nil)
        (table (make-hash-table :test 'equal)))
    (setf (gethash "boom" table) :asked (gethash "fine" table) :asked)
    (sb-int:encapsulate 'tuatara.defaults::spelling-of 'test-failing
                        (lambda (function languages word)
                          (if (string= word "boom") (error "the speller failed") (funcall function languages word))))
    (unwind-protect
         (progn
           (setf tuatara.defaults::*spelling-wanted* (list (list* '() table '("boom")) (list* '() table '("fine"))))
           (tuatara.defaults::spelling-drain)
           (is eq :right (gethash "boom" table))
           (is eq :right (gethash "fine" table))
           (false tuatara.defaults::*spelling-draining*))
      (sb-int:unencapsulate 'tuatara.defaults::spelling-of 'test-failing))))

(define-test (spellcheck words-of-every-script)
  ;; a decomposed accent is part of its word; scripts without spaces give no words to spell
  (flet ((words (text) (mapcar #'third (tuatara.defaults::spelling-words text 0 :prose))))
    (is equal (list (format nil "cafe~c" (code-char #x301)) "noir") (words (format nil "cafe~c noir" (code-char #x301))))
    (is equal '() (words "東京タワー"))
    (is equal '() (words "สวัสดีครับ"))
    (is equal '("hello") (words "hello 世界")))
  ;; a word a row cuts short is spelt whole
  (let ((b (tuatara.buffer:make-buffer "spell-reach" :text "one misspelt two")))
    (unwind-protect
         (multiple-value-bind (start end) (tuatara.defaults::whole-words-segment b 6 9)
           (is = 4 start)
           (is = 12 end))
      (tuatara.buffer:kill-buffer b)))
  ;; a word added to one language's list is right where that language is asked, not in another's
  (let ((tuatara.defaults::*spelling-answers* (make-hash-table :test 'equal :synchronized t))
        (tuatara.defaults::*speller* (cons sb-thread:*current-thread* (sb-concurrency:make-mailbox)))
        (tuatara.defaults::*spelling-wake-function* nil))
    (setf (gethash "kia" (tuatara.defaults::spelling-table '("en_NZ"))) :wrong
          (gethash "kia" (tuatara.defaults::spelling-table '("de_DE"))) :wrong)
    (tuatara.defaults::spelling-add '("en_NZ") "kia" "en_NZ")
    (is eq :right (gethash "kia" (tuatara.defaults::spelling-table '("en_NZ"))))
    (is eq :wrong (gethash "kia" (tuatara.defaults::spelling-table '("de_DE"))))))

(define-test (spellcheck missing-dictionary-said)
  ;; a language with no dictionary here: said once, by the speller, without the editor asking again and again
  (if (not (tuatara.defaults::enchant-p))
      (skip "enchant is not installed" (true nil))
      (let ((tuatara.defaults::*spellcheck-languages* '("xx_NOPE"))
            (tuatara.defaults::*spelling-reports* (make-hash-table :test 'equal)))
        (let ((f (run-keys (append (spellcheck-keys) (list (lambda () (sleep 0.3)) "")) :text (format nil "Some words.~%") :width 100 :height 6)))
          (true (tuatara.frontend.fake:fake-frame-showing f "Spellcheck: no dictionary for xx_NOPE"))))))

(define-test (spellcheck file-word-list-kept-past-edits-above)
  ;; an edit further up leaves the file's own word list as read, nothing of its end copied;
  ;; an edit to the list is read
  (let ((b (tuatara.buffer:make-buffer "*local words*"
                                       :text (format nil "~a~%LocalWords: Qwzxk~%" (make-string 200000 :initial-element #\a)))))
    (unwind-protect
         (flet ((consed-after-edit (reach)
                  (let ((tuatara.defaults::*local-words-reach* reach))
                    (tuatara.defaults::local-words b)
                    (loop repeat 3
                          minimize (progn (tuatara.buffer:buffer-insert b 0 "b")
                                          (nth-value 2 (tuatara.bench::measuring (tuatara.defaults::local-words b))))))))
           (true (gethash "Qwzxk" (tuatara.defaults::local-words b)))
           (let ((per-octet (/ (- (consed-after-edit 65536) (consed-after-edit 16384)) 49152.0)))
             (true (< per-octet 0.05) "~,3f octets consed for each octet of the end looked at" per-octet))
           (true (gethash "Qwzxk" (tuatara.defaults::local-words b)))
           (tuatara.buffer:buffer-insert b (1- (tuatara.buffer:buffer-size b)) " Vrbltz")
           (true (gethash "Vrbltz" (tuatara.defaults::local-words b)))
           (tuatara.buffer:buffer-insert b 0 "c")
           (true (gethash "Vrbltz" (tuatara.defaults::local-words b))))
      (tuatara.buffer:kill-buffer b))))

(define-test (spellcheck shown-rows-read-after-waiting)
  ;; the window's rows are read again once the speller has answered: a redraw meanwhile
  ;; fills the rows it showed before with other text
  (multiple-value-bind (f editor) (run-keys "" :text (format nil "Some words.~%") :width 60 :height 6)
    (declare (ignore f))
    (let* ((tuatara.command:*editor* editor)
           (window (tuatara.command:editor-window editor))
           (redrawn (vector))
           (read '()))
      (sb-int:encapsulate 'tuatara.defaults::spelling-in-rows 'rows-read
                          (lambda (function buffer rows syntax)
                            (declare (ignore function buffer syntax))
                            (push rows read)
                            (values '() (if (rest read) '() (list (list 0 4 "Some"))) '("en_US"))))
      (sb-int:encapsulate 'tuatara.defaults::spelling-ask 'rows-read (lambda (function &rest arguments)
                                                                        (declare (ignore function arguments))))
      (sb-int:encapsulate 'tuatara.command:wait-until-interruptibly 'rows-read
                          (lambda (function &rest arguments)
                            (declare (ignore function arguments))
                            (setf (tuatara.display:window-rows window) redrawn)
                            t))
      (unwind-protect (tuatara.defaults::spelling-answered-in window)
        (sb-int:unencapsulate 'tuatara.defaults::spelling-in-rows 'rows-read)
        (sb-int:unencapsulate 'tuatara.defaults::spelling-ask 'rows-read)
        (sb-int:unencapsulate 'tuatara.command:wait-until-interruptibly 'rows-read))
      (is = 2 (length read))
      (is eq redrawn (first read)))))

(define-test (spellcheck shown-words-kept-while-unchanged)
  ;; the cursor moving over text that stays as it was finds its words spelt wrong once, not
  ;; at every redraw; a change to the text finds them again
  (let ((tuatara.defaults::*spellcheck-languages* '("xx_KEPT"))
        (tuatara.defaults::*modal-editing* nil)
        (table (tuatara.defaults::spelling-table '("xx_KEPT")))
        (calls 0) (moving nil))
    (dolist (word '("Some" "words" "here" "more")) (setf (gethash word table) :right))
    (setf (gethash "wrnog" table) :wrong)
    (sb-int:encapsulate 'tuatara.defaults::spelling-words 'counted
                        (lambda (function &rest arguments) (incf calls) (apply function arguments)))
    (unwind-protect
         (let ((f (run-keys (list "M-x s p e l l c h e c k - m o d e RET" :tick (lambda () (setf calls 0))
                                  "C-f" :tick "C-f" :tick "C-f" :tick "C-b" :tick
                                  (lambda () (setf moving calls calls 0))
                                  "M-> m o r e" :tick)
                            :text (format nil "Some wrnog words here.~%") :width 60 :height 6)))
           (is equal '("wrnog") (spelt-wrong f))
           (true (<= moving 1) "words read ~d times for four moves" moving)
           (true (plusp calls)))
      (sb-int:unencapsulate 'tuatara.defaults::spelling-words 'counted)
      (remhash "xx_KEPT" tuatara.defaults::*spelling-answers*))))
