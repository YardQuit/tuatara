;;;; test/incremental-search.lisp --- Incremental search and replace through the fake frontend.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test incremental-search
  :description "Incremental search: regex by default, literal with M-r; typing, repeating, wrapping, DEL, exit and abort, highlights; replacing from the prompt with pattern/replacement/flags; query-replace and replace-regexp.")

(define-test (incremental-search incremental-basics)
  (multiple-value-bind (f editor) (run-keys "C-s w o r RET" :text "hello world")
    (declare (ignore f))
    (is = 9 (session-point editor))
    (is = 0 (tuatara.buffer:buffer-mark (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
  ;; repeating moves through the matches
  (multiple-value-bind (f editor) (run-keys "C-s o C-s C-s RET" :text "foo boo")
    (declare (ignore f))
    (is = 6 (session-point editor)))
  ;; a failing search keeps point at the last match; C-s again wraps around
  (multiple-value-bind (f editor) (run-keys "g e C-s o C-s RET" :text "foo boo")
    (declare (ignore f))
    (is = 2 (session-point editor)))
  ;; no match at all: the count says so, 0/0, in place of Failing
  (multiple-value-bind (f editor) (run-keys (list "C-s x y z" #'wait-for-search-count :tick "C-g") :text "hello" :width 80)
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "0/0    Regex search forward: xyz"))
    (false (tuatara.frontend.fake:fake-frame-showing f "Failing")))
  ;; RET on a search that found nothing says so, and how to go on from the start
  (multiple-value-bind (f editor) (run-keys "g e C-s h e RET" :text "hello")
    (is = 5 (session-point editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "No match for he after point: C-s in the search goes on from the start")))
  ;; a pattern that cannot be searched for says why, and RET waits for it to be mended
  (multiple-value-bind (f editor) (run-keys "C-s ( RET DEL l RET" :text "hello" :width 160)
    (true (tuatara.frontend.fake:fake-frame-showing f "Regex search forward [incomplete; finish it, or C-g gives up]: ("))
    (is = 3 (session-point editor)))
  ;; C-g restores point; DEL backs up
  (multiple-value-bind (f editor) (run-keys "l l C-s o C-g" :text "foo boo")
    (declare (ignore f))
    (is = 2 (session-point editor)))
  (multiple-value-bind (f editor) (run-keys "C-s w o x DEL RET" :text "hello world")
    (declare (ignore f))
    (is = 8 (session-point editor)))
  ;; backward search
  (multiple-value-bind (f editor) (run-keys "g e C-r o RET" :text "foo boo")
    (declare (ignore f))
    (is = 6 (session-point editor)))
  ;; keys edit the pattern in the minibuffer: C-a goes to its start, the search follows
  (multiple-value-bind (f editor) (run-keys "C-s o C-a f RET" :text "foo boo")
    (declare (ignore f))
    (is = 2 (session-point editor)))
  ;; NORMAL state: C-s is the one key that searches; / wraps what is selected, as = and * do
  (multiple-value-bind (f editor) (run-keys "m w /" :text "foo boo")
    (declare (ignore f))
    (is string= "/foo/ boo" (session-text editor)))
  ;; highlights: the current match reversed, the others bold
  (multiple-value-bind (f editor) (run-keys "C-s o" :text "foo boo" :width 80)
    (declare (ignore editor))
    (true (some (lambda (screen) (declare (ignore screen)) t) (tuatara.frontend.fake:fake-history f)))
    (let ((spans (loop for frame in (tuatara.frontend.fake:fake-history f)
                       thereis (and (find-if (lambda (row) (search "Regex search forward: o" row)) frame)
                                    frame))))
      (true spans))))

(define-test (incremental-search going-on-after-ret)
  (flet ((point-after (keys)
           (multiple-value-bind (f editor) (run-keys keys :text "xb yb zb")
             (declare (ignore f))
             (session-point editor))))
    ;; after RET at a match, C-s and C-r go on through the matches without the prompt
    (is = 2 (point-after "C-s b RET"))
    (is = 5 (point-after "C-s b RET C-s"))
    (is = 8 (point-after "C-s b RET C-s C-s"))
    ;; past the last, from the start, saying so
    (is = 2 (point-after "C-s b RET C-s C-s C-s"))
    (multiple-value-bind (f editor) (run-keys "C-s b RET C-s C-s C-s" :text "xb yb zb")
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "Wrapped to the start")))
    ;; C-r goes back, to the start of the match before
    (is = 1 (point-after "C-s b RET C-s C-r"))
    ;; any other key ends that: C-s begins a new search (left, it goes back to where it began)
    (is = 3 (point-after "C-s b RET l C-s"))
    ;; nor after C-g, or a search that found nothing
    (is = 0 (point-after "C-s b C-g C-s"))
    (is = 0 (point-after "C-s q RET C-s")))
  ;; its matches stay lit after RET, the one C-s and C-r are on as the current one,
  ;; and after other keys too, till C-g
  (flet ((faces (keys)
           (let ((f (run-keys keys :text "xb yb zb")))
             (loop for (text face) in (org-row-faces f 0)
                   when (member (if (listp face) (first face) face) '(:search-current :search-other))
                     collect (list text (if (listp face) (first face) face))))))
    (is equal '(("b" :search-current) ("b" :search-other) ("b" :search-other)) (faces "C-s b RET"))
    (is equal '(("b" :search-other) ("b" :search-current) ("b" :search-other)) (faces "C-s b RET C-s"))
    (is equal '(("b" :search-other) ("b" :search-other) ("b" :search-other)) (faces "C-s b RET l l"))
    (is equal '() (faces "C-s b RET l C-g"))
    ;; a search that found nothing lights nothing
    (is equal '() (faces "C-s q RET"))
    ;; C-x T s lights them again after C-g, and takes them away when lit
    (is equal '(("b" :search-other) ("b" :search-other) ("b" :search-other)) (faces "C-s b RET l C-g C-x T s"))
    (is equal '() (faces "C-s b RET l C-x T s")))
  (let ((tuatara.defaults::*search-lit* nil))
    (true (tuatara.frontend.fake:fake-frame-showing (run-keys "C-x T s" :text "xb") "No search to light the matches of")))
  ;; lit in a window of another buffer, the matches are found in that buffer's text, however short the current one
  (let ((f (run-keys "C-s z z RET C-x 2 C-x b o t h e r RET" :text "aaaa zz bbbb zz" :height 16)))
    (true (loop for row below 14 thereis (org-faces-include f row "zz" :search-other))))
  ;; a search's buffer killed: it is forgotten, its text let go
  (run-keys "C-s b RET C-x k RET" :text "xb yb")
  (false tuatara.defaults::*search-lit*)
  ;; C-r leaves the cursor on the match's first character, as b does: on a match of one
  ;; character, which it would hide, the cursor takes the match's ground; on a longer one it
  ;; stays as it is, the rest of the match showing; C-s leaves it after the match
  (flet ((cursor-colour (keys &optional (text "xb yb zb"))
           (tuatara.frontend.fake:fake-cursor-colour (run-keys keys :text text))))
    (let ((match (tuatara.display:face-ground :search-current)))
      (true match)
      (is equal match (cursor-colour "C-s b RET C-r"))
      (is equal match (cursor-colour "g e C-r b RET"))
      (is equal match (cursor-colour "g e C-r é RET" "xé yé"))
      (false (equal match (cursor-colour "g e C-r y b RET")))
      (false (equal match (cursor-colour "C-s b RET C-s")))
      (false (equal match (cursor-colour "C-s b RET C-r C-g"))))))

(define-test (incremental-search highlight-spans)
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width 80 :height 8)))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*"))) (when scratch (tuatara.buffer:kill-buffer scratch)))
    (tuatara.buffer:make-buffer "*scratch*" :text "foo boo")
    (setf tuatara.defaults::*command-history* '())
    (tuatara.frontend.fake:fake-queue-keys f "C-s o o")
    (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults :buffer (tuatara.buffer:find-buffer "*scratch*"))
    ;; the frame painted while "oo" was the search string: match 1-3 current, 5-7 lazy
    (let ((frame (find-if (lambda (fr) (find-if (lambda (row) (search "Regex search forward: oo" row)) fr))
                          (tuatara.frontend.fake:fake-history f))))
      (true frame)
      ;; spans are recorded per frame only for the last frame; re-run the layout check directly
      (let* ((editor (tuatara.command:make-editor f)))
        (declare (ignore editor))
        t))))

(define-test (incremental-search incremental-regex)
  ;; the prompt names the direction both ways
  (multiple-value-bind (f editor) (run-keys "g e C-r o" :text "foo boo" :width 80)
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "Regex search backward: o")))
  ;; a regular expression; point goes to the end of the match
  (multiple-value-bind (f editor) (run-keys "C-s [ 0 - 9 ] + RET" :text "abc 123 def")
    (declare (ignore f))
    (is = 7 (session-point editor)))
  ;; . matches any character; M-r makes it literal, as does literal-search-forward
  (multiple-value-bind (f editor) (run-keys "C-s . RET" :text "a.b")
    (declare (ignore f))
    (is = 1 (session-point editor)))
  (multiple-value-bind (f editor) (run-keys "C-s . M-r RET" :text "a.b" :width 80)
    (true (tuatara.frontend.fake:fake-frame-showing f "Literal search forward: ."))
    (is = 2 (session-point editor)))
  (multiple-value-bind (f editor) (run-keys "C-M-s . RET" :text "a.b")
    (declare (ignore f))
    (is = 2 (session-point editor)))
  ;; backward: the last match before point, at its start
  (multiple-value-bind (f editor) (run-keys "g e C-r [ a - z ] + RET" :text "foo 12 bar")
    (declare (ignore f))
    (is = 7 (session-point editor)))
  ;; ^ matches at line starts only
  (multiple-value-bind (f editor) (run-keys "C-s ^ b RET" :text (format nil "ab~%bc"))
    (declare (ignore f))
    (is = 4 (session-point editor)))
  ;; smart case: \W is an escape, not an upper-case letter; an upper-case letter matches exactly
  (multiple-value-bind (f editor) (run-keys "C-s \\ W b RET" :text "xa Bc")
    (declare (ignore f))
    (is = 4 (session-point editor)))
  (multiple-value-bind (f editor) (run-keys "C-s B RET" :text "b B")
    (declare (ignore f))
    (is = 3 (session-point editor)))
  ;; a pattern that does not compile yet keeps the last match and says so; RET waits, C-g goes back
  (multiple-value-bind (f editor) (run-keys "C-s b ( RET C-g" :text "abc(" :width 160)
    (true (tuatara.frontend.fake:fake-frame-showing f "Regex search forward [incomplete]: b("))
    (true (tuatara.frontend.fake:fake-frame-showing f "Regex search forward [incomplete; finish it, or C-g gives up]: b("))
    (is = 0 (session-point editor))))

(define-test (incremental-search substitute-parse)
  ;; pattern/replacement/flags; a leading % for the whole buffer
  (let ((line (tuatara.defaults::parse-search-line "%a\\/b/c/gi" t)))
    (true (tuatara.defaults::search-line-whole-buffer line))
    (is string= "a\\/b" (tuatara.defaults::search-line-pattern line))
    (is string= "c" (tuatara.defaults::search-line-replacement line))
    (is string= "gi" (tuatara.defaults::search-line-flags line)))
  ;; a slash in a bracket expression belongs to the pattern; in literal text \/ is a slash
  (is string= "[/,]" (tuatara.defaults::search-line-pattern (tuatara.defaults::parse-search-line "[/,]/x" t)))
  (is string= "a/b" (tuatara.defaults::search-line-pattern (tuatara.defaults::parse-search-line "a\\/b" nil)))
  (false (tuatara.defaults::search-line-replacement (tuatara.defaults::parse-search-line "plain" t)))
  (is string= "unknown flag z" (tuatara.defaults::search-line-problem (tuatara.defaults::parse-search-line "a/b/z" t)))
  ;; in a replacement: & \1 \u \U \E \n, and escaped characters for themselves
  (is string= "[ab]" (tuatara.defaults::expand-replacement "[&]" "ab" nil))
  (is string= "B-a" (tuatara.defaults::expand-replacement "\\u\\2-\\1" "ab" #("a" "b")))
  (is string= "AB c" (tuatara.defaults::expand-replacement "\\U&\\E c" "ab" nil))
  (is string= (format nil "a~%&/") (tuatara.defaults::expand-replacement "a\\n\\&\\/" "x" nil))
  (is string= (format nil "&~%/") (tuatara.defaults::literal-replacement "&\\n\\/")))

(defun wait-for-search-count ()
  "Wait, on the editor's thread, until the open search has taken in its count
of matches made in the background."
  (loop repeat 500
        until (let ((state tuatara.defaults::*search*))
                (or (null state)
                    (and (not (tuatara.defaults::update-search-count state))
                         (plusp (length (tuatara.defaults::search-count-words state))))))
        do (sleep 0.01)))

(define-test (incremental-search substitute)
  ;; r r with nothing selected: the line at point, its first match; % every match in the buffer
  (multiple-value-bind (f editor) (run-keys "r r o / 0 RET" :text (format nil "foo foo~%foo") :width 80)
    (true (tuatara.frontend.fake:fake-frame-showing f "Regex replace on this line: o/0"))
    (is string= (format nil "f0o foo~%foo") (session-text editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "Replaced 1 occurrence on 1 line"))
    (is = 0 (session-point editor)))
  (multiple-value-bind (f editor) (run-keys "r r % o / 0 RET" :text (format nil "foo foo~%foo") :width 80)
    (true (tuatara.frontend.fake:fake-frame-showing f "Regex replace in the buffer: %o/0"))
    (true (tuatara.frontend.fake:fake-frame-showing f "Replaced 6 occurrences on 2 lines"))
    (is string= (format nil "f00 f00~%f00") (session-text editor)))
  ;; a selection: the first match on each of its lines; % every match in it
  (multiple-value-bind (f editor) (run-keys "% r r o / 0 RET" :text (format nil "foo foo~%foo") :width 80)
    (true (tuatara.frontend.fake:fake-frame-showing f "Regex replace in the selection: o/0"))
    (is string= (format nil "f0o foo~%f0o") (session-text editor)))
  (multiple-value-bind (f editor) (run-keys "V r r % o / 0 RET" :text (format nil "foo foo~%foo"))
    (declare (ignore f))
    (is string= (format nil "f00 f00~%foo") (session-text editor)))
  (multiple-value-bind (f editor) (run-keys "C-SPC l l l l l r r % a / x RET" :text "aaa bbb aaa" :width 80)
    (true (tuatara.frontend.fake:fake-frame-showing f "Regex replace in the selection: %a/x"))
    (is string= "xxx bbb aaa" (session-text editor)))
  ;; before the prompt, how many matches the replacement replaces: the first on each line, or with % every one
  (dolist (case '(("r r o" "1      Regex replace on this line: o")
                  ("r r % o" "6      Regex replace in the buffer: %o")
                  ("% r r o" "2      Regex replace in the selection: o")))
    (multiple-value-bind (f editor) (run-keys (list (first case) #'wait-for-search-count :tick "C-g") :text (format nil "foo foo~%foo") :width 80)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f (second case)) "~a" (first case))))
  ;; groups and case in the replacement
  (multiple-value-bind (f editor) (run-keys "% r r ( \\ w + ) @ ( \\ w + ) / \\ 2 @ \\ u \\ 1 RET" :text "ann@example")
    (declare (ignore f))
    (is string= "example@Ann" (session-text editor)))
  ;; r q asks at each: y n y q
  (multiple-value-bind (f editor) (run-keys "r q % o / 0 RET y n y q" :text "foo boo")
    (declare (ignore f))
    (is string= "f0o b0o" (session-text editor)))
  ;; an empty pattern, as in /bar, reuses the last search's
  (multiple-value-bind (f editor) (run-keys "C-s f o o RET g g r r / b a r RET" :text "x foo foo")
    (declare (ignore f))
    (is string= "x bar foo" (session-text editor)))
  ;; \/ and a bracket expression put a slash in the pattern
  (multiple-value-bind (f editor) (run-keys "r r % \\ / / - RET" :text "a/b/c")
    (declare (ignore f))
    (is string= "a-b-c" (session-text editor)))
  (multiple-value-bind (f editor) (run-keys "r r % [ / , ] / ; RET" :text "a/b,c")
    (declare (ignore f))
    (is string= "a;b;c" (session-text editor)))
  ;; literal text: & is itself
  (multiple-value-bind (f editor) (run-keys "M-% . RET & RET !" :text "a.b.c")
    (declare (ignore f))
    (is string= "a&b&c" (session-text editor)))
  ;; flags are refused, RET waits; C-g leaves the text alone
  (multiple-value-bind (f editor) (run-keys "r r o / 0 / g RET C-g" :text "foo" :width 200)
    (true (tuatara.frontend.fake:fake-frame-showing f "[no flags here: a / in the pattern or the replacement is written \\/, and % in front replaces every match]"))
    (is string= "foo" (session-text editor)))
  ;; nothing to replace
  (multiple-value-bind (f editor) (run-keys "r r x / y RET" :text "foo")
    (true (tuatara.frontend.fake:fake-frame-showing f "No match on this line"))
    (is string= "foo" (session-text editor)))
  ;; one undo takes back every replacement
  (multiple-value-bind (f editor) (run-keys "r r % o / 0 RET u" :text (format nil "foo~%boo"))
    (declare (ignore f))
    (is string= (format nil "foo~%boo") (session-text editor)))
  ;; a search is all pattern: a slash and a % are themselves
  (multiple-value-bind (f editor) (run-keys "C-s a / b RET" :text "x a/b")
    (declare (ignore f))
    (is = 5 (session-point editor)))
  (multiple-value-bind (f editor) (run-keys "C-s % 2 RET" :text "x 1%2")
    (declare (ignore f))
    (is = 5 (session-point editor))))

(define-test (incremental-search gives-way)
  ;; a search past the serial window stops when a key is waiting and shows it is searching
  (let ((tuatara.syntax:*serial-window* 100) (tuatara.syntax:*parallel-piece* 100)
        (tuatara.defaults::*search-notice-delay* 0)
        (text (concatenate 'string (make-string 1000 :initial-element #\a) "zz")))
    (let ((f (tuatara.frontend.fake:make-fake-frontend :width 60 :height 8)))
      (let ((scratch (tuatara.buffer:find-buffer "*scratch*"))) (when scratch (tuatara.buffer:kill-buffer scratch)))
      (tuatara.buffer:make-buffer "*scratch*" :text text)
      (setf (tuatara.frontend.fake:fake-pending f) t)
      (tuatara.frontend.fake:fake-queue-keys f "C-s z z RET")
      (let ((editor (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                                  :buffer (tuatara.buffer:find-buffer "*scratch*"))))
        ;; its count's place says it: no [searching] after the prompt
        (false (tuatara.frontend.fake:fake-frame-showing f "[searching]"))
        ;; with keys always waiting the search never finishes; the session ends where it began
        (is = 0 (session-point editor))))
    ;; with no key waiting the same search finishes
    (multiple-value-bind (f editor) (run-keys "C-s z z RET" :text text)
      (declare (ignore f))
      (is = 1002 (session-point editor)))
    ;; keys typed ahead: each search gives way while keys wait, DEL goes back to
    ;; a search that was cut short, and RET finishes the last one
    (let ((f (tuatara.frontend.fake:make-fake-frontend :width 60 :height 8)))
      (let ((scratch (tuatara.buffer:find-buffer "*scratch*"))) (when scratch (tuatara.buffer:kill-buffer scratch)))
      (tuatara.buffer:make-buffer "*scratch*" :text text)
      (setf (tuatara.frontend.fake:fake-pending f) (lambda (f) (tuatara.frontend.fake::fake-events f)))
      (tuatara.frontend.fake:fake-queue-keys f "C-s z z DEL RET")
      (let ((editor (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                                  :buffer (tuatara.buffer:find-buffer "*scratch*"))))
        (is = 1001 (session-point editor))))))

(defun span-has-face-p (spans face)
  "True when one of SPANS, (START END FACE-OR-FACES) lists, carries FACE."
  (some (lambda (span) (let ((f (third span))) (or (eq f face) (and (listp f) (member face f))))) spans))

(define-test (incremental-search scope)
  ;; a region limits a search to itself, from its start; the prompt says so
  (multiple-value-bind (f editor) (run-keys "C-SPC l l l l l C-s a" :text "abc abc abc" :width 80)
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "Regex search forward in the region: a")))
  (multiple-value-bind (f editor) (run-keys "C-SPC l l l l l C-s a C-s RET" :text "abc abc abc")
    (declare (ignore f))
    (is = 5 (session-point editor)))
  ;; the region's next match would lie outside it: the search fails instead of leaving it
  (multiple-value-bind (f editor) (run-keys (list "C-SPC l l l l l C-s a C-s C-s" #'wait-for-search-count :tick "C-g")
                                            :text "abc abc abc" :width 80)
    (declare (ignore editor))
    ;; past the last of the region's two
    (true (tuatara.frontend.fake:fake-frame-showing f "0/2    Regex search forward in the region: a")))
  ;; an empty region, right after the mark is set, limits nothing: the search extends it
  (multiple-value-bind (f editor) (run-keys "C-SPC C-s c RET" :text "abc abc")
    (declare (ignore f))
    (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
      (is = 3 (session-point editor))
      (is = 0 (tuatara.buffer:buffer-mark b))
      (true (tuatara.buffer:buffer-mark-active-p b))))
  (multiple-value-bind (f editor) (run-keys "C-SPC M-% b" :text "abc abc" :width 100)
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "Literal replace in the buffer, asking at each: b")))
  ;; C-g gives the region back as it was
  (multiple-value-bind (f editor) (run-keys "C-SPC l l C-s b C-g" :text "abc abc abc")
    (declare (ignore f))
    (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
      (is = 2 (session-point editor))
      (true (tuatara.buffer:buffer-mark-active-p b))))
  ;; what lies outside a line's scope is faded
  (multiple-value-bind (f editor) (run-keys "r r o / 0" :text (format nil "foo~%boo") :width 80)
    (declare (ignore editor))
    (multiple-value-bind (screen spans) (tuatara.frontend.fake:fake-frame-with f "Regex replace on this line: o/0")
      (true screen)
      (false (span-has-face-p (aref spans 0) :search-outside-scope))
      (true (span-has-face-p (aref spans 1) :search-outside-scope)))))

(define-test (incremental-search replacing-keys)
  ;; M-% opens the prompt for replacing: RET after the text goes on to the replacement
  (multiple-value-bind (f editor) (run-keys "M-% f o o" :text "foo x foo" :width 100)
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "Literal replace in the buffer, asking at each: foo")))
  (multiple-value-bind (f editor) (run-keys "M-% f o o RET b a r RET !" :text "foo x foo")
    (declare (ignore f))
    (is string= "bar x bar" (session-text editor)))
  ;; M-% is literal: . is a dot
  (multiple-value-bind (f editor) (run-keys "M-% . RET ! RET !" :text "a.b.c")
    (declare (ignore f))
    (is string= "a!b!c" (session-text editor)))
  ;; r r with % in front replaces every match of a regular expression without asking
  (multiple-value-bind (f editor) (run-keys "r r % ( \\ w + ) RET < \\ 1 > RET" :text "ab cd")
    (declare (ignore f))
    (is string= "<ab> <cd>" (session-text editor)))
  ;; empty matches: ^ and $ each line's start and end, the last line's too, never met twice
  (multiple-value-bind (f editor) (run-keys "r r % ^ RET # SPC RET" :text (format nil "a~%b~%c"))
    (declare (ignore f))
    (is string= (format nil "# a~%# b~%# c") (session-text editor)))
  (multiple-value-bind (f editor) (run-keys "r r % $ RET ; RET" :text (format nil "a~%b"))
    (declare (ignore f))
    (is string= (format nil "a;~%b;") (session-text editor)))
  (multiple-value-bind (f editor) (run-keys "C-M-% % $ RET ; RET y y y" :text (format nil "a~%b"))
    (declare (ignore f))
    (is string= (format nil "a;~%b;") (session-text editor)))
  (multiple-value-bind (f editor) (run-keys "C-M-% % ^ RET > RET n y" :text (format nil "a~%b"))
    (declare (ignore f))
    (is string= (format nil "a~%>b") (session-text editor)))
  ;; with a region active, only within it
  (multiple-value-bind (f editor) (run-keys "C-SPC l l l M-% o RET 0 RET !" :text "foo foo")
    (declare (ignore f))
    (is string= "f00 foo" (session-text editor))))

(define-test (incremental-search letter-case)
  ;; a pattern without capitals: each replacement takes the case of what it replaces
  (multiple-value-bind (f editor) (run-keys "r r % f o o / b a r RET" :text "foo Foo FOO")
    (declare (ignore f))
    (is string= "bar Bar BAR" (session-text editor)))
  ;; a capital in the pattern: matched exactly, replaced as typed
  (multiple-value-bind (f editor) (run-keys "r r % F o o / b a r RET" :text "foo Foo FOO")
    (declare (ignore f))
    (is string= "foo bar FOO" (session-text editor)))
  ;; letters past ASCII are matched in either case too, as plain text (M-r) as
  ;; well as in a regular expression
  (is string= "x X X" (session-text (nth-value 1 (run-keys (format nil "r r ~a M-r RET" (key-string "%élan/x"))
                                                            :text "élan ÉLAN Élan"))))
  (is string= "x X X" (session-text (nth-value 1 (run-keys (format nil "r r ~a RET" (key-string "%élan/x"))
                                                            :text "élan ÉLAN Élan"))))
  (is string= "x ÉLAN Élan" (session-text (nth-value 1 (run-keys (format nil "r r ~a M-r RET" (key-string "%élaN/x"))
                                                                  :text "élaN ÉLAN Élan"))))
  (is string= "Bar" (tuatara.defaults::follow-case "bar" "Foo"))
  (is string= "BAR" (tuatara.defaults::follow-case "bar" "FOO"))
  (is string= "bar" (tuatara.defaults::follow-case "bar" "foo"))
  (is string= "New Name" (tuatara.defaults::follow-case "new name" "Old Word"))
  (is string= "Bar" (tuatara.defaults::follow-case "bar" "A"))
  (is string= "bar" (tuatara.defaults::follow-case "bar" "fOO"))
  (is string= "TEST-NEW" (tuatara.defaults::follow-case "test-new" "TEST"))
  ;; a replacement that says its case (\u \U \l \L) is written so, not in the case of what it replaces
  (is string= "Kia Ora Koutou" (session-text (nth-value 1 (run-keys (format nil "r r ~a RET" (key-string "%\\b(\\w)(\\w*)/\\u\\1\\L\\2"))
                                                                      :text "kia ORA koutou" :width 100))))
  ;; with no escape the case is followed as before
  (is string= "BAR" (session-text (nth-value 1 (run-keys (format nil "r r ~a RET" (key-string "%foo/bar")) :text "FOO" :width 100))))
  (true (tuatara.defaults::replacement-sets-case-p "\\U&"))
  (false (tuatara.defaults::replacement-sets-case-p "\\\\U&")))

(define-test (incremental-search octets-and-codes)
  ;; \xNN from 80 to FF is the octet itself, a raw byte, not the character of
  ;; that code, which \x{NN} gives; below 80 the two are the same
  (let ((text (format nil "é~ax" (code-char (+ #xDC00 #xE9)))))
    (flet ((after (typed &optional (text text))
             (session-text (nth-value 1 (run-keys (format nil "r r ~a RET" (key-string typed)) :text text)))))
      (is string= "éRx" (after "%\\xE9/R"))
      (is string= (format nil "C~ax" (code-char (+ #xDC00 #xE9))) (after "%\\x{E9}/C"))
      (is string= "éOx" (after "%[\\x80-\\xFF]/O"))
      (is string= "a-b" (after "%\\x20/-" "a b"))))
  (is string= "a\\.b" (tuatara.syntax::engine-pattern "a\\x{2E}b"))
  (is string= "\\\\xE9" (tuatara.syntax::engine-pattern "\\\\xE9")))

(define-test (incremental-search thorough)
  ;; blocks of 12 KB, each after 6 KB of other lines, matched as wholes: typed,
  ;; C-s finds the first, its search starting at the match as the pattern grows;
  ;; repeated from there it misses the second, which needs text more than 4 KB
  ;; past where it looks, and so does replacing from the start; the thorough
  ;; twins find them whole, and say so in the prompt
  (let* ((filler (with-output-to-string (o) (dotimes (i 3000) (format o "y~%"))))
         (block (concatenate 'string "BEGIN" (string #\Newline)
                             (with-output-to-string (o) (dotimes (i 6000) (format o "x~%")))
                             "END" (string #\Newline)))
         (text (concatenate 'string filler block filler block))
         (first-end (+ (length filler) (length block) -1))
         (second-start (+ (length filler) (length block) (length filler)))
         (pattern (key-string "BEGIN[\\s\\S]*?END")))
    (flet ((after (keys) (run-keys keys :text text :width 80 :height 10)))
      (is = first-end (session-point (nth-value 1 (after (format nil "C-s ~a C-s RET" pattern)))))
      (multiple-value-bind (f editor)
          (after (format nil "M-x ~a RET ~a C-s RET" (key-string "regex-search-forward-thorough") pattern))
        (true (tuatara.frontend.fake:fake-frame-showing f "Thorough regex search forward: "))
        (is = (1- (length text)) (session-point editor)))
      (is = second-start (session-point (nth-value 1 (after (format nil "g e C-r ~a C-r RET" pattern)))))
      (is = (length filler)
          (session-point (nth-value 1 (after (format nil "g e M-x ~a RET ~a C-r RET" (key-string "regex-search-backward-thorough") pattern)))))
      (is string= text (session-text (nth-value 1 (after (format nil "r r % ~a / Z RET" pattern)))))
      (is string= (format nil "~aZ~%~aZ~%" filler filler)
          (session-text (nth-value 1 (after (format nil "M-x ~a RET % ~a / Z RET" (key-string "replace-regexp-thorough") pattern)))))))
  ;; a pattern kept within a line is found the same either way
  (is = 3 (session-point (nth-value 1 (run-keys (format nil "M-x ~a RET f o o RET" (key-string "regex-search-forward-thorough"))
                                                :text "foo bar"))))
  (false tuatara.syntax:*regex-thorough*))

(define-test (incremental-search symbol-at-point)
  ;; M-. puts the symbol at point into the search, whole words only
  (multiple-value-bind (f editor) (run-keys "C-s M-. C-s RET" :text "alpha beta alphabet alpha" :width 80)
    (true (tuatara.frontend.fake:fake-frame-showing f "Regex search forward: (?<![\\p{L}0-9_-])alpha(?![\\p{L}0-9_-])"))
    (is = 25 (session-point editor)))
  (multiple-value-bind (f editor) (run-keys "r r % M-. / o m e g a RET" :text "alpha beta alphabet alpha")
    (declare (ignore f))
    (is string= "omega beta alphabet omega" (session-text editor)))
  (is string= "a\\.b\\/c" (tuatara.defaults::search-quote "a.b/c" t))
  (is string= "a\\/b" (tuatara.defaults::search-quote "a/b" nil)))

(define-test (incremental-search pattern-history)
  ;; one history for every question that takes a pattern, kept however the question ended
  (multiple-value-bind (f editor) (run-keys "C-s b o C-g C-s M-p RET" :text "foo boo")
    (declare (ignore f))
    (is = 6 (session-point editor))
    (is equal '("bo") tuatara.defaults::*pattern-history*))
  ;; a search's pattern brought back in flush-lines, and the other way round
  (multiple-value-bind (f editor) (run-keys "C-s b RET C-g % M-x f l u s h - l i n e s RET M-p RET"
                                            :text (format nil "one~%two b~%three~%") :width 100)
    (declare (ignore f))
    (is string= (format nil "one~%three~%") (session-text editor)))
  (multiple-value-bind (f editor) (run-keys "% M-x f l u s h - l i n e s RET t w RET C-g g g C-s M-p RET"
                                            :text (format nil "one~%two~%three tw~%") :width 100)
    (declare (ignore f))
    (is string= (format nil "one~%") (session-text editor)))
  ;; the arrows too
  (multiple-value-bind (f editor) (run-keys "C-s b o C-g C-s <up> RET" :text "foo boo")
    (declare (ignore f))
    (is = 6 (session-point editor)))
  ;; C-r in a forward search goes to the match before, the search staying forward
  (multiple-value-bind (f editor) (run-keys (list "C-s o C-s C-s C-r" #'wait-for-search-count :tick "C-g") :text "foo boo" :width 80)
    (declare (ignore editor))
    (false (tuatara.frontend.fake:fake-frame-showing f "Regex search backward"))
    (true (tuatara.frontend.fake:fake-frame-showing f "2/4    Regex search forward: o")))
  ;; the place counted again in the background when the match is far from the one counted for
  (let ((tuatara.defaults::*search-count-near* 0))
    (multiple-value-bind (f editor) (run-keys (list "C-s o C-s C-s C-r" #'wait-for-search-count :tick "C-g") :text "foo boo" :width 80)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "2/4    Regex search forward: o"))))
  ;; a text past *typing-pause-octets*: counted once typing pauses, its place kept empty till then
  (let ((tuatara.defaults::*typing-pause-octets* 0) (tuatara.defaults::*typing-pause* 1000))
    (multiple-value-bind (f editor) (run-keys (list "C-s o" :tick "C-g") :text "foo boo" :width 80)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "       Regex search forward: o"))
      (false (tuatara.frontend.fake:fake-frame-showing f "/4    Regex search forward: o"))))
  (let ((tuatara.defaults::*typing-pause-octets* 0) (tuatara.defaults::*typing-pause* 1/100))
    (multiple-value-bind (f editor) (run-keys (list "C-s o" #'wait-for-search-count :tick "C-g") :text "foo boo" :width 80)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "1/4    Regex search forward: o"))))
  ;; M-n goes forward again, past the newest to what was typed
  (multiple-value-bind (f editor) (run-keys "C-s a RET C-g C-s b RET C-g C-s M-p M-p M-n RET" :text "a b a b")
    (declare (ignore f))
    (is = 7 (session-point editor))))

(define-test (incremental-search replace-regexp-as-sed)
  ;; r r with nothing selected: the first match on the line at point; % every match in the buffer
  (multiple-value-bind (f editor) (run-keys "r r o / 0 RET" :text (format nil "foo~%boo~%"))
    (declare (ignore f))
    (is string= (format nil "f0o~%boo~%") (session-text editor)))
  (multiple-value-bind (f editor) (run-keys "r r % o / 0 RET" :text (format nil "foo~%boo~%"))
    (declare (ignore f))
    (is string= (format nil "f00~%b00~%") (session-text editor)))
  ;; in two parts: RET after the pattern types the /
  (multiple-value-bind (f editor) (run-keys "r r % o RET 0 RET" :text (format nil "foo~%boo~%"))
    (declare (ignore f))
    (is string= (format nil "f00~%b00~%") (session-text editor)))
  ;; with a selection: the first match on each of its lines; % every match within it
  (multiple-value-bind (f editor) (run-keys "% r r o / 0 RET" :text (format nil "foo~%boo~%"))
    (declare (ignore f))
    (is string= (format nil "f0o~%b0o~%") (session-text editor)))
  (multiple-value-bind (f editor) (run-keys "V r r % o / 0 RET" :text (format nil "foo~%boo~%"))
    (declare (ignore f))
    (is string= (format nil "f00~%boo~%") (session-text editor)))
  ;; no flags: they are refused, the question kept open, saying why
  (multiple-value-bind (f editor) (run-keys "r r o / 0 / g RET C-g" :text (format nil "foo~%boo~%") :width 200)
    (true (tuatara.frontend.fake:fake-frame-showing f "no flags here: a / in the pattern or the replacement is written \\/, and % in front replaces every match"))
    (is string= (format nil "foo~%boo~%") (session-text editor)))
  ;; the word at point offered as the pattern, selected: RET takes it, typing replaces it
  (multiple-value-bind (f editor) (run-keys "l l r r RET x RET" :text (format nil "a kea kea~%kea~%") :width 120)
    (true (tuatara.frontend.fake:fake-frame-showing f "(?<![\\p{L}0-9_-])kea(?![\\p{L}0-9_-])"))
    (is string= (format nil "a x kea~%kea~%") (session-text editor)))
  (multiple-value-bind (f editor) (run-keys "l l r r % k e a RET x RET" :text (format nil "a kea kea~%kea~%"))
    (declare (ignore f))
    (is string= (format nil "a x x~%x~%") (session-text editor)))
  ;; the cursors' lines, as f takes them
  (multiple-value-bind (f editor) (run-keys "t t r r % o / 0 RET" :text (format nil "foo~%boo~%zoo~%moo~%"))
    (declare (ignore f))
    (is string= (format nil "f00~%b00~%z00~%moo~%") (session-text editor)))
  (multiple-value-bind (f editor) (run-keys "t t r r o / 0 RET" :text (format nil "foo~%boo~%zoo~%moo~%"))
    (declare (ignore f))
    (is string= (format nil "f0o~%b0o~%z0o~%moo~%") (session-text editor)))
  ;; r q: the same, asking at each match; n leaves it and goes on to the next
  (multiple-value-bind (f editor) (run-keys "r q o RET 0 RET y n" :text (format nil "foo~%boo~%"))
    (declare (ignore f))
    (is string= (format nil "f0o~%boo~%") (session-text editor)))
  (multiple-value-bind (f editor) (run-keys "r q % o RET 0 RET y n y n" :text (format nil "foo~%boo~%"))
    (declare (ignore f))
    (is string= (format nil "f0o~%b0o~%") (session-text editor))))

(define-test (incremental-search replace-preview)
  ;; while the replacement is typed, what takes each match's place is shown beside it
  (multiple-value-bind (f editor) (run-keys "r r % o / 0" :text (format nil "foo~%bar~%boo") :width 40)
    (declare (ignore editor))
    (multiple-value-bind (screen spans) (tuatara.frontend.fake:fake-frame-with f "fo0o0")
      (true screen)
      (true (span-has-face-p (aref spans 0) :replace-going))
      (true (span-has-face-p (aref spans 0) :replace-coming)))
    (true (tuatara.frontend.fake:fake-frame-showing f "bo0o0")))
  ;; while the pattern alone is typed, the matches already show as what goes
  (multiple-value-bind (f editor) (run-keys "r r % o" :text "foo" :width 80)
    (declare (ignore editor))
    (multiple-value-bind (screen spans) (tuatara.frontend.fake:fake-frame-with f "Regex replace in the buffer: %o")
      (true screen)
      (true (span-has-face-p (aref spans 0) :replace-going))
      (false (span-has-face-p (aref spans 0) :search-other))))
  ;; every match on the screen, however many: 60 rows of 50 matches
  (let ((text (format nil "~{~a~%~}" (loop repeat 60 collect (make-string 50 :initial-element #\o)))))
    (multiple-value-bind (f editor) (run-keys "r r % o" :text text :width 80 :height 64)
      (declare (ignore editor))
      (multiple-value-bind (screen spans) (tuatara.frontend.fake:fake-frame-with f "Regex replace in the buffer: %o")
        (true screen)
        (true (span-has-face-p (aref spans 59) :replace-going)))))
  ;; the first match on each line only, without %; groups and case read as on RET
  (multiple-value-bind (f editor) (run-keys "% r r ( o + ) / < \\ 1 >" :text (format nil "foo~%boo") :width 40)
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "foo<oo>"))
    (true (tuatara.frontend.fake:fake-frame-showing f "boo<oo>")))
  ;; in any mode: inside a Lisp string the replacement keeps its own colour, not the string's
  (multiple-value-bind (f editor) (run-keys "M-x l i s p - m o d e RET r r % h e l l o / b y e"
                                            :text "(f \"hello\")" :width 60)
    (declare (ignore editor))
    (multiple-value-bind (screen spans) (tuatara.frontend.fake:fake-frame-with f "hellobye")
      (let* ((at (search "bye" (aref screen 0)))
             (faces (loop for (start end face) in (aref spans 0)
                          when (and (<= start at) (< at end)) append (if (listp face) face (list face)))))
        (true (member :replace-coming faces))
        (false (member :string faces)))))
  ;; given up, nothing changed and nothing is shown beside the text any more
  (multiple-value-bind (f editor) (run-keys "r r % o / 0 C-g" :text "foo" :width 40)
    (is string= "foo" (session-text editor))
    (is string= "foo" (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f 0)))))

(tuatara.command:define-command test-exhaust-stack ()
  "Recurse until the control stack runs out: the editor must survive it."
  (labels ((deep (n) (1+ (deep (1+ n))))) (deep 0)))

(define-test (incremental-search memory)
  ;; a replacement too large for its budget goes on in the background, and completes
  (let ((tuatara.defaults::*replacement-budget* 1000000) (text (make-string 5000 :initial-element #\o)))
    (multiple-value-bind (f editor) (run-keys "r r % o / 0 RET" :text text :width 100)
      (true (tuatara.frontend.fake:fake-frame-showing f "Replacing in the buffer in the background"))
      (true (tuatara.frontend.fake:fake-frame-showing f "Replaced 5,000 occurrences on 1 line in the buffer"))
      (is string= (make-string 5000 :initial-element #\0) (session-text editor)))
    ;; the search's count keeps to the matches alone, so it fits where replacing did not
    (multiple-value-bind (f editor) (run-keys (list "C-s o" #'wait-for-search-count :tick "C-g") :text text :width 100)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "1/5000 Regex search forward: o")))
    ;; ! after asking: more than the budget holds goes on in the background, and completes
    (multiple-value-bind (f editor) (run-keys "M-% o RET 0 RET !" :text text :width 100)
      (true (tuatara.frontend.fake:fake-frame-showing f "Replaced 5,000 occurrences"))
      (is string= (make-string 5000 :initial-element #\0) (session-text editor))))
  ;; running out of stack in a command becomes a message; the editor goes on
  (multiple-value-bind (f editor) (run-keys "M-x t e s t - e x h a u s t - s t a c k RET i a b" :text "" :width 100)
    (true (tuatara.frontend.fake:fake-frame-showing f "Stopped: control stack exhausted"))
    (is string= "ab" (session-text editor))))

(define-test (incremental-search replace)
  ;; y, n, then ! replaces the rest as one batch
  (multiple-value-bind (f editor) (run-keys "M-% o RET 0 RET y n !" :text "foo boo zoo")
    (is string= "f0o b00 z00" (session-text editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "Replaced 5 occurrences")))
  ;; asked at each, its count in thousands as every replacement says it
  (multiple-value-bind (f editor) (run-keys "M-% a RET b RET !" :text (make-string 1500 :initial-element #\a) :width 100)
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "Replaced 1,500 occurrences")))
  ;; q stops
  (multiple-value-bind (f editor) (run-keys "M-% o RET 0 RET y q" :text "foo boo")
    (declare (ignore f))
    (is string= "f0o boo" (session-text editor)))
  ;; n skips the first match, . replaces the second and stops
  (multiple-value-bind (f editor) (run-keys "M-% o RET 0 RET n ." :text "foo boo")
    (declare (ignore f))
    (is string= "fo0 boo" (session-text editor)))
  ;; replace-all is undone as one step
  (multiple-value-bind (f editor) (run-keys "M-% o RET 0 RET ! u" :text "foo boo zoo")
    (declare (ignore f))
    (is string= "foo boo zoo" (session-text editor)))
  ;; NORMAL state's r q and r r
  (multiple-value-bind (f editor) (run-keys "r q % a RET b RET !" :text "banana")
    (declare (ignore f))
    (is string= "bbnbnb" (session-text editor)))
  (multiple-value-bind (f editor) (run-keys "r r % ( [ a - z ] ) o RET \\ 1 _ RET" :text "fo bo x")
    (declare (ignore f))
    (is string= "f_ b_ x" (session-text editor)))
  ;; query-replace-regexp with a group
  (multiple-value-bind (f editor) (run-keys "C-M-% % ( \\ d ) RET < \\ 1 > RET !" :text "a1b2")
    (declare (ignore f))
    (is string= "a<1>b<2>" (session-text editor))))

(define-test (incremental-search searched-in-the-background)
  ;; a search that takes more than a moment goes on in the background: RET while it does
  ;; ends the search once it has found its match; C-g stops one that finds nothing
  ;; no moment on the editor's thread: every search goes on in the background
  (let ((text (concatenate 'string (make-string 30000000 :initial-element #\a) "bc"))
        (tuatara.defaults::*search-foreground-seconds* 0))
    (multiple-value-bind (f editor)
        (run-keys (list "C-s b c RET"
                        ;; until the search in the background has found it (the editor is woken then)
                        (lambda ()
                          (loop with deadline = (tuatara.runtime:deadline-in 20)
                                for state = tuatara.defaults::*search*
                                until (or (null state)
                                          (let ((background (tuatara.defaults::search-state-background state)))
                                            (or (null background) (car (first background))))
                                          (<= (tuatara.runtime:seconds-left deadline) 0))
                                do (sleep 0.02)))
                        :tick)
                  :text text :width 80)
      (true (tuatara.frontend.fake:fake-frame-showing f "searching"))
      (is = (+ 30000000 2) (session-point editor))
      (false tuatara.defaults::*search*))
    (let ((start (get-internal-real-time)))
      (multiple-value-bind (f editor) (run-keys "C-s q q z C-g" :text text :width 80)
        (declare (ignore f))
        (is = 0 (session-point editor))
        (false tuatara.defaults::*search*))
      (true (< (- (get-internal-real-time) start) (* 20 internal-time-units-per-second))))))

(define-test (incremental-search search-gives-way-soon)
  ;; a search longer than a moment gives the editor's thread back soon at every key, going on
  ;; in the background: typing a pattern found nowhere in a large text costs little more per
  ;; key than typing one found at once
  (let ((large (let ((line (make-string 99 :initial-element #\a)))
                 (with-output-to-string (out)
                   (loop repeat 400000 do (write-line line out))))))
    (flet ((seconds (keys)
             (let ((start nil) (end nil))
               (run-keys (list (lambda () (setf start (get-internal-real-time))) keys
                               (lambda () (setf end (get-internal-real-time))))
                         :text large :width 80)
               (/ (- end start) internal-time-units-per-second))))
      (seconds "C-M-s a a a a a a a a a C-g")
      (let ((found (seconds "C-M-s a a a a a a a a a C-g"))
            (nowhere (seconds "C-M-s a * q q q q q q q C-g")))
        (true (< (- nowhere found) (* 8 1/40 (machine-slowdown))) "~,3f s against ~,3f s" nowhere found)))))

(define-test (incremental-search search-prompt-cursor)
  ;; C-s: the pattern typed in the minibuffer, the cursor after it; the match lit, point after it a cursor the typing does not go to
  (multiple-value-bind (cursor screen spans) (cursor-at-question '("C-s o" :look "RET") :text "foo boo" :width 60)
    (is string= "Regex search forward: o" (subseq (string-right-trim " " (aref screen 7)) 7))
    (is equal (cons 7 30) cursor)
    (true (face-at-column-p (aref spans 0) 1 :search-current))
    (true (face-at-column-p (aref spans 0) 2 :cursor-inactive)))
  ;; query-replace asking at a match: the cursor after the question, the match lit as the current one, point after it
  (multiple-value-bind (cursor screen spans) (cursor-at-question '("M-% o RET 0 RET" :look "q") :text "foo boo" :width 60)
    (let ((question (string-right-trim " " (aref screen 7))))
      (true (search "Query replacing o with 0" question))
      (is equal (cons 7 (length question)) cursor))
    (true (face-at-column-p (aref spans 0) 1 :search-current))
    (false (face-at-column-p (aref spans 0) 1 :region))
    (true (face-at-column-p (aref spans 0) 2 :cursor-inactive))))

(define-test (incremental-search problems-in-plain-words)
  ;; why a pattern cannot be used, in words, never the program's insides
  (flet ((problem (pattern) (nth-value 1 (tuatara.defaults::make-pattern-matcher pattern :regex t))))
    (is string= "invalid: a lookbehind must have a fixed length" (problem "(?<=hel+)o"))
    (is string= "invalid: nothing for + to repeat" (problem "+a"))
    (is string= "invalid: a ) with no ( before it" (problem "a)"))
    (is string= "invalid: the range z-a runs backwards" (problem "[z-a]"))
    (is string= "incomplete" (problem "(kea"))
    (is string= "incomplete" (problem "[a"))
    (is string= "not supported: \\K" (problem "user=\\Kkiri"))
    (is string= "not supported: \\G" (problem "\\Gx"))
    (is string= "not supported: \\R in a set" (problem "[\\R]"))
    (is string= "invalid: no character property \\p{Foo}" (problem "\\p{Foo}"))
    (is string= "invalid: no POSIX class [:foo:]" (problem "[[:foo:]]"))
    (is string= "invalid: no POSIX class [:foo:]" (problem "[[:^foo:]]"))
    (is string= "invalid: [:alpha:] goes inside a set: [[:alpha:]]" (problem "[:alpha:]"))
    (dolist (pattern '("(?<=hel+)o" "+a" "a)" "[z-a]" "(?<1a)" "\\p{Foo}" "x{2}{3}" "(?(?=a)b|c|d)"))
      (let ((words (problem pattern)))
        (true words)
        (false (find #\# words) "~a: ~a" pattern words)
        (false (search "*" words) "~a: ~a" pattern words)))))

(define-test (incremental-search pattern-language)
  ;; what PCRE has and cl-ppcre lacks is read as PCRE reads it, never as plain letters
  (flet ((match (pattern text) (multiple-value-list (cl-ppcre:scan (tuatara.syntax:compile-regex pattern) text))))
    (is equal '(2 4) (subseq (match "[[:alpha:]]+" "12Ab]") 0 2))
    (is equal '(0 3) (subseq (match "[[:^digit:]x]+" "ab:1") 0 2))
    ;; a pattern's (?x) is its own: a pattern read after it keeps its spaces and its #
    (tuatara.syntax:compile-regex "(?x) a b # note")
    (is equal '(0 7) (subseq (match "kia ora" "kia ora") 0 2))
    (is equal '(0 2) (subseq (match "#1" "#1") 0 2))
    (let ((rope (tuatara.text:string->rope "x kia ora")) (found '()))
      (tuatara.syntax:map-regex-matches (lambda (s e m g) (declare (ignore m g)) (push (list s e) found) nil)
                                        rope (tuatara.syntax:compile-regex "kia ora") 0 9 :texts nil)
      (is equal '((2 9)) found))
    ;; a group's own case flag: the text every match holds is not looked for as written
    (let ((rope (tuatara.text:string->rope "AB ab")) (found '()))
      (tuatara.syntax:map-regex-matches (lambda (s e m g) (declare (ignore m g)) (push (list s e) found) nil)
                                        rope (tuatara.syntax:compile-regex "(?i:a)B") 0 5 :texts nil)
      (is equal '((0 2)) found))
    ;; every class turned round: the ASCII outside it and all past ASCII
    (is equal '(2 5) (subseq (match "[[:^alpha:]]+" "ab12 tūī") 0 2))
    (is equal '(1 3) (subseq (match "[[:^punct:]]+" ",ab!") 0 2))
    (is equal '(2 6) (subseq (match "[[:^xdigit:]]+" "ff zz 1a") 0 2))
    (is equal '(0 2) (subseq (match "[[:^ascii:]]+" "ūīa") 0 2))
    (is equal '(1 3) (subseq (match "[x[:^alpha:]]+" "ax1b") 0 2))
    (is equal '(nil) (match "[[:^print:]]" "a b~"))
    ;; a negated case still names a case: [:^lower:] passes over KEA's small letters
    (false (tuatara.syntax:regex-case-fold-p "[[:^lower:]]+"))
    (is equal '(2 5) (subseq (match "\\p{L}+" "12éAb") 0 2))
    (is equal '(4 7) (subseq (match "\\p{Greek}+" "abc αβγ") 0 2))
    (is equal '(1 3) (subseq (match "\\pL\\p{Ll}" "1Ab") 0 2))
    (is equal '(4 7) (subseq (match "\\Qa.b\\E" "axb a.b") 0 2))
    (is equal '(10 19) (subseq (match "(?<year>\\d{4})-\\k<year>" "2020-2021 2020-2020") 0 2))
    (is equal '(0 4) (subseq (match "a\\h+b" (format nil "a ~cb" (code-char #xA0))) 0 2))
    (is equal '(1 3) (subseq (match "\\R" (format nil "a~c~cb" #\Return #\Newline)) 0 2))
    ;; a named group is numbered as any other in a replacement
    (is string= "2020/01" (session-text (nth-value 1 (run-keys (format nil "r r ~a RET" (key-string "%(?<y>\\d{4})-(?<m>\\d\\d)/\\1\\/\\2"))
                                                               :text "2020-01")))))
  ;; a slash inside \Q...\E is part of the pattern, as it is inside a set; the prompt reads the set as the engine does
  (is equal '("\\Qa/b\\E" "x") (tuatara.defaults::split-search-line "\\Qa/b\\E/x" t))
  (is equal '("[[:a]" "b:]") (tuatara.defaults::split-search-line "[[:a]/b:]" t))
  (is = 11 (tuatara.syntax:bracket-end "[[:alpha:]]" 0))
  ;; a capital in a property's name does not turn ignoring case off
  (true (tuatara.syntax:regex-case-fold-p "\\p{L}x"))
  (true (tuatara.syntax:regex-case-fold-p "\\p{Greek}x"))
  (false (tuatara.syntax:regex-case-fold-p "\\p{L}X")))

(define-test (incremental-search smart-case-heeds-named-case)
  ;; a pattern naming a case heeds case, as one with a capital letter does
  (flet ((found (pattern) (cl-ppcre:all-matches-as-strings (tuatara.syntax:compile-regex pattern) "Meeting Ōtaki")))
    (is equal '("M") (found "[[:upper:]]"))
    (is equal '("eeting" "taki") (found "[[:lower:]]+"))
    (is equal '("M" "Ō") (found "\\p{lu}"))
    (is equal '("eeting" "taki") (found "\\p{lowercase_letter}+"))
    (is equal '("Meeting") (found "\\p{Lu}eeting"))
    (is equal '() (found "\\p{Lu}EETING"))
    ;; other classes leave smart case as it was
    (is equal '("MEETING") (cl-ppcre:all-matches-as-strings (tuatara.syntax:compile-regex "[[:alpha:]]eeting") "MEETING")))
  (dolist (pattern '("[[:upper:]]" "[x[:lower:]]" "\\p{Ll}" "\\P{lt}" "\\p{Uppercase_Letter}"))
    (false (tuatara.syntax:regex-case-fold-p pattern) "~a" pattern))
  (dolist (pattern '("[[:alpha:]]" "\\p{LC}" "\\pL" "\\[:upper:\\]x"))
    (true (tuatara.syntax:regex-case-fold-p pattern) "~a" pattern))
  ;; the search goes to the first capital
  (is = 7 (session-point (nth-value 1 (run-keys (format nil "C-s ~a RET" (key-string "[[:upper:]]")) :text "notes Meeting" :width 80)))))

(define-test (incremental-search possessive-quantifiers)
  ;; a++ a*+ a?+ a{n,m}+ take as much as they can and never give it back: (?>a+)
  (flet ((match (pattern text) (cl-ppcre:scan (tuatara.syntax:compile-regex pattern) text)))
    (is string= "(?>a+)b" (tuatara.syntax::engine-pattern "a++b"))
    (is string= "x(?>[ab]*)(?>(cd)?)(?>\\d{2,3})" (tuatara.syntax::engine-pattern "x[ab]*+(cd)?+\\d{2,3}+"))
    (is string= "a(?>c+)" (tuatara.syntax::engine-pattern "\\Qac\\E++"))
    (is string= "a+?b" (tuatara.syntax::engine-pattern "a+?b"))
    (is string= "(?#a++)b" (tuatara.syntax::engine-pattern "(?#a++)b"))
    (true (match "a+b" "aaab"))
    (true (match "a++b" "aaab"))
    ;; what a possessive repetition took is not given back to what follows
    (false (match "a++a" "aaaa"))
    (true (match "a+a" "aaaa"))
    (false (match "\".*+\"" "\"kiwi\""))
    (true (match "[^\"]*+\"" "kiwi\""))
    (false (match "(?:ab)++ab" "ababab")))
  (is equal nil (nth-value 1 (tuatara.defaults::make-pattern-matcher "l++" :regex t)))
  (is string= "invalid: nothing for * to repeat" (nth-value 1 (tuatara.defaults::make-pattern-matcher "a+**" :regex t))))

(define-test (incremental-search unusable-pattern-waits)
  ;; RET on a search, filter or replacement whose pattern cannot be used keeps the question open, saying why
  (multiple-value-bind (f editor) (run-keys (format nil "r r ~a RET x C-g" (key-string "%+a/b")) :text "aaa" :width 160)
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "[invalid: nothing for + to repeat; mend it, or C-g gives up]: %+a/b"))
    ;; the next key mends: the prompt shows the problem alone again
    (true (tuatara.frontend.fake:fake-frame-showing f "[invalid: nothing for + to repeat]: %+a/bx")))
  (multiple-value-bind (f editor) (run-keys (format nil "r r ~a RET C-g" (key-string "%a(/b")) :text "aaa" :width 160)
    (true (tuatara.frontend.fake:fake-frame-showing f "[incomplete; finish it, or C-g gives up]: %a(/b"))
    (is string= "aaa" (session-text editor)))
  (multiple-value-bind (f editor) (run-keys "C-s a ( RET C-g" :text "aaa" :width 160)
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "[incomplete; finish it, or C-g gives up]: a("))))

(defun call-with-watchdog (seconds function)
  "Call FUNCTION, failing with an error when it has not returned within
SECONDS: the editor's thread interrupted, so that a hang fails the test rather
than holding the run."
  (let* ((thread sb-thread:*current-thread*)
         (done (list nil))
         (watchdog (sb-thread:make-thread
                    (lambda ()
                      (loop repeat (* seconds 20) until (car done) do (sleep 0.05))
                      (unless (car done)
                        (sb-thread:interrupt-thread thread (lambda () (error "Still busy after ~d s" seconds)))))
                    :name "tuatara test watchdog")))
    (unwind-protect (funcall function)
      (setf (car done) t)
      (sb-thread:join-thread watchdog :default nil))))

(defun threads-named (name)
  "The threads alive whose name holds NAME."
  (remove-if-not (lambda (thread) (search name (sb-thread:thread-name thread))) (sb-thread:list-all-threads)))

(define-test (incremental-search slow-pattern)
  ;; a nested repeat on a line that almost matches: each key answered within a tenth of a second,
  ;; the search going on in the background and saying so, C-g ending it, its thread too
  (let ((times '()))
    (flet ((stamp () (push (thread-cpu-time) times)))
      (call-with-watchdog
       20 (lambda ()
            (multiple-value-bind (f editor)
                ;; wide enough for the prompt and the pattern on one row
                (run-keys (list "C-s ^ ( a + ) +" #'stamp "$" #'stamp "x" #'stamp "DEL" #'stamp "C-g" #'stamp)
                          :text (format nil "~a!~%" (make-string 40 :initial-element #\a)) :width 120)
              (true (tuatara.frontend.fake:fake-frame-showing f "Regex search forward [slow pattern: still searching]: ^(a+)+$"))
              (is = 0 (session-point editor))
              (false tuatara.defaults::*search*))))
      (loop for (later earlier) on times while earlier
            do (true (< (- later earlier) (/ internal-time-units-per-second 10)) "a key took ~,3f s"
                     (/ (- later earlier) internal-time-units-per-second)))
      (loop repeat 50 while (threads-named "tuatara search") do (sleep 0.02))
      (false (threads-named "tuatara search")))))

(define-test (incremental-search slow-replacement-stopped)
  ;; a replacement whose pattern holds up one line goes on in the background at RET, saying why;
  ;; M-x stop-job ends it at once, though it is inside that one line
  (let ((stopped-in nil)
        (text (format nil "~{~a~%~}" (list "kea" (format nil "~a!" (make-string 40 :initial-element #\a)) "tui"))))
    (call-with-watchdog
     20 (lambda ()
          (multiple-value-bind (f editor)
              (run-keys (list (format nil "r r ~a RET" (key-string "%^(a+)+$/x"))
                              (lambda () (sleep 0.1))
                              "M-x s t o p - j o b RET y RET"
                              (lambda ()
                                (let ((start (get-internal-real-time)))
                                  (loop repeat 100 while (find-if #'tuatara.runtime:job-running-p tuatara.runtime:*jobs*)
                                        do (sleep 0.01))
                                  (setf stopped-in (/ (- (get-internal-real-time) start) internal-time-units-per-second))))
                              :tick)
                        :text text :width 120)
            (declare (ignore editor))
            (true (tuatara.frontend.fake:fake-frame-showing f "The pattern is slow here: replacing in the buffer in the background"))
            (true (tuatara.frontend.fake:fake-frame-showing f "Stop replacing")))))
    (true stopped-in)
    (true (< stopped-in 5) "stopped in ~,3f s" stopped-in)
    (false (find-if #'tuatara.runtime:job-running-p tuatara.runtime:*jobs*))))

(define-test (incremental-search steps-through-empty-matches)
  ;; C-s finds the empty lines that its count counts, one after another, and fails past the last
  (multiple-value-bind (f editor) (run-keys "C-s ^ $ C-s RET" :text (format nil "a~%~%b~%~%c"))
    (declare (ignore f))
    (is = 5 (session-point editor)))
  (multiple-value-bind (f editor) (run-keys (list "C-s ^ $ C-s" #'wait-for-search-count :tick "C-g")
                                            :text (format nil "a~%~%b~%~%c") :width 80)
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "2/2    Regex search forward: ^$")))
  ;; at each word's start and end in turn, never twice at one
  (multiple-value-bind (f editor) (run-keys "C-s \\ b C-s C-s RET" :text "kia ora")
    (declare (ignore f))
    (is = 4 (session-point editor)))
  ;; and back
  (multiple-value-bind (f editor) (run-keys "g e C-r ^ RET" :text (format nil "kia~%ora"))
    (declare (ignore f))
    (is = 4 (session-point editor))))

(define-test (incremental-search symbol-never-inside-longer)
  ;; M-. offers the symbol whole: never the start of a longer symbol holding a -
  (is string= "Z foo-bar-baz Z"
      (session-text (nth-value 1 (run-keys "r r % M-. / Z RET" :text "foo-bar foo-bar-baz foo-bar"))))
  ;; in Lisp, where * and / are a name's too
  (is string= "(Z *foo*s Z)"
      (session-text (nth-value 1 (run-keys (format nil "M-x ~a RET l r r % M-. / Z RET" (key-string "lisp-mode"))
                                           :text "(*foo* *foo*s *foo*)"))))
  ;; a literal search takes the symbol as it is, nothing escaped in it
  (is string= "%make/x" (tuatara.defaults::search-quote "%make/x" nil :whole t :parsed nil))
  (multiple-value-bind (f editor)
      (run-keys (format nil "M-x ~a RET M-x ~a RET M-. C-s RET" (key-string "lisp-mode") (key-string "literal-search-forward"))
                :text "%make/x (%make/x)")
    (declare (ignore f))
    (is = 16 (session-point editor))))

(define-test (incremental-search replacement-line-breaks-as-the-file)
  ;; \n in a replacement writes the buffer's own line end: a carriage return and a newline in a
  ;; Windows file, by r r here and in the background alike, and by the question that asks
  (dolist (keys (list (format nil "r r % ~a RET" (key-string ";/;\\n"))
                      (format nil "C-M-% % ; RET ~a RET !" (key-string ";\\n"))))
    (uiop:with-temporary-file (:stream s :pathname file :type "txt")
      (write-string (format nil "a;b~c~%c~c~%" #\Return #\Return) s)
      (finish-output s)
      (unwind-protect
           (is string= (format nil "a;~c~%b~c~%c~c~%" #\Return #\Return #\Return)
               (session-text (nth-value 1 (run-keys keys :file (uiop:native-namestring file))))
               "~a" keys)
        (kill-buffers-visiting file)))))

(define-test (incremental-search count-sees-past-point)
  ;; the count of matches before the current one reads the text past it too: o$ is not
  ;; matched by the o before point where point stands inside a line
  (dolist (case '(("o$" "1/2" "2/2") ("o\\b" "1/2" "2/2")))
    (destructuring-bind (pattern first second) case
      (let ((keys (key-string pattern)) (text (format nil "foo~%foo~%")))
        (true (tuatara.frontend.fake:fake-frame-showing
               (run-keys (list (format nil "C-s ~a" keys) #'wait-for-search-count :tick "C-g") :text text :width 80)
               (format nil "~a    Regex search forward: ~a" first pattern)))
        (true (tuatara.frontend.fake:fake-frame-showing
               (run-keys (list (format nil "C-s ~a C-s" keys) #'wait-for-search-count :tick "C-g") :text text :width 80)
               (format nil "~a    Regex search forward: ~a" second pattern))))))
  ;; an empty line found is counted where it stands, as the search goes to it
  (true (tuatara.frontend.fake:fake-frame-showing
         (run-keys (list "C-s ^ $" #'wait-for-search-count :tick "C-g") :text (format nil "a~%~%b~%~%c") :width 80)
         "1/2    Regex search forward: ^$")))

(define-test (incremental-search pattern-prompts-alike)
  ;; every question taking a pattern names its kind one way, a thorough regex too, and
  ;; refuses one that cannot be used, saying why
  (let ((tuatara.syntax:*regex-thorough* t))
    (true (tuatara.frontend.fake:fake-frame-showing (filtered-session (list "C-x / s a" :tick)) "Thorough regex filter: a")))
  (true (tuatara.frontend.fake:fake-frame-showing (filtered-session (list "C-x / s M-r a" :tick)) "Literal filter: a"))
  (is string= "Regex keep lines [incomplete]: " (tuatara.defaults::pattern-prompt t "keep lines" "incomplete"))
  (is string= "Literal search files: " (tuatara.defaults::pattern-prompt nil "search files"))
  ;; a search of files refuses a word that is no regular expression, as the others refuse it
  (is string= "incomplete" (tuatara.defaults::search-input-problem "kia ora(" t))
  (false (tuatara.defaults::search-input-problem "kia ora(" nil))
  (false (tuatara.defaults::search-input-problem "kia ora" t)))
