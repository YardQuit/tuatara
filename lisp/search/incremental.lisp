;;;; lisp/search/incremental.lisp --- Incremental search, regex by default, with match highlighting.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; C-s and C-r search as you type for a regular expression, the pattern
;;;; typed in the minibuffer; M-r switches to literal text and back. The
;;;; prompt says what runs: "Regex search forward: ".
;;;; Typed as vim's :s, pattern/replacement/flags replaces on RET instead
;;;; (substitute.lisp); the matches it will replace are the ones
;;;; highlighted. A region limits a search to itself unless the string starts
;;;; with %; M-%, C-M-% and NORMAL state's r q and r r open this same prompt set up for
;;;; replacing. On a file of many gigabytes a search runs in parallel and
;;;; gives way as soon as another key is typed.

(in-package #:tuatara.defaults)

(defvar *search-keymap* (make-keymap :name "search")
  "The keys of an incremental search, before the minibuffer's: every other
key edits the text typed as in any question there.")

(defstruct (search-state (:constructor make-search-state (origin direction regex &optional region preset filtering)) (:copier nil))
  "State of an incremental search: STRING typed so far; DIRECTION, true when
forward; REGEX, true when the pattern is a regular expression rather than
literal text; ORIGIN, where point was when it started; REGION, the active
region then as (START . END), or NIL; PRESET, for a prompt opened for
replacing (\\[query-replace]), a plist whose :FLAGS apply as though typed and
whose :EVERY-AFTER-PERCENT, true for \\[replace-regexp], has a leading % stand
for every match rather than the whole buffer, flags refused, or NIL; MATCH, the current
\(START . END) or NIL; FAILING when the last search found nothing; WRAPPED after
continuing from the other end; STOPPED, the offset a search began from when
the next key cut it short, or NIL; BACKGROUND, while a search goes on on a
thread of its own, (RESULT CANCEL), RESULT a cons whose car it fills, CANCEL
one whose car stops it; EXIT-WHEN-FOUND, true when the search is to end once
that thread has found its match; SLOW, true when the pattern was found so slow
on this text that one line held the search up, which then goes on in the
background and lights no other matches; REFUSED, true when
\\<search>\\[search-exit] was refused because the pattern cannot be used,
until the string changes; HISTORY of earlier states,
newest first, gone back to as what was typed is taken back; FILTERING, for a
search that narrows a list (SEARCH-FILTER-REGEX), that list, or for the
matches of a list's filters lit elsewhere (LIGHT-SEARCH-FILTERS) the buffer
they are lit in, its string then all pattern, a slash or a leading % in it
standing for itself; COUNT, how many matches there are and which the current
one is, as (KEY TOTAL ANCHOR BEFORE), KEY what they were counted for, BEFORE
how many come before the match starting at ANCHOR; COUNTING, while they are
counted on a thread of its own, (KEY RESULT CANCEL); COUNT-WORDS, what the
prompt last said of the count; TALLY, for a replacement, its count of the
matches it replaces (a PATTERN-COUNT); and caches of the parsed string and
its compiled pattern."
  (string "" :type string)
  (direction t)
  (regex t)
  (origin 0 :type fixnum)
  (region nil)
  (preset nil)
  (match nil)
  (failing nil)
  (wrapped nil)
  (stopped nil)
  (background nil)
  (exit-when-found nil)
  (slow nil)
  (refused nil)
  (history '())
  (filtering nil)
  (count nil)
  (counting nil)
  (count-words nil)
  (tally nil)
  (parsed nil)
  (compiled nil))

(setf (documentation 'make-search-state 'function) "Return a fresh search state starting at ORIGIN; DIRECTION true means forward, REGEX true a regular expression; REGION the active region or NIL."
      (documentation 'search-state-p 'function) "True when OBJECT is a SEARCH-STATE."
      (documentation 'search-state-string 'function) "The search string typed so far."
      (documentation 'search-state-direction 'function) "True when searching forward."
      (documentation 'search-state-regex 'function) "True when the pattern is a regular expression, NIL for literal text."
      (documentation 'search-state-origin 'function) "Point when the search started."
      (documentation 'search-state-region 'function) "The region active when the search started, as (START . END), or NIL."
      (documentation 'search-state-preset 'function) "For a prompt opened for replacing, a plist with the :FLAGS that apply; else NIL."
      (documentation 'search-state-match 'function) "The current match as (START . END), or NIL."
      (documentation 'search-state-failing 'function) "True when the last search found nothing."
      (documentation 'search-state-wrapped 'function) "True once the search continued from the other end."
      (documentation 'search-state-stopped 'function) "Where a search that gave way to the next key began, or NIL."
      (documentation 'search-state-background 'function) "While the search goes on in the background, (RESULT CANCEL); else NIL."
      (documentation 'search-state-exit-when-found 'function) "True when the search ends once its search in the background is done."
      (documentation 'search-state-slow 'function) "True when the pattern was found too slow on this text to search for on the editor's thread."
      (documentation 'search-state-refused 'function) "True when the search's question was not ended because its pattern cannot be used."
      (documentation 'search-state-history 'function) "Earlier states, gone back to as what was typed is taken back."
      (documentation 'search-state-filtering 'function) "For a search that narrows a list, that list (or where its filters are lit); else NIL."
      (documentation 'search-state-count 'function) "The matches counted, as (KEY TOTAL ANCHOR BEFORE): how many there are, and how many come before the match starting at ANCHOR; or NIL."
      (documentation 'search-state-counting 'function) "While the matches are counted in the background, (KEY RESULT CANCEL); else NIL."
      (documentation 'search-state-tally 'function) "For a replacement, its count of the matches it replaces (a PATTERN-COUNT), or NIL."
      (documentation 'search-state-count-words 'function) "What the prompt last said of the count, kept while the matches are counted again."
      (documentation 'search-state-parsed 'function) "Cache: (KEY . SEARCH-LINE) for the string last parsed."
      (documentation 'search-state-compiled 'function) "Cache: (KEY . (KIND MATCHER FOLD)) for the pattern last compiled.")

(defvar *search* nil "The active incremental search, or NIL.")
(defvar *search-foreground-seconds* 1/100
  "Seconds a search runs on the editor's thread before it goes on in the
background, the editor answering keys and drawing meanwhile. While recorded
keys are played a search runs on the editor's thread until it ends or
\\`C-g' is typed.")

(defvar *typing-pause* 1/2
  "Seconds without a key typed before work over a text larger than
*TYPING-PAUSE-OCTETS* starts while a pattern is typed: the count of a
search's matches (\\[regex-search-forward]) and a filter being typed
\(\\[search-filter-regex]), its lines in the window shown at once. Typing so
stays quick on a text of gigabytes, the count and the rest coming a moment
after the last key; on a smaller text they come as each key is typed.")

(defvar *typing-pause-octets* (* 64 1024 1024)
  "The largest text, in octets, whose matches are counted and filtered as each
key of a pattern is typed; on a larger one that waits for typing to pause
\(*TYPING-PAUSE*).")

(defun typing-pause-left (octets)
  "Seconds still to wait, since the text of a pattern's question last changed,
before work over OCTETS of text starts (*TYPING-PAUSE*); NIL once that is
over, or at once for no more than *TYPING-PAUSE-OCTETS*."
  (let ((left (- (* *typing-pause* internal-time-units-per-second) (- (get-internal-real-time) *last-typed*))))
    (and (> octets *typing-pause-octets*) (plusp left) (/ left internal-time-units-per-second))))

(defstruct (pattern-count (:constructor make-pattern-count ()) (:copier nil))
  "How many matches a pattern being typed has, counted in the background:
KEY, what was counted for, and TOTAL, how many; JOB, while counting, (KEY
RESULT CANCEL); WORDS, what was shown last; FILTER, the pattern last typed,
what it was typed as and its filter, (TEXT REGEX FILTER), made once
\(STRETCH-PATTERN-COUNT)."
  (key nil) (total nil) (job nil) (words nil) (filter nil))

(setf (documentation 'make-pattern-count 'function) "Return a PATTERN-COUNT that has counted nothing yet."
      (documentation 'pattern-count-p 'function) "True when OBJECT is a PATTERN-COUNT."
      (documentation 'pattern-count-key 'function) "What a PATTERN-COUNT counted for, or NIL."
      (documentation 'pattern-count-total 'function) "How many matches a PATTERN-COUNT found for its KEY, or NIL."
      (documentation 'pattern-count-job 'function) "While a PATTERN-COUNT counts, (KEY RESULT CANCEL); else NIL."
      (documentation 'pattern-count-words 'function) "What a PATTERN-COUNT showed last, kept while it counts again."
      (documentation 'pattern-count-filter 'function) "The pattern a PATTERN-COUNT last counted, as (TEXT REGEX FILTER).")

(defun stop-pattern-count (counter)
  "Stop COUNTER's count in the background, when one goes on."
  (let ((job (pattern-count-job counter)))
    (when job
      (tuatara.runtime:stop-work (third job))
      (setf (pattern-count-job counter) nil))))

(defun pattern-count-update (counter key octets count)
  "Keep COUNTER's count for KEY up to date while a pattern is typed, KEY
saying what is counted (the pattern, the text, the scope), NIL for nothing:
COUNT, a function of a function telling it to stop, run on a thread of its
own over OCTETS of text, returns how many (NIL when stopped); over a large
text it starts once typing pauses (TYPING-PAUSE-LEFT). Two values, as a
question's UPDATE takes them: true while it counts, or the seconds still to
wait; and what to show before the prompt: \"12\", what it showed last until
the new count comes, \"\" before any or with nothing to count."
  (let* ((job (pattern-count-job counter))
         (result (and job (car (second job)))))
    (when result
      (setf (pattern-count-job counter) nil)
      (when (integerp (first result))
        (setf (pattern-count-key counter) (first job) (pattern-count-total counter) (first result)))))
  (flet ((shown () (or (pattern-count-words counter) "")))
    (cond ((null key) (stop-pattern-count counter) (values nil ""))
          ((and (equal key (pattern-count-key counter)) (pattern-count-total counter))
           (stop-pattern-count counter)
           (values nil (setf (pattern-count-words counter) (format nil "~d" (pattern-count-total counter)))))
          ((equal key (first (pattern-count-job counter))) (values t (shown)))
          ((typing-pause-left octets)
           (stop-pattern-count counter)
           (values (typing-pause-left octets) (shown)))
          (t (stop-pattern-count counter)
             (let ((cancel (list nil)))
               (setf (pattern-count-job counter)
                     (list key
                           (tuatara.runtime:start-background-result
                            (lambda () (list (funcall count (lambda () (car cancel)))))
                            "tuatara pattern count" (editor-job-waker) cancel)
                           cancel)))
             (values t (shown))))))

(defun count-in-stretches (rope matcher stretches &key every stop)
  "How many matches of MATCHER ROPE has in STRETCHES, (START . END) each: every
one (COUNT-LINE-MATCHES), or without EVERY the first on each line
\(COUNT-FIRST-PER-LINE); NIL once STOP answers true."
  (loop for (start . end) in stretches
        sum (multiple-value-bind (count status)
                (if every
                    (count-line-matches rope matcher start end :stop stop)
                    (count-first-per-line rope matcher start end :stop stop))
              (if status (return nil) count))))

(defun stretch-pattern-count (counter buffer stretches text regex &key every ends)
  "STRETCH-COUNT of TEXT in STRETCHES of BUFFER as it is typed at a question,
kept up to date by COUNTER as PATTERN-COUNT-UPDATE counts: its two values,
for READ-PATTERN's COUNT. REGEX tells whether TEXT is a regular expression;
$ matches before a carriage return where BUFFER's lines end with one and a
newline. Nothing is counted for a pattern that cannot be used; a pattern is
compiled once, however often it is asked about."
  (let ((filter (let ((last (pattern-count-filter counter)))
                  (if (and last (string= (first last) text) (eq (second last) regex))
                      (third last)
                      (third (setf (pattern-count-filter counter)
                                   (list text regex (and (plusp (length text))
                                                         (make-search-filter text :regex regex :crlf (buffer-crlf-p buffer))))))))))
    (pattern-count-update counter (and filter (list text regex every stretches (tuatara.buffer:buffer-text buffer)))
                          (loop for (start . end) in stretches sum (- end start))
                          (lambda (stop) (stretch-count buffer stretches filter :every every :ends ends :stop stop)))))

(defvar *search-in-loop* nil
  "True while the search's question reads keys, which takes in what a search
in the background finds; a search outside it (the next match after it ended)
is finished at once.")

(defvar *search-notice-delay* 1/10
  "Seconds a search runs before [searching] is added to its prompt;
shorter searches finish without it, so typing does not flicker.")
(defvar *search-echo* nil
  "The text a search last put in the echo area, outside its question (going
on through the matches after it, replacing), so that what comes after clears
it but keeps a message such as a count of replacements.")
(defvar *last-search-string* ""
  "The string of the most recent search, reused by an empty
\\[regex-search-forward] \\<search>\\[search-repeat-forward].")

(defvar *search-finished* nil
  "The last search ended with \\<search>\\[search-exit] at a match, a SEARCH-STATE:
right after it, \\[regex-search-forward] and \\[regex-search-backward] go through its
matches (SEARCH-NEXT-MATCH).")

(defvar *search-repeat-map* nil
  "The keys that go on through the matches of a search just ended with
\\<search>\\[search-exit], \\[search-repeat-forward] and \\[search-repeat-backward], while no other key is pressed.")

(defun search-line-of (state)
  "STATE's string as a SEARCH-LINE, cached: all pattern for a search or a
filter, read as pattern/replacement (PARSE-SEARCH-LINE) for a prompt opened
for replacing."
  (let ((key (list (search-state-string state) (search-state-regex state) *last-search-pattern*))
        (cache (search-state-parsed state)))
    (if (and cache (equal (car cache) key))
        (cdr cache)
        (cdr (setf (search-state-parsed state)
                   (cons key (let ((string (search-state-string state)) (regex (search-state-regex state)))
                               (if (search-state-preset state)
                                   (parse-search-line string regex)
                                   (make-search-line :pattern string)))))))))

(defun search-matcher (state)
  "How to find STATE's pattern, as SEARCH-LINE-MATCHER returns it (a MATCHER,
or NIL and why the pattern does not compile), compiled once per pattern."
  (let* ((line (search-line-of state))
         (key (list (search-line-pattern line) (search-state-regex state) (search-line-fold line (search-state-regex state))))
         (cache (search-state-compiled state)))
    (values-list
     (if (and cache (equal (car cache) key))
         (cdr cache)
         (cdr (setf (search-state-compiled state)
                    (cons key (multiple-value-list (search-line-matcher line (search-state-regex state) (current-buffer))))))))))

(defun search-problem (state)
  "Why STATE's string cannot be searched for or carried out yet, or NIL."
  (let ((line (search-line-of state)))
    (or (and (getf (search-state-preset state) :every-after-percent)
             (plusp (length (search-line-flags line)))
             "no flags here: a / in the pattern or the replacement is written \\/, and % in front replaces every match")
        (search-line-problem line)
        (and (plusp (length (search-line-pattern line)))
             (multiple-value-bind (matcher problem) (search-matcher state)
               (and (null matcher) problem))))))

(defun search-region (state)
  "The region active as STATE began, as (START . END), when it holds some text;
NIL when there was none or it was empty, as right after the mark is set."
  (let ((region (search-state-region state)))
    (and region (< (car region) (cdr region)) region)))

(defun search-replacing-p (state)
  "True when STATE replaces rather than searches: its prompt was opened for
replacing (\\[replace-regexp], \\[query-replace-regexp], \\[query-replace])."
  (and (search-state-preset state) t))

(defun search-flags (state)
  "The flags in force for STATE: those typed after the second / and those of the
command that opened the prompt; g after a leading % when that command takes %
for every match (:EVERY-AFTER-PERCENT, \\[replace-regexp])."
  (let ((line (search-line-of state)) (preset (search-state-preset state)))
    (concatenate 'string (search-line-flags line)
                 (or (getf preset :flags) "")
                 (if (and (getf preset :every-after-percent) (search-line-whole-buffer line)) "g" ""))))

(defun search-scope (state)
  "Values: where the text a replacement applies to starts and ends, words for
it, and when it is made of several stretches, those, (START . END) in order.
For \\[replace-regexp] and \\[query-replace-regexp] (:EVERY-AFTER-PERCENT)
what was selected as the prompt opened, as f takes it (SELECTED-RANGES: the
cursors, a block, banked lines, the selection), else the whole buffer after
a leading % and the line at point without. For the others the whole buffer
after a leading %, the region when one was active as the search began, the
whole buffer for a prompt opened for replacing, else the line the search
began on, its newline included."
  (let* ((b (current-buffer)) (rope (tuatara.buffer:buffer-text b)) (size (tuatara.buffer:buffer-size b))
         (region (search-region state))
         (preset (search-state-preset state)))
    (cond ((getf preset :every-after-percent)
           (let ((selected (getf preset :selected)))
             (cond (selected
                    (values (min size (car (first selected))) (min size (cdr (car (last selected))))
                            (format nil "in ~a" (getf preset :selected-words))
                            (and (rest selected) selected)))
                   ((search-line-whole-buffer (search-line-of state)) (values 0 size "in the buffer"))
                   (t (let ((origin (min (search-state-origin state) size)))
                        (values (tuatara.text:rope-line-start rope origin) (tuatara.text:rope-line-end rope origin)
                                "on this line"))))))
          ((search-line-whole-buffer (search-line-of state))
           (values 0 size "in the buffer"))
          (region (values (min (car region) size) (min (cdr region) size) "in the region"))
          ((search-state-preset state) (values 0 size "in the buffer"))
          (t (let ((origin (min (search-state-origin state) size)))
               (values (tuatara.text:rope-line-start rope origin)
                       (min size (1+ (tuatara.text:rope-line-end rope origin)))
                       "on this line"))))))

(defun search-bounds (state &optional (buffer (current-buffer)))
  "Values: the start and end of the text STATE searches in BUFFER, the current
one unless given, and words for them when they are not the whole buffer: the
scope of a replacement; for a search, the region active as it began."
  (let ((region (search-region state))
        (size (tuatara.buffer:buffer-size buffer)))
    (cond ((search-replacing-p state) (search-scope state))
          (region (values (min (car region) size) (min (cdr region) size) "in the region"))
          (t (values 0 size nil)))))

(defun search-pieces (state)
  "The stretches, (START . END) in order, that STATE's replacement is limited
to when it is made of several (SEARCH-SCOPE); NIL for one."
  (and (search-replacing-p state) (nth-value 3 (search-scope state))))

(defun in-pieces-p (pieces start end)
  "True when the text from START to END lies within one of PIECES."
  (some (lambda (piece) (and (<= (car piece) start) (<= end (cdr piece)))) pieces))

(defun pieces-matches-only (rope pieces matches per-line)
  "MATCHES, in order, that lie within PIECES, also in order; with PER-LINE the
first on each line of each piece only. One walk through both lists."
  (let ((runs '()) (run '()) (piece nil))
    (flet ((close-run ()
             (when run (push (if per-line (first-per-line rope (nreverse run)) (nreverse run)) runs))
             (setf run '())))
      (dolist (m matches)
        ;; the pieces ending before this match can hold none of the later ones either
        (loop while (and pieces (< (cdr (first pieces)) (second m))) do (pop pieces))
        (when (and pieces (<= (car (first pieces)) (first m)))
          (unless (eq piece (first pieces)) (close-run) (setf piece (first pieces)))
          (push m run)))
      (close-run)
      (loop for r in (nreverse runs) append r))))

(defun search-keeps-region-p (state)
  "True when the region active as STATE began stays live: a search begun right
after the mark was set, the region empty, extends it to the match, so that a
search selects the text up to what it finds. A search or replacement limited
to the region shows it as its scope instead."
  (and (search-state-region state)
       (not (search-replacing-p state))
       (null (search-region state))))

(defun search-prompt (state &optional doing)
  "The prompt of STATE's question: what the search does, then, in brackets,
what it is DOING (searching, replacing), or else any problem with its string
\(after \\<search>\\[search-exit] was refused for it, how to go on), or that a
slow pattern is still being searched for; which match is current
and how many there are shows before it (SEARCH-COUNT-WORDS)."
  (let* ((replacing (search-replacing-p state))
         (flags (search-flags state))
         (words (append (when (and (search-state-failing state) (not (search-count-shown-p state))) '("failing"))
                        (when (and (search-state-wrapped state) (not replacing)) '("wrapped"))
                        (list (pattern-kind-words (search-state-regex state)))
                        (if replacing
                            (list (if (find #\n flags) "count" "replace") (nth-value 2 (search-scope state)))
                            (let ((scope (nth-value 2 (search-bounds state))))
                              (list* "search" (if (search-state-direction state) "forward" "backward")
                                     (and scope (list scope)))))))
         (asking (and replacing (find #\c flags) (not (find #\n flags)))))
    (when (search-state-filtering state)
      (setf words (list (pattern-kind-words (search-state-regex state)) "filter")))
    (format nil "~@(~{~a~^ ~}~)~:[~;, asking at each~]~@[ [~a]~]: "
            words asking
            (or doing
                (let ((problem (search-problem state)))
                  (and problem
                       (if (search-state-refused state)
                           (refused-pattern-words problem (substitute-command-keys "\\<search>\\[search-abort]"))
                           problem)))
                (and (search-state-background state)
                     (or (search-state-slow state) (search-state-exit-when-found state) (not (search-count-shown-p state)))
                     (format nil "~:[~;slow pattern: still ~]~:[searching~;searching, then done~]"
                             (search-state-slow state) (search-state-exit-when-found state)))
                (and (search-state-filtering state) (filtering-slowly-p (search-state-filtering state))
                     "slow pattern: still filtering")))))

(defun search-count-shown-p (state)
  "True when STATE's question shows its count before its prompt, which then
says that it fails (0/12 past the last of 12, 0/0 with none, 0 for a
replacement), in place of Failing and of searching in brackets."
  (and *show-completion-count* *search-in-loop* (eq state *search*)))

(defun show-search-prompt (state &optional doing)
  "Show STATE's prompt (SEARCH-PROMPT) with what it is DOING: in its
question while that is open, else in the echo area with its string."
  (let ((editor *editor*))
    (if (and (eq state *search*) (editor-minibuffer-active editor))
        (setf (editor-minibuffer-prompt editor) (search-prompt state doing))
        (setf *search-echo* (concatenate 'string (search-prompt state doing) (search-state-string state))
              (editor-message editor) *search-echo*))))

(defun search-stopper (state &optional (doing "searching") budget)
  "A function for the search engine's STOP argument: once the work has run
*SEARCH-NOTICE-DELAY* seconds it shows that it is DOING it, when DOING is
given; each call answers
whether a key is waiting, or with BUDGET, seconds, whether they have passed.
While recorded keys play only \\`C-g' stops it, which stops the play too,
other keys waiting till the play is over (QUIT-WHILE-PLAYING-P)."
  (let ((editor *editor*) (shown nil)
        (show-at (tuatara.runtime:deadline-in *search-notice-delay*))
        (until (and budget (tuatara.runtime:deadline-in budget))))
    (lambda ()
      (when (and doing (not shown) (tuatara.runtime:deadline-passed-p show-at))
        (setf shown t)
        (show-search-prompt state doing)
        (redisplay-or-say editor))
      (if (keys-playing-p)
          (quit-while-playing-p)
          (or (key-waiting-p)
              (and until (tuatara.runtime:deadline-passed-p until)))))))

(defun cancel-background-search (state)
  "Stop STATE's search in the background, when one goes on; what it finds is
not taken in."
  (let ((background (search-state-background state)))
    (when background
      (tuatara.runtime:stop-work (second background))
      (setf (search-state-background state) nil
            (search-state-exit-when-found state) nil))))

(defun search-in-background (state rope matcher from limit forward bound)
  "Go on with STATE's search on a thread of its own: MATCHER in ROPE from FROM
towards LIMIT, as MATCHER-SEARCH takes them; the editor woken when it is done,
and the match taken in by SEARCH-TAKE-IN."
  (cancel-background-search state)
  (let ((cancel (list nil)))
    (setf (search-state-background state)
          (list (tuatara.runtime:start-background-result
                 (lambda ()
                   (multiple-value-bind (start end)
                       (matcher-search rope matcher from limit :forward forward :bound bound :stop (lambda () (car cancel)))
                     (list :done start end)))
                 "tuatara search" (editor-job-waker) cancel)
                cancel))))

(defun search-take-in (state)
  "Take in what STATE's search in the background found, once it is done: the
match, point moved to it, or the search failing. True when it was taken in."
  (let* ((background (search-state-background state))
         (found (and background (car (first background)))))
    (when found
      (setf (search-state-background state) nil)
      (destructuring-bind (kind start &optional end) found
        (cond ((eq kind :error) (message "Search failed: ~a" start) (setf (search-state-failing state) t))
              (start (setf (search-state-match state) (cons start end) (search-state-failing state) nil)
                     (goto (if (search-state-direction state) end start)))
              (t (setf (search-state-failing state) t))))
      t)))

(defun search-count-key (state)
  "What STATE's matches are counted for: the pattern as it is read and the
text searched."
  (list (search-key state) (multiple-value-list (search-bounds state))))

(defun stop-search-count (state)
  "Stop counting STATE's matches in the background, when that goes on."
  (let ((counting (search-state-counting state)))
    (when counting
      (tuatara.runtime:stop-work (third counting))
      (setf (search-state-counting state) nil))))

(defvar *search-count-near* (* 256 1024)
  "Octets between the match last counted for and the current one within which
the place of the current match is counted at once, on the editor's thread;
farther apart, it is counted again in the background.")

(defun search-pause-left (state)
  "Seconds still to wait before STATE's matches are counted (TYPING-PAUSE-LEFT
for the text it searches), or NIL."
  (multiple-value-bind (low high) (search-bounds state)
    (typing-pause-left (- high low))))

(defun search-count-place (state)
  "Which match STATE's search is at, from its count (SEARCH-STATE-COUNT), or
NIL when that is to be counted again: the place of the match last counted
for moved by the matches counted between the two, while they stand within
*SEARCH-COUNT-NEAR* of each other."
  (let ((count (search-state-count state))
        (start (car (search-state-match state))))
    (when (equal (first count) (search-count-key state))
      (destructuring-bind (total anchor before) (rest count)
        (declare (ignore total))
        (cond ((null anchor) nil)
              ((= start anchor) (1+ before))
              ((<= (abs (- start anchor)) *search-count-near*)
               (let* ((rope (tuatara.buffer:buffer-text (current-buffer)))
                      (between (count-line-matches rope (search-matcher state) (min start anchor) (max start anchor)
                                                   :bound (nth-value 1 (search-bounds state))))
                      (before (if (> start anchor) (+ before between) (- before between))))
                 (setf (third (rest count)) before (second (rest count)) start)
                 (1+ before))))))))

(defun update-search-count (state)
  "Keep STATE's count of matches (SEARCH-STATE-COUNT) up to date while it is a
search standing at a match, or failing (how many there are then, wherever
they stand): take in a count done in the background, and start
one, on a thread of its own, when the pattern or the text searched has changed
since, or the match is far from the one counted for (SEARCH-COUNT-PLACE);
going from match to match counts only the matches between. Nothing is kept
for each match, so millions of them take no memory. A count of a pattern
changed waits till typing pauses (*TYPING-PAUSE*). True while one goes on,
or the seconds still to wait."
  (when (search-replacing-p state)
    (return-from update-search-count (update-replace-count state)))
  (let* ((counting (search-state-counting state))
         (result (and counting (car (second counting)))))
    (when result
      (setf (search-state-counting state) nil)
      (unless (eq (first result) :error)
        (destructuring-bind (total anchor before) result
          (setf (search-state-count state)
                (list (first (first counting)) (or total (second (search-state-count state))) anchor before))))))
  (let ((match (search-state-match state))
        (failing (search-state-failing state)))
    (if (or (search-replacing-p state) (search-state-filtering state) (search-state-background state)
            (search-problem state) (and (null match) (not failing)))
        (progn (stop-search-count state) nil)
        (let* ((key (search-count-key state))
               (start (and (not failing) (car match)))
               (known (equal key (first (search-state-count state)))))
          (cond ((and known (or failing (search-count-place state))) nil)
                ((equal (list key start) (first (search-state-counting state))) t)
                ((and (not known) (search-pause-left state))
                 (stop-search-count state)
                 (search-pause-left state))
                (t (stop-search-count state)
                   (let ((rope (tuatara.buffer:buffer-text (current-buffer)))
                         (matcher (search-matcher state))
                         (cancel (list nil)))
                     (multiple-value-bind (low high) (search-bounds state)
                       (setf (search-state-counting state)
                             (list (list key start)
                                   (tuatara.runtime:start-background-result
                                    (lambda ()
                                      (block counting
                                        (flet ((count-to (to)
                                                 ;; the matches of one scan from LOW to HIGH that start before TO
                                                 (multiple-value-bind (count status)
                                                     (count-line-matches rope matcher low to :bound high
                                                                                             :stop (lambda () (car cancel)))
                                                   (if status (return-from counting (list :error status)) count))))
                                          (if (null start)
                                              (list (count-to high) nil nil)
                                              ;; the total stands while only the match moved
                                              (list (and (not known) (count-to high)) start (count-to start))))))
                                    "tuatara search count" (editor-job-waker) cancel)
                                   cancel))))
                   t))))))

(defun update-replace-count (state)
  "Keep the count of the matches STATE's replacement replaces up to date
\(PATTERN-COUNT-UPDATE): with the g flag (a leading % for \\[replace-regexp])
every match in its scope, else the first on each line. True while it counts,
or the seconds still to wait."
  (let ((counter (or (search-state-tally state) (setf (search-state-tally state) (make-pattern-count)))))
    (multiple-value-bind (low high words pieces) (search-scope state)
      (declare (ignore words))
      (let* ((matcher (and (not (search-problem state))
                           (plusp (length (search-line-pattern (search-line-of state))))
                           (search-matcher state)))
             (every (and (find #\g (search-flags state)) t))
             (stretches (or pieces (list (cons low high))))
             (rope (tuatara.buffer:buffer-text (current-buffer))))
        (multiple-value-bind (going words)
            (pattern-count-update counter (and matcher (list (search-key state) stretches every rope))
                                  (loop for (a . z) in stretches sum (- z a))
                                  (lambda (stop) (count-in-stretches rope matcher stretches :every every :stop stop)))
          (setf (search-state-count-words state) words)
          going)))))

(defun search-count-words (state)
  "What STATE's question shows before its prompt, as a completing read shows
its count: for a search which match it is at and how many there are,
\"3/12\" (0/12 past the last, 0/0 with none); for a filter what it leaves of
the list or file, \"100/300\" (FILTER-COUNT-FIGURES); for a replacement how
many matches it replaces, \"12\" (UPDATE-REPLACE-COUNT); until they are
counted again what it showed last; \"\" while there is none to show, keeping
its place."
  (let ((count (search-state-count state))
        (match (search-state-match state))
        (filtering (search-state-filtering state)))
    (if (search-replacing-p state)
        (search-state-count-words state)
        (setf (search-state-count-words state)
              (if filtering
                  (cond ((zerop (length (search-state-string state))) "")
                        ((filter-count-figures filtering))
                        (t (or (search-state-count-words state) "")))
                  (let ((place (and match (not (search-state-failing state)) (search-count-place state))))
                    (cond ((search-problem state) "")
                          ((search-state-failing state)
                           ;; past the last match, or none at all: how many there are
                           (if (equal (first count) (search-count-key state)) (format nil "0/~d" (second count)) ""))
                          ((null match) "")
                          (place (format nil "~d/~d" place (second count)))
                          (t (or (search-state-count-words state) "")))))))))

(defun shown-match-search (buffer rope matcher from limit &rest keys &key (forward t) &allow-other-keys)
  "MATCHER-SEARCH in ROPE, BUFFER's text, passing over the matches in the
lines search filters hide there: the search goes on past each stretch they
hide that a match starts in."
  (loop
    (multiple-value-bind (start end) (apply #'matcher-search rope matcher from limit keys)
      (multiple-value-bind (fold filtered) (and (integerp start) (tuatara.display:fold-hiding buffer start))
        (if (and filtered (if forward (< (cdr fold) limit) (> (car fold) limit)))
            (setf from (if forward (cdr fold) (car fold)))
            (return (if filtered (values nil nil) (values start end))))))))

(defun pieces-search (state buffer rope matcher from limit forward bound stop)
  "SHOWN-MATCH-SEARCH from FROM towards LIMIT, passing over the matches outside
the stretches STATE's replacement is limited to (SEARCH-PIECES)."
  (let ((pieces (search-pieces state)))
    (loop
      (multiple-value-bind (start end)
          (shown-match-search buffer rope matcher from limit :forward forward :bound bound :stop stop)
        (if (or (null pieces) (not (integerp start)) (in-pieces-p pieces start end))
            (return (values start end))
            (let ((next (if forward
                            (find-if (lambda (piece) (> (cdr piece) start)) pieces)
                            (find-if (lambda (piece) (< (car piece) end)) pieces :from-end t))))
              (cond ((null next) (return (values nil nil)))
                    (forward (setf from (max (1+ start) (car next))))
                    (t (setf from (min (1- end) (cdr next)))))))))))

(defun search-find (state from)
  "Search for STATE's pattern from byte FROM in its direction, within a
replacement's scope when there is one; update the match and failing flag and
move point to the match. A search that gave way to a waiting key changes
nothing but STOPPED. A pattern that does not compile keeps the last match. A
search longer than a moment, or held up by one line longer than
TUATARA.RUNTIME:*SLOW-CALL-SECONDS* (a slow pattern, SLOW then), goes on in
the background while the question is open; while recorded keys play it runs
here to its end, \\`C-g' stopping it and the play."
  (let* ((b (current-buffer)) (rope (tuatara.buffer:buffer-text b)) (line (search-line-of state)))
    (setf (search-state-stopped state) nil)
    (cancel-background-search state)
    (cond ((search-state-filtering state) (filter-list-as-typed state))
          ((search-problem state) nil)
          ((zerop (length (search-line-pattern line)))
           (setf (search-state-match state) nil (search-state-failing state) nil))
          (t
           (let ((matcher (search-matcher state)))
             (multiple-value-bind (low high) (search-bounds state)
               (let ((from (max low (min from high)))
                     (forward (search-state-direction state)))
                 (multiple-value-bind (start end)
                     (tuatara.runtime:stoppable-work ((list nil) :slow (and (not (keys-playing-p)) tuatara.runtime:*slow-call-seconds*))
                       (pieces-search state b rope matcher from (if forward high low) forward high
                                      (search-stopper state (and (not (search-count-shown-p state)) "searching")
                                                      (and *search-in-loop* (not (keys-playing-p))
                                                           *search-foreground-seconds*))))
                   (setf (search-state-slow state) (eq end :slow))
                   (cond ((and (member end '(:stopped :slow)) *search-in-loop*
                               (not (key-waiting-p)) (not (keys-playing-p)))
                          ;; longer than a moment, no key waiting: on in the background
                          (search-in-background state rope matcher from (if forward high low) forward high))
                         ((and (eq end :slow) (not *search-in-loop*))
                          (message "The pattern is slow here: \\[regex-search-forward] searches for it in the background"))
                         ((member end '(:stopped :slow)) (setf (search-state-stopped state) from))
                         (start (setf (search-state-match state) (cons start end) (search-state-failing state) nil)
                                (goto (if forward end start)))
                         (t (setf (search-state-failing state) t)))))))))))

(defun search-start-offset (state)
  "Where STATE's search begins while it has no match: the origin; for a search
limited to the region, the region's start going forward and its end going back."
  (if (and (search-region state) (not (search-replacing-p state)) (not (search-keeps-region-p state)))
      (multiple-value-bind (low high) (search-bounds state)
        (if (search-state-direction state) low high))
      (search-state-origin state)))

(defun search-again (state)
  "Search after STATE's string changed: from the current match's start (going
forward) or end (going backward), or when there is none from where the search
begins (SEARCH-START-OFFSET). For a replacement, from the origin within its
scope, then from the scope's other end."
  (let ((match (search-state-match state)))
    (cond ((search-replacing-p state)
           (setf (search-state-wrapped state) nil)
           (search-find state (search-state-origin state))
           (when (and (search-state-failing state) (not (search-state-stopped state)))
             (multiple-value-bind (low high) (search-bounds state)
               (search-find state (if (search-state-direction state) low high)))))
          ((null match) (search-find state (search-start-offset state)))
          ((search-state-direction state) (search-find state (car match)))
          (t (search-find state (cdr match))))))

(defun search-key (state)
  "What decides where STATE's matches are: the pattern, how it is read, the
scope and the case flags. Typing into the replacement changes none of it."
  (let ((line (search-line-of state)))
    (list (search-line-pattern line) (search-state-regex state)
          (and (search-replacing-p state) t) (search-line-whole-buffer line)
          (search-line-fold line (search-state-regex state)))))

(defvar *search-lit* nil
  "The last search that ended at a match, whose matches stay lit after
\\<search>\\[search-exit] while *SEARCH-LIT-SHOWN*: (STATE . BUFFER), or NIL.")

(defvar *search-lit-shown* nil
  "True while the matches of the last search (*SEARCH-LIT*) are lit: from its
\\<search>\\[search-exit] till \\[keyboard-quit]; \\[toggle-search-highlight] turns it.")

(defun search-highlights (editor window rows)
  "Highlight function: while a search runs, its matches in ROWS (SEARCH-RANGES),
the current one too, and outside its scope dimmed. Only windows showing the
buffer searched show them."
  (declare (ignore editor))
  (let ((state *search*))
    (when (and state (plusp (length rows)) (eq (tuatara.display:window-buffer window) (current-buffer)))
      (search-ranges state window rows :current t :scope t))))

(defun lit-search-highlights (editor window rows)
  "Highlight function: after a search ended at a match, its matches in ROWS
of the windows showing its buffer stay lit (*SEARCH-LIT*) until
\\[keyboard-quit]; while \\[regex-search-forward] and \\[regex-search-backward] go on through them, the
match they are on as the current one."
  (let ((lit *search-lit*))
    (when (and lit *search-lit-shown* (null *search*) (plusp (length rows))
               (eq (tuatara.display:window-buffer window) (cdr lit)))
      (search-ranges (car lit) window rows :current (going-through-lit-search-p editor)))))

(defun going-through-lit-search-p (editor)
  "True while \\[regex-search-forward] and \\[regex-search-backward] go on through the matches of the
last search, still lit, right after it ended (*SEARCH-FINISHED*)."
  (let ((lit *search-lit*))
    (and lit *search-lit-shown* (eq *search-finished* (car lit))
         (eq (editor-repeat-map editor) *search-repeat-map*))))

(defun search-cursor-colour (editor)
  "The cursor's colour while it stands on the current match of a search one
character long, as \\[regex-search-backward] leaves it on a match's first character, where it
would hide all of it: the ground of :search-current; else NIL, a longer
match showing beside the cursor."
  (let* ((state (cond (*search*)
                      ((and (going-through-lit-search-p editor) (eq (cdr *search-lit*) (current-buffer)))
                       (car *search-lit*))))
         (match (and state (search-state-match state))))
    (when (and match (= (point) (car match)) (< (car match) (cdr match))
               (= (cdr match) (tuatara.buffer:buffer-forward-char (current-buffer) (car match) 1)))
      (tuatara.display:face-ground :search-current))))

(defun forget-lit-search-of (buffer)
  "Forget the last search when it was in BUFFER, which is being killed, so that
its text is let go."
  (when (and *search-lit* (eq (cdr *search-lit*) buffer))
    (setf *search-lit* nil *search-lit-shown* nil)))

(defun unlight-search ()
  "Let the matches of the last search go unlit, \\[toggle-search-highlight] lighting them again."
  (setf *search-lit-shown* nil))

(define-command toggle-search-highlight ()
  "Light the matches of the last search that ended at a match again, in its
buffer, or take them away when they are lit, as \\[keyboard-quit] does."
  (cond ((null *search-lit*) (message "No search to light the matches of"))
        ((not (live-buffer-p (cdr *search-lit*)))
         (setf *search-lit* nil)
         (message "The buffer of the last search is gone"))
        (t (setf *search-lit-shown* (not *search-lit-shown*))
           (message "Matches of ~a ~:[unlit~;lit~]~@[ in ~a~]"
                    (search-state-string (car *search-lit*)) *search-lit-shown*
                    (unless (eq (cdr *search-lit*) (current-buffer))
                      (tuatara.buffer:buffer-name (cdr *search-lit*)))))))

(defun search-ranges (state window rows &key current scope)
  "STATE's matches in ROWS of WINDOW as highlight ranges: with CURRENT its
current match with :search-current, the others with :search-other. For a
replacement, only the matches it will replace: those in its scope, and
without the g flag the first on each line; while its question is open
\(REPLACE-GOING-P), with :replace-going, what takes their place shown beside
them once the replacement is typed (SHOW-REPLACE-PREVIEW), the one asked at
first still :search-current when each is asked at. With SCOPE, when the search or
replacement is limited to a line or the region, the text outside it is
shown with :search-outside-scope."
  (let* ((rope (tuatara.buffer:buffer-text (tuatara.display:window-buffer window)))
         (line (search-line-of state))
         (replacing (search-replacing-p state))
         (per-line (and replacing (not (find #\g (search-flags state)))))
         (from (tuatara.display:row-start (aref rows 0)))
         (to (tuatara.display:row-end (aref rows (1- (length rows)))))
         (match (and current (search-state-match state)))
         (preview (replace-going-p state))
         (ranges '()))
    ;; while the pattern does not compile (a bracket still open) the last match stays lit;
    ;; a pattern too slow for this text lights the current match alone
    (when (and (not (search-problem state)) (not (search-state-slow state)) (plusp (length (search-line-pattern line))))
      (let ((matcher (search-matcher state)))
        (multiple-value-bind (low high) (search-bounds state (tuatara.display:window-buffer window))
          ;; only what the rows show: a line of megabytes cut short on screen, the part of it shown
          (multiple-value-bind (found cut)
              (tuatara.runtime:stoppable-work ((list nil) :slow tuatara.runtime:*slow-call-seconds*)
                (loop for (shown-start . shown-end) in (tuatara.display:shown-ranges rows :buffer (tuatara.display:window-buffer window))
                      for scan-from = (max low (if per-line (tuatara.text:rope-line-start rope shown-start) shown-start))
                      for scan-to = (min high shown-end)
                      when (< scan-from scan-to)
                        append (if (eq (matcher-kind matcher) :literal)
                                   (let ((n (tuatara.text:string-octet-length (matcher-pattern matcher))))
                                     (map 'list (lambda (p) (list p (+ p n)))
                                          (tuatara.syntax:search-all rope (matcher-pattern matcher) :start scan-from
                                                                                                    :end (min high (+ scan-to n))
                                                                                                    :case-fold (matcher-fold matcher)
                                                                                                    :threads 1 :limit (+ 2 (- scan-to scan-from)))))
                                   (tuatara.syntax:regex-search-all rope (matcher-scanner matcher) :start scan-from :end scan-to
                                                                                                   :bound high :limit (+ 2 (- scan-to scan-from))))))
            ;; too slow on the lines shown: the current match alone is lit from now on
            (when (eq cut :slow) (setf (search-state-slow state) t))
            ;; in order; a line's start looked back to for its first match finds some twice
            (let ((found (let ((last -1))
                           (loop for m in found
                                 when (> (first m) last) collect m and do (setf last (first m))))))
              (dolist (m (let ((pieces (search-pieces state)))
                           (cond (pieces (pieces-matches-only rope pieces found per-line))
                                 (per-line (first-per-line rope found))
                                 (t found))))
                (unless (or (<= (second m) from) (and match (= (first m) (car match))))
                  (push (list (first m) (second m) (if preview :replace-going :search-other)) ranges))))))))
    (when match
      (push (list (car match) (cdr match) (if (and preview (not (find #\c (search-flags state)))) :replace-going :search-current))
            ranges))
    ;; dim what lies outside a limited scope; first in the list, so matches win
    (when scope
      (multiple-value-bind (low high) (search-bounds state (tuatara.display:window-buffer window))
        (when (< high to) (push (list (max from high) to :search-outside-scope) ranges))
        (when (< from low) (push (list from (min low to) :search-outside-scope) ranges))
        (loop for (a b) on (search-pieces state)
              when (and b (< (cdr a) (car b)) (< (cdr a) to) (> (car b) from))
                do (push (list (max from (cdr a)) (min to (car b)) :search-outside-scope) ranges))))
    ranges))

(defun replace-going-p (state)
  "True while STATE's question for replacing is open, its pattern usable: its
matches show as what the replacement takes away (:replace-going), from the
first key of the pattern on."
  (let ((line (search-line-of state)))
    (and (search-replacing-p state) (editor-minibuffer-active *editor*)
         (plusp (length (search-line-pattern line)))
         (not (search-problem state)))))

(defun replace-preview-p (state)
  "True when STATE's replacement is being typed (REPLACE-GOING-P), its pattern
quick enough here to show beside each match what takes its place
\(REPLACE-PREVIEW-TEXTS)."
  (and (replace-going-p state) (search-line-replacement (search-line-of state))
       (not (search-state-slow state))))

(defun replace-preview-texts (state buffer start end)
  "What STATE's replacement puts in place of each of its matches in BUFFER
from START to END, shown beside the match as no text of the buffer: each
(AT AT TEXT :REPLACE-COMING), AT the match's end, in order, as a
:DISPLAY-REPLACEMENTS-FUNCTION gives them. A line break in it shows as ⏎."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (line (search-line-of state))
         (regex (search-state-regex state))
         (matcher (search-matcher state))
         (per-line (not (find #\g (search-flags state))))
         (replacement-for (replacement-function (search-line-replacement line) regex matcher
                                               (tuatara.buffer:buffer-newline buffer))))
    (multiple-value-bind (low high) (search-bounds state buffer)
      (let ((from (max low (if per-line (tuatara.text:rope-line-start rope start) start)))
            (to (min high end)))
        (when (< from to)
          (multiple-value-bind (found status)
              (tuatara.runtime:stoppable-work ((list nil) :slow tuatara.runtime:*slow-call-seconds*)
                (line-matches rope matcher regex from to))
            (if (eq status :slow)
                (progn (setf (search-state-slow state) t) '())
                (let ((pieces (search-pieces state)))
                  (loop for m in (cond (pieces (pieces-matches-only rope pieces found per-line))
                                       (per-line (first-per-line rope found))
                                       (t found))
                        when (and (>= (second m) start) (<= (second m) end))
                          collect (list (second m) (second m)
                                        (substitute #\⏎ #\Newline (funcall replacement-for m))
                                        :replace-coming))))))))))

(defun show-replace-preview (state buffer)
  "Have BUFFER's display show, while STATE's replacement is typed, what takes
the place of each match beside it (REPLACE-PREVIEW-TEXTS), as well as what its
mode shows otherwise; returns a function that puts the display back."
  (let ((before (tuatara.buffer:buffer-local buffer :display-replacements-function)))
    (setf (tuatara.buffer:buffer-local buffer :display-replacements-function)
          (lambda (b start end)
            (let ((own (and (eq *search* state) (replace-preview-p state)
                            (replace-preview-texts state b start end)))
                  (theirs (and before (funcall before b start end))))
              (if (and own theirs)
                  (merge 'list (copy-list theirs) own #'< :key #'car)
                  (or own theirs)))))
    (lambda () (setf (tuatara.buffer:buffer-local buffer :display-replacements-function) before))))

(defun search-push-history (state)
  "Remember the state before a change, to go back to as what was typed is taken back."
  (push (list (search-state-string state) (search-state-regex state) (search-state-direction state)
              (search-state-match state) (search-state-failing state) (search-state-wrapped state)
              (search-state-stopped state) (point))
        (search-state-history state)))

(defun search-set-string (state string &key afresh)
  "Make STRING STATE's string and search again, unless only the replacement
or the flags other than i and I changed; AFRESH, from where the search
began rather than from its match."
  (search-push-history state)
  (let ((before (search-key state)))
    (when afresh
      (setf (search-state-match state) nil (search-state-wrapped state) nil))
    (setf (search-state-string state) string)
    (unless (and (equal before (search-key state)) (not (search-state-stopped state)))
      (search-again state))))

(defun search-text-changed (state text)
  "The text typed in STATE's question is now TEXT: taken back to a string the
search had before, the search goes back to that state, its match and point;
added to at its end, the search goes on from its match; changed elsewhere,
it searches for TEXT from where it began (SEARCH-SET-STRING)."
  (setf (search-state-refused state) nil
        *last-typed* (get-internal-real-time))
  (let ((old (search-state-string state)))
    (cond ((and (< (length text) (length old)) (string= text old :end2 (length text))
                (find text (search-state-history state) :key #'first :test #'string=))
           (loop until (string= (search-state-string state) text)
                 do (search-step-back state)))
          ((and (<= (length old) (length text)) (string= old text :end2 (length old)))
           (search-set-string state text))
          (t (search-set-string state text :afresh t)))))

(defun search-type (text)
  "Add TEXT to the end of what is typed in the search's question, as though
typed there."
  (setf (editor-minibuffer-point *editor*) (length (editor-minibuffer-text *editor*)))
  (minibuffer-insert text))

(defun search-repeat (state forward)
  "Go to the next match in direction FORWARD from beyond the current one, the
search keeping its own direction for what is typed after; with an empty
string, reuse the last search string; when failing, wrap around to the other
end (of a replacement's scope, when there is one)."
  (search-push-history state)
  (when (zerop (length (search-state-string state)))
    (setf (search-state-string state) *last-search-string*)
    ;; shown in its question, to be edited there
    (when (and (eq state *search*) (editor-minibuffer-active *editor*))
      (setf (editor-minibuffer-text *editor*) (copy-seq *last-search-string*)
            (editor-minibuffer-point *editor*) (length *last-search-string*))))
  (let ((match (search-state-match state))
        (own (search-state-direction state)))
    (setf (search-state-direction state) forward)
    (unwind-protect
         (multiple-value-bind (low high) (search-bounds state)
           (cond ((search-state-failing state)
                  (setf (search-state-wrapped state) t)
                  (search-find state (if forward low high)))
                 ((and match forward (= (car match) (cdr match)))
                  ;; from an empty match, on from the character after it: never the same one again
                  (let ((next (tuatara.buffer:buffer-forward-char (current-buffer) (cdr match) 1)))
                    (if (and (> next (cdr match)) (<= next high))
                        (search-find state next)
                        (setf (search-state-failing state) t))))
                 (match (search-find state (if forward (cdr match) (car match))))
                 (t (search-find state (search-start-offset state)))))
      (setf (search-state-direction state) own))))

(defun search-step-back (state)
  "Undo the last change to the search, back to the state before it. A state
whose search the next key cut short is searched again. A list being narrowed
is narrowed again by the shorter pattern, point staying on its entry: the
list's text has changed since, so where point was then means nothing now."
  (let ((previous (pop (search-state-history state))))
    (when previous
      (destructuring-bind (string regex direction match failing wrapped stopped point) previous
        (setf (search-state-string state) string (search-state-regex state) regex
              (search-state-direction state) direction (search-state-match state) match
              (search-state-failing state) failing (search-state-wrapped state) wrapped
              (search-state-stopped state) nil)
        (cond ((search-state-filtering state) (filter-list-as-typed state))
              (t (goto point)
                 (when stopped (search-find state stopped))))))))

(defun search-toggle (state)
  "Switch STATE between a regular expression and literal text, and search again."
  (search-push-history state)
  (setf (search-state-regex state) (not (search-state-regex state)))
  (search-again state))

(defun search-remember (state)
  "Keep STATE's string and pattern for the next search:
\\[regex-search-forward] \\<search>\\[search-repeat-forward] and //new."
  (setf *last-search-string* (search-state-string state))
  (let ((pattern (search-line-pattern (search-line-of state))))
    (when (plusp (length pattern))
      (setf *last-search-pattern* (cons pattern (search-state-regex state))))))

(defun search-show-match (state)
  "Open the folds that hide any of STATE's match, so that it and point are
seen; the other folds stay as they are."
  (let ((match (search-state-match state))
        (folds (tuatara.display:folds-now (current-buffer))))
    (when (and match folds (tuatara.display:folds-hiding folds (car match) (max (cdr match) (1+ (car match)))))
      (tuatara.display:unfold-region (current-buffer) (car match) (max (cdr match) (1+ (car match)))))))

(defun search-ready-to-end-p (state)
  "Whether \\<search>\\[search-exit] ends STATE's question now. Opened for
replacing with only a pattern typed, it types the / before the replacement
instead; a search, filter, count or replacement whose pattern
cannot be used goes on, its prompt saying why and how to go on; a search still going on in
the background ends once it has found its match; one cut short by this key is
finished first."
  (let ((line (search-line-of state)))
    (cond ((search-problem state)
           (setf (search-state-refused state) t)
           nil)
          ((search-state-filtering state) t)
          ((and (search-state-preset state) (null (search-line-replacement line)))
           (search-type "/")
           nil)
          ((search-line-replacement line) t)
          ((search-state-background state)
           (setf (search-state-exit-when-found state) t)
           nil)
          ((search-state-stopped state)
           (search-find state (search-state-stopped state))
           (cond ((search-state-background state) (setf (search-state-exit-when-found state) t) nil)
                 ((search-state-stopped state) nil)
                 (t t)))
          (t t))))

(defun search-carry-out (state)
  "Finish STATE once its question has ended with \\<search>\\[search-exit]
\(SEARCH-READY-TO-END-P): a filter is kept on its list; a replacement is
carried out (RUN-SUBSTITUTION); a search ends at its match, a fold hiding it
opened, or says it found nothing."
  (let ((line (search-line-of state)))
    (cond ((search-state-filtering state)
           (search-remember state)
           (keep-typed-filter (search-state-filtering state)))
          ((search-line-replacement line)
           (multiple-value-bind (low high words pieces) (search-scope state)
             (search-remember state)
             ;; the edits move text under the last match; the preview of what is left stays lit
             (setf (search-state-match state) nil)
             (run-substitution (current-buffer) line (search-state-regex state) low high words
                               (search-state-origin state) :flags (search-flags state) :pieces pieces
                               :stop (search-stopper state "replacing"))))
          (t (search-remember state)
             (search-show-match state)
             ;; the mark goes where the search began when it moved, unless it holds the far
             ;; end of a selection the search extended; the mark before on the mark ring
             (unless (or (search-keeps-region-p state) (= (point) (search-state-origin state)))
               (push-mark :position (search-state-origin state) :quiet t)
               (message "Mark saved where search started"))
             (let ((scope (nth-value 2 (search-bounds state))))
               (cond ((not (search-state-failing state)))
                     (scope (message "No match for ~a ~a" (search-state-string state) scope))
                     (t (message (if (search-state-direction state)
                                     "No match for ~a after point: \\<search>\\[search-repeat-forward] in the search goes on from the start"
                                     "No match for ~a before point: \\<search>\\[search-repeat-backward] in the search goes on from the end")
                                 (search-state-string state))))
               (when (search-state-failing state) (note-command-failed)))
             ;; its matches stay lit till C-g; C-s and C-r right after go on through them (SEARCH-NEXT-MATCH)
             (when (and (search-state-match state) (not (search-state-failing state)))
               (setf *search-lit* (cons state (current-buffer)) *search-lit-shown* t))
             (when (and (search-state-match state) *search-repeat-map*)
               (setf *search-finished* state
                     (editor-repeat-map *editor*) *search-repeat-map*))))))

(defun search-question-update (state)
  "Before STATE's question is drawn, and whenever the editor is woken: take
in what the search in the background found, ending the question when
\\<search>\\[search-exit] waits for it; show a region the search is limited to as
its scope; take in what a list narrowed in the background left; keep the
count of matches up to date (UPDATE-SEARCH-COUNT). Three values, as
READ-STRING's UPDATE gives them: true while work for it goes on in the
background, its prompt saying how things stand, and which match is current
and how many there are (SEARCH-COUNT-WORDS)."
  (when (and (search-take-in state) (search-state-exit-when-found state))
    (setf (search-state-exit-when-found state) nil)
    (finish-minibuffer))
  ;; a region the search is limited to shows as its scope, not as a
  ;; selection that moving point would reshape
  (when (search-state-region state)
    (setf (tuatara.buffer:buffer-mark-active-p (current-buffer)) (search-keeps-region-p state)))
  (let ((filtering (and (search-state-filtering state) (take-in-filter-jobs (search-state-filtering state))))
        (counting (update-search-count state)))
    (values (or (search-state-background state) filtering counting) (search-prompt state) (search-count-words state))))

(defun search-loop (state &optional offered)
  "Run the incremental search: its string is typed in the minibuffer, OFFERED
there first, selected, when given, and searched for at once; the
buffer searched (or the list narrowed) at each change of it, the keys of
*SEARCH-KEYMAP* before the minibuffer's, the patterns typed before
\(*PATTERN-HISTORY*) brought back with \\<minibuffer>\\[minibuffer-previous-history] and \\[minibuffer-next-history].
\\<search>\\[search-exit] carries it out (SEARCH-CARRY-OUT); \\[search-abort]
goes back to where it began; what was typed is kept in the history either way."
  (let* ((editor *editor*) (*search-in-loop* t)
         (filtering (search-state-filtering state))
         (unpreview (and (search-replacing-p state) (show-replace-preview state (current-buffer)))))
    (setf *search* state *search-lit-shown* nil)
    (push #'search-highlights *highlight-functions*)
    (unwind-protect
         (handler-case
             (progn
               (read-string (search-prompt state)
                            :initial (or offered "") :select-initial t
                            :changed (lambda (text) (search-text-changed state text))
                            :update (lambda ()
                                      ;; the pattern offered is searched for once the question is open,
                                      ;; unless keys typed already took its place
                                      (let ((text (shiftf offered nil)))
                                        (when (and text (string= text (editor-minibuffer-text editor)))
                                          (search-text-changed state text)))
                                      (search-question-update state))
                            :keymap *search-keymap*
                            :history *pattern-history*)
               (search-carry-out state))
           (quit ()
             (search-go-back state)
             (message "Quit")))
      (cancel-background-search state)
      (stop-search-count state)
      (when (search-state-tally state) (stop-pattern-count (search-state-tally state)))
      (when unpreview (funcall unpreview))
      (keep-typed-pattern (search-state-string state))
      ;; however it ended, a filter still being typed goes
      (when filtering (drop-typed-filter filtering))
      ;; a prompt left in the echo area goes; a message, such as a count of replacements, stays
      (when (eq (editor-message editor) *search-echo*) (setf (editor-message editor) nil))
      (setf *search* nil *search-echo* nil
            *highlight-functions* (remove #'search-highlights *highlight-functions*)))
    nil))

(defun search-go-back (state)
  "\\<search>\\[search-abort]: return to where STATE began, with the region
active again if it was; a list it narrowed shows again what the filters kept
before it leave."
  (when (search-state-filtering state) (drop-typed-filter (search-state-filtering state)))
  (goto (search-state-origin state))
  (when (search-state-region state)
    (setf (tuatara.buffer:buffer-mark-active-p (current-buffer)) t)))

(defun symbol-search-text (position regex &optional (parsed t))
  "The symbol at POSITION in the current buffer, written for the search's
prompt (SEARCH-QUOTE), whole symbols only when REGEX; PARSED true when the
prompt reads a pattern and a replacement. NIL when there is no symbol there."
  (multiple-value-bind (start end) (symbol-at (tuatara.buffer:buffer-text (current-buffer)) position)
    (when start
      (search-quote (tuatara.buffer:buffer-string (current-buffer) :start start :end end) regex
                    :whole t :parsed parsed))))

(defun search-insert-symbol-text (state)
  "The symbol where STATE began, written for its prompt (SYMBOL-SEARCH-TEXT)."
  (symbol-search-text (search-state-origin state) (search-state-regex state) (search-replacing-p state)))

(defun search-start (forward regex &key preset filtering suggest)
  "Begin an incremental search from point in direction FORWARD, for a regular
expression when REGEX is true, else for literal text. PRESET opens the prompt
for replacing (see SEARCH-STATE). With FILTERING, or in a list whose
searches filter it (SET-SEARCH-FILTERING), what is typed narrows the list to
the entries it matches instead (SEARCH-FILTER-REGEX). With SUGGEST and no
region active, the symbol at point is offered as the pattern, selected, so
that typing takes its place (SYMBOL-SEARCH-TEXT)."
  (let* ((b (current-buffer))
         (filtering (or filtering (and (not preset) (search-filters-list-p b :searches) b)))
         (region (when (and (not filtering) (tuatara.buffer:buffer-mark-active-p b))
                   (multiple-value-bind (start end) (tuatara.buffer:buffer-region b)
                     (and start (cons start end)))))
         (offered (and suggest (not region) (symbol-search-text (point) regex (and preset t)))))
    (if (getf preset :every-after-percent)
        ;; what f would act on, kept for the scope: the cursors, a block, banked lines, the selection
        (multiple-value-bind (ranges words) (selected-ranges (prefix-argument))
          (declare (notinline block-scope-p))
          (if (block-scope-p ranges)
              (message "This block is too large to replace in: select fewer rows, or use f on it")
              (search-loop (make-search-state (point) forward regex region
                                              (list* :selected ranges :selected-words words preset)
                                              filtering)
                           (and (null ranges) offered))))
        (search-loop (make-search-state (point) forward regex region preset filtering) offered))))

(define-command regex-search-forward ()
  "Search forward as you type for a regular expression (PCRE syntax); a pattern
without upper-case letters ignores case, unless it names a case: [[:upper:]],
[[:lower:]], \\p{Lu}, \\p{Ll} and \\p{Lt} heed it, as a capital letter does.
\\xNN from 80 to FF finds the octet NN where it stands on its own, not inside
a character (shown as \\xNN), and \\x{E9} the character of that code; where lines end with a carriage return and
a newline, $ matches before the carriage return and . matches neither.
A set may hold POSIX classes, [[:alpha:]_], of ASCII characters only; \\p{L} and
\\P{L} match by a Unicode property or script (\\p{Lu}, \\p{Greek}); (?<year>...)
is a named group, numbered as any other, \\k<year> matching it again;
\\Q...\\E is literal text; \\h and \\v match horizontal and vertical space,
\\R any line break; a++, a*+, a?+ and a{2,5}+ repeat possessively, taking as
much as they can and never giving it back, as (?>a+) does. An escape this
search lacks, as \\K or \\G, is refused in the prompt as not
supported, never read as a letter.
A pattern that cannot be used shows why in the prompt, and \\<search>\\[search-exit] waits
for it to be mended. A pattern so slow on some line (a nested repeat such as
(a+)+ on a line that almost matches) that it holds the search up goes on in
the background, the prompt saying slow pattern, and keys are answered
meanwhile; the next key, or \\[search-abort], stops it.
The pattern is typed in the minibuffer, and edited there as any answer is
(\\<minibuffer>\\[minibuffer-backward-delete-char], \\[minibuffer-beginning], \\[minibuffer-end], the arrows), the
buffer searched again at each change: taking back what was typed goes back
to where the search was for the shorter text.
\\<search>\\[search-repeat-forward] goes to the next match,
\\[search-repeat-backward] turns around, \\[search-toggle-regex] switches to
literal text, \\[search-exit] stops at
the match and \\[search-abort] goes back to where the search began; after
\\[search-exit], \\[search-repeat-forward] and \\[search-repeat-backward] pressed right away go to the next and the
previous match without the prompt (search-next-match), till another key.
\\[search-insert-symbol] puts the symbol at point into the search;
\\<minibuffer>\\[minibuffer-previous-history] brings back the pattern typed before, in this or any question
that takes one, and again the one before that, \\[minibuffer-next-history] going forward again
\(*PATTERN-HISTORY*, kept while the editor runs).\\<search> With a region
holding text the search stays within it; a search right after the mark is
set extends the region to the match instead. Everything typed is the
pattern, a / or a % too. The prompt says which match the search is at and how
many there are before it, 3/12, counted in the background on a large text.
Replacing is \\[replace-regexp] and \\[query-replace-regexp].
A match of more than 4 KB (4,096 characters of ASCII) that runs across lines
and needs text further on, as BEGIN[\\s\\S]*?END around a large block, can be
missed where the search starts far from it: going on to the next match, or
replacing from the start. \\[regex-search-forward-thorough] finds every match
of up to a megabyte."
  (search-start t t))

(define-command regex-search-backward ()
  "Search backward as you type for a regular expression; keys, replacing and
the limit on matches of more than 4 KB as in regex-search-forward.
\\[regex-search-backward-thorough] finds every match of up to a megabyte."
  (search-start nil t))

(define-command literal-search-forward ()
  "Search forward as you type for literal text: keys as in regex-search-forward,
nothing typed special. \\[query-replace] replaces literal text."
  (search-start t nil))

(define-command literal-search-backward ()
  "Search backward as you type for literal text; see literal-search-forward."
  (search-start nil nil))

(define-command search-repeat-forward ()
  "During a search: the next match forward (with an empty string, the last search string)."
  (when *search* (search-repeat *search* t)))

(define-command search-repeat-backward ()
  "During a search: the next match backward."
  (when *search* (search-repeat *search* nil)))

(defun search-jump (forward)
  "Go to the next match of the search just ended (*SEARCH-FINISHED*) in
direction FORWARD, past the last from the other end, saying so; a fold
hiding it is opened."
  (let ((state *search-finished*))
    (if (null state)
        (message "No search to go on with")
        (progn
          (setf (search-state-failing state) nil)
          (search-repeat state forward)
          (when (search-state-failing state)
            ;; no more that way: from the other end
            (search-repeat state forward)
            (if (search-state-failing state)
                (progn (message "No match for ~a" (search-state-string state))
                       (note-command-failed))
                (message "Wrapped to the ~:[end~;start~]" forward)))
          (unless (search-state-failing state) (search-show-match state))))))

(define-command search-next-match ()
  "Go to the next match of the search just ended with \\<search>\\[search-exit], without its
prompt: \\[search-repeat-forward] right after the search does, again and again, the last
match followed by the first. Any other key ends that; \\[regex-search-forward] then
begins a new search."
  (search-jump t))

(define-command search-previous-match ()
  "Go to the previous match of the search just ended with \\<search>\\[search-exit], without
its prompt: \\[search-repeat-backward] right after the search does, again and again, the
first match followed by the last. Any other key ends that."
  (search-jump nil))

(define-command search-toggle-regex ()
  "During a search, or a search of files (\\[search-in-files]): switch between a
regular expression and literal text."
  (cond (*search* (search-toggle *search*))
        (*toggle-pattern-kind* (funcall *toggle-pattern-kind*))))

(define-command search-insert-symbol ()
  "During a search: add the symbol at point where the search began to the
search string, in a regular expression as a whole symbol, never found inside
a longer one."
  (when *search*
    (let ((text (search-insert-symbol-text *search*)))
      (if text (search-type text) (message "No symbol at point")))))

(define-command query-replace ()
  "Replace literal text, asking at each match: \\`y' replaces, \\`n' skips, \\`!'
replaces all the rest, \\`.' replaces and stops, \\`q' stops. Opens the search
prompt set up for replacing, in the region when one is active, else in the
whole buffer: type the text, \\<search>\\[search-exit] (or /), the replacement,
\\[search-exit]. \\[search-toggle-regex] switches to a regular expression.
A match in folded text is asked at too: its fold is opened first, so that it
is seen, and the other folds stay as they are."
  (search-start t nil :preset '(:flags "gc")))

(define-command query-replace-regexp ()
  "Replace a regular expression as \\[replace-regexp] does, asking at each
match: \\`y' replaces, \\`n' leaves it and goes to the next, \\`!' replaces all
the rest, \\`.' replaces and stops, \\`q' stops. The first match on each line
is asked at; with a % in front, %pattern/replacement, every match. In the
region when one is active, else in the whole buffer; no flags; the word or
symbol at point offered as the pattern, selected. & and \\1 in the
replacement stand for the match and its first group. How many matches there
are to ask at shows before the prompt as the pattern is typed. A match in
folded text is asked at too, its fold opened first. A match of more than 4 KB that runs
across lines and needs text further on can be missed, as regex-search-forward
says; \\[query-replace-regexp-thorough] finds every match of up to a megabyte."
  (search-start t t :preset '(:flags "c" :every-after-percent t) :suggest t))

(define-command replace-regexp ()
  "Replace a regular expression, as one edit, in the region when one is
active, else in the whole buffer: pattern/replacement, typed on one line or
in two parts, as \\<search>\\[search-exit] after the pattern types the /
before the replacement. The first match on each line is replaced; with a %
in front, %pattern/replacement, every match. It takes no flags:
\\[query-replace-regexp] asks at each match, and
\\[regex-search-forward] counts with %pattern//gn. & and \\1 in the replacement stand for
the match and its first group. How many matches it replaces shows before the
prompt as the pattern is typed.
On a word or symbol, with no region, it is offered as the pattern (as whole
words), selected: \\[search-exit] takes it, typing takes its place. The
matches in folded text are replaced too, the folds kept folded. A match of
more than 4 KB that runs across lines and needs text further on can be
missed, as regex-search-forward says; \\[replace-regexp-thorough] finds every
match of up to a megabyte."
  (search-start t t :preset '(:flags "" :every-after-percent t) :suggest t))

;;; Thorough: every match of up to a megabyte found whole

(define-command regex-search-forward-thorough ()
  "Search forward as regex-search-forward does, finding every match of up to a
megabyte (about a million characters of ASCII) whole however many lines it
spans, where regex-search-forward may miss one of more than 4 KB that runs
across lines and needs text further on (a block of lines matched by
BEGIN[\\s\\S]*?END, say). A match of more than a megabyte is still cut short,
or missed when it needs text further on. As quick as regex-search-forward for a
pattern kept within a line or no longer than 4 KB; about twice as slow for one
that may run on across lines without end. The prompt says thorough. In its
place, in the init file: (bind-key \"C-s\" 'regex-search-forward-thorough)."
  (let ((tuatara.syntax:*regex-thorough* t)) (search-start t t)))

(define-command regex-search-backward-thorough ()
  "Search backward as regex-search-backward does, finding every match of up to a
megabyte whole however many lines it spans, with the cost and the limit of
regex-search-forward-thorough: a match of more than a megabyte is still cut
short, or missed when it needs text further on."
  (let ((tuatara.syntax:*regex-thorough* t)) (search-start nil t)))

(define-command query-replace-regexp-thorough ()
  "Replace a regular expression asking at each match, as query-replace-regexp
does, finding every match of up to a megabyte whole however many lines it
spans, with the cost and the limit of regex-search-forward-thorough: a match of
more than a megabyte is still cut short, or missed when it needs text further
on. In the background too when large."
  (let ((tuatara.syntax:*regex-thorough* t)) (search-start t t :preset '(:flags "c" :every-after-percent t) :suggest t)))

(define-command replace-regexp-thorough ()
  "Replace a regular expression as replace-regexp does (the first match on each
line, every match after a leading %), finding every match of up to a megabyte whole however many lines it spans,
with the cost and the limit of regex-search-forward-thorough: a match of more
than a megabyte is still cut short, or missed when it needs text further on.
In the background too when large."
  (let ((tuatara.syntax:*regex-thorough* t)) (search-start t t :preset '(:flags "" :every-after-percent t) :suggest t)))

(define-command search-exit ()
  "End the search, leaving point at the match and the mark where the search
began; when the string holds a replacement, carry it out; for a search
filter, keep it on the list. Opened for replacing with only a pattern typed,
type the / before the replacement instead (SEARCH-READY-TO-END-P)."
  (when (and *search* (search-ready-to-end-p *search*))
    (finish-minibuffer)))

(define-command search-abort ()
  "Cancel the search and return to where it began; a search filter being
typed goes, the list shown as the filters kept leave it."
  (when *search* (keyboard-quit-signal)))

(defun install-search-keys ()
  "Bind the keys of the search's question and a search filter's, and those that go on through the matches
of a search just ended (*SEARCH-REPEAT-MAP*)."
  (define-keys *search-keymap* '(("C-s" search-repeat-forward) ("C-r" search-repeat-backward)
                                 ("<down>" minibuffer-next-history) ("<up>" minibuffer-previous-history)
                                 ("M-r" search-toggle-regex) ("M-." search-insert-symbol)
                                 ("RET" search-exit) ("C-g" search-abort) ("ESC" search-abort)))
  (setf *search-repeat-map* (define-repeat-keys '(("C-s" search-next-match) ("C-r" search-previous-match))))
  (pushnew 'lit-search-highlights *highlight-functions*)
  (pushnew 'unlight-search *keyboard-quit-functions*)
  (pushnew 'forget-lit-search-of tuatara.buffer:*kill-buffer-functions*)
  (pushnew 'search-cursor-colour *cursor-colour-functions*))
