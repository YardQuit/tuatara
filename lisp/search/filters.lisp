;;;; lisp/search/filters.lisp --- A list or a file of records narrowed to what searches match, the searches kept as filters.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Any list a command makes with RENDER-PIECES (or SHOW-LIST-TEXT, as
;;;; rimu's lists of directories and the agenda are made) and its entries can be
;;;; narrowed: C-x / s reads a pattern at the search prompt, and as it is
;;;; typed the list shows only the entries whose lines match, or the more
;;;; text a list gives of each entry (SET-SEARCH-FILTERING: a mailbox gives
;;;; each message's headers and contents). RET keeps the pattern as a
;;;; filter, and the next search narrows what that one left; the filters are
;;;; listed with C-x / l, and taken away one at a time or all at once. The
;;;; whole list is kept as it was made, and narrowed again when it is made
;;;; again. Matching runs in the background where a list gives more text, or
;;;; has many lines; each filter keeps what it left, so a filter added
;;;; searches only those, and one taken away has only the later ones worked
;;;; out again.
;;;;
;;;; A file of records, one a line (a log, a CSV under its header line), is
;;;; narrowed by the same commands and filters: the lines no filter leaves are
;;;; hidden from its windows (TUATARA.DISPLAY:HIDE-RANGES), its text never
;;;; changed, and what each filter leaves is kept as runs of lines, so that a
;;;; file of gigabytes costs no more memory than the runs it shows. Whether a
;;;; buffer is narrowed so is its mode's to say (FILTERS-HIDE-LINES-P).
;;;;
;;;; Where the work is large, each key typed matches the lines from the top
;;;; of the window first, for as long as *SEARCH-FILTER-SCREEN-SECONDS*, and
;;;; shows what it found at once; the rest is worked out in the background on
;;;; all but two cores, and the next key typed starts again.

(in-package #:tuatara.defaults)

(defstruct (search-filter (:constructor %make-search-filter (string kind fold matcher where test part hide)) (:copier nil))
  "A pattern narrowing a list: STRING as typed; KIND :REGEX for a regular
expression, :LITERAL for literal text; FOLD true when it ignores case; MATCHER,
how it is found in a text (MAKE-PATTERN-MATCHER), and SCANNER, it compiled to
try on a string, made when first asked for (SEARCH-FILTER-SCANNER); WHERE, the
part of each entry it looks at as the list names parts
\(NIL for the line and all the list gives of the entry). In a file of
records, TEST, when given, tries SCANNER on a record's line in its own way
\(FILTER-LINE-MATCHER: a column), PART the words naming what it tries
\(\"$2\"). HIDE true when it leaves the entries it does not match. KEPT,
which of the list's entries it and the filters before it leave, a bit for
each, worked out for the whole list SOURCE; COUNT, how many entries that is,
headings aside."
  string kind fold matcher (compiled nil) where test part hide (kept nil) (source nil) (count 0))

(setf (documentation 'search-filter-p 'function) "True when OBJECT is a SEARCH-FILTER."
      (documentation 'search-filter-string 'function) "The pattern of a search filter, as typed."
      (documentation 'search-filter-kind 'function) "How a search filter's pattern is read: :REGEX or :LITERAL."
      (documentation 'search-filter-fold 'function) "True when a search filter ignores case."
      (documentation 'search-filter-matcher 'function) "How a search filter's pattern is found in a text, a MATCHER."
      (documentation 'search-filter-compiled 'function) "A search filter's pattern compiled for strings, once asked for, or NIL."
      (documentation 'search-filter-where 'function) "The part of an entry a search filter looks at, or NIL for all of it."
      (documentation 'search-filter-test 'function) "How a search filter tries its pattern on a record's line (a column), or NIL for the whole line."
      (documentation 'search-filter-part 'function) "Words naming what of a record a search filter tries (\"$2\"), or NIL for the whole line."
      (documentation 'search-filter-hide 'function) "True when a search filter leaves the entries it does not match."
      (documentation 'search-filter-kept 'function) "Which entries a search filter and those before it leave, a bit each, or NIL."
      (documentation 'search-filter-source 'function) "The whole list a search filter's KEPT was worked out for."
      (documentation 'search-filter-count 'function) "How many entries a search filter and those before it leave.")

(defun search-filter-scanner (filter)
  "FILTER's pattern compiled to try on a string: its matcher's regular
expression, or for literal text one made the first time it is asked for."
  (or (search-filter-compiled filter)
      (setf (search-filter-compiled filter)
            (let ((matcher (search-filter-matcher filter)))
              (if (eq (matcher-kind matcher) :regex)
                  (matcher-scanner matcher)
                  (tuatara.syntax:compile-pattern (matcher-pattern matcher) :regex nil :case-fold (matcher-fold matcher)))))))

(defun make-search-filter (string &key (regex t) where test part hide crlf matcher)
  "A SEARCH-FILTER for STRING, a regular expression when REGEX is true, else
literal text, nothing in it special; case ignored when it has no capital
letter, as a search ignores it. MATCHER, when given, is how it is found
\(SEARCH-MATCHER, made for it already); else one is made, $ matching before
a carriage return with CRLF. WHERE names the part of each entry it looks at;
TEST and PART how and what of a record's line it tries; HIDE true to leave
what it does not match. Returns NIL and why when the pattern does not
compile."
  (let* ((line (make-search-line :pattern string))
         (fold (search-line-fold line regex)))
    (multiple-value-bind (matcher why)
        (or matcher (make-pattern-matcher (search-line-pattern line) :regex regex :fold fold :crlf crlf))
      (if matcher
          (%make-search-filter string (if regex :regex :literal) fold matcher where test part hide)
          (values nil why)))))

(defun search-filter-match-p (filter text)
  "True when the pattern of FILTER, a SEARCH-FILTER, matches somewhere in TEXT, a string."
  (and (tuatara.runtime:stoppable (cl-ppcre:scan (search-filter-scanner filter) text)) t))

(defun search-filter-leaves-line-p (filter text length)
  "True when FILTER leaves the record whose line TEXT holds in its first
LENGTH characters: its pattern found in the line, or in the part of it the
filter tries (a column); not found, for a filter that hides what it
matches."
  (let ((test (search-filter-test filter)) (scanner (search-filter-scanner filter)))
    (if (if test
            (funcall test scanner text length)
            (tuatara.runtime:stoppable (cl-ppcre:scan scanner text :start 0 :end length)))
        (not (search-filter-hide filter))
        (search-filter-hide filter))))

(defun search-filter-words (filter)
  "FILTER's pattern as the lists show it: a regular expression as it is,
literal text in double quotes, after the part of a record it tries ($2:)
and not, for one that hides what it matches."
  (format nil "~:[~;not ~]~@[~a: ~]~:[\"~a\"~;~a~]" (search-filter-hide filter) (search-filter-part filter)
          (search-filter-regex-p filter) (search-filter-string filter)))

(defun search-filter-regex-p (filter)
  "True when FILTER's pattern is a regular expression, NIL for literal text."
  (eq (search-filter-kind filter) :regex))

;;; What a list keeps of its filtering

(defstruct (list-source (:constructor make-list-source (text spans buttons entries first-line)) (:copier nil))
  "A list as it was made, before filters narrowed it: its TEXT, SPANS and
BUTTONS as PIECES-TEXT gives them, its ENTRIES (a vector), shown from line
FIRST-LINE on, and LINES, the text of each entry's line, and TOTAL, how many
entries it shows, headings aside, each worked out once."
  text spans buttons entries first-line (lines nil) (total nil))

(setf (documentation 'make-list-source 'function) "A LIST-SOURCE of a list's TEXT, SPANS, BUTTONS, ENTRIES and FIRST-LINE."
      (documentation 'list-source-p 'function) "True when OBJECT is a LIST-SOURCE."
      (documentation 'list-source-text 'function) "The whole text of a list, as made."
      (documentation 'list-source-spans 'function) "The parts of a list's whole text shown in a face."
      (documentation 'list-source-buttons 'function) "The buttons of a list's whole text."
      (documentation 'list-source-entries 'function) "The entries of a list, as made, a vector."
      (documentation 'list-source-first-line 'function) "The line of a list's whole text its first entry is on."
      (documentation 'list-source-lines 'function) "The text of each entry's line, or NIL until worked out."
      (documentation 'list-source-total 'function) "How many entries a list shows, headings aside, or NIL until counted.")

(defstruct (list-filtering (:constructor make-list-filtering ()) (:copier nil))
  "How a list is narrowed: FILTERS kept, the first first; TYPING, the filter
being typed at the search prompt, or NIL; SOURCE, the whole list (a
LIST-SOURCE) while filters narrow it; SHOWN, the bits of the entries shown,
or NIL for all; JOB, the filters being worked out in the background; and what
the list says of itself (SET-SEARCH-FILTERING): TEXT, NOUNS, HEADING,
EMPTY-HEADINGS, DECORATION and SEARCHES. For a file of records (LINES, how
many header lines it has, always shown: 0 for none), SOURCE is the version
of its text the filters were worked out for, SHOWN the runs of lines shown
and TOTAL how many records it has, once known. While
JOB works the filters out, SHOWN is what they match from the window's top;
SLOW is true when it works them out because a pattern was too slow on this
text for the editor's thread. DUE holds the filters whose job waits for
typing to pause."
  (filters '()) (typing nil) (source nil) (shown nil) (shown-count 0) (job nil)
  (text nil) (nouns "entries") (heading nil) (empty-headings t) (decoration nil) (searches nil)
  (lines nil) (total nil) (slow nil) (due nil))

(setf (documentation 'list-filtering-p 'function) "True when OBJECT is a LIST-FILTERING."
      (documentation 'list-filtering-filters 'function) "The filters kept on a list, the first first."
      (documentation 'list-filtering-typing 'function) "The filter being typed at the search prompt, or NIL."
      (documentation 'list-filtering-source 'function) "The whole list, a LIST-SOURCE, while filters narrow it."
      (documentation 'list-filtering-shown 'function) "The bits of the entries shown, or NIL for all of them."
      (documentation 'list-filtering-shown-count 'function) "How many entries are shown, headings aside."
      (documentation 'list-filtering-job 'function) "The job working the filters out, or NIL."
      (documentation 'list-filtering-text 'function) "A function of an entry and a part's name giving more text to match, or NIL."
      (documentation 'list-filtering-nouns 'function) "What the list calls its entries, in the plural."
      (documentation 'list-filtering-heading 'function) "A function true of an entry that heads a section, or NIL."
      (documentation 'list-filtering-empty-headings 'function) "True when a heading with nothing under it stays while filters narrow the list."
      (documentation 'list-filtering-decoration 'function) "A function true of a line's entry that goes with its section's heading, or NIL."
      (documentation 'list-filtering-searches 'function) "True when the list's searches narrow it."
      (documentation 'list-filtering-lines 'function) "For a file of records whose lines the filters hide, how many header lines it has (0 for none); NIL for a list of entries."
      (documentation 'list-filtering-total 'function) "How many records a file narrowed by filters has, or NIL until known."
      (documentation 'list-filtering-due 'function) "The filters still to work out and what the ones before leave, (LEFT PREVIOUS), while their job waits for typing to pause; or NIL."
      (documentation 'list-filtering-slow 'function) "True when the filters are worked out in the background because a pattern was slow on this text.")

(defvar *buffers-filtered-by-lines* '()
  "The buffers whose lines search filters hide, looked at before each
redisplay for a change that reached into what they hide (REFILTER-TOUCHED).")

(defun list-filtering (buffer)
  "How BUFFER, a list, is narrowed by search filters (a LIST-FILTERING), or NIL."
  (tuatara.buffer:buffer-local buffer 'list-filtering))

(defun list-filtering-made (buffer)
  "BUFFER's LIST-FILTERING, made when it has none: one hiding lines, its
entries called records, for a buffer whose mode says so (FILTERS-HIDE-LINES-P)."
  (or (list-filtering buffer)
      (let ((filtering (make-list-filtering)))
        (let ((mode (buffer-major-mode buffer)))
          (when (filters-hide-lines-p mode buffer)
            (setf (list-filtering-lines filtering) (if (list-header-p buffer) 1 0)
                  (list-filtering-nouns filtering) "records")))
        (setf (tuatara.buffer:buffer-local buffer 'list-filtering) filtering))))

(defun set-search-filtering (buffer &key text (nouns "entries") heading (empty-headings t) decoration searches)
  "Say how BUFFER, a list made by RENDER-PIECES, is narrowed by search filters
\(\\[search-filter-regex]): TEXT, a function of an entry and the name of a
part of it (NIL for all), gives more of it to match than its line, as a
string or NIL, called in the background (it must not touch the editor);
NOUNS names its entries in the plural, for the line saying how many are
shown; HEADING, a function of an entry, is true of one that heads a section,
shown while an entry under it is and never matched itself, and, unless
EMPTY-HEADINGS is NIL, while nothing at all is under it; DECORATION, a
function of an entry, is true of a line that goes with the heading above it
\(a time grid's line under a day): never matched nor counted, shown while
that heading is; with SEARCHES, \\[regex-search-forward] and
\\[regex-search-backward] there narrow it too. Returns BUFFER."
  (let ((filtering (list-filtering-made buffer)))
    (setf (list-filtering-text filtering) text (list-filtering-nouns filtering) nouns
          (list-filtering-heading filtering) heading (list-filtering-empty-headings filtering) empty-headings
          (list-filtering-decoration filtering) decoration (list-filtering-searches filtering) searches))
  buffer)

(defun search-filters-list-p (buffer &optional searches)
  "True when BUFFER is a list a search can narrow, one made by RENDER-PIECES
with its entries; with SEARCHES, only when its searches narrow it
\(SET-SEARCH-FILTERING)."
  (and (not (eq :none (tuatara.buffer:buffer-local buffer 'list-entries :none)))
       (or (not searches)
           (let ((filtering (list-filtering buffer))) (and filtering (list-filtering-searches filtering))))))

(defun search-filterable-p (buffer)
  "True when search filters can narrow BUFFER: a list with its entries
\(SEARCH-FILTERS-LIST-P), or a file of records whose mode hides the lines
they leave out (FILTERS-HIDE-LINES-P)."
  (or (search-filters-list-p buffer)
      (filters-hide-lines-p (buffer-major-mode buffer) buffer)))

(defun filters-lines-p (buffer)
  "True when search filters narrow BUFFER by hiding its lines."
  (let ((filtering (list-filtering buffer)))
    (and filtering (list-filtering-lines filtering))))

(defun search-filter-chain (filtering)
  "The filters narrowing a list, as FILTERING has them: those kept, then the one being typed."
  (append (list-filtering-filters filtering)
          (and (list-filtering-typing filtering) (list (list-filtering-typing filtering)))))

(defun search-filters (buffer)
  "The search filters kept on BUFFER, a list, the first first; NIL for none."
  (let ((filtering (list-filtering buffer)))
    (and filtering (list-filtering-filters filtering))))

;;; Which entries a filter leaves

(defun list-source-entry-lines (source)
  "The text of the line of each entry of SOURCE, a LIST-SOURCE, a vector of
strings, worked out the first time. O(size of the text)."
  (or (list-source-lines source)
      (let* ((text (list-source-text source))
             (entries (list-source-entries source))
             (lines (make-array (length entries) :initial-element ""))
             (start 0))
        (loop repeat (list-source-first-line source)
              while start
              do (setf start (let ((end (position #\Newline text :start start))) (and end (1+ end)))))
        (loop for i from 0 below (length entries)
              while (and start (< start (length text)))
              do (let ((end (or (position #\Newline text :start start) (length text))))
                   (setf (svref lines i) (subseq text start end)
                         start (1+ end))))
        (setf (list-source-lines source) lines))))

(defun decoration-entry-p (filtering entry)
  "True when ENTRY is a line of the list FILTERING narrows that goes with the
heading above it, as the list's own DECORATION says (SET-SEARCH-FILTERING)."
  (let ((decoration (list-filtering-decoration filtering)))
    (and entry decoration (funcall decoration entry) t)))

(defun heading-entry-p (filtering entry)
  "True when ENTRY heads a section of the list FILTERING narrows, goes with
a heading, or shows no entry at all: a section's line, (:SECTION NAME) as
every list writes it, or what the list's own HEADING or DECORATION says is
one. Such a line is never matched nor counted."
  (or (null entry)
      (and (consp entry) (eq (first entry) :section))
      (let ((heading (list-filtering-heading filtering)))
        (and heading (funcall heading entry) t))
      (decoration-entry-p filtering entry)))

(defun listed-entry-count (filtering source)
  "How many entries SOURCE, the whole list FILTERING narrows, shows, headings
aside, an entry shown on several lines counted once (ENTRY-START-P): counted
the first time, O(entries), and kept with SOURCE."
  (or (list-source-total source)
      (setf (list-source-total source)
            (let ((entries (list-source-entries source)))
              (loop for i from 0 below (length entries)
                    count (and (entry-start-p entries i) (not (heading-entry-p filtering (svref entries i)))))))))

(defun entry-matches-p (filtering filter entry line-matched)
  "True when FILTER matches ENTRY of the list FILTERING narrows: its line,
when LINE-MATCHED is true (LIST-LINES-MATCHED), or the more text the list
gives of it; only that text when FILTER looks at a part of it. An entry
whose text cannot be had is not matched by it."
  (let ((text (list-filtering-text filtering)) (where (search-filter-where filter)))
    (when (and (consp where) (eq (first where) :column))
      ;; a column of a table: the text of that cell as the list was last drawn
      (destructuring-bind (column list) (rest where)
        (let ((cell (list-table-cell list entry column)))
          (return-from entry-matches-p (and cell (search-filter-match-p filter cell))))))
    (or (and (null where) line-matched)
        (and text
             (let ((more (handler-case (funcall text entry where) (error () nil))))
               (and (stringp more) (search-filter-match-p filter more)))))))

(defun entry-end (entries index)
  "The index of the first line of ENTRIES, a list's, after INDEX that does not
show the entry line INDEX shows: the next entry's first line."
  (let ((entry (svref entries index)))
    (if entry
        (or (position-if-not (lambda (other) (eq other entry)) entries :start (1+ index)) (length entries))
        (1+ index))))

(defun list-lines-matched (filter lines low high candidate-p)
  "Which of LINES, the lines of a list (strings), from LOW below HIGH, FILTER's
pattern matches, as bits from LOW: each run of lines in a row for which
CANDIDATE-P, a function of a line's index, is true, is searched as one text,
a line after another, and a line is matched when any part of a match lies
on it, from the line holding the match's first character through the line
holding its last, or an empty match is in it, as the lines of a file of
records are matched (LINES-MATCHING)."
  (let ((bits (make-array (- high low) :element-type 'bit :initial-element 0))
        (scanner (search-filter-scanner filter))
        (k low))
    (loop while (< k high)
          do (if (not (funcall candidate-p k))
                 (incf k)
                 (let* ((end (or (loop for j from (1+ k) below high unless (funcall candidate-p j) return j) high))
                        (starts (make-array (1+ (- end k)) :element-type 'fixnum))
                        (text (with-output-to-string (out)
                                (loop for j from k below end
                                      for at = 0 then (+ at 1 (length (svref lines (1- j))))
                                      do (setf (aref starts (- j k)) at)
                                         (write-string (svref lines j) out)
                                         (write-char #\Newline out))))
                        (size (length text)))
                   (setf (aref starts (- end k)) size)
                   (labels ((line-at (at) (1- (tuatara.runtime:bisect (j 0 (length starts)) (<= (aref starts j) at))))
                            (unit-of (at from)
                              (declare (ignore from))
                              (let ((j (line-at at)))
                                (values (aref starts j) (1- (aref starts (1+ j))) (aref starts (1+ j)))))
                            (take (start text-end after)
                              (declare (ignore text-end after))
                              (setf (sbit bits (- (+ k (line-at start)) low)) 1)))
                     (let ((marked 0))
                       (tuatara.runtime:stoppable
                        (cl-ppcre:do-scans (s e rs re scanner text)
                          (when (< s size)
                            (setf marked (map-match-units #'take #'unit-of s e marked size)))))))
                   (setf k end))))
    bits))

(defun work-out-filter (filtering filter source previous &optional progress)
  "Which entries of SOURCE, a LIST-SOURCE, FILTER leaves of those PREVIOUS
leaves (bits, or NIL for all), as two values: their bits and how many they
are. The lines of the entries left are matched together (LIST-LINES-MATCHED),
a match running over several lines matching each of them; an entry shown on
several lines in a row is matched by any of them, and left or not as a
whole. PROGRESS, a function of no arguments, is called for each entry looked
at. Headings are left for LIST-SHOWN-LINES. O(entries) matches."
  (let* ((entries (list-source-entries source))
         (lines (list-source-entry-lines source))
         (total (length entries))
         (bits (make-array total :element-type 'bit :initial-element 0))
         (matched (and (null (search-filter-where filter))
                       (list-lines-matched filter lines 0 total
                                           (lambda (k) (and (or (null previous) (= 1 (sbit previous k)))
                                                            (not (heading-entry-p filtering (svref entries k))))))))
         (count 0) (i 0))
    (loop while (< i total)
          do (let ((entry (svref entries i))
                   (end (entry-end entries i)))
               (when (and (or (null previous) (= 1 (sbit previous i)))
                          (not (heading-entry-p filtering entry)))
                 (when progress (funcall progress))
                 (when (eq (not (search-filter-hide filter))
                           (entry-matches-p filtering filter entry (and matched (find 1 matched :start i :end end) t)))
                   (fill bits 1 :start i :end end)
                   (incf count)))
               (setf i end)))
    (values bits count)))

(defun keep-worked-out (filter source bits count)
  "Keep in FILTER that it leaves the entries BITS, COUNT of them, of SOURCE,
the whole list. Returns BITS."
  (setf (search-filter-kept filter) bits
        (search-filter-count filter) count
        (search-filter-source filter) source)
  bits)

(defun carry-filters-over (filtering old new)
  "Have the filters of FILTERING, every one worked out for OLD, a list's
source, stand for NEW, the list drawn again with entries OLD had (EQUAL),
as when an entry is marked or a section folded: what each leaves carried
over, nothing matched again. True when that could be done; NIL, nothing
changed, when NEW has an entry OLD had not or a filter is not worked out.
O(entries)."
  (let ((chain (search-filter-chain filtering))
        (places (make-hash-table :test 'equal))
        (entries (list-source-entries new)))
    (when (and chain (every (lambda (filter) (eq (search-filter-source filter) old)) chain))
      (loop for entry across (list-source-entries old)
            for i from 0
            when entry do (setf (gethash entry places) i))
      (when (every (lambda (entry) (or (null entry) (nth-value 1 (gethash entry places)))) entries)
        (dolist (filter chain t)
          (let ((before (search-filter-kept filter))
                (bits (make-array (length entries) :element-type 'bit :initial-element 0))
                (count 0))
            (loop for entry across entries
                  for i from 0
                  when (and entry (= 1 (sbit before (gethash entry places))))
                    do (setf (sbit bits i) 1)
                       (when (and (entry-start-p entries i) (not (heading-entry-p filtering entry))) (incf count)))
            (keep-worked-out filter new bits count)))))))

(defun filters-to-work-out (filtering)
  "The filters of FILTERING's chain whose entries are not yet worked out for
its whole list, with the bits of the last one before them that is (NIL for
all entries), as two values."
  (let ((source (list-filtering-source filtering)) (previous nil) (left '()) (stale nil))
    (dolist (filter (search-filter-chain filtering))
      (if (and (not stale) (eq (search-filter-source filter) source))
          (setf previous (search-filter-kept filter))
          (progn (setf stale t) (push filter left))))
    (values (nreverse left) previous)))

(defparameter *search-filter-quick-characters* (* 2 1024 1024)
  "The most characters of text a list may have to be filtered at once, as it is
typed; a larger list, or one that gives more text of its entries, is filtered
in the background.")

(defvar *search-filter-screen-seconds* 1/100
  "Seconds a search filter typed in a large list or file (\\[search-filter-regex])
spends at each key on the lines from the top of the window, so that what
it matches there is shown at once; the rest is matched in the background
on all but two cores, the mode line saying how far it has got, and the next
key starts again. More seconds fill a window where matches are few and far
between; fewer keep typing quicker on a slow machine. The default is 1/100, a
hundredth of a second.")

(defun filter-quickly-p (filtering &optional previous)
  "True when what FILTERING narrows is small enough to filter at once, on the
editor's thread: for a list, no more text given of its entries than their
lines, and no more text than *SEARCH-FILTER-QUICK-CHARACTERS* characters;
for a file of records, no more octets to look at than that: those of the
runs PREVIOUS leaves (LINES-MATCHING), or of every record when it is NIL."
  (if (list-filtering-lines filtering)
      (< (if previous
             (runs-octets previous)
             (tuatara.text:rope-bytes (tuatara.text:version-rope (list-filtering-source filtering))))
         *search-filter-quick-characters*)
      (and (null (list-filtering-text filtering))
           (< (length (list-source-text (list-filtering-source filtering))) *search-filter-quick-characters*))))

;;; Records, a line each, hidden where no filter leaves them

(defun text-unchanged-since (version buffer)
  "True when BUFFER's text is as it was at VERSION, one of its versions:
nothing changed since, only more learnt of it (its lines counted)."
  (let ((now (tuatara.buffer:buffer-version buffer)) (v version))
    (loop
      (when (eq v now) (return t))
      (let ((next (tuatara.text:version-next v)))
        (unless (and next
                     (let ((batch (tuatara.text:version-batch next)))
                       (and (null (tuatara.text:batch-swap batch)) (zerop (length (tuatara.text:batch-edits batch))))))
          (return nil))
        (setf v next)))))

(defun records-start (rope header &optional ends)
  "Where the records of ROPE begin, after its first HEADER records, a line
each or as ENDS ends them (RECORD-ENDS: a CSV's header may hold a quoted
line break): its start when it has none, its end when it has no more than
that."
  (let ((start 0) (total (tuatara.text:rope-bytes rope)))
    (dotimes (i header start)
      (setf start (min total (nth-value 1 (record-bounds rope start ends)))))))

(defun add-run (runs from to)
  "Add the lines from FROM below TO to RUNS (LINES-MATCHING), made one with
the run before when it ends at FROM; nothing when FROM is not before TO."
  (declare (type fixnum from to))
  (when (< from to)
    (let ((fill (fill-pointer runs)))
      (if (and (plusp fill) (= from (aref runs (1- fill))))
          (setf (aref runs (1- fill)) to)
          (progn (vector-push-extend from runs) (vector-push-extend to runs))))))

(defun runs-turned-round (runs first low high)
  "Make the runs of RUNS from its FIRST place on, all within LOW below HIGH,
the lines between them there instead."
  (declare (type fixnum first low high))
  (let ((kept (subseq runs first)) (at low))
    (declare (type fixnum at))
    (setf (fill-pointer runs) first)
    (loop for i from 0 below (length kept) by 2
          do (add-run runs at (aref kept i))
             (setf at (aref kept (1+ i))))
    (add-run runs at high)))

(defun line-holding (rope at from crlf)
  "Three values for the line of ROPE holding octet AT: where it starts, where
its text ends (before its newline, and before a carriage return there with
CRLF) and where the line after it starts. FROM is a line's start at or before
AT, from which the text is read: a line holding many matches is read once."
  (declare (type fixnum at from))
  (let* ((total (tuatara.text:rope-bytes rope))
         (first (tuatara.text:rope-find-line-end rope :start from))
         (start from) (newline first))
    (when (and first (< first at))
      (setf start (1+ (or (tuatara.text:rope-find-line-end-backward rope :start (1+ first) :end at) first))
            newline (tuatara.text:rope-find-line-end rope :start at)))
    (if newline
        (values start
                (tuatara.text:rope-line-text-end rope start newline crlf)
                (1+ newline))
        (values start total total))))

(defun lines-found (matcher rope ranges crlf &key progress halt most ends hide carry)
  "LINES-MATCHING for one filter tried on whole lines, found by MATCHER as a
search finds it: a line is matched when any part of a match of one scan of
the stretch lies on it (MAP-MATCH-UNITS), from the line holding the match's
first octet through the line holding its last, or an empty match is in it.
With ENDS, the records that may hold line breaks are matched the same way,
whole, and each of their lines kept with them; with HIDE, the lines not
matched are those left. CARRY, when given, is (FROM . REACH): a match of the
text before ran on to REACH, so a stretch starting at FROM has its lines
matched up to there and its scan goes on from there. Nothing is decoded but
where a regular expression is tried. A fourth value, the CARRY for the text
after: when the last stretch's last match runs past its end, (END . REACH)."
  (let* ((total (tuatara.text:rope-bytes rope))
         (runs (make-array 16 :element-type 'fixnum :adjustable t :fill-pointer 0))
         (count 0) (looked 0) (onward nil)
         ;; with ENDS, the record walked to: its start (-1 before any), its text's end, the next one's start
         (at -1) (at-end 0) (at-after 0))
    (declare (type fixnum total count looked at at-end at-after))
    (block finding
      (loop for i from 0 below (length ranges) by 2
            when (< (aref ranges i) (aref ranges (1+ i)))
            do (let ((low (aref ranges i)) (high (aref ranges (1+ i)))
                     (first (fill-pointer runs))
                     (marked 0) (matched 0) (units 0) (told 0) (reach 0) (scan 0))
                 (declare (type fixnum low high first marked matched units told reach scan))
                 (labels ((read-record ()
                            (multiple-value-bind (text-end after) (funcall ends rope at)
                              (setf at-end (record-text-end rope text-end crlf) at-after after)))
                          (next-record ()
                            (when (>= at low) (incf units))
                            (setf at at-after)
                            (when (< at total) (read-record)))
                          (unit-holding (s from)
                            (if ends
                                (progn (loop while (and (<= at-after s) (< at total)) do (next-record))
                                       (values at at-end at-after))
                                (line-holding rope s from crlf)))
                          (take (start text-end after)
                            (declare (ignore text-end) (type fixnum start after))
                            (add-run runs (max start low) (min after high))
                            (when (>= start low)
                              (incf matched)
                              (when (and most (not hide) (>= (+ count matched) most))
                                (incf count matched)
                                (return-from finding))))
                          (try (s e m g)
                            (declare (ignore m g) (type fixnum s e))
                            (setf reach (max reach e)
                                  marked (map-match-units #'take #'unit-holding s e marked high))
                            (when (and progress (> (- s told) 262144))
                              (sb-ext:atomic-incf (car progress) (- s told))
                              (setf told s))
                            nil))
                   (setf told low marked low)
                   (when ends
                     ;; the record holding the stretch's first line: walked to from the last one
                     ;; when that is before it, else looked for
                     (if (<= 0 at low)
                         (loop while (and (<= at-after low) (< at total)) do (next-record))
                         (progn (setf at (line-record (make-record-memo) rope low
                                                      (min total (1+ (tuatara.text:rope-line-end rope low))) ends))
                                (read-record)))
                     (setf marked at))
                   (setf scan marked)
                   (when (and carry (= low (car carry)) (> (cdr carry) low))
                     ;; a match of the text before runs into this stretch
                     (try low (cdr carry) nil nil)
                     (setf scan (cdr carry)))
                   (when (< scan high)
                     (stretch-matches #'try rope matcher scan high (= high total) nil halt total))
                   (when ends
                     ;; the records to the stretch's end counted; one going on past it matched whole
                     (loop while (and (< at high) (< at-after high)) do (next-record))
                     (when (and (< at high) (> at-after high) (< marked at-after))
                       (stretch-matches #'try rope matcher (max high marked) at-after (= at-after total) nil halt total))
                     (when (< at high) (next-record)))
                   (when progress (sb-ext:atomic-incf (car progress) (max 0 (- high told))))
                   (unless ends
                     (setf units (+ (tuatara.text:rope-count-newlines rope low high)
                                    (if (and (< low high) (= high total) (not (tuatara.text:rope-line-start-p rope high))) 1 0))))
                   (incf looked units)
                   (if hide
                       (progn (runs-turned-round runs first low high) (incf count (- units matched)))
                       (incf count matched))
                   (setf onward (and (> reach high) (cons high reach)))
                   (when (and halt (funcall halt)) (return-from finding))))))
    (values (coerce runs '(simple-array fixnum (*))) count looked onward)))

(defun lines-decoded (filter rope ranges crlf &key progress halt most ends)
  "LINES-MATCHING for one FILTER that tries its pattern in its own way (a
column, SEARCH-FILTER-TEST): each line decoded and tried as it stands, a line
longer than *REWRITE-LINE-LIMIT* octets on its first that many; with ENDS a
record over several lines tried once, whole, its lines left or not with it.
Reads a block of the text at a time, and makes nothing for a line that does
not match."
  (let* ((total (tuatara.text:rope-bytes rope))
         (holder (list (make-string 256)))
         (runs (make-array 16 :element-type 'fixnum :adjustable t :fill-pointer 0))
         (count 0) (looked 0) (limit *rewrite-line-limit*) (since 0)
         (memo (and ends (make-record-memo))))
    (declare (type fixnum count looked since limit total))
    (block matching
      (labels ((leaves-p (octets from to)
                 (let ((length (decode-into octets from to holder)))
                   (search-filter-leaves-line-p filter (car holder) length)))
               (try (line end next octets at)
                 ;; the line from LINE, its text to END, the next from NEXT; its octets in OCTETS from AT
                 (declare (type fixnum line end next at))
                 (incf since (- next line))
                 (when (and progress (> since 262144))
                   (sb-ext:atomic-incf (car progress) since)
                   (setf since 0))
                 (multiple-value-bind (start text-end after)
                     (if ends (line-record memo rope line next ends) (values line end next))
                   (let* ((first (= start line))
                          (leaves (if (and first (= after next))
                                      (leaves-p octets at (+ at (- (min end (+ line limit)) line)))
                                      ;; a line of a record over several lines: the record tried once, whole
                                      (record-verdict memo rope start text-end crlf limit holder
                                                      (lambda (text length) (search-filter-leaves-line-p filter text length))))))
                     (when first (incf looked))
                     (when leaves
                       (when first (incf count))
                       (add-run runs line next)
                       (when (and most (>= count most)) (return-from matching)))))
                 ;; a string grown for a long line is not kept for the short ones after it
                 (when (> (length (the string (car holder))) 1048576)
                   (setf (car holder) (make-string 256)))
                 (when (and halt (zerop (logand looked 1023)) (funcall halt))
                   (return-from matching))))
        (loop for i from 0 below (length ranges) by 2
              do (map-piece-lines
                  (lambda (line line-end rope block block-start block-end)
                    (declare (type fixnum line line-end))
                    (multiple-value-bind (octets at) (line-octets rope line (min line-end (+ line limit)) block block-start block-end)
                      (try line line-end (slice-next-line rope total crlf line-end block block-start block-end) octets at)))
                  rope (aref ranges i) (aref ranges (1+ i)) crlf))))
    (when progress (sb-ext:atomic-incf (car progress) since))
    (values (coerce runs '(simple-array fixnum (*))) count looked)))

(defun filter-lines-of (filter rope ranges crlf &key progress halt most ends carry)
  "LINES-MATCHING for FILTER alone: found as a search finds its pattern
\(LINES-FOUND, CARRY as it takes it, and the carry onward as a fourth
value), or each line decoded for a filter that tries it in its own way
\(LINES-DECODED)."
  (if (search-filter-test filter)
      (lines-decoded filter rope ranges crlf :progress progress :halt halt :most most :ends ends)
      (lines-found (search-filter-matcher filter) rope ranges crlf :progress progress :halt halt :most most :ends ends
                                                              :hide (search-filter-hide filter) :carry carry)))

(defun lines-matching (filters rope ranges crlf &key progress halt most ends carries)
  "The lines of ROPE starting in RANGES (a vector of fixnums, the start and
end of each stretch in turn, each starting at a line's start) that every
one of FILTERS leaves: three values, the runs of lines left, a vector of
fixnums holding the start of each run's first line and the start of the
line after its last; how many lines they are; and how many were looked at
by the first filter. A filter tried on the whole line leaves every line on
which any part of a match lies, as one scan of the stretch finds the
matches: from the line holding a match's first octet through the line
holding its last, a match running over several lines leaving them all; an
empty match leaves the line it is in. Its pattern's text is skipped to at
the speed of a search, and no line is read as text but where a regular
expression is tried. A filter trying a column decodes each line
\(LINES-DECODED). PROGRESS, a cons, has the octets looked at added to its
car now and then; HALT, a function of none, stops the work when it answers
true, asked now and then; MOST, when given, stops it once that many lines
are left, the stretches then taken a megabyte at a time, a match running
from one into the next as one scan finds it. With ENDS (RECORD-ENDS), the
lines are those of records that may hold line breaks: a record over several
lines is matched whole, its lines left or not with it, and counted once.
CARRIES, one for each filter or NIL, and a fourth value, the same for the
text after RANGES, carry a match running past the end of one part of the
text into the next, as LINES-FOUND takes and gives them."
  (flet ((through (ranges most carries)
           ;; every filter in turn, each on what the one before left
           (let ((count 0) (looked nil) (onward '()))
             (loop for (filter . more) on filters
                   for carry = (pop carries)
                   do (multiple-value-bind (runs left seen reach)
                          (filter-lines-of filter rope ranges crlf :progress progress :halt halt :most (and (null more) most)
                                                                   :ends ends :carry carry)
                        (push reach onward)
                        (setf ranges runs count left looked (or looked seen))))
             (values ranges count (or looked 0) (nreverse onward)))))
    (if (null most)
        (through ranges nil carries)
        (let ((parts '()) (count 0) (looked 0))
          (dolist (piece (line-pieces rope ranges 1048576))
            (multiple-value-bind (runs left seen onward) (through piece (- most count) carries)
              (push runs parts)
              (incf count left)
              (incf looked seen)
              (setf carries onward))
            (when (or (>= count most) (and halt (funcall halt))) (return)))
          (values (join-runs (nreverse parts)) count looked carries)))))

(defun line-pieces (rope ranges size)
  "RANGES, as LINES-MATCHING takes them, cut and gathered into pieces of
about SIZE octets each, every cut at a line's start: a list of vectors of
the same kind, in order."
  (let ((pieces '()) (piece '()) (held 0))
    (flet ((close-piece ()
             (when piece
               (push (coerce (nreverse piece) '(simple-array fixnum (*))) pieces)
               (setf piece '() held 0))))
      (loop for i from 0 below (length ranges) by 2
            do (loop for (start end) in (tuatara.text:rope-line-pieces rope (aref ranges i) (aref ranges (1+ i)) size)
                     do (push start piece) (push end piece)
                        (incf held (- end start))
                        (when (>= held size) (close-piece))))
      (close-piece))
    (nreverse pieces)))

(defun join-runs (parts)
  "The runs of PARTS, vectors of runs (LINES-MATCHING) in order, as one
vector, a run ending where the next begins made one with it."
  (let ((out (make-array (reduce #'+ parts :key #'length) :element-type 'fixnum)) (w 0))
    (dolist (part parts)
      (loop for i from 0 below (length part) by 2
            do (if (and (plusp w) (= (aref out (1- w)) (aref part i)))
                   (setf (aref out (1- w)) (aref part (1+ i)))
                   (setf (aref out w) (aref part i) (aref out (1+ w)) (aref part (1+ i)) w (+ w 2)))))
    (subseq out 0 w)))

(defun lines-matching-everywhere (filter rope ranges crlf progress halt hook &optional ends)
  "LINES-MATCHING for FILTER over RANGES of ROPE, cut into pieces of
*FILTER-PIECE* octets matched on all but two cores at once, the runs put
together in order: the same three values, the same as one scan finds them:
a piece into which a match of the pieces before runs is matched again from
where that match ends. Each piece begins by calling HOOK, when given
\(*REWRITE-PIECE-HOOK* as the work began); records ending as ENDS says
\(RECORD-ENDS)."
  (let* ((pieces (coerce (line-pieces rope ranges *filter-piece*) 'simple-vector))
         (results (tuatara.runtime:run-tasks
                   (map 'simple-vector (lambda (piece)
                                         (lambda ()
                                           (when hook (funcall hook))
                                           (multiple-value-list (lines-matching (list filter) rope piece crlf
                                                                               :progress progress :halt halt :ends ends))))
                        pieces)
                   :parallelism (max 1 (- (tuatara.runtime:cpu-count) 2))))
         (carry nil))
    (dotimes (k (length pieces))
      (when (and carry (= (car carry) (aref (aref pieces k) 0)) (> (cdr carry) (car carry)))
        (setf (svref results k)
              (multiple-value-list (lines-matching (list filter) rope (aref pieces k) crlf
                                                   :halt halt :ends ends :carries (list carry)))))
      (setf carry (first (fourth (svref results k)))))
    (values (join-runs (map 'list #'first results))
            (reduce #'+ results :key #'second)
            (reduce #'+ results :key #'third))))

(defun stretch-count (buffer stretches filter &key every ends stop)
  "How many matches FILTER's pattern has in STRETCHES of BUFFER, (START .
END) each: with EVERY each match, else the lines FILTER leaves
\(LINES-MATCHING), records ending as ENDS says (RECORD-ENDS); NIL once STOP
answers true. Counted on all but two cores, with EVERY on nine in ten."
  (let ((rope (tuatara.buffer:buffer-text buffer)))
    (if every
        (count-in-stretches rope (search-filter-matcher filter) stretches :every t :stop stop)
        (let ((count (nth-value 1 (lines-matching-everywhere
                                   filter rope
                                   (coerce (loop for (start . end) in stretches collect start collect end) '(simple-array fixnum (*)))
                                   (buffer-crlf-p buffer) nil stop nil ends))))
          (and (not (and stop (funcall stop))) count)))))

(defun runs-octets (runs)
  "How many octets of the text RUNS (LINES-MATCHING) take."
  (loop for i from 0 below (length runs) by 2 sum (- (aref runs (1+ i)) (aref runs i))))

(defun all-records (rope header &optional ends)
  "Every record of ROPE as LINES-MATCHING takes stretches: from after its
first HEADER records, ending as ENDS says (RECORDS-START), to its end."
  (make-array 2 :element-type 'fixnum :initial-contents (list (records-start rope header ends) (tuatara.text:rope-bytes rope))))

(defun hidden-lines-bounds (rope runs crlf header &optional ends)
  "What to hide of ROPE (TUATARA.DISPLAY:HIDE-RANGES) so that only its first
HEADER records (RECORDS-START, ENDS as it takes it) and the lines of RUNS
\(LINES-MATCHING) are shown: from the
line end of each line shown to the one ending the last line hidden after
it, from the carriage return before a newline when CRLF; the lines hidden at
the very start, before any line shown, from the start to the first line
shown. O(runs)."
  (let* ((total (tuatara.text:rope-bytes rope))
         (out (make-array (+ 2 (length runs)) :element-type 'fixnum :fill-pointer 0))
         (shown-end (records-start rope header ends)))
    (labels ((text-end-before (at)
               ;; where the text of the line ending just before AT, a line's start, ends
               (if (plusp at) (tuatara.text:rope-line-text-end rope 0 (1- at) crlf) 0))
             (hide (from to)
               ;; the lines from FROM, a line's start, to TO, the end of the last of them
               (let ((start (text-end-before from)))
                 (when (< start to) (vector-push start out) (vector-push to out)))))
      (loop for i from 0 below (length runs) by 2
            do (when (< shown-end (aref runs i))
                 (if (zerop shown-end)
                     ;; no line shown before these: they go with their line breaks
                     (hide 0 (aref runs i))
                     (hide shown-end (text-end-before (aref runs i)))))
               (setf shown-end (aref runs (1+ i))))
      (when (< shown-end total)
        (hide shown-end (if (and (plusp shown-end) (tuatara.text:rope-line-start-p rope total))
                            (text-end-before total)
                            total))))
    (coerce out '(simple-array fixnum (*)))))

(defun screen-deadline ()
  "The internal real time by which the work at a key typed shows what it found."
  (+ (get-internal-real-time) (ceiling (* *search-filter-screen-seconds* internal-time-units-per-second))))

(defun window-rows-of (buffer)
  "How many rows the window showing BUFFER has, 50 when none shows it."
  (let ((window (and *editor* (buffer-window buffer))))
    (if window (tuatara.display:window-height window) 50)))

(defun lines-on-screen (buffer chain)
  "The lines of BUFFER's text from the top of its window on that every filter
of CHAIN matches, as many as fill the window, or as are found within
*SEARCH-FILTER-SCREEN-SECONDS*: runs, as LINES-MATCHING gives them, and how
many lines they hold."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (window (and *editor* (buffer-window buffer)))
         (top (max (records-start rope (list-filtering-lines (list-filtering buffer)) (record-ends buffer))
                   (if window (tuatara.buffer:buffer-line-start buffer (tuatara.display:window-start window)) 0)))
         (deadline (screen-deadline)))
    (multiple-value-bind (runs count)
        (lines-matching chain rope (make-array 2 :element-type 'fixnum :initial-contents (list top (tuatara.text:rope-bytes rope)))
                       (buffer-crlf-p buffer)
                       :ends (record-ends buffer)
                       :most (window-rows-of buffer)
                       :halt (lambda () (> (get-internal-real-time) deadline)))
      (values runs count))))

(defun list-on-screen (buffer filtering source chain)
  "The entries of SOURCE, the whole list FILTERING narrows, from the one at
the top of BUFFER's window on, that every filter of CHAIN matches by its
line, as many as fill the window, or as are found within
*SEARCH-FILTER-SCREEN-SECONDS*: their bits and how many, those before it
and after the last looked at left out. The lines are matched a few hundred
at a time, each time as LIST-LINES-MATCHED matches them. What more a list
gives of its entries is left to the background, and so are the filters
looking at a part of them."
  (let* ((entries (list-source-entries source))
         (lines (list-source-entry-lines source))
         (total (length entries))
         (bits (make-array total :element-type 'bit :initial-element 0))
         (window (and *editor* (buffer-window buffer)))
         (top (or (and window (let ((entry (list-entry-at buffer (tuatara.display:window-start window))))
                                (and entry (position entry entries))))
                  0))
         (rows (window-rows-of buffer))
         (deadline (screen-deadline))
         (count 0) (i top))
    (when (some #'search-filter-where chain)
      (return-from list-on-screen (values bits 0)))
    (loop while (and (< i total) (< count rows) (<= (get-internal-real-time) deadline))
          do (let* ((high (let ((at (min total (+ i (max 256 (* 4 rows)))))) (if (< at total) (entry-end entries (1- at)) at)))
                    (left (make-array (- high i) :element-type 'bit :initial-element 0)))
               ;; the lines every filter so far leaves, from the entries' lines left before
               (loop for k from i below high
                     unless (heading-entry-p filtering (svref entries k)) do (setf (sbit left (- k i)) 1))
               (dolist (filter chain)
                 (let ((matched (list-lines-matched filter lines i high (lambda (k) (= 1 (sbit left (- k i))))))
                       (k i))
                   (loop while (< k high)
                         do (let ((end (entry-end entries k)))
                              (when (= 1 (sbit left (- k i)))
                                (unless (eq (not (search-filter-hide filter)) (and (find 1 matched :start (- k i) :end (- end i)) t))
                                  (fill left 0 :start (- k i) :end (- end i))))
                              (setf k end)))))
               (loop for k = i then (entry-end entries k)
                     while (< k high)
                     when (= 1 (sbit left (- k i)))
                       do (fill bits 1 :start k :end (entry-end entries k))
                          (incf count))
               (setf i high)))
    (values bits count)))

(defstruct (search-filter-job (:include tuatara.runtime:job) (:constructor make-search-filter-job (buffer filtering source chain))
                              (:copier nil))
  "Filters of a list or a file of records worked out in the background for
BUFFER: those of CHAIN, FILTERING's at the start, for SOURCE, its whole list
or the version of its text; DONE of TOTAL entries looked at so far, or for
records the octets looked at in PROGRESS's car of those in its cdr, on
filter PASS of PASSES; RESULTS, what each filter worked out leaves, each
\(FILTER KEPT COUNT LOOKED), kept in the filters when the job is taken in
while they are still the list's; WHOLE true when the first of them looked at
every record, so that its LOOKED is how many there are."
  filtering source chain (done 0) (total 1) (results '())
  (progress (cons 0 1)) (pass 0) (passes 1) (whole nil))

(setf (documentation 'search-filter-job-p 'function) "True when OBJECT is a SEARCH-FILTER-JOB.")

(defun search-filter-job-lines-p (job)
  "True when JOB filters the records of a file rather than a list."
  (list-filtering-lines (search-filter-job-filtering job)))

(tuatara.runtime:define-job-words search-filter-job
  :doing "Filtering"
  :description (format nil "Filtering the ~:[list~;records~] by ~{~a~^, ~}" (search-filter-job-lines-p job)
                       (mapcar #'search-filter-words (search-filter-job-chain job)))
  :verb (if (search-filter-job-lines-p job) "filter the records" "filter the list")
  :left-words "the filters not yet worked out are taken away"
  :at-exit :quiet)

(defmethod tuatara.runtime:job-percent ((job search-filter-job))
  "How many of the entries, or of the octets of the records, to look at have been."
  (if (search-filter-job-lines-p job)
      (let ((progress (search-filter-job-progress job)))
        (tuatara.runtime:running-percent (+ (search-filter-job-pass job) (/ (car progress) (max 1 (cdr progress))))
                                         (search-filter-job-passes job)))
      (tuatara.runtime:running-percent (search-filter-job-done job) (search-filter-job-total job))))

(defmethod tuatara.runtime:job-holds-queue-p ((job search-filter-job))
  "Filtering a list lets other work on it start beside it."
  nil)

(defmethod tuatara.runtime:job-take-in ((job search-filter-job))
  "Show the list or the records narrowed, when the filters are still those
worked out; when the job was stopped, or failed, take away the filters it
did not work out, showing what those before them leave."
  (let* ((buffer (tuatara.runtime:job-buffer job))
         (filtering (search-filter-job-filtering job))
         (source (search-filter-job-source job))
         ;; any change to the filters or the list stops it and starts another
         (current (eq (list-filtering-job filtering) job)))
    (when current
      (setf (list-filtering-job filtering) nil))
    (when (and current (live-buffer-p buffer) (eq (list-filtering-source filtering) source))
      (cond ((eq (tuatara.runtime:job-status job) :done)
             (let ((results (reverse (search-filter-job-results job))))
               (loop for (filter kept count) in results
                     do (keep-worked-out filter source kept count))
               (when (and (search-filter-job-whole job) results)
                 (setf (list-filtering-total filtering) (fourth (first results))))))
            (t (setf (list-filtering-filters filtering)
                     (remove-if-not (lambda (filter) (eq (search-filter-source filter) source))
                                    (list-filtering-filters filtering)))
               (let ((typing (list-filtering-typing filtering)))
                 (when (and typing (not (eq (search-filter-source typing) source)))
                   (setf (list-filtering-typing filtering) nil)))))
      (shown-as-last-filter filtering)
      (show-filtered buffer))))

(defun filtering-slowly-p (buffer)
  "True while filters of BUFFER, a list or a file of records, are worked out in
the background because a pattern was too slow on its text for the editor's
thread."
  (let ((filtering (list-filtering buffer)))
    (and filtering (list-filtering-slow filtering) (list-filtering-job filtering) t)))

(defun take-in-filter-jobs (buffer)
  "Take in the jobs filtering BUFFER, a list or a file of records, that have
ended, shown narrowed, and start the one waiting for typing to pause once it
has (*TYPING-PAUSE*): true while one still runs, or the seconds still to
wait."
  (tuatara.runtime:take-in-jobs (lambda (job) (and (search-filter-job-p job) (eq (tuatara.runtime:job-buffer job) buffer))))
  (let ((filtering (list-filtering buffer)))
    (cond ((null filtering) nil)
          ((and (list-filtering-due filtering) (filter-pause-left filtering)))
          ((list-filtering-due filtering) (start-due-filter-job buffer) t)
          (t (and (list-filtering-job filtering) t)))))

(defun filter-pause-left (filtering)
  "Seconds still to wait before FILTERING's job starts (TYPING-PAUSE-LEFT for
the text it narrows), or NIL."
  (let ((source (list-filtering-source filtering)))
    (typing-pause-left (if (list-filtering-lines filtering)
                           (tuatara.text:rope-bytes (tuatara.text:version-rope source))
                           (length (list-source-text source))))))

(defun start-due-filter-job (buffer)
  "Start the job of BUFFER's filters that waits for typing to pause, when
one waits."
  (let* ((filtering (list-filtering buffer))
         (due (and filtering (list-filtering-due filtering))))
    (when due
      (setf (list-filtering-due filtering) nil)
      (start-filter-job buffer filtering (first due) (second due)))))

(defun stop-filtering-job (filtering)
  "Stop the job working out FILTERING's filters, when one runs or waits."
  (setf (list-filtering-due filtering) nil)
  (let ((job (list-filtering-job filtering)))
    (when job
      (setf (list-filtering-job filtering) nil)
      (tuatara.runtime:job-stop job))))

(defun work-out-now (buffer filtering filter source previous)
  "Work out FILTER, of FILTERING, BUFFER's, for SOURCE on this thread, from what
PREVIOUS leaves: its entries' bits, or for records its runs of lines, kept
in it (KEEP-WORKED-OUT), and returned."
  (if (list-filtering-lines filtering)
      (let ((rope (tuatara.text:version-rope source)))
        (multiple-value-bind (runs count looked)
            (lines-matching (list filter) rope (or previous (all-records rope (list-filtering-lines filtering) (record-ends buffer)))
                           (buffer-crlf-p buffer) :ends (record-ends buffer))
          (unless previous (setf (list-filtering-total filtering) looked))
          (keep-worked-out filter source runs count)))
      (multiple-value-call #'keep-worked-out filter source (work-out-filter filtering filter source previous))))

(defvar *filter-as-typed* nil
  "True while a filter is worked out for what is typed at its prompt, its job
then waiting for typing to pause.")

(defun work-out-in-background (buffer filtering left previous)
  "Start the job working out the filters LEFT of FILTERING, BUFFER's, from
what PREVIOUS leaves, on all but two cores, while a filter is typed once
typing pauses (*FILTER-AS-TYPED*, *TYPING-PAUSE*); meanwhile what the whole
chain matches from the top of the window is shown (LIST-ON-SCREEN,
LINES-ON-SCREEN)."
  (let* ((source (list-filtering-source filtering))
         (chain (search-filter-chain filtering))
         (lines (list-filtering-lines filtering)))
    (multiple-value-bind (shown count)
        (tuatara.runtime:stoppable-work ((list nil) :slow tuatara.runtime:*slow-call-seconds*)
          (if lines (lines-on-screen buffer chain) (list-on-screen buffer filtering source chain)))
      (when (eq count :slow)
        ;; a pattern slow on the lines from the window's top: nothing shown till the job has done
        (setf (list-filtering-slow filtering) t
              shown (if lines
                        (make-array 0 :element-type 'fixnum)
                        (make-array (length (list-source-entries source)) :element-type 'bit :initial-element 0))
              count 0))
      (setf (list-filtering-shown filtering) shown (list-filtering-shown-count filtering) count))
    (if (and *filter-as-typed* (filter-pause-left filtering))
        (setf (list-filtering-due filtering) (list left previous))
        (start-filter-job buffer filtering left previous))))

(defun start-filter-job (buffer filtering left previous)
  "Start the job working out the filters LEFT of FILTERING, BUFFER's, from
what PREVIOUS leaves, on all but two cores, the whole shown narrowed again
once it is done."
  (let* ((source (list-filtering-source filtering))
         (job (make-search-filter-job buffer filtering source (search-filter-chain filtering)))
         (lines (list-filtering-lines filtering)))
    (setf (list-filtering-job filtering) job)
    (if lines
        (let* ((rope (tuatara.text:version-rope source))
               (crlf (buffer-crlf-p buffer))
               (ends (record-ends buffer))
               (header (list-filtering-lines filtering))
               (hook *rewrite-piece-hook*))
          (setf (search-filter-job-passes job) (length left) (search-filter-job-whole job) (null previous))
          (tuatara.runtime:start-job-thread
           job (lambda (job)
                 (let ((ranges (or previous (all-records rope header ends)))
                       (cancel (tuatara.runtime:job-cancel job)))
                   (dolist (filter left)
                     (setf (search-filter-job-progress job) (cons 0 (max 1 (runs-octets ranges))))
                     (multiple-value-bind (runs count looked)
                         (lines-matching-everywhere filter rope ranges crlf (search-filter-job-progress job)
                                                   (lambda () (car cancel)) hook ends)
                       (tuatara.runtime:job-check-stop)
                       (push (list filter runs count looked) (search-filter-job-results job))
                       (incf (search-filter-job-pass job))
                       (setf ranges runs)))))
           :name "tuatara record filter"))
        (progn
          (list-source-entry-lines source)
          (setf (search-filter-job-total job)
                (* (length left) (max 1 (if previous (count 1 previous) (length (list-source-entries source))))))
          (tuatara.runtime:start-job-thread
           job (lambda (job)
                 (let ((bits previous))
                   (dolist (filter left)
                     (multiple-value-bind (kept count)
                         (work-out-filter filtering filter source bits
                                          (lambda ()
                                            (when (zerop (mod (incf (search-filter-job-done job)) 64))
                                              (tuatara.runtime:job-check-stop))))
                       (push (list filter kept count) (search-filter-job-results job))
                       (setf bits kept)))))
           :name "tuatara list filter")))))

(defun work-out-filters (buffer filtering)
  "Work out the filters of FILTERING, BUFFER's, not yet worked out for its
whole list or its text: at once when that is quick (FILTER-QUICKLY-P), else
in the background, what the filters match from the window's top shown
meanwhile, and the whole shown narrowed again once they are done; what is
shown is then what the last filter leaves. True when they are all worked
out now. Work on the editor's thread held up by one line longer than
TUATARA.RUNTIME:*SLOW-CALL-SECONDS*, a slow pattern, goes on in the
background (SLOW in FILTERING then)."
  (stop-filtering-job filtering)
  (setf (list-filtering-slow filtering) nil)
  (multiple-value-bind (left previous) (filters-to-work-out filtering)
    (let ((source (list-filtering-source filtering)))
      (let ((quick (filter-quickly-p filtering previous)))
        (cond ((null left))
              ((and quick
                    (tuatara.runtime:stoppable-work ((list nil) :slow tuatara.runtime:*slow-call-seconds*)
                      (dolist (filter left t)
                        (setf previous (work-out-now buffer filtering filter source previous))))))
              (t (when quick (setf (list-filtering-slow filtering) t))
                 ;; what a slow pattern left not worked out here goes on in the background
                 (multiple-value-bind (left previous) (filters-to-work-out filtering)
                   (work-out-in-background buffer filtering left previous))
                 (return-from work-out-filters nil))))
      (shown-as-last-filter filtering)
      t)))

(defun shown-as-last-filter (filtering)
  "Have the list FILTERING narrows show the entries its last filter leaves,
all of them when it has none."
  (let ((last (car (last (search-filter-chain filtering)))))
    (setf (list-filtering-shown filtering) (and last (search-filter-kept last))
          (list-filtering-shown-count filtering) (if last (search-filter-count last) 0))))

;;; The list shown narrowed

(defun list-shown-lines (filtering source &optional folded)
  "Which lines of SOURCE's text FILTERING shows, as a function of a line's
number: those before and after the entries, the entries its last worked out
filter leaves, lines of no entry, the headings of sections with an entry
shown, or with none at all unless the list says otherwise (EMPTY-HEADINGS),
and the lines that go with a heading shown (DECORATION-ENTRY-P); but no line
under the heading of a section of FOLDED
\(its names, as LIST-FOLDED gives them), whose heading alone shows. An empty
line of no entry, as between sections, is shown only between lines shown,
one of a run."
  (let* ((entries (list-source-entries source))
         (shown (list-filtering-shown filtering))
         (first (list-source-first-line source))
         (keep (make-array (length entries) :element-type 'bit :initial-element 1)))
    (when shown
      ;; from the end, so that a heading knows whether an entry under it is shown
      (let ((any nil) (under nil) (empty (list-filtering-empty-headings filtering)))
        (loop for i from (1- (length entries)) downto 0
              for entry = (svref entries i)
              do (cond ((null entry))
                       ((decoration-entry-p filtering entry))
                       ((heading-entry-p filtering entry)
                        (setf (sbit keep i) (if (or any (and empty (not under))) 1 0) any nil under nil))
                       ((= 1 (sbit shown i)) (setf any t under t))
                       (t (setf (sbit keep i) 0 under t)))))
      ;; a line that goes with a heading shows as that heading does
      (when (list-filtering-decoration filtering)
        (let ((heading-kept 1))
          (loop for i from 0 below (length entries)
                for entry = (svref entries i)
                do (cond ((decoration-entry-p filtering entry) (setf (sbit keep i) heading-kept))
                         ((heading-entry-p filtering entry) (when entry (setf heading-kept (sbit keep i)))))))))
    ;; a folded section's lines hidden, its heading kept: the list has them all while filters narrow it
    (when folded
      (let ((hiding nil))
        (loop for i from 0 below (length entries)
              for entry = (svref entries i)
              do (cond ((and (consp entry) (eq (first entry) :section))
                        (setf hiding (and (member (second entry) folded :test #'equal) t)))
                       ((and hiding (or entry (plusp (length (svref (list-source-entry-lines source) i)))))
                        (setf (sbit keep i) 0))))))
    (when (or shown folded)
      (let ((lines (list-source-entry-lines source)) (before nil) (blank nil))
        (loop for i from 0 below (length entries)
              do (cond ((and (null (svref entries i)) (zerop (length (svref lines i))))
                        (setf (sbit keep i) 0)
                        (when (and before (not blank)) (setf blank i)))
                       ((= 1 (sbit keep i))
                        (when blank (setf (sbit keep blank) 1 blank nil))
                        (setf before t))))))
    (lambda (line)
      (let ((i (- line first)))
        (or (not (< -1 i (length entries))) (= 1 (sbit keep i)))))))

(defun moved-spans (spans segments)
  "SPANS, each (START END . MORE) in octets of a text, as they fall in the
text made of its SEGMENTS, each (FROM TO AT): the octets FROM below TO put at
AT; a span cut where a segment ends, the parts of it outside the segments
left out. Both in the order of their starts."
  (let ((out '()) (segments (coerce segments 'simple-vector)))
    (dolist (span (coerce spans 'list))
      (destructuring-bind (start end . more) span
        (let ((i (tuatara.runtime:bisect (middle 0 (length segments)) (<= (second (svref segments middle)) start))))
          (loop for k from i below (length segments)
                for (from to at) = (svref segments k)
                while (< from end)
                do (let ((s (max start from)) (e (min end to)))
                     (when (< s e) (push (list* (+ at (- s from)) (+ at (- e from)) more) out)))))))
    (nreverse out)))

(defun filter-left (filtering total)
  "How many of TOTAL entries or records FILTERING's filters leave, or NIL
while they are being worked out in the background or wait to be."
  (unless (or (list-filtering-job filtering) (list-filtering-due filtering))
    (if (list-filtering-shown filtering) (list-filtering-shown-count filtering) (or total 0))))

(defun filter-total (filtering)
  "How many entries FILTERING's list has, or records its file has; NIL while
not known."
  (if (list-filtering-lines filtering)
      (list-filtering-total filtering)
      (listed-entry-count filtering (list-filtering-source filtering))))

(defun filter-count-words (filtering total)
  "What FILTERING's filters leave of TOTAL entries or records (NIL while not
known), for a list's first line and a file's mode line: Filtered by kiwi,
nz: 12 of 1,000 records; while they are being worked out in the
background, Filtered by kiwi: ... of 1,000 records (counting)."
  (let ((patterns (mapcar #'search-filter-words (search-filter-chain filtering)))
        (nouns (list-filtering-nouns filtering))
        (left (filter-left filtering total)))
    (if left
        (format nil "Filtered by ~{~a~^, ~}: ~:d of ~:d ~a" patterns left (or total 0) nouns)
        (format nil "Filtered by ~{~a~^, ~}: …~@[ of ~:d~] ~a (counting)" patterns total nouns))))

(defun filter-count-figures (buffer)
  "What the filter being typed on BUFFER, a list or a file of records, and the
ones kept leave of it, as its question shows that before its prompt: 12/1000;
NIL while not known, or with nothing typed."
  (let ((filtering (list-filtering buffer)))
    (when (and filtering (list-filtering-typing filtering) (list-filtering-source filtering))
      (let* ((total (filter-total filtering))
             (left (and total (filter-left filtering total))))
        (and left (format nil "~d/~d" left total))))))

(defun filter-summary (filtering)
  "The line a list narrowed by FILTERING's filters starts with: the patterns,
how many of its entries are shown (FILTER-COUNT-WORDS), and the keys for the
filters."
  (substitute-command-keys
   (format nil "~a; \\[list-search-filters] lists the filters, \\[remove-search-filters] takes them away~%"
           (filter-count-words filtering (listed-entry-count filtering (list-filtering-source filtering))))))

(defun filtered-list-text (filtering &optional folded)
  "The text FILTERING's whole list shows narrowed, as five values: text, spans,
buttons, entries and the line of the first, a line saying how it is
narrowed above the rest; the lines of the sections FOLDED hidden under
their headings (LIST-SHOWN-LINES). It ends in a line break when the whole
list does, so that its last line is an entry's as the whole list's is.
O(size of the text)."
  (let* ((source (list-filtering-source filtering))
         (text (list-source-text source))
         (shows (list-shown-lines filtering source folded))
         (summary (filter-summary filtering))
         (summary-octets (tuatara.text:string-octet-length summary))
         (entries (list-source-entries source))
         (first (list-source-first-line source))
         (out (make-string-output-stream))
         (segments '()) (kept '()) (octet 0) (at summary-octets) (start 0) (line 0) (line-break nil))
    (write-string summary out)
    ;; each line's break written before the next line shown, so that none ends the text the whole list does not end in
    (loop while (< start (length text))
          do (let* ((end (let ((e (position #\Newline text :start start))) (if e (1+ e) (length text))))
                    (body (if (char= (char text (1- end)) #\Newline) (1- end) end))
                    (octets (tuatara.text:string-octet-length text :start start :end end)))
               (when (funcall shows line)
                 (when line-break
                   (write-char #\Newline out)
                   (incf at))
                 (write-string text out :start start :end body)
                 (push (list octet (+ octet (- octets (- end body))) at) segments)
                 (incf at (- octets (- end body)))
                 (setf line-break (< body end))
                 (let ((i (- line first)))
                   (when (< -1 i (length entries)) (push (svref entries i) kept))))
               (incf octet octets)
               (setf start end)
               (incf line)))
    (when (and line-break (char= (char text (1- (length text))) #\Newline))
      (write-char #\Newline out))
    (let ((segments (nreverse segments)))
      (values (get-output-stream-string out)
              (cons (list 0 (max 0 (1- summary-octets)) :list-filters) (moved-spans (list-source-spans source) segments))
              (moved-spans (list-source-buttons source) segments)
              (nreverse kept)
              (1+ first)))))

(defun filter-list-text (buffer text spans buttons entries first-line capture)
  "What BUFFER, a list made with ENTRIES from line FIRST-LINE of TEXT (with its
SPANS and BUTTONS), shows while search filters narrow it: the five as they
are when none do; else the entries the filters leave, under a line saying
so (FILTERED-LIST-TEXT). With CAPTURE, TEXT is the list made anew, kept as
the whole list and narrowed again: a list searched in the background, drawn
again with no entry it had not, keeps what the filters left of it
\(CARRY-FILTERS-OVER)."
  (let ((filtering (list-filtering buffer)))
    (cond ((or (null filtering) (null (search-filter-chain filtering)))
           (when filtering
             (stop-filtering-job filtering)
             (setf (list-filtering-source filtering) nil (list-filtering-shown filtering) nil))
           (values text spans buttons entries first-line))
          (t (when (or capture (null (list-filtering-source filtering)))
               (let ((old (list-filtering-source filtering))
                     (new (make-list-source text spans buttons (coerce entries 'simple-vector) first-line)))
                 (setf (list-filtering-source filtering) new)
                 ;; a list searched in the background, drawn again with the same entries: its matches kept
                 (if (and old (null (list-filtering-job filtering)) (not (filter-quickly-p filtering))
                          (carry-filters-over filtering old new))
                     (shown-as-last-filter filtering)
                     (progn (setf (list-filtering-shown filtering) nil)
                            (work-out-filters buffer filtering)))))
             (filtered-list-text filtering (list-folded buffer))))))

(defun drop-search-filters (buffer)
  "Take every search filter off BUFFER, a list, and the one being worked out,
without showing it again: for a list about to show other entries altogether,
which it then shows whole. In a file whose lines they hid, every line is
shown again, and the filtering forgotten, as when it leaves the mode whose
records they were (FILTERS-HIDE-LINES-P)."
  (let ((filtering (list-filtering buffer)))
    (when filtering
      (stop-filtering-job filtering)
      (setf (list-filtering-filters filtering) '() (list-filtering-typing filtering) nil
            (list-filtering-source filtering) nil (list-filtering-shown filtering) nil)
      (when (list-filtering-lines filtering)
        (tuatara.display:hide-ranges buffer nil)
        (setf *buffers-filtered-by-lines* (remove buffer *buffers-filtered-by-lines*)
              (tuatara.buffer:buffer-local buffer 'list-filtering) nil)))))

(defun list-filters-on-p (buffer)
  "True when search filters narrow BUFFER, a list: it is then made with the
members of its folded sections too, which the filtering hides, so that what
the filters leave is counted and judged in the whole list."
  (let ((filtering (list-filtering buffer)))
    (and filtering (search-filter-chain filtering) t)))

(defun keys-hidden-by-filters (list keys)
  "Those of KEYS, the mark keys of entries of LIST (ENTRY-MARK-KEY), whose
entries the search filters narrowing LIST hide now, in the order of KEYS:
what a command on the marked entries takes though they are not on the
screen. A key of no entry of the whole list (a file marked in another
directory) is not hidden by them. NIL when filters narrow LIST not. O(entries)."
  (let ((source (and (list-filters-on-p list) (list-filtering-source (list-filtering list)))))
    (when source
      (let ((hidden (make-hash-table :test 'equal)))
        (loop for entry across (list-source-entries source)
              do (let ((key (entry-mark-key list entry))) (when key (setf (gethash key hidden) t))))
        (loop for entry across (coerce (tuatara.buffer:buffer-local list 'list-entries #()) 'vector)
              do (let ((key (entry-mark-key list entry))) (when key (remhash key hidden))))
        (remove-if-not (lambda (key) (gethash key hidden)) keys)))))

(defun show-filtered-list (buffer)
  "Show BUFFER, a list, as its search filters narrow it now, from the whole
list it keeps, point on the entry it was on while that is shown, what it
was made from (its key, KEEP-SHOWN-CURRENT) as it was; with its filters gone
and sections folded, made again, folded as they are."
  (let* ((filtering (list-filtering buffer))
         (source (and filtering (list-filtering-source filtering))))
    (when (and source (null (search-filter-chain filtering)) (list-folded buffer))
      (stop-filtering-job filtering)
      (setf (list-filtering-source filtering) nil (list-filtering-shown filtering) nil)
      (redraw-list buffer)
      (return-from show-filtered-list))
    (when source
      (show-list-text buffer (list-source-text source) (list-source-spans source) (list-source-buttons source)
                      :entries (list-source-entries source) :first-line (list-source-first-line source)
                      :key (tuatara.buffer:buffer-local buffer 'list-key) :capture nil))))

(defun refilter-list (buffer)
  "Narrow BUFFER, a list, again after its filters changed: at once where
that is quick, else showing it as it was till the filters are worked out in
the background."
  (let ((filtering (list-filtering buffer)))
    (cond ((null filtering))
          ((null (list-filtering-source filtering))
           ;; narrowed for the first time: its text now is the whole list
           (when (search-filter-chain filtering)
             (let ((entries (tuatara.buffer:buffer-local buffer 'list-entries))
                   (made (tuatara.buffer:buffer-local buffer 'made-spans)))
               (show-list-text buffer (tuatara.buffer:buffer-string buffer)
                               (and made (coerce (car made) 'list))
                               (tuatara.buffer:buffer-local buffer :buttons)
                               :entries entries :first-line (tuatara.buffer:buffer-local buffer 'list-first-line 0)
                               :key (tuatara.buffer:buffer-local buffer 'list-key)))))
          (t (work-out-filters buffer filtering)
             (show-filtered-list buffer)))))

;;; Records shown narrowed

(defun show-filtered-lines (buffer)
  "Hide the lines of BUFFER that its search filters leave out, as worked out
for the version of its text they are worked out for, carried on to the text
as it is (TUATARA.DISPLAY:HIDE-RANGES); every line shown when none is
worked out yet."
  (let* ((filtering (list-filtering buffer))
         (source (list-filtering-source filtering))
         (shown (list-filtering-shown filtering)))
    (tuatara.display:hide-ranges
     buffer (and source shown (hidden-lines-bounds (tuatara.text:version-rope source) shown (buffer-crlf-p buffer)
                                                   (list-filtering-lines filtering) (record-ends buffer)))
     :version source)))

(defun refilter-lines (buffer)
  "Narrow BUFFER, whose lines search filters hide, again after its filters or
its text changed: when its text changed since the filters were worked out,
every filter is worked out again for it as it is now, the lines edited
meanwhile included; else only those not worked out yet. With no filters
left, every line is shown."
  (let ((filtering (list-filtering buffer)))
    (stop-filtering-job filtering)
    (cond ((null (search-filter-chain filtering))
           (setf (list-filtering-source filtering) nil (list-filtering-shown filtering) nil
                 (list-filtering-total filtering) nil
                 *buffers-filtered-by-lines* (remove buffer *buffers-filtered-by-lines*))
           (tuatara.display:hide-ranges buffer nil))
          (t (pushnew buffer *buffers-filtered-by-lines*)
             (let ((source (list-filtering-source filtering)))
               (unless (and source (text-unchanged-since source buffer))
                 (dolist (filter (search-filter-chain filtering))
                   (setf (search-filter-source filter) nil (search-filter-kept filter) nil))
                 (setf (list-filtering-source filtering) (tuatara.buffer:buffer-version buffer)
                       (list-filtering-total filtering) nil)))
             (work-out-filters buffer filtering)
             (show-filtered-lines buffer)))))

(defun show-filtered (buffer)
  "Show BUFFER as its search filters narrow it now: a list made again from
the whole list it keeps (SHOW-FILTERED-LIST), or a file's lines hidden
\(SHOW-FILTERED-LINES)."
  (if (filters-lines-p buffer) (show-filtered-lines buffer) (show-filtered-list buffer)))

(defun refilter (buffer)
  "Narrow BUFFER, a list or a file of records, again after its filters
changed (REFILTER-LIST, REFILTER-LINES)."
  (if (filters-lines-p buffer) (refilter-lines buffer) (refilter-list buffer)))

(defun filtered-counts (buffer)
  "How many entries or records BUFFER's search filters leave and how many it
has, two values; NIL as the second while not known."
  (let ((filtering (list-filtering buffer)))
    (values (list-filtering-shown-count filtering) (filter-total filtering))))

(defun filters-mode-line (editor buffer)
  "Mode line function: in a file whose lines search filters hide, what they
leave of its records (FILTER-COUNT-WORDS): Filtered by kiwi: 12 of 1,000
records."
  (declare (ignore editor))
  (let ((filtering (list-filtering buffer)))
    (when (and filtering (list-filtering-lines filtering) (search-filter-chain filtering))
      (filter-count-words filtering (list-filtering-total filtering)))))

(defun refilter-touched (editor)
  "Tick function: work the search filters out again in each buffer whose
lines they hide where a change reached into those lines, or replaced its
whole text (reading its file again, sorting it), in the background where
that takes time."
  (declare (ignore editor))
  (dolist (buffer *buffers-filtered-by-lines*)
    (cond ((not (live-buffer-p buffer))
           (setf *buffers-filtered-by-lines* (remove buffer *buffers-filtered-by-lines*)))
          ((tuatara.display:hidden-touched-p buffer)
           (refilter-lines buffer))))
  nil)

(defun refilter-reloaded (buffer)
  "After BUFFER's file was read again: when search filters hide its lines,
work them out again for its new text, in the background where that takes
time."
  (when (and (filters-lines-p buffer) (search-filter-chain (list-filtering buffer)))
    (refilter-lines buffer)))

(defun guard-hidden-lines (buffer edits)
  "Before-edit function (TUATARA.BUFFER:*BEFORE-EDIT-FUNCTIONS*): refuse
EDITS of BUFFER that would change the lines search filters hide there
\(TUATARA.DISPLAY:HIDDEN-EDIT-P), whatever command makes them. The whole
text replaced at once (reading the file again, a sort done in the
background) and undo and redo go through, the filters worked out again
after them (REFILTER-TOUCHED)."
  (let ((bounds (tuatara.display:hidden-bounds buffer)))
    (when (and bounds
               (not (and *editor* (member (editor-this-command *editor*) '(undo redo))))
               (not (and (= 1 (length edits))
                         (let ((edit (aref edits 0)))
                           (and (zerop (tuatara.text:edit-start edit))
                                (= (tuatara.text:edit-end edit) (tuatara.buffer:buffer-size buffer))
                                (null (tuatara.text:edit-insert edit)))))))
      (let ((rope (tuatara.buffer:buffer-text buffer)))
        (loop for edit across edits
              for start = (tuatara.text:edit-start edit)
              for end = (tuatara.text:edit-end edit)
              do (loop for i from (stretch-before bounds start) below (length bounds) by 2
                       while (<= (aref bounds i) end)
                       when (tuatara.display:hidden-edit-p edit (aref bounds i) (aref bounds (1+ i)) rope)
                         do (refuse-hidden-lines)))))))

(defun stretch-before (bounds position)
  "The index in BOUNDS (TUATARA.DISPLAY:HIDE-RANGES) of the first stretch
that ends at POSITION or after it, less one, by halving."
  (let ((k (tuatara.runtime:bisect (k 0 (ash (length bounds) -1)) (< (1+ (aref bounds (1+ (* 2 k)))) position))))
    (* 2 k)))

;;; Typing, keeping and taking away filters

(define-major-mode search-filters-mode special-mode
  "The search filters narrowing a list (\\[list-search-filters]), the first
first: each pattern, whether it is a regular expression or literal text,
whether it ignores case, and how many entries it leaves of those the filters
before it left. \\<search-filters-mode>\\[remove-search-filter] takes away the
filter on the line, the list widening, and \\[remove-search-filters] all of
them; \\[revert-buffer] lists them again; \\[quit-window] returns."
  :lighter "Filters" :interactive nil)

(defun filter-list-as-typed (state)
  "Narrow the list STATE, a search's state, filters (SEARCH-STATE-FILTERING)
to what its string matches, as the filter being typed; nothing when the
pattern does not compile yet, the list as it was; with nothing typed, as the
filters kept narrow it."
  (let* ((buffer (search-state-filtering state))
         (filtering (list-filtering-made buffer))
         (typing (list-filtering-typing filtering))
         (string (search-state-string state)))
    (setf (search-state-failing state) nil)
    (cond ((search-problem state))
          ((zerop (length (search-line-pattern (search-line-of state))))
           (when typing
             (setf (list-filtering-typing filtering) nil)
             (refilter buffer)))
          ((and typing (string= string (search-filter-string typing))
                (eq (and (search-state-regex state) t) (search-filter-regex-p typing))))
          (t (setf (list-filtering-typing filtering)
                   (make-search-filter string :regex (search-state-regex state) :matcher (search-matcher state)))
             (let ((*filter-as-typed* t))
               (refilter buffer))))))

(defun keep-typed-filter (buffer)
  "Keep the filter being typed on BUFFER, a list, as one of its filters; the
next narrows what it leaves."
  (let* ((filtering (list-filtering buffer))
         (typing (and filtering (list-filtering-typing filtering))))
    (when typing
      (start-due-filter-job buffer)
      (setf (list-filtering-filters filtering) (append (list-filtering-filters filtering) (list typing))
            (list-filtering-typing filtering) nil)
      (show-filtered buffer)
      (if (list-filtering-job filtering)
          (message "Filter ~a kept; the ~:[list narrows~;records narrow~] once searched" (search-filter-words typing)
                   (list-filtering-lines filtering))
          (multiple-value-bind (shown total) (filtered-counts buffer)
            (message "~:d of ~:d ~a left; \\[list-search-filters] lists the filters"
                     shown (or total 0) (list-filtering-nouns filtering)))))))

(defun drop-typed-filter (buffer)
  "Let the filter being typed on BUFFER, a list, go: the list shows what the
filters kept leave."
  (let ((filtering (list-filtering buffer)))
    (when (and filtering (list-filtering-typing filtering))
      (setf (list-filtering-typing filtering) nil)
      (refilter buffer))))

(defun take-away-filters (buffer filters)
  "Take FILTERS away from those kept on BUFFER, a list: the filters after the
first taken away are worked out again from what those before it leave, and
the list widens."
  (let* ((filtering (list-filtering buffer))
         (kept (list-filtering-filters filtering))
         (first (position-if (lambda (filter) (member filter filters)) kept)))
    (when first
      (dolist (filter (nthcdr first kept))
        (setf (search-filter-source filter) nil (search-filter-kept filter) nil))
      (setf (list-filtering-filters filtering) (remove-if (lambda (filter) (member filter filters)) kept))
      (refilter buffer))))

(defun filtered-list-at-hand ()
  "The list the filter commands act on: the current buffer when it is a list,
or the list the list of filters there is for; NIL when there is none."
  (let ((buffer (current-buffer)))
    (if (typep (buffer-major-mode buffer) 'search-filters-mode)
        (let ((list (tuatara.buffer:buffer-local buffer 'filtered-list)))
          (and list (live-buffer-p list) list))
        (and (search-filterable-p buffer) buffer))))

;;; The commands

(define-command search-filter-regex ()
  "Narrow the list here to the entries a regular expression matches, as it
is typed: those whose line matches, or what more the list holds of them (a
mailbox the headers and contents of its messages). Case is ignored while the
pattern has no capital letter and names no case ([[:upper:]], \\p{Lu}), as a
search ignores it. The pattern is typed in the minibuffer and
edited there as any answer is, the list narrowed again at each change.
\\<search>\\[search-exit] keeps the pattern
as a filter on the list, and the next one narrows what it leaves;
\\[search-abort] lets the one typed go, the filters kept staying.
\\[search-toggle-regex] switches to literal text;
\\<minibuffer>\\[minibuffer-previous-history] and \\[minibuffer-next-history] bring back the patterns typed before,
in any question that takes one (*PATTERN-HISTORY*). The list shows above its
entries how many are left and by what; \\[list-search-filters] lists the
filters, to take one away, and \\[remove-search-filters] takes them all away.
A list made again is narrowed again.
In a file of records, a line each, the records are narrowed the same way:
in a CSV or TSV file, a log, a JSON Lines file and the others
*LINE-RECORD-MODES* names. A line, or an entry's line in a list, is matched
when any part of a match lies on it, from the line holding the match's first
character through the line holding its last, its line end counted with it:
1\\ny keeps the line ending in 1 and the line starting with y, and a block
wrapped over several lines keeps all of them. The lines left out are
hidden, never deleted, a CSV's header line always shown, and the mode line
says how many are left. Saving
writes the whole file; editing the lines shown is allowed, and a line edited
so that it no longer matches stays in sight until the filters are worked out
again (a filter added or taken away, the file read again or sorted), but
nothing changes the lines hidden.
Where a list or file is large, what is typed is matched from the top of the
window first and shown at once (*SEARCH-FILTER-SCREEN-SECONDS*), and the
rest in the background on all but two cores, the mode line saying how far it
has got (\\[stop-job] stops it, taking the filter away); the editor goes on
meanwhile. So is a pattern so slow on some line that matching it there would
hold the editor up, the prompt saying slow pattern. A pattern that cannot be
used shows why in the prompt, and \\<search>\\[search-exit] waits for it to be
mended."
  (let ((buffer (current-buffer)))
    (if (search-filterable-p buffer)
        (progn (list-filtering-made buffer)
               (search-start t t :filtering buffer))
        (message "Only a list or a file of records (a CSV, a log) can be narrowed by a filter: this buffer has none"))))

(defparameter *search-filters-columns*
  '(("Filter" :width 40) ("Kind" :width 26) ("Leaves" :width 12 :right t))
  "The columns of the list of search filters, as TABLE-HEADING-PIECES takes them.")

(defun search-filters-render (list filtered)
  "Fill LIST, a list of search filters, with those narrowing FILTERED, a list."
  (let ((filters (search-filters filtered)))
    (render-pieces
     list
     (append (list (format nil "The filters narrowing ~a, each narrowing what those above it leave:~%~%"
                           (tuatara.buffer:buffer-name filtered)))
             (table-heading-pieces *search-filters-columns*)
             (loop for filter in filters
                   append (table-row-pieces *search-filters-columns*
                                            (list (search-filter-words filter)
                                                  (format nil "~:[literal~;regex~], ~:[case kept~;case ignored~]"
                                                          (search-filter-regex-p filter) (search-filter-fold filter))
                                                  (if (search-filter-kept filter)
                                                      (format nil "~:d" (search-filter-count filter))
                                                      "working out"))))
             (and (null filters) (list (format nil "~%   No filters: the whole list is shown~%"))))
     :point :line :entries filters :first-line 4
     :refill (lambda (list) (search-filters-render list filtered)))))

(defun search-filters-key (list)
  "What the list of search filters LIST shows, cheaply: each filter and
how many entries it leaves; another key means it is out of date."
  (let ((filtered (tuatara.buffer:buffer-local list 'filtered-list)))
    (and filtered (mapcar (lambda (filter) (list filter (and (search-filter-kept filter) (search-filter-count filter))))
                          (search-filters filtered)))))

(keep-shown-current 'search-filters-mode 'search-filters-key)

(define-command list-search-filters ()
  "List the search filters narrowing the list here (\\[search-filter-regex])
in a buffer of their own, each with how many entries it leaves; there
\\<search-filters-mode>\\[remove-search-filter] takes away the filter on the
line and \\[remove-search-filters] all of them."
  (let ((filtered (filtered-list-at-hand)))
    (if (null filtered)
        (message "Not a list: no filters to list")
        (let ((list (buffer-in-mode (format nil "*Filters of ~a*" (tuatara.buffer:buffer-name filtered)) 'search-filters-mode)))
          (setf (tuatara.buffer:buffer-local list 'filtered-list) filtered)
          (search-filters-render list filtered)
          (show-buffer list)))))

(defun refresh-filters-list ()
  "Fill the current buffer again when it is a list of search filters."
  (let ((buffer (current-buffer)))
    (when (typep (buffer-major-mode buffer) 'search-filters-mode)
      (search-filters-render buffer (tuatara.buffer:buffer-local buffer 'filtered-list)))))

(define-command remove-search-filter ()
  "Take away the search filter on the current line of the list of filters
\(\\[list-search-filters]): the list it narrowed widens, the filters after
it narrowing again what those before it leave, in the background where that
takes time."
  (let ((filter (list-entry-at)) (filtered (filtered-list-at-hand)))
    (cond ((not (search-filter-p filter)) (message "No filter on this line"))
          ((null filtered) (message "The list it narrowed is gone"))
          (t (take-away-filters filtered (list filter))
             (refresh-filters-list)
             (message "Filter ~a taken away" (search-filter-words filter))))))

(define-command remove-search-filters ()
  "Take away every search filter narrowing the list here, or the list the
list of filters here is for: the whole list is shown again."
  (let ((filtered (filtered-list-at-hand)))
    (cond ((or (null filtered) (null (search-filters filtered))) (message "No filters to take away"))
          (t (let ((count (length (search-filters filtered))))
               (take-away-filters filtered (search-filters filtered))
               (refresh-filters-list)
               (message "~d filter~:p taken away: ~:[the whole list is~;every record is~] shown"
                        count (filters-lines-p filtered)))))))

(defun filter-records-by (command hide)
  "Narrow the list or file of records here by a filter asked for as
READ-LINE-FILTER asks (a column first in a file of records), kept with the others; with
HIDE, one that leaves what it does not match. COMMAND names the command in
messages."
  (let ((buffer (current-buffer)))
    (if (not (search-filterable-p buffer))
        (message "Only a list or a file of records (a CSV, a log) can be narrowed by a filter: ~(~a~) was not run" command)
        (let* ((filtering (list-filtering-made buffer))
               (words (format nil "~:[show~;hide~] ~a" hide (list-filtering-nouns filtering))))
          (multiple-value-bind (pattern regex matcher what column)
              (if (and (not (filters-lines-p buffer)) (list-table-columns buffer))
                  ;; a table: a column asked for first, its cell tried alone
                  (multiple-value-bind (column title) (read-list-column buffer "Filter by the column: ")
                    (multiple-value-bind (pattern regex) (read-pattern (format nil "~a~@[ whose ~a holds~]" words title))
                      (values pattern regex nil title column)))
                  (multiple-value-bind (pattern regex matcher what) (read-line-filter buffer words 0 (tuatara.buffer:buffer-size buffer))
                    (values pattern regex matcher what nil)))
            (if (zerop (length pattern))
                (message "No pattern: ~(~a~) was not run" command)
                (multiple-value-bind (filter why)
                    (make-search-filter pattern :regex regex :test (and what matcher) :part what :hide hide
                                                :where (and column (list :column column buffer))
                                                :crlf (buffer-crlf-p buffer))
                  (if (null filter)
                      (message "~a: ~(~a~) was not run" (string-capitalize why :end 1) command)
                      (progn (setf (list-filtering-typing filtering) filter)
                             (refilter buffer)
                             (keep-typed-filter buffer))))))))))

(define-command list-show-records ()
  "Show only the records whose line a regular expression matches, hiding
the others, as \\[search-filter-regex] does, deleting nothing. In a file of
records a column is asked for first, and the pattern tried on its value
alone; nothing, \"whole record\" or $* for the whole line. Each column is
offered with the first record's value beside it, by its name or by its
number from 0 ($0, $1): a CSV or TSV by its separator, quotes taken off,
named by its header; a log by its words, apart by spaces, named by its
header when \\[toggle-list-header] makes its first line one; the hosts file
by address and name; the password file by user, password, uid, gid, info,
home and shell; an environment file (.env) by name and value; a Python
requirements file by name, version and markers; todo.txt by done,
priority, completed, created, text, project, context and the keys of its
key:value tags; JSON Lines by its keys, strings read with their escapes
undone, a path of keys apart by dots (user.id) reaching inside. A
.gitignore and every list try the whole line. The filter is kept with the others, the next
narrowing what it leaves; \\[list-search-filters] lists them and
\\[remove-search-filters] takes them all away. \\[list-keep-records] deletes
the others instead."
  (filter-records-by 'list-show-records nil))

(define-command list-hide-records ()
  "Hide the records whose line a regular expression matches, showing the
others, deleting nothing: asked for as \\[list-show-records] asks, kept with
the other filters. \\[list-delete-records] deletes them instead."
  (filter-records-by 'list-hide-records t))

(defun list-header-line (window)
  "Header line function: the first line of WINDOW's buffer, its header, as the
window shows its text, scrolled as far to the side and coloured as it is below
\(by column in a CSV), its spans as the second value."
  (let* ((buffer (tuatara.display:window-buffer window))
         (margin (tuatara.display:window-margin window))
         (rows (tuatara.display:layout (tuatara.buffer:buffer-text buffer) 0
                                       (max 1 (- (tuatara.display:window-width window) margin)) 1
                                       :hscroll (if (tuatara.display:window-wraps-p window) 0 (tuatara.display:window-hscroll window))
                                       :tab-width (tuatara.display:tab-width-of buffer)
                                       :crlf (buffer-crlf-p buffer))))
    (if (zerop (length rows))
        (values "" nil)
        (let ((row (aref (tuatara.display:apply-highlights rows (mode-syntax-highlights (buffer-major-mode buffer) buffer rows)) 0)))
          (values (concatenate 'string (make-string margin :initial-element #\Space) (tuatara.display:row-string row))
                  (mapcar (lambda (span) (list (+ margin (first span)) (+ margin (second span)) (third span)))
                          (tuatara.display:row-spans row)))))))

(defun set-header-lock (buffer on)
  "Keep BUFFER's first line at the top of its windows when ON is true, else
let it scroll away with the rest."
  (setf (tuatara.buffer:buffer-local buffer :header-line) (and on 'list-header-line)))

(define-command toggle-list-header-lock ()
  "Keep the first line of a file of records, its header, at the top of the
window as the rest scrolls under it, coloured as it is below; the same again
lets it scroll away; with a count, kept when it is positive, let go when it
is zero or less. A CSV's first line is its header; in a log and the other
files of records \\[toggle-list-header] makes it one first.
*LIST-HEADER-LOCK* keeps a header there wherever there is one."
  (let ((buffer (current-buffer)))
    (cond ((not (filters-hide-lines-p (buffer-major-mode buffer) buffer))
           (message "Not a file of records (a CSV, a log): toggle-list-header-lock was not run"))
          ((not (list-header-p buffer))
           (message "No header: C-x / t makes the first line one"))
          (t (let ((on (switched-by-count (tuatara.buffer:buffer-local buffer :header-line))))
               (set-header-lock buffer on)
               (message "Header ~:[scrolls with the rest~;kept at the top~]" on))))))

(defmethod mode-exit :before ((mode fundamental-mode) buffer)
  "A file of records leaving its mode: its search filters, its header lock and
what \\[toggle-list-header] said of its first line are let go."
  (when (filters-hide-lines-p mode buffer)
    (set-header-lock buffer nil)
    (drop-search-filters buffer)
    (setf (tuatara.buffer:buffer-local buffer :list-header) :mode)))

(define-command toggle-list-header ()
  "Take the first line of a file of records as its header, or as a record like
the rest; the same again switches back; with a count, a header when it is
positive, a record when it is zero or less. A header is always shown by
search filters and never deleted by \\[list-keep-records] and
\\[list-delete-records], and its words name the columns offered; a record
is filtered with the rest, the columns offered by their numbers but where
the file's kind names them. A CSV's first line is its header unless *CSV-HEADER* is NIL; a
log's, a JSON Lines file's and the others' is a record. Filters set are
worked out again at once."
  (let ((buffer (current-buffer)))
    (if (not (filters-hide-lines-p (buffer-major-mode buffer) buffer))
        (message "Not a file of records (a CSV, a log): toggle-list-header was not run")
        (let ((on (switched-by-count (list-header-p buffer)))
              (filtering (list-filtering buffer)))
          (setf (tuatara.buffer:buffer-local buffer :list-header) on)
          (set-header-lock buffer (and on (or *list-header-lock* (tuatara.buffer:buffer-local buffer :header-line))))
          (when (and filtering (list-filtering-lines filtering))
            (setf (list-filtering-lines filtering) (if on 1 0)
                  (list-filtering-source filtering) nil)
            (refilter buffer))
          (message "First line ~:[a record like the rest~;the header, always shown~]" on)))))

(defun shift-group-references (pattern by)
  "PATTERN, a regular expression, with each back reference (\\1, \\12) naming
the group BY further on, as it is when BY groups come before it in a longer
pattern. A number more than PATTERN has groups is an octal character, kept
as it is, and so is what a set ([...]) or \\Q...\\E holds."
  (if (zerop by)
      pattern
      (let ((groups (tuatara.syntax:pattern-group-count pattern)) (n (length pattern)) (i 0))
        (with-output-to-string (out)
          (loop while (< i n)
                do (let ((c (char pattern i)))
                     (cond ((char= c #\[)
                            (let ((end (tuatara.syntax:bracket-end pattern i)))
                              (write-string pattern out :start i :end end)
                              (setf i end)))
                           ((and (char= c #\\) (< (1+ i) n) (char= (char pattern (1+ i)) #\Q))
                            (let ((end (let ((at (search "\\E" pattern :start2 (+ i 2)))) (if at (+ at 2) n))))
                              (write-string pattern out :start i :end end)
                              (setf i end)))
                           ((and (char= c #\\) (< (1+ i) n) (digit-char-p (char pattern (1+ i)))
                                 (char/= (char pattern (1+ i)) #\0))
                            (let* ((end (or (position-if-not #'digit-char-p pattern :start (1+ i)) n))
                                   (number (parse-integer pattern :start (1+ i) :end end)))
                              (if (<= number groups)
                                  (format out "\\~d" (+ by number))
                                  (write-string pattern out :start i :end end))
                              (setf i end)))
                           ((and (char= c #\\) (< (1+ i) n))
                            (write-char c out) (write-char (char pattern (1+ i)) out) (incf i 2))
                           (t (write-char c out) (incf i)))))))))

(defun light-search-filters (list buffer)
  "Light in BUFFER the matches of the filters narrowing LIST, as the matches
of a search are lit after it ends (\\[toggle-search-highlight] takes them
away and back): all of them at once, each ignoring case as it does, its
groups its own (\\1 its first). Those that hide what they match, and those
that look at one column of a record or of a table, light nothing. An empty
\\[regex-search-forward] \\<search>\\[search-repeat-forward] then goes to them."
  (let ((filters (remove-if (lambda (filter) (or (search-filter-hide filter) (search-filter-test filter)
                                                    (consp (search-filter-where filter))))
                            (and list (search-filters list))))
        (groups 0))
    (when filters
      (let* ((string (format nil "~{~a~^|~}"
                             (mapcar (lambda (filter)
                                       (let ((pattern (search-filter-string filter)))
                                         (format nil "(?~:[-~;~]i:~a)" (search-filter-fold filter)
                                                 (if (search-filter-regex-p filter)
                                                     (prog1 (shift-group-references pattern groups)
                                                       (incf groups (tuatara.syntax:pattern-group-count pattern)))
                                                     (search-quote pattern t :parsed nil)))))
                                     filters)))
             (state (make-search-state 0 t t nil nil buffer)))
        (setf (search-state-string state) string
              *search-lit* (cons state buffer) *search-lit-shown* t
              *last-search-string* string)))))

(defun install-search-filters ()
  "Bind the keys of the search filters: \\[search-filter-regex],
\\[list-search-filters] and \\[remove-search-filters] in every buffer, and
those of the list of filters; in a file whose lines they hide, say in the
mode line what they leave, keep the lines hidden from every edit, and work
them out again after a change that reached into those lines, or once the
file is read again."
  (pushnew 'filters-mode-line *mode-line-functions*)
  (pushnew 'refilter-touched *tick-functions*)
  (pushnew 'refilter-reloaded *after-reload-functions*)
  (pushnew 'guard-hidden-lines tuatara.buffer:*before-edit-functions*)
  (define-keys (mode-keymap 'search-filters-mode) '(("d" remove-search-filter) ("D" remove-search-filters)))
  (commands-for-mode 'search-filters-mode "the list of search filters (\\[list-search-filters])"
                     'remove-search-filter))
