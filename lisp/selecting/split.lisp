;;;; lisp/selecting/split.lisp --- Split: every match of a regexp in the selection held, and acted on at once.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A split holds places, stretches of one buffer: the matches of f, or the
;;;; cursors t makes (cursors.lisp). What is typed at the place point is in is
;;;; written at every other place after each command, one change for undo, and a
;;;; deletion just past the place's edge is made at every other place's edge,
;;;; at a match only where the very text deleted is there (else the split ends).
;;;; The buffer is not saved while a split is written at, and a split typed at
;;;; ends before the buffer is read again from its file.
;;;; Over a large stretch the search runs in the background, cut into pieces
;;;; searched on all but two cores at once, and the split stands when it ends;
;;;; no more places are held than *SPLIT-LIMIT*, and the search stops as soon
;;;; as it finds more.

(in-package #:tuatara.defaults)

;;; Places

(defstruct (place (:constructor %make-place (start end)) (:copier nil))
  "A stretch of text a split holds, between the anchors START (which stays
before text put in at it) and END (which goes after it). In a split of
cursors a place is where its cursor is and what the cursor selects: AT-END
puts the cursor at the end rather than the start, ANCHORED says the cursor
selects from the other end, LINE that it selects its whole line, OFFSET how
far into that line it stands, and MEMORY holds what the selecting keys
left there to grow on the next press."
  start end (at-end nil) (anchored nil) (line nil) (offset 0) (memory nil))

(setf (documentation 'place-p 'function) "True when OBJECT is a PLACE of a split."
      (documentation 'place-start 'function) "The anchor where PLACE begins."
      (documentation 'place-end 'function) "The anchor where PLACE ends."
      (documentation 'place-at-end 'function) "True when PLACE's cursor is at its end rather than its start."
      (documentation 'place-anchored 'function) "True when PLACE's cursor selects from the place's other end."
      (documentation 'place-line 'function) "True when PLACE's cursor selects its whole line."
      (documentation 'place-offset 'function) "How far into its line PLACE's cursor stands, when it selects the line."
      (documentation 'place-memory 'function) "What the selecting keys left at PLACE's cursor, to grow on the next press.")

(defun make-place (buffer start end)
  "A place of BUFFER from START to END."
  (let ((version (tuatara.buffer:buffer-version buffer)))
    (%make-place (tuatara.text:make-anchor version start :bias :before)
                 (tuatara.text:make-anchor version end :bias :after))))

(defun place-bounds (buffer place)
  "Two values: where PLACE of BUFFER starts and ends now."
  (let ((version (tuatara.buffer:buffer-version buffer)))
    (values (tuatara.text:anchor-position (place-start place) version)
            (tuatara.text:anchor-position (place-end place) version))))

(defun move-place (buffer place start end)
  "Make PLACE of BUFFER run from START to END."
  (let ((version (tuatara.buffer:buffer-version buffer)))
    (tuatara.text:anchor-move (place-start place) version start)
    (tuatara.text:anchor-move (place-end place) version end)
    place))

(defun place-text (buffer place)
  "The text PLACE of BUFFER holds."
  (multiple-value-bind (start end) (place-bounds buffer place)
    (tuatara.buffer:buffer-string buffer :start start :end end)))

;;; The split

(defstruct (split (:constructor make-split (buffer)) (:copier nil))
  "The split in force: its BUFFER and PLACES (a vector, in buffer order); the
PRIMARY place, point's, whose TEXT (as last seen) the others follow; the PHASE,
:SELECT while a verb is awaited or :EDIT while typing at the places; what
the verb DID, for the report; the SCOPE searched, named for the reminder;
AGREE when every place held the same text; TICK, the buffer's content number
when it was made; SINCE, the undo stack as the typing began; BOUNDS, the
primary place's start and end as the last change left them; EDGES, the
deletions a command made just past the primary place, and STRAYED when it
changed text away from the places. For a split of cursors, CURSORS is true,
COLUMN is where new cursors go, ORDER lists the cursors added after the first,
newest first, LAST is the command the cursors last ran, RUNNING is true in a
mark run, whose steps are HISTORY and REDO, and REPEAT is what
\\<*cursor-keymap*>\\[cursors-repeat] runs again. VIRTUAL, for a
split past its limit, holds what it needs to write at the places it does not
hold, those out of sight (a VIRTUAL). A split holds no match that a fold hides
any of; HIDDEN counts those it left alone, for what it says."
  buffer (places #()) primary (text "") (phase :select) did (scope "this line") agree tick since
  bounds edges strayed cursors (column 0) order last running history redo repeat virtual (hidden 0))

(setf (documentation 'split-p 'function) "True when OBJECT is a SPLIT."
      (documentation 'split-buffer 'function) "The buffer SPLIT holds places in."
      (documentation 'split-places 'function) "SPLIT's places, a vector in buffer order."
      (documentation 'split-primary 'function) "The place of SPLIT point is in, whose text the others follow."
      (documentation 'split-text 'function) "What SPLIT's primary place held when last looked at."
      (documentation 'split-phase 'function) "SPLIT's phase: :SELECT while a verb is awaited, :EDIT while typing."
      (documentation 'split-did 'function) "What SPLIT's verb did, for the report as it ends:
:BEFORE, :AFTER, :LINE-STARTS, :LINE-ENDS, :OPENED-BELOW, :OPENED-ABOVE (typed
there), :CHANGED, :DELETED, :WRAPPED, :UNWRAPPED, :PASTED, :EDITED, or NIL for
nothing."
      (documentation 'split-scope 'function) "What SPLIT searched, named for the reminder."
      (documentation 'split-agree 'function) "True when every place of SPLIT held the same text."
      (documentation 'split-tick 'function) "The content number of SPLIT's buffer as the split was made."
      (documentation 'split-since 'function) "The undo stack of SPLIT's buffer as its typing began."
      (documentation 'split-bounds 'function) "The primary place's start and end, as the last change left them."
      (documentation 'split-edges 'function) "Deletions just past the primary place's edges, newest first, as (SIDE . TEXT)."
      (documentation 'split-strayed 'function) "True when a command changed text away from SPLIT's places."
      (documentation 'split-cursors 'function) "True when SPLIT is a split of cursors."
      (documentation 'split-column 'function) "The column on the screen new cursors of SPLIT go to."
      (documentation 'split-order 'function) "The cursors added to SPLIT after the first, newest first."
      (documentation 'split-last 'function) "The command SPLIT's cursors last ran together."
      (documentation 'split-running 'function) "True while SPLIT's cursors are in a mark run."
      (documentation 'split-history 'function)
      "The steps of the cursors' mark run, newest first, for
\\<*mark-run-keymap*>\\[mark-run-step-back]."
      (documentation 'split-redo 'function)
      "The steps \\<*mark-run-keymap*>\\[mark-run-step-back] took
back in the cursors' mark run, for \\[mark-run-step-forward]."
      (documentation 'split-repeat 'function)
      "What \\<*cursor-keymap*>\\[cursors-repeat] runs again at
SPLIT's cursors."
      (documentation 'split-virtual 'function)
      "For SPLIT past its limit, what it needs to write at the places it does not hold (a VIRTUAL), or NIL."
      (documentation 'split-hidden 'function)
      "How many matches SPLIT left alone because a fold hides them.")

(defstruct (block-scope (:constructor make-block-scope (top high left right wide crlf tab-width)) (:copier nil))
  "A rectangle's rows as a split searches them, read as they come rather than
gathered first: the lines starting from TOP below HIGH, between the columns
LEFT and RIGHT on the screen (a tab TAB-WIDTH columns at most), or with WIDE
their whole lines; CRLF when lines end with a carriage return and a newline."
  (top 0) (high 0) (left 0) (right 0) wide crlf (tab-width 8))

(defstruct (virtual (:constructor make-virtual) (:copier nil))
  "What a split past its limit needs to write at the places it does not hold,
those out of sight, when the work at those it holds is done. KIND :MATCHES:
the matches of REGEXP (found by MATCHER) in the scope made of SCOPE at the
anchors ANCHORS (SCOPE-AT). KIND :ROWS: the lines from the one starting at the
anchor TOP to the one starting at the anchor BOTTOM, a cursor on each at
COLUMN (a number, or :END for the line's end). The place of the verb under way
on each row runs from column LEFT to column RIGHT, or with AT-END is the line's
end, a row short of LEFT padded out to it when PAD; the columns count
characters, or with TAB-WIDTH they are on the screen, a tab that many columns
at most, as a rectangle's are. MODE is what the verb did
to the places first: :START or :END, narrowed to that edge, or :CLEAR, emptied.
COUNT is how many places there are, at least unless EXACT; LIMIT the most the
split holds, and SETTING the setting it comes from, for what is said. STARTED
is true once a job was started for it. PASSED lists the matches come into sight
while typing that were let go, the text beside them not what was deleted beside
the others, or emptying them taking part of the text a fold hides, so that
they are not held again. SEEN is what the places in sight were last looked
for in: the text's version, the places held, the columns and mode, and the
stretches in sight (VIRTUAL-FILL)."
  kind regexp matcher scope anchors top bottom (column 0) (left 0) (right 0) at-end pad
  (mode :start) (count 0) exact (limit 0) setting started (passed '()) tab-width (seen nil))

(setf (documentation 'virtual-p 'function) "True when OBJECT is a VIRTUAL.")

(defstruct (split-diff (:constructor %make-split-diff) (:copier nil))
  "What turns a place holding one text into one holding another: whether the
text CHANGED; the deletions just past its edges (EDGES true), BEFORE and AFTER
it, and where their line breaks fall; the new text WHOLE and its MIDDLE, what
differs, as octets; the octets the two share at the start and end
(PREFIX-OCTETS, SUFFIX-OCTETS), and the old text's (OLD-OCTETS)."
  changed edges (before "") (after "") before-breaks after-breaks whole middle
  (prefix-octets 0) (suffix-octets 0) (old-octets 0))

(defvar *split* nil
  "The split in force, or NIL.")

(defvar *split-keeping* nil
  "True while the split takes its own keys down on purpose, so that it stays.")

(defvar *split-copying* nil
  "True while the split writes its places itself, which are not its reader's changes.")

(defstruct (split-typing (:constructor make-split-typing (original)) (:copier nil))
  "The typing under way at the places of a split, its matches or its cursors:
the primary place's ORIGINAL text as it began, the deletions just past its edges since
then (EDGES, oldest first, each (SIDE . TEXT)), and the places written as it
goes (LIVE, a set); the others are written once it ends."
  (original "") (edges '()) (live (make-hash-table :test 'eq)))

(defvar *split-typing* nil
  "The typing at the split in force, or NIL: while it lasts, each key is written
only at the places in sight, the others when it ends, so that a key costs the
same at a hundred places as at a million.")

(defvar *split-sight-octets* 65536
  "How far before and after what a window shows, and around point, the places
written as each key is typed reach at least, so that those the typing itself
brings into sight are written by then.")

(defun split-here-p (&optional (buffer (current-buffer)))
  "True when a split is in force in BUFFER."
  (and *split* (eq (split-buffer *split*) buffer) (plusp (length (split-places *split*)))))

(defun places-phrase (split n)
  "N written as a count of the places of SPLIT, or of its cursors."
  (format nil "~:d ~a~:[s~;~]" n (if (split-cursors split) "cursor" "place") (= n 1)))

(defun split-places-phrase (split)
  "The places or cursors of SPLIT counted, for what the echo area says: past its
limit, those held and how many there are in all."
  (let ((n (length (split-places split))) (virtual (split-virtual split)))
    (if virtual
        (format nil "~a of ~:[more than ~;~]~:d" (places-phrase split n) (virtual-exact virtual) (virtual-count virtual))
        (places-phrase split n))))

(defun split-hint (split)
  "What the echo area says while SPLIT lasts."
  (let ((n (length (split-places split))))
    (cond ((and (split-cursors split) (eq (split-phase split) :edit))
           (format nil (substitute-command-keys
                        "Split: writing at ~a~:[~;, those out of sight as you finish~]; \\<insert>\\[tuatara-normal] back to the cursors"
                        :marks t :for-format t)
                   (split-places-phrase split)
                   (or (split-virtual split)
                       (and *split-typing* (< (hash-table-count (split-typing-live *split-typing*)) n)))))
          ((eq (split-phase split) :edit)
           (format nil (substitute-command-keys
                        "Split: writing at ~a~:[~;, those out of sight as you finish~]; \\<insert>\\[tuatara-normal] to finish"
                        :marks t :for-format t)
                   (split-places-phrase split)
                   (or (split-virtual split)
                       (and *split-typing* (< (hash-table-count (split-typing-live *split-typing*)) n)))))
          ((and (split-cursors split) (split-virtual split))
           (format nil (substitute-command-keys
                        "Split: ~a, those out of sight written in the background; ~
\\<*tuatara-normal-keymap*>\\[backward-char] \\[forward-char] \\[beginning-of-line] \\[end-of-line] move all; ~
\\<*virtual-cursor-keymap*>\\[cursors-insert] \\[cursors-append] ~
\\[cursors-insert-line] \\[cursors-append-line] ~
\\[cursors-change] \\[cursors-delete], \\[split-quit]"
                        :marks t :for-format t)
                   (split-places-phrase split)))
          ((and (split-cursors split) (split-running split))
           (format nil (substitute-command-keys
                        "Split: ~a, mark run; ~
\\<*mark-run-keymap*>\\[select-word] \\[select-symbol] ~
\\[select-word-backward] \\[select-symbol-backward] ~
\\[select-sentence] \\[select-sentence-backward] grow, ~
\\[mark-run-left] \\[mark-run-right] ~
\\[mark-run-line-start] \\[mark-run-line-end] ~
\\[mark-run-exchange] adjust, \\[repeat] again, ~
\\[mark-run-step-back] \\[mark-run-step-forward] step, ~
\\[mark-run-cancel] or \\<*cursor-keymap*>\\[split-quit] ends"
                        :marks t :for-format t)
                   (places-phrase split n)))
          ((split-cursors split)
           (format nil (substitute-command-keys
                        "Split: ~a~:[~; selecting~]; ~
\\<*cursor-keymap*>\\[add-cursor-below] ~
\\[add-cursor-above] add, ~
\\[drop-last-cursor] drop, \\[align-cursors] line up, ~
\\[cursors-select] \\[cursors-select-lines] \\`m' ~
\\[cursors-run-toggle] select, ~
\\[cursors-insert] \\[cursors-append] ~
\\[cursors-insert-line] \\[cursors-append-line] ~
\\[cursors-open-below] \\[cursors-open-above] ~
\\[cursors-change] \\[cursors-delete] ~
\\[cursors-copy] \\[cursors-yank] ~
\\[cursors-kill-line], \\[split-selection] find, \\[split-quit]"
                        :marks t :for-format t)
                   (places-phrase split n) (some #'cursor-selecting-p (split-places split))))
          (t (format nil (substitute-command-keys
                          "Split: ~a in ~a~:[~;, those out of sight written in the background~]~a; ~
\\<*split-chooser-keymap*>\\[split-insert] before, ~
\\[split-append] after, \\[split-change] change, ~
\\[split-delete] delete, \\[split-wrap] wrap, \\[split-quit]"
                          :marks t :for-format t)
                     (split-places-phrase split) (split-scope split) (split-virtual split)
                     (hidden-left-words (split-hidden split)))))))

(defun split-report (split)
  "What to say as SPLIT ends: what its verb did, and to how many places, and
how many matches it left alone because a fold hides them."
  (let ((places (if (split-virtual split)
                    (format nil "~a in sight~:[~;, ~a in the background (~a stops it)~]"
                            (places-phrase split (length (split-places split)))
                            (virtual-started (split-virtual split)) (virtual-words split)
                            (substitute-command-keys "\\[stop-job]"))
                    (places-phrase split (length (split-places split))))))
    (format nil "~a~a"
            (if (eql (split-tick split) (tuatara.buffer:buffer-content-number (split-buffer split)))
                (format nil "Split ended: ~a left alone" places)
                (case (split-did split)
                  (:before (format nil "Split: typed before ~a" places))
                  (:after (format nil "Split: typed after ~a" places))
                  (:line-starts (format nil "Split: typed at the line starts of ~a" places))
                  (:line-ends (format nil "Split: typed at the line ends of ~a" places))
                  (:opened-below (format nil "Split: typed on new lines below ~a" places))
                  (:opened-above (format nil "Split: typed on new lines above ~a" places))
                  (:changed (format nil "Split: changed ~a" places))
                  (:deleted (format nil "Split: deleted ~a" places))
                  (:wrapped (format nil "Split: wrapped ~a" places))
                  (:unwrapped (format nil "Split: unwrapped ~a" places))
                  (:pasted (format nil "Split: pasted at ~a" places))
                  (:edited (format nil "Split: edited at ~a" places))
                  (t (format nil "Split ended: ~a left alone" places))))
            (hidden-left-words (split-hidden split)))))

(defun close-split-edit (split &key (background t))
  "End the typing at SPLIT's places: the places it has not written yet are
written, and all it wrote becomes one change for undo. Past the limit, the
places not held are then written in the background, unless BACKGROUND is NIL."
  (when (eq (split-phase split) :edit)
    (let ((typing *split-typing*))
      (unless (write-pending-places split)
        (message "The places out of sight could not be written as the one typed at: an edit beside them differs"))
      (tuatara.buffer:merge-undo-groups (split-buffer split) (split-since split))
      (setf (tuatara.buffer:buffer-change-functions (split-buffer split))
            (remove 'split-note-change (tuatara.buffer:buffer-change-functions (split-buffer split))))
      (setf (split-phase split) :select)
      ;; past the limit, the places not held are written as those held were
      (when (and typing background (split-virtual split))
        (start-virtual-job split :typing typing)))))

(defun dissolve-split (&key quiet (background t))
  "Take the split in force down, saying what it did unless QUIET; BACKGROUND
NIL leaves the places past its limit that it does not hold as they are."
  (let ((split *split*))
    (when split
      (setf *split* nil)
      (close-split-edit split :background background)
      (unless quiet (message "~a" (split-report split)))
      (when (member (editor-transient-map *editor*) (list *split-chooser-keymap* *cursor-keymap* *cursor-run-keymap*))
        (let ((*split-keeping* t)) (clear-transient-map *editor*))))))

(defun split-exit ()
  "When the split's keys are taken down by a key they do not have: end it."
  (unless *split-keeping* (dissolve-split)))

(defun split-before-reload (buffer)
  "Before BUFFER is read again from its file while typing at the places of a
split there: end the split first, the places it holds written and what was
typed one change for undo, the reading again a step of its own; the places past
its limit that it does not hold are left as they are. Returns what to say of
it, or NIL."
  (let ((split *split*))
    (when (and split (eq (split-buffer split) buffer) (eq (split-phase split) :edit))
      (dissolve-split :quiet t :background nil)
      "the split typed at ended first")))

(defvar *split-writers* '()
  "The jobs writing in the background at the places of splits they did not
hold, each (BUFFER JOB WORDS): JOB a REWRITE, or a QUEUED-JOB waiting its
turn, and WORDS what the places are called.")

(defun note-split-writer (buffer job words)
  "Note JOB, writing at the places of a split in BUFFER called WORDS (a
REWRITE, or :QUEUED when it waits its turn there), so that the buffer is not
saved before it is done."
  (let ((entry (list buffer (if (eq job :queued) (car (last (queued-in buffer))) job) words)))
    (when (queued-job-p (second entry))
      ;; the job its turn starts takes the waiting one's place
      (let* ((queued (second entry)) (starter (queued-job-starter queued)))
        (setf (queued-job-starter queued)
              (lambda (&rest positions)
                (let ((started (apply starter positions)))
                  (when (rewrite-p started) (setf (second entry) started))
                  started)))))
    (when (second entry) (push entry *split-writers*))))

(defun split-writer (buffer)
  "The entry of *SPLIT-WRITERS* for a job writing at a split's places in
BUFFER that is not done yet: running, waiting its turn, or ended with its
result not yet taken in; NIL when none."
  (setf *split-writers*
        (remove-if-not (lambda (entry)
                         (let ((job (second entry)))
                           (if (queued-job-p job) (member job *job-queue*) (member job tuatara.runtime:*jobs*))))
                       *split-writers*))
  (find buffer *split-writers* :key #'first))

(defun split-save-refusal (buffer)
  "Why BUFFER may not be saved yet, for *SAVE-REFUSALS*: typing at a split's
places there, or a split's writing at the places out of sight still under way;
NIL when it may."
  (let ((split *split*) (writer (split-writer buffer)))
    (cond ((and split (eq (split-buffer split) buffer) (eq (split-phase split) :edit))
           (substitute-command-keys
            "Not saved: the split is still being typed at; \\<insert>\\[tuatara-normal] ends it, then save again"))
          ((and writer (rewrite-p (second writer)) (eq (rewrite-status (second writer)) :running))
           (format nil "Not saved: the split is still writing at ~a (~d%); save again when it is done"
                   (third writer) (rewrite-progress (second writer))))
          (writer
           (format nil "Not saved: the split's writing at ~a is not done yet; save again when it is" (third writer))))))

;;; Finding the places

(defun split-ranges (whole wide)
  "The stretches of the current buffer a split searches, (START . END) in
order, and what they are called: the whole buffer with WHOLE (the part shown
of a narrowed one); else what is selected (SELECTED-RANGES, WIDE as it takes
it), or the line at point."
  (let ((b (current-buffer)))
    (if whole
        (values (list (multiple-value-call #'cons (tuatara.buffer:buffer-bounds b)))
                (if (tuatara.buffer:buffer-narrowed-p b) "the part shown" "the buffer"))
        (multiple-value-bind (ranges words) (selected-ranges wide)
          (if ranges
              (values ranges words)
              (values (list (cons (tuatara.buffer:buffer-line-start b (point))
                                  (tuatara.buffer:buffer-line-end b (point))))
                      "this line"))))))

(defun selected-ranges (wide)
  "What is selected in the current buffer, as the stretches a split or a
replacement acts on, (START . END) in order, and what they are called: the
cursors' selections, or their lines; a rectangle's block, or with WIDE its
rows' whole lines (for a rectangle over more than *REWRITE-THRESHOLD*, a
BLOCK-SCOPE in place of the list, its rows read as the search comes to them);
banked lines with a selection made at the time; the selection. NIL when
nothing is."
  (let ((b (current-buffer)))
    (cond ((split-here-p b)
           (let* ((places (coerce (split-places *split*) 'list))
                  (selected (some #'cursor-selecting-p places)))
             (values (mapcar (lambda (place)
                               (if selected
                                   (multiple-value-bind (s e) (place-bounds b place) (cons s e))
                                   (multiple-value-bind (s e) (cursor-line-bounds b place) (cons s e))))
                             places)
                     (if selected "the cursors' selections" "the cursors' lines"))))
          ((and (rectangle-live-p b) (large-rectangle-p b))
           (let ((top (tuatara.buffer:buffer-line-start b (min (point) (tuatara.buffer:buffer-mark b))))
                 (bottom (tuatara.buffer:buffer-line-start b (max (point) (tuatara.buffer:buffer-mark b)))))
             (multiple-value-bind (left right) (rectangle-columns b)
               (values (make-block-scope top (min (tuatara.buffer:buffer-size b) (1+ (tuatara.buffer:buffer-line-end b bottom)))
                                         left right wide (buffer-crlf-p b) (tuatara.display:tab-width-of b))
                       (if wide "the block's rows" "the block")))))
          ((rectangle-live-p b)
           (values (mapcar (lambda (row)
                             (if wide
                                 (cons (tuatara.buffer:buffer-line-start b (first row))
                                       (tuatara.buffer:buffer-line-end b (first row)))
                                 (cons (first row) (second row))))
                           (region-rectangle-rows b))
                   (if wide "the block's rows" "the block")))
          ((banked-p b)
           (let* ((banked (banked-spans b))
                  (lines (span-line-count b banked)))
             (multiple-value-bind (start end) (selection-range)
               (values (tuatara.text:merge-ranges
                        (sort (append (mapcar (lambda (span)
                                                (cons (car span)
                                                      (if (eql (char-before-position b (cdr span)) #\Newline)
                                                          (tuatara.buffer:buffer-forward-char b (cdr span) -1)
                                                          (cdr span))))
                                              banked)
                                      (and start (< start end) (list (cons start end))))
                              #'< :key #'car))
                       (format nil "~d banked line~:p~:[~; and the selection~]" lines (and start (< start end)))))))
          (t (multiple-value-bind (start end) (selection-range)
               (and start (< start end)
                    (values (list (cons start end)) "the selection")))))))

(defvar *split-limit* 100000
  "The most places \\<normal>\\[split-selection] holds, or NIL for as many as the
memory has room for. What is typed is written as it is typed only at the
places in sight, so a key costs the same however many there are; holding the
places, and writing the others when the typing ends, take about a second a
million. Past it, the split holds the places in sight, and the others are
written in the background once the verb is done at those.")

(defvar *split-budget* nil
  "Octets of heap a split may plan to use, or NIL for a quarter of what is free
when it is made. Its places are counted against it as they are found, so that a
split too large for the memory is refused before the heap runs out.")

(defconstant +split-key-octets+ 256
  "The heap each key typed at a place of a split keeps, for undo, with room to spare.")

(defconstant +split-place-octets+ (* 3 +split-key-octets+)
  "The heap a place of a split is planned to take: itself, and what typing at
it keeps for undo, written once when the typing ends, with room to spare.")

(defun place-limit (&optional (setting *split-limit*))
  "Three values: the most places a split may hold now, matches or cursors, the
lower of SETTING (*SPLIT-LIMIT* for \\<normal>\\[split-selection],
*CURSORS-LIMIT* for \\[add-cursor-below], *RECTANGLE-CURSORS-LIMIT* for
\\[tuatara-change] on a rectangle; NIL for none but the memory's) and what the
memory allows (*SPLIT-BUDGET*); :MEMORY when it is the memory's, else
:SETTING; and the memory's alone."
  (let ((memory (floor (heap-budget *split-budget*) +split-place-octets+)))
    (if (and setting (<= setting memory))
        (values setting :setting memory)
        (values memory :memory memory))))

(defun place-count-cap (&optional (setting *split-limit*))
  "How many places to count, at most, before PAST-LIMIT-CHOICE: one more than
there may be (PLACE-LIMIT), or than the memory has room for when the question
may be asked (*SPLIT-LIMIT-ASK*)."
  (multiple-value-bind (limit why memory) (place-limit setting)
    (1+ (if (and (eq why :setting) *split-limit-ask*) memory limit))))

(defun past-limit-choice (n exact noun &optional (setting *split-limit*))
  "Whether to hold every one of N places, matches or cursors (NOUN, as
\"cursors\"), or only those in sight, the others written in the background:
:ALL when they are no more than there may be (PLACE-LIMIT); past SETTING,
with *SPLIT-LIMIT-ASK* true, N counted EXACT and the memory having room, the
question asked, the same for \\<normal>\\[split-selection],
\\[add-cursor-below] and \\[tuatara-change] on a rectangle, and :ALL when
it is answered yes; else :IN-SIGHT."
  (multiple-value-bind (limit why memory) (place-limit setting)
    (if (or (<= n limit)
            (and (eq why :setting) *split-limit-ask* exact (<= n memory)
                 (yes-or-no-p* (format nil "Hold all ~:d ~a, with every key slow at that many (else those in sight)? " n noun))))
        :all
        :in-sight)))

(defun split-room-to-write-p (split)
  "True when the heap has room, twice over, for one more key typed at the
places of SPLIT it is written at: those written so far and in sight while the
typing writes as it goes, else every place."
  (heap-room-p (* 2 +split-key-octets+
                  (if *split-typing*
                      (+ (hash-table-count (split-typing-live *split-typing*)) 4096)
                      (length (split-places split))))))

(defun split-search (matcher ranges &key limit)
  "Every match of MATCHER (SPLIT-MATCHER) in RANGES of the current buffer,
(START . END) in order, the empty match at a range's end too (STRETCH-MATCHES).
The empty line after the last newline holds no match, and neither does text a
fold hides: a match it hides any of is left out (TUATARA.DISPLAY:FOLDS-HIDING,
nothing looked at when the buffer has no folds). Three values: the matches,
NIL, and how many a fold hides; with LIMIT, the search stops once it finds
more than that many, returning NIL and true."
  (let* ((b (current-buffer)) (rope (tuatara.buffer:buffer-text b)) (folds (tuatara.display:folds-now b))
         (spans '()) (count 0) (hidden '()))
    (dolist (range ranges)
      (when (stretch-matches (lambda (s e m g)
                               (declare (ignore m g))
                               (if (and folds (tuatara.display:folds-hiding folds s e))
                                   (progn (push (cons s e) hidden) nil)
                                   (progn (push (cons s e) spans)
                                          (and limit (> (incf count) limit)))))
                             rope matcher (car range) (cdr range) t nil nil)
        (return-from split-search (values nil t 0))))
    (values (sort (remove-duplicates spans :test #'equal) #'< :key #'car) nil
            (length (remove-duplicates hidden :test #'equal)))))

(defun split-where (buffer position)
  "Where POSITION of BUFFER is, in words: on which line, or at which octet while
the lines of a large text are still being counted."
  (let ((rope (tuatara.buffer:buffer-text buffer)))
    (if (tuatara.text:rope-indexed-p rope)
        (format nil "on line ~:d" (1+ (tuatara.text:byte->line rope position)))
        (format nil "at octet ~:d" position))))

(defun places-agree-p (buffer places)
  "True when every one of PLACES of BUFFER holds the text the first holds."
  (or (<= (length places) 1)
      (multiple-value-bind (s e) (place-bounds buffer (aref places 0))
        (let ((size (- e s)))
          ;; the lengths first, which cost no text
          (and (loop for i from 1 below (length places)
                     always (multiple-value-bind (s2 e2) (place-bounds buffer (aref places i)) (= (- e2 s2) size)))
               (or (zerop size)
                   (let ((first (tuatara.buffer:buffer-string buffer :start s :end e)))
                     (loop for i from 1 below (length places)
                           always (string= first (place-text buffer (aref places i)))))))))))

(defparameter *split-chooser-bindings*
  '(("i" split-insert) ("a" split-append) ("c" split-change)
    ("d" split-delete) ("w" split-wrap) ("C-g" split-quit))
  "The keys of a split while it waits for a verb, besides each delimiter's.")

(defvar *split-chooser-keymap* (make-keymap :name "split")
  "The keys a split takes while it waits for a verb:
\\<*split-chooser-keymap*>\\[split-insert] \\[split-append]
\\[split-change] \\[split-delete] \\[split-wrap], each
delimiter, \\[split-quit].")

(defvar *split-commands*
  '(split-insert split-append split-change split-delete
    split-wrap split-wrap-key split-quit)
  "The commands after which a split stays; the cursors' add to them.")

(defvar *split-inert-commands* '(tuatara-normal-inert universal-argument recenter scroll-by-wheel)
  "Commands that change nothing a split holds, which keep it:
\\<normal>\\[tuatara-normal-inert], a count, \\[recenter], the mouse wheel.")

(defun split-looked-p ()
  "True when the command just run only looked about while the split in force
waits for a verb: its buffer still the one here, its text as it was, nothing
selected, in NORMAL state; point and the view may have moved. Not for a split
of cursors, whose keys move them all."
  (let ((split *split*))
    (and split (not (split-cursors split)) (eq (split-phase split) :select)
         (split-here-p)
         (let ((b (split-buffer split)))
           (and (eql (split-tick split) (tuatara.buffer:buffer-content-number b))
                (not (tuatara.buffer:buffer-mark-active-p b))
                (eq (modal-state b) :normal))))))

(defun split-keep-p (command)
  "True while the split stays after COMMAND: one of its own, one that changes
nothing, or one that only moved about while it waits for a verb
(SPLIT-LOOKED-P); for the cursors, \\<normal>\\[split-selection] too, which leaves
them where nothing matches."
  (or (member command *split-commands*) (member command *split-inert-commands*)
      (and (eq command 'split-selection) *split* (split-cursors *split*))
      (split-looked-p)))

(defun arm-split (split map)
  "Put MAP before every other keymap for SPLIT, the split ending when a key it
does not have is pressed."
  (setf (split-phase split) :select)
  (let ((*split-keeping* t))
    (set-transient-map *editor* map #'split-keep-p #'split-exit)))

(defun spans-touch (buffer spans)
  "Why SPANS, matches in BUFFER in order, cannot be a split's places: where two
touch, as what to say; NIL when none do."
  (let ((touching (loop for (a b) on spans when (and b (<= (car b) (cdr a))) return (list a b))))
    (when touching
      (destructuring-bind (a b) touching
        (format nil "Matches touch ~a (~a): a split needs a character between places"
                (split-where buffer (car a))
                (tuatara.buffer:buffer-string buffer :start (car a) :end (min (cdr b) (+ (car a) 60))))))))

(defun hold-virtual-matches (buffer regexp scope words whole first &key (move t) count)
  "Hold the matches of REGEXP in SCOPE of BUFFER, named WORDS (the whole buffer
with WHOLE), more than a split holds whole (PLACE-LIMIT), as a split of those
in sight, as many as it may hold, and wait for a verb: the others are written
in the background once the verb is done at those held. Point moves to FIRST,
the first match's (START . END), unless MOVE is false; a match that a fold
hides any of is not held, and the writing in the background leaves it alone
and counts it. COUNT, when known, is how many there are in all. Returns what
to say."
  (multiple-value-bind (limit why) (place-limit)
    (let ((banked (and (not whole) (not (split-here-p buffer)) (not (rectangle-live-p buffer)) (banked-spans buffer)))
          (version (tuatara.buffer:buffer-version buffer))
          (split (make-split buffer)))
      (when *split* (dissolve-split :quiet t))
      (setf (tuatara.buffer:buffer-mark-active-p buffer) nil)
      (fresh-selection buffer)
      (when (and move first) (goto (car first)))
      (setf (split-virtual split)
            (make-virtual :kind :matches :regexp regexp :matcher (split-matcher regexp (block-scope-p scope) buffer)
                          :scope scope
                          :anchors (mapcar (lambda (position) (tuatara.text:make-anchor version (car position) :bias (cdr position)))
                                           (scope-positions scope))
                          :count (or count limit) :exact (and count t) :limit limit :setting why)
            (split-scope split) words
            (split-tick split) (tuatara.buffer:buffer-content-number buffer))
      (virtual-fill split)
      (when (and (zerop (length (split-places split))) first)
        (setf (split-places split) (vector (make-place buffer (car first) (cdr first)))))
      (if (zerop (length (split-places split)))
          (format nil "Nothing matched ~a" regexp)
          (progn
            (setf (split-primary split) (aref (split-places split) 0)
                  (split-text split) (place-text buffer (split-primary split))
                  *split* split)
            (build-split-chooser-keymap)
            (arm-split split *split-chooser-keymap*)
            (when banked (spend-banks buffer banked))
            (split-hint split))))))

(defun hold-split (buffer regexp spans scope whole &key (move t) (hidden 0))
  "Hold SPANS of BUFFER, the matches of REGEXP found in SCOPE (the whole buffer
with WHOLE), as the places of a new split and wait for a verb, point moved to
the first unless MOVE is false; the lines banked for it are spent. HIDDEN is
how many matches were left alone because a fold hides them, for what the split
says (SPLIT-HIDDEN). Returns what to say: the split's reminder, or why it is
not held."
  (let ((touching (spans-touch buffer spans)))
    (cond ((null spans) (format nil "Nothing matched ~a~a" regexp (hidden-left-words hidden)))
          (touching touching)
          (t
           (let ((banked (and (not whole) (not (split-here-p buffer)) (not (rectangle-live-p buffer)) (banked-spans buffer))))
             (when *split* (dissolve-split :quiet t))
             (setf (tuatara.buffer:buffer-mark-active-p buffer) nil)
             (fresh-selection buffer)
             (let ((split (make-split buffer)))
               (setf (split-places split) (map 'simple-vector (lambda (span) (make-place buffer (car span) (cdr span))) spans)
                     (split-primary split) (aref (split-places split) 0)
                     (split-scope split) scope
                     (split-hidden split) hidden
                     (split-tick split) (tuatara.buffer:buffer-content-number buffer))
               (setf (split-agree split) (places-agree-p buffer (split-places split))
                     (split-text split) (place-text buffer (split-primary split))
                     *split* split)
               (when move (goto (car (first spans))))
               (build-split-chooser-keymap)
               (arm-split split *split-chooser-keymap*)
               ;; spent once the split stands, as y and d spend them
               (when banked (spend-banks buffer banked))
               (split-hint split)))))))

(defun split-in-background-p (buffer scope)
  "True when a split's search over SCOPE of BUFFER runs in the background: a
large rectangle's rows (a BLOCK-SCOPE), or stretches of *REWRITE-THRESHOLD*
octets or more in all, however many (banked lines), not the cursors'."
  (or (block-scope-p scope)
      (and (not (split-here-p buffer))
           (>= (scope-extent scope) *rewrite-threshold*))))

(defun split-input (text regex)
  "What TEXT typed at \\<normal>\\[split-selection]'s question asks for, as two
values: whether a leading % asks for the whole buffer, and the pattern after
it as a regular expression (REGEX tells whether it was typed as one)."
  (let ((whole (and (plusp (length text)) (char= (char text 0) #\%))))
    (values whole (pattern-regex (if whole (subseq text 1) text) regex))))

(define-command split-selection ()
  "Hold every match of a regexp in the selection, then wait for a verb:
\\<*split-chooser-keymap*>\\[split-insert] types before every match and
\\[split-append] after, \\[split-change] empties them first,
\\[split-delete] deletes them, \\[split-wrap] or any delimiter
wraps them (again, to take the pair off), and \\[split-quit] lets go.
What is typed at one match appears at all of them, and the writing is one
change for undo. The selection says what is searched: a rectangle's block
(with \\[universal-argument] each row's whole line), banked lines with any
selection made at the time, the selection, or else the line at point; the
regexp with % in front, %foo, searches the whole buffer whatever is selected.
As the regexp is typed, how many matches it has there shows before the prompt.
Only what is shown is acted on, whatever is searched: a match that a fold
hides any of is no place and is left alone, and the split says how many were
(a folded headline's line selected is the line shown, not what its fold hides).
Case is ignored when the regexp has
no capital letter. Matches that touch are refused. Past *SPLIT-LIMIT* the
matches in and near the window are held, more as you scroll to them, and what
is done at them is written at the others in the background once the verb is
done; with *SPLIT-LIMIT-ASK* true, you are asked first whether to hold them
all, as \\[add-cursor-below] and \\[tuatara-change] on a rectangle ask (not
for a search in the background). Moving about keeps the split
waiting for its verb; any other key ends it, doing its own job. A large text is
searched in the background, on all but two cores at once, the mode line saying
how far it has got (\\[stop-job] stops it); the split stands when the search
ends, point left where it is, unless the text has changed or another buffer is
shown by then. From the cursors of \\<normal>\\[add-cursor-below], each
cursor's selection or line is searched, and the matches take the cursors'
place. A match of more than 4 KB (4,096 characters of ASCII) that runs across
lines and needs text further on, as BEGIN[\\s\\S]*?END around a large block,
can be missed; \\[split-selection-thorough] finds every match of up to a
megabyte."
  (let* ((wide (prefix-argument))
         (b (current-buffer))
         (selected (split-ranges nil wide))
         (counter (make-pattern-count)))
    (multiple-value-bind (whole regexp)
        (multiple-value-call #'split-input
          (unwind-protect
               (read-pattern "split on"
                             :count (lambda (text regex)
                                      (multiple-value-bind (whole regexp) (split-input text regex)
                                        (let ((scope (if whole (list (multiple-value-call #'cons (tuatara.buffer:buffer-bounds b))) selected)))
                                          ;; a rectangle's block is searched row by row as the split comes to it
                                          (if (block-scope-p scope)
                                              (stretch-pattern-count counter b '() "" t)
                                              (stretch-pattern-count counter b scope regexp t :every t))))))
            (stop-pattern-count counter)))
      (if (zerop (length regexp))
          (message "Nothing to split on")
          (multiple-value-bind (scope words) (split-ranges whole wide)
            (if (split-in-background-p b scope)
                (start-split-search b regexp scope words whole)
                (multiple-value-bind (spans too-many hidden)
                    (split-search (split-matcher regexp nil b) scope :limit (place-count-cap))
                  (message "~a" (if (or too-many
                                        (and spans (not (spans-touch b spans))
                                             (eq (past-limit-choice (length spans) t "matches") :in-sight)))
                                    (hold-virtual-matches b regexp scope words whole (first spans)
                                                          :count (and (not too-many) (length spans)))
                                    (hold-split b regexp spans words whole :hidden hidden))))))))))

(define-command split-selection-thorough ()
  "Hold every match of a regexp in the selection, as \\<normal>\\[split-selection]
does, finding every match of up to a megabyte (about a million characters of
ASCII) whole however many lines it spans, where \\[split-selection] may miss one of
more than 4 KB that runs across lines and needs text further on; the writing
out of sight the same. A match of more than a megabyte is still cut short, or
missed when it needs text further on. As quick as \\[split-selection] for a
pattern kept within a line or no longer than 4 KB; about twice as slow for one
that may run on across lines without end. In its place, in the init file:
(bind-key \"f\" 'split-selection-thorough :map :normal)."
  (let ((tuatara.syntax:*regex-thorough* t)) (split-selection)))

;;; Searching a large text in the background

(defun scope-extent (scope)
  "The octets SCOPE, a list of (START . END) stretches or a BLOCK-SCOPE, spans."
  (if (block-scope-p scope)
      (- (block-scope-high scope) (block-scope-top scope))
      (loop for (start . end) in scope sum (- end start))))

(defun scope-positions (scope)
  "What SCOPE, a list of (START . END) stretches or a BLOCK-SCOPE, reads its
text by, as (OFFSET . BIAS) for anchors to follow while a job over it waits."
  (if (block-scope-p scope)
      (list (cons (block-scope-top scope) :before) (cons (block-scope-high scope) :after))
      (loop for (start . end) in scope collect (cons start :before) collect (cons end :after))))

(defun scope-at (scope positions)
  "SCOPE as its SCOPE-POSITIONS stand now, at POSITIONS."
  (if (block-scope-p scope)
      (make-block-scope (first positions) (second positions) (block-scope-left scope) (block-scope-right scope)
                        (block-scope-wide scope) (block-scope-crlf scope) (block-scope-tab-width scope))
      (loop for (start end) on positions by #'cddr collect (cons start end))))

(defun scope-shade (buffer scope)
  "The SHADE of a job over SCOPE of BUFFER waiting its turn: its stretches, or
a block's columns row by row."
  (if (block-scope-p scope)
      (let ((rows (block-shade buffer (block-scope-left scope) (block-scope-right scope)
                               :tab-width (block-scope-tab-width scope) :wide (block-scope-wide scope))))
        ;; a block reaches past its last row's end; its rows are the lines starting before that
        (lambda (positions from to)
          (funcall rows (list (first positions) (max (first positions) (1- (second positions)))) from to)))
      #'ranges-shade))

(defun scope-groups (rope scope)
  "SCOPE, a list of (START . END) stretches or a BLOCK-SCOPE, cut into pieces for
the workers, as a list of (STRETCH . PIECES) in order: a stretch's pieces end
at line starts (REWRITE-PIECES-FOR); between two stretches, a piece of the
text left alone, its stretch NIL, so that the pieces cover what the first
stretch starts to what the last ends."
  (if (block-scope-p scope)
      (let ((stretch (cons (block-scope-top scope) (block-scope-high scope))))
        (list (cons stretch (coerce (rewrite-pieces-for rope (car stretch) (cdr stretch)) 'list))))
      (let ((groups '()) (at nil))
        (dolist (range scope (nreverse groups))
          (when (and at (< at (car range)))
            (push (cons nil (list (make-rewrite-piece at (car range)))) groups))
          (push (cons range (coerce (rewrite-pieces-for rope (car range) (cdr range)) 'list)) groups)
          (setf at (cdr range))))))

(defun split-matcher (regexp block buffer)
  "How a split finds REGEXP in BUFFER, the MATCHER the search prompt's
replacements use too (MAKE-PATTERN-MATCHER): case ignored when it has no
capital letter, found as literal text when nothing in it is special, $ before
the carriage return where BUFFER's lines end with one and a newline; compiled
always for a block's rows (BLOCK), whose parts of lines are matched as text.
Signals why when it does not compile."
  (multiple-value-bind (matcher problem)
      (make-pattern-matcher regexp :regex t :compiled block :crlf (buffer-crlf-p buffer))
    (or matcher (error "Regexp ~a: ~a" regexp problem))))

(defun piece-matches (function rope matcher scope groups from to halt &key texts)
  "Call FUNCTION as STRETCH-MATCHES does for each match of MATCHER in the
piece of SCOPE from FROM below TO, in order: within the stretch of GROUPS it is
part of (none in the text between stretches), a match running on past TO to
the stretch's end, the empty match at the stretch's end included when the
piece ends it; or within each of a block's rows the piece holds, its
line before it the pattern's context (TUATARA.SYNTAX:MAP-SLICE-MATCHES), the
empty match at its end included, from the whole row FROM is in (a caller that
starts after a match passes over those before it). True when FUNCTION stopped
it."
  (if (block-scope-p scope)
      (let ((wide (block-scope-wide scope))
            (size (tuatara.text:rope-bytes rope))
            (rows 0))
        (declare (type fixnum rows))
        (block each-row
          (tuatara.syntax:with-decoder (decoder)
           (tuatara.text:with-octet-block (reused (* 1024 1024))
             (apply
              #'tuatara.text:map-line-slices
              (lambda (line s e start-column end-column line-end)
                (declare (ignore start-column end-column))
                (when (and halt (zerop (logand (incf rows) 1023)) (funcall halt)) (return-from each-row nil))
                (let ((s (if wide line s)) (e (if wide line-end e)))
                  ;; the empty line after the last newline holds no match
                  (unless (and (= s e size) (plusp size))
                    (when (tuatara.syntax:map-slice-matches function rope (matcher-scanner matcher) line s e :decoder decoder :texts texts)
                      (return-from each-row t)))))
              ;; from the start of the line FROM is in: the rows are read whole, and a
              ;; walk over the matches may begin just after one
              rope (tuatara.text:rope-line-start rope from) to
              (if wide 0 (block-scope-left scope)) (if wide 0 (block-scope-right scope))
              :block reused
              (tuatara.display:screen-slice-keys (block-scope-crlf scope) (block-scope-tab-width scope)))))
          nil))
      (let ((range (car (find-if (lambda (group)
                                   (let ((stretch (car group)))
                                     (and stretch (<= (car stretch) from) (<= to (cdr stretch)))))
                                 groups))))
        (and range (stretch-matches function rope matcher from to (eql to (cdr range)) texts halt (cdr range))))))

(defun split-piece-run (job piece matcher scope groups found index limit count too-many folds hidden)
  "Find the matches of MATCHER in PIECE of JOB, a split's search over SCOPE
(PIECE-MATCHES), keeping their start and end offsets in pairs at the piece's
place (INDEX) in FOUND, but those FOLDS hide any of (TUATARA.DISPLAY:FOLDS-HIDING)
when FOLDS is not NIL, counted in the car of HIDDEN; once there are more than
LIMIT in all, counted in the car of COUNT, set the car of TOO-MANY and stop the
job. A match may run past the piece's end (MEND-SEAMS)."
  (start-piece job piece nil)
  (let ((cancel (rewrite-cancel job))
        (offsets (make-array 64 :element-type 'fixnum :adjustable t :fill-pointer 0))
        (end (rewrite-piece-end piece)))
    (piece-matches (lambda (s e m g)
                     (declare (ignore m g))
                     (setf (rewrite-piece-done piece) e
                           (rewrite-piece-reach piece) (max e (rewrite-piece-reach piece)))
                     (if (and folds (tuatara.display:folds-hiding folds s e))
                         (tally hidden)
                         (progn
                           (vector-push-extend s offsets)
                           (vector-push-extend e offsets)
                           (when (and limit (>= (tally count) limit))
                             (setf (car too-many) t (car cancel) t))))
                     (car cancel))
                   (rewrite-rope job) matcher scope groups (rewrite-piece-start piece) end (lambda () (car cancel)))
    (setf (svref found (gethash piece index)) offsets)
    (unless (car cancel) (setf (rewrite-piece-done piece) end))))

(defun search-ended-here-p (buffer content)
  "Why what a search in the background found in BUFFER cannot be taken up now,
a message, or NIL when it can: BUFFER is still the one here, with the text it
had (CONTENT, its content number), in NORMAL state and with nothing asked."
  (flet ((not-held (why) (format nil (substitute-command-keys why :for-format t) (tuatara.buffer:buffer-name buffer))))
    (cond ((not (eq buffer (current-buffer)))
           (not-held "Not held: ~a was left while \\<normal>\\[split-selection] searched it"))
          ((not (eql content (tuatara.buffer:buffer-content-number buffer)))
           (not-held "Not held: ~a changed while \\<normal>\\[split-selection] searched it"))
          ((or (editor-minibuffer-active *editor*) (not (eq (modal-state buffer) :normal)))
           (not-held "Not held: ~a was busy when \\<normal>\\[split-selection] finished searching it")))))

(defun split-search-take-in (buffer regexp scope words whole found content limit hidden)
  "Hold as a split what a search in the background FOUND in BUFFER, in order,
the matches of REGEXP in SCOPE, named WORDS (the whole buffer with WHOLE): no
more than LIMIT places, past which those in sight are held, to act on every
match in the background; HIDDEN, the matches left alone because a fold hides
them, as HOLD-SPLIT takes it. Only while the search can be taken up
(SEARCH-ENDED-HERE-P). Returns what to say."
  (or (search-ended-here-p buffer content)
      (if (> (loop for offsets across found sum (floor (length offsets) 2)) limit)
          (armed-between-commands (hold-virtual-matches buffer regexp scope words whole (first-found found) :move nil))
          (let ((spans (loop for offsets across found
                             nconc (loop for k from 0 below (length offsets) by 2
                                         collect (cons (aref offsets k) (aref offsets (1+ k)))))))
            ;; point stays where moving about meanwhile left it: the verbs go to the places
            (armed-between-commands (hold-split buffer regexp spans words whole :move nil :hidden hidden))))))

(defun first-found (found)
  "The first match a search in the background FOUND, (START . END), or NIL."
  (loop for offsets across found
        when (plusp (length offsets)) return (cons (aref offsets 0) (aref offsets 1))))

(defun armed-between-commands (said)
  "Return SAID, after a transient map was set between commands: the next
command is asked whether it keeps it, as the one that set it would not be."
  (setf (editor-transient-fresh *editor*) nil)
  said)

(defun start-split-search (buffer regexp scope words whole)
  "Search SCOPE of BUFFER (a list of (START . END) stretches or a BLOCK-SCOPE),
named WORDS, for REGEXP in the background, cut into pieces searched on every
core at once, and hold the matches as a split when it ends
(SPLIT-SEARCH-TAKE-IN); or, as soon as there are more than a split may hold
(PLACE-LIMIT), hold those in sight, to act on them all. A match that a fold
hides any of as the search starts is left out. Returns the job, or :QUEUED when another job runs or waits in BUFFER: it
then waits its turn, SCOPE followed meanwhile."
  (when (queue-if-busy buffer (job-words :search :doing (format nil "~a for ~a" words regexp)) (scope-positions scope)
                       (lambda (&rest positions)
                         (start-split-search buffer regexp (scope-at scope positions) words whole))
                       :shade (scope-shade buffer scope))
    (return-from start-split-search :queued))
  (multiple-value-bind (limit why) (place-limit)
    (let* ((rope (tuatara.buffer:buffer-text buffer))
           (folds (tuatara.display:folds-now buffer))
           (hidden (list 0))
           (matcher (split-matcher regexp (block-scope-p scope) buffer))
           (groups (scope-groups rope scope))
           (pieces (coerce (loop for group in groups append (cdr group)) 'simple-vector))
           (index (let ((table (make-hash-table :test 'eq)))
                    (loop for piece across pieces for i from 0 do (setf (gethash piece table) i))
                    table))
           (found (make-array (length pieces) :initial-element #()))
           (count (list 0))
           (too-many (list nil))
           (content (tuatara.buffer:buffer-content-number buffer))
           (job (%make-rewrite :buffer buffer :base (tuatara.buffer:buffer-version buffer) :rope rope
                               :low (car (car (first groups))) :high (cdr (car (car (last groups))))
                               :words (format nil "the matches of ~a" regexp)
                               :what (job-words :search :doing (format nil "~a for ~a" words regexp))
                               :count-only t :doing (job-words :search :doing)
                               :verb (job-words :search :verb (format nil "for ~a" regexp)) :leaves "nothing held"
                               :runner (lambda (job piece)
                                         (split-piece-run job piece matcher scope groups found index limit count too-many folds hidden))
                               :report (lambda (job how changes seconds kept)
                                         (declare (ignore job changes seconds kept))
                                         (ecase how
                                           (:done (split-search-take-in buffer regexp scope words whole found content limit (car hidden)))
                                           ;; stopped at the limit, not by hand: the places found are held
                                           (:stopped (and (car too-many)
                                                          (or (search-ended-here-p buffer content)
                                                              (armed-between-commands
                                                               (hold-virtual-matches buffer regexp scope words whole
                                                                                     (first-found found) :move nil)))))))
                               :pieces pieces
                               :checkpoint *rewrite-checkpoint* :hook *rewrite-piece-hook*)))
      (launch-rewrite job "tuatara split" (format nil "~:d octets in ~d piece~:p, at most ~:d (~(~a~))"
                                                  (scope-extent scope) (length pieces) limit why))
      ;; the selection searched let go meanwhile: a key typed before the split comes acts at the cursor, not on all of it
      (setf (tuatara.buffer:buffer-mark-active-p buffer) nil)
      (say-in-background (job-words :search :doing (format nil "~a for ~a" words regexp)) "the split comes when it is done")
      job)))

;;; Past the limit: the places in sight held, the others written in the background

(defun virtual-now (buffer anchor)
  "Where ANCHOR stands in BUFFER now."
  (tuatara.text:anchor-position anchor (tuatara.buffer:buffer-version buffer)))

(defun virtual-scope-now (buffer virtual)
  "The scope VIRTUAL's matches are in, as it stands in BUFFER now."
  (scope-at (virtual-scope virtual) (mapcar (lambda (anchor) (virtual-now buffer anchor)) (virtual-anchors virtual))))

(defun virtual-spans (buffer virtual from to)
  "The places of VIRTUAL in BUFFER's text from the line FROM is on to the end
of the line TO is on, (START . END) in order: its matches there, or on each of
its rows there the columns LEFT to RIGHT, or the line's end."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (from (tuatara.text:rope-line-start rope from))
         (to (tuatara.text:rope-line-end rope to))
         (spans '()))
    (flet ((found (s e m g) (declare (ignore m g)) (push (cons s e) spans) nil))
      (ecase (virtual-kind virtual)
        (:matches
         (let ((scope (virtual-scope-now buffer virtual))
               (matcher (virtual-matcher virtual)))
           (if (block-scope-p scope)
               (let ((a (max from (block-scope-top scope))) (z (min to (block-scope-high scope))))
                 (when (< a z) (piece-matches #'found rope matcher scope nil a z nil)))
               (loop for (start . end) in scope
                     for a = (max from start) for z = (min to end)
                     when (<= a z) do (stretch-matches #'found rope matcher a z (= z end) nil nil)))))
        (:rows
         (let ((a (max (virtual-now buffer (virtual-top virtual)) from))
               (z (min (1+ to) (1+ (virtual-now buffer (virtual-bottom virtual))))))
           (when (< a z)
             (apply
              #'tuatara.text:map-line-slices
              (lambda (line s e start-column end-column line-end)
                (declare (ignore line start-column end-column))
                (push (if (virtual-at-end virtual) (cons line-end line-end) (cons s e)) spans))
              rope a z (virtual-left virtual) (virtual-right virtual)
              (tuatara.display:screen-slice-keys (buffer-crlf-p buffer) (virtual-tab-width virtual))))))))
    (nreverse spans)))

(defun virtual-held-p (split span)
  "True when SPLIT holds the place SPAN, (START . END), of its virtual part
already, or let it go (VIRTUAL-PASSED): a place meeting it, or for rows, one
reaching into its line."
  (let ((b (split-buffer split)))
    (if (eq (virtual-kind (split-virtual split)) :rows)
        (let ((start (tuatara.buffer:buffer-line-start b (car span)))
              (end (tuatara.buffer:buffer-line-end b (car span))))
          (and (or (places-in split start end)
                   (some (lambda (place) (<= start (place-bounds b place) end)) (virtual-passed (split-virtual split))))
               t))
        (flet ((meets-p (place)
                 (multiple-value-bind (s e) (place-bounds b place)
                   (and (<= s (cdr span)) (<= (car span) e)))))
          (or (some #'meets-p (places-in split (car span) (cdr span)))
              (some #'meets-p (virtual-passed (split-virtual split))))))))

(defun catch-up-places (split places)
  "Make PLACES, just held by SPLIT while typing at its places, what the typing
made the others: narrowed to the edge they were narrowed to, or emptied (a
block's row padded out to its column), then what was typed written; from then
on each key is written at them as it is typed. A match without the very text
deleted beside the others is let go first, left as it is, and so is one whose
emptying would take part of the text a fold hides, but not all of it. Returns
the places kept."
  (let* ((b (split-buffer split)) (virtual (split-virtual split)) (typing *split-typing*)
         (since (and typing (split-diff (split-typing-original typing) (split-text split) (split-typing-edges typing)))))
    (labels ((pass (other)
               (when other
                 (setf places (remove-if (lambda (place) (member place other)) places)
                       (split-places split) (remove-if (lambda (place) (member place other)) (split-places split))
                       (virtual-passed virtual) (append other (virtual-passed virtual)))))
             (let-go ()
               (when (and since (split-diff-edges since) (not (split-cursors split)))
                 (pass (remove-if-not (lambda (place) (eq (place-edit b place since nil :exact t) :differs)) places))))
             (let-go-hidden ()
               ;; a place whose emptying would take part of what a fold hides
               (let ((folds (tuatara.display:folds-now b)))
                 (when folds
                   (pass (remove-if-not (lambda (place)
                                          (multiple-value-bind (s e) (place-bounds b place)
                                            (tuatara.display:folds-cut folds s e)))
                                        places))))))
      (if (eq (virtual-mode virtual) :clear)
          ;; emptied as one change (joining the typing's), a block's short rows padded out
          (progn
            (let-go-hidden)
            (let-go)
            (let ((tails '()))
              (when places
                (let ((*split-copying* t)
                      (column (if (virtual-tab-width virtual) #'tuatara.display:buffer-screen-column #'tuatara.buffer:buffer-column)))
                  (delete-spans b (held-spans-of b places) :folds-checked t :group (and typing t)
                                  :replacements
                                  (loop for place in places
                                        collect (multiple-value-bind (s e) (place-bounds b place)
                                                  ;; a short row padded out; a tab across an edge, its columns outside it
                                                  (let ((pad (if (or (virtual-pad virtual) (and (< s e) (virtual-tab-width virtual)))
                                                                 (max 0 (- (virtual-left virtual) (funcall column b s)))
                                                                 0))
                                                        (tail (if (and (< s e) (virtual-tab-width virtual))
                                                                  (max 0 (- (funcall column b e) (virtual-right virtual)))
                                                                  0)))
                                                    (push tail tails)
                                                    (and (plusp (+ pad tail)) (spaces (+ pad tail)))))))))
              ;; each place is now its padding: the cursor goes after it, before the spaces past the block
              (loop for place in places
                    for tail in (nreverse tails)
                    do (let ((e (- (nth-value 1 (place-bounds b place)) tail))) (move-place b place e e)))))
          (progn
            (dolist (place places)
              (multiple-value-bind (s e) (place-bounds b place)
                (if (eq (virtual-mode virtual) :end) (move-place b place e e) (move-place b place s s))))
            (let-go))))
    (when typing
      (when since (write-places split (mapcar (lambda (place) (cons place since)) places)))
      (dolist (place places) (setf (gethash place (split-typing-live typing)) t)))
    places))

(defun virtual-fill (split &key prepare)
  "Hold the places of SPLIT's virtual part that have come into sight
(SIGHT-RANGES), as many as it may hold: a cursor at each of a split of cursors,
and while typing, or with PREPARE, each made what the verb made the others
(CATCH-UP-PLACES). A match that a fold hides any of is not held. Nothing is
looked for again while the text, the places, what the verb made of them and
what is in sight are as they were. Returns those held."
  (let* ((virtual (split-virtual split))
         (b (split-buffer split))
         (room (and virtual (- (virtual-limit virtual) (length (split-places split)))))
         (ranges (and room (plusp room) (sight-ranges b)))
         (seen (and ranges (list* (tuatara.buffer:buffer-version b) (length (split-places split))
                                  (virtual-column virtual) (virtual-left virtual) (virtual-right virtual)
                                  (virtual-at-end virtual) (virtual-pad virtual) (virtual-mode virtual) ranges))))
    (when (and seen (not (equal seen (virtual-seen virtual))))
      (setf (virtual-seen virtual) seen)
      (let ((new '())
            (folds (and (eq (virtual-kind virtual) :matches) (tuatara.display:folds-now b))))
        (dolist (range ranges)
          (dolist (span (virtual-spans b virtual (car range) (cdr range)))
            (unless (or (virtual-held-p split span) (find (car span) new :key #'car)
                        (and folds (tuatara.display:folds-hiding folds (car span) (cdr span))))
              (push span new))))
        (when new
          (let* ((sorted (sort new #'< :key #'car))
                 (places (mapcar (lambda (span) (make-place b (car span) (cdr span)))
                                 (subseq sorted 0 (min room (length sorted))))))
            (when (and (split-cursors split) (not (eq (split-phase split) :edit)) (not prepare))
              (dolist (place places)
                (set-cursor b place (place-bounds b place) nil nil nil)))
            (setf (split-places split)
                  (merge 'simple-vector (copy-seq (split-places split)) (coerce places 'simple-vector) #'<
                         :key (lambda (place) (place-bounds b place))))
            (if (or prepare (eq (split-phase split) :edit))
                (catch-up-places split places)
                places)))))))

(defun held-spans-of (buffer places)
  "Where PLACES of BUFFER are now, (START . END), in their order, as a vector."
  (map 'simple-vector (lambda (place) (multiple-value-bind (s e) (place-bounds buffer place) (cons s e))) places))

(defun held-spans (split)
  "Where the places SPLIT holds are now, (START . END), in order, as a vector."
  (held-spans-of (split-buffer split) (split-places split)))

(defun held-lines (buffer spans)
  "The starts of the lines of BUFFER that SPANS, (START . END), reach into, as keys of a hash table."
  (let ((lines (make-hash-table)))
    (loop for (start . end) across spans
          do (loop for line = (tuatara.buffer:buffer-line-start buffer start)
                     then (tuatara.buffer:buffer-forward-char buffer (tuatara.buffer:buffer-line-end buffer line))
                   do (setf (gethash line lines) t)
                   while (< (tuatara.buffer:buffer-line-end buffer line) end)))
    lines))

(defun typed-match (rope s e mode text back forward)
  "What the typing at a split's places makes of the match from S to E of ROPE,
as it made the places it was typed at: TEXT (octets) at the match's start or
end (MODE :START or :END) or in place of it (:CLEAR), the text BACK (octets)
just before the place and FORWARD just after it deleted too. Three values,
where the change starts and ends and the octets put there; NIL when the very
text deleted beside the place it was typed at is not there beside this one."
  (declare (type fixnum s e) (type tuatara.text:octets back forward))
  (let* ((from (if (eq mode :end) e s))
         (to (if (eq mode :start) s e))
         (start (- from (length back)))
         (end (+ to (length forward))))
    (declare (type fixnum from to start end))
    (when (and (>= start 0) (<= end (tuatara.text:rope-bytes rope))
               (loop for i of-type fixnum from 0 below (length back)
                     always (= (tuatara.text:rope-octet rope (+ start i)) (aref back i)))
               (loop for i of-type fixnum from 0 below (length forward)
                     always (= (tuatara.text:rope-octet rope (+ to i)) (aref forward i))))
      (values start end text))))

(defstruct (matches-job (:constructor make-matches-job) (:copier nil))
  "What a background job at every match of a split's pattern changes
\(START-EVERY-MATCH-JOB): in ROPE, the matches MATCHER finds in SCOPE, cut into
GROUPS (SCOPE-GROUPS), up to HIGH, the scope's end. KIND says what changes:
:TYPE writes what was typed at the places (TYPED-MATCH: TEXT as MODE says,
the octets BACK before the match and FORWARD after it deleted too), :DELETE
takes the match away, :WRAP puts OPEN before it and CLOSE after it, the
match's text read for it (TEXTS), and :UNWRAP takes them off where they stand
just outside it (WRAPPED-MATCH). The matches meeting a place of SKIP (a vector
of (START . END) in order), done in sight, are left as they are. FOLDS are the
folds of the text as the job began (FOLDS-NOW): a match one hides any of is
left as it is, counted in the car of LEFT, and one whose change would take
part of the text one hides, but not all of it, in the car of HIDDEN; REFUSED
counts in its car the matches :TYPE could not change."
  rope matcher scope groups (high 0) kind texts mode text (back *no-octets*) (forward *no-octets*)
  open close skip folds (refused (list 0)) (hidden (list 0)) (left (list 0)))

(setf (documentation 'make-matches-job 'function) "Return a MATCHES-JOB: what a background job at every match changes."
      (documentation 'matches-job-p 'function) "True when OBJECT is a MATCHES-JOB.")

(defun start-every-match-job (buffer regexp scope words kind &key typed open close skip joins report matcher)
  "Write at every match of REGEXP in SCOPE of BUFFER (a list of stretches or a
BLOCK-SCOPE, named WORDS), but those meeting a place of SKIP (a vector of
(START . END) in order), in the background on all but two cores: KIND :TYPE as
TYPED says (TYPED-MATCH), :DELETE, :WRAP each in OPEN and CLOSE, or :UNWRAP
the pair off where it stands outside it. A match that a fold hides any of as
the job starts is left alone, and the report counts it; and one whose change
would take part of the text a fold hides, but not all of it, is left as it is.
One change for undo, joining JOINS's (REWRITE), edits made meanwhile kept,
\\[stop-job] to stop it; REPORT, when given, says how it ended; MATCHER, when
given, finds REGEXP as SPLIT-MATCHER made it for the split. Returns the job,
or :QUEUED when another job runs or waits in the buffer, or its lines are
still being counted: it then waits its turn, the matches' stretches and SKIP's
places followed meanwhile."
  (let ((what (job-words kind :doing (format nil "every other match of ~a in ~a" regexp words))))
    (when (or (job-busy-p buffer) (queued-in buffer) (lines-uncounted-p buffer))
      (let ((followed (and skip (spans-followed buffer skip))))
        (when (queue-if-busy buffer what (scope-positions scope)
                             (lambda (&rest positions)
                               (start-every-match-job buffer regexp (scope-at scope positions) words kind
                                                      :typed typed :open open :close close
                                                      :report report :matcher matcher
                                                      :skip (and followed (funcall followed))))
                             :shade (scope-shade buffer scope)
                             :counted t)
          (return-from start-every-match-job :queued))))
    (tuatara.buffer:ensure-indexed buffer)
    (let* ((rope (tuatara.buffer:buffer-text buffer))
           (groups (scope-groups rope scope))
           (spec (make-matches-job :rope rope :scope scope :groups groups :high (cdr (car (car (last groups))))
                                   :matcher (or matcher (split-matcher regexp (block-scope-p scope) buffer))
                                   :kind kind :texts (eq kind :wrap)
                                   :mode (getf typed :mode) :text (getf typed :text)
                                   :back (getf typed :back-octets *no-octets*)
                                   :forward (getf typed :forward-octets *no-octets*)
                                   :open (and open (tuatara.text:string-octets open))
                                   :close (and close (tuatara.text:string-octets close))
                                   :skip skip :folds (tuatara.display:folds-now buffer)))
           (job (%make-rewrite :session t :buffer buffer :base (tuatara.buffer:buffer-version buffer) :rope rope
                               :low (car (car (first groups))) :high (matches-job-high spec)
                               :words (format nil "every other match of ~a" regexp)
                               :what what
                               :joins joins :hidden-left (matches-job-left spec)
                               :mapper (lambda (function from to halt) (every-match-edits spec function from to halt))
                               :expand (lambda (m g) (declare (ignore m)) g)
                               :all t :doing (job-words kind :doing)
                               :verb (job-words kind :verb (format nil "the matches of ~a out of sight" regexp))
                               :leaves "the matches out of sight are as they were"
                               :report (or report (job-report kind
                                                              (lambda (n) (format nil "~:d more match~:*~[es~;~:;es~] of ~a, out of sight," n regexp))
                                                              (format nil "the other matches of ~a" regexp)
                                                              :refused (matches-job-refused spec) :hidden (matches-job-hidden spec)))
                               :pieces (coerce (loop for group in groups append (cdr group)) 'simple-vector)
                               :checkpoint *rewrite-checkpoint* :hook *rewrite-piece-hook*
                               :directory (tuatara.io:buffer-scratch-directory buffer))))
      (launch-rewrite job "tuatara every match" (format nil "~:d octets in ~d piece~:p, ~:d held"
                                                        (scope-extent scope) (length (rewrite-pieces job)) (length skip)))
      job)))

(defun match-change (spec s e m)
  "What SPEC, a MATCHES-JOB, writes at the match from S below E, M its text
when SPEC reads it: the stretch from A below Z it writes over and the octets
it puts there, three values; NIL when that match cannot be changed (what was
typed beside the places not there beside it, counted in REFUSED; no pair
around it to take off)."
  (let ((rope (matches-job-rope spec)) (open (matches-job-open spec)) (close (matches-job-close spec)))
    (ecase (matches-job-kind spec)
      (:type (multiple-value-bind (a z octets)
                 (typed-match rope s e (matches-job-mode spec) (matches-job-text spec)
                              (matches-job-back spec) (matches-job-forward spec))
               (unless a (tally (matches-job-refused spec)))
               (values a z octets)))
      (:delete (values s e *no-octets*))
      (:wrap (values s e (tuatara.text:string-octets (concatenate 'string (tuatara.text:octets-string open)
                                                                  m (tuatara.text:octets-string close)))))
      (:unwrap (multiple-value-bind (a z) (wrapped-match rope s e open close)
                 (and a (values a z (tuatara.text:rope-octets rope :start s :end e))))))))

(defun every-match-edits (spec function from to halt)
  "The mapper of a job at every match (REWRITE), SPEC a MATCHES-JOB: call
FUNCTION with (A Z NIL OCTETS) for each change MATCH-CHANGE makes to a match
starting from FROM below TO, in order (PIECE-MATCHES), a match left as it is
told as passed over (PASS-OVER), and one whose change would overlap the one
before or run past the scope left as it is too. What folds keep it from is
counted as the pieces are written (HALT given), not as its result is mapped."
  (let ((previous from) (skip (matches-job-skip spec)) (folds (matches-job-folds spec)))
    (piece-matches
     (lambda (s e m g)
       (declare (ignore g))
       (cond
         ((and skip (spans-meet-p skip s e)) (pass-over function s e))
         ((and folds (tuatara.display:folds-hiding folds s e))
          (when halt (tally (matches-job-left spec)))
          (pass-over function s e))
         (t
          (multiple-value-bind (a z octets) (match-change spec s e m)
            (cond ((not (and a (>= a previous) (<= z (matches-job-high spec))))
                   (pass-over function s e))
                  ((and folds (tuatara.display:folds-cut folds a z))
                   (when halt (tally (matches-job-hidden spec)))
                   (pass-over function s e))
                  (t (setf previous z)
                     (funcall function a z nil octets))))))
       nil)
     (matches-job-rope spec) (matches-job-matcher spec) (matches-job-scope spec) (matches-job-groups spec)
     from to halt :texts (matches-job-texts spec))))

(defun virtual-typed (typing new)
  "What TYPING, a SPLIT-TYPING, typed at the places, NEW the text the primary
place holds now, for TYPED-MATCH: :TEXT, and the text deleted beside the
places, :BACK-OCTETS and :FORWARD-OCTETS, and how many characters it is,
:BACK and :FORWARD, with the offsets of its line breaks."
  (multiple-value-bind (before after) (edge-texts (split-typing-edges typing))
    (list :text (tuatara.text:string-octets new)
          :back-octets (tuatara.text:string-octets before) :forward-octets (tuatara.text:string-octets after)
          :back (length before) :back-breaks (line-break-offsets before)
          :forward (length after) :forward-breaks (line-break-offsets after))))

(defun virtual-words (split)
  "What the places of SPLIT past its limit that it does not hold are called."
  (let ((virtual (split-virtual split)))
    (if (eq (virtual-kind virtual) :matches)
        (format nil "the other matches of ~a" (virtual-regexp virtual))
        "the other lines")))

(defun start-virtual-job (split &key typing kind open close)
  "Write at the places of SPLIT past its limit that it does not hold, in the
background, what was done at those it holds: what TYPING (a SPLIT-TYPING)
typed there, or KIND :DELETE, :WRAP or :UNWRAP (in the pair OPEN and CLOSE).
The places held are left as they are, and the change joins theirs for undo
when nothing comes between. Says so; returns the job, :QUEUED or NIL."
  (let* ((b (split-buffer split))
         (virtual (split-virtual split))
         (typed (and typing (virtual-typed typing (split-text split))))
         (skip (held-spans split))
         (joins (tuatara.buffer:buffer-undo-stack b)))
    (when (and typing (equal (split-typing-original typing) (split-text split)) (null (split-typing-edges typing))
               (not (eq (virtual-mode virtual) :clear)))
      (return-from start-virtual-job nil))
    (let ((job
            (ecase (virtual-kind virtual)
              (:matches
               (start-every-match-job b (virtual-regexp virtual) (virtual-scope-now b virtual) (split-scope split)
                                      (or kind :type)
                                      :typed (and typed (list* :mode (virtual-mode virtual) typed))
                                      :open open :close close :skip skip :joins joins
                                      :matcher (virtual-matcher virtual)))
              (:rows
               (cond ((member kind '(:wrap :unwrap))
                      (message "~@(~a~) were left as they were: a wrap is made only at those in sight" (virtual-words split))
                      (return-from start-virtual-job nil))
                     ((or (getf typed :back-breaks) (getf typed :forward-breaks))
                      (message "~@(~a~) were left as they were: a deletion across a line's end is made only at those in sight"
                               (virtual-words split))
                      (return-from start-virtual-job nil))
                     (t (start-lines-job b virtual (or kind :type)
                                         :text (if typing (split-text split) "")
                                         :back (or (getf typed :back) 0) :forward (or (getf typed :forward) 0)
                                         :skip skip :joins joins)))))))
      (when job
        (setf (virtual-started virtual) t)
        (note-split-writer b job (virtual-words split))
        (say-in-background (job-words (or kind :type) :doing (virtual-words split))))
      job)))

;;; The verbs

(defun delete-places (split)
  "Delete the text of every place of SPLIT, one change (DELETE-SPANS); returns
what they held. When that would take part of the text a fold hides, but not
all of it, nothing is deleted: the fold is shown instead."
  (let* ((b (split-buffer split))
         (texts (map 'list (lambda (place) (place-text b place)) (split-places split))))
    (delete-spans b (held-spans split))
    texts))

(defun enter-split-edit (split clear where &key (since (tuatara.buffer:buffer-undo-stack (split-buffer split))))
  "Start typing at every place of SPLIT: point at the primary place's start,
or its end when WHERE is :END; with CLEAR, every place emptied first, what
they held on the kill ring. Places that differ are narrowed to that edge, so
only what is typed is copied. What is changed from SINCE, the buffer's undo stack
before the verb began (as it is now by default), to the typing's end is one
change for undo, the emptying with the typing, as \\<normal>\\[tuatara-change]
is at every kind of many places: a split's matches, cursors, a rectangle's
rows."
  (check-writable (split-buffer split))
  (let* ((b (split-buffer split)) (virtual (split-virtual split)))
    (when (split-cursors split)
      (setf (split-agree split) (places-agree-p b (split-places split))))
    ;; past the limit, the matches out of sight need not agree with those held
    (when (and virtual (not (split-cursors split)))
      (setf (split-agree split) nil
            (virtual-mode virtual) (cond (clear :clear) ((eq where :end) :end) (t :start))))
    (unless (or clear (split-agree split))
      (loop for place across (split-places split)
            do (multiple-value-bind (s e) (place-bounds b place)
                 (let ((edge (if (eq where :end) e s))) (move-place b place edge edge)))))
    (when clear
      (let ((texts (delete-places split)))
        ;; past the limit only the places in sight are known: nothing to the kill ring
        (unless virtual (kill-in-parts texts))))
    (setf (split-did split) (cond (clear :changed) ((eq where :end) :after) (t :before)))
    (let ((*split-keeping* t)) (clear-transient-map *editor*))
    (let ((primary (or (and (split-cursors split) (split-primary split)) (aref (split-places split) 0))))
      (multiple-value-bind (s e) (place-bounds b primary)
        (goto (if (eq where :end) e s))
        (setf (split-primary split) primary
              (split-text split) (place-text b primary)
              (split-bounds split) (cons s e)
              (split-edges split) '()
              (split-strayed split) nil
              (split-since split) since
              (split-phase split) :edit)
        ;; written as it goes where it can be seen, the rest at the end
        (setf *split-typing* (let ((typing (make-split-typing (split-text split))))
                               (setf (gethash primary (split-typing-live typing)) t)
                               typing))))
    (pushnew 'split-note-change (tuatara.buffer:buffer-change-functions b))
    (tuatara-insert)
    (echo "~a" (split-hint split))))

(defun live-split ()
  "The split in force in this buffer; signals when there is none."
  (unless (split-here-p) (error "No split here"))
  *split*)

(define-command split-insert ()
  "Type before every place of the split, keeping what is there: what is typed
at one appears at all. \\<insert>\\[tuatara-normal] ends it, keeping what
was typed."
  (enter-split-edit (live-split) nil :start))

(define-command split-append ()
  "Type after every place of the split, keeping what is there."
  (enter-split-edit (live-split) nil :end))

(define-command split-change ()
  "Empty every place of the split, then type at all of them. What they held
goes on the kill ring as one kill: one copy when they held the same text,
else one a line. When a place would take part of the text a fold hides, but
not all of it, nothing is emptied: the fold is shown instead, and the key
pressed again empties them; a match out of sight in such a place is left as
it is."
  (enter-split-edit (live-split) t :start))

(define-command split-delete ()
  "Delete every place of the split, one change for undo, and end the split.
What they held goes on the kill ring as one kill. When a place would take
part of the text a fold hides, but not all of it, nothing is deleted: the
fold is shown instead, and the key pressed again deletes; a match out of
sight in such a place is left as it is."
  (let ((split (live-split)))
    (check-writable (split-buffer split))
    (let ((texts (delete-places split)))
      (unless (split-virtual split) (kill-in-parts texts)))
    (setf (split-did split) :deleted)
    (when (split-virtual split) (start-virtual-job split :kind :delete))
    (dissolve-split)))

(defun wrap-places (split char)
  "Wrap every place of SPLIT (in a split of cursors, every one selecting) in
the pair of the wrap key CHAR (WRAP-PAIR), or take it off where it stands
outside every one; one change (DELETE-SPANS), the places still held inside."
  (check-writable (split-buffer split))
  (multiple-value-bind (open close) (wrap-pair char)
    (let* ((b (split-buffer split))
           (targets (remove-if-not (lambda (place) (or (not (split-cursors split))
                                                       (multiple-value-bind (s e) (place-bounds b place) (< s e))))
                                   (coerce (split-places split) 'list)))
           (off (and targets
                     (every (lambda (place) (multiple-value-bind (s e) (place-bounds b place)
                                              (already-wrapped-p b s e open close)))
                            targets)))
           (opening (string open))
           (closing (string close)))
      (cond ((null targets) (message "~a is undefined" (pressed-key-name)))
            (t
             (let* ((bounds (mapcar (lambda (place) (multiple-value-list (place-bounds b place))) targets))
                    (spans (loop for (s e) in bounds
                                 if off collect (cons (tuatara.buffer:buffer-forward-char b s -1) s)
                                   and collect (cons e (tuatara.buffer:buffer-forward-char b e))
                                 else collect (cons s s) and collect (cons e e)))
                    (positions (delete-spans b spans :replacements (loop for span in bounds
                                                                         collect (and (not off) opening)
                                                                         collect (and (not off) closing)))))
               ;; each place goes back between its pair, or where the pair was
               (loop for place in targets
                     for i from 0 by 2
                     do (if off
                            (move-place b place (aref positions i) (aref positions (1+ i)))
                            (move-place b place (aref positions i)
                                        (- (aref positions (1+ i)) (tuatara.text:string-octet-length closing))))))
             (setf (split-did split) (if off :unwrapped :wrapped)
                   (split-text split) (place-text b (split-primary split)))
             (when (split-virtual split)
               (start-virtual-job split :kind (if off :unwrap :wrap) :open opening :close closing))
             (when (split-cursors split) (adopt-cursor-places split))
             (echo "~a" (split-hint split)))))))

(define-command split-wrap ()
  "Wrap every place of the split in the pair of the delimiter asked for, or
take the pair off where it stands outside every place already. The split
stays, so pairs nest and a verb can still follow."
  (wrap-places (live-split) (read-typed-char "Wrap every place in: " "to wrap in")))

(define-command split-wrap-key ()
  "Wrap every place of the split in the pair of the delimiter pressed, or take
it off; see split-wrap."
  (let ((key (editor-this-key *editor*)))
    (wrap-places (live-split) (tuatara.frontend:key-base key))))

(define-command split-quit ()
  "End the split, keeping what it changed. With cursors in a mark run, or
selecting, let go of the selections and keep the cursors."
  (let ((split *split*))
    (if (and split (split-cursors split)
             (or (split-running split) (some #'cursor-selecting-p (split-places split))))
        (progn (when (split-running split) (end-cursors-run split))
               (deselect-cursors split))
        (dissolve-split))))

;;; Typing at every place

(defun note-edge (split side octets)
  "Note that the command deleted OCTETS just past SIDE (:BEFORE or :AFTER) of SPLIT's primary place."
  (push (cons side (tuatara.text:octets-string octets)) (split-edges split)))

(defun split-note-change (buffer batch)
  "After each change to the split's buffer while typing at its places: note a
deletion reaching past the primary place's edges, and a change away from the
places, and follow the primary place's bounds, through a background job's
result taken in too (which is no typing of yours)."
  (let ((split *split*))
    (when (and split (eq buffer (split-buffer split)) (eq (split-phase split) :edit))
      (destructuring-bind (s . e) (split-bounds split)
        (unless (or *split-copying* (tuatara.text:batch-swap batch))
          (loop for edit across (tuatara.text:batch-edits batch)
                for a = (tuatara.text:edit-start edit)
                for z = (tuatara.text:edit-end edit)
                for inserts = (plusp (tuatara.text:edit-insert-length edit))
                for deleted = (tuatara.text:edit-deleted edit)
                do (cond ((and (<= s a) (<= z e)))
                         ((and (not inserts) (< a s) (>= z s) (<= z e))
                          (note-edge split :before (subseq deleted 0 (- s a))))
                         ((and (not inserts) (<= s a e) (> z e))
                          (note-edge split :after (subseq deleted (- e a))))
                         ((and (not inserts) (< a s) (> z e))
                          (note-edge split :before (subseq deleted 0 (- s a)))
                          (note-edge split :after (subseq deleted (- e a))))
                         (t (setf (split-strayed split) t)))))
        (setf (split-bounds split) (cons (tuatara.text:map-position s batch :before)
                                         (tuatara.text:map-position e batch :after)))))))

(defun place-at (split position)
  "The place of SPLIT that POSITION is in or at the edge of, or NIL."
  (let* ((b (split-buffer split)) (places (split-places split)) (lo 0) (hi (length places)))
    (setf lo (tuatara.runtime:bisect (mid lo hi) (<= (place-bounds b (aref places mid)) position)) hi lo)
    ;; the last place starting at or before POSITION, or the one after when an
    ;; empty place stands just there
    (loop for i from (max 0 (- lo 1)) below (min (length places) (+ lo 1))
          do (multiple-value-bind (s e) (place-bounds b (aref places i))
               (when (<= s position e) (return (aref places i)))))))

(defun line-break-offsets (text)
  "The offsets of the line breaks in TEXT."
  (loop for i from 0 below (length text) when (char= (char text i) #\Newline) collect i))

(defun common-ends (old new)
  "Two values: how many characters OLD and NEW share at the start, and at the
end after that."
  (let* ((prefix (or (mismatch old new) (length old)))
         (room (- (min (length old) (length new)) prefix))
         (suffix (loop for k from 0 below room
                       while (char= (char old (- (length old) 1 k)) (char new (- (length new) 1 k)))
                       count t)))
    (values prefix suffix)))

(defun edge-texts (edges)
  "The text deleted just before a place and the text deleted just after it,
as two values, from EDGES (oldest first, each (SIDE . TEXT), SIDE :BEFORE or
:AFTER)."
  (values (with-output-to-string (out)
            (dolist (edge (reverse edges)) (when (eq (car edge) :before) (write-string (cdr edge) out))))
          (with-output-to-string (out)
            (dolist (edge edges) (when (eq (car edge) :after) (write-string (cdr edge) out))))))

(defun split-diff (old new edges)
  "What turns a place holding OLD into one holding NEW, EDGES (oldest first,
each (SIDE . TEXT)) deleted just past its edges: a SPLIT-DIFF, or NIL when
nothing changes."
  (multiple-value-bind (before after) (edge-texts edges)
    (let ((changed (string/= old new)))
      (when (or changed edges)
        (multiple-value-bind (prefix suffix) (common-ends old new)
          (%make-split-diff :changed changed :edges (and edges t)
                            :before before :after after
                            :before-breaks (line-break-offsets before) :after-breaks (line-break-offsets after)
                            :whole (tuatara.text:string-octets new)
                            :middle (tuatara.text:string-octets (subseq new prefix (- (length new) suffix)))
                            :prefix-octets (tuatara.text:string-octet-length old :end prefix)
                            :suffix-octets (tuatara.text:string-octet-length old :start (- (length old) suffix))
                            :old-octets (tuatara.text:string-octet-length old)))))))

(defun char-steps (buffer text)
  "How many steps of BUFFER-FORWARD-CHAR in BUFFER pass over TEXT: its
characters, a carriage return and the newline after it one step where
BUFFER's lines end with both."
  (if (buffer-crlf-p buffer)
      (- (length text) (search-count (coerce '(#\Return #\Newline) 'string) text))
      (length text)))

(defun place-edit (buffer place diff previous-end &key exact)
  "Two values: the edit that changes PLACE of BUFFER as DIFF says (NIL when
none is needed), and where the place then reaches to. :DIFFERS when the
deletions past its edges cannot be made there the same way: the same number
of characters with the line breaks where they were, or with EXACT the very
same text; :MEETS when they would reach PREVIOUS-END, the end of the place
before (when given)."
  (multiple-value-bind (s e) (place-bounds buffer place)
    (let* ((before (split-diff-before diff))
           (after (split-diff-after diff))
           (from (if (plusp (length before)) (tuatara.buffer:buffer-forward-char buffer s (- (char-steps buffer before))) s))
           (to (if (plusp (length after)) (tuatara.buffer:buffer-forward-char buffer e (char-steps buffer after)) e)))
      (flet ((same-p (there text breaks)
               (if exact
                   (string= there text)
                   (and (= (length there) (length text)) (equal (line-break-offsets there) breaks)))))
        (cond ((not (and (or (zerop (length before))
                             (same-p (tuatara.buffer:buffer-string buffer :start from :end s)
                                     before (split-diff-before-breaks diff)))
                         (or (zerop (length after))
                             (same-p (tuatara.buffer:buffer-string buffer :start e :end to)
                                     after (split-diff-after-breaks diff)))))
               :differs)
              ((and previous-end (<= from previous-end)) :meets)
              (t (values (flet ((octets (vector) (and (plusp (length vector)) vector)))
                           (cond ((or (split-diff-edges diff) (/= (- e s) (split-diff-old-octets diff)))
                                  (tuatara.text:make-edit from to (octets (split-diff-whole diff))))
                                 ((split-diff-changed diff)
                                  (tuatara.text:make-edit (+ s (split-diff-prefix-octets diff)) (- e (split-diff-suffix-octets diff))
                                                          (octets (split-diff-middle diff))))))
                         to)))))))

(defun write-places (split items &key check)
  "Write ITEMS of SPLIT, each (PLACE . DIFF): as the SPLIT-DIFF says, nothing
for NIL or :HERE (the primary place, as it should be already); one change
joining the command's for undo. With CHECK, ITEMS are every place in order,
each checked against the one before (PLACE-EDIT). A deletion past the edges
is made at a match only where the very text deleted is there, at a cursor
where as many characters are. True when written; else, writing nothing, NIL
and why: :DIFFERS when the text beside a place is not the same, :MEETS when a
deletion would reach the place before."
  (let ((b (split-buffer split)) (edits '()) (previous-end -1) (exact (not (split-cursors split))))
    (loop for (place . diff) in items
          do (if (or (null diff) (eq diff :here))
                 (setf previous-end (nth-value 1 (place-bounds b place)))
                 (multiple-value-bind (edit end) (place-edit b place diff (and check previous-end) :exact exact)
                   (when (member edit '(:differs :meets)) (return-from write-places (values nil edit)))
                   (setf previous-end end)
                   (when edit (push edit edits)))))
    (when edits
      (let ((*split-copying* t))
        (tuatara.buffer:buffer-edit b (coerce (sort edits #'< :key #'tuatara.text:edit-start) 'simple-vector)
                                    :group t)))
    t))

(defun write-every-place (split step here)
  "Write STEP (a SPLIT-DIFF, or NIL) at every place of SPLIT but HERE, each
checked against its neighbours; the places the typing has not written yet
(*SPLIT-TYPING*) are brought up to the primary place as last copied first, and
from then on every key is written at them all. True when written; else NIL and
why, as WRITE-PLACES says."
  (let ((typing *split-typing*))
    (when typing
      (let ((since (split-diff (split-typing-original typing) (split-text split) (split-typing-edges typing)))
            (live (split-typing-live typing)))
        (multiple-value-bind (written why)
            (or (null since)
                (write-places split (loop for place across (split-places split)
                                          collect (cons place (if (gethash place live) :here since)))
                              :check t))
          (loop for place across (split-places split) do (setf (gethash place live) t))
          (unless written (return-from write-every-place (values nil why))))))
    (or (null step)
        (write-places split (loop for place across (split-places split)
                                  collect (cons place (if (eq place here) :here step)))
                      :check t))))

(defun sight-ranges (buffer)
  "The stretches of BUFFER in sight or near it, as (FROM . TO): in every window
showing it, from a little before each stretch of text the window shows to a
little after it (what a fold or a search filter hides between them left
out), and around point."
  (let* ((size (tuatara.buffer:buffer-size buffer))
         (point (tuatara.buffer:buffer-point buffer))
         (ranges (list (cons (max 0 (- point *split-sight-octets*)) (min size (+ point *split-sight-octets*))))))
    (dolist (window (editor-windows *editor*) ranges)
      (when (eq (tuatara.display:window-buffer window) buffer)
        (let ((start (min size (tuatara.display:window-start window)))
              (rows (tuatara.display:window-rows window)))
          (loop for (from . to) in (or (tuatara.display:shown-ranges rows :buffer buffer) (list (cons start start)))
                for margin = (max *split-sight-octets* (* 4 (- to from)))
                do (push (cons (max 0 (- from margin)) (min size (+ to margin))) ranges)))))))

(defun split-places-in-sight (split)
  "The places of SPLIT in sight or near it (SIGHT-RANGES)."
  (remove-duplicates (loop for (from . to) in (sight-ranges (split-buffer split)) append (places-in split from to))))

(defun copy-to-places (split here)
  "Write what the command did at the place HERE onto the other places of
SPLIT: its text, and the deletions just past its edges, one change joining the
command's for undo. While typing (*SPLIT-TYPING*), only at the places in sight
or near it, and those written so before; the others once the typing ends
(WRITE-PENDING-PLACES). A deletion past the edges is
written at every place at once, checked against its neighbours, and so is
every key after it. True when written; else, the command's change left at
HERE alone and the split as it was before it, NIL and why (WRITE-PLACES)."
  (let* ((b (split-buffer split))
         (typing *split-typing*)
         (old (split-text split))
         (new (place-text b here))
         (edges (reverse (split-edges split)))
         (step (split-diff old new edges)))
    (setf (split-edges split) '())
    (multiple-value-bind (written why)
        (cond ((null step) t)
              ((or (null typing) edges (= (hash-table-count (split-typing-live typing)) (length (split-places split))))
               (write-every-place split step here))
              (t
               (let ((live (split-typing-live typing))
                     (since :unknown)
                     (items (list (cons here :here))))
                 (loop for place being the hash-keys of live
                       unless (eq place here) do (push (cons place step) items))
                 (dolist (place (split-places-in-sight split))
                   (unless (gethash place live)
                     (when (eq since :unknown)
                       (setf since (split-diff (split-typing-original typing) new (split-typing-edges typing))))
                     (push (cons place since) items)
                     (setf (gethash place live) t)))
                 (write-places split items))))
      (when written
        (when (and typing edges)
          (setf (split-typing-edges typing) (append (split-typing-edges typing) edges)))
        (setf (split-text split) new))
      (values written why))))

(defun write-pending-places (split)
  "The typing at SPLIT's places is over: write the places it has not written
yet as the primary place stood when last copied, one change. False when a
deletion past the edges could not be made the same way at every place."
  (prog1 (or (null *split-typing*) (write-every-place split nil nil))
    (setf *split-typing* nil)))

(defun split-sync (editor)
  "After every command while a split lasts: copy what was typed at the place
point is in onto the others, end the split when the typing is over or went
astray, and keep its reminder up."
  (declare (ignore editor))
  (let ((split *split*))
    (when split
      (let ((b (split-buffer split)))
        (when (and (split-virtual split) (live-buffer-p b) (eq (current-buffer) b)
                   (or (not (eq (split-phase split) :edit)) (eq (modal-state b) :insert)))
          ;; the places come into sight held
          (virtual-fill split))
        (cond ((not (live-buffer-p b)) (setf *split* nil *split-typing* nil))
              ((not (eq (split-phase split) :edit))
               (when (and (null (editor-message *editor*)) (eq (current-buffer) b))
                 (echo "~a" (split-hint split))))
              ((not (eq (modal-state b) :insert))
               ;; the typing is over
               (close-split-edit split)
               (if (split-cursors split) (resume-cursors split) (dissolve-split)))
              ((split-strayed split)
               (dissolve-split :quiet t)
               (message "Split ended: an edit away from the places"))
              (t
               (let ((here (place-at split (tuatara.buffer:buffer-point b))))
                 (cond ((null here) (dissolve-split))
                       (t
                        (unless (eq here (split-primary split))
                          ;; the place moved into holds what was last copied, as every place then does
                          (when *split-typing* (write-every-place split nil nil))
                          (multiple-value-bind (s e) (place-bounds b here)
                            (setf (split-primary split) here
                                  (split-text split) (place-text b here)
                                  (split-bounds split) (cons s e))))
                        (if (not (split-room-to-write-p split))
                            ;; the heap would run out: the split ends before it does
                            (let ((n (length (split-places split))))
                              (dissolve-split :quiet t)
                              (message "Split ended: no room in memory to write at ~:d places; what was typed is at one of them" n))
                            (multiple-value-bind (written why) (copy-to-places split here)
                              (cond (written
                                     (when (null (editor-message *editor*)) (echo "~a" (split-hint split))))
                                    (t (dissolve-split :quiet t)
                                       (if (eq why :differs)
                                           (message "Split ended: the text deleted beside this place differs at another; it was deleted here only")
                                           (message "Split ended: an edit beside a place could not be made at every place")))))))))))))))

;;; Showing the places and their cursors

(defun places-in (split from to)
  "The places of SPLIT that reach into FROM to TO."
  (let* ((b (split-buffer split)) (places (split-places split)) (lo 0) (hi (length places)))
    ;; the first place ending at or after FROM
    (setf lo (tuatara.runtime:bisect (mid lo hi) (< (nth-value 1 (place-bounds b (aref places mid))) from)) hi lo)
    (loop for i from lo below (length places)
          for place = (aref places i)
          while (<= (place-bounds b place) to)
          collect place)))

(defun split-highlights (editor window rows)
  "The places the split holds among ROWS of WINDOW, in :split-place: every
match, or what each cursor selects."
  (declare (ignore editor))
  (let ((split *split*))
    (when (and split (eq (tuatara.display:window-buffer window) (split-buffer split)) (plusp (length rows)))
      (let ((b (split-buffer split)))
        (loop for (from . to) in (tuatara.display:shown-ranges rows :buffer b)
              nconc (loop for place in (places-in split from to)
                          for (s e) = (multiple-value-list (place-bounds b place))
                          when (and (< s e) (or (not (split-cursors split)) (cursor-selecting-p place)))
                            collect (list s e :split-place)))))))

(defun split-cursor-positions (editor window rows)
  "Where the split's cursors are drawn in ROWS of WINDOW besides point: at the
start of every other match, or while typing as far into it as point is into
its own; in a split of cursors, at every other cursor."
  (declare (ignore editor))
  (let ((split *split*))
    (when (and split (eq (tuatara.display:window-buffer window) (split-buffer split)))
      (let* ((b (split-buffer split))
             (offset (and (eq (split-phase split) :edit)
                          (- (tuatara.buffer:buffer-point b) (car (split-bounds split))))))
        (loop for (from . to) in (tuatara.display:shown-ranges rows :buffer b)
              nconc (loop for place in (places-in split from to)
                          unless (eq place (split-primary split))
                            collect (multiple-value-bind (s e) (place-bounds b place)
                                      (cond (offset (max s (min e (+ s offset))))
                                            ((split-cursors split) (cursor-position b place))
                                            (t s)))))))))

(defun build-split-chooser-keymap ()
  "Make the keys a split waits for a verb on:
\\<*split-chooser-keymap*>\\[split-insert] \\[split-append]
\\[split-change] \\[split-delete] \\[split-wrap]
\\[split-quit], and each delimiter of *PAIR-DELIMITERS* as it
stands now."
  (setf *split-chooser-keymap* (make-keymap :name "split"))
  (define-keys *split-chooser-keymap* *split-chooser-bindings*)
  (bind-free-delimiter-keys *split-chooser-keymap* 'split-wrap-key))

(defun install-split (editor)
  "Bind the split's keys, and have EDITOR copy what is typed at its places and
show them; a buffer is not saved while a split is written at there, and a split
typed at ends before its buffer is read again from its file."
  (build-split-chooser-keymap)
  (pushnew 'split-highlights *highlight-functions*)
  (pushnew 'split-cursor-positions *other-cursor-functions*)
  (pushnew 'split-save-refusal *save-refusals*)
  (pushnew 'split-before-reload *before-reload-functions*)
  (pushnew 'split-sync (editor-post-command-functions editor)))
