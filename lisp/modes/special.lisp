;;;; lisp/modes/special.lisp --- Buffers a command fills: their text and faces, made again, kept current, left.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The lists, help pages and reports commands make (*Buffers*, *Jobs*, *Help*,
;;;; *Apropos*, the agenda, *Health*, the calendar, rimu's lists, the trash,
;;;; *Fonts*, *xref*...) are filled one way. SET-MADE-TEXT puts the whole text
;;;; in, read-only, unmodified and with nothing to undo, keeping the parts shown
;;;; in a face, which special-mode's one highlight method finds by halves;
;;;; RENDER-PIECES makes that text from pieces, point kept on its line or on the
;;;; entry it was on; TABLE-HEADING-PIECES and TABLE-ROW-PIECES lay out columns
;;;; under titles, control characters in their cells escaped; the lists of
;;;; marked-list-mode have their entries marked in one way, those of
;;;; sectioned-list-mode their sections folded in one way. Each list says how
;;;; it is made again (its :REFILL local), which
;;;; revert-buffer runs, on r and g r in every one of them; KEEP-SHOWN-CURRENT
;;;; names the lists made again by themselves while shown, when what they show
;;;; has changed; q leaves each of them alike (quit-window).

(in-package #:tuatara.defaults)

(defvar *shown-lists* '()
  "The kinds of list made again by themselves while a window shows them
\(KEEP-SHOWN-CURRENT): each (MODE KEY FOLLOW).")

;;; The text

(defun buffer-in-mode (name mode)
  "The buffer called NAME, made when there is none, in the major MODE: set to
it when it is in another, left as it is when it is in MODE already."
  (let ((buffer (find-or-make-buffer name)))
    (unless (typep (buffer-major-mode buffer) mode)
      (set-major-mode buffer mode))
    buffer))

(defun fresh-made-buffer (name mode text)
  "A new buffer NAME in the major MODE, one by that name before killed,
holding TEXT (a string or a rope), read-only text a command made
\(SET-MADE-TEXT, VIEW-MADE-TEXT), point at its start."
  (let ((old (tuatara.buffer:find-buffer name)))
    (when old (tuatara.buffer:kill-buffer old)))
  (let ((buffer (buffer-in-mode name mode)))
    (set-made-text buffer text)
    (setf (tuatara.buffer:buffer-point buffer) 0)
    (view-made-text buffer)))

(defvar *made-text-keymap* (make-keymap :name "text made by a command")
  "Keys of read-only text a command shows in a buffer of its own, in a major
mode other than special-mode (a commit git shows, a file's metadata), before
its mode's: \\`q' closes it (quit-window).")

(defun view-made-text (buffer)
  "Have BUFFER, read-only text a command made in a major mode other than
special-mode, read as special-mode's buffers are in modal editing (moving,
selecting, copying, nothing that edits), coloured as its mode colours it,
\\`q' closing it (*MADE-TEXT-KEYMAP*). Returns BUFFER."
  (setf (tuatara.buffer:buffer-local buffer :modal-class) :support
        (tuatara.buffer:buffer-local buffer :override-keymap) *made-text-keymap*)
  buffer)

(defun span-ends (spans)
  "For SPANS, a vector of (START END FACE) in the order of their starts, the
furthest end reached by each span and those before it, as a vector of the
same length: what MADE-SPANS-BETWEEN halves. O(n)."
  (let ((ends (make-array (length spans) :element-type 'fixnum)) (furthest 0))
    (loop for i from 0 below (length spans)
          do (setf furthest (max furthest (second (svref spans i)))
                   (aref ends i) furthest))
    ends))

(defun shown-list-kind (buffer)
  "The entry of *SHOWN-LISTS* for BUFFER's major mode, or NIL when it is not a
list kept current while shown."
  (let ((mode (buffer-major-mode buffer)))
    (find-if (lambda (kind) (typep mode (first kind))) *shown-lists*)))

(defun set-made-text (buffer text &key spans buttons (key nil key-p) refill)
  "Make TEXT the whole of BUFFER, a read-only buffer whose text the editor
makes: read-only again however the change ends, unmodified after (so that
killing it asks nothing), with nothing to undo. SPANS, each (START END FACE) in
octets of TEXT, a list or a vector, are the parts of it shown in a face, found
by special-mode's highlight method (MADE-SPANS-BETWEEN); BUTTONS, each (START
END ACTION), are its buttons (\\[push-button]); KEY is what the list was made
from, for KEEP-SHOWN-CURRENT, worked out by its kind's key function when not
given; REFILL, when given, is how BUFFER is made again (its :REFILL local,
which \\[revert-buffer] calls with BUFFER). A narrowed BUFFER is widened
first. Point is left where the change leaves it. O(size of TEXT + number of SPANS)."
  (tuatara.buffer:widen-buffer buffer)
  (tuatara.buffer:with-read-only-lifted (buffer :after t)
    (tuatara.buffer:buffer-replace buffer 0 (tuatara.buffer:buffer-size buffer) text))
  (let ((spans (coerce spans 'simple-vector)))
    (unless (loop for i from 1 below (length spans)
                  always (<= (first (svref spans (1- i))) (first (svref spans i))))
      (setf spans (stable-sort (copy-seq spans) #'< :key #'first)))
    (setf (tuatara.buffer:buffer-local buffer 'made-spans)
          (and (plusp (length spans)) (cons spans (span-ends spans)))))
  (setf (tuatara.buffer:buffer-local buffer :buttons) buttons
        (tuatara.buffer:buffer-local buffer 'list-key)
        (if key-p key (let ((kind (shown-list-kind buffer))) (and kind (funcall (second kind) buffer))))
        (tuatara.buffer:buffer-undo-stack buffer) '()
        (tuatara.buffer:buffer-redo-stack buffer) '())
  (when refill
    (setf (tuatara.buffer:buffer-local buffer :refill) refill))
  (tuatara.buffer:buffer-mark-saved buffer)
  buffer)

(defun made-spans-between (buffer from to)
  "The parts of BUFFER's made text (SET-MADE-TEXT) shown in a face that reach
between the octets FROM and TO, each (START END FACE), in the order of their
starts; the first found by halves, so O(log n + those found)."
  (let ((made (tuatara.buffer:buffer-local buffer 'made-spans)))
    (when made
      (destructuring-bind (spans . ends) made
        (let ((low 0) (high (length spans)))
          (setf low (tuatara.runtime:bisect (middle low high) (<= (aref ends middle) from)) high low)
          (loop for i from low below (length spans)
                for span = (svref spans i)
                while (< (first span) to)
                when (> (second span) from) collect span))))))

(defun made-spans-shown (buffer rows)
  "The parts of BUFFER's made text shown in a face within ROWS, the rows laid
out for a window, what a fold or a search filter hides between them left out
(MADE-SPANS-BETWEEN)."
  (loop for (from . to) in (tuatara.display:shown-ranges rows :buffer buffer)
        nconc (made-spans-between buffer from to)))

(defmethod mode-syntax-highlights ((mode special-mode) buffer rows)
  "Colour the text a command made in BUFFER as it was made, else as its mode
colours text."
  (if (tuatara.buffer:buffer-local buffer 'made-spans)
      (made-spans-shown buffer rows)
      (call-next-method)))

(defun piece-parts (piece)
  "PIECE, as PIECES-TEXT takes it, as three values: its text, its face and its action."
  (cond ((stringp piece) (values piece nil nil))
        ((listp (cdr piece)) (values (first piece) (second piece) (third piece)))
        (t (values (car piece) (cdr piece) nil))))

(defun pieces-text (pieces &key keys)
  "PIECES made one text, as three values: the text; the parts of it shown in a
face, each (START END FACE) in octets, in order; and its buttons, each (START
END ACTION). A piece is a string, (TEXT . FACE), or (TEXT FACE ACTION), where
FACE shows TEXT when there is one and ACTION, a function of no arguments, makes
TEXT a button. With KEYS, the keys SUBSTITUTE-COMMAND-KEYS marked in the
pieces' texts are unmarked and shown in :help-key-binding, over the piece's
face. O(size of the text)."
  (let ((out (make-string-output-stream)) (spans '()) (buttons '()) (octets 0))
    (dolist (piece pieces)
      (multiple-value-bind (text face action) (piece-parts piece)
        (multiple-value-bind (plain marked) (if keys (unmark-keys text) (values text nil))
          (let ((end (+ octets (tuatara.text:string-octet-length plain))))
            (write-string plain out)
            (when (and face (< octets end)) (push (list octets end face) spans))
            (let ((char 0) (octet octets))
              (loop for (start stop key-face) in marked
                    do (incf octet (tuatara.text:string-octet-length plain :start char :end start))
                       (let ((after (+ octet (tuatara.text:string-octet-length plain :start start :end stop))))
                         (push (list octet after key-face) spans)
                         (setf char stop octet after))))
            (when (and action (< octets end)) (push (list octets end action) buttons))
            (setf octets end)))))
    (values (get-output-stream-string out) (nreverse spans) (nreverse buttons))))

(defun list-entry-at (&optional (buffer (current-buffer)) (position (tuatara.buffer:buffer-point buffer)))
  "The entry RENDER-PIECES was given for the line of BUFFER (the current
buffer when not given) holding POSITION (point when not given), or NIL on a
line that shows none (titles, a summary). A buffer whose text grows as it
comes, a compile's output, finds its entries itself: its :ENTRY-AT local, a
function of the buffer and the position, gives the entry there."
  (let ((finder (tuatara.buffer:buffer-local buffer :entry-at)))
    (if finder
        (funcall finder buffer position)
        (let ((entries (tuatara.buffer:buffer-local buffer 'list-entries))
              (index (- (tuatara.buffer:buffer-line-number buffer position)
                        (tuatara.buffer:buffer-local buffer 'list-first-line 0))))
          (and (< -1 index (length entries)) (elt entries index))))))

(defun render-pieces (buffer pieces &key (point :keep) (entries nil entries-p) (first-line 0) keys (key nil key-p) refill
                                         redraw mark-key)
  "Make PIECES (as PIECES-TEXT joins them, the keys marked in them shown with
KEYS) the text of BUFFER, read-only (SET-MADE-TEXT, with KEY and REFILL), and
put point back. PIECES may be a function of no arguments making them: it is
called, and called again when the text is made again (\\[revert-buffer]), unless
REFILL says otherwise. REDRAW, a function of BUFFER, draws it again from what
it holds, when its marks or folds change, where making it again (REFILL) would
read the disk (REDRAW-LIST). ENTRIES, a list or a vector, are what the lines
from line FIRST-LINE on show, one each, for LIST-ENTRY-AT; MARK-KEY, a
function of an entry, gives what a mark on it is kept by (ENTRY-MARK-KEY), NIL
for an entry that takes none. POINT :keep keeps point on the entry it was on
while that is still listed (an entry made again found by its likeness), else
on its line, at the same column as far as the line goes (point on an entry
gone stays among the entries' lines); :line does the same at the line's
start; :start puts it at the start, :button on the first button (else the
start), a number at the start of that line, one of the entries' lines when
there are ENTRIES. Each window showing BUFFER keeps its own point so too, and
shows it from the same line as before. Returns BUFFER."
  (when (functionp pieces)
    (let ((make pieces))
      (setf pieces (funcall make))
      (unless refill (setf refill (lambda (buffer) (render-pieces buffer make :keys keys))))))
  (multiple-value-bind (text spans buttons) (pieces-text pieces :keys keys)
    (apply #'show-list-text buffer text spans buttons :point point :first-line first-line :refill refill
           :redraw redraw :mark-key mark-key
           (append (and entries-p (list :entries entries)) (and key-p (list :key key))))))

(defun show-list-text (buffer text spans buttons &key (point :keep) (entries nil entries-p) (first-line 0)
                                                    (key nil key-p) refill redraw mark-key (capture t))
  "Make TEXT, with its SPANS and BUTTONS as PIECES-TEXT gives them, the text
of BUFFER, as RENDER-PIECES does with the rest of its arguments. A list
narrowed by search filters shows only the entries they leave
\(FILTER-LIST-TEXT), TEXT kept as the whole list to narrow again unless
CAPTURE is NIL, when it is that whole list being narrowed again. Returns BUFFER."
  (when entries-p
    (multiple-value-setq (text spans buttons entries first-line)
      (filter-list-text buffer text spans buttons entries first-line capture))
    ;; kept as a vector, so that the entry on a line is found at once
    (setf entries (coerce entries 'simple-vector)))
  (let* ((old-entries (tuatara.buffer:buffer-local buffer 'list-entries :none))
         (old-first (tuatara.buffer:buffer-local buffer 'list-first-line 0))
         (windows (and *editor* (buffer-windows buffer)))
         (own-points (remove-if-not #'tuatara.display:window-point-anchor windows)))
    (labels ((line-of (position) (ignore-errors (tuatara.buffer:buffer-line-number buffer position)))
             (place (position)
               ;; where POSITION is: its line, its column, and the index among ENTRIES of its entry
               (let ((line (line-of position)))
                 (list line
                       (if (and line (eq point :keep)) (- position (tuatara.buffer:buffer-line-start buffer position)) 0)
                       (and entries-p (member point '(:keep :line)) line (not (eq old-entries :none))
                            ;; an entry made again is found by its likeness, the one on the same line first
                            (let ((entry (list-entry-at buffer position))
                                  (index (- line old-first)))
                              (and entry
                                   (if (and (< -1 index (length entries)) (equal entry (elt entries index)))
                                       index
                                       (position entry entries :test #'equal))))))))
             (on-entry-p (line)
               (or (eq old-entries :none) (null line)
                   (and (<= old-first line) (< line (+ old-first (length old-entries)))))))
      (let ((point-place (place (tuatara.buffer:buffer-point buffer)))
            (window-places (mapcar (lambda (window) (place (tuatara.display:window-point window))) own-points))
            (start-lines (mapcar (lambda (window) (line-of (tuatara.display:window-start window))) windows)))
        (apply #'set-made-text buffer text :spans spans :buttons buttons :refill refill (and key-p (list :key key)))
        (when entries-p
          (setf (tuatara.buffer:buffer-local buffer 'list-entries) entries
                (tuatara.buffer:buffer-local buffer 'list-first-line) first-line))
        (when mark-key
          (setf (tuatara.buffer:buffer-local buffer 'list-mark-key) mark-key))
        (when redraw
          (setf (tuatara.buffer:buffer-local buffer 'list-redraw) redraw))
        (labels ((among-entries (line) (max first-line (min line (+ first-line (max 0 (1- (length entries)))))))
                 (within-text (line) (min line (max 0 (1- (tuatara.buffer:buffer-line-count buffer)))))
                 (at-column (line column)
                   (let ((start (tuatara.buffer:buffer-goto-line buffer line)))
                     (min (+ start column) (tuatara.buffer:buffer-line-end buffer start))))
                 (put-back (place)
                   (destructuring-bind (line column found) place
                     (cond ((eq point :start) 0)
                           ((eq point :button) (let ((first (first buttons))) (if first (first first) 0)))
                           ((integerp point)
                            (tuatara.buffer:buffer-goto-line buffer (if entries-p (among-entries point) (within-text point))))
                           (found (at-column (+ first-line found) column))
                           ((and entries-p (on-entry-p line)) (at-column (among-entries (or line first-line)) column))
                           (t (at-column (within-text (or line 0)) column))))))
          (setf (tuatara.buffer:buffer-point buffer) (put-back point-place))
          (loop for window in own-points
                for place in window-places
                do (setf (tuatara.display:window-point window) (put-back place)))
          (loop for window in windows
                for line in start-lines
                do (setf (tuatara.display:window-start window)
                         (tuatara.buffer:buffer-goto-line buffer (within-text (or line 0))))))))
    buffer))

;;; Marks

(define-major-mode marked-list-mode special-mode
  "A list whose entries can be marked, for its commands to act on them
together, as the buffer list (\\[buffers]) and the bookmark manager
\(\\[bookmark-manager]) are: \\<marked-list-mode>\\[list-mark-entry] marks the entry on the line
\(>), \\[list-mark-for-deletion] marks it for deletion (D), \\[list-unmark-entry] takes its mark off,
each then moving down, a count marking as many; \\[list-unmark-all] takes every mark
off; \\[list-toggle-marks] marks those not marked and unmarks those marked, as in rimu;
\\[list-delete-marked] deletes those marked, or the one on the line, \\[list-execute] those marked for
deletion, after asking."
  :lighter "List" :interactive nil)

(defvar *list-mark-order* 0
  "How many marks have been given in the lists, the order of each mark kept
with it (LIST-MARK-TABLE).")

(defun list-mark-table (list)
  "The marks on the entries of LIST, a list made by RENDER-PIECES, NIL while
it has none: a table from what the list's mark key gives of an entry
\(ENTRY-MARK-KEY) to (MARK . ORDER), MARK :MARKED (>) or :DELETION (D), ORDER
saying which marks were given first."
  (tuatara.buffer:buffer-local list 'list-marks))

(defun list-marks (list)
  "The marks on the entries of LIST, each (KEY . MARK): KEY what the list's
mark key gives of the entry (ENTRY-MARK-KEY), MARK :MARKED (>) or :DELETION
\(D); the last marked first."
  (let ((table (list-mark-table list)) (marks '()))
    (when table
      (maphash (lambda (key value) (push (list* (cdr value) key (car value)) marks)) table))
    (mapcar #'cdr (sort marks #'> :key #'car))))

(defun entry-mark (list key)
  "The mark of the entry of LIST known by KEY: :MARKED, :DELETION or NIL."
  (let ((table (list-mark-table list)))
    (and table (car (gethash key table)))))

(defun list-marked (list mark)
  "The keys of the entries of LIST marked MARK (:MARKED or :DELETION), the
first marked first."
  (reverse (loop for (key . m) in (list-marks list) when (eq m mark) collect key)))

(defun set-entry-mark (list key mark)
  "Give the entry of LIST known by KEY the MARK :MARKED or :DELETION, in place
of the one it had, or with NIL none."
  (let ((table (or (list-mark-table list)
                   (setf (tuatara.buffer:buffer-local list 'list-marks) (make-hash-table :test 'equal)))))
    (if mark
        (setf (gethash key table) (cons mark (incf *list-mark-order*)))
        (remhash key table))))

(defun keep-list-marks (list keys)
  "Keep only the marks of LIST on entries still listed, KEYS being theirs."
  (let ((table (list-mark-table list)))
    (when (and table (plusp (hash-table-count table)))
      (let ((listed (make-hash-table :test 'equal)))
        (dolist (key keys) (setf (gethash key listed) t))
        (maphash (lambda (key value)
                   (declare (ignore value))
                   (unless (gethash key listed) (remhash key table)))
                 table)))))

(defun list-mark-sign (mark)
  "What a list shows for MARK in its mark column: > marked (:marked), D for
deletion (:deletion), a mark that is a character (as rimu keeps them, M a
master) that character, else a space."
  (cond ((eq mark :marked) ">")
        ((eq mark :deletion) "D")
        ((characterp mark) (string mark))
        (t " ")))

(defun list-mark-face (mark)
  "The face of an entry's name in a list for MARK: :list-marked for :marked
or #\\>, :list-deletion for :deletion or #\\D, :list-marked-other for any other
character (a mark of another kind, as rimu's M of a master); NIL for no mark."
  (case mark
    ((:marked #\>) :list-marked)
    ((:deletion #\D) :list-deletion)
    (t (and (characterp mark) :list-marked-other))))

(defun entry-mark-key (list entry)
  "What a mark on ENTRY of LIST is kept by: what the list's mark key gives of
it (RENDER-PIECES' MARK-KEY), else ENTRY itself; NIL for no entry, or one
that takes no mark (a section's heading)."
  (and entry (funcall (tuatara.buffer:buffer-local list 'list-mark-key #'identity) entry)))

(defun list-line-after (list position count)
  "The start of the line COUNT lines after the one of LIST holding POSITION
\(before it for a negative COUNT), as far as the text goes."
  (let ((line (+ (tuatara.buffer:buffer-line-number list position) count)))
    (tuatara.buffer:buffer-goto-line list (max 0 (min line (1- (tuatara.buffer:buffer-line-count list)))))))

(defgeneric mark-entries (mode list mark count)
  (:documentation "Give the entry on the current line of LIST, a buffer in MODE, MARK
\(:MARKED, :DELETION, or NIL for none) in place of the one it had, and as many
as COUNT says, each moving down a line, or with a negative COUNT up a line
first: what \\[list-mark-entry], \\[list-mark-for-deletion] and \\[list-unmark-entry] do in that list.")
  (:method ((mode marked-list-mode) list mark count)
    "A line showing no entry, or one that takes no mark, is passed over; the
list is drawn again once (REDRAW-LIST)."
    (loop repeat (abs count)
          do (when (minusp count)
               (setf (tuatara.buffer:buffer-point list) (list-line-after list (tuatara.buffer:buffer-point list) -1)))
             (let ((key (entry-mark-key list (list-entry-at list))))
               (when key (set-entry-mark list key mark)))
             (when (plusp count)
               (setf (tuatara.buffer:buffer-point list) (list-line-after list (tuatara.buffer:buffer-point list) 1))))
    (redraw-list list)))

(defgeneric unmark-entries (mode list)
  (:documentation "Take every mark off the entries of LIST, a buffer in MODE, as
\\[list-unmark-all] does there. Returns how many marks were taken off.")
  (:method ((mode marked-list-mode) list)
    "Every mark, > and D, the list drawn again."
    (let* ((table (list-mark-table list)) (count (if table (hash-table-count table) 0)))
      (unmark-list list)
      count)))

(defgeneric swap-entry-marks (mode list)
  (:documentation "Mark the entries LIST, a buffer in MODE, shows that are not marked and
take the mark off those marked, as \\[list-toggle-marks] does there; those marked
for deletion, or given a mark of another kind, stay so. Returns how many are
marked now.")
  (:method ((mode marked-list-mode) list)
    "The entries a filter or a folded section hides stay as they are."
    (let ((seen (make-hash-table :test 'equal)) (marked 0))
      (map nil (lambda (entry)
                 (let ((key (entry-mark-key list entry)))
                   (when (and key (not (gethash key seen)))
                     (setf (gethash key seen) t)
                     (case (entry-mark list key)
                       (:marked (set-entry-mark list key nil))
                       ((nil) (set-entry-mark list key :marked) (incf marked))))))
           (tuatara.buffer:buffer-local list 'list-entries))
      (redraw-list list)
      marked)))

(defun unmark-list (list)
  "Take every mark off the entries of LIST and draw it again."
  (let ((table (list-mark-table list)))
    (when table (clrhash table)))
  (redraw-list list))

(defun list-acted-on (list)
  "What a command for the marked entries of LIST acts on: the keys of those
marked (>), the first marked first, or with none marked the key of the entry
on the current line; NIL on a line showing none, or one that takes no mark."
  (or (list-marked list :marked)
      (let ((key (entry-mark-key list (list-entry-at list)))) (and key (list key)))))

(define-command list-mark-entry ()
  "Mark the entry on the current line of the list (>), for the list's commands that act on those marked, and move down; with
a count, as many entries, up the list for a negative one."
  (mark-entries (buffer-major-mode (current-buffer)) (current-buffer) :marked (prefix)))

(define-command list-mark-for-deletion ()
  "Mark the entry on the current line of the list for deletion (D), for the
list's command that deletes them, and move down; with a count, as many
entries, up the list for a negative one."
  (mark-entries (buffer-major-mode (current-buffer)) (current-buffer) :deletion (prefix)))

(define-command list-unmark-entry ()
  "Take the mark off the entry on the current line of the list, of any kind,
and move down; with a count, off as many entries, up the list for a negative
one."
  (mark-entries (buffer-major-mode (current-buffer)) (current-buffer) nil (prefix)))

(define-command list-toggle-marks ()
  "Mark the entries the list shows that are not marked and take the mark off
those marked; those marked for deletion (D), or given a mark of another kind,
stay so, as do the entries a filter or a folded section hides."
  (message "~d marked" (swap-entry-marks (buffer-major-mode (current-buffer)) (current-buffer))))

(define-command list-unmark-all ()
  "Take every mark off the entries of the list, of any kind; in rimu, those
of the files marked in other directories too."
  (message "~d mark~:p taken away" (unmark-entries (buffer-major-mode (current-buffer)) (current-buffer))))

(defvar *list-hidden-count* 0
  "How many of the entries a list command acts on the list's search filters
hide, while it runs (ACT-ON-LIST); its question says so (LIST-QUESTION).")

(defun list-hidden-count (list keys)
  "How many of the entries of LIST known by KEYS (ENTRY-MARK-KEY) its search
filters hide (KEYS-HIDDEN-BY-FILTERS): 0 when it is not narrowed."
  (length (keys-hidden-by-filters list keys)))

(defun act-on-list (list act noun &key deletion)
  "Call ACT with LIST and the keys of what a command for its marked entries
acts on: with DELETION, those marked for deletion (D); else those marked (>),
or with none marked the entry on the current line (LIST-ACTED-ON); how many
of them the list's search filters hide is *LIST-HIDDEN-COUNT* meanwhile. With
none, say so, NOUN naming an entry (\"bookmark\"): no NOUNs marked for deletion, or
no NOUN on this line."
  (let* ((keys (if deletion (list-marked list :deletion) (list-acted-on list)))
         (*list-hidden-count* (list-hidden-count list keys)))
    (cond (keys (funcall act list keys))
          (deletion (message "No ~as marked for deletion" noun))
          (t (message "No ~a on this line" noun)))))

(defun list-question (verb noun keys &key (name-of #'identity) where after (the t))
  "What a list asks before VERB (\"Delete\") acts on its entries known by KEYS,
NOUN naming one, without the question mark: VERB the NOUN and its name (what
NAME-OF gives of the key) for one, VERB the N NOUNs for more; WHERE, words
saying where they are (\"in 2 directories\"), and AFTER, when given, then.
With THE NIL, the name of one alone (VERB NAME), and N NOUNs for more; with
VERB NIL, the words for the entries alone. Those of them the list's search
filters hide are said (*LIST-HIDDEN-COUNT*): the 4 buffers (2 hidden by the
filter)."
  (format nil "~@[~a ~]~a~[~:; (~:*~d hidden by the filter)~]~@[ ~a~]~@[ ~a~]"
          verb
          (if (rest keys)
              (format nil "~:[~;the ~]~d ~as" the (length keys) noun)
              (format nil "~:[~*~;the ~a ~]~a" the noun (funcall name-of (first keys))))
          (min *list-hidden-count* (length keys))
          where after))

(defun list-entries-of (list keys)
  "The entries LIST shows whose mark keys (ENTRY-MARK-KEY) are among KEYS, in
the order shown."
  (let ((wanted (make-hash-table :test 'equal)))
    (dolist (key keys) (setf (gethash key wanted) t))
    (remove-if-not (lambda (entry) (gethash (entry-mark-key list entry) wanted))
                   (coerce (tuatara.buffer:buffer-local list 'list-entries #()) 'list))))

;;; Deleting and opening entries

(defgeneric list-entry-noun (mode)
  (:documentation "What one entry of a list in major mode MODE is called in what the
list says (\"buffer\", \"bookmark\"): no buffer on this line, no buffers marked
for deletion.")
  (:method (mode) (declare (ignore mode)) "entry"))

(defgeneric delete-entries (mode list keys mark)
  (:documentation "Delete the entries of LIST, a buffer in MODE, known by KEYS (what its
mark key gives of them, ENTRY-MARK-KEY), after asking: with MARK :DELETION
those marked for deletion (D), as \\[list-execute] does there, with :MARKED those
marked (>) or the one on the line, as \\[list-delete-marked] does.")
  (:method (mode list keys mark)
    "A list that deletes nothing says so."
    (declare (ignore mode list keys mark))
    (message "Nothing is deleted from this list")))

(defgeneric delete-list-entries (mode list mark)
  (:documentation "What \\[list-execute] (MARK :DELETION) and \\[list-delete-marked] (MARK :MARKED) do in
LIST, a buffer in MODE.")
  (:method ((mode marked-list-mode) list mark)
    "Those marked for deletion (D), or those marked (>) or with none marked the
one on the line (ACT-ON-LIST), deleted as the list deletes (DELETE-ENTRIES);
the preview beside the list then shows the entry on its line anew."
    (act-on-list list (lambda (list keys) (delete-entries mode list keys mark)) (list-entry-noun mode)
                 :deletion (eq mark :deletion))
    (entry-preview-again)))

(define-command list-execute ()
  "Delete the entries of the list marked for deletion (D), after asking, as the
list deletes: the buffer list kills its buffers, rimu puts files in the trash,
the list of jobs drops those waiting and stops those running."
  (delete-list-entries (buffer-major-mode (current-buffer)) (current-buffer) :deletion))

(define-command list-delete-marked ()
  "Delete the marked entries of the list (>), or with none marked the one on
the current line, after asking, as the list deletes, as \\[list-execute] does those
marked for deletion; in rimu, with \\[universal-argument], for good rather than to the trash."
  (delete-list-entries (buffer-major-mode (current-buffer)) (current-buffer) :marked))

(defgeneric visit-entry (mode list entry other)
  (:documentation "Open ENTRY of LIST, a buffer in MODE, what its line shows: the buffer,
the bookmark's place, the file; with OTHER in another window. What
\\[list-visit] and \\[list-visit-other] do there; in a list in sections, ENTRY is a member of
a section (ENTRY-MEMBER).")
  (:method (mode list entry other)
    "A list whose entries open nothing says so."
    (declare (ignore mode list entry other))
    (message "Nothing to open on this line")))

(defun visit-entry-at (other)
  "Open the entry on the current line of the list (VISIT-ENTRY), in another
window with OTHER; on a section's heading, fold or unfold the section."
  (let* ((list (current-buffer))
         (mode (buffer-major-mode list))
         (entry (list-entry-at list))
         (sectioned (typep mode 'sectioned-list-mode))
         (shown (if sectioned (entry-member entry) entry)))
    (cond ((and sectioned (eq (first entry) :section)) (toggle-list-section list (second entry)))
          (shown (visit-entry mode list shown other))
          (t (message "No ~a on this line" (list-entry-noun mode))))))

(define-command list-visit ()
  "Open the entry on the current line of the list: the buffer, the bookmark's
place, the file; on a section's heading, fold or unfold the section."
  (visit-entry-at nil))

(define-command list-visit-other ()
  "Open the entry on the current line of the list in another window; on a
section's heading, fold or unfold the section."
  (visit-entry-at t))

;;; Sections

(define-major-mode sectioned-list-mode marked-list-mode
  "A list whose entries are in sections, each under a heading saying how many
it holds, as the buffer list (\\[buffers]) and the bookmark manager
\(\\[bookmark-manager]) are: ▾ before an open section's heading, ▸ before a
folded one, its entries hidden. \\<sectioned-list-mode>\\[list-toggle-fold] folds or unfolds the
section of the current line, \\[list-toggle-all-folds] all of them; the entries are marked as
in any list of marked entries."
  :lighter "List" :interactive nil)

(defun list-folded (list)
  "The sections folded in LIST, a list in sections (SECTIONED-PIECES)."
  (tuatara.buffer:buffer-local list 'list-folded '()))

(defun group-sections (items key &key first last order)
  "ITEMS in sections, each (SECTION . ITEMS): SECTION what KEY, a function of
an item, gives of it, items with EQUAL keys in one section, in their order in
ITEMS. The sections come in the order of their first items, or as ORDER, a
function of two sections, sorts them, when given; the section FIRST, when
there is one, before them all, and the section LAST after them. O(items)."
  (let ((table (make-hash-table :test 'equal)) (sections '()))
    (dolist (item items)
      (let* ((section (funcall key item))
             (place (gethash section table)))
        (if place
            (push item (cdr place))
            (push (setf (gethash section table) (list section item)) sections))))
    (let* ((sections (mapcar (lambda (place) (cons (car place) (reverse (cdr place)))) (nreverse sections)))
           (others (remove-if (lambda (place) (or (and first (equal (car place) first)) (and last (equal (car place) last))))
                              sections)))
      (flet ((alone (section) (let ((place (and section (assoc section sections :test #'equal)))) (and place (list place)))))
        (append (alone first)
                (if order (stable-sort others order :key #'car) others)
                (alone last))))))

(defun sectioned-pieces (list sections title row &key count after)
  "The lines of LIST for SECTIONS, each (SECTION . MEMBERS), as two values:
their entries, for RENDER-PIECES, and their pieces. Each section has a
heading: ▾, or ▸ when it is folded, its title (TITLE, a function of the
section) and how many it holds in brackets, all in :heading, its entry
\(:SECTION SECTION); how many is the number of its members, or what COUNT, a
function of the section and its members, gives, no brackets when that is
NIL. Unless the section is folded, each member has a line under it (folded
too while search filters narrow LIST, which then hide it), ROW (a
function of the member and the section) giving its pieces, its entry
\(:MEMBER MEMBER SECTION); for a member shown on several lines, or on lines
with entries of their own, ROW gives as a second value their entries, a
sequence of one for each line. AFTER, a function of the section and its
members, gives lines following the members of an open section the same way,
their pieces and their entries (\"... 3 more\"). An empty line follows each
open section another comes after. The sections are kept for
\\[list-toggle-all-folds]."
  (setf (tuatara.buffer:buffer-local list 'list-sections) (mapcar #'car sections))
  (let ((folded (list-folded list)) (entries '()) (pieces '()))
    (flet ((add (&optional more-pieces more-entries)
             (dolist (piece more-pieces) (push piece pieces))
             (map nil (lambda (entry) (push entry entries)) more-entries)))
      (loop for (section . members) in sections
            for open-before = nil then open
            for open = (not (member section folded :test #'equal))
            for number = (if count (funcall count section members) (length members))
            do (when open-before
                 (push nil entries)
                 (push (string #\Newline) pieces))
               (push (list :section section) entries)
               (push (cons (format nil "~:[▸~;▾~] ~a~@[ (~:d)~]" open (escape-controls (funcall title section)) number)
                           :heading)
                     pieces)
               (push (string #\Newline) pieces)
               (when (or open (list-filters-on-p list))
                 (dolist (member members)
                   (multiple-value-bind (row-pieces row-entries) (funcall row member section)
                     (add row-pieces (or row-entries (list (list :member member section))))))
                 (when after
                   (multiple-value-call #'add (funcall after section members))))))
    (values (nreverse entries) (nreverse pieces))))

(defun entry-lines (function)
  "Call FUNCTION with a function of a line's TEXT and, as it chooses, its
FACE and its ENTRY, which adds that line; TEXT may also be a list of pieces,
each a string or (STRING . FACE), the line made of them in their faces. The
lines added, as pieces for RENDER-PIECES, and their entries, a vector, as two
values."
  (let ((pieces '()) (entries '()))
    (funcall function (lambda (text &optional face entry)
                        (if (listp text)
                            (progn (dolist (piece text) (push piece pieces))
                                   (push (string #\Newline) pieces))
                            (let ((text (format nil "~a~%" text)))
                              (push (if face (cons text face) text) pieces)))
                        (push entry entries)))
    (values (nreverse pieces) (coerce (nreverse entries) 'vector))))

(defmacro collecting-pieces ((put) &body body)
  "Run BODY with PUT, a local function of a TEXT and, as it chooses, its FACE
and its ACTION (a function of no arguments that makes TEXT a button), which
adds that piece; TEXT may also be a piece already made, added as it is. The
pieces added, in order, as RENDER-PIECES and PIECES-TEXT take them."
  (let ((pieces (gensym "PIECES")))
    `(let ((,pieces '()))
       (flet ((,put (text &optional face action)
                (push (cond ((consp text) text) (action (list text face action)) (face (cons text face)) (t text)) ,pieces)))
         (declare (ignorable #',put))
         ,@body)
       (nreverse ,pieces))))

(defun entry-member (entry)
  "The member ENTRY, a line of a list in sections (SECTIONED-PIECES), shows;
NIL for a heading's or another line's."
  (and (eq (first entry) :member) (second entry)))

(defun list-member-at (list)
  "The member of a section on the current line of LIST (SECTIONED-PIECES), or
NIL on a heading or another line."
  (entry-member (list-entry-at list)))

(defun list-section-at (list)
  "The section the current line of LIST (SECTIONED-PIECES) is the heading of
or one of the lines under; NIL on another line."
  (let ((entry (list-entry-at list)))
    (case (and (consp entry) (first entry))
      (:section (second entry))
      (:member (third entry))
      ((nil) nil)
      (t ;; a line with an entry of its own kind belongs to the heading above it
       (let ((entries (coerce (tuatara.buffer:buffer-local list 'list-entries) 'vector)))
         (loop for index downfrom (- (tuatara.buffer:buffer-line-number list (tuatara.buffer:buffer-point list))
                                     (tuatara.buffer:buffer-local list 'list-first-line 0))
                 to 0
               for entry = (aref entries index)
               do (cond ((null entry) (return nil))
                        ((and (consp entry) (eq (first entry) :section)) (return (second entry)))
                        ((and (consp entry) (eq (first entry) :member)) (return (third entry))))))))))

(defun list-goto-entry (list entry)
  "Put point of LIST at the start of the line showing ENTRY (EQUAL to one
RENDER-PIECES was given), when one shows it."
  (let ((index (position entry (tuatara.buffer:buffer-local list 'list-entries) :test #'equal)))
    (when index
      (setf (tuatara.buffer:buffer-point list)
            (tuatara.buffer:buffer-goto-line list (+ index (tuatara.buffer:buffer-local list 'list-first-line 0)))))))

(defun fold-list-sections (list folded)
  "Fold the sections FOLDED of LIST, the others open, and make it again; when
the section point was in is folded, point goes to its heading."
  (let ((section (list-section-at list)))
    (setf (tuatara.buffer:buffer-local list 'list-folded) folded)
    (redraw-list list)
    (when (and section (member section folded :test #'equal))
      (list-goto-entry list (list :section section)))))

(defun toggle-list-section (list section)
  "Fold SECTION of LIST, or unfold it when it is folded."
  (let ((folded (list-folded list)))
    (fold-list-sections list (if (member section folded :test #'equal)
                                 (remove section folded :test #'equal)
                                 (cons section folded)))))

(defun unfold-list-sections (list sections)
  "Unfold those of SECTIONS of LIST that are folded, so that what a command
acts on shows."
  (let ((folded (list-folded list)))
    (when (intersection sections folded :test #'equal)
      (fold-list-sections list (remove-if (lambda (section) (member section sections :test #'equal)) folded)))))

(define-command list-toggle-fold ()
  "Fold the section of the list the current line is in, its heading left
showing, or unfold it when it is folded."
  (let ((section (list-section-at (current-buffer))))
    (when section (toggle-list-section (current-buffer) section))))

(define-command list-toggle-all-folds ()
  "Fold every section of the list, or unfold them all when every one is
folded; a section not in the list now keeps its fold for when it comes back."
  (let* ((list (current-buffer))
         (sections (tuatara.buffer:buffer-local list 'list-sections))
         (folded (list-folded list)))
    (fold-list-sections list (if (subsetp sections folded :test #'equal)
                                 (set-difference folded sections :test #'equal)
                                 (union folded sections :test #'equal)))))

;;; Columns

(defun column-text (text width &key right (elide t))
  "TEXT in a column WIDTH columns wide: padded with spaces after it, or before
it when RIGHT; when it is wider, cut, with ... for its last three, or left
whole unless ELIDE."
  (let ((columns (text-columns text)))
    (cond ((and (> columns width) (not elide)) text)
          ((> columns width)
           (let ((cut (values (tuatara.display:fit-columns text '() width :ellipsis "..."))))
             (concatenate 'string cut (make-string (max 0 (- width (text-columns cut))) :initial-element #\Space))))
          (right (concatenate 'string (make-string (- width columns) :initial-element #\Space) text))
          (t (concatenate 'string text (make-string (- width columns) :initial-element #\Space))))))

(defun table-list-cell-text (cell)
  "The text of CELL, a string or (TEXT . FACE) as TABLE-ROW-PIECES takes it."
  (if (consp cell) (car cell) cell))

(defun kept-row-pieces (columns row-cells key texts)
  "The pieces of a row of ROW-CELLS under COLUMNS (TABLE-ROW-PIECES), the
texts of its cells kept in TEXTS under KEY, the entry the row is about, unless
KEY is NIL; the texts as a second value. Touches no list: a job's thread may
make rows."
  (let ((cell-texts (map 'simple-vector #'table-list-cell-text row-cells)))
    (when key (setf (gethash key texts) cell-texts))
    (values (table-row-pieces columns row-cells) cell-texts)))

(defun table-list-rows (columns items cells &key (key #'identity))
  "The rows a table list shows of ITEMS under COLUMNS, made apart from any
list, so on a job's thread too, for RENDER-MADE-TABLE-LIST: CELLS, a function
of an item, gives its cells, KEY the entry its line is about, NIL for a line
about none. Three values: the pieces of the rows, a table from each entry to
the texts of its cells, and the first row's texts."
  (let ((texts (make-hash-table :test 'eq)) (first nil))
    (values (loop for item in items
                  append (multiple-value-bind (pieces cell-texts)
                             (kept-row-pieces columns (funcall cells item) (funcall key item) texts)
                           (unless first (setf first cell-texts))
                           pieces))
            texts first)))

(defun render-made-table-list (list columns rows texts first &rest options &key before after first-line &allow-other-keys)
  "Fill LIST with a table whose ROWS are made (TABLE-LIST-ROWS, TEXTS and
FIRST as it gives them): the titles of COLUMNS over them, BEFORE and AFTER,
pieces, above the titles and below the rows; the columns kept for a filter to
try a pattern on one (KEEP-LIST-TABLE). The other OPTIONS go to
RENDER-PIECES, FIRST-LINE counted from the titles when not given."
  (keep-list-table list (map 'simple-vector (lambda (column) (string-trim " " (first column))) columns) texts first)
  (apply #'render-pieces list
         (append before (table-heading-pieces columns) rows after)
         :first-line (or first-line (+ 2 (count #\Newline (pieces-text before))))
         (loop for (name value) on options by #'cddr
               unless (member name '(:before :after :first-line))
                 append (list name value))))

(defun render-table-list (list columns items cells
                          &rest options
                          &key (marked-by (lambda (item section) (declare (ignore section)) item))
                               sections section-title (keep-marks t)
                          &allow-other-keys)
  "Fill LIST with a table (TABLE-HEADING-PIECES): the titles of COLUMNS over
a row for each of ITEMS, its cells what CELLS, a function of the item, its
mark (ENTRY-MARK) and its section, gives (TABLE-ROW-PIECES). With SECTIONS,
ITEMS are sections, each (SECTION . MEMBERS), under headings that fold
\(SECTIONED-PIECES, SECTION-TITLE naming each). MARKED-BY, a function of an
item and its section, gives what its mark is kept by; with KEEP-MARKS, the marks of
items no longer listed are dropped. The other OPTIONS go to
RENDER-MADE-TABLE-LIST: BEFORE and AFTER, pieces above the titles and below
the rows, and those of RENDER-PIECES. Each row's cells are kept as text, so
that \\[list-show-records] can try a pattern on one column."
  (let ((texts (make-hash-table :test 'eq)) (first nil))
    (flet ((row (item section)
             (multiple-value-bind (pieces cell-texts)
                 (kept-row-pieces columns (funcall cells item (entry-mark list (funcall marked-by item section)) section) item texts)
               (unless first (setf first cell-texts))
               pieces)))
      (when keep-marks
        (keep-list-marks list (if sections
                                  (loop for (section . members) in items
                                        append (mapcar (lambda (member) (funcall marked-by member section)) members))
                                  (mapcar (lambda (item) (funcall marked-by item nil)) items))))
      (multiple-value-bind (entries rows)
          (if sections
              (sectioned-pieces list items section-title #'row)
              (values items (loop for item in items append (row item nil))))
        (apply #'render-made-table-list list columns rows texts first
               :entries entries
               :mark-key (if sections
                             (lambda (entry) (let ((member (entry-member entry))) (and member (funcall marked-by member (third entry)))))
                             (lambda (entry) (funcall marked-by entry nil)))
               (loop for (name value) on options by #'cddr
                     unless (member name '(:marked-by :sections :section-title :keep-marks))
                       append (list name value)))))))

(defun keep-list-table (list titles texts first)
  "Keep the columns of LIST, as it was drawn, for a filter to try a pattern on
one (LIST-TABLE-COLUMNS, LIST-TABLE-CELL): TITLES, a vector of their titles;
TEXTS, a table from each entry to a vector of the texts of its cells; FIRST,
the first row's, shown as an example. TITLES NIL: the list has no columns."
  (setf (tuatara.buffer:buffer-local list 'list-table) (and titles (list titles texts first))))

(defun list-table-cell (list entry column)
  "The text of COLUMN (from 0) of ENTRY's row in LIST, a table made by
RENDER-TABLE-LIST, as it was last drawn; NIL when it has none."
  (let* ((table (tuatara.buffer:buffer-local list 'list-table))
         (cells (and table (gethash (if (and (consp entry) (eq (first entry) :member)) (second entry) entry) (second table)))))
    (and cells (< column (length cells)) (svref cells column))))

(defun list-table-columns (list)
  "The columns of LIST, a table made by RENDER-TABLE-LIST, a pattern may be
tried on, as (TITLE COLUMN SAMPLE): those with a title, SAMPLE the first
row's text; NIL for a list that is no such table."
  (let ((table (tuatara.buffer:buffer-local list 'list-table)))
    (when table
      (let ((first (third table)))
        (loop for title across (first table)
              for column from 0
              when (plusp (length title))
                collect (list title column (and first (< column (length first)) (svref first column))))))))

(defun table-cell (column text)
  "TEXT in COLUMN (see TABLE-HEADING-PIECES), its control characters escaped
\(ESCAPE-CONTROLS): cut or padded to its width, as COLUMN-TEXT does, or as it
is in a column with no width."
  (destructuring-bind (title &key width right (elide t) shade) column
    (declare (ignore title shade))
    (let ((text (escape-controls text)))
      (if width (column-text text width :right right :elide elide) text))))

(defun table-columns-width (columns last)
  "The columns a row of COLUMNS (TABLE-HEADING-PIECES) takes: the width of each,
LAST for a last column with no width, and a space between each two."
  (+ (loop for column in columns sum (or (getf (rest column) :width) last))
     (max 0 (1- (length columns)))))

(defun table-heading-pieces (columns)
  "The two lines over the rows of a table, as pieces for RENDER-PIECES: the
titles of COLUMNS and dashes under them, both in :header-line. Each column is (TITLE
&key WIDTH RIGHT ELIDE SHADE): TITLE heads it; its cells are WIDTH columns wide
(the last may have no width, taking the rest of the line), RIGHT set to the
right, cut with ... unless ELIDE is NIL; with SHADE a cell's face covers the
spaces either side of it too (TABLE-ROW-PIECES). One space between columns."
  (let ((title (format nil "~{~a~^ ~}" (mapcar (lambda (column) (table-cell column (first column))) columns))))
    (list (cons title :header-line) (string #\Newline)
          (cons (substitute-if #\- (lambda (c) (char/= c #\Space)) title) :header-line) (string #\Newline))))

(defun table-row-pieces (columns cells)
  "A row of a table, as pieces for RENDER-PIECES: CELLS, each a string or
\(TEXT . FACE), in COLUMNS (TABLE-HEADING-PIECES), one space between them, and
the line's end."
  (let ((pieces '()) (spaced t))
    (loop for column in columns
          for cell in cells
          do (multiple-value-bind (text face) (if (consp cell) (values (car cell) (cdr cell)) (values cell nil))
               (let ((text (table-cell column text)))
                 (if (getf (cdr column) :shade)
                     (progn (push (cons (format nil "~:[ ~;~]~a " spaced text) face) pieces)
                            (setf spaced t))
                     (progn (unless spaced (push " " pieces))
                            (push (cons text face) pieces)
                            (setf spaced nil))))))
    (nreverse (cons (string #\Newline) pieces))))

;;; Made again, kept current, left

(defun refill-list (buffer)
  "Make BUFFER again as \\[revert-buffer] makes it (its :REFILL), when it says how."
  (let ((refill (tuatara.buffer:buffer-local buffer :refill)))
    (when refill (funcall refill buffer))))

(defun redraw-list (buffer)
  "Draw BUFFER again, its marks and folds as they are now: from what it holds
\(its REDRAW, RENDER-PIECES), else as \\[revert-buffer] makes it (REFILL-LIST)."
  (let ((redraw (tuatara.buffer:buffer-local buffer 'list-redraw)))
    (if redraw (funcall redraw buffer) (refill-list buffer))))

(defun keep-shown-current (mode key &optional follow)
  "Keep the lists of major mode MODE current while a window shows them. KEY, a
function of such a list, gives cheaply what it is made from, equal keys
meaning nothing it shows has changed; when KEY gives other than it gave when
the list was last filled (SET-MADE-TEXT), the list is made again: by FOLLOW, a
function of the list and a window showing it (the selected one when that is
one), when given, else as \\[revert-buffer] makes it (its :REFILL)."
  (setf *shown-lists* (append (remove mode *shown-lists* :key #'first) (list (list mode key follow))))
  mode)

(defun refill-open-list (name mode)
  "Make the list called NAME again, when there is one in major MODE, as
\\[revert-buffer] makes it (its :REFILL): what it shows has changed."
  (let ((buffer (tuatara.buffer:find-buffer name)))
    (when (and buffer (typep (buffer-major-mode buffer) mode))
      (refill-list buffer))))

(defun keep-shown-lists-current (editor)
  "Tick function: make again each list kept current (KEEP-SHOWN-CURRENT) that a
window of EDITOR shows and whose key has changed since it was filled; a list
whose key is the same is left as it is. Wants no tick of its own."
  (let ((seen '()))
    (dolist (window (cons (editor-window editor) (editor-windows editor)))
      (let ((buffer (and window (tuatara.display:window-buffer window))))
        (when (and buffer (not (member buffer seen)))
          (push buffer seen)
          (let ((kind (shown-list-kind buffer)))
            (when (and kind (not (equal (funcall (second kind) buffer)
                                        (tuatara.buffer:buffer-local buffer 'list-key))))
              (if (third kind)
                  (funcall (third kind) buffer window)
                  (refill-list buffer))))))))
  nil)

(defgeneric mode-before-quit (mode buffer)
  (:documentation "What \\[quit-window] does first in a window showing BUFFER, of
major mode MODE, before it puts the window back: nothing, unless MODE has
something of its own to end (a rimu list's preview and quick edit).")
  (:method (mode buffer) (declare (ignore mode buffer)) nil))

(defparameter *list-free-keys* '("w" "b" "W" "B" "v" "%")
  "The keys of the modal layer's NORMAL state that move by word or expression
and select but by lines, which do nothing in a list that does not bind them
itself: a list moves a line at a time and acts on whole entries, \\`V'
selecting lines and \\`J' and \\`K' growing that selection.")

(defun free-list-keys (keymap)
  "Make *LIST-FREE-KEYS* do nothing in KEYMAP, a list's, but those it binds itself."
  (dolist (keys *list-free-keys*)
    (unless (lookup-key keymap (tuatara.frontend:parse-key-sequence keys) :parents nil)
      (define-key keymap keys :undefined))))

(defun install-special-buffers ()
  "Give every buffer a command makes (special-mode and those made from it) the
same keys to leave it and to make it again: \\`q' quit-window, \\`r' and \\`g r'
revert-buffer, and text made in a mode of its own (VIEW-MADE-TEXT) its \\`q';
every list whose entries are marked (marked-list-mode) the same keys to mark
and delete them and no moves by word nor selections but by lines (FREE-LIST-KEYS), and every list in sections (sectioned-list-mode) the same keys to fold
them; and make the lists kept current again as what they show changes
\(KEEP-SHOWN-LISTS-CURRENT)."
  (define-keys (mode-keymap 'special-mode) '(("q" quit-window) ("r" revert-buffer) ("g r" revert-buffer)))
  (define-key *made-text-keymap* "q" 'quit-window)
  (define-keys (mode-keymap 'marked-list-mode) '(("m" list-mark-entry) ("d" list-mark-for-deletion)
                                                 ("u" list-unmark-entry) ("U" list-unmark-all) ("t" list-toggle-marks)
                                                 ("x" list-execute) ("D" list-delete-marked)))
  (free-list-keys (mode-keymap 'marked-list-mode))
  (commands-for-mode 'marked-list-mode "a list whose entries are marked, as the buffer list (\\[buffers])"
                     'list-mark-entry 'list-mark-for-deletion 'list-unmark-entry 'list-unmark-all 'list-toggle-marks
                     'list-execute 'list-delete-marked)
  (define-keys (mode-keymap 'sectioned-list-mode) '(("TAB" list-toggle-fold) ("S-TAB" list-toggle-all-folds)))
  (commands-for-mode 'sectioned-list-mode "a list in sections, as the buffer list (\\[buffers])"
                     'list-toggle-fold 'list-toggle-all-folds)
  (pushnew 'keep-shown-lists-current *tick-functions*))

;;; Going from entry to entry

(defun entry-start-p (entries index)
  "True when line INDEX of ENTRIES, a list's (RENDER-PIECES), begins an entry:
it shows one, and not the one the line before it shows, an entry being shown
on several lines in a row by one object."
  (let ((entry (elt entries index)))
    (and entry (or (zerop index) (not (eq entry (elt entries (1- index))))))))

(defun list-step-entry (list direction &key (test (constantly t)))
  "Put point of LIST, a list made by RENDER-PIECES, at the start of the first
line of the next entry after the one on the current line (DIRECTION 1), or
of the one before it (-1), passing over those TEST, a function of an entry,
is false of. Returns that entry, or NIL, point left where it is, when there
is none that way."
  (let* ((entries (tuatara.buffer:buffer-local list 'list-entries #()))
         (first (tuatara.buffer:buffer-local list 'list-first-line 0))
         (here (- (tuatara.buffer:buffer-line-number list (tuatara.buffer:buffer-point list)) first))
         (count (length entries))
         (current (and (< -1 here count) (elt entries here))))
    (loop for index = (if (plusp direction) (max 0 (1+ here)) (min (1- count) (1- here))) then (+ index direction)
          while (< -1 index count)
          do (let ((entry (elt entries index)))
               (when (and (entry-start-p entries index) (not (eq entry current)) (funcall test entry))
                 (setf (tuatara.buffer:buffer-point list) (tuatara.buffer:buffer-goto-line list (+ first index)))
                 (return entry))))))

(defun list-heading-p (list entry)
  "True when ENTRY of LIST heads a section rather than standing for something:
a section's line, (:SECTION NAME), or one the list's search filtering says is
a heading (SET-SEARCH-FILTERING)."
  (let ((filtering (list-filtering list)))
    (if filtering
        (heading-entry-p filtering entry)
        (and (consp entry) (eq (first entry) :section)))))

(defgeneric list-entry-stepped (mode list entry)
  (:documentation "What a list of major mode MODE does once \\[list-next-entry] or
\\[list-previous-entry] has put point on ENTRY of LIST: nothing, unless the
mode shows the entry somewhere (a definition in the window the list was
asked from).")
  (:method (mode list entry)
    (declare (ignore mode list entry))
    nil))

(defun list-step-to-entry (direction)
  "Go to the next entry of the current list, or with DIRECTION -1 the one
before, passing over the headings of its sections (LIST-STEP-ENTRY), and do
what the list does then (LIST-ENTRY-STEPPED); say so when there is none."
  (let* ((list (current-buffer))
         (entry (list-step-entry list direction :test (lambda (entry) (not (list-heading-p list entry))))))
    (if entry
        (list-entry-stepped (buffer-major-mode list) list entry)
        (message (if (plusp direction) "No more below" "No more above")))))

(define-command list-next-entry ()
  "Go to the next entry of the list, the start of its first line, passing
over the headings of its sections."
  (list-step-to-entry 1))

(define-command list-previous-entry ()
  "Go to the entry of the list before the one on the current line, the start
of its first line, passing over the headings of its sections."
  (list-step-to-entry -1))

(defun entry-list-keys (mode where)
  "Give the lists of major MODE, WHERE in words for those elsewhere, the keys
of a list whose entries are opened: \\`RET' and \\`l' open the entry on the
line (\\[list-visit]), \\`o' in another window (\\[list-visit-other]), \\`h' goes back
\(\\[quit-window]), \\`n' and \\`p' go to the next entry and the one before; the
commands run there (COMMANDS-FOR-MODE)."
  (define-keys (mode-keymap mode) '(("RET" list-visit) ("l" list-visit) ("o" list-visit-other) ("h" quit-window)
                                    ("n" list-next-entry) ("p" list-previous-entry)))
  (commands-for-mode mode where 'list-visit 'list-visit-other 'list-next-entry 'list-previous-entry))
