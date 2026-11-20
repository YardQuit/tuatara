;;;; lisp/modes/base.lisp --- The base modes: text, programming, special.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.defaults)

(define-major-mode text-mode fundamental-mode
  "Prose and plain text: the parent of the modes for written and tabular text."
  :extensions ("txt" "text"))

(define-major-mode prog-mode fundamental-mode
  "Programming languages: the parent of the modes for code, where filling
(\\[fill-paragraph]) fills comments, not the code around them."
  :lighter "Prog")

(define-major-mode special-mode fundamental-mode
  "Read-only buffers made by commands, such as help and lists. Letters are the
buffer's own keys first; modal editing adds NORMAL state's keys that move, select and
copy (\\<support>\\[backward-char] \\[next-line] \\[previous-line] \\[forward-char],
\\[forward-word] \\[backward-word], \\[beginning-of-buffer], \\`g e',
\\[end-of-buffer], \\[start-selection], \\[select-lines], \\[copy-selection-or-char]
and the rest) and the \\`SPC' leader."
  :modal-class :support :lighter "Special" :interactive nil)

;;; Colouring by major mode

(defvar *colouring-rules* '()
  "The colouring styles set-colouring has given major modes and every buffer:
each (MODE STYLE LEVEL), MODE a major mode's name or T for every buffer, STYLE
:RICH, :SPARSE or :NONE, LEVEL the sparse style's level or NIL.")

(defun colouring-rule (mode)
  "The style and the sparse level set-colouring gives major mode MODE (an
instance, or NIL for text in no mode of its own), as two values: those given
the nearest mode it is made from, else those given every buffer; NIL when
neither has been given one."
  (let ((rule (or (mode-setting mode *colouring-rules*) (cdr (assoc t *colouring-rules*)))))
    (values (first rule) (second rule))))

(define-setting *line-record-modes*
  '(log-mode jsonl-mode todotxt-mode hosts-generic-mode etc-passwd-generic-mode
    pip-requirements-mode gitignore-mode dotenv-mode)
  "The major modes whose files hold a record a line, as a log does. Search
filters (\\[search-filter-regex]) narrow them, and the modes made from them,
by hiding the lines they leave out, as they narrow a CSV.
- a list of mode names: log-mode, jsonl-mode, todotxt-mode and the others
  of the standard list."
  :values (list-of symbol))

(defgeneric filters-hide-lines-p (mode buffer)
  (:documentation "True when search filters (\\[search-filter-regex]) narrow BUFFER, in
major mode MODE, by hiding its lines, each a record matched as it stands,
rather than as a list of entries: the lines no filter leaves hidden from its
windows, its header lines always shown (RECORD-HEADER-LINES), and its text
never changed by them (a CSV, a log).")
  (:method (mode buffer)
    "A file of a mode in *LINE-RECORD-MODES*, or made from one."
    (declare (ignore buffer))
    (and mode (intersection *line-record-modes* (mode-precedence mode)) t)))

(defgeneric mode-link-search (mode buffer search)
  (:documentation "Go to what SEARCH, the part after :: of a link to a file
\([[file:notes.org::A heading]]), names in BUFFER, that file visited in
major mode MODE; true when it is found. A text is searched as an Org link
searches it; an image's view shows the picture inside it a search of
picture 2 names."))

(defgeneric record-header-lines (mode buffer)
  (:documentation "How many lines at the top of BUFFER, a file of records in major mode MODE,
are its header unless \\[list-toggle-header] says otherwise (a CSV's one).")
  (:method (mode buffer)
    "No header: every line is a record."
    (declare (ignore mode buffer))
    0))

(define-setting *list-header-lock* nil
  "Whether the header of a file of records stays at the top of the window as
the rest scrolls under it, wherever there is one: a CSV's first line, a
log's once \\[list-toggle-header] makes it a header.
\\[list-toggle-header-lock] does it for one buffer.
- t: it stays.
- nil: it scrolls with the rest, as standard."
  :values boolean)

(defun list-header-p (buffer)
  "True when the first line of BUFFER, a file of records, is its header,
always shown by search filters and never deleted by \\[list-keep-records]: as
\\[list-toggle-header] said for BUFFER, else as its mode has it
\(RECORD-HEADER-LINES: a CSV's first line, unless *CSV-HEADER* is NIL)."
  (let ((said (tuatara.buffer:buffer-local buffer :list-header :mode)))
    (if (eq said :mode)
        (plusp (record-header-lines (buffer-major-mode buffer) buffer))
        said)))

(define-record (record-fields (:constructor make-record-fields (&key each names keys)) (:copier nil))
  "How the records of a file are cut into fields, for a filter to try its
pattern on one."
  (each nil :documentation "A function of the text of a record, its length, a field and a function FN:
calls FN with a string and where a value of the field stands in it (its start
and end), for each value till FN returns true, returning what it returned; NIL
when there is no such value. Makes nothing where it can.")
  (names nil :documentation "The names of the fields in order, a field being its number from 0; those past
the last named go by their numbers.")
  (keys nil :documentation "NIL when a field is its number; else a function of the buffer giving the names
its records' fields go by, a field being its name (JSON Lines' keys)."))

(setf (documentation 'make-record-fields 'function)
      "Make how a mode cuts its records into fields, for MODE-RECORD-FIELDS to give.
EACH is a function of a record's text, its length, a field and a function FN,
calling FN with a string and where a value of the field stands in it for each
value till FN returns true. NAMES is the fields' names in order, a field being
its number from 0. Or KEYS is a function of the buffer giving the names its
fields go by, a field being its name.")

(defgeneric mode-record-fields (mode buffer)
  (:documentation "How the records of BUFFER, in major mode MODE, are cut into fields (a
RECORD-FIELDS): \\[search-filter-show-records], \\[keep-lines] and the others ask for
one and try the pattern on it alone. NIL when only the whole line is tried.")
  (:method (mode buffer)
    "No fields: the whole line."
    (declare (ignore mode buffer))
    nil))
