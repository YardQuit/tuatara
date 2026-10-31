;;;; lisp/org/fontify.lisp --- Org's colours: a buffer's parse kept up, its shown part coloured.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; What the window shows of an Org buffer is coloured from the parses of
;;;; its parts in sight (parts.lisp), each parsed again after a change within
;;;; it, at once while it is small and in the background when it is large;
;;;; commands that work on the whole buffer take its whole parse, made of
;;;; the parts' parses when they ask for it. The colours are those Emacs's
;;;; Org gives: headlines by level, TODO keywords, priorities and tags, the
;;;; keyword lines, blocks, drawers, planning, tables, lists' checkboxes and
;;;; terms, and in text, emphasis, code, links, timestamps, footnotes,
;;;; macros and the rest.

(in-package #:tuatara.defaults)

(defvar *org-startup-indented* t
  "True for Org files to open in the indented view (\\[org-indent-mode]), as
they do: each line shown indented by the level of the headline it is under, a
headline's stars but the last hidden, the file itself as it is; NIL for them
to open as they are written. #+STARTUP: indent and noindent in a file say
otherwise for it, \\[org-indent-mode] for the buffer.")

(defparameter *org-startup-read-limit* (* 1024 1024)
  "How far into an Org buffer, in octets, its #+STARTUP: lines are looked for.")

(defun org-startup-options (buffer)
  "The words of BUFFER's #+STARTUP: lines (overview, indent, ...), in order
and in lower case: every such line in its first *ORG-STARTUP-READ-LIMIT*
octets, a line cut there read as far as that. The text is read where it is
kept, a window at a time; only those lines are copied out of it.
Complexity: O(the octets read)."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (limit (min (tuatara.text:rope-bytes rope) *org-startup-read-limit*))
         (keyword "#+STARTUP:")
         (window (make-array 65536 :element-type '(unsigned-byte 8)))
         ;; where the reading is: at a line's start, in KEYWORD (MATCHED octets of it), in a
         ;; #+STARTUP: line's words (VALUE), or past a line that is none
         (state :start) (matched 0) (value nil)
         (words '()))
    (flet ((take ()
             (setf words (append words (remove "" (cl-ppcre:split "\\s+" (string-downcase (tuatara.text:octets-string (subseq value 0))))
                                               :test #'string=)))))
      (loop for from from 0 below limit by (length window)
            for to = (min limit (+ from (length window)))
            do (tuatara.text:rope-copy-into rope from to window)
               (loop for i below (- to from)
                     for octet = (aref window i)
                     do (ecase state
                          (:start (case octet
                                    ((10 13 32 9))
                                    (35 (setf state :keyword matched 1))
                                    (t (setf state :skip))))
                          (:keyword (cond ((member octet '(10 13)) (setf state :start))
                                          ((char-equal (code-char octet) (char keyword matched))
                                           (when (= (incf matched) (length keyword))
                                             (setf state :value
                                                   value (make-array 16 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))))
                                          (t (setf state :skip))))
                          (:value (if (member octet '(10 13))
                                      (progn (take) (setf state :start))
                                      (vector-push-extend octet value)))
                          (:skip (when (member octet '(10 13)) (setf state :start))))))
      (when (eq state :value) (take)))
    words))

(defun org-startup-choice (options choices)
  "Of the #+STARTUP: words OPTIONS (ORG-STARTUP-OPTIONS), the last that is one of
CHOICES, as the last such word says in Org; NIL when none is."
  (find-if (lambda (word) (member word choices :test #'string=)) options :from-end t))

(defvar *org-bullets* '("◉" "○" "✸" "✿")
  "The signs Org headlines show in place of their last star, by level, the
first for the first level and round again past the last;
NIL to show the stars. The file keeps its stars.")

(defvar *org-list-bullets* nil
  "The signs list items show in place of their bullets, each (BULLET . SIGN),
such as '((\"-\" . \"•\") (\"+\" . \"◦\") (\"*\" . \"▸\"));
NIL, as by default, to show the bullets as written. Numbered items keep their
numbers; the file keeps its bullets.")

(defvar *org-checkbox-signs* nil
  "The signs list items' checkboxes show, each (CHECKBOX . SIGN), such as
'((\"[ ]\" . \"☐\") (\"[-]\" . \"◐\") (\"[X]\" . \"☑\")), each followed by a space,
so that a sign drawn wider than one column shows whole; NIL, as by default,
to show them as written. \\[org-ctrl-c-ctrl-c] ticks them as before; the file keeps [ ], [-]
and [X].")

(defvar *org-hide-leading-stars* t
  "True to hide a headline's stars but the last, with *ORG-BULLETS* or in the
indented view; NIL to show them faint
\(the :org-leading-stars face).")

(defun org-indented-p (buffer)
  "True when BUFFER, an Org buffer, is shown in the indented view."
  (tuatara.buffer:buffer-local buffer 'org-indent))

(defvar *org-link-descriptive* t
  "True for Org links to be shown as their labels, [[target][label]] as label
and [[target]] as target, and entities as their characters, \\dollar{} as $;
NIL to show them as written. \\[org-toggle-link-display] changes it for a buffer.")

(defvar *org-log-done* :time
  "What is noted when a task is done: :TIME (as standard) a CLOSED: line with
the time, which the agenda's log mode lists; NIL nothing.")

(defvar *org-todo-keyword-faces* '(("WAIT" . :org-wait) ("CANCELLED" . :org-cancelled) ("CANCELED" . :org-cancelled))
  "The TODO keywords with a face of their own, each (KEYWORD . FACE), in an
Org file's headlines and in the agenda: WAIT in :org-wait, CANCELLED in
:org-cancelled. Any other keyword is in :org-todo while its task is open and
in :org-done once it is closed. Add your own:
\(push '(\"NEXT\" . :org-wait) *org-todo-keyword-faces*).")

(defun org-todo-keyword-face (keyword done)
  "The face of the TODO keyword KEYWORD, of a task closed when DONE is true:
its own in *ORG-TODO-KEYWORD-FACES*, else :org-done or :org-todo."
  (or (cdr (assoc keyword *org-todo-keyword-faces* :test #'string=))
      (if done :org-done :org-todo)))

(defvar *org-log-into-drawer* t
  "True for the notes logged of a task (a change of state, a repeat, a
reschedule, a note taken) to go into the entry's :LOGBOOK: drawer, where
they fold with it (*ORG-FOLD-METADATA*); NIL for them to go right after its
planning line and properties, outside any drawer.")

(defvar *org-log-repeat* :time
  "What Org notes when a task that repeats is done: :TIME the change of state
and LAST_REPEAT with the time; NIL nothing.")

(defvar *org-current-text* ""
  "The text of the Org buffer a command works on, as parsed.")

;;; The parse of a buffer, kept up

(defun org-strip-returns (string)
  "STRING with the carriage return before each newline left out, as a note
whose lines end with CR LF is read, and each carriage return that ends a
line alone read as a newline: two values, the text and the places in it of
the newlines that lost one (a vector in order), or NIL when none did."
  (if (not (find #\Return string))
      (values string nil)
      (let ((out (make-string (length string)))
            (returns (make-array 64 :element-type 'fixnum :adjustable t :fill-pointer 0))
            (j 0) (n (length string)))
        (dotimes (i n)
          (let ((c (char string i)))
            (cond ((and (char= c #\Return) (< (1+ i) n) (char= (char string (1+ i)) #\Newline))
                   (vector-push-extend j returns))
                  ((char= c #\Return) (setf (char out j) #\Newline) (incf j))
                  (t (setf (char out j) c) (incf j)))))
        (values (subseq out 0 j)
                (and (plusp (length returns)) (coerce returns '(simple-array fixnum (*))))))))

(defun lone-returns-as-newlines (string)
  "STRING with each carriage return that no newline follows, which ends a
line, made a newline: as long as STRING, every character where it was."
  (if (not (find #\Return string))
      string
      (let ((out (copy-seq string)))
        (dotimes (i (length out) out)
          (when (and (char= (char out i) #\Return)
                     (not (and (< (1+ i) (length out)) (char= (char out (1+ i)) #\Newline))))
            (setf (char out i) #\Newline))))))

(defun org-buffer-text (buffer &key (start 0) (end (tuatara.buffer:buffer-size buffer)))
  "The text Org reads of BUFFER from octet START below END, all of it unless
they are given: in a buffer whose lines end with a carriage return and a
newline, each return before a newline left out; a carriage return that ends
a line alone read as a newline."
  (let ((string (lone-returns-as-newlines (tuatara.buffer:buffer-string buffer :start start :end end))))
    (if (buffer-crlf-p buffer) (lf-newlines string) string)))

(defun org-line-head (buffer position &optional (octets 512))
  "The start of the line of BUFFER that octet POSITION is on, at most OCTETS
of it (512 unless given) as a string, cut before a character the limit
falls in: enough to tell what kind of line it is without reading a line of
a million characters whole."
  (let* ((start (tuatara.buffer:buffer-line-start buffer position))
         (end (tuatara.buffer:buffer-line-end buffer position)))
    (tuatara.buffer:buffer-string buffer :start start
                                         :end (if (<= (- end start) octets)
                                                  end
                                                  (tuatara.text:rope-char-start (tuatara.buffer:buffer-text buffer) (+ start octets))))))

(defun org-checkbox-line-p (line)
  "True when LINE, the start of a line of Org text, begins a list item with a
checkbox: - [ ], 1. [X], + [-]."
  (and (cl-ppcre:scan "^(?:[ \\t]*(?:[-+]|[0-9]+[.)])|[ \\t]+\\*)[ \\t]+\\[[ X-]\\]" line) t))

(defun org-returns-in-rope (rope &key (start 0) (end (tuatara.text:rope-bytes rope)))
  "Where the text Org reads of ROPE from the octet START below END (all of it
unless they are given), the carriage returns before its newlines left out,
lost one: as ORG-STRIP-RETURNS gives them, found in one pass over the
octets, no copy of the text made; NIL when none did."
  (let ((returns (make-array 64 :element-type 'fixnum :adjustable t :fill-pointer 0))
        (chars 0)
        (pending nil))                  ; a carriage return ending a chunk, its newline yet to come
    (loop for chunk from start below end by 1048576
          do (let ((octets (tuatara.text:rope-octets rope :start chunk :end (min end (+ chunk 1048576)))))
               (declare (type (simple-array (unsigned-byte 8) (*)) octets))
               (loop for octet across octets
                     for i from 0
                     do (when pending
                          (setf pending nil)
                          (if (= octet 10) (vector-push-extend chars returns) (incf chars)))
                        (cond ((= octet 13)
                               (if (< (1+ i) (length octets))
                                   (if (= (aref octets (1+ i)) 10) (vector-push-extend chars returns) (incf chars))
                                   (setf pending t)))
                              ((/= (logand octet #xC0) #x80) (incf chars))))))
    (when pending (incf chars))
    (and (plusp (length returns)) (coerce returns '(simple-array fixnum (*))))))

(defun org-returns-of (buffer)
  "Where the text Org reads of BUFFER lost a carriage return before a newline
(ORG-STRIP-RETURNS): its parse's, while that is of the text as it is now,
else put together from its parts' (ORG-PARTS-RETURNS), once for each
change; NIL for a buffer whose lines end with a newline alone."
  (let ((state (org-buffer-state buffer))
        (version (tuatara.buffer:buffer-version buffer)))
    (cond ((not (buffer-crlf-p buffer)) nil)
          ((eql (org-state-version state) version) (org-state-returns state))
          (t (let ((kept (tuatara.buffer:buffer-local buffer 'org-fresh-returns)))
               (if (and kept (eql (car kept) version))
                   (cdr kept)
                   (let ((returns (org-parts-returns buffer)))
                     (setf (tuatara.buffer:buffer-local buffer 'org-fresh-returns) (cons version returns))
                     returns)))))))

(defun org-returns-before (returns position)
  "How many of RETURNS (ORG-STRIP-RETURNS) are before POSITION."
  (let ((lo 0) (hi (length returns)))
    (setf lo (tuatara.runtime:bisect (mid lo hi) (< (aref returns mid) position)) hi lo)
    lo))

(defun org-char->byte (buffer position &optional (returns (org-returns-of buffer)) (base 0))
  "The octet of BUFFER where the character POSITION of the text Org reads of
it is: the carriage returns it left out (RETURNS) counted back in, a position
at a line's end standing before its CR LF. For a text read from a part of
BUFFER, BASE is the character of BUFFER the part begins at, RETURNS those
the part's text left out."
  (tuatara.text:char->byte (tuatara.buffer:buffer-text buffer)
                           (+ base (if returns (+ position (org-returns-before returns position)) position))))

(defun org-byte->char (buffer byte &optional (returns (org-returns-of buffer)) (base 0))
  "The character of the text Org reads of BUFFER at its octet BYTE: the
carriage returns before it (RETURNS) left out, one itself taken as its
newline. For a text read from a part of BUFFER, BASE is the character of
BUFFER the part begins at, RETURNS those the part's text left out."
  (let ((char (- (tuatara.text:byte->char (tuatara.buffer:buffer-text buffer) byte) base)))
    (if (null returns)
        char
        ;; the Ith return left out stood at the character (+ (aref returns i) i) of the buffer
        (let ((lo 0) (hi (length returns)))
          (setf lo (tuatara.runtime:bisect (mid lo hi) (< (+ (aref returns mid) mid) char)) hi lo)
          (- char lo)))))

(defun org-buffer-state (buffer)
  "BUFFER's ORG-STATE, made the first time."
  (or (tuatara.buffer:buffer-local buffer 'org-state)
      (setf (tuatara.buffer:buffer-local buffer 'org-state) (make-org-state))))

(defun org-worker-parse (job)
  "Parse what JOB, as ORG-WANT-PARSE asks for it, holds: (TREE TEXT RETURNS
PLACES), PLACES where the parts are in a whole buffer's parse (ORG-ASSEMBLE);
NIL for a text the parser cannot read."
  (destructuring-bind (version source crlf keywords) job
    (declare (ignore version))
    (if (org-snapshot-p source)
        (ignore-errors (multiple-value-list (org-assemble source)))
        (let ((raw (if (stringp source)
                       source
                       (destructuring-bind (rope start end) source
                         (ignore-errors (tuatara.text:rope-string rope :start start :end end))))))
          (when raw
            (multiple-value-bind (text returns) (if crlf (org-strip-returns raw) (values (lone-returns-as-newlines raw) nil))
              (let ((tree (ignore-errors (org-parse text :todo-keywords keywords))))
                (and tree (list tree text returns nil)))))))))

(defun org-parse-worker (state editor)
  "Parse in the background what STATE wants, the latest asked for, again as
long as something newer is asked for meanwhile; each parse put in STATE's
RESULT in one step, EDITOR, when given, woken for it."
  (unwind-protect
       (loop
         (let* ((job (sb-thread:with-mutex ((org-state-lock state)) (org-state-wanted state)))
                (start (get-internal-real-time))
                (parse (org-worker-parse job)))
           (setf (org-state-parse-seconds state) (tuatara.runtime:seconds-since start))
           ;; the result put in whatever came of it: a text the parser cannot read is not asked for again and again
           (destructuring-bind (&optional tree text returns places) parse
             (setf (org-state-result state)
                   (list tree text (first job) returns (and tree (org-src-language-names tree))
                         (and places (cons (second job) places))))
             (when (and tree editor) (wake-editor editor)))
           (sb-thread:with-mutex ((org-state-lock state))
             (sb-thread:condition-broadcast (org-state-done state))
             (when (eq job (org-state-wanted state))
               (setf (org-state-wanted state) nil
                     (org-state-worker state) nil)
               (return)))))
    ;; ended however it ended: the next parse asked for starts a worker of its own
    (sb-thread:with-mutex ((org-state-lock state))
      (when (eq (org-state-worker state) sb-thread:*current-thread*)
        (setf (org-state-worker state) nil)))))

(defun org-want-parse (buffer state version source &key todo-keywords)
  "Have BUFFER's text at VERSION parsed in the background into STATE: SOURCE,
a function of no arguments called when the job is made, gives what is
parsed, an ORG-SNAPSHOT of its parts for the whole buffer (ORG-ASSEMBLE) or
the octets (START . END) of a part, parsed with TODO-KEYWORDS. The worker
running takes it next, or one is started. A buffer of a file mapped from
disk gives the worker a copy of a part's text, made when a worker starts,
since its mapping may go with the buffer; another gives its text as it is,
which never changes."
  (let ((mapped (tuatara.buffer:buffer-mapping buffer)))
    (flet ((job ()
             (let ((what (funcall source)))
               (list version
                     (cond ((org-snapshot-p what) what)
                           (mapped (tuatara.buffer:buffer-string buffer :start (car what) :end (cdr what)))
                           (t (list (tuatara.buffer:buffer-text buffer) (car what) (cdr what))))
                     (buffer-crlf-p buffer) todo-keywords))))
      (sb-thread:with-mutex ((org-state-lock state))
        (unless (eql (car (org-state-wanted state)) version)
          (cond ((let ((worker (org-state-worker state))) (and worker (sb-thread:thread-alive-p worker)))
                 (unless mapped
                   (setf (org-state-wanted state) (job))))
                (t
                 (setf (org-state-wanted state) (job)
                       (org-state-worker state)
                       (let ((editor *editor*))
                         (tuatara.runtime:make-guarded-thread (lambda () (org-parse-worker state editor)) :name "tuatara org parse"))))))))))

(defun org-src-language-names (tree)
  "The languages the source blocks of TREE name, each once, as written."
  (let ((names '()))
    (org-walk-tree tree (lambda (node)
                          (when (and (eq (org-node-type node) 'src-block) (org-property node :language))
                            (pushnew (org-property node :language) names :test #'string-equal))))
    names))

(defun org-warm-src-languages (names)
  "Have the colouring rules of the languages of NAMES (source blocks' languages
as written) compiled in the background, those not compiled yet: each block's
code is coloured as soon as it is shown, the note redrawn as each is ready."
  (dolist (name names)
    (let ((language (ignore-errors (org-src-language name))))
      (when (and language (not (rules-ready-p language)))
        (warm-rules language)))))

(defun org-version-number (version)
  "VERSION, a buffer's version or the number of one, as a number that grows with each change."
  (if (integerp version) version (tuatara.text:version-number version)))

(defun org-take-parse (state)
  "Take into STATE the parse its worker finished last, when it is newer than STATE's own."
  (let ((result (org-state-result state)))
    (when (and result (> (org-version-number (third result)) (org-version-number (org-state-version state))))
      (destructuring-bind (tree text parsed returns names made) result
        (setf (org-state-tree state) tree
              (org-state-text state) text
              (org-state-returns state) returns
              (org-state-version state) parsed)
        (when made
          (org-keep-whole-places (car made) tree (cdr made))
          ;; the parts are let go of with it, once taken
          (setf (nth 5 result) nil))
        (org-warm-src-languages names)))))

(defun org-await-parse (state version)
  "Wait at most *ORG-PARSE-WAIT* seconds for STATE's worker to have parsed the
text at VERSION, once for each VERSION; true when it has. A buffer whose last
parse took longer than that is not waited for: typing never waits on it, its
colours coming with the parse."
  (unless (or (eql (org-state-waited state) version)
              (let ((took (org-state-parse-seconds state))) (and took (> took *org-parse-wait*))))
    (setf (org-state-waited state) version)
    (let ((deadline (tuatara.runtime:deadline-in *org-parse-wait*)))
      (sb-thread:with-mutex ((org-state-lock state))
        (loop for result = (org-state-result state)
              until (and result (>= (org-version-number (third result)) (org-version-number version)))
              do (let ((left (tuatara.runtime:seconds-left deadline)))
                   (when (<= left 0) (return nil))
                   (tuatara.runtime:wait-on (org-state-done state) (org-state-lock state) left))
              finally (return t))))))

(defun org-buffer-tree (buffer)
  "BUFFER's parse, as two values: the tree and the text it is of (characters,
as the tree's positions count them, the carriage returns before newlines
left out: ORG-CHAR->BYTE and ORG-BYTE->CHAR go between them and the
buffer). Made of the parses of BUFFER's parts (ORG-ASSEMBLE), each part
parsed again only after a change within it, when BUFFER changed: at once
while it is small (*ORG-PARSE-AT-ONCE*), else in the background, the latest
text first, waited for a moment (*ORG-PARSE-WAIT*) and the parse before
returned until that one is done; NIL while there has been none."
  (let ((state (org-buffer-state buffer))
        (version (tuatara.buffer:buffer-version buffer)))
    (org-take-parse state)
    (unless (eql version (org-state-version state))
      (let ((parts (org-buffer-parts buffer)))
        (if (<= (tuatara.buffer:buffer-size buffer) *org-parse-at-once*)
            (let ((snapshot (org-parts-snapshot buffer parts)))
              (multiple-value-bind (tree text returns places) (org-assemble snapshot)
                (setf (org-state-tree state) tree
                      (org-state-text state) text
                      (org-state-returns state) returns
                      (org-state-version state) version)
                (org-keep-whole-places snapshot tree places)
                (org-warm-src-languages (org-src-language-names tree))))
            (progn
              (org-want-parse buffer state version (lambda () (org-parts-snapshot buffer parts)))
              (when (org-await-parse state version)
                (org-take-parse state))))))
    (values (org-state-tree state) (org-state-text state))))

(defun org-parse-current-p (buffer)
  "True when BUFFER's parse is of its text as it is now."
  (eql (org-state-version (org-buffer-state buffer)) (tuatara.buffer:buffer-version buffer)))

;;; Colouring

(defvar *org-buffer* nil
  "The buffer being coloured.")

(defun org-level-face (level)
  "The face of headlines of LEVEL: :org-level-1 to :org-level-8, and past the
eighth the first again."
  (aref #(:org-level-1 :org-level-2 :org-level-3 :org-level-4 :org-level-5 :org-level-6 :org-level-7 :org-level-8)
        (rem (1- level) 8)))

(defvar *org-ranges* '()
  "The colours being collected for the rows shown, each (START END FACE) in octets, the latest last.")

(defvar *org-view* nil
  "What is known of the part of the buffer being coloured, an ORG-VIEW.")

(defvar *org-shown* '(0 . 0)
  "The characters being coloured, (START . END).")

(defvar *org-src-blocks* '()
  "The source blocks among the rows being coloured, each (LANGUAGE FROM TO),
FROM and TO the octets of their code.")

(defvar *org-src-languages*
  '(("emacs-lisp" . "elisp") ("C++" . "cpp") ("sqlite" . "sql") ("spec" . "rpmspec")
    ("sshclientconfig" . "ssh_client_config") ("conf" . "ini") ("desktop" . "ini") ("ash" . "bash")
    ("dash" . "bash") ("ksh" . "bash") ("mksh" . "bash") ("posix" . "bash") ("screen" . "bash")
    ("text" . nil) ("fundamental" . nil) ("calc" . nil))
  "The language each name a source block is given (#+begin_src emacs-lisp)
means, where Org's name is not the language's own: each (NAME . LANGUAGE),
LANGUAGE NIL for none. Any other
name is the language of that name, else the one a file ending in .NAME is in.
Add your own: (push '(\"kitty\" . \"ini\") *org-src-languages*).")

(defparameter *org-face-kinds*
  '((:org-level-1 . "markup.heading") (:org-level-2 . "markup.heading") (:org-level-3 . "markup.heading")
    (:org-level-4 . "markup.heading") (:org-level-5 . "markup.heading") (:org-level-6 . "markup.heading")
    (:org-level-7 . "markup.heading") (:org-level-8 . "markup.heading") (:org-headline-done . "markup.heading")
    (:org-archived . "markup.heading") (:org-leading-stars . "markup.heading") (:org-document-title . "markup.heading")
    (:comment . "comment") (:org-meta-line . "comment") (:org-block-begin-line . "comment")
    (:org-block-end-line . "comment") (:org-document-info-keyword . "comment") (:org-document-info . "string")
    (:org-todo . "keyword") (:org-done . "keyword") (:org-wait . "keyword") (:org-cancelled . "keyword")
    (:org-priority . "keyword") (:org-log-note . "comment")
    (:org-special-keyword . "keyword") (:org-scheduled-keyword . "keyword") (:org-deadline-keyword . "keyword") (:org-drawer . "keyword") (:org-tag . "tag")
    (:org-bold . "markup.bold") (:org-italic . "markup.italic") (:org-underline . "markup.underline")
    (:org-link . "markup.link") (:org-cite . "markup.link") (:org-footnote . "markup.link") (:org-target . "markup.link")
    (:org-code . "markup.raw") (:org-verbatim . "markup.raw") (:org-block . "markup.raw")
    (:org-latex-and-related . "markup.raw") (:org-table . "markup.raw") (:org-quote . "markup.quote")
    (:org-verse . "markup.quote") (:org-checkbox . "markup.list") (:org-list-dt . "markup.list")
    (:org-date . "constant") (:org-macro . "constant") (:org-macro-text . "markup.link") (:org-formula . "constant")
    (:org-property-value . "attribute"))
  "What each of Org's faces colours, named as the colouring rules of
languages name what they capture, for the sparse style's levels
\(*SPARSE-LEVELS*): a face not here is coloured only at level 4.")

(defvar *org-colouring-level* nil
  "While Org text is coloured in the sparse style, its level, 0 in the style
none; NIL in the rich style.")

(defun org-face-coloured-p (face)
  "True when FACE is put on Org text in the style it is coloured in
\(*ORG-COLOURING-LEVEL*); the hidden stars of the indented view are hidden in
every style."
  (or (null *org-colouring-level*)
      (eq face :org-hide)
      (let ((kind (cdr (assoc face *org-face-kinds*))))
        (if kind
            (and (plusp *org-colouring-level*) (sparse-colours-p kind *org-colouring-level*))
            (>= *org-colouring-level* 4)))))

(defun org-colour (from to face)
  "Colour the characters FROM below TO in FACE, as far as they are shown and
the style Org is coloured in has FACE (ORG-FACE-COLOURED-P)."
  (let ((from (max from (car *org-shown*))) (to (min to (cdr *org-shown*))))
    (when (and (< from to) (org-face-coloured-p face))
      (push (list (org-view-byte *org-buffer* *org-view* from) (org-view-byte *org-buffer* *org-view* to) face)
            *org-ranges*))))

(defparameter *org-element-types*
  '(org-data section headline babel-call center-block clock comment comment-block diary-sexp drawer
    dynamic-block example-block export-block fixed-width footnote-definition horizontal-rule item keyword
    latex-environment node-property paragraph plain-list planning property-drawer quote-block section
    special-block src-block table table-row verse-block)
  "The types of Org's elements, as against its objects.")

(defun org-content-end (node)
  "Where NODE's own text ends, before the blank lines (or spaces) after it."
  (if (member (org-node-type node) *org-element-types*)
      (org-end-of-content (org-node-end node) (org-node-begin node))
      (- (org-node-end node) (org-node-post-blank node))))

(defun org-shown-p (node)
  "True when some of NODE is in the characters being coloured."
  (and (< (org-node-begin node) (cdr *org-shown*)) (> (org-node-end node) (car *org-shown*))))

(defun org-fontify (node)
  "Collect the colours of NODE and of what is in it, as far as it is shown."
  (when (org-shown-p node)
    (let ((type (org-node-type node)))
      (case type
        (headline (org-fontify-headline node))
        (keyword (org-fontify-keyword node))
        (src-block (org-fontify-block node :org-block) (org-note-src-block node))
        ((example-block export-block comment-block) (org-fontify-block node :org-block))
        (quote-block (org-fontify-block node :org-quote))
        (verse-block (org-fontify-block node :org-verse))
        ((center-block special-block dynamic-block) (org-fontify-block node nil))
        ((drawer property-drawer) (org-fontify-drawer node))
        (node-property (org-fontify-node-property node))
        ((planning clock) (org-fontify-planning node))
        (comment (org-colour (org-node-post-affiliated node) (org-content-end node) :comment))
        (fixed-width (org-colour (org-node-post-affiliated node) (org-content-end node) :org-code))
        (table (org-fontify-table node))
        (footnote-definition (org-fontify-footnote-definition node))
        (item (when (org-state-note-p node)
                ;; wholly in :org-log-note, its timestamp and note too
                (org-colour (org-node-begin node) (org-content-end node) :org-log-note)
                (return-from org-fontify))
              (org-fontify-item node))
        ((babel-call latex-environment diary-sexp horizontal-rule)
         (org-colour (org-node-post-affiliated node) (org-content-end node)
                     (if (eq type 'latex-environment) :org-latex-and-related :org-meta-line))))
      (when (member type *org-element-types*)
        (org-fontify-affiliated node))
      (org-fontify-object node)
      (org-children-in-range node (car *org-shown*) (cdr *org-shown*) #'org-fontify))))

(defun org-fontify-affiliated (node)
  "The keyword lines naming or captioning NODE, before it, in :org-meta-line."
  (let ((begin (org-node-begin node)) (after (org-node-post-affiliated node)))
    (when (and after (< begin after))
      (org-colour begin after :org-meta-line))))

(defun org-fontify-headline (node)
  "A headline: its line in its level's face, its TODO keyword, priority and
tags in theirs, its title in :org-headline-done when its task is done, the
objects of its title."
  (let* ((text *org-text*)
         (begin (org-node-begin node))
         (line-end (string-line-end text begin))
         (level (org-property node :level))
         (keyword (org-property node :todo-keyword))
         (title-begin (org-property node :title-begin))
         (title-end (org-property node :title-end)))
    (org-colour begin line-end (org-level-face level))
    ;; an indented view, or bullets, hide all its stars but the last, as Org's org-hide-leading-stars, or show them faint
    (when (and (> level 1) (or (org-indented-p *org-buffer*) *org-bullets*))
      (org-colour begin (+ begin level -1) (if *org-hide-leading-stars* :org-hide :org-leading-stars)))
    (when (org-property node :archivedp)
      (org-colour begin line-end :org-archived))
    (when keyword
      (let ((at (search keyword text :start2 (+ begin level) :end2 line-end)))
        (when at
          (let ((done (eq (org-property node :todo-type) 'done)))
            (org-colour at (+ at (length keyword)) (org-todo-keyword-face keyword done))
            (when (and done title-begin) (org-colour title-begin line-end :org-headline-done))))))
    (when (org-property node :priority)
      (let ((at (search "[#" text :start2 (+ begin level) :end2 line-end)))
        (when at (org-colour at (+ at 4) :org-priority))))
    (when (org-property node :tags)
      (let ((at (and title-end (position #\: text :start title-end :end line-end))))
        (when at (org-colour at (1+ (or (position #\: text :end line-end :from-end t) at)) :org-tag))))
    (dolist (object (org-property node :title))
      (org-fontify object))))

(defparameter *org-info-keywords* '("TITLE" "SUBTITLE" "AUTHOR" "DATE" "EMAIL" "DESCRIPTION" "LANGUAGE")
  "The keywords that say what an Org document is: its key in
:org-document-info-keyword, its value as the document's own.")

(defun org-fontify-keyword (node)
  "A keyword line: an Org document's title, author and the like in their
faces, any other in :org-meta-line."
  (let* ((start (org-node-post-affiliated node))
         (end (org-content-end node))
         (key (org-property node :key))
         (colon (position #\: *org-text* :start start :end end)))
    (if (and colon (member key *org-info-keywords* :test #'string=))
        (progn (org-colour start (1+ colon) :org-document-info-keyword)
               (org-colour (1+ colon) end (if (string= key "TITLE") :org-document-title :org-document-info)))
        (org-colour start end :org-meta-line))))

(defun org-fontify-block (node body-face)
  "A block: its first and last lines in :org-block-begin-line and
:org-block-end-line, what is between in BODY-FACE (none when NIL)."
  (let* ((start (org-node-post-affiliated node))
         (first-end (min (length *org-text*) (1+ (string-line-end *org-text* start))))
         (content-end (org-content-end node))
         (last-start (org-line-start (max start (1- content-end)))))
    (org-colour start first-end :org-block-begin-line)
    (when (> last-start start)
      (when (and body-face (< first-end last-start))
        (org-colour first-end last-start body-face))
      (org-colour last-start content-end :org-block-end-line))))

(defun org-note-src-block (node)
  "Note NODE, a source block with a language, for its code to be coloured as that language."
  (let ((language (org-property node :language)))
    (when language
      (let* ((start (org-node-post-affiliated node))
             (from (min (length *org-text*) (1+ (string-line-end *org-text* start))))
             (to (org-line-start (max start (1- (org-content-end node))))))
        (when (< from to)
          (push (list language
                      (org-view-byte *org-buffer* *org-view* from)
                      (org-view-byte *org-buffer* *org-view* to))
                *org-src-blocks*))))))

(defun org-src-language (name)
  "The language of a source block given NAME, as *ORG-SRC-LANGUAGES* says, or NIL."
  (let ((alias (assoc name (setting-list '*org-src-languages* *org-src-languages* :pairs t) :test #'string-equal)))
    (if alias
        (and (cdr alias) (language-for-injection (cdr alias)))
        (or (language-for-injection name)
            (and (every (lambda (c) (or (alphanumericp c) (find c "-_+"))) name)
                 (language-for-injection (list :filename (format nil "x.~a" name))))))))

(defun org-src-highlights (buffer rope blocks first last)
  "The colours of the code of BLOCKS (as *ORG-SRC-BLOCKS* has them) in their
languages, from octet FIRST below LAST of ROPE, each language in its own
colouring style; a block's code is read as far as *INJECTION-REACH* around
what is shown."
  (loop for (name from to) in blocks
        for language = (org-src-language name)
        for start = (max first from)
        for end = (min last to)
        when (and language (< start end))
          append (multiple-value-bind (style level) (language-colouring language)
                   (unless (eq style :none)
                     (let* ((reach tuatara.syntax:*injection-reach*)
                            (window-from (tuatara.syntax:injection-cut rope (max from (- start reach))))
                            (window-to (tuatara.text:rope-char-start rope (min to (+ end reach)))))
                       (when (< window-from window-to)
                         ;; parsed by the program parsing this buffer's text, as its own
                         (let ((tuatara.syntax:*helper* (or (tuatara.syntax:buffer-helper-soon buffer) :none))
                               (tuatara.syntax:*parsing-buffer* buffer))
                           (tuatara.syntax:language-highlights (injection-resolver style level)
                                                               (symbol-name (language-name language))
                                                               (list (cons window-from window-to)) rope start end))))))))

(defun org-fontify-drawer (node)
  "A drawer: its first and last lines in :org-drawer; in an :EXPORT: drawer each
setting as a property is, its name in :org-special-keyword (in :error when it
names no setting, ORG-LATEX-SETTING-NAME-P) and its value in :org-property-value."
  (let* ((start (org-node-post-affiliated node))
         (content-end (org-content-end node))
         (last-start (org-line-start (max start (1- content-end)))))
    (org-colour start (string-line-end *org-text* start) :org-drawer)
    (when (> last-start start)
      (org-colour last-start content-end :org-drawer)
      (when (string-equal (org-property node :drawer-name) "EXPORT")
        (loop for line = (1+ (string-line-end *org-text* start)) then (1+ end)
              for end = (string-line-end *org-text* line)
              while (< line last-start)
              do (let ((colon (position #\: *org-text* :start line :end end)))
                   (when colon
                     (let ((name (string-trim " 	" (subseq *org-text* line colon))))
                       (org-colour line (1+ colon) (if (org-latex-setting-name-p name) :org-special-keyword :error))
                       (org-colour (1+ colon) end :org-property-value)))))))))

(defun org-fontify-node-property (node)
  "A property: its :KEY: in :org-special-keyword, its value in :org-property-value."
  (let* ((start (org-node-begin node))
         (end (string-line-end *org-text* start))
         (first (position #\: *org-text* :start start :end end))
         (second (and first (position #\: *org-text* :start (1+ first) :end end))))
    (when second
      (org-colour first (1+ second) :org-special-keyword)
      (org-colour (1+ second) end :org-property-value))))

(defun org-fontify-planning (node)
  "A planning or clock line: SCHEDULED: and DEADLINE: in the colours the
agenda gives them (:org-scheduled-keyword, :org-deadline-keyword), so that
they stand apart from the text under them, CLOSED: and CLOCK: in
:org-special-keyword; its timestamps in :org-date."
  (let* ((start (org-node-post-affiliated node))
         (end (string-line-end *org-text* start)))
    (cl-ppcre:do-matches (s e "(?:CLOSED|DEADLINE|SCHEDULED|CLOCK):" *org-text* nil :start start :end end)
      (org-colour s e (case (char *org-text* s)
                        (#\S :org-scheduled-keyword)
                        (#\D :org-deadline-keyword)
                        (t :org-special-keyword))))
    (cl-ppcre:do-matches (s e *org-one-timestamp-scanner* *org-text* nil :start start :end end)
      (org-colour s e :org-date))))

(defun org-fontify-table (node)
  "A table: its rows in :org-table, its formulas in :org-formula."
  (let ((rows-end (or (org-node-contents-end node) (org-content-end node))))
    (org-colour (org-node-post-affiliated node) rows-end :org-table)
    (when (< rows-end (org-content-end node))
      (org-colour rows-end (org-content-end node) :org-formula))))

(defun org-fontify-footnote-definition (node)
  "A footnote definition: its [fn:LABEL] in :org-footnote."
  (let* ((start (org-node-post-affiliated node))
         (close (position #\] *org-text* :start start :end (string-line-end *org-text* start))))
    (when close (org-colour start (1+ close) :org-footnote))))

(defun org-state-note-p (item)
  "True when ITEM, a list item, notes a change of a task's state, as a task's
log keeps it: - State \"DONE\" from \"TODO\" [2026-10-01 Thu 18:00]."
  (let* ((text *org-text*)
         (start (org-node-begin item))
         (end (string-line-end text start))
         (at (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) text :start start :end end) end))
         (mark "- State \""))
    (and (<= (+ at (length mark)) end)
         (string= mark text :start2 at :end2 (+ at (length mark))))))

(defun org-fontify-item (node)
  "An item: its checkbox in :org-checkbox, a description's term in :org-list-dt."
  (let* ((start (org-node-begin node))
         (end (string-line-end *org-text* start))
         (after-bullet (+ start (length (string-left-trim " 	" (org-property node :bullet)))
                          (- (length (org-line start)) (length (string-left-trim " 	" (org-line start)))))))
    (when (org-property node :checkbox)
      (let ((at (search "[" *org-text* :start2 after-bullet :end2 end)))
        (when at (org-colour at (+ at 3) :org-checkbox))))
    (let ((separator (search " ::" *org-text* :start2 after-bullet :end2 end)))
      (when (and separator (org-property node :tag))
        (org-colour (or (and (org-property node :checkbox) (let ((c (search "]" *org-text* :start2 after-bullet :end2 end))) (and c (+ c 2))))
                        after-bullet)
                    separator :org-list-dt)))))

(defun org-fontify-object (node)
  "An object in text: its face, over the whole of it."
  (let ((face (case (org-node-type node)
                (bold :org-bold) (italic :org-italic) (underline :org-underline)
                (code :org-code) (verbatim :org-verbatim)
                ((link radio-target) :org-link) (timestamp :org-date)
                ((latex-fragment entity) :org-latex-and-related)
                (macro :org-macro) (target :org-target)
                (footnote-reference :org-footnote) (citation :org-cite)
                (statistics-cookie :org-checkbox)
                ((inline-src-block inline-babel-call) :org-code)
                (export-snippet :org-meta-line))))
    (when face
      (org-colour (org-node-begin node) (- (org-node-end node) (org-node-post-blank node)) face))
    ;; what a call shows, between its parentheses, as a link's label shows
    (when (and (eq face :org-macro) (org-property node :args))
      (let ((open (+ (org-node-begin node) 3 (length (org-property node :key)) 1))
            (close (- (org-node-end node) (org-node-post-blank node) 4)))
        (when (< open close)
          (org-colour open close :org-macro-text))))))

(defmethod mode-own-colouring ((mode org-mode))
  "Org mode's colouring is its own, made from its parse."
  'org-mode)

(defun org-view-colours (buffer view first last style-level)
  "The colours of the Org text of BUFFER from octet FIRST below LAST, within
the part VIEW (an ORG-VIEW) knows, in the style STYLE-LEVEL (NIL rich, else
the sparse style's level): (START END FACE) in octets, in order, code in its
language's colours after them when the parse is of the text as it is now."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (text (org-view-text view))
         (from (max first (org-view-from view)))
         (to (min last (tuatara.text:rope-bytes rope) (org-view-to view)))
         (*org-buffer* buffer)
         (*org-colouring-level* style-level)
         (*org-text* text)
         (*org-view* view)
         (*org-ranges* '())
         (*org-src-blocks* '())
         ;; a parse of the text before a change, kept while the new one is made, is not past its end
         (*org-shown* (cons (min (length text) (org-view-char buffer view from))
                            (min (length text) (org-view-char buffer view to)))))
    (org-fontify (org-view-tree view))
    (let ((ranges (nreverse *org-ranges*)))
      (if (and *org-src-blocks* (not (eql style-level 0)) (org-view-current view))
          (append ranges (org-src-cached-highlights buffer rope from to))
          ranges))))

(defmethod mode-syntax-highlights ((mode org-mode) buffer rows)
  "The colours of the Org text of BUFFER in ROWS, the rows shown, from the
parses of the parts of it they show (ORG-VIEWS)."
  (let ((rope (tuatara.buffer:buffer-text buffer))
        (style-level (multiple-value-bind (style level) (mode-colouring mode)
                       (case style (:rich nil) (:none 0) (t level)))))
    (when (and (plusp (length rows)) (tuatara.text:rope-indexed-p rope))
      ;; only the text the rows show: what a fold or a search filter hides between them is not looked at
      (loop with bytes = (tuatara.text:rope-bytes rope)
            for (first . last) in (tuatara.display:shown-ranges rows :buffer buffer)
            nconc (loop for view in (org-views buffer first (min last bytes))
                        nconc (org-view-colours buffer view first last style-level))))))

(defun org-src-cached-highlights (buffer rope first last)
  "ORG-SRC-HIGHLIGHTS for *ORG-SRC-BLOCKS* of BUFFER from octet FIRST below
LAST, kept and given again while the text, the range, the blocks and the
rules compiled are as they were; the last eight ranges asked for are kept."
  (let* ((key (list (tuatara.buffer:buffer-version buffer) first last *org-src-blocks* (car tuatara.syntax:*rules-generation*)))
         (cache (tuatara.buffer:buffer-local buffer 'org-src-colours))
         (hit (assoc key cache :test #'equal)))
    (if hit
        (cdr hit)
        (let ((ranges (org-src-highlights buffer rope *org-src-blocks* first last)))
          (setf (tuatara.buffer:buffer-local buffer 'org-src-colours)
                (cons (cons key ranges) (subseq cache 0 (min 7 (length cache)))))
          ranges))))
