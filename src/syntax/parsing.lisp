;;;; src/syntax/parsing.lisp --- Keeping a buffer's syntax tree up to date.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Each buffer with a grammar keeps a syntax tree of its text. Parsing runs in
;;;; a thread of the buffer's own, so a large file never holds up typing: every edit is
;;;; passed at once to the tree, which moves its nodes along with the text, and
;;;; a parse started after the edit brings the tree up to date, reusing all it
;;;; can of the old one. Redisplay waits a moment for that parse, so the colours
;;;; of a small change are right in the very frame that shows it; a parse that
;;;; takes longer finishes in the background and has the editor redraw.

(in-package #:tuatara.syntax)

(defvar *syntax-size-limit* (* 64 1024 1024)
  "The largest text, in octets, that is parsed; a larger one shows no syntax colours.")

(defvar *syntax-wait* 0.02
  "Seconds redisplay waits for a parse started after an edit, before it shows
the text with the colours of the tree as it was. It does not wait at all while
parses after edits take longer than this, as they do in very large files: the
edit shows at once, and its colours a moment later.")

(defvar *syntax-edit-limit* 1000
  "The most edits of one change that are passed to the tree one by one; after a
change with more, the text is parsed afresh.")

(defvar *syntax-wake-function* nil
  "A function of no arguments that parsing threads call when a parse finishes,
to have the editor redraw; NIL for none.")

(defvar *syntax-fault-function* nil
  "A function of (SYNTAX) called in the editor's thread when a grammar's own
code failed while reading SYNTAX's text, to say so; NIL for none.")

(defvar *syntax-time-limit* 60
  "The seconds a parse of a text may take, and *SYNTAX-TIME-PER-MEGABYTE* more
for each megabyte of it; a parse that takes longer is given up, and its buffer
shows no syntax colours: a grammar can be very slow on text it was not made
for, such as binary data. NIL for no limit.")

(defvar *syntax-time-per-megabyte* 2
  "The seconds a parse may take for each megabyte of text, on top of
*SYNTAX-TIME-LIMIT*.")

(defvar *syntax-slow-function* nil
  "A function of (SYNTAX) called in the editor's thread when a parse of
SYNTAX's text took longer than *SYNTAX-TIME-LIMIT*, or more memory than
*SYNTAX-MEMORY-LIMIT*, and was given up, to say so; NIL for none.")

(defun parse-time-limit (bytes)
  "The seconds a parse of a text of BYTES octets may take (*SYNTAX-TIME-LIMIT*),
or NIL for no limit."
  (and *syntax-time-limit* (+ *syntax-time-limit* (* *syntax-time-per-megabyte* (/ bytes 1048576)))))

(defvar *syntax-memory-limit* (* 256 1024 1024)
  "The octets of memory a parse of a text may take, and
*SYNTAX-MEMORY-PER-OCTET* more for each octet of it, but never more than a
quarter of the machine's, nor more than half of what the program parsing
may still take (*SYNTAX-HELPER-MEMORY*); a parse that takes more is
given up, and its buffer shows no syntax colours: a grammar can go wrong on
a text and build without end, which would take all the memory there is. A
parse under way when memory runs short is given up too, and its text parsed
again at its next edit once memory is back. NIL for no limit.")

(defvar *syntax-memory-per-octet* 256
  "The octets of memory a parse may take for each octet of text, on top of
*SYNTAX-MEMORY-LIMIT*. A syntax tree takes some 15 (JSON) to 30 (C,
JavaScript) octets for each octet of its text, Markdown's 50, and up to 200
where every character or two is a token of its own (a long list of numbers).")

(defun parse-memory-limit (bytes)
  "The octets of memory a parse of a text of BYTES octets may take
(*SYNTAX-MEMORY-LIMIT*), or NIL for no limit. The program parsing holds it
to half of what it may still take, too (*SYNTAX-HELPER-MEMORY*), a parse
stopped by that then tried again once memory is back."
  (when *syntax-memory-limit*
    (let ((wanted (+ *syntax-memory-limit* (* *syntax-memory-per-octet* bytes)))
          (machine (physical-memory)))
      (if (plusp machine) (min wanted (floor machine 4)) wanted))))

(defstruct (parse-job (:constructor make-parse-job (rope old cancel wake)) (:copier nil))
  "One parse, run in its syntax's parse thread: of ROPE, from OLD (a copy of the tree before,
edited to match ROPE, or NIL), until it finishes or CANCEL (a foreign flag) is
set. RESULT is the new tree, or NIL; FINISHED is set when the parse is done;
OBSOLETE when the text changed too much for its tree to be of use. WAKE is
called when it finishes; STARTED and ENDED are when it began and ended, in
internal time units. FAULT is the condition when the grammar's own code failed
while reading; LIMIT is how many seconds the parse may take and BUDGET how
many octets of memory (NIL: no limit), SLOW :SLOW when it took longer (or
the program parsing was ended for it), :MEMORY when it took more and :SHORT
when memory ran short, and was given up. REFRESH is
set for a parse afresh of a text left alone (REFRESH-SYNTAX), which an edit
stops. SETTINGS holds the values the settings for starting a helper had
where the parse was asked for (HELPER-SETTINGS), for the thread parsing."
  (rope nil :read-only t)
  (settings (helper-settings) :read-only t)
  (old nil :read-only t)
  (cancel (null-pointer) :type sb-sys:system-area-pointer :read-only t)
  (wake nil :read-only t)
  (result nil)
  (finished nil)
  (obsolete nil)
  (started (get-internal-real-time))
  (ended nil)
  (fault nil)
  (limit nil)
  (budget nil)
  (slow nil)
  (refresh nil))

(setf (documentation 'make-parse-job 'function) "A PARSE-JOB of ROPE from tree OLD, stopped by the flag CANCEL, calling WAKE when done."
      (documentation 'parse-job-p 'function) "True when OBJECT is a PARSE-JOB."
      (documentation 'parse-job-rope 'function) "The text the job parses."
      (documentation 'parse-job-old 'function) "The job's copy of the tree it starts from, or NIL."
      (documentation 'parse-job-cancel 'function) "The foreign flag that stops the job's parse."
      (documentation 'parse-job-wake 'function) "The function the job calls when it finishes, or NIL."
      (documentation 'parse-job-result 'function) "The tree the job made, or NIL."
      (documentation 'parse-job-finished 'function) "True once the job's parse is done."
      (documentation 'parse-job-obsolete 'function) "True when the job's tree is no longer of use."
      (documentation 'parse-job-started 'function) "When the job started, in internal time units."
      (documentation 'parse-job-ended 'function) "When the job's parse ended, in internal time units, or NIL before."
      (documentation 'parse-job-fault 'function) "The condition when the grammar's own code failed while reading, or NIL."
      (documentation 'parse-job-limit 'function) "The seconds the job's parse may take, or NIL for no limit."
      (documentation 'parse-job-budget 'function) "The octets of memory the job's parse may take, or NIL for no limit."
      (documentation 'parse-job-slow 'function) "Why the job's parse was given up: :SLOW when it took too long, :MEMORY when it took too much memory, :SHORT when the editor's memory ran short; else NIL."
      (documentation 'parse-job-refresh 'function) "True when the job parses afresh a text left alone, which an edit stops.")

(defstruct (syntax (:constructor %make-syntax (buffer grammar parser)) (:copier nil))
  "The syntax tree of BUFFER's text in GRAMMAR, parsed by PARSER, both held
by HELPER, the program parsing for the editor (src/syntax/helper.lisp). TREE matches
the text ROPE, and is CURRENT when it was parsed from ROPE itself rather than
edited to match it. JOB is the parse asked for or running, EDITS the edits
made since it started (to apply to its tree), newest first; NEXT is that job
until the parse thread WORKER takes it up, and ENDING tells WORKER to end. GENERATION counts the trees, for
caches; INJECTED keeps the trees of the text in other languages parsed for
its colours (KEEP-INJECTED-TREES); BROKEN is set when the text could not be parsed, and TOO-BIG while the
text is larger than *SYNTAX-SIZE-LIMIT*. LAST-UPDATE is how long the last parse
after an edit took, in internal time units. EDITED is when the text was last
edited; PATCHED is set when the tree was brought up to date by parses after
edits since it was last parsed afresh, FRESH-COST how long that took, and
NO-REFRESH once a parse afresh of the text was given up (REFRESH-SYNTAX). LOCK
and DONE guard the job's result, NEXT and ENDING."
  (buffer nil :read-only t)
  (grammar nil :read-only t)
  (helper nil)
  (parser nil)
  (tree nil)
  (rope nil)
  (current nil)
  (job nil)
  (edits '())
  (generation 0 :type fixnum)
  (broken nil)
  (too-big nil)
  (last-update nil)
  (edited nil)
  (patched nil)
  (fresh-cost nil)
  (no-refresh nil)
  (cache nil)
  (injected nil)
  (worker nil)
  (next nil)
  (ending nil)
  (lock (sb-thread:make-mutex :name "syntax") :read-only t)
  (done (sb-thread:make-waitqueue :name "syntax") :read-only t))

(setf (documentation 'syntax-p 'function) "True when OBJECT is a SYNTAX."
      (documentation 'syntax-buffer 'function) "The buffer whose text the syntax tree is of."
      (documentation 'syntax-grammar 'function) "The grammar the text is parsed with."
      (documentation 'syntax-helper 'function) "The program parsing the text (a HELPER)."
      (documentation 'syntax-parser 'function) "The tree-sitter parser (a PARSER), or NIL while there is none."
      (documentation 'syntax-tree 'function) "The newest syntax tree, a foreign pointer, or NIL."
      (documentation 'syntax-rope 'function) "The text the tree matches."
      (documentation 'syntax-current 'function) "True when the tree was parsed from its text rather than edited to match it."
      (documentation 'syntax-job 'function) "The parse running, or NIL."
      (documentation 'syntax-edits 'function) "Edits made since the running parse began, newest first."
      (documentation 'syntax-generation 'function) "A number that changes whenever the tree does."
      (documentation 'syntax-broken 'function) "True when the text could not be parsed; :SLOW when parsing it took too long, :MEMORY when it took too much memory, :SHORT when the editor's memory ran short, the text then parsed again at its next edit once memory is back."
      (documentation 'syntax-too-big 'function) "True while the text is too large to parse."
      (documentation 'syntax-last-update 'function) "How long the last parse after an edit took, in internal time units, or NIL."
      (documentation 'syntax-edited 'function) "When the text was last edited, in internal time units, or NIL."
      (documentation 'syntax-patched 'function) "True when the tree was brought up to date by parses after edits since it was last parsed afresh."
      (documentation 'syntax-fresh-cost 'function) "How long the last parse of the text afresh took, in internal time units, or NIL."
      (documentation 'syntax-no-refresh 'function) "True once a parse afresh of the text was given up: it is not tried again."
      (documentation 'syntax-cache 'function) "The colours worked out lately, each with what it was for, as KEPT-COLOURS keeps them."
      (documentation 'syntax-injected 'function) "The trees of text in other languages kept for the colours, as (GENERATION . TABLE), or NIL."
      (documentation 'syntax-worker 'function) "The thread that parses the text, or NIL while none is needed."
      (documentation 'syntax-next 'function) "The parse handed to the parse thread and not yet taken up, or NIL."
      (documentation 'syntax-ending 'function) "True once the parse thread is to end."
      (documentation 'syntax-lock 'function) "The lock guarding the running parse's result and what the parse thread is given."
      (documentation 'syntax-done 'function) "Signalled when a parse finishes, and when the parse thread is given work.")

(defmethod print-object ((syntax syntax) stream)
  (print-unreadable-object (syntax stream :type t :identity t)
    (format stream "~a of ~a~:[~; parsing~]~:[~; broken~]~:[~; too big~]"
            (grammar-name (syntax-grammar syntax)) (tuatara.buffer:buffer-name (syntax-buffer syntax))
            (syntax-job syntax) (syntax-broken syntax) (syntax-too-big syntax))))

(defun buffer-syntax (buffer)
  "The SYNTAX of BUFFER, or NIL when its text is not parsed."
  (tuatara.buffer:buffer-local buffer :syntax))

;;; Edits

(defun point-at (rope byte)
  "The row of octet BYTE of ROPE and its column in octets, as a cons."
  (let ((row (tuatara.text:byte->line rope byte)))
    (cons row (- byte (tuatara.text:line->byte rope row)))))

(defun point-after (point octets)
  "The point after OCTETS inserted at POINT (a cons of row and column)."
  (let ((newlines (count 10 octets)))
    (if (zerop newlines)
        (cons (car point) (+ (cdr point) (length octets)))
        (cons (+ (car point) newlines) (- (length octets) 1 (position 10 octets :from-end t))))))

(defun edit-records (rope edits)
  "What EDITS (sorted edits of ROPE) tell a tree, one list (START OLD-END NEW-END
START-POINT OLD-END-POINT NEW-END-POINT) for each, last edit first, so that
each is in the offsets of the text the ones before it made."
  (loop for i from (1- (length edits)) downto 0
        for edit = (aref edits i)
        collect (let* ((start (tuatara.text:edit-start edit))
                       (old-end (tuatara.text:edit-end edit))
                       (insert (or (tuatara.text:edit-insert edit) #()))
                       (start-point (point-at rope start)))
                  (list start old-end (+ start (length insert))
                        start-point (point-at rope old-end) (point-after start-point insert)))))

(defun apply-edit-record (tree record)
  "Pass the edit RECORD (as EDIT-RECORDS makes) to TREE."
  (destructuring-bind (start old-end new-end start-point old-end-point new-end-point) record
    (edit-tree tree start old-end new-end start-point old-end-point new-end-point)))

(defun ensure-helper (syntax)
  "Make ready to parse SYNTAX's text: when its helper has ended (the trees
that one held are gone, so the text is then parsed afresh) or is no longer
the one for its buffer (a project you trust, or no longer), let go of it and
of all it held, so that the next parse starts another (RUN-PARSE-JOB), off
the editor's own thread."
  (let ((helper (syntax-helper syntax)))
    (when (and helper
               (or (not (helper-alive helper))
                   (not (eq (helper-shared helper) (buffer-text-trusted-p (syntax-buffer syntax))))
                   ;; never another than the buffer's own
                   (and (not (helper-shared helper))
                        (not (eq helper (tuatara.buffer:buffer-local (syntax-buffer syntax) :syntax-helper))))))
      (when (and (helper-alive helper) (helper-shared helper))
        (when (syntax-tree syntax) (delete-tree (syntax-tree syntax)))
        (delete-parser (syntax-parser syntax)))
      (when (syntax-tree syntax)
        (setf (syntax-tree syntax) nil (syntax-current syntax) nil)
        (incf (syntax-generation syntax)))
      (setf (syntax-injected syntax) nil
            (syntax-parser syntax) nil
            (syntax-edits syntax) '()
            (syntax-helper syntax) nil))))

(defun drop-tree (syntax)
  "Forget SYNTAX's tree, its edits and the running parse's result, so the text
is parsed afresh."
  (when (syntax-tree syntax)
    (delete-tree (syntax-tree syntax))
    (setf (syntax-tree syntax) nil))
  (let ((job (syntax-job syntax)))
    (when job
      (setf (parse-job-obsolete job) t
            (sb-sys:signed-sap-ref-32 (parse-job-cancel job) 0) 1)))
  (setf (syntax-edits syntax) '()
        (syntax-current syntax) nil)
  (incf (syntax-generation syntax)))

(defun syntax-change (buffer batch)
  "Tell BUFFER's syntax tree about BATCH, an edit just made: pass each edit to
the tree, keep them for the parse running, and ask for a new parse. A change
of the whole text, or of very many places, has the text parsed afresh. A text
whose parse was given up when memory ran short is parsed again once memory
is back."
  (let ((syntax (buffer-syntax buffer)))
    (when syntax
      (setf (syntax-edited syntax) (get-internal-real-time))
      ;; a parse afresh of the text as it was is of no more use, and would
      ;; hold up the parse this edit calls for
      (let ((job (syntax-job syntax)))
        (when (and job (parse-job-refresh job))
          (setf (parse-job-obsolete job) t
                (sb-sys:signed-sap-ref-32 (parse-job-cancel job) 0) 1)))
      (let ((old (syntax-rope syntax))
            (edits (tuatara.text:batch-edits batch)))
        (if (or (tuatara.text:batch-swap batch) (null old) (> (length edits) *syntax-edit-limit*)
                (not (tuatara.text:rope-indexed-p old)))
            (drop-tree syntax)
            (let ((records (edit-records old edits)))
              (when (syntax-tree syntax)
                (dolist (record records) (apply-edit-record (syntax-tree syntax) record)))
              (when (syntax-job syntax)
                (setf (syntax-edits syntax) (revappend records (syntax-edits syntax))))
              (setf (syntax-current syntax) nil)
              (incf (syntax-generation syntax))))
        (setf (syntax-rope syntax) (tuatara.buffer:buffer-text buffer))
        (when (and (eq (syntax-broken syntax) :short) (nth-value 1 (helper-memory (syntax-helper syntax))))
          (setf (syntax-broken syntax) nil))
        (request-parse syntax)))))

;;; Parsing

(defvar *parse-thread-idle* 10
  "Seconds a buffer's parse thread waits for the next parse before it ends,
to be started again by the next edit.")

(sb-ext:defglobal **parsers-let-go** (list 0)
  "How many parsers were let go, left half way when memory ran out, in a cons
for atomic counting.")

(defun parsers-let-go ()
  "How many parsers were let go this session, left half way when memory ran
out: the memory they hold comes back only when the editor starts again."
  (car **parsers-let-go**))

(defun parser-let-go (grammar what)
  "Count a parser of GRAMMAR, reading WHAT (a buffer's name, or words for
it), as let go, left half way when memory ran out, and say so in the debug
log."
  (let ((count (1+ (sb-ext:atomic-incf (car **parsers-let-go**)))))
    (tuatara.runtime:debug-report :parse "a ~a parser reading ~a let go, memory having run out within it (~d this session)"
                                  (grammar-name grammar) what count)))

(defun run-parse-job (syntax job)
  "Parse in this thread for JOB of SYNTAX, then record the result and call the
job's wake function. A failure inside the grammar's own code, or the program
parsing ending as it parsed, is recorded as the job's fault, for CATCH-UP to
deal with; the editor carries on."
  (let ((tree (handler-case
                  (progv *helper-setting-names* (parse-job-settings job)
                  (tuatara.runtime:reporting-errors ("parsing ~a with ~a" (tuatara.buffer:buffer-name (syntax-buffer syntax))
                                                     (grammar-name (syntax-grammar syntax)))
                    (multiple-value-bind (tree why unusable)
                        ;; the helper started here, not in the editor's own thread
                        (let* ((helper (or (syntax-helper syntax)
                                           (setf (syntax-helper syntax) (buffer-helper (syntax-buffer syntax)))))
                               (parser (and helper
                                            (or (syntax-parser syntax)
                                                (setf (syntax-parser syntax)
                                                      (make-parser (syntax-grammar syntax) helper))))))
                          (cond (parser
                                 (parse-rope parser (parse-job-rope job)
                                             :old (parse-job-old job) :cancel (parse-job-cancel job)
                                             :limit (parse-job-limit job) :budget (parse-job-budget job)))
                                (helper (values nil (helper-gone-why helper) t))
                                ;; no helper to be had (no sandbox): not parsed
                                (t (values nil nil t))))
                      (when (eq why :hung) (setf why :slow))
                      ;; its helper ended by the editor (for room): parsed again in another
                      (when (eq why :stopped) (setf (parse-job-obsolete job) t))
                      (when (member why '(:slow :memory :short)) (setf (parse-job-slow job) why))
                      (when (eq why :crashed) (setf (parse-job-fault job) :crashed))
                      ;; a parser left half way when memory ran out is let go;
                      ;; another is made once memory is back (REQUEST-PARSE)
                      (when (and unusable (eq why :short))
                        (parser-let-go (syntax-grammar syntax) (tuatara.buffer:buffer-name (syntax-buffer syntax))))
                      (when unusable (setf (syntax-parser syntax) nil))
                      (when (eq why :failed)
                        (setf (parse-job-fault job) (make-condition 'grammar-failure :grammar (syntax-grammar syntax))))
                      tree)))
                (error () nil))))
    (when (parse-job-old job) (delete-tree (parse-job-old job)))
    (sb-thread:with-mutex ((syntax-lock syntax))
      (setf (parse-job-result job) tree
            (parse-job-ended job) (get-internal-real-time)
            (parse-job-finished job) t)
      (sb-thread:condition-broadcast (syntax-done syntax))))
  (let ((wake (parse-job-wake job)))
    (when wake (ignore-errors (funcall wake)))))

(defun parse-thread-loop (syntax)
  "What SYNTAX's parse thread does: run each parse handed to it (SYNTAX-NEXT),
one after the other; end when told to (SYNTAX-ENDING) or when no parse came
for *PARSE-THREAD-IDLE* seconds."
  (loop
    (let ((job (sb-thread:with-mutex ((syntax-lock syntax))
                 (tuatara.runtime:wait-on-until (syntax-done syntax) (syntax-lock syntax)
                                                (lambda () (or (syntax-next syntax) (syntax-ending syntax)))
                                                *parse-thread-idle*)
                 (or (shiftf (syntax-next syntax) nil)
                     (progn (setf (syntax-worker syntax) nil)
                            (return))))))
      (run-parse-job syntax job))))

(defun hand-to-parse-thread (syntax job)
  "Have SYNTAX's parse thread run JOB, starting the thread when none is
waiting. True when handed over; NIL when no thread could be started."
  (sb-thread:with-mutex ((syntax-lock syntax))
    (let ((worker (syntax-worker syntax)))
      (unless (and worker (sb-thread:thread-alive-p worker))
        (setf (syntax-worker syntax)
              (ignore-errors (tuatara.runtime:make-guarded-thread #'parse-thread-loop :name "tuatara parse"
                                                                    :arguments (list syntax))))))
    (when (syntax-worker syntax)
      (setf (syntax-next syntax) job)
      (sb-thread:condition-broadcast (syntax-done syntax))
      t)))

(defun request-parse (syntax &key fresh)
  "Start parsing SYNTAX's text in its parse thread, unless a parse is running
(when it is done, CATCH-UP asks again), the tree is current, or the text cannot
or should not be parsed. With FRESH, parse it afresh, from no tree, though the
tree is current (REFRESH-SYNTAX)."
  (let ((rope (syntax-rope syntax)))
    (cond ((or (syntax-job syntax) (and (syntax-current syntax) (not fresh)) (syntax-broken syntax) (null rope)
               (grammar-faulted (syntax-grammar syntax))))
          ((> (tuatara.text:rope-bytes rope) *syntax-size-limit*)
           (unless (syntax-too-big syntax)
             (drop-tree syntax)
             (setf (syntax-too-big syntax) t)))
          (t
           (setf (syntax-too-big syntax) nil)
           (ensure-helper syntax)
           (let* ((cancel (sb-alien:alien-sap (sb-alien:make-alien (sb-alien:signed 32))))
                  (job (progn (setf (sb-sys:signed-sap-ref-32 cancel 0) 0)
                              (make-parse-job rope (and (not fresh) (syntax-tree syntax) (copy-tree-pointer (syntax-tree syntax)))
                                              cancel *syntax-wake-function*))))
             (setf (parse-job-refresh job) fresh)
             ;; worked out here, where the editor's settings are seen
             (setf (parse-job-limit job) (parse-time-limit (tuatara.text:rope-bytes rope)))
             (setf (parse-job-budget job) (parse-memory-limit (tuatara.text:rope-bytes rope)))
             (if (hand-to-parse-thread syntax job)
                 (setf (syntax-job syntax) job
                       (syntax-edits syntax) '())
                 ;; no thread to parse in: give up on this text
                 (progn (when (parse-job-old job) (delete-tree (parse-job-old job)))
                        (sb-alien:free-alien (sb-alien:sap-alien cancel (* (sb-alien:signed 32))))
                        (setf (syntax-broken syntax) t))))))))

(defun finish-job (job)
  "Release what the parse JOB holds once it is done; return its tree unless
the tree is of no use, in which case it is released too."
  (sb-alien:free-alien (sb-alien:sap-alien (parse-job-cancel job) (* (sb-alien:signed 32))))
  (let ((tree (parse-job-result job)))
    (if (and tree (parse-job-obsolete job))
        (progn (delete-tree tree) nil)
        tree)))

(defun job-finished-p (syntax job)
  "True when JOB of SYNTAX has finished."
  (sb-thread:with-mutex ((syntax-lock syntax))
    (parse-job-finished job)))

(defun catch-up (syntax)
  "Take the result of SYNTAX's parse when it has finished: bring the new tree up
to date with the edits made meanwhile and make it SYNTAX's tree. Then start
another parse if the tree is not current."
  (let ((job (syntax-job syntax)))
    (when (and job (job-finished-p syntax job))
      (setf (syntax-job syntax) nil)
      (let ((tree (finish-job job))
            (edits (reverse (shiftf (syntax-edits syntax) '()))))
        (tuatara.runtime:debug-report
         :parse "~a in ~a: ~:d octets ~a in ~,1f ms~a"
         (grammar-name (syntax-grammar syntax)) (tuatara.buffer:buffer-name (syntax-buffer syntax))
         (tuatara.text:rope-bytes (parse-job-rope job))
         (cond ((parse-job-refresh job) "parsed afresh, left alone") ((parse-job-old job) "parsed after edits") (t "parsed"))
         (/ (* 1000.0 (- (or (parse-job-ended job) (get-internal-real-time)) (parse-job-started job)))
            internal-time-units-per-second)
         (cond (tree (if edits (format nil ", ~d edit~:p made meanwhile" (length edits)) ""))
               ((parse-job-fault job) (format nil ", the grammar failed: ~a" (parse-job-fault job)))
               ((parse-job-slow job) (format nil ", given up (~(~a~))" (parse-job-slow job)))
               ((parse-job-obsolete job) ", of no use: the text changed")
               (t ", no tree")))
        (cond (tree
               ;; how long updating the tree takes, as opposed to parsing afresh
               (if (parse-job-old job)
                   (setf (syntax-last-update syntax) (- (parse-job-ended job) (parse-job-started job))
                         (syntax-patched syntax) t)
                   (setf (syntax-fresh-cost syntax) (- (parse-job-ended job) (parse-job-started job))
                         (syntax-patched syntax) nil))
               (dolist (record edits) (apply-edit-record tree record))
               (when (syntax-tree syntax) (delete-tree (syntax-tree syntax)))
               (setf (syntax-tree syntax) tree
                     (syntax-current syntax) (null edits))
               (incf (syntax-generation syntax)))
              ((parse-job-fault job) (grammar-failed syntax (parse-job-fault job)))
              ((and (parse-job-refresh job) (not (eq (parse-job-slow job) :short)))
               ;; a parse afresh given up (too slow, too large) leaves the
               ;; tree as it was, and is not tried again
               (unless (parse-job-obsolete job)
                 (setf (syntax-no-refresh syntax) t)))
              ((parse-job-slow job)
               ;; not parsed again, which would take as long, or as much
               (setf (syntax-broken syntax) (parse-job-slow job))
               (when *syntax-slow-function*
                 (ignore-errors (funcall *syntax-slow-function* syntax))))
              ((not (parse-job-obsolete job))
               ;; not stopped on purpose: the text could not be read
               (setf (syntax-broken syntax) t)))))
    (request-parse syntax)))

(defvar *syntax-refresh-delay* 1
  "The seconds a text is left alone, at least, before it is parsed afresh in
the background: a parse after an edit reuses the tree from before, and some
grammars then get a part of the text wrong, which a parse from nothing puts
right. A text is left alone ten times as long as its last parse from nothing
took, too, so that a large one is parsed afresh seldom. NIL for never.")

(defun refresh-syntax (syntax)
  "Parse SYNTAX's text afresh in the background when its tree was brought up
to date by parses after edits and the text has been left alone long enough
(*SYNTAX-REFRESH-DELAY*); an edit stops that parse, and one given up leaves
the tree as it was. Returns the seconds until it is worth asking again, or
NIL when there is nothing to do until the text is edited. A text whose
helper went wrong and ended is parsed afresh at once, in a new one; one whose
helper was ended for room, when it is next shown."
  (when (and (syntax-helper syntax) (not (helper-alive (syntax-helper syntax)))
             (not (eq (helper-ended (syntax-helper syntax)) :stopped))
             (null (syntax-job syntax)) (not (syntax-broken syntax)) (not (grammar-faulted (syntax-grammar syntax))))
    (request-parse syntax :fresh t)
    (return-from refresh-syntax nil))
  (when (and *syntax-refresh-delay* (syntax-patched syntax) (not (syntax-no-refresh syntax))
             (null (syntax-job syntax)) (syntax-current syntax) (syntax-tree syntax)
             (not (syntax-broken syntax)) (not (grammar-faulted (syntax-grammar syntax))))
    (let ((delay (max *syntax-refresh-delay*
                      (* 10 (/ (or (syntax-fresh-cost syntax) 0) internal-time-units-per-second))))
          (idle (tuatara.runtime:seconds-since (or (syntax-edited syntax) 0))))
      (if (>= idle delay)
          (progn (request-parse syntax :fresh t) nil)
          (float (- delay idle))))))

(defun grammar-failed (syntax fault)
  "Turn SYNTAX's grammar off for SYNTAX's text after its own code failed
while reading it, FAULT saying how, and for all text once it has gone wrong
on several (GRAMMAR-WENT-WRONG): SYNTAX stops parsing, keeping the tree it
has; its parser is deleted when the grammar only gave it a value it cannot
have (a GRAMMAR-FAILURE); and *SYNTAX-FAULT-FUNCTION* says so."
  (when (typep fault 'grammar-failure)
    (delete-parser (syntax-parser syntax)))
  (grammar-went-wrong (syntax-grammar syntax) (syntax-buffer syntax) fault)
  (setf (syntax-broken syntax) t
        (syntax-parser syntax) nil)
  (when *syntax-fault-function*
    (ignore-errors (funcall *syntax-fault-function* syntax))))

(defun parse-under-way-p (syntax &optional (seconds 0))
  "True when a parse of SYNTAX's text is under way, after waiting at most
SECONDS for it to finish; SYNTAX-READY-TREE then gives the tree it made."
  (let ((job (syntax-job syntax)))
    (and job
         (sb-thread:with-mutex ((syntax-lock syntax))
           (not (tuatara.runtime:wait-on-until (syntax-done syntax) (syntax-lock syntax)
                                               (lambda () (parse-job-finished job)) seconds))))))

(defun wait-for-parse (syntax)
  "Catch up with SYNTAX's parse, first waiting for it to finish: at most
*SYNTAX-WAIT* seconds from now, and not past that long after it began; not
at all while updates of the tree take longer than that, nor for a parse
afresh of a text left alone (REFRESH-SYNTAX), whose tree is current already."
  (catch-up syntax)
  (loop with wait = (* *syntax-wait* internal-time-units-per-second)
        with slow = (let ((last (syntax-last-update syntax))) (and last (> last wait)))
        with deadline = (+ (get-internal-real-time) wait)
        for job = (syntax-job syntax)
        while (and job (not slow) (not (parse-job-refresh job)))
        do (let ((limit (min deadline (+ (parse-job-started job) wait))))
             ;; a wait may end early, without the job finished: wait again until the limit
             (sb-thread:with-mutex ((syntax-lock syntax))
               (tuatara.runtime:wait-on-until (syntax-done syntax) (syntax-lock syntax) (lambda () (parse-job-finished job))
                                              (max 0 (tuatara.runtime:seconds-left limit))))
             (unless (job-finished-p syntax job) (return))
             (catch-up syntax))))

;;; Attaching to buffers

(defun attach-syntax (buffer grammar)
  "Keep a syntax tree of BUFFER's text in GRAMMAR from now on, parsing it at
once in the background. Returns the SYNTAX; NIL when GRAMMAR has failed and
reads nothing more."
  (detach-syntax buffer)
  (when (grammar-faulted grammar)
    (return-from attach-syntax nil))
  ;; the grammar went wrong on this buffer's text before
  (when (grammar-off-p grammar buffer)
    (return-from attach-syntax nil))
  ;; its helper started now, or as it is first parsed (RUN-PARSE-JOB) when
  ;; the sandbox is still to be looked for
  (let* ((syntax (%make-syntax buffer grammar nil)))
    (setf (syntax-helper syntax) (buffer-helper-soon buffer))
    (setf (syntax-rope syntax) (tuatara.buffer:buffer-text buffer)
          (tuatara.buffer:buffer-local buffer :syntax) syntax)
    (pushnew 'syntax-change (tuatara.buffer:buffer-change-functions buffer))
    (request-parse syntax)
    syntax))

(defun detach-syntax (buffer)
  "Stop keeping a syntax tree of BUFFER's text: stop the parse running, end
the parse thread and release the tree and the parser. A parse that does not stop in time
(TUATARA.RUNTIME:*STOP-WAIT*, a grammar that loops) is left to end on its own:
the parser and the trees it may be reading are then never released, and
BUFFER's file stays mapped for it (BUFFER-LENT-P)."
  (let ((syntax (buffer-syntax buffer))
        (running nil))
    (when syntax
      (setf (tuatara.buffer:buffer-local buffer :syntax) nil
            (tuatara.buffer:buffer-change-functions buffer)
            (remove 'syntax-change (tuatara.buffer:buffer-change-functions buffer)))
      (let ((job (shiftf (syntax-job syntax) nil))
            (worker (sb-thread:with-mutex ((syntax-lock syntax))
                      (setf (syntax-ending syntax) t)
                      (sb-thread:condition-broadcast (syntax-done syntax))
                      (syntax-worker syntax))))
        (when job
          (setf (parse-job-obsolete job) t
                (sb-sys:signed-sap-ref-32 (parse-job-cancel job) 0) 1))
        (cond ((tuatara.runtime:join-within worker)
               (when job (finish-job job)))
              (job
               (setf running t
                     (tuatara.buffer:buffer-lent-p buffer) t)
               (tuatara.runtime:left-running worker "A parse"))))
      (when (and (syntax-tree syntax) (not running))
        (delete-tree (syntax-tree syntax)))
      (forget-injected-trees syntax)
      (setf (syntax-tree syntax) nil)
      ;; a parser left alone after its grammar failed is not touched again
      (unless (or running (null (syntax-parser syntax)))
        (delete-parser (syntax-parser syntax)))
      (setf (syntax-parser syntax) nil))
    nil))

(defun syntax-ready-tree (syntax)
  "SYNTAX's tree for showing its buffer's text now, after waiting a moment for
a parse under way; NIL when there is none yet. The tree matches the buffer's
text, though parts just edited may not be parsed yet. A text whose helper
was ended is parsed afresh, in another."
  (let ((helper (syntax-helper syntax)))
    (when (and helper (not (helper-alive helper)) (null (syntax-job syntax)))
      (request-parse syntax :fresh t)))
  (if (eq (syntax-rope syntax) (tuatara.buffer:buffer-text (syntax-buffer syntax)))
      (wait-for-parse syntax)
      ;; the text changed without the tree being told: start afresh
      (progn (drop-tree syntax)
             (setf (syntax-rope syntax) (tuatara.buffer:buffer-text (syntax-buffer syntax)))
             (request-parse syntax)))
  (syntax-tree syntax))
