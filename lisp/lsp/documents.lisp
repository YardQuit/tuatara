;;;; lisp/lsp/documents.lisp --- Which buffers a language server sees, and keeping it in step with their text.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A language server runs the project's code and reads its settings: a build
;;;; script, a plugin, a macro. So it starts by itself only for a project you
;;;; trust (trust.lisp: M-x lsp asks, once, and remembers; *LSP-TRUSTED-PROJECTS*),
;;;; and never for a file opened safely, in *SAFE-DIRECTORIES*, the text of an
;;;; encrypted file, or one not all UTF-8. A buffer of a project that has one
;;;; is opened in it, and each change sent once typing pauses (*LSP-SYNC-DELAY*):
;;;; only what changed, to a server that takes changes so, else its whole
;;;; text. What a server says of a place is moved with the text typed
;;;; meanwhile (PLACED), so that nothing it marks jumps about or stands in the
;;;; wrong place.

(in-package #:tuatara.defaults)

(defvar *lsp-sync-delay* 0.5
  "The seconds after the last change to a buffer before its text is sent to its
language servers: what they find, hint at and colour is brought up to date
that long after typing stops.")

(defvar *lsp-largest-document* (* 8 1024 1024)
  "The largest buffer, in octets, given to a language server.")

(defun lsp-refusal (buffer)
  "Why BUFFER is never given to a language server, in words, or NIL: no file,
kept from other programs (OUTSIDE-PROGRAM-REFUSAL), too large, or not all
UTF-8, which the protocol sends."
  (let ((text (tuatara.buffer:buffer-text buffer)))
    (cond ((null (tuatara.buffer:buffer-file buffer)) "it has no file")
          ((outside-program-refusal buffer))
          ((> (tuatara.buffer:buffer-size buffer) *lsp-largest-document*) "it is larger than *lsp-largest-document*")
          ((not (tuatara.text:rope-indexed-p text)) "it is still being read")
          ((plusp (tuatara.text:rope-invalid text)) "it is not valid UTF-8"))))

(defun lsp-project-of (buffer language)
  "The top of the project BUFFER's file is in, for LANGUAGE's servers: as
FORMAT-PROJECT-DIRECTORY finds it, a native name ending in /."
  (tuatara.io:slash-ended (tuatara.io:native (format-project-directory buffer language))))

;;; Documents: what a server sees of a buffer

(defstruct (lsp-doc (:constructor make-lsp-doc (buffer language uri &optional shadow)) (:copier nil))
  "What servers of LANGUAGE see of BUFFER, by the address URI: its whole text,
or, with SHADOW, the code of its LANGUAGE alone (a note's source blocks):
SHADOW is then a function of the buffer returning that text, line for line
with the buffer (the lines of no such code empty), and for each of the
buffer's lines the octets left out at its start, NIL for a line not of it.
ROPE and SKIPPED keep those as last sent, positions being reckoned with them;
SENT, the buffer's text as last sent (a TUATARA.TEXT:VERSION). SERVERS, the
servers it is open in; VERSION, the number of the text last sent."
  buffer language uri shadow rope skipped sent (servers '()) (version 0))

(setf (documentation 'lsp-doc-p 'function) "True when OBJECT is an LSP-DOC.")

(defun buffer-docs (buffer)
  "What servers see of BUFFER, as LSP-DOCs."
  (tuatara.buffer:buffer-local buffer 'lsp-docs))

(defun buffer-servers (buffer)
  "The language servers BUFFER is open in, by any of its documents."
  (remove-duplicates (loop for doc in (buffer-docs buffer) append (lsp-doc-servers doc))))

(defun lsp-doc-of-uri (uri)
  "The document of an open buffer whose address is URI, or NIL."
  (loop for buffer in tuatara.buffer:*buffers*
        thereis (find uri (buffer-docs buffer) :key #'lsp-doc-uri :test #'equal)))

(defun uri-buffer (uri &key visit)
  "The buffer of what the address URI names, and its document named so (or
NIL), as two values: the buffer it is open in (a note, for its source
blocks), or the one visiting the file it names; with VISIT, the file opened
when no buffer has it, if there is such a file (none is made). NIL when there
is no such buffer."
  (let ((doc (lsp-doc-of-uri uri))
        (path (uri-path uri)))
    (cond (doc (values (lsp-doc-buffer doc) doc))
          ((null path) nil)
          ((buffer-visiting path))
          ((and visit (tuatara.io:path-stamp path))
           (let ((buffer (visit-path path :show #'identity)))
             ;; a file shown by a view of its own (a PDF) is no text to change or go to
             (and (typep buffer 'tuatara.buffer:buffer) buffer))))))

(defun uri-place (server uri position &key visit)
  "The buffer of what the address URI names (URI-BUFFER, VISIT as it takes
it) and its byte at POSITION, a position SERVER sent, as two values: the
start of the text when POSITION is none; NIL when there is no such buffer."
  (multiple-value-bind (buffer doc) (uri-buffer uri :visit visit)
    (when buffer
      (values buffer (min (tuatara.buffer:buffer-size buffer)
                          (or (if doc
                                  (doc-byte doc server position)
                                  (lsp-byte server (tuatara.buffer:buffer-text buffer) position))
                              0))))))

(defun lsp-doc-text (doc)
  "DOC's text as it is now, its ROPE, SKIPPED and SENT made from it."
  (let ((buffer (lsp-doc-buffer doc)))
    (setf (lsp-doc-sent doc) (tuatara.buffer:buffer-version buffer))
    (if (null (lsp-doc-shadow doc))
        (tuatara.text:rope-string (tuatara.buffer:buffer-text buffer))
        (multiple-value-bind (text skipped) (funcall (lsp-doc-shadow doc) buffer)
          (setf (lsp-doc-rope doc) (tuatara.text:string->rope text)
                (lsp-doc-skipped doc) skipped)
          text))))

(defun lsp-doc-sent-rope (doc)
  "DOC's buffer's text as it was last sent, which what its servers say is of."
  (let ((sent (lsp-doc-sent doc)))
    (if sent (tuatara.text:version-rope sent) (tuatara.buffer:buffer-text (lsp-doc-buffer doc)))))

(defun doc-skipped (doc line)
  "The octets left out at the start of the 0-based LINE of DOC's buffer in
what DOC shows of a note's source blocks; NIL for a line of no such code."
  (let ((skipped (lsp-doc-skipped doc)))
    (and skipped (< -1 line (length skipped)) (aref skipped line))))

(defun lsp-doc-covers-p (doc byte)
  "True when BYTE of DOC's buffer is in what DOC shows: anywhere for a whole
text, on a line of its code for a note's source blocks."
  (or (null (lsp-doc-shadow doc))
      (and (doc-skipped doc (tuatara.text:byte->line (tuatara.buffer:buffer-text (lsp-doc-buffer doc)) byte)) t)))

;;; Positions

(defun rope-line-text (rope line)
  "The text of the 0-based LINE of ROPE, without its line end; empty past its last line."
  (if (<= 0 line (tuatara.text:rope-lines rope))
      (let ((start (tuatara.text:line->byte rope line)))
        (tuatara.text:rope-string rope :start start :end (tuatara.text:rope-line-end rope start)))
      ""))

(defun line-end-byte (rope line)
  "The byte offset where the 0-based LINE of ROPE ends, before its line end;
its last line's end past it."
  (tuatara.text:rope-line-end rope (tuatara.text:line->byte rope (min line (tuatara.text:rope-lines rope)))))

(defun lsp-position (server rope byte)
  "BYTE of ROPE as a position SERVER counts: a JSON object of its 0-based line
and the characters before it on that line, in octets or UTF-16 units as the
server counts them."
  (let* ((line (tuatara.text:byte->line rope byte))
         (start (tuatara.text:line->byte rope line)))
    (tuatara.text:json-object "line" line
                              "character" (if (eq (lsp-server-encoding server) :utf-8)
                                              (- byte start)
                                              (- (tuatara.text:byte->utf16 rope byte) (tuatara.text:byte->utf16 rope start))))))

(defun position-line-character (position)
  "The 0-based line and the character of POSITION, a position a server sent,
as two values; NIL when it is none (either missing, or not a whole number)."
  (let ((line (tuatara.text:json-integer position "line"))
        (character (tuatara.text:json-integer position "character")))
    (and line character (values line character))))

(defun lsp-byte (server rope position)
  "The byte offset of ROPE at POSITION, a position SERVER sent (LSP-POSITION):
one past a line's end or the text's end kept to it; NIL when POSITION is none."
  (multiple-value-bind (line character) (position-line-character position)
    (and line (lsp-line-byte server rope line character))))

(defun lsp-line-byte (server rope line character)
  "The byte offset of ROPE at the 0-based LINE and CHARACTER as SERVER counts
them (LSP-BYTE): within that line, at the start of a character."
  (let* ((line (max 0 (min line (tuatara.text:rope-lines rope))))
         (start (tuatara.text:line->byte rope line))
         (end (tuatara.text:rope-line-end rope start))
         (character (max 0 character)))
    (if (eq (lsp-server-encoding server) :utf-8)
        (tuatara.text:rope-char-start rope (min end (+ start character)))
        (let ((unit (+ (tuatara.text:byte->utf16 rope start) character)))
          (if (>= unit (tuatara.text:byte->utf16 rope end))
              end
              (tuatara.text:utf16->byte rope unit))))))

(defun lsp-range (server rope start end)
  "The range of ROPE from byte START to END, as SERVER counts it."
  (tuatara.text:json-object "start" (lsp-position server rope start) "end" (lsp-position server rope end)))

(defun lsp-range-bytes (server rope range)
  "The byte offsets of ROPE where RANGE, as SERVER sent it, starts and ends, as
two values; NIL when it is no range."
  (let ((start (lsp-byte server rope (tuatara.text:json-get range "start")))
        (end (lsp-byte server rope (tuatara.text:json-get range "end"))))
    (and start end (values (min start end) end))))

(defun doc-position (doc server byte)
  "BYTE of DOC's buffer as a position of DOC for SERVER: the same for a whole
text; for a note's source blocks, on the same line, less the octets left out
at its start."
  (let ((rope (tuatara.buffer:buffer-text (lsp-doc-buffer doc))))
    (if (null (lsp-doc-shadow doc))
        (lsp-position server rope byte)
        (let* ((line (tuatara.text:byte->line rope byte))
               (shadow (lsp-doc-rope doc))
               (line (min line (tuatara.text:rope-lines shadow)))
               (skipped (or (doc-skipped doc line) 0))
               (column (max 0 (- byte (tuatara.text:line->byte rope (min line (tuatara.text:rope-lines rope))) skipped))))
          (lsp-position server shadow (min (line-end-byte shadow line) (+ (tuatara.text:line->byte shadow line) column)))))))

(defun doc-byte (doc server position &optional (bias :before))
  "The byte of DOC's buffer at POSITION, a position of DOC SERVER sent
\(DOC-POSITION), in the text as it was sent, moved to where the text typed
since has put it; BIAS :AFTER moves it past text typed right at it. NIL when
POSITION is none."
  (multiple-value-bind (line character) (position-line-character position)
    (and line (doc-line-byte doc server line character bias))))

(defun doc-line-byte (doc server line character &optional (bias :before))
  "The byte of DOC's buffer at the 0-based LINE and CHARACTER of DOC as SERVER
counts them, as DOC-BYTE finds it."
  (let* ((rope (lsp-doc-sent-rope doc))
         (at (if (null (lsp-doc-shadow doc))
                 (lsp-line-byte server rope line character)
                 (let* ((shadow (lsp-doc-rope doc))
                        (at (lsp-line-byte server shadow line character))
                        (line (tuatara.text:byte->line shadow at))
                        (column (- at (tuatara.text:line->byte shadow line)))
                        (line (min line (tuatara.text:rope-lines rope)))
                        (skipped (or (doc-skipped doc line) 0)))
                   (min (line-end-byte rope line) (+ (tuatara.text:line->byte rope line) skipped column)))))
         (sent (lsp-doc-sent doc))
         (now (tuatara.buffer:buffer-version (lsp-doc-buffer doc))))
    (if (and sent (not (eq sent now)))
        (tuatara.text:map-forward at sent now bias)
        at)))

(defun doc-range (doc server start end)
  "The range of DOC's buffer from START to END as SERVER counts it in DOC."
  (tuatara.text:json-object "start" (doc-position doc server start) "end" (doc-position doc server end)))

(defun doc-range-bytes (doc server range)
  "Where RANGE, a range of DOC SERVER sent, starts and ends in DOC's buffer, as
two values: text typed since at its start left out of it, as at its end; NIL
when it is no range."
  (let ((start (doc-byte doc server (tuatara.text:json-get range "start") :after))
        (end (doc-byte doc server (tuatara.text:json-get range "end"))))
    (and start end (values (min start end) end))))

;;; Places that follow the text

(defstruct (placed (:constructor make-placed (version items)) (:copier nil))
  "What a server said of places in a buffer, kept where the text typed since
puts them: ITEMS, a list or a vector of lists each beginning with the byte
offsets it starts and ends at in the text of VERSION (a
TUATARA.TEXT:VERSION), as PLACED-ITEMS-NOW moves them on; VERSIONS, once
items of a vector have been moved on one by one (PLACED-ITEM), the version
each item's offsets are in."
  version (items '()) (versions nil))

(setf (documentation 'placed-p 'function) "True when OBJECT is a PLACED.")

(defun move-placed-item (item then now)
  "Move ITEM's offsets from the text of version THEN to that of NOW: text
typed at its start or end left out of it, the text it covered deleted leaving
it empty where that was."
  (unless (eq then now)
    (let ((start (tuatara.text:map-forward (first item) then now :after))
          (end (tuatara.text:map-forward (second item) then now :before)))
      (setf (first item) (min start end) (second item) end)))
  item)

(defun placed-items-now (placed buffer)
  "PLACED's items, their places moved to where BUFFER's text as it is now has
them (MOVE-PLACED-ITEM). Complexity: O(items) when the text changed."
  (let ((now (tuatara.buffer:buffer-version buffer))
        (then (placed-version placed))
        (versions (placed-versions placed)))
    (cond (versions
           (loop for item across (placed-items placed)
                 for i from 0
                 do (move-placed-item item (svref versions i) now))
           (setf (placed-versions placed) nil))
          ((not (eq now then))
           (map nil (lambda (item) (move-placed-item item then now)) (placed-items placed))))
    (setf (placed-version placed) now)
    (placed-items placed)))

(defun placed-item (placed index buffer)
  "Item INDEX of PLACED, whose items are a vector, its places moved to where
BUFFER's text as it is now has them, the others left as they are until they
are wanted. Complexity: O(1) while the text is unchanged."
  (let ((now (tuatara.buffer:buffer-version buffer))
        (item (svref (placed-items placed) index)))
    (unless (and (eq now (placed-version placed)) (null (placed-versions placed)))
      (let ((versions (or (placed-versions placed)
                          (setf (placed-versions placed)
                                (make-array (length (placed-items placed)) :initial-element (placed-version placed))))))
        (unless (eq (svref versions index) now)
          (move-placed-item item (svref versions index) now)
          (setf (svref versions index) now))))
    item))

(defun placed-first (placed buffer test)
  "The index of the first of PLACED's items, a vector in order of place, that
TEST, a function of an item as BUFFER's text now has it, is true of, TEST
being false of all before it and true of all after; their number when none.
Only the items looked at are moved on (PLACED-ITEM). Complexity: O(log items)."
  (tuatara.runtime:bisect (mid 0 (length (placed-items placed)))
                          (not (funcall test (placed-item placed mid buffer)))))

(defun keep-placed (buffer local key items)
  "Keep ITEMS, what a server said of places in BUFFER as PLACED keeps them,
under KEY in the buffer's LOCAL list of (KEY . PLACED), in place of what was
kept so before; with no ITEMS, only that let go."
  (let ((others (remove key (tuatara.buffer:buffer-local buffer local) :key #'car :test #'equal)))
    (setf (tuatara.buffer:buffer-local buffer local)
          (if items (acons key (make-placed (tuatara.buffer:buffer-version buffer) items) others) others))))

(defun doc-unchanged-p (doc version)
  "True when DOC is still open in its buffer and its text is still that of
VERSION, the number of the text sent: an answer asked of it then still holds."
  (let ((buffer (lsp-doc-buffer doc)))
    (and (live-buffer-p buffer) (member doc (buffer-docs buffer)) (eql version (lsp-doc-version doc)))))

(defun doc-identifier (doc)
  "The JSON object naming DOC for a server."
  (tuatara.text:json-object "uri" (lsp-doc-uri doc)))

(defun doc-params (doc server byte)
  "The parameters naming BYTE of DOC's buffer for SERVER: DOC and the position."
  (tuatara.text:json-object "textDocument" (doc-identifier doc) "position" (doc-position doc server byte)))

;;; Opening, keeping in step, closing

(defun lsp-sync-kind (server)
  "How SERVER wants changes: 0 not at all, 1 the whole text, 2 only what changed."
  (let ((sync (tuatara.text:json-get (lsp-server-capabilities server) "textDocumentSync")))
    (if (integerp sync) sync (or (tuatara.text:json-get sync "change") 0))))

(defun lsp-open-document (server doc)
  "Tell SERVER that DOC is open, with its whole text."
  (lsp-notify server "textDocument/didOpen"
              (tuatara.text:json-object "textDocument"
                                        (tuatara.text:json-object "uri" (lsp-doc-uri doc)
                                                                  "languageId" (language-server-id (lsp-doc-language doc))
                                                                  "version" (incf (lsp-doc-version doc))
                                                                  "text" (lsp-doc-text doc))))
  (lsp-ask-all doc))

(defun lsp-ask-all (doc)
  "Ask DOC's servers for what they show of its text as it was last sent:
inlay hints, colours, code lenses and what they find wrong."
  (lsp-ask-hints doc)
  (lsp-ask-tokens doc)
  (lsp-ask-lenses doc)
  (lsp-pull-diagnostics doc))

;; a server that asks for what it shows again: asked of each of its documents
(dolist (entry '(("workspace/inlayHint/refresh" . lsp-ask-hints) ("workspace/semanticTokens/refresh" . lsp-ask-tokens)
                 ("workspace/codeLens/refresh" . lsp-ask-lenses) ("workspace/diagnostic/refresh" . lsp-pull-diagnostics)))
  (let ((ask (cdr entry)))
    (define-lsp-request (car entry) (server params)
      (mapc ask (lsp-server-documents server))
      :null)))

(defun lsp-attach (server doc)
  "Have DOC open in SERVER: told now when it is ready, else once it is; each
change of its buffer noted to be sent (LSP-NOTE-CHANGE)."
  (unless (member server (lsp-doc-servers doc))
    (let ((buffer (lsp-doc-buffer doc)))
      (push server (lsp-doc-servers doc))
      (push doc (lsp-server-documents server))
      (pushnew 'lsp-note-change (tuatara.buffer:buffer-change-functions buffer))
      (when (eq (lsp-server-status server) :ready)
        (lsp-open-document server doc)))))

(defun lsp-forget-document (server doc)
  "Have SERVER forget DOC: told it is closed, when it runs; what it found and hinted there let go."
  (when (member server (lsp-doc-servers doc))
    (when (eq (lsp-server-status server) :ready)
      (lsp-notify server "textDocument/didClose" (tuatara.text:json-object "textDocument" (doc-identifier doc))))
    (setf (lsp-doc-servers doc) (remove server (lsp-doc-servers doc))
          (lsp-server-documents server) (remove doc (lsp-server-documents server)))
    (let ((buffer (lsp-doc-buffer doc)))
      (lsp-forget-diagnostics buffer doc)
      (forget-hints buffer doc)
      (keep-placed buffer 'lsp-tokens doc nil)
      (unless (lsp-doc-servers doc)
        (setf (tuatara.buffer:buffer-local buffer 'lsp-docs) (remove doc (buffer-docs buffer))))
      (unless (buffer-docs buffer)
        (setf (tuatara.buffer:buffer-change-functions buffer)
              (remove 'lsp-note-change (tuatara.buffer:buffer-change-functions buffer)))))))

(defvar *lsp-most-changes* 1000
  "The most edits sent to a language server as changes of their own: past
that, the whole text is sent instead.")

(defun doc-changes (doc server)
  "What changed in DOC's text since it was last sent, as SERVER takes changes
one at a time (contentChanges with ranges), each edit's range in the text as
the edits before it left it; NIL when it is to be sent whole: a note's source
blocks, the whole text replaced at once, or more than *LSP-MOST-CHANGES* edits."
  (let ((sent (lsp-doc-sent doc))
        (now (tuatara.buffer:buffer-version (lsp-doc-buffer doc)))
        (changes '()) (count 0))
    (when (and sent (null (lsp-doc-shadow doc)))
      (loop until (eq sent now)
            do (let* ((next (tuatara.text:version-next sent))
                      (batch (and next (tuatara.text:version-batch next)))
                      (rope (tuatara.text:version-rope sent)))
                 (when (or (null batch) (tuatara.text:batch-swap batch))
                   (return-from doc-changes nil))
                 (let ((edits (tuatara.text:batch-edits batch)))
                   (when (> (incf count (length edits)) *lsp-most-changes*)
                     (return-from doc-changes nil))
                   ;; a batch's edits all count from the text before it: the last first, so
                   ;; that each leaves the places of those before it as they were
                   (loop for i from (1- (length edits)) downto 0
                         for e = (aref edits i)
                         do (push (tuatara.text:json-object
                                   "range" (lsp-range server rope (tuatara.text:edit-start e) (tuatara.text:edit-end e))
                                   "text" (if (tuatara.text:edit-insert e) (tuatara.text:octets-string (tuatara.text:edit-insert e)) ""))
                                  changes)))
                 (setf sent next)))
      (coerce (nreverse changes) 'simple-vector))))

(defvar *lsp-changed* '()
  "The buffers changed since their text was last sent to their servers, each (BUFFER . WHEN), WHEN in internal time.")

(defun lsp-note-change (buffer batch)
  "Note that BUFFER changed (BATCH, its edits), to be sent once typing pauses."
  (declare (ignore batch))
  (let ((entry (assoc buffer *lsp-changed*)))
    (if entry
        (setf (cdr entry) (get-internal-real-time))
        (push (cons buffer (get-internal-real-time)) *lsp-changed*))))

(defun lsp-send-changes (&key now)
  "Send the text of each buffer changed *LSP-SYNC-DELAY* seconds ago or more
\(or at once, with NOW) to its servers that want it."
  (let ((due (- (get-internal-real-time) (* *lsp-sync-delay* internal-time-units-per-second))))
    (dolist (entry *lsp-changed*)
      (destructuring-bind (buffer . when) entry
        (when (or now (<= when due) (not (live-buffer-p buffer)))
          (setf *lsp-changed* (remove entry *lsp-changed*))
          (when (live-buffer-p buffer)
            (lsp-send-text buffer)))))))

(defun lsp-changes-due-in ()
  "The seconds until the soonest change waiting in *LSP-CHANGED* is due to be
sent (*LSP-SYNC-DELAY* after it was made), or NIL when none waits."
  (when *lsp-changed*
    (let ((soonest (reduce #'min *lsp-changed* :key #'cdr)))
      (max 0.01 (- *lsp-sync-delay*
                   (/ (- (get-internal-real-time) soonest) internal-time-units-per-second))))))

(defun lsp-send-text (buffer)
  "Send what each of BUFFER's documents shows now to its servers that are
ready and want changes; a note's source blocks looked at again first, a
language's blocks added or gone opened or closed (LSP-CONNECT)."
  (when (lsp-doc-specs-change-p buffer)
    (lsp-connect buffer :start :trusted))
  (dolist (doc (buffer-docs buffer))
    (let ((servers (remove-if-not (lambda (s) (and (eq (lsp-server-status s) :ready) (plusp (lsp-sync-kind s))))
                                  (lsp-doc-servers doc))))
      (when servers
        (let* ((version (incf (lsp-doc-version doc)))
               ;; only what changed, to each server that takes it so; else the whole text, made once
               (changes (mapcar (lambda (s) (and (= 2 (lsp-sync-kind s)) (doc-changes doc s))) servers))
               (text (and (member nil changes) (lsp-doc-text doc))))
          (unless text
            (setf (lsp-doc-sent doc) (tuatara.buffer:buffer-version buffer)))
          (loop for server in servers
                for changed in changes
                do (lsp-notify server "textDocument/didChange"
                               (tuatara.text:json-object "textDocument" (tuatara.text:json-object "uri" (lsp-doc-uri doc) "version" version)
                                                         "contentChanges" (or changed (vector (tuatara.text:json-object "text" text))))))
          (lsp-ask-all doc))))))

(defun lsp-in-step (buffer)
  "Send BUFFER's text now when it changed since it was last sent, so that a
question about it is asked of the text as it is."
  (when (assoc buffer *lsp-changed*)
    (setf *lsp-changed* (remove buffer *lsp-changed* :key #'car))
    (lsp-send-text buffer)))

(defun lsp-saved (buffer)
  "Tell the servers of BUFFER's whole text it was saved."
  (dolist (doc (buffer-docs buffer))
    (unless (lsp-doc-shadow doc)
      (dolist (server (lsp-doc-servers doc))
        (when (eq (lsp-server-status server) :ready)
          (lsp-notify server "textDocument/didSave" (tuatara.text:json-object "textDocument" (doc-identifier doc))))))))

(defun lsp-killed (buffer)
  "BUFFER is being killed: its servers forget it."
  (dolist (doc (copy-list (buffer-docs buffer)))
    (dolist (server (copy-list (lsp-doc-servers doc)))
      (lsp-forget-document server doc))))

;;; Which documents and servers a buffer has

(defvar *lsp-doc-spec-functions* '()
  "Functions of a buffer giving what servers see of it instead of its whole
text, each a list of (LANGUAGE URI SHADOW), as an LSP-DOC is made from, or
NIL to leave it to the next: a note's source blocks.")

(defun lsp-doc-specs (buffer &key all)
  "What servers see of BUFFER, as a list of (LANGUAGE URI SHADOW): its source
blocks for a note (*LSP-DOC-SPEC-FUNCTIONS*), else its whole text in its
language; NIL for a buffer no server may see (LSP-REFUSAL). Those of a
language *LSP-LANGUAGES* leaves out are left out too, unless ALL."
  (when (null (lsp-refusal buffer))
    (remove-if-not (lambda (spec) (or all (lsp-language-on-p (first spec))))
                   (or (some (lambda (f) (funcall f buffer)) *lsp-doc-spec-functions*)
                       (let ((language (buffer-language buffer)))
                         (and language (list (list language (path-uri (tuatara.buffer:buffer-file buffer)) nil))))))))

(defun lsp-doc-specs-change-p (buffer)
  "True when what servers should see of BUFFER is no longer what they do (a
language's source blocks added to a note, or gone), nor what was last asked
of them for it: what no server took (a project not trusted) is asked again
only once it changes."
  (and *lsp-doc-spec-functions*
       (let ((wanted (sort (mapcar #'second (lsp-doc-specs buffer)) #'string<)))
         (and (not (equal wanted (sort (mapcar #'lsp-doc-uri (buffer-docs buffer)) #'string<)))
              (not (equal wanted (shiftf (tuatara.buffer:buffer-local buffer 'lsp-docs-asked) wanted)))))))

(defun lsp-servers-for (buffer language &key start)
  "The servers LANGUAGE has for the project of BUFFER, those running found,
and with START those not yet running started (with :TRUSTED, only for a
project trusted)."
  (let* ((root (lsp-project-of buffer language))
         (start (if (eq start :trusted) (project-trusted-p root) start)))
    (remove nil
            (mapcar (lambda (spec)
                      (or (lsp-server-for (getf spec :name) root)
                          (and start (installed-program (getf spec :command)) (lsp-start spec root language))))
                    (language-server-commands language)))))

(defun lsp-connect (buffer &key start)
  "Open what servers see of BUFFER (LSP-DOC-SPECS) in its languages' servers
for its project, starting those not running with START; what they no longer
see closed. Returns the servers."
  (let ((specs (lsp-doc-specs buffer)) (servers '()))
    ;; documents no longer wanted closed
    (dolist (doc (copy-list (buffer-docs buffer)))
      (unless (find (lsp-doc-uri doc) specs :key #'second :test #'equal)
        (dolist (server (copy-list (lsp-doc-servers doc)))
          (lsp-forget-document server doc))))
    (loop for (language uri shadow) in specs
          for running = (lsp-servers-for buffer language :start start)
          when running
            do (let ((doc (or (find uri (buffer-docs buffer) :key #'lsp-doc-uri :test #'equal)
                              (car (push (make-lsp-doc buffer language uri shadow) (tuatara.buffer:buffer-local buffer 'lsp-docs))))))
                 (dolist (server running)
                   (lsp-attach server doc)
                   (pushnew server servers))))
    servers))

(defun lsp-installed-servers (buffer)
  "The servers installed for what servers would see of BUFFER, by project:
a list of (ROOT LANGUAGE SPEC...)."
  (loop for (language) in (lsp-doc-specs buffer)
        for installed = (remove-if-not (lambda (s) (installed-program (getf s :command))) (language-server-commands language))
        when installed
          collect (list* (lsp-project-of buffer language) language installed)))

(defvar *lsp-offered* '()
  "The projects whose language servers \\[lsp] has been offered for this session, not to say so again.")

(defun lsp-visit (buffer)
  "As BUFFER's file is opened (its mode set by then): open it in its project's
language servers, started when the project is trusted; else, when one is
installed, say once that \\[lsp] starts them."
  (defer-action
   (lambda ()
     (when (live-buffer-p buffer)
       (dolist (entry (lsp-installed-servers buffer))
         (destructuring-bind (root language &rest installed) entry
           (declare (ignore language))
           (cond ((project-trusted-p root) (lsp-connect buffer :start t))
                 ((not (member root *lsp-offered* :test #'string=))
                  (push root *lsp-offered*)
                  (message "~{~a~^, ~} can serve ~a: ~a starts ~:[it~;them~] (~:*~:[it runs~;they run~] the project's code)"
                           (mapcar (lambda (s) (getf s :name)) installed) (abbreviated-path root)
                           (substitute-command-keys "\\[lsp]") (rest installed))))))
       ;; servers already running for its project see it too
       (lsp-connect buffer)))))

(defun lsp-unserved-words (buffer installed)
  "Why no language server can start for BUFFER, in words, INSTALLED being
its servers installed (LSP-INSTALLED-SERVERS); NIL when one can."
  (let ((specs (lsp-doc-specs buffer))
        (refusal (lsp-refusal buffer)))
    (cond (refusal (format nil "No language server for ~a: ~a" (tuatara.buffer:buffer-name buffer) refusal))
          ((and (null specs) (lsp-doc-specs buffer :all t))
           (format nil "Language servers are off for ~{~(~a~)~^, ~} (*lsp-languages*)"
                   (mapcar (lambda (spec) (language-name (first spec))) (lsp-doc-specs buffer :all t))))
          ((null specs) (format nil "~a has no language a server knows" (tuatara.buffer:buffer-name buffer)))
          ((null installed)
           (format nil "No language server is installed for ~{~(~a~)~^, ~}~@[: ~{~a~^, ~} would serve it~]"
                   (mapcar (lambda (spec) (language-name (first spec))) specs)
                   (loop for (language) in specs
                         append (mapcar (lambda (s) (first (getf s :command))) (language-server-commands language))))))))

(define-command lsp ()
  "Start the language servers of the current buffer's language for its project,
after asking whether you trust the project: a server runs the project's code
\(build scripts, macros, plugins). Trusted, its servers start by themselves
whenever one of its files is opened, in later sessions too; \\[lsp-forget-project]
takes that back. In a note, those of its source blocks' languages. A file
opened safely, in *safe-directories*, or the text of an encrypted file is
never given to a server, nor one that is not all UTF-8."
  (let* ((buffer (current-buffer))
         (installed (lsp-installed-servers buffer))
         (unserved (lsp-unserved-words buffer installed)))
    (if unserved
        (message "~a" unserved)
        (when (every #'trust-project
                     (remove-duplicates (mapcar #'first installed) :test #'string=))
          (let ((servers (lsp-connect buffer :start t)))
            (dolist (other tuatara.buffer:*buffers*)
              (unless (eq other buffer)
                (lsp-connect other)))
            (unless servers (message "No language server could be started")))))))

(define-command lsp-forget-project ()
  "Trust the current buffer's project no more: its language servers end, and
no longer start by themselves."
  (let* ((buffer (current-buffer))
         (roots (and (tuatara.buffer:buffer-file buffer)
                     (remove-duplicates (mapcar (lambda (spec) (lsp-project-of buffer (first spec))) (lsp-doc-specs buffer))
                                        :test #'string=))))
    (if (null roots)
        (message "~a is no file of a project" (tuatara.buffer:buffer-name buffer))
        (dolist (root roots)
          (state-list-drop *lsp-trusted* (list (or (real-folder root) root)))
          (dolist (server (copy-list *lsp-servers*))
            (when (string= (lsp-server-root server) root) (lsp-stop server)))
          (message "~a is trusted no more~:[~;; it is in *lsp-trusted-projects*, which still trusts it~]"
                   (abbreviated-path root) (project-trusted-p root))))))

(define-command lsp-shutdown ()
  "End the language servers the current buffer is open in; \\[lsp] starts them again."
  (let ((servers (buffer-servers (current-buffer))))
    (if (null servers)
        (message "~a is open in no language server" (tuatara.buffer:buffer-name (current-buffer)))
        (progn (mapc #'lsp-stop servers)
               (message "Ended ~{~a~^, ~}" (mapcar #'lsp-server-name servers))))))

(define-command lsp-restart ()
  "End the language servers the current buffer is open in and start them
again; in a project not trusted, none is started (\\[lsp] asks first)."
  (let ((buffer (current-buffer)))
    (mapc #'lsp-stop (buffer-servers buffer))
    (unless (lsp-connect buffer :start :trusted)
      (message "No language server started: M-x lsp starts them, asking first in a project not trusted"))))
