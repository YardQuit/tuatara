;;;; lisp/lsp/features.lisp --- What language servers do for a buffer: completion, definitions, help, renames, fixes, layout.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; In a buffer open in a language server, M-x completion-at-point offers what
;;;; the server offers, M-x xref-find-definitions goes where it says a name is
;;;; defined, M-x lsp-find-references lists where it is used, M-x lsp-describe-thing-at-point
;;;; shows what the server knows of it, the echo area shows the parameters of
;;;; the call the cursor rests in, else what the thing under it is, its other
;;;; uses in the file are shown in :symbol-highlight, M-x lsp-rename renames it everywhere,
;;;; M-x lsp-code-actions offers the server's fixes, and M-x format-buffer and
;;;; M-x format-region lay out code as the server does. Of several servers for
;;;; one language, each does what the language leaves to it.

(in-package #:tuatara.defaults)

(defvar *lsp-signature-help* t
  "True for the echo area to show the parameters of the call the cursor is in,
as the language server knows them, once the cursor rests there.")

(defvar *lsp-hover-echo* t
  "True for the echo area to show what the thing the cursor rests on is (its
type, its signature), as the language server knows it, when the cursor is in
no call whose parameters are shown.")

(defvar *lsp-highlight-symbol* t
  "True for the name the cursor rests on, and its other uses the language
server finds in the file, to be shown in :symbol-highlight.")

(defvar *lsp-idle-delay* 0.5
  "The seconds the cursor rests before what is known of where it stands is
shown: what a server found wrong there, the parameters of the call, what the
thing is.")

(defun lsp-feature-allowed-p (server feature)
  "True when SERVER may serve FEATURE (\"completion\") for its language: not
left to another of the language's servers (the :ONLY and :EXCEPT of
LANGUAGE-TOOLS)."
  (let* ((tools (language-tools (lsp-server-language server)))
         (only (cdr (assoc (lsp-server-name server) (getf tools :only) :test #'string=)))
         (except (cdr (assoc (lsp-server-name server) (getf tools :except) :test #'string=))))
    (and (or (null only) (member feature only :test #'string=))
         (not (member feature except :test #'string=)))))

(defun server-offers-p (server capability &optional feature)
  "True when SERVER is ready, offers CAPABILITY (\"hoverProvider\") and may serve FEATURE."
  (and (eq (lsp-server-status server) :ready)
       (let ((offered (tuatara.text:json-get (lsp-server-capabilities server) capability)))
         (and offered (not (eq offered :false))))
       (or (null feature) (lsp-feature-allowed-p server feature))))

(defun doc-server-offers (doc capability &optional feature)
  "The first server DOC is open in that offers CAPABILITY and may serve FEATURE, or NIL."
  (find-if (lambda (server) (server-offers-p server capability feature)) (lsp-doc-servers doc)))

(defun lsp-doc-at (buffer position capability &optional feature)
  "The document of BUFFER that shows POSITION and has a server offering
CAPABILITY for FEATURE, and that server, as two values; NIL when there is none."
  (dolist (doc (buffer-docs buffer) nil)
    (when (lsp-doc-covers-p doc position)
      (let ((server (doc-server-offers doc capability feature)))
        (when server (return (values doc server)))))))

(defun lsp-server-offers (buffer capability &optional feature)
  "The document of BUFFER's whole text and its server that offers CAPABILITY
for FEATURE, as two values, as LSP-DOC-AT gives them; NIL for a buffer whose
whole text no server sees (a note: its blocks are seen apart)."
  (let ((doc (find-if-not #'lsp-doc-shadow (buffer-docs buffer))))
    (when doc
      (let ((server (doc-server-offers doc capability feature)))
        (and server (values doc server))))))

(defun markup-source (contents)
  "CONTENTS, what a server says of a thing (a string, MarkupContent, MarkedString
or a list of them), as one Markdown text: a MarkedString's code in a fence."
  (typecase contents
    (string contents)
    (hash-table (let ((value (or (tuatara.text:json-get contents "value") ""))
                      (language (tuatara.text:json-get contents "language")))
                  (if language (format nil "```~a~%~a~%```" language value) value)))
    (vector (format nil "~{~a~^~%~%~}" (map 'list #'markup-source contents)))
    (t "")))

(defun markup-text (contents)
  "CONTENTS, what a server says of a thing (MARKUP-SOURCE), as plain text: code
fences taken out, blank lines kept single."
  (let ((text (markup-source contents)))
    (string-trim '(#\Newline #\Space)
                 (cl-ppcre:regex-replace-all "\\n{3,}" (cl-ppcre:regex-replace-all "(?m)^```\\w*\\s*$" text "") (format nil "~%~%")))))

;;; Completion

(defun lsp-word-char-p (char)
  "True when CHAR is of a name as servers' completions and the names asked
about are taken: a letter, a digit or _."
  (and char (or (alphanumericp char) (char= char #\_))))

(defun lsp-word-bounds (buffer position)
  "Where the name at POSITION of BUFFER (LSP-WORD-CHAR-P) starts and ends, as
two values (WORD-BOUNDS-AT); both POSITION when there is none."
  (word-bounds-at buffer position #'lsp-word-char-p))

(defun lsp-word-at (buffer position)
  "The name at POSITION of BUFFER (LSP-WORD-BOUNDS), or NIL."
  (multiple-value-bind (start end) (lsp-word-bounds buffer position)
    (and (< start end) (tuatara.buffer:buffer-string buffer :start start :end end))))

(defun lsp-resolve-completion (server item)
  "Ask SERVER, once, for what it leaves out of ITEM, a completion it offered,
until it is chosen (its documentation, what it is); put in ITEM once it comes,
for the box beside the list to show."
  (when (and (not (tuatara.text:json-get item "tuatara-asked"))
             (tuatara.text:json-true-p (tuatara.text:json-get (lsp-server-capabilities server) "completionProvider" "resolveProvider")))
    (lsp-request server "completionItem/resolve" item
                 (lambda (result error)
                   (when (and (null error) (hash-table-p result))
                     (dolist (key '("documentation" "detail"))
                       (let ((value (tuatara.text:json-get result key)))
                         (when value (setf (gethash key item) value)))))))
    ;; noted once sent, so that the server never sees it
    (setf (gethash "tuatara-asked" item) t)))

(defun completion-item-insert (item)
  "What ITEM, a completion a server offers, puts in: its edit's text, its text,
or its label."
  (or (tuatara.text:json-text item "textEdit" "newText") (tuatara.text:json-text item "insertText") (tuatara.text:json-text item "label") ""))

(defun lsp-completion-item-text (item)
  "What ITEM, a completion a server offers, is matched by and shown as: what
it puts in; for a snippet (put in by LSP-SNIPPET-INSERTED), its label."
  (if (snippet-item-p item)
      (or (tuatara.text:json-text item "filterText") (tuatara.text:json-text item "label") "")
      (completion-item-insert item)))

(defun snippet-item-p (item)
  "True when ITEM, a completion a server offers, is a snippet: text with fields to fill in."
  (and (hash-table-p item) (eql 2 (tuatara.text:json-get item "insertTextFormat"))))

(defun lsp-snippet-inserted (buffer candidate start end)
  "After CANDIDATE was put in BUFFER from START below END: when it is a
server's snippet, the snippet in its place, its fields to fill in."
  (let ((item (candidate-value candidate)))
    (when (snippet-item-p item)
      (put-snippet buffer start end (make-snippet (tuatara.text:json-text item "label") nil (completion-item-insert item))))))

(defun completion-items (result)
  "The completions RESULT, a server's answer, offers, as a list: a list's
items, or an array of them; any that is no object left out."
  (remove-if-not #'hash-table-p (coerce (if (hash-table-p result) (tuatara.text:json-items result "items") (tuatara.text:json-items result)) 'list)))

(defun lsp-completion-start (buffer doc server items position)
  "Where the text completed at POSITION of BUFFER begins: where the first of
ITEMS, SERVER's completions for DOC, that says so has its edit begin, or the
start of the name there, whichever comes first."
  (let ((edit-start (loop for item in items
                          for range = (or (tuatara.text:json-get item "textEdit" "range")
                                          (tuatara.text:json-get item "textEdit" "insert"))
                          for at = (and range (doc-byte doc server (tuatara.text:json-get range "start")))
                          when at return at)))
    (min position (or edit-start position) (lsp-word-bounds buffer position))))

(defun lsp-completion-annotation (candidate)
  "What is said beside CANDIDATE, a server's completion: what it is, else its
label when that is not what it puts in."
  (let ((item (candidate-value candidate)))
    (format nil "~@[~a~]" (or (tuatara.text:json-text item "detail")
                              (let ((label (tuatara.text:json-text item "label")))
                                (and label (string/= label (candidate-string candidate)) label))))))

(defun lsp-completion-at-point (buffer position)
  "What a server offers to complete at POSITION of BUFFER, as
MODE-COMPLETION-AT-POINT gives it: the word's start and end, the candidates,
and the functions giving what is said beside each and its documentation; NIL
when no server completes there, or does not answer in time."
  (lsp-in-step buffer)
  (multiple-value-bind (doc server) (lsp-doc-at buffer position "completionProvider" "completion")
    (when server
      (let ((items (completion-items (lsp-request-now server "textDocument/completion" (doc-params doc server position)))))
        (when items
          (values (lsp-completion-start buffer doc server items position) position
                  (mapcar (lambda (item) (make-candidate (lsp-completion-item-text item) :value item)) items)
                  #'lsp-completion-annotation
                  (lambda (candidate)
                    (let ((item (candidate-value candidate)))
                      (unless (tuatara.text:json-get item "documentation")
                        (lsp-resolve-completion server item))
                      (markup-text (tuatara.text:json-get item "documentation"))))
                  #'lsp-word-char-p))))))

(defmethod mode-completion-at-point :around (mode buffer position)
  "In a buffer open in a language server that completes, what the server offers; else what the mode offers."
  (multiple-value-bind (start end candidates annotate document constituent)
      (and (buffer-servers buffer) (lsp-completion-at-point buffer position))
    (if start
        (values start end candidates annotate document constituent)
        (call-next-method))))

;;; Definitions and references

(defvar *location-files* nil
  "While a server's places are made DEFINITIONs, the lines of the files read
for them, by path, each file read once; NIL otherwise.")

(defvar *lsp-location-file-largest* (* 2 1024 1024)
  "The largest file, in octets, whose line a server names is read to show it
beside the place.")

(defvar *lsp-location-line-length* 200
  "The most characters of a line shown beside a place a server names.")

(defvar *lsp-location-files-total* (* 16 1024 1024)
  "The most octets of files read in all to show the lines of the places one
question to a server names; past it, the places of files not read yet are
shown without their lines, so that hundreds of them never hold the editor up.")

(defvar *location-octets-left* nil
  "While a server's places are made DEFINITIONs, the octets of files that may
still be read for their lines (*LSP-LOCATION-FILES-TOTAL*); NIL when they are
not counted.")

(defun location-line (server path line)
  "The text of the 0-based LINE of the file at PATH, for a place SERVER names:
from the buffer that has it open, else from the file when it is in SERVER's
project, not in *SAFE-DIRECTORIES*, no larger than *LSP-LOCATION-FILE-LARGEST*
and than the octets left to read (*LSP-LOCATION-FILES-TOTAL*); empty
otherwise. At most *LSP-LOCATION-LINE-LENGTH* characters of it."
  (let* ((open (buffer-visiting path))
         (text (if open
                   (rope-line-text (tuatara.buffer:buffer-text open) line)
                   (let ((lines (and *location-files* (gethash path *location-files*))))
                     (unless lines
                       (setf lines (if (and (or (null *location-octets-left*) (plusp *location-octets-left*))
                                            (tuatara.io:really-within-p path (lsp-server-root server)) (not (safe-directory-of path)))
                                       (let ((text (handler-case (tuatara.io:file-text path :limit (min *lsp-location-file-largest* (or *location-octets-left* most-positive-fixnum))
                                                                                            :too-large (constantly "") :missing "")
                                                     ;; a file that cannot be read shows no line
                                                     (tuatara.io:file-system-error () ""))))
                                         (when *location-octets-left* (decf *location-octets-left* (length text)))
                                         (coerce (uiop:split-string text :separator '(#\Newline)) 'vector))
                                       #()))
                       (when *location-files* (setf (gethash path *location-files*) lines)))
                     (if (< -1 line (length lines)) (aref lines line) "")))))
    (string-trim '(#\Space #\Tab) (subseq text 0 (min (length text) *lsp-location-line-length*)))))

(defun lsp-location-definition (server location)
  "LOCATION, a place a server names (a Location or a LocationLink), as a
DEFINITION: in a note's source blocks when it names them; NIL when it names
no place."
  (let* ((uri (or (tuatara.text:json-text location "targetUri") (tuatara.text:json-text location "uri")))
         (range (or (tuatara.text:json-get location "targetSelectionRange") (tuatara.text:json-get location "range")))
         (doc (and uri (lsp-doc-of-uri uri))))
    (cond ((not (and uri (position-line-character (tuatara.text:json-get range "start")))) nil)
          ((and doc (lsp-doc-shadow doc))
           (let* ((buffer (lsp-doc-buffer doc))
                  (rope (tuatara.buffer:buffer-text buffer))
                  (at (doc-byte doc server (tuatara.text:json-get range "start")))
                  (line (tuatara.text:byte->line rope at)))
             (make-definition (format nil "~d: ~a" (1+ line) (string-trim '(#\Space #\Tab) (rope-line-text rope line)))
                              (tuatara.buffer:buffer-name buffer)
                              (lambda () (if (live-buffer-p buffer) (values buffer (min at (tuatara.buffer:buffer-size buffer)))
                                             (values nil "Its buffer is gone"))))))
          (t (file-location-definition server uri (tuatara.text:json-get range "start"))))))

(defun file-location-definition (server uri start)
  "The place START, a position SERVER sent, of the file URI names, as a DEFINITION."
  (let ((path (uri-path uri))
        (line (tuatara.text:json-integer start "line")))
    (make-definition (format nil "~d: ~a" (1+ line) (if path (location-line server path line) ""))
                     (if path (abbreviated-path path) uri)
                     (lambda ()
                       (multiple-value-bind (buffer at) (uri-place server uri start :visit t)
                         (if buffer
                             (values buffer at)
                             (values nil (format nil "~a is no file here" (if path (abbreviated-path path) uri)))))))))

(defun lsp-locations (buffer method capability feature &key (position (tuatara.buffer:buffer-point buffer)) extra-params)
  "The places the server of BUFFER that offers CAPABILITY names for the thing
at POSITION (point), asked by METHOD, with EXTRA-PARAMS (keys and values)
added to the question, as DEFINITIONs; :NONE when no server offers it."
  (lsp-in-step buffer)
  (multiple-value-bind (doc server) (lsp-doc-at buffer position capability feature)
    (if (null server)
        :none
        (let ((params (doc-params doc server position))
              (*location-files* (make-hash-table :test 'equal))
              (*location-octets-left* *lsp-location-files-total*))
          (loop for (key value) on extra-params by #'cddr do (setf (gethash key params) value))
          (let ((result (lsp-request-now server method params)))
            (remove nil (map 'list (lambda (location) (lsp-location-definition server location))
                             (if (hash-table-p result) (vector result) (tuatara.text:json-items result)))))))))

(defmethod mode-name-at-point :around (mode buffer position)
  "In a buffer open in a server that finds definitions, the word at point; else as the mode finds it."
  (or (call-next-method)
      (and (lsp-doc-at buffer position "definitionProvider" "goto-definition") (lsp-word-at buffer position))))

(defmethod mode-find-definitions :around (mode buffer name)
  "In a buffer open in a server that finds definitions, where the server says
the name at point is defined; else as the mode finds them."
  (let ((server (nth-value 1 (lsp-doc-at buffer (tuatara.buffer:buffer-point buffer) "definitionProvider" "goto-definition"))))
    (cond ((null server) (call-next-method))
          ((string= name "") '())
          ((equal name (lsp-word-at buffer (tuatara.buffer:buffer-point buffer)))
           (let ((found (lsp-locations buffer "textDocument/definition" "definitionProvider" "goto-definition")))
             (if (and (listp found) found) found (let ((own (call-next-method))) (if (eq own :none) '() own)))))
          (t (let ((own (call-next-method))) (if (eq own :none) '() own))))))

(defun show-server-places (found title again &key (none "No language server here finds that") (nothing "None the server knows of") one-at-once)
  "Show FOUND, the places a server names (DEFINITIONs), in *xref* under TITLE,
AGAIN a function giving them anew; with ONE-AT-ONCE, go to the only one at
once. NONE is said when FOUND is :NONE (no server asked), NOTHING when there
are none."
  (cond ((eq found :none) (message "~a" none))
        ((null found) (message "~a" nothing))
        ((and one-at-once (null (rest found))) (push-xref-marker) (go-to-definition (first found)))
        (t (push-xref-marker)
           (show-definitions title found :again again))))

(defun lsp-go-to-locations (method capability feature what &key extra-params one-at-once title)
  "Show where the server of the current buffer that offers CAPABILITY says,
asked by METHOD with EXTRA-PARAMS, the thing at point has WHAT
\(\"implementations\"), in *xref* (SHOW-SERVER-PLACES, ONE-AT-ONCE as it
takes it), under TITLE or WHAT of the thing's name."
  (let* ((buffer (current-buffer))
         (position (point))
         (name (or (lsp-word-at buffer position) "this")))
    (show-server-places (lsp-locations buffer method capability feature :extra-params extra-params)
                        (or title (format nil "~a of ~a" what name))
                        (lambda () (lsp-locations buffer method capability feature :extra-params extra-params
                                                                               :position (min position (tuatara.buffer:buffer-size buffer))))
                        :none (format nil "No language server here finds ~a" what)
                        :nothing (format nil "No ~a the server knows of" what)
                        :one-at-once one-at-once)))

(define-command lsp-find-implementations ()
  "Go to the implementations of the thing at point (of a trait, an interface,
an abstract method), as its language server finds them: there at once when it
is one, else listed in *xref*."
  (lsp-go-to-locations "textDocument/implementation" "implementationProvider" "goto-implementation" "implementations"
                       :one-at-once t))

(define-command lsp-find-type-definition ()
  "Go to the definition of the type of the thing at point (a variable's type,
a function's result), as its language server finds it."
  (lsp-go-to-locations "textDocument/typeDefinition" "typeDefinitionProvider" "goto-type-definition" "type definitions"
                       :one-at-once t))

(define-command lsp-find-references ()
  "List where the thing at point is used, as its language server finds it, in
*xref*: RET goes to one."
  (lsp-go-to-locations "textDocument/references" "referencesProvider" "goto-reference" "uses"
                       :extra-params (list "context" (tuatara.text:json-object "includeDeclaration" t))
                       :title (or (lsp-word-at (current-buffer) (point)) "this")))

;;; What the server knows of a thing

(define-command lsp-describe-thing-at-point ()
  "Show what the language server knows of the thing at point (its type, its
documentation) in *Help*."
  (let ((buffer (current-buffer)))
    (lsp-in-step buffer)
    (multiple-value-bind (doc server) (lsp-doc-at buffer (point) "hoverProvider" "hover")
      (if (null server)
          (message "No language server here describes things")
          (multiple-value-bind (result error) (lsp-request-now server "textDocument/hover" (doc-params doc server (point)))
            (let ((text (and result (markup-text (tuatara.text:json-get result "contents")))))
              (cond (error (message "~a could not describe what is here: ~a" (lsp-server-name server) (lsp-error-words error)))
                    ((or (null text) (zerop (length text))) (message "~a knows nothing of what is here" (lsp-server-name server)))
                    (t (show-help (format nil "~a~%~%(~a)~%" text (lsp-server-name server)))))))))))

(defun lsp-signature-words (result)
  "RESULT, a server's signature help, as a line: the signature, the parameter
the cursor is at named."
  (let* ((signatures (tuatara.text:json-items result "signatures"))
         (signature (and (plusp (length signatures))
                         (aref signatures (max 0 (min (or (tuatara.text:json-integer result "activeSignature") 0) (1- (length signatures)))))))
         (label (tuatara.text:json-text signature "label"))
         (parameter (tuatara.text:json-text signature "parameters"
                               (or (tuatara.text:json-integer signature "activeParameter") (tuatara.text:json-integer result "activeParameter") -1)
                               "label")))
    (and label (if parameter (format nil "~a  — ~a" label parameter) label))))

(defun hover-summary (contents)
  "What a server says of a thing (CONTENTS, as MARKUP-TEXT takes it) as one
line: its last block of code, which is its type or signature, else its first
line of words."
  (let* ((text (markup-source contents))
         (code (let ((blocks '()))
                 (cl-ppcre:do-register-groups (body) ("(?s)```[^\\n]*\\n(.*?)```" text)
                   (push body blocks))
                 (first blocks)))
         (line (if code
                   (format nil "~{~a~^ ~}" (remove "" (mapcar (lambda (l) (string-trim '(#\Space #\Tab) l))
                                                               (uiop:split-string code :separator '(#\Newline)))
                                                    :test #'string=))
                   (find-if (lambda (l) (plusp (length l)))
                            (mapcar (lambda (l) (string-trim '(#\Space #\Tab) l))
                                    (uiop:split-string (markup-text contents) :separator '(#\Newline)))))))
    (and line (plusp (length line)) line)))

(defun lsp-echo-hover (buffer position)
  "Ask the server of BUFFER that describes things what the thing at POSITION
is, and say it in a line once it comes, while the cursor is still there and
nothing else is said."
  (multiple-value-bind (doc server) (and *lsp-hover-echo* (lsp-word-at buffer position)
                                         (lsp-doc-at buffer position "hoverProvider" "hover"))
    (when server
      (lsp-request server "textDocument/hover" (doc-params doc server position)
                   (lambda (result error)
                     (declare (ignore error))
                     (let ((line (and (hash-table-p result) (hover-summary (tuatara.text:json-get result "contents")))))
                       (when (and line (eq (current-buffer) buffer) (eql (point) position) (null (editor-message *editor*)))
                         (echo "~a" line))))))))

(defun lsp-echo-at (buffer position)
  "Say in the echo area the parameters of the call at POSITION of BUFFER, as
its server knows them, once they come; with no call there, what the thing at
POSITION is (LSP-ECHO-HOVER)."
  (multiple-value-bind (doc server) (and *lsp-signature-help* (lsp-doc-at buffer position "signatureHelpProvider" "signature-help"))
    (if (null server)
        (lsp-echo-hover buffer position)
        (lsp-request server "textDocument/signatureHelp" (doc-params doc server position)
                     (lambda (result error)
                       (declare (ignore error))
                       (let ((words (and (hash-table-p result) (lsp-signature-words result))))
                         (cond ((not (and (eq (current-buffer) buffer) (eql (point) position))))
                               (words (when (null (editor-message *editor*)) (echo "~a" words)))
                               (t (lsp-echo-hover buffer position)))))))))

;;; The name at the cursor, and its other uses

(defvar *lsp-most-highlights* 2000
  "The most uses of the name the cursor rests on that are shown in a buffer.")

(defun lsp-ask-symbol-highlights (buffer position)
  "Ask the server of BUFFER that finds a name's uses for those of the name at
POSITION in its file, and show them once they come, while the cursor is still
there."
  (multiple-value-bind (doc server) (and *lsp-highlight-symbol* (lsp-word-at buffer position)
                                         (lsp-doc-at buffer position "documentHighlightProvider" "document-highlight"))
    (when server
      (lsp-request server "textDocument/documentHighlight" (doc-params doc server position)
                   (lambda (result error)
                     (when (and (null error) (live-buffer-p buffer)
                                (eql (tuatara.buffer:buffer-point buffer) position))
                       (setf (tuatara.buffer:buffer-local buffer 'lsp-symbol-highlights)
                             (make-placed (tuatara.buffer:buffer-version buffer)
                                          (loop for h across (tuatara.text:json-items result)
                                                for i below *lsp-most-highlights*
                                                for (start end) = (multiple-value-list
                                                                   (doc-range-bytes doc server (tuatara.text:json-get h "range")))
                                                when start
                                                  collect (list start end :symbol-highlight))))))))))

(defun symbol-highlights-here-p (buffer position)
  "True when POSITION of BUFFER is within one of the uses of a name shown."
  (let ((placed (tuatara.buffer:buffer-local buffer 'lsp-symbol-highlights)))
    (and placed (some (lambda (h) (<= (first h) position (second h))) (placed-items-now placed buffer)))))

(defun symbol-highlights (editor window rows)
  "The uses of the name the cursor rests on in ROWS of WINDOW."
  (declare (ignore editor))
  (let* ((buffer (tuatara.display:window-buffer window))
         (placed (tuatara.buffer:buffer-local buffer 'lsp-symbol-highlights)))
    (when (and placed (plusp (length rows)))
      (let ((from (tuatara.display:row-start (aref rows 0)))
            (to (tuatara.display:row-end (aref rows (1- (length rows))))))
        (loop for (start end face) in (placed-items-now placed buffer)
              when (and (< start to) (> end from) (< start end)) collect (list start end face))))))

(defvar *lsp-rest* nil
  "Where the cursor last stopped in a buffer open in a language server, as
\(BUFFER POSITION WHEN DONE), WHEN in internal time, DONE true once what is
known there was shown.")

(defun lsp-note-rest (editor)
  "After a command: note where the cursor stopped, so that what is known
there is shown once it rests (LSP-REST-TICK); the box under the cursor closed
when the cursor is no longer where it was shown."
  (let* ((buffer (current-buffer))
         (position (tuatara.buffer:buffer-point buffer))
         (note (editor-note editor)))
    (when (and note (not (and (eq (first note) buffer) (eql (second note) position))))
      (close-note editor))
    ;; the uses of the name shown while the cursor is on one of them
    (unless (symbol-highlights-here-p buffer position)
      (setf (tuatara.buffer:buffer-local buffer 'lsp-symbol-highlights) nil))
    (unless (and *lsp-rest* (eq (first *lsp-rest*) buffer) (eql (second *lsp-rest*) position))
      (setf *lsp-rest* (and (buffer-docs buffer) (list buffer position (get-internal-real-time) nil))))))

(defun lsp-rest-tick (editor)
  "Once the cursor has rested *LSP-IDLE-DELAY* seconds where LSP-NOTE-REST
saw it stop: show what a server found there (DIAGNOSTIC-SAY), say in the
echo area the parameters of the call, or what the thing is (LSP-ECHO-AT), and
show the other uses of the name there (LSP-ASK-SYMBOL-HIGHLIGHTS)."
  (when (and *lsp-rest* (not (fourth *lsp-rest*)))
    (destructuring-bind (buffer position when done) *lsp-rest*
      (declare (ignore done))
      (let ((left (- *lsp-idle-delay* (tuatara.runtime:seconds-since when))))
        (cond ((plusp left) left)
              ((or (not (eq buffer (current-buffer))) (not (eql position (point))) (editor-minibuffer-active editor))
               (setf *lsp-rest* nil))
              (t (setf (fourth *lsp-rest*) t)
                 (lsp-in-step buffer)
                 (unless (and (diagnostic-say editor) (eq *lsp-diagnostics-display* :echo))
                   (lsp-echo-at buffer position))
                 (unless (symbol-highlights-here-p buffer position)
                   (lsp-ask-symbol-highlights buffer position))
                 nil))))))

;;; Changes a server makes: renames, fixes, layout

(defun text-edits (edits range-bytes)
  "EDITS, a server's TextEdits, as a vector of TUATARA.TEXT:EDIT in order of
place, RANGE-BYTES a function of an edit's range giving where it starts and
ends (two values, NIL for no range); NIL when any of them says not where it
goes, or two overlap."
  (let ((made '()))
    (loop for e across (tuatara.text:json-items edits)
          do (multiple-value-bind (start end) (funcall range-bytes (tuatara.text:json-get e "range"))
               (unless start (return-from text-edits nil))
               (let ((text (or (tuatara.text:json-text e "newText") "")))
                 (push (tuatara.text:make-edit start end (and (plusp (length text)) (tuatara.text:string-octets text))) made))))
    ;; edits at one place go in in the order the server gave them
    (let ((sorted (stable-sort (nreverse made) #'< :key #'tuatara.text:edit-start)))
      (and (loop for (a b) on sorted while b always (<= (tuatara.text:edit-end a) (tuatara.text:edit-start b)))
           (coerce sorted 'vector)))))

(defun doc-text-edits (doc server edits)
  "EDITS, SERVER's TextEdits for DOC, as TEXT-EDITS gives them, of DOC's
buffer: each where the text as it was sent has it, moved with the text typed since."
  (text-edits edits (lambda (range) (doc-range-bytes doc server range))))

(defun put-in-edits (buffer edits)
  "Make EDITS, a vector of TUATARA.TEXT:EDIT, in BUFFER as one change undo
takes back whole; how many. They are made wherever they go, though BUFFER is
narrowed to another part, as a server's changes reach the whole file."
  (when (plusp (length edits))
    (tuatara.buffer:with-narrowing-lifted (buffer)
      (tuatara.buffer:buffer-edit buffer edits)))
  (length edits))

(defun workspace-edit-changes (edit)
  "The changes of EDIT, a server's WorkspaceEdit, a file at a time: each (URI
. EDITS); and as a second value how many files it would make, rename or delete."
  (let ((changes '()) (others 0))
    (let ((by-uri (tuatara.text:json-get edit "changes")))
      (when (hash-table-p by-uri)
        (maphash (lambda (uri edits) (push (cons uri edits) changes)) by-uri)))
    (loop for change across (tuatara.text:json-items edit "documentChanges")
          do (if (tuatara.text:json-get change "kind")
                 (incf others)
                 (push (cons (tuatara.text:json-text change "textDocument" "uri") (tuatara.text:json-get change "edits")) changes)))
    (values (nreverse changes) others)))

(defun lsp-apply-file-edits (server uri edits)
  "Make EDITS, SERVER's TextEdits of what the address URI names, in its
buffer as one change, the file opened when no buffer has it: true when it
changed, NIL when there was nothing to change; else words saying why not."
  (let ((path (uri-path uri)))
    (cond ((null path) (format nil "~a is no file here" uri))
          ((safe-directory-of path) (format nil "~a is in one of *safe-directories*" (abbreviated-path path)))
          (t (multiple-value-bind (buffer doc) (uri-buffer uri :visit t)
               (cond ((null buffer) (format nil "~a is not there (no file is made)" (abbreviated-path path)))
                     ((or (tuatara.buffer:buffer-local buffer :read-only) (tuatara.io:safe-visit-p buffer))
                      (format nil "~a is ~:[read only~;opened safely~]" (tuatara.buffer:buffer-name buffer) (tuatara.io:safe-visit-p buffer)))
                     (t (let ((made (if doc
                                        (doc-text-edits doc server edits)
                                        (let ((rope (tuatara.buffer:buffer-text buffer)))
                                          (text-edits edits (lambda (range) (lsp-range-bytes server rope range)))))))
                          (cond ((null made) (format nil "~a: changes that say not where they go" (tuatara.buffer:buffer-name buffer)))
                                ((zerop (length made)) nil)
                                (t (put-in-edits buffer made) t))))))))))

(defun workspace-edit-question (server paths asked)
  "The question whether to make SERVER's change of the files at PATHS: one it
ASKED for of itself, or one that reaches outside its project."
  (let ((outside (remove-if (lambda (p) (tuatara.io:really-within-p p (lsp-server-root server))) paths)))
    (format nil "~a ~:[would change~;asks to change~] ~d file~:p~:[~*~; (~:*~d outside ~a)~]: ~{~a~^, ~}~:[~;, ...~]; change ~:[it~;them~]?"
            (lsp-server-name server) asked (length paths)
            (and outside (length outside)) (abbreviated-path (lsp-server-root server))
            (mapcar #'abbreviated-path (subseq (or outside paths) 0 (min 4 (length (or outside paths)))))
            (> (length (or outside paths)) 4) (rest paths))))

(defun lsp-apply-workspace-edit (server edit &key asked)
  "Make EDIT, a server's WorkspaceEdit, in the buffers of its files, those not
open opened first: each file's changes one change undo takes back whole,
nothing saved. Asked first when it would change a file outside the server's
project, or always when the server ASKED for it of itself. Never made: files
to make, rename or delete, files not there, in *safe-directories*, read only
or opened safely, and changes that say not where they go. Returns how many
files changed, NIL when it was not made, and as a second value words saying
what was left alone, or NIL."
  (multiple-value-bind (changes others) (workspace-edit-changes edit)
    (let ((paths (remove nil (mapcar (lambda (change) (uri-path (car change))) changes))))
      (when (or (null paths)
                (not (or asked (notevery (lambda (p) (tuatara.io:really-within-p p (lsp-server-root server))) paths)))
                (yes-or-no-p* (workspace-edit-question server paths asked)))
        (let ((done 0) (left '()))
          (loop for (uri . edits) in changes
                do (let ((outcome (lsp-apply-file-edits server uri edits)))
                     (cond ((stringp outcome) (push outcome left))
                           (outcome (incf done)))))
          (when (plusp others)
            (push (format nil "~d file~:p to make, rename or delete" others) left))
          (values done (and left (format nil "left alone: ~{~a~^; ~}" (nreverse left)))))))))

(define-lsp-request "workspace/applyEdit" (server params)
  ;; asked as the next command; the server answered once that is done
  (let ((answer *lsp-answer*)
        (asked (zerop (lsp-server-acting server))))
    (defer-action
     (lambda ()
       (multiple-value-bind (files left) (lsp-apply-workspace-edit server (tuatara.text:json-get params "edit") :asked asked)
         (cond ((null files) (message "~a's change not made" (lsp-server-name server)))
               ((or (plusp files) left)
                (message "~a changed ~d file~:p~@[ (~a)~]~@[; ~a~]" (lsp-server-name server) files (tuatara.text:json-text params "label") left)))
         (funcall answer (if (and files (null left))
                             (tuatara.text:json-object "applied" t)
                             (tuatara.text:json-object "applied" :false "failureReason" (or left "not made")))))))
    :later))

(define-command lsp-rename ()
  "Rename the thing at point everywhere the language server knows it is used,
the new name asked for: each file's changes one change undo takes back,
nothing saved (\\[save-some-buffers] saves them). Asked first when it would
change files outside the project."
  (let* ((buffer (current-buffer))
         (old (lsp-word-at buffer (point))))
    (multiple-value-bind (doc server) (lsp-doc-at buffer (point) "renameProvider" "rename-symbol")
      (if (null server)
          (message "No language server here renames things")
          (let ((new (read-string (format nil "Rename ~@[~a ~]to: " old) :initial (or old "") :select-initial t)))
            (lsp-in-step buffer)
            (multiple-value-bind (result error)
                (lsp-request-now server "textDocument/rename"
                                 (let ((params (doc-params doc server (point)))) (setf (gethash "newName" params) new) params))
              (cond (error (message "~a could not rename ~:[this~;~:*~a~]: ~a" (lsp-server-name server) old (lsp-error-words error)))
                    ((not (hash-table-p result)) (message "Nothing to rename here"))
                    (t (multiple-value-bind (files left) (lsp-apply-workspace-edit server result)
                         (if files
                             (message "Renamed in ~d file~:p; not saved~@[; ~a~]" files left)
                             (message "Not renamed")))))))))))

(defun code-action-params (doc server start end diagnostics &optional only)
  "What a server is asked for the changes it offers for DOC from START to END
of its buffer: DIAGNOSTICS, what servers found there, and ONLY, a vector of
the kinds wanted, or NIL for all."
  (tuatara.text:json-object
   "textDocument" (doc-identifier doc)
   "range" (doc-range doc server start end)
   "context" (tuatara.text:json-object
              "diagnostics" (map 'vector (lambda (d)
                                           (tuatara.text:json-object
                                            "range" (doc-range doc server (diagnostic-start d) (diagnostic-end d))
                                            "severity" (diagnostic-severity d)
                                            "message" (diagnostic-message d)))
                                 diagnostics)
              "only" (or only :omit))))

(define-command lsp-code-actions ()
  "Offer the language server's fixes and changes for the selection, or for
point: those for what it found wrong there first; the one chosen is made."
  (let ((buffer (current-buffer)))
    (lsp-in-step buffer)
    (multiple-value-bind (doc server) (lsp-doc-at buffer (point) "codeActionProvider" "code-action")
      (if (null server)
          (message "No language server here offers changes")
          (multiple-value-bind (start end) (if (tuatara.buffer:buffer-mark-active-p buffer)
                                               (tuatara.buffer:buffer-region buffer)
                                               (values (point) (point)))
            (multiple-value-bind (result error)
                (lsp-request-now server "textDocument/codeAction"
                                 (code-action-params doc server start end (diagnostics-in buffer start end)))
              (let ((actions (remove-if-not (lambda (a) (tuatara.text:json-text a "title"))
                                            (coerce (tuatara.text:json-items result) 'list))))
                (cond (error (message "~a could not offer changes: ~a" (lsp-server-name server) (lsp-error-words error)))
                      ((null actions) (message "~a offers nothing here" (lsp-server-name server)))
                      (t (let ((action (completing-read-value "Change: " (mapcar (lambda (a) (make-candidate (tuatara.text:json-text a "title") :value a)) actions)
                                                              :require-match t :keep-order t)))
                           (when (hash-table-p action)
                             (lsp-do-action server action))))))))))))

(defun title-in-a-sentence (title)
  "TITLE, a server's name for an action (Import `HashMap`), as it reads
inside a sentence: its first letter small, unless the second is a capital
too (an abbreviation, as in CSS)."
  (if (and (plusp (length title)) (upper-case-p (char title 0))
           (not (and (> (length title) 1) (upper-case-p (char title 1)))))
      (concatenate 'string (string (char-downcase (char title 0))) (subseq title 1))
      title))

(defun lsp-do-action (server action)
  "Make ACTION, one of SERVER's CodeActions (its edit, then its command) or a
Command alone, and say its title: a command the editor runs itself
\(*LSP-CLIENT-COMMANDS*) run here, any other by the server. Its edit is asked
about first as LSP-APPLY-WORKSPACE-EDIT asks; not made, its command is not run."
  (let ((edit (tuatara.text:json-get action "edit"))
        (command (let ((c (tuatara.text:json-get action "command")))
                   (cond ((hash-table-p c) c) ((stringp c) action))))
        (left nil))
    (when edit
      (multiple-value-bind (files why) (lsp-apply-workspace-edit server edit)
        (setf left why)
        (unless files (setf command nil))))
    (let ((own (and command (gethash (tuatara.text:json-text command "command") *lsp-client-commands*))))
      (cond (own (funcall own server (tuatara.text:json-items command "arguments")))
            (command
             ;; what the server changes meanwhile (workspace/applyEdit) is what was chosen
             (incf (lsp-server-acting server))
             (lsp-request server "workspace/executeCommand"
                          (tuatara.text:json-object "command" (tuatara.text:json-text command "command")
                                                    "arguments" (tuatara.text:json-items command "arguments"))
                          (lambda (result error)
                            (declare (ignore result))
                            (decf (lsp-server-acting server))
                            (when error
                              (message "~a could not ~a: ~a" (lsp-server-name server)
                                       (title-in-a-sentence (or (tuatara.text:json-text action "title") "do it"))
                                       (lsp-error-words error))))))))
    (message "~a~@[; ~a~]" (tuatara.text:json-text action "title") left)))

(define-lsp-client-command "rust-analyzer.showReferences" (server arguments)
  ;; the file, the place, and the places to show (a lens's implementations, references)
  (let ((locations (and (> (length arguments) 2) (tuatara.text:json-items arguments 2)))
        (*location-files* (make-hash-table :test 'equal))
        (*location-octets-left* *lsp-location-files-total*))
    (show-server-places (remove nil (map 'list (lambda (l) (lsp-location-definition server l)) locations))
                        "places" nil :nothing "Nowhere to show")))

(define-command lsp-organise-imports ()
  "Have the language server put the file's imports in order, as its own
action for that does it (unused ones taken out, where it does that), once it
answers, when the file is still as it was asked of."
  (let ((buffer (current-buffer)))
    (lsp-in-step buffer)
    (multiple-value-bind (doc server) (lsp-server-offers buffer "codeActionProvider" "code-action")
      (if (null server)
          (message "No language server here puts imports in order")
          (let ((version (lsp-doc-version doc)))
            (lsp-request server "textDocument/codeAction"
                         (code-action-params doc server 0 (tuatara.buffer:buffer-size buffer) '() #("source.organizeImports"))
                         (lambda (result error)
                           (let ((action (and (null error)
                                              (find-if (lambda (a) (let ((kind (tuatara.text:json-text a "kind")))
                                                                     (and kind (uiop:string-prefix-p "source.organizeImports" kind))))
                                                       (tuatara.text:json-items result)))))
                             (cond (error (message "~a could not put imports in order: ~a" (lsp-server-name server) (lsp-error-words error)))
                                   ((not (doc-unchanged-p doc version))
                                    (message "~a changed meanwhile: imports not put in order" (tuatara.buffer:buffer-name buffer)))
                                   ;; made as a command of its own, which may ask first
                                   (action (defer-action (lambda () (lsp-do-action server action))))
                                   (t (message "~a has nothing to put in order here" (lsp-server-name server))))))))))))

(defun lsp-formatting-options (buffer)
  "How BUFFER is indented, as a server's formatting is told."
  (tuatara.text:json-object "tabSize" (buffer-tab-width buffer)
                            "insertSpaces" (if (buffer-indent-tabs-p buffer) :false t)))

(defun lsp-format (buffer &key start end then)
  "Have BUFFER's server lay out its text, or that from START to END, and make
its changes once it answers, when the buffer is as it was asked of; THEN, a
function of the buffer and whether its text was laid out, called once it is
over. What came of it is said (FINISH-FORMATTING): the places changed, that
it was laid out already, or why nothing changed: the server's own words when
it failed, no answer within *FORMAT-SECONDS*, its changes not saying where
they go, or the buffer changed meanwhile. NIL when no server lays it out."
  (let ((range (and start end)))
    (multiple-value-bind (doc server)
        (lsp-server-offers buffer (if range "documentRangeFormattingProvider" "documentFormattingProvider") "format")
      (when server
        (lsp-in-step buffer)
        (let ((version (tuatara.buffer:buffer-version buffer)))
          (lsp-request server (if range "textDocument/rangeFormatting" "textDocument/formatting")
                       (tuatara.text:json-object "textDocument" (doc-identifier doc)
                                                 "options" (lsp-formatting-options buffer)
                                                 "range" (if range (doc-range doc server start end) :omit))
                       (lambda (result error)
                         (when (live-buffer-p buffer)
                           (let* ((made (and (null error) (doc-text-edits doc server result)))
                                  (formatted (finish-formatting buffer (lsp-server-name server) version
                                                                (cond (error (lsp-error-words error :seconds *format-seconds*
                                                                                                    :setting '*format-seconds*))
                                                                      ((null made) "its changes say not where they go"))
                                                                (lambda () (put-in-edits buffer made)))))
                             (when then (funcall then buffer formatted)))))
                       :seconds *format-seconds*)
          server)))))

(defun servers-not-formatting-words (buffer &optional part)
  "Why no language server lays out BUFFER's code, or with PART the part of
it shown, in words: there is none for it; its servers see only a note's
source blocks; else of each of its servers, that it is still starting,
leaves formatting to the language's other servers, does not format code,
or formats whole files only."
  (let ((servers (buffer-servers buffer)))
    (cond ((null servers) "no language server formats it")
          ((every #'lsp-doc-shadow (buffer-docs buffer)) "its language servers see only its source blocks")
          (t (format nil "~{~a~^, ~}"
                     (mapcar (lambda (s)
                               (format nil "~a ~a" (lsp-server-name s)
                                       (cond ((eq (lsp-server-status s) :starting) "is still starting")
                                             ((not (lsp-feature-allowed-p s "format")) "leaves formatting to other servers")
                                             ((and part (server-offers-p s "documentFormattingProvider" "format"))
                                              "formats whole files only")
                                             (t "does not format code"))))
                             servers))))))

(define-command format-region ()
  "Lay out the code selected as the language server does; with no server that
lays out a part, each line selected indented as the mode indents it."
  (let ((buffer (current-buffer)))
    (if (not (tuatara.buffer:buffer-mark-active-p buffer))
        (message "No selection: format-region was not run")
        (multiple-value-bind (start end) (tuatara.buffer:buffer-region buffer)
          (unless (lsp-format buffer :start start :end end)
            (indent-region))))))

(defparameter *code-context-keys*
  '(("C-c d" xref-find-definitions "definition") ("C-c D" xref-go-back "back") ("C-c r" lsp-find-references "references")
    ("C-c R" lsp-rename "rename") ("C-c a" lsp-code-actions "fixes") ("C-c h" lsp-describe-thing-at-point "describe")
    ("C-c f" format-buffer "format buffer") ("C-c F" format-region "format selection")
    ("C-c n" lsp-next-diagnostic "next problem") ("C-c p" lsp-previous-diagnostic "previous problem")
    ("C-c l" lsp-diagnostics "problems") ("C-c i" lsp-inlay-hints-mode "inlay hints") ("C-c S" lsp-semantic-tokens-mode "server colours")
    ("C-c I" lsp-find-implementations "implementations") ("C-c T" lsp-find-type-definition "type")
    ("C-c o" lsp-organise-imports "organise imports") ("C-c z" lsp-fold "fold") ("C-c Z" lsp-fold-all "fold all")
    ("C-c k" lsp-code-lens "run lens") ("C-c K" lsp-code-lenses-mode "code lenses")
    ("C-c c" lsp-incoming-calls "callers") ("C-c C" lsp-outgoing-calls "calls made") ("C-c g" lsp-find-symbol "project symbol")
    ("C-c L" lsp "start server"))
  "The keys of code under \\`C-c', in every programming language's buffers:
each its keys, its command and what the key hints call it.")

(defun code-context-keys (editor prefix)
  "For \\`C-c' in code open in a language server: what the cursor is on, as
*KEY-HINT-FOCUS-FUNCTIONS* has it, the keys that act on it first: something
a server found wrong, or a name."
  (declare (ignore editor))
  (let ((buffer (current-buffer)))
    (when (and (equalp prefix (tuatara.frontend:parse-key-sequence "C-c"))
               (typep (buffer-major-mode buffer) 'prog-mode)
               (buffer-docs buffer))
      (let ((position (tuatara.buffer:buffer-point buffer)))
        (cond ((diagnostics-at buffer position) '("problem at point" "a" "n" "p" "l"))
              ((lsp-word-at buffer position) '("name at point" "d" "r" "R" "h" "I" "T" "c" "C")))))))

(defun install-lsp-features (editor)
  "Show what servers know of where the cursor rests: what they found wrong,
the parameters of the call, what the thing is; bind code's keys under \\`C-c'."
  (pushnew 'lsp-note-rest (editor-post-command-functions editor))
  (pushnew 'lsp-rest-tick *tick-functions*)
  (pushnew 'symbol-highlights *highlight-functions*)
  (pushnew 'code-context-keys *key-hint-focus-functions*)
  (pushnew 'lsp-snippet-inserted *popup-insert-functions*)
  (let ((keymap (mode-keymap 'prog-mode)))
    (loop for (keys command label) in *code-context-keys*
          do (define-key keymap keys command)
             (set-key-label keymap keys label))))
