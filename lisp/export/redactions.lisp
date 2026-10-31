;;;; lisp/export/redactions.lisp --- Text hidden in one place, found shown in another.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A name hidden by {{{redact(...)}}} in an Org note, or by \R{...} in a
;;;; LaTeX file, is no secret when it is written out plainly elsewhere in the
;;;; same file. M-x check-redactions gathers what the buffer hides and lists in
;;;; *Redactions* every other place it shows, RET on one showing it; the
;;;; enhanced export and latex-compile-pdf look first, and ask before making a
;;;; PDF that would give a hidden text away.

(in-package #:tuatara.defaults)

(defvar *latex-redaction-commands* '("R" "D" "redact")
  "The LaTeX commands that hide their argument in your LaTeX files, \\\\R{John
Otago} for one, by their names without the backslash: what \\[check-redactions]
and \\[latex-compile-pdf] look for in a LaTeX file, as hidden text to be
found nowhere else.")

(defparameter *redaction-shortest-part* 3
  "The fewest characters a word of a hidden text has to be looked for alone
\(Otago of John Otago), so that short words do not crowd what is found.")

(defun hidden-spans (text kind &optional directory)
  "The texts TEXT hides, each (START END INSIDE NAME): where the call is,
the text it hides, and the name of the call (redact). In Org, the calls of
redact and protect, by their short names R and P too, of the kinds its
export hides (ORG-HIDDEN-KINDS: redact unless redact:nil, protect with
protect:t, in #+OPTIONS: or a setup file's, DIRECTORY the note's folder); in
LaTeX, those of *LATEX-REDACTION-COMMANDS*."
  (let ((found '()))
    (ecase kind
      (:org
       (let ((hidden (org-hidden-kinds text directory)))
         (cl-ppcre:do-scans (start end rs re *org-hiding-call-re* text)
           (let ((name (org-hiding-kind (subseq text (aref rs 0) (aref re 0)))))
             (when (member name hidden :test #'string=)
               (push (list start end (subseq text (aref rs 1) (aref re 1)) name) found))))))
      (:latex
       (let ((names (format nil "~{~a~^|~}" (mapcar #'cl-ppcre:quote-meta-chars *latex-redaction-commands*))))
         (when (plusp (length names))
           (cl-ppcre:do-scans (start end rs re (format nil "\\\\(~a)\\{([^{}]*)\\}" names) text)
             (push (list start end (subseq text (aref rs 1) (aref re 1)) (subseq text (aref rs 0) (aref re 0))) found))))))
    (nreverse found)))

(defun token-char-p (c)
  "True when C joins characters into one token, as in an address (90.233.1.40), a path or a name."
  (or (alphanumericp c) (find c ".:-/_@")))

(defun redaction-whole (text start end inside)
  "What INSIDE, hidden by the call from START below END of TEXT, stands for
there: INSIDE alone, or when the call is part of a token (90.233.1.{{{redact(40)}}}),
that token with INSIDE in the call's place (90.233.1.40), what would give it
away written elsewhere."
  (let ((from (loop for i downfrom start above 0 while (token-char-p (char text (1- i))) finally (return i)))
        (to (loop for i from end below (length text) while (token-char-p (char text i)) finally (return i))))
    (if (and (= from start) (= to end))
        inside
        (concatenate 'string (subseq text from start) inside (subseq text end to)))))

(defun redaction-needles (inside &optional (whole inside))
  "What to look for elsewhere of INSIDE, a hidden text, WHOLE what it stands
for where it is (REDACTION-WHOLE): WHOLE, and when INSIDE has several words,
each word of *REDACTION-SHORTEST-PART* characters or more, without its
punctuation."
  (let* ((whole (string-trim " 	()[]{},.;:'\"" whole))
         (words (remove-if (lambda (w) (< (length w) *redaction-shortest-part*))
                           (uiop:split-string (string-trim " 	()[]{},.;:'\"" inside)
                              :separator (remove-if #'token-char-p (remove-duplicates inside))))))
    (remove-duplicates (remove "" (cons whole (and (rest words) words)) :test #'string=) :test #'string-equal :from-end t)))

(defun word-starts (text)
  "Where each word of TEXT begins (a run of letters, digits and _), by the
word in lower case: a table of lists of positions, latest first."
  (let ((table (make-hash-table :test 'equal)) (n (length text)) (i 0))
    (flet ((word-char-p (c) (or (alphanumericp c) (char= c #\_))))
      (loop while (< i n)
            do (if (word-char-p (char text i))
                   (let ((end (or (position-if-not #'word-char-p text :start i) n)))
                     (push i (gethash (string-downcase (subseq text i end)) table))
                     (setf i end))
                   (incf i))))
    table))

(defun redaction-findings (text kind &optional directory (spans (hidden-spans text kind directory)))
  "Where TEXT shows a text it hides elsewhere: each (POSITION NEEDLE INSIDE
NAME), POSITION a character of TEXT outside every hiding call, NEEDLE what
stands there as written, found as a whole word in any case, INSIDE the
hidden text it gives away, NAME its call; in the order of the text. SPANS
are TEXT's hidden texts (HIDDEN-SPANS), found when not given. The text's
words are read once, and each text looked for once however often it is
hidden, so that a long note with hundreds of hidden texts is looked through
at once."
  (let* ((spans (sort (copy-list spans) #'< :key #'first))
         (starts (map 'vector #'first spans))
         (ends (map 'vector #'second spans))
         (words (word-starts text))
         (found (make-hash-table)))
    (labels ((word-char-p (c) (or (alphanumericp c) (char= c #\_)))
             (hidden-at-p (position)
               ;; the last span beginning at or before POSITION, by halves; spans do not overlap
               (let ((low 0) (high (length starts)))
                 (loop while (< low high)
                       do (let ((mid (floor (+ low high) 2)))
                            (if (<= (aref starts mid) position) (setf low (1+ mid)) (setf high mid))))
                 (and (plusp low) (< position (aref ends (1- low))))))
             (places (needle)
               ;; where NEEDLE may stand: where its first word does, else anywhere it is
               (let ((first-end (or (position-if-not #'word-char-p needle) (length needle))))
                 (if (plusp first-end)
                     (gethash (string-downcase (subseq needle 0 first-end)) words)
                     (loop for at = (search needle text :test #'char-equal) then (search needle text :test #'char-equal :start2 (1+ at))
                           while at collect at)))))
      ;; each text to look for once, with the first hidden text it gives away
      (let ((owners (make-hash-table :test 'equalp)) (needles '()))
        (dolist (span spans)
          (destructuring-bind (start end inside name) span
            (dolist (needle (redaction-needles inside (redaction-whole text start end inside)))
              (when (and (plusp (length needle)) (not (gethash needle owners)))
                (setf (gethash needle owners) (cons inside name))
                (push needle needles)))))
        (dolist (needle (nreverse needles))
          (destructuring-bind (inside . name) (gethash needle owners)
            (let ((n (length needle)))
              (dolist (at (places needle))
                (when (and (<= (+ at n) (length text))
                           (string-equal needle text :start2 at :end2 (+ at n))
                           (not (hidden-at-p at))
                           (or (zerop at) (not (word-char-p (char text (1- at)))))
                           (or (>= (+ at n) (length text)) (not (word-char-p (char text (+ at n))))))
                  ;; what stands there, as it is written; one finding a place
                  (unless (gethash at found)
                    (setf (gethash at found) (list at (subseq text at (+ at n)) inside name)))))))))
      ;; a word found inside a longer text found (Otago in John Otago) is that one place, said once:
      ;; in the order of the text, the longest first where two begin together, one is left out
      ;; when one before it reaches as far
      (let ((ordered (sort (loop for f being the hash-values of found collect f)
                           (lambda (f g) (or (< (first f) (first g))
                                             (and (= (first f) (first g)) (> (length (second f)) (length (second g))))))))
            (reach -1)
            (kept '()))
        (dolist (f ordered (nreverse kept))
          (let ((f-end (+ (first f) (length (second f)))))
            (unless (>= reach f-end) (push f kept))
            (setf reach (max reach f-end))))))))

(define-major-mode redactions-mode special-mode
  "*Redactions*: where a text the file hides shows plainly elsewhere in it,
the places under each hidden text. \\<redactions-mode>\\[list-visit] on a place
shows it in another window, \\[list-visit-other] the list staying selected;
\\[list-next-entry] and \\[list-previous-entry] go from one to the next;
\\[search-filter-regex] narrows the list to the places whose words match;
\\`q' closes."
  :lighter "Redactions" :interactive nil)

(defun show-redaction-findings (buffer text findings)
  "List FINDINGS (REDACTION-FINDINGS) of BUFFER, whose text is TEXT, in
*Redactions*: under the hidden text each gives away, each place with its line."
  (let ((rope (tuatara.buffer:buffer-text buffer))
        (lines (coerce (uiop:split-string text :separator '(#\Newline)) 'vector)))
    (multiple-value-bind (pieces entries)
        (entry-lines
         (lambda (put)
           (loop for group in (let ((groups '()))
                                (dolist (f findings (nreverse groups))
                                  (let ((g (assoc (third f) groups :test #'string=)))
                                    (if g (push f (cdr g)) (push (list (third f) f) groups)))))
                 do (destructuring-bind (inside . places) group
                      (funcall put "")
                      (funcall put (format nil "~a (~a), ~d place~:p:" inside (fourth (first places)) (length places)) :heading
                               (list :hidden inside))
                      (dolist (f (reverse places))
                        (destructuring-bind (position needle inside name) f
                          (declare (ignore inside name))
                          (let* ((octet (tuatara.text:char->byte rope position))
                                 (line (1+ (tuatara.buffer:buffer-line-number buffer octet)))
                                 ;; one entry, the place, on both its lines
                                 (entry (list :place buffer octet)))
                            (funcall put (format nil "  ~a:~d  ~a" (tuatara.buffer:buffer-name buffer) line needle) nil entry)
                            (funcall put (format nil "    | ~a" (string-trim " 	" (aref lines (1- line)))) nil entry))))))))
      (let ((list (set-search-filtering (buffer-in-mode "*Redactions*" 'redactions-mode)
                                        :nouns "places" :heading (lambda (entry) (eq (first entry) :hidden)))))
        (render-pieces list (cons (format nil "Text hidden in ~a, found shown elsewhere in it:~%" (tuatara.buffer:buffer-name buffer))
                                  pieces)
                       :entries entries :first-line 1 :point :start)
        (display-buffer list)))))

(defmethod visit-entry ((mode redactions-mode) list entry other)
  "Show the place on the line in the file it is in, in another window,
selected, or with OTHER the list kept selected."
  (declare (ignore list))
  (destructuring-bind (kind &optional buffer octet) entry
    (cond ((not (eq kind :place)) (message "No place on this line"))
          ((not (live-buffer-p buffer)) (message "The buffer the place is in is gone"))
          (t (show-place-beside buffer octet other)))))

(defun check-buffer-redactions (buffer &key quiet (text (tuatara.buffer:buffer-string buffer)))
  "Look in BUFFER, whose text is TEXT, for its hidden texts shown elsewhere in
it, list them in *Redactions* when there are any, and return how many places;
say what was found, and with QUIET nothing when there is nothing to say."
  (let* ((kind (export-kind buffer))
         (directory (note-directory buffer))
         (spans (and kind (hidden-spans text kind directory)))
         (findings (and spans (redaction-findings text kind directory spans))))
    (cond ((null kind)
           (message "~a is neither an Org nor a LaTeX file: nothing hidden to look for" (tuatara.buffer:buffer-name buffer)))
          ((null spans)
           (unless quiet (message "Nothing in ~a is hidden (redact unless redact:nil, protect with protect:t)" (tuatara.buffer:buffer-name buffer))))
          ((null findings)
           (unless quiet
             (message "None of the ~d text~:p hidden in ~a shows anywhere else in it" (length spans) (tuatara.buffer:buffer-name buffer))))
          (t
           (show-redaction-findings buffer text findings)
           (message "~d place~:p in ~a show~:[~;s~] text hidden elsewhere in it: see *Redactions*"
                    (length findings) (tuatara.buffer:buffer-name buffer) (= 1 (length findings)))))
    (length findings)))

(define-command check-redactions ()
  "Look for the texts this Org or LaTeX file hides shown plainly elsewhere in
it, and list each place in *Redactions*, where \\<redactions-mode>\\[list-visit] shows it. Hidden are in an
Org note what {{{redact(...)}}} marks, and {{{protect(...)}}} when its
#+options say protect:t; in a LaTeX file the arguments of
*LATEX-REDACTION-COMMANDS* (\\\\R{...}, \\\\D{...}). Each hidden text is looked
for whole, and each of its words alone, as whole words in any case: John Otago
hidden is given away by John Otago or by Otago written elsewhere. The enhanced
export and \\[latex-compile-pdf] look first, and ask before making the PDF."
  (check-buffer-redactions (current-buffer)))

(defparameter *export-kept-folders*
  '(latex-templates-directory docx-templates-directory note-templates-directory logos-directory)
  "Functions giving the folders of yours an export never writes in unasked, beside
*EXPORT-TRUSTED-FOLDERS*.")

(defun export-target-allowed-p (buffer types &key text)
  "True when exporting BUFFER's Org note, or TEXT, the part of it exported,
may write its files of TYPES (endings, pdf or docx): when its
#+EXPORT_FILE_NAME puts them outside the note's own folder and the folders
in it (links followed), or in your templates, logos or trusted folders
\(*EXPORT-KEPT-FOLDERS*, *EXPORT-TRUSTED-FOLDERS*), only when you say to go
on. A note opened from an encrypted file is exported only when you say so:
what the export writes is not encrypted. Never into an evidence folder
\(*EVIDENCE-FOLDERS*): NIL then, and as a second value the words saying why
\(EVIDENCE-REFUSAL)."
  (unless (or (not (tuatara.buffer:buffer-local buffer :plaintext))
              (yes-or-no-p* (format nil "~a is encrypted, but its export and what is made for it are not; export it all the same?"
                                    (tuatara.buffer:buffer-name buffer))))
    (return-from export-target-allowed-p nil))
  (let* ((file (tuatara.io:native (tuatara.io:expand-path (tuatara.buffer:buffer-file buffer))))
         (named (and (eq (export-kind buffer) :org)
                     (ignore-errors (org-export-file-name (or text (tuatara.buffer:buffer-string buffer)) file))))
         (kept (append (mapcar #'funcall *export-kept-folders*) *export-trusted-folders*))
         ;; links followed: a folder in the note's that is a link to elsewhere is elsewhere
         (outside (and named (notevery (lambda (type)
                                         (let ((path (replaced-type named type)))
                                           (and (org-path-beside-p path (tuatara.io:parent-path file))
                                                (not (path-in-folders-p path kept)))))
                                       types)))
         (existing (and outside
                        (find-if (lambda (path) (tuatara.io:path-stamp path))
                                 (mapcar (lambda (type) (replaced-type named type)) types))))
         (refusal (some (lambda (type) (evidence-refusal "export to" (replaced-type (or named file) type))) types)))
    (when refusal
      (return-from export-target-allowed-p (values nil refusal)))
    (or (not outside)
        (yes-or-no-p* (if existing
                          (format nil "#+EXPORT_FILE_NAME writes over ~a, outside the note's folder or in one of your own; go on?"
                                  (abbreviated-path existing))
                          (format nil "#+EXPORT_FILE_NAME writes ~a, outside the note's folder or in one of your own; go on?"
                                  (abbreviated-path (replaced-type named (first types)))))))))

(defun redactions-allow-export-p (buffer &optional (text (tuatara.buffer:buffer-string buffer)))
  "True when a PDF may be made of BUFFER, whose text is TEXT, as far as its
hidden texts go: none shows elsewhere, or you say to go on when *Redactions*
lists where they do."
  (let ((count (check-buffer-redactions buffer :quiet t :text text)))
    (or (zerop count)
        (yes-or-no-p* (format nil "~d place~:p show~:[~;s~] text hidden elsewhere (see *Redactions*); make the PDF anyway?"
                              count (= 1 count))))))

;;; Wrapping text in a macro: R and P to hide it, or any the note declares

(defun macro-call-on-line (buffer point)
  "The macro call with arguments (*ORG-MACRO-CALL-RE*) on BUFFER's line that
holds the octet POINT, as (START END NAME INSIDE), octets; NIL when none."
  (let* ((line (tuatara.buffer:buffer-line-start buffer point))
         (text (tuatara.buffer:buffer-line-text buffer line)))
    (flet ((octet (char) (+ line (tuatara.text:string-octet-length text :end char))))
      (cl-ppcre:do-scans (start end rs re *org-macro-call-re* text)
        (when (<= (octet start) point (octet end))
          (return-from macro-call-on-line
            (list (octet start) (octet end) (subseq text (aref rs 0) (aref re 0)) (subseq text (aref rs 1) (aref re 1)))))))))

(defun token-at (buffer point)
  "The token around the octet POINT of BUFFER (TOKEN-CHAR-P: a name, an address
as 90.233.1.40, a mail address or a path), as START and END octets; NIL when
point is on none."
  (let* ((line (tuatara.buffer:buffer-line-start buffer point))
         (text (tuatara.buffer:buffer-line-text buffer line))
         (at (length (tuatara.buffer:buffer-string buffer :start line :end point)))
         (from (loop for i downfrom at above 0 while (token-char-p (char text (1- i))) finally (return i)))
         (to (loop for i from at below (length text) while (token-char-p (char text i)) finally (return i))))
    ;; a full stop or colon ending a sentence is not the token's
    (loop while (and (> to from) (find (char text (1- to)) ".:")) do (decf to))
    (when (< from to)
      (values (+ line (tuatara.text:string-octet-length text :end from))
              (+ line (tuatara.text:string-octet-length text :end to))))))

(defun org-macro-choices (buffer &key call)
  "The macros text in BUFFER, an Org note, may be wrapped in, as candidates:
R and P, which hide it in the enhanced export's PDF, then each its #+macro:
lines declare, with what it stands for; with CALL, the name of the call the
text is in, a last choice taking the call away (its value :NONE)."
  (let ((declared (remove-if #'org-hiding-kind
                             (with-org-note-keywords ((tuatara.buffer:buffer-string buffer) (note-directory buffer))
                               (org-macro-definitions))
                             :key #'car)))
    (append (list (make-candidate "R" :annotation "hidden in the PDF, a black box (redact; #+options: redact:nil prints it)")
                  (make-candidate "P" :annotation "hidden in the PDF only with #+options: protect:t (protect)"))
            (loop for (name . body) in declared
                  collect (make-candidate name :annotation (format nil "declared: ~a" body)))
            (and call (list (make-candidate "none" :annotation (format nil "the text alone, the call of ~a taken away" call)
                                                   :value :none))))))

(defvar *org-last-macro* "R"
  "The macro \\[org-macro-wrap] wrapped text in last, offered first the next time.")

(define-command org-macro-wrap ()
  "In an Org note, wrap the selection, or the word at point, in a macro chosen
from a list: R, {{{R(John Otago)}}}, hidden in the enhanced export's PDF as a
black box; P, hidden there only with #+options: protect:t; or one the note
declares with a #+macro: line, what it stands for shown beside it. A word is
a name, an address as 90.233.1.40, a mail address or a path. On text in a
call already, change the call to another macro, or choose none to have the
text alone again. While reading, a call shows as its text, in green.
\\[check-redactions] finds hidden text written plainly elsewhere."
  (let* ((buffer (current-buffer))
         (point (tuatara.buffer:buffer-point buffer))
         (call (let ((call (macro-call-on-line buffer point)))
                 ;; a selection reaching out of the call is text to wrap, not the call
                 (and call (or (not (region-active-p buffer))
                               (multiple-value-bind (start end) (tuatara.buffer:buffer-region buffer)
                                 (and (<= (first call) start) (<= end (second call)))))
                      call))))
    (if call
        (destructuring-bind (start end name inside) call
          (let ((chosen (completing-read (format nil "Change {{{~a(...)}}} to: " name) (org-macro-choices buffer :call name)
                                         :require-match t :keep-order t)))
            (tuatara.buffer:buffer-replace buffer start end (if (eq chosen :none) inside (format nil "{{{~a(~a)}}}" chosen inside)))
            (drop-selection)
            (setf (tuatara.buffer:buffer-point buffer) start)
            (if (eq chosen :none)
                (message "~a: the call taken away" inside)
                (progn (setf *org-last-macro* chosen)
                       (message "~a: in ~a now" inside chosen)))))
        (multiple-value-bind (start end)
            (if (region-active-p buffer) (selection-range) (token-at buffer point))
          (if (or (null start) (= start end))
              (message "Select the text to wrap, or put point on a word")
              (let ((inside (tuatara.buffer:buffer-string buffer :start start :end end)))
                (if (search ")}}}" inside)
                    (message "The text holds )}}}, which would end the call early")
                    (let ((chosen (completing-read "Wrap in: " (org-macro-choices buffer) :require-match t :keep-order t
                                                   :default *org-last-macro*)))
                      (let ((after (tuatara.buffer:buffer-replace buffer start end (format nil "{{{~a(~a)}}}" chosen inside))))
                        (drop-selection)
                        (setf *org-last-macro* chosen
                              (tuatara.buffer:buffer-point buffer) after)
                        (message "~a: in ~a" (string-trim '(#\Newline) inside) chosen))))))))))

(define-command org-macro-unwrap ()
  "In an Org note, take away the macro call point is in, its text left as it
was: {{{R(John Otago)}}} becomes John Otago. \\[org-macro-wrap] wraps text again."
  (let* ((buffer (current-buffer))
         (call (macro-call-on-line buffer (tuatara.buffer:buffer-point buffer))))
    (if (null call)
        (message "Point is in no macro call with text to leave")
        (destructuring-bind (start end name inside) call
          (tuatara.buffer:buffer-replace buffer start end inside)
          (drop-selection)
          (setf (tuatara.buffer:buffer-point buffer) start)
          (message "~a: the call of ~a taken away" inside name)))))

(defun install-redactions ()
  "Give *Redactions* the keys of a list whose entries are opened, offer
\\[check-redactions] in Org and LaTeX buffers, and bind \\`C-c m' and \\`C-c M' in
Org notes to wrap text in a macro (\\[org-macro-wrap]) and take a call away
\\(\\[org-macro-unwrap])."
  (entry-list-keys 'redactions-mode "*Redactions*")
  (commands-for-mode 'org-mode "Org and LaTeX files" 'check-redactions)
  (commands-for-mode 'org-mode "Org notes" 'org-macro-wrap 'org-macro-unwrap)
  (define-key (mode-keymap 'org-mode) "C-c m" 'org-macro-wrap)
  (define-key (mode-keymap 'org-mode) "C-c M" 'org-macro-unwrap)
  (commands-for-mode 'latex-mode "Org and LaTeX files" 'check-redactions))
