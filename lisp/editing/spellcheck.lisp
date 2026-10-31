;;;; lisp/editing/spellcheck.lisp --- Words spelt wrong, marked as they are shown: spellcheck-mode.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Emacs's jinx does it: only the words shown in windows are checked, as
;;;; they are shown, each word's answer kept, so that nothing waits on the
;;;; speller and a large file costs no more than a small one. In text, Org,
;;;; Markdown and LaTeX every word is checked but those in code, links, dates,
;;;; tags and the like; in code only comments and strings. What is which is
;;;; read from the colours the text is shown in. Off unless the init file or
;;;; M-x turns it on; never in a file opened safely.

(in-package #:tuatara.defaults)

(defvar *spellcheck-languages* nil
  "The languages words are checked in, a list of names as enchant knows them,
such as '(\"en_NZ\" \"sv_SE\"): a word is right when one of them takes it.
NIL: the language of the system's locale (LC_ALL, LC_MESSAGES or LANG, as
en_NZ.UTF-8), else en_US. \\[spellcheck-languages] sets them for one buffer.")

(defvar *spellcheck-excluded-faces*
  '(:org-code :org-verbatim :org-link :org-meta-line :org-block :org-block-begin-line :org-date
    :org-tag :org-property-value :org-special-keyword :org-drawer :org-macro :org-cite :org-footnote
    :org-formula :org-latex-and-related :org-document-info-keyword :org-checkbox :org-priority
    :org-todo :org-done :org-wait :org-cancelled :link :markup.raw :markup.link.url :keyword :function :variable :constant)
  "In text (Org, Markdown, LaTeX and the like), the faces whose words are not
checked: code, links, dates, tags, keywords. A face counts for those named
after it with a dot (:markup.raw for :markup.raw.block).")

(defvar *spellcheck-comment-faces* '(:comment)
  "In code, the faces of comments, whose words are checked (unless
*SPELLCHECK-MODES* says :strings): those named after them with a dot too
\(:comment.documentation).")

(defvar *spellcheck-string-faces* '(:string)
  "In code, the faces of strings, whose words are checked (unless
*SPELLCHECK-MODES* says :comments): those named after them with a dot too.")

(defun spellcheck-never-p (buffer)
  "True when BUFFER is never checked: opened safely, or shown as octets."
  (or (tuatara.io:safe-visit-p buffer) (tuatara.display:hex-view-p buffer)))

(defun spellcheck-on-words (buffer)
  "What spellcheck checks in BUFFER, in words: comments and strings in code,
every word in text; on, for every buffer, when BUFFER is NIL."
  (case (and buffer (spellcheck-kind buffer))
    (:code "comments and strings")
    (:text "every word")
    (t "on")))

(define-display-toggle spellcheck "Spellcheck"
  "Mark the words spelt wrong in the current buffer, or stop: as they are
shown, underlined in the :spellcheck face; in code only in comments and
strings, unless *SPELLCHECK-MODES* says otherwise for its mode (:comments,
:strings, or :all for the code too). \\[spellcheck-correct] corrects one,
\\[spellcheck-next] and \\[spellcheck-previous] go to the next and the one
before."
  :command spellcheck-mode :lookup spellcheck-p
  :global *global-spellcheck-mode*
  :what "mark the words spelt wrong"
  :example "'((org-mode . t) (prog-mode . :comments) (python-mode . :all) (csv-mode . nil))"
  :values ((t spellcheck-on-words) (:comments "comments") (:strings "strings") (:all "code, comments and strings"))
  :unless (spellcheck-never-p "in a file opened safely or shown as octets"))

(defun locale-language ()
  "The language the system's locale names (LC_ALL, LC_MESSAGES or LANG), as
en_NZ for en_NZ.UTF-8; NIL for C, POSIX or none."
  (let ((locale (find-if (lambda (v) (and v (plusp (length v))))
                         (mapcar #'uiop:getenv '("LC_ALL" "LC_MESSAGES" "LANG")))))
    (when locale
      (let ((name (subseq locale 0 (position-if (lambda (c) (find c ".@")) locale))))
        (and (cl-ppcre:scan "^[a-z]{2,3}(_[A-Z]{2})?$" name) name)))))

(defun spellcheck-languages-of (buffer)
  "The languages BUFFER's words are checked in: its own (\\[spellcheck-languages]),
else *SPELLCHECK-LANGUAGES*, else the locale's, else en_US."
  (or (tuatara.buffer:buffer-local buffer :spellcheck-languages)
      *spellcheck-languages*
      (list (or (locale-language) "en_US"))))

(defun spellcheck-kind (buffer)
  "How BUFFER's words are checked: :CODE in a programming mode (comments and
strings, or as SPELLCHECK-P says), :TEXT in a mode of text or none (all but
code, links and the like), NIL in lists, help and the other buffers made by
commands."
  (let ((mode (buffer-major-mode buffer)))
    (cond ((typep mode 'prog-mode) :code)
          ((typep mode 'special-mode) nil)
          (t :text))))

(defun spelling-faces-checked (buffer)
  "In code, the faces whose words are checked in BUFFER, as SPELLCHECK-P says
there: comments and strings (T, and when spellcheck is off there), :comments,
:strings; :ALL for every word."
  (case (spellcheck-p buffer)
    (:comments *spellcheck-comment-faces*)
    (:strings *spellcheck-string-faces*)
    (:all :all)
    (t (append *spellcheck-comment-faces* *spellcheck-string-faces*))))

(defun face-among-p (face faces)
  "True when FACE (a face name, or a list of them) is one of FACES, or named
after one of them with a dot."
  (if (listp face)
      (some (lambda (f) (face-among-p f faces)) face)
      (let ((name (symbol-name face)))
        (some (lambda (f)
                (let ((other (symbol-name f)))
                  (or (eq f face)
                      (and (> (length name) (length other))
                           (string= other name :end2 (length other))
                           (char= (char name (length other)) #\.)))))
              faces))))

(defun word-character-p (c)
  "True for a character words are made of: a letter, or a mark joined to one
(the accent of a decomposed é, the vowel signs of Devanagari)."
  (or (alpha-char-p c) (member (sb-unicode:general-category c) '(:mn :mc :me))))

(defparameter *unspaced-scripts* '(:han :hiragana :katakana :thai :lao :khmer :myanmar :tibetan)
  "The scripts written without spaces between words, whose runs of letters
are no words to spell one by one.")

(defun unspaced-word-p (word)
  "True when WORD holds a letter of a script written without spaces
(*UNSPACED-SCRIPTS*)."
  (some (lambda (c) (member (sb-unicode:script c) *unspaced-scripts*)) word))

(defparameter *spelling-reach* 256
  "Octets looked at beyond what a row shows for the rest of a word it cuts
short (a line truncated at the window's edge or scrolled sideways).")

(defun whole-words-segment (buffer start end)
  "START and END, octets of BUFFER, moved out to the blanks before and after,
at most *SPELLING-REACH* octets: a word a row cuts short is spelt whole."
  (flet ((blank-p (c) (member c '(#\Space #\Tab #\Newline #\Return))))
    (let* ((size (tuatara.buffer:buffer-size buffer))
           (before (tuatara.buffer:buffer-string buffer :start (max 0 (- start *spelling-reach*)) :end start))
           (after (tuatara.buffer:buffer-string buffer :start end :end (min size (+ end *spelling-reach*))))
           (back (let ((blank (position-if #'blank-p before :from-end t)))
                   (if blank (subseq before (1+ blank)) "")))
           (on (let ((blank (position-if #'blank-p after))) (if blank (subseq after 0 blank) ""))))
      (values (- start (tuatara.text:string-octet-length back))
              (+ end (tuatara.text:string-octet-length on))))))

(defun apostrophe-p (c)
  "True for an apostrophe, which a word may hold between its letters (don't, it’s)."
  (or (char= c #\') (char= c #\RIGHT_SINGLE_QUOTATION_MARK)))

(defun spelling-words (text offset kind)
  "The words of TEXT to spell, its first character at octet OFFSET, each
(START END WORD) in octets, in order. A word is a run of letters with an
apostrophe between two of them; single letters are left, and so is every word
of a run of text that is a web address, an e-mail address, a path or a name
with _ in it, and a word next to a digit. In code (KIND :CODE) a word with a
capital after its first letter (camelCase) is left too."
  (let ((words '()) (n (length text)) (i 0) (at offset))
    (flet ((octets (from to) (loop for k from from below to sum (tuatara.text:char-octet-length (char text k)))))
      (loop while (< i n)
            do (if (member (char text i) '(#\Space #\Tab #\Newline #\Return))
                   (progn (incf at (tuatara.text:char-octet-length (char text i))) (incf i))
                   ;; a run of text up to the next blank
                   (let* ((j (or (position-if (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return))) text :start i) n))
                          (run-at at)
                          (skip (or (search "://" text :start2 i :end2 j) (find #\@ text :start i :end j)
                                    (find #\_ text :start i :end j) (find #\\ text :start i :end j)
                                    (find (char text i) "/~"))))
                     (unless skip
                       (let ((k i) (k-at run-at))
                         (loop while (< k j)
                               do (if (not (word-character-p (char text k)))
                                      (progn (incf k-at (tuatara.text:char-octet-length (char text k))) (incf k))
                                      (let ((e k))
                                        (loop while (and (< e j)
                                                         (or (word-character-p (char text e))
                                                             (and (apostrophe-p (char text e)) (< (1+ e) j)
                                                                  (word-character-p (char text (1+ e))) (> e k))))
                                              do (incf e))
                                        (let ((word (subseq text k e))
                                              (end-at (+ k-at (octets k e))))
                                          (unless (or (< (length word) 2) (> (length word) *longest-word*)
                                                      (unspaced-word-p word)
                                                      (and (> k i) (digit-char-p (char text (1- k))))
                                                      (and (< e j) (digit-char-p (char text e)))
                                                      (and (eq kind :code) (find-if #'upper-case-p word :start 1)))
                                            (push (list k-at end-at word) words))
                                          (setf k e k-at end-at)))))))
                     (incf at (octets i j))
                     (setf i j)))))
    (nreverse words)))

(defparameter *local-words-reach* (* 64 1024)
  "Octets from the end of a file within which its own word list (its
LocalWords: lines) is looked for.")

(defun tail-kept-p (from to start)
  "True when the text of version TO ends in the same octets as that of version
FROM, a version before it, did from START on, and these are its last
*LOCAL-WORDS-REACH* octets (or the whole of it): no change made between them
touched them."
  (let ((span (cons start (tuatara.text:rope-bytes (tuatara.text:version-rope from)))))
    (loop for version = from then next
          for next = (tuatara.text:version-next version)
          until (eq version to)
          do (unless (and next (setf span (tuatara.buffer:span-after-batch span (tuatara.text:version-batch next))))
               (return-from tail-kept-p nil)))
    (let ((size (tuatara.text:rope-bytes (tuatara.text:version-rope to))))
      (and (= (cdr span) size) (= (car span) (max 0 (- size *local-words-reach*)))))))

(defun local-words (buffer)
  "The words BUFFER's own word list takes as right: those after
\"LocalWords:\" on its lines near its end (*LOCAL-WORDS-REACH*), written
as comments (# LocalWords: Aotearoa Ōtaki in Org, ;; LocalWords: in
Lisp), a table of strings; read again when the text near the end changes."
  (let ((version (tuatara.buffer:buffer-version buffer))
        (kept (tuatara.buffer:buffer-local buffer :local-words)))
    (cond ((and kept (eq (first kept) version)) (third kept))
          ((and kept (tail-kept-p (first kept) version (second kept)))
           ;; an edit further up: the same words
           (setf (first kept) version
                 (second kept) (max 0 (- (tuatara.buffer:buffer-size buffer) *local-words-reach*)))
           (third kept))
          (t
           (let* ((size (tuatara.buffer:buffer-size buffer))
                  (start (max 0 (- size *local-words-reach*)))
                  (text (tuatara.text:lenient-string
                         (tuatara.text:rope-octets (tuatara.buffer:buffer-text buffer) :start start :end size)))
                  (words (make-hash-table :test 'equal)))
             ;; the list's own name is no word spelt wrong
             (setf (gethash "LocalWords" words) t)
             (cl-ppcre:do-register-groups (list) ("LocalWords:([^\\n]*)" text)
               (dolist (w (uiop:split-string list :separator '(#\Space #\Tab)))
                 (when (plusp (length w)) (setf (gethash w words) t))))
             (setf (tuatara.buffer:buffer-local buffer :local-words) (list version start words))
             words)))))

(defun add-local-word (buffer word)
  "Add WORD to BUFFER's own word list: to its last LocalWords: line near its
end while that stays within the fill column, else on a new one at the end,
written as a comment of the buffer's language (# in Org, ;; in Lisp), or as a
line of its own in plain text. Nothing is added when the list has WORD
already: :LISTED then, else T. Refused, with an error, where the language has
no comments to hold the list in (CSV, Markdown), as a line there would be
part of the data or show as text."
  (check-writable buffer)
  (let ((mode (buffer-major-mode buffer)))
    (unless (or (mode-comment-syntax mode) (member (type-of mode) '(text-mode fundamental-mode)))
      (error (substitute-command-keys "~a has no comments to keep a word list in: \\[spellcheck-correct] keeps “~a” while the editor runs, or in a language's word list" :for-format t)
             (tuatara.modes:mode-lighter (type-of mode)) word)))
  (when (gethash word (local-words buffer))
    (return-from add-local-word :listed))
  (let* ((size (tuatara.buffer:buffer-size buffer))
         (from (max 0 (- size *local-words-reach*)))
         (text (tuatara.buffer:buffer-string buffer :start from :end size))
         (at (search "LocalWords:" text :from-end t))
         (line-end (and at (tuatara.buffer:buffer-line-end buffer (+ from (tuatara.text:string-octet-length (subseq text 0 at)))))))
    (if (and line-end
             (<= (+ (tuatara.buffer:buffer-column buffer line-end) 1 (length word)) (buffer-fill-column buffer)))
        (tuatara.buffer:buffer-insert buffer line-end (format nil " ~a" word))
        (multiple-value-bind (marker commenting) (mode-comment-syntax (buffer-major-mode buffer))
          (let ((prefix (or commenting marker)))
            (tuatara.buffer:buffer-insert
             buffer size
             (format nil "~:[~;~%~]~@[~a ~]LocalWords: ~a~%"
                     (and (plusp size) (not (member (tuatara.buffer:buffer-char-at buffer (1- size)) '(#\Newline #\Return))))
                     prefix word))))))
  t)

(defun spelling-in-rows (buffer rows syntax)
  "The words of BUFFER that ROWS show, read as SYNTAX (the ranges the text is
coloured in, (START END FACE)) says they are to be spelt: as three values,
those spelt wrong and those not yet answered, each (START END WORD) in order,
and the languages they are spelt in."
  (let* ((kind (spellcheck-kind buffer))
         (code-faces (and (eq kind :code) (spelling-faces-checked buffer)))
         (languages (spellcheck-languages-of buffer))
         (local (and kind (local-words buffer)))
         (ranges (sort (copy-list syntax) #'< :key #'first))
         (active '()) (wrong '()) (unknown '()))
    (when kind
      (dolist (segment (tuatara.display:shown-ranges rows :join 2 :shown t :buffer buffer))
        (dolist (word (multiple-value-bind (start end) (whole-words-segment buffer (car segment) (cdr segment))
                        (spelling-words (tuatara.buffer:buffer-string buffer :start start :end end) start kind)))
          (let ((at (first word)))
            ;; the ranges the word's first letter is coloured by
            (loop while (and ranges (<= (first (first ranges)) at)) do (push (pop ranges) active))
            (setf active (delete-if (lambda (r) (<= (second r) at)) active))
            (when (and (not (gethash (third word) local))
                       (if (eq kind :code)
                      (or (eq code-faces :all)
                          (some (lambda (r) (face-among-p (third r) code-faces)) active))
                      (notany (lambda (r) (face-among-p (third r) *spellcheck-excluded-faces*)) active)))
              (case (spelling-known languages (third word))
                (:wrong (push word wrong))
                ((nil) (push word unknown))))))))
    (values (nreverse wrong) (nreverse unknown) languages)))

(defvar *spelling-reports* (make-hash-table :test 'equal)
  "The lists of languages words have been checked in, as their keys, each
said of once when they cannot all be checked.")

(defun spelling-note-languages (languages)
  "Have it said once, soon, when LANGUAGES cannot all be checked: enchant
missing at once, a language without a dictionary by the speller thread once
it knows (MESSAGE takes it to the editor's)."
  (let ((key (languages-key languages)))
    (unless (gethash key *spelling-reports*)
      (setf (gethash key *spelling-reports*) t)
      (if (not (enchant-p))
          (message "Spellcheck needs enchant (libenchant-2), which could not be loaded: nothing is checked")
          (speller-run (lambda ()
                         (let ((missing (remove-if #'enchant-dictionary languages)))
                           (when missing
                             (message "Spellcheck: no dictionary for ~{~a~^, ~}, of ~a; install hunspell's or nuspell's"
                                      missing key)))))))))

(defvar *spelling-shown* (make-hash-table :test 'eq :weakness :key)
  "For each window, the words it was last found to show spelt wrong, each
\(START END :SPELLCHECK), and what they were found from.")

(defun spellcheck-highlights (editor window rows syntax)
  "The words spelt wrong in ROWS of WINDOW, in the :spellcheck face, where
its buffer is checked (SPELLCHECK-P); the words shown not yet answered are
asked about, and the window redrawn when they are. Found again only when the
text, what is shown of it, its colours, the languages or the answers change."
  (declare (ignore editor))
  (let ((buffer (tuatara.display:window-buffer window)))
    (when (and (plusp (length rows)) (spellcheck-p buffer))
      (let ((key (list (tuatara.buffer:buffer-version buffer) (car *spelling-generation*)
                       (spellcheck-p buffer) (spellcheck-languages-of buffer)
                       (tuatara.display:shown-ranges rows :join 2 :shown t :buffer buffer) syntax))
            (kept (gethash window *spelling-shown*)))
        (if (and kept (equal (car kept) key))
            (cdr kept)
            (multiple-value-bind (wrong unknown languages) (spelling-in-rows buffer rows syntax)
              (spelling-note-languages languages)
              (let ((marks (loop for (start end) in wrong collect (list start end :spellcheck))))
                (if unknown
                    (progn (remhash window *spelling-shown*)
                           (spelling-ask languages (mapcar #'third unknown)))
                    (setf (gethash window *spelling-shown*) (cons key marks)))
                marks)))))))

;;; Correcting

(defun spelling-answered-in (window &optional rows)
  "The words ROWS of WINDOW show spelt wrong, each (START END WORD), once
every one of them is answered, the editor waiting meanwhile (\\`C-g' stops
it, and NIL is returned); and the languages, as a second value. Without
ROWS, the rows WINDOW shows, read again after the wait, as the window may
have been drawn anew meanwhile."
  (let ((buffer (tuatara.display:window-buffer window)))
    (flet ((wrong-in-rows ()
             (let ((rows (or rows (tuatara.display:window-rows window))))
               (spelling-in-rows buffer rows (window-syntax *editor* window rows)))))
      (multiple-value-bind (wrong unknown languages) (wrong-in-rows)
        (if (null unknown)
            (values wrong languages)
            (let ((words (mapcar #'third unknown)))
              (spelling-ask languages words)
              ;; asked again while waiting: a lot dropped for newer ones (*SPELLING-BACKLOG*) still gets answered
              (when (wait-until-interruptibly (lambda ()
                                                (spelling-ask languages words)
                                                (every (lambda (w) (spelling-known languages w)) words))
                                              :message "Checking the spelling...")
                (values (nth-value 0 (wrong-in-rows)) languages))))))))

(defun spelling-ready (buffer)
  "Signal an error unless BUFFER's words can be checked here."
  (cond ((spellcheck-never-p buffer) (error "Spelling is never checked in a file opened safely or shown as octets"))
        ((null (spellcheck-kind buffer)) (error "Spelling is not checked in ~a" (tuatara.buffer:buffer-name buffer)))
        ((not (enchant-p)) (error "Spellcheck needs enchant (libenchant-2), which could not be loaded"))))

(defun spelling-word-at (buffer position)
  "The word around POSITION in BUFFER, as SPELLING-WORDS finds words, (START
END WORD); NIL when POSITION is in none."
  (let* ((line-start (tuatara.buffer:buffer-line-start buffer position))
         (line-end (tuatara.buffer:buffer-line-end buffer position))
         (start (max line-start (- position 256)))
         (end (min line-end (+ position 256))))
    (find-if (lambda (w) (<= (first w) position (second w)))
             (spelling-words (tuatara.buffer:buffer-string buffer :start start :end end) start
                             (spellcheck-kind buffer)))))

(defun spelling-rows-from (window start forward)
  "Rows of WINDOW's buffer about START, a window's height of them: from START's
row on when FORWARD, else those ending at it."
  (let ((height (max 1 (tuatara.display:window-height window))))
    (tuatara.display:window-layout-rows
     window
     (if forward
         (tuatara.display:window-row-start window start)
         (tuatara.display:rows-back window start height))
     (1+ height))))

(defparameter *spelling-search-limit* (* 4 1024 1024)
  "Octets \\[spellcheck-next] and \\[spellcheck-previous] read past point
looking for a word spelt wrong before they say there is none.")

(defun spelling-search (forward)
  "Point to the start of the next word spelt wrong after it (FORWARD), or the
one before; the count's worth. Says so when there is none within
*SPELLING-SEARCH-LIMIT* octets."
  (let* ((window (current-window)) (buffer (current-buffer)))
    (spelling-ready buffer)
    (dotimes (i (prefix))
      (let ((point (point)) (at (point)) (found nil))
        (loop
          (let* ((rows (spelling-rows-from window at forward))
                 (wrong (or (spelling-answered-in window rows) '())))
            (when (zerop (length rows)) (return))
            (setf found (if forward
                            (find-if (lambda (w) (> (first w) point)) wrong)
                            (find-if (lambda (w) (< (first w) point)) wrong :from-end t)))
            (when found (return))
            (let ((next (if forward
                            (tuatara.display:row-end (aref rows (1- (length rows))))
                            (tuatara.display:row-start (aref rows 0)))))
              (when (or (= next at)
                        (if forward
                            (tuatara.display:row-last-p (aref rows (1- (length rows))))
                            (zerop next))
                        (> (abs (- next point)) *spelling-search-limit*))
                (return))
              (setf at next))))
        (if found
            (goto (first found))
            (return (message "No word spelt wrong ~:[before~;after~] point" forward)))))))

(define-command spellcheck-next ()
  "Go to the next word spelt wrong after point (the count's worth), as
\\[spellcheck-mode] marks them, though they are not shown yet."
  (spelling-search t))

(define-command spellcheck-previous ()
  "Go to the word spelt wrong before point (the count's worth), as
\\[spellcheck-mode] marks them."
  (spelling-search nil))

(define-command spellcheck-correct ()
  "Correct the word spelt wrong at point, or else the one nearest point in the
window: choose one of enchant's suggestions, or type the word as it should be,
or keep the word: while the editor runs, in the file's own word list (as
\\[spellcheck-add-word-to-file] does), or for good in a language's word list
\(as \\[spellcheck-add-word] does)."
  (let* ((window (current-window)) (buffer (current-buffer)) (point (point)))
    (spelling-ready buffer)
    (multiple-value-bind (wrong languages) (spelling-answered-in window)
      (let ((target (or (find-if (lambda (w) (<= (first w) point (second w))) wrong)
                        (first (sort (copy-list wrong) #'<
                                     :key (lambda (w) (min (abs (- (first w) point)) (abs (- (second w) point)))))))))
        (if (null target)
            (message "No word spelt wrong in sight")
            (destructuring-bind (start end word) target
              (let ((suggested (spelling-suggest languages word)))
                (when (wait-until-interruptibly (lambda () (cdr suggested)) :message "Looking for suggestions...")
                  (let ((chosen (completing-read
                                 (format nil "Correct ~a: " word)
                                 (append (loop for (s . language) in (car suggested)
                                               collect (make-candidate s :annotation language :group "Suggestions"))
                                         (list (make-candidate (format nil "Keep “~a” while the editor runs" word)
                                                               :value '(:session) :group "Keep it")
                                               (make-candidate (format nil "Keep “~a” in this file's word list" word)
                                                               :value '(:file) :group "Keep it"))
                                         (loop for language in languages
                                               collect (make-candidate (format nil "Add “~a” to the ~a word list" word language)
                                                                       :value (list :add language) :group "Keep it")))
                                 :keep-order t)))
                    (cond ((equal chosen '(:session))
                           (spelling-add languages word (first languages) :session t)
                           (message "“~a” is taken as right while the editor runs" word))
                          ((equal chosen '(:file))
                           (say-local-word-added (add-local-word buffer word) word))
                          ((and (consp chosen) (eq (first chosen) :add))
                           (spelling-add languages word (second chosen))
                           (message "Added “~a” to the ~a word list" word (second chosen)))
                          ((and (stringp chosen) (plusp (length chosen)) (string/= chosen word))
                           (check-writable buffer)
                           (tuatara.buffer:buffer-replace buffer start end chosen)
                           (goto (+ start (tuatara.text:string-octet-length chosen))))))))))))))

(define-command spellcheck-add-word ()
  "Add the word at point to a language's personal word list, kept by enchant
in ~/.config/enchant/, the list other programs using enchant share: asked which
language when words are checked in more than one."
  (let ((buffer (current-buffer)))
    (spelling-ready buffer)
    (let ((word (spelling-word-at buffer (point)))
          (languages (spellcheck-languages-of buffer)))
      (if (null word)
          (message "No word at point")
          (let ((language (if (rest languages)
                              (completing-read (format nil "Add “~a” to the word list of: " (third word)) languages
                                               :require-match t)
                              (first languages))))
            (spelling-add languages (third word) language)
            (message "Added “~a” to the ~a word list" (third word) language))))))

(defun say-local-word-added (added word)
  "Say that WORD was kept in the file's word list, or with ADDED :LISTED that
it was there already (ADD-LOCAL-WORD)."
  (message "~:[Kept~;Already kept~] “~a” in this file's word list" (eq added :listed) word))

(define-command spellcheck-add-word-to-file ()
  "Keep the word at point as right in this file only: added to the file's own
word list, a LocalWords: line at its end, written as a comment (# in Org,
;; in Lisp) that other spelling checkers read too. A word the file names that is
no word elsewhere (a name, a case number) stays out of the language's list."
  (let ((buffer (current-buffer)))
    (spelling-ready buffer)
    (let ((word (spelling-word-at buffer (point))))
      (if (null word)
          (message "No word at point")
          (say-local-word-added (add-local-word buffer (third word)) (third word))))))

(define-command spellcheck-languages ()
  "Set the languages the current buffer's words are checked in, by their
names as enchant knows them, apart by spaces (en_NZ sv_SE): a word is right
when one of them takes it. Empty: as *SPELLCHECK-LANGUAGES* says."
  (let* ((buffer (current-buffer))
         (typed (read-string (format nil "Check the spelling in (~{~a~^ ~}): " (spellcheck-languages-of buffer))))
         (languages (tuatara.text:split-words (string-trim " " typed) " ")))
    (setf (tuatara.buffer:buffer-local buffer :spellcheck-languages) languages)
    (message "Spelling checked in ~{~a~^ ~}" (spellcheck-languages-of buffer))))

(defun install-spellcheck (editor)
  "Mark the words spelt wrong in the windows whose buffers want it, redrawn
as the speller answers, and say what cannot be checked."
  (setf *spelling-wake-function* (lambda () (wake-editor editor)))
  (pushnew 'spellcheck-highlights tuatara.command:*text-highlight-functions*))
