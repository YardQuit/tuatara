;;;; test/search-agreement.lisp --- Every way of searching agrees with one scan of the whole text.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A search over a rope goes window by window and piece by piece on every
;;;; core, from a needle every match holds, counted without keeping the
;;;; matches, the first on each line for a replacement, forward and backward
;;;; from anywhere. Each of these is held against one scan of the whole text as
;;;; a string (REGEX-ORACLE, ORACLE-POSITIONS), for patterns of every kind, on
;;;; texts of logs, addresses and every kind of character, with the windows
;;;; and pieces made small so that matches fall across their edges. What
;;;; investigators search for has to be found every time, and only that.

(in-package #:tuatara.tests)

(define-test search-agreement
  :description "Literal and regex search, serial and parallel, all, counted, first per line, forward and backward, against one scan of the whole text.")

(defun evidence-text (lines &key crlf)
  "LINES lines like those an investigation searches: logs with dates, times,
addresses, users, hashes, amounts and messages in several languages and
cases, a line now and then empty or long, or a block of Base64 and an onion
address; ending with a carriage return
and line feed when CRLF. The same each time it is asked for, so that a text
that shows a fault shows it again."
  (let ((*random-state* (sb-ext:seed-random-state 2026))
        (users '("aroha" "Hemi" "JOE" "mere.smith" "tane_99" "Ōtautahi" "ζωή" "李明"))
        (words '("login" "LOGIN" "failed" "kia ora" "transfer" "Transfer" "ERROR" "error" "warn" "kōrero"
                 "payment" "Αθήνα" "中文" "😀" "café" "naïve" "x" "")))
    (with-output-to-string (o)
      (dotimes (i lines)
        (let ((r (random 20)))
          (cond ((zerop r) nil)
                ((= r 1) (write-string (make-string (+ 200 (random 1000)) :initial-element #\a) o))
                ;; a block of Base64 wrapped over lines of one length, and an onion address
                ((= r 2) (format o "S2lhIG9yYSBrb3V0b3Uga2F0b2Es~%IHRoZSBrZWEgaXMgYSBwYXJyb3Q=~%via ~a.onion"
                                 (if (zerop (random 2)) "duckduckgogg42xjoc72x3sjasowoarfbgcmvfimaftt6twagswzczad" "expyuzz4wqqyqhjn")))
                ;; identifiers in hex, found by their shape: a UUID or GUID, a MAC address, a digest
                ((= r 3) (format o "session ~(~8,'0x-~4,'0x-~x~3,'0x-~x~3,'0x-~12,'0x~) {~:@(~8,'0x-~4,'0x-~4,'0x-~4,'0x-~12,'0x~)} ~
                                    on ~(~2,'0x:~2,'0x:~2,'0x:~2,'0x:~2,'0x:~2,'0x~) sum ~(~32,'0x~) seen ~d.~d.2026 ~d/~d/2026"
                                 (random (expt 16 8)) (random (expt 16 4)) (random 10) (random (expt 16 3))
                                 (+ 8 (random 6)) (random (expt 16 3)) (random (expt 16 12))
                                 (random (expt 16 8)) (random (expt 16 4)) (random (expt 16 4)) (random (expt 16 4)) (random (expt 16 12))
                                 (random 256) (random 256) (random 256) (random 256) (random 256) (random 256)
                                 (random (expt 16 32)) (1+ (random 31)) (1+ (random 12)) (1+ (random 12)) (1+ (random 31))))
                (t (format o "2026-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d ~a user=~a ip=~d.~d.~d.~d ~a ~a@example.~a #~36r £~:d.~2,'0d ~a"
                           (1+ (random 12)) (1+ (random 28)) (random 24) (random 60) (random 60)
                           (nth (random (length words)) words) (nth (random (length users)) users)
                           (random 256) (random 256) (random 256) (random 256)
                           (nth (random (length words)) words) (nth (random (length users)) users)
                           (nth (random 3) '("org" "nz" "com")) (random (expt 36 12))
                           (random 100000) (random 100) (nth (random (length words)) words)))))
        (write-string (if crlf (format nil "~c~c" #\Return #\Newline) (string #\Newline)) o)))))

(defparameter *agreement-patterns*
  '(;; first, a pattern that reads spaces and # its own way: those after it keep theirs
    "(?x) \\d{4} - \\d\\d  # a year and a month"
    ;; plain text, and plain text a regex holds
    "login" "LOGIN" "kia ora" "kōrero" "中文" "😀" "café" "user=Hemi"
    ;; sets, named sets, short sets, properties
    "[0-9]+" "[[:digit:]]{4}-[[:digit:]]{2}" "[[:alpha:]]+" "[[:upper:]]{2,}" "[[:^alnum:][:space:]]+" "[[:xdigit:]]{8,}"
    "\\d+\\.\\d+" "\\w+@\\w+\\.(?:org|nz|com)" "\\b\\d{1,3}(?:\\.\\d{1,3}){3}\\b" "\\s{2,}" "\\h+" "\\S+=\\S+"
    "\\p{L}+" "\\p{Lu}\\p{Ll}+" "\\p{Greek}+" "\\p{Han}+" "\\P{L}{5,}" "\\p{Sc}\\d"
    ;; anchors and edges
    "^2026-0[1-6]" "\\d$" "\\bfailed\\b" "\\Bor" "^\\S+ \\d\\d:"
    ;; how many: greedy, lazy, possessive
    "a{100,}" "user=.*? " "#\\w+?\\b" "\\d++\\." "(?:ab|a)+b"
    ;; groups, either, again
    "(error|warn)" "(?i)transfer|payment" "(\\w)\\1" "(?<d>\\d)\\k<d>" "(?>\\d+)-"
    ;; looking around, conditions, flags
    "(?<=user=)\\w+" "\\w+(?=@)" "^(?!.*ERROR).*kia" "(?<!\\d)\\d{2}(?!\\d)" "(<)?\\d+(?(1)>)" "(?i:t)RANSFER" "(?s)login.{0,40}failed"
    ;; named patterns: addresses, and card numbers checked by Luhn as they are matched
    "[[:ipv4:]]" "[[:ipaddress:]]" "[[:email:]]" "[[:creditcard:]]" "[[:onion:]]" "[[:base32:]]" "[[:base64:]]" "[[:xdigit:]]{4}|[[:md5:]]" "[[:email:]] [[:ipv4:]]" "(?<=user=)\\w+ ip=[[:ipv4:]]"
    ;; found from their shape, every match a run of sets of octets: hex identifiers, dates
    "[[:uuid:]]" "[[:guid:]]" "[[:macaddress:]]" "[[:md5:]]" "[0-9a-f]{12}" "[0-9A-F]{8}-" "sum [0-9a-f]{32}"
    "[[:date:]]" "[[:dateutc<2026-07-01:]]" "[[:date:2026-03-01..2026-03-31:]]" "[[:datedmy:]]" "[[:datemdy<06/30/2026:]]"
    "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:" "(?:on|sum) [0-9a-f]{2}"
    ;; across lines
    "ora\\nfailed" "\\d\\n2026" "\\R2026")
  "Patterns of every kind, none of which matches an empty text, searched for
in every evidence text.")

(defun agreement-sizes ()
  "The window and piece sizes the agreement is checked at: as they are, and
small, so that matches fall across their edges."
  (list (list tuatara.syntax:*serial-window* tuatara.syntax:*parallel-piece*) '(700 900)))

(defmacro with-evidence-seed (&body body)
  "Run BODY with the random numbers always the same, so that a text that shows
a fault shows it again on every run."
  `(let ((*random-state* (sb-ext:seed-random-state 2026)))
     ,@body))

;; A text and where each of its characters starts in its octets
(defstruct (scanned (:constructor %make-scanned (string offsets)) (:copier nil))
  "A text searched by one scan of it whole: STRING, and OFFSETS, the octet
each of its characters starts at, its length in octets last."
  string offsets)

(setf (documentation '%make-scanned 'function) "Return a SCANNED of STRING and its OFFSETS."
      (documentation 'scanned-p 'function) "True when OBJECT is a SCANNED."
      (documentation 'scanned-string 'function) "A SCANNED's text, a string."
      (documentation 'scanned-offsets 'function) "The octet each character of a SCANNED's text starts at, its length last.")

(defun make-scanned (string)
  "STRING with the octet each of its characters starts at."
  (let ((offsets (make-array (1+ (length string)) :element-type 'fixnum)) (b 0))
    (dotimes (i (length string))
      (setf (aref offsets i) b)
      (incf b (let ((code (char-code (char string i)))) (cond ((< code #x80) 1) ((< code #x800) 2) ((< code #x10000) 3) (t 4)))))
    (setf (aref offsets (length string)) b)
    (%make-scanned string offsets)))

(defun next-match-at (scanner scanned start)
  "The octet offsets where the first match of SCANNER in SCANNED's text at or
after the octet offset START begins and ends, two values, scanning from
there as a search from START does; NIL when there is none."
  (let* ((offsets (scanned-offsets scanned))
         (from (let ((low 0) (high (1- (length offsets))))
                 ;; the first character starting at or after START
                 (loop while (< low high)
                       do (let ((middle (ash (+ low high) -1)))
                            (if (< (aref offsets middle) start) (setf low (1+ middle)) (setf high middle))))
                 low)))
    (multiple-value-bind (ms me) (cl-ppcre:scan scanner (scanned-string scanned) :start from :real-start-pos 0)
      (and ms (values (aref offsets ms) (aref offsets me))))))

(defun nearest-before-p (scanner scanned oracle found start)
  "True when FOUND, an octet offset, is where a backward search from START
may stop in SCANNED's text: a match starts there, before START, and none of
the matches of one scan of the whole text, ORACLE as (START END), lies whole
between where that match ends and START."
  (multiple-value-bind (at end) (next-match-at scanner scanned found)
    (and (< found start) (eql at found)
         (notany (lambda (m) (and (>= (first m) end) (< (first m) start))) oracle))))

(defun matcher-matches (rope matcher)
  "Every match of MATCHER in ROPE as (START END), as a replacement gathers them
in parallel (LINE-MATCHES)."
  (mapcar (lambda (m) (subseq m 0 2))
          (tuatara.defaults::line-matches rope matcher t 0 (tuatara.text:rope-bytes rope))))

(define-test (search-agreement regex-agrees)
  :time-limit 900
  ;; each pattern on each text, at each size: every match, how many, the first on each
  ;; line, and the next and the one before from anywhere, as the whole text's scan has them
  (with-evidence-seed
    (dolist (crlf '(nil t))
      (let* ((text (evidence-text 600 :crlf crlf))
             (scanned (make-scanned text))
             (rope (tuatara.text:string->rope text))
             (size (tuatara.text:rope-bytes rope)))
        (dolist (pattern *agreement-patterns*)
          (let* ((matcher (tuatara.defaults::make-pattern-matcher pattern :regex t :crlf crlf))
                 (scanner (tuatara.syntax:compile-regex pattern :crlf crlf))
                 (oracle (mapcar (lambda (m) (subseq m 0 2)) (regex-oracle text scanner))))
            (true matcher "~a compiles" pattern)
            (dolist (sizes (agreement-sizes))
              (let ((tuatara.syntax:*serial-window* (first sizes)) (tuatara.syntax:*parallel-piece* (second sizes)))
                ;; where the matches part, not the lists of them, is what the report keeps
                (let ((apart (mismatch oracle (matcher-matches rope matcher) :test #'equal)))
                  (false apart "every match of ~s~:[~;, CRLF~] at ~a: apart at match ~a" pattern crlf sizes apart))
                (is = (length oracle) (tuatara.defaults::count-line-matches rope matcher 0 size)
                    "how many ~s~:[~;, CRLF~] at ~a" pattern crlf sizes)
                (is = (length (tuatara.defaults::first-per-line rope oracle))
                    (tuatara.defaults::count-first-per-line rope matcher 0 size)
                    "first on each line of ~s~:[~;, CRLF~] at ~a" pattern crlf sizes)
                (unless crlf
                  (loop repeat 3
                        for start = (tuatara.text:rope-char-start rope (random size))
                        do (is eql (next-match-at scanner scanned start)
                               (tuatara.syntax:regex-search-forward rope pattern :start start :threads 3)
                               "next ~s from ~a at ~a" pattern start sizes)
                           (let ((before (tuatara.syntax:regex-search-backward rope pattern :start start :threads 3)))
                             ;; the start of the last match one scan of the whole text has before START
                             (is eql (first (car (last (remove-if-not (lambda (m) (< (first m) start)) oracle)))) before
                                 "before ~s from ~a at ~a" pattern start sizes)
                             (when before
                               (true (nearest-before-p scanner scanned oracle before start)
                                     "before ~s from ~a at ~a: ~a" pattern start sizes before)))))))))))))

(define-test (search-agreement literal-agrees)
  ;; plain text, case kept and folded, ASCII and every kind of character: all of it, how
  ;; many, and from anywhere both ways the nearest place it stands, at each size
  (with-evidence-seed
    (let* ((text (evidence-text 1500))
           (rope (tuatara.text:string->rope text))
           (size (tuatara.text:rope-bytes rope)))
      (dolist (needle '("login" "LOGIN" "user=" "example.nz" "kōrero" "Αθήνα" "中文" "😀" "café" "aaaaaaaaaaaaaaaa"
                        "@example.org #" "2026-1" "£1" "ζωή" "kŌrero" "αθήνα" "CAFÉ" "ōtautahi"))
        (dolist (fold '(nil t))
          (let ((oracle (oracle-positions text needle :case-fold fold))
                (wanted (to-octets (if fold (string-downcase needle) needle)))
                (in (to-octets (if fold (string-downcase text) text))))
            (dolist (sizes (agreement-sizes))
              (let ((tuatara.syntax:*serial-window* (first sizes)) (tuatara.syntax:*parallel-piece* (second sizes)))
                (is equalp (coerce oracle 'simple-vector) (tuatara.syntax:search-all rope needle :case-fold fold :threads 4)
                    "every ~s~:[~;, case folded~] at ~a" needle fold sizes)
                (is = (length oracle)
                    (tuatara.defaults::count-line-matches rope (tuatara.defaults::make-pattern-matcher needle :fold fold) 0 size)
                    "how many ~s~:[~;, case folded~] at ~a" needle fold sizes)
                (loop repeat 6
                      for start = (random size)
                      do (is eql (search wanted in :start2 start)
                             (tuatara.syntax:search-forward rope needle :start start :case-fold fold :threads 3)
                             "next ~s from ~a" needle start)
                         (is eql (search wanted in :from-end t :end2 (min (length in) (+ start (length wanted) -1)))
                             (tuatara.syntax:search-backward rope needle :start start :case-fold fold :threads 3)
                             "before ~s from ~a" needle start))))))))))

(define-test (search-agreement counted-before)
  ;; how many matches come before a match, as C-s counts its place, over a text of
  ;; many pieces: the whole text's scan has as many
  (with-evidence-seed
    (let* ((text (evidence-text 4000))
           (rope (tuatara.text:string->rope text))
           (tuatara.syntax:*parallel-piece* 1000))
      (dolist (pattern '("\\d+" "user=\\w+" "[[:alpha:]]{3}" "kia ora" "\\p{L}+@"))
        (let ((matcher (tuatara.defaults::make-pattern-matcher pattern :regex t))
              (starts (mapcar #'first (regex-oracle text (tuatara.syntax:compile-regex pattern)))))
          (loop repeat 10
                for k = (random (max 1 (length starts)))
                for start = (nth k starts)
                when start
                  do (is = k (tuatara.defaults::count-line-matches rope matcher 0 start)
                         "matches of ~s before the ~:r" pattern (1+ k))))))))

(defun matches-of-scan (text pattern)
  "Every match of PATTERN in TEXT, non-empty ones only, as (START END) octet
offsets, by one scan of the whole text as a string."
  (mapcar (lambda (m) (subseq m 0 2)) (regex-oracle text (tuatara.syntax:compile-regex pattern))))

(defun found-span (&optional start end &rest more)
  "What a search found, as (START END), or NIL when it found nothing."
  (declare (ignore more))
  (and start (list start end)))

(define-test (search-agreement long-line-starts-agree)
  ;; a match whose fixed text lies far into a long line is found from where it begins,
  ;; forward, backward and gathered, as one scan of the whole text finds it
  (dolist (case (list (list (format nil "~a~afoo~%x foo~%" (make-string 5000 :initial-element #\x) "")
                            '("x+foo" "^.*foo" "x{2,}foo" "^x foo"))
                      (list (format nil "~a~a~a~%" (make-string 100 :initial-element #\x)
                                    (make-string 20 :initial-element #\a) (make-string 65530 :initial-element #\b))
                            '("a+" "a+b" "x+a"))))
    (destructuring-bind (text patterns) case
      (let* ((rope (tuatara.text:string->rope text)) (size (tuatara.text:rope-bytes rope)))
        (dolist (pattern patterns)
          (let ((oracle (matches-of-scan text pattern))
                (matcher (tuatara.defaults::make-pattern-matcher pattern :regex t)))
            (true oracle "~s matches" pattern)
            (is equal oracle (matcher-matches rope matcher) "every match of ~s" pattern)
            (is equal (first oracle) (multiple-value-call #'found-span (tuatara.syntax:regex-search-forward rope pattern :threads 2))
                "first ~s" pattern)
            (is eql (first (car (last oracle))) (tuatara.syntax:regex-search-backward rope pattern :start size :threads 2)
                "last ~s" pattern)))))))

(define-test (search-agreement raw-bytes-stand-alone)
  ;; a raw byte in a pattern finds that byte standing alone, never the same octet inside a
  ;; character: as plain text, as a pattern of no special character, and as \xA9
  (let* ((rope (tuatara.text:octets->rope (coerce #(#xC3 #xA9 32 #xA9 32 #xC3 #xA9) '(vector (unsigned-byte 8)))))
         (size (tuatara.text:rope-bytes rope))
         (raw (string (code-char #xDCA9))))
    (dolist (way (list (list raw nil) (list raw t) (list "\\xA9" t) (list (concatenate 'string " " raw) nil)))
      (destructuring-bind (pattern regex) way
        (let ((matcher (tuatara.defaults::make-pattern-matcher pattern :regex regex)))
          (is = 1 (tuatara.defaults::count-line-matches rope matcher 0 size) "how many ~s~:[~;, a pattern~]" pattern regex)
          (is equal (if (char= (char pattern 0) #\Space) '(2 4) '(3 4))
              (multiple-value-call #'found-span (tuatara.defaults::matcher-search rope matcher 0 size))
              "where ~s~:[~;, a pattern~]" pattern regex))))))

(defun stepped-matches (rope matcher forward)
  "Every match of MATCHER in ROPE as C-s and C-r step through them, (START
END) each in order of the text: from one end, from each match on, one
character on after an empty one going forward."
  (let ((size (tuatara.text:rope-bytes rope)) (out '()) (from (if forward 0 (tuatara.text:rope-bytes rope))))
    (loop
      (multiple-value-bind (s e) (tuatara.defaults::matcher-search rope matcher from (if forward size 0) :forward forward :bound size)
        (unless s (return))
        (push (list s e) out)
        (setf from (cond ((not forward) s)
                         ((< s e) e)
                         ((< e size) (loop for next from (1+ e)
                                           when (= next (tuatara.text:rope-char-start rope next)) return next))
                         (t (return))))))
    (if forward (nreverse out) out)))

(define-test (search-agreement empty-matches-stepped)
  ;; C-s and C-r step through the empty matches as well, just those that are counted and
  ;; replaced, both ways
  (dolist (text (list (format nil "kia ora~%~%tūī  kea~%~%end") (format nil "kea~%~%tui~%")))
    (dolist (crlf '(nil t))
      (let* ((text (if crlf (cl-ppcre:regex-replace-all "\n" text (format nil "~c~c" #\Return #\Newline)) text))
             (rope (tuatara.text:string->rope text)))
        (dolist (pattern '("^$" "^" "$" "\\b" "(?=k)" "a*" "\\s*" "(?<=a)"))
          (let ((matcher (tuatara.defaults::make-pattern-matcher pattern :regex t :crlf crlf))
                (counted '()))
            (tuatara.defaults::stretch-matches (lambda (s e m g) (declare (ignore m g)) (push (list s e) counted) nil)
                                               rope matcher 0 (tuatara.text:rope-bytes rope) t nil nil)
            (setf counted (nreverse counted))
            (true counted "~s matches" pattern)
            (is = (length counted) (tuatara.defaults::count-line-matches rope matcher 0 (tuatara.text:rope-bytes rope))
                "how many ~s" pattern)
            (is equal counted (stepped-matches rope matcher t) "~s stepped forward~:[~;, CRLF~]" pattern crlf)
            (is equal (remove (tuatara.text:rope-bytes rope) counted :key #'first)
                (stepped-matches rope matcher nil) "~s stepped backward~:[~;, CRLF~]" pattern crlf))))))
  ;; just after the last line end is no line: no empty match there, of any pattern
  (let ((rope (tuatara.text:string->rope (format nil "a~%b~%"))))
    (dolist (case '(("$" 2) ("x*" 4) ("^" 2) ("^$" 0) ("\\z" 0) ("\\b" 4)))
      (is = (second case)
          (tuatara.defaults::count-line-matches rope (tuatara.defaults::make-pattern-matcher (first case) :regex t) 0 4)
          "how many ~s" (first case)))))

(define-test (search-agreement first-per-line-after-empty)
  ;; without g one match on each line, also when the first is an empty one at a line's start
  (let* ((text (format nil "foo bar~%foo bar~%bxx~%"))
         (rope (tuatara.text:string->rope text))
         (size (tuatara.text:rope-bytes rope)))
    (dolist (case '(("\\b" ((0 0) (8 8) (16 16))) ("x*" ((0 0) (8 8) (16 16)))))
      (destructuring-bind (pattern wanted) case
        (let ((matcher (tuatara.defaults::make-pattern-matcher pattern :regex t)) (all '()))
          (tuatara.defaults::stretch-matches (lambda (s e m g) (declare (ignore m g)) (push (list s e) all) nil)
                                             rope matcher 0 size t nil nil)
          (is equal wanted (mapcar (lambda (m) (subseq m 0 2)) (tuatara.defaults::first-per-line rope (nreverse all)))
              "first on each line of ~s" pattern)
          (is = (length wanted) (tuatara.defaults::count-first-per-line rope matcher 0 size) "how many of ~s" pattern))))
    ;; r r over the whole text selected
    (is string= (format nil "|foo bar~%|foo bar~%|bxx~%")
        (session-text (nth-value 1 (run-keys (format nil "C-x h r r ~a RET" (key-string "\\b/|")) :text text :width 200))))))

(define-test (search-agreement files-line-ends-agree)
  ;; search-in-files reads each file's lines as they end: $ before a carriage return and a
  ;; newline in a Windows file, as a search of that file in a buffer finds it, the line
  ;; listed without its carriage return; a file of newlines alone beside it as before
  (let* ((lf (format nil "error~%ok error~%"))
         (crlf (format nil "error~c~%fine~c~%x error~c~%" #\Return #\Return #\Return))
         (text (concatenate 'string lf crlf))
         (corpus (tuatara.defaults::make-file-corpus (tuatara.text:string->rope text)
                                                     (vector "notes.txt" "log.txt")
                                                     (vector 0 (length lf)) "/tmp/")))
    (is equal (list (list 0 1 0 "error") (list 0 2 3 "ok error") (list 1 1 0 "error") (list 1 3 2 "x error"))
        (tuatara.defaults::corpus-lines corpus "error$"))
    (is equal (list (list 1 2 0 "fine")) (tuatara.defaults::corpus-lines corpus "^f.*$ ine"))
    (is equal '() (tuatara.defaults::corpus-lines corpus "error."))))

(define-test (search-agreement files-lines-a-match-runs-over)
  ;; search-in-files lists every line any part of the longest word's match lies on, once,
  ;; the other words checked on each; a match never runs from one file into the next; a
  ;; text cut into chunks for the cores lists the same lines
  (dolist (crlf '(nil t))
    (flet ((text (&rest lines) (format nil "~{~a~:[~*~;~c~]~%~}" (loop for line in lines collect line collect crlf collect #\Return))))
      (let* ((one (text "a,b" "foo,c" "zz"))
             (two (text "x,b"))
             (three (text "foo" "head,1" "S2lhIG9yYSBrb3V0b3Uga2F0b2Es" "IHRoZSBrZWEgaXMgYSBwYXJyb3Q=" "tail,2"))
             (corpus (tuatara.defaults::make-file-corpus (tuatara.text:string->rope (concatenate 'string one two three))
                                                         (vector "one.txt" "two.txt" "three.txt")
                                                         (vector 0 (length one) (+ (length one) (length two))) "/tmp/")))
        (dolist (parallel '(nil t))
          (let ((tuatara.defaults::*search-chunk* (if parallel 4 tuatara.defaults::*search-chunk*)))
            (is equal '((0 1 2 "a,b") (0 2 0 "foo,c") (2 1 0 "foo"))
                (tuatara.defaults::corpus-lines corpus "[^,]*foo" :parallel parallel))
            (is equal '((0 1 3 "a,b"))
                (tuatara.defaults::corpus-lines corpus "\\s+foo b" :parallel parallel))
            (is equal '((2 3 0 "S2lhIG9yYSBrb3V0b3Uga2F0b2Es") (2 4 0 "IHRoZSBrZWEgaXMgYSBwYXJyb3Q="))
                (tuatara.defaults::corpus-lines corpus "(?:[A-Za-z0-9+/]{4}\\s*){12,}=*" :parallel parallel))))))))

(define-test (search-agreement disk-pieces-keep-lines-whole)
  ;; a file searched from disk a piece at a time: a line longer than the piece is searched
  ;; whole, a match across the piece's edge found, ^ only at the line's start, listed once
  (uiop:with-temporary-file (:stream s :pathname file :direction :output :type "txt")
    (format s "kia ora~%~aneedle~a~%tail~%" (make-string 60 :initial-element #\x) (make-string 100 :initial-element #\y))
    (finish-output s)
    (let ((path (uiop:native-namestring file))
          (tuatara.defaults::*search-disk-piece* 64))
      (flet ((found (input)
               (mapcar (lambda (candidate) (second (tuatara.command::candidate-value candidate)))
                       (tuatara.defaults::lines-on-disk path "long.txt" "/tmp/" input :parallel nil))))
        (is equal '(2) (found "needle"))
        (is equal '(2) (found "x+needle"))
        (is equal '(2) (found "^x"))
        (is equal '() (found "^y"))
        (is equal '(3) (found "tail"))))))

(defun crossing-texts (crlf)
  "Texts whose matches of *CROSSING-AGREEMENT-PATTERNS* run over line ends,
their lines ending with a carriage return and a newline when CRLF: the
evidence, and short lines where a match ends just on the line a fresh scan
from it would match again."
  (flet ((text (&rest lines) (format nil "~{~a~:[~*~;~c~]~%~}" (loop for line in lines collect line collect crlf collect #\Return))))
    (list (evidence-text 120 :crlf crlf)
          (text "k,x" (make-string 20 :initial-element #\y) "foo,1" (make-string 20 :initial-element #\z) "zz,foo" "end")
          (text (concatenate 'string (make-string 20 :initial-element #\z) "a") "b" (concatenate 'string "c" (make-string 20 :initial-element #\z))
                "a" "b" "c" "tail,2")
          (text "head,1" "S2lhIG9yYSBrb3V0b3Uga2F0b2Es" "IHRoZSBrZWEgaXMgYSBwYXJyb3Q=" "S2lhIG9yYSBrb3V0b3Uga2F0b2Es" "tail,2"))))

(defparameter *crossing-agreement-patterns*
  '("[^,]*foo" "\\s+foo" "\\Wfoo" "[a-c]\\r?\\n[a-c]" "[^,]*login" "\\s+2026" "\\W2026"
    "(?:[A-Za-z0-9+/]{4}\\s*){10,}=*")
  "Patterns whose matches run over line ends, for the ways of finding lines
worked out in pieces.")

(defun kept-by-scan (text pattern crlf keep)
  "TEXT with only the lines one scan of it finds PATTERN on (SCAN-KEPT-STARTS)
left, or with KEEP false those it does not find it on."
  (let* ((rope (tuatara.text:string->rope text))
         (starts (scan-kept-starts rope (tuatara.defaults::search-filter-matcher
                                         (tuatara.defaults::make-search-filter pattern :crlf crlf))
                                   crlf)))
    ;; each line's start counted in octets, as the oracle gives them
    (with-output-to-string (o)
      (loop with octet = 0
            for start = 0 then next
            while (< start (length text))
            for next = (let ((newline (position #\Newline text :start start))) (if newline (1+ newline) (length text)))
            when (eq keep (and (member octet starts) t))
              do (write-string text o :start start :end next)
            do (incf octet (length (sb-ext:string-to-octets text :start start :end next :external-format :utf-8)))))))

(define-test (search-agreement keep-and-flush-lines-agree-with-a-scan)
  ;; keep-lines and flush-lines keep or delete every line one scan finds a match on, a
  ;; match running over line ends too: at once, in the background, and in the background
  ;; in pieces of a few lines, where a piece begins inside a match; a pattern slow on
  ;; this text given the time it takes on a busy machine
  (let ((tuatara.runtime:*slow-call-seconds* 10))
    (dolist (crlf '(nil t))
      (dolist (text (crossing-texts crlf))
        (dolist (pattern *crossing-agreement-patterns*)
          (loop for (on-disk here) in '((nil t) (nil nil) (t nil))
                do (dolist (keep '(t nil))
                     (is string= (kept-by-scan text pattern crlf keep)
                         (let ((tuatara.defaults::*rewrite-piece* (if on-disk 16 tuatara.defaults::*rewrite-piece*))
                               (tuatara.defaults::*sort-in-memory-limit* (if on-disk 0 tuatara.defaults::*sort-in-memory-limit*)))
                           (session-text
                            (nth-value 1 (run-keys (list (lambda ()
                                                           (let ((b (tuatara.buffer:find-buffer "*scratch*")))
                                                             (when here (setf (tuatara.buffer:buffer-local b :plaintext) t))
                                                             (when crlf (setf (tuatara.buffer:buffer-local b :line-ending) :crlf))))
                                                         (format nil "C-x h M-x ~a RET ~a RET"
                                                                 (key-string (if keep "keep-lines" "flush-lines")) (key-string pattern)))
                                                   :text text :width 200))))
                         "~:[flush~;keep~] ~a~:[~;, CRLF~]~:[~;, at once~]~:[~;, in pieces~]" keep pattern crlf here on-disk))))))))

(define-test (search-agreement disk-pieces-agree-with-a-scan)
  ;; a file searched from disk a piece at a time lists the lines one scan of the whole
  ;; file lists, a match running over a piece's end too, LF and CRLF: a text of matches
  ;; running over line ends, larger than a piece and what is read ahead of it together
  (dolist (crlf '(nil t))
    (let* ((block (format nil "~{~a~}" (rest (crossing-texts crlf))))
           (text (with-output-to-string (o)
                   (loop repeat (ceiling (* 1600 1024) (length block)) do (write-string block o))))
           (corpus (tuatara.defaults::make-file-corpus (tuatara.text:string->rope text) (vector "x.txt") (vector 0) "/tmp/")))
      (uiop:with-temporary-file (:stream s :pathname file :direction :output :type "txt" :external-format :utf-8)
        (write-string text s)
        (finish-output s)
        (dolist (pattern *crossing-agreement-patterns*)
          (dolist (piece '(400009 523777))
            (let* ((tuatara.defaults::*search-disk-piece* piece)
                   (scanned (mapcar (lambda (line) (subseq line 1 3))
                                    (tuatara.defaults::corpus-lines corpus pattern :parallel nil :most 100000)))
                   (from-disk (mapcar (lambda (candidate) (rest (tuatara.command::candidate-value candidate)))
                                      (tuatara.defaults::lines-on-disk (uiop:native-namestring file) "x.txt" "/tmp/" pattern
                                                                       :parallel nil :most 100000)))
                   (apart (mismatch scanned from-disk :test #'equal)))
              ;; only where they part is kept in the report, not lists of a hundred thousand lines
              (false apart "~a in pieces of ~d~:[~;, CRLF~]: ~d lines against ~d, apart at ~a: ~s against ~s"
                     pattern piece crlf (length scanned) (length from-disk) apart
                     (and apart (nth apart scanned)) (and apart (nth apart from-disk))))))))))
