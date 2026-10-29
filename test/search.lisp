;;;; test/search.lisp --- Literal and regex search over ropes.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test search
  :description "Literal search across chunk and leaf boundaries, case folding, backward search, parallel all-matches, regex search.")

(defun oracle-positions (haystack needle &key case-fold)
  "All non-overlapping byte offsets of NEEDLE in HAYSTACK (strings), via the octets."
  (let* ((h (to-octets (if case-fold (string-downcase haystack) haystack)))
         (n (to-octets (if case-fold (string-downcase needle) needle)))
         (out '()) (i 0))
    (loop
      (let ((p (search n h :start2 i)))
        (unless p (return))
        (push p out) (setf i (+ p (length n)))))
    (nreverse out)))

(define-test (search literal)
  (let* ((s "abc needle xyz NEEDLE  needleneedle") (rope (tuatara.text:string->rope s)))
    (multiple-value-bind (a b) (tuatara.syntax:search-forward rope "needle")
      (is = 4 a) (is = 10 b))
    (is = 15 (tuatara.syntax:search-forward rope "needle" :start 5))    ; case folded: NEEDLE at 15
    (is = 23 (tuatara.syntax:search-forward rope "needle" :start 5 :case-fold nil))
    (is = 15 (tuatara.syntax:search-forward rope "NEEDLE"))             ; upper case in the needle: exact
    (false (tuatara.syntax:search-forward rope "missing"))
    (false (tuatara.syntax:search-forward rope ""))
    (is = 29 (tuatara.syntax:search-backward rope "needle"))
    (is = 23 (tuatara.syntax:search-backward rope "needle" :start 29))
    (is = 4 (tuatara.syntax:search-backward rope "needle" :start 15 :case-fold nil))
    (is equalp #(4 15 23 29) (tuatara.syntax:search-all rope "needle" :threads 1))
    (is equalp #(4 23 29) (tuatara.syntax:search-all rope "needle" :case-fold nil :threads 2))
    (is equalp #(4 15) (tuatara.syntax:search-all rope "needle" :limit 2))))

(define-test (search across-boundaries)
  ;; a needle straddling leaf and chunk boundaries in a long text, against the oracle
  (let* ((piece "xx needle yy ")
         (s (with-output-to-string (o) (loop repeat 40000 do (write-string piece o) (write-string (random-text 3) o))))
         (rope (tuatara.text:string->rope s))
         (expected (oracle-positions s "needle")))
    (is = (length expected) (length (tuatara.syntax:search-all rope "needle" :threads 4)))
    (is equalp (coerce expected 'simple-vector) (tuatara.syntax:search-all rope "needle" :threads 4))
    (is equalp (coerce expected 'simple-vector) (tuatara.syntax:search-all rope "needle" :threads 1))
    ;; the parallel waves give the same answers: force them with a tiny serial window and piece
    (let ((tuatara.syntax:*serial-window* 5000) (tuatara.syntax:*parallel-piece* 7000))
      (loop repeat 20
            for start = (random (length s))
            do (let ((fwd (find-if (lambda (p) (>= p start)) expected))
                     (bwd (find-if (lambda (p) (< p start)) expected :from-end t)))
                 (is eql fwd (tuatara.syntax:search-forward rope "needle" :start start :threads 3))
                 (is eql bwd (tuatara.syntax:search-backward rope "needle" :start start :threads 3))))
      (false (tuatara.syntax:search-forward rope "zzznotthere" :threads 3))
      (false (tuatara.syntax:search-backward rope "zzznotthere" :threads 3)))
    ;; forward and backward agree with the oracle from random starts
    (loop repeat 20
          for start = (random (length s))
          do (let ((fwd (find-if (lambda (p) (>= p start)) expected))
                   (bwd (find-if (lambda (p) (< p start)) expected :from-end t)))
               (is eql fwd (tuatara.syntax:search-forward rope "needle" :start start))
               (is eql bwd (tuatara.syntax:search-backward rope "needle" :start start))))
    ;; multibyte needle
    (let* ((s2 "aé中😀 é中 zz é中😀") (r2 (tuatara.text:string->rope s2)))
      (is equalp (coerce (oracle-positions s2 "é中") 'simple-vector) (tuatara.syntax:search-all r2 "é中" :threads 1))
      (is = 11 (tuatara.syntax:search-backward r2 "é中" :start 14))
      (is = 20 (tuatara.syntax:search-backward r2 "é中")))))

(define-test (search regex)
  (let* ((s (format nil "foo123 bar45~%baz6789 qux")) (rope (tuatara.text:string->rope s)))
    (multiple-value-bind (a b m) (tuatara.syntax:regex-search-forward rope "[a-z]+(\\d+)")
      (is = 0 a) (is = 6 b) (is string= "foo123" m))
    (multiple-value-bind (a b) (tuatara.syntax:regex-search-forward rope "\\d+" :start 3)
      (is = 3 a) (is = 6 b))
    (is equal '((3 6 "123" nil) (10 12 "45" nil) (16 20 "6789" nil)) (tuatara.syntax:regex-search-all rope "\\d+"))
    (is equal '((0 6 "foo123" nil)) (tuatara.syntax:regex-search-all rope "^foo\\d+"))
    (is equal '((13 20 "baz6789" nil)) (tuatara.syntax:regex-search-all rope "^ba.\\d+"))
    (is equal '((21 24 "qux" nil)) (tuatara.syntax:regex-search-all rope "qux$"))
    ;; groups come with each match
    (is equalp '((0 6 "foo123" #("foo" "123"))) (tuatara.syntax:regex-search-all rope "([a-z]+)(\\d+)" :limit 1))
    (false (tuatara.syntax:regex-search-forward rope "nothing"))
    ;; case folding follows the pattern's case
    (is = 2 (length (tuatara.syntax:regex-search-all (tuatara.text:string->rope "Foo foo") "foo")))
    (is = 1 (length (tuatara.syntax:regex-search-all (tuatara.text:string->rope "Foo foo") "Foo"))))
  ;; matches across chunk boundaries in a long text
  (let* ((s (with-output-to-string (o) (loop repeat 20000 do (write-string "ab12 " o))))
         (rope (tuatara.text:string->rope s)))
    (is = 20000 (length (tuatara.syntax:regex-search-all rope "b\\d\\d")))))

(defun regex-oracle (string regex)
  "All non-empty, non-overlapping matches of REGEX in STRING as (START END TEXT)
byte triples, scanning the whole string at once (the reference for the
windowed engine); REGEX a pattern, or a scanner made already (as
TUATARA.SYNTAX:COMPILE-REGEX makes one)."
  (let ((offsets (make-array (1+ (length string)) :element-type 'fixnum)) (b 0)
        (scanner (if (functionp regex)
                     regex
                     (cl-ppcre:create-scanner regex :multi-line-mode t
                                                    :case-insensitive-mode (tuatara.syntax:regex-case-fold-p regex))))
        (out '()) (from 0))
    (loop for i from 0 below (length string)
          do (setf (aref offsets i) b)
             (incf b (let ((c (char-code (char string i)))) (cond ((< c #x80) 1) ((< c #x800) 2) ((< c #x10000) 3) (t 4)))))
    (setf (aref offsets (length string)) b)
    (loop
      (when (> from (length string)) (return))
      (multiple-value-bind (ms me) (cl-ppcre:scan scanner string :start from :real-start-pos 0)
        (unless ms (return))
        (if (= ms me)
            (setf from (1+ ms))
            (progn (push (list (aref offsets ms) (aref offsets me) (subseq string ms me)) out)
                   (setf from me)))))
    (nreverse out)))

(defun regex-text (n)
  "N random characters for regex tests: letters, digits, commas, spaces,
newlines and some multibyte characters."
  (with-output-to-string (o)
    (loop repeat n
          do (write-char (let ((r (random 40)))
                           (cond ((< r 26) (code-char (+ 97 r))) ((< r 30) #\Space) ((< r 32) #\Newline)
                                 ((< r 34) #\é) ((< r 35) #\中) ((< r 36) #\😀)
                                 ((< r 38) (code-char (+ 48 (random 10)))) (t #\,)))
                         o))))

(defun triples (matches) "MATCHES without their groups." (mapcar (lambda (m) (subseq m 0 3)) matches))

(define-test (search regex-windows)
  ;; ^, \b and lookbehind see the character before a window: a line longer than one
  (let* ((s (concatenate 'string (make-string 65536 :initial-element #\a) "xyz xyz" (string #\Newline) "xyz"))
         (rope (tuatara.text:string->rope s)))
    (dolist (p '("\\bxyz" "^xyz" "(?<=a)xyz"))
      (is equal (regex-oracle s p) (triples (tuatara.syntax:regex-search-all rope p)) "pattern ~s" p))
    (false (tuatara.syntax:regex-search-forward rope "^a" :start 5))
    (is = 65544 (tuatara.syntax:regex-search-forward rope "^x" :start 5)))
  ;; the windowed engine agrees with a whole-string scan on random multibyte text
  (loop repeat 2
        do (let* ((s (regex-text 100000)) (rope (tuatara.text:string->rope s)))
             (dolist (p '("\\d+" "[a-z]+,\\d" "^[^,]*" "é中" "z$" "😀." "(ab)+c" "(?<=q)z+" "Qz" "q[a-z]*,x"))
               (is equal (regex-oracle s p) (triples (tuatara.syntax:regex-search-all rope p)) "pattern ~s" p))))
  ;; a match longer than a window's overlap is found whole
  (let* ((s (concatenate 'string (make-string 70000 :initial-element #\b) "x" (make-string 10000 :initial-element #\a) "y"))
         (rope (tuatara.text:string->rope s)))
    (is equal '((70000 80002)) (mapcar (lambda (m) (subseq m 0 2)) (tuatara.syntax:regex-search-all rope "xa+y")))
    (is equal '((0 70000)) (mapcar (lambda (m) (subseq m 0 2)) (tuatara.syntax:regex-search-all rope "b+"))))
  ;; octets that are not UTF-8 decode one by one as raw bytes' characters, as the display shows them
  (let ((rope (tuatara.text:octets->rope
               (coerce (concatenate '(vector (unsigned-byte 8)) (to-octets "ab") #(#x80) (to-octets "cd") #(#xE4 #xB8) (to-octets "ef é"))
                       'tuatara.text:octets))))
    (is equal '((0 2) (3 5) (7 9)) (mapcar (lambda (m) (subseq m 0 2)) (tuatara.syntax:regex-search-all rope "[a-z]+")))
    (is equal '((2 3)) (mapcar (lambda (m) (subseq m 0 2)) (tuatara.syntax:regex-search-all rope (string (code-char #xDC80)))))
    (is equal '((5 6)) (mapcar (lambda (m) (subseq m 0 2)) (tuatara.syntax:regex-search-all rope (string (code-char #xDCE4)))))
    (is equal '((2 3) (5 6) (6 7)) (mapcar (lambda (m) (subseq m 0 2)) (tuatara.syntax:regex-search-all rope (format nil "[~c-~c]" (code-char #xDC80) (code-char #xDCFF)))))
    (is equal '((10 12)) (mapcar (lambda (m) (subseq m 0 2)) (tuatara.syntax:regex-search-all rope "é"))))
  ;; BOUND: the pattern sees nothing past it
  (let ((rope (tuatara.text:string->rope "aaa bbb")))
    (is equal '((0 2 "aa" nil)) (tuatara.syntax:regex-search-all rope "a+" :bound 2))
    (is equal '((0 2 "aa" nil)) (tuatara.syntax:regex-search-all rope "a+$" :bound 2))))

(define-test (search regex-directions)
  ;; forward and backward from random starts, serial and in parallel waves, against the oracle
  (let* ((s (regex-text 150000)) (rope (tuatara.text:string->rope s)))
    (dolist (p '("\\d\\d\\d" "é[a-z]" "^z" "q,"))
      (let ((all (regex-oracle s p)))
        (dolist (sizes '((4000000 8000000) (5000 7000)))
          (let ((tuatara.syntax:*serial-window* (first sizes)) (tuatara.syntax:*parallel-piece* (second sizes)))
            (loop repeat 8
                  for start = (tuatara.text:rope-char-start rope (random (tuatara.text:rope-bytes rope)))
                  do (let ((fwd (find-if (lambda (m) (>= (first m) start)) all))
                           (bwd (find-if (lambda (m) (< (first m) start)) all :from-end t)))
                       (is eql (first fwd) (tuatara.syntax:regex-search-forward rope p :start start :threads 3)
                           "forward ~s from ~a" p start)
                       (is eql (first bwd) (tuatara.syntax:regex-search-backward rope p :start start :threads 3)
                           "backward ~s from ~a" p start))))))))
  ;; STOP gives up between waves
  (let* ((rope (tuatara.text:string->rope (make-string 50000 :initial-element #\a)))
         (tuatara.syntax:*serial-window* 1000) (tuatara.syntax:*parallel-piece* 1000))
    (is equal '(nil :stopped) (multiple-value-list (tuatara.syntax:regex-search-forward rope "b+" :stop (constantly t))))
    (is equal '(nil :stopped) (multiple-value-list (tuatara.syntax:regex-search-backward rope "b+" :stop (constantly t))))
    (is equal '(nil :stopped) (multiple-value-list (tuatara.syntax:search-forward rope "b" :stop (constantly t))))
    (is equal '(nil :stopped) (multiple-value-list (tuatara.syntax:search-backward rope "b" :stop (constantly t))))
    (false (tuatara.syntax:regex-search-forward rope "b+" :stop (constantly nil)))))

(define-test (search regex-prefilter)
  ;; the text every match holds, which scans skip ahead to at literal speed
  (is equal '("1999-13" t) (multiple-value-list (tuatara.syntax::required-literal "^1999-13")))
  (is equal '("foobar" t) (multiple-value-list (tuatara.syntax::required-literal "(foo)bar")))
  (is equal '("zzz" nil) (multiple-value-list (tuatara.syntax::required-literal "\\d+zzz")))
  (is equal '("abc" t) (multiple-value-list (tuatara.syntax::required-literal "a(?:bc)+d")))
  (is equal "word" (tuatara.syntax::required-literal "\\bword\\b"))
  (false (tuatara.syntax::required-literal "x|y"))
  (false (tuatara.syntax::required-literal "(?i)abc"))
  (false (tuatara.syntax::required-literal "[xyz]{9}"))
  ;; which patterns keep every match (and what they look at) within a line
  (flet ((crosses (pattern) (tuatara.syntax::crosses-lines-p (cl-ppcre:parse-string pattern))))
    (false (crosses "a+"))
    (false (crosses "foo.*bar"))
    (false (crosses "[a-z]+\\d"))
    (false (crosses "(?<=\\n)x"))
    (true (crosses "[^,]*"))
    (true (crosses "\\s+"))
    (true (crosses "\\D"))
    (true (crosses "a\\nb"))
    (true (crosses "x(?=\\n)")))
  ;; \z matches only at the end of the text, never at the end of a window inside it
  (let ((rope (tuatara.text:string->rope (concatenate 'string (make-string 200000 :initial-element #\b) "c"))))
    (false (tuatara.syntax:regex-search-forward rope "b\\z"))
    (is = 200000 (tuatara.syntax:regex-search-forward rope "c\\z")))
  ;; a match within a line is found whole however long the line, up to a mebibyte
  (let ((rope (tuatara.text:string->rope (concatenate 'string "x" (make-string 100000 :initial-element #\a) "y"))))
    (is equal '((0 100002)) (mapcar (lambda (m) (subseq m 0 2)) (tuatara.syntax:regex-search-all rope "xa+y"))))
  ;; smart case: escapes such as \W are not upper-case letters
  (true (tuatara.syntax:regex-case-fold-p "\\W\\d foo"))
  (false (tuatara.syntax:regex-case-fold-p "Foo"))
  (true (tuatara.syntax:literal-pattern-p "foo bar,1"))
  (false (tuatara.syntax:literal-pattern-p "foo.")))

(define-test (search regex-prefilter-shapes)
  ;; where no text is held by every match, a shape is: a run of sets of octets, each
  ;; place one of a few, as [0-9A-Fa-f]{8}- in every UUID
  (flet ((shape (pattern &optional case-fold)
           (multiple-value-bind (shape prefix-p)
               (tuatara.syntax::required-shape
                (tuatara.syntax::with-pattern-syntax (cl-ppcre:parse-string (tuatara.syntax::engine-pattern pattern)))
                case-fold)
             (list (and shape (length shape)) prefix-p)))
         (scanner-shape (pattern)
           (let ((traits (gethash (tuatara.syntax:compile-regex pattern) tuatara.syntax::**regex-traits**)))
             (and (tuatara.syntax::regex-traits-shape traits) t))))
    (is equal '(10 t) (shape "[0-9]{4}-[0-9]{2}-[0-9]{2}"))
    (is equal '(36 t) (shape "[[:uuid:]]"))
    (is equal '(32 t) (shape "[[:md5:]]"))
    ;; a lookahead at the start says what the text at a match starts with
    (is equal '(8 t) (shape "[[:date<2026-07-01:]]"))
    ;; either way: the places they have in common, every octet either allows
    (is equal '(3 t) (shape "(?:[0-9]{3}|[a-f]{4})x"))
    (is equal '(4 nil) (shape "\\w+ [0-9]{3}"))
    ;; nothing for what may be more than one octet, or a newline
    (is equal '(nil nil) (shape "\\d{4}"))
    (is equal '(nil nil) (shape ".{4}"))
    (is equal '(nil nil) (shape "[^,]{4}"))
    (is equal '(nil nil) (shape "é{4}" t))
    (is equal '(1 t) (shape "a\\nb"))
    ;; a scanner looks for a shape only when it tells more than the text every match holds
    (true (scanner-shape "[[:uuid:]]"))
    (true (scanner-shape "[[:macaddress:]]"))
    (false (scanner-shape "user=[0-9]{2}"))
    (false (scanner-shape "[0-9]:"))
    (false (scanner-shape "[[:ipv6:]]")))
  ;; a search for a shape over text that has none goes at the speed of a search for text:
  ;; it never tries the pattern there
  (let* ((line "The kea is a large parrot of the alpine South Island, a cafe of bad beef and dead feed, 2026 at 10:42.")
         (rope (tuatara.text:string->rope (with-output-to-string (o) (dotimes (i 40000) (write-line line o))))))
    (flet ((fastest (pattern)
             (loop repeat 3
                   minimize (let ((start (get-internal-real-time)))
                              (tuatara.syntax:regex-search-all rope pattern)
                              (- (get-internal-real-time) start)))))
      (let ((text (fastest "zqzq")))
        (dolist (pattern '("[[:md5:]]" "[[:uuid:]]" "[[:macaddress:]]" "[0-9]{4}-[0-9]{2}-[0-9]{2}" "[[:date:]]"
                           "[[:creditcard:]]" "[[:iban:]]" "[[:bitcoin:]]"))
          (let ((time (fastest pattern)))
            (true (<= time (max (* 4 text) (* 30 (/ internal-time-units-per-second 1000))))
                  "~a over 4 MB took ~,1f ms, text ~,1f ms" pattern
                  (/ (* 1000 time) internal-time-units-per-second) (/ (* 1000 text) internal-time-units-per-second))))))))

(define-test (search regex-compiled-once)
  ;; a pattern compiled again the same way is the scanner made before: nothing is built anew
  (let ((scanner (tuatara.syntax:compile-regex "kea error")))
    (is eq scanner (tuatara.syntax:compile-regex (copy-seq "kea error")))
    (isnt eq scanner (tuatara.syntax:compile-regex "kea error" :case-fold nil))
    (isnt eq scanner (tuatara.syntax:compile-regex "kea error" :crlf t))
    (let ((consed (loop repeat 3
                        minimize (let ((before (tuatara.bench::bytes-consed)))
                                   (tuatara.syntax:compile-regex "kea error")
                                   (- (tuatara.bench::bytes-consed) before)))))
      (true (< consed 4096) "compiling it again consed ~:d octets" consed))))

(define-test (search crlf-plain-text-kept-plain)
  ;; a pattern of plain text alone, its special characters escaped, means the same in CRLF text
  (let ((text (format nil "~c[1m 1.5 1x5 (a+b) aab~c~%" (code-char 27) #\Return)))
    (dolist (pattern '("\\e\\[" "1\\.5" "\\(a\\+b\\)" "\\Q(a+b)\\E"))
      (is equal (cl-ppcre:all-matches-as-strings (tuatara.syntax:compile-regex pattern) text)
          (cl-ppcre:all-matches-as-strings (tuatara.syntax:compile-regex pattern :crlf t) text)
          "~s, CRLF" pattern))))

(define-test (search regex-tries-lines-holding-text)
  ;; a pattern whose matches lie within a line and hold text not at their start is
  ;; tried on the lines holding that text alone, and finds what a whole-text scan finds
  (let ((s (with-output-to-string (o)
             (dotimes (i 3000)
               (format o "~a~%" (case (mod i 5)
                                  (0 "12 kea Error at the door, kea error again")
                                  (1 "é中 kea error")
                                  (2 "kea erro r and errorless")
                                  (3 (format nil "~a~c" "77 kea error" #\Return))
                                  (t "nothing here"))))))
        (patterns '("\\d+ kea error" "[a-z]+ again$" "\\w+ error\\b" "o[a-z]* the door" "\\S+ kea error$")))
    (let ((rope (tuatara.text:string->rope s)))
      (dolist (p patterns)
        (is equal (regex-oracle s p) (triples (tuatara.syntax:regex-search-all rope p)) "~a" p))))
  ;; and goes at near the speed of a pattern that begins with its text
  (let* ((lines '("2026-10-23 12:00:01 kea INFO all good here on the mountain, nothing to report at all"
                  "2026-10-23 12:00:03 kea Error the door would not open, tried again with the key"))
         (rope (tuatara.text:string->rope
                (with-output-to-string (o) (dotimes (i 40000) (write-line (nth (mod i 2) lines) o))))))
    (flet ((fastest (pattern)
             (let ((scanner (tuatara.syntax:compile-regex pattern)))
               (loop repeat 3
                     minimize (let ((start (get-internal-real-time)))
                                (tuatara.syntax:map-regex-matches (lambda (s e m g) (declare (ignore s e m g)) nil)
                                                                  rope scanner 0 (tuatara.text:rope-bytes rope)
                                                                  :texts nil)
                                (- (get-internal-real-time) start))))))
      (let ((begun (fastest "kea error the")))
        (dolist (pattern '("\\d+ kea error" "[Ee]rror the"))
          (let ((time (fastest pattern)))
            (true (<= time (max (* 3 begun) (* 10 (/ internal-time-units-per-second 1000))))
                  "~a took ~,1f ms, kea error the ~,1f ms" pattern
                  (/ (* 1000 time) internal-time-units-per-second) (/ (* 1000 begun) internal-time-units-per-second))))))))

(define-test (search searches-read-into-kept-blocks)
  ;; a search, and a walk over a line's columns, read the text into blocks kept for that,
  ;; not into new ones each time: a key of an incremental search makes next to nothing
  (let ((rope (tuatara.text:string->rope (format nil "~{~a~%~}" (loop for i below 3000 collect (format nil "line ~d of the kea" i))))))
    (flet ((consed (function)
             (funcall function)
             (loop repeat 3
                   minimize (let ((before (tuatara.bench::bytes-consed)))
                              (funcall function)
                              (- (tuatara.bench::bytes-consed) before)))))
      (dolist (way (list (lambda () (tuatara.syntax:search-forward rope "zzzq"))
                         (lambda () (tuatara.syntax:search-backward rope "zzzq"))
                         (lambda () (tuatara.text:map-line-slices (lambda (&rest line) (declare (ignore line))) rope 0 1 0 10))))
        (let ((consed (consed way)))
          (true (< consed 1024) "consed ~:d octets" consed)))
      (let ((consed (consed (lambda () (tuatara.syntax:search-all rope "zzzq")))))
        (true (< consed (* 16 1024)) "search-all consed ~:d octets" consed)))))

(define-test (search collect)
  ;; gathered in parallel pieces, the matches are exactly those of one serial scan,
  ;; also where a match runs across a piece boundary
  (let ((tuatara.syntax:*parallel-piece* 1000))
    (let* ((s (regex-text 60000)) (rope (tuatara.text:string->rope s)))
      (dolist (p '("[a-z]+" "\\d+" "q[a-z]*,x" "^[^,]*" "é[a-z]"))
        (let ((scanner (tuatara.syntax:compile-regex p)))
          (flet ((mapper (function from to halt)
                   (tuatara.syntax:map-regex-matches function rope scanner from to :halt halt)))
            (is equal (regex-oracle s p)
                (triples (tuatara.syntax:collect-matches #'mapper 0 (tuatara.text:rope-bytes rope) :threads 4))
                "pattern ~s" p)
            ;; only counted, nothing kept: the same number
            (is = (length (regex-oracle s p))
                (nth-value 2 (tuatara.syntax:collect-matches #'mapper 0 (tuatara.text:rope-bytes rope) :threads 4 :keep nil))
                "count of ~s" p)))))
    ;; a needle that overlaps itself, in runs longer than a piece
    (let* ((s (with-output-to-string (o) (loop repeat 300 do (write-string (make-string (+ 1 (random 2500)) :initial-element #\a) o)
                                                               (write-char #\b o))))
           (rope (tuatara.text:string->rope s))
           (serial '()))
      (tuatara.syntax:map-literal-matches (lambda (a b m g) (declare (ignore m g)) (push (list a b) serial) nil)
                                          rope "aa" 0 (tuatara.text:rope-bytes rope))
      (flet ((mapper (function from to halt)
               (tuatara.syntax:map-literal-matches function rope "aa" from to :halt halt)))
        (setf serial (nreverse serial))
        (is equal serial
            (mapcar (lambda (m) (subseq m 0 2))
                    (tuatara.syntax:collect-matches #'mapper 0 (tuatara.text:rope-bytes rope) :threads 4)))
        (is = (length serial)
            (nth-value 2 (tuatara.syntax:collect-matches #'mapper 0 (tuatara.text:rope-bytes rope) :threads 4 :keep nil))))
      ;; a match longer than a piece, counted without keeping it
      (let ((scanner (tuatara.syntax:compile-regex "a+")))
        (is = (length (regex-oracle s "a+"))
            (nth-value 2 (tuatara.syntax:collect-matches
                          (lambda (function from to halt)
                            (tuatara.syntax:map-regex-matches function rope scanner from to :halt halt))
                          0 (tuatara.text:rope-bytes rope) :threads 4 :keep nil))))))
  ;; a literal match must end by BOUND; TEXTS gives the text matched
  (let ((rope (tuatara.text:string->rope "abcabcabc")) (found '()))
    (tuatara.syntax:map-literal-matches (lambda (a b m g) (declare (ignore g)) (push (list a b m) found) nil)
                                        rope "ABC" 0 9 :bound 8 :case-fold t :texts t)
    (is equal '((0 3 "abc") (3 6 "abc")) (nreverse found)))
  ;; too costly, or a key waiting: nothing collected, and why
  (let ((rope (tuatara.text:string->rope (make-string 20000 :initial-element #\x))))
    (flet ((mapper (function from to halt)
             (tuatara.syntax:map-literal-matches function rope "x" from to :halt halt)))
      (is eq :over (nth-value 1 (tuatara.syntax:collect-matches #'mapper 0 20000 :cost-limit 10000)))
      (is eq :stopped (nth-value 1 (tuatara.syntax:collect-matches #'mapper 0 20000 :stop (constantly t))))
      (is = 20000 (nth-value 2 (tuatara.syntax:collect-matches #'mapper 0 20000))))))

(define-test (search bracket-end-values)
  (flet ((ends (string) (multiple-value-list (tuatara.syntax:bracket-end string 0))))
    (is equal '(5 t 1) (ends "[abc]x"))
    (is equal '(6 t 2) (ends "[^abc]"))
    (is equal '(4 t 2) (ends "[]a]"))
    (is equal '(5 t 3) (ends "[^]a]"))
    (is equal '(11 t 1) (ends "[[:alpha:]]"))
    (is equal '(4 t 1) (ends "[\\]]]"))
    (is equal '(4 nil 1) (ends "[abc"))
    (is equal '(2 nil 2) (ends "[]"))))

(define-test (search smart-case-skips-codes-and-names)
  ;; a character's code and a group's name are no capitals: the search still ignores case
  (dolist (pattern '("foo\\x{1F600}" "\\x4A" "\\x4Ab" "(?<Year>\\d{4})-\\k<Year>" "kea\\x{1F}" "(?<=a)b" "(?<!a)b"))
    (true (tuatara.syntax:regex-case-fold-p pattern) "~s ignores case" pattern))
  (dolist (pattern '("Foo\\x{1f600}" "\\x4AB" "(?<Year>\\d)Y" "(?<=A)b" "(?<!A)b" "\\k<y>K"))
    (false (tuatara.syntax:regex-case-fold-p pattern) "~s keeps case" pattern))
  (let ((rope (tuatara.text:string->rope (format nil "FOO~a" (code-char #x1F600)))))
    (is eql 0 (tuatara.syntax:regex-search-forward rope "foo\\x{1F600}"))))

(define-test (search sign-fades-on-one-thread)
  ;; a sign lit again and again, as each key of a search lights it, is faded by one thread
  (flet ((faders () (count "tuatara sign fading" (sb-thread:list-all-threads) :key #'sb-thread:thread-name :test #'equal)))
    (let ((editor (tuatara.command:make-editor (tuatara.frontend.fake:make-fake-frontend)))
          (tuatara.defaults::*sign-lit-seconds* 0.3))
      (dotimes (i 20) (tuatara.defaults::note-sign-work :searching editor))
      (true (<= (faders) 1) "~d threads" (faders))
      (loop repeat 200 while (plusp (faders)) do (sleep 0.01))
      (is = 0 (faders))
      (false (car tuatara.defaults::**sign-fader**)))))

(define-test (search idle-sign-colour-once)
  ;; the faint sign's colour is worked out once for the faces in force, not on every frame
  (let ((mixed 0))
    (sb-int:encapsulate 'tuatara.display:face-attribute 'count-mixes
                        (lambda (real &rest arguments) (incf mixed) (apply real arguments)))
    (unwind-protect
         (progn
           (incf tuatara.display:*face-generation*)
           (dotimes (i 50) (tuatara.defaults::mode-line-sign "S" nil))
           (true (<= mixed 10) "the faces looked up ~d times" mixed))
      (sb-int:unencapsulate 'tuatara.display:face-attribute 'count-mixes))))

(define-test (search chunk-scans-agree-with-every-place-tried)
  ;; the first and last places a needle stands in a chunk, found from where its first octet stands
  ;; (both cases of a letter side by side when case is ignored), are those trying every place finds
  (let ((*random-state* (sb-ext:seed-random-state 26)))
    (flet ((naive (chunk needle fold from below filled backward)
             (let ((n (length needle)))
               (flet ((at-p (i) (loop for k below n
                                      always (= (let ((b (aref chunk (+ i k)))) (if (and fold (<= 65 b 90)) (+ b 32) b))
                                                (aref needle k)))))
                 (if backward
                     (loop for i from (min from (- filled n)) downto 0 when (at-p i) return i)
                     (loop for i from from below (min below (- (1+ filled) n)) when (at-p i) return i))))))
      (dotimes (round 400)
        (let* ((size (+ 1 (random 300)))
               (chunk (let ((c (make-array size :element-type '(unsigned-byte 8))))
                        (dotimes (i size c) (setf (aref c i) (aref (sb-ext:string-to-octets "aAbB-zZ") (random 7))))))
               (fold (zerop (random 2)))
               (needle (tuatara.syntax::needle-octets (coerce (loop repeat (+ 1 (random 4)) collect (char "abBz-" (random 5))) 'string) fold))
               (from (random size))
               (below (+ from (random (- (1+ size) from)))))
          (is eql (naive chunk needle fold from below size nil)
              (tuatara.syntax::scan-chunk chunk needle fold from below size))
          (is eql (naive chunk needle fold from 0 size t)
              (tuatara.syntax::scan-chunk-backward chunk needle fold from size))
          ;; every match in turn, the other case's place kept from one call to the next
          (let ((other-case (cons nil nil)) (n (length needle)))
            (is equal (loop with i = 0 for at = (naive chunk needle fold i size size nil)
                            while at collect at do (setf i (+ at n)))
                (loop with i = 0 for at = (tuatara.syntax::scan-chunk chunk needle fold i size size other-case)
                      while at collect at do (setf i (+ at n))))))))))
