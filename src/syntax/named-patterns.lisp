;;;; src/syntax/named-patterns.lisp --- The named patterns of the search patterns: addresses, card numbers, hashes,
;;;; encoded blocks, dates.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; [[:ipv4:]], [[:creditcard:]], [[:base64:]] and the others, each what a
;;;; standard says the thing is: a pattern written out here, or, for what a
;;;; pattern cannot check (a card number's Luhn digit, an address's checksum, a
;;;; block over lines, a date before or after another), a filter the search
;;;; runs as it matches, reading the text on past its window when it must.
;;;; The search (search.lisp) reads them.

(in-package #:tuatara.syntax)

;;; Named patterns: [[:ipv4:]], [[:creditcard:]] and the others, each what a
;;; standard says the thing is, written out by the functions below.

(defparameter *filter-mark* "tuatarazfilter"
  "How a named pattern marks, in the text cl-ppcre is given, where code reads
the text instead of a pattern: an empty named group, (?<tuatarazfiltercard>),
made the filter its name gives (*PATTERN-FILTERS*) once the pattern is read.")

(defun written-out (pattern least &optional (most least))
  "PATTERN LEAST to MOST times, as (?:PATTERN){LEAST,MOST} matches it,
written out in full: three times, (?:P)(?:P)(?:P); one to three,
\(?:P)(?:(?:P)(?:(?:P))?)?. A pattern written so is matched without a count
kept for the repetition."
  (let ((group (format nil "(?:~a)" pattern)))
    (with-output-to-string (out)
      (dotimes (i least) (write-string group out))
      (dotimes (i (- most least)) (format out "(?:~a" group))
      (dotimes (i (- most least)) (write-string ")?" out)))))

(defun dec-octet ()
  "RFC 3986's dec-octet, 0 to 255 with no leading zero, as a pattern."
  "(?:25[0-5]|2[0-4][0-9]|1[0-9]{2}|[1-9][0-9]|[0-9])")

(defun ipv4-core ()
  "RFC 3986's IPv4address, four dec-octets apart by dots, with no edges."
  (format nil "~a~a" (written-out (format nil "~a\\." (dec-octet)) 3) (dec-octet)))

(defun ipv4-pattern ()
  "An IPv4 address standing alone: no digit or dot just before it, and no
digit, or dot and digit, just after it, so that 1.2.3.4.5 and 1.2.3.456
hold none."
  ;; first, cheaply, a dot after the first number
  (format nil "(?<![0-9.])(?=[0-9]{1,3}\\.)~a(?![0-9]|\\.[0-9])" (ipv4-core)))

(defun ipv6-pattern ()
  "An IPv6 address as RFC 3986's IPv6address has it (RFC 4291's text forms):
eight groups of one to four hex digits, a run of them left out as ::, the
last two may be an IPv4 address. It is not inside a word (no letter, digit,
underscore or colon just before it, nor a letter, digit or underscore just
after, so std::cout holds none). It is not followed by a colon and more of an
address, or a dot and a digit. A colon that ends a sentence may follow."
  (let* ((h16 "[0-9A-Fa-f]{1,4}")
         (ls32 (format nil "(?:~a|~a:~a)" (ipv4-core) h16 h16))
         (forms (flet ((groups (least &optional (most least))
                         (written-out (format nil "~a:" h16) least most)))
                  (list (format nil "~a~a" (groups 6) ls32)
                        (format nil "::~a~a" (groups 5) ls32)
                        (format nil "(?:~a)?::~a~a" h16 (groups 4) ls32)
                        (format nil "(?:~a~a)?::~a~a" (groups 0 1) h16 (groups 3) ls32)
                        (format nil "(?:~a~a)?::~a~a" (groups 0 2) h16 (groups 2) ls32)
                        (format nil "(?:~a~a)?::~a:~a" (groups 0 3) h16 h16 ls32)
                        (format nil "(?:~a~a)?::~a" (groups 0 4) h16 ls32)
                        (format nil "(?:~a~a)?::~a" (groups 0 5) h16 h16)
                        (format nil "(?:~a~a)?::" (groups 0 6) h16)))))
    ;; first, cheaply, a colon within the first five characters, as every form has
    (format nil "(?<![0-9A-Za-z_:])(?=[0-9A-Fa-f]{0,4}:)(?:~{~a~^|~})(?![0-9A-Za-z_]|:[0-9A-Fa-f:]|\\.[0-9])" forms)))

(defun mac-pattern ()
  "An IEEE 802 MAC address (EUI-48): six pairs of hex digits apart by hyphens,
as IEEE writes them, or by colons; or three groups of four apart by dots."
  (let ((h2 "[0-9A-Fa-f]{2}") (h4 "[0-9A-Fa-f]{4}"))
    (format nil "(?<![0-9A-Fa-f:.-])(?:~a(?:-~a){5}|~a(?::~a){5}|~a\\.~a\\.~a)(?![0-9A-Fa-f]|[:.-][0-9A-Fa-f])"
            h2 h2 h2 h2 h4 h4 h4)))

(defun email-pattern ()
  "An e-mail address as the HTML standard's valid e-mail address has it
\(WHATWG): the name of letters, digits and .!#$%&'*+/=?^_`{|}~-, an @, and
labels of letters, digits and hyphens apart by dots, none starting or ending
with a hyphen nor longer than 63."
  (let ((name "[A-Za-z0-9.!#$%&'*+/=?^_`{|}~-]")
        (label "[A-Za-z0-9](?:[A-Za-z0-9-]{0,61}[A-Za-z0-9])?"))
    (format nil "(?<!~a)~a+@~a(?:\\.~a)*(?![A-Za-z0-9-]|\\.[A-Za-z0-9])" name name label label)))

(defun url-pattern (&key file)
  "A web or FTP address, http, https, ftp or ftps (in any case), or with FILE
a file:// address too, :// and the characters RFC 3986 lets a uniform
resource identifier (URI) hold. Its end is
not a dot, comma, semicolon, colon, mark, quote or bracket, which close a
sentence rather than the address (RFC 3986, appendix C). The exception is a
closing bracket whose opening one it holds:
https://en.wikipedia.org/wiki/Kea_(bird) whole, (see https://x.nz/a) without the
last bracket."
  (let* ((inside "[A-Za-z0-9._~:/?#\\[\\]@!$&'*+,;=%-]")
         (last "[A-Za-z0-9_~/#@$&*+=%-]")
         (brackets (format nil "\\(~a*\\)" inside)))
    (format nil "(?<![A-Za-z0-9+.-])(?:[Hh][Tt][Tt][Pp][Ss]?|[Ff][Tt][Pp][Ss]?~:[~;|[Ff][Ii][Ll][Ee]~])://~
                 (?:~a|~a)*(?:~a|~a)"
            file inside brackets last brackets)))

(defun uuid-pattern ()
  "A universally unique identifier (UUID) as RFC 9562 writes one: 8-4-4-4-12
hex digits, its version (1 to 8) and variant (8, 9, a or b) where they
stand; or the nil or max UUID."
  (let ((h "[0-9A-Fa-f]"))
    (format nil "(?<![0-9A-Fa-f])(?:~a{8}-~a{4}-[1-8]~a{3}-[89ABab]~a{3}-~a{12}~
                 |0{8}-0{4}-0{4}-0{4}-0{12}|[Ff]{8}-[Ff]{4}-[Ff]{4}-[Ff]{4}-[Ff]{12})(?![0-9A-Fa-f])"
            h h h h h)))

(defun guid-pattern ()
  "A GUID as Windows writes one: any 8-4-4-4-12 hex digits, in braces or not."
  (let ((core "[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{12}"))
    (format nil "(?:\\{~a\\}|(?<![0-9A-Fa-f{])~a(?![0-9A-Fa-f}]))" core core)))

(defun hex-run-pattern (digits)
  "Exactly DIGITS hex digits standing alone, as a digest is written."
  (format nil "(?<![0-9A-Fa-f])[0-9A-Fa-f]{~d}(?![0-9A-Fa-f])" digits))

;;; Reading a run of characters: what the filters below read before their
;;; checks, into a buffer on the stack, so that trying a place makes no garbage.

(declaim (inline alphanumericp*))
(defun alphanumericp* (c)
  "True when C is an ASCII letter or digit."
  (declare (type character c))
  (or (char<= #\0 c #\9) (char<= #\A c #\Z) (char<= #\a c #\z)))

(defun char-table (predicate)
  "The ASCII characters PREDICATE is true of, as a bit for each code below 128."
  (let ((table (make-array 128 :element-type 'bit :initial-element 0)))
    (dotimes (code 128 table)
      (when (funcall predicate (code-char code)) (setf (sbit table code) 1)))))

(declaim (inline in-table-p))
(defun in-table-p (table ch)
  "True when CH is one of the characters of TABLE (CHAR-TABLE)."
  (declare (type simple-bit-vector table) (type character ch))
  (let ((code (char-code ch))) (and (< code 128) (= 1 (sbit table code)))))

(sb-ext:define-load-time-global *ascii-digits* (char-table (lambda (ch) (char<= #\0 ch #\9)))
  "The digits 0 to 9.")
(sb-ext:define-load-time-global *ascii-alphanumerics* (char-table #'alphanumericp*)
  "The ASCII letters and digits.")
(sb-ext:define-load-time-global *base32-either*
    (char-table (lambda (ch) (or (char<= #\A ch #\Z) (char<= #\a ch #\z) (char<= #\2 ch #\7))))
  "The characters of Base32, in capitals or small.")

(defun read-run (text start end table run &key gaps (separators ""))
  "Copy the characters of TABLE (CHAR-TABLE) from START of TEXT, before END,
into RUN, a string, as many as it holds, passing over one of SEPARATORS that
stands between two of them; GAPS, a vector of fixnums, keeps how many were
copied before each one passed, as many as it holds. Two values: how many
were copied, and how many separators were passed."
  (declare (type simple-string run) (type simple-bit-vector table) (type fixnum start end)
           (type (or null (simple-array fixnum (*))) gaps) (type simple-string separators))
  (let ((count 0) (gap-count 0) (i start))
    (declare (type fixnum count gap-count i))
    (loop while (and (< i end) (< count (length run)))
          do (let ((c (char text i)))
               (cond ((in-table-p table c)
                      (setf (schar run count) c)
                      (incf count)
                      (incf i))
                     ((and gaps (plusp count) (find c separators) (< gap-count (length gaps))
                           (< (1+ i) end) (in-table-p table (char text (1+ i))))
                      (setf (aref gaps gap-count) count)
                      (incf gap-count)
                      (incf i))
                     (t (return)))))
    (values count gap-count)))

(defun run-index (start count gaps gap-count)
  "The index of the text after the first COUNT characters READ-RUN read from
START, the separators it passed among them (GAPS, GAP-COUNT) counted in."
  (declare (type fixnum start count gap-count) (type (simple-array fixnum (*)) gaps))
  (+ start count (loop for k below gap-count count (< (aref gaps k) count))))

(defun grouped-in-fours-p (gaps gap-count length &optional other)
  "True when the separators READ-RUN passed among the first LENGTH characters
\(GAPS, GAP-COUNT) are none, or one after every fourth character, or, when
OTHER is given, a list of counts, one after each of those."
  (declare (type fixnum gap-count length) (type (simple-array fixnum (*)) gaps))
  (let ((among (loop for k below gap-count count (< (aref gaps k) length))))
    (or (zerop among)
        (and (= among (floor (1- length) 4))
             (loop for k below among always (= (aref gaps k) (* 4 (1+ k)))))
        (and other (= among (length other))
             (loop for k below among for count in other always (= (aref gaps k) count))))))

(defun one-case-p (run length)
  "True when the letters of the first LENGTH characters of RUN are all
capitals or all small."
  (declare (type simple-string run) (type fixnum length))
  (or (loop for k below length never (lower-case-p (schar run k)))
      (loop for k below length never (upper-case-p (schar run k)))))

;;; Card numbers, IBANs, Bitcoin and onion addresses: each the standard's check
;;; on the run read.

(defparameter *card-numbers*
  '(("visa" ((4 4)) (13 16 19))
    ("mastercard" ((51 55) (2221 2720)) (16))
    ("amex" ((34 34) (37 37)) (15))
    ("discover" ((6011 6011) (644 649) (65 65) (622126 622925)) (16 17 18 19))
    ("diners" ((36 36) (300 305) (3095 3095) (38 39)) (14 15 16 17 18 19))
    ("jcb" ((3528 3589)) (16 17 18 19))
    ("unionpay" ((62 62)) (16 17 18 19))
    ("maestro" ((5018 5018) (5020 5020) (5038 5038) (5893 5893) (6304 6304) (6759 6759) (6761 6763))
     (12 13 14 15 16 17 18 19))
    ("mir" ((2200 2204)) (16 17 18 19)))
  "The payment card networks a named pattern finds the numbers of, each (NAME
PREFIXES LENGTHS): PREFIXES, how its numbers begin, the network's issuer
numbers (ISO/IEC 7812) as it publishes them, each (LOW HIGH), every number
from LOW to HIGH, the two of as many digits; LENGTHS, how many digits its
numbers have.")

(defun card-network-p (digits length network)
  "True when the first LENGTH of DIGITS, a string of digits, are a number of
the card NETWORK (an entry of *CARD-NUMBERS*): one of its lengths and
prefixes."
  (declare (type simple-string digits) (type fixnum length))
  (destructuring-bind (name prefixes lengths) network
    (declare (ignore name))
    (and (member length lengths)
         (loop for (low high) in prefixes
               thereis (let ((width (loop for n = low then (floor n 10) while (plusp n) count t)))
                         (and (<= width length)
                              (<= low
                                  (let ((value 0))
                                    (dotimes (k width value)
                                      (setf value (+ (* value 10) (- (char-code (schar digits k)) 48)))))
                                  high)))))))

(defun luhn-p (digits &optional (length (length digits)))
  "True when the first LENGTH of DIGITS, a string of digits, pass the Luhn
check (ISO/IEC 7812): every second digit from the last doubled, nine taken
off a double past nine, the sum a multiple of ten."
  (let ((sum 0))
    (loop for i from (1- length) downto 0
          for count from 0
          do (let ((digit (- (char-code (char digits i)) 48)))
               (when (oddp count)
                 (setf digit (* 2 digit))
                 (when (> digit 9) (decf digit 9)))
               (incf sum digit)))
    (zerop (mod sum 10))))

(defun card-end (text start end networks)
  "Where the card number that begins at START of TEXT (before END) ends, or
NIL when none does. It is the most digits, read with the spaces or hyphens
between them (READ-RUN), that are written as a card number is, together, in
fours, or for 14 and 15 digits after the fourth and the tenth
\(GROUPED-IN-FOURS-P), are a number of one of NETWORKS (CARD-NETWORK-P), pass
the Luhn check, and are followed by no digit."
  (let ((digits (make-string 19)) (gaps (make-array 18 :element-type 'fixnum)))
    (declare (dynamic-extent digits gaps))
    (multiple-value-bind (count gap-count)
        (read-run text start end *ascii-digits* digits :gaps gaps :separators " -")
      (loop for length from count downto 12
            for after = (run-index start length gaps gap-count)
            ;; no digit right after it: the next is a space or hyphen, or no digit
            when (and (or (< length count) (>= after end) (not (char<= #\0 (char text after) #\9)))
                      (or (= length count) (find length gaps :end gap-count))
                      (grouped-in-fours-p gaps gap-count length (and (member length '(14 15)) '(4 10)))
                      (loop for network in networks thereis (card-network-p digits length network))
                      (luhn-p digits length))
              return after))))

(defun text-filter (function &rest arguments)
  "A cl-ppcre filter that matches where FUNCTION says a match ends: FUNCTION
is called with the text being matched, the position, the end of the text
and ARGUMENTS, and gives the position after the match, or NIL when none
stands there."
  (lambda (position)
    (apply function cl-ppcre::*string* position cl-ppcre::*end-pos* arguments)))

(defun card-filter (&optional network)
  "A cl-ppcre filter that matches a card number of NETWORK (a name of
*CARD-NUMBERS*), or of any network, where it stands (CARD-END)."
  (text-filter #'card-end (if network
                              (list (assoc network *card-numbers* :test #'string=))
                              *card-numbers*)))

(defparameter *iban-lengths*
  '(("AD" . 24) ("AE" . 23) ("AL" . 28) ("AT" . 20) ("AZ" . 28) ("BA" . 20) ("BE" . 16) ("BG" . 22)
    ("BH" . 22) ("BI" . 27) ("BR" . 29) ("BY" . 28) ("CH" . 21) ("CR" . 22) ("CY" . 28) ("CZ" . 24)
    ("DE" . 22) ("DJ" . 27) ("DK" . 18) ("DO" . 28) ("EE" . 20) ("EG" . 29) ("ES" . 24) ("FI" . 18)
    ("FK" . 18) ("FO" . 18) ("FR" . 27) ("GB" . 22) ("GE" . 22) ("GI" . 23) ("GL" . 18) ("GR" . 27)
    ("GT" . 28) ("HR" . 21) ("HU" . 28) ("IE" . 22) ("IL" . 23) ("IQ" . 23) ("IS" . 26) ("IT" . 27)
    ("JO" . 30) ("KW" . 30) ("KZ" . 20) ("LB" . 28) ("LC" . 32) ("LI" . 21) ("LT" . 20) ("LU" . 20)
    ("LV" . 21) ("LY" . 25) ("MC" . 27) ("MD" . 24) ("ME" . 22) ("MK" . 19) ("MN" . 20) ("MR" . 27)
    ("MT" . 31) ("MU" . 30) ("NI" . 28) ("NL" . 18) ("NO" . 15) ("OM" . 23) ("PK" . 24) ("PL" . 28)
    ("PS" . 29) ("PT" . 25) ("QA" . 29) ("RO" . 24) ("RS" . 22) ("RU" . 33) ("SA" . 24) ("SC" . 31)
    ("SD" . 18) ("SE" . 24) ("SI" . 19) ("SK" . 24) ("SM" . 27) ("SO" . 23) ("ST" . 25) ("SV" . 28)
    ("TL" . 23) ("TN" . 24) ("TR" . 26) ("UA" . 29) ("VA" . 22) ("VG" . 24) ("XK" . 20) ("YE" . 30))
  "How many characters an IBAN of each country has, by its code, as the IBAN
registry (SWIFT, for ISO 13616) lists them.")

(defun iban-check-p (iban &optional (length (length iban)))
  "True when the first LENGTH characters of IBAN, without spaces, pass ISO
7064 MOD 97-10: its first four put last, each letter written as its number
\(A 10 to Z 35), the whole leaves 1 when divided by 97."
  (let ((rest 0))
    (flet ((add (c)
             (let ((value (digit-char-p c 36)))
               (setf rest (mod (if (< value 10) (+ (* rest 10) value) (+ (* rest 100) value)) 97)))))
      (loop for i from 4 below length do (add (char iban i)))
      (loop for i from 0 below 4 do (add (char iban i))))
    (= rest 1)))

(defun iban-end (text start end)
  "Where the IBAN that begins at START of TEXT (before END) ends, or NIL when
none does (ISO 13616): a country's code, two check digits and the rest, as
many characters in all as that country's IBANs have (*IBAN-LENGTHS*),
written together or in groups of four apart by a space, followed by no
letter or digit, passing the check (IBAN-CHECK-P). Letters are capitals, or
all small."
  (let ((iban (make-string 34)) (gaps (make-array 9 :element-type 'fixnum)))
    (declare (dynamic-extent iban gaps))
    (multiple-value-bind (count gap-count)
        (read-run text start end *ascii-alphanumerics* iban :gaps gaps :separators " ")
      (let ((length (and (>= count 2)
                         (loop for (code . characters) in *iban-lengths*
                               when (and (char-equal (char code 0) (schar iban 0))
                                         (char-equal (char code 1) (schar iban 1)))
                                 return characters))))
        (when (and length (>= count length))
          (let ((after (run-index start length gaps gap-count)))
            (and (or (>= after end) (not (alphanumericp* (char text after))))
                 (grouped-in-fours-p gaps gap-count length)
                 (one-case-p iban length)
                 (char<= #\0 (schar iban 2) #\9) (char<= #\0 (schar iban 3) #\9)
                 (iban-check-p iban length)
                 after)))))))

(defparameter *sha256-constants*
  (coerce '(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
            #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
            #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
            #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
            #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
            #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
            #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
            #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2)
          '(simple-array (unsigned-byte 32) (64)))
  "SHA-256's round constants (FIPS 180-4).")

(defun short-sha256 (octets end digest)
  "The SHA-256 digest (FIPS 180-4) of the first END of OCTETS, a short
message of at most 55 octets, which one block holds, written into DIGEST, 32
octets, which it returns: for the few octets a check sums, as a Bitcoin
address's. DIGEST may be OCTETS."
  (declare (optimize speed) (type (simple-array (unsigned-byte 8) (*)) octets digest) (type (integer 0 55) end))
  (let* ((k *sha256-constants*)
         (h (make-array 8 :element-type '(unsigned-byte 32)))
         (padded 64)
         (message (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0))
         (w (make-array 64 :element-type '(unsigned-byte 32))))
    (declare (type (simple-array (unsigned-byte 32) (64)) k w)
             (type (simple-array (unsigned-byte 32) (8)) h)
             (type (simple-array (unsigned-byte 8) (64)) message)
             (type fixnum padded)
             (dynamic-extent h message w))
    (setf (aref h 0) #x6a09e667 (aref h 1) #xbb67ae85 (aref h 2) #x3c6ef372 (aref h 3) #xa54ff53a
          (aref h 4) #x510e527f (aref h 5) #x9b05688c (aref h 6) #x1f83d9ab (aref h 7) #x5be0cd19)
    (replace message octets :end2 end)
    (setf (aref message end) #x80)
    (loop for i from 0 below 8
          do (setf (aref message (- padded 1 i)) (ldb (byte 8 (* 8 i)) (* 8 end))))
    (macrolet ((rotr (x n) `(logior (ash ,x (- ,n)) (ldb (byte 32 0) (ash ,x (- 32 ,n)))))
               (add (&rest xs) `(ldb (byte 32 0) (+ ,@xs))))
      (loop for block of-type fixnum from 0 below padded by 64
            do (dotimes (i 16)
                 (setf (aref w i) (logior (ash (aref message (+ block (* 4 i))) 24)
                                          (ash (aref message (+ block (* 4 i) 1)) 16)
                                          (ash (aref message (+ block (* 4 i) 2)) 8)
                                          (aref message (+ block (* 4 i) 3)))))
               (loop for i from 16 below 64
                     do (let ((a (aref w (- i 15))) (b (aref w (- i 2))))
                          (setf (aref w i)
                                (add (aref w (- i 16)) (logxor (rotr a 7) (rotr a 18) (ash a -3))
                                     (aref w (- i 7)) (logxor (rotr b 17) (rotr b 19) (ash b -10))))))
               (let ((a (aref h 0)) (b (aref h 1)) (c (aref h 2)) (d (aref h 3))
                     (e (aref h 4)) (f (aref h 5)) (g (aref h 6)) (hh (aref h 7)))
                 (declare (type (unsigned-byte 32) a b c d e f g hh))
                 (dotimes (i 64)
                   (let* ((t1 (add hh (logxor (rotr e 6) (rotr e 11) (rotr e 25))
                                   (logxor (logand e f) (logand (logxor e #xffffffff) g))
                                   (aref k i) (aref w i)))
                          (t2 (add (logxor (rotr a 2) (rotr a 13) (rotr a 22))
                                   (logxor (logand a b) (logand a c) (logand b c)))))
                     (setf hh g g f f e e (add d t1) d c c b b a a (add t1 t2))))
                 (setf (aref h 0) (add (aref h 0) a) (aref h 1) (add (aref h 1) b)
                       (aref h 2) (add (aref h 2) c) (aref h 3) (add (aref h 3) d)
                       (aref h 4) (add (aref h 4) e) (aref h 5) (add (aref h 5) f)
                       (aref h 6) (add (aref h 6) g) (aref h 7) (add (aref h 7) hh)))))
    (dotimes (i 8 digest)
      (dotimes (j 4)
        (setf (aref digest (+ (* 4 i) j)) (ldb (byte 8 (* 8 (- 3 j))) (aref h i)))))))

(defparameter *base58* "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  "The characters of Base58, as Bitcoin writes numbers in them, by value.")

(defparameter *bech32* "qpzry9x8gf2tvdw0s3jn54khce6mua7l"
  "The characters of Bech32 (BIP 173), by value.")

(defun base58check-address-p (address length)
  "True when the first LENGTH characters of ADDRESS, Base58, are a Bitcoin
address of the first kinds (P2PKH, starting 1; P2SH, starting 3): 25 octets
once read, a version of 0 or 5, and its last four the first four of SHA-256
twice over the rest."
  (let ((octets (make-array 25 :element-type '(unsigned-byte 8) :initial-element 0))
        (sum (make-array 32 :element-type '(unsigned-byte 8)))
        (zeros (loop for k below length while (char= (schar address k) #\1) count t)))
    (declare (dynamic-extent octets sum))
    ;; the number the characters write, an octet at a time
    (dotimes (k length)
      (let ((carry (position (schar address k) *base58*)))
        (unless carry (return-from base58check-address-p nil))
        (loop for i from 24 downto 0
              do (setf carry (+ carry (* 58 (aref octets i)))
                       (aref octets i) (ldb (byte 8 0) carry)
                       carry (ash carry -8)))
        (unless (zerop carry) (return-from base58check-address-p nil))))
    ;; each leading 1 an octet of zero
    (and (loop for i below zeros always (zerop (aref octets i)))
         (member (aref octets 0) '(0 5))
         (progn (short-sha256 octets 21 sum)
                (short-sha256 sum 32 sum)

                (loop for i below 4 always (= (aref sum i) (aref octets (+ 21 i))))))))

(defun bech32-step (check value)
  "BIP 173's checksum CHECK with VALUE, a number below 32, taken in."
  (let ((top (ash check -25))
        (check (logxor (ash (logand check #x1ffffff) 5) value)))
    (loop for g across #(#x3b6a57b2 #x26508e6d #x1ea119fa #x3d4233dd #x2a1462b3)
          for i from 0
          when (logbitp i top) do (setf check (logxor check g)))
    check))

(defun segwit-address-p (address length)
  "True when the first LENGTH characters of ADDRESS (bc1..., all small or all
capitals) are a Bitcoin SegWit address: Bech32 for version 0 (BIP 173),
Bech32m for versions 1 to 16 (BIP 350), its checksum right, its program 2 to
40 octets, 20 or 32 for version 0."
  (let ((values (- length 3)))
    (when (and (one-case-p address length) (>= values 7))
      (let ((check 1) (version 0) (bits 0) (count 0) (program 0))
        ;; the part before the 1, "bc", as BIP 173 expands it
        (dolist (value '(3 3 0 2 3))
          (setf check (bech32-step check value)))
        (loop for k from 3 below length
              for index from 0
              do (let ((value (position (char-downcase (schar address k)) *bech32*)))
                   (unless value (return-from segwit-address-p nil))
                   (setf check (bech32-step check value))
                   (cond ((zerop index) (setf version value))
                         ;; the program: its five-bit values made octets, nothing left over but zeros
                         ((< index (- values 6))
                          (setf bits (logior (ash bits 5) value))
                          (incf count 5)
                          (when (>= count 8)
                            (decf count 8)
                            (incf program)
                            (setf bits (ldb (byte count 0) bits)))))))
        (and (<= version 16) (< count 5) (zerop bits)
             (<= 2 program 40)
             (if (zerop version)
                 (and (= check 1) (member program '(20 32)))
                 (= check #x2bc830a3)))))))

(defun bitcoin-end (text start end)
  "Where the Bitcoin address that begins at START of TEXT (before END) ends,
or NIL when none does: a Base58Check one of 26 to 35 characters
\(BASE58CHECK-ADDRESS-P) or a SegWit one (SEGWIT-ADDRESS-P), followed by no
letter or digit."
  (let ((address (make-string 91)))
    (declare (dynamic-extent address))
    (let ((length (read-run text start end *ascii-alphanumerics* address)))
      (and (< length 91)
           (if (and (>= length 3) (string-equal "bc1" address :end2 3))
               (and (<= 14 length 90) (segwit-address-p address length))
               (and (<= 26 length 35) (base58check-address-p address length)))
           (+ start length)))))

(defparameter *keccak-round-constants*
  (coerce '(#x0000000000000001 #x0000000000008082 #x800000000000808A #x8000000080008000
            #x000000000000808B #x0000000080000001 #x8000000080008081 #x8000000000008009
            #x000000000000008A #x0000000000000088 #x0000000080008009 #x000000008000000A
            #x000000008000808B #x800000000000008B #x8000000000008089 #x8000000000008003
            #x8000000000008002 #x8000000000000080 #x000000000000800A #x800000008000000A
            #x8000000080008081 #x8000000000008080 #x0000000080000001 #x8000000080008008)
          '(simple-array (unsigned-byte 64) (24)))
  "Keccak-f[1600]'s round constants (FIPS 202).")

(defparameter *keccak-rotations*
  (coerce '(0 1 62 28 27 36 44 6 55 20 3 10 43 25 39 41 45 15 21 8 18 2 61 56 14)
          '(simple-array (unsigned-byte 8) (25)))
  "How far Keccak-f[1600] turns each lane, lane X + 5Y (FIPS 202).")

(defun sha3-256 (octets &optional (end (length octets))
                           (digest (make-array 32 :element-type '(unsigned-byte 8))))
  "The SHA3-256 digest (FIPS 202) of the first END of OCTETS, all of them when
not given, written into DIGEST, 32 octets, a new one when not given, which it
returns: for the few octets a check sums, as an onion address's."
  (declare (optimize speed) (type (simple-array (unsigned-byte 8) (*)) octets digest) (type fixnum end))
  (let* ((size (* 136 (ceiling (1+ end) 136)))
         (a (make-array 25 :element-type '(unsigned-byte 64) :initial-element 0))
         (b (make-array 25 :element-type '(unsigned-byte 64) :initial-element 0))
         (c (make-array 5 :element-type '(unsigned-byte 64) :initial-element 0))
         (padded (make-array size :element-type '(unsigned-byte 8) :initial-element 0))
         (round-constants *keccak-round-constants*)
         (rotations *keccak-rotations*))
    (declare (type (simple-array (unsigned-byte 64) (25)) a b)
             (type (simple-array (unsigned-byte 64) (5)) c)
             (type (simple-array (unsigned-byte 8) (*)) padded)
             (type (simple-array (unsigned-byte 64) (24)) round-constants)
             (type (simple-array (unsigned-byte 8) (25)) rotations)
             (type fixnum size)
             (dynamic-extent a b c padded))
    (flet ((rot (x n)
             (declare (type (unsigned-byte 64) x) (type (integer 0 63) n))
             (logior (ldb (byte 64 0) (ash x n)) (ash x (- n 64)))))
      (declare (inline rot))
      (replace padded octets :end2 end)
      (setf (aref padded end) #x06)
      (setf (aref padded (1- size)) (logior (aref padded (1- size)) #x80))
      (loop for block of-type fixnum from 0 below size by 136
            do (dotimes (lane 17)
                 (dotimes (k 8)
                   (setf (aref a lane) (logxor (aref a lane) (ash (aref padded (+ block (* 8 lane) k)) (* 8 k))))))
               (dotimes (round 24)
                 (dotimes (x 5)
                   (setf (aref c x)
                         (logxor (aref a x) (aref a (+ x 5)) (aref a (+ x 10)) (aref a (+ x 15)) (aref a (+ x 20)))))
                 (dotimes (x 5)
                   (let ((d (logxor (aref c (mod (+ x 4) 5)) (rot (aref c (mod (1+ x) 5)) 1))))
                     (dotimes (y 5) (setf (aref a (+ x (* 5 y))) (logxor (aref a (+ x (* 5 y))) d)))))
                 (dotimes (x 5)
                   (dotimes (y 5)
                     (setf (aref b (+ y (* 5 (mod (+ (* 2 x) (* 3 y)) 5))))
                           (rot (aref a (+ x (* 5 y))) (aref rotations (+ x (* 5 y)))))))
                 (dotimes (x 5)
                   (dotimes (y 5)
                     (setf (aref a (+ x (* 5 y)))
                           (logxor (aref b (+ x (* 5 y)))
                                   (logand (logxor (aref b (+ (mod (1+ x) 5) (* 5 y))) #xFFFFFFFFFFFFFFFF)
                                           (aref b (+ (mod (+ x 2) 5) (* 5 y))))))))
                 (setf (aref a 0) (logxor (aref a 0) (aref round-constants round))))))
    (dotimes (i 32 digest)
      (setf (aref digest i) (ldb (byte 8 (* 8 (mod i 8))) (aref a (floor i 8)))))))

(defparameter *base32* "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"
  "The characters of Base32 (RFC 4648), by value.")

(defun onion-v3-p (name)
  "True when NAME, the 56 characters before .onion, is a version 3 onion
address (Tor's rend-spec-v3): Base32 for a key of 32 octets, two octets of
check and the version 3, the check the first two octets of SHA3-256 of
\".onion checksum\", the key and the version."
  (let ((octets (make-array 35 :element-type '(unsigned-byte 8)))
        (input (make-array 48 :element-type '(unsigned-byte 8)))
        (sum (make-array 32 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent octets input sum))
    ;; 56 characters of five bits each: 280 bits, 35 octets
    (let ((bits 0) (count 0) (at 0))
      (dotimes (k 56)
        (setf bits (logior (ash bits 5) (position (char-upcase (schar name k)) *base32*)))
        (incf count 5)
        (when (>= count 8)
          (decf count 8)
          (setf (aref octets at) (ldb (byte 8 count) bits)
                bits (ldb (byte count 0) bits))
          (incf at))))
    (and (= (aref octets 34) 3)
         (progn
           (loop for c across ".onion checksum" for i from 0 do (setf (aref input i) (char-code c)))
           (replace input octets :start1 15 :end2 32)
           (setf (aref input 47) 3)
           (sha3-256 input 48 sum)
           (and (= (aref sum 0) (aref octets 32)) (= (aref sum 1) (aref octets 33)))))))

(defun onion-end (text start end)
  "Where the onion address that begins at START of TEXT (before END) ends, or
NIL when none does: 56 characters of Base32, all small or all capitals, its
check right (ONION-V3-P), or 16 for a version 2 address, which has none; then
.onion, followed by no letter, digit or hyphen."
  (let ((name (make-string 57)))
    (declare (dynamic-extent name))
    (let* ((length (read-run text start end *base32-either* name))
           (stop (+ start length)))
      (and (member length '(16 56))
           (<= (+ stop 6) end)
           (string-equal ".onion" text :start2 stop :end2 (+ stop 6))
           (one-case-p name length)
           (or (= length 16) (onion-v3-p name))
           (let ((after (+ stop 6)))
             (and (or (>= after end) (not (or (alphanumericp* (char text after)) (char= (char text after) #\-))))
                  after))))))

(defvar *window-open* nil
  "True while a pattern is tried on a window of a longer text, more of it
after the window's end: a filter reading to that end then says it may go on.")

(defvar *window-rope* nil
  "While a pattern is tried on a window of a rope, (ROPE CONTEXT OCTET BOUND):
the rope, the octets the window's text begins and ends at, and the one past
which the pattern sees nothing. A filter may read the rope before the window
and, when the window says it may go on (*WINDOW-OPEN*), after it
\(CALL-WITH-TEXT-READER).")

(defvar *window-reached* nil
  "The octet a filter's match reached, read on past the window's end
(WINDOW-INDEX); NIL while none did.")

(defun rope-reader (rope low high &optional (size 65536))
  "Two functions reading ROPE's octets from LOW below HIGH SIZE at a time:
of an offset, the octet there as a character; of an offset, a limit and a
CHAR-TABLE, the first offset from there below the limit whose octet is not in
the table, or the limit."
  (let ((buffer (tuatara.text:make-octets size)) (from 0) (to 0))
    (declare (type fixnum from to) (type tuatara.text:octets buffer))
    (values
     (lambda (at)
       (declare (type fixnum at) (optimize speed))
       (unless (and (<= from at) (< at to))
         (setf from (max low (- at (floor (length buffer) 2)))
               to (min high (+ from (length buffer))))
         (tuatara.text:rope-copy-into rope from to buffer))
       (code-char (aref buffer (- at from))))
     (lambda (at limit table)
       (declare (type fixnum at limit) (type simple-bit-vector table) (optimize speed))
       (block run
         (loop
           (when (>= at limit) (return limit))
           (unless (and (<= from at) (< at to))
             (setf from at to (min high (+ at (length buffer))))
             (tuatara.text:rope-copy-into rope from to buffer))
           (let ((stop (min to limit)))
             (loop for j of-type fixnum from at below stop
                   do (let ((b (aref buffer (- j from))))
                        (unless (and (< b 128) (= 1 (sbit table b))) (return-from run j))))
             (setf at stop))))))))

(sb-ext:define-load-time-global *base32-capitals*
    (char-table (lambda (ch) (or (char<= #\A ch #\Z) (char<= #\2 ch #\7))))
  "The characters of Base32 written in capitals (RFC 4648).")
(sb-ext:define-load-time-global *base32-small* (char-table (lambda (ch) (or (char<= #\a ch #\z) (char<= #\2 ch #\7))))
  "The characters of Base32 written small.")
(sb-ext:define-load-time-global *base64-standard* (char-table (lambda (ch) (or (alphanumericp* ch) (find ch "+/"))))
  "The characters of Base64 (RFC 4648).")
(sb-ext:define-load-time-global *base64-safe* (char-table (lambda (ch) (or (alphanumericp* ch) (find ch "-_"))))
  "The characters of Base64 for addresses and file names, - and _ for + and /.")

(defun call-with-text-reader (text end function)
  "Call FUNCTION with five values for reading TEXT, which ends at END. The
first is a function of an index giving the character there. The second is the
index the text it reads ends at. The third is whether that end is a window's
that says it may go on (*WINDOW-OPEN*). The fourth is the first index it
reads. The fifth is a function of an index and a CHAR-TABLE giving the first
index from there whose character is not in the table, or the end.
On a window of a rope (*WINDOW-ROPE*), END the window's end, the rope is read
before the window, from its start, and after it, as far as the pattern sees,
when the window may go on. An index before 0 or past END stands for the octet
as far before or past the window, each octet as a character. So a run of ASCII
characters, a block of Base64 say, is read whole however long, for
WINDOW-INDEX to give back. The two functions are made on the stack: FUNCTION
may pass them on, never keep them."
  (declare (type function function))
  (destructuring-bind (&optional rope context octet bound) *window-rope*
    (if rope
        (let ((back nil) (on nil) (on-run nil) (open *window-open*))
          (declare (type fixnum end context octet bound))
          ;; the rope is read only where a filter reads past the window
          (flet ((before (at)
                   (funcall (the function (or back (setf back (rope-reader rope 0 context 4096)))) at))
                 (after (at)
                   (unless on (setf (values on on-run) (rope-reader rope octet bound)))
                   (funcall (the function on) at))
                 (after-run (at table)
                   (unless on (setf (values on on-run) (rope-reader rope octet bound)))
                   (funcall (the function on-run) at bound table)))
            (declare (inline before after after-run))
            (macrolet ((reading (type)
                         `(let ((text text))
                            (declare (type ,type text))
                            (flet ((char-at (i)
                                     (declare (type fixnum i) (optimize speed))
                                     (cond ((minusp i) (before (+ context i)))
                                           ((< i end) (schar text i))
                                           (t (after (+ octet (- i end))))))
                                   (run (i table)
                                     (declare (type fixnum i) (type simple-bit-vector table) (optimize speed))
                                     (let ((j (loop for j of-type fixnum from i below end
                                                    unless (in-table-p table (schar text j)) return j
                                                    finally (return (max i end)))))
                                       (declare (type fixnum j))
                                       (if (and open (>= j end))
                                           (+ end (- (the fixnum (after-run (+ octet (- j end)) table)) octet))
                                           j))))
                              (declare (dynamic-extent #'char-at #'run))
                              (funcall function #'char-at (if open
                                                              (+ end (- bound octet))
                                                              end) nil (- context) #'run)))))
              (if (typep text 'simple-base-string)
                  (reading simple-base-string)
                  (reading (simple-array character (*)))))))
        (flet ((char-at (i) (char text i))
               (run (i table)
                 (loop for j from i below end
                       unless (in-table-p table (char text j)) return j
                       finally (return (max i end)))))
          (declare (dynamic-extent #'char-at #'run))
          (funcall function #'char-at end *window-open* 0 #'run)))))

(defmacro with-text-reader (((char-at reach open first run) text end) &body body)
  "Run BODY with CHAR-AT, REACH, OPEN, FIRST and RUN bound as
CALL-WITH-TEXT-READER gives them for reading TEXT, which ends at END."
  (let ((reader (gensym "READER")))
    `(flet ((,reader (,char-at ,reach ,open ,first ,run)
              (declare (ignorable ,char-at ,reach ,open ,first ,run))
              ,@body))
       (declare (dynamic-extent #',reader))
       (call-with-text-reader ,text ,end #',reader))))

(defun window-index (index end)
  "INDEX, from CALL-WITH-TEXT-READER's reading of a text ending at END, as an
index of that text: one at END or past it read on in the rope is kept in
*WINDOW-REACHED* as its octet and given as END."
  (if (and index *window-open* *window-rope* (>= index end))
      (progn (setf *window-reached* (+ (third *window-rope*) (- index end))) end)
      index))

(defun continued-line-p (char-at first start end table group)
  "True when START, read by CHAR-AT from FIRST, begins a line that goes on a
block of RFC 4648 encoding before it. The line before is of the block's
characters (TABLE, a CHAR-TABLE) from a start a block may have, after no +,
/, -, _ or =, its width a whole number of GROUP and 16 or more. The line at
START is no longer, before END. Such a line begins no block, the block before
taking it in."
  (declare (type function char-at) (type fixnum first start end group))
  (let ((e (1- start)))
    (declare (type fixnum e))
    (and (>= e first)
         (char= (funcall char-at e) #\Newline)
         (progn (when (and (> e first) (char= (funcall char-at (1- e)) #\Return)) (decf e)) t)
         (let ((s e))
           (declare (type fixnum s))
           (loop while (and (> s first) (in-table-p table (funcall char-at (1- s)))) do (decf s))
           (let ((width (- e s)))
             (and (>= width 16) (zerop (mod width group))
                  (or (= s first) (not (find (funcall char-at (1- s)) "+/-_=")))
                  (loop for i of-type fixnum from start below (min end (+ start width 1))
                        while (in-table-p table (funcall char-at i))
                        count t into own
                        finally (return (<= own width)))))))))

(defun encoded-block-end (char-at run first start end table group padding least open)
  "Where the block of RFC 4648 encoding that begins at START of a text (before
END), read by CHAR-AT and RUN from FIRST (CALL-WITH-TEXT-READER), ends, or NIL
when none does: none begins on a line that goes on the block before it
\(CONTINUED-LINE-P). Its characters are those of TABLE (CHAR-TABLE), GROUP of
them making whole octets, and the = PADDING allows after them (the counts of =
it may hold); at least LEAST of them, a letter and a digit among them, followed
by no letter, digit, +, /, -, _ or =. It may go on over lines of one length, a
whole number of groups each and 16 or more, the last line no longer: the block
is the most lines that make one. Cut by an END that is OPEN, a window's of a
longer text, it is said to reach that end, so that the window grows and it is
read whole."
  (declare (type function char-at run) (type fixnum first start end group least) (optimize speed))
  (when (continued-line-p char-at first start end table group)
    (return-from encoded-block-end nil))
  (let ((i start) (count 0) (width nil) (letter nil) (digit nil) (best nil))
    (declare (type fixnum i count) (type (or null fixnum) width))
    (flet ((ends-here (at count pad)
             ;; the block ending at AT, after COUNT characters and PAD of =, when it is one
             (and (>= count least) letter digit
                  (if (plusp pad)
                      (and (member pad padding) (zerop (mod (+ count pad) group)))
                      (let ((left (mod count group)))
                        (or (zerop left) (loop for p in padding thereis (= left (- group p))))))
                  (or (>= at end) (not (or (alphanumericp* (funcall char-at at)) (find (funcall char-at at) "+/-_="))))
                  at)))
      (loop
        (let ((line-start i) (stop (funcall run i table)))
          (declare (type fixnum stop))
          (unless (and letter digit)
            (loop for k of-type fixnum from i below stop
                  do (let ((ch (funcall char-at k)))
                       (if (char<= #\0 ch #\9) (setf digit t) (when (alphanumericp* ch) (setf letter t))))
                  until (and letter digit)))
          (setf i stop)
          (when (and open (>= i end)) (return end))
          (let ((length (- i line-start)))
            (when (and width (> length width)) (return best))
            (incf count length)
            (unless width (setf width length))
            (let ((next (cond ((and (< i end) (char= (funcall char-at i) #\Newline)) (1+ i))
                              ((and (< (1+ i) end)
                                    (char= (funcall char-at i) #\Return)
                                    (char= (funcall char-at (1+ i)) #\Newline)) (+ i 2)))))
              (when (and open next (>= next end)) (return end))
              (if (and next (= length width) (>= width 16) (zerop (mod width group))
                       (< next end) (in-table-p table (funcall char-at next)))
                  (progn (setf best (or (ends-here i count 0) best) i next))
                  (let ((pad (loop while (and (< i end) (char= (funcall char-at i) #\=)) count t do (incf i))))
                    (return (if (and open (>= i end)) end (or (ends-here i count pad) best))))))))))))

(defun base32-end (text start end)
  "Where the Base32 block that begins at START of TEXT ends (RFC 4648), all
capitals or all small, or NIL: see ENCODED-BLOCK-END."
  (with-text-reader ((char-at reach open first run) text end)
    (let ((table (if (lower-case-p (char text start)) *base32-small* *base32-capitals*)))
      (window-index (encoded-block-end char-at run first start reach table 8 '(1 3 4 6) 16 open) end))))

(defun base64-end (text start end)
  "Where the Base64 block that begins at START of TEXT ends (RFC 4648), of
+ and / or of - and _ (the alphabet for addresses and file names), never
both, or NIL: see ENCODED-BLOCK-END. It holds small and capital letters."
  (with-text-reader ((char-at reach open first run) text end)
    (let* ((safe (loop for i from start below reach
                       for ch = (funcall char-at i)
                       while (or (alphanumericp* ch) (find ch "+/-_"))
                       thereis (find ch "-_")))
           (block (encoded-block-end char-at run first start reach (if safe *base64-safe* *base64-standard*)
                                     4 '(1 2) 32 open)))
      (and block
           (loop for i from start below block thereis (upper-case-p (funcall char-at i)))
           (loop for i from start below block thereis (lower-case-p (funcall char-at i)))
           (window-index block end)))))

;;; Dates: [[:date:]], [[:date<2026-07-01:]], [[:dateutc:2026-01-01..2026-06-30:]]
;;; and the others, a date read and compared as a date, not as text.

(defparameter *date-families*
  '(("date" :iso nil) ("dateutc" :iso t)
    ("datedmy" :dmy nil) ("datedmyutc" :dmy t)
    ("datemdy" :mdy nil) ("datemdyutc" :mdy t))
  "The named patterns of dates, each (NAME FORMAT UTC). FORMAT is the way the
dates are written. :ISO is 2026-06-30, RFC 3339's 2026-06-30T14:05:00+02:00.
:DMY is day, month, year (30.06.2026 14:05, 30/06/2026, as in Norway, Denmark,
Finland, New Zealand and Britain). :MDY is month, day, year (06/30/2026 2:05
PM, as in the United States). UTC is true when a time with an offset is
compared as the moment it names, in UTC, else as it is written.")

(defconstant +date-mark-days+ 1000000
  "What is added to a day number written in a date filter's mark, so that it
is never below 0: a mark holds letters, digits and hyphens only.")

(defun date-filter (spec)
  "A cl-ppcre filter for the date pattern SPEC (DATE-MARK-SPEC writes it):
it matches a date of its format where it stands (DATE-TIME-AT) that lies
from its first moment and before its last, when they are given."
  (destructuring-bind (kinds low high) (uiop:split-string spec :separator "-")
    (flet ((bound (text)
             (unless (string= text "n")
               (destructuring-bind (day nanos) (uiop:split-string text :separator "x")
                 (cons (- (parse-integer day) +date-mark-days+) (parse-integer nanos))))))
      (let ((format (ecase (char kinds 0) (#\i :iso) (#\d :dmy) (#\m :mdy)))
            (utc (char= (char kinds 1) #\u))
            (low (bound low))
            (high (bound high)))
        (text-filter
         (lambda (string position end)
           (multiple-value-bind (after day nanos zone) (tuatara.dates:date-time-at string position end format)
             (when after
               (when (and utc zone)
                 (setf (values day nanos) (tuatara.dates:utc-moment day nanos zone)))
               (and (or (null low) (not (tuatara.dates:moment< day nanos (car low) (cdr low))))
                    (or (null high) (tuatara.dates:moment< day nanos (car high) (cdr high)))
                    after)))))))))

(defun date-mark-spec (format utc low high)
  "The words a date filter's mark carries: FORMAT and UTC as *DATE-FAMILIES*
has them, LOW and HIGH each NIL or (DAY . NANOSECONDS), the first moment
taken and the first after the last one."
  (flet ((bound (point)
           (if point (format nil "~dx~d" (+ (car point) +date-mark-days+) (cdr point)) "n")))
    (format nil "~a~a-~a-~a" (ecase format (:iso "i") (:dmy "d") (:mdy "m")) (if utc "u" "w")
            (bound low) (bound high))))

(defun date-pattern (format utc low high)
  "The pattern of a date written in FORMAT, from LOW and before HIGH (see
DATE-MARK-SPEC), no digit just before: read by a filter (DATE-FILTER)."
  (format nil "(?<![0-9])(?=~a)(?<~adate-~a>)"
          (if (eq format :iso) "[0-9]{4}-[0-9]{2}-" "[0-9]")
          *filter-mark* (date-mark-spec format utc low high)))

(defun date-bound (text name format utc)
  "TEXT, a date bounding the dates of [[:NAME...:]], read in FORMAT or as
2026-07-01T12:00 is, as three values: its day, its nanoseconds and the
nanoseconds its last figure counts in, in UTC when UTC is true. Signals
PATTERN-REFUSED when TEXT is no date, or gives an offset that NAME, comparing
dates as written, cannot use."
  (destructuring-bind (&optional after day nanos zone unit)
      (flet ((whole (format)
               (let ((found (multiple-value-list (tuatara.dates:date-time-at text 0 (length text) format))))
                 (and (eql (first found) (length text)) found))))
        (or (whole format) (and (not (eq format :iso)) (whole :iso))))
    (unless after
      (refuse-pattern "invalid: [[:~a…:]] has no date in ~s: write one as 2026-07-01 or 2026-07-01T12:00~a"
                      name text (if utc "Z" "")))
    (when zone
      (unless utc
        (refuse-pattern "invalid: [[:~a…:]] compares dates as they are written, with no offset: ~
                         [[:~autc…:]] compares the moments in UTC"
                        name name))
      (setf (values day nanos) (tuatara.dates:utc-moment day nanos zone)))
    (values day nanos unit)))

(defun date-pattern-at (regex i)
  "When a date pattern opens at index I of REGEX, two values: the index after
it and the pattern it stands for (DATE-PATTERN); else NIL. One of
*DATE-FAMILIES* in its brackets, alone, [[:date:]], or with a bound:
<B before B, <=B up to the end of B, >B after the end of B, >=B from B, =B
within B, :A..B from A to the end of B, either left out. B is a day or a
time: <=2026-06-30 takes the whole of that day, <=2026-06-30T14:05 the whole
of that minute. Signals PATTERN-REFUSED for a bound that is no date."
  (when (and (< (+ i 3) (length regex)) (string= "[[:" regex :start2 i :end2 (+ i 3)))
    (let ((family (loop for entry in (sort (copy-list *date-families*) #'> :key (lambda (e) (length (first e))))
                        for name = (first entry)
                        for after = (+ i 3 (length name))
                        when (and (<= after (length regex))
                                  (string= name regex :start2 (+ i 3) :end2 after)
                                  (tuatara.dates:char-at-p regex after (length regex) "<>=:"))
                          return entry)))
      (when family
        (destructuring-bind (name format utc) family
          (let* ((from (+ i 3 (length name)))
                 (close (search ":]]" regex :start2 from)))
            (when close
              (let ((words (subseq regex from close)) (low nil) (high nil))
                (flet ((start-of (text)
                         (multiple-value-bind (day nanos) (date-bound text name format utc) (cons day nanos)))
                       (end-of (text)
                         (multiple-value-bind (day nanos unit) (date-bound text name format utc)
                           (multiple-value-bind (d n) (tuatara.dates:moment-after day nanos unit) (cons d n)))))
                  (cond ((zerop (length words)))
                        ((char= (char words 0) #\:)
                         (let ((dots (search ".." words)))
                           (unless dots
                             (refuse-pattern "invalid: [[:~a:A..B:]] wants two dots between its dates, ~
                                              either left out"
                                             name))
                           (let ((a (subseq words 1 dots)) (b (subseq words (+ dots 2))))
                             (when (plusp (length a)) (setf low (start-of a)))
                             (when (plusp (length b)) (setf high (end-of b))))))
                        ((uiop:string-prefix-p "<=" words) (setf high (end-of (subseq words 2))))
                        ((uiop:string-prefix-p ">=" words) (setf low (start-of (subseq words 2))))
                        ((char= (char words 0) #\<) (setf high (start-of (subseq words 1))))
                        ((char= (char words 0) #\>) (setf low (end-of (subseq words 1))))
                        (t (setf low (start-of (subseq words 1)) high (end-of (subseq words 1))))))
                (when (and low high (not (tuatara.dates:moment< (car low) (cdr low) (car high) (cdr high))))
                  (refuse-pattern "invalid: [[:~a~a:]] ends before it begins" name words))
                (values (+ close 3) (date-pattern format utc low high))))))))))

(defparameter *pattern-filters*
  (list* (cons "card" (lambda () (card-filter)))
         (cons "date" #'date-filter)
         (cons "iban" (lambda () (text-filter #'iban-end)))
         (cons "bitcoin" (lambda () (text-filter #'bitcoin-end)))
         (cons "onion" (lambda () (text-filter #'onion-end)))
         (cons "base32" (lambda () (text-filter #'base32-end)))
         (cons "base64" (lambda () (text-filter #'base64-end)))
         (loop for (name) in *card-numbers*
               collect (let ((name name)) (cons (format nil "card~a" name) (lambda () (card-filter name))))))
  "The filters a named pattern may hold (*FILTER-MARK*), each (NAME . MAKER),
MAKER a function making the filter: of no arguments, or, for a mark NAME-WORDS,
of WORDS.")

(defun card-pattern (&optional network)
  "The numbers of the card NETWORK (a name of *CARD-NUMBERS*), or of any of
them, no digit just before: read by a filter (CARD-FILTER), as they are
written, together or in groups, of the network's prefixes and lengths, the
Luhn check digit right (ISO/IEC 7812)."
  (format nil "(?<![0-9])(?=[0-9]{4}(?:[0-9]{4}|[ -][0-9]{4}))(?<~acard~@[~a~]>)" *filter-mark* network))

(defparameter *named-patterns*
  (append
   `(("ipv4" "an IPv4 address (RFC 3986)" ,(ipv4-pattern))
     ("ipv6" "an IPv6 address, :: and an IPv4 tail too (RFC 3986, RFC 4291)" ,(ipv6-pattern))
     ("ipaddress" "an IPv4 or IPv6 address" ,(format nil "(?:~a|~a)" (ipv4-pattern) (ipv6-pattern)))
     ("macaddress" "a MAC address, AC-DE-48-00-00-80, ac:de:48:00:00:80 or acde.4800.0080 (IEEE 802)"
      ,(mac-pattern))
     ("email" "an e-mail address (the HTML standard's valid e-mail address)" ,(email-pattern))
     ("url" "a web or FTP address: http, https, ftp, ftps, a closing bracket only with its opening one (RFC 3986)"
      ,(url-pattern))
     ("uuid" "a UUID, its version and variant right, or the nil or max UUID (RFC 9562)" ,(uuid-pattern))
     ("guid" "a GUID as Windows writes it, in braces or not: any 8-4-4-4-12 hex digits" ,(guid-pattern))
     ("md5" "an MD5 digest: 32 hex digits (RFC 1321)" ,(hex-run-pattern 32))
     ("sha1" "a SHA-1 digest: 40 hex digits (FIPS 180-4)" ,(hex-run-pattern 40))
     ("sha256" "a SHA-256 digest: 64 hex digits (FIPS 180-4)" ,(hex-run-pattern 64))
     ("sha512" "a SHA-512 digest: 128 hex digits (FIPS 180-4)" ,(hex-run-pattern 128))
     ("iban" "an IBAN, its country's length and its check digits right (ISO 13616)"
      ,(format nil "(?<![A-Za-z0-9])(?=[A-Za-z]{2}[0-9]{2}[ A-Za-z0-9][A-Za-z0-9]{4})(?<~aiban>)" *filter-mark*))
     ("bitcoin" "a Bitcoin address, 1..., 3... or bc1..., its checksum right (BIP 173, BIP 350)"
      ,(format nil "(?<![A-Za-z0-9])(?=[13][1-9A-HJ-NP-Za-km-z]{25}|[Bb][Cc]1[~a~:@(~a~)]{11})(?<~abitcoin>)"
               *bech32* *bech32* *filter-mark*))
     ("onion" "a Tor onion address, version 3 its check right, or version 2 (Tor's rend-spec)"
      ,(format nil "(?<![A-Za-z0-9-])(?=[A-Za-z2-7]{16})(?<~aonion>)" *filter-mark*))
     ("base32" "a block of Base32, capitals or small, padded right, over lines too (RFC 4648)"
      ,(format nil "(?<![A-Za-z0-9=])(?=[A-Z2-7]{16}|[a-z2-7]{16})(?<~abase32>)" *filter-mark*))
     ("base64" "a block of Base64, + and / or - and _, padded right, over lines too (RFC 4648)"
      ,(format nil "(?<![A-Za-z0-9+/_=-])(?=[A-Za-z0-9+/_-]{16})(?<~abase64>)" *filter-mark*))
     ("creditcard" "a payment card number of any network below, its check digit right (ISO/IEC 7812)"
      ,(card-pattern)))
   (loop for (name) in *card-numbers*
         collect (list name (format nil "a ~a card number, its check digit right" name) (card-pattern name)))
   (loop for (name format utc) in *date-families*
         collect (list name
                       (format nil "a date~a~:[~;, a bound with an offset compared in UTC~], ~
                                    [[:~a<2026-07-01:]] and the like for a range"

                               (ecase format
                                 (:iso " as RFC 3339 and ISO 8601 write it, 2026-06-30 or 2026-06-30T14:05:00+02:00")
                                 (:dmy ", day, month, year, 30.06.2026, 30/06/2026 or 30-06-2026 14:05")
                                 (:mdy ", month, day, year, 06/30/2026 or 6-30-2026 2:05 PM"))
                               utc name)
                       (date-pattern format utc nil nil))))
  "The named patterns, [[:ipv4:]] and the others, each (NAME WORDS PATTERN):
WORDS say what it finds, PATTERN finds it, as cl-ppcre reads it. Each stands
alone in its brackets, as a group: [[:ipv4:]]+ repeats it; it holds no group
that counts, so \\1 is still the first group you write.")

(sb-ext:defglobal **own-named-patterns** (make-hash-table :test 'equal :synchronized t)
  "The named patterns of your own (DEFINE-NAMED-PATTERN), by name: (WORDS
. PATTERN), PATTERN as you wrote it.")

(defvar *named-patterns-expanding* '()
  "The names of your own patterns being put in, innermost first, so that one
that names itself, by way of others or not, is refused.")

(defparameter *own-named-pattern-most* 65536
  "The most characters one of your own named patterns may stand for once the
named patterns in it are put in: one naming others over and over, each twice
\([[:b:]] for [[:a:]][[:a:]], [[:c:]] for [[:b:]][[:b:]], ...), doubles at each
step, and is refused before it can hold the editor up; so is one whose repeats
make it as large ((?:x){1000} counts a thousand x).")

(defvar *own-named-patterns-put-in* nil
  "While one of your own named patterns is put in (NAMED-PATTERN), a list
holding how many characters the own patterns within it have stood for so far.")

(defun named-pattern-p (name)
  "True when NAME names a pattern, [[:NAME:]]: one of *NAMED-PATTERNS* or your own."
  (or (assoc name *named-patterns* :test #'string=)
      (nth-value 1 (gethash name **own-named-patterns**))))

(defun own-named-pattern-p (name)
  "True when NAME names a pattern of your own (DEFINE-NAMED-PATTERN), whether the
init file defined it or it was saved."
  (nth-value 1 (gethash name **own-named-patterns**)))

(defun forget-own-named-pattern (name)
  "Take away the pattern of your own NAME names, so that [[:NAME:]] stands for
nothing, and forget the regexes compiled with it."
  (remhash name **own-named-patterns**)
  (forget-compiled-regexes))

(defun named-pattern (name)
  "The pattern [[:NAME:]] stands for, as cl-ppcre reads it, or NIL when NAME
names none: one of *NAMED-PATTERNS*, or your own (DEFINE-NAMED-PATTERN) read
as every pattern is (ENGINE-PATTERN). One of your own that stands for more
than *OWN-NAMED-PATTERN-MOST* characters, its repeats counted (REPEATED-SIZE)
and the own patterns in it as they are put in, is refused, so that putting it
in stops early."
  (or (third (assoc name *named-patterns* :test #'string=))
      (let ((own (gethash name **own-named-patterns**)))
        (when own
          (when (member name *named-patterns-expanding* :test #'string=)
            (refuse-pattern "invalid: [[:~a:]] names itself" name))
          (let* ((*named-patterns-expanding* (cons name *named-patterns-expanding*))
                 (*own-named-patterns-put-in* (or *own-named-patterns-put-in* (list 0)))
                 (text (engine-pattern (cdr own))))
            (when (> (incf (car *own-named-patterns-put-in*) (repeated-size text)) *own-named-pattern-most*)
              (refuse-pattern "invalid: [[:~a:]] stands for more than ~:d characters" name *own-named-pattern-most*))
            text)))))

(defun filter-tree (tree)
  "TREE, a cl-ppcre parse tree, each mark of a filter (*FILTER-MARK*) made
the filter it names (*PATTERN-FILTERS*)."
  (cond ((and (consp tree) (eq (first tree) :named-register) (stringp (second tree))
              (uiop:string-prefix-p *filter-mark* (second tree)))
         (let* ((name (subseq (second tree) (length *filter-mark*)))
                (dash (position #\- name))
                (maker (cdr (assoc (subseq name 0 dash) *pattern-filters* :test #'string=))))
           (list :filter (or (and maker (tuatara.runtime:ignoring-errors-reported ("making the filter ~a" name)
                                          (if dash (funcall maker (subseq name (1+ dash))) (funcall maker))))
                             (refuse-pattern "invalid: a group may not be named ~a…" *filter-mark*)))))
        ((consp tree) (mapcar #'filter-tree tree))
        (t tree)))
