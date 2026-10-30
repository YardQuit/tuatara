;;;; test/search-constructs.lisp --- Every construct of the search patterns, each with an example run as C-s and r r run it.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The examples are those of the regex handbook's reference, which is
;;;; written from these rows (.project/tools/regex-reference.lisp): what the
;;;; handbook says a pattern finds is what this test finds it finds.

(in-package #:tuatara.tests)

(define-test search-constructs
  :description "Every construct of the search patterns, an example of each: what a search finds and a replacement writes.")

(defun code-text (code)
  "The character of CODE as a string that can hold any character."
  (coerce (string (code-char code)) '(simple-array character (*))))

(defun joined-lines (&rest parts)
  "PARTS as lines of one text, a line end between each."
  (format nil "~{~a~^~%~}" parts))

;;; Every construct of the patterns, an example of each. A section is
;;; (TITLE INTRODUCTION ROWS); a row is
;;; (WRITTEN MEANS PATTERN TEXT FINDS &optional SHOWN): FINDS a list of the
;;; texts found, or (:refused WORDS); SHOWN how TEXT is written in the table
;;; when it holds what cannot be seen. A replacement row is
;;; (:replace WRITTEN MEANS PATTERN REPLACEMENT TEXT RESULT).

(defparameter *search-constructs*
  `(("Characters and escapes"
     "Each character is itself but for the special ones, =. ^ $ * + ? ( ) [ ] { } | \\=, which a backslash makes plain."
     (("abc" "the text abc" "kea" "a kea, keas" ("kea" "kea"))
      ("." "any one character but a line end" "k.a" "kea koa" ("kea" "koa"))
      ("\\." "a special character as itself: =\\$=, =\\(=, =\\\\=, …" "1\\.5" "1.5 1x5" ("1.5"))
      ("\\Q…\\E" "plain text inside a pattern, up to =\\E= or the end" "\\Q(a+b)\\E" "(a+b) aab" ("(a+b)"))
      ("\\t" "a tab" "a\\tb" ,(format nil "a~cb" #\Tab) (,(format nil "a~cb" #\Tab)) "a⇥b")
      ("\\n" "a line end (a line feed)" "a\\nb" ,(joined-lines "a" "b") (,(joined-lines "a" "b")) "a↵b")
      ("\\r" "a carriage return" "a\\r" ,(format nil "a~c" #\Return) (,(format nil "a~c" #\Return)) "a, carriage return")
      ("\\f" "a form feed" "\\f" ,(format nil "a~cb" #\Page) (,(code-text 12)) "a, form feed, b")
      ("\\e" "an escape, code 27" "\\e\\[" ,(format nil "~c[1m" (code-char 27)) (,(format nil "~c[" (code-char 27))) "ESC [1m")
      ("\\a" "a bell, code 7" "\\a" ,(format nil "x~cy" (code-char 7)) (,(code-text 7)) "x, bell, y")
      ("\\0" "the character of code 0" "\\0" ,(format nil "x~cy" (code-char 0)) (,(code-text 0)) "x, code 0, y")
      ("\\cX" "a control character: =\\cA= is code 1, =\\cZ= code 26" "\\cA" ,(format nil "x~cy" (code-char 1)) (,(code-text 1)) "x, code 1, y")
      ("\\xNN" "the character of hexadecimal code NN: =\\x41= is A; 80 to FF a raw octet, as the display shows it" "\\x41" "zA" ("A"))
      ("\\x{H…}" "the character of that code, any length" "\\x{2122}" "Tuatara™" ("™"))
      ("\\NNN" "the character of octal code NNN" "\\101" "zA" ("A"))))
    ("Sets: one character of several"
     "A set, in square brackets, matches one character. Inside it only =] \\ ^ -= are special, and =^= only first."
     (("[abc]" "one of a, b, c" "[ck]a" "ka ca da" ("ka" "ca"))
      ("[a-z]" "one in a range; several ranges may follow one another" "[a-c0-2]+" "abc012 d3" ("abc012"))
      ("[^abc]" "one character that is none of them" "[^aeiou ]+" "kia ora" ("k" "r"))
      ("[]a]" "a =]= first in the set is itself" "[]a]+" "x]a]x" ("]a]"))
      ("[a\\]]" "or written =\\]= anywhere" "[a\\]]+" "x]a]x" ("]a]"))
      ("[a-]" "a =-= first or last is itself" "[\\w-]+" "kia-ora koutou" ("kia-ora" "koutou"))
      ("[\\d.]" "escapes keep their meaning in a set" "[\\d.]+" "v1.25!" ("1.25"))
      ("[^[:alpha:]]" "a whole set turned round" "[^[:alpha:] ]+" "kea 42 tui 7" ("42" "7"))))
    ("Named sets"
     "=[:name:]= goes inside a set's brackets: =[[:alpha:]]= alone, =[[:alpha:]_-]= with others. They hold ASCII characters only, as in PCRE, but for =[:word:]=; =[:^name:]= is the set turned round, ASCII and all past it."
     (("[[:alnum:]]" "a letter A–Z, a–z or a digit" "[[:alnum:]]+" "tūī-42" ("t" "42"))
      ("[[:alpha:]]" "a letter A–Z or a–z" "[[:alpha:]]+" "tūī-42" ("t"))
      ("[[:blank:]]" "a space or a tab" "a[[:blank:]]+b" ,(format nil "a ~cb" #\Tab) (,(format nil "a ~cb" #\Tab)) "a, space, tab, b")
      ("[[:cntrl:]]" "a control character, code 0–31 or 127" "[[:cntrl:]]" ,(format nil "a~cb" #\Tab) (,(code-text 9)) "a⇥b")
      ("[[:digit:]]" "a digit 0–9" "[[:digit:]]+" "ra 42" ("42"))
      ("[[:graph:]]" "a printing character, not the space: =!= to =~=" "[[:graph:]]+" "kia ora!" ("kia" "ora!"))
      ("[[:lower:]]" "a small letter a–z; case then counts" "[[:lower:]]+" "Kia ORA" ("ia"))
      ("[[:print:]]" "a printing character or the space" "[[:print:]]+" ,(format nil "kia ora~ctui" #\Tab) ("kia ora" "tui") "kia ora⇥tui")
      ("[[:punct:]]" "punctuation and symbols of ASCII" "[[:punct:]]+" "kia ora, e hoa!" ("," "!"))
      ("[[:space:]]" "white space: space, tab, line end, carriage return, form feed, vertical tab" "a[[:space:]]+b" ,(joined-lines "a " "b") (,(joined-lines "a " "b")) "a, space, line end, b")
      ("[[:upper:]]" "a capital A–Z; case then counts" "[[:upper:]]+" "Kia ORA" ("K" "ORA"))
      ("[[:xdigit:]]" "a hexadecimal digit 0–9, A–F, a–f" "#[[:xdigit:]]{6}" "#1a2B3c #12345z" ("#1a2B3c"))
      ("[[:word:]]" "what =\\w= holds: letters and digits of every language, and =_=" "[[:word:]]+" "tūī_2 kea" ("tūī_2" "kea"))
      ("[[:ascii:]]" "any ASCII character, code 0–127" "[[:ascii:]]+" "tūī" ("t"))
      ("[[:^alnum:]]" "one not in =[:alnum:]=" "[[:^alnum:]]+" "a1, b2!" (", " "!"))
      ("[[:^alpha:]]" "one not in =[:alpha:]=, ī too" "[[:^alpha:]]+" "tūī 42" ("ūī 42"))
      ("[[:^blank:]]" "one not a space or a tab" "[[:^blank:]]+" "kia ora" ("kia" "ora"))
      ("[[:^cntrl:]]" "one not a control character" "[[:^cntrl:]]+" ,(format nil "kia~cora" #\Tab) ("kia" "ora") "kia⇥ora")
      ("[[:^digit:]]" "one not a digit" "[[:^digit:]]+" "ab12cd" ("ab" "cd"))
      ("[[:^graph:]]" "one not in =[:graph:]=: the space, a control character, all past ASCII" "[[:^graph:]]+" "kia ora" (" "))
      ("[[:^lower:]]" "one not a small letter a–z; case counts" "[[:^lower:]]+" "keaKEA" ("KEA"))
      ("[[:^print:]]" "one not in =[:print:]=" "[[:^print:]]" ,(format nil "kia~cora" #\Tab) (,(code-text 9)) "kia⇥ora")
      ("[[:^punct:]]" "one not in =[:punct:]=" "[[:^punct:]]+" "kia, ora!" ("kia" " ora"))
      ("[[:^space:]]" "one not white space" "[[:^space:]]+" "kia ora" ("kia" "ora"))
      ("[[:^upper:]]" "one not a capital A–Z; case counts" "[[:^upper:]]+" "KEAkea" ("kea"))
      ("[[:^xdigit:]]" "one not a hexadecimal digit" "[[:^xdigit:]]+" "ff zz 1a" (" zz "))
      ("[[:^word:]]" "one not in =[:word:]=" "[[:^word:]]+" "kia, ora" (", "))
      ("[[:^ascii:]]" "one past ASCII" "[[:^ascii:]]+" "tūī" ("ūī"))))
    ("Named patterns"
     "=[[:name:]]= alone in its brackets stands for a whole pattern, what a standard says the thing is, as a group: =[[:ipv4:]]+= repeats it, and it holds no group that counts, so =\\1= is still the first you write. Each is found standing alone, not inside a longer run of the same characters. A card number must pass the Luhn check of its last digit (ISO/IEC 7812); the numbers here are the networks' published test numbers."
     (("[[:ipv4:]]" "an IPv4 address: four numbers 0–255, no leading zero (RFC 3986)" "[[:ipv4:]]" "at 10.0.0.1. not 256.1.1.1, 1.2.3.4.5 or 01.2.3.4" ("10.0.0.1"))
      ("[[:ipv6:]]" "an IPv6 address, =::= for a run of zeros, an IPv4 tail too (RFC 3986, RFC 4291)" "[[:ipv6:]]" "fe80::1 and ::ffff:192.0.2.1, at 2001:db8::7334: not 12:30 or std::cout" ("fe80::1" "::ffff:192.0.2.1" "2001:db8::7334"))
      ("[[:ipaddress:]]" "an IPv4 or IPv6 address" "[[:ipaddress:]]" "10.1.1.1 or 2001:db8::7334" ("10.1.1.1" "2001:db8::7334"))
      ("[[:macaddress:]]" "a MAC address, by hyphens, colons or dots (IEEE 802)" "[[:macaddress:]]" "AC-DE-48-00-00-80, ac:de:48:00:00:80, acde.4800.0080" ("AC-DE-48-00-00-80" "ac:de:48:00:00:80" "acde.4800.0080"))
      ("[[:email:]]" "an e-mail address (the HTML standard's)" "[[:email:]]" "kea@example.co.nz, not @tui or x@-bad.nz" ("kea@example.co.nz"))
      ("[[:url:]]" "a web or FTP address, a closing dot or bracket left out but for a bracket it opened (RFC 3986)" "[[:url:]]" "see https://example.nz/a?b=1. or (ftp://f.org/x), https://en.wikipedia.org/wiki/Kea_(bird)." ("https://example.nz/a?b=1" "ftp://f.org/x" "https://en.wikipedia.org/wiki/Kea_(bird)"))
      ("[[:uuid:]]" "a UUID, version 1–8 and variant right, or the nil or max one (RFC 9562)" "[[:uuid:]]" "550e8400-e29b-41d4-a716-446655440000, not 550e8400-e29b-41d4-c716-446655440000" ("550e8400-e29b-41d4-a716-446655440000"))
      ("[[:guid:]]" "a GUID as Windows writes it, in braces or not" "[[:guid:]]" "{21EC2020-3AEA-1069-A2DD-08002B30309D}" ("{21EC2020-3AEA-1069-A2DD-08002B30309D}"))
      ("[[:md5:]]" "an MD5 digest, 32 hex digits (RFC 1321)" "[[:md5:]]" "d41d8cd98f00b204e9800998ecf8427e" ("d41d8cd98f00b204e9800998ecf8427e"))
      ("[[:sha1:]]" "a SHA-1 digest, 40 hex digits (FIPS 180-4)" "[[:sha1:]]" "da39a3ee5e6b4b0d3255bfef95601890afd80709" ("da39a3ee5e6b4b0d3255bfef95601890afd80709"))
      ("[[:sha256:]]" "a SHA-256 digest, 64 hex digits (FIPS 180-4)" "[[:sha256:]]" "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" ("e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
      ("[[:sha512:]]" "a SHA-512 digest, 128 hex digits (FIPS 180-4)" "[[:sha512:]]" "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e" ("cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e") "cf83e1357eefb8bd…")
      ("[[:iban:]]" "an IBAN: its country's length, together or in fours, the check digits right (ISO 13616)" "[[:iban:]]" "GB82 WEST 1234 5698 7654 32, DE89370400440532013000, not GB82 WEST 1234 5698 7654 33" ("GB82 WEST 1234 5698 7654 32" "DE89370400440532013000"))
      ("[[:bitcoin:]]" "a Bitcoin address, 1…, 3… or bc1…, its checksum right (BIP 173, BIP 350)" "[[:bitcoin:]]" "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa bc1qar0srrr7xfkvy5l643lydnw9re59gtzzwf5mdq, not 1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNb" ("1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa" "bc1qar0srrr7xfkvy5l643lydnw9re59gtzzwf5mdq"))
      ("[[:onion:]]" "a Tor onion address: version 3, 56 characters, its check right (SHA3-256); version 2, 16 characters, no check (Tor's rend-spec)" "[[:onion:]]" "duckduckgogg42xjoc72x3sjasowoarfbgcmvfimaftt6twagswzczad.onion and www.expyuzz4wqqyqhjn.onion, not duckduckgoag42xjoc72x3sjasowoarfbgcmvfimaftt6twagswzczad.onion" ("duckduckgogg42xjoc72x3sjasowoarfbgcmvfimaftt6twagswzczad.onion" "expyuzz4wqqyqhjn.onion") "duckduckgogg42…zczad.onion and www.expyuzz4wqqyqhjn.onion, not duckduckgoag42…zczad.onion")
      ("[[:base32:]]" "a block of Base32: 16 or more of A–Z and 2–7, all capitals or all small, a letter and a digit among them, = padding right, over lines of one length too (RFC 4648)" "[[:base32:]]" "NNUWCIDPOJQSA23POV2G65I= and JBSWY3DPEHPK3PXP, not OR2WC5DBOJQQ==== (too short) or ABCDEFGHIJKLMNOPQ" ("NNUWCIDPOJQSA23POV2G65I=" "JBSWY3DPEHPK3PXP"))
      ("[[:base64:]]" "a block of Base64: 32 or more of A–Z, a–z, 0–9 and + / (or - _ for addresses, never both), small and capital letters and a digit among them, = padding right, over lines of one length too (RFC 4648)" "[[:base64:]]" "S2lhIG9yYSBrb3V0b3Uga2F0b2EsIHRoZSBrZWEgaXMgYSBwYXJyb3Q= not S2lhIG9yYSBrb3V0b3Uga2F0b2EsIHRo+ZS_Br" ("S2lhIG9yYSBrb3V0b3Uga2F0b2EsIHRoZSBrZWEgaXMgYSBwYXJyb3Q=") "S2lhIG9yYSBrb3V0…YXJyb3Q= not S2lhIG9y…+ZS_Br")
      ("[[:creditcard:]]" "a card number of any network below, together or in groups by spaces or hyphens, its check digit right" "[[:creditcard:]]" "4111 1111 1111 1111, not 4111 1111 1111 1112" ("4111 1111 1111 1111"))
      ("[[:creditcard:]], exp" "a card number and text after it: what a filter reads comes before the text" "[[:creditcard:]], exp \\d\\d/\\d\\d" "card 5500-0000-0000-0004, exp 12/28" ("5500-0000-0000-0004, exp 12/28"))
      ("[[:visa:]]" "a Visa number: 4, 13, 16 or 19 digits" "[[:visa:]]" "4111111111111111, 4222222222222" ("4111111111111111" "4222222222222"))
      ("[[:mastercard:]]" "a Mastercard number: 51–55 or 2221–2720, 16 digits" "[[:mastercard:]]" "5500-0000-0000-0004, 2221000000000009" ("5500-0000-0000-0004" "2221000000000009"))
      ("[[:amex:]]" "an American Express number: 34 or 37, 15 digits, 4-6-5" "[[:amex:]]" "3782 822463 10005" ("3782 822463 10005"))
      ("[[:discover:]]" "a Discover number: 6011, 644–649, 65, 622126–622925; 16–19 digits" "[[:discover:]]" "6011111111111117" ("6011111111111117"))
      ("[[:diners:]]" "a Diners Club number: 36, 300–305, 3095, 38–39; 14–19 digits" "[[:diners:]]" "3056 930902 5904" ("3056 930902 5904"))
      ("[[:jcb:]]" "a JCB number: 3528–3589, 16–19 digits" "[[:jcb:]]" "3530111333300000" ("3530111333300000"))
      ("[[:unionpay:]]" "a UnionPay number: 62, 16–19 digits" "[[:unionpay:]]" "6200000000000005" ("6200000000000005"))
      ("[[:maestro:]]" "a Maestro number: 5018, 5020, 5038, 5893, 6304, 6759, 6761–6763; 12–19 digits" "[[:maestro:]]" "6759649826438453" ("6759649826438453"))
      ("[[:mir:]]" "a Mir number: 2200–2204, 16–19 digits" "[[:mir:]]" "2200000000000004" ("2200000000000004"))
      ("[[:date:]]" "a date as RFC 3339 and ISO 8601 write it, a day of the calendar, with its time and offset when written" "[[:date:]]" "2026-06-30, 2026-06-30T14:05:00+02:00, 2026-06-30 14:05:00,123, not 2026-02-30" ("2026-06-30" "2026-06-30T14:05:00+02:00" "2026-06-30 14:05:00,123"))
      ("[[:date<B:]]" "a date before B, compared as a date, not as text" "[[:date<2026-07-01:]]" "2025-12-31 2026-06-30T23:30 2026-07-01" ("2025-12-31" "2026-06-30T23:30"))
      ("[[:date<=B:]]" "a date up to the end of B: of its day, or of its minute for 14:05" "[[:date<=2026-06-30T14:05:]]" "2026-06-30T14:05:59 2026-06-30T14:06" ("2026-06-30T14:05:59"))
      ("[[:date>=B:]]" "a date from B on; =[[:date>B:]]= after the end of B" "[[:date>=2026-07-01:]]" "2026-06-30 2026-07-01T08:00" ("2026-07-01T08:00"))
      ("[[:date=B:]]" "a date within B" "[[:date=2026-06-30:]]" "2026-06-29 2026-06-30T23:59:59 2026-07-01" ("2026-06-30T23:59:59"))
      ("[[:date:A..B:]]" "a date from A to the end of B, either left out" "[[:date:2026-01-01..2026-06-30:]]" "2025-12-31 2026-03-15 2026-06-30T18:00 2026-07-01" ("2026-03-15" "2026-06-30T18:00"))
      ("[[:dateutc<B:]]" "a moment before B in UTC: an offset taken off; a time with none, and B with none, taken as UTC" "[[:dateutc<2026-07-01:]]" "2026-06-30T23:30:00-05:00 2026-06-30T23:30:00+02:00" ("2026-06-30T23:30:00+02:00"))
      ("[[:datedmy:]]" "a date written day, month, year, apart by . / or - (Norway, Denmark, Finland, New Zealand, Britain)" "[[:datedmy:]]" "30.06.2026 14.05, 30/06/2026, 1.7.2026, not 31.02.2026" ("30.06.2026 14.05" "30/06/2026" "1.7.2026"))
      ("[[:datemdy:]]" "a date written month, day, year, apart by / or - (the United States), AM and PM read" "[[:datemdy:]]" "06/30/2026 2:05 PM, 7-1-2026, not 13/01/2026 or 07.01.2026" ("06/30/2026 2:05 PM" "7-1-2026"))
      ("[[:datedmy<B:]]" "B written as the dates are, or as 2026-07-01; =datedmyutc= and =datemdyutc= compare in UTC" "[[:datedmy<01.07.2026:]]" "30.06.2026 01.07.2026" ("30.06.2026"))))
    ("Short sets"
     "Letters for sets used often, inside a set or out; the capital is the set turned round."
     (("\\d" "a digit, of any language" "\\d+" "ra 42" ("42"))
      ("\\D" "anything but a digit" "\\D+" "ab12cd" ("ab" "cd"))
      ("\\w" "a letter or digit of any language, or =_=" "\\w+" "tūī_2, kea" ("tūī_2" "kea"))
      ("\\W" "anything else" "\\W+" "kia, ora" (", "))
      ("\\s" "white space, line ends too" "a\\s+b" ,(joined-lines "a " "b") (,(joined-lines "a " "b")) "a, space, line end, b")
      ("\\S" "anything else" "\\S+" "kia ora" ("kia" "ora"))
      ("\\h" "white space within a line: space, tab, no-break space" "a\\h+b" ,(format nil "a ~cb" #\Tab) (,(format nil "a ~cb" #\Tab)) "a, space, tab, b")
      ("\\H" "anything else" "\\H+" ,(format nil "a b~cc" #\Tab) ("a" "b" "c") "a b⇥c")
      ("\\v" "white space between lines: line end, carriage return, vertical tab, form feed" "a\\vb" ,(joined-lines "a" "b") (,(joined-lines "a" "b")) "a↵b")
      ("\\V" "anything else" "\\V+" ,(joined-lines "kia ora" "tui") ("kia ora" "tui") "kia ora↵tui")
      ("\\R" "one line break of any kind, a carriage return and line feed as one; not in a set" "a\\Rb" ,(format nil "a~c~cb" #\Return #\Newline) (,(format nil "a~c~cb" #\Return #\Newline)) "a, CR LF, b")))
    ("Properties of Unicode"
     "=\\p{…}= is a set by what characters are, in every language; =\\P{…}= the set turned round. Case, spaces, =-= and =_= in a name do not matter, and =Is= may go before it."
     (("\\p{L}" "a letter" "\\p{L}+" "tūī 42" ("tūī"))
      ("\\pL" "the same, a one-letter name without braces" "\\pL+" "Ωμέγα 7" ("Ωμέγα"))
      ("\\P{L}" "anything but a letter" "\\P{L}+" "tūī 42" (" 42"))
      ("\\p{Lu}" "a capital letter; case then counts" "\\p{Lu}" "Āporo kiwi" ("Ā"))
      ("\\p{Ll}" "a small letter; case then counts" "\\p{Ll}+" "Āporo" ("poro"))
      ("\\p{Lt}" "a title-case letter" "\\p{Lt}" "ǅx" ("ǅ"))
      ("\\p{Lm}" "a modifier letter" "\\p{Lm}" "kʰa" ("ʰ"))
      ("\\p{Lo}" "a letter of no case" "\\p{Lo}+" "kea 漢字" ("漢字"))
      ("\\p{LC}" "a letter with a case: =Lu=, =Ll= or =Lt=; also =\\p{L&}=" "\\p{LC}+" "Kea 漢字" ("Kea"))
      ("\\p{M}" "a mark, as an accent made of its own" "\\p{M}" ,(format nil "e~c" (code-char #x301)) (,(code-text #x301)) "e and a combining acute")
      ("\\p{N}" "a number of any kind" "\\p{N}+" "x 42 ½ Ⅻ" ("42" "½" "Ⅻ"))
      ("\\p{Nd}" "a decimal digit" "\\p{Nd}+" "x 42 ½" ("42"))
      ("\\p{Nl}" "a number written with letters" "\\p{Nl}" "x Ⅻ" ("Ⅻ"))
      ("\\p{No}" "another number" "\\p{No}" "x ½" ("½"))
      ("\\p{P}" "punctuation" "\\p{P}+" "kia ora, e hoa!" ("," "!"))
      ("\\p{Pd}" "a dash" "\\p{Pd}" "1-2—3" ("-" "—"))
      ("\\p{Ps} \\p{Pe}" "an opening, a closing bracket" "\\p{Ps}\\w+\\p{Pe}" "(kea) [tui]" ("(kea)" "[tui]"))
      ("\\p{Pi} \\p{Pf}" "an opening, a closing quote mark" "\\p{Pi}\\w+\\p{Pf}" "“kia” ‘ora’" ("“kia”" "‘ora’"))
      ("\\p{Pc}" "punctuation that joins, as =_=" "\\p{Pc}" "a_b" ("_"))
      ("\\p{Po}" "other punctuation" "\\p{Po}+" "kia, ora!" ("," "!"))
      ("\\p{S}" "a symbol" "\\p{S}+" "$5 + €3" ("$" "+" "€"))
      ("\\p{Sm}" "a mathematical symbol" "\\p{Sm}" "2 + 2 = 4" ("+" "="))
      ("\\p{Sc}" "a currency symbol" "\\p{Sc}" "$5 €3" ("$" "€"))
      ("\\p{Sk}" "a modifier symbol" "\\p{Sk}" "a^b" ("^"))
      ("\\p{So}" "another symbol" "\\p{So}" "kia ora ☺" ("☺"))
      ("\\p{Z}" "a separator: a space, a line or paragraph separator" "\\p{Z}" "kia ora" (" "))
      ("\\p{Zs}" "a space" "a\\p{Zs}b" ,(format nil "a~cb" (code-char #xA0)) (,(format nil "a~cb" (code-char #xA0))) "a, no-break space, b")
      ("\\p{C}" "a control or format character" "\\p{C}" ,(format nil "a~cb" #\Tab) (,(code-text 9)) "a⇥b")
      ("\\p{Cc}" "a control character" "\\p{Cc}" ,(format nil "a~cb" #\Tab) (,(code-text 9)) "a⇥b")
      ("\\p{Cf}" "a format character, as the soft hyphen" "\\p{Cf}" ,(format nil "kia~cora" (code-char #xAD)) (,(code-text #xAD)) "kia, soft hyphen, ora")
      ("\\p{Letter}" "a category by its long name, =\\p{Uppercase_Letter}= for =Lu= and so on" "\\p{Uppercase_Letter}" "Kea" ("K"))
      ("\\p{Alphabetic}" "a letter, or a mark or number that is part of one" "\\p{Alphabetic}+" "tūī-42" ("tūī"))
      ("\\p{White_Space}" "white space of any kind" "\\p{White_Space}" "a b" (" "))
      ("\\p{ASCII}" "any ASCII character" "\\p{ASCII}+" "tūī" ("t"))
      ("\\p{Any}" "any character at all" "\\p{Any}" "ū" ("ū"))
      ("\\p{Latin}" "a script's letters: =Latin=, =Greek=, =Cyrillic=, =Arabic=, =Hebrew=, =Devanagari=, =Han=, =Hiragana=, =Katakana=, =Hangul=, =Thai= and every other" "\\p{Latin}+" "tūī Ωμέγα" ("tūī"))
      ("\\p{Greek}" "the Greek script" "\\p{Greek}+" "kea Ωμέγα" ("Ωμέγα"))
      ("\\p{Han}" "the Han script" "\\p{Han}+" "kanji 漢字" ("漢字"))
      ("\\p{IsGreek}" "=Is= before a name changes nothing" "\\p{IsGreek}+" "Ωμέγα x" ("Ωμέγα"))))
    ("How many"
     "A quantifier repeats what stands before it: a character, a set, a group. Each takes as many as it can, then gives back what the rest of the pattern needs; with =?= after it, as few as it can; with =+= after it, as many as it can, never given back."
     (("?" "none or one" "colou?r" "color colour" ("color" "colour"))
      ("*" "none or more" "ab*c" "ac abbbc" ("ac" "abbbc"))
      ("+" "one or more" "ab+c" "ac abbbc" ("abbbc"))
      ("{n}" "exactly n" "\\d{4}" "12 2026" ("2026"))
      ("{n,}" "n or more" "\\d{3,}" "12 123 12345" ("123" "12345"))
      ("{n,m}" "n to m" "\\d{2,3}" "1 12 1234" ("12" "123"))
      ("{,m}" "not a quantifier but the text itself: write ={0,m}=" "a{,2}" "aa a{,2}" ("a{,2}"))
      ("??" "none or one, as few as it can" "ab??" "ab" ("a"))
      ("*?" "as few as it can" "<.*?>" "<b>kia</b>" ("<b>" "</b>"))
      ("+?" "one or more, as few as it can" "a+?" "aaa" ("a" "a" "a"))
      ("{n,m}?" "n to m, as few as it can" "a{2,3}?" "aaaaa" ("aa" "aa"))
      ("?+" "possessive: none or one, never given back" "a?+a" "a aa" ("aa"))
      ("*+" "possessive: as many as it can, never given back" "\\d*+0" "100 10" ())
      ("++" "possessive, one or more" "a++b" "aaab aaa" ("aaab"))
      ("{n,m}+" "possessive, n to m" "a{1,3}+a" "aaa aaaa" ("aaaa"))))
    ("Where"
     "These match no character, only a place."
     (("^" "the start of a line" "^\\w+" ,(joined-lines "kia ora" "tui") ("kia" "tui") "kia ora↵tui")
      ("$" "the end of a line, before a carriage return that ends it" "\\w+$" ,(joined-lines "kia ora" "tui") ("ora" "tui") "kia ora↵tui")
      ("\\A" "the start of the whole text" "\\A\\w+" ,(joined-lines "kia ora" "tui") ("kia") "kia ora↵tui")
      ("\\z" "the end of the whole text" "\\w+\\z" ,(joined-lines "kia ora" "tui") ("tui") "kia ora↵tui")
      ("\\Z" "the end of the whole text, or before a last line end" "\\w+\\Z" ,(format nil "kia~%ora~%") ("ora") "kia↵ora↵")
      ("\\b" "the edge of a word" "\\bora\\b" "ora kōrero ora" ("ora" "ora"))
      ("\\B" "anywhere but a word's edge" "\\Bre\\B" "re kōrero" ("re"))))
    ("Either, and groups"
     ""
     (("a|b" "one or the other, the whole pattern on each side unless a group holds it" "kea|tui" "kea tui weka" ("kea" "tui"))
      ("(…)" "a group, remembered and numbered from 1 by its opening bracket" "(ka)+" "kakaka" ("kakaka"))
      ("(?:…)" "a group, not remembered" "(?:ha)+" "hahaha" ("hahaha"))
      ("(?<name>…)" "a group with a name, numbered with the others too" "(?<year>\\d{4})-\\d\\d" "2026-10" ("2026-10"))
      ("(?>…)" "atomic: what it matched is never given back" "(?>a+)b" "aab" ("aab"))
      ("(?#…)" "a note, matching nothing" "kea(?# the bird)s" "keas" ("keas"))))
    ("What a group matched, again"
     ""
     (("\\1 … \\9" "the text group 1 to 9 matched, again" "\\b(\\w+) \\1\\b" "the the bird" ("the the"))
      ("\\k<name>" "the text the named group matched, again" "(?<w>\\w+) \\k<w>" "kia kia ora" ("kia kia"))))
    ("Looking around"
     "These look at the text after or before a place without taking it in."
     (("(?=…)" "followed by" "\\w+(?=@)" "joe@example.org" ("joe"))
      ("(?!…)" "not followed by" "\\bkea(?!s)\\b" "kea keas" ("kea"))
      ("(?<=…)" "after; of a fixed length" "(?<=\\$)\\d+" "$15 and 20" ("15"))
      ("(?<!…)" "not after; of a fixed length" "(?<!\\$)\\b\\d+" "$15 and 20" ("20"))
      ("^(?!.*x)" "a line without x" "^(?!.*ERROR).+" ,(joined-lines "ok" "ERROR here" "fine") ("ok" "fine") "ok↵ERROR here↵fine")))
    ("Either, by a condition"
     ""
     (("(?(1)yes|no)" "=yes= when group 1 matched, else =no=" "(\\()?\\d+(?(1)\\))" "(12) 34" ("(12)" "34"))
      ("(?(?=…)yes|no)" "=yes= where the lookahead holds, else =no=" "(?(?=\\d)\\d+|[a-z]+)" "42 kea" ("42" "kea"))))
    ("Changing how it reads"
     "A flag from where it stands to the end of the group it is in; =(?i:…)= for the group alone."
     (("(?i)" "ignore case" "(?i)kea" "KEA Kea" ("KEA" "Kea"))
      ("(?-i)" "respect case, in a pattern of no capitals" "(?-i)kea" "KEA kea" ("kea"))
      ("(?<Name>…)" "a group's name, a character's code and a property's name are no capitals: case is still ignored" "(?<Bird>k\\x{45}a)" "KEA kea" ("KEA" "kea"))
      ("(?i:…)" "ignore case within the group alone" "(?i:k)EA" "kEA KEA kea" ("kEA" "KEA"))
      ("(?s)" "=.= matches line ends too" "(?s)kia.ora" ,(joined-lines "kia" "ora") (,(joined-lines "kia" "ora")) "kia↵ora")
      ("(?x)" "spaces in the pattern ignored, =#= to its end a note" "(?x) \\d{4} - \\d\\d   # year and month" "2026-10" ("2026-10"))
      ("(?m)" "changes nothing: =^= and =$= are always a line's start and end" "(?m)^tui" ,(joined-lines "kea" "tui") ("tui") "kea↵tui")))
    ("Refused, saying why"
     "Each of these PCRE has; the search keeps its question open and says why in its prompt."
     (("\\K" "write a lookbehind instead" "user=\\Kkiri" "" (:refused "not supported: \\K"))
      ("\\G" "where the last match ended" "\\Gx" "" (:refused "not supported: \\G"))
      ("\\X" "a whole cluster of characters" "\\X" "" (:refused "not supported: \\X"))
      ("\\C" "one byte" "\\C" "" (:refused "not supported: \\C"))
      ("\\N" "anything but a line end: write =.=" "\\N" "" (:refused "not supported: \\N"))
      ("\\g1" "a group again: write =\\1=" "(a)\\g1" "" (:refused "not supported: \\g"))
      ("\\k'name'" "write =\\k<name>=" "(?<n>a)\\k'n'" "" (:refused "not supported: \\k without <name>"))
      ("[[:date<B:]]" "B no date" "[[:date<2026-13-01:]]" "" (:refused "invalid: [[:date…:]] has no date in \"2026-13-01\": write one as 2026-07-01 or 2026-07-01T12:00"))
      ("[[:date<B:]]" "B with an offset: write =dateutc=" "[[:date<2026-07-01T00:00Z:]]" "" (:refused "invalid: [[:date…:]] compares dates as they are written, with no offset: [[:dateutc…:]] compares the moments in UTC"))
      ("[[:date:A..B:]]" "B before A" "[[:date:2026-07-01..2026-06-01:]]" "" (:refused "invalid: [[:date:2026-07-01..2026-06-01:]] ends before it begins"))
      ("(?P<name>…)" "write =(?<name>…)=" "(?P<n>a)" "" (:refused "invalid: (?P is no kind of group"))
      ("(?R) (?1)" "a pattern within itself" "(?R)" "" (:refused "invalid: (?R is no kind of group"))
      ("(?\\vert{}…)" "groups that share numbers" "(?|(a)|(b))" "" (:refused "invalid: (?| is no kind of group"))
      ("(?(<name>)…)" "a condition on a named group: write its number" "(?<x>a)?(?(<x>)b)" "" (:refused "invalid: a condition is a lookaround or a group's number"))
      ("(?U)" "every quantifier lazy" "(?U)a+" "" (:refused "invalid: (?U is no kind of group"))
      ("(*VERB)" "PCRE's verbs, (*UTF) and the rest" "(*UTF)a" "" (:refused "invalid: nothing for * to repeat"))
      ("[:alpha:]" "a named set outside a set" "[:alpha:]" "" (:refused "invalid: [:alpha:] goes inside a set: [[:alpha:]]"))
      ("[[:nope:]]" "a name no set has" "[[:nope:]]" "" (:refused "invalid: no POSIX class [:nope:]"))
      ("[[:ipv4:]x]" "a named pattern with others in its brackets" "[[:ipv4:]x]" "" (:refused "invalid: [:ipv4:] stands alone in its brackets: [[:ipv4:]]"))
      ("[[:ipv4:]" "a named pattern not closed yet, as it is typed" "[[:ipv4:]" "" (:refused "incomplete"))
      ("[[:^ipv4:]]" "a named pattern turned round" "[[:^ipv4:]]" "" (:refused "invalid: a named pattern is not turned round: [[:^ipv4:]]"))
      ("\\p{Nope}" "a property no character has" "\\p{Nope}" "" (:refused "invalid: no character property \\p{Nope}"))))
    ("In a replacement"
     "What follows the =/= of =r r= or =r q= is the text written for each match, not a pattern. Without =\\u=, =\\U=, =\\l= or =\\L= in it, a pattern of no capitals writes it in the case of what it replaces."
     ((:replace "&" "the whole match" "%\\d+" "[&]" "rooms 12 and 7" "rooms [12] and [7]")
      (:replace "\\0" "the same" "%kea" "\\0s" "a kea" "a keas")
      (:replace "\\1 … \\9" "what group 1 to 9 matched; a named group by its number" "%(\\w+), (\\w+)" "\\2 \\1" "Smith, John" "John Smith")
      (:replace "\\u" "the next letter a capital" "%\\bk" "\\u&" "kia kaha" "Kia Kaha")
      (:replace "\\U … \\E" "capitals up to =\\E= or the end" "%(\\w+)@" "\\U\\1\\E@" "joe@example" "JOE@example")
      (:replace "\\l" "the next letter small" "%\\bK" "\\l&" "Kia Kaha" "kia kaha")
      (:replace "\\L … \\E" "small letters up to =\\E= or the end" "%\\b(\\w)(\\w*)" "\\u\\1\\L\\2" "kia ORA" "Kia Ora")
      (:replace "\\n \\t \\r" "a line end, a tab, a carriage return" "%, " "\\n" "kea, tui" ,(joined-lines "kea" "tui"))
      (:replace "\\& \\/ \\\\" "=&=, =/=, =\\= themselves" "%and" "\\&" "kea and tui" "kea & tui")
      (:replace "(the case)" "a match all capitals writes capitals; one whose words each start with a capital, capitals there; any other as it is written" "%fox" "wolf" "FOx FoX fOX Fox FOX foX" "Wolf Wolf wolf Wolf WOLF wolf"))))
  "Every construct of the search patterns, by sections (TITLE INTRODUCTION
ROWS), each row an example: a search's (WRITTEN MEANS PATTERN TEXT FINDS
SHOWN), or a replacement's (:REPLACE WRITTEN MEANS PATTERN REPLACEMENT TEXT
RESULT).")

;;; Running the rows

(defun construct-finds (pattern text)
  "What a search for PATTERN finds in TEXT, as C-s finds it: the texts of the
matches in order, or (:refused WORDS)."
  (multiple-value-bind (matcher problem) (tuatara.defaults::make-pattern-matcher pattern :regex t)
    (if (null matcher)
        (list :refused problem)
        (let ((rope (tuatara.text:string->rope text)) (out '()))
          (tuatara.defaults::stretch-matches
           (lambda (s e m g) (declare (ignore m g)) (push (tuatara.text:rope-string rope :start s :end e) out) nil)
           rope matcher 0 (tuatara.text:rope-bytes rope) nil nil nil)
          (nreverse out)))))

(defun construct-finds-in-string (pattern text)
  "What the scanner every other question that takes a pattern uses
\(TUATARA.SYNTAX:COMPILE-PATTERN: filters, keep and flush lines, sorting,
links to notes, dynamic blocks) finds of PATTERN in TEXT, a string: the texts
of the matches in order, or (:refused WORDS)."
  (handler-case
      (let ((scanner (tuatara.syntax:compile-pattern pattern)) (out '()) (start 0))
        (loop
          (multiple-value-bind (s e) (cl-ppcre:scan scanner text :start start)
            (unless s (return (nreverse out)))
            (push (subseq text s e) out)
            (setf start (if (= s e) (1+ e) e))
            (when (> start (length text)) (return (nreverse out))))))
    (error (e) (list :refused (tuatara.defaults::regex-problem e)))))

(defun construct-replaced (pattern replacement text)
  "TEXT once r r has replaced PATTERN by REPLACEMENT in it, typed at its
question as PATTERN/REPLACEMENT."
  (session-text (nth-value 1 (run-keys (format nil "r r ~a RET" (key-string (format nil "~a/~a" pattern replacement)))
                                       :text text :width 200))))

(define-test (search-constructs every-row)
  ;; each example as the handbook gives it: the search finds just these, the replacement writes just this
  (dolist (section *search-constructs*)
    (dolist (row (third section))
      (if (eq (first row) :replace)
          (destructuring-bind (written means pattern replacement text result) (rest row)
            (declare (ignore means))
            (is string= result (construct-replaced pattern replacement text) "~a: ~a on ~s" written pattern text))
          (destructuring-bind (written means pattern text finds &optional shown) row
            (declare (ignore means shown))
            (is equal finds (construct-finds pattern text) "~a: ~a on ~s" written pattern text)
            (is equal finds (construct-finds-in-string pattern text) "~a: ~a on ~s, in a string" written pattern text))))))

(define-test (search-constructs encoded-blocks-and-onions)
  ;; SHA3-256 as FIPS 202 gives it
  (is string= "a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a"
      (tuatara.text:octets-hex (tuatara.syntax::sha3-256 (tuatara.text:string-octets ""))))
  (is string= "3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532"
      (tuatara.text:octets-hex (tuatara.syntax::sha3-256 (tuatara.text:string-octets "abc"))))
  ;; SHA-256 as FIPS 180-4 gives it, the digest written where it is asked
  (let ((digest (make-array 32 :element-type '(unsigned-byte 8))))
    (is string= "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
        (tuatara.text:octets-hex (tuatara.syntax::sha256 (tuatara.text:string-octets "abc") 3 digest))))
  (flet ((both (pattern text)
           (let ((finds (construct-finds pattern text)))
             (is equal finds (construct-finds-in-string pattern text) "~a on ~s, in a string" pattern text)
             finds)))
    ;; onion: capitals too, a port or path after it, not inside a longer label
    (is equal '("2GZYXA5IHM7NSGGFXNU52RCK2VV4RVMDLKIU3ZZUI5DU4XYCLEN53WID.ONION")
        (both "[[:onion:]]" "http://2GZYXA5IHM7NSGGFXNU52RCK2VV4RVMDLKIU3ZZUI5DU4XYCLEN53WID.ONION:80/"))
    (is equal '() (both "[[:onion:]]" "x-expyuzz4wqqyqhjn.onion expyuzz4wqqyqhjn.onionx"))
    ;; a block wrapped over lines of one length is one match, its last line shorter
    (let ((block (format nil "S2lhIG9yYSBrb3V0b3Uga2F0b2Es~%IHRoZSBrZWEgaXMgYSBwYXJyb3Q=~%")))
      (is equal (list (string-right-trim '(#\Newline) block)) (both "[[:base64:]]" block)))
    (is equal (list (format nil "NNUWCIDPOJQSA23P~%OV2G65I=")) (both "[[:base32:]]" (format nil "NNUWCIDPOJQSA23P~%OV2G65I=")))
    ;; the last line longer than the first: the first alone, the rest too long to be a block's last line
    (is equal '("NNUWCIDPOJQSA23P") (both "[[:base32:]]" (format nil "NNUWCIDPOJQSA23P~%OV2G65IKIAKEATUI2345=")))
    ;; a line that only begins like the block's next one: the block ends before it
    (is equal '("JBSWY3DPEHPK3PXP") (both "[[:base32:]]" (format nil "secret: JBSWY3DPEHPK3PXP~%Attachment:")))
    ;; a block of 19 KB wrapped at 64, found whole however the search cuts the text
    (let* ((line "S2lhIG9yYSBrb3V0b3Uga2F0b2EsIHRoZSBrZWEgaXMgYSBwYXJyb3QgS2lhIG9y")
           (block (format nil "~{~a~^~%~}=" (loop repeat 300 collect line)))
           (text (format nil "before~%~a~%after~%" (subseq block 0 (1- (length block)))))
           (rope (tuatara.text:string->rope text))
           (matcher (tuatara.defaults::make-pattern-matcher "[[:base64:]]" :regex t))
           (start (length (format nil "before~%"))))
      (dolist (sizes (list (list tuatara.syntax:*serial-window* tuatara.syntax:*parallel-piece*) '(700 900)))
        (let ((tuatara.syntax:*serial-window* (first sizes)) (tuatara.syntax:*parallel-piece* (second sizes)))
          (is equal (list (list start (+ start (1- (length block)))))
              (mapcar (lambda (m) (subseq m 0 2))
                      (tuatara.defaults::line-matches rope matcher t 0 (tuatara.text:rope-bytes rope)))
              "a block of 19 KB at ~a" sizes)
          (is eql start (tuatara.syntax:regex-search-forward rope "[[:base64:]]" :start 0 :threads 3))))
      ;; longer than a window may grow to: read on past it, found whole all the same
      (let ((tuatara.syntax::*regex-longest-match* 4096))
        (dolist (sizes (list (list tuatara.syntax:*serial-window* tuatara.syntax:*parallel-piece*) '(700 900)))
          (let ((tuatara.syntax:*serial-window* (first sizes)) (tuatara.syntax:*parallel-piece* (second sizes)))
            (is equal (list (list start (+ start (1- (length block)))))
                (mapcar (lambda (m) (subseq m 0 2))
                        (tuatara.defaults::line-matches rope matcher t 0 (tuatara.text:rope-bytes rope)))
                "a block of 19 KB, windows of 4 KB at most, at ~a" sizes)
            (is equal (list start (+ start (1- (length block))) (subseq block 0 (1- (length block))))
                (subseq (multiple-value-list (tuatara.syntax:regex-search-forward rope "[[:base64:]]" :start 0 :threads 3)) 0 3)
                "a block of 19 KB, windows of 4 KB at most, searched for at ~a" sizes))))
      ;; a line that goes on the block before it begins none: searched for from it, the block after
      (let ((second-line (+ start (length line) 1)))
        (is eq nil (tuatara.syntax:regex-search-forward rope "[[:base64:]]" :start second-line :threads 3))
        (let ((two (tuatara.text:string->rope (format nil "~a~%~%~a~%" (subseq block 0 (1- (length block))) block))))
          (is eql (+ (length block) 1)
              (tuatara.syntax:regex-search-forward two "[[:base64:]]" :start (1+ (length line)) :threads 3)))))
    ;; Base64 for addresses
    (is equal '("yMnKy8zNzs_Q0dLT1NXW19jZ2tvc3d7f4OHi4-Tl5ufo6err7O3u7w==")
        (both "[[:base64:]]" "yMnKy8zNzs_Q0dLT1NXW19jZ2tvc3d7f4OHi4-Tl5ufo6err7O3u7w=="))
    ;; padding wrong: no block
    (is equal '() (both "[[:base64:]]" "S2lhIG9yYSBrb3V0b3Uga2F0b2EsIHRoZSBrZWEgaXMgYSBwYXJyb3==="))
    (is equal '() (both "[[:base32:]]" "NNUWCIDPOJQSA23POV2G65I=="))
    ;; all letters of one case, or no digit: not taken for Base32 or Base64
    (is equal '() (both "[[:base32:]]" "KIWIKEAKAKAPOTUIWEKA"))
    (is equal '() (both "[[:base64:]]" "TheKeaIsAParrotOfTheSouthernAlpsOfAotearoa"))))

(define-test (search-constructs named-patterns-make-no-garbage)
  :time-limit 900
  ;; a named pattern tried at every place of a long text makes no garbage: its checks
  ;; read into buffers on the stack, and repetitions are written out, never counted
  (let ((line (format nil "id 99887766, 3.14 at 192.168.1.1 by fe80::1 on ac:de:48:00:00:80 ~
                           paid 4111 1111 1111 1111 to GB82 WEST 1234 5698 7654 32, wallet ~
                           1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa bc1qar0srrr7xfkvy5l643lydnw9re59gtzzwf5mdq ~
                           duckduckgogg42xjoc72x3sjasowoarfbgcmvfimaftt6twagswzczad.onion ~
                           S2lhIG9yYSBrb3V0b3Uga2F0b2EsIHRoZSBrZWEgaXMgYSBwYXJyb3Q= JBSWY3DPEHPK3PXP ~
                           2026-06-30T14:05:00+02:00 30.06.2026 06/30/2026 kea@example.co.nz https://example.nz/a ~
                           550e8400-e29b-41d4-a716-446655440000 d41d8cd98f00b204e9800998ecf8427e~%")))
    (let* ((text (with-output-to-string (o) (dotimes (i 5000) (write-string line o))))
           (rope (tuatara.text:string->rope text)))
      (dolist (pattern '("[[:creditcard:]]" "[[:visa:]]" "[[:iban:]]" "[[:bitcoin:]]" "[[:onion:]]" "[[:base32:]]"
                         "[[:base64:]]" "[[:ipv4:]]" "[[:ipv6:]]" "[[:ipaddress:]]" "[[:macaddress:]]" "[[:email:]]"
                         "[[:url:]]" "[[:uuid:]]" "[[:md5:]]" "[[:date<2026-07-01:]]" "[[:datedmy:]]" "[[:datemdy:]]"))
        (let ((scanner (tuatara.syntax:compile-pattern pattern)) (found 0))
          (flet ((in-string ()
                   (cl-ppcre:do-scans (s e rs re scanner text) (incf found)))
                 (in-rope ()
                   (tuatara.syntax:map-regex-matches (lambda (s e m g) (declare (ignore s e m g)) (incf found) nil)
                                                     rope scanner 0 (tuatara.text:rope-bytes rope) :texts nil)))
            (in-string)
            (true (plusp found) "~a finds something" pattern)
            ;; the least of a few readings, other threads' work left out
            (dolist (way (list #'in-string #'in-rope))
              (let ((consed (loop repeat 3
                                  minimize (let ((before (sb-ext:get-bytes-consed)))
                                             (funcall way)
                                             (- (sb-ext:get-bytes-consed) before)))))
                (true (< consed (* 64 1024)) "~a over 5,000 lines consed ~:d octets" pattern consed)))))))))

(define-test (search-constructs date-patterns)
  ;; dates read and compared as dates: their edges, their times and offsets, in a search of
  ;; the text and in a string alike
  (flet ((both (pattern text)
           (let ((finds (construct-finds pattern text)))
             (is equal finds (construct-finds-in-string pattern text) "~a on ~s, in a string" pattern text)
             finds)))
    ;; no digit on either side, a T that promises a time it does not keep, a day not in the calendar
    (is equal '() (both "[[:date:]]" "12026-01-01 2026-01-011 2026-06-30T25:00 2026-06-31 2026-6-30"))
    ;; a space and no time: the date alone; small t and z as RFC 3339 allows; a leap second
    (is equal '("2026-06-30" "2026-06-30t10:00:00z" "2016-12-31T23:59:60Z") (both "[[:date:]]" "2026-06-30 at 2026-06-30t10:00:00z 2016-12-31T23:59:60Z"))
    ;; offsets as logs write them, a space before or none, and UTC or GMT
    (is equal '("2026-06-30 09:00:00 +0200" "2026-06-30T09:00+02" "2026-06-30 10:00 UTC" "2026-06-30 10:00:00.123456789 GMT")
        (both "[[:date:]]" "2026-06-30 09:00:00 +0200, 2026-06-30T09:00+02, 2026-06-30 10:00 UTC, 2026-06-30 10:00:00.123456789 GMT"))
    ;; as written: the offset is not taken off; in UTC it is
    (is equal '("2026-06-30T23:30:00-05:00") (both "[[:date<2026-07-01:]]" "2026-06-30T23:30:00-05:00"))
    (is equal '() (both "[[:dateutc<2026-07-01:]]" "2026-06-30T23:30:00-05:00"))
    (is equal '("2026-06-30T23:30:00-05:00") (both "[[:dateutc>2026-07-01T04:29Z:]]" "2026-06-30T23:30:00-05:00"))
    (is equal '() (both "[[:dateutc>2026-07-01T04:30Z:]]" "2026-06-30T23:30:00-05:00"))
    ;; a bound's last figure says how much of it counts: a minute, a second, a tenth
    (is equal '("2026-06-30T14:05:00.9") (both "[[:date<=2026-06-30T14:05:00:]]" "2026-06-30T14:05:00.9 2026-06-30T14:05:01"))
    (is equal '("2026-06-30T14:05:00.1") (both "[[:date<=2026-06-30T14:05:00.1:]]" "2026-06-30T14:05:00.1 2026-06-30T14:05:00.2"))
    ;; the day and month apart by the same mark both times; a time apart by . in day, month, year
    (is equal '("1-7-2026" "30.06.2026 14.05.30,5") (both "[[:datedmy:]]" "30.06/2026, 1-7-2026, 30.06.2026 14.05.30,5"))
    ;; 12 AM is midnight, 12 PM noon; an hour past 12 with AM or PM is no time
    (is equal '("06/30/2026 12:30 AM") (both "[[:datemdy<06/30/2026 01:00:]]" "06/30/2026 12:30 AM, 06/30/2026 12:30 PM"))
    (is equal '("06/30/2026, 11:00PM") (both "[[:datemdy>06/30/2026 22:59:]]" "06/30/2026, 11:00PM, 06/30/2026 9:00 pm"))
    (is equal '() (both "[[:datemdy:]]" "06/30/2026 13:00 PM"))
    ;; repeated and inside one of your own named patterns
    (unwind-protect
         (progn
           (tuatara-user:define-named-pattern "early" "a line of the first half of 2026" "^[[:date:2026-01-01..2026-06-30:]] ")
           (is equal '("2026-03-01 ") (both "[[:early:]]" (format nil "2026-03-01 a~%2026-08-01 b"))))
      (remhash "early" tuatara.syntax::**own-named-patterns**))
    ;; a date-time cut by the search's windows is found whole
    (let* ((text (with-output-to-string (o)
                   (dotimes (i 400) (format o "line ~d 2026-06-30T14:05:00.123+02:00 x~%" i))))
           (rope (tuatara.text:string->rope text))
           (matcher (tuatara.defaults::make-pattern-matcher "[[:dateutc<2026-06-30T12:06Z:]]" :regex t)))
      (dolist (sizes '((700 900) (64 128)))
        (let ((tuatara.syntax:*serial-window* (first sizes)) (tuatara.syntax:*parallel-piece* (second sizes)))
          (let ((matches (tuatara.defaults::line-matches rope matcher t 0 (tuatara.text:rope-bytes rope))))
            (is = 400 (length matches) "every date at ~a" sizes)
            (true (every (lambda (m) (= 29 (- (second m) (first m)))) matches) "each whole at ~a" sizes)))))))

(define-test (search-constructs own-named-patterns)
  ;; patterns of your own, as an init file defines them: found as Tuatara's are, in a
  ;; search of the text and in a string alike
  (flet ((both (pattern text)
           (let ((finds (construct-finds pattern text)))
             (is equal finds (construct-finds-in-string pattern text) "~a on ~s, in a string" pattern text)
             finds))
         (refused-p (name pattern)
           (handler-case (progn (tuatara-user:define-named-pattern name "test" pattern) nil)
             (error () t))))
    (unwind-protect
         (progn
           (tuatara-user:define-named-pattern "kea9" "kea and a digit" "kea[[:digit:]]")
           (is equal '("kea1kea2") (both "[[:kea9:]]+" "a kea1kea2 keax"))
           ;; Tuatara's named patterns inside, the card numbers' Luhn check too
           (tuatara-user:define-named-pattern "paid" "a card paid with" "paid [[:creditcard:]]")
           (is equal '("paid 4111 1111 1111 1111") (both "[[:paid:]]" "paid 4111 1111 1111 1111, paid 4111 1111 1111 1112"))
           ;; it counts no group: \1 is the first written
           (is equal '("x kea3 x") (both "(x) [[:kea9:]] \\1" "x kea3 x"))
           ;; defined again: the new one
           (tuatara-user:define-named-pattern "kea9" "kea and two digits" "kea[[:digit:]]{2}")
           (is equal '("kea12") (both "[[:kea9:]]" "kea1 kea12"))
           ;; refused, and what was there before kept
           (true (refused-p "Kea" "kea"))
           (true (refused-p "ipv4" "kea"))
           (true (refused-p "alpha" "kea"))
           (true (refused-p "grouped" "(kea)"))
           (true (refused-p "broken" "kea("))
           (false (tuatara.syntax::named-pattern-p "broken"))
           (tuatara-user:define-named-pattern "loopa" "a" "a")
           (tuatara-user:define-named-pattern "loopb" "b" "[[:loopa:]]b")
           (true (refused-p "loopa" "[[:loopb:]]"))
           (is equal '("ab") (both "[[:loopb:]]" "xab"))
           ;; the starter init file's example, as written there with its ;; taken off:
           ;; one pattern of yours using another
           (let ((text (uiop:read-file-string (asdf:system-relative-pathname :tuatara "data/init.lisp"))))
             (dolist (form (loop for start = (search ";; (define-named-pattern" text) then (search ";; (define-named-pattern" text :start2 (1+ start))
                                 while start
                                 collect (let ((end (search ")
" text :start2 start)))
                                           (cl-ppcre:regex-replace-all "(?m)^ *;; ?" (subseq text start (1+ end)) ""))))
               (let ((*package* (find-package :tuatara-user))) (eval (read-from-string form)))))
           (is equal '("021 123 4567" "+64 21 123 4567" "0064 9 123 4567" "+64 (0)21 123 4567" "(09) 123 4567" "0800 123 456")
               (both "[[:nzphone:]]" "021 123 4567, +64 21 123 4567, 0064 9 123 4567, +64 (0)21 123 4567, (09) 123 4567, 0800 123 456, not +61 2 1234 5678 or 2026-10-12")))
      (dolist (name '("kea9" "paid" "loopa" "loopb" "nzlocal" "nzphone"))
        (remhash name tuatara.syntax::**own-named-patterns**)))))
