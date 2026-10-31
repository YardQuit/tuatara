;;;; test/search-filters.lisp --- Lists narrowed by search filters, stacked, listed and taken away.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test search-filters
  :description "Search filters: any list narrowed as a pattern is typed (C-x / s), the pattern kept on RET and
the next one narrowing what it left, C-g letting only the one typed go; headings kept over the entries shown;
more text a list gives of its entries matched in the background; the filters listed, one taken away from the
middle and all taken away; C-s narrowing the lists that ask for it; the filters' matches lit elsewhere.")

(defparameter *filter-test-entries*
  '((:heading . "Fruit") "apple" "banana" "cherry" (:heading . "Vegetables") "carrot" "leek")
  "The entries of the list the search filters' tests narrow: two headings, each over its foods.")

(defun filter-test-list (&key text searches)
  "Make the current buffer the list of *FILTER-TEST-ENTRIES*, a line each
under a title line, narrowed as SET-SEARCH-FILTERING says with TEXT and SEARCHES."
  (let ((buffer (tuatara.command:current-buffer)))
    (tuatara.modes:set-major-mode buffer 'tuatara.defaults::special-mode)
    (tuatara.defaults::render-pieces
     buffer (cons (format nil "Foods~%")
                  (mapcar (lambda (entry) (format nil "~a~%" (if (consp entry) (cdr entry) entry))) *filter-test-entries*))
     :entries *filter-test-entries* :first-line 1 :point :start)
    (tuatara:set-search-filtering buffer :heading #'consp :text text :searches searches :nouns "foods")))

(defvar *seen-lines* nil
  "The lines the test list showed where a session's keys said :LOOK.")

(defun filtered-session (keys &rest options)
  "Run KEYS in a session whose buffer is the test list (FILTER-TEST-LIST,
given OPTIONS), :LOOK among them keeping the lines it shows there in
*SEEN-LINES*, as a search is still typed; return the frontend, the editor and
the list."
  (let ((list nil))
    (setf *seen-lines* nil)
    (multiple-value-bind (f editor)
        (run-keys (list* (lambda () (setf list (apply #'filter-test-list options)))
                         (substitute (lambda () (setf *seen-lines* (list (shown-lines list) (tuatara.buffer:buffer-string list))))
                                     :look (if (listp keys) keys (list keys))))
                  :width 100 :height 20)
      (values f editor list))))

(defun shown-lines (buffer)
  "The lines BUFFER shows, the line on its filters dropped."
  (remove-if (lambda (line) (or (zerop (length line)) (search "iltered by" line)))
             (uiop:split-string (tuatara.buffer:buffer-string buffer) :separator '(#\Newline))))

(defun filter-strings (buffer)
  "The patterns of the filters kept on BUFFER."
  (mapcar #'tuatara:search-filter-string (tuatara:search-filters buffer)))

(define-test (search-filters narrowed-as-typed)
  ;; typed, the list narrows; headings stay over the entries shown, a heading with none goes
  (multiple-value-bind (f editor list) (filtered-session '("C-x / s a n" :look))
    (declare (ignore editor list))
    (is equal '("Foods" "Fruit" "banana") (first *seen-lines*))
    (true (tuatara.frontend.fake:fake-frame-showing f "1/5    Regex filter: an"))
    (true (search "Filtered by an: 1 of 5 foods" (second *seen-lines*))))
  ;; RET keeps it as a filter; the summary says how to list and take them away
  (multiple-value-bind (f editor list) (filtered-session "C-x / s a n RET")
    (declare (ignore f editor))
    (is equal '("an") (filter-strings list))
    (true (search "C-x / l lists the filters, C-x / r takes them away" (tuatara.buffer:buffer-string list)))
    ;; the entry on each line is the one shown there
    (is equal "banana" (tuatara.defaults::list-entry-at list (tuatara.buffer:buffer-goto-line list 3))))
  ;; C-g lets the one typed go: the whole list again
  (multiple-value-bind (f editor list) (filtered-session "C-x / s a n C-g")
    (declare (ignore f editor))
    (false (filter-strings list))
    (is equal '("Foods" "Fruit" "apple" "banana" "cherry" "Vegetables" "carrot" "leek") (shown-lines list)))
  ;; case ignored without capitals, kept with them; DEL widens again; literal text with M-r
  (filtered-session '("C-x / s A" :look))
  (is equal '("Foods") (first *seen-lines*))
  (filtered-session '("C-x / s l e e DEL DEL" :look))
  (is equal '("Foods" "Fruit" "apple" "Vegetables" "leek") (first *seen-lines*))
  ;; DEL with point further down than the narrowed list reaches: narrowed again, point on its entry
  (multiple-value-bind (f editor list)
      (filtered-session (list (lambda () (tuatara.defaults::goto (tuatara.buffer:buffer-size (tuatara.command:current-buffer))))
                              "C-x / s l e e k DEL DEL" :look "RET"))
    (declare (ignore editor))
    (is equal '("Foods" "Fruit" "apple" "Vegetables" "leek") (first *seen-lines*))
    (is equal '("le") (filter-strings list))
    (false (tuatara.frontend.fake:fake-frame-showing f "outside buffer")))
  (multiple-value-bind (f editor list) (filtered-session "C-x / s M-r a . RET")
    (declare (ignore f editor))
    (is equal '("Foods") (shown-lines list))
    (is equal "\"a.\"" (tuatara.defaults::search-filter-words (first (tuatara:search-filters list)))))
  ;; a pattern that does not compile yet leaves the list as it was
  (multiple-value-bind (f editor list) (filtered-session '("C-x / s c (" :look))
    (declare (ignore editor list))
    (is equal '("Foods" "Fruit" "cherry" "Vegetables" "carrot") (first *seen-lines*))
    (true (tuatara.frontend.fake:fake-frame-showing f "incomplete")))
  ;; not a list: said so
  (true (tuatara.frontend.fake:fake-frame-showing (run-keys "C-x / s" :text "plain text") "Only a list or a file of records")))

(define-test (search-filters stacked)
  ;; each filter narrows what the one before it left
  (multiple-value-bind (f editor list) (filtered-session "C-x / s a RET C-x / s r RET")
    (declare (ignore f editor))
    (is equal '("a" "r") (filter-strings list))
    (is equal '("Foods" "Vegetables" "carrot") (shown-lines list))
    (true (search "Filtered by a, r: 1 of 5 foods" (tuatara.buffer:buffer-string list))))
  ;; C-g drops only the one being typed
  (multiple-value-bind (f editor list) (filtered-session "C-x / s a RET C-x / s z C-g")
    (declare (ignore f editor))
    (is equal '("a") (filter-strings list))
    (is equal '("Foods" "Fruit" "apple" "banana" "Vegetables" "carrot") (shown-lines list)))
  ;; the pattern is edited as any answer in the minibuffer; no key but RET and C-g ends it
  (multiple-value-bind (f editor list) (filtered-session "C-x / s l C-a e C-n RET")
    (declare (ignore f editor))
    (is equal '("el") (filter-strings list)))
  ;; M-p brings back a pattern kept before
  (multiple-value-bind (f editor list) (filtered-session "C-x / s a RET C-x / s M-p DEL r RET")
    (declare (ignore f editor))
    (is equal '("a" "r") (filter-strings list)))
  ;; made again, the list is narrowed again
  (multiple-value-bind (f editor list) (filtered-session (list "C-x / s e RET" (lambda () (filter-test-list))))
    (declare (ignore f editor))
    (is equal '("Foods" "Fruit" "apple" "cherry" "Vegetables" "leek") (shown-lines list))))

(define-test (search-filters taken-away)
  ;; three filters; the middle one taken away from the list of filters, the last worked out again
  (let ((filters-list nil))
    (multiple-value-bind (f editor list)
        (filtered-session (list "C-x / s e RET C-x / s r RET C-x / s l RET C-x / l"
                                (lambda ()
                                  (setf filters-list (tuatara.command:current-buffer))
                                  ;; the second filter's line
                                  (setf (tuatara.buffer:buffer-point filters-list) (tuatara.buffer:buffer-goto-line filters-list 5)))
                                "d"))
      (declare (ignore f editor))
      (is equal '("Foods" "Fruit" "apple" "Vegetables" "leek") (shown-lines list))
      (is equal '("e" "l") (filter-strings list))
      (true (search "The filters narrowing" (tuatara.buffer:buffer-string filters-list)))
      (false (search "\"r\"" (tuatara.buffer:buffer-string filters-list)))))
  ;; the list of filters says what each leaves
  (multiple-value-bind (f editor list) (filtered-session "C-x / s e RET C-x / s r RET C-x / l")
    (declare (ignore f list))
    (let ((text (session-text editor)))
      (true (search "regex, case ignored" text))
      (true (cl-ppcre:scan "(?m)^e\\s+regex, case ignored\\s+3$" text))
      (true (cl-ppcre:scan "(?m)^r\\s+regex, case ignored\\s+1$" text))))
  ;; all taken away, from the list itself
  (multiple-value-bind (f editor list) (filtered-session "C-x / s e RET C-x / s r RET C-x / r")
    (declare (ignore editor))
    (false (filter-strings list))
    (is equal '("Foods" "Fruit" "apple" "banana" "cherry" "Vegetables" "carrot" "leek") (shown-lines list))
    (true (tuatara.frontend.fake:fake-frame-showing f "2 filters taken away"))))

(defun wait-for-filter-jobs ()
  "Wait, a few seconds at most, till no list is being filtered in the background."
  (let ((until (+ (get-internal-real-time) (* 10 internal-time-units-per-second))))
    (loop while (and (some (lambda (job) (and (tuatara.defaults::search-filter-job-p job) (tuatara.runtime:job-running-p job)))
                                 tuatara.runtime:*jobs*)
                     (< (get-internal-real-time) until))
          do (sleep 0.01))))

(define-test (search-filters more-text-in-background)
  ;; a list giving more text of its entries is matched in the background; only that text with a part named
  (let ((more (lambda (entry where)
                (declare (ignore where))
                (cdr (assoc entry '(("banana" . "long and yellow") ("leek" . "long and green")) :test #'equal)))))
    (multiple-value-bind (f editor list)
        (filtered-session (list "C-x / s l o n g" #'wait-for-filter-jobs :tick "RET") :text more)
      (declare (ignore f editor))
      (is equal '("Foods" "Fruit" "banana" "Vegetables" "leek") (shown-lines list))
      (is equal '("long") (filter-strings list)))
    ;; what the line says counts too
    (multiple-value-bind (f editor list)
        (filtered-session (list "C-x / s a p p" #'wait-for-filter-jobs :tick "RET") :text more)
      (declare (ignore f editor))
      (is equal '("Foods" "Fruit" "apple") (shown-lines list)))
    ;; matched alone, made for the test: a filter looking at a part sees only what the list gives of it
    (let* ((buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "filter parts")))
           (filtering (tuatara.defaults::list-filtering-made buffer))
           (filter (tuatara.defaults::make-search-filter "an" :where :colour)))
      (setf (tuatara.defaults::list-filtering-text filtering) (lambda (entry where) (declare (ignore entry)) (and (eq where :colour) "green")))
      (false (tuatara.defaults::entry-matches-p filtering filter "banana" "banana"))
      (true (tuatara.defaults::entry-matches-p filtering (tuatara.defaults::make-search-filter "green" :where :colour) "x" "x"))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (search-filters searches-narrow)
  ;; a list that asks for it is narrowed by C-s itself
  (multiple-value-bind (f editor list) (filtered-session "C-s c h RET" :searches t)
    (declare (ignore f editor))
    (is equal '("ch") (filter-strings list))
    (is equal '("Foods" "Fruit" "cherry") (shown-lines list)))
  ;; elsewhere C-s searches the text
  (multiple-value-bind (f editor list) (filtered-session "C-s c h RET")
    (declare (ignore f))
    (false (filter-strings list))
    (true (plusp (session-point editor)))))

(define-test (search-filters lit-elsewhere)
  ;; the filters' matches lit in another buffer, as a search's are after it ends
  (multiple-value-bind (f editor list) (filtered-session "C-x / s a n RET C-x / s M-r e RET")
    (declare (ignore f editor))
    (let ((other (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "lit") :text "Banana and EEL")))
      (tuatara:light-search-filters list other)
      (is eq other (cdr tuatara.defaults::*search-lit*))
      (is string= "(?i:an)|(?i:e)" (tuatara.defaults::search-state-string (car tuatara.defaults::*search-lit*)))
      (true tuatara.defaults::*search-lit-shown*)
      (setf tuatara.defaults::*search-lit* nil tuatara.defaults::*search-lit-shown* nil)
      (tuatara.buffer:kill-buffer other))))

(define-test (search-filters sections-are-headings)
  ;; a list that says nothing of its headings: its section lines, (:section NAME)
  ;; as every list writes them, are headings, kept over what is shown, never counted
  (let ((list nil))
    (run-keys (list (lambda ()
                      (setf list (tuatara.command:current-buffer))
                      (tuatara.modes:set-major-mode list 'tuatara.defaults::special-mode)
                      (tuatara.defaults::render-pieces list (list (format nil "Fruit~%") (format nil "apple~%") (format nil "banana~%"))
                                                       :entries '((:section :fruit) "apple" "banana") :point :start))
                    "C-x / s n a n RET")
              :width 100 :height 20)
    (is equal '("Fruit" "banana") (shown-lines list))
    (true (search "1 of 2 entries" (tuatara.buffer:buffer-string list)))))

(define-test (search-filters sections-spaced)
  ;; the empty line between sections shows only between sections shown
  (let ((list nil))
    (run-keys (list (lambda ()
                      (setf list (tuatara.command:current-buffer))
                      (tuatara.modes:set-major-mode list 'tuatara.defaults::special-mode)
                      (tuatara.defaults::render-pieces
                       list (mapcar (lambda (line) (format nil "~a~%" line))
                                    '("Fruit" "apple" "" "Nuts" "walnut" "" "Roots" "kumara" "" "Greens" "puha"))
                       :entries '((:section :fruit) "apple" nil (:section :nuts) "walnut" nil
                                  (:section :roots) "kumara" nil (:section :greens) "puha")
                       :point :start))
                    "C-x / s p p l e | p u h a RET")
              :width 100 :height 20)
    ;; the line on the filters, then the rest
    (is equal '("Fruit" "apple" "" "Greens" "puha" "")
        (rest (uiop:split-string (tuatara.buffer:buffer-string list) :separator '(#\Newline))))))

(define-test (search-filters entries-on-several-lines)
  ;; an entry shown on several lines in a row is matched by any of them, shown whole and counted once
  (let ((list nil)
        (kea (list :bird "kea")) (tui (list :bird "tui")))
    (run-keys (list (lambda ()
                      (setf list (tuatara.command:current-buffer))
                      (tuatara.modes:set-major-mode list 'tuatara.defaults::special-mode)
                      (tuatara.defaults::render-pieces
                       list (mapcar (lambda (line) (format nil "~a~%" line))
                                    '("Birds" "kea" "  alpine parrot" "tui" "  white throat tuft"))
                       :entries (list '(:section :birds) kea kea tui tui) :point :start))
                    "C-x / s p a r r o t RET")
              :width 100 :height 20)
    (is equal '("Birds" "kea" "  alpine parrot") (shown-lines list))
    (true (search "1 of 2 entries" (tuatara.buffer:buffer-string list)))))

(define-test (search-filters stepping-by-entries)
  ;; n and p of a list: from the first line of one entry to the next, an entry on several lines passed whole
  (let* ((list (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "stepping")))
         (kea (list :bird "kea")) (tui (list :bird "tui")) (ruru (list :bird "ruru")))
    (unwind-protect
         (flet ((line () (tuatara.buffer:buffer-line-number list (tuatara.buffer:buffer-point list))))
           (tuatara.defaults::render-pieces
            list (mapcar (lambda (line) (format nil "~a~%" line)) '("Birds" "kea" "  alpine" "" "tui" "  tuft" "ruru"))
            :entries (list kea kea nil tui tui ruru) :first-line 1 :point :start)
           (is eq kea (tuatara.defaults::list-step-entry list 1))
           (is = 1 (line))
           (is eq tui (tuatara.defaults::list-step-entry list 1))
           (is = 4 (line))
           ;; from the second line of an entry, back to the one before it
           (setf (tuatara.buffer:buffer-point list) (tuatara.buffer:buffer-goto-line list 5))
           (is eq kea (tuatara.defaults::list-step-entry list -1))
           (is = 1 (line))
           ;; a test passes entries over; none left: NIL, point where it was
           (is eq ruru (tuatara.defaults::list-step-entry list 1 :test (lambda (entry) (not (eq entry tui)))))
           (is = 6 (line))
           (false (tuatara.defaults::list-step-entry list 1))
           (is = 6 (line)))
      (tuatara.buffer:kill-buffer list))))

(define-test (search-filters folded-sections)
  ;; folding a list narrowed by filters: what the filters leave is counted in the whole list, a
  ;; folded section with none of it stays hidden, and the filters taken away leave the folds as they are
  (let ((list nil))
    (labels ((draw (buffer)
               (multiple-value-bind (entries pieces)
                   (tuatara.defaults::sectioned-pieces buffer '((:fruit "apple" "banana") (:nuts "walnut" "pecan"))
                                                       (lambda (section) (string-downcase (symbol-name section)))
                                                       (lambda (member section) (declare (ignore section)) (list (format nil "  ~a~%" member))))
                 (tuatara.defaults::render-pieces buffer pieces :entries entries :redraw #'draw :point :keep)))
             (lines () (rest (remove "" (uiop:split-string (tuatara.buffer:buffer-string list) :separator '(#\Newline)) :test #'string=)))
             (summary () (first (uiop:split-string (tuatara.buffer:buffer-string list) :separator '(#\Newline)))))
      (run-keys (list (lambda ()
                        (setf list (tuatara.command:current-buffer))
                        (tuatara.modes:set-major-mode list 'tuatara.defaults::sectioned-list-mode)
                        (draw list))
                      "C-x / s a p p RET" "S-TAB"
                      (lambda ()
                        (is equal '("▸ fruit (2)") (lines))
                        (true (search "1 of 4 entries" (summary))))
                      "S-TAB"
                      (lambda ()
                        (is equal '("▾ fruit (2)" "  apple") (lines))
                        (true (search "1 of 4 entries" (summary)))
                        ;; of the entries marked, those the filters hide
                        (is equal '((:member "walnut" :nuts))
                            (tuatara.defaults::keys-hidden-by-filters list '((:member "apple" :fruit) (:member "walnut" :nuts)))))
                      (lambda () (setf (tuatara.buffer:buffer-point list) (tuatara.buffer:buffer-goto-line list 1)))
                      "TAB"
                      (lambda ()
                        (is equal '("▸ fruit (2)") (lines))
                        (true (search "1 of 4 entries" (summary))))
                      ;; the filters taken away, the section folded meanwhile stays folded
                      "M-x r e m o v e - s e a r c h - f i l t e r s RET"
                      (lambda ()
                        (is equal '("▸ fruit (2)" "▾ nuts (2)" "  walnut" "  pecan")
                            (remove "" (uiop:split-string (tuatara.buffer:buffer-string list) :separator '(#\Newline)) :test #'string=))))
                :width 100 :height 20))))

(define-test (search-filters lines-going-with-headings)
  ;; a list may hide a heading with nothing under it, and give lines that go with their heading, never matched nor counted
  (let ((entries '((:day 1) (:grid 1) "alpha" (:day 2) (:grid 2) "beta" (:day 3)))
        (list nil))
    (run-keys (list (lambda ()
                      (setf list (tuatara.command:current-buffer))
                      (tuatara.modes:set-major-mode list 'tuatara.defaults::special-mode)
                      (tuatara.defaults::render-pieces
                       list (list (format nil "Plan~%") (format nil "Day 1~%") (format nil "  grid~%") (format nil "alpha~%")
                                  (format nil "Day 2~%") (format nil "  grid~%") (format nil "beta~%") (format nil "Day 3~%"))
                       :entries entries :first-line 1 :point :start)
                      (tuatara:set-search-filtering list :nouns "items" :empty-headings nil
                                                         :heading (lambda (entry) (and (consp entry) (eq (first entry) :day)))
                                                         :decoration (lambda (entry) (and (consp entry) (eq (first entry) :grid)))))
                    "C-x / s a l p h a RET"
                    (lambda ()
                      (is equal '("Plan" "Day 1" "  grid" "alpha") (shown-lines list))
                      (true (search "Filtered by alpha: 1 of 2 items" (tuatara.buffer:buffer-string list))))
                    "C-x / s g r i d RET"
                    (lambda ()
                      (is equal '("Plan") (shown-lines list))))
              :width 100 :height 20)))
;;; Records of a CSV narrowed by search filters: the lines no filter leaves hidden, the text kept whole

(defparameter *filter-test-records*
  (format nil "name,place,count~%kiwi,Akaroa,3~%tui,Raglan,5~%kea,Arthur's Pass,2~%kiwi,Raglan,7~%weka,Akaroa,1~%")
  "The CSV the search filters' tests of records narrow: a header and five birds.")

(defun records-file (text &key (type "csv"))
  "A new file of TYPE in the test directory holding TEXT; its path."
  (let ((path (merge-pathnames (format nil "tuatara-filter-~a.~a" (random 100000000) type) (uiop:temporary-directory))))
    (with-open-file (o path :direction :output :if-exists :supersede) (write-string text o))
    path))

(defun records-shown (buffer)
  "The lines of BUFFER its windows show, those search filters hide left out."
  (let* ((text (tuatara.buffer:buffer-string buffer))
         (bounds (tuatara.display:hidden-bounds buffer))
         (start 0) (shown '()))
    (loop for line in (uiop:split-string text :separator '(#\Newline))
          do (unless (and bounds (loop for i from 0 below (length bounds) by 2
                                       ;; a range from the very start hides the lines up to its end
                                       thereis (if (zerop (aref bounds i))
                                                   (< start (aref bounds (1+ i)))
                                                   (< (aref bounds i) start (1+ (aref bounds (1+ i)))))))
               (push line shown))
             (incf start (1+ (tuatara.text:string-octet-length line))))
    (remove "" (nreverse shown) :test #'string=)))

(defun records-session (items &key (text *filter-test-records*) (type "csv") (height 20))
  "Run ITEMS, as RUN-KEYS takes them, in a buffer visiting a new file of TYPE
holding TEXT; return the frontend, the buffer and the file, the buffer left
for the caller to look at and kill (KILL-RECORDS)."
  (let* ((path (records-file text :type type))
         (buffer nil))
    (multiple-value-bind (f editor)
        (run-keys (cons (lambda () (setf buffer (tuatara.command:current-buffer))) items)
                  :file path :width 100 :height height)
      (declare (ignore editor))
      (values f buffer path))))

(defun kill-records (buffer path)
  "Kill BUFFER, a records session's, without asking, and delete its file PATH."
  (when (and buffer (tuatara.buffer:live-buffer-p buffer))
    (tuatara.buffer:kill-buffer buffer))
  (ignore-errors (delete-file path)))

(defun screen-showing (f text)
  "True when the last frame F painted has TEXT on one of its rows."
  (loop for row below (length (tuatara.frontend.fake::fake-screen f))
        thereis (search text (tuatara.frontend.fake:fake-screen-line f row))))

(defmacro with-records ((f buffer path) (items &rest options) &body body)
  "Run BODY with F, BUFFER and PATH bound to RECORDS-SESSION's values for
ITEMS and OPTIONS, the buffer killed and its file deleted after."
  `(multiple-value-bind (,f ,buffer ,path) (records-session ,items ,@options)
     (declare (ignorable ,f))
     (unwind-protect (progn ,@body)
       (kill-records ,buffer ,path))))

(define-test (search-filters csv-records-shown-and-hidden)
  ;; C-x / s in a CSV hides the records the pattern does not match, the header kept, the text whole
  (with-records (f buffer path) ('("C-x / s k i w i RET"))
    (is equal '("name,place,count" "kiwi,Akaroa,3" "kiwi,Raglan,7") (records-shown buffer))
    (is string= *filter-test-records* (tuatara.buffer:buffer-string buffer))
    (true (screen-showing f "Filtered by kiwi: 2 of 5 records"))
    (true (screen-showing f "kiwi,Akaroa,3..."))
    (is equal '("kiwi") (filter-strings buffer)))
  ;; the whole line is matched, smart case as in a search: a capital heeds it
  (with-records (f buffer path) ('("C-x / s a k a r o a RET"))
    (is equal '("name,place,count" "kiwi,Akaroa,3" "weka,Akaroa,1") (records-shown buffer)))
  (with-records (f buffer path) ('("C-x / s R a g RET"))
    (is equal '("name,place,count" "tui,Raglan,5" "kiwi,Raglan,7") (records-shown buffer)))
  (with-records (f buffer path) ('("C-x / s r A G RET"))
    (is equal '("name,place,count") (records-shown buffer)))
  ;; the header is never hidden, whatever the pattern
  (with-records (f buffer path) ('("C-x / s z z z RET"))
    (is equal '("name,place,count") (records-shown buffer))
    (true (screen-showing f "Filtered by zzz: 0 of 5 records")))
  ;; narrowed as typed, C-g lets the one typed go
  (let ((seen nil))
    (with-records (f buffer path) ((list "C-x / s t u" (lambda () (setf seen (records-shown (tuatara.command:current-buffer)))) "C-g"))
      (is equal '("name,place,count" "tui,Raglan,5") seen)
      (false (filter-strings buffer))
      (is = 6 (length (records-shown buffer))))))

(define-test (search-filters log-records)
  ;; a log is narrowed as a CSV is, but has no header: its first line is hidden as any other
  (let ((log (format nil "2026-10-01 00:00:01 INFO start~%2026-10-01 00:00:02 ERROR disk full~%~
                          2026-10-01 00:00:03 INFO ok~%2026-10-01 00:00:04 ERROR no network~%")))
    (with-records (f buffer path) ('("C-x / s e r r o r RET") :text log :type "log")
      (true (typep (tuatara.modes:buffer-major-mode buffer) 'tuatara.defaults::log-mode))
      (is equal '("2026-10-01 00:00:02 ERROR disk full" "2026-10-01 00:00:04 ERROR no network") (records-shown buffer))
      (is string= log (tuatara.buffer:buffer-string buffer))
      (true (search "ERROR disk full" (tuatara.frontend.fake:fake-screen-line f 0)))
      (true (screen-showing f "Filtered by error: 2 of 4 records")))
    (with-records (f buffer path) ('("C-x / s z z z RET") :text log :type "log")
      (false (records-shown buffer))
      (true (screen-showing f "Filtered by zzz: 0 of 4 records")))
    (with-records (f buffer path) ('("C-x / s s t a r t RET") :text log :type "log")
      (is equal '("2026-10-01 00:00:01 INFO start") (records-shown buffer))))
  ;; JSON Lines has a mode of its own, coloured as JSON, its lines records
  (let ((lines (format nil "{\"bird\": \"kiwi\", \"count\": 3}~%{\"bird\": \"tui\", \"count\": 5}~%")))
    (with-records (f buffer path) ('("C-x / s t u i RET") :text lines :type "jsonl")
      (true (typep (tuatara.modes:buffer-major-mode buffer) 'tuatara.defaults::jsonl-mode))
      (true (typep (tuatara.modes:buffer-major-mode buffer) 'tuatara.defaults::json-mode))
      (is equal '("{\"bird\": \"tui\", \"count\": 5}") (records-shown buffer))))
  ;; a JSON file is no file of records
  (false (tuatara.defaults::filters-hide-lines-p (make-instance 'tuatara.defaults::json-mode) nil))
  (true (tuatara.defaults::filters-hide-lines-p (make-instance 'tuatara.defaults::hosts-generic-mode) nil)))

(define-test (search-filters csv-stacked)
  ;; each filter narrows what the one before left; C-x / l lists them; C-x / r takes them away
  (with-records (f buffer path) ('("C-x / s k i w i RET C-x / s r a g RET"))
    (is equal '("name,place,count" "kiwi,Raglan,7") (records-shown buffer))
    (true (screen-showing f "Filtered by kiwi, rag: 1 of 5 records")))
  (let ((text nil))
    (with-records (f buffer path) ((list "C-x / s k i w i RET C-x / s r a g RET C-x / l"
                                         (lambda () (setf text (tuatara.buffer:buffer-string (tuatara.command:current-buffer))))))
      (true (cl-ppcre:scan "(?m)^kiwi\\s+regex, case ignored\\s+2$" text))
      (true (cl-ppcre:scan "(?m)^rag\\s+regex, case ignored\\s+1$" text))))
  ;; one taken away from the list of filters: the other works out again
  (with-records (f buffer path) ((list "C-x / s k i w i RET C-x / s r a g RET C-x / l"
                                       (lambda () (let ((list (tuatara.command:current-buffer)))
                                                    (setf (tuatara.buffer:buffer-point list) (tuatara.buffer:buffer-goto-line list 4))))
                                       "d"))
    (is equal '("rag") (filter-strings buffer))
    (is equal '("name,place,count" "tui,Raglan,5" "kiwi,Raglan,7") (records-shown buffer)))
  (with-records (f buffer path) ('("C-x / s k i w i RET C-x / s r a g RET C-x / r"))
    (false (filter-strings buffer))
    (is = 6 (length (records-shown buffer)))
    (false (tuatara.display:hidden-bounds buffer))
    (true (tuatara.frontend.fake:fake-frame-showing f "2 filters taken away: every record is shown"))))

(define-test (search-filters csv-tsv-and-header-lock)
  ;; a TSV alike; the header kept at the top while filters hide the rest
  (with-records (f buffer path) ('("C-x / s k e a RET") :text (format nil "name	place~%kiwi	Akaroa~%kea	Arthur's Pass~%") :type "tsv")
    (is equal '("name	place" "kea	Arthur's Pass") (records-shown buffer))
    (true (screen-showing f "Filtered by kea: 1 of 2 records")))
  (with-records (f buffer path) ('("C-x / L C-x / s w e k a RET M->"))
    (is string= "name,place,count" (tuatara.frontend.fake:fake-screen-line f 0))
    (is equal '("name,place,count" "weka,Akaroa,1") (records-shown buffer))))

(define-test (search-filters csv-edits)
  (let ((kiwi-line 1))
    (flet ((on-line (n) (lambda () (let ((b (tuatara.command:current-buffer)))
                                     (setf (tuatara.buffer:buffer-point b) (tuatara.buffer:buffer-goto-line b n))))))
      ;; a record shown edited stays shown though it no longer matches, until the filters are worked out again
      (with-records (f buffer path) ((list "C-x / s k i w i RET" (on-line kiwi-line) "d d"))
        (is equal '("name,place,count" "wi,Akaroa,3" "kiwi,Raglan,7") (records-shown buffer)))
      (with-records (f buffer path) ((list "C-x / s k i w i RET" (on-line kiwi-line) "d d C-x / s , RET"))
        (is equal '("name,place,count" "kiwi,Raglan,7") (records-shown buffer)))
      ;; a selection over hidden records: deleting it is refused, nothing changed
      (with-records (f buffer path) ((list "C-x / s k i w i RET"
                                           (lambda () (let ((b (tuatara.command:current-buffer)))
                                                        (setf (tuatara.buffer:buffer-mark b) 0 (tuatara.buffer:buffer-mark-active-p b) t
                                                              (tuatara.buffer:buffer-point b) (tuatara.buffer:buffer-size b))))
                                           "C-w"))
        (is string= *filter-test-records* (tuatara.buffer:buffer-string buffer))
        (true (tuatara.frontend.fake:fake-frame-showing f "That would change lines the search filters hide")))
      ;; so are keep-lines and a sort over them
      (with-records (f buffer path) ((list "C-x / s k i w i RET"
                                           (lambda () (let ((b (tuatara.command:current-buffer)))
                                                        (setf (tuatara.buffer:buffer-mark b) 0 (tuatara.buffer:buffer-mark-active-p b) t
                                                              (tuatara.buffer:buffer-point b) (tuatara.buffer:buffer-size b))))
                                           "M-x k e e p - l i n e s RET RET a RET"))
        (is string= *filter-test-records* (tuatara.buffer:buffer-string buffer))
        (true (tuatara.frontend.fake:fake-frame-showing f "That would change lines the search filters hide")))
      (with-records (f buffer path) ((list "C-x / s k i w i RET C-x h M-x s o r t - l i n e s RET"))
        (is string= *filter-test-records* (tuatara.buffer:buffer-string buffer))
        (true (tuatara.frontend.fake:fake-frame-showing f "That would change lines the search filters hide")))
      ;; deleting the newline that joins a record shown to one hidden: refused
      (with-records (f buffer path) ((list "C-x / s k i w i RET" (on-line kiwi-line) "C-e C-d"))
        (is string= *filter-test-records* (tuatara.buffer:buffer-string buffer)))
      ;; a record shown killed whole goes, the hidden ones stay; undo brings it back
      (with-records (f buffer path) ((list "C-x / s k i w i RET" (on-line kiwi-line) "M-x k i l l - w h o l e - l i n e RET"))
        (is string= (remove-first-line *filter-test-records* 1) (tuatara.buffer:buffer-string buffer))
        (is equal '("name,place,count" "kiwi,Raglan,7") (records-shown buffer)))
      (with-records (f buffer path) ((list "C-x / s k i w i RET" (on-line kiwi-line) "M-x k i l l - w h o l e - l i n e RET u" :tick))
        (is string= *filter-test-records* (tuatara.buffer:buffer-string buffer))
        (is equal '("name,place,count" "kiwi,Akaroa,3" "kiwi,Raglan,7") (records-shown buffer)))
      ;; point is kept on the records shown: down from the first kiwi goes to the second
      (with-records (f buffer path) ((list "C-x / s k i w i RET" (on-line kiwi-line) "j"))
        (is = (tuatara.buffer:buffer-goto-line buffer 4) (tuatara.buffer:buffer-point buffer))))))

;;; Taking text from records narrowed: only the records shown

(defparameter *filter-test-pairs*
  (format nil "name,n~%a1,x~%b2,y~%a3,x~%b4,y~%a5,x~%b6,y~%")
  "A CSV whose records alternate, those with x shown by the filter x, those
with y hidden between them and after the last.")

(defun line-start-of (n)
  "A step of keys putting point at the start of line N, from 0."
  (lambda () (let ((b (tuatara.command:current-buffer)))
               (setf (tuatara.buffer:buffer-point b) (tuatara.buffer:buffer-goto-line b n)))))

(defvar *seen-kill* nil
  "The kill first on the kill ring where a session's keys looked at it (SEE-KILL).")

(defun see-kill ()
  "A step of keys keeping the kill first on the kill ring in *SEEN-KILL*."
  (lambda () (setf *seen-kill* (first (tuatara.command:editor-kill-ring tuatara.command:*editor*)))))

(define-test (search-filters copies-take-records-shown)
  (let ((shown (format nil "a1,x~%a3,x~%a5,x~%")))
    ;; V over three records shown, two hidden between: y copies the three alone, and says what it left out
    (with-records (f buffer path) ((list "C-x / s x RET" (line-start-of 1) "V j j y" (see-kill)) :text *filter-test-pairs*)
      (is string= shown *seen-kill*)
      (is string= *filter-test-pairs* (tuatara.buffer:buffer-string buffer))
      (true (tuatara.frontend.fake:fake-frame-showing f "Copied, 2 lines hidden by the search filters left out")))
    ;; a region copied with M-w alike, from mid-record to mid-record
    (with-records (f buffer path) ((list "C-x / s x RET"
                                         (lambda () (let ((b (tuatara.command:current-buffer)))
                                                      (setf (tuatara.buffer:buffer-mark b) (1+ (tuatara.buffer:buffer-goto-line b 1))
                                                            (tuatara.buffer:buffer-mark-active-p b) t
                                                            (tuatara.buffer:buffer-point b) (+ 2 (tuatara.buffer:buffer-goto-line b 5)))))
                                         "M-w" (see-kill))
                                   :text *filter-test-pairs*)
      (is string= (format nil "1,x~%a3,x~%a5") *seen-kill*))
    ;; banking over hidden records banks those shown, and they are copied and pasted as lines
    (let ((banked nil))
      (with-records (f buffer path) ((list "C-x / s x RET" (line-start-of 1) "V j j m l"
                                           (lambda () (setf banked (tuatara.defaults::banked-line-count (tuatara.command:current-buffer))))
                                           "y" (see-kill) "g g p")
                                     :text *filter-test-pairs*)
        (is = 3 banked)
        (true (tuatara.frontend.fake:fake-frame-showing f "Banked 3 lines (3 total), 2 lines hidden by the search filters left out"))
        (is string= shown *seen-kill*)
        (true (search shown (tuatara.buffer:buffer-string buffer)))))
    ;; deleting banked records takes the shown ones alone, the hidden ones kept
    (with-records (f buffer path) ((list "C-x / s x RET" (line-start-of 1) "V j j m l d") :text *filter-test-pairs*)
      (is string= (format nil "name,n~%b2,y~%b4,y~%b6,y~%") (tuatara.buffer:buffer-string buffer)))
    ;; a rectangle over them: the rows of the hidden records left out, in the store and on the kill ring
    (with-records (f buffer path) ((list "C-x / s x RET" (line-start-of 1) "m v j j l y") :text *filter-test-pairs*)
      (is equal '("a1" "a3" "a5") tuatara.defaults::*killed-rectangle*)
      (true (tuatara.frontend.fake:fake-frame-showing f "Copied a rectangle of 3 rows, 2 lines hidden by the search filters left out")))
    (with-records (f buffer path) ((list "C-x / s x RET" (line-start-of 1) "m v j j l M-w" (see-kill)) :text *filter-test-pairs*)
      (is string= (format nil "a1~%a3~%a5") *seen-kill*))
    ;; counting words counts the records shown
    (with-records (f buffer path) ((list "C-x / s x RET M-x c o u n t - w o r d s RET") :text *filter-test-pairs*)
      (true (tuatara.frontend.fake:fake-frame-showing f "Buffer has 4 lines"))
      (true (tuatara.frontend.fake:fake-frame-showing f "3 lines hidden by the search filters left out")))
    ;; with no filter, everything between is copied, and nothing said of it
    (with-records (f buffer path) ((list (line-start-of 1) "V j j y" (see-kill)) :text *filter-test-pairs*)
      (is string= (format nil "a1,x~%b2,y~%a3,x~%") *seen-kill*)
      (false (tuatara.frontend.fake:fake-frame-showing f "left out")))))

(define-test (search-filters copies-take-folds-whole)
  ;; a folded Org subtree is copied whole: folds are not search filters
  (let ((tuatara-user:*org-startup-indented* nil) (*seen-kill* nil))
    (run-keys (list "M-x o r g - m o d e RET S-TAB V y" (see-kill)) :text (format nil "* One~%body one~%* Two~%body two~%"))
    (is string= (format nil "* One~%body one~%") *seen-kill*)))

(defun remove-first-line (text n)
  "TEXT without its line N, from 0."
  (format nil "~{~a~%~}" (let ((lines (butlast (uiop:split-string text :separator '(#\Newline)))))
                           (append (subseq lines 0 n) (nthcdr (1+ n) lines)))))

(define-test (search-filters csv-save-and-revert)
  ;; saving writes the whole file, with the edit made to a record shown
  (with-records (f buffer path) ((list "C-x / s k i w i RET"
                                       (lambda () (let ((b (tuatara.command:current-buffer)))
                                                    (setf (tuatara.buffer:buffer-point b) (tuatara.buffer:buffer-goto-line b 4))))
                                       "i X C-g C-x C-s" :tick))
    (is string= (cl-ppcre:regex-replace "kiwi,Raglan" *filter-test-records* "Xkiwi,Raglan") (uiop:read-file-string path))
    (is equal '("name,place,count" "kiwi,Akaroa,3" "Xkiwi,Raglan,7") (records-shown buffer)))
  ;; read again after the file changed on disk: the filters worked out again for what it holds now
  (with-records (f buffer path) ((list "C-x / s k i w i RET"
                                       (lambda ()
                                         (sleep 0.01)
                                         (with-open-file (o (tuatara.buffer:buffer-file (tuatara.command:current-buffer))
                                                            :direction :output :if-exists :supersede)
                                           (write-string (cl-ppcre:regex-replace "tui,Raglan" *filter-test-records* "kiwi,Ohope") o)))
                                       "M-x r e v e r t - b u f f e r RET" :tick))
    (is equal '("name,place,count" "kiwi,Akaroa,3" "kiwi,Ohope,5" "kiwi,Raglan,7") (records-shown buffer))))

(defun many-records (n)
  "A CSV of N records under a header, every third a kiwi."
  (with-output-to-string (out)
    (format out "name,number~%")
    (dotimes (i n) (format out "~:[weka~;kiwi~],~d~%" (zerop (mod i 3)) i))))

(define-test (search-filters csv-in-background)
  ;; a large file: the window's lines matched at once, the rest in the background, counted once done
  (let* ((release (list nil))
         (tuatara.defaults::*search-filter-quick-characters* 100)
         (tuatara.defaults::*filter-piece* 300)
         (tuatara.defaults::*rewrite-piece-hook*
           (lambda () (loop repeat 1000 until (car release) do (sleep 0.01))))
         (partial nil) (counting nil) (first-job nil) (second-job nil))
    (unwind-protect
         (with-records (f buffer path)
             ((list "C-x / s k i"
                    (lambda () (setf first-job (tuatara.defaults::list-filtering-job
                                                (tuatara.defaults::list-filtering (tuatara.command:current-buffer)))))
                    "w i"
                    (lambda ()
                      (let ((b (tuatara.command:current-buffer)))
                        (setf partial (records-shown b)
                              second-job (tuatara.defaults::list-filtering-job (tuatara.defaults::list-filtering b)))))
                    "RET"
                    (lambda () (setf counting (tuatara.defaults::filtered-counts (tuatara.command:current-buffer))))
                    (lambda () (setf (car release) t) (wait-for-filter-jobs))
                    :tick)
              :text (many-records 300) :height 12)
           ;; before the job ended: the window's worth from the top, a header and kiwis only
           (true (< 1 (length partial) 20))
           (true (every (lambda (line) (or (search "kiwi" line) (search "name" line))) partial))
           (true (tuatara.frontend.fake:fake-frame-showing f "Filtered by kiwi: … records (counting)"))
           (true (processing-shown f))
           ;; the next key stopped the first job and started another
           (true first-job)
           (true second-job)
           (false (eq first-job second-job))
           (true (car (tuatara.runtime:job-cancel first-job)))
           ;; done: every kiwi, counted
           (true counting)
           (is = 101 (length (records-shown buffer)))
           (true (screen-showing f "Filtered by kiwi: 100 of 300 records")))
      (setf (car release) t)
      (wait-for-filter-jobs))))

(define-test (search-filters csv-after-typing-pauses)
  ;; a file past *typing-pause-octets*: the window's lines at each key, the job only once typing
  ;; pauses, or at once on RET
  (let ((tuatara.defaults::*search-filter-quick-characters* 100)
        (tuatara.defaults::*typing-pause-octets* 0)
        (tuatara.defaults::*typing-pause* 1000)
        (job :unset) (due nil) (partial nil))
    (with-records (f buffer path)
        ((list "C-x / s k i w i"
               (lambda ()
                 (let* ((b (tuatara.command:current-buffer)) (filtering (tuatara.defaults::list-filtering b)))
                   (setf job (tuatara.defaults::list-filtering-job filtering)
                         due (tuatara.defaults::list-filtering-due filtering)
                         partial (records-shown b))))
               "RET" #'wait-for-filter-jobs :tick)
         :text (many-records 300) :height 12)
      (false job)
      (true due)
      (true (< 1 (length partial) 20))
      (true (every (lambda (line) (or (search "kiwi" line) (search "name" line))) partial))
      (is = 101 (length (records-shown buffer)))
      (true (screen-showing f "Filtered by kiwi: 100 of 300 records")))))

(define-test (search-filters csv-stop-job)
  ;; stopped: the filter it was working out taken away, everything shown as before
  (let* ((release (list nil))
         (tuatara.defaults::*search-filter-quick-characters* 100)
         (tuatara.defaults::*filter-piece* 300)
         (tuatara.defaults::*rewrite-piece-hook*
           (lambda () (loop repeat 1000 until (car release) do (sleep 0.01)))))
    (unwind-protect
         (with-records (f buffer path)
             ((list "C-x / s k i w i RET M-x s t o p - j o b RET y RET"
                    (lambda () (setf (car release) t) (wait-for-filter-jobs))
                    :tick)
              :text (many-records 300) :height 12)
           (true (tuatara.frontend.fake:fake-frame-showing f "Stop filtering"))
           (false (filter-strings buffer))
           (is = 301 (length (records-shown buffer))))
      (setf (car release) t)
      (wait-for-filter-jobs))))

(define-test (search-filters filter-prompt-cursor)
  ;; C-x / s: the pattern typed in the minibuffer, the cursor after it on its row
  (let ((seen nil))
    (filtered-session (list "C-x / s a n" (lambda () (setf seen (question-look))) "RET"))
    (destructuring-bind (cursor screen spans) seen
      (declare (ignore spans))
      ;; what it leaves of the list before the prompt, as a search's count
      (is string= "1/5    Regex filter: an" (string-right-trim " " (aref screen (car cursor))))
      (is = 23 (cdr cursor)))))

(define-test (search-filters filter-unusable-pattern-waits)
  ;; RET on a filter whose pattern cannot be used keeps the question open, saying why; C-g keeps none
  (with-records (f buffer path) ('("C-x / s ( k e a RET C-g"))
    (true (tuatara.frontend.fake:fake-frame-showing f "Regex filter [incomplete; finish it, or C-g gives up]: (kea"))
    (false (filter-strings buffer))
    (is = 6 (length (records-shown buffer)))))

(define-test (search-filters filter-slow-pattern)
  ;; a nested repeat stuck on a record: each key answered within a tenth of a second, the
  ;; filtering going on in the background and saying so; C-g stops it, inside that record too
  (let ((times '()) (left-running t))
    (flet ((stamp () (push (get-internal-real-time) times)))
      (call-with-watchdog
       20 (lambda ()
            (with-records (f buffer path)
                ((list "C-x / s ^ ( a + ) +" #'stamp "$" #'stamp :tick #'stamp "C-g" #'stamp
                       (lambda ()
                         (loop repeat 50 while (find-if #'tuatara.runtime:job-running-p tuatara.runtime:*jobs*) do (sleep 0.01))
                         (setf left-running (find-if #'tuatara.runtime:job-running-p tuatara.runtime:*jobs*))))
                 :text (format nil "name,count~%kea,2~%~a!,3~%tui,5~%" (make-string 40 :initial-element #\a)))
              (true (tuatara.frontend.fake:fake-frame-showing f "Regex filter [slow pattern: still filtering]: ^(a+)+$"))
              (false (filter-strings buffer))
              (is = 4 (length (records-shown buffer)))))))
    (false left-running)
    (loop for (later earlier) on times while earlier
          do (true (< (- later earlier) (/ internal-time-units-per-second 10)) "a key took ~,3f s"
                   (/ (- later earlier) internal-time-units-per-second)))))

(define-test (search-filters rectangle-lit-where-shown)
  ;; a rectangle across lines the filters hide is lit on the lines shown alone,
  ;; not worked out for every hidden line between them at each frame
  (let ((path (records-file (with-output-to-string (o)
                              (format o "n,a~%")
                              (loop for i below 5000 do (format o "~d,~d~%" i (* 7 i))))))
        (b nil) (lit nil))
    (unwind-protect
         (run-keys (list (lambda () (setf b (tuatara.command:current-buffer)))
                         "C-x / s ^ ( 5 | 4 9 9 5 ) , RET" :tick
                         (lambda () (loop repeat 500 while (tuatara.runtime:jobs-of b :running t) do (sleep 0.01)))
                         (line-start-of 6) "m v j l" :tick
                         (lambda ()
                           (let* ((e tuatara.command:*editor*) (w (tuatara.command:editor-window e)))
                             (setf lit (remove :region (tuatara.command::window-highlights-for
                                                        e w (tuatara.display:window-rows w))
                                               :key #'third :test-not #'eq)))))
                   :file path :width 60 :height 10)
      (kill-records b path))
    (is = 2 (length lit) "lit: ~s" (subseq lit 0 (min 6 (length lit))))))


(defun list-records-after (keys text &key on-disk (mode "csv-mode") crlf)
  "The text after typing KEYS on TEXT in MODE, with CRLF its lines ending
with a carriage return and a newline. With ON-DISK, the work goes to disk in
tiny pieces, a few lines each, so that a small text exercises it."
  (flet ((run ()
           (session-text (nth-value 1 (run-keys-in-turns (list (format nil "M-x ~a RET ~a" (typed-keys mode) keys) "")
                                                         :text text :width 100 :crlf crlf)))))
    (if on-disk
        (let ((tuatara.defaults::*rewrite-piece* 16)
              (tuatara.defaults::*rewrite-checkpoint* 8))
          (run))
        (run))))

(defparameter *list-records-sample*
  (format nil "NAME,CITY,NOTE~%Ann,Auckland,tea~%Bob,\"Napier, Hawke's Bay\",coffee~%Cat,Nelson,\"said \"\"hi\"\"\"~%Dan,Auckland~%")
  "Records: a quoted field holding the separator, one holding doubled quotes, one short a field.")

(define-test (search-filters records-kept-and-deleted)
  (dolist (on-disk '(nil t))
    ;; by the header's name: the quoted field's value, the comma inside it no separator
    (is string= (format nil "NAME,CITY,NOTE~%Bob,\"Napier, Hawke's Bay\",coffee~%")
        (list-records-after (format nil "C-x / k C I T Y RET ~a RET" (typed-keys "^Napier, H")) *list-records-sample* :on-disk on-disk))
    ;; by its number, from 0: the city, Auckland's records deleted, the header kept though it does not match
    (is string= (format nil "NAME,CITY,NOTE~%Bob,\"Napier, Hawke's Bay\",coffee~%Cat,Nelson,\"said \"\"hi\"\"\"~%")
        (list-records-after "C-x / d 1 RET a u c k RET" *list-records-sample* :on-disk on-disk))
    ;; a doubled quote read as one; a record short of the column does not match
    (is string= (format nil "NAME,CITY,NOTE~%Cat,Nelson,\"said \"\"hi\"\"\"~%")
        (list-records-after (format nil "C-x / k $ 2 RET ~a RET" (typed-keys "\"hi\"$")) *list-records-sample* :on-disk on-disk))
    ;; the whole record
    (is string= (format nil "NAME,CITY,NOTE~%Ann,Auckland,tea~%Dan,Auckland~%")
        (list-records-after "C-x / k RET l a n d RET" *list-records-sample* :on-disk on-disk))
    ;; a tab-separated file by its own separator
    (is string= (format nil "A	B~%2	y~%")
        (list-records-after "C-x / k B RET y RET" (format nil "A	B~%1	x~%2	y~%") :on-disk on-disk :mode "tsv-mode"))
    ;; a log: the whole line, no header
    (is string= (format nil "10:00 ERROR disk~%10:02 ERROR net~%")
        (list-records-after "C-x / k RET E R R O R RET" (format nil "10:00 ERROR disk~%10:01 INFO up~%10:02 ERROR net~%")
                            :on-disk on-disk :mode "log-mode"))
    ;; a log by the date its lines begin with, compared as a date, as written and in UTC
    (let ((log (format nil "2026-06-29T22:10:04+02:00 login aroha~%2026-06-30T23:30:00-05:00 login hemi~%2026-07-01T00:15:00+02:00 login aroha~%2026-07-01 08:00:00 +0200 backup~%")))
      (is string= (format nil "2026-07-01T00:15:00+02:00 login aroha~%2026-07-01 08:00:00 +0200 backup~%")
          (list-records-after (format nil "C-x / k $ 0 RET ~a RET" (typed-keys "[[:date>=2026-07-01:]]")) log
                              :on-disk on-disk :mode "log-mode"))
      (is string= (format nil "2026-06-29T22:10:04+02:00 login aroha~%2026-07-01T00:15:00+02:00 login aroha~%")
          (list-records-after (format nil "C-x h M-x k e e p - l i n e s RET $ 0 RET ~a RET" (typed-keys "[[:dateutc<2026-07-01:]]")) log
                              :on-disk on-disk :mode "log-mode"))))
  ;; no such column: said, nothing changed
  (multiple-value-bind (f editor) (run-keys "M-x c s v - m o d e RET C-x / k P L A C E RET" :text *list-records-sample* :width 100)
    (is string= *list-records-sample* (session-text editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "No such column: PLACE")))
  ;; not a file of records: said, nothing changed
  (multiple-value-bind (f editor) (run-keys "C-x / d" :text "a b c" :width 100)
    (is string= "a b c" (session-text editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "Not a file of records (a CSV, a log): list-delete-records was not run")))
  ;; one step to undo
  (is string= *list-records-sample* (list-records-after "C-x / d 1 RET a u c k RET u" *list-records-sample*)))

(define-test (search-filters records-shown-and-hidden)
  ;; shown by a column, the others hidden, nothing deleted; the filter kept with the others
  (with-records (f buffer path) ((list "C-x / f C I T Y RET ^ a u c k RET" :tick) :text *list-records-sample*)
    (is string= *list-records-sample* (tuatara.buffer:buffer-string buffer))
    (is equal '("NAME,CITY,NOTE" "Ann,Auckland,tea" "Dan,Auckland") (records-shown buffer))
    (is equal '("$1: ^auck") (mapcar #'tuatara.defaults::search-filter-words (tuatara.defaults::search-filters buffer))))
  ;; hidden by a column; the next filter narrows what it leaves
  (with-records (f buffer path) ((list "C-x / h 1 RET ^ a u c k RET" :tick "C-x / s c o f RET" :tick) :text *list-records-sample*)
    (is string= *list-records-sample* (tuatara.buffer:buffer-string buffer))
    (is equal '("NAME,CITY,NOTE" "Bob,\"Napier, Hawke's Bay\",coffee") (records-shown buffer))
    (is equal '("not $1: ^auck" "cof") (mapcar #'tuatara.defaults::search-filter-words (tuatara.defaults::search-filters buffer))))
  ;; a log: the whole line hidden; the filters taken away show every line again
  (with-records (f buffer path) ((list "C-x / h RET I N F O RET" :tick) :type "log"
                                 :text (format nil "10:00 ERROR disk~%10:01 INFO up~%10:02 ERROR net~%"))
    (is equal '("10:00 ERROR disk" "10:02 ERROR net") (records-shown buffer)))
  ;; a large file: worked out in the background on all but two cores, as C-x / s is
  (with-records (f buffer path) ((list "C-x / h 1 RET 7 RET" :tick
                                       (lambda () (loop repeat 500 while (tuatara.runtime:jobs-of (tuatara.command:current-buffer) :running t)
                                                        do (sleep 0.01))))
                                 :text (with-output-to-string (o)
                                         (format o "n,a~%")
                                         (loop for i below 100000 do (format o "~d,~d~%" i (* 7 i)))))
    (is = (loop for i below 100000 count (not (find #\7 (princ-to-string (* 7 i)))))
        (tuatara.defaults::filtered-counts buffer)))
  ;; a list: its entries hidden, or shown, by the whole line, headings kept over those left
  (multiple-value-bind (f editor list) (filtered-session "C-x / h a n RET")
    (declare (ignore f editor))
    (is equal '("Foods" "Fruit" "apple" "cherry" "Vegetables" "carrot" "leek") (shown-lines list))
    (is equal '("not an") (mapcar #'tuatara.defaults::search-filter-words (tuatara.defaults::search-filters list))))
  (multiple-value-bind (f editor list) (filtered-session "C-x / f e RET")
    (declare (ignore f editor))
    (is equal '("Foods" "Fruit" "apple" "cherry" "Vegetables" "leek") (shown-lines list)))
  ;; not filterable: said
  (multiple-value-bind (f editor) (run-keys "C-x / f" :text "a b c" :width 100)
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "list-show-records was not run"))))

(defparameter *headerless-records*
  (format nil "Eve,Wellington~%Ann,Auckland~%Dan,Auckland~%")
  "Records with no header: the first line is a record like the rest.")

(define-test (search-filters first-line-a-record)
  ;; C-x / t in a CSV: the first line filtered with the rest, deleted when it does not match
  (is string= (format nil "Ann,Auckland~%Dan,Auckland~%")
      (list-records-after "C-x / t C-x / k 1 RET ^ a u c k RET" *headerless-records*))
  (is string= (format nil "Eve,Wellington~%")
      (list-records-after "C-x / t C-x / d $ 1 RET a u c k RET" *headerless-records*))
  ;; the columns offered by number, the first record's values beside them, never as names
  (multiple-value-bind (f editor) (run-keys "M-x c s v - m o d e RET C-x / t C-x / k E v e RET" :text *headerless-records* :width 100)
    (is string= *headerless-records* (session-text editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "First line a record like the rest"))
    (true (tuatara.frontend.fake:fake-frame-showing f "No such column: Eve (a number from 0)")))
  ;; hidden by filters like the rest; the same key again keeps it shown as the header
  (let ((before nil))
    (with-records (f buffer path) ((list "C-x / t" "C-x / f 1 RET ^ a u c k RET" :tick
                                         (lambda () (setf before (records-shown (tuatara.command:current-buffer))))
                                         "C-x / t" :tick)
                                   :text *headerless-records*)
      (is equal '("Ann,Auckland" "Dan,Auckland") before)
      (is equal '("Eve,Wellington" "Ann,Auckland" "Dan,Auckland") (records-shown buffer))
      (true (tuatara.frontend.fake:fake-frame-showing f "First line the header, always shown"))))
  ;; *csv-header* nil for every CSV buffer; its first line no name in the mode line
  (let ((tuatara-user::*csv-header* nil))
    (multiple-value-bind (f editor) (run-keys "M-x c s v - m o d e RET C-x / k 1 RET W RET" :text *headerless-records* :width 100)
      (is string= (format nil "Eve,Wellington~%") (session-text editor))
      (false (tuatara.frontend.fake:fake-frame-showing f "col 0 Eve")))))

(define-test (search-filters first-line-a-header)
  ;; a log's first line is a record; C-x / t makes it a header, shown and kept whatever the pattern
  (let ((log (format nil "time level message~%10:00 ERROR disk~%10:01 INFO up~%10:02 ERROR net~%")))
    (is string= (format nil "10:00 ERROR disk~%10:02 ERROR net~%")
        (list-records-after "C-x / k RET E R R O R RET" log :mode "log-mode"))
    (is string= (format nil "time level message~%10:00 ERROR disk~%10:02 ERROR net~%")
        (list-records-after "C-x / t C-x / k RET E R R O R RET" log :mode "log-mode"))
    (is string= (format nil "time level message~%10:01 INFO up~%")
        (list-records-after "C-x / t C-x / d RET E R R O R RET" log :mode "log-mode"))
    (with-records (f buffer path) ((list "C-x / t" "C-x / s I N F O RET" :tick) :type "log" :text log)
      (is equal '("time level message" "10:01 INFO up") (records-shown buffer))))
  ;; not a file of records: said, nothing changed
  (multiple-value-bind (f editor) (run-keys "C-x / t" :text "a b c" :width 100)
    (is string= "a b c" (session-text editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "Not a file of records (a CSV, a log): toggle-list-header was not run"))))

(defun by-column (mode text column pattern &optional (keys "C-x / k"))
  "TEXT in MODE after KEYS (keep records) on COLUMN with PATTERN."
  (list-records-after (format nil "~a ~a RET ~a RET" keys (typed-keys column) (typed-keys pattern)) text :mode mode))

(define-test (search-filters columns-of-every-record-file)
  ;; a log by its words, $1 the second; named by its header once C-x / t makes the first line one
  (let ((log (format nil "10:00 ERROR disk~%10:01 INFO up ERROR~%10:02 ERROR net~%")))
    (is string= (format nil "10:00 ERROR disk~%10:02 ERROR net~%") (by-column "log-mode" log "$1" "^ERROR$"))
    (is string= (format nil "time level message~%10:01 INFO up ERROR~%")
        (list-records-after (format nil "C-x / t C-x / k l e v e l RET ~a RET" (typed-keys "^INFO$"))
                            (format nil "time level message~%~a" log) :mode "log-mode")))
  ;; the password file by its seven names, each offered with the first record's value
  (multiple-value-bind (f editor) (run-keys "M-x e t c - p a s s w d - g e n e r i c - m o d e RET C-x / k" :text (format nil "root:x:0:0:root:/root:/bin/bash~%") :width 100 :height 14)
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "$6 /bin/bash")))
  (is string= (format nil "bob:x:1000:1000:Bob:/home/bob:/bin/zsh~%")
      (by-column "etc-passwd-generic-mode" (format nil "root:x:0:0:root:/root:/bin/bash~%bob:x:1000:1000:Bob:/home/bob:/bin/zsh~%") "shell" "zsh$"))
  ;; hosts by address and name, the other names by number
  (is string= (format nil "127.0.0.1 localhost~%")
      (by-column "hosts-generic-mode" (format nil "127.0.0.1 localhost~%10.0.0.2 nas nas.lan~%") "name" "^nas$" "C-x / d"))
  (is string= (format nil "10.0.0.2 nas nas.lan~%")
      (by-column "hosts-generic-mode" (format nil "127.0.0.1 localhost~%10.0.0.2 nas nas.lan~%") "$2" "lan$"))
  ;; .env: export left out, quotes and a comment after them taken off
  (let ((env (format nil "export A=1~%B=\"two words\" # note~%C = 3 # three~%")))
    (is string= (format nil "B=\"two words\" # note~%") (by-column "dotenv-mode" env "value" "^two words$"))
    (is string= (format nil "C = 3 # three~%") (by-column "dotenv-mode" env "value" "^3$"))
    (is string= (format nil "export A=1~%") (by-column "dotenv-mode" env "name" "^A$")))
  ;; requirements by name, version and markers
  (let ((reqs (format nil "requests>=2.0~%numpy==1.26 ; python_version>'3'~%flask~%")))
    (is string= (format nil "numpy==1.26 ; python_version>'3'~%") (by-column "pip-requirements-mode" reqs "version" "^=="))
    (is string= (format nil "numpy==1.26 ; python_version>'3'~%") (by-column "pip-requirements-mode" reqs "markers" "python"))
    (is string= (format nil "flask~%") (by-column "pip-requirements-mode" reqs "name" "^flask$")))
  ;; todo.txt by its parts and its tags' keys
  (let ((todo (format nil "x 2026-10-01 2026-09-30 pay rent +home~%(A) 2026-10-02 call mum @phone due:2026-10-20~%buy milk +home @shops~%")))
    (is string= (format nil "x 2026-10-01 2026-09-30 pay rent +home~%buy milk +home @shops~%") (by-column "todotxt-mode" todo "project" "^home$"))
    (is string= (format nil "(A) 2026-10-02 call mum @phone due:2026-10-20~%") (by-column "todotxt-mode" todo "priority" "A"))
    (is string= (format nil "(A) 2026-10-02 call mum @phone due:2026-10-20~%") (by-column "todotxt-mode" todo "due" "10-20$"))
    (is string= (format nil "x 2026-10-01 2026-09-30 pay rent +home~%") (by-column "todotxt-mode" todo "created" "^2026-09"))
    (is string= (format nil "x 2026-10-01 2026-09-30 pay rent +home~%") (by-column "todotxt-mode" todo "done" "x"))
    (is string= (format nil "buy milk +home @shops~%") (by-column "todotxt-mode" todo "text" "^buy")))
  ;; JSON Lines by key, a path of keys, a string's escapes undone; a line that is no JSON has no value
  (let ((json (format nil "{\"level\":\"error\",\"user\":{\"id\":7}}~%{\"level\":\"info\",\"msg\":\"caf\\u00e9\"}~%not json~%")))
    (is string= (format nil "{\"level\":\"error\",\"user\":{\"id\":7}}~%") (by-column "jsonl-mode" json "level" "^error$"))
    (is string= (format nil "{\"level\":\"error\",\"user\":{\"id\":7}}~%") (by-column "jsonl-mode" json "user.id" "^7$"))
    (is string= (format nil "{\"level\":\"info\",\"msg\":\"caf\\u00e9\"}~%") (by-column "jsonl-mode" json "msg" "\\x{e9}$")))
  ;; shown by a column, the filter named by it
  (with-records (f buffer path) ((list "C-x / f l e v e l RET e r r RET" :tick) :type "jsonl"
                                 :text (format nil "{\"level\":\"error\"}~%{\"level\":\"info\"}~%"))
    (is equal '("{\"level\":\"error\"}") (records-shown buffer))
    (is equal '("level: err") (mapcar #'tuatara.defaults::search-filter-words (tuatara.defaults::search-filters buffer))))
  ;; a .gitignore has no columns: the whole line, asked at once
  (is string= (format nil "build/~%") (list-records-after "C-x / k b u i l d RET" (format nil "build/~%*.o~%") :mode "gitignore-mode")))

(define-test (search-filters columns-asked-before-any-grammar-question)
  ;; the first file of a language whose grammar is off asks whether to turn it on in a session
  ;; typing its own keys; the keys of these sessions must reach the column question
  (dolist (mode '("todotxt-mode" "hosts-generic-mode" "pip-requirements-mode"))
    (setf tuatara.defaults::*grammars-not-now* '()
          tuatara.defaults::*grammar-answers-read* nil)
    (is string= (format nil "keep~%")
        (list-records-after "C-x / k RET k e e p RET" (format nil "keep~%drop~%") :mode mode))))

(define-test (search-filters header-lock-in-a-log)
  (let ((log (format nil "time level message~%~{10:~2,'0d INFO line ~:*~d~%~}" (loop for i below 40 collect i))))
    ;; no header yet: said, nothing kept
    (with-records (f buffer path) ((list "C-x / L") :type "log" :text log :height 10)
      (true (screen-showing f "No header: C-x / t makes the first line one"))
      (false (tuatara.buffer:buffer-local buffer :header-line)))
    ;; once C-x / t makes the first line a header, C-x / L keeps it at the top as the rest scrolls
    (with-records (f buffer path) ((list "C-x / t C-x / L M->") :type "log" :text log :height 10)
      (is string= "time level message" (tuatara.frontend.fake:fake-screen-line f 0))
      (true (tuatara.frontend.fake:fake-frame-showing f "Header kept at the top")))
    ;; *list-header-lock*: kept as soon as C-x / t makes one; C-x / t again lets it go with the header
    (let ((tuatara-user:*list-header-lock* t))
      (with-records (f buffer path) ((list "C-x / t M->") :type "log" :text log :height 10)
        (is string= "time level message" (tuatara.frontend.fake:fake-screen-line f 0)))
      (with-records (f buffer path) ((list "C-x / t C-x / t M->") :type "log" :text log :height 10)
        (false (tuatara.buffer:buffer-local buffer :header-line))))
    ;; another mode lets go of a log's filters, its lock and its header
    (with-records (f buffer path) ((list "C-x / t C-x / L C-x / s E R R O R RET" :tick "M-x t e x t - m o d e RET") :type "log" :text log :height 10)
      (false (tuatara.defaults::search-filters buffer))
      (false (tuatara.display:hidden-bounds buffer))
      (false (tuatara.buffer:buffer-local buffer :header-line))
      (is eq :mode (tuatara.buffer:buffer-local buffer :list-header :mode)))
    ;; not a file of records: said
    (multiple-value-bind (f editor) (run-keys "C-x / L" :text "a b c" :width 100)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "Not a file of records (a CSV, a log): toggle-list-header-lock was not run")))))

(define-test (search-filters columns-of-lists)
  ;; a table list asks for a column, by its title, the first row's value beside it, and tries the cell alone
  (let ((made '()))
    (flet ((forget-list ()
             (let ((list (tuatara.buffer:find-buffer "*Buffers*")))
               (when list (tuatara.buffer:kill-buffer list)))))
     (forget-list)
     (unwind-protect
         (multiple-value-bind (f editor)
             (run-keys (list (lambda ()
                               (dolist (name '("alpha-notes" "beta" "gamma"))
                                 (let ((b (tuatara.buffer:make-buffer name)))
                                   (push b made)
                                   (tuatara.buffer:buffer-insert b 0 (if (string= name "beta") "alpha inside" "text")))))
                             "C-x C-b C-x / f N a m e RET ^ a l p h a RET" :tick)
                       :width 120 :height 30)
           (declare (ignore editor))
           (let* ((list (tuatara.buffer:find-buffer "*Buffers*"))
                  (shown (shown-lines list)))
             (true (find-if (lambda (line) (search "alpha-notes" line)) shown))
             (false (find-if (lambda (line) (search " beta " line)) shown))
             (false (find-if (lambda (line) (search "gamma" line)) shown))
             (is equal '("Name: ^alpha") (mapcar #'tuatara.defaults::search-filter-words (tuatara.defaults::search-filters list)))
             ;; a column's filter lights nothing: what it matches in other columns is no match of it
             (setf tuatara.defaults::*search-lit* nil)
             (tuatara:light-search-filters list list)
             (false tuatara.defaults::*search-lit*)
             (true (tuatara.frontend.fake:fake-frame-showing f "Filter by the column:"))
             ;; the columns offered: those with a title
             (is equal '("MR" "Name" "Size" "Mode" "Filename/Process")
                     (mapcar #'first (tuatara.defaults::list-table-columns list)))))
      (dolist (b made) (when (tuatara.buffer:live-buffer-p b) (tuatara.buffer:kill-buffer b)))
      (forget-list))))
  ;; the package manager: by its State column
  (let ((tuatara.defaults::*declared-packages* '(("kea" "https://example.nz/kea" nil) ("tui" "https://example.nz/tui" nil))))
    (multiple-value-bind (f editor) (run-keys "M-x p a c k a g e - m a n a g e r RET C-x / h N a m e RET k e a RET" :width 160 :height 20)
      (declare (ignore f editor))
      (let ((list (find-if (lambda (b) (search "ackage" (tuatara.buffer:buffer-name b))) tuatara.buffer:*buffers*)))
        (true list)
        (when list
          (let ((shown (shown-lines list)))
            (true (find-if (lambda (line) (search "tui" line)) shown))
            (false (find-if (lambda (line) (search " kea " line)) shown))))))))

(define-test (search-filters lit-with-their-own-groups)
  ;; filters lit together each keep their own groups: the second's \1 is its own first
  (multiple-value-bind (f editor list) (filtered-session "C-x / s ( r ) \\ 1 RET C-x / s ( e ) \\ 1 RET")
    (declare (ignore f editor))
    (let ((other (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "lit") :text "cherry and beet")))
      (tuatara:light-search-filters list other)
      (let ((pattern (tuatara.defaults::search-state-string (car tuatara.defaults::*search-lit*))))
        (is string= "(?i:(r)\\1)|(?i:(e)\\2)" pattern)
        (is equal '("rr" "ee") (mapcar #'third (regex-oracle "cherry and beet" pattern))))
      (setf tuatara.defaults::*search-lit* nil tuatara.defaults::*search-lit-shown* nil)
      (tuatara.buffer:kill-buffer other)))
  (is string= "(?<y>a)\\3\\\\1" (tuatara.defaults::shift-group-references "(?<y>a)\\1\\\\1" 2))
  ;; a set and \Q...\E hold no references, and a number past the groups is a character
  (is string= "[\\1](a)\\2\\Q\\1\\E\\12" (tuatara.defaults::shift-group-references "[\\1](a)\\1\\Q\\1\\E\\12" 1))
  ;; literal text lit as itself, letters beyond ASCII too
  (let ((pattern (tuatara.defaults::search-quote "café (x)" t :parsed nil)))
    (true (tuatara.syntax:compile-regex pattern))
    (is equal '("café (x)") (mapcar #'third (regex-oracle "a café (x) b" pattern)))))

;;; What a filter of records leaves, held against one scan of the whole text

(defun filter-units (rope crlf ends)
  "Every record of ROPE, a line each or as ENDS ends them (RECORD-ENDS), as
\(START TEXT-END AFTER): where it starts, where its text ends (before a
carriage return too with CRLF) and where the next starts."
  (let ((total (tuatara.text:rope-bytes rope)))
    (loop for at = 0 then after
          while (< at total)
          for (text-end after) = (multiple-value-list (tuatara.defaults::record-bounds rope at ends))
          collect (list at (tuatara.defaults::record-text-end rope text-end crlf) after))))

(defun scan-kept-starts (rope matcher crlf &key ends ranges hide)
  "The starts of the records of ROPE a filter by MATCHER leaves, worked out
from one scan of the whole text: a record is matched when any part of a
match lies in it, its line end included, or an empty match is in it no
further on than the end of its text. Only the records starting in RANGES, a
list of (START END), when given; with HIDE, those not matched."
  (let ((matches (tuatara.defaults::line-matches rope matcher t 0 (tuatara.text:rope-bytes rope))))
    (loop for (start text-end after) in (filter-units rope crlf ends)
          when (and (or (null ranges) (loop for (a b) in ranges thereis (<= a start (1- b))))
                    (eq (not hide)
                        (loop for (s e) in matches
                              thereis (if (= s e)
                                          (<= start s text-end)
                                          (and (< s after) (>= (1- e) start))))))
            collect start)))

(defun runs-starts (rope runs crlf ends)
  "The starts of the records of ROPE that RUNS (LINES-MATCHING) hold."
  (loop for (start) in (filter-units rope crlf ends)
        when (loop for i from 0 below (length runs) by 2
                   thereis (<= (aref runs i) start (1- (aref runs (1+ i)))))
          collect start))

(defun runs-of (ranges)
  "RANGES, a list of (START END), as LINES-MATCHING takes them."
  (coerce (loop for (a b) in ranges collect a collect b) '(simple-array fixnum (*))))

(defun filter-scan-agrees (text pattern &key crlf ends ranges hide (everywhere nil))
  "True when a filter for PATTERN on TEXT leaves the records one scan of the
whole text finds (SCAN-KEPT-STARTS), and counts them; with EVERYWHERE, worked
out in small pieces on all but two cores. Else what each found, for the
report."
  (let* ((rope (tuatara.text:string->rope text))
         (filter (tuatara.defaults::make-search-filter pattern :crlf crlf :hide hide))
         (matcher (tuatara.defaults::search-filter-matcher filter))
         (ranges (or ranges (list (list 0 (tuatara.text:rope-bytes rope)))))
         (oracle (scan-kept-starts rope matcher crlf :ends ends :ranges ranges :hide hide)))
    (multiple-value-bind (runs count)
        (if everywhere
            (let ((tuatara.defaults::*filter-piece* 700))
              (tuatara.defaults::lines-matching-everywhere filter rope (runs-of ranges) crlf nil nil nil ends))
            (tuatara.defaults::lines-matching (list filter) rope (runs-of ranges) crlf :ends ends))
      (let ((found (runs-starts rope runs crlf ends)))
        (or (and (equal oracle found) (= count (length oracle)))
            (list :pattern pattern :scan oracle :filter found :count count))))))

(defparameter *crossing-filter-patterns*
  '("[^,]*login" "\\s+2026" "\\W2026" "\\s+via" "(?:[A-Za-z0-9+/]{4}\\s*){10,}=*")
  "Patterns whose matches run over line ends, held against one scan whole and in pieces.")

(defparameter *filter-agreement-patterns*
  (append *agreement-patterns*
          '("^$" "$" "^" "x*" "a\\s" "\\s" "\\z" "\\A2026" "(?<=\\n)2026" "(?<!\\d)\\d{2}$" "\\r" "\\n")
          *crossing-filter-patterns*)
  "The patterns filters are held against one scan with: every kind searched
for, anchors, empty matches, line ends, and matches running over them.")

(defparameter *line-bound-filter-patterns*
  '("login" "ERROR" "[0-9]+" "\\d$" "^2026-0[1-6]" "^$" "$" "(?<=user=)\\w+" "kōrero" "x*")
  "Patterns whose matches stay within a line, for filters worked out in pieces.")

(define-test (search-filters filters-agree-with-a-scan)
  ;; a filter of records leaves the lines in which a scan of the whole text finds a match,
  ;; LF and CRLF, as the search finds it: the same lines a search lights
  (with-evidence-seed
    (dolist (crlf '(nil t))
      (let ((text (evidence-text 300 :crlf crlf)))
        (dolist (pattern *filter-agreement-patterns*)
          (is eq t (filter-scan-agrees text pattern :crlf crlf)))
        (dolist (pattern '("login" "\\d$" "^$" "a\\s"))
          (is eq t (filter-scan-agrees text pattern :crlf crlf :hide t)))
        (dolist (pattern (append *line-bound-filter-patterns* *crossing-filter-patterns*))
          (is eq t (filter-scan-agrees text pattern :crlf crlf :everywhere t)))
        ;; lines a filter before left: the runs it leaves, others hidden
        (let* ((rope (tuatara.text:string->rope text))
               (runs (tuatara.defaults::lines-matching (list (tuatara.defaults::make-search-filter "e" :crlf crlf)) rope
                                                      (runs-of (list (list 0 (tuatara.text:rope-bytes rope)))) crlf))
               (ranges (loop for i from 0 below (length runs) by 2 collect (list (aref runs i) (aref runs (1+ i))))))
          (dolist (pattern *line-bound-filter-patterns*)
            (is eq t (filter-scan-agrees text pattern :crlf crlf :ranges ranges))
            (is eq t (filter-scan-agrees text pattern :crlf crlf :ranges ranges :everywhere t)))))))
  ;; records over several lines, quoted line breaks in a CSV: a record matched anywhere is
  ;; left whole, its line breaks part of it
  (dolist (crlf '(nil t))
    (let ((text (format nil (if crlf "n,a~c~%kiwi,\"one~c~%two\"~c~%tui,\"x~c~%kea\"~c~%weka,y~c~%" "n,a~*~%kiwi,\"one~*~%two\"~*~%tui,\"x~*~%kea\"~*~%weka,y~*~%")
                        #\Return #\Return #\Return #\Return #\Return #\Return)))
      (dolist (pattern '("kiwi" "two" "kea" "^kea" "one\\s+two" "\\n" "y$" "$" "^" "zzz"))
        (is eq t (filter-scan-agrees text pattern :crlf crlf :ends #'tuatara.defaults::csv-record-end))
        (is eq t (filter-scan-agrees text pattern :crlf crlf :ends #'tuatara.defaults::csv-record-end :hide t))
        (is eq t (filter-scan-agrees text pattern :crlf crlf :ends #'tuatara.defaults::csv-record-end :everywhere t))))))

(defun kept-line-numbers (text runs)
  "The numbers (from 0) of the lines of TEXT that RUNS (LINES-MATCHING) hold."
  (let ((starts (cons 0 (loop for i from 0 below (1- (length text)) when (char= #\Newline (char text i)) collect (1+ i)))))
    (loop for start in starts for n from 0
          when (loop for i from 0 below (length runs) by 2 thereis (<= (aref runs i) start (1- (aref runs (1+ i)))))
            collect n)))

(define-test (search-filters lines-a-match-runs-over)
  ;; a match running over a line end leaves every line any part of it lies on: whole, in
  ;; pieces cut inside the match, a megabyte at a time, LF and CRLF, and in a list
  (dolist (crlf '(nil t))
    (flet ((text (&rest lines) (format nil "~{~a~:[~*~;~c~]~%~}" (loop for line in lines collect line collect crlf collect #\Return))))
      (dolist (case (list (list "[^,]*foo" (text "a,b" "foo,c" "zz") '(0 1))
                          (list "\\s+foo" (text "a,b" "foo,c" "zz") '(0 1))
                          (list "\\Wfoo" (text "a,b" "foo,c" "zz") '(0 1))
                          (list "1\\r?\\ny" (text "h1,h2" "x,1" "y,2" "z,3") '(1 2))
                          (list "\\nz" (text "h1,h2" "x,1" "y,2" "z,3") '(2 3))
                          (list "(?:[A-Za-z0-9+/]{4}\\s*){12,}=*"
                                (text "head,1" "S2lhIG9yYSBrb3V0b3Uga2F0b2Es" "IHRoZSBrZWEgaXMgYSBwYXJyb3Q=" "tail,2") '(1 2))))
        (destructuring-bind (pattern string lines) case
          (let* ((rope (tuatara.text:string->rope string))
                 (filter (tuatara.defaults::make-search-filter pattern :crlf crlf))
                 (all (runs-of (list (list 0 (length string))))))
            (is equal lines (kept-line-numbers string (tuatara.defaults::lines-matching (list filter) rope all crlf)) "~a" pattern)
            (is equal lines (kept-line-numbers string (tuatara.defaults::lines-matching (list filter) rope all crlf :most 10)))
            (let ((tuatara.defaults::*filter-piece* 1))
              (is equal lines (kept-line-numbers string (tuatara.defaults::lines-matching-everywhere filter rope all crlf nil nil nil))
                  "~a in pieces" pattern))
            (is eq t (filter-scan-agrees string pattern :crlf crlf))
            ;; hidden: the lines no part of a match lies on
            (is equal (sort (set-difference (loop for n from 0 below (count #\Newline string) collect n) lines) #'<)
                (sort (kept-line-numbers string (tuatara.defaults::lines-matching
                                                 (list (tuatara.defaults::make-search-filter pattern :crlf crlf :hide t)) rope all crlf))
                      #'<))
            ;; a list's lines, matched together
            (let ((list-lines (coerce (butlast (uiop:split-string (remove #\Return string) :separator '(#\Newline))) 'simple-vector)))
              (is equal lines (loop for bit across (tuatara.defaults::list-lines-matched filter list-lines 0 (length list-lines)
                                                                                         (constantly t))
                                    for n from 0 when (= bit 1) collect n)
                  "~a in a list" pattern))))))))

(define-test (search-filters list-total-counted-once)
  ;; how many entries a list has is counted once for the list made, not at every key
  (let* ((filtering (tuatara.defaults::make-list-filtering))
         (two-lines (list "b"))
         (source (tuatara.defaults::make-list-source "" nil nil (vector '(:section "A") "a" two-lines two-lines "c") 0))
         (looked 0))
    (is = 3 (tuatara.defaults::listed-entry-count filtering source))
    (sb-int:encapsulate 'tuatara.defaults::heading-entry-p 'counted
                        (lambda (function &rest arguments) (incf looked) (apply function arguments)))
    (unwind-protect (is = 3 (tuatara.defaults::listed-entry-count filtering source))
      (sb-int:unencapsulate 'tuatara.defaults::heading-entry-p 'counted))
    (is = 0 looked)))

(defun least-seconds (function)
  "The least of three runs' seconds of FUNCTION, called with none."
  (loop repeat 3
        minimize (let ((start (thread-cpu-time)))
                   (funcall function)
                   (/ (- (thread-cpu-time) start) internal-time-units-per-second))))

(define-test (search-filters filters-at-search-speed)
  ;; a filter of a large log finds its lines as fast as a search finds its matches, not
  ;; reading every line as text: within a few times one scan's time
  (let* ((text (with-output-to-string (o)
                 (dotimes (i 120000)
                   (format o "2026-10-01 10:~2,'0d:~2,'0d ~a request ~d took ~dms~%"
                           (mod i 60) (mod (* 7 i) 60) (if (zerop (mod i 97)) "ERROR" "INFO") i (mod (* 13 i) 1000)))))
         (rope (tuatara.text:string->rope text))
         (all (runs-of (list (list 0 (tuatara.text:rope-bytes rope))))))
    (dolist (pattern '("ERROR" "error" "took 9\\d\\dms"))
      (let* ((filter (tuatara.defaults::make-search-filter pattern))
             (matcher (tuatara.defaults::search-filter-matcher filter))
             (scan (least-seconds (lambda ()
                                    (let ((n 0))
                                      (tuatara.defaults::map-matcher-matches (lambda (s e m g) (declare (ignore s e m g)) (incf n) nil)
                                                                             rope matcher 0 (tuatara.text:rope-bytes rope))
                                      n))))
             (filter-time (least-seconds (lambda () (tuatara.defaults::lines-matching (list filter) rope all nil)))))
        (true (< filter-time (+ 0.002 (* 3 scan))) "~a: filtered in ~,4fs, scanned in ~,4fs" pattern filter-time scan)))))

(define-test (search-filters json-path-cut-once)
  ;; a JSON Lines filter by a path of keys cuts the path once, not on every line
  (let* ((fields (tuatara.defaults::mode-record-fields (make-instance 'tuatara.defaults::jsonl-mode) nil))
         (each (tuatara.defaults::record-fields-each fields))
         (line "{\"user\": {\"id\": 7, \"name\": \"Hēmi \\\"H\\\"\"}}")
         (cuts 0))
    (sb-int:encapsulate 'uiop:split-string 'counted (lambda (function &rest arguments) (incf cuts) (apply function arguments)))
    (unwind-protect
         (progn
           (dotimes (i 100)
             (is string= "7" (funcall each line (length line) "user.id" (lambda (s a z) (subseq s a z)))))
           (dotimes (i 100)
             (is string= "Hēmi \"H\"" (funcall each line (length line) "user.name" (lambda (s a z) (subseq s a z))))))
      (sb-int:unencapsulate 'uiop:split-string 'counted))
    (true (<= cuts 2) "two paths cut ~d times for 200 lines" cuts)
    (false (funcall each "{\"user\": " 9 "user.id" (lambda (s a z) (subseq s a z))))))

(defun json-line-value-read-whole (text field)
  "What a JSON Lines line TEXT gives for FIELD when the whole line is read as
JSON and the value looked up in what is read: the oracle of the JSON Lines filters."
  (let* ((object (handler-case (tuatara.text:json-read text) (tuatara.text:json-error () nil)))
         (value (and (hash-table-p object)
                     (multiple-value-bind (whole found) (gethash field object)
                       (if found
                           (tuatara.text:json-get whole)
                           (apply #'tuatara.text:json-get object (uiop:split-string field :separator ".")))))))
    (and value (if (stringp value) value (tuatara.text:json-write value)))))

(define-test (search-filters json-column-in-parallel)
  ;; a JSON Lines filter by a path of keys, worked out in pieces on all but two cores at once, tries
  ;; each line's own value: the same lines a line-by-line reading keeps, however the pieces start
  (let* ((text (with-output-to-string (o)
                 (dotimes (i 400)
                   (format o "{\"user\": {\"id\": ~d}, \"name\": \"~:[weka~;kiwi~]\"}~%" i (zerop (mod i 5))))))
         (rope (tuatara.text:string->rope text))
         (each (tuatara.defaults::record-fields-each
                (tuatara.defaults::mode-record-fields (make-instance 'tuatara.defaults::jsonl-mode) nil)))
         (filter (tuatara.defaults::make-search-filter "kiwi|^1" :test (tuatara.defaults::field-line-matcher each "user.id")))
         (oracle (loop for line in (uiop:split-string (string-right-trim '(#\Newline) text) :separator '(#\Newline))
                       count (cl-ppcre:scan "kiwi|^1" (json-line-value-read-whole line "user.id"))))
         (count nil))
    ;; the path is slow to cut, so every piece asks for it before the first has it
    (sb-int:encapsulate 'uiop:split-string 'slowed (lambda (function &rest arguments) (sleep 0.05) (apply function arguments)))
    (unwind-protect
         (let ((tuatara.defaults::*filter-piece* 300))
           (setf count (nth-value 1 (tuatara.defaults::lines-matching-everywhere
                                     filter rope (runs-of (list (list 0 (tuatara.text:rope-bytes rope)))) nil nil nil nil))))
      (sb-int:unencapsulate 'uiop:split-string 'slowed))
    (is = 111 oracle)
    (is = oracle count)))

(define-test (search-filters json-value-found-in-place)
  ;; a JSON Lines field is what reading the whole line finds, for every kind of value and line;
  ;; a string or a whole number is found in the line itself, nothing made for it
  (let* ((each (tuatara.defaults::record-fields-each
                (tuatara.defaults::mode-record-fields (make-instance 'tuatara.defaults::jsonl-mode) nil)))
         (lines (list "{\"id\": 7, \"name\": \"Hēmi\", \"ok\": true, \"no\": false, \"gone\": null}"
                      "{\"id\": -12, \"name\": \"tab\\there \\\"q\\\" \\u0041\\u00e9\\ud83d\\ude00 \\ud800x\"}"
                      "{\"id\": 1.50, \"size\": 2e3, \"neg\": -0, \"big\": 12345678901234567890}"
                      "{\"user\": {\"id\": 3, \"name\": {\"first\": \"Aroha\"}, \"tags\": [1, \"two\", {\"id\": 9}]}}"
                      "{\"user.id\": \"dotted key\", \"user\": {\"id\": 4}}"
                      "{\"user.id\": null, \"user\": {\"id\": 5}}"
                      "{\"id\": 1, \"id\": 2, \"user\": {\"id\": 6}, \"user\": 7}"
                      "{\"user\": 7, \"user\": {\"id\": 8}}"
                      "{\"n\\u0061me\": \"escaped key\", \"list\": [], \"obj\": {}, \"arr\": [1, [2, 3], {\"a\": null}]}"
                      "  {\"id\" : 10 ,\"name\":\"spaced\"}  "
                      "[1, 2, 3]" "7" "\"text\"" "" "{" "{\"id\": 1,}" "{\"id\": 01}" "{\"id\": 1} x"
                      "{\"id\": tru}" "{\"name\": \"bad \\x escape\"}" "{\"id\": 1.}" "{\"id\": -}" "{\"id\": 1e}"
                      (format nil "{\"name\": \"control ~c\"}" (code-char 1))
                      (format nil "{\"deep\": ~a1~a}" (make-string 600 :initial-element #\[) (make-string 600 :initial-element #\]))
                      (format nil "{\"long\": ~a}" (make-string 1200 :initial-element #\7))))
         (fields '("id" "name" "ok" "no" "gone" "size" "neg" "big" "user" "user.id" "user.name.first" "user.tags"
                   "user.name" "list" "obj" "arr" "deep" "long" "missing" "user.missing" "" "id.x")))
    (dolist (line lines)
      (dolist (field fields)
        (is equal (json-line-value-read-whole line field)
            (funcall each line (length line) field (lambda (s a z) (subseq s a z)))
            "~s of ~s" field line)))
    (let ((line "{\"id\": 1234, \"name\": \"Aroha Ngata\", \"user\": {\"id\": 42, \"city\": \"Ōtautahi\"}, \"ok\": true}"))
      (dolist (field '("id" "name" "user.id" "user.city" "ok"))
        (let ((made (least-bytes (lambda () (funcall each line (length line) field (lambda (s a z) (declare (ignore s a z)) t))) 1000)))
          (true (< made 16) "~,1f octets made for ~a of a line" made field))))))

(define-test (search-filters group-named-like-a-filter-refused)
  ;; a group whose name begins as a filter's mark does is refused in words, as
  ;; any pattern that cannot be read is, not a failure of the search
  (dolist (pattern (list (format nil "(?<~ax>)" tuatara.syntax::*filter-mark*)
                         (format nil "(?<~aiban-x>)" tuatara.syntax::*filter-mark*)
                         (format nil "(?<~adate-zz>)" tuatara.syntax::*filter-mark*)))
    (let ((words (handler-case (progn (tuatara.syntax:compile-regex pattern) nil)
                   (tuatara.syntax:pattern-refused (e) (tuatara.syntax:pattern-refused-words e)))))
      (true (and words (search "may not be named" words)) "~s: ~s" pattern words))))
