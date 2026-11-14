;;;; lisp/search/saved-patterns.lisp --- Patterns saved under a name, [[:name:]] in every search, kept between
;;;; sessions.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A pattern typed once, a regular expression or literal text, is saved
;;;; under a name and stands for itself as [[:name:]] in every search after,
;;;; as the named patterns of the init file do (DEFINE-NAMED-PATTERN). The
;;;; saved ones are a list kept between sessions in the state directory, each
;;;; with its kind: literal text is put in with its special characters
;;;; escaped, so that it matches only itself.

(in-package #:tuatara.defaults)

(defvar *saved-patterns-most* 1000
  "How many saved patterns there may be before saving another says there are many.")

(defun parse-saved-pattern-line (line)
  "The saved pattern LINE writes, (NAME KIND PATTERN WORDS), KIND :REGEX or
:LITERAL, or NIL for a line that makes no sense. A tab ends each field, each
escaped (ESCAPE-PATH): the name, regex or literal, the pattern and what it
finds."
  (let ((fields (state-line-fields line)))
    (when (<= 3 (length fields))
      (destructuring-bind (name kind pattern &optional (words "") &rest later) fields
        (declare (ignore later))
        (let ((kind (cdr (assoc kind '(("regex" . :regex) ("literal" . :literal)) :test #'string=))))
          (and kind (cl-ppcre:scan "^[a-z][a-z0-9]*$" name)
               (list name kind (unescape-path pattern) (unescape-path words))))))))

(defun saved-pattern-line (saved)
  "The line of the file of saved patterns for SAVED, (NAME KIND PATTERN WORDS)."
  (destructuring-bind (name kind pattern words) saved
    (format nil "~a~c~(~a~)~c~a~c~a" name #\Tab kind #\Tab (escape-path pattern) #\Tab (escape-path words))))

(define-state-list *saved-patterns* "patterns" 'parse-saved-pattern-line 'saved-pattern-line '*saved-patterns-most*
  :header (format nil "# Patterns saved under a name, [[:name:]] in every search, the latest first. A~%~
                       # tab ends each field: the name, regex or literal, the pattern and what it~%~
                       # finds (\\\\, \\n and \\r stand for \\, a newline and a return).~%")
  :path-of #'first :class :kept :fields 4)

(defvar *names-of-saved-patterns* (make-hash-table :test 'equal)
  "The names a saved pattern stands under in this session (DEFINE-SAVED-PATTERN),
as against those the init file named itself, which saving and forgetting leave
alone.")

(defun define-saved-pattern (saved)
  "Make [[:NAME:]] stand for the pattern SAVED, (NAME KIND PATTERN WORDS),
holds: a regular expression as it is, literal text with its special
characters escaped; NAME kept among *NAMES-OF-SAVED-PATTERNS*."
  (destructuring-bind (name kind pattern words) saved
    (tuatara-user:define-named-pattern name (if (plusp (length words)) words pattern)
      (pattern-regex pattern (eq kind :regex)))
    (setf (gethash name *names-of-saved-patterns*) t)))

(defun init-named-pattern-p (name)
  "True when NAME names a pattern of your own that the init file defined, not
a saved one."
  (and (tuatara.syntax:own-named-pattern-p name)
       (not (gethash name *names-of-saved-patterns*))))

(define-record (saved-patterns-job (:include tuatara.runtime:job) (:constructor make-saved-patterns-job (buffer))
                                   (:copier nil))
  "The saved patterns being made to stand for themselves at start, a job:
FAILED, each (NAME . WHY), those that could not be."
  (failed '() :documentation "The saved patterns that could not be used, each (NAME . WHY)."))

(tuatara.runtime:define-job-words saved-patterns-job
  :at-exit :quiet
  :doing "Naming the saved patterns"
  :description "Naming the saved patterns"
  :verb "name the saved patterns"
  :left-words "not named")

(defun define-saved-patterns-work (job)
  "Make each saved pattern stand for itself as [[:NAME:]] (DEFINE-SAVED-PATTERN),
but those whose names the init file has given a pattern of its own, keeping
in JOB those that cannot be used."
  (dolist (saved (reverse (state-list-entries *saved-patterns*)))
    (tuatara.runtime:job-check-stop)
    (unless (tuatara.syntax:named-pattern-p (first saved))
      (handler-case (define-saved-pattern saved)
        (error (e) (push (cons (first saved) (princ-to-string e)) (saved-patterns-job-failed job)))))))

(defmethod tuatara.runtime:job-take-in ((job saved-patterns-job))
  "Say in *Messages* which saved patterns could not be used."
  (loop for (name . why) in (reverse (saved-patterns-job-failed job))
        do (message "The saved pattern [[:~a:]] is not used: ~a" name why)))

(defun define-saved-patterns ()
  "Make the saved patterns stand for themselves as [[:NAME:]] in every search,
in the background, so that putting them in never holds the start up
\(DEFINE-SAVED-PATTERNS-WORK). Returns the job."
  (tuatara.runtime:start-job-thread (make-saved-patterns-job (current-buffer)) #'define-saved-patterns-work
                                    :name "tuatara saved patterns"))

(define-command save-named-pattern ()
  "Save a pattern under a name, so that [[:NAME:]] stands for it in every
regular expression searched for after, in this session and the next: the
pattern is asked for as a filter's is, \\<pattern>\\[search-toggle-regex]
switching between a regular expression and literal text, then its name, small
letters and then digits too, and what it finds, in words. Literal text
matches only itself where the name is written. Saving a name saved already
replaces it, after asking; Tuatara's own names ([[:ipv4:]], [[:alpha:]] and
the others) and those your init file gives a pattern are refused. \\[forget-named-pattern] takes one away."
  (multiple-value-bind (pattern regex) (read-pattern "save as a named pattern")
    (if (zerop (length pattern))
        (message "No pattern: nothing saved")
        (let ((name (string-trim " " (read-string "Save it as [[:NAME:]], NAME: "))))
          (cond ((not (cl-ppcre:scan "^[a-z][a-z0-9]*$" name))
                 (message "A pattern's name is small letters, then digits too: nothing saved"))
                ((init-named-pattern-p name)
                 (message "[[:~a:]] is named by your init file: nothing saved" name))
                ((and (tuatara.syntax:named-pattern-p name) (not (gethash name *names-of-saved-patterns*)))
                 (message "[[:~a:]] is Tuatara's own: nothing saved" name))
                ((and (tuatara.syntax:named-pattern-p name)
                      (not (y-or-n-p* (format nil "[[:~a:]] is saved already: replace it?" name))))
                 (message "Nothing saved"))
                (t (let ((saved (list name (if regex :regex :literal) pattern
                                      (tuatara.text:one-line (read-string (format nil "What [[:~a:]] finds, in words: " name))))))
                     (handler-case (progn (define-saved-pattern saved)
                                          (state-list-put *saved-patterns* (list saved))
                                          (message "[[:~a:]] now stands for ~:[literal ~;~]~a" name regex pattern))
                       (error (e) (message "~a: nothing saved" e))))))))))

(define-command forget-named-pattern ()
  "Take away a pattern saved under a name (\\[save-named-pattern]), chosen from
those saved: [[:NAME:]] stands for nothing after, in this session and the
next. One your init file names as well keeps the init file's pattern."
  (let ((names (mapcar #'first (state-list-entries *saved-patterns*))))
    (if (null names)
        (message "No patterns are saved")
        (let ((name (completing-read "Forget the saved pattern: " names :require-match t)))
          (when (member name names :test #'string=)
            (state-list-drop *saved-patterns* (list name))
            (if (init-named-pattern-p name)
                (message "The saved [[:~a:]] forgotten; your init file's [[:~:*~a:]] stays" name)
                (progn (remhash name *names-of-saved-patterns*)
                       (tuatara.syntax:forget-own-named-pattern name)
                       (message "[[:~a:]] forgotten" name))))))))
