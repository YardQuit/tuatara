;;;; test/tools.lisp --- The scripts of tools/: the desktop entry, the development container, the AppImage.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test tools
  :description "The scripts of tools/ that build, install and set up Tuatara.")

(defun tools-script (name)
  "The native path of the script NAME in this repository's tools/."
  (uiop:native-namestring (asdf:system-relative-pathname :tuatara (format nil "tools/~a" name))))

(defun bash-output (script &rest arguments)
  "What bash prints running the text SCRIPT with ARGUMENTS as $1 and on."
  (uiop:run-program (list* "bash" "-c" script "_" arguments) :output :string :ignore-error-status t))

(define-test (tools desktop-entry-exec-line)
  ;; a path with characters a desktop entry's Exec= line treats specially: each
  ;; escaped as a quoted argument wants it after the line is read as a string
  (let* ((entry (tuatara.terminal::desktop-entry :application :program "/opt/50%/a\\b\"c`d$e f|g&h/tuatara"))
         (line (find-if (lambda (line) (uiop:string-prefix-p "Exec=" line))
                        (uiop:split-string entry :separator '(#\Newline)))))
    (is string= "Exec=\"/opt/50%%/a\\\\\\\\b\\\\\"c\\\\`d\\\\$e f|g&h/tuatara\" --gui %F" line))
  ;; no word with a control character in it, which would end the line
  (fail (tuatara.terminal::exec-argument (format nil "/opt/a~%Exec=evil")) 'tuatara.runtime:refusal))

(defun template-with-exec (name exec)
  "The template NAME of data/ as the scripts filled it before the program did,
its @EXEC@ replaced by EXEC."
  (let* ((text (uiop:read-file-string (asdf:system-relative-pathname :tuatara (format nil "data/~a" name))))
         (at (search "@EXEC@" text)))
    (concatenate 'string (subseq text 0 at) exec (subseq text (+ at 6)))))

(define-test (tools application-entries-as-before)
  ;; the application entry the program writes is, byte for byte, the one the scripts wrote: the packages'
  ;; and the AppImage's (tuatara), the AppImage installed (its path, a style), the project's own
  (is string= (template-with-exec "tuatara.desktop.in" "tuatara") (tuatara.terminal::desktop-entry :application))
  (is string= (template-with-exec "tuatara.desktop.in"
                                  "\"/home/k a/50%%/x\\\\\\\\y\\\\\"z\\\\`\\\\$w|q&r/tuatara\" --dark")
      (tuatara.terminal::desktop-entry :application :program "/home/k a/50%/x\\y\"z`$w|q&r/tuatara"
                                                    :arguments '("--dark")))
  (is string= (template-with-exec "tuatara.desktop.in" "\"/opt/my tools/tuatara-tui.sh\"")
      (tuatara.terminal::desktop-entry :application :program "/opt/my tools/tuatara-tui.sh")))

(defun desktop-entry-valid-p (path)
  "True when desktop-file-validate finds nothing wrong in the entry at PATH,
or is not installed here; what it says as a second value."
  (if (tuatara.io:find-program "desktop-file-validate")
      (multiple-value-bind (out error code)
          (uiop:run-program (list "desktop-file-validate" path) :output :string :error-output :output
                                                                :ignore-error-status t)
        (declare (ignore error))
        (values (and (zerop code) (zerop (length (string-trim '(#\Newline #\Space) out)))) out))
      (values t "")))

(define-test (tools application-entries-written)
  ;; both application entries, Tuatara's and the client's, written where asked, each valid, the client's
  ;; starting a client that falls back to an editor of its own
  (with-test-directory (directory)
    (let* ((dir (namestring (merge-pathnames "applications/" directory)))
           (paths (tuatara.terminal::write-application-entries dir "/opt/tuatara 1/tuatara" "--light")))
      (is equal (list (concatenate 'string dir "tuatara.desktop")
                      (concatenate 'string dir "tuatara-client.desktop"))
          paths)
      (dolist (path paths)
        (multiple-value-bind (valid said) (desktop-entry-valid-p path)
          (true valid "~a: ~a" path said)))
      (let ((client (file-text (second paths))))
        (true (search (format nil "~%Name=Tuatara (client)~%") client))
        (true (search (format nil "~%Exec=\"/opt/tuatara 1/tuatara\" --light --gui --client --fallback %F~%") client)))
      ;; asked by the environment, as the scripts ask: the directory there, the program and its words as arguments
      (let ((again (namestring (merge-pathnames "again/" directory))))
        (sb-posix:setenv "TUATARA_DESKTOP_ENTRIES" again 1)
        (let ((out (with-output-to-string (*standard-output*)
                     (is = 0 (tuatara.terminal::desktop-entries-child '("tuatara"))))))
          (true (search "tuatara-client.desktop" out)))
        (false (sb-posix:getenv "TUATARA_DESKTOP_ENTRIES") "the request goes with its answer")
        (is string= (tuatara.terminal::desktop-entry :client)
            (file-text (concatenate 'string again "tuatara-client.desktop")))))))

(define-test (tools desktop-entries-request-checked)
  ;; the request to write desktop entries is taken only for this program and option words, into a directory
  ;; named from the root: a file named on the command line with the variable left set never becomes a
  ;; program the desktop runs, and the program starts as it would
  (with-test-directory (directory)
    (let* ((dir (namestring (merge-pathnames "applications/" directory)))
           (evidence (namestring (put-file (merge-pathnames "attachment.exe" directory) "MZ"))))
      (flet ((asked (dir &rest arguments)
               (sb-posix:setenv "TUATARA_DESKTOP_ENTRIES" dir 1)
               (let* ((code :none)
                      (said (with-output-to-string (*error-output*)
                              (let ((*standard-output* (make-broadcast-stream)))
                                (setf code (tuatara.terminal::desktop-entries-child arguments))))))
                 (false (sb-posix:getenv "TUATARA_DESKTOP_ENTRIES") "the request goes, taken or not")
                 (values code said))))
        (dolist (refused (list (list dir evidence) (list "applications/" "tuatara")
                               (list dir "tuatara" evidence) (list dir "tuatara" "--dark" "~/x.exe")
                               (list dir)))
          (multiple-value-bind (code said) (apply #'asked refused)
            (false code "~s taken" refused)
            (true (search "TUATARA_DESKTOP_ENTRIES left alone" said))
            (false (probe-file (concatenate 'string dir "tuatara.desktop")))))
        (is eql 0 (asked dir "tuatara" "--dark"))
        (true (search (format nil "~%Exec=tuatara --dark --gui %F~%")
                      (file-text (concatenate 'string dir "tuatara.desktop"))))))
    ;; this program (the AppImage's path when it runs from one), or the launcher of the checkout it was
    ;; built in
    (let ((self (namestring (merge-pathnames "checkout/build/tuatara" directory)))
          (launcher (namestring (merge-pathnames "checkout/tools/tuatara-tui.sh" directory))))
      (dolist (file (list self launcher))
        (put-file (ensure-directories-exist file) ""))
      (false (tuatara.terminal::desktop-entries-refusal "/a/" (list self) :self self))
      (false (tuatara.terminal::desktop-entries-refusal "/a/" (list launcher) :self self))
      (false (tuatara.terminal::desktop-entries-refusal "/a/" (list "/bin/sh" "--light") :self "/bin/sh"))
      (true (tuatara.terminal::desktop-entries-refusal "/a/" (list "/bin/sh") :self self))))
  ;; every word of the Exec line quoted as it needs
  (is string= "--dark" (tuatara.terminal::exec-argument "--dark" :plain t))
  (is string= "\"a b\"" (tuatara.terminal::exec-argument "a b" :plain t)))

(define-test (tools desktop-entries-program-named-from-root)
  ;; the program an entry starts is named from the root, never relative to where the request was made; an
  ;; AppImage named by the environment alone is not taken for this program
  (with-test-directory (directory)
    (let ((self (namestring (merge-pathnames "checkout/build/tuatara" directory))))
      (put-file (ensure-directories-exist self) "")
      (uiop:with-current-directory ((merge-pathnames "checkout/" directory))
        (let ((why (tuatara.terminal::desktop-entries-refusal "/a/" (list "build/tuatara") :self self)))
          (true (and why (search "not named from the root" why)) "~a" why)))
      (let ((before (sb-posix:getenv "APPIMAGE")))
        (sb-posix:setenv "APPIMAGE" "/bin/sh" 1)
        (unwind-protect
             (true (tuatara.terminal::desktop-entries-refusal "/a/" (list "/bin/sh") :self self)
                   "an inherited APPIMAGE taken")
          (if before (sb-posix:setenv "APPIMAGE" before 1) (sb-posix:unsetenv "APPIMAGE")))))))

(define-test (tools autostart-generated)
  ;; tuatara --generate-autostart writes the daemon's autostart entry and says where; never over a file
  ;; there, a link neither; --daemon=NAME writes the named daemon's
  (let* ((directory (namestring (uiop:xdg-config-home "autostart/")))
         (path (concatenate 'string directory "tuatara-daemon.desktop"))
         (named (concatenate 'string directory "tuatara-daemon-work.desktop")))
    (true (uiop:subpathp (pathname directory) *test-home-directory*) "the tests' own configuration directory")
    (unwind-protect
         (progn
           (let ((out (with-output-to-string (*standard-output*)
                        (is = 0 (tuatara.terminal:main '("--generate-autostart"))))))
             (true (search (format nil "Wrote ~a" path) out)))
           (let ((text (file-text path)))
             (true (search (format nil "~%Exec=") text))
             (true (search (format nil " --daemon~%") text))
             (true (search (format nil "~%Name=Tuatara daemon~%") text))
             (true (search (format nil "~%NoDisplay=true~%") text)))
           (multiple-value-bind (valid said) (desktop-entry-valid-p path)
             (true valid "~a" said))
           (alexandria:write-string-into-file "mine" path :if-exists :supersede)
           (let ((said (with-output-to-string (*error-output*)
                         (is = 1 (tuatara.terminal:main '("--generate-autostart"))))))
             (true (search "move it away" said)))
           (is string= "mine" (file-text path) "a file there is left as it is")
           (let ((out (with-output-to-string (*standard-output*)
                        (is = 0 (tuatara.terminal:main '("--generate-autostart" "--daemon=work"))))))
             (true (search named out)))
           (let ((text (file-text named)))
             (true (search (format nil " --daemon=work~%") text))
             (true (search (format nil "~%Name=Tuatara daemon (work)~%") text)))
           ;; a link where the entry goes is not written through
           (delete-file named)
           (let ((target (concatenate 'string directory "target")))
             (tuatara.io:make-symbolic-link target named)
             (is = 1 (tuatara.terminal:main '("--generate-autostart" "--daemon=work")))
             (false (probe-file target) "nothing written through the link")))
      (dolist (file (list path named))
        (ignore-errors (tuatara.io:remove-file file)))))
  ;; the program named as the PATH finds it when it is this one, else by its own path
  (is string= "tuatara" (tuatara.terminal::autostart-program :self "/bin/sh" :found "/bin/sh"))
  (is string= "/bin/sh" (tuatara.terminal::autostart-program :self "/bin/sh" :found "/bin/ls"))
  (is string= "/bin/sh" (tuatara.terminal::autostart-program :self "/bin/sh" :found nil)))

(define-test (tools remove-desktop-takes-both)
  ;; tools/install-desktop.sh --remove takes away both application entries and the icon, and no program but
  ;; the AppImage it put there
  (with-test-directory (directory)
    (let* ((home (merge-pathnames "home/" directory))
           (applications (merge-pathnames ".local/share/applications/" home))
           (bin (merge-pathnames ".local/bin/tuatara" home)))
      (ensure-directories-exist applications)
      (ensure-directories-exist bin)
      (dolist (name '("tuatara.desktop" "tuatara-client.desktop" "other.desktop"))
        (put-file (merge-pathnames name applications) "[Desktop Entry]"))
      (put-file bin "#!/bin/sh")
      (uiop:run-program (list "bash" (tools-script "install-desktop.sh") "--remove")
                        :output nil :error-output nil
                        :environment (list (format nil "HOME=~a" (uiop:native-namestring home))
                                           (format nil "XDG_DATA_HOME=~a"
                                                   (uiop:native-namestring (merge-pathnames ".local/share/" home)))
                                           "PATH=/usr/bin:/bin"))
      (false (probe-file (merge-pathnames "tuatara.desktop" applications)))
      (false (probe-file (merge-pathnames "tuatara-client.desktop" applications)))
      (true (probe-file (merge-pathnames "other.desktop" applications)))
      (true (probe-file bin) "not an AppImage: left"))))

(define-test (tools weekly-check-asked-first)
  ;; setting up the container starts no weekly network job unless asked, and
  ;; with no one to answer, none is started
  (with-test-directory (directory)
    (let ((tools (merge-pathnames "tools/" directory))
          (bin (merge-pathnames "bin/" directory))
          (started (merge-pathnames "started" directory)))
      (ensure-directories-exist tools)
      (ensure-directories-exist bin)
      (uiop:copy-file (tools-script "toolbox-setup.sh") (merge-pathnames "toolbox-setup.sh" tools))
      (flet ((script (path text)
               (with-open-file (o path :direction :output) (write-string text o))
               (sb-posix:chmod (uiop:native-namestring path) #o755)))
        (script (merge-pathnames "check-updates.sh" tools)
                (format nil "#!/bin/sh~%touch '~a'~%" (uiop:native-namestring started)))
        (script (merge-pathnames "toolbox" bin)
                (format nil "#!/bin/sh~%[ \"$1\" = list ] && echo ' x tuatara-made-up y'~%exit 0~%")))
      (uiop:run-program (list "bash" (uiop:native-namestring (merge-pathnames "toolbox-setup.sh" tools)))
                        :input nil :output nil :error-output nil :ignore-error-status t
                        :environment (list (format nil "PATH=~a:/usr/bin:/bin" (uiop:native-namestring bin))
                                           "TUATARA_CONTAINER=tuatara-made-up"))
      (false (probe-file started)))))

(define-test (tools appimage-runtime-pinned)
  ;; the runtime put at the front of every AppImage: a fixed release, checked by its SHA-256
  (let ((text (uiop:read-file-string (tools-script "appimage.sh"))))
    (true (cl-ppcre:scan "(?m)^runtime_url=https://github\\.com/AppImage/type2-runtime/releases/download/[0-9]{8}/runtime-x86_64$" text))
    (true (cl-ppcre:scan "(?m)^runtime_sha256=[0-9a-f]{64}$" text))
    (true (search "fetch \"$runtime_url\" \"$tools_dir/runtime-x86_64\" \"$runtime_sha256\"" text))
    (true (search "--runtime-file \"$tools_dir/runtime-x86_64\"" text))))

(define-test (tools grammars-compiled-one-way)
  ;; tools/grammars.sh and tools/fuzz-grammars.sh compile a grammar alike, the
  ;; fuzzing with its assertions kept
  (with-test-directory (directory)
    (let ((src (uiop:native-namestring (merge-pathnames "src/" directory))))
      (ensure-directories-exist src)
      (dolist (name '("parser.c" "scanner.cc"))
        (with-open-file (o (concatenate 'string src name) :direction :output) (write-line "" o)))
      (flet ((calls (&rest options)
               (uiop:split-string
                (string-right-trim '(#\Newline)
                                   (apply #'bash-output
                                          (format nil ". '~a'
cc() { printf 'cc'; printf ' [%s]' \"$@\"; echo; }
c++() { printf 'c++'; printf ' [%s]' \"$@\"; echo; }
compile_grammar \"$@\"" (tools-script "grammar-build.sh"))
                                          src "/tmp/made-up/g.so" options))
                :separator '(#\Newline))))
        (let ((plain (calls))
              (fuzzed (calls "--assertions")))
          (is = 3 (length plain))
          (true (search "[-DNDEBUG]" (first plain)))
          (true (search "[-DNDEBUG]" (second plain)))
          (true (search "safe-ctype.h]" (first plain)))
          (true (search "[-fvisibility=hidden]" (second plain)))
          (is = 3 (length fuzzed))
          (false (search "NDEBUG" (format nil "~{~a~}" fuzzed)))
          (false (search "[]" (format nil "~{~a~}" fuzzed)))
          (true (search "safe-ctype.h]" (first fuzzed))))))))

(define-test (tools grammar-generation-failure-stops)
  ;; a patched grammar whose parser cannot be generated again stops the build,
  ;; saying why, and leaves no parser of before to be compiled
  (with-test-directory (directory)
    (let* ((grammar (uiop:native-namestring (merge-pathnames "grammar/" directory)))
           (parser (concatenate 'string grammar "src/parser.c")))
      (ensure-directories-exist parser)
      (flet ((generated (program)
               (with-open-file (o parser :direction :output :if-exists :supersede) (write-line "/* before */" o))
               (bash-output (format nil ". '~a'
cli=\"$1/../cli\"; printf '#!/bin/sh\\n%s\\n' \"$2\" > \"$cli\"; chmod +x \"$cli\"
generate_parser \"$1\" \"$cli\" 2>&1; echo \"went on\"" (tools-script "grammar-build.sh"))
                            (string-right-trim "/" grammar) program)))
        (let ((said (generated "echo 'grammar.js: no such rule' >&2; exit 1")))
          (true (search "no such rule" said))
          (true (search "could not be generated" said))
          (false (search "went on" said))
          (false (probe-file parser)))
        (let ((said (generated "echo 'a single element rule'; echo '/* new */' > src/parser.c")))
          (true (search "went on" said))
          (false (search "single element" said))
          (is string= (format nil "/* new */~%") (uiop:read-file-string parser)))))))

(define-test (tools program-built-again-after-data)
  ;; build/tuatara is made again when a source of it is newer, data/'s starter init
  ;; file and LaTeX templates among them, by one test the scripts share
  (with-test-directory (directory)
    (let ((root (string-right-trim "/" (uiop:native-namestring directory))))
      (flet ((stale-p ()
               (string= "stale" (string-trim '(#\Newline)
                                             (bash-output (format nil ". '~a'; image_stale \"$1\" && echo stale || echo fresh"
                                                                  (tools-script "inside.sh"))
                                                          root))))
             (made (name seconds-ago)
               (let ((path (format nil "~a/~a" root name)))
                 (ensure-directories-exist path)
                 (with-open-file (o path :direction :output :if-exists :supersede) (write-line "" o))
                 (uiop:run-program (list "touch" "-d" (format nil "@~d" (- (sb-posix:time) seconds-ago)) path))
                 path)))
        (made "src/core.lisp" 100)
        (true (stale-p))
        (sb-posix:chmod (made "build/tuatara" 50) #o755)
        (false (stale-p))
        (made "data/latex-templates/article.tex" 10)
        (true (stale-p))
        (made "build/tuatara" 5)
        (sb-posix:chmod (format nil "~a/build/tuatara" root) #o755)
        (false (stale-p))
        (made "data/init.lisp" 1)
        (true (stale-p)))))
  (true (search "image_stale \"$root\"" (uiop:read-file-string (tools-script "tuatara-tui.sh")))))

(define-test (tools terminal-library-built-again-only-for-its-inputs)
  ;; the terminal library is built again when its pin, layout.c or shim.c change, not when
  ;; the script that builds it is only reworded
  (with-test-directory (directory)
    (let ((root (string-right-trim "/" (uiop:native-namestring directory))))
      (flet ((put (name text)
               (let ((path (format nil "~a/~a" root name)))
                 (ensure-directories-exist path)
                 (with-open-file (o path :direction :output :if-exists :supersede) (write-string text o))
                 path))
             (run (&rest arguments)
               (multiple-value-bind (out err code)
                   (uiop:run-program (list* "env" "PATH=/nonexistent:/usr/bin:/bin" "bash"
                                            (format nil "~a/tools/terminal-library.sh" root) arguments)
                                     :output :string :error-output :string :ignore-error-status t)
                 (declare (ignore err))
                 (values (string-trim '(#\Newline) out) code))))
        (put "tools/terminal-library.sh" (uiop:read-file-string (tools-script "terminal-library.sh")))
        (put "tools/fetch.sh" (uiop:read-file-string (tools-script "fetch.sh")))
        (put "lisp/terminal/library.pin" (format nil "repository	x~%commit	y~%zig	99.0.0~%version	1~%"))
        (put "lisp/terminal/layout.c" "int main(void) { return 0; }")
        (put "lisp/terminal/shim.c" "")
        (put "build/terminal/libghostty-vt.so" "")
        (put "build/terminal/.built-from" (format nil "~a~%" (run "--stamp")))
        (is = 0 (nth-value 1 (run)))
        (put "tools/terminal-library.sh"
             (concatenate 'string (uiop:read-file-string (tools-script "terminal-library.sh"))
                          (format nil "~%# a comment reworded~%")))
        (is = 0 (nth-value 1 (run)))
        (put "lisp/terminal/shim.c" "int changed;")
        (isnt = 0 (nth-value 1 (run)))))))

(define-test (tools inside-says-what-is-missing)
  ;; run on this system, a program that is not installed is said with what to install,
  ;; after env and its NAME=VALUE words, as the package tests run sbcl; one that is runs
  (flet ((inside (command)
           (bash-output (format nil "export TUATARA_CONTAINER=no-such-toolbox-here; . '~a'; ~a 2>&1; echo \"exit $?\""
                                (tools-script "inside.sh") command))))
    (let ((said (inside "inside env TUATARA_SOURCE=/tmp no-such-program-here --load x")))
      (true (search "No no-such-program-here on this system, and no toolbox called no-such-toolbox-here" said) "~a" said)
      (true (search "HACKING.org" said))
      (true (search "exit 127" said)))
    (is string= (format nil "ran~%exit 0~%") (inside "inside env A=1 echo ran"))))

(define-test (tools release-names-no-home-directory)
  ;; the release's program is built from a copy in a directory of its own, and refused
  ;; when it names this machine's home directory or user, which the check finds
  (let ((release (uiop:read-file-string (tools-script "release.sh"))))
    (true (search "git archive HEAD" release))
    (true (search "work=\"$(mktemp -d /tmp/tuatara-release-build.XXXXXX)\"" release))
    (true (search "trap 'rm -rf \"$work\"" release))
    (true (search "XDG_CACHE_HOME=\"$work/cache\"" release))
    (true (search "if names_this_machine \"$program\"; then" release)))
  (with-test-directory (directory)
    (let ((file (uiop:native-namestring (merge-pathnames "program" directory))))
      (flet ((names-p (text)
               (with-open-file (o file :direction :output :if-exists :supersede) (write-string text o))
               (string= "yes" (string-trim '(#\Newline)
                                           (bash-output (format nil "export HOME=/home/annt USER=annt; . '~a'
names_this_machine \"$1\" 2>/dev/null && echo yes || echo no" (tools-script "inside.sh"))
                                                        file)))))
        (false (names-p "sources in /tmp/tuatara-release-build.Kp3x9Q/src/"))
        (true (names-p (format nil "x~cy /home/annt/Projects/tuatara/src/core.lisp" (code-char 0))))
        (true (names-p "built by annt"))))))

(define-test (tools grammar-licences-as-spdx-has-them)
  ;; a grammar under the GNU GPL is written GPL-3.0-only or GPL-3.0-or-later, as its
  ;; package says; when nothing says which, it is left to be filled in by hand
  (with-test-directory (directory)
    (flet ((file (name text)
             (let ((path (merge-pathnames name directory)))
               (ensure-directories-exist path)
               (with-open-file (o path :direction :output :if-exists :supersede) (write-string text o)))))
      (file "syntax/grammars.tsv" (format nil "said	https://example.org/said	0	unknown~%unsaid	https://example.org/unsaid	0	unknown~%plain	https://example.org/plain	0	unknown~%handed	https://example.org/handed	0	unknown~%bare	https://example.org/bare	0	unknown"))
      (dolist (name '("said" "unsaid"))
        (file (format nil "build/grammar-sources/~a/COPYING" name)
              "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 ... either version 3 of the License, or (at your option) any later version."))
      (file "build/grammar-sources/said/package.json" "{\"license\": \"GPL-3.0-or-later\"}")
      (file "build/grammar-sources/unsaid/package.json" "{\"license\": \"GPL-3.0\"}")
      (file "build/grammar-sources/plain/LICENSE" "Permission is hereby granted, free of charge, to any person")
      ;; a grammar taken over under the Affero GPL, the first author's
      ;; licence kept below it in the same file
      (file "build/grammar-sources/handed/LICENSE"
            (format nil "GNU AFFERO GENERAL PUBLIC LICENSE~%Version 3, 19 November 2007~%...~%~
                         Copyright (c) 2021 A. Kaikoura~%~%Permission to use, copy, modify, and/or distribute this software~%~
                         for any purpose with or without fee is hereby granted, provided~%~
                         that the above copyright notice and this permission notice~%appear in all copies.~%"))
      (file "build/grammar-sources/handed/package.json" "{\"license\": \"AGPL-3.0-or-later\"}")
      (file "build/grammar-sources/bare/LICENSE"
            (format nil "Permission to use, copy, modify, and/or distribute this software for~%~
                         any purpose with or without fee is hereby granted.~%~%THE SOFTWARE IS PROVIDED \"AS IS\" ..."))
      (uiop:run-program (list "python3" "-I" (tools-script "languages/licenses.py"))
                        :directory directory :output nil :error-output nil)
      (is equal '("GPL-3.0-or-later" "unknown" "MIT" "AGPL-3.0-or-later AND ISC" "0BSD")
          (mapcar (lambda (line) (fourth (uiop:split-string line :separator '(#\Tab))))
                  (uiop:read-file-lines (merge-pathnames "syntax/grammars.tsv" directory)))))))

(defun licence-names-of (&rest arguments)
  "The exit status of tools/licence-names.sh run with ARGUMENTS, and what it
printed, the error output after the output."
  (multiple-value-bind (out error status)
      (uiop:run-program (list* "bash" (tools-script "licence-names.sh") arguments)
                        :output :string :error-output :string :ignore-error-status t)
    (values status (concatenate 'string out error))))

(define-test (tools licence-names-read-from-the-text)
  ;; each licence a file holds is named, two in one file both; a label agrees
  ;; with the files only when it names what they hold, all of it, a GNU
  ;; licence whatever it says of later versions; a LICENSES/ directory may hold
  ;; more than the label names
  (with-test-directory (directory)
    (let ((handed (write-test-file directory "handed/LICENSE"
                                   (format nil "GNU AFFERO GENERAL PUBLIC LICENSE~%Version 3, 19 November 2007~%...~%~
                                                Copyright (c) 2021 A. Kaikoura~%~%Permission to use, copy, modify, and/or distribute this software~%~
                                                for any purpose with or without fee is hereby granted, provided~%~
                                                that the above copyright notice and this permission notice~%appear in all copies.~%")))
          (bare (write-test-file directory "bare/LICENSE"
                                 (format nil "Permission to use, copy, modify, and/or distribute this software for~%~
                                              any purpose with or without fee is hereby granted.~%~%THE SOFTWARE IS PROVIDED \"AS IS\" ...")))
          (gpl (write-test-file directory "gpl/COPYING"
                                (format nil "                    GNU GENERAL PUBLIC LICENSE~%                       Version 3, 29 June 2007~%~
                                             ... the GNU Affero General Public License ...")))
          (mit (write-test-file directory "dual/LICENSE-MIT"
                                "Permission is hereby granted, free of charge, to any person obtaining a copy"))
          (apache (write-test-file directory "dual/LICENSE-APACHE"
                                   (format nil "                              Apache License~%                        Version 2.0, January 2004~%")))
          (named (write-test-file directory "named/LICENSE"
                                  (format nil "SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception~%")))
          (reuse (write-test-file directory
                                  "reuse/LICENSES/CC0-1.0.txt"
                                  "Creative Commons Legal Code CC0 1.0 Universal"))
          (mpl (write-test-file directory "reuse/LICENSE.txt" "Mozilla Public License Version 2.0"))
          (none (write-test-file directory "none/LICENSE" "All rights reserved.")))
      (flet ((names (&rest files)
               (multiple-value-bind (status out) (apply #'licence-names-of (mapcar #'uiop:native-namestring files))
                 (is = 0 status)
                 (uiop:split-string (string-right-trim '(#\Newline) out) :separator '(#\Newline))))
             (agrees (label &rest files)
               (multiple-value-bind (status out) (apply #'licence-names-of "--agrees" label
                                                        (mapcar #'uiop:native-namestring files))
                 (values (zerop status) out))))
        (is equal '("AGPL-3.0" "ISC") (names handed))
        (is equal '("0BSD") (names bare))
        (is equal '("GPL-3.0") (names gpl))
        (is equal '("Apache-2.0" "LLVM-exception") (names named))
        (false (agrees "0BSD" handed))
        (multiple-value-bind (agreed out) (agrees "0BSD" handed)
          (declare (ignore agreed))
          (true (search "not in the label: AGPL-3.0 ISC" out) "~a" out)
          (true (search "in no file: 0BSD" out) "~a" out))
        (false (agrees "AGPL-3.0-or-later" handed))
        (true (agrees "AGPL-3.0-or-later AND ISC" handed))
        (false (agrees "ISC" bare))
        (true (agrees "0BSD" bare))
        (true (agrees "GPL-3.0-only" gpl))
        (true (agrees "GPL-3.0-or-later" gpl))
        (false (agrees "Apache-2.0" mit apache))
        (true (agrees "MIT OR Apache-2.0" mit apache))
        (false (agrees "(MIT OR Apache-2.0) AND W3C-20150513" mit apache))
        (true (agrees "Apache-2.0 WITH LLVM-exception" named))
        (true (agrees "MPL-2.0" mpl reuse))
        (false (agrees "MPL-2.0 AND MIT" mpl reuse))
        (false (agrees "MIT" none))
        ;; a licence named by an identifier or a notice, without its text
        (multiple-value-bind (status out) (licence-names-of "--untexted" (uiop:native-namestring named))
          (is = 0 status)
          (is string= (format nil "Apache-2.0~%LLVM-exception~%") out))
        (let ((notice (write-test-file directory "notice/LICENSE.md"
                                       (format nil
                                               "This program is free software: you can redistribute it and/or modify~%~
                                                    it under the terms of the GNU General Public License as published by~%~
                                                    the Free Software Foundation, either version 3 of the License, or~%~
                                                    (at your option) any later version.~%"))))
          (is equal '("GPL-3.0") (names notice))
          (is string=
              (format nil "GPL-3.0~%")
              (nth-value 1 (licence-names-of "--untexted" (uiop:native-namestring notice))))
          (is string= "" (nth-value 1 (licence-names-of "--untexted" (uiop:native-namestring notice) (uiop:native-namestring gpl)))))
        (is string= "" (nth-value 1 (licence-names-of "--untexted" (uiop:native-namestring handed))))))))

(defun grammar-origin (directory name licence)
  "A git repository NAME under DIRECTORY with one commit, a LICENSE file of the
text LICENCE: its address and its commit."
  (let ((origin (merge-pathnames (format nil "~a/" name) directory)))
    (write-test-file origin "LICENSE" licence)
    (values (format nil "file://~a" (uiop:native-namestring origin))
            (string-trim '(#\Newline)
                         (bash-output "set -e; cd \"$1\"; git init -q
git add LICENSE; git -c user.name=t -c user.email=t@example.invalid commit -q -m one
git rev-parse HEAD" (uiop:native-namestring origin))))))

(defun run-grammars-script (directory manifest)
  "Run a copy of tools/grammars.sh in DIRECTORY with MANIFEST as its
syntax/grammars.tsv: its exit status, and its output and error output."
  (let ((tools (merge-pathnames "tools/" directory)))
    (ensure-directories-exist tools)
    (dolist (script '("grammars.sh" "grammar-build.sh" "fetch.sh" "inside.sh" "licence-names.sh"))
      (uiop:copy-file (tools-script script) (merge-pathnames script tools)))
    ;; what the script reads to tell whether it has anything to do
    (dolist (file '("syntax/grammar-inputs.sha256" "syntax/syntax.c" "syntax/helper.c" "syntax/safe-ctype.h"
                    "syntax/counted-memory.h" "syntax/patches/other.patch" "syntax/licenses/other.txt"))
      (write-test-file directory file ""))
    (write-test-file directory "syntax/grammars.tsv" manifest)
    (multiple-value-bind (out error status)
        (uiop:run-program (list "env" "TUATARA_CONTAINER=tuatara-none-such"
                                "bash" (uiop:native-namestring (merge-pathnames "grammars.sh" tools)))
                          :output :string :error-output :string :ignore-error-status t)
      (values status (concatenate 'string out error)))))

(define-test (tools grammars-stop-on-a-misnamed-licence)
  ;; tools/grammars.sh compiles nothing of a grammar whose licence the manifest
  ;; names otherwise than its licence files
  (with-test-directory (directory)
    (multiple-value-bind (url commit)
        (grammar-origin directory "origin"
                        (format nil "GNU AFFERO GENERAL PUBLIC LICENSE~%Version 3, 19 November 2007~%"))
      (flet ((build (label)
               (run-grammars-script directory (format nil "g	~a	~a	~a~%" url commit label))))
        (multiple-value-bind (status out) (build "0BSD")
          (is = 1 status)
          (true (search "names the licence of g otherwise" out) "~a" out)
          (true (search "g: the label says 0BSD, the licence files hold AGPL-3.0" out) "~a" out))
        ;; named as its file names it, it goes on to be checked and compiled
        (multiple-value-bind (status out) (build "AGPL-3.0-or-later")
          (declare (ignore status))
          (false (search "otherwise than its licence files" out) "~a" out)
          (true (probe-file (merge-pathnames "build/grammars/licenses/g/LICENSE" directory)) "~a" out))
        ;; a text of syntax/licenses/ for a grammar whose own files hold it stops it
        (write-test-file directory "syntax/licenses/g.txt"
                         (format nil "GNU AFFERO GENERAL PUBLIC LICENSE~%Version 3~%"))
        (multiple-value-bind (status out) (build "AGPL-3.0-or-later")
          (is = 1 status)
          (true (search "has a text for g, whose own licence files hold" out) "~a" out))))))

(define-test (tools grammars-carry-the-text-of-a-licence-only-named)
  ;; a grammar whose licence file only names its licence has the text of
  ;; syntax/licenses/NAME.txt carried beside it; without that text it stops
  (with-test-directory (directory)
    (multiple-value-bind (url commit)
        (grammar-origin directory "origin" (format nil "SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception~%"))
      (let ((manifest (format nil "w	~a	~a	Apache-2.0 WITH LLVM-exception~%" url commit)))
        (multiple-value-bind (status out) (run-grammars-script directory manifest)
          (is = 1 status)
          (true (search "w: its files name Apache-2.0 LLVM-exception without the text" out) "~a" out)
          (true (search "name a licence without its text" out) "~a" out))
        (write-test-file directory "syntax/licenses/w.txt"
                         (format nil "                                 Apache License~%                           Version 2.0, January 2004~%...~%~
                                      ---- LLVM Exceptions to the Apache 2.0 License ----~%"))
        (multiple-value-bind (status out) (run-grammars-script directory manifest)
          (declare (ignore status))
          (false (search "without its text" out) "~a" out)
          (false (search "otherwise than its licence files" out) "~a" out)
          (true (probe-file (merge-pathnames "build/grammars/licenses/w/LICENSE" directory)) "~a" out)
          (true (search "Version 2.0, January 2004"
                        (uiop:read-file-string (merge-pathnames "build/grammars/licenses/w/LICENSE-TEXT" directory)))))))))

(define-test (tools language-generators-share-their-helpers)
  ;; the generators of tools/languages/ write Lisp strings one way, from common.py
  (is string= (format nil "(\"a\\\"b\" \"c\\\\d\")~%")
      (uiop:run-program (list "python3" "-I" "-c"
                              (format nil "import sys; sys.path.insert(0, '~a'); from common import lisp_list; print(lisp_list(['a\"b', 'c\\\\d']))"
                                      (tools-script "languages")))
                        :output :string))
  (dolist (script '("generate.py" "tools.py" "languages.py"))
    (let ((text (uiop:read-file-string (tools-script (format nil "languages/~a" script)))))
      (true (search "from common import" text) script)
      (false (search "def lisp_string" text) script)
      (false (search "HELIX = " text) script))))

(defun archive-check-tree (directory)
  "In DIRECTORY, a copy of the archive script with a list of two made-up
libraries and a Quicklisp holding their archives; returns the script's path
and the Quicklisp directory."
  (let* ((tools (merge-pathnames "tools/" directory))
         (quicklisp (merge-pathnames "ql/" directory))
         (archives (merge-pathnames "dists/quicklisp/archives/" quicklisp)))
    (ensure-directories-exist tools)
    (ensure-directories-exist archives)
    (uiop:copy-file (tools-script "lisp-archives.sh") (merge-pathnames "lisp-archives.sh" tools))
    (uiop:copy-file (tools-script "fetch.sh") (merge-pathnames "fetch.sh" tools))
    (flet ((archive (name text)
             (with-open-file (o (merge-pathnames name archives) :direction :output :if-exists :supersede)
               (write-string text o))))
      (archive "one-1.tgz" "first")
      (archive "two-1.tgz" "second")
      (archive "stray-1.tgz" "not listed"))
    (with-open-file (o (merge-pathnames "lisp-archives.sha256" tools) :direction :output :if-exists :supersede)
      (format o "# made up~%")
      (dolist (name '("one-1.tgz" "two-1.tgz"))
        (write-string (string-right-trim '(#\Newline)
                                         (uiop:run-program (list "bash" "-c" "cd \"$1\" && sha256sum \"$2\"" "_"
                                                                 (uiop:native-namestring archives) name)
                                                           :output :string))
                      o)
        (terpri o)))
    (values (uiop:native-namestring (merge-pathnames "lisp-archives.sh" tools))
            (string-right-trim "/" (uiop:native-namestring quicklisp)))))

(define-test (tools lisp-archive-changed-is-refused)
  ;; the archives are as the list has them or the check stops, saying which
  (with-test-directory (directory)
    (multiple-value-bind (script quicklisp) (archive-check-tree directory)
      (flet ((run (&rest arguments)
               (multiple-value-bind (out error code)
                   (uiop:run-program (list* "bash" script arguments) :output :string :error-output :string
                                                                    :ignore-error-status t)
                 (declare (ignore out))
                 (values code error))))
        (is = 0 (run "check" quicklisp))
        (with-open-file (o (format nil "~a/dists/quicklisp/archives/two-1.tgz" quicklisp)
                           :direction :output :if-exists :append)
          (write-string "changed" o))
        (multiple-value-bind (code error) (run "check" quicklisp)
          (is = 1 code)
          (true (search "two-1.tgz is not the archive" error))
          (false (search "one-1.tgz" error)))
        (delete-file (format nil "~a/dists/quicklisp/archives/one-1.tgz" quicklisp))
        (multiple-value-bind (code error) (run "check" quicklisp)
          (is = 1 code)
          (true (search "one-1.tgz is not in" error)))
        (is = 1 (run "no-such-command"))))))

(define-test (tools lisp-archives-prepared-for-a-release)
  ;; a copy a release is compiled from keeps the listed archives only, nothing
  ;; unpacked or of local-projects, and downloads nothing more; a changed
  ;; archive stops it before anything is removed
  (with-test-directory (directory)
    (multiple-value-bind (script quicklisp) (archive-check-tree directory)
      (flet ((path (name) (format nil "~a/~a" quicklisp name))
             (run (&rest arguments)
               (nth-value 2 (uiop:run-program (list* "bash" script arguments) :output nil :error-output nil
                                                                              :ignore-error-status t))))
        (ensure-directories-exist (path "dists/quicklisp/software/one-1/"))
        (ensure-directories-exist (path "local-projects/shadow/"))
        (with-open-file (o (path "dists/quicklisp/archives/one-1.tgz") :direction :output :if-exists :append)
          (write-string "changed" o))
        (is = 1 (run "prepare" quicklisp))
        (true (probe-file (path "dists/quicklisp/software/one-1/")))
        (true (probe-file (path "dists/quicklisp/archives/stray-1.tgz")))
        (with-open-file (o (path "dists/quicklisp/archives/one-1.tgz") :direction :output :if-exists :supersede)
          (write-string "first" o))
        (is = 0 (run "prepare" quicklisp))
        (false (probe-file (path "dists/quicklisp/software/one-1/")))
        (false (probe-file (path "local-projects/shadow/")))
        (false (probe-file (path "dists/quicklisp/archives/stray-1.tgz")))
        (true (probe-file (path "dists/quicklisp/archives/two-1.tgz")))
        (true (search "127.0.0.1" (uiop:read-file-string (path "config/proxy-url.txt"))))))))

(define-test (tools builds-check-the-archives-first)
  ;; every path that compiles the libraries checks their archives before it does
  (let ((release (uiop:read-file-string (tools-script "release.sh")))
        (image (uiop:read-file-string (tools-script "build-image.sh")))
        (setup (uiop:read-file-string (tools-script "ci-setup.sh"))))
    (let ((prepare (search "tools/lisp-archives.sh prepare \"$work/quicklisp\"" release)))
      (true prepare)
      (true (< (search "cp -a \"$quicklisp\" \"$work/quicklisp\"" release) prepare
               (search "\"$work/src/tools/build-image.sh\"" release))))
    (true (search "tools/lisp-archives.sh tools/lisp-archives.sha256" release))
    (true (< (search "tools/lisp-archives.sh check" image) (search "build-image.lisp" image :from-end t)))
    (dolist (word '("index" "fetch" "check" "lock"))
      (true (search (format nil "tools/lisp-archives.sh\" ~a \"$ql\"" word) setup) word))))

(defun manifest-rows ()
  "The rows of syntax/grammars.tsv, each a list of its fields."
  (loop for line in (uiop:read-file-lines (asdf:system-relative-pathname :tuatara "syntax/grammars.tsv"))
        unless (or (zerop (length line)) (char= (char line 0) #\#))
          collect (uiop:split-string line :separator '(#\Tab))))

(define-test (tools grammars-fetched-from-where-they-are)
  ;; no grammar is fetched by an address its repository has left (an owner
  ;; renamed, a repository moved to another owner: the old name may be taken
  ;; by anyone), nor pinned to a commit on no branch or tag of its repository
  (let ((rows (manifest-rows)))
    (dolist (old '("kepet19/tree-sitter-ldif" "jbellerb/tree-sitter-dhall" "markdoc-extra/tree-sitter-markdoc"
                   "wgsl-tooling-wg/tree-sitter-wesl" "the-mikedavis/tree-sitter-diff" "sogaiu/tree-sitter-clojure"
                   "ckafi/tree-sitter-elvish" "theHamsta/tree-sitter-glsl" "santhoshtr/tree-sitter-wikitext"
                   "amaanq/tree-sitter-kdl" "eonpatapon/tree-sitter-cue" "erasin/tree-sitter-po"
                   "MaeBrooks/tree-sitter-gren" "rhizoome/tree-sitter-ink" "jzelinskie/tree-sitter-spicedb"
                   "Philipp-M/tree-sitter-yuck"
                   ;; copies of a grammar kept on elsewhere: taken from where it is kept
                   "ikatyang/tree-sitter-yaml" "Giorbo/tree-sitter-sml"
                   "mtoohey31/tree-sitter-gitattributes" "numtide/tree-sitter-nix"
                   "RenjiSann/tree-sitter-xml" "KMikeeU/tree-sitter-dtd"
                   "ikatyang/tree-sitter-vue"
                   "themixednuts/tree-sitter-htmlx"
                   "polychromatist/tree-sitter-luau"
                   "sdoerner/tree-sitter-proto"
                   "xenogenics/tree-sitter-slisp"
                   "varpeti/tree-sitter-jinja2"
                   ;; its pin on a tag alone, the history rewritten: the grammar Helix takes in its place
                   "rhizoome/tree-sitter-ink2"))
      (false (find (format nil "https://github.com/~a" old) rows :key #'second :test #'string-equal)
             "~a has moved" old))
    ;; archived, their development carried on elsewhere
    (dolist (old '("https://github.com/poliorcetics/tree-sitter-just" "https://github.com/inko-lang/tree-sitter-inko"
                   "https://codeberg.org/maribu/tree-sitter-gnuplot" "https://codeberg.org/maribu/tree-sitter-proverif"
                   "https://github.com/tingerrr/tree-sitter-doxyfile/"))
      (false (find old rows :key #'second :test #'string-equal) "~a is archived" old))
    (dolist (orphan '("3d7c53b8038f9ebbb57cd2e61296180aa5c1cf64" "29f9c6707ce9dfc2fc915d175ec720b207f179f3"
                      "d98426109258b266e1e92358c5f11716d2e8f638"))
      (false (find orphan rows :key #'third :test #'string=) "~a is on no branch or tag" orphan))))

(defun grammar-build-output (script directory)
  "What bash prints running SCRIPT in DIRECTORY with tools/grammar-build.sh sourced, standard error with it."
  (uiop:run-program (list "bash" "-c" (format nil ". '~a'~%~a" (tools-script "grammar-build.sh") script))
                    :directory directory :output :string :error-output :output :ignore-error-status t))

(defun write-test-file (directory name text)
  "Write TEXT to NAME under DIRECTORY, its directories made."
  (let ((path (merge-pathnames name directory)))
    (ensure-directories-exist path)
    (with-open-file (o path :direction :output :if-exists :supersede) (write-string text o))
    path))

(define-test (tools grammar-inputs-are-what-is-compiled)
  ;; what compiling a grammar reads: its parser and scanner and the headers
  ;; they include from its repository (a relative path too), not the
  ;; system's; a header changed, or one added where an include finds it,
  ;; changes the hash
  (with-test-directory (directory)
    (write-test-file directory "src/parser.c" (format nil "#include \"tree_sitter/parser.h\"~%"))
    (write-test-file directory "src/scanner.c" (format nil "#include <stdio.h>~%#include \"../common/scan.h\"~%"))
    (write-test-file directory "src/tree_sitter/parser.h" (format nil "int a;~%"))
    (write-test-file directory "common/scan.h" (format nil "int b;~%"))
    (write-test-file directory "README" "not compiled")
    (is equal '("common/scan.h" "src/parser.c" "src/scanner.c" "src/tree_sitter/parser.h")
        (uiop:split-string (string-trim '(#\Newline)
                                        (grammar-build-output "grammar_inputs \"$PWD\" \"$PWD/src\"" directory))
                           :separator '(#\Newline)))
    (flet ((hash ()
             (string-trim '(#\Newline) (grammar-build-output "grammar_inputs_hash \"$PWD\" \"$PWD/src\"" directory))))
      (let ((first (hash)))
        (is = 64 (length first))
        (write-test-file directory "README" "still not compiled")
        (is string= first (hash) "a file not compiled leaves it")
        (write-test-file directory "common/scan.h" (format nil "int c;~%"))
        (isnt string= first (hash) "a header changed changes it")))))

(define-test (tools grammar-checkout-fetched-once-and-checked)
  ;; a grammar pinned by a tag object is fetched once, not again on each build;
  ;; a checkout changed, or with a file added, is refused, and so is one whose
  ;; inputs hash otherwise than recorded
  (with-test-directory (directory)
    (let* ((repo (merge-pathnames "repo/" directory))
           (out (grammar-build-output
                 (format nil "set -e
git init -q '~a'; cd '~:*~a'
git -c user.name=t -c user.email=t@example.invalid commit -q --allow-empty -m one
git -c user.name=t -c user.email=t@example.invalid tag -a -m v1 v1
tag=$(git rev-parse v1)
cd '~a'
fetch_grammar \"$PWD/g\" 'file://~a' \"$tag\"
touch g/.git/fetched-once
fetch_grammar \"$PWD/g\" 'file://~a' \"$tag\"
[ -f g/.git/fetched-once ] && echo kept
echo 'x  g' > recorded
grammar_checked g \"$PWD/g\" \"$PWD/recorded\" x && echo clean
touch g/added
grammar_checked g \"$PWD/g\" \"$PWD/recorded\" x || echo refused-added
rm g/added
grammar_checked g \"$PWD/g\" \"$PWD/recorded\" y || echo refused-hash"
                         (uiop:native-namestring repo) (uiop:native-namestring directory)
                         (uiop:native-namestring repo) (uiop:native-namestring repo))
                 directory)))
      (true (search "kept" out) "~a" out)
      (true (search "clean" out))
      (true (search "refused-added" out))
      (true (search "refused-hash" out)))))

(define-test (tools grammar-source-check-stops-on-a-pin-off-branches)
  ;; the check before a bump stops on a pin on no branch or tag of its
  ;; repository, and passes one on a branch
  (with-test-directory (directory)
    (let ((repo (uiop:native-namestring (merge-pathnames "repo/" directory)))
          (tools (merge-pathnames "tools/" directory)))
      (ensure-directories-exist tools)
      (uiop:copy-file (tools-script "check-grammar-sources.sh") (merge-pathnames "check-grammar-sources.sh" tools))
      (uiop:copy-file (tools-script "grammar-build.sh") (merge-pathnames "grammar-build.sh" tools))
      (let ((commits (uiop:split-string
                      (string-trim '(#\Newline)
                                   (bash-output (format nil "set -e
git init -q '~a'; cd '~:*~a'
c() { git -c user.name=t -c user.email=t@example.invalid commit -q --allow-empty -m \"$1\"; }
c one; git rev-parse HEAD
git checkout -q -b gone; c lost; git rev-parse HEAD
git checkout -q main; git checkout -q -b tagged; c kept; git rev-parse HEAD; git tag kept
git checkout -q main; git branch -q -D gone tagged" repo)))
                      :separator '(#\Newline))))
        (flet ((check (commit)
                 (write-test-file directory "syntax/grammars.tsv" (format nil "g	file://~a	~a	MIT~%" repo commit))
                 (multiple-value-bind (out error status)
                     (uiop:run-program (list "bash" (uiop:native-namestring (merge-pathnames "check-grammar-sources.sh" tools)))
                                       :output :string :error-output nil :ignore-error-status t)
                   (declare (ignore error))
                   (values status out))))
          (multiple-value-bind (status out) (check (first commits))
            (is = 0 status "~a" out))
          (multiple-value-bind (status out) (check (second commits))
            (is = 1 status)
            (true (search "on no branch or tag" out)))
          ;; on a tag alone: watched, not stopped on
          (multiple-value-bind (status out) (check (third commits))
            (is = 0 status "~a" out)
            (true (search "only on a tag" out)))
          ;; a history that cannot be fetched stops it, whatever was fetched before
          (setf repo (uiop:native-namestring (merge-pathnames "moved-away/" directory)))
          (multiple-value-bind (status out) (check (first commits))
            (is = 1 status)
            (true (search "could not be fetched" out))))))))

(define-test (tools grammar-source-check-asks-tangled-who-keeps-it)
  ;; a grammar on tangled.org is named by its owner's did:plc: DID, and the
  ;; check asks the owner's records where the repository is: one declared by
  ;; another owner, one gone, an owner named by a domain (a handle, a
  ;; did:web:), git sent to another server than the records name, or answers
  ;; that cannot be had, stop it; the owner's own passes, and a change to a
  ;; DID after the day of the grammar's review is to be watched
  (with-test-directory (directory)
    (let* ((repo (uiop:native-namestring (merge-pathnames "repo/" directory)))
           (tools (merge-pathnames "tools/" directory))
           (bin (merge-pathnames "bin/" directory))
           (answers (merge-pathnames "answers/" directory))
           (url "https://tangled.org/did:plc:owner/g")
           (commit (string-trim '(#\Newline)
                                (bash-output (format nil "set -e
git init -q '~a'; cd '~:*~a'
git -c user.name=t -c user.email=t@example.invalid commit -q --allow-empty -m one; git rev-parse HEAD" repo)))))
      (ensure-directories-exist tools)
      (ensure-directories-exist answers)
      (dolist (script '("check-grammar-sources.sh" "grammar-build.sh"))
        (uiop:copy-file (tools-script script) (merge-pathnames script tools)))
      (write-test-file directory "syntax/grammar-reviews.tsv" (format nil "g	2026-10-01~%"))
      ;; curl answers from answers/, a file named after the address asked, and
      ;; fails as with no network for any other
      (write-test-file bin "curl" (format nil "#!/bin/bash
for word; do case \"$word\" in http*) url=\"$word\" ;; esac; done
file='~a'\"$(printf %s \"$url\" | tr -c 'A-Za-z0-9' _)\"
[ -f \"$file\" ] || exit 6
cat \"$file\"~%" (uiop:native-namestring answers)))
      (bash-output "chmod +x \"$1\"" (uiop:native-namestring (merge-pathnames "curl" bin)))
      (flet ((answer (address text)
               (write-test-file answers (map 'string (lambda (c) (if (alphanumericp c) c #\_)) address) text))
             (check (address)
               (write-test-file directory "syntax/grammars.tsv" (format nil "g	~a	~a	MIT~%" address commit))
               (multiple-value-bind (out error status)
                   (uiop:run-program (list* "env"
                                            (format nil "PATH=~a:~a" (uiop:native-namestring bin) (uiop:getenv "PATH"))
                                            "GIT_CONFIG_COUNT=3"
                                            (append
                                             (loop for from in (list url "https://tangled.org/owner.example/g"
                                                                     "https://tangled.org/did:web:owner.example/g")
                                                   for i from 0
                                                   collect (format nil
                                                                   "GIT_CONFIG_KEY_~d=url.file://~a.insteadOf" i repo)
                                                   collect (format nil "GIT_CONFIG_VALUE_~d=~a" i from))
                                             (list "bash" (uiop:native-namestring (merge-pathnames "check-grammar-sources.sh" tools)))))
                                     :output :string :error-output nil :ignore-error-status t)
                 (declare (ignore error))
                 (values status out))))
        (answer "https://plc.directory/did:plc:owner"
                "{\"id\":\"did:plc:owner\",\"service\":[{\"id\":\"#atproto_pds\",\"serviceEndpoint\":\"https://pds.example\"}]}")
        (answer "https://plc.directory/did:plc:repo"
                "{\"id\":\"did:plc:repo\",\"service\":[{\"id\":\"#atproto_pds\",\"serviceEndpoint\":\"https://knot.example\"}]}")
        (answer "https://plc.directory/did:plc:owner/log/audit" "[{\"createdAt\":\"2026-09-01T10:00:00Z\"}]")
        (answer "https://plc.directory/did:plc:repo/log/audit" "[{\"createdAt\":\"2026-09-02T10:00:00Z\"}]")
        (answer "https://pds.example/xrpc/com.atproto.repo.getRecord?repo=did:plc:owner&collection=sh.tangled.repo&rkey=g"
                "{\"value\":{\"knot\":\"knot.example\",\"repoDid\":\"did:plc:repo\"}}")
        (answer "https://tangled.org/did:plc:owner/g/info/refs?service=git-upload-pack"
                "https://knot.example/did:plc:repo/info/refs?service=git-upload-pack")
        (flet ((declared (owner)
                 (answer "https://knot.example/xrpc/com.atproto.repo.getRecord?repo=did:plc:repo&collection=org.tangled.repo.manifest&rkey=self"
                         (format nil "{\"value\":{\"declaration\":{\"name\":\"g\",\"owner\":\"~a\"}}}" owner))))
          (declared "did:plc:owner")
          (multiple-value-bind (status out) (check url)
            (is = 0 status "~a" out)
            (false (search "WATCH" out) "~a" out))
          ;; the repository's DID changed after the review: watched
          (answer "https://plc.directory/did:plc:repo/log/audit"
                  "[{\"createdAt\":\"2026-09-02T10:00:00Z\"},{\"createdAt\":\"2026-10-20T10:00:00Z\"}]")
          (multiple-value-bind (status out) (check url)
            (is = 0 status "~a" out)
            (true (search "did:plc:repo was changed on 2026-10-20" out) "~a" out))
          ;; git sent to another server than the records name
          (answer "https://tangled.org/did:plc:owner/g/info/refs?service=git-upload-pack"
                  "https://elsewhere.example/did:plc:repo/info/refs?service=git-upload-pack")
          (multiple-value-bind (status out) (check url)
            (is = 1 status)
            (true (search "sends git to https://elsewhere.example" out) "~a" out))
          (answer "https://tangled.org/did:plc:owner/g/info/refs?service=git-upload-pack"
                  "https://knot.example/did:plc:repo/info/refs?service=git-upload-pack")
          ;; declared by another owner: moved
          (declared "did:plc:other")
          (multiple-value-bind (status out) (check url)
            (is = 1 status)
            (true (search "is now did:plc:other/g" out) "~a" out)))
        ;; an owner named by a domain, which can change hands
        (dolist (owner '("owner.example" "did:web:owner.example"))
          (multiple-value-bind (status out) (check (format nil "https://tangled.org/~a/g" owner))
            (is = 1 status)
            (true (search (format nil "~a names a domain" owner) out) "~a" out)))
        ;; no record keyed g, and the owner's records cannot be listed
        (answer "https://pds.example/xrpc/com.atproto.repo.getRecord?repo=did:plc:owner&collection=sh.tangled.repo&rkey=g"
                "{\"error\":\"RecordNotFound\",\"message\":\"Could not locate record\"}")
        (multiple-value-bind (status out) (check url)
          (is = 1 status)
          (true (search "no record of did:plc:owner's is keyed g" out) "~a" out))
        ;; none of them named g either: gone
        (answer "https://pds.example/xrpc/com.atproto.repo.listRecords?repo=did:plc:owner&collection=sh.tangled.repo&limit=100"
                "{\"records\":[{\"value\":{\"name\":\"h\",\"knot\":\"knot.example\"}}]}")
        (multiple-value-bind (status out) (check url)
          (is = 1 status)
          (true (search "is gone" out) "~a" out))
        ;; a record named g, kept under another key, is the repository's
        (answer "https://pds.example/xrpc/com.atproto.repo.listRecords?repo=did:plc:owner&collection=sh.tangled.repo&limit=100"
                "{\"records\":[{\"value\":{\"name\":\"g\",\"knot\":\"knot.example\",\"repoDid\":\"did:plc:repo\"}}]}")
        (answer "https://knot.example/xrpc/com.atproto.repo.getRecord?repo=did:plc:repo&collection=org.tangled.repo.manifest&rkey=self"
                "{\"value\":{\"declaration\":{\"name\":\"g\",\"owner\":\"did:plc:owner\"}}}")
        (multiple-value-bind (status out) (check url)
          (is = 0 status "~a" out))
        ;; a DID document for another DID than the one asked about
        (answer "https://plc.directory/did:plc:owner"
                "{\"id\":\"did:plc:someone\",\"service\":[{\"id\":\"#atproto_pds\",\"serviceEndpoint\":\"https://pds.example\"}]}")
        (multiple-value-bind (status out) (check url)
          (is = 1 status)
          (true (search "cannot be told" out) "~a" out))
        ;; no answer from the directory of DIDs
        (uiop:delete-file-if-exists (merge-pathnames "https___plc_directory_did_plc_owner" answers))
        (multiple-value-bind (status out) (check url)
          (is = 1 status)
          (true (search "cannot be told" out) "~a" out))))))

(define-test (tools archived-grammars-left-out)
  ;; a grammar whose repository is archived, with no source kept up in its
  ;; place, is left out; the mode of its files stays, with no language
  (let ((rows (manifest-rows)))
    (loop for (grammar mode) in '(("rust-format-args" nil) ("djot" tuatara.defaults::djot-mode)
                                  ("cpon" tuatara.defaults::cpon-mode) ("move" tuatara.defaults::move-mode)
                                  ("jinja" tuatara.defaults::jinja2-mode)
                                  ("cylc" tuatara.defaults::cylc-mode))
          do (false (find grammar rows :key #'first :test #'string=) "~a is left out" grammar)
             (false (find grammar
                          tuatara.defaults::*languages*
                          :key #'tuatara.defaults::language-grammar
                          :test #'string=))
             (when mode
               (true (tuatara.modes:find-mode mode) "~(~a~) stays" mode)
               (false (tuatara.defaults::mode-language (make-instance mode)))))))

(define-test (tools grammar-js-cannot-reach-git)
  ;; the program generating a parser runs a grammar's grammar.js: whatever it
  ;; writes to a .git (a file monitor, a hook git would run later) never
  ;; reaches the checkout's, and only the parser comes back
  (with-test-directory (directory)
    (let* ((repo (uiop:native-namestring (merge-pathnames "g/" directory)))
           (cli (uiop:native-namestring (merge-pathnames "fake-tree-sitter" directory))))
      (write-test-file directory "fake-tree-sitter"
                       (format nil "#!/bin/bash~%mkdir -p .git src~%printf '[core]\\n\\tfsmonitor = touch /tmp/never\\n' >> .git/config~%echo 'int parser;' > src/parser.c~%"))
      (sb-posix:chmod cli #o755)
      (let ((out (grammar-build-output
                  (format nil "set -e
git init -q '~a'; cd '~:*~a'; echo 'module.exports = {}' > grammar.js; mkdir -p src
before=$(sha256sum .git/config)
generate_parser '~:*~a' '~a'
[ \"$before\" = \"$(sha256sum .git/config)\" ] && echo git-untouched
grep -q 'int parser' src/parser.c && echo parser-back" repo cli)
                  directory)))
        (true (search "git-untouched" out) "~a" out)
        (true (search "parser-back" out) "~a" out)))))

(define-test (tools grammar-hashes-recorded-only-as-named)
  ;; recording what grammars compile asks which: none named records nothing,
  ;; and a name not in the manifest stops it before anything is written
  (flet ((record (&rest arguments)
           (multiple-value-bind (out error status)
               (uiop:run-program (list* "bash" (tools-script "grammars.sh") "--record" arguments)
                                 :output :string :error-output :string :ignore-error-status t)
             (declare (ignore out))
             (values status error))))
    (multiple-value-bind (status error) (record)
      (is = 1 status)
      (true (search "--all" error)))
    (multiple-value-bind (status error) (record "rust" "no-such-grammar")
      (is = 1 status)
      (true (search "no grammar called no-such-grammar" error)))))

(define-test (tools fuzzed-grammars-shut-in)
  ;; what fuzzing runs of a grammar's code reads its sources and writes only
  ;; its scratch directory, with no environment of the caller's
  (with-test-directory (directory)
    (let ((out (grammar-build-output
                (format nil "mkdir -p '~a/sources' '~:*~a/scratch'
SECRET=caller run_shut_in '~:*~a/sources' '~:*~a/scratch' bash -c 'touch ../sources/x 2>/dev/null && echo wrote-sources
touch y && echo wrote-scratch; echo \"secret=${SECRET:-none}\"'"
                        (string-right-trim "/" (uiop:native-namestring directory)))
                directory)))
      (false (search "wrote-sources" out) "~a" out)
      (true (search "wrote-scratch" out) "~a" out)
      (true (search "secret=none" out) "~a" out))))

(define-test (tools generated-parser-taken-through-no-link)
  ;; what generating a parser leaves in the copy is taken back only as plain
  ;; files within it: a link the program made, to a directory or a file of the
  ;; host, brings nothing back
  (with-test-directory (directory)
    (let* ((repo (uiop:native-namestring (merge-pathnames "g/" directory)))
           (host (uiop:native-namestring (merge-pathnames "host/" directory)))
           (cli (uiop:native-namestring (merge-pathnames "fake-tree-sitter" directory))))
      (write-test-file directory "host/parser.c" "int host_secret;")
      (write-test-file directory "fake-tree-sitter"
                       (format nil "#!/bin/bash~%rm -rf src~%ln -s '~a' src~%" (string-right-trim "/" host)))
      (sb-posix:chmod cli #o755)
      (let ((out (grammar-build-output
                  (format nil "git init -q '~a'; cd '~:*~a'; mkdir -p src
( generate_parser '~:*~a' '~a' ) 2>/dev/null
grep -qs host_secret src/parser.c && echo took-host || echo took-nothing" repo cli)
                  directory)))
        (true (search "took-nothing" out) "~a" out)))))

(define-test (tools bundled-libraries-recorded)
  ;; each library copied with its package, version and hash and the package's
  ;; copyright file; dpkg knows a file under /lib where its real path is under
  ;; /usr/lib; a library of no package, or a package without a copyright file,
  ;; stops it
  (with-test-directory (directory)
    (flet ((script (name text)
             (sb-posix:chmod (uiop:native-namestring (write-test-file directory name text)) #o755)))
      (script "bin/dpkg"
              (format nil "#!/bin/sh~%for file in \"$@\"; do [ \"$file\" = /lib/libfoo.so.1.0 ] && echo \"libfoo1:amd64: $file\" && exit 0; done~%exit 1~%"))
      (script "bin/dpkg-query" (format nil "#!/bin/sh~%printf 1.2-3~%"))
      (write-test-file directory "usr/lib/libfoo.so.1.0" "foo")
      (write-test-file directory "usr/lib/libbar.so.2" "bar")
      (write-test-file directory "docs/libfoo1/copyright" "License: LGPL-2.1+")
      (uiop:run-program (list "ln" "-s" "libfoo.so.1.0"
                              (uiop:native-namestring (merge-pathnames "usr/lib/libfoo.so.1" directory))))
      (flet ((run (library)
               (multiple-value-bind (out error status)
                   (uiop:run-program
                    (list "bash"
                          (tools-script "bundle-libraries.sh")
                          (uiop:native-namestring (merge-pathnames "out/" directory))
                          (uiop:native-namestring (merge-pathnames library directory)))
                    :output :string :error-output :string :ignore-error-status t
                    :environment (list (format nil "PATH=~a:/usr/bin:/bin"
                                               (uiop:native-namestring (merge-pathnames "bin/" directory)))
                                       (format nil "DOC_DIR=~a"
                                               (uiop:native-namestring (merge-pathnames "docs/" directory)))
                                       (format nil "USR_ROOT=~a" (string-right-trim "/" (uiop:native-namestring (merge-pathnames "usr/" directory))))))
                 (declare (ignore out))
                 (values status error))))
        (is = 0 (run "usr/lib/libfoo.so.1"))
        (is equal (list (format nil "libfoo.so.1~Alibfoo1~A1.2-3" #\Tab #\Tab))
            (mapcar (lambda (line) (subseq line 0 (position #\Tab line :from-end t)))
                    (uiop:read-file-lines (merge-pathnames "out/lib.tsv" directory))))
        (is = 64 (length (subseq (first (uiop:read-file-lines (merge-pathnames "out/lib.tsv" directory)))
                                 (1+
                                  (position #\Tab
                                            (first (uiop:read-file-lines (merge-pathnames "out/lib.tsv" directory)))
                                            :from-end t)))))
        (true (probe-file (merge-pathnames "out/copyright/libfoo1.copyright" directory)))
        (true (probe-file (merge-pathnames "out/lib/libfoo.so.1" directory)))
        (multiple-value-bind (status error) (run "usr/lib/libbar.so.2")
          (isnt = 0 status)
          (true (search "belongs to no package" error)))
        (delete-file (merge-pathnames "docs/libfoo1/copyright" directory))
        (multiple-value-bind (status error) (run "usr/lib/libfoo.so.1")
          (isnt = 0 status)
          (true (search "has no copyright file" error)))))))

(define-test (tools library-sources-fetched)
  ;; one source package for the libraries of each package, at the version
  ;; bundled, its files checked against its .dsc, a directory no longer wanted
  ;; removed; a version not the one bundled, or a file that is not the one the
  ;; .dsc names, stops it
  (with-test-directory (directory)
    (flet ((script (name text)
             (sb-posix:chmod (uiop:native-namestring (write-test-file directory name text)) #o755))
           (row (&rest fields) (format nil "~{~a~}~%" (rest (loop for field in fields collect #\Tab collect field)))))
      (script "bin/dpkg-query" (format nil "#!/bin/sh~%printf '1.2-3\\tfoo\\t1:1.0-1'~%"))
      (script "bin/apt-get"
              (format nil "#!/bin/sh
case \"$*\" in *\" foo=1:1.0-1\"*) ;; *) exit 1 ;; esac
printf orig > foo_1.0.orig.tar.xz
sum=$(printf orig | sha256sum | cut -c1-64)
printf 'Source: foo\\nChecksums-Sha256:\\n %s 4 foo_1.0.orig.tar.xz\\nFiles:\\n x 4 foo_1.0.orig.tar.xz\\n' \"$sum\" > foo_1.0-1.dsc
[ -z \"$TAMPER\" ] || printf other > foo_1.0.orig.tar.xz~%"))
      (write-test-file directory "docs/libfoo1/copyright"
                       (format nil "Format: x~%License: MIT~%~%License: LGPL-2.1+~%"))
      (write-test-file directory "lib.tsv" (concatenate 'string (row "libfoo.so.1" "libfoo1" "1.2-3" "a")
                                                        (row "libfoo-extra.so.1" "libfoo1" "1.2-3" "b")))
      (write-test-file directory "out/gone/gone_1.dsc" "")
      (write-test-file directory "out/foo/foo_0.9.orig.tar.xz" "old")
      (flet ((run (&rest environment)
               (multiple-value-bind (out error status)
                   (uiop:run-program
                    (list "bash"
                          (tools-script "library-sources.sh")
                          (uiop:native-namestring (merge-pathnames "out/" directory))
                          (uiop:native-namestring (merge-pathnames "lib.tsv" directory)))
                    :output :string :error-output :string :ignore-error-status t
                    :environment (list* (format nil "PATH=~a:/usr/bin:/bin"
                                                (uiop:native-namestring (merge-pathnames "bin/" directory)))
                                        (format nil "DOC_DIR=~a"
                                                (uiop:native-namestring (merge-pathnames "docs/" directory)))
                                        environment))
                 (declare (ignore out))
                 (values status error))))
        (is = 0 (run))
        (is equal '("foo_1.0-1.dsc" "foo_1.0.orig.tar.xz")
            (sort (mapcar #'file-namestring (directory (merge-pathnames "out/foo/*.*" directory))) #'string<))
        (false (probe-file (merge-pathnames "out/gone/" directory)))
        (let ((readme (uiop:read-file-string (merge-pathnames "out/README" directory))))
          (true (search (row "libfoo.so.1" "libfoo1" "foo" "1:1.0-1" "LGPL-2.1+, MIT") readme))
          (true (search (row "libfoo-extra.so.1" "libfoo1" "foo" "1:1.0-1" "LGPL-2.1+, MIT") readme)))
        (multiple-value-bind (status error) (run "TAMPER=1")
          (isnt = 0 status)
          (true (search "differs from its .dsc" error)))
        (write-test-file directory "lib.tsv" (row "libfoo.so.1" "libfoo1" "1.2-4" "a"))
        (multiple-value-bind (status error) (run)
          (isnt = 0 status)
          (true (search "not 1.2-4 as bundled" error)))))))

(define-test (tools library-sources-released)
  ;; the AppImage's library sources are a release file: named in its manifest,
  ;; attached to the draft and in SHA256SUMS
  (let ((appimage (uiop:read-file-string (tools-script "appimage.sh")))
        (workflow (uiop:read-file-string (asdf:system-relative-pathname :tuatara ".github/workflows/release.yml"))))
    (true (search "library_sources=\"$root/build/release/Tuatara-x86_64.AppImage.library-sources.tar.zst\"" appimage))
    (true (search "# Their sources, at exactly these versions: $(basename \"$library_sources\")" appimage))
    (true (search " build/release/Tuatara-x86_64.AppImage.library-sources.tar.zst dist/" workflow))))

(define-test (tools bundled-libraries-each-once)
  ;; the libraries the AppImage bundles are listed once each, a root library
  ;; the others need too (libzstd) among them; a file name given twice stops
  ;; tools/bundle-libraries.sh
  (true (search "for path in $(printf \"%s\\n\" $roots $(ldd $roots | grep \"=> /\" | sed \"s/.*=> //; s/ (.*//\") | sort -u); do"
                (uiop:read-file-string (tools-script "appimage.sh"))))
  (with-test-directory (directory)
    (sb-posix:chmod (uiop:native-namestring (write-test-file directory "bin/dpkg"
                                                             (format nil "#!/bin/sh~%echo \"libfoo1:amd64: $1\"~%")))
                    #o755)
    (sb-posix:chmod (uiop:native-namestring (write-test-file directory "bin/dpkg-query"
                                                             (format nil "#!/bin/sh~%printf 1~%")))
                    #o755)
    (write-test-file directory "docs/libfoo1/copyright" "License: MIT")
    (let ((library (uiop:native-namestring (write-test-file directory "usr/lib/libfoo.so.1" "foo"))))
      (multiple-value-bind (out error status)
          (uiop:run-program
           (list "bash" (tools-script "bundle-libraries.sh") (uiop:native-namestring (merge-pathnames "out/" directory))
                 library library)
           :output :string :error-output :string :ignore-error-status t
           :environment (list (format nil "PATH=~a:/usr/bin:/bin"
                                      (uiop:native-namestring (merge-pathnames "bin/" directory)))
                              (format nil "DOC_DIR=~a" (uiop:native-namestring (merge-pathnames "docs/" directory)))))
        (declare (ignore out))
        (isnt = 0 status)
        (true (search "libfoo.so.1 is given twice" error))))))

(define-test (tools possible-copies-ranks-by-words-and-names)
  ;; a fixture index and a branch adding two definitions: one with the words and the name of an
  ;; entry from another file, one like nothing; the definition itself is left out, a near-match in its own file is
  ;; listed
  (with-test-directory (directory)
    (flet ((path (name) (uiop:native-namestring (merge-pathnames name directory)))
           (git (&rest arguments)
             (uiop:run-program (list* "git" "-C" (uiop:native-namestring directory) "-c" "user.name=T"
                                      "-c" "user.email=t@example.org" "-c" "commit.gpgsign=false" arguments)
                               :output :string :error-output :output)))
      (ensure-directories-exist (merge-pathnames "tools/" directory))
      (ensure-directories-exist (merge-pathnames "test/api/" directory))
      (ensure-directories-exist (merge-pathnames "lisp/" directory))
      (dolist (name '("possible-copies.sh" "possible-copies.py" "meaning.py"))
        (uiop:copy-file (tools-script name) (path (format nil "tools/~a" name))))
      (with-open-file (out (path "test/api/reuse-index.txt") :direction :output)
        (format out "~{~a~%~}"
                '(";; The reuse index"
                  "tuatara:read-bank-statement  function  Read the bank statement from a file and return its lines.  lisp/bank.lisp"
                  "tuatara:parse-bank-statement  function  Read the bank statement the same way.  lisp/new.lisp"
                  "tuatara:load-bank-statement  function  Read the bank statement in FILE and give its lines.  lisp/new.lisp"
                  "tuatara:draw-the-moon  function  Paint the night sky.  lisp/sky.lisp"
                  "tuatara:other-one  function  Something else entirely about cheese.  lisp/cheese.lisp"
                  "tuatara:another  function  Further things about rivers.  lisp/rivers.lisp"
                  "tuatara:tea  function  Brew a pot of tea.  lisp/tea.lisp"
                  "tuatara:lamp  function  Switch the lamp on.  lisp/lamp.lisp"
                  "tuatara:door  function  Lock the front door.  lisp/door.lisp"
                  "tuatara:kite  function  Fly a kite in wind.  lisp/kite.lisp")))
      (git "init" "-q")
      (git "add" "-A")
      (git "commit" "-q" "-m" "base")
      (with-open-file (out (path "lisp/new.lisp") :direction :output)
        (format out "(defun load-bank-statement (file)~%  \"Read the bank statement in FILE and give its lines.\"~%  file)~%~%~
                     (defun zzz-quux (x)~%  \"Frobnicate the widgets.\"~%  x)~%"))
      (git "add" "-A")
      (git "commit" "-q" "-m" "branch")
      (let ((out (uiop:run-program (list "bash" (path "tools/possible-copies.sh") "HEAD~1" "HEAD")
                                   :output :string)))
        (true (search "lisp/new.lisp  load-bank-statement" out))
        (true (search "read-bank-statement  lisp/bank.lisp" out))
        (true (search "parse-bank-statement  lisp/new.lisp" out))
        (false (search "  load-bank-statement  lisp/new.lisp" out))
        (false (search "draw-the-moon" out))
        (false (search "zzz-quux" out))))))

(define-test (tools possible-copies-alike-meaning-without-the-model)
  ;; with no model file the mode says so in one line and ends well, so a report can leave its section out
  (with-test-directory (directory)
    (let ((out (uiop:run-program
                (list "env" (format nil "TUATARA_AI_MODEL=~a"
                                    (uiop:native-namestring (merge-pathnames "none.gguf" directory)))
                      "bash" (uiop:native-namestring (tools-script "possible-copies.sh")) "--alike-meaning")
                :output :string)))
      (is = 1 (count #\Newline out))
      (true (eql 0 (search "no meaning model" out))))))

(define-test (tools generated-parser-not-written-through-a-link)
  ;; a grammar repository that commits src as a link to a directory outside it gets nothing written there
  ;; by the parser taken back from the copy
  (with-test-directory (directory)
    (let* ((repo (uiop:native-namestring (merge-pathnames "g/" directory)))
           (host (uiop:native-namestring (merge-pathnames "host/" directory)))
           (cli (uiop:native-namestring (merge-pathnames "fake-tree-sitter" directory))))
      (ensure-directories-exist (merge-pathnames "host/" directory))
      (write-test-file directory "fake-tree-sitter"
                       (format nil "#!/bin/bash~%rm -rf src~%mkdir src~%echo 'int made;' > src/parser.c~%"))
      (sb-posix:chmod cli #o755)
      (let ((out (grammar-build-output
                  (format nil "mkdir -p '~a'; cd '~a'; ln -s '~a' src
( generate_parser '~a' '~a' ) 2>/dev/null; echo \"exit $?\"
[ -e '~a/parser.c' ] && echo wrote-outside || echo wrote-nothing-outside"
                          repo repo (string-right-trim "/" host) repo cli (string-right-trim "/" host))
                  directory)))
        (true (search "wrote-nothing-outside" out) "~a" out)))))

(define-test (tools downloads-checked-before-they-are-kept)
  ;; a file there with the hash is not fetched again, one that cannot be got or is not the one expected leaves
  ;; nothing, and only https is taken
  (with-test-directory (directory)
    (let ((file (uiop:native-namestring (merge-pathnames "kept" directory)))
          (hash "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))
      (write-test-file directory "kept" "abc")
      (flet ((run (url sha256)
               (multiple-value-bind (out error code)
                   (uiop:run-program (list "bash" "-c" ". \"$1\"; checked_download \"$2\" \"$3\" \"$4\"" "_"
                                           (tools-script "fetch.sh") url file sha256)
                                     :output :string :error-output :string :ignore-error-status t)
                 (declare (ignore out))
                 (values code error))))
        (is = 0 (run "https://127.0.0.1:9/never-asked" hash))
        (multiple-value-bind (code error) (run "https://127.0.0.1:9/other" (make-string 64 :initial-element #\0))
          (is = 1 code)
          (true (search "could not be downloaded" error)))
        (multiple-value-bind (code error) (run "http://127.0.0.1:9/plain" (make-string 64 :initial-element #\0))
          (is = 1 code)
          (true (search "could not be downloaded" error)))
        (false (probe-file (concatenate 'string file ".part")))
        (is string= "abc" (uiop:read-file-string file))))))

(define-test (tools release-sources-named-for-this-machine)
  ;; the second file of a release, the sources, is looked through for this machine's names as the first is,
  ;; but for the archives of pinned commits, the same on every machine: a path their authors wrote (a CI
  ;; runner's home) is theirs
  (let* ((release (uiop:read-file-string (tools-script "release.sh")))
         (scan "done < <(find \"$pack\" -type f ! -path \"$top/ghostty/*\" ! -path \"$top/grammars/*\" -print0)"))
    (true (search scan release))
    (true (< (search scan release) (search "tar -C \"$pack\" -cf - tuatara-sources" release)))))

(defun run-python-tool (name &rest arguments)
  "What the script NAME of tools/ prints, run with ARGUMENTS."
  (uiop:run-program (list* "python3" "-I" (tools-script name) arguments) :output :string))

(define-test (tools health-counts-packages-reaches-and-long-lines)
  ;; two colons between packages are counted where the file is in another package, not in its own, and not in
  ;; a test; the long lines of every repository given are listed
  (with-test-directory (directory)
    (write-test-file directory "r/a.lisp"
                     (format nil "(in-package #:one)~%(defun f () (two::g) (one::h) (three::i))  ; (x::y)~%~
                                  (defun long () \"~a\")~%" (make-string 130 :initial-element #\x)))
    (write-test-file directory "r/test/t.lisp" (format nil "(in-package #:one)~%(two::g)~%"))
    (write-test-file directory "q/b.lisp" (format nil "(in-package #:two)~%(defun k () (one::f))~%"))
    (loop for repo in '("r" "q")
          do (uiop:run-program (list "bash" "-c" "cd \"$1\" && git init -q && git add . && git -c user.name=t -c user.email=t@example.invalid commit -q -m x"
                                     "_" (uiop:native-namestring (merge-pathnames (format nil "~a/" repo) directory)))))
    (let ((r (uiop:native-namestring (merge-pathnames "r/" directory)))
          (q (uiop:native-namestring (merge-pathnames "q/" directory))))
      (let ((out (run-python-tool "package-reaches.py" r q)))
        (true (search "two::g" out))
        (true (search "three::i" out))
        (true (search "one::f" out))
        (false (search "one::h" out))
        (false (search "x::y" out))
        (true (search "3 references" out) "~a" out))
      (let ((out (run-python-tool "line-check.py" r q)))
        (true (search "r/a.lisp:3: 148" out) "~a" out)
        (true (search "1 lines past the column" out) "~a" out)))))

(defun tools-text (name)
  "The text of the file NAME in this repository's tools/."
  (uiop:read-file-string (tools-script name)))

(define-test (tools sanitized-grammar-compile)
  ;; fuzzing compiles a grammar as the build does but for the sanitizers, assertions kept, and links a C++ scanner
  ;; with them too
  (with-test-directory (directory)
    (let ((src (uiop:native-namestring (merge-pathnames "src/" directory))))
      (write-test-file directory "src/parser.c" "")
      (write-test-file directory "src/scanner.cc" "")
      (let ((lines (uiop:split-string
                    (string-right-trim '(#\Newline)
                                       (bash-output (format nil ". '~a'
cc() { printf 'cc'; printf ' [%s]' \"$@\"; echo; }
c++() { printf 'c++'; printf ' [%s]' \"$@\"; echo; }
compile_grammar \"$1\" /tmp/made-up/g.so --sanitize" (tools-script "grammar-build.sh")) src))
                    :separator '(#\Newline))))
        (is = 3 (length lines))
        (true (search "[-fsanitize=address,undefined]" (first lines)))
        (true (search "safe-ctype.h]" (first lines)))
        (true (search "[-fsanitize=address,undefined] [/tmp/made-up/g.so.parser.o]" (third lines)))
        (false (search "NDEBUG" (format nil "~{~a~}" lines)))
        (false (search "[-O2]" (format nil "~{~a~}" lines)))))))

(define-test (tools helper-compiled-one-way)
  ;; the program that runs the grammars is compiled from one function, by the build and by the fuzzer
  (let ((build (tools-text "grammar-build.sh")))
    (true (search "compile_helper() {" build))
    (true (search "compile_helper \"$root\"" (tools-text "grammars.sh")))
    (true (search "compile_helper \"$root\"" (tools-text "fuzz-helper.sh")))
    (false (search "-rdynamic" (tools-text "grammars.sh")))
    (false (search "-rdynamic" (tools-text "fuzz-helper.sh")))
    (true (search "run_shut_in \"$cli\" \"$copy\"" build))))

(define-test (tools grammar-manifest-read-once)
  ;; the manifest's rows and a grammar's directory come from one place, comments and blank lines left out
  (with-test-directory (directory)
    (let ((tools (merge-pathnames "tools/" directory)))
      (uiop:copy-file (tools-script "grammar-build.sh") (merge-pathnames "grammar-build.sh" (ensure-directories-exist tools)))
      (write-test-file directory "syntax/grammars.tsv"
                       (format nil "# comment~%a	https://x/a	1	MIT~%~%b	https://x/b	2	MIT	sub/dir~%  ~%"))
      (is string= (format nil "a	https://x/a	1	MIT~%b	https://x/b	2	MIT	sub/dir~%")
          (bash-output ". \"$1\"; grammar_manifest"
                       (uiop:native-namestring (merge-pathnames "grammar-build.sh" tools))))
      (is string= (format nil "sub/dir~%")
          (bash-output ". \"$1\"; grammar_subpath b" (uiop:native-namestring (merge-pathnames "grammar-build.sh" tools))))
      (is string= (format nil "~%")
          (bash-output ". \"$1\"; grammar_subpath a" (uiop:native-namestring (merge-pathnames "grammar-build.sh" tools)))))
    (dolist (script '("grammars.sh" "check-grammar-sources.sh" "fuzz-grammars.sh" "release.sh"))
      (false (search "grep -v '^#' \"$root/syntax/grammars.tsv\"" (tools-text script)) script)
      (false (search "$1 == n { print $5 }" (tools-text script)) script))))

(define-test (tools names-this-machine-looks-for-its-host-name)
  ;; the check that nothing published names this machine tries the host name too, and the manual's pages go
  ;; through the same check
  (with-test-directory (directory)
    (let ((file (uiop:native-namestring (merge-pathnames "page" directory))))
      (flet ((names-p (text)
               (with-open-file (o file :direction :output :if-exists :supersede) (write-string text o))
               (string= "yes" (string-trim '(#\Newline)
                                           (bash-output (format nil "export HOME=/home/annt USER=annt
hostname() { echo kiwi-box; }
. '~a'
names_this_machine \"$1\" 2>/dev/null && echo yes || echo no" (tools-script "inside.sh"))
                                                        file)))))
        (true (names-p "served from kiwi-box"))
        (true (names-p "kiwi-box, served"))
        (false (names-p "served from kiwi"))
        (false (names-p "kiwi-boxes and kiwi-boxed"))))
    ;; a host with a common word for a name is not found inside longer words, nor as a word of the text: only
    ;; where a machine writes a host name
    (let ((file (uiop:native-namestring (merge-pathnames "page" directory))))
      (flet ((names-p (host text)
               (with-open-file (o file :direction :output :if-exists :supersede) (write-string text o))
               (string= "yes" (string-trim '(#\Newline)
                                           (bash-output (format nil "export HOME=/home/annt USER=annt
hostname() { echo ~a; }
. '~a'
names_this_machine \"$1\" 2>/dev/null && echo yes || echo no" host (tools-script "inside.sh"))
                                                        file)))))
        (false (names-p "port" "a report and its porter"))
        (false (names-p "rata" "ratatouille"))
        (false (names-p "rata" "the rata is here"))
        (false (names-p "develop" "If you develop a new program, and you want it to be of use"))
        (false (names-p "develop" "a mail to @developers"))
        (true (names-p "rata" "annt@rata:~/src$"))
        (true (names-p "rata" "file://rata/shared/report.pdf"))))
    (let ((publish (tools-text "manual-publish.sh")))
      (true (search "names_this_machine" publish))
      (false (search "for word in" publish)))))

(define-test (tools lisp-archives-stop-without-quicklisp)
  ;; with no Quicklisp found the script stops, rather than going on in the current directory
  (with-test-directory (directory)
    (multiple-value-bind (script quicklisp) (archive-check-tree directory)
      (declare (ignore quicklisp))
      (multiple-value-bind (out error code)
          (uiop:run-program (list "env" "-i" (format nil "HOME=~a" (uiop:native-namestring directory))
                                  "PATH=/usr/bin:/bin" "bash" script "check")
                            :output :string :error-output :string :ignore-error-status t)
        (declare (ignore out))
        (is = 1 code)
        (true (search "Quicklisp is not installed" error))
        (false (search "dists/quicklisp/archives" error))))))

(define-test (tools lisp-archives-add-cleans-up-quietly)
  ;; adding a library leaves no complaint from the cleanup of its scratch directory, and its line is in the list
  (with-test-directory (directory)
    (multiple-value-bind (script quicklisp) (archive-check-tree directory)
      (let ((fakes (uiop:native-namestring (merge-pathnames "fakes/" directory))))
        (write-test-file directory "fakes/curl"
                         "#!/bin/sh
while [ $# -gt 0 ]; do case \"$1\" in -o) out=$2; shift 2 ;; *) url=$1; shift ;; esac; done
case \"$url\" in
  */releases.txt) echo 'proj https://example.org/proj-1.tgz 3 900150983cd24fb0d6963f7d28e17f72 0 0 proj-1' > \"$out\" ;;
  *) printf abc > \"$out\" ;;
esac
")
        (uiop:run-program (list "chmod" "+x" (concatenate 'string fakes "curl")))
        (multiple-value-bind (out error code)
            (uiop:run-program (list "env" (format nil "PATH=~a:/usr/bin:/bin" (string-right-trim "/" fakes))
                                    "bash" script "add" "proj" quicklisp)
                              :output :string :error-output :string :ignore-error-status t)
          (declare (ignore out))
          (is = 0 code "~a" error)
          (false (search "unbound" error))
          (true (search "proj-1.tgz" (uiop:read-file-string
                                      (merge-pathnames "tools/lisp-archives.sha256" directory)))))))))

(define-test (tools downloads-keep-to-https)
  ;; a download of the Lisp libraries or the grammars' hosts goes over HTTPS only, a redirect too, and never waits for good
  (multiple-value-bind (out error code)
      (uiop:run-program (list "bash" "-c" ". \"$1\"; https_curl -o /dev/null http://127.0.0.1:9/never" "_"
                              (tools-script "fetch.sh"))
                        :output :string :error-output :string :ignore-error-status t)
    (declare (ignore out))
    (is = 1 code)
    (true (search "disabled" error)))
  (let ((archives (tools-text "lisp-archives.sh")) (sources (tools-text "check-grammar-sources.sh")))
    (is = 0 (length (cl-ppcre:all-matches "(?m)^\\s*curl " archives)))
    (false (search "Authorization: Bearer $GITHUB_TOKEN\" \"https" sources))
    (false (search "curl -sSf \"https://codeberg" sources))
    (false (search "curl -sSf \"https://gitlab" sources))))

(define-test (tools sample-files-in-a-directory-of-their-own)
  ;; with no directory given the files go to a new one, mode 700, never to a name another account could have made
  (with-test-directory (directory)
    (let ((script (tools-script "sample-files.sh"))
          (temporary (string-right-trim "/" (uiop:native-namestring directory))))
      (flet ((made ()
               (let ((line (first (uiop:split-string
                                   (uiop:run-program (list "env" (format nil "TMPDIR=~a" temporary) "bash" script)
                                                     :output :string)
                                   :separator '(#\Newline)))))
                 (string-right-trim ":" (subseq line (1+ (position #\Space line :from-end t)))))))
        (let ((first (made)) (second (made)))
          (false (string= first second))
          (true (uiop:string-prefix-p (concatenate 'string temporary "/tuatara-sample-files.") first))
          (is string= "700" (string-trim '(#\Newline)
                                         (uiop:run-program (list "stat" "-c" "%a" first) :output :string))))
        (uiop:run-program (list "ln" "-s" temporary (concatenate 'string temporary "/link")))
        (multiple-value-bind (out error code)
            (uiop:run-program (list "bash" script (concatenate 'string temporary "/link"))
                              :output :string :error-output :string :ignore-error-status t)
          (declare (ignore out error))
          (is = 1 code))
        ;; also through a trailing slash, and for a directory others may write in
        (multiple-value-bind (out error code)
            (uiop:run-program (list "bash" script (concatenate 'string temporary "/link/"))
                              :output :string :error-output :string :ignore-error-status t)
          (declare (ignore out error))
          (is = 1 code))
        (let ((open (concatenate 'string temporary "/open")))
          (uiop:run-program (list "mkdir" "-m" "777" open))
          (multiple-value-bind (out error code)
              (uiop:run-program (list "bash" script open) :output :string :error-output :string
                                                          :ignore-error-status t)
            (declare (ignore out error))
            (is = 1 code))
          (uiop:run-program (list "chmod" "700" open))
          (is = 0 (nth-value 2 (uiop:run-program (list "bash" script open) :output :string))))))))

(define-test (tools scripts-name-no-person)
  ;; packages are made under TUATARA_PACKAGER or the project's name, not the builder's git identity; the CI's Zig
  ;; is the library pin's
  (flet ((packager (&optional set)
           (string-trim '(#\Newline)
                        (bash-output (format nil "~@[export TUATARA_PACKAGER='~a'~]
git() { echo 'Somebody <somebody@example.org>'; }
eval \"$(grep '^packager=' '~a')\"
echo \"$packager\"" set (tools-script "distro-packages.sh"))))))
    (is string= "A B <a@b.example>" (packager "A B <a@b.example>"))
    (let ((default (packager)))
      (true (search "Tuatara Editor <" default))
      (false (search "somebody" default))))
  (with-test-directory (directory)
    (write-test-file directory "lisp/terminal/library.pin" (format nil "zig	9.9.9~%"))
    (is string= "https://ziglang.org/download/9.9.9/zig-x86_64-linux-9.9.9.tar.xz"
        (string-trim '(#\Newline)
                     (bash-output (format nil "root='~a'
eval \"$(grep -E '^zig_(version|url)=' '~a')\"
echo \"$zig_url\"" (string-right-trim "/" (uiop:native-namestring directory)) (tools-script "ci-setup.sh")))))))

(define-test (tools grammar-probes-keep-the-token-off-the-command-line)
  ;; GitHub's API is asked with the token on curl's input, never in its arguments
  (with-test-directory (directory)
    (write-test-file directory "bin/curl"
                     (format nil "#!/bin/sh~%printf '%s\\n' \"$@\" > \"$RECORD.argv\"~%/usr/bin/cat > \"$RECORD.stdin\"~%echo '{}'~%"))
    (uiop:run-program (list "chmod" "+x" (uiop:native-namestring (merge-pathnames "bin/curl" directory))))
    (let ((record (uiop:native-namestring (merge-pathnames "record" directory)))
          (bin (string-right-trim "/" (uiop:native-namestring (merge-pathnames "bin/" directory)))))
      (uiop:run-program
       (list "env" (format nil "RECORD=~a" record) "GITHUB_TOKEN=s3cret"
             "/usr/bin/bash" "-c"
             "text=\"$(sed -n '/^https_only=/p;/^github() {/,/^}/p' \"$1\")\"; PATH=$2; eval \"$text\"; github repos/a/b"
             "_" (tools-script "check-grammar-sources.sh") bin)
       :output :string :ignore-error-status t)
      (let ((argv (uiop:read-file-string (concatenate 'string record ".argv")))
            (stdin (uiop:read-file-string (concatenate 'string record ".stdin"))))
        (false (search "s3cret" argv))
        (true (search "s3cret" stdin))
        (true (search "--proto" argv))))))

(define-test (tools desktop-installer-builds-before-copying)
  ;; the completion is copied after the entries are made, which build the program that writes it, and said only
  ;; when it was copied
  (let ((text (tools-text "install-desktop.sh")))
    (true (< (search "entries >/dev/null" text) (search "cp \"$source_completion\"" text)))
    (true (search "completion_copied" text))))

(define-test (tools fuzzers-catch-hangs-and-keep-going)
  ;; the SFTP reader is cut at the deadline, and the second connection of the helper fuzzer goes on after the
  ;; first one it loses
  (true (search "(sb-ext:with-timeout *deadline* (read-stream octets))" (tools-text "fuzz-sftp.lisp")))
  (let ((helper (tools-text "fuzz-helper.py")))
    (false (search "except (Ended, ConnectionError, socket.timeout, OSError):
            return" helper))
    (true (search "class Mismatch" helper))
    (true (search "other = connect()" helper))
    (true (search "'interfering'" helper)))
  (true (search "(defpackage #:tuatara.bidi-conformance" (tools-text "bidi-conformance.lisp"))))

(define-test (tools python-tools-documented)
  ;; every module, function and class of the Python tools has a docstring
  (is string= "" (string-trim '(#\Newline)
                              (uiop:run-program
                               (list "python3" "-I" "-c" "import ast, glob, sys
for name in glob.glob(sys.argv[1] + '/*.py') + glob.glob(sys.argv[1] + '/languages/*.py'):
    tree = ast.parse(open(name).read())
    if not ast.get_docstring(tree):
        print(name, 'module')
    for node in ast.walk(tree):
        if isinstance(node, (ast.FunctionDef, ast.ClassDef)) and not ast.get_docstring(node):
            print(name, node.name, node.lineno)"
                                     (string-right-trim "/" (tools-script "")))
                               :output :string))))

(define-test (tools helix-commit-needs-helix)
  ;; with no Helix checkout the generators stop, rather than writing a notice naming no commit
  (multiple-value-bind (out error code)
      (uiop:run-program (list "env" "HELIX_SOURCE=/nonexistent/helix" "python3" "-I" "-c"
                              (format nil "import sys; sys.path.insert(0, '~a'); import common; common.helix_commit()"
                                      (tools-script "languages")))
                        :output :string :error-output :string :ignore-error-status t)
    (declare (ignore out error))
    (is /= 0 code)))

(define-test (tools meaning-cache-follows-the-model)
  ;; the cached vectors are kept for each model file, not mixed when the model is replaced
  (with-test-directory (directory)
    (write-test-file directory "a.gguf" "one")
    (write-test-file directory "b.gguf" "three")
    (flet ((path (model)
             (uiop:run-program (list "env" (format nil "TUATARA_AI_MODEL=~a" (uiop:native-namestring (merge-pathnames model directory)))
                                     (format nil "TUATARA_AI_CACHE=~a" (uiop:native-namestring directory))
                                     "python3" "-I" "-c"
                                     (format nil "import sys; sys.path.insert(0, '~a'); import meaning; print(meaning.cache_path())"
                                             (string-right-trim "/" (tools-script ""))))
                               :output :string)))
      (false (string= (path "a.gguf") (path "b.gguf")))
      (true (search "a.gguf-3" (path "a.gguf"))))))

(define-test (tools package-roots-are-repositories)
  ;; the package repositories beside the checkout: those with a .git directory, not the worktrees of their branches
  (with-test-directory (directory)
    (write-test-file directory "tuatara/tools/repositories.py" (tools-text "repositories.py"))
    (ensure-directories-exist (merge-pathnames "tuatara-packages/tuatara-a/.git/" directory))
    (ensure-directories-exist (merge-pathnames "tuatara-packages/tuatara-b/" directory))
    (write-test-file directory "tuatara-packages/tuatara-b/.git" "gitdir: elsewhere")
    (ensure-directories-exist (merge-pathnames "tuatara-packages/tuatara-c-work/.git/" directory))
    (ensure-directories-exist (merge-pathnames "tuatara-packages/other/.git/" directory))
    (is string= (format nil "tuatara-a tuatara-c-work~%")
        (uiop:run-program (list "python3" "-I" "-c"
                                (format nil "import sys; sys.path.insert(0, '~a'); import repositories
print(*[p.name for p in repositories.package_roots()])"
                                        (uiop:native-namestring (merge-pathnames "tuatara/tools" directory))))
                          :output :string))))

(define-test (tools package-reaches-reads-code-only)
  ;; a double colon inside a string or a block comment is no reach; a semicolon inside a string or a character
  ;; does not hide what follows it
  (with-test-directory (directory)
    (write-test-file directory "r/a.lisp"
                     (format nil "(in-package #:one)~%(format t \"two::g ~~a ; x\" (y))~%(f #\\; three::i)~%~
                                  #| four::j |#~%(five::k \"a;b\" six::l)~%"))
    (uiop:run-program (list "bash" "-c" "cd \"$1\" && git init -q && git add . && git -c user.name=t -c user.email=t@example.invalid commit -q -m x"
                            "_" (uiop:native-namestring (merge-pathnames "r/" directory))))
    (let ((out (run-python-tool "package-reaches.py" (uiop:native-namestring (merge-pathnames "r/" directory)))))
      (false (search "two::g" out))
      (false (search "four::j" out))
      (true (search "three::i" out))
      (true (search "five::k" out))
      (true (search "six::l" out))
      (true (search "3 references" out) "~a" out))))
