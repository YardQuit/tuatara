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
  (let ((line (bash-output (format nil "eval \"$(sed -n '/^exec_argument() {/,/^}/p' '~a')\"
printf 'Exec=@EXEC@ %%F\\n' | sed \"s|@EXEC@|$(exec_argument \"$1\")|\"" (tools-script "install-desktop.sh"))
                           "/opt/50%/a\\b\"c`d$e f|g&h/tuatara")))
    (is string= (format nil "Exec=\"/opt/50%%/a\\\\\\\\b\\\\\"c\\\\`d\\\\$e f|g&h/tuatara\" %F~%") line)))

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

(define-test (tools release-names-no-home-folder)
  ;; the release's program is built from a copy in a folder of its own, and refused
  ;; when it names this machine's home folder or user, which the check finds
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
      (file "syntax/grammars.tsv" (format nil "said	https://example.org/said	0	unknown~%unsaid	https://example.org/unsaid	0	unknown~%plain	https://example.org/plain	0	unknown"))
      (dolist (name '("said" "unsaid"))
        (file (format nil "build/grammar-sources/~a/COPYING" name)
              "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 ... either version 3 of the License, or (at your option) any later version."))
      (file "build/grammar-sources/said/package.json" "{\"license\": \"GPL-3.0-or-later\"}")
      (file "build/grammar-sources/unsaid/package.json" "{\"license\": \"GPL-3.0\"}")
      (file "build/grammar-sources/plain/LICENSE" "Permission is hereby granted, free of charge, to any person")
      (uiop:run-program (list "python3" "-I" (tools-script "languages/licenses.py"))
                        :directory directory :output nil :error-output nil)
      (is equal '("GPL-3.0-or-later" "unknown" "MIT")
          (mapcar (lambda (line) (fourth (uiop:split-string line :separator '(#\Tab))))
                  (uiop:read-file-lines (merge-pathnames "syntax/grammars.tsv" directory)))))))

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
and the Quicklisp folder."
  (let* ((tools (merge-pathnames "tools/" directory))
         (quicklisp (merge-pathnames "ql/" directory))
         (archives (merge-pathnames "dists/quicklisp/archives/" quicklisp)))
    (ensure-directories-exist tools)
    (ensure-directories-exist archives)
    (uiop:copy-file (tools-script "lisp-archives.sh") (merge-pathnames "lisp-archives.sh" tools))
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
                   "mtoohey31/tree-sitter-gitattributes"))
      (false (find (format nil "https://github.com/~a" old) rows :key #'second :test #'string-equal)
             "~a has moved" old))
    ;; archived, their development carried on elsewhere
    (dolist (old '("https://github.com/poliorcetics/tree-sitter-just" "https://github.com/inko-lang/tree-sitter-inko"
                   "https://codeberg.org/maribu/tree-sitter-gnuplot" "https://codeberg.org/maribu/tree-sitter-proverif"))
      (false (find old rows :key #'second :test #'string-equal) "~a is archived" old))
    (dolist (orphan '("3d7c53b8038f9ebbb57cd2e61296180aa5c1cf64" "29f9c6707ce9dfc2fc915d175ec720b207f179f3"
                      "d98426109258b266e1e92358c5f11716d2e8f638"))
      (false (find orphan rows :key #'third :test #'string=) "~a is on no branch or tag" orphan))))

(defun grammar-build-output (script directory)
  "What bash prints running SCRIPT in DIRECTORY with tools/grammar-build.sh sourced, standard error with it."
  (uiop:run-program (list "bash" "-c" (format nil ". '~a'~%~a" (tools-script "grammar-build.sh") script))
                    :directory directory :output :string :error-output :output :ignore-error-status t))

(defun write-test-file (directory name text)
  "Write TEXT to NAME under DIRECTORY, its folders made."
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
        (uiop:split-string (string-trim '(#\Newline) (grammar-build-output "grammar_inputs \"$PWD\" \"$PWD/src\"" directory))
                           :separator '(#\Newline)))
    (flet ((hash () (string-trim '(#\Newline) (grammar-build-output "grammar_inputs_hash \"$PWD\" \"$PWD/src\"" directory))))
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
      (let ((commits (uiop:split-string
                      (string-trim '(#\Newline)
                                   (bash-output (format nil "set -e
git init -q '~a'; cd '~:*~a'
c() { git -c user.name=t -c user.email=t@example.invalid commit -q --allow-empty -m \"$1\"; }
c one; git rev-parse HEAD
git checkout -q -b gone; c lost; git rev-parse HEAD
git checkout -q -; git branch -q -D gone" repo)))
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
            (true (search "on no branch or tag" out))))))))

(define-test (tools archived-grammars-left-out)
  ;; a grammar whose repository is archived, with no source kept up in its
  ;; place, is left out; the mode of its files stays, with no language
  (let ((rows (manifest-rows)))
    (loop for (grammar mode) in '(("rust-format-args" nil) ("djot" tuatara.defaults::djot-mode)
                                  ("cpon" tuatara.defaults::cpon-mode) ("move" tuatara.defaults::move-mode))
          do (false (find grammar rows :key #'first :test #'string=) "~a is left out" grammar)
             (false (find grammar tuatara.defaults::*languages* :key #'tuatara.defaults::language-grammar :test #'string=))
             (when mode
               (true (tuatara.modes:find-mode mode) "~(~a~) stays" mode)
               (false (tuatara.defaults::mode-language (make-instance mode)))))))
