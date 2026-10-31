#!/usr/bin/env bash
# tools/fuzz-grammars.sh --- Fuzz the tree-sitter grammars.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Fuzz the grammars of syntax/grammars.tsv, or those named, with the fuzzer of
# the pinned tree-sitter program (in the toolbox when there is one): each case of a grammar's
# own test corpus is edited at random, parsed again after every edit, and the
# tree compared with the one the corpus expects once the edits are undone. A
# crash, a hang or a tree that differs is a failure; a case whose first parse
# already differs (a corpus written for another version of tree-sitter) is
# counted apart, not fuzzed. The samples of a grammar's languages in
# test/languages.lisp are cases too, each expecting the tree the grammar gives
# it now: a grammar without a corpus of its own is fuzzed on them alone. The grammar is compiled as
# tools/grammars.sh compiles it (tools/grammar-build.sh, a scanner in C++ too),
# its assertions kept to find more.
# A grammar's pin is moved only after this passes for it.
#
# With --hostile, each grammar with a scanner (or each named) is compiled as
# it is built, its patches applied, but with AddressSanitizer and
# UndefinedBehaviorSanitizer, as are tree-sitter and tools/grammar-hostile.c,
# which parses texts made to be hard on a scanner (ROUNDS of them, 300 by
# default: random octets, text not UTF-8, nesting thousands deep, a line of a
# megabyte, the samples and the corpus cut and spliced), each edited and
# parsed again. The first fault a sanitizer sees fails the grammar, the log
# saying where. Each run is held to 30 minutes of the processor, 4 GB of
# memory as the sanitizer counts it, an hour in all, at a low priority.
# Usage: tools/fuzz-grammars.sh [-n ITERATIONS] [NAME...]
#        tools/fuzz-grammars.sh --hostile [-n ROUNDS] [NAME...]
set -euo pipefail
cd "$(dirname "$0")/.."
. tools/inside.sh
iterations=200 hostile=""
if [ "${1:-}" = "--hostile" ]; then hostile=1; iterations=300; shift; fi
if [ "${1:-}" = "-n" ]; then iterations=$2; shift 2; fi
tools/grammars.sh
mkdir -p build/fuzz/cases build/tools/fuzz-lib
names=("$@")
if [ ${#names[@]} -eq 0 ]; then
  mapfile -t names < <(awk -F'\t' '!/^#/ && NF >= 4 && $1 != "tree-sitter" && $1 != "tree-sitter-cli" && $1 !~ /\// { print $1 }' syntax/grammars.tsv)
  # hostile texts are for the scanners, the code of a grammar's own
  if [ -n "$hostile" ]; then
    mapfile -t names < <(for name in "${names[@]}"; do
      subpath=$(awk -F'\t' -v n="$name" '$1 == n { print $5 }' syntax/grammars.tsv)
      ls "build/grammar-sources/$name${subpath:+/$subpath}"/src/scanner.c* >/dev/null 2>&1 && echo "$name"
    done)
  fi
fi
# the samples of test/languages.lisp, by grammar: build/fuzz/samples/GRAMMAR/LANGUAGE.txt
rm -rf build/fuzz/samples
python3 - <<'EOF'
import glob, os, re
grammar_of = {}
for path in ['lisp/modes/languages.lisp'] + sorted(glob.glob('lisp/modes/more-languages-*.lisp')):
    text = open(path, encoding='utf-8').read()
    for m in re.finditer(r'\(define-language\s+([^\s()]+)\s+"(?:[^"\\]|\\.)*"\s+:grammar\s+"([^"]+)"', text):
        grammar_of[m.group(1)] = m.group(2)
text = open('test/languages.lisp', encoding='utf-8').read()
i = text.index("(defparameter *language-samples*")
i = text.index("'(", i) + 2
while True:
    while text[i].isspace(): i += 1
    if text[i] == ')': break
    assert text[i] == '(', text[i:i + 40]
    j = i + 1
    while not text[j].isspace(): j += 1
    language = text[i + 1:j]
    while text[j].isspace(): j += 1
    assert text[j] == '"', text[i:i + 40]
    j += 1
    out = []
    while text[j] != '"':
        if text[j] == '\\': j += 1
        out.append(text[j]); j += 1
    j += 1
    while text[j].isspace(): j += 1
    assert text[j] == ')', text[i:j + 1]
    i = j + 1
    grammar = grammar_of.get(language)
    if grammar:
        directory = os.path.join('build/fuzz/samples', grammar)
        os.makedirs(directory, exist_ok=True)
        with open(os.path.join(directory, language + '.txt'), 'w', encoding='utf-8') as f:
            f.write(''.join(out))
EOF
failed=0
if [ -n "$hostile" ]; then
  work="$PWD/build/fuzz/hostile"
  mkdir -p "$work"
  sanitize=(-g -O1 -fno-omit-frame-pointer -fsanitize=address,undefined -fno-sanitize-recover=undefined)
  inside bash -euo pipefail -c '
    ts=$1; out=$2; shift 2
    cc "$@" -std=c11 -D_POSIX_C_SOURCE=200112L -D_DEFAULT_SOURCE -I"$ts/lib/include" -I"$ts/lib/src" \
       "$ts/lib/src/lib.c" tools/grammar-hostile.c -ldl -o "$out/grammar-hostile"
  ' hostile "$PWD/build/grammar-sources/tree-sitter" "$work" "${sanitize[@]}"
  for name in "${names[@]}"; do
    subpath=$(awk -F'\t' -v n="$name" '$1 == n { print $5 }' syntax/grammars.tsv)
    src="$PWD/build/grammar-sources/$name${subpath:+/$subpath}/src"
    log="build/fuzz/$name.hostile.log"
    start=$(date +%s)
    # the texts the grammar is tried on besides the made ones: its samples and corpus
    texts=()
    for text in "$PWD"/build/fuzz/samples/"$name"/*.txt "$src"/../test/corpus/*.txt "$src"/../corpus/*.txt; do
      [ -f "$text" ] && texts+=("$text")
    done
    if inside bash -euo pipefail -c '
         src=$1; lib=$2; name=$3; rounds=$4; harness=$5; root=$6; shift 6
         flags=(-g -O1 -fno-omit-frame-pointer -fsanitize=address,undefined -fno-sanitize-recover=undefined -fPIC -fno-strict-aliasing)
         if [ -f "$src/scanner.cc" ]; then
           cc "${flags[@]}" -std=c11 -I"$src" -c "$src/parser.c" -o "$lib.parser.o"
           c++ "${flags[@]}" -fvisibility=hidden -I"$src" -c "$src/scanner.cc" -o "$lib.scanner.o"
           c++ -shared -fsanitize=address,undefined "$lib.parser.o" "$lib.scanner.o" -o "$lib"
         else
           files=("$src/parser.c"); [ -f "$src/scanner.c" ] && files+=("$src/scanner.c")
           cc "${flags[@]}" -shared -std=c11 -include "$root/syntax/safe-ctype.h" -include "$root/syntax/counted-memory.h" \
              -I"$src" "${files[@]}" -o "$lib"
         fi
         ulimit -t 1800
         export ASAN_OPTIONS=hard_rss_limit_mb=4096:allocator_may_return_null=1:detect_leaks=0:handle_abort=1
         export UBSAN_OPTIONS=halt_on_error=1:print_stacktrace=1
         exec nice -n 10 timeout 3600 "$harness" "$lib" "$name" "$rounds" 1 "$@"
       ' hostile "$src" "$work/$name.so" "${name//-/_}" "$iterations" "$work/grammar-hostile" "$PWD" ${texts[@]+"${texts[@]}"} \
       > "$log" 2>&1; then
      echo "$name: passed, $(tail -1 "$log" | sed "s/^[^:]*: //") ($(( $(date +%s) - start )) s)"
    else
      status=$?
      why=$(grep -a -m1 -E "ERROR: AddressSanitizer|runtime error:|SUMMARY:" "$log" || true)
      echo "$name: FAILED ($([ "$status" -eq 124 ] && echo "no end in an hour" || echo "exit $status")${why:+: $why}), see $log"
      failed=1
    fi
  done
  exit $failed
fi
for name in "${names[@]}"; do
  # a grammar in a directory of its repository (the manifest's fifth column)
  subpath=$(awk -F'\t' -v n="$name" '$1 == n { print $5 }' syntax/grammars.tsv)
  dir="build/grammar-sources/$name${subpath:+/$subpath}"
  # its corpus in test/corpus, tests/corpus or corpus, in its directory or at
  # the top of its repository; and the samples
  corpus="" samples=""
  for candidate in "$dir/test/corpus" "$dir/tests/corpus" "$dir/corpus" \
                   "build/grammar-sources/$name/test/corpus" "build/grammar-sources/$name/tests/corpus" \
                   "build/grammar-sources/$name/corpus"; do
    if [ -d "$candidate" ]; then corpus="$candidate"; break; fi
  done
  if [ -d "build/fuzz/samples/$name" ]; then samples="$PWD/build/fuzz/samples/$name"; fi
  if [ -z "$corpus" ] && [ -z "$samples" ]; then
    echo "$name: no test corpus or sample to fuzz"
    continue
  fi
  start=$(date +%s)
  log="build/fuzz/$name.log"
  lib="$PWD/build/tools/fuzz-lib/$name.so"
  # the fuzzer is run where the corpus is all it finds, so that it takes the
  # grammar compiled here rather than compiling it itself
  cases="build/fuzz/cases/$name"
  mkdir -p "$cases/test"
  rm -rf "$cases/test/corpus"
  mkdir "$cases/test/corpus"
  if [ -n "$corpus" ]; then cp -r "$corpus"/. "$cases/test/corpus/"; fi
  if inside bash -euo pipefail -c '
       src=$1; lib=$2
       . "$3"
       compile_grammar "$src" "$lib" --assertions
       # the samples as cases, each expecting the tree the grammar gives it; the
       # text of a case ends before the line end ahead of its divider, so one
       # more goes after the sample
       if [ -n "$8" ]; then
         bar=$(printf "%080d" 0 | tr 0 =) dashes=$(printf "%080d" 0 | tr 0 -)
         for sample in "$8"/*.txt; do
           sample_name=$(basename "$sample" .txt)
           tree=$("$5" parse --no-ranges --lib-path "$lib" --lang-name "$6" "$sample" 2>/dev/null || true)
           { printf "%s\n%s\n%s\n" "$bar" "$sample_name" "$bar"; cat "$sample"; printf "\n%s\n\n%s\n" "$dashes" "$tree"; } \
             > "$4/test/corpus/sample-$sample_name.txt"
         done
       fi
       # a grammar gone wrong can build without end: 8 GB at most, then it
       # fails; and at a low priority, the editor and its tests going first
       ulimit -v 8000000
       cd "$4" && exec nice -n 10 timeout 3600 "$5" fuzz --lib-path "$lib" --lang-name "$6" --iterations "$7" --edits 3
     ' fuzz "$PWD/$dir/src" "$lib" "$PWD/tools/grammar-build.sh" "$cases" "$PWD/build/tools/tree-sitter" "${name//-/_}" "$iterations" \
       "$samples" > "$log" 2>&1; then
    status=0
  else
    status=$?
  fi
  total=$(grep -a -c -E '^ +[0-9]+\. .* - corpus - ' "$log" || true)
  first=$(grep -a -c '^Incorrect initial parse' "$log" || true)
  edited=$(grep -a -c -E '^Incorrect parse for .* - seed [0-9]+' "$log" || true)
  aside=""
  if [ "$first" -gt 0 ]; then aside=", $first whose first parse differs from the corpus left out"; fi
  if [ "$status" -ne 0 ]; then
    echo "$name: FAILED ($([ "$status" -eq 124 ] && echo "no end in an hour" || echo "exit $status")), see $log"
    failed=1
  elif [ "$edited" -gt 0 ]; then
    echo "$name: FAILED, $edited of $total cases parsed wrongly after edits$aside, see $log"
    failed=1
  else
    echo "$name: passed, $iterations rounds of edits on each of $((total - first)) cases$aside, ${corpus:+its corpus}${corpus:+${samples:+ and }}${samples:+the samples} ($(( $(date +%s) - start )) s)"
  fi
done
exit $failed
