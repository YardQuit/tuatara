;;;; test/languages.lisp --- Every language: its grammar, its rules, a sample coloured, its files.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test languages
  :description "Every language has its grammar and rules that compile (colouring, indentation,
selection, text in other languages, local names, definitions, definitions and headings for the
outline, a ; inherits: A,B line taking in the rules of A and B), a sample of each parses without
errors and is coloured, and files find their languages' modes by extension (as written, then in
any case), name and first line.")

(define-test (languages every-language-works)
  ;; the grammar is there, and every set of rules the language has compiles
  (dolist (language tuatara.defaults::*languages*)
    (let* ((name (string-downcase (symbol-name (tuatara.defaults::language-name language))))
           (grammar (tuatara.syntax:find-grammar (tuatara.defaults::language-grammar language))))
      (true grammar "the grammar of ~a" name)
      (when grammar
        (true (tuatara.defaults::language-compiled-query language :colouring) "the colouring rules of ~a" name)
        (loop for (what source) in `(("indentation" ,(tuatara.defaults::language-indents language))
                                     ("selection" ,(tuatara.defaults::language-textobjects language))
                                     ("injection" ,(tuatara.defaults::language-injections language))
                                     ("local names" ,(tuatara.defaults::language-locals language))
                                     ("definition" ,(tuatara.defaults::language-definitions language))
                                     ("tags" ,(tuatara.defaults::language-tags language)))
              when source
                do (true (handler-case (tuatara.syntax:compile-query grammar source) (error () nil))
                         "the ~a rules of ~a" what name))))))

(defparameter *language-samples*
  '((toml "[server]
name = \"x\"
port = 8080
")
    (json "{\"a\": [1, 2, true, null]}
")
    (jsonc "{
  // a comment
  \"a\": 1
}
")
    (json5 "{a: 1, 'b': [2,],}
")
    (xml "<?xml version=\"1.0\"?>
<root a=\"1\"><b>text</b></root>
")
    (ini "[section]
key = value
")
    (markdown "# Title

Some *text* and `code`.

```python
x = 1
```
")
    (html "<!DOCTYPE html>
<html><body><p class=\"x\">Hi</p></body></html>
")
    (css "body { color: red; margin: 0 auto; }
")
    (scss "$c: red;
.a { .b { color: $c; } }
")
    (javascript "function f(x) {
  return x + 1;
}
const y = f(2);
")
    (jsx "const e = <div className=\"a\">{x}</div>;
")
    (typescript "function f(x: number): number {
  return x + 1;
}
")
    (tsx "const e = <div>{x}</div>;
")
    (python "def f(x):
    return x + 1

class A:
    pass
")
    (go "package main

func main() {
	x := 1
	_ = x
}
")
    (gomod "module example.com/x

go 1.21
")
    (java "class A {
  int f(int x) { return x + 1; }
}
")
    (cpp "#include <vector>
class A { public: int f(int x) { return x + 1; } };
")
    (c-sharp "class A {
  int F(int x) { return x + 1; }
}
")
    (lua "local function f(x)
  return x + 1
end
")
    (ruby "def f(x)
  x + 1
end
")
    (php "<?php
function f($x) { return $x + 1; }
")
    (perl "sub f { my ($x) = @_; return $x + 1; }
")
    (sql "SELECT a, b FROM t WHERE a > 1;
")
    (make "all: main.o
	cc -o main main.o
")
    (cmake "cmake_minimum_required(VERSION 3.10)
project(x)
")
    (nix "{ pkgs }: pkgs.hello
")
    (diff "--- a/x
+++ b/x
@@ -1 +1 @@
-old
+new
")
    (git-config "[user]
	name = Me
")
    (git-ignore "*.o
/build/
!keep.o
")
    (git-attributes "*.txt text eol=lf
")
    (git-rebase "pick abc1234 A commit
squash def5678 Another
")
    (elisp "(defun f (x)
  \"Doc.\"
  (1+ x))
")
    (zig "pub fn main() void {
    const x: u32 = 1;
    _ = x;
}
")
    (haskell "f :: Int -> Int
f x = x + 1
")
    (kotlin "fun f(x: Int): Int {
    return x + 1
}
")
    (swift "func f(x: Int) -> Int {
    return x + 1
}
")
    (scala "object A {
  def f(x: Int): Int = x + 1
}
")
    (dart "int f(int x) {
  return x + 1;
}
")
    (elixir "defmodule A do
  def f(x), do: x + 1
end
")
    (erlang "-module(a).
f(X) -> X + 1.
")
    (ocaml "let f x = x + 1
")
    (ocaml-interface "val f : int -> int
")
    (latex "\\documentclass{article}
\\begin{document}
Hello \\emph{world}.
\\end{document}
")
    (bibtex "@article{key,
  title = {A Title},
  year = 2020
}
")
    (fish "function f
    echo $argv
end
")
    (awk "{ print $1 }
")
    (just "build:
    cargo build
")
    (protobuf "syntax = \"proto3\";
message A { int32 x = 1; }
")
    (graphql "query Q { a { b } }
")
    (hcl "resource \"a\" \"b\" {
  x = 1
}
")
    (svelte "<script>
  let x = 1;
</script>
<p>{x}</p>
")
    (powershell "function F($x) { return $x + 1 }
")
    (nginx "server {
  listen 80;
}
")
    (systemd "[Unit]
Description=X

[Service]
ExecStart=/bin/true
")
    (env "A=1
B=\"two\"
")
    (ssh_client_config "Host example
  User me
  Port 22
")
    (dot "digraph G { a -> b; }
")
    (meson "project('x', 'c')
executable('x', 'x.c')
")
    (groovy "def f(x) { return x + 1 }
")
    (r "f <- function(x) { x + 1 }
")
    (julia "function f(x)
    x + 1
end
")
    (clojure "(defn f [x] (inc x))
")
    (scheme "(define (f x) (+ x 1))
")
    (racket "(define (f x) (+ x 1))
")
    (fennel "(fn f [x] (+ x 1))
")
    (vim "let g:x = 1
")
    (log "2024-01-01 12:00:00 INFO Started
")
    (jq ".a | map(. + 1)
")
    (regex "a+b*[c-d]")
    (markdown-inline "*a* and `b`")
    (comment "TODO(me): check #123 with @someone")
    (jsdoc "/**
 * @param {number} x The value.
 */")
    (luap "^%d+%s*$")
    (rust-format-args "{} {name:>8} {0:?}")
    (rust-format-args-macro "fn main() { println!(\"{}\", 1); }
")
    (go-format-string "%d items, %-8s")
    (lua-format-string "%5.2f %s")
    (ada "procedure Hello is
begin
   null;
end Hello;
")
    (d "void main() {
    int x = 1;
}
")
    (elm "module Main exposing (main)

main =
    42
")
    (fortran "program hello
  print *, 'Hello'
end program hello
")
    (fsharp "let f x = x + 1
")
    (gleam "pub fn main() {
  1 + 1
}
")
    (nim "proc f(x: int): int =
  x + 1
")
    (odin "package main

main :: proc() {
	x := 1
}
")
    (verilog "module m(input a, output b);
  assign b = a;
endmodule
")
    (v "fn main() {
	x := 1
	println(x)
}
")
    (hare "export fn main() void = {
	let x = 1;
};
")
    (crystal "def f(x)
  x + 1
end
")
    (purescript "module Main where

f :: Int -> Int
f x = x + 1
")
    (sml "fun f x = x + 1
")
    (prolog "parent(tom, bob).
ancestor(X, Y) :- parent(X, Y).
")
    (tcl "proc f {x} {
    return [expr {$x + 1}]
}
")
    (pascal "program Hello;
begin
  writeln('Hi');
end.
")
    (matlab "function y = f(x)
  y = x + 1;
end
")
    (vala "int main () {
    return 0;
}
")
    (solidity "pragma solidity ^0.8.0;
contract A {
    uint x = 1;
}
")
    (gdscript "extends Node

func f(x):
	return x + 1
")
    (haxe "class Main {
  static function main() {
    trace(1);
  }
}
")
    (janet "(defn f [x] (+ x 1))
")
    (hy "(defn f [x] (+ x 1))
")
    (teal "local function f(x: number): number
  return x + 1
end
")
    (luau "local function f(x: number): number
  return x + 1
end
")
    (nu "def f [x: int] {
  $x + 1
}
")
    (elvish "fn f {|x| put $x }
")
    (forth ": square ( n -- n ) dup * ;
")
    (dune "(executable
 (name main))
")
    (llvm "define i32 @f(i32 %x) {
  %y = add i32 %x, 1
  ret i32 %y
}
")
    (gas "	.text
	.globl main
main:
	movl $0, %eax
	ret
")
    (nasm "section .text
global _start
_start:
    mov eax, 1
    ret
")
    (wat "(module
  (func $f (param i32) (result i32)
    local.get 0))
")
    (glsl "void main() {
    gl_FragColor = vec4(1.0);
}
")
    (wgsl "@fragment
fn main() -> @location(0) vec4<f32> {
    return vec4<f32>(1.0);
}
")
    (vhdl "entity e is
end entity e;
")
    (devicetree "/dts-v1/;
/ {
	model = \"x\";
};
")
    (kconfig "config FOO
	bool \"Foo\"
	default y
")
    (ld "SECTIONS
{
  .text : { *(.text) }
}
")
    (bitbake "SUMMARY = \"x\"
LICENSE = \"MIT\"
")
    (thrift "struct A {
  1: i32 x
}
")
    (capnp "@0xdbb9ad1f14bf0b36;
struct A {
  x @0 :Int32;
}
")
    (prisma "model User {
  id Int @id
}
")
    (cython "cdef int f(int x):
    return x + 1
")
    (gnuplot "set title \"x\"
plot sin(x)
")
    (openscad "cube([1, 2, 3]);
")
    (smali ".class public LA;
.super Ljava/lang/Object;
")
    (yara "rule A
{
    strings:
        $a = \"x\"
    condition:
        $a
}
")
    (rego "package example

allow {
    input.x == 1
}
")
    (starlark "load(\"//x:y.bzl\", \"z\")

cc_binary(
    name = \"x\",
)
")
    (rst "Title
=====

Some *text*.
")
    (typst "= Title

Some *text*.
")
    (djot "# Title

Some _text_.
")
    (mermaid "flowchart TD
    A --> B
")
    (gemini "# Title
=> gemini://example.org Link
")
    (wikitext "== Title ==
'''bold''' text
")
    (kdl "node \"arg\" key=1 {
    child
}
")
    (hocon "a {
  b = 1
}
")
    (cue "a: 1
b: \"x\"
")
    (dhall "let x = 1 in x
")
    (nickel "let x = 1 in x
")
    (pkl "name = \"x\"
port = 8080
")
    (jsonnet "{ a: 1 + 1 }
")
    (ron "(a: 1, b: \"x\")
")
    (properties "key=value
other = 1
")
    (hosts "127.0.0.1 localhost
::1 localhost
")
    (passwd "root:x:0:0:root:/root:/bin/bash
")
    (rpmspec "Name: x
Version: 1.0
Release: 1

%description
X.
")
    (debian "Source: x
Section: misc
")
    (po "msgid \"Hello\"
msgstr \"Kia ora\"
")
    (ledger "2024/01/01 Shop
    Expenses:Food    $10
    Assets:Cash
")
    (todotxt "(A) Call Mum +family @phone
")
    (pem "-----BEGIN CERTIFICATE-----
MIIB
-----END CERTIFICATE-----
")
    (ldif "dn: CN=Jane Smith,OU=Accounting,DC=example,DC=com
changetype: modify
replace: employeeID
employeeID: 5678
-
")
    (strace "open(\"/etc/passwd\", O_RDONLY) = 3
")
    (robots-txt "User-agent: *
Disallow: /private/
")
    (pip-requirements "requests>=2.0
flask==3.0.0
")
    (hyprlang "general {
    gaps_in = 5
}
")
    (caddyfile "example.com {
	reverse_proxy localhost:8080
}
")
    (jinja "{% if x %}{{ x }}{% endif %}
")
    (twig "{% if x %}{{ x }}{% endif %}
")
    (less "@c: red;
.a { color: @c; }
")
    (pug "div.a
  p Hello
")
    (heex "<div class={@x}><%= @y %></div>
")
    (templ "package main

templ Hello(name string) {
	<p>{ name }</p>
}
")
    (git-commit "Fix the thing

A body.
# Please enter the commit message
")
    (markdown-rustdoc "# Examples

Adds one.
")
    (htmldjango "{% if user %}<p>{{ user.name }}</p>{% endif %}
")
    (edoc "@doc Adds one.
")
    (agda "module Main where

id : {A : Set} -> A -> A
id x = x
")
    (alloy "log_level = \"WARN\"
replicas = 5
")
    (amber "let x = 1
echo x
")
    (basic "10 PRINT \"HELLO\"
20 GOTO 10
")
    (bicep "param location string = 'westus'
")
    (blade "<p>{{ $name }}</p>
@if ($x)
<b>x</b>
@endif
")
    (blueprint "using Gtk 4.0;

Label {
  label: \"Hi\";
}
")
    (c3 "module hello;
fn void main()
{
    int x = 1;
}
")
    (cairo "fn main() -> felt252 {
    let x = 1;
    x
}
")
    (cel "request.auth.claims.group == 'admin'
")
    (chuck "SinOsc s => dac;
1::second => now;
")
    (circom "pragma circom 2.0.0;

template A() {
    signal input a;
    signal output b;
    b <== a;
}
")
    (clarity "(define-public (hello)
  (ok u1))
")
    (codeql "import javascript

from Function f
select f
")
    (concerto "namespace org.example@1.0.0

concept Person {
  o String name
}
")
    (cylc "[scheduling]
    [[graph]]
        R1 = \"a => b\"
")
    (dbml "Table users {
  id integer [primary key]
  name varchar
}
")
    (doxyfile "PROJECT_NAME = \"x\"
RECURSIVE = YES
")
    (dtd "<!ELEMENT note (to, body)>
<!ATTLIST note id CDATA #REQUIRED>
")
    (dunstrc "[global]
    font = Monospace 10
")
    (earthfile "VERSION 0.8
FROM alpine:3.18

build:
    RUN echo hi
")
    (ebnf "digit = \"0\" | \"1\" ;
")
    (eex "<p><%= @name %></p>
")
    (eiffel "class
    HELLO
feature
    make
        do
            print (\"Hi%N\")
        end
end
")
    (ejs "<p><%= name %></p>
")
    (erb "<p><%= @name %></p>
")
    (esdl "module default {
  type Person {
    required name: str;
  }
}
")
    (fga "model
  schema 1.1

type user
")
    (fidl "library example;

type Point = struct {
    x int32;
};
")
    (flatbuffers "table Monster {
  hp: short = 100;
}
root_type Monster;
")
    (freebasic "FUNCTION add(a AS INTEGER, b AS INTEGER) AS INTEGER
    RETURN a + b
END FUNCTION
")
    (gherkin "Feature: Adding
  Scenario: Two numbers
    Given the number 1
    And the number 2
    When they are added
    Then the sum is 3
")
    (ghostty "font = TX-02
")
    (glimmer "<div class=\"a\">{{this.name}}</div>
")
    (gjs "const x = 1;
<template><p>{{x}}</p></template>
")
    (gts "const x: number = 1;
<template><p>{{x}}</p></template>
")
    (gn "executable(\"hello\") {
  sources = [ \"hello.cc\" ]
}
")
    (godot-resource "[gd_scene format=3]

[node name=\"Root\" type=\"Node2D\"]
")
    (gotmpl "{{ if .x }}{{ .y }}{{ end }}
")
    (gowork "go 1.21

use ./x
")
    (gpr "project Hello is
   for Main use (\"hello.adb\");
end Hello;
")
    (gren "module Main exposing (main)

main =
    1
")
    (haskell-literate "Some prose.

> f :: Int -> Int
> f x = x + 1
")
    (hdl "CHIP And {
    IN a, b;
    OUT out;
    PARTS:
    Nand(a=a, b=b, out=x);
    Not(in=x, out=out);
}
")
    (hoon "|=  a=@
(add a 1)
")
    (hurl "GET https://example.org
HTTP 200
")
    (ink "Hello.
* [Go on] -> next
=== next ===
The end.
-> END
")
    (inko "type async Main {
  fn async main {
  }
}
")
    (jjdescription "Fix the thing

JJ: This commit contains the following changes:
")
    (kcl "a = 1
b = \"x\"
")
    (klog "2024-01-01
    8:00 - 9:00 Work
")
    (koka "fun main()
  println(\"hi\")
")
    (koto "x = 1
print x
")
    (mail "From: me@example.org
To: you@example.org
Subject: Hi

Hello.
")
    (markdoc "# Title

{% callout %}
Hi
{% /callout %}
")
    (mojo "def main():
    var x = 1
    print(x)
")
    (move "module 0x1::m {
    fun f(): u64 { 1 }
}
")
    (nearley "main -> \"a\" | \"b\"
")
    (ohm "G {
  Start = \"a\"
}
")
    (penrose "Set A
")
    (pest "digit = { '0'..'9' }
")
    (picat "main =>
    X = 1,
    println(X).
")
    (pod "=head1 NAME

Hello - say hi

=cut
")
    (ponylang "actor Main
  new create(env: Env) =>
    env.out.print(\"hi\")
")
    (proverif "free c: channel.
process 0
")
    (prql "from employees
filter salary > 100
")
    (ptx ".target sm_70
.address_size 64
")
    (qml "import QtQuick

Rectangle {
    width: 100
}
")
    (quarto "# Title

```{python}
x = 1
```
")
    (quint "module m {
  var x: int
}
")
    (rescript "let f = x => x + 1
")
    (rmarkdown "# Title

```{r}
x <- 1
```
")
    (robot "*** Test Cases ***
Example
    Log    Hello
")
    (sage "x = 1
print(x)
")
    (scfg "profile {
    output eDP-1 enable
}
")
    (shellcheckrc "disable=SC2034
")
    (slang "float4 main() : SV_Target
{
    return float4(1.0);
}
")
    (slint "export component App inherits Window {
    Text { text: \"hi\"; }
}
")
    (smithy "$version: \"2\"
namespace example

structure Person {
    name: String
}
")
    (snakemake "rule all:
    input: \"a.txt\"
")
    (sourcepawn "public void OnPluginStart()
{
    int x = 1;
}
")
    (spade "entity main(a: bool) -> bool {
    a
}
")
    (spicedb "definition user {}
")
    (strictdoc "[DOCUMENT]
TITLE: Requirements
")
    (supercollider "(
x = 1;
x.postln;
)
")
    (sway "contract;

fn f() -> u64 {
    1
}
")
    (systemverilog "module m(input logic a, output logic b);
  assign b = a;
endmodule
")
    (t32 "PRINT \"hi\"
ENDDO
")
    (tablegen "def X {
  int a = 1;
}
")
    (tact "contract A {
    x: Int = 1;
}
")
    (tera "{% if x %}{{ x }}{% endif %}
")
    (textproto "name: \"x\"
count: 1
")
    (tlaplus "---- MODULE M ----
VARIABLE x
Init == x = 0
====
")
    (tolk "fun main() {
    var x = 1;
}
")
    (typespec "model Person {
  name: string;
}
")
    (ungrammar "Name = 'ident'
")
    (unison "f x = x + 1
")
    (uxntal "|0100
#01 #02 ADD
BRK
")
    (vento "{{ if x }}{{ x }}{{ /if }}
")
    (vhs "Output demo.gif
Type \"echo hi\"
Enter
")
    (wast "(module (func (result i32) i32.const 1))
(assert_return (invoke \"x\") (i32.const 1))
")
    (werk "let x = \"a\"
")
    (wesl "fn main() -> f32 {
    return 1.0;
}
")
    (wit "record r1 {
  a: u32,
  b: u32,
}
")
    (wren "var x = 1
System.print(x)
")
    (xit "Open
[ ] a task
[x] done
")
    (yuck "(defwindow bar
  :geometry (geometry :width \"100%\")
  \"hi\")
")
    (adl "module a.b.c {};
")
    (bass "(def x 1)
[:a 1 :b 2]
")
    (cpon "<\"foo\":\"bar\", 1:<2:\"baz\">\"baz\">1
")
    (lpf "SYSCONFIG CONFIG_IOVOLTAGE=1.2;
")
    (ripple "component Button(props: { text: string }) {
  <button>{props.text}</button>
}
")
    (slisp "(def add (a b)
  (+ a b))
")
    (styx "key>value
")
    (task "# ID:                2
# Status:            Pending
")
    (tql "a = b()
")
    (yaml "# a comment
name: app
ports:
  - 80
  - \"443\"
nested: {a: 1, b: [x, y]}
")
    (docker-compose "services:
  web:
    image: \"nginx:1.27\"
    ports:
      - \"8080:80\"
")
    (github-action "name: CI
on: [push]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - run: make test
")
    (gitlab-ci "test:
  stage: test
  script:
    - make test
")
    (woodpecker-ci "steps:
  - name: test
    image: alpine
    commands:
      - make test
")
    (nestedtext "# a comment
name: app
ports:
    - 80
")
    (vue "<template>
  <p :class=\"c\">{{ msg }}</p>
</template>

<script>
export default { data() { return { msg: 'hi' } } }
</script>
")
    (lean "-- a comment
def double (n : Nat) : Nat := n + n

theorem t : double 2 = 4 := rfl
")
    (beancount "2024-01-01 open Assets:Cash USD

2024-01-02 * \"Shop\" \"Bread\"
  Assets:Cash  -3.00 USD
  Expenses:Food
")
    (astro "---
const name = \"world\";
---
<h1>Hello {name}</h1>
")
    (xtc "; A comment
P_INDICES [0] TEST
"))
  "A few lines of each language Tuatara knows: each must parse without errors and be
coloured.")

(define-test (languages samples-coloured)
  (loop for (name text) in *language-samples*
        do (let ((language (tuatara.defaults::find-language name)))
             (true language "~(~a~) is a language" name)
             (when language
               (let ((grammar (tuatara.defaults::language-grammar language)))
                 (multiple-value-bind (tree rope parser) (parse-string grammar text)
                   (declare (ignore rope))
                   (false (tuatara.syntax:tree-has-error-p tree) "~(~a~)'s sample parses without errors" name)
                   (release-parse tree parser))
                 (true (rich-ranges grammar text (tuatara.defaults::language-rules language))
                       "~(~a~)'s sample is coloured" name))))))

(define-test (languages files-find-their-modes)
  (loop for (file mode first-line) in '(("/src/app.py" python-mode) ("/src/app.js" js-mode) ("/src/app.tsx" tsx-mode)
                                        ("/src/app.ts" typescript-mode) ("/src/x.json" json-mode)
                                        ("/src/Cargo.toml" toml-mode) ("/src/README.md" markdown-mode) ("/src/index.html" html-mode)
                                        ("/src/main.go" go-mode) ("/src/go.mod" go-mod-mode) ("/src/A.java" java-mode)
                                        ("/src/a.cpp" c++-mode) ("/src/a.hpp" c++-mode) ("/src/a.h" c-mode)
                                        ;; the extension as written first, then in any case
                                        ("/src/a.c" c-mode) ("/src/a.C" c++-mode) ("/src/a.H" c++-mode)
                                        ("/src/APP.PY" python-mode) ("/src/Main.Rs" rust-mode)
                                        ("/src/Makefile" makefile-mode) ("/src/CMakeLists.txt" cmake-mode)
                                        ("/src/.gitignore" gitignore-mode) ("/repo/.git/config" gitconfig-mode)
                                        ("/src/init.el" emacs-lisp-mode) ("/src/x.el" emacs-lisp-mode)
                                        ("/src/fix.patch" diff-mode) ("/src/main.rs" rust-mode)
                                        ("/bin/tool" python-mode "#!/usr/bin/env python3")
                                        ("/etc/systemd/system/x.service" systemd-mode)
                                        ;; where several languages claim the same files, the one chosen
                                        ("/src/start.S" asm-mode) ("/src/start.s" asm-mode) ("/src/boot.asm" nasm-mode)
                                        ("/src/top.v" verilog-mode) ("/src/app.fs" fsharp-mode) ("/src/x.pl" perl-mode)
                                        ("/src/unit.pas" pascal-mode) ("/src/common.inc" fundamental-mode)
                                        ;; whole names, and names under a directory
                                        ("/src/linux/Kconfig" kconfig-mode) ("/etc/hosts" hosts-generic-mode)
                                        ("/mnt/evidence/etc/passwd" etc-passwd-generic-mode) ("/src/requirements.txt" pip-requirements-mode)
                                        ("/srv/Caddyfile" caddyfile-mode) ("/repo/.git/COMMIT_EDITMSG" git-commit-mode)
                                        ("/src/pkg.spec" rpm-spec-mode) ("/src/fr.po" po-mode) ("/src/rules.yar" yara-mode)
                                        ("/src/app.properties" conf-javaprop-mode) ("/src/php.ini" ini-mode)
                                        ("/app/views/home.blade.php" blade-mode) ("/app/index.php" php-mode)
                                        ("/rtl/core.sv" systemverilog-mode) ("/lib/Foo.pod" pod-mode) ("/mail/x.eml" eml-mode)
                                        ("/src/old.bas" basic-mode) ("/src/x.bi" freebasic-mode) ("/work/Snakefile" snakemake-mode)
                                        ("/src/go.work" go-work-mode) ("/home/me/.shellcheckrc" shellcheckrc-mode)
                                        ("/tmp/x.jjdescription" jj-description-mode) ("/src/x.lhs" literate-haskell-mode)
                                        ;; YAML, and the files in YAML that have modes of their own
                                        ("/src/x.yaml" yaml-mode) ("/src/x.yml" yaml-mode) ("/home/me/.kube/config" yaml-mode)
                                        ("/app/docker-compose.yml" docker-compose-mode) ("/app/compose.yaml" docker-compose-mode)
                                        ("/repo/.github/workflows/ci.yml" github-action-mode) ("/repo/.gitlab-ci.yml" gitlab-ci-mode)
                                        ("/repo/.woodpecker/build.yaml" woodpecker-ci-mode) ("/src/x.nt" nestedtext-mode)
                                        ("/src/App.vue" vue-mode) ("/src/Main.lean" lean4-mode) ("/books/main.beancount" beancount-mode)
                                        ("/src/index.astro" astro-mode))
        do (is eq (find-symbol (symbol-name mode) :tuatara.defaults)
               (if first-line
                   (tuatara.modes:mode-for-file (pathname file) first-line)
                   (tuatara.modes:mode-for-file (pathname file)))
               "~a" file)))

(define-test (languages file-types-as-written)
  ;; file types named by braces, extensions with a dot, a #! line's program with its
  ;; arguments: each finds its mode; OCaml's interface files are commented between (* *)
  (loop for (file mode first-line) in '(("/src/tsconfig.json" jsonc-mode) ("/src/jsconfig.json" jsonc-mode)
                                        ("/dist/app.js.map" json-mode) ("/dist/site.css.map" json-mode)
                                        ("/src/App.csproj.user" xml-mode) ("/src/App.vcxproj.filters" xml-mode)
                                        ("/src/build" v-mode "#!/usr/bin/env v run"))
        do (is eq (find-symbol (symbol-name mode) :tuatara.defaults)
               (if first-line
                   (tuatara.modes:mode-for-file (pathname file) first-line)
                   (tuatara.modes:mode-for-file (pathname file)))
               "~a" file))
  (is equal '("(*" "*)")
      (multiple-value-list (tuatara.modes:mode-block-comment-syntax (make-instance 'tuatara.defaults::ocaml-interface-mode))))
  (false (tuatara.modes:mode-comment-syntax (make-instance 'tuatara.defaults::ocaml-interface-mode))))

(define-test (languages visited-files-coloured)
  ;; a file of a language is parsed and coloured when visited, the modes of
  ;; text (Markdown, LaTeX) as well as those of code
  (loop for (extension text) in '(("md" "# Title

Some *text* and `code`.
")
                                  ("tex" "\\section{Intro}
Text.
")
                                  ("py" "def f(x):
    return x
")
                                  ("json" "{\"a\": 1}
"))
        do (let ((f (colour-session "" text extension)))
             (true (plusp (length (aref (tuatara.frontend.fake:fake-spans f) 0)))
                   "the first line of a .~a file is coloured" extension)))
  ;; Markdown's text inside its blocks is parsed as text in another language,
  ;; its marks (the * of emphasis) given to it too
  (let ((spans (aref (tuatara.frontend.fake:fake-spans (colour-session "" (format nil "# Title~%~%Some *text* and `code`.~%") "md")) 2)))
    (true (find :markup.italic spans :key #'third))
    (true (find :markup.raw spans :key #'third))))

(define-test (languages every-sample-shown)
  ;; each language's sample, visited as a file of its mode, shows through the
  ;; whole of redisplay (text in other languages, local names) in that mode,
  ;; as its text: no view of a package (a mail as a message) takes it
  (let ((tuatara.defaults::*file-views* '()))
    (loop for (name text) in *language-samples*
          for language = (tuatara.defaults::find-language name)
          for mode = (and language (first (tuatara.defaults::language-modes language)))
          for extension = (and mode (first (tuatara.modes:mode-file-rules mode)))
          when extension
            do (with-file-session ((f editor) "" text extension)
                 (is eq mode (type-of (tuatara.modes:buffer-major-mode
                                       (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
                     "a .~a file opens in ~(~a~)" extension mode)
                 (false (tuatara.frontend.fake:fake-frame-showing f "cannot be shown")
                        "~(~a~)'s sample shows" name)))))

(define-test (languages mode-language-by-table)
  ;; a mode's language is the first to name the nearest mode it is made from, found in a table
  ;; made once for the languages, not by going through them all at each call
  (dolist (name (tuatara.modes:mode-names))
    (is eq (loop for each in (tuatara.modes:mode-precedence name)
                 thereis (find-if (lambda (language) (member each (tuatara.defaults::language-modes language)))
                                  tuatara.defaults::*languages*))
        (tuatara.defaults::mode-language name)
        "~(~a~)" name))
  (let ((languages tuatara.defaults::*languages*))
    (flet ((seconds-a-call (list)
             (let ((tuatara.defaults::*languages* list))
               (tuatara.defaults::mode-language 'tuatara.defaults::fundamental-mode)
               (loop repeat 3
                     minimize (nth-value 1 (tuatara.bench::measuring
                                             (loop repeat 2000 do (tuatara.defaults::mode-language 'tuatara.defaults::fundamental-mode))))))))
      (let* ((few (seconds-a-call languages))
             (many (seconds-a-call (append languages languages languages languages)))
             (per-language (/ (- many few) 2000 (* 3 (length languages)))))
        (true (< per-language 1d-9) "~,2f ns a call for each language more" (* per-language 1d9))))))

(define-test (languages rules-found-without-a-source-tree)
  ;; a program that knows no source tree looks beside itself, with no error
  (let ((tuatara.runtime:*source-tree* nil))
    (dolist (kind '("rules" "indents"))
      (let ((found (tuatara.defaults::syntax-directory kind)))
        (true (or (null found) (pathnamep found)) "~a: ~s" kind found)))))
