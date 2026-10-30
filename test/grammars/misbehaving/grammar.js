// test/grammars/misbehaving/grammar.js --- A grammar whose scanner goes wrong on purpose.
// Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
//
// Words, the keywords "begin" and "bad", and marks: a "!" and a letter
// saying what the scanner does (src/scanner.c). Its src/parser.c is the one
// tree-sitter generates from this, changed by hand where it says
// "misbehaving" so that its lexing goes wrong too. The tests compile it as
// the build compiles any grammar.
module.exports = grammar({
  name: 'misbehaving',
  externals: $ => [$.mark],
  extras: $ => [/\s/],
  word: $ => $.word,
  rules: {
    source_file: $ => repeat(choice($.word, $.mark, $.keyword)),
    keyword: _ => choice('begin', 'bad'),
    word: _ => /[a-z]+/,
  },
});
