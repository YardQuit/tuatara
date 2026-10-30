/* test/grammars/misbehaving/src/scanner.c --- A scanner that goes wrong on purpose.
   Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

   A mark is "!" and a letter: "!s" gives the parser a token number the
   grammar does not have; "!m" takes a megabyte of memory, kept until the
   parser is deleted; after "!z" the scanner says its state is longer than
   the buffer for it, and after "!o" it writes 64 octets past that buffer,
   after "!O" 64 KB; "!h" asks tree-sitter's own allocator for a terabyte;
   any other letter is a plain mark. */

#include "tree_sitter/parser.h"
#include <stdlib.h>
#include <string.h>

/* tree-sitter's allocator, where the program has it */
extern void *(*ts_current_malloc)(size_t) __attribute__((weak));

/* The memory taken by "!m" marks, each block holding the one before, and
   what the next saving of the state does wrong: 'z', 'o' or 'O', or 0. */
typedef struct {
  void *taken;
  int wrong;
} Scanner;

void *tree_sitter_misbehaving_external_scanner_create(void) {
  return calloc(1, sizeof(Scanner));
}

void tree_sitter_misbehaving_external_scanner_destroy(void *payload) {
  Scanner *scanner = payload;
  while (scanner->taken) {
    void *next = *(void **)scanner->taken;
    free(scanner->taken);
    scanner->taken = next;
  }
  free(scanner);
}

unsigned tree_sitter_misbehaving_external_scanner_serialize(void *payload, char *buffer) {
  Scanner *scanner = payload;
  int wrong = scanner->wrong;
  scanner->wrong = 0;
  if (wrong == 'z') return TREE_SITTER_SERIALIZATION_BUFFER_SIZE + 1;
  if (wrong == 'o' || wrong == 'O') {
    unsigned length = TREE_SITTER_SERIALIZATION_BUFFER_SIZE + (wrong == 'o' ? 64 : 65536);
    memset(buffer, wrong, length);
    return length;
  }
  return 0;
}

void tree_sitter_misbehaving_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
  (void)payload;
  (void)buffer;
  (void)length;
}

bool tree_sitter_misbehaving_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
  Scanner *scanner = payload;
  if (!valid_symbols[0]) return false;
  while (lexer->lookahead == ' ' || lexer->lookahead == '\n') lexer->advance(lexer, true);
  if (lexer->lookahead != '!') return false;
  lexer->advance(lexer, false);
  int what = lexer->lookahead;
  lexer->advance(lexer, false);
  lexer->result_symbol = 0;
  if (what == 'z' || what == 'o' || what == 'O') {
    scanner->wrong = what;
  } else if (what == 'h') {
    if (&ts_current_malloc) ts_current_malloc((size_t)1 << 40);
  } else if (what == 's') {
    lexer->result_symbol = 60000;
  } else if (what == 'm') {
    void **block = malloc(1 << 20);
    if (block) {
      *block = scanner->taken;
      scanner->taken = block;
    }
  }
  return true;
}
