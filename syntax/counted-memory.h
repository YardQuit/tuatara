/* syntax/counted-memory.h --- A grammar's own memory, taken as tree-sitter takes its own.
   Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

   Included before each grammar's own code by tools/grammar-build.sh. A
   scanner that takes memory with malloc and its kin takes it through the
   editor's counted functions (syntax/syntax.c), so that a parse's memory
   budget sees the scanner's memory too, and a scanner that builds without end
   is stopped with the parse. They answer as the C library's do, NULL when
   there is no memory, which a scanner looks for; tree-sitter's own memory
   kept back for running out is not theirs to take. In a program without
   those functions (the tree-sitter program that fuzzes grammars) the C
   library's are used. */

#ifndef TUATARA_COUNTED_MEMORY_H
#define TUATARA_COUNTED_MEMORY_H

#include <stdlib.h>

void *tuatara_counted_malloc(size_t) __attribute__((weak));
void *tuatara_counted_calloc(size_t, size_t) __attribute__((weak));
void *tuatara_counted_realloc(void *, size_t) __attribute__((weak));
void tuatara_counted_free(void *) __attribute__((weak));

static inline void *tuatara_grammar_malloc(size_t size) {
  return &tuatara_counted_malloc ? tuatara_counted_malloc(size) : malloc(size);
}

static inline void *tuatara_grammar_calloc(size_t count, size_t size) {
  return &tuatara_counted_calloc ? tuatara_counted_calloc(count, size) : calloc(count, size);
}

static inline void *tuatara_grammar_realloc(void *pointer, size_t size) {
  return &tuatara_counted_realloc ? tuatara_counted_realloc(pointer, size) : realloc(pointer, size);
}

static inline void tuatara_grammar_free(void *pointer) {
  if (&tuatara_counted_free) tuatara_counted_free(pointer); else free(pointer);
}

#define malloc(size) tuatara_grammar_malloc(size)
#define calloc(count, size) tuatara_grammar_calloc(count, size)
#define realloc(pointer, size) tuatara_grammar_realloc(pointer, size)
#define free(pointer) tuatara_grammar_free(pointer)

#endif
