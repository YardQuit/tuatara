/* tools/grammar-hostile.c --- Parse hostile text with one grammar, for tools/fuzz-grammars.sh --hostile.
   Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

   Usage: grammar-hostile LIBRARY NAME ROUNDS SEED [SAMPLE...]

   Loads the grammar NAME from the shared library LIBRARY (built with
   AddressSanitizer and UndefinedBehaviorSanitizer by the caller, as this
   program and tree-sitter are) and parses ROUNDS texts made to be hard on a
   scanner: random octets, text that is not UTF-8, brackets, quotes and
   indentation nested thousands deep, one line of a megabyte, NUL octets,
   and the SAMPLE files cut, repeated and spliced. Each text is parsed, then
   edited in its middle and parsed again from the first tree, so that the
   scanner's state is saved and taken back many times. A text that takes
   longer than its time is given up and said, as a parse in the editor is.
   The sanitizers end the program on the first fault they see, saying where;
   the exit status is then not 0. Nothing is written but to standard error. */

#include <dlfcn.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <tree_sitter/api.h>

static uint64_t state;

static uint32_t next_random(void) {
  state ^= state << 13;
  state ^= state >> 7;
  state ^= state << 17;
  return (uint32_t)(state >> 11);
}

typedef struct {
  char *text;
  size_t length, room;
} Text;

static void put(Text *t, const char *octets, size_t count) {
  if (t->length + count + 1 > t->room) {
    t->room = (t->length + count + 1) * 2;
    t->text = realloc(t->text, t->room);
    if (!t->text) { fprintf(stderr, "out of memory\n"); exit(2); }
  }
  memcpy(t->text + t->length, octets, count);
  t->length += count;
  t->text[t->length] = 0;
}

static void put_string(Text *t, const char *s) { put(t, s, strlen(s)); }

static Text *samples;
static int sample_count;

static Text read_file(const char *path) {
  Text t = {0};
  FILE *in = fopen(path, "rb");
  if (!in) { fprintf(stderr, "cannot read %s\n", path); exit(2); }
  char buffer[65536];
  size_t count;
  while ((count = fread(buffer, 1, sizeof buffer, in)) > 0) put(&t, buffer, count);
  fclose(in);
  if (!t.text) put(&t, "", 0);
  return t;
}

static const char *openers[] = {"(", "[", "{", "<", "\"", "'", "`", "/*", "<!--", "{{", "{%", "#(", "$(", "${", "\"\"\"", "f\"", "r#\"", "\\begin{x}", "<<EOF\n"};

/* one hostile text, of the kind KIND, into T */
static void make_text(Text *t, int kind) {
  t->length = 0;
  put(t, "", 0);
  switch (kind) {
  case 0: { /* random octets */
    size_t n = next_random() % 65536;
    for (size_t i = 0; i < n; i++) { char c = (char)next_random(); put(t, &c, 1); }
    break;
  }
  case 1: { /* not UTF-8: lone continuations, overlong and cut sequences, surrogates */
    static const char *bad[] = {"\x80", "\xbf", "\xc0\xaf", "\xe0\x80\xaf", "\xed\xa0\x80", "\xf4\x90\x80\x80", "\xf8\x88\x80\x80\x80", "\xff", "\xfe", "\xe2\x82", "\xc3"};
    size_t n = 1000 + next_random() % 20000;
    for (size_t i = 0; i < n; i++) {
      if (next_random() % 3) put_string(t, bad[next_random() % (sizeof bad / sizeof *bad)]);
      else { char c = (char)(32 + next_random() % 95); put(t, &c, 1); }
    }
    break;
  }
  case 2: { /* one opener nested deep, sometimes closed */
    const char *open = openers[next_random() % (sizeof openers / sizeof *openers)];
    size_t depth = 1000 + next_random() % 20000;
    for (size_t i = 0; i < depth; i++) put_string(t, open);
    put_string(t, "x");
    if (next_random() % 2) {
      const char *close = strcmp(open, "(") == 0 ? ")" : strcmp(open, "[") == 0 ? "]" : strcmp(open, "{") == 0 ? "}" : open;
      for (size_t i = 0; i < depth; i++) put_string(t, close);
    }
    break;
  }
  case 3: { /* indentation thousands deep, a string left open in it */
    size_t depth = 200 + next_random() % 2000;
    bool tabs = next_random() % 2;
    const char *line = (next_random() % 2) ? "a:\n" : "if x:\n";
    for (size_t i = 0; i < depth; i++) {
      for (size_t j = 0; j < i; j++) put_string(t, (tabs && next_random() % 8 == 0) ? "\t" : " ");
      put_string(t, line);
    }
    for (size_t j = 0; j < depth; j++) put_string(t, " ");
    put_string(t, "x = f\"\"\"a\nb\n");
    break;
  }
  case 4: { /* one line of a megabyte */
    char c = (char)(33 + next_random() % 94);
    for (size_t i = 0; i < 1024 * 1024; i++) put(t, &c, 1);
    break;
  }
  default: { /* the samples cut, repeated, spliced, NULs and line ends put in */
    if (sample_count == 0) { make_text(t, (int)(next_random() % 5)); return; }
    size_t pieces = 1 + next_random() % 200;
    for (size_t i = 0; i < pieces; i++) {
      Text *s = &samples[next_random() % sample_count];
      if (s->length == 0) continue;
      size_t start = next_random() % s->length, length = next_random() % (s->length - start + 1);
      put(t, s->text + start, length);
      switch (next_random() % 6) {
      case 0: put(t, "\0", 1); break;
      case 1: put_string(t, "\r"); break;
      case 2: put_string(t, "\r\n"); break;
      case 3: put_string(t, openers[next_random() % (sizeof openers / sizeof *openers)]); break;
      default: break;
      }
    }
    break;
  }
  }
}

static struct timespec deadline;

static bool past_deadline(TSParseState *parse_state) {
  (void)parse_state;
  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC, &now);
  return now.tv_sec > deadline.tv_sec || (now.tv_sec == deadline.tv_sec && now.tv_nsec > deadline.tv_nsec);
}

static const char *read_text(void *payload, uint32_t byte, TSPoint point, uint32_t *read) {
  (void)point;
  Text *t = payload;
  if (byte >= t->length) { *read = 0; return ""; }
  uint32_t left = (uint32_t)(t->length - byte);
  *read = left < 4096 ? left : 4096;
  return t->text + byte;
}

int main(int argc, char **argv) {
  if (argc < 5) {
    fprintf(stderr, "Usage: grammar-hostile LIBRARY NAME ROUNDS SEED [SAMPLE...]\n");
    return 2;
  }
  void *library = dlopen(argv[1], RTLD_NOW | RTLD_LOCAL);
  if (!library) { fprintf(stderr, "%s\n", dlerror()); return 2; }
  char symbol[256];
  snprintf(symbol, sizeof symbol, "tree_sitter_%s", argv[2]);
  const TSLanguage *(*language)(void) = (const TSLanguage *(*)(void))dlsym(library, symbol);
  if (!language) { fprintf(stderr, "no %s in %s\n", symbol, argv[1]); return 2; }
  long rounds = strtol(argv[3], NULL, 10);
  state = (uint64_t)strtoull(argv[4], NULL, 10) * 2654435761u + 1;
  sample_count = argc - 5;
  samples = calloc((size_t)(sample_count + 1), sizeof *samples);
  for (int i = 0; i < sample_count; i++) samples[i] = read_file(argv[5 + i]);

  TSParser *parser = ts_parser_new();
  if (!ts_parser_set_language(parser, language())) { fprintf(stderr, "%s: wrong ABI\n", argv[2]); return 2; }
  Text t = {0};
  long given_up = 0;
  for (long round = 0; round < rounds; round++) {
    int kind = (int)(next_random() % 8);
    make_text(&t, kind);
    TSParseOptions options = {.payload = NULL, .progress_callback = past_deadline};
    TSInput input = {.payload = &t, .read = read_text, .encoding = TSInputEncodingUTF8, .decode = NULL};
    clock_gettime(CLOCK_MONOTONIC, &deadline);
    deadline.tv_sec += 10;
    TSTree *tree = ts_parser_parse_with_options(parser, NULL, input, options);
    if (!tree) { given_up++; ts_parser_reset(parser); continue; }
    /* an edit in the middle: some octets taken out, others put in, parsed again from the tree */
    uint32_t at = t.length ? next_random() % (uint32_t)t.length : 0;
    uint32_t cut = t.length - at ? next_random() % (uint32_t)(t.length - at + 1) % 64 : 0;
    const char *in = openers[next_random() % (sizeof openers / sizeof *openers)];
    uint32_t added = (uint32_t)strlen(in);
    Text edited = {0};
    put(&edited, t.text, at);
    put(&edited, in, added);
    put(&edited, t.text + at + cut, t.length - at - cut);
    TSInputEdit edit = {.start_byte = at, .old_end_byte = at + cut, .new_end_byte = at + added,
                        .start_point = {0, at}, .old_end_point = {0, at + cut}, .new_end_point = {0, at + added}};
    ts_tree_edit(tree, &edit);
    input.payload = &edited;
    clock_gettime(CLOCK_MONOTONIC, &deadline);
    deadline.tv_sec += 10;
    TSTree *again = ts_parser_parse_with_options(parser, tree, input, options);
    if (!again) { given_up++; ts_parser_reset(parser); }
    else {
      /* every node visited, as colouring and moving by structure reach them */
      TSTreeCursor cursor = ts_tree_cursor_new(ts_tree_root_node(again));
      for (bool more = true; more;) {
        if (ts_tree_cursor_goto_first_child(&cursor)) continue;
        while (!ts_tree_cursor_goto_next_sibling(&cursor))
          if (!ts_tree_cursor_goto_parent(&cursor)) { more = false; break; }
      }
      ts_tree_cursor_delete(&cursor);
      ts_tree_delete(again);
    }
    ts_tree_delete(tree);
    free(edited.text);
  }
  ts_parser_delete(parser);
  free(t.text);
  printf("%s: %ld texts parsed, %ld given up after 10 seconds\n", argv[2], rounds, given_up);
  return 0;
}
