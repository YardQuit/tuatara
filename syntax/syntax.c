/* syntax/syntax.c --- The calls into tree-sitter that pass structures by value.
   Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

   Built by tools/grammars.sh together with tree-sitter itself into
   libtuatara-syntax.so. Lisp's foreign calls do not pass or return C
   structures by value; these functions take their parts one by one. */

#include <malloc.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <tree_sitter/api.h>

/* The octets the tree-sitter library holds: every allocation it makes, and a
   grammar's through it, is counted here, so that a parse that takes far more
   than its text calls for (a grammar gone wrong, building without end) can be
   stopped before it takes the machine's memory. */
static _Atomic int64_t tuatara_held_octets;

static void tuatara_count(void *pointer, int64_t sign) {
  if (pointer)
    atomic_fetch_add_explicit(&tuatara_held_octets, sign * (int64_t)malloc_usable_size(pointer), memory_order_relaxed);
}

static void *tuatara_malloc(size_t size) {
  void *pointer = malloc(size);
  tuatara_count(pointer, 1);
  return pointer;
}

static void *tuatara_calloc(size_t count, size_t size) {
  void *pointer = calloc(count, size);
  tuatara_count(pointer, 1);
  return pointer;
}

static void *tuatara_realloc(void *old, size_t size) {
  int64_t before = old ? (int64_t)malloc_usable_size(old) : 0;
  void *pointer = realloc(old, size);
  if (pointer)
    atomic_fetch_add_explicit(&tuatara_held_octets, (int64_t)malloc_usable_size(pointer) - before, memory_order_relaxed);
  else if (size == 0 && old)
    atomic_fetch_sub_explicit(&tuatara_held_octets, before, memory_order_relaxed);
  return pointer;
}

static void tuatara_release(void *pointer) {
  tuatara_count(pointer, -1);
  free(pointer);
}

__attribute__((constructor)) static void tuatara_count_allocations(void) {
  ts_set_allocator(tuatara_malloc, tuatara_calloc, tuatara_realloc, tuatara_release);
}

/* The octets the tree-sitter library holds now. */
int64_t tuatara_held(void) {
  return atomic_load_explicit(&tuatara_held_octets, memory_order_relaxed);
}

/* The octets of memory the machine has, or 0 when the system does not say. */
int64_t tuatara_physical_memory(void) {
  long pages = sysconf(_SC_PHYS_PAGES), size = sysconf(_SC_PAGESIZE);
  return pages > 0 && size > 0 ? (int64_t)pages * size : 0;
}

/* Reads the text: returns the octets from BYTE on and stores how many there
   are in *LENGTH, zero at the end of the text. */
typedef const char *(*tuatara_reader)(void *payload, uint32_t byte, uint32_t *length);

struct tuatara_input {
  tuatara_reader read;
  void *payload;
};

static const char *tuatara_read(void *payload, uint32_t byte, TSPoint point, uint32_t *length) {
  struct tuatara_input *input = payload;
  (void)point;
  return input->read(input->payload, byte, length);
}

/* When a parse is to stop: when *CANCEL becomes nonzero; once DEADLINE has
   passed if LIMITED, which sets *CANCEL to 2; or once the library holds more
   than BUDGET octets beyond the HELD it held when the parse began, unless
   BUDGET is 0, which sets *CANCEL to 3. */
struct tuatara_progress {
  volatile int32_t *cancel;
  bool limited;
  struct timespec deadline;
  int64_t held;
  int64_t budget;
};

static bool tuatara_cancelled(TSParseState *state) {
  struct tuatara_progress *progress = state->payload;
  if (*progress->cancel != 0) return true;
  if (progress->budget > 0 && tuatara_held() - progress->held > progress->budget) {
    *progress->cancel = 3;
    return true;
  }
  if (progress->limited) {
    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);
    if (now.tv_sec > progress->deadline.tv_sec ||
        (now.tv_sec == progress->deadline.tv_sec && now.tv_nsec >= progress->deadline.tv_nsec)) {
      *progress->cancel = 2;
      return true;
    }
  }
  return false;
}

/* Parse the UTF-8 text READ gives with PARSER, reusing OLD (already edited to
   match the text) unless it is NULL. The parse stops, returning NULL, soon
   after *CANCEL becomes nonzero; once LIMIT milliseconds have passed unless
   LIMIT is 0, *CANCEL then set to 2; or once it has taken BUDGET octets of
   memory unless BUDGET is 0, *CANCEL then set to 3. It returns NULL as well
   when the grammar's own code gave the parser what it cannot have (a token
   number the grammar does not have, a state longer than its buffer), *CANCEL
   then set to 4. The parser must be reset after a NULL. */
TSTree *tuatara_parse(TSParser *parser, const TSTree *old, tuatara_reader read, void *payload,
                      volatile int32_t *cancel, uint32_t limit, uint64_t budget) {
  struct tuatara_input source = {read, payload};
  TSInput input = {&source, tuatara_read, TSInputEncodingUTF8, NULL};
  struct tuatara_progress progress = {cancel, limit > 0, {0, 0}, tuatara_held(),
                                      budget > INT64_MAX ? INT64_MAX : (int64_t)budget};
  if (limit > 0) {
    clock_gettime(CLOCK_MONOTONIC, &progress.deadline);
    progress.deadline.tv_sec += limit / 1000;
    progress.deadline.tv_nsec += (long)(limit % 1000) * 1000000L;
    if (progress.deadline.tv_nsec >= 1000000000L) {
      progress.deadline.tv_sec++;
      progress.deadline.tv_nsec -= 1000000000L;
    }
  }
  TSParseOptions options = {&progress, tuatara_cancelled};
  TSTree *tree = ts_parser_parse_with_options(parser, old, input, options);
  /* stopped by none of the above: the grammar went wrong */
  if (!tree && *cancel == 0) *cancel = 4;
  return tree;
}

/* Tell TREE that the octets from START below OLD_END became those below
   NEW_END, giving the row and column (in octets) of each of the three. */
void tuatara_edit(TSTree *tree, uint32_t start, uint32_t old_end, uint32_t new_end,
                  uint32_t start_row, uint32_t start_column,
                  uint32_t old_end_row, uint32_t old_end_column,
                  uint32_t new_end_row, uint32_t new_end_column) {
  TSInputEdit edit = {start, old_end, new_end,
                      {start_row, start_column},
                      {old_end_row, old_end_column},
                      {new_end_row, new_end_column}};
  ts_tree_edit(tree, &edit);
}

/* Whether TREE's text has syntax errors. */
bool tuatara_tree_has_error(const TSTree *tree) {
  return ts_node_has_error(ts_tree_root_node(tree));
}

/* Set CURSOR running QUERY over the nodes of TREE between octets START and END. */
void tuatara_query(TSQueryCursor *cursor, const TSQuery *query, const TSTree *tree,
                   uint32_t start, uint32_t end) {
  ts_query_cursor_set_byte_range(cursor, start, end);
  ts_query_cursor_exec(cursor, query, ts_tree_root_node(tree));
}

/* The next match of CURSOR: stores its pattern's index in *PATTERN and up to
   MAX of its captures in CAPTURES, four numbers each: the capture's index, the
   octets its node starts and ends at, and the node's grammar symbol. Returns
   how many captures the match has (it may be more than MAX), or -1 when there
   are no more matches. */
int32_t tuatara_next_match(TSQueryCursor *cursor, uint32_t *pattern, uint32_t *captures, uint32_t max) {
  TSQueryMatch match;
  if (!ts_query_cursor_next_match(cursor, &match))
    return -1;
  *pattern = match.pattern_index;
  uint32_t n = match.capture_count < max ? match.capture_count : max;
  for (uint32_t i = 0; i < n; i++) {
    TSNode node = match.captures[i].node;
    captures[4 * i] = match.captures[i].index;
    captures[4 * i + 1] = ts_node_start_byte(node);
    captures[4 * i + 2] = ts_node_end_byte(node);
    captures[4 * i + 3] = ts_node_symbol(node);
  }
  return (int32_t)match.capture_count;
}

/* The syntax tree of TREE's smallest node spanning octets START below END, as
   an S-expression in text the caller releases with tuatara_free. */
char *tuatara_node_string(const TSTree *tree, uint32_t start, uint32_t end) {
  return ts_node_string(ts_node_descendant_for_byte_range(ts_tree_root_node(tree), start, end));
}

/* Release memory the tree-sitter library allocated for the caller. */
void tuatara_free(void *pointer) {
  tuatara_release(pointer);
}

/* The smallest node of TREE spanning octets START below END, named or not,
   then each node around it out to the root: five numbers each in OUT (the
   octets the node starts and ends at, its grammar symbol, 1 when it is named
   plus 2 when it is a syntax error or holds one, and how many named children
   it has), at most MAX nodes. Returns how many were stored. */
int32_t tuatara_ancestry(const TSTree *tree, uint32_t start, uint32_t end, uint32_t *out, uint32_t max) {
  enum { deepest = 1024 };
  TSNode path[deepest];
  TSNode root = ts_tree_root_node(tree);
  TSNode leaf = ts_node_descendant_for_byte_range(root, start, end);
  uint32_t depth = 0;
  TSNode node = root;
  while (!ts_node_is_null(node) && depth < deepest) {
    path[depth++] = node;
    if (ts_node_eq(node, leaf))
      break;
    node = ts_node_child_with_descendant(node, leaf);
  }
  uint32_t n = 0;
  while (depth > 0 && n < max) {
    TSNode here = path[--depth];
    out[5 * n] = ts_node_start_byte(here);
    out[5 * n + 1] = ts_node_end_byte(here);
    out[5 * n + 2] = ts_node_symbol(here);
    out[5 * n + 3] = (uint32_t)ts_node_is_named(here) | ((uint32_t)ts_node_has_error(here) << 1);
    out[5 * n + 4] = ts_node_named_child_count(here);
    n++;
  }
  return (int32_t)n;
}

/* A named node of TREE for a selection of octets START below END, by HOW:
   0, the smallest named node spanning them; 1, the smallest spanning more
   than them; 2, the first named child of the smallest; 3 or 4, the next or
   previous named sibling of the smallest, or when it has none, that of the
   nearest node around it that has one. The octets the node starts and ends
   at go in OUT. Returns 1, or 0 when there is no such node. */
int32_t tuatara_select_node(const TSTree *tree, uint32_t start, uint32_t end, int32_t how, uint32_t *out) {
  TSNode node = ts_node_named_descendant_for_byte_range(ts_tree_root_node(tree), start, end);
  if (ts_node_is_null(node))
    return 0;
  if (how == 1) {
    while (ts_node_start_byte(node) == start && ts_node_end_byte(node) == end) {
      TSNode parent = ts_node_parent(node);
      if (ts_node_is_null(parent))
        return 0;
      node = parent;
    }
  } else if (how == 2) {
    node = ts_node_named_child(node, 0);
    if (ts_node_is_null(node))
      return 0;
  } else if (how == 3 || how == 4) {
    for (;;) {
      TSNode sibling = how == 3 ? ts_node_next_named_sibling(node) : ts_node_prev_named_sibling(node);
      if (!ts_node_is_null(sibling)) {
        node = sibling;
        break;
      }
      node = ts_node_parent(node);
      if (ts_node_is_null(node))
        return 0;
    }
  }
  out[0] = ts_node_start_byte(node);
  out[1] = ts_node_end_byte(node);
  return 1;
}

/* Make PARSER read only the COUNT ranges of the text in RANGES, six numbers
   each: the octets a range starts and ends at, then the row and column (in
   octets) where it starts and where it ends. Returns 1, or 0 when tree-sitter
   refuses them (they must be in order and apart). */
int32_t tuatara_set_included_ranges(TSParser *parser, const uint32_t *ranges, uint32_t count) {
  TSRange *r = malloc(sizeof(TSRange) * (count ? count : 1));
  if (!r)
    return 0;
  for (uint32_t i = 0; i < count; i++) {
    r[i].start_byte = ranges[6 * i];
    r[i].end_byte = ranges[6 * i + 1];
    r[i].start_point = (TSPoint){ranges[6 * i + 2], ranges[6 * i + 3]};
    r[i].end_point = (TSPoint){ranges[6 * i + 4], ranges[6 * i + 5]};
  }
  bool ok = ts_parser_set_included_ranges(parser, r, count);
  free(r);
  return ok ? 1 : 0;
}

/* The children of the node of TREE that starts at octet START, ends at END
   and is of SYMBOL that end after FROM and start before TO, its named ones
   alone when NAMED, else named or not: the octets each starts and ends at,
   two numbers each in OUT, at most MAX of them. Returns how many, or -1 when
   there is no such node. A cursor goes to the first of them at once, so a
   node of very many children (a code block's lines) costs no more than the
   children in the range. */
int32_t tuatara_node_child_ranges(const TSTree *tree, uint32_t start, uint32_t end, uint32_t symbol,
                                  bool named, uint32_t from, uint32_t to, uint32_t *out, uint32_t max) {
  TSNode node = ts_node_descendant_for_byte_range(ts_tree_root_node(tree), start, end);
  while (!ts_node_is_null(node) &&
         !(ts_node_start_byte(node) == start && ts_node_end_byte(node) == end && ts_node_symbol(node) == symbol)) {
    if (ts_node_start_byte(node) < start || ts_node_end_byte(node) > end)
      return -1;
    node = ts_node_parent(node);
  }
  if (ts_node_is_null(node))
    return -1;
  uint32_t k = 0;
  TSTreeCursor cursor = ts_tree_cursor_new(node);
  if (ts_tree_cursor_goto_first_child_for_byte(&cursor, from) >= 0) {
    do {
      TSNode child = ts_tree_cursor_current_node(&cursor);
      uint32_t child_start = ts_node_start_byte(child);
      if (child_start >= to || k >= max)
        break;
      if (named && !ts_node_is_named(child))
        continue;
      out[2 * k] = child_start;
      out[2 * k + 1] = ts_node_end_byte(child);
      k++;
    } while (ts_tree_cursor_goto_next_sibling(&cursor));
  }
  ts_tree_cursor_delete(&cursor);
  return (int32_t)k;
}
