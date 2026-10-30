/* syntax/syntax.c --- The calls into tree-sitter that pass structures by value.
   Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

   Built by tools/grammars.sh together with tree-sitter itself into
   libtuatara-syntax.so. Lisp's foreign calls do not pass or return C
   structures by value; these functions take their parts one by one. */

#include <malloc.h>
#include <pthread.h>
#include <setjmp.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/resource.h>
#include <time.h>
#include <unistd.h>
#include <tree_sitter/api.h>

/* The octets the tree-sitter library holds: every allocation it makes, and
   each a grammar's code makes (syntax/counted-memory.h), is counted here, so
   that a parse that takes far more than its text calls for (a grammar gone
   wrong, building without end) can be stopped before it takes the machine's
   memory. */
static _Atomic int64_t tuatara_held_octets;

static void tuatara_count(void *pointer, int64_t sign) {
  if (pointer)
    atomic_fetch_add_explicit(&tuatara_held_octets, sign * (int64_t)malloc_usable_size(pointer), memory_order_relaxed);
}

/* What a grammar's own code takes, through syntax/counted-memory.h: counted,
   and NULL when there is no memory, as the C library's functions are. */
void *tuatara_counted_malloc(size_t size) {
  void *pointer = malloc(size);
  tuatara_count(pointer, 1);
  return pointer;
}

void *tuatara_counted_calloc(size_t count, size_t size) {
  void *pointer = calloc(count, size);
  tuatara_count(pointer, 1);
  return pointer;
}

void *tuatara_counted_realloc(void *old, size_t size) {
  int64_t before = old ? (int64_t)malloc_usable_size(old) : 0;
  void *pointer = realloc(old, size);
  if (pointer)
    atomic_fetch_add_explicit(&tuatara_held_octets, (int64_t)malloc_usable_size(pointer) - before, memory_order_relaxed);
  else if (size == 0 && old)
    atomic_fetch_sub_explicit(&tuatara_held_octets, before, memory_order_relaxed);
  return pointer;
}

void tuatara_counted_free(void *pointer) {
  tuatara_count(pointer, -1);
  free(pointer);
}

/* Tree-sitter's own allocations cannot fail: none of the places it asks for
   memory looks for NULL. So memory is kept back for the moment it runs out:
   TUATARA_RESERVE octets, taken and never written, given back to the system
   when an allocation fails, which is then made again. TUATARA_SHORT is set
   while the reserve is given back, and every parse under way stops at its
   next check (tuatara_cancelled) and none starts; the reserve is taken
   again when it can be had, at the start and the end of a parse
   (tuatara_take_reserve) or when the editor asks (tuatara_memory_back).
   The two change together under TUATARA_RESERVE_LOCK, so the flag is set
   exactly while the reserve is given back: a thread taking it again cannot
   clear the flag another has just set on giving it back.

   An allocation that fails again, or one larger than the reserve (which
   giving the reserve back could not make room for), cannot be answered. In
   a parse the allocation does not return: the parse is given up there
   (TUATARA_ESCAPE) and the parser is not used again, as its state is left
   half made. A parse is where the sizes asked for follow the text and its
   tree (the parse stack, a node's children, the tree's nodes); elsewhere
   (queries, edits of a tree, deleting one, a parser's own structure) they
   are small and bounded, far below the reserve, and an allocation failing
   there, even with the reserve given back or larger than it, ends the
   editor, saying why, as tree-sitter's own allocator does: going on would
   write through no memory, at an offset that may reach the editor's own,
   and damage it unseen. While the text a parse reads is fetched (from the
   editor's Lisp, which an escape must not cross), no parse is under way. */
#define TUATARA_RESERVE ((size_t)64 << 20)
static pthread_mutex_t tuatara_reserve_lock = PTHREAD_MUTEX_INITIALIZER;
static void *tuatara_reserve;
static atomic_bool tuatara_short;
static _Thread_local jmp_buf *tuatara_escape;

/* Give the reserve back; false when it was given back already. */
static bool tuatara_give_reserve(void) {
  pthread_mutex_lock(&tuatara_reserve_lock);
  void *reserve = tuatara_reserve;
  tuatara_reserve = NULL;
  atomic_store(&tuatara_short, true);
  pthread_mutex_unlock(&tuatara_reserve_lock);
  free(reserve);
  return reserve != NULL;
}

/* Take the reserve again when it was given back and the memory can be had. */
static void tuatara_take_reserve(void) {
  if (!atomic_load(&tuatara_short)) return;
  void *reserve = malloc(TUATARA_RESERVE);
  if (!reserve) return;
  pthread_mutex_lock(&tuatara_reserve_lock);
  if (!tuatara_reserve) {
    tuatara_reserve = reserve;
    reserve = NULL;
    atomic_store(&tuatara_short, false);
  }
  pthread_mutex_unlock(&tuatara_reserve_lock);
  free(reserve);
}

/* An allocation of SIZE octets for tree-sitter failed: true to make it again,
   the reserve given back; else it does not return (see above). */
static bool tuatara_again(size_t size) {
  if (size <= TUATARA_RESERVE && tuatara_give_reserve()) return true;
  if (tuatara_escape) longjmp(*tuatara_escape, 1);
  static const char said[] = "tuatara: out of memory in the syntax library\n";
  if (write(2, said, sizeof said - 1) < 0) {}
  abort();
}

static void *tuatara_malloc(size_t size) {
  void *pointer = malloc(size);
  while (!pointer && size && tuatara_again(size)) pointer = malloc(size);
  tuatara_count(pointer, 1);
  return pointer;
}

static void *tuatara_calloc(size_t count, size_t size) {
  void *pointer = calloc(count, size);
  size_t total = size && count > SIZE_MAX / size ? SIZE_MAX : count * size;
  while (!pointer && total && tuatara_again(total)) pointer = calloc(count, size);
  tuatara_count(pointer, 1);
  return pointer;
}

static void *tuatara_realloc(void *old, size_t size) {
  int64_t before = old ? (int64_t)malloc_usable_size(old) : 0;
  void *pointer = realloc(old, size);
  while (!pointer && size && tuatara_again(size)) pointer = realloc(old, size);
  if (pointer)
    atomic_fetch_add_explicit(&tuatara_held_octets, (int64_t)malloc_usable_size(pointer) - before, memory_order_relaxed);
  else if (size == 0 && old)
    atomic_fetch_sub_explicit(&tuatara_held_octets, before, memory_order_relaxed);
  return pointer;
}

__attribute__((constructor)) static void tuatara_count_allocations(void) {
  tuatara_reserve = malloc(TUATARA_RESERVE);
  atomic_store(&tuatara_short, tuatara_reserve == NULL);
  ts_set_allocator(tuatara_malloc, tuatara_calloc, tuatara_realloc, tuatara_counted_free);
}

/* True when the memory kept back for running out is kept again, taking it
   when it can be had: memory is no longer short. */
bool tuatara_memory_back(void) {
  tuatara_take_reserve();
  return !atomic_load(&tuatara_short);
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

/* The octets of address space the editor may still take, when it is held to
   a limit (ulimit -v); -1 when it is not, or the system does not say. Read
   from the system at most once a second, unless FRESH: the figure kept may
   be from before memory was given back. */
int64_t tuatara_address_space_left(bool fresh) {
  static _Atomic int64_t left = -1;
  static _Atomic int64_t read_at = INT64_MIN;
  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC, &now);
  if (!fresh && now.tv_sec == atomic_load(&read_at)) return atomic_load(&left);
  struct rlimit limit;
  int64_t found = -1;
  if (getrlimit(RLIMIT_AS, &limit) == 0 && limit.rlim_cur != RLIM_INFINITY) {
    long pages = 0, page = sysconf(_SC_PAGESIZE);
    FILE *statm = fopen("/proc/self/statm", "r");
    if (statm) {
      int fields = fscanf(statm, "%ld", &pages);
      fclose(statm);
      if (fields == 1 && page > 0) {
        int64_t used = (int64_t)pages * page;
        found = (int64_t)limit.rlim_cur > used ? (int64_t)limit.rlim_cur - used : 0;
      }
    }
  }
  atomic_store(&left, found);
  atomic_store(&read_at, (int64_t)now.tv_sec);
  return found;
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
  jmp_buf *escape = tuatara_escape;
  tuatara_escape = NULL;
  const char *text = input->read(input->payload, byte, length);
  tuatara_escape = escape;
  return text;
}

/* When a parse is to stop: when *CANCEL becomes nonzero; once DEADLINE has
   passed if LIMITED, which sets *CANCEL to 2; once the library holds more
   than BUDGET octets beyond the HELD it held when the parse began, unless
   BUDGET is 0, which sets *CANCEL to 3; or when memory runs short, which sets
   it to 6. */
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
  if (atomic_load_explicit(&tuatara_short, memory_order_relaxed)) {
    *progress->cancel = 6;
    return true;
  }
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
   match the text) unless it is NULL. The parse stops, returning NULL and
   setting *CANCEL to say why, soon after *CANCEL becomes nonzero; once LIMIT
   milliseconds have passed unless LIMIT is 0 (2); once it has taken BUDGET
   octets of memory unless BUDGET is 0 (3); when the grammar's own code gave
   the parser what it cannot have, a token number the grammar does not have
   or a state longer than its buffer (4); and when memory runs short, or is
   short already, so that none starts (6). When memory ran out within an
   allocation of tree-sitter's (7), PARSER is left half way and is not to be
   used again; else it must be reset after a NULL. */
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
  tuatara_take_reserve();
  if (atomic_load(&tuatara_short)) {
    *cancel = 6;
    return NULL;
  }
  TSParseOptions options = {&progress, tuatara_cancelled};
  jmp_buf escape;
  TSTree *volatile tree = NULL;
  if (setjmp(escape) == 0) {
    tuatara_escape = &escape;
    tree = ts_parser_parse_with_options(parser, old, input, options);
  } else {
    *cancel = 7;
  }
  tuatara_escape = NULL;
  /* stopped by none of the above: the grammar went wrong */
  if (!tree && *cancel == 0) *cancel = 4;
  tuatara_take_reserve();
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
