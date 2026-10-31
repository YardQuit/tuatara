/* syntax/helper.c --- The program that parses for the editor, shut in.
   Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

   Built by tools/grammars.sh, with tree-sitter and syntax/syntax.c, into
   tuatara-syntax-helper beside the grammars. The editor starts it shut in
   (src/io/sandbox.lisp: no network, no files but the grammars, its memory
   held), one for each file it parses or one for the files of projects you
   trust, and asks it everything it knows of syntax trees over its standard
   input, a local socket both ways. So a grammar's own code, which a file's
   text sets going (a scanner), runs in this program and never in the
   editor: when it goes wrong, crashing, looping or taking memory without
   end, this program is ended and another started, and the editor carries
   on.

   The editor speaks to it over connections, local sockets: its standard
   input is the first, over which the editor hands it the others
   (OP_CONNECT, with the socket's descriptor), one for each of the editor's
   threads asking at once, each served by a thread of this program's own.
   What goes over a connection are messages: a header of three 32-bit numbers
   (the octets after it, the number of the request, what is asked or, in a
   reply, how it went), then what the message holds, numbers of 32 bits
   unless said. The editor asks; this program answers each request with a
   reply of the same number, on the same connection. The one thing it asks
   the editor is the text a parse reads (OP_READ), on the connection the
   parse was asked on, a request numbered with the top bit set. Trees,
   parsers and queries are known by numbers this program gives them, or for
   queries the editor gives. */

#define _GNU_SOURCE
#include <dlfcn.h>
#include <errno.h>
#include <malloc.h>
#include <pthread.h>
#include <signal.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/prctl.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <unistd.h>
#include <tree_sitter/api.h>

/* syntax/syntax.c, compiled with this program */
typedef const char *(*tuatara_reader)(void *payload, uint32_t byte, uint32_t *length);
TSTree *tuatara_parse(TSParser *parser, const TSTree *old, tuatara_reader read, void *payload,
                      volatile int32_t *cancel, uint32_t limit, uint64_t budget);
void tuatara_edit(TSTree *tree, uint32_t start, uint32_t old_end, uint32_t new_end,
                  uint32_t start_row, uint32_t start_column, uint32_t old_end_row, uint32_t old_end_column,
                  uint32_t new_end_row, uint32_t new_end_column);
bool tuatara_tree_has_error(const TSTree *tree);
void tuatara_query(TSQueryCursor *cursor, const TSQuery *query, const TSTree *tree, uint32_t start, uint32_t end);
int32_t tuatara_next_match(TSQueryCursor *cursor, uint32_t *pattern, uint32_t *captures, uint32_t max);
int32_t tuatara_ancestry(const TSTree *tree, uint32_t start, uint32_t end, uint32_t *out, uint32_t max);
int32_t tuatara_select_node(const TSTree *tree, uint32_t start, uint32_t end, int32_t how, uint32_t *out);
int32_t tuatara_set_included_ranges(TSParser *parser, const uint32_t *ranges, uint32_t count);
int32_t tuatara_node_child_ranges(const TSTree *tree, uint32_t start, uint32_t end, uint32_t symbol,
                                  bool named, uint32_t from, uint32_t to, uint32_t *out, uint32_t max);
bool tuatara_memory_back(void);
int64_t tuatara_held(void);
int64_t tuatara_address_space_left(bool fresh);
int64_t tuatara_physical_memory(void);

/* What is asked. */
enum {
  OP_GRAMMAR = 1,     /* name -> version, count, then per symbol: named (0/1), length, octets */
  OP_PARSER,          /* grammar name -> parser */
  OP_PARSER_DELETE,   /* parser -> */
  OP_PARSE,           /* parser, old tree or 0, limit ms, budget (64 bits) -> tree or 0, why */
  OP_CANCEL,          /* parser, on the first connection -> (no reply) */
  OP_EDIT,            /* tree, nine numbers -> */
  OP_COPY,            /* tree -> tree */
  OP_TREE_DELETE,     /* tree -> */
  OP_QUERY,           /* query, grammar name, source -> 0, captures, patterns, ... or 1, offset, kind */
  OP_QUERY_DELETE,    /* query -> */
  OP_DISABLE,         /* query, pattern -> */
  OP_MATCHES,         /* query, tree, start, end -> matches: pattern, count, count x (capture, start, end, symbol) */
  OP_ANCESTRY,        /* tree, start, end -> count x (start, end, symbol, flags, named children) */
  OP_SELECT,          /* tree, start, end, how -> found, start, end */
  OP_CHILDREN,        /* tree, start, end, symbol, named, from, to -> count or -1, count x (start, end) */
  OP_HAS_ERROR,       /* tree -> 0/1 */
  OP_RESET,           /* parser -> */
  OP_MEMORY,          /* -> held (64 bits), memory back (0/1), left (64 bits) */
  OP_CONNECT,         /* on the first connection, with a socket's descriptor -> (no reply) */
  OP_PARSE_RANGES,    /* texts in other languages: n, then for each a tree already made, or 0 then
                         its grammar's name, count, count x six numbers, limit ms, budget (64 bits);
                         and the queries to run on the tree: n, n x (query, start, end); after them
                         the text: the octet it starts at, its length and its octets (no length:
                         asked for as a parse's is) -> for each, tree or 0, why, then for each query
                         its matches, after their octets (as OP_MATCHES gives them) */
  OP_MATCHES_MANY,    /* n, n x (query, tree, start, end) -> for each, its matches after their octets */
  OP_CHILDREN_MANY,   /* n, n x (tree, start, end, symbol, named, from, to) -> for each, as OP_CHILDREN gives */
  OP_READ = 64,       /* asked of the editor: parser, octet -> the octets */
};

/* How a request went, in its reply's third number. */
enum { DONE = 0, REFUSED = 1, TOO_LARGE = 2 };

/* The most octets a reply may hold, as the editor takes them. */
#ifndef REPLY_MOST
#define REPLY_MOST (256u << 20)
#endif

#define TOP_BIT 0x80000000u

/* ---- a connection, read and written by its thread alone ---- */

/* BODY holds the body of the request being answered; TEXT the text a parse
   asked for on it, apart, so that the request stays whole while it runs. */
typedef struct {
  int fd;
  char *body;
  uint32_t room;
  char *text;
  uint32_t text_room;
} Connection;

static bool write_all(int fd, const void *data, size_t size) {
  const char *at = data;
  while (size > 0) {
    ssize_t n = write(fd, at, size);
    if (n < 0 && errno == EINTR) continue;
    if (n <= 0) return false;
    at += n;
    size -= (size_t)n;
  }
  return true;
}

static bool send_message(Connection *c, uint32_t id, uint32_t how, const void *body, uint32_t size) {
  uint32_t header[3] = {size, id, how};
  return write_all(c->fd, header, sizeof header) && (!size || write_all(c->fd, body, size));
}

static bool read_all(int fd, void *data, size_t size) {
  char *at = data;
  while (size > 0) {
    ssize_t n = read(fd, at, size);
    if (n < 0 && errno == EINTR) continue;
    if (n <= 0) return false;
    at += n;
    size -= (size_t)n;
  }
  return true;
}

/* The next message of connection C: its header in HEADER, its body in
   C->body; false when the connection has ended. */
static bool read_into(int fd, uint32_t header[3], char **body, uint32_t *room) {
  if (!read_all(fd, header, 12) || header[0] > (256u << 20)) return false;
  if (header[0] + 1 > *room) {
    char *grown = realloc(*body, header[0] + 1);
    if (!grown) _exit(3);
    *body = grown;
    *room = header[0] + 1;
  }
  if (!read_all(fd, *body, header[0])) return false;
  (*body)[header[0]] = 0;
  return true;
}

static bool read_message(Connection *c, uint32_t header[3]) {
  return read_into(c->fd, header, &c->body, &c->room);
}

/* A reply's body, made up as it is written; TOO_LARGE once more than
   REPLY_MOST was put in it, the rest then left out. */
typedef struct {
  char *data;
  uint32_t size, room;
  bool too_large;
} Out;

static void out_put(Out *out, const void *data, uint32_t size) {
  if (out->too_large || (uint64_t)out->size + size > REPLY_MOST) {
    out->too_large = true;
    return;
  }
  if (out->size + size > out->room) {
    uint64_t room = out->room ? out->room : 4096;
    while (room < (uint64_t)out->size + size) room *= 2;
    if (room > REPLY_MOST) room = REPLY_MOST;
    char *data = realloc(out->data, (size_t)room);
    if (!data) _exit(3);
    out->data = data;
    out->room = (uint32_t)room;
  }
  memcpy(out->data + out->size, data, size);
  out->size += size;
}

static void out_u32(Out *out, uint32_t value) { out_put(out, &value, 4); }

/* ---- what this program holds, by number ---- */

typedef struct {
  void **items;
  uint32_t count;
} Table;

static pthread_mutex_t table_lock = PTHREAD_MUTEX_INITIALIZER;

/* Put ITEM in TABLE; its number, from 1. */
static uint32_t table_put(Table *table, void *item) {
  pthread_mutex_lock(&table_lock);
  uint32_t i;
  for (i = 0; i < table->count && table->items[i]; i++) {}
  if (i == table->count) {
    void **items = realloc(table->items, sizeof(void *) * (table->count * 2 + 16));
    if (!items) _exit(3);
    memset(items + table->count, 0, sizeof(void *) * (table->count + 16));
    table->items = items;
    table->count = table->count * 2 + 16;
  }
  table->items[i] = item;
  pthread_mutex_unlock(&table_lock);
  return i + 1;
}

static void *table_get(Table *table, uint32_t number) {
  pthread_mutex_lock(&table_lock);
  void *item = number >= 1 && number <= table->count ? table->items[number - 1] : NULL;
  pthread_mutex_unlock(&table_lock);
  return item;
}

/* Put ITEM in TABLE as number NUMBER, from 1, giving what was there. */
static void *table_set(Table *table, uint32_t number, void *item) {
  if (number < 1 || number > (1u << 24)) return item;
  pthread_mutex_lock(&table_lock);
  if (number > table->count) {
    uint32_t count = number * 2 + 16;
    void **items = realloc(table->items, sizeof(void *) * count);
    if (!items) _exit(3);
    memset(items + table->count, 0, sizeof(void *) * (count - table->count));
    table->items = items;
    table->count = count;
  }
  void *before = table->items[number - 1];
  table->items[number - 1] = item;
  pthread_mutex_unlock(&table_lock);
  return before;
}

static void *table_take(Table *table, uint32_t number) {
  pthread_mutex_lock(&table_lock);
  void *item = NULL;
  if (number >= 1 && number <= table->count) {
    item = table->items[number - 1];
    table->items[number - 1] = NULL;
  }
  pthread_mutex_unlock(&table_lock);
  return item;
}

static Table trees, parsers, queries;

/* ---- grammars ---- */

typedef struct Grammar {
  char name[65];
  const TSLanguage *language;
  struct Grammar *next;
} Grammar;

static Grammar *grammars;
/* The directories grammars are loaded from, in order: those the editor named. */
static char **grammar_directories;
static int grammar_directory_count;

/* The grammar NAME (letters, digits, _ and -), loaded from NAME.so in the
   first grammar directory that has it, the first time; NULL when there is
   none, or when that file cannot be loaded: a later directory's file of the
   same name is never taken in its place. */
static const TSLanguage *find_language(const char *name, uint32_t length) {
  if (length < 1 || length > 64) return NULL;
  for (uint32_t i = 0; i < length; i++) {
    char c = name[i];
    if (!((c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '_' || c == '-')) return NULL;
  }
  for (Grammar *g = grammars; g; g = g->next)
    if (strlen(g->name) == length && memcmp(g->name, name, length) == 0) return g->language;
  char path[4200], symbol[80];
  snprintf(symbol, sizeof symbol, "tree_sitter_%.*s", (int)length, name);
  for (char *c = symbol; *c; c++) if (*c == '-') *c = '_';
  void *library = NULL;
  for (int i = 0; i < grammar_directory_count; i++) {
    snprintf(path, sizeof path, "%s/%.*s.so", grammar_directories[i], (int)length, name);
    if (access(path, F_OK) == 0) {
      library = dlopen(path, RTLD_NOW | RTLD_LOCAL);
      break;
    }
  }
  if (!library) return NULL;
  const TSLanguage *(*entry)(void) = (const TSLanguage *(*)(void))dlsym(library, symbol);
  const TSLanguage *language = entry ? entry() : NULL;
  if (!language || ts_language_abi_version(language) < 13 || ts_language_abi_version(language) > 15) return NULL;
  Grammar *g = calloc(1, sizeof *g);
  if (!g) _exit(3);
  memcpy(g->name, name, length);
  g->language = language;
  g->next = grammars;
  grammars = g;
  return language;
}

/* ---- parsers ---- */

typedef struct {
  TSParser *parser;
  uint32_t number;
  pthread_mutex_t lock;
  /* BUSY while a parse runs; ENDING when the parser is to be deleted once
     it is done */
  bool busy, ending;
  volatile int32_t cancel;
  /* the connection the parse was asked on, which its text is read over;
     or the text it was given with the request, GIVEN octets of it from
     octet GIVEN_FROM, at GIVEN_TEXT */
  Connection *connection;
  const char *given_text;
  uint32_t given_from, given;
} Parser;

static void parser_delete(Parser *p) {
  table_take(&parsers, p->number);
  ts_parser_delete(p->parser);
  free(p);
}

/* The parser NUMBER, made busy for the caller alone, or NULL when there is
   none, another uses it or it is being deleted. Looked up and claimed
   under the table's lock, which deleting it takes first, so that it is not
   freed meanwhile. */
static Parser *parser_claim(uint32_t number) {
  pthread_mutex_lock(&table_lock);
  Parser *p = number >= 1 && number <= parsers.count ? parsers.items[number - 1] : NULL;
  if (p) {
    pthread_mutex_lock(&p->lock);
    bool free_to_use = !p->busy && !p->ending;
    if (free_to_use) p->busy = true;
    pthread_mutex_unlock(&p->lock);
    if (!free_to_use) p = NULL;
  }
  pthread_mutex_unlock(&table_lock);
  return p;
}

/* Let go of P, claimed by PARSER_CLAIM, deleting it when that was asked meanwhile. */
static void parser_release(Parser *p) {
  pthread_mutex_lock(&p->lock);
  p->busy = false;
  bool ending = p->ending;
  pthread_mutex_unlock(&p->lock);
  if (ending) parser_delete(p);
}

/* The text from octet BYTE on of the text the parse of PARSER reads, asked
   of the editor on the parse's connection; a zero LENGTH at its end, and
   when the connection has ended. */
static const char *read_text(void *payload, uint32_t byte, uint32_t *length) {
  Parser *p = payload;
  Connection *c = p->connection;
  if (p->given_text) {
    *length = byte >= p->given_from && byte - p->given_from < p->given ? p->given - (byte - p->given_from) : 0;
    return *length ? p->given_text + (byte - p->given_from) : "";
  }
  uint32_t body[2] = {p->number, byte}, header[3];
  *length = 0;
  if (!send_message(c, TOP_BIT | p->number, OP_READ, body, sizeof body)) return "";
  if (!read_into(c->fd, header, &c->text, &c->text_room) || header[1] != (TOP_BIT | p->number)) {
    p->cancel = 1;
    return "";
  }
  *length = header[0];
  return header[0] ? c->text : "";
}

/* ---- the requests ---- */

static uint32_t word(const char *body, uint32_t size, uint32_t i) {
  uint32_t value = 0;
  if ((i + 1) * 4 <= size) memcpy(&value, body + 4 * i, 4);
  return value;
}

/* A parser a connection keeps for each grammar it parses ranges of text in
   (text in another language), used again. */
typedef struct Kept {
  const TSLanguage *language;
  Parser *parser;
  struct Kept *next;
} Kept;

/* What a connection's thread answers with: the reply made up, the numbers
   a call into tree-sitter gives, the cursor queries run with, the parsers
   kept for ranges of text. */
typedef struct {
  Out out;
  uint32_t numbers[4 * 1024 * 5];
  TSQueryCursor *cursor;
  Kept *kept;
} Answering;

static pthread_mutex_t grammar_lock = PTHREAD_MUTEX_INITIALIZER;

/* Put in OUT the matches of QUERY in TREE from START below END, as OP_MATCHES
   gives them, after their octets when SIZED (all ones when the query or the
   tree is not known). */
static void put_matches(Out *out, Answering *a, TSQuery *query, TSTree *tree, uint32_t start, uint32_t end, bool sized) {
  /* nothing more once the reply is too large: it is refused whole */
  if (out->too_large) return;
  uint32_t at = out->size;
  if (sized) out_u32(out, 0);
  if (out->too_large) return;
  if (!query || !tree) {
    if (sized) memcpy(out->data + at, &(uint32_t){0xffffffffu}, 4);
    return;
  }
  tuatara_query(a->cursor, query, tree, start, end);
  uint32_t pattern;
  int32_t count;
  while (!out->too_large && (count = tuatara_next_match(a->cursor, &pattern, a->numbers, 64)) >= 0) {
    uint32_t n = count < 64 ? (uint32_t)count : 64;
    out_u32(out, pattern);
    out_u32(out, n);
    out_put(out, a->numbers, n * 16);
  }
  if (sized && !out->too_large) {
    uint32_t size = out->size - at - 4;
    memcpy(out->data + at, &size, 4);
  }
}

/* Answer on connection C request ID, asking HOW with BODY of SIZE octets
   (C->body), working in A; false when the connection has ended. */
static bool answer(Connection *c, Answering *a, uint32_t id, uint32_t how, char *body, uint32_t size) {
  Out out = a->out;
  uint32_t *numbers = a->numbers;
  out.size = 0;
  out.too_large = false;
  uint32_t status = DONE;
#define W(i) word(body, size, i)
  switch (how) {
  case OP_GRAMMAR: {
    pthread_mutex_lock(&grammar_lock);
    const TSLanguage *language = find_language(body, size);
    pthread_mutex_unlock(&grammar_lock);
    if (!language) { status = REFUSED; break; }
    uint32_t count = ts_language_symbol_count(language);
    out_u32(&out, ts_language_abi_version(language));
    out_u32(&out, count);
    for (uint32_t i = 0; i < count; i++) {
      const char *name = ts_language_symbol_name(language, (TSSymbol)i);
      uint32_t length = name ? (uint32_t)strlen(name) : 0;
      out_u32(&out, ts_language_symbol_type(language, (TSSymbol)i) == TSSymbolTypeRegular);
      out_u32(&out, length);
      out_put(&out, name ? name : "", length);
      while (out.size % 4) out_put(&out, "", 1);
    }
    break;
  }
  case OP_PARSER: {
    pthread_mutex_lock(&grammar_lock);
    const TSLanguage *language = find_language(body, size);
    pthread_mutex_unlock(&grammar_lock);
    TSParser *parser = language ? ts_parser_new() : NULL;
    if (!parser || !ts_parser_set_language(parser, language)) { status = REFUSED; break; }
    Parser *p = calloc(1, sizeof *p);
    if (!p) _exit(3);
    p->parser = parser;
    pthread_mutex_init(&p->lock, NULL);
    p->number = table_put(&parsers, p);
    out_u32(&out, p->number);
    break;
  }
  case OP_PARSER_DELETE: {
    /* one parsing is deleted by its parse's thread once that is done */
    pthread_mutex_lock(&table_lock);
    Parser *p = W(0) >= 1 && W(0) <= parsers.count ? parsers.items[W(0) - 1] : NULL;
    bool now = false;
    if (p) {
      pthread_mutex_lock(&p->lock);
      now = !p->busy && !p->ending;
      if (now) p->busy = true;
      p->ending = true;
      pthread_mutex_unlock(&p->lock);
    }
    pthread_mutex_unlock(&table_lock);
    if (now) parser_delete(p);
    break;
  }
  case OP_PARSE: {
    /* parsed here, the text read over this connection */
    if (size < 20) { status = REFUSED; break; }
    Parser *p = parser_claim(W(0));
    if (!p) { status = REFUSED; break; }
    TSTree *old = W(1) ? table_get(&trees, W(1)) : NULL;
    uint32_t limit = W(2);
    uint64_t budget;
    memcpy(&budget, body + 12, 8);
    p->cancel = 0;
    p->connection = c;
    /* the budget is held to half of what this program may still take, the
       figure kept read again when it is what holds the parse down: memory
       may have been given back since; a parse that spends a budget so held
       down is stopped as one that memory ran short for */
    bool tight = false;
    int64_t left = tuatara_address_space_left(false);
    if (left >= 0 && (budget == 0 || (uint64_t)(left / 2) < budget)) left = tuatara_address_space_left(true);
    if (left >= 0 && (budget == 0 || (uint64_t)(left / 2) < budget)) {
      budget = left / 2 > 0 ? (uint64_t)(left / 2) : 1;
      tight = true;
    }
    TSTree *tree = tuatara_parse(p->parser, old, read_text, p, &p->cancel, limit, budget);
    if (!tree && tight && p->cancel == 3) p->cancel = 6;
    out_u32(&out, tree ? table_put(&trees, tree) : 0);
    out_u32(&out, (uint32_t)p->cancel);
    /* a parser left half way when memory ran out is not used again */
    if (!tree && p->cancel != 7) ts_parser_reset(p->parser);
    parser_release(p);
    break;
  }
  case OP_PARSE_RANGES: {
    /* first where each text's part of the request starts, and where the text given starts */
    uint32_t n = W(0), at = 4;
    uint32_t starts[256], made[256], made_count = 0;
    if (n > 256) { status = REFUSED; break; }
    bool good = true;
    for (uint32_t i = 0; i < n && good; i++) {
      starts[i] = at;
      if (at + 4 > size) { good = false; break; }
      if (word(body, size, at / 4) == 0) {
        uint32_t name_length = word(body, size, at / 4 + 1);
        if (name_length > 256) { good = false; break; }
        at += 8 + ((name_length + 3) & ~3u);
        uint32_t count = at + 4 <= size ? word(body, size, at / 4) : 0;
        if (count > 4096) { good = false; break; }
        at += 4 + count * 24 + 12;
      } else {
        at += 4;
      }
      uint32_t asked = at + 4 <= size ? word(body, size, at / 4) : 0;
      if (asked > 64) { good = false; break; }
      at += 4 + asked * 12;
      if (at > size) good = false;
    }
    if (!good || at + 8 > size) { status = REFUSED; break; }
    uint32_t given_from = word(body, size, at / 4), given = word(body, size, at / 4 + 1);
    const char *given_text = given > 0 && given <= size - at - 8 ? body + at + 8 : NULL;
    for (uint32_t i = 0; i < n && !out.too_large; i++) {
      uint32_t item = starts[i];
      TSTree *tree = NULL;
      uint32_t why = 0;
      if (word(body, size, item / 4) != 0) {
        tree = table_get(&trees, word(body, size, item / 4));
        out_u32(&out, tree ? word(body, size, item / 4) : 0);
        item += 4;
      } else {
        /* parsed with a parser kept for its grammar */
        uint32_t name_length = word(body, size, item / 4 + 1);
        pthread_mutex_lock(&grammar_lock);
        const TSLanguage *language = find_language(body + item + 8, name_length);
        pthread_mutex_unlock(&grammar_lock);
        item += 8 + ((name_length + 3) & ~3u);
        uint32_t count = word(body, size, item / 4);
        const uint32_t *ranges = (const uint32_t *)(body + item + 4);
        item += 4 + count * 24;
        uint32_t limit = word(body, size, item / 4);
        uint64_t budget;
        memcpy(&budget, body + item + 4, 8);
        item += 12;
        Kept *k = a->kept;
        while (language && k && k->language != language) k = k->next;
        if (language && !k) {
          TSParser *parser = ts_parser_new();
          if (ts_parser_set_language(parser, language)) {
            k = calloc(1, sizeof *k);
            Parser *p = k ? calloc(1, sizeof *p) : NULL;
            if (!p) _exit(3);
            p->parser = parser;
            pthread_mutex_init(&p->lock, NULL);
            k->language = language;
            k->parser = p;
            k->next = a->kept;
            a->kept = k;
          } else {
            ts_parser_delete(parser);
          }
        }
        Parser *p = k ? k->parser : NULL;
        if (p && tuatara_set_included_ranges(p->parser, ranges, count)) {
          p->cancel = 0;
          p->connection = c;
          p->given_text = given_text;
          p->given_from = given_from;
          p->given = given_text ? given : 0;
          tree = tuatara_parse(p->parser, NULL, read_text, p, &p->cancel, limit, budget);
          p->given_text = NULL;
          why = (uint32_t)p->cancel;
          if (!tree && p->cancel == 7) {
            /* left half way: another is made next time */
            k->language = NULL;
          } else if (!tree) {
            ts_parser_reset(p->parser);
          }
        }
        uint32_t number = tree ? table_put(&trees, tree) : 0;
        if (number) made[made_count++] = number;
        out_u32(&out, number);
      }
      out_u32(&out, why);
      /* the queries asked to run on it */
      uint32_t asked = word(body, size, item / 4);
      for (uint32_t j = 0; j < asked; j++)
        put_matches(&out, a, tree ? table_get(&queries, word(body, size, item / 4 + 1 + 3 * j)) : NULL, tree,
                    word(body, size, item / 4 + 2 + 3 * j), word(body, size, item / 4 + 3 + 3 * j), true);
    }
    /* a reply refused as too large: the trees it would have named let go */
    if (out.too_large)
      for (uint32_t i = 0; i < made_count; i++) {
        TSTree *made_tree = table_take(&trees, made[i]);
        if (made_tree) ts_tree_delete(made_tree);
      }
    break;
  }
  case OP_EDIT: {
    TSTree *tree = table_get(&trees, W(0));
    if (tree) tuatara_edit(tree, W(1), W(2), W(3), W(4), W(5), W(6), W(7), W(8), W(9));
    break;
  }
  case OP_COPY: {
    TSTree *tree = table_get(&trees, W(0));
    if (!tree) { status = REFUSED; break; }
    out_u32(&out, table_put(&trees, ts_tree_copy(tree)));
    break;
  }
  case OP_TREE_DELETE: {
    TSTree *tree = table_take(&trees, W(0));
    if (tree) ts_tree_delete(tree);
    break;
  }
  case OP_QUERY: {
    uint32_t number = W(0), name_length = W(1);
    if (8 + name_length > size) { status = REFUSED; break; }
    pthread_mutex_lock(&grammar_lock);
    const TSLanguage *language = find_language(body + 8, name_length);
    pthread_mutex_unlock(&grammar_lock);
    if (!language) { status = REFUSED; break; }
    uint32_t start = 8 + ((name_length + 3) & ~3u), offset = 0;
    TSQueryError kind = TSQueryErrorNone;
    TSQuery *query = start <= size ? ts_query_new(language, body + start, size - start, &offset, &kind) : NULL;
    if (!query) { out_u32(&out, 1); out_u32(&out, offset); out_u32(&out, kind); break; }
    TSQuery *before = table_set(&queries, number, query);
    if (before) ts_query_delete(before);
    uint32_t captures = ts_query_capture_count(query), patterns = ts_query_pattern_count(query);
    out_u32(&out, 0);
    out_u32(&out, captures);
    for (uint32_t i = 0; i < captures; i++) {
      uint32_t length;
      const char *name = ts_query_capture_name_for_id(query, i, &length);
      out_u32(&out, length);
      out_put(&out, name, length);
      while (out.size % 4) out_put(&out, "", 1);
    }
    out_u32(&out, patterns);
    for (uint32_t i = 0; i < patterns; i++) {
      uint32_t steps;
      const TSQueryPredicateStep *step = ts_query_predicates_for_pattern(query, i, &steps);
      out_u32(&out, ts_query_start_byte_for_pattern(query, i));
      out_u32(&out, ts_query_is_pattern_non_local(query, i));
      out_u32(&out, steps);
      for (uint32_t s = 0; s < steps; s++) {
        out_u32(&out, step[s].type);
        if (step[s].type == TSQueryPredicateStepTypeString) {
          uint32_t length;
          const char *text = ts_query_string_value_for_id(query, step[s].value_id, &length);
          out_u32(&out, length);
          out_put(&out, text, length);
          while (out.size % 4) out_put(&out, "", 1);
        } else {
          out_u32(&out, step[s].value_id);
        }
      }
    }
    break;
  }
  case OP_QUERY_DELETE: {
    TSQuery *query = table_take(&queries, W(0));
    if (query) ts_query_delete(query);
    break;
  }
  case OP_DISABLE: {
    TSQuery *query = table_get(&queries, W(0));
    if (query && W(1) < ts_query_pattern_count(query)) ts_query_disable_pattern(query, W(1));
    break;
  }
  case OP_MATCHES: {
    TSQuery *query = table_get(&queries, W(0));
    TSTree *tree = table_get(&trees, W(1));
    if (!query || !tree) { status = REFUSED; break; }
    put_matches(&out, a, query, tree, W(2), W(3), false);
    break;
  }
  case OP_MATCHES_MANY: {
    uint32_t n = W(0);
    if (n > 64 || size < 4 + n * 16) { status = REFUSED; break; }
    for (uint32_t i = 0; i < n; i++)
      put_matches(&out, a, table_get(&queries, W(1 + 4 * i)), table_get(&trees, W(2 + 4 * i)),
                  W(3 + 4 * i), W(4 + 4 * i), true);
    break;
  }
  case OP_ANCESTRY: {
    TSTree *tree = table_get(&trees, W(0));
    if (!tree) { status = REFUSED; break; }
    int32_t n = tuatara_ancestry(tree, W(1), W(2), numbers, 1024);
    out_put(&out, numbers, (uint32_t)n * 20);
    break;
  }
  case OP_SELECT: {
    TSTree *tree = table_get(&trees, W(0));
    if (!tree) { status = REFUSED; break; }
    uint32_t found = (uint32_t)tuatara_select_node(tree, W(1), W(2), (int32_t)W(3), numbers);
    out_u32(&out, found);
    out_put(&out, numbers, 8);
    break;
  }
  case OP_CHILDREN: {
    TSTree *tree = table_get(&trees, W(0));
    if (!tree) { status = REFUSED; break; }
    int32_t n = tuatara_node_child_ranges(tree, W(1), W(2), W(3), W(4) != 0, W(5), W(6), numbers, 4096);
    out_u32(&out, (uint32_t)n);
    if (n > 0) out_put(&out, numbers, (uint32_t)n * 8);
    break;
  }
  case OP_CHILDREN_MANY: {
    uint32_t n = W(0);
    if (n > 4096 || size < 4 + n * 28) { status = REFUSED; break; }
    for (uint32_t i = 0; i < n && !out.too_large; i++) {
      uint32_t k = 1 + 7 * i;
      TSTree *tree = table_get(&trees, W(k));
      int32_t got = tree ? tuatara_node_child_ranges(tree, W(k + 1), W(k + 2), W(k + 3), W(k + 4) != 0, W(k + 5),
                                                     W(k + 6), numbers, 4096)
                         : -1;
      out_u32(&out, (uint32_t)got);
      if (got > 0) out_put(&out, numbers, (uint32_t)got * 8);
    }
    break;
  }
  case OP_HAS_ERROR: {
    TSTree *tree = table_get(&trees, W(0));
    if (!tree) { status = REFUSED; break; }
    out_u32(&out, tuatara_tree_has_error(tree));
    break;
  }
  case OP_RESET: {
    /* not while it parses on another connection */
    Parser *p = parser_claim(W(0));
    if (!p) { status = REFUSED; break; }
    ts_parser_reset(p->parser);
    parser_release(p);
    break;
  }
  case OP_MEMORY: {
    int64_t held = tuatara_held(), left = tuatara_address_space_left(true);
    out_put(&out, &held, 8);
    out_u32(&out, tuatara_memory_back());
    out_u32(&out, 0);
    out_put(&out, &left, 8);
    break;
  }
  default:
    status = REFUSED;
  }
#undef W
  a->out = out;
  if (out.too_large) return send_message(c, id, TOO_LARGE, NULL, 0);
  return send_message(c, id, status, out.data, out.size);
}

/* What the thread of a connection does: answer its requests until it ends. */
static void *serve(void *payload) {
  Connection *c = payload;
  Answering *a = calloc(1, sizeof *a);
  if (!a) _exit(3);
  a->cursor = ts_query_cursor_new();
  ts_query_cursor_set_match_limit(a->cursor, 256);
  uint32_t header[3];
  while (read_message(c, header) && answer(c, a, header[1], header[2], c->body, header[0])) {}
  close(c->fd);
  ts_query_cursor_delete(a->cursor);
  for (Kept *k = a->kept, *next; k; k = next) {
    next = k->next;
    if (k->language) { ts_parser_delete(k->parser->parser); free(k->parser); }
    free(k);
  }
  free(a->out.data);
  free(a);
  free(c->body);
  free(c->text);
  free(c);
  return NULL;
}

/* Read the next message of the first connection, a socket descriptor sent
   with it put in *FD (-1 when none came); false when it has ended. */
static bool read_first(Connection *c, uint32_t header[3], int *fd) {
  *fd = -1;
  char control[CMSG_SPACE(sizeof(int))];
  struct iovec part = {header, 12};
  struct msghdr message = {0};
  message.msg_iov = &part;
  message.msg_iovlen = 1;
  message.msg_control = control;
  message.msg_controllen = sizeof control;
  ssize_t n;
  do n = recvmsg(c->fd, &message, MSG_CMSG_CLOEXEC); while (n < 0 && errno == EINTR);
  if (n <= 0) return false;
  for (struct cmsghdr *m = CMSG_FIRSTHDR(&message); m; m = CMSG_NXTHDR(&message, m))
    if (m->cmsg_level == SOL_SOCKET && m->cmsg_type == SCM_RIGHTS) memcpy(fd, CMSG_DATA(m), sizeof(int));
  if (n < 12 && !read_all(c->fd, (char *)header + n, 12 - (size_t)n)) return false;
  if (header[0] > (256u << 20)) return false;
  if (header[0] + 1 > c->room) {
    char *body = realloc(c->body, header[0] + 1);
    if (!body) _exit(3);
    c->body = body;
    c->room = header[0] + 1;
  }
  return read_all(c->fd, c->body, header[0]);
}

int main(int argc, char **argv) {
  /* ended with what started it, the editor or the sandbox it is shut in
     (which ends with the editor), if that ends first */
  pid_t parent = getppid();
  prctl(PR_SET_PDEATHSIG, SIGKILL);
  if (getppid() != parent) return 0;
  grammar_directories = argv + 1;
  grammar_directory_count = argc - 1;
  signal(SIGPIPE, SIG_IGN);
  /* the memory of each thread's own kept to few places: what this program
     may take is held to a limit, and each place keeps much in reserve */
  mallopt(M_ARENA_MAX, 2);
  /* the first connection: the editor hands the others over it, and stops
     parses with it; this program ends when it ends */
  Connection first = {.fd = 0};
  pthread_attr_t attributes;
  pthread_attr_init(&attributes);
  pthread_attr_setdetachstate(&attributes, PTHREAD_CREATE_DETACHED);
  pthread_attr_setstacksize(&attributes, (size_t)4 << 20);
  for (;;) {
    uint32_t header[3];
    int fd;
    if (!read_first(&first, header, &fd)) return 0;
    if (header[2] == OP_CONNECT && fd >= 0) {
      Connection *c = calloc(1, sizeof *c);
      if (!c) _exit(3);
      c->fd = fd;
      pthread_t thread;
      if (pthread_create(&thread, &attributes, serve, c) != 0) _exit(3);
    } else {
      if (fd >= 0) close(fd);
      if (header[2] == OP_CANCEL) {
        /* under the table's lock, which deleting the parser takes first */
        uint32_t number = word(first.body, header[0], 0);
        pthread_mutex_lock(&table_lock);
        Parser *p = number >= 1 && number <= parsers.count ? parsers.items[number - 1] : NULL;
        if (p) p->cancel = 1;
        pthread_mutex_unlock(&table_lock);
      }
    }
  }
}
