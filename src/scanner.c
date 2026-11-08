// External scanner for Delphi 12 multi-line string literals:
//
//   s := '''
//     line one
//     line two
//     ''';
//
// The opening delimiter is an odd number (>= 3) of quotes followed only by
// whitespace up to the end of the line. The literal ends at the first line
// whose only content (after leading whitespace) is the same number of quotes.

#include "tree_sitter/parser.h"

#include <stdbool.h>

enum TokenType { MULTILINE_STRING };

void *tree_sitter_pascal_external_scanner_create(void) { return NULL; }
void tree_sitter_pascal_external_scanner_destroy(void *payload) { (void)payload; }
unsigned tree_sitter_pascal_external_scanner_serialize(void *payload, char *buffer) {
  (void)payload;
  (void)buffer;
  return 0;
}
void tree_sitter_pascal_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
  (void)payload;
  (void)buffer;
  (void)length;
}

static inline void advance(TSLexer *lexer) { lexer->advance(lexer, false); }

static inline bool is_blank(int32_t c) { return c == ' ' || c == '\t'; }

bool tree_sitter_pascal_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
  (void)payload;
  if (!valid_symbols[MULTILINE_STRING]) return false;

  while (is_blank(lexer->lookahead) || lexer->lookahead == '\r' || lexer->lookahead == '\n')
    lexer->advance(lexer, true);

  if (lexer->lookahead != '\'') return false;

  unsigned quotes = 0;
  while (lexer->lookahead == '\'') {
    advance(lexer);
    quotes++;
  }
  if (quotes < 3 || quotes % 2 == 0) return false;

  while (is_blank(lexer->lookahead)) advance(lexer);
  if (lexer->lookahead == '\r') advance(lexer);
  if (lexer->lookahead != '\n') return false;
  advance(lexer);

  for (;;) {
    while (is_blank(lexer->lookahead)) advance(lexer);
    unsigned closing = 0;
    while (lexer->lookahead == '\'') {
      advance(lexer);
      closing++;
    }
    if (closing == quotes) {
      lexer->result_symbol = MULTILINE_STRING;
      return true;
    }
    while (lexer->lookahead != '\n') {
      if (lexer->eof(lexer)) return false;
      advance(lexer);
    }
    advance(lexer);
  }
}
