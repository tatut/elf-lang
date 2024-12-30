/**
 * Header for Elf scanner.
 * Defines types and public function signatures.
 */
#include "elf.h"

typedef enum TokenType {
  LITERAL,
  IDENTIFIER,
  NAMEREF,
  MULTIPLY, DIVIDE, ADD, SUBTRACT, MODULO, CONCAT,
  COMMA,
  PAREN_OPEN, PAREN_CLOSE, // '(' ')'
  BRACKET_OPEN, BRACKET_CLOSE, // '[' ']'
  BRACE_OPEN, BRACE_CLOSE, // '{' '}'
  BAR, // '|' vertical bar
  DICT_OPEN, // "%{" special syntax for dictionary
  ASSIGN, // ':'
  LESSER_THAN, LESSER_THAN_EQUAL, // '<' "<="
  GREATER_THAN, GREATER_THAN_EQUAL, // '>' ">="
  EQUAL, // '='
  EOS,
  ERROR
} TokenType;

typedef union TokenData {
  ElfVal val;
  char ch;
  char *name;
} TokenData;

typedef struct Token {
  TokenType type;
  size_t line, column;
  TokenData data; // data associated with token
} Token;

typedef struct Scanner {
  char *source, *at;
  size_t line, column;
} Scanner;

Scanner* scanner_new(char* input);
void scanner_free(Scanner *s);

/**
 * Scan for next token. Mutates scanner state as appropriate.
 * Returns Token of type EOS if at end of input.
 * Returns Token of type ERROR if encountering unrecognized input.
 * Tokens may have claimed memory, you must call token_free once done with it.
 */
Token scan(Scanner *s);

/**
 * Free any memory used by the token.
 */
void token_free(Token t);
