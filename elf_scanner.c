#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "elf.h"

typedef enum TokenType {
  LITERAL,
  IDENTIFIER,
  NAMEREF,
  MULTIPLY, DIVIDE, ADD, SUBTRACT, MODULO,
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

#define next(s) { s->at++; s->column++; }
#define cur(s) *s->at

bool is_alpha(char ch) {
  return (ch >= 'a' && ch <= 'z') ||
    (ch >= 'A' && ch <= 'Z');
}

bool is_digit(char ch) { return ch >= '0' && ch <= '9'; }

bool is_alphanumeric(char ch) { return is_alpha(ch) || is_digit(ch); }

bool is_identifier(char ch) {
  return is_alphanumeric(ch) ||
    ch == '_' || ch == '?';
}
bool is_identifier_start(char ch) {
  return is_alpha(ch) || ch == '_' || ch == '?';
}

bool looking_at_word(Scanner *s, char *word) {
  char *at = s->at;
  size_t c = 0;
  while(*word != 0) {
    if(*at == 0) return false;
    if(*at != *word) return false;
    at++;
    word++;
    c++;
  }
  if(!is_identifier(*at)) {
    /* source does not continue with an identifier char,
       we found it */
    s->at = at;
    s->column += c;
    return true;
  } else {
    return false;
  }
}

// Read literal ElfVal, allocates new value
Token literal(size_t line, size_t col, ElfVal literal) {
  return (Token) { LITERAL, line, col, literal };
}

// Read identifier, identifier name is interned (FIXME: doit)
Token identifier(Scanner *s) {
  char *at = s->at + 1;
  while(is_identifier(*at)) {
    at++;
  }
  size_t sz = at - s->at;
  char *id = malloc(at - s->at + 1);
  id[sz] = 0;
  memcpy(id, s->at, sz);
  //printf("allocate identifier of len: %ld\n", sz);
  Token t = { IDENTIFIER, s->line, s->column };
  t.data.name = id;
  s->at = at;
  s->column += sz;
  return t;
}

void skip_comment(Scanner *s) {
  // comment until the end of line
  while(*s->at != '\n') s->at++;
  s->at++;
  s->line++;
  s->column = 1;
}

Token number(Scanner *s) {
  long f = 1;
  size_t col = s->column;
  if(cur(s) == '-') {
    f = -1;
    next(s);
  }
  long num = 0;
  while(is_digit(cur(s))) {
    num *= 10;
    num += cur(s) - '0';
    next(s);
  }
  if(cur(s) == '.') {
    next(s);
    // fraction
    double fr = 1;
    double frac = 0;
    while(is_digit(cur(s))) {
      fr *= 10;
      frac = 10*frac + (cur(s) - '0');
      next(s);
    }
    ElfVal v;
    v.type = TYPE_DOUBLE;
    set_double_val(&v, (double) f * ((double) num + frac/fr));
    return literal(s->line, col, v);
  } else {
    ElfVal v;
    v.type = TYPE_INT;
    set_long_val(&v, num * f);
    return literal(s->line, col, v);
  }
  return (Token) { ERROR }; // FIXME
}

/* Return next token from scanner,
   Modifies scanner state. */
Token scan(Scanner *s) {
  for(;;) {
    size_t line = s->line, col = s->column;
    char ch = *s->at;
    char nextch = *(s->at+1);
    TokenType c;
    switch(ch) {
      // single char tokens
    case '[': next(s); return (Token) { BRACKET_OPEN, line, col };
    case ']': next(s); return (Token) { BRACKET_CLOSE, line, col };
    case '(': next(s); return (Token) { PAREN_OPEN, line, col };
    case ')': next(s); return (Token) { PAREN_CLOSE, line, col };
    case '{': next(s); return (Token) { BRACE_OPEN, line, col };
    case '}': next(s); return (Token) { BRACE_CLOSE, line, col };
    case ':': next(s); return (Token) { ASSIGN, line, col };
    case '|': next(s); return (Token) { BAR, line, col };
    case '=': next(s); return (Token) { EQUAL, line, col };
    case ',': next(s); return (Token) { COMMA, line, col };

      // comments and whitespace skips
    case '#': skip_comment(s); break;
    case '\n': s->at++; s->line++; s->column = 1; break;
    case ' ': case '\r': case '\t': next(s); break;

      // binary operators
    case '*': next(s); return (Token) { MULTIPLY, line, col };
    case '/': next(s); return (Token) { DIVIDE, line, col };
    case '+': next(s); return (Token) { ADD, line, col };
    case '-':
      if(is_digit(nextch))
        return number(s);
      else {
        next(s);
        return (Token) { SUBTRACT, line, col };
      }
    case '%':
      next(s);
      if(nextch == '{') {
        next(s);
        return (Token) { DICT_OPEN, line, col };
      }
      else {
        return (Token) { MODULO, line, col };
      }
    case '<': case '>': // comparison
      c = (ch == '<' ? LESSER_THAN : GREATER_THAN);
      next(s);
      if(nextch == '=') { next(s); c++; } // increment to OR EQUAL variant
      return (Token) { c, line, col };

      // reserved words (true, false, nil)
    case 't':
      if(looking_at_word(s, "true")) {
        return literal(line, col, ELF_TRUE);
      } else {
          return identifier(s);
      }
    case 'f':
      if(looking_at_word(s, "false")) {
        return literal(line, col, ELF_FALSE);
      } else {
        return identifier(s);
      }
    case 'n':
      if(looking_at_word(s, "nil")) {
        return literal(line, col, ELF_NIL);
      } else {
        return identifier(s);
      }

      // numbers and identifiers
    default:
      if(is_identifier_start(ch)) {
        return identifier(s);
      } else if(is_digit(ch)) {
        return number(s);
      }
      return (Token){ ERROR };
    }
  }
}

// Unit tests
#ifdef TEST

#include "test.h"

Scanner s_(char *source) { return (Scanner){source, source, 1, 1}; }

#define tok(str)                                                            \
  {                                                                            \
    s = s_(str);                                                               \
    t = scan(&s);                                                              \
  }

#define nexttok()                                                              \
  { t = scan(&s); }

void test() {
  Scanner s;
  Token t;
  printf("sizeof elfdata: %ld\nsizeof token: %ld\n", sizeof(ElfVal), sizeof(Token));
  testing("Booleans", {
      it("reads true", { tok("true");
          assert(t.type == LITERAL && is_true( &t.data ));
        });
      it("reads false", { tok("false");
          assert(t.type == LITERAL && is_false( &t.data ));
        });
      it("doesn't read too long", { tok("trueness");
          assert(t.type == IDENTIFIER &&
                 strcmp(t.data.name, "trueness")==0)
            });
      it("skip ws", { tok("\n\n   true");
          assert(t.line == 3 && t.column == 4 && is_true(&t.data))
            });
      it("skips comment", { tok("# true true, so true\n  false");
          assert(t.line == 2 && t.column == 3 && is_false(&t.data));
        });
    });

  testing("Numbers", {
      it("reads positive number", {
          tok("  42069 ");
          assert(t.type == LITERAL && is_type(&t.data, TYPE_INT) &&
                 long_val(&t.data) == 42069)
            });
      it("reads negative number", {
          tok("# very cold temp\n-273");
          assert(t.type == LITERAL && is_type(&t.data, TYPE_INT) &&
                 long_val(&t.data) == -273)
            });
      it("reads float number", {
          tok("420.69");
          assert(t.type == LITERAL && is_type(&t.data, TYPE_DOUBLE) &&
                 double_val(&t.data) == 420.69);
        });

      it("reads negative float", {
          tok("-3.1415666");
          assert(t.type == LITERAL && is_type(&t.data, TYPE_DOUBLE) &&
                 double_val(&t.data) == -3.1415666);
        });
    });

  testing("Binary operators", {
      it("reads *", {
          tok("   * ");
          assert(t.type == MULTIPLY);
        });
      it("doesn't confuse - with negative number", {
          tok(" -foo");
          assert(t.type == SUBTRACT);
          nexttok();
          assert(t.type == IDENTIFIER && strcmp("foo", t.data.name)==0);
        });
    });

  testing("Comparison operators", {
      it("reads < and <=", {
          tok("< <=");
          assert(t.type == LESSER_THAN);
          nexttok();
          assert(t.type == LESSER_THAN_EQUAL);
        });
      it("reads > and >=", {
          tok("> >=");
          assert(t.type == GREATER_THAN);
          nexttok();
          assert(t.type == GREATER_THAN_EQUAL);
        });
      it("reads =", {
          tok("=");
          assert(t.type == EQUAL);
        });
    });
}

#endif
