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
  BINARY,
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
    switch(ch) {
    case '#': skip_comment(s); break;
    case '\n': s->at++; s->line++; s->column = 1; break;
    case ' ': case '\r': case '\t':
      s->at++; s->column++; break;
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

    default:
      if(is_identifier_start(ch)) {
        return identifier(s);
      } else if(is_digit(ch) || ch == '-') {
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

void test() {
  Scanner s;
  Token t;
  printf("sizeof elfdata: %ld\nsizeof token: %ld\n", sizeof(ElfVal), sizeof(Token));
  testing("Booleans", {
      it("reads true", {
          s = s_("true");
          t = scan(&s);
          assert(t.type == LITERAL && is_true( &t.data ));
        });
      it("reads false", { s = s_("false");
          t = scan(&s);
          assert(t.type == LITERAL && is_false( &t.data ));
        });
      it("doesn't read too long", { s = s_("trueness");
          t = scan(&s);
          assert(t.type == IDENTIFIER &&
                 strcmp(t.data.name, "trueness")==0)
            });
      it("skip ws", { s = s_("\n\n   true");
          t = scan(&s);
          assert(t.line == 3 && t.column == 4 && is_true(&t.data))
            });
      it("skips comment", { s = s_("# true true, so true\n  false");
          t = scan(&s);
          assert(t.line == 2 && t.column == 3 && is_false(&t.data));
        });
    });

  testing("Numbers", {
      it("reads positive number", {
          s = s_("  42069 ");
          t = scan(&s);
          assert(t.type == LITERAL && is_type(&t.data, TYPE_INT) &&
                 long_val(&t.data) == 42069)
            });
      it("reads negative number", {
          s = s_("# very cold temp\n-273");
          t = scan(&s);
          assert(t.type == LITERAL && is_type(&t.data, TYPE_INT) &&
                 long_val(&t.data) == -273)
            });
      it("reads float number", {
          s = s_("420.69");
          t = scan(&s);
          assert(t.type == LITERAL && is_type(&t.data, TYPE_DOUBLE) &&
                 double_val(&t.data) == 420.69);
        });

      it("reads negative float", {
          s = s_("-3.1415666");
          t = scan(&s);
          assert(t.type == LITERAL && is_type(&t.data, TYPE_DOUBLE) &&
                 double_val(&t.data) == -3.1415666);
        });
    });
}

#endif
