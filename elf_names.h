#include <stdint.h>

typedef struct ElfName {
  uint32_t hash;
  char* name;
} ElfName;

/**
 * Intern the given name. Always returns the same shared
 * ElfName for the same string name.
 * Interned names are never freed.
 */
const ElfName *intern(char *name);
