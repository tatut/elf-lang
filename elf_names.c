/**
 * Elf name internment.
 * All names of methods and types are interned by the scanner.
 * We hold a hashtable here.
 *
 * A ElfName struct holds the precalculated hash and the pointer to the string data.
 * Callers of intern can never own the returned name and must not modify it.
 *
 */
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "elf_names.h"

// hash from https://stackoverflow.com/a/69812981
#define SEED     0x12345678
uint32_t hash(char* str) { // MurmurOAAT_32
  uint32_t h = SEED;
  for (; *str; ++str) {
    h ^= *str;
    h *= 0x5bd1e995;
    h ^= h >> 15;
  }
  return h;
}

// linked entry for when names collide in buckets
typedef struct ElfNameEntry {
  ElfName *name;
  struct ElfNameEntry *next;
} ElfNameEntry;

// PENDING: ideally rehash when adding names, now just statically allocated buckets
#define NBUCKETS 2048

ElfNameEntry **elf_name_buckets = NULL;

ElfNameEntry *new_name_entry(char *name, uint32_t h) {
  size_t len = strlen(name);
  ElfNameEntry *e = malloc(sizeof(ElfNameEntry));
  e->name = malloc(sizeof(ElfName));
  e->name->hash = h;
  e->name->name = malloc(sizeof(char)*(len+1));
  strcpy(e->name->name, name);
  e->next = NULL;
  return e;
}

const ElfName *intern(char* name) {
  if(elf_name_buckets == NULL) {
    elf_name_buckets = calloc(sizeof(ElfNameEntry), NBUCKETS);
  }
  const uint32_t h = hash(name);
  size_t b = h % NBUCKETS;
  ElfNameEntry *first = elf_name_buckets[b];
  if(first == NULL) {
    // first time encountering this, whole bucket empty
    elf_name_buckets[b] = new_name_entry(name, h);
    return elf_name_buckets[b]->name;
  } else {
    // chase down the collided names and find the given
    ElfNameEntry *at = first, *prev;
    while(at != NULL) {
      // if this is our name, return it
      if(at->name->hash == h && strcmp(at->name->name, name)==0) {
        return at->name;
      }
      prev = at;
      at = at->next;
    }
    // not found, add new entry to prev
    prev->next = new_name_entry(name, h);
    return prev->next->name;
  }
}

#ifdef TEST

#include "test.h"

void test() {

  testing("Existing name", {
      it("returns the same name for same input", {
          assert(intern("foo") == intern("foo"));
        });

      it("returns different name for different inputs", {
          assert(intern("foo") != intern("bar"));
        });

    });

}

#endif
