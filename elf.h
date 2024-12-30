#ifndef ELF_H
#define ELF_H

#include <_types/_uint16_t.h>
#include <_types/_uint8_t.h>
#include <stddef.h>

/* Elf values are always of ElfVal struct.
   The type tells what the type of the value is
   4096 first values are reserved for implementation,
   rest are available for user defined types.

   Values that can fit in 64bits, are directly stored in the
   data. Otherwise it points to another struct.
*/

typedef union ElfData {
  long longVal;
  double doubleVal;
  char bytesVal[8];
  void *ptrVal;
} ElfData;

typedef struct ElfVal {
  uint16_t type; // 64k types max
  ElfData data;
} ElfVal;

// sizeof(ElfVal) == 16 bytes

typedef struct ElfCons {
  ElfVal value;
  struct ElfCons *next;
} ElfCons;

typedef struct ElfArray {
  size_t size[4]; // max size of each dimension, upto 4 dimensions, defaults to 1
  ElfVal* data; //
} ElfArray;

// bytestring of >8 bytes
typedef struct ElfBytes {
  size_t length;
  char* data;
} ElfBytes;

#define TYPE_NIL 0 // nil, *data = NULL
#define TYPE_TRUE 1 // true boolean value
#define TYPE_FALSE 2 // false boolean value
#define TYPE_INT 3 // signed 64bit integer, immediate
#define TYPE_DOUBLE 4 // signed 64bit float, immediate
#define TYPE_FLAGS 5  // 64 booleans in a single bitmask, immediate
#define TYPE_LIST 6  // a cons cell, *data is ElfCons
#define TYPE_EMPTY_LIST 7 // marker for empty list, has no data allocated
#define TYPE_ARRAY 8 // array, *data is ElfArray
#define TYPE_BYTES1 50 // bytestring of size 1
#define TYPE_BYTES2 51 // bytestring of size 2
#define TYPE_BYTES3 52 // bytestring of size 3
#define TYPE_BYTES4 53 // bytestring of size 4
#define TYPE_BYTES5 54 // bytestring of size 5
#define TYPE_BYTES6 55 // bytestring of size 6
#define TYPE_BYTES7 56 // bytestring of size 7
#define TYPE_BYTES8 57 // bytestring of size 8
#define TYPE_BYTES 58  // bytestring of arbitrary size (pointer to ElfBytes)

// PENDING: could define fixed point numbers, with 1-6 decimals
// eg. TYPE_FIX1 means signed 64bit number with 1 decimal (eg. -4205 means
// -420.5)


#define ELF_TRUE                                                               \
  (ElfVal) { TYPE_TRUE, {} }
#define ELF_FALSE                                                              \
  (ElfVal) { TYPE_FALSE, {} }
#define ELF_NIL                                                                \
  (ElfVal) { TYPE_NIL, {} }

#define is_nil(v) ((ElfVal*)v)->type == TYPE_NIL
#define is_true(v) ((ElfVal*)v)->type == TYPE_TRUE
#define is_false(v) ((ElfVal*)v)->type == TYPE_FALSE
#define is_type(v, atype) ((ElfVal*)v)->type == atype
#define long_val(v) (((ElfVal *)v)->data.longVal)
#define double_val(v) (((ElfVal *)v)->data.doubleVal)
#define ptr_val(v) (((ElfVal *)v)->data.ptrVal)
#define bytes_val(v) (((ElfVal *)v)->data.bytesVal)

#define set_long_val(v, l) (((ElfVal *)v)->data.longVal = l)
#define set_double_val(v, d) (((ElfVal *)v)->data.doubleVal = d)
#define set_bytes_val(v, b) (((ElfVal *)v)->data.bytesVal = b)
#define set_ptr_val(v, p) (((ElfVal *)v)->data.ptrVal = p)

#define alloc_val() malloc(sizeof(ElfVal))
#define alloc_cons() malloc(sizeof(ElfCons))

#endif
