#include <stddef.h>
#include <stdio.h>

char *current_test;

size_t test_success=0;
size_t test_failure=0;

#define testing(name, body)                                                    \
  printf("Testing: %s\n", name);                                               \
  body

#define it(name, body) \
  printf("  %s", name); \
  body

#define assert(body)                                                           \
  if ((body)) {                                                                \
    printf(" [OK]\n");                                                         \
    test_success++;                                                            \
  } else {                                                                     \
    printf(" [FAIL]\n");                                                       \
    test_failure++;                                                            \
  }


// must be declared by file
void test();

void run_tests() {
  test();
  printf("Success: %ld\nFail: %ld\n", test_success, test_failure);
}

int main(int argc, char **argv){
  run_tests();
  if(test_failure==0)
    return 0;
  else
    return 1;
}
