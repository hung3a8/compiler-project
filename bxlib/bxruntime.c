#include <sys/types.h>
#include <stdio.h>

void print_int(long long value) {
  (void) printf("%lld\n", value);
}

void print_bool(long long value) {
  (void) printf("%s\n", value ? "true" : "false");
}
