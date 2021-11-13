#include <ctype.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define SIZE 1 << 20

int main() {
    char* buf = (char*) malloc(SIZE);
    int64_t sum = 0;
    while(fgets(buf, SIZE, stdin)) {
        for(char* ptr = buf; *ptr; isdigit(*ptr)
                || ((*ptr == '-' || *ptr == '+') && isdigit(*(ptr + 1)))
                ? sum += strtoll(ptr, &ptr, 10)
                : (int64_t) ++ptr) {}
    }
    printf("%lld\n", sum);
    free(buf);
    return 0;
}
