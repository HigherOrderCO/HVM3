#include <stdio.h>
#include <stdint.h>
#include <time.h>

uint32_t count(uint32_t p, uint32_t k) {
    if (p == 0) {
        return k;
    }
    return count(p - 1, k + 1);
}

int main() {
    uint32_t result;
    clock_t start, end;
    double cpu_time_used;
    
    start = clock();
    
    result = count(2000000000, 0);
    
    end = clock();
    cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
    
    printf("Result: %u\n", result);
    printf("Time taken: %f seconds\n", cpu_time_used);
    
    return 0;
}
