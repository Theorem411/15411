#include <stdio.h>
#include <stdlib.h>
int main() {
    int **x = (int**)malloc(sizeof(int*));
    *x = (int*)malloc(sizeof(int));
    **x = 0;
    return **x;
}