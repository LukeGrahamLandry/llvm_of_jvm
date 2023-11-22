#include <stdio.h>
#include <assert.h>

int add(int a, int b);

int main() {
    assert(add(1, 2) == 3);
    assert(add(5, -2) == 3);
}
