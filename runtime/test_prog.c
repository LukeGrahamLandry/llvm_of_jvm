#include <stdio.h>
#include <assert.h>

int add(int a, int b);
int math(int a, int b, int c);

int main() {
    assert(add(1, 2) == 3);
    assert(add(5, -2) == 3);
    assert(math(3, 2, 1) == 6);
}
