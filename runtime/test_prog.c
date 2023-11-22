#include <stdio.h>
#include <assert.h>

int add(int a, int b);
int math(int a, int b, int c);
int add_mut(int a, int b);
int max(int a, int b);
int ifzero(int a);

int main() {
    assert(add(1, 2) == 3);
    assert(add(5, -2) == 3);
    assert(math(3, 2, 1) == 6);
    assert(add_mut(1, 2) == 3);
    assert(add_mut(5, -2) == 3);
    assert(max(1, 2) == 2);
    assert(max(5, -2) == 5);
    assert(ifzero(0) == 1);
    assert(ifzero(2) == 2);
}
