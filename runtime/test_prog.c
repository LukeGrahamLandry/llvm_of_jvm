#include <stdio.h>
#include <assert.h>

int add(int a, int b);
int math(int a, int b, int c);
int add_mut(int a, int b);
int max(int a, int b);
int ifzero(int a);
int minus_ten_while(int a);
int minus_ten_for(int a);
int add3(int a, int b, int c);
int short_circuit_or(int a, int b);
int short_circuit_and(int a, int b);

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
    assert(minus_ten_while(10) == 0);
    assert(minus_ten_while(25) == 15);
    assert(minus_ten_while(-5) == 0);
    assert(minus_ten_for(10) == 0);
    assert(minus_ten_for(25) == 15);
    assert(minus_ten_for(-5) == 0);
    assert(add3(1, 2, 3) == 6);
    assert(add3(1, 2, -3) == 0);
    assert(short_circuit_or(0, 9) == 0);
    assert(short_circuit_or(5, 5) == 5);
    assert(short_circuit_or(6, 5) == 5);
    assert(short_circuit_and(1, 2) == 1);
    assert(short_circuit_and(1, 10) == 10);
    assert(short_circuit_and(10, 2) == 2);
}
