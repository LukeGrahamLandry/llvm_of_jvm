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
float fadd(float a, float b);
float fmath(float a, float b, float c);
int fcmp_normal(float a, float b);
float ffmax(float a, float b);
int just_fcmp(float a, float b);
long longs(long a, long b);

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
    assert(fadd(1, 2) == 3);
    assert(fadd(1.5, -0.5) == 1.0);
    assert(fadd(1.5, 2.5) == 4);
    assert(fmath(3, 2, 1) == 6);
    assert(ffmax(1, 2) == 2);
    assert(ffmax(5, -2) == 5);
    assert(just_fcmp(1, 0) == 1);
    assert(just_fcmp(1, 2) == -1);
    assert(just_fcmp(1, 1) == 0);
    assert(fcmp_normal(1, 1) == 1);
    assert(fcmp_normal(5.5, 1) == 2);
    assert(fcmp_normal(-2.2, 5) == -2);
    assert(longs(1, 2) == 6);
    assert(longs(1, -2) == 0);
}
