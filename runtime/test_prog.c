#include <stdio.h>
#include <assert.h>
#include "obj.h"

// OpTest.java
int OpTest_add(int a, int b);
int OpTest_math(int a, int b, int c);
int OpTest_add_mut(int a, int b);
int OpTest_max(int a, int b);
int OpTest_ifzero(int a);
int OpTest_minus_ten_while(int a);
int OpTest_minus_ten_for(int a);
int OpTest_add3(int a, int b, int c);
int OpTest_short_circuit_or(int a, int b);
int OpTest_short_circuit_and(int a, int b);
float OpTest_fadd(float a, float b);
float OpTest_fmath(float a, float b, float c);
int OpTest_fcmp_normal(float a, float b);
float OpTest_ffmax(float a, float b);
int OpTest_just_fcmp(float a, float b);
long OpTest_longs(long a, long b);
int OpTest_modulo(int a, int b);
double OpTest_doubles(double a, double b);
int OpTest_cast(float a);
signed char OpTest_bytes(signed char a, signed char b);
signed char OpTest_minzero(signed char a);
int OpTest_cmp_zero(int a);
int OpTest_call_native(int a);
int OpTest_inc_static();
void OpTest_do_nothing();
void OpTest__clinit_();  // Static initializer block
int OpTest_add_array(int a);
int OpTest_mul(int a, int b) {  // Imported native
    return a * b;
}
int OpTest_reuse_local(int a);
int OpTest_arr_of_arr(int a);
int OpTest_nested_arr(int a);
int OpTest_useimported(int a);
int OpTest_objtest();
void TestObjs__clinit_();

void optestjava() {
    assert(OpTest_add(1, 2) == 3);
    assert(OpTest_add(5, -2) == 3);
    assert(OpTest_math(3, 2, 1) == 6);
    assert(OpTest_add_mut(1, 2) == 3);
    assert(OpTest_add_mut(5, -2) == 3);
    assert(OpTest_max(1, 2) == 2);
    assert(OpTest_max(5, -2) == 5);
    assert(OpTest_ifzero(0) == 1);
    assert(OpTest_ifzero(2) == 2);
    assert(OpTest_minus_ten_while(10) == 0);
    assert(OpTest_minus_ten_while(25) == 15);
    assert(OpTest_minus_ten_while(-5) == 0);
    assert(OpTest_minus_ten_for(10) == 0);
    assert(OpTest_minus_ten_for(25) == 15);
    assert(OpTest_minus_ten_for(-5) == 0);
    assert(OpTest_add3(1, 2, 3) == 6);
    assert(OpTest_add3(1, 2, -3) == 0);
    assert(OpTest_short_circuit_or(0, 9) == 0);
    assert(OpTest_short_circuit_or(5, 5) == 5);
    assert(OpTest_short_circuit_or(6, 5) == 5);
    assert(OpTest_short_circuit_and(1, 2) == 1);
    assert(OpTest_short_circuit_and(1, 10) == 10);
    assert(OpTest_short_circuit_and(10, 2) == 2);
    assert(OpTest_fadd(1, 2) == 3);
    assert(OpTest_fadd(1.5, -0.5) == 1.0);
    assert(OpTest_fadd(1.5, 2.5) == 4);
    assert(OpTest_fmath(3, 2, 1) == 6);
    assert(OpTest_ffmax(1, 2) == 2);
    assert(OpTest_ffmax(5, -2) == 5);
    assert(OpTest_just_fcmp(1, 0) == 1);
    assert(OpTest_just_fcmp(1, 2) == -1);
    assert(OpTest_just_fcmp(1, 1) == 0);
    assert(OpTest_fcmp_normal(1, 1) == 1);
    assert(OpTest_fcmp_normal(5.5, 1) == 2);
    assert(OpTest_fcmp_normal(-2.2, 5) == -2);
    assert(OpTest_longs(1, 2) == 6);
    assert(OpTest_longs(1, -2) == 0);
    assert(OpTest_modulo(10, 3) == 1);
    assert(OpTest_modulo(10, 5) == 0);
    assert(OpTest_doubles(1, 2) == 0);
    assert(OpTest_doubles(-1, -2) == 6);
    assert(OpTest_cast(1.5) == 1);
    assert(OpTest_bytes(1, 2) == -6);
    assert(OpTest_bytes(-1, -2) == 6);
    assert(OpTest_cmp_zero(-2) == -1);
    assert(OpTest_cmp_zero(0) == 0);
    assert(OpTest_cmp_zero(2) == 1);
    assert(OpTest_minzero(-5) == 0);
    assert(OpTest_minzero(5) == 5);
    assert(OpTest_call_native(5) == 10);
    OpTest_do_nothing();
    OpTest__clinit_();
    assert(OpTest_inc_static() == 11);
    assert(OpTest_inc_static() == 12);
    assert(OpTest_inc_static() == 13);
    assert(OpTest_add_array(3) == 6);
    assert(OpTest_reuse_local(0) == 0);
    assert(OpTest_arr_of_arr(3) == 66);
    assert(OpTest_nested_arr(2) == (2*3*4*5));
    assert(OpTest_useimported(49) == 50);
    TestObjs__clinit_();

    int t = OpTest_objtest();
    if (t) {
        printf("OpTest_objtest() = %d\n", t);
        assert(0);
    }
    printf("Tests passed!\n");
}

// PrimitiveTemplateTYPE.txt
#define primitive_template(cty, jty) \
    cty PrimitiveTemplate##jty##_add_array(int a);          \
    cty PrimitiveTemplate##jty##_math(cty a, cty b, cty c); \
    \
    void do_primitive_template_ ## jty() { \
        assert(PrimitiveTemplate##jty##_add_array(3) == 6);   \
        assert(PrimitiveTemplate##jty##_add_array(4) == 10);  \
        assert(PrimitiveTemplate##jty##_math(3, 2, 1) == 6);  \
    }                                     \

// IntegerTemplateTYPE.txt TODO
#define integer_template(cty, jty) ;

primitive_template(i32, int)
primitive_template(i64, long)
primitive_template(f32, float)
primitive_template(f64, double)

void templates() {
    do_primitive_template_int();
    do_primitive_template_long();
    do_primitive_template_float();
    do_primitive_template_double();
}

int main() {
    optestjava();
    templates();
}

// TODO: should i move this and java/* to tests/*. kinda confusing for this to be in runtime but not part of the runtime. 


