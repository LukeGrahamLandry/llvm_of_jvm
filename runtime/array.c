#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include "obj.h"

typedef struct AnyArray {
    i32 length;
} AnyArray;

i32 array_length(AnyArray* arr) { 
    assert(arr != NULL);                 
    return arr->length;                  
}   

// hehe template goes brrr.
#define generic_array(ty) \
    /* impl !Sized for Array_##ty {} */ \
    typedef struct Array_##ty { \
        AnyArray h;          \
        ty data[];              \
    } Array_##ty;               \
    \
    Array_##ty* array_init_##ty (i32 length) {                      \
        assert(length >= 0);                                        \
        size_t bytes = sizeof(Array_##ty) + (length * sizeof(ty));  \
        Array_##ty* arr = malloc(bytes);                            \
        memset(arr->data, 0, length * sizeof(ty)); /* TODO: calloc*/\
        arr->h.length = length;                                     \
        return arr;                                                 \
    }                                                               \
    \
    void array_free_##ty(Array_##ty* arr) { \
        free(arr);                          \
    }                                       \
    \
    ty array_get_##ty (Array_##ty* arr, i32 i) {            \
        assert(arr != NULL && i < arr->h.length && i >= 0); \
        return arr->data[i];                                \
    }                                                       \
    \
    void array_set_##ty (Array_##ty* arr, i32 i, ty v) {   \
        assert(arr != NULL && i < arr->h.length && i >= 0);\
        arr->data[i] = v;                                  \
    }                                                      \


generic_array(i8)
generic_array(i16)
generic_array(u16)
generic_array(i32)
generic_array(i64)
generic_array(f32)
generic_array(f64)
generic_array(objptr)
