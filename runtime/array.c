#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <inttypes.h>

typedef void* objptr;
typedef int16_t i16;
typedef int8_t i8;
typedef int32_t i32;
typedef int64_t i64;
typedef float f32;
typedef double f64;
typedef uint16_t u16;  // java char

// hehe template goes brrr.
#define generic_array(ty) \
    /* impl !Sized for Array_##ty {} */ \
    typedef struct Array_##ty { \
        i32 length;             \
        ty data[];              \
    } Array_##ty;               \
    \
    int array_length_##ty(Array_##ty* arr) { \
        assert(arr != NULL);                 \
        return arr->length;                  \
    }                                        \
    \
    Array_##ty * array_init_##ty (i32 length) {                     \
        assert(length >= 0);                                        \
        size_t bytes = sizeof(Array_##ty) + (length * sizeof(ty));  \
        Array_##ty* arr = malloc(bytes);                            \
        memset(arr->data, 0, length * sizeof(ty)); /* TODO: calloc*/\
        arr->length = length;                                       \
        return arr;                                                 \
    }                                                               \
    \
    void array_free_##ty(Array_##ty* arr) { \
        free(arr);                          \
    }                                       \
    \
    ty array_get_##ty (Array_##ty* arr, i32 i) {         \
        assert(arr != NULL && i < arr->length && i > 0); \
        return arr->data[i];                             \
    }                                                    \
    \
    void array_set_##ty (Array_##ty* arr, i32 i, ty v) { \
        assert(arr != NULL && i < arr->length && i > 0); \
        arr->data[i] = v;                                \
    }                                                    \


generic_array(i8)
generic_array(i16)
generic_array(u16)
generic_array(i32)
generic_array(i64)
generic_array(f32)
generic_array(f64)
generic_array(objptr)
