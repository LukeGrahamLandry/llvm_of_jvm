#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include "obj.h"
#include <stdio.h>

#define DEBUG_CANARY 31415

// These could be functions but then I have to figure out how to turn off -Wincompatible-pointer-types because "source: trust be bro"
#define NULL_CHK(ptr_) assert(ptr_ != NULL);
#define DEBUG_CHK(arr_, ty_) \
    assert(arr_->h.debug_canary == DEBUG_CANARY); \
    assert(arr_->h.debug_type == ty_);


typedef struct AnyArray {
    ObjHeader header; // TODO: fill this in. Arrays extend Objetct and implement no interfaces. 
    i32 length; 
    // These are for debugging my compiler. Can be removed later since I trust javac to only emit well typed classfiles. 
    i16 debug_canary;
    i8 debug_type;  
} AnyArray;

i32 array_length(AnyArray* arr) { 
    assert(arr != NULL);
    assert(arr->debug_canary == DEBUG_CANARY);
    return arr->length;                  
}

// hehe template goes brrr.
#define generic_array(ty, dbg_type) \
    /* impl !Sized for Array_##ty {} */ \
    typedef struct Array_##ty { \
        AnyArray h;             \
        ty data[];              \
    } Array_##ty;               \
    \
    Array_##ty* array_init_##ty (i32 length) {                      \
        assert(length >= 0);                                        \
        size_t bytes = sizeof(Array_##ty) + (length * sizeof(ty));  \
        Array_##ty* arr = malloc(bytes);                            \
        memset(arr->data, 0, length * sizeof(ty)); /* TODO: calloc*/\
        arr->h.length = length;                                     \
        arr->h.debug_canary = DEBUG_CANARY;                         \
        arr->h.debug_type = dbg_type;                               \
        return arr;                                                 \
    }                                                               \
    \
    void array_free_##ty(Array_##ty* arr) {     \
        NULL_CHK(arr); DEBUG_CHK(arr, dbg_type);\
        assert(arr->h.debug_type == dbg_type);  \
        free(arr);                              \
    }                                           \
    \
    ty array_get_##ty (Array_##ty* arr, i32 i) {            \
        NULL_CHK(arr); DEBUG_CHK(arr, dbg_type);            \
        assert(i < arr->h.length && i >= 0);                \
        return arr->data[i];                                \
    }                                                       \
    \
    void array_set_##ty (Array_##ty* arr, i32 i, ty v) {   \
        NULL_CHK(arr); DEBUG_CHK(arr, dbg_type);           \
        assert(i < arr->h.length && i >= 0);               \
        arr->data[i] = v;                                  \
    }                                                      \
    \
    /* TODO: needing to call this once for each layer instead of just doing it as you recurse down is dumb. take a list of lengths */\
    /* its silly that i ordered these backwards because of stack order in the compiler */\
    Array_objptr* array_fillmulti_##ty(i32 inner_len, Array_objptr* outer, i32 depth) { \
        NULL_CHK(outer); DEBUG_CHK(outer, OBJ_TYPE_ID);                                 \
        assert(inner_len >= 0);                                                         \
        if (depth > 0) {                                                                \
            for (i32 i=0;i<outer->h.length;i++) {                                       \
                Array_objptr* arr = (Array_objptr*) outer->data[i];                     \
                array_fillmulti_##ty(inner_len, arr, depth - 1);                        \
            }                                                                           \
            return outer;                                                               \
        }                                                                               \
        for (i32 i=0;i<outer->h.length;i++) {                                           \
            outer->data[i] = (objptr) array_init_##ty(inner_len);                       \
        }                                                                               \
        return outer;                                                                   \
    }                                                                                   \

#define OBJ_TYPE_ID 1

generic_array(objptr, OBJ_TYPE_ID)
generic_array(i8, 2)
generic_array(i16, 3)
generic_array(u16, 4)
generic_array(i32, 5)
generic_array(i64, 6)
generic_array(f32, 7)
generic_array(f64, 8)


void fill_string_const(Array_u16* arr, char* bytes) {
    i32 len = array_length((AnyArray*) arr);
    for (i32 i=0;i<len;i++) {
        u16 c = bytes[i];
        array_set_u16(arr, i, c);
    }
}

void log_throw(objptr throwable) {
    printf("Unhandled Exception: object@%p\n", throwable);
    exit(1);
}

// TODO: fragile because if you dont seem to call a native method it emits an empty body for you so cant define 
// TODO: do this properly 
char* java_lang_Throwable_fillInStackTrace(char* obj, i32 dummy) {
    printf("Called java_lang_Throwable_fillInStackTrace1 (this is very very bad)\n");
    return obj;
}
// char* java_lang_Throwable_fillInStackTrace1(char* obj, i32 dummy) {
//     printf("Called java_lang_Throwable_fillInStackTrace1 (this is very very bad)\n");
//     return obj;
// }
// TODO: i highly distrust my name mangling
char* java_lang_Throwable_fillInStackTrace2(char* obj, i32 dummy) {
    printf("Called java_lang_Throwable_fillInStackTrace1 (this is very very bad)\n");
    return obj;
}

void java_lang_Object_registerNatives(){

}

void java_lang_System_arraycopy(objptr src, i32 srcPos, objptr dest, i32 destPos, i32 length) {
    printf("TODO: implement java_lang_system_arraycopy!\n"); 
    exit(1);
}

#undef generic_array
#undef OBJ_TYPE_ID
#undef NULL_CHK
#undef DEBUG_CANARY
#undef DEBUG_CHK
