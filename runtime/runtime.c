#include "array.c"

int check_instanceof(Vtable* current_vptr, Vtable* target_vptr) {
    if (current_vptr == target_vptr) return 1;
    if (current_vptr == NULL) return 0;
    return check_instanceof(current_vptr->super, target_vptr);
}

Vtable* resolve_interface_vtable(Vtable* current_obj_vptr, Vtable* root_target_interface_vptr) {
    InterfaceNode* interfaces = current_obj_vptr->interfaces;

    while (interfaces != NULL) {
        if (interfaces->root_interface_vptr == root_target_interface_vptr) {
            return interfaces->current_interface_vptr;
        }
        interfaces = interfaces->next_interface;
    }
    
    printf("UNREACHABLE: Interface vtable not found\n");
    exit(1);
}

// TODO: redundant and will need to throw an exception
void assert_instanceof(Vtable* current_vptr, Vtable* target_vptr) {
    if (!check_instanceof(current_vptr, target_vptr)) {
        printf("Attempted Invalid Cast\n");
        exit(1);
    }
}

// sometimes null is allowed but want to guard against thinking i have an object but i forgot to set the vptr.
void assert_has_vptr_or_null(objptr obj) {
    assert(obj == NULL || obj->h.vptr != NULL);
}

typedef struct StringObj StringObj;
typedef struct StringConst StringConst;

// This must be kept in sync with the compiler.
struct StringObj {
    ObjHeader header;
    Array_u16* value;
    i32 hash;
};

// This must be kept in sync with the compiler.
struct StringConst {
    const char* bytes;
    StringObj* obj;
    i32 length;
};

// TODO: since this relies on layout of the string class (java 8) its not going to work on java 17
// TODO: last 4 arguments could be in a constant struct passed as one or in a named global. 
StringObj* fill_string_const(StringConst strings[], i32 index, i64 strObjSize, i64 strConstSize, i64 stringValueOffset, Vtable* vptr) {
    assert(index >= 0);
    assert(strConstSize == sizeof(StringConst));
    assert(strObjSize == sizeof(StringObj));
    StringObj test;
    i64 offset = ((char*) &test.value) - ((char*) &test);
    assert(offset == stringValueOffset);

    if (strings[index].obj == NULL) {  // first time evaluating this constant
        assert(strings[index].bytes[strings[index].length] == 0);  // Null terminated. dont rely on this but can use it as a sanity check that we got a good pointer
        i32 len = strings[index].length;
        Array_u16* arr = array_init_u16(len);
        for (i32 i=0;i<len;i++) {
            u16 c = strings[index].bytes[i];
            arr->data[i] = c;
        }
        StringObj* obj = malloc(sizeof(StringObj));  // Note: this needs to never get garbage collected. 
        obj->value = arr;
        obj->hash = 0;  // Calculated on first use. 
        obj->header.vptr = vptr;  // Forgetting this manifests as .equals failing because it thinks constants are not instanceof String 
        strings[index].obj = obj;
    }
    return strings[index].obj;
}

Array_u16* concat_strings(Array_u16* strings[], i32 count) {
    i32 total_len = 0;
    for (int i=0;i<count;i++) {
        NULL_CHK(strings[i]); DEBUG_CHK(strings[i], CHAR_TYPE_ID);
        total_len += strings[i]->h.length;
    }
    
    Array_u16* value = array_init_u16(total_len);
    total_len = 0;
    for (int i=0;i<count;i++) {
        i32 len = strings[i]->h.length;
        u16* rest = value->data + total_len;
        memcpy((char*) rest, (char*) strings[i]->data, len * sizeof(u16));
        total_len += len;
    }
    return value;
}

// TODO: take a toString function pointer so you can log the actual messaage. that gets a bit scary if i dont fix Throwable::<clinit> first
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

// referenced from:  _java_lang_Integer__clinit_ 
objptr java_lang_Class_getPrimitiveClass() {
    printf("TODO: implement java_lang_Class_getPrimitiveClass\n");
    exit(1);
}

void java_lang_Object_registerNatives(){

}
