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
    assert(0);
}

// TODO: redundant and will need to throw an exception
void assert_instanceof(Vtable* current_vptr, Vtable* target_vptr) {
    if (!check_instanceof(current_vptr, target_vptr)) {
        printf("Attempted Invalid Cast\n");
        exit(1);
    }
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

void slow_log_jstr(Array_u16* s) {
    NULL_CHK(s); DEBUG_CHK(s, CHAR_TYPE_ID);
    for (int i=0;i<s->h.length;i++) {
        printf("%c", s->data[i]);
    }
    printf("\n");
}

void TestObjs_slow_log_jstr(Array_u16* arr) {
    slow_log_jstr(arr);
}

void java_lang_System_arraycopy(objptr src, i32 srcPos, objptr dest, i32 destPos, i32 length) {
    printf("TODO: implement java_lang_system_arraycopy!\n");
    exit(1);
}



StringObj* fill_string_const(StringConst strings[], i32 index, i64 strObjSize, i64 strConstSize, i64 stringValueOffset) {
    assert(index >= 0);
    assert(strConstSize == sizeof(StringConst));
    assert(strObjSize == sizeof(StringObj));
    StringObj test;
    i64 offset = ((char*) &test.value) - ((char*) &test);
    assert(offset == stringValueOffset);

    if (strings[index].obj == NULL) {
        printf("init string constant %d len=%d: %s\n", index, strings[index].length, strings[index].bytes);
        assert(strings[index].bytes[strings[index].length] == 0);  // Null terminated. dont rely on this but can use it as a sanity check that we got a good pointer
        i32 len = strings[index].length;
        Array_u16* arr = array_init_u16(len);
        for (i32 i=0;i<len;i++) {
            u16 c = strings[index].bytes[i];
            arr->data[i] = c;
        }
        StringObj* obj = malloc(sizeof(StringObj));
        obj->value = arr;
        obj->hash = 0;  // Calculated on first use. 
        strings[index].obj = obj;
    } else {
        printf("use string constant %d: %s\n", index, strings[index].bytes);
    }
    slow_log_jstr(strings[index].obj->value);
    return strings[index].obj;
}

Array_u16* concat_strings(Array_u16* strings[], i32 count) {
    printf("concat_strings %d\n", count);
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
    slow_log_jstr(value);
    return value;
}
