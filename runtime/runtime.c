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

void fill_string_const(Array_u16* arr, char* bytes) {
    i32 len = array_length((AnyArray*) arr);
    for (i32 i=0;i<len;i++) {
        u16 c = bytes[i];
        array_set_u16(arr, i, c);
    }
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
