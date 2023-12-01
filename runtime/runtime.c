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
