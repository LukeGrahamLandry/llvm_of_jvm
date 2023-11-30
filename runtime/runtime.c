#include "array.c"


typedef struct Vtable Vtable;
typedef struct InterfaceNode InterfaceNode;

struct Vtable {
    Vtable* super; // First slot in root vtable is pointer to super vtable
    InterfaceNode* interfaces;
    void* func_ptrs[];  // Unsized
};

struct InterfaceNode {
    InterfaceNode* next_interface;
    Vtable* current_interface_vptr;
    Vtable* root_interface_vptr;
};


int check_instanceof(Vtable* current_vptr, Vtable* target_vptr) {
    printf("vptr%p instanceof vptr%p\n", current_vptr, target_vptr);
    if (current_vptr == target_vptr) return 1;
    if (current_vptr == NULL) return 0;
    void* super_vptr = current_vptr->super;
    return check_instanceof(super_vptr, target_vptr);
}

Vtable* resolve_interface_vtable(Vtable* current_obj_vptr, Vtable* root_target_interface_vptr) {
    printf("class vptr%p call interface vptr%p\n", current_obj_vptr, root_target_interface_vptr);
    
    InterfaceNode* interfaces = current_obj_vptr->interfaces;

    if (((size_t) interfaces) < 1000) {
        printf("debug %ld\n", (size_t) interfaces);
        assert(0);
    }

    while (interfaces != NULL) {
        printf("check vptr%p\n", interfaces);
        if (interfaces->root_interface_vptr == root_target_interface_vptr) {
            return interfaces->current_interface_vptr;
        }
        interfaces = interfaces->next_interface;
    }
    
    printf("Interface vtable not found\n");
    assert(0);
}
