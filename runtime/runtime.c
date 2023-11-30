#include "array.c"

int check_instanceof(void* current_vptr, void* target_vptr) {
    printf("vptr%p instanceof vptr%p\n", current_vptr, target_vptr);
    if (current_vptr == target_vptr) return 1;
    if (current_vptr == NULL) return 0;
    void* super_vptr = *((void**) current_vptr);
    return check_instanceof(super_vptr, target_vptr);
}
