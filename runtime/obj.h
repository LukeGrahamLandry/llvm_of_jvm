#include <inttypes.h>


typedef struct Vtable Vtable;
typedef struct InterfaceNode InterfaceNode;
typedef struct ObjHeader ObjHeader;
typedef struct AnyObject AnyObject;

// Layout must be in kept sync with the compiler! 
struct ObjHeader {
    Vtable* vptr;
};

// Barely used. Just to make it clear that an ObjHeader can't be assigned to an objptr. 
struct AnyObject {
    ObjHeader h;
    char data[];  // unsized
};

// Layout must be in kept sync with the compiler! 
struct Vtable {
    Vtable* super; // First slot in root vtable is pointer to super vtable
    InterfaceNode* interfaces;
    void* func_ptrs[];  // Unsized
};

// Layout must be in kept sync with the compiler! 
struct InterfaceNode {
    InterfaceNode* next_interface;
    Vtable* current_interface_vptr;
    Vtable* root_interface_vptr;
};

typedef AnyObject* objptr;  
typedef int16_t i16;
typedef int8_t i8;
typedef int32_t i32;
typedef int64_t i64;
typedef float f32;
typedef double f64;
typedef uint16_t u16;  // java char
