#include <inttypes.h>

typedef struct ObjHeader {
    // TODO: what goes here? 
} ObjHeader;


// TODO should be ObjHeader* 
//      but need to figure out how to turn off `warning: incompatible pointer types`
//      do i really need to cast it every time? 
typedef void* objptr;  
typedef int16_t i16;
typedef int8_t i8;
typedef int32_t i32;
typedef int64_t i64;
typedef float f32;
typedef double f64;
typedef uint16_t u16;  // java char
