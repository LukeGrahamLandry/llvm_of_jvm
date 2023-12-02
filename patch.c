// UNUSED
///////
// gcc -O2 patch.c -o out/patch && ./out/patch

#include "stdio.h"
#include "assert.h"
#include "stdlib.h"
#include "inttypes.h"

u_int32_t readint(u_int8_t* bytes, u_int32_t addr) {
    assert(addr % 4 == 0);
    u_int8_t a = bytes[addr];
    u_int8_t b = bytes[addr+1];
    u_int8_t c = bytes[addr+2];
    u_int8_t d = bytes[addr+3];
    return d | (c << 8) | (b << 16) | (a << 24);
}

void writeint(u_int8_t* bytes, u_int32_t addr, u_int32_t v) {
    assert(addr % 4 == 0);
    u_int8_t a = bytes[addr];
    u_int8_t b = bytes[addr+1];
    u_int8_t c = bytes[addr+2];
    u_int8_t d = bytes[addr+3];
    printf("Old: addr=%08X:  %02X %02X %02X %02X \n", addr, a, b, c, d);
    a = (v >> 24) & ((1 << 8) - 1);
    b = (v >> 16) & ((1 << 8) - 1);
    c = (v >> 8) & ((1 << 8) - 1);
    d = (v >> 0) & ((1 << 8) - 1);
    printf("New: addr=%08X:  %02X %02X %02X %02X \n", addr, a, b, c, d);
    a = bytes[addr] = a;
    b = bytes[addr+1] = b;
    c = bytes[addr+2] = c;
    d = bytes[addr+3] = d;
}

int main() {
    FILE* exe = fopen("_build/default/bin/main.exe", "rb");
    fseek(exe, 0, SEEK_END);
    size_t len = ftell(exe);
    rewind(exe);

    u_int8_t* bytes = malloc(len);
    size_t found = fread(bytes, 1, len, exe);
    assert(found == len);
    fclose(exe);

    // 10005d1b4: af fd ff 97 	bl	0x10005c870 <_camlZip.unixtime_of_dostime_313>
    u_int64_t addr = strtol("0005d1b4", NULL, 16);
    assert(readint(bytes, addr) == strtol("affdff97", NULL, 16));

    // // Make sure next line reads the return value as expected
    // // 10005d1b8: ec 3b 40 f9 	ldr	x12, [sp, #112]
    // assert(readint(bytes, strtol("0005d1b8", NULL, 16)) == strtol("ec3b40f9", NULL, 16));

    // 00 00 80 d2 	mov	x0, #0
    u_int32_t mov_x0_imm0 = strtol("000080d2", NULL, 16);

    // 1f 20 03 d5 	nop
    u_int32_t nop = strtol("1f2003d5", NULL, 16);

    // 0c 00 80 d2 	mov	x12, #0
    // u_int32_t mov_x12_imm0 = strtol("0c0080d2", NULL, 16);

    writeint(bytes, addr, nop);
    // writeint(bytes, strtol("0005d1b8", NULL, 16), mov_x12_imm0);

    exe = fopen("out/main2.exe", "wb");
    fwrite(bytes, 1, len, exe);
    fclose(exe);

    printf("Hello World! %u\n", readint(bytes, addr));
    return 0;
}