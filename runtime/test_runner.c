#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

void run(char* cmd) {
    assert(!system(cmd));
}

int main() {
    run("javac java/*.java");
    run("dune exec llvm_of_jvm add > out/test.ll");
    run("gcc runtime/test_prog.c out/test.ll -o out/testbin");
    run("./out/testbin");
}
