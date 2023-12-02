// Every time it unzips a jar file it converts the last modified time to a unix timestamp 
// which is insanely slow somehow (~50% of my programs time). 
// https://github.com/xavierleroy/camlzip/blob/master/zip.ml#L94
// Since I don't care about the time, ask the linker to replace that call with this. 
// A bit unplesent because if I ever actually want to call this function I need to remember that I did this. 
// But so little effort for immeidate 2x speed up is hard to resist. 
int mktime(void* timeptr) {
    return 0;
}
