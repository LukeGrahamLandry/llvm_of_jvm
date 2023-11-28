public class TestObjs {
    int field;
    TestObjs hello_me_again;
    float number; 

    public static int importmebb(int a) {
        var hello = new TestObjs();
        hello.field = 1;
        return readmyfield(a, hello);
    }

    public static int readmyfield(int a, TestObjs hello) {
        return a + hello.field;
    }

    // Not referenced and this class isn't a root so won't even try to compile these. 
    public static native void thisdoesntexist();
    public static void luckyimnotanentrypoint() {
        thisdoesntexist();
        var nocompile = new Thread(() -> {
            System.out.println("You'll never see this.");
        });
        nocompile.start();
    }
}
