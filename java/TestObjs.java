public class TestObjs {
    int field;
    TestObjs hello_me_again;
    float number; 

    TestObjs(int v) {
        this.field = v;
    }

    public static int importmebb(int a) {
        var hello = new TestObjs(1);
        return readmyfield(a, hello);
    }

    public static int readmyfield(int a, TestObjs hello) {
        return a + hello.field;
    }

    public static boolean sometests() {
        if (importmebb(1) != 2) return false;
        if (new TestObjs(2).addFieldFinal(3) != 5) return false;
        if (new InheritedTestObjs(2).addFieldFinal(3) != 5) return false;
        return true;
    }

    // final methods can't be inherited so don't need dynamic dispatch 
    public final int addFieldFinal(int a) {
        return a + this.field;
    }

    static class InheritedTestObjs extends TestObjs {
        // int field2;
        InheritedTestObjs(int v) {
            super(v);
            // this.field2 = v * 2;
        }
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
