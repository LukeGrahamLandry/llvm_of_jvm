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
        if (new InheritedTestObjs(2).parentfield() != 2) return false;
        if (new InheritedTestObjs(2).mineownfield() != 4) return false;
        if (new InheritedTestObjs(2).nonfinalbutclassfinal() != 3) return false;
        if (new InheritedTestObjs(2).parentnonfinalbutclassfinal() != 3) return false;
        if (new InheritedTestObjs(2).overrideinfinalchild() != 5) return false;
        if (new AnotherFinalTestObjs(2).overrideinfinalchild() != 4) return false; // didn't override 
        return true;
    }

    // final methods can't be overriden so don't need dynamic dispatch 
    public final int addFieldFinal(int a) {
        return a + this.field;
    }

    // calling this on a TestObjs would need dynamic dispatch 
    // but InheritedTestObjs is final so calling it on one of those can be done statically. 
    int parentnonfinalbutclassfinal() {
        return this.field + 1;
    }

    // same as above but its gonna override also 
    int overrideinfinalchild() {
        return this.field + 2;
    }

    static final class InheritedTestObjs extends TestObjs {
        int field2;
        InheritedTestObjs(int v) {
            super(v);
            this.field2 = v * 2;
        }

        // access inherited field
        final int parentfield() {
            return this.field;
        }

        // access own field of object that inherited other fields 
        final int mineownfield() {
            return this.field2;
        }

        // class is final so this method can't be overriden so don't need dynamic dispatch 
        int nonfinalbutclassfinal() {
            return this.field + 1;
        }

        // same as above but we inherited the method
        int overrideinfinalchild() {
            return super.overrideinfinalchild() + 1;
        }
    }


    static final class AnotherFinalTestObjs extends TestObjs {
        AnotherFinalTestObjs(int v) {
            super(v);
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
