import java.security.AccessController;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

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

    public static int sometests() {
        if (importmebb(1) != 2) return 1;

        // == static dispatch == 
        if (new TestObjs(2).addFieldFinal(3) != 5) return 2;
        if (new InheritedTestObjs(2).addFieldFinal(3) != 5) return 3;
        if (new InheritedTestObjs(2).parentfield() != 2) return 4;
        if (new InheritedTestObjs(2).mineownfield() != 4) return 5;
        if (new InheritedTestObjs(2).nonfinalbutclassfinal() != 3) return 6;
        if (new InheritedTestObjs(2).parentnonfinalbutclassfinal() != 3) return 7;
        if (new InheritedTestObjs(2).overrideinfinalchild() != 5) return 8;
        if (new AnotherFinalTestObjs(2).overrideinfinalchild() != 4) return 9; // didn't override 

        // == dynamic dispatch == 
        if (new TestObjs(2).overrideinfinalchild() != 4) return 10; // didn't override 
        if (new NotFinal(2).overrideinfinalchild() != 4) return 11;
        TestObjs a = new NotFinal(2);
        if (a.overrideinfinalchild() != 4) return 12;
        if (new DoOverride(2).overrideinfinalchild() != 12) return 13;
        TestObjs b = new DoOverride(2);
        if (b.overrideinfinalchild() != 12) return 14;
        // var aaa = Math.fma(1, 2, 3);  // TODO: why no compile?

        if (imoverloaded() != 1) return 15;
        if (imoverloaded(5) != 6) return 16;

        imoverloaded(); // discard (pop opcode)

        if ("hello".length() != 5) return 17;
        if (false) throw new RuntimeException(); 
        assertzero(0);
        if (ternary_max(1, 2) != 2) return 18;
        if (ternary_max(2, 1) != 2) return 19;
        if (ternary_max_harder(1, 2) != 2) return 18;
        if (ternary_max_harder(2, 1) != 2) return 19;
        if (Math.max(5, 10) != 10) return 20;
        var empty = new String(); 
        if (globalinnonentry != 15) return 21;

        // == abstract methods == 
        var aaaa = new B();
        if (new B().overrideonly() != 50) return 22;
        if (new B().overrideonlywitharg(10) != 100) return 23;


        if (temp() != 0) return 24;
        sync();

        var al = new ArrayList();

        // == interfaces == 

        if (new Four().getNum() != 4) return 25;
        if (new Five().getNum() != 5) return 26;

        if (!(new Four() instanceof Four)) return 27;

        GetNum n = new Four();
        if (n.getNum() != 4) return 28;
        n = new Five();
        if (n.getNum() != 5) return 27;

        Five nn = (Five) n;

        n = new Four();
        if (n.callindirect() != 1) return 29;
        n = new Five();
        if (n.callindirect() != 2) return 30;
        new Six();

        WithDefault d = new UseDefault();
        if (d.doop(5) != 10) return 31;
        d = new ChangeDefault();
        if (d.doop(5) != 25) return 32;


        //  void java.lang.SecurityManager.checkPermission(java.security.Permission)
        "hi".charAt(0);
        // if ("hi".charAt(0) !='h') return 31;

        var ul = Collections.unmodifiableList(al);

        var ab = "a" + "b";
        if (!ab.equals("ab")) return 33;
        var ab2 = concat("a", "b");
        if (!ab2.equals("ab")) return 34;

        // oh shit
        // NOT YET IMPLEMENTED: stack_delta invokedynamic makeConcatWithConstants:(Ljava/lang/String;)Ljava/lang/String;\n\t method_ref {\n\t   method-handle : invokestatic, method : java.lang.invoke.CallSite java.lang.invoke.StringConcatFactory::makeConcatWithConstants(java.lang.invoke.MethodHandles$Lookup,java.lang.String,java.lang.invoke.MethodType,java.lang.String,java.lang.Object[])\n\t }\n\t bootstrap_arguments {\n\t   string 'Hello \\001'\n\t 
        // sayhi("bob");

        // NOT YET IMPLEMENTED: stack_delta dupX1
        // var vv = "hi".getBytes();

        // System.out.println("Hello World!");
        return 0;
    }

    static String concat(String a, String b) {
        return a + b;
    }

    static String sayhi(String name) {
        return "Hello " + name;
    }

    static interface GetNum {
        int getNum();

        int callindirect();
    }

    static class Four implements GetNum {
        public int getNum() {
            return 4;
        }

        public int callindirect() {
            return 1;
        }
    }

    static class Five implements GetNum {
        // Just take up a slot in the vtable so its less likely to successfully cheat the interface call
        public int garbage() {
            return 123;
        }

        public int getNum() {
            return 5;
        }

        public int callindirect() {
            return 2;
        }
    }

    // interface methods never called but could be so should be emitted 
    static class Six implements GetNum {
        public int getNum() {
            return 6;
        }

        public int callindirect() {
            return 3;
        }
    }

    static void sync() {
        synchronized(new Object()) {  // TODO: this is just a no-op for now
            
        }
    }

    public static abstract class A {
        abstract int overrideonly();
        abstract int overrideonlywitharg(int a);

        float nocalla() {
            return 1.0f;
        }
    }

    public static class B extends A {
        int overrideonly() {
            return 50;
        }

        int overrideonlywitharg(int a) {
            return a * 10;
        }

        float nocallb() {
            return 1.0f;
        }
    }

    static int globalinnonentry = 15;

    static void assertzero(int a) {
        if (a != 0) throw new RuntimeException();
    }

    static int ternary_max(int a, int b) {
        return a > b ? a : b;
    }

    static int ternary_max_harder(int a, int b) {
        return -((a + 1 + 2 + 3) > (b + 1 + 2 + 3) ? -a : -b);
    }

    static int imoverloaded() {
        return 1;
    }

    static int imoverloaded(int a) {
        return imoverloaded() + a;
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

    static class NotFinal extends TestObjs {
        NotFinal(int v) {
            super(v);
        }
        // no override overrideinfinalchild
        // requires simple vtable inheritance 
    }

    static class DoOverride extends TestObjs { 
        DoOverride(int v) {
            super(v);
        }

        // override and change behaviour. needs vtable inheritance 
        int overrideinfinalchild() {
            return this.field + 10;
        }
    }

    static class C extends B {
    
    }

    static int temp() {
        if (!(new B() instanceof B)) return 24;
        if (!(new B() instanceof A)) return 25;
        if (!(new C() instanceof A)) return 26;

        A c = new C();
        A b = new B();
        if (!(c instanceof B)) return 26;
        if (b instanceof C) return 27;
        if (!(c instanceof B)) return 28;
        if (!(c instanceof Object)) return 29;
        return 0;
    }

    static interface WithDefault {
        default int doop(int a) {
            return a + a;
        }
    } 

    static class UseDefault implements WithDefault {

    }

    static class ChangeDefault implements WithDefault {
        @Override
        public int doop(int a) {
            return a * a;
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

    public void luckyimnotanentrypoint_instance() {
        luckyimnotanentrypoint();
    }
}
