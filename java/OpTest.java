public class OpTest {
    // int d;
    // public Hello(int e) {
    //     this.d = add(e, 1);
    //     if (e == 10) {
    //         this.d = 77;
    //     }
    // }

    public static int add(int a, int b) {
        return a + b;
    }

    public static int max(int a, int b) {
        if (a > b) {
            return a;
        } else {
            return b;
        }
    }

    public static int redundant(int a) {
        int b = 0;
        if (a == 5) {
            b = 1;
        } else {
            b = 2;
        }
        return b;
    }

    public static int loop(int a) {
        int b = 0;
        while (a < 10) {
            a -= 1;
            b += a;
        }
        return b;
    }
}
