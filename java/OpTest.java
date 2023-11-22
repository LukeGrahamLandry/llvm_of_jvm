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

    public static int math(int a, int b, int c) {
        // 3 2 1 -> 6
        return (((a + b) - c) * a) / b;
    }

    public static int add_mut(int a, int b) {
        a += b;
        return a;
    }

    public static int max(int a, int b) {
        if (a > b) {
            return a;
        } else {
            return b;
        }
    }

    public static int ifzero(int a) {
        if (a == 0) {
            return 1;
        } else {
            return a;
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

    public static int minus_ten_while(int a) {
        int b = 0;
        while (a > 10) {
            a -= 1;
            b += 1;
        }
        return b;
    }

    public static int minus_ten_for(int a) {
        int b = 0;
        for (int c=a; c > 10; c--) {
            a -= 1;
            b += 1;
        }
        return b;
    }
}
