public class OpTest {
    public static int add(int a, int b) {
        return a + b;
    }

    public static int math(int a, int b, int c) {
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
            b += 1;
        }
        return b;
    }

    public static int add3(int a, int b, int c) {
        return add(add(a, b), c);
    }

    public static int short_circuit_or(int a, int b) {
        if (a == 0 || a == b) return a;
        else return b;
    }

    public static int short_circuit_and(int a, int b) {
        if (a == 1 && b == 2) return a;
        else return b;
    }

    public static float fadd(float a, float b) {
        return a + b;
    }

    public static float fmath(float a, float b, float c) {
        float d = a + b;
        d += -c;
        d *= a;
        return d / b;
    }

    // public static int fcmp_ternary(float a, float b) {
    //     int c = 0;
    //     c += a == b ? 1 : 0;
    //     c += a < b ? -1 : 0;
    //     c += a > b ? 1 : 0;
    //     c += a <= b ? -1 : 0;
    //     c += a >= b ? 1 : 0;
    //     return c;
    // }

    public static float ffmax(float a, float b) {
        if (a > b) {
            return a;
        } else {
            return b;
        }
    }

    public static int fcmp_normal(float a, float b) {
        int c = 0;
        if (a == b) c += 1;
        if (a < b) c -= 1;
        if (a > b) c += 1;
        if (a <= b) c -= 1;
        if (a >= b) c += 1;
        return c;
    }

    public static int just_fcmp(float a, float b) {
        if (a == b) return 0;
        if (a > b) return 1;
        return -1;
    }

    public static long longs(long a, long b) {
        long c = a + b;
        c *= 2;
        if (c < 0) return 0;
        return c;
    }

    public static double doubles(double a, double b) {
        double c = -(a + b);
        c *= 2;
        if (c < 0) return 0;
        return c;
    }

    public static int modulo(int a, int b) {
        return a % b;
    }
}
