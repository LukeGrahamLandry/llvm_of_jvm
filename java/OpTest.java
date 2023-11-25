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

    public static int cast(float a) {
        return (int) a;
    }

    public static byte bytes(byte a, byte b) {
        byte c = (byte) -(a + b);
        c *= 2;
        return (byte) c;
    }

    public static int cmp_zero(int a) {
        if (a == 0) return 0;
        if (a > 0) return 1;
        return -1;
    }

    public static byte minzero(byte a) {
        if (a < 0) return 0;
        return a;
    }

    public static native int mul(int a, int b);

    public static int call_native(int a) {
        return mul(a, 2);
    }

    public static void do_nothing() {
        return;
    }

    public static int acc = 4;
    public static int inc_static() {
        acc += 1;
        return acc;
    }
    static {
        acc += 6;
    }

    public static int add_array(int a) {
        int[] nums = new int[a];
        for (int i=0;i<a;i++) {
            nums[i] = i + 1;
        }
        int acc = 0;
        for (int i=0;i<nums.length;i++) {
            acc += nums[i];
        }
        return acc;
    }

    public static int reuse_local(int a) {
        int d = 0;
        if (a < 0) {
            int b = 10;
            if (b == a) return 1;  // The llvm int compare forces it to observe both variables as an int
        } else {
            float c = 10.0f;  // This reuses the stack slot of b and overwrites its type to be float so the comparison above won't compile. 
        }
        return 0;
    }

    public static int arr_of_arr(int a) {
        int[][] nums = new int[a][a+1];
        if (nums.length >= nums[0].length) return -1;
        int i =0;
        for (int[] s : nums) {
            for (int j=0;j<s.length;j++) {
                s[j] = i;
                i++;
            }
        }
        int acc = 0;
        for (int[] s : nums) {
            for (int z : s) {
                acc += z;
            }
        }
        return acc;
    }

    public static int nested_arr(int a) {
        int[][][][] nums = new int[a][a+1][a+2][a+3];
        int i = 0;
        for (var b : nums) {
            for (var c : b) {
                for (var d : c) {
                    for (int e : d) {
                        i++;
                    }
                }
            }
        }
        nums[0][1][2][3] = i;
        return nums[0][1][2][3];
    }
}
