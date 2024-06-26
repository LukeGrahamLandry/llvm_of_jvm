// Alas, java classes cannot be generic over primitive types. HACK: 
// A new version of this file will be generated for each of [byte, short, char, int, float, double].
// The string TYPE will be replaced by one of the primitive types. 
// This allows testing operations that should work on all primitives without writing the same code repeatedly. 

public class PrimitiveTemplateTYPE {
    public static TYPE math(TYPE a, TYPE b, TYPE c) {
        TYPE d = (TYPE) (a + b);
        d += -c;
        d *= a;
        return (TYPE) (d / b);
    }

    public static TYPE add_array(int a) {
        TYPE[] nums = new TYPE[a];
        for (int i=0;i<nums.length;i++) {
            nums[i] = (TYPE) (i + 1);
        }

        TYPE acc = 0;
        
        for (int i=0;i<nums.length;i++) {
            acc += nums[i];
        }
        
        return acc;
    }   
}
