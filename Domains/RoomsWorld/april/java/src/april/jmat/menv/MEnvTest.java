package april.jmat.menv;

import java.util.*;
import april.jmat.*;

public class MEnvTest
{
    MEnv menv = new MEnv();

    public static void main(String args[])
    {
        MEnvTest t = new MEnvTest();

        t.run();
    }

    void test(String expr)
    {
        menv.evaluate(expr);
    }

    void test(String expr, String stest)
    {
        String s = TypeUtil.toString(menv.evaluate(expr));
        assert(stest.equals(s));
    }

    void test(String expr, Object o)
    {
        Matrix m = TypeUtil.toMatrix(menv.evaluate(expr));
    }

    void test(String expr, Matrix mtest)
    {
        Matrix m = TypeUtil.toMatrix(menv.evaluate(expr));
        assert(TypeUtil.matrixEquals(m, mtest));
    }

    void test(String expr, double v)
    {
        Matrix m = TypeUtil.toMatrix(menv.evaluate(expr));
        assert(m.getRowDimension()==1 && m.getColumnDimension()==1);
        assert(v == m.get(0,0));
    }

    void testShouldThrow(String expr)
    {
        try {
            menv.evaluate(expr);
        } catch (MEnvRuntimeException ex) {
            return;
        } catch (RuntimeException ex) {
            return;
        }

        System.out.println("FAIL: "+expr+" did not throw");
        assert(false);
    }

    void run()
    {
        test("5", 5);
        test("X = 5", 5);
        test("X", 5);
        test("-3", -3);
        test("X = -7");
        test("X == -7", 1);
        test("5+-3", 2);
        test("4*2", 8);
        test("2*4", 8);
        test("-2*4", -8);
        test("-2*-4", 8);
        test("-2 * -4 + 5", 13);
        test("2*-4 + 5", -3);
        test("5*-1*(6+3)", -45);
        test("4/2*2", 4);
        test("1+3/4*0", 1);
        test("5^3", 125);
        test("1*2+3-4*5+6*7-8^2", -37);
        test("1*2+3-4*5+(6*7-8)^2", 1141);
        test("1*2+(3-4*5)+(6*7-8)^2", 1141);
        test("5*(2+(3-4*5))+(6*7-8)^2", 1081);
        test("5'", 5);
        test("5'''", 5);
        test("X=-(- 5)", 5);
        test("X=5");
        test("X==4", 0);
        test("X--");
        test("X==4", 1);
        test("X=(2)", 2);
        test("{ X=4; X = X * 2 }");
        test("X", 8);
        test("X = 3; X = X * 3 + 2;", 11);

        test("3+7", 10);
        test("3-7", -4);
        test("3*7", 21);
        test("10/2", 5);
        test("3^2", 9);
        test("0==1", 0);
        test("1==1", 1);
        test("0!=1", 1);
        test("0<1", 1);
        test("1>0", 1);
        test("0>=1", 0);
        test("0<=1", 1);
        test("1 && 0", 0);
        test("1 || 0", 1);
        test("!(1==0)", 1);

        test("function fibb(n) { if (n < 2) return 1; else return fibb(n-1) + fibb(n-2) }");
        test("fibb(-4)", 1);
        test("fibb(2)", 2);
        test("fibb(7)", 21);

        test("trace(eye(5))", 5);

        test("A=[1 2];");
        test("B=[A;A];");
        test("trace(B'*B)", 10);

        test("a=0; b=71; for (i=0;i<b;i++) a++; a;", 71);

        test("2^2^6", 4096); // verified with octave

        test("a=0;                      "+
             "for (i=0;i<10;i++) {      "+
             " for (j=0;j<i;j++) {      "+
             "   if (j>5)               "+
             "     break;               "+
             "   a++;                   "+
             "  }                       "+
             "}                         "+
             "a                         ",
             39);

        test("a=0;                      "+
             "for (i=0;i<10;i++) {      "+
             " for (j=0;j<i;j++) {      "+
             "   if (j>5)               "+
             "     continue;            "+
             "   a++;                   "+
             "  }                       "+
             "}                         "+
             "a                         ",
             39);

        test("A=0; B=A++", 0);
        test("A=0; B=A++; A", 1);

        test("A=0; B=++A", 1);
        test("A=0; B=++A+2; B", 3);
        test("2+ +4", 6);
        test("sum=1; sum*=4", 4);
        test("a=7; sum=2; sum+=++a", 10);

        testShouldThrow("3/0");

        test("A=eye(3)");
        testShouldThrow("A(5,5)=2");
        test("B=eye(5)");
        testShouldThrow("C=A*B");
        testShouldThrow("C=A+B");
        test("trace(A*2)==6", 1);

        test("A=\"abc\"", "abc");
        test("A+[1 1 1]", "bcd");
        test("B=A*2");
        test("B[0]==194", 1);
        test("B[0]=65");
        test("string(B[0])", "A");
        test("string(B[0,0])", "A");

        test("A=[1 2; -3 -4]");

        test("s=4; s = s - 2;");
        test("s", 2);

        test("length([1-2])", 1);
        test("length([1 -2])", 2);
        test("length([1- 2])", 1);
        test("length([1 - 2])", 1);

        test("r=3;g=2;acc=0; for(i=0;i<7;i++) { acc+=g^i*r}; acc", 381);
        System.out.println("Tests passed.");
    }
}
