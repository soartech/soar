package april.jmat.menv;

import java.util.*;

import april.jmat.*;

public class TypeUtil
{
    public static final Matrix TRUE  = new Matrix(1);
    public static final Matrix FALSE = new Matrix(0);

    public static final boolean toBoolean(Object o)
    {
        if (o == null)
            return false;

        if (o instanceof Matrix)
            return matrixToBoolean((Matrix) o);

        if (o instanceof String)
            return stringToBoolean((String) o);

        assert(false);
        return false;
    }

    public static final Matrix toMatrix(Object o)
    {
        if (o == null)
            return null;

        if (o instanceof Matrix)
            return (Matrix) o;

        if (o instanceof String)
            return stringToMatrix((String) o);

        assert(false);
        return null;
    }

    public static final String toString(Object o)
    {
        if (o == null)
            return null;

        if (o instanceof String)
            return (String) o;

        if (o instanceof Matrix)
            return matrixToString((Matrix) o);

        assert(false);
        return null;
    }

    public static final boolean stringToBoolean(String s)
    {
        if (s==null || s.length()==0)
            return false;

        char c = s.charAt(0);
        return (c=='t' || c=='T');
    }

    public static final boolean matrixToBoolean(Matrix m)
    {
        return m.get(0,0)!=0;
    }

    public static final String matrixToString(Matrix m)
    {
        StringBuilder sb = new StringBuilder();

        for (int i = 0; i < m.getRowDimension(); i++) {
            for (int j = 0; j < m.getColumnDimension(); j++) {
                int v = (int) m.get(i,j);
                char c = (char) v;

                if (v < 32 || v > 126)
                    c = '?';

                sb.append(c);
            }
        }

        return sb.toString();
    }

    public static final Matrix stringToMatrix(String s)
    {
        Matrix m = new Matrix(1, s.length());

        for (int i = 0; i < s.length(); i++)
            m.set(0, i, (int) s.charAt(i));

        return m;
    }

    public static final boolean equal(Object a, Object b)
    {
        if (a instanceof Matrix && b instanceof Matrix)
            return matrixEquals((Matrix) a, (Matrix) b);
        if (a instanceof String && b instanceof String)
            return ((String) a).equals((String) b);
        return false;
    }

    // are the two matrices equal?
    public static final boolean matrixEquals(Matrix A, Matrix B)
    {
        if (A.getColumnDimension()!=B.getColumnDimension() ||
            A.getRowDimension()!=B.getRowDimension())
            return false;

        for (int i = 0; i < A.getRowDimension(); i++) {
            for (int j = 0; j < A.getColumnDimension(); j++) {
                if (A.get(i,j)!=B.get(i,j))
                    return false;
            }
        }

        return true;
    }

    public static final int toInt(Object o)
    {
        if (o == null)
            return -1;

        if (o instanceof Matrix)
            return matrixToInt((Matrix) o);

        if (o instanceof String)
            return Integer.parseInt((String) o);

        assert(false);
        return -1;
    }

    // used for indexing matrices; ints are not exposed to the user.
    public static final int matrixToInt(Matrix m)
    {
        return (int) m.get(0,0);
    }

    // get a one-line toString representation of a value
    public static String debug(Object o)
    {
        if (o==null)
            return "null";

        if (o instanceof String) {
            // escape string
            String in = (String) o;
            StringBuilder sb = new StringBuilder();
            sb.append("\"");
            for (int i = 0; i < in.length(); i++) {
                char c = in.charAt(i);
                if (c=='\n')
                    sb.append("\\n");
                else if (c=='\t')
                    sb.append("\\t");
                else
                    sb.append(c);
            }
            sb.append("\"");
            return sb.toString();
        }

        if (o instanceof Matrix) {
            StringBuffer sb = new StringBuffer();
            Matrix m = (Matrix) o;
            int nrows = m.getRowDimension();
            int ncols = m.getColumnDimension();

            if (nrows==1 && ncols==1)
                return String.format("%f", m.get(0,0));

            sb.append("[");
            for (int i = 0; i < nrows; i++) {
                for (int j = 0; j < ncols; j++) {
                    sb.append(String.format("%15.5f", m.get(i,j)));
                }
                sb.append(";");
            }
            sb.append("]");
            return sb.toString();
        }

        assert(false);
        return "UH-OH";
    }
}
