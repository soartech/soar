package april.jmat.menv;

import april.jmat.*;
import java.util.*;

public class BuiltinFunctions
{
    public static HashMap<String, Function> functions = new HashMap<String, Function>();

    static {

        new FunctionMap("sin") {
            double eval(double v) {
                return Math.sin(v);
            }};

        new FunctionMap("cos") {
            double eval(double v) {
                return Math.cos(v);
            }};

        new FunctionMap("log") {
            double eval(double v) {
                return Math.log(v);
            }};

        new FunctionMap("ln") {
            double eval(double v) {
                return Math.log(v);
            }};

        new FunctionMap("log2") {
            double eval(double v) {
                return Math.log(v)/Math.log(2);
            }};

        new FunctionMap("log10") {
            double eval(double v) {
                return Math.log(v)/Math.log(10);
            }};

        new FunctionX("inv") {
            Matrix eval(Matrix x) {
                return x.inverse();
            }};

        new FunctionX("size") {
            Matrix eval(Matrix x) {
                return new Matrix(new double[][] {{ x.getRowDimension(), x.getColumnDimension() }});
            }};

        new FunctionX("length") {
            Matrix eval(Matrix x) {
                return new Matrix(new double[][] {{ x.getRowDimension()*x.getColumnDimension() }});
            }};

        new FunctionX("trace") {
            Matrix eval(Matrix x) {
                return new Matrix(x.trace());
            }};

        /*	new FunctionX("cond") {
            Matrix eval(Matrix x) {
            return new Matrix(x.cond());
            }};*/


        new Function0("nargs") {
            public Object eval(Environment env) {
                return new Matrix(env.args!=null ? env.args.length : 0);
            }};

        new FunctionArg();

        new FunctionXY("zeros") {
            Matrix eval(Matrix rows, Matrix cols) {
                int nrows = (int) rows.get(0,0);
                int ncols = cols==null ? nrows : (int) cols.get(0,0);

                return new Matrix(nrows, ncols);
            }};

        new FunctionXY("ones") {
            Matrix eval(Matrix rows, Matrix cols) {
                int nrows = (int) rows.get(0,0);
                int ncols = cols==null ? nrows : (int) cols.get(0,0);

                Matrix x = new Matrix(nrows, ncols);
                for (int i = 0; i < nrows; i++)
                    for (int j = 0; j < ncols; j++)
                        x.set(i,j, 1);
                return x;
            }};

        new FunctionXY("eye") {
            Matrix eval(Matrix rows, Matrix cols) {
                int nrows = (int) rows.get(0,0);
                int ncols = cols==null ? nrows : (int) cols.get(0,0);

                return Matrix.identity(nrows, ncols);
            }};

        new RandFunction();
        new RandnFunction();
        new PrintFunction();
        new FormatFunction();

        new FunctionRawX("is_null") {
            Object eval(Object x) {
                return x==null ? TypeUtil.TRUE : TypeUtil.FALSE;
            }};

        new FunctionRawX("is_string") {
            Object eval(Object x) {
                return (x instanceof String) ? TypeUtil.TRUE : TypeUtil.FALSE;
            }};

        new FunctionRawX("string") {
            Object eval(Object x) {
                return TypeUtil.toString(x);
            }};

        new FunctionRawX("is_matrix") {
            Object eval(Object x) {
                return (x instanceof Matrix) ? TypeUtil.TRUE : TypeUtil.FALSE;
            }};

        new FunctionRawX("matrix") {
            Object eval(Object x) {
                return TypeUtil.toMatrix(x);
            }};
    }

    /////////////////////////////////////////////////////////////////

    // generic function of one argument in which every element is
    // mapped through a scalar function
    static abstract class FunctionMap extends Function
    {
        public FunctionMap(String name)
        {
            this.name = name;
            this.sideEffectFree = true;

            parameterNames.add("x");
            functions.put(name, this);
        }

        public Matrix eval(Environment env)
        {
            Matrix x = env.getVariableAsMatrix("x");
            Matrix y = x.copy();

            for (int i = 0; i < x.getRowDimension(); i++)
                for (int j = 0; j < x.getColumnDimension(); j++)
                    y.set(i,j, eval(x.get(i,j)));

            return y;
        }

        abstract double eval(double v);
    }

    // generic function of no arguments
    static abstract class Function0 extends Function
    {
        public Function0(String name)
        {
            this.name = name;
            this.sideEffectFree = true;

            functions.put(name, this);
        }
    }

    // generic function of one argument
    static abstract class FunctionX extends Function
    {
        public FunctionX(String name)
        {
            this.name = name;
            this.sideEffectFree = true;

            parameterNames.add("x");
            parameterDefaults.add(null);
            functions.put(name, this);
        }

        public Matrix eval(Environment env)
        {
            Matrix x = env.getVariableAsMatrix("x");
            return eval(x);
        }

        abstract Matrix eval(Matrix x);
    }

    // generic function of two arguments
    static abstract class FunctionXY extends Function
    {
        public FunctionXY(String name)
        {
            this.name = name;
            this.sideEffectFree = true;

            parameterNames.add("x");
            parameterNames.add("y");
            parameterDefaults.add(null);
            parameterDefaults.add(null);
            functions.put(name, this);
        }

        public Matrix eval(Environment env)
        {
            Matrix x = env.getVariableAsMatrix("x");
            Matrix y = env.getVariableAsMatrix("y");
            return eval(x,y);
        }

        abstract Matrix eval(Matrix x, Matrix y);
    }

    static class RandFunction extends Function
    {
        public RandFunction()
        {
            this.name = "rand";
            this.sideEffectFree = false; /**** NOTE ****/

            parameterNames.add("x");
            parameterNames.add("y");
            parameterDefaults.add(null);
            parameterDefaults.add(null);
            functions.put(name, this);
        }

        public Matrix eval(Environment env)
        {
            Matrix x = env.getVariableAsMatrix("x");
            Matrix y = env.getVariableAsMatrix("y");

            int nrows = x==null ? 1 : (int) x.get(0,0);
            int ncols = y==null ? nrows : (int) y.get(0,0);

            Matrix M = new Matrix(nrows, ncols);
            for (int i = 0; i < nrows; i++)
                for (int j = 0; j < ncols; j++)
                    M.set(i,j, env.menv.random.nextDouble());

            return M;
        }
    }

    static class RandnFunction extends Function
    {
        public RandnFunction()
        {
            this.name = "randn";
            this.sideEffectFree = false; /**** NOTE ****/

            parameterNames.add("x");
            parameterNames.add("y");
            parameterDefaults.add(null);
            parameterDefaults.add(null);
            functions.put(name, this);
        }

        public Matrix eval(Environment env)
        {
            Matrix x = env.getVariableAsMatrix("x");
            Matrix y = env.getVariableAsMatrix("y");

            int nrows = x==null ? 1 : (int) x.get(0,0);
            int ncols = y==null ? nrows : (int) y.get(0,0);

            Matrix M = new Matrix(nrows, ncols);
            for (int i = 0; i < nrows; i++)
                for (int j = 0; j < ncols; j++)
                    M.set(i,j, env.menv.random.nextGaussian());

            return M;
        }
    }

    static String format(Object args[])
    {
        if (args==null)
            return "";

        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < args.length; i++) {
            Object o = args[i];

            if (o instanceof String)
                sb.append((String) o);
            else
                sb.append(TypeUtil.debug(o));
        }

        return sb.toString();
    }

    static class PrintFunction extends Function
    {
        public PrintFunction()
        {
            this.name = "print";
            this.sideEffectFree = false; /**** NOTE ****/

            functions.put(name, this);
        }

        public Object eval(Environment env)
        {
            String s = format(env.args);
            env.menv.out.print(s);
            return null; //new Matrix((double) s.length());
        }
    }

    static class FormatFunction extends Function
    {
        public FormatFunction()
        {
            this.name = "format";
            this.sideEffectFree = false; /**** NOTE ****/

            functions.put(name, this);
        }

        public Object eval(Environment env)
        {
            String s = format(env.args);
            return s;
        }
    }

    // generic function of one argument, that does NOT convert the
    // argument to a matrix.
    static abstract class FunctionRawX extends Function
    {
        public FunctionRawX(String name)
        {
            this.name = name;
            this.sideEffectFree = true;

            parameterNames.add("x");
            parameterDefaults.add(null);
            functions.put(name, this);
        }

        public Object eval(Environment env)
        {
            Object x = env.getVariable("x");
            return eval(x);
        }

        abstract Object eval(Object x);
    }

    // fetch the nth argument
    static class FunctionArg extends Function
    {
        public FunctionArg()
        {
            this.name = "arg";
            this.sideEffectFree = true;

            parameterNames.add("x");
            parameterDefaults.add(null);
            functions.put(name, this);
        }

        public Object eval(Environment env)
        {
            Object x = env.getVariable("x");

            int idx = TypeUtil.matrixToInt(TypeUtil.toMatrix(x));
            if (env.args==null || idx >=env.args.length)
                return null;
            return env.args[idx];
        }
    }

}
