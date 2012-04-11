package april.jmat.menv;

import april.jmat.*;

import java.io.*;
import java.util.*;

import april.jmat.menv.node.*;
import april.jmat.menv.inst.*;

public class MEnv
{
    Environment env = new Environment(this);
    HashMap<String,ArrayList<Assembly>> exprCache = new HashMap<String,ArrayList<Assembly>>();

    Random      random;
    PrintStream out = System.out;
    InputStream in  = System.in;

    public Matrix getVariableAsMatrix(String name)
    {
        return env.getVariableAsMatrix(name);
    }

    public double getVariableAsDouble(String name)
    {
        Matrix m = getVariableAsMatrix(name);
        return m.get(0,0);
    }

    public void putVariable(String name, Object o)
    {
        env.putVariable(name, o);
    }

    public void clear()
    {
        exprCache.clear();
        env.functions.clear();
        env.variables.clear();
    }

    public Object evaluate(String expr)
    {
        ArrayList<Assembly> asms = exprCache.get(expr);

        if (asms == null) {

            asms = new ArrayList<Assembly>();
            Compiler cp = new Compiler(expr);

            for (Function f : cp.functions)
                env.putFunction(f.name, f);

            for (Assembly asm : cp.commands) {
                asms.add(asm);
            }

            exprCache.put(expr, asms);
        }

        Object lastReturnValue = null;

        for (Assembly asm : asms)
            lastReturnValue = env.run(asm);

        return lastReturnValue;
    }

    public static void main(String args[])
    {
        MEnv menv = new MEnv();

        for (int i = 0; i < args.length; i++) {
            try {
                Object o = menv.evaluate(args[i]);

                if (o==null)
                    System.out.println("null");
                else
                    System.out.println(o);

            } catch (MEnvRuntimeException ex) {
                System.out.println("Error: "+ex);
            }
        }
    }
}
