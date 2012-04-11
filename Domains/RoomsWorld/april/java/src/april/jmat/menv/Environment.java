package april.jmat.menv;

import java.io.*;
import java.util.*;
import april.jmat.*;

import april.jmat.menv.inst.*;

public class Environment
{
    static HashMap<String, Matrix> constants = new HashMap<String, Matrix>();
    static
    {
        constants.put("pi", new Matrix(Math.PI));
        constants.put("e", new Matrix(Math.E));
    }

    HashMap<String,Object>   variables;
    HashMap<String,Function> functions;

    MEnv menv;

    ////////////////////////////////////

    public int     pc;
    public Object  r[];
    public Object  retval;

    // the arguments to the function are passed in here.
    // we use this for var-args support.
    public Object  args[];

    ////////////////////////////////////

    public Environment(MEnv menv)
    {
        variables = new HashMap<String,Object>();
        functions = new HashMap<String,Function>();
        this.menv = menv;
    }

    public Environment(Environment parent)
    {
        variables = new HashMap<String,Object>();
        functions = parent.functions;
        this.menv = parent.menv;
    }

    public Object run(Assembly asm)
    {
        retval = null;
        pc = 0;

        // make sure there's enough temp registers.
        if (r == null || r.length <= asm.rMax)
            r = new Object[asm.rMax+1];

        while (pc >= 0 && pc < asm.instructions.size()) {
            Instruction inst = asm.instructions.get(pc);
            inst.execute(this);
        }

        // free pointers to temp registers.
        for (int i = 0; i < asm.rMax; i++)
            r[i] = null;

        return retval;
    }

    public void putVariable(String name, Object o)
    {
        variables.put(name, o);
    }

    public Object getVariable(String name)
    {
        return variables.get(name);
    }

    public Matrix getVariableAsMatrix(String name)
    {
        return TypeUtil.toMatrix(variables.get(name));
    }

    public String getVariableAsString(String name)
    {
        return TypeUtil.toString(variables.get(name));
    }

    public Function getFunction(String name)
    {
        Function f = functions.get(name);

        if (f == null)
            f = BuiltinFunctions.functions.get(name);
        return f;
    }

    public void putFunction(String name, Function f)
    {
        if (BuiltinFunctions.functions.get(name)!=null)
            throw new MEnvRuntimeException("Cannot redefine built-in function '"+name+"'");

        functions.put(name, f);
    }
}
