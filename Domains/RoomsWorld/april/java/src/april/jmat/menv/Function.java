package april.jmat.menv;

import java.util.*;
import april.jmat.*;

public class Function
{
    public ArrayList<String> parameterNames = new ArrayList<String>();
    public ArrayList<Object> parameterDefaults = new ArrayList<Object>();

    public Assembly asm;

    public String name;

    /** Does the function have any "magic" side effects, such as altering
        a global variable, producing input/output?

        If it *is* side-effect free, then the optimizer can eliminate
        unnecessary calls. However, because functions may not be
        defined at compile time, and functions may be redeclared
        between compile time and execution time, this optimization is
        limited to built-in functions.

        It's always safe for sideEffectFree = false.
    **/
    public boolean sideEffectFree = false;

    protected Function()
    {
    }

    public Function(String functionName,
                    ArrayList<String> parameterNames,
                    ArrayList<Object> parameterDefaults,
                    Assembly asm)
    {
        this.name = functionName;
        this.parameterNames = parameterNames;
        this.parameterDefaults = parameterDefaults;
        this.asm = asm;
    }

    // this can be overridden for native functions.
    public Object eval(Environment env)
    {
        return env.run(asm);
    }
}
