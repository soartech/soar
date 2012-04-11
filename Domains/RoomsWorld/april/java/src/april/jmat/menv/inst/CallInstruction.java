package april.jmat.menv.inst;

import java.util.*;

import april.jmat.menv.*;

public class CallInstruction extends Instruction
{
    public String functionName;

    public CallInstruction(int r, String functionName, ArrayList<Integer> params)
    {
        this.out = new int[] {r};
        this.in = new int[params.size()];
        for (int i = 0; i < params.size(); i++)
            this.in[i] = params.get(i);

        this.functionName = functionName;
    }

    public void execute(Environment env)
    {
        Environment callEnv = new Environment(env);
        Function f = callEnv.getFunction(functionName);
        if (f==null)
            throw new MEnvRuntimeException("Unknown function '"+functionName+"'");

        callEnv.args = new Object[in.length];
        for (int i = 0; i < in.length; i++) {
            callEnv.args[i] = env.r[in[i]];
        }

        // set up parameters.
        for (int i = 0; i < f.parameterNames.size(); i++) {
            if (i < in.length)
                callEnv.putVariable(f.parameterNames.get(i), env.r[in[i]]);
            else
                callEnv.putVariable(f.parameterNames.get(i), f.parameterDefaults.get(i));

        }

        Object res = f.eval(callEnv);
        env.r[out[0]] = res;
        env.pc++;
    }

    public String toString()
    {
        StringBuffer sb = new StringBuffer();
        sb.append(String.format("r%d <- (function '%s' ", out[0], functionName));
        for (int i : in)
            sb.append(String.format("r%d ", i));

        sb.append(")");
        return sb.toString();
    }
}
