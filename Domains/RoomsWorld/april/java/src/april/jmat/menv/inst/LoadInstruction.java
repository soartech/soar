package april.jmat.menv.inst;

import java.util.*;

import april.jmat.menv.*;

public class LoadInstruction extends Instruction
{
    public String name;

    public LoadInstruction(int r, String s)
    {
        this.out = new int[] {r};
        this.name = s;
    }

    public String toString()
    {
        return String.format("r%d <- '%s'", out[0], name);
    }

    public void execute(Environment env)
    {
        env.r[out[0]] = env.getVariable(name);
        if (env.r[out[0]]==null)
            throw new MEnvRuntimeException("No such variable '"+name+"'");

        env.pc++;
    }
}
