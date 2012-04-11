package april.jmat.menv.inst;

import java.util.*;

import april.jmat.menv.*;

public class ConstInstruction extends Instruction
{
    public Object v;

    public ConstInstruction(int r, Object v)
    {
        this.out = new int[] {r};
        this.v = v;
    }

    public String toString()
    {
        return String.format("r%d <- %s", out[0], TypeUtil.debug(v));
    }

    public void execute(Environment env)
    {
        env.r[out[0]] = v;
        env.pc++;
    }

}
