package april.jmat.menv.inst;

import java.util.*;
import april.jmat.menv.*;

public class ReturnInstruction extends Instruction
{
    public ReturnInstruction(int r)
    {
        if (r >= 0)
            in = new int[] {r};
    }

    public String toString()
    {
        if (in == null)
            return "return";
        return String.format("return r%d", in[0]);
    }

    public void execute(Environment env)
    {
        if (in==null)
            env.retval = null;
        else
            env.retval = env.r[in[0]];

        env.pc = -1;
    }
}
