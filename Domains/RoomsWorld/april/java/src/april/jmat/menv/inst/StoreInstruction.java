package april.jmat.menv.inst;

import java.util.*;
import april.jmat.menv.*;

public class StoreInstruction extends Instruction
{
    public String name;

    public StoreInstruction(String name, int r)
    {
        this.in = new int[] {r};
        this.name = name;
    }

    public String toString()
    {
        return String.format("'%s' <- r%d", name, in[0]);
    }

    public void execute(Environment env)
    {
        env.putVariable(name, env.r[in[0]]);
        env.pc++;
    }
}
