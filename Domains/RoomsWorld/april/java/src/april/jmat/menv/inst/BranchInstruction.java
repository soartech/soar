package april.jmat.menv.inst;

import java.util.*;

import april.jmat.menv.*;

public class BranchInstruction extends Instruction
{
    public int label;
    public int labelidx;

    public boolean cond;

    public BranchInstruction(int label)
    {
        this.label = label;
    }

    public BranchInstruction(int r, boolean cond, int label)
    {
        this.in = new int[] {r};
        this.cond = cond;
        this.label = label;
    }

    public void execute(Environment env)
    {
        if (in == null || in.length == 0) {
            env.pc = labelidx;
        } else {
            boolean b = TypeUtil.toBoolean(env.r[in[0]]);
            if (b==cond)
                env.pc = labelidx;
            else
                env.pc++;
        }
    }

    public String toString()
    {
        if (in == null || in.length == 0)
            return String.format("b  %d", labelidx);
        else
            return String.format("b%s r%d %d", cond ? "t" : "f", in[0], labelidx);
    }
}
