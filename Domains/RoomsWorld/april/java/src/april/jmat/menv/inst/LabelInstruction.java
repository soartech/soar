package april.jmat.menv.inst;

import java.util.*;

import april.jmat.menv.*;

// Not really an instruction-- a marker that a label belongs
// here. These are removed at assemble time.
public class LabelInstruction extends Instruction
{
    public int label;

    public LabelInstruction(int label)
    {
        this.label = label;
    }

    public void execute(Environment env)
    {
        assert(false);
    }
}

