package april.jmat.menv.node;

import april.jmat.menv.*;
import april.jmat.menv.inst.*;

public class ContinueNode implements Node
{
    public int emit(Emit e)
    {
        e.emit(new BranchInstruction(e.peekContinueLabel()));
        return -1;
    }

    public String toString()
    {
        return "continue";
    }
}
