package april.jmat.menv.node;

import april.jmat.menv.*;
import april.jmat.menv.inst.*;

public class BreakNode implements Node
{
    public int emit(Emit e)
    {
        e.emit(new BranchInstruction(e.peekBreakLabel()));
        return -1;
    }

    public String toString()
    {
        return "break";
    }
}
