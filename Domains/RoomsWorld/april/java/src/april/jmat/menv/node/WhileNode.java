package april.jmat.menv.node;

import java.util.*;

import april.jmat.menv.*;
import april.jmat.menv.inst.*;

public class WhileNode implements Node
{
    public Node cond;
    public Node body;

    public String toString()
    {
        StringBuffer sb = new StringBuffer();
        sb.append("(while "+cond+" "+body);

        return sb.toString();
    }

    public int emit(Emit e)
    {
        int breakLabel = e.allocLabel();
        int continueLabel = e.allocLabel();

        e.pushBreakLabel(breakLabel);
        e.pushContinueLabel(continueLabel);

        e.emitLabel(continueLabel);

        // check the condition
        int r = cond.emit(e);
        e.emit(new BranchInstruction(r, false, breakLabel));

        body.emit(e);

        e.emit(new BranchInstruction(continueLabel));

        // exit
        e.emitLabel(breakLabel);

        e.popBreakLabel();
        e.popContinueLabel();
        return -1;
    }
}
