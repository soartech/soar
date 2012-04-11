package april.jmat.menv.node;

import java.util.*;

import april.jmat.menv.*;
import april.jmat.menv.inst.*;

public class ForNode implements Node
{
    public Node init;
    public Node cond;
    public Node incr;
    public Node body;

    public String toString()
    {
        StringBuffer sb = new StringBuffer();
        sb.append("(for "+init+" "+cond+" "+incr+" "+body);

        return sb.toString();
    }

    public int emit(Emit e)
    {
        int loopLabel = e.allocLabel();
        int breakLabel = e.allocLabel();
        int continueLabel = e.allocLabel();

        init.emit(e);

        e.pushBreakLabel(breakLabel);
        e.pushContinueLabel(continueLabel);

        e.emitLabel(loopLabel);

        // check the condition
        int r = cond.emit(e);
        e.emit(new BranchInstruction(r, false, breakLabel));

        body.emit(e);

        e.emitLabel(continueLabel);
        incr.emit(e);

        e.emit(new BranchInstruction(loopLabel));

        // exit
        e.emitLabel(breakLabel);

        e.popBreakLabel();
        e.popContinueLabel();
        return -1;
    }
}
