package april.jmat.menv.node;

import java.util.*;

import april.jmat.menv.*;
import april.jmat.menv.inst.*;

public class IfNode implements Node
{
    public Node cond;
    public Node trueClause, falseClause;

    public String toString()
    {
        StringBuffer sb = new StringBuffer();
        sb.append("(if "+cond+" "+trueClause);
        if (falseClause != null)
            sb.append(" "+falseClause);
        sb.append(")");
        return sb.toString();
    }

    public int emit(Emit e)
    {
        int r = cond.emit(e);

        if (falseClause == null) {

            int exitLabel = e.allocLabel();
            e.emit(new BranchInstruction(r, false, exitLabel));
            trueClause.emit(e);
            e.emitLabel(exitLabel);

        } else {
            int falseLabel = e.allocLabel();
            int exitLabel = e.allocLabel();

            e.emit(new BranchInstruction(r, false, falseLabel));
            trueClause.emit(e);
            e.emit(new BranchInstruction(exitLabel));
            e.emitLabel(falseLabel);
            falseClause.emit(e);
            e.emitLabel(exitLabel);
        }
        return -1;
    }
}
