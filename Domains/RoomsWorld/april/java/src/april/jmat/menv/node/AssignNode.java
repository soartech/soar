package april.jmat.menv.node;

import java.util.*;

import april.jmat.menv.*;
import april.jmat.menv.inst.*;

public class AssignNode implements Node
{
    Node    lhs;
    Node    rhs;

    public AssignNode(Node lhs, Node rhs)
    {
        this.lhs = lhs;
        this.rhs = rhs;
    }

    public String toString()
    {
        return String.format("(assign %s %s)", lhs, rhs);
    }

    public int emit(Emit e)
    {
        int r = rhs.emit(e);

        if (lhs instanceof SubselNode) {
            SubselNode sn = (SubselNode) lhs;
            if (!(sn.M instanceof VarNode))
                throw new MEnvRuntimeException("illegal assignment to '"+sn.M+"'");

            VarNode vn = ((VarNode) sn.M);

            int ra = sn.rowA.emit(e);
            int rb = sn.rowB==null ? ra : sn.rowB.emit(e);
            int ca = sn.colA==null ? -1 : sn.colA.emit(e);
            int cb = sn.colB==null ? ca : sn.colB.emit(e);

            e.emit(new SubselStoreInstruction(vn.name, r, ra, rb, ca, cb));
            return r;
        }

        if (!(lhs instanceof VarNode))
            throw new MEnvRuntimeException("illegal assignment to '"+lhs+"'");

        VarNode vn = ((VarNode) lhs);
        e.emit(new StoreInstruction(vn.name, r));

        return r;
    }
}
