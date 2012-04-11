package april.jmat.menv.node;

import java.util.*;

import april.jmat.menv.*;
import april.jmat.menv.inst.*;

public class SubselNode implements Node
{
    Node rowA, rowB;
    Node colA, colB;
    Node M;

    public SubselNode(Node M, Node rowA, Node rowB, Node colA, Node colB)
    {
        this.M = M;
        this.rowA = rowA;
        this.rowB = rowB;
        this.colA = colA;
        this.colB = colB;
    }

    public String toString()
    {
        return String.format("[ %s : %s , %s : %s ]", rowA, rowB, colA, colB);
    }

    public int emit(Emit e)
    {
        int rsrc = M.emit(e);
        int ra = rowA.emit(e);
        int rb = rowB != null ? rowB.emit(e) : ra;
        int ca = colA != null ? colA.emit(e) : -1; // -1 means vector notation
        int cb = colB != null ? colB.emit(e) : ca;

        int r = e.allocVar();

        e.emit(new SubselInstruction(r, rsrc, ra, rb, ca, cb));

        return r;
    }
}
