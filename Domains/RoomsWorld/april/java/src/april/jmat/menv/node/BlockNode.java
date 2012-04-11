package april.jmat.menv.node;

import java.util.*;

import april.jmat.menv.*;
import april.jmat.menv.inst.*;

public class BlockNode implements Node
{
    public ArrayList<Node> statements = new ArrayList<Node>();

    public String toString()
    {
        String s = "";
        for (Node n : statements)
            s = s+n+"\n";
        return s;
    }

    public int emit(Emit e)
    {
        int r = -1;

        for (Node n : statements)
            r = n.emit(e);

        return r;
    }
}
