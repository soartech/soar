package april.jmat.menv.node;

import java.util.*;

import april.jmat.menv.*;
import april.jmat.menv.inst.*;

public class VarNode implements Node
{
    String       name;

    public VarNode(String name)
    {
        this.name = name;
    }

    public String toString()
    {
        return String.format("\"%s\"", name);
    }

    public int emit(Emit e)
    {
        int r = e.allocVar();
        e.emit(new LoadInstruction(r, name));
        return r;
    }
}
