package april.jmat.menv.node;

import java.util.*;

import april.jmat.menv.*;
import april.jmat.menv.inst.*;

public class ReturnNode implements Node
{
    Node value;

    public ReturnNode(Node value)
    {
        this.value = value;
    }

    public String toString()
    {
        return "(return "+value+")";
    }

    public int emit(Emit e)
    {
        int r = value.emit(e);
        e.emit(new ReturnInstruction(r));
        return r;
    }
}
