package april.jmat.menv.node;

import java.util.*;

import april.jmat.*;

import april.jmat.menv.*;
import april.jmat.menv.inst.*;

public class ConstNode implements Node
{
    public Object v;

    public ConstNode(double scalar)
    {
        this.v = new Matrix(scalar);
    }

    public ConstNode(Object v)
    {
        this.v = v;
    }

    public String toString()
    {
        return TypeUtil.debug(v);
    }

    public int emit(Emit e)
    {
        int r = e.allocVar();
        e.emit(new ConstInstruction(r, v));
        return r;
    }
}
