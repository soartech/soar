package april.jmat.menv.node;

import java.util.*;

import april.jmat.menv.*;
import april.jmat.menv.inst.*;

public class NopNode implements Node
{
    public String toString()
    {
        return "(nop)";
    }

    public int emit(Emit e)
    {
        return -1;
    }
}
