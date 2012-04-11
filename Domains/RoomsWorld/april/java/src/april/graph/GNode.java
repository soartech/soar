package april.graph;

import java.io.*;
import java.util.*;
import java.lang.reflect.*;

import april.jmat.*;
import april.jmat.geom.*;
import april.util.*;

/** A location in the world whose position is related via GraphConstraints. **/
public abstract class GNode
{
    /** Current state of the graph node. NEVER NULL. **/
    public double state[];

    /** Initial value of the node. NEVER NULL. **/
    public double init[];

    /** Ground truth of the node, may be null **/
    public double truth[];

    public Attributes attributes;

    public GNode()
    {
    }

    /** What is the dimensionality of state? **/
    public abstract int getDOF();

    public abstract double[] toXyzRpy(double s[]);

    public void setAttribute(String s, Object o, StructureCoder coder)
    {
        if (attributes == null)
            attributes = new Attributes();
        attributes.setAttribute(s,o,coder);
    }

    public void setAttribute(String s, Object o)
    {
        if (attributes == null)
            attributes = new Attributes();
        attributes.setAttribute(s,o);
    }

    public Object getAttribute(String s)
    {
        if (attributes == null)
            return null;
        return attributes.getAttribute(s);
    }

    public void write(StructureWriter outs) throws IOException
    {
        outs.writeComment("state");
        outs.writeDoubles(state);

        outs.writeComment("");
        outs.writeComment("truth");
        outs.writeDoubles(truth);

        outs.writeComment("");
        outs.writeComment("initial value");
        outs.writeDoubles(init);
        Attributes.write(attributes, outs);
    }

    public void read(StructureReader ins) throws IOException
    {
        state = ins.readDoubles();
        truth = ins.readDoubles();
        init = ins.readDoubles();
        attributes = Attributes.read(ins);
    }

    public abstract GNode copy();
}
