package april.graph;

import java.io.*;
import java.util.*;

import lcm.lcm.*;

import april.util.*;

/** Abstract information relating the position of two GraphNodes. **/
public abstract class GEdge
{
    /** Which GraphNodes are related via this edge? For a typical
     * edge, this will be [a, b], though edges can (in fact) relate an
     * arbitrary number of nodes.
     **/
    public int nodes[];

    public Attributes attributes;

    /** Additional information associated with a GraphNode, often
     * sensor data.
     **/
    public static class Attribute
    {
        public Object          o;
        public StructureCoder  coder;

        public Attribute(Object o, StructureCoder coder)
        {
            this.o = o;
            this.coder = coder;
        }
    }

    /** What is the Chi^2 error of this edge, given the graph? **/
    public abstract double getChi2(Graph g);

    public abstract void write(StructureWriter outs) throws IOException;
    public abstract void read(StructureReader ins) throws IOException;

    /** how many degrees of freedom? **/
    public abstract int getDOF();

    /** Linearize the edge. If lin is null, a new linearization will
     * be allocated. Alternatively, a lin can be passed in from a
     * previous invocation of this edge, and will re-use the data
     * structures available. **/
    public abstract Linearization linearize(Graph g, Linearization lin);

    public abstract GEdge copy();

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
}
