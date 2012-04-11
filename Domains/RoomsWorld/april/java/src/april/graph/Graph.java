package april.graph;

import java.util.*;
import java.io.*;
import java.lang.reflect.*;

import april.jmat.*;
import april.jmat.geom.*;

import april.util.*;

/** A a collection of GNodes and GEdges. It is generally safe to add
 * nodes and edges, but it is not safe to remove them due to cached
 * state in stateIndices and nodeIndices. **/
public class Graph
{
    /** the set of all constraints. **/
    public ArrayList<GEdge> edges = new ArrayList<GEdge>();

    /** the set of all nodes. **/
    public ArrayList<GNode> nodes = new ArrayList<GNode>();

    /** Each node may have a different number of degrees of
        freedom. The stateIndices array allows us to rapidly lookup
        the state vector index corresponding to a particular
        node. (These are computed on-demand by getStateIndex().)
    **/
    ArrayList<Integer> stateIndices = new ArrayList<Integer>();

    /** What is the index in g.nodes for a given GNode? This is
     * updated on demand by indexOf. **/
    HashMap<GNode, Integer> nodeIndices = new HashMap<GNode, Integer>();

    public Attributes attributes;

    /** Chi^2 error and other error statistics. **/
    public static class ErrorStats
    {
        public double chi2;
        public double chi2normalized;
        public int degreesOfFreedom;

        public double meanSquaredDistanceError;
        public double meanSquaredThetaError;
    }

    public int getStateLength()
    {
        return getStateIndex(nodes.size()-1) + nodes.get(nodes.size()-1).getDOF();
    }

    public int getStateIndex(int nodeIndex)
    {
        for (int node = stateIndices.size(); node <= nodeIndex; node++) {
            if (node == 0) {
                stateIndices.add(0);
                continue;
            }

            int lastIndex = stateIndices.get(node-1);
            GNode gn = nodes.get(node - 1);
            stateIndices.add(lastIndex + gn.getDOF());
        }

        return (int) stateIndices.get(nodeIndex);
    }

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

    public int indexOf(GNode gn)
    {
        for (int i = nodeIndices.size(); i < nodes.size(); i++)
            nodeIndices.put(nodes.get(i), i);

        Integer i = nodeIndices.get(gn);
        if (i == null)
            return -1;
        return i;
    }

    /** Edges are copied since their .a and .b will be remapped. Nodes
     * are not copied. Nodes that are the same object (same pointer)
     * will be merged into the same node. **/
    public void merge(Graph g)
    {
        // indices in g.nodes map to indices in this.nodes
        HashMap<Integer, Integer> indexMap = new HashMap<Integer, Integer>();

        for (int gidx = 0; gidx < g.nodes.size(); gidx++) {
            GNode gn = g.nodes.get(gidx);

            // is this node already in this graph?
            int idx = indexOf(gn);

            if (idx >= 0) {
                // yep. record the mapping; we're done.
                indexMap.put(gidx, idx);
            } else {
                // nope, add this node.
                indexMap.put(gidx, nodes.size());
                nodes.add(gn);
            }
        }

        for (int eidx = 0; eidx < g.edges.size(); eidx++) {
            GEdge ge = g.edges.get(eidx).copy();

            for (int i = 0; i < ge.nodes.length; i++)
                ge.nodes[i] = indexMap.get(ge.nodes[i]);
            edges.add(ge);
        }
    }

    /** performs copies of the edges, in order to account for the new
     * indices for the nodes. The nodes themselves are not copied. **/
    public ArrayList<Graph> getConnectedComponents()
    {
        // create an extra node for those edges which implicitly
        // connect to the coordinate frame (e.g. GXYTPosEdge)
        int common_node = nodes.size();
        UnionFindSimple uf = new UnionFindSimple(nodes.size()+1);

        for (GEdge ge : edges) {

            if (ge.nodes.length == 1)
                uf.connectNodes(ge.nodes[0], common_node);

            for (int i = 0; i < ge.nodes.length; i++)
                for (int j = i+1; j < ge.nodes.length; j++)
                    uf.connectNodes(ge.nodes[i], ge.nodes[j]);
        }

        // for each representative ID (which yields a graph), we
        // maintain a mapping from original node indices to the
        // indices in this graph.
        HashMap<Integer, HashMap<Integer, Integer>> mappings = new HashMap<Integer, HashMap<Integer, Integer>>();

        // map representative IDs to graphs.
        HashMap<Integer, Graph> graphs = new HashMap<Integer, Graph>();

        // copy each node into the appropriate component, remembering
        // its new index.
        for (int gidx = 0; gidx < nodes.size(); gidx++) {
            int rep = uf.getRepresentative(gidx);

            HashMap<Integer, Integer> indicesMap = mappings.get(rep);
            if (indicesMap == null) {
                indicesMap = new HashMap<Integer, Integer>();
                mappings.put(rep, indicesMap);
            }

            Graph g = graphs.get(rep);
            if (g == null) {
                g = new Graph();
                graphs.put(rep, g);
            }

            indicesMap.put(gidx, g.nodes.size());
            g.nodes.add(nodes.get(gidx));
        }

        // now, handle edges
        for (int eidx = 0; eidx < edges.size(); eidx++) {
            GEdge ge = edges.get(eidx);
            int rep = uf.getRepresentative(ge.nodes[0]);

            // check: all nodes connected via this edge must be in the same component.
            for (int i = 1; i < ge.nodes.length; i++)
                assert(rep == uf.getRepresentative(ge.nodes[i]));

            HashMap<Integer, Integer> indicesMap = mappings.get(rep);
            ge = ge.copy();

            for (int i = 0; i < ge.nodes.length; i++)
                ge.nodes[i] = indicesMap.get(ge.nodes[i]);

            graphs.get(rep).edges.add(ge);
        }

        // make the list.
        ArrayList<Graph> list = new ArrayList<Graph>();
        for (Graph g : graphs.values())
            list.add(g);

        return list;
    }

    public void write(String path) throws IOException
    {
        FileWriter outs = new FileWriter(path);
        write(new BufferedWriter(outs));
        outs.close();
    }

    public void write(BufferedWriter _outs) throws IOException
    {
        StructureWriter outs = new TextStructureWriter(_outs);

        Attributes.write(attributes, outs);

        for (int i = 0; i < nodes.size(); i++) {
            GNode gn = nodes.get(i);

            outs.writeComment("node "+i);
            outs.writeString(gn.getClass().getName());
            outs.blockBegin();
            gn.write(outs);
            outs.blockEnd();
        }

        for (int i = 0; i < edges.size(); i++) {
            GEdge gc = edges.get(i);

            outs.writeComment("constraint "+i);
            outs.writeString(gc.getClass().getName());
            outs.blockBegin();
            gc.write(outs);
            outs.blockEnd();
        }

        outs.close();
        System.out.printf("wrote %d nodes and %d edges\n", nodes.size(), edges.size());
    }

    /** Returns { (xmin, ymin, zmin), (xmax, ymax, zmax) } **/
    public ArrayList<double[]> getBounds()
    {
        ArrayList<double[]> bounds = new ArrayList<double[]>();

        double xmin = Double.MAX_VALUE, xmax = -Double.MAX_VALUE;
        double ymin = Double.MAX_VALUE, ymax = -Double.MAX_VALUE;
        double zmin = Double.MAX_VALUE, zmax = -Double.MAX_VALUE;

        for (GNode gn : nodes) {
            double xytrpy[] = gn.toXyzRpy(gn.state);

            xmin = Math.min(xmin, xytrpy[0]);
            ymin = Math.min(ymin, xytrpy[1]);
            zmin = Math.min(zmin, xytrpy[2]);

            xmax = Math.max(xmax, xytrpy[0]);
            ymax = Math.max(ymax, xytrpy[1]);
            zmax = Math.max(zmax, xytrpy[2]);
        }

        bounds.add(new double[] {xmin, ymin, zmin});
        bounds.add(new double[] {xmax, ymax, zmax});

        return bounds;
    }

    public Graph()
    {
    }

    public Graph(String path) throws IOException
    {
        StructureReader ins = new TextStructureReader(new BufferedReader(new FileReader(path)));

        attributes = Attributes.read(ins);

        while (true) {
            String classname = ins.readString();

            if (classname == null) // EOF?
                break;

            Object obj = ReflectUtil.createObject(classname);

            if (obj instanceof GNode) {
                GNode gn = (GNode) obj;

                ins.blockBegin();
                gn.read(ins);
                nodes.add(gn);
                ins.blockEnd();

            } else if (obj instanceof GEdge) {
                GEdge ge = (GEdge) obj;
                ins.blockBegin();
                ge.read(ins);
                edges.add(ge);
                ins.blockEnd();
            } else {
                System.out.println("Unable to handle object of type: "+obj);
            }
        }

        System.out.printf("loaded %d nodes and %d edges\n", nodes.size(), edges.size());

        ins.close();
    }

    // Returns a deep copy of this graph with the exception of a
    // node's attributes.
    public Graph copy()
    {
        return copy(null);
    }

    // Copies the contents of this graph into graph g if g is null, a
    // new graph is created, otherwise, all containers in g are
    // emptied forcibly
    public Graph copy(Graph g)
    {
        if (g == null)
            g = new Graph();
        else {
            g.nodes.clear();
            g.edges.clear();
            g.stateIndices.clear();
        }

        for (GEdge edge : edges)
            g.edges.add(edge.copy());
        for (GNode node: nodes)
            g.nodes.add(node.copy());
        for (Integer index : stateIndices)
            g.stateIndices.add(index);

        return g;
    }

    // Copies the entire node state from graph g into this one
    // Use this method when copying the graph to perform thread safe optimization
    // NOTE: it must be ensured externally that the ith node in each graph
    // is of identical type (e.g. GXYTNode)
    public void setNodeState(Graph g)
    {
        assert(nodes.size() >= g.nodes.size());

        for (int i = 0; i < g.nodes.size(); i++) {
            GNode src  = g.nodes.get(i);
            GNode dest = nodes.get(i);
            System.arraycopy(src.state, 0, dest.state, 0, src.state.length);
        }
    }

    public ErrorStats getErrorStats()
    {
        ErrorStats estats = new ErrorStats();

        int stateDOF = 0;
        int edgeDOF = 0;

        for (GEdge edge: edges) {
            estats.chi2 += edge.getChi2(this);
            edgeDOF += edge.getDOF();
        }

        for (GNode node: nodes) {
            stateDOF += node.getDOF();
        }

        estats.chi2normalized = estats.chi2 / (edgeDOF - stateDOF);

        ArrayList<double[]> xs = new ArrayList<double[]>();
        ArrayList<double[]> ys = new ArrayList<double[]>();

        for (GNode n : nodes) {
            if (n.truth != null) {
                xs.add(n.state);
                ys.add(n.truth);
            }
        }

        double t[] = AlignPoints2D.align(xs, ys);

        ArrayList<double[]> xts = LinAlg.transform(t, xs);

        double xyDist2 = 0;
        double thetaDist2 = 0;

        for (int i = 0; i < xts.size(); i++) {
            double x[] = xts.get(i);
            double y[] = ys.get(i);

            double xyDistSq = LinAlg.squaredDistance(x, y, 2);
            xyDist2 += xyDistSq;

            if (x.length == 3) {
                double thetaErr = MathUtil.mod2pi(y[2] - x[2] - t[2]);
                thetaDist2 += thetaErr*thetaErr;
            }
        }

        estats.meanSquaredDistanceError = xyDist2 / xs.size();
        estats.meanSquaredThetaError = thetaDist2 / xs.size();

        return estats;
    }
}
