package april.graph;

import april.jmat.*;
import april.util.*;

import java.util.*;

public class GaussSeidelSolver implements GraphSolver
{
    Graph g;
    int preprocessedEdges;

    // for each node, a list of the edges that are attached to it
    ArrayList<ArrayList<Integer>> nodeEdges = new ArrayList<ArrayList<Integer>>();
    ArrayList<Linearization> linearizations = new ArrayList<Linearization>();

    DoublesCache cache = new DoublesCache(8, 4);

    public GaussSeidelSolver(Graph g)
    {
        this.g = g;
    }

    public boolean canIterate()
    {
        return true;
    }

    public void iterate()
    {
        while (nodeEdges.size() < g.nodes.size()) {
            nodeEdges.add(new ArrayList<Integer>());
        }

        while (preprocessedEdges < g.edges.size()) {
            GEdge ge = g.edges.get(preprocessedEdges);

            for (int i = 0; i < ge.nodes.length; i++)
                nodeEdges.get(ge.nodes[i]).add(preprocessedEdges);

            linearizations.add(g.edges.get(preprocessedEdges).linearize(g, null));

            preprocessedEdges++;
        }

        for (int i = 0; i < g.nodes.size(); i++) {
            relaxNode(i);
        }
    }

    public void iterate(int node)
    {
        while (nodeEdges.size() < g.nodes.size()) {
            nodeEdges.add(new ArrayList<Integer>());
        }

        while (preprocessedEdges < g.edges.size()) {
            GEdge ge = g.edges.get(preprocessedEdges);

            for (int i = 0; i < ge.nodes.length; i++)
                nodeEdges.get(ge.nodes[i]).add(preprocessedEdges);

            linearizations.add(g.edges.get(preprocessedEdges).linearize(g, null));

            preprocessedEdges++;
        }

        relaxNode(node);
    }

    void relaxNode(int i)
    {
        GNode gn = g.nodes.get(i);
        int sz = gn.state.length;

        ArrayList<Integer> edges = nodeEdges.get(i);
        if (edges.size() == 0) {
            return;
        }

        double JTWJ[][]     = cache.get(sz, sz);
        LinAlg.clear(JTWJ);
        double JTWR[]       = cache.get(sz);
        LinAlg.clear(JTWR);

        double thisJTWJ[][] = cache.get(sz, sz);
        double thisJTWR[]   = cache.get(sz);

        for (int edgeidx : edges) {
            GEdge ge = g.edges.get(edgeidx);

            Linearization lin = ge.linearize(g, linearizations.get(edgeidx));

            for (int n = 0; n < ge.nodes.length; n++) {
                if (ge.nodes[n] == i) {

                    double J[][] = lin.J.get(n);

                    double thisJTW[][] = cache.get(J[0].length, lin.W[0].length);
                    LinAlg.matrixAtB(J, lin.W, thisJTW);

                    LinAlg.matrixAB(thisJTW, J, thisJTWJ);
                    LinAlg.plusEquals(JTWJ, thisJTWJ);

                    LinAlg.matrixAB(thisJTW, lin.R, thisJTWR);
                    LinAlg.plusEquals(JTWR, thisJTWR);
                    cache.put(thisJTW);

                    // assume nodes only appear once in each GEdge.nodes
                    break;
                }
            }
        }

        double invJTWJ[][] = cache.get(sz, sz);
        double dx[] = cache.get(sz);

        LinAlg.inverse(JTWJ, invJTWJ);
        LinAlg.matrixAB(invJTWJ, JTWR, dx);
        LinAlg.minusEquals(gn.state, dx);

        cache.put(invJTWJ);
        cache.put(dx);
        cache.put(thisJTWJ);
        cache.put(thisJTWR);
        cache.put(JTWJ);
        cache.put(JTWR);
    }
}
