package april.graph;

import java.util.*;

import april.jmat.*;
import april.util.*;

/** Given a graph and a "reference node", search for the shortest
 * (least uncertain) path from the reference node to all other nodes.
 * This is often a fair approximation to the optimal graph, but can
 * be computed very quickly.
 *
 * The approximation is poor only when a path segment has a number of
 * equal-variance links in parallel, with no low-variance links. In
 * this case, the variance should decrease as 1/N with N parallel
 * links.
 *
 * This is implemented as a breadth-first search, which is in fact
 * optimal.
 **/

public class DijkstraProjection
{
    int refpose;

    // The best edge to each node in the graph from refpos
    ArrayList<GXYTEdge> projection;

    // For each node in the graph, all of the edges
    // that lead away from it.
    ArrayList<ArrayList<GXYTEdge>> nodeEdges;

    // we won't follow these edges.
    HashSet<GEdge> forbiddenEdges;

    // We'll stop when we've found paths to all of these nodes. (if
    // null, all nodes are processed.)
    HashSet<Integer> neededNodes;

    MaxHeap<GXYTEdge> heap;

    public DijkstraProjection(Graph g, int refpose)
    {
        this(g, refpose, null, null);
    }

    /** neededNodes will be modified! **/
    public DijkstraProjection(Graph g, int refpose,
                              HashSet<GEdge> forbiddenEdges,
                              HashSet<Integer> neededNodes)
    {
        this.refpose = refpose;
        this.forbiddenEdges = forbiddenEdges;
        this.neededNodes = neededNodes;

        ///////////////////////////////////////////////////////////
        // Build a table of edges for each node.
        // We'll use this for the breadth-first search
        nodeEdges = new ArrayList<ArrayList<GXYTEdge>>(g.nodes.size());
        while (nodeEdges.size() < g.nodes.size())
            nodeEdges.add(new ArrayList<GXYTEdge>());

        // we handle GXYTPosEdges by introducing an additional
        // GXYTNode which connects those edges. We add this node to
        // the end of the graph.
        int posidx = g.nodes.size(); // index of our "extra" node.
        nodeEdges.add(new ArrayList<GXYTEdge>());

        for (GEdge _ge : g.edges) {
            if (_ge instanceof GXYTEdge) {
                GXYTEdge ge = (GXYTEdge) _ge;
                nodeEdges.get(ge.nodes[0]).add(ge);
                nodeEdges.get(ge.nodes[1]).add(ge);
            } else if (_ge instanceof GXYTPosEdge) {
                GXYTPosEdge gpe = (GXYTPosEdge) _ge;
                GXYTEdge ge = new GXYTEdge();
                ge.nodes = new int[] { posidx, gpe.nodes[0] }; // XXX backwards???
                ge.z = gpe.z;
                ge.P = gpe.P;
                nodeEdges.get(gpe.nodes[0]).add(ge);
                nodeEdges.get(posidx).add(ge);
            }
        }

        ///////////////////////////////////////////////////////////
        // allocate the projection datastructure

        projection = new ArrayList<GXYTEdge>();
        while (projection.size() < nodeEdges.size())
            projection.add(null);

        heap = new MaxHeap<GXYTEdge>(nodeEdges.size());

        ///////////////////////////////////////////////////////////
        // Initialize the search by expanding the first node
        GXYTEdge initialEdge = new GXYTEdge();
        initialEdge.z = new double[3];
        initialEdge.truth = new double[3];
        initialEdge.P = new double[3][3];
        initialEdge.nodes = new int[] { refpose, refpose };

        projection.set(refpose, initialEdge);
        if (neededNodes != null)
            neededNodes.remove(refpose);

        expand(refpose);

        ///////////////////////////////////////////////////////////
        // keep searching.
        while (heap.size() > 0) {
            GXYTEdge ge = heap.removeMax();

            // every edge must be with respect to the reference pose...
            assert(ge.nodes[0] == refpose);

            // already have a (better) projection for this node?
            // if so, skip this one.
            if (projection.get(ge.nodes[1]) != null)
                continue;

            // this edge is a keeper, let's expand from here.
            projection.set(ge.nodes[1], ge);

            if (neededNodes != null) {
                neededNodes.remove(ge.nodes[1]);
                if (neededNodes.size() == 0)
                    return;
            }

            expand(ge.nodes[1]);
        }

        // clean up to help the garbage collector
        heap = null;
        nodeEdges = null;
    }

    void expand(int pose)
    {
        GXYTEdge current = projection.get(pose);

        for (GXYTEdge ge : nodeEdges.get(pose)) {

            if (forbiddenEdges!=null && forbiddenEdges.contains(ge))
                continue;

            // invert the edge if it's not going from 'pose' to some other node
            if (ge.nodes[1] == pose)
                ge = ge.invert();

            // don't need this edge if we've already been there, skip it.
            if (projection.get(ge.nodes[1])!=null)
                continue;

            GXYTEdge newEdge = current.compose(ge);
            heap.add(newEdge, 1.0/(1.0 + LinAlg.det(newEdge.P)));
        }
    }

    public GXYTEdge getEdge(int node)
    {
        return projection.get(node);
    }
}
