package april.jmat.ordering;

import april.jmat.*;

import java.util.*;

/** This is an exact minimum-degree ordering method to allow
 * comparisons to approximations. Despite being exact, it's actually
 * pretty fast.
 *
 * The neighbors of each variable are maintained in an unordered list.
 * When marginalizing out a node, all its neighbors become fully
 * connected (but lose their reference to the marginalized-out node).
 * Duplicate items are detected (and thus removed) using a hash set.
 *
 * We then repeatedly pick the variable with the lowest degree.
 **/
public class MinimumDegreeOrdering implements Ordering
{
    IntHashSet nodeSet = new IntHashSet();

    Node nodes[];
    int perm[];   // the permutation we're building

    IntHashSet tmpset = new IntHashSet();

    public MinimumDegreeOrdering()
    {
    }

    public int[] getPermutation(Matrix A)
    {
        int m = A.getRowDimension();
        int n = A.getColumnDimension();

        tmpset.ensureCapacity(n);

        nodes = new Node[n];
        perm = new int[n];

        // create graph data structure
        for (int i = 0; i < n; i++) {
            nodes[i] = new Node(i);

            Vec row = A.getRow(i);

            if (row instanceof CSRVec) {
                CSRVec crow = (CSRVec) row;
                for (int j = 0; j < crow.nz; j++)
                    if (crow.indices[j]!=i)
                        nodes[i].addNeighbor(crow.indices[j]);

            } else {
                for (int j = 0; j < m; j++) {
                    if (i!=j && A.get(i,j)!=0)
                        nodes[i].addNeighbor(j);
                }
            }
        }

        //////////////
        // Repeatedly pick the node with lowest degree.
        for (int i = 0; i < n; i++) {

            // find the best node.
            Node bestNode = null;
            int  bestj = -1;

            for (int j = 0; j < n; j++) {
                Node node = nodes[j];
                if (node.used)
                    continue;

                if (bestNode==null || node.size() < bestNode.size()) {
                    bestNode = node;
                    bestj = j;
                }
            }

            perm[i] = bestj;
            bestNode.used = true;

            // marginalize out this node, connecting all of its
            // neighbors.
            for (int k = 0; k < bestNode.nneighbors; k++)
                nodes[bestNode.neighbors[k]].removeNodeAndAddNeighbors(bestNode, nodes.length, tmpset);

            if (false) {
                // this code used to investigate whether supernodes
                // are a common occurence in SLAM problems. Answer: no
                // csw, for example, has a total of 80 variables that
                // could be merged (out of 10500).
                HashMap<Long, ArrayList<Node>> supernodes = new HashMap<Long, ArrayList<Node>>();
                for (int j = 0; j < n; j++) {
                    Node node = nodes[j];
                    if (node.used)
                        continue;

                    // compute hash
                    long hash = 0;
                    for (int nidx = 0; nidx < node.nneighbors; nidx++) {
                        hash = (hash<<7) ^ node.neighbors[nidx];
                    }

                    // look for collisions
                    ArrayList<Node> collisions = supernodes.get(hash);
                    if (collisions == null) {
                        collisions = new ArrayList<Node>();
                        supernodes.put(hash, collisions);
                    }

                    for (Node tn : collisions) {
                        if (node.nneighbors != tn.nneighbors)
                            continue;

                        // do tn and node have equivalent neighbors?
                        // (keep in mind that if they ARE equivalent,
                        // that their neighbor lists won't be the
                        // same.  tn will have node as a neighbor and
                        // vice versa. But neither have self links.)
                        boolean good = true;

                        for (int a = 0; a < node.nneighbors; a++) {

                            // consider each of node's neighbors. tn
                            // must have that node as well, except for
                            // node itself.

                            if (node.neighbors[a] == tn.sidx)
                                continue;

                            boolean hasmatch = false;
                            // one of tn's neighbors must be node.neighbors[a].
                            for (int b = 0; b < tn.nneighbors; b++) {
                                if (tn.neighbors[b] == node.neighbors[a]) {
                                    hasmatch = true;
                                    break;
                                }
                            }
                            if (!hasmatch) {
                                good = false;
                                break;
                            }
                        }

                        if (good) {
                            System.out.printf("%d\n", j);
                        }
                    }

                    collisions.add(node);
                }

            }
        }

        return perm;
    }
}
