package april.util;

import java.util.*;

/** Implementation of disjoint set data structure that packs each
 * entry into a single array of 'int' for performance. *
 */
public final class UnionFindSimple
{
    int data[]; // alternating arent ids, rank, size.

    static final int SZ = 2;

    /** @param maxid The maximum node id that will be referenced. **/
    public UnionFindSimple(int maxid)
    {
        data = new int[maxid*SZ];

        for (int i = 0; i < maxid; i++) {
            // everyone is their own cluster of size 1
            data[SZ*i+0] = i;
            data[SZ*i+1] = 1;
        }
    }

    public int getSetSize(int id)
    {
        return data[SZ*getRepresentative(id)+1];
    }

    public int getRepresentative(int id)
    {
        // terminal case: a node is its own parent.
        if (data[SZ*id]==id)
            return id;

        // otherwise, recurse...
        int root = getRepresentative(data[SZ*id]);

        // short circuit the path.
        data[SZ*id] = root;

        return root;
    }

    /** returns the id of the merged node. **/
    public int connectNodes(int aid, int bid)
    {
        int aroot = getRepresentative(aid);
        int broot = getRepresentative(bid);

        if (aroot == broot)
            return aroot;

        int asz = data[SZ*aroot+1];
        int bsz = data[SZ*broot+1];

        if (asz > bsz) {
            data[SZ*broot] = aroot;
            data[SZ*aroot+1] += bsz;
            return aroot;
        } else {
            data[SZ*aroot] = broot;
            data[SZ*broot+1] += asz;
            return broot;
        }
    }

    public static void main(String args[])
    {
        int nedges = 100000;
        int nnodes = 1000;

        UnionFindSimple uf = new UnionFindSimple(nnodes);

        ArrayList<int[]> edges = new ArrayList<int[]>();
        Random r = new Random();

        for (int i = 0; i < nedges; i++) {
            int a = r.nextInt(nnodes);
            int b = r.nextInt(nnodes);

            edges.add(new int[] {a, b});

            uf.connectNodes(a, b);
        }

        System.out.println("");

        for (int a = 0; a < nnodes; a++) {

            // construct set of all reachable nodes.
            HashSet<Integer> reachable = new HashSet<Integer>();
            reachable.add(a);

            while (true) {
                int size0 = reachable.size();

                for (int edge[] : edges) {
                    if (reachable.contains(edge[0])) {
                        reachable.add(edge[1]);
                    }
                    if (reachable.contains(edge[1])) {
                        reachable.add(edge[0]);
                    }
                }

                if (reachable.size() == size0)
                    break;
            }

            for (int b = 0; b < nnodes; b++) {
                if (reachable.contains(b))
                    assert(uf.getRepresentative(a)==uf.getRepresentative(b));
                else
                    assert(uf.getRepresentative(a)!=uf.getRepresentative(b));
            }

            assert (reachable.size() == uf.getSetSize(a));
        }
    }
}
