package april.jmat.ordering;

public final class Node
{
    /**  an un-ordered list of neighbors. 16 is an initial capacity **/
    public int neighbors[] = new int[16];
    public int nneighbors;

    /** self index: what is the index of this node? **/
    public int sidx;

    /** For use by our creator. No internal function.**/
    public boolean used;
    /** For use by our creator. No internal function.**/
    public int cost;

    public Node(int sidx)
    {
        this.sidx = sidx;
    }

    /** Ensure that we can store at least n neighbors. **/
    final void ensureCapacity(int mincapacity)
    {
        if (mincapacity < neighbors.length)
            return;

        int newsz = neighbors.length;
        while (newsz < mincapacity)
            newsz *= 2;

        int newneighbors[] = new int[newsz];
        System.arraycopy(neighbors, 0, newneighbors, 0, nneighbors);
        neighbors = newneighbors;
    }

    /** n must not already be a neighbor. **/
    public final void addNeighbor(int n)
    {
        ensureCapacity(nneighbors + 1);

        neighbors[nneighbors++] = n;
    }

    /**  how many neighbors? **/
    public final int size()
    {
        return nneighbors;
    }

    public final int removeNodeAndAddNeighborsDryRun(Node node, int maxnodes, IntHashSet tmpset)
    {
        // allocate enough room for the worst case.
        int maxsize = Math.min(maxnodes, node.nneighbors + nneighbors); // Math.min(node.nneighbors + nneighbors, nodes.length);
        ensureCapacity(maxsize);

        tmpset.reset();

        // disallow any neighbor nodes corresponding to either
        // our own sidx or node.sidx.
        tmpset.ban(sidx);
        tmpset.ban(node.sidx);

        int fillin = 0;

        //////////////////////////////////
        // populate the hash set with our already-known neighbors. As
        // we process nodes, we will remove node.sidx.
        for (int inpos = 0; inpos < nneighbors; inpos++) {
            tmpset.add(neighbors[inpos]);
        }

        //////////////////////////////////
        // add the new neighbors.
        for (int inpos = 0; inpos < node.nneighbors; inpos++) {
            if (tmpset.add(node.neighbors[inpos]))
                fillin++;
        }

        return fillin;
    }

    /** Add all of the neighbors of 'node' to our set of neighbors,
        and remove 'node'. This is used during marginalization.

        tmpset must have ensureCapacity of nnodes. Will be modified.
    **/
    public final void removeNodeAndAddNeighbors(Node node, int maxnodes, IntHashSet tmpset)
    {
        // allocate enough room for the worst case.
        int maxsize = Math.min(maxnodes, node.nneighbors + nneighbors); // Math.min(node.nneighbors + nneighbors, nodes.length);
        ensureCapacity(maxsize);

        tmpset.reset();

        // disallow any neighbor nodes corresponding to either
        // our own sidx or node.sidx.
        tmpset.ban(sidx);
        tmpset.ban(node.sidx);

        if (false) {
            //////////////////////////////////
            // populate the hash set with our already-known neighbors. As
            // we process nodes, we will remove node.sidx.
            int rmpos = -1;

            for (int inpos = 0; inpos < nneighbors; inpos++) {
                if (neighbors[inpos] == node.sidx)
                    rmpos = inpos;
                tmpset.add(neighbors[inpos]);
            }

            // shuffle remove node.sidx at position rmpos.
            neighbors[rmpos] = neighbors[nneighbors-1];
            nneighbors--;

            //////////////////////////////////
            // add the new neighbors.
            for (int inpos = 0; inpos < node.nneighbors; inpos++) {
                if (tmpset.add(node.neighbors[inpos]))
                    neighbors[nneighbors++] = node.neighbors[inpos];
            }
        } else {

            //////////////////////////////////
            // populate the hash set with our already-known neighbors. As
            // we process nodes, we will remove node.sidx.
            for (int inpos = 0; inpos < nneighbors; inpos++) {
                tmpset.add(neighbors[inpos]);
            }

            //////////////////////////////////
            // add the new neighbors.
            for (int inpos = 0; inpos < node.nneighbors; inpos++) {
                tmpset.add(node.neighbors[inpos]);
            }

            int sz = tmpset.getSize();
            ensureCapacity(sz);
            System.arraycopy(tmpset.members, 0, neighbors, 0, sz);
            nneighbors = sz;
        }
    }
}
