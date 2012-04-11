package april.util;

import java.util.*;

/** Max heap that holds integers. **/
public final class IntMaxHeap
{
    int objs[];
    double scores[];
    int    heapsize;

    public IntMaxHeap()
    {
        this(10);
    }

    public IntMaxHeap(int initialsize)
    {
        objs = new int[initialsize];
        scores = new double[initialsize];
        heapsize = 0;
    }

    /** Add a node to the heap, in O(log n) time. **/
    public void add(int o, double score)
    {
        // grow?
        if (heapsize == objs.length) {
            int objs2[] = new int[objs.length * 2];
            double scores2[] = new double[objs.length * 2];

            for (int i = 0; i < objs.length; i++) {
                objs2[i] = objs[i];
                scores2[i] = scores[i];
            }

            objs = objs2;
            scores = scores2;
        }

        objs[heapsize] = o;
        scores[heapsize] = score;
        int node = heapsize;
        heapsize++;

        do {
            node = (node - 1) / 2;
            fixup(node);
        } while (node != 0);
    }

    public int size()
    {
        return heapsize;
    }

    /** Remove and return the element with maximum score in O(log n) time.
     * Return Integer.MIN_VALUE if no elements
     **/
    public int removeMax()
    {
        if (heapsize == 0)
            return Integer.MIN_VALUE;
        int m = objs[0];
        objs[0] = objs[heapsize - 1];
        scores[0] = scores[heapsize - 1];
        objs[heapsize - 1] = Integer.MIN_VALUE;
        heapsize--;
        fixup(0);
        return (int) m;
    }

    public int peekMax()
    {
        if (heapsize == 0)
            return Integer.MIN_VALUE;
        return (int) objs[0];
    }

    public IntHeapPair peekMaxPair()
    {
        if (heapsize == 0)
            return null;
        IntHeapPair p = new IntHeapPair();
        p.o = (int) objs[0];
        p.score = scores[0];
        return p;
    }

    /** same as removeMax, but returns the object and its score **/
    public IntHeapPair removeMaxPair()
    {
        if (heapsize == 0)
            return null;
        int m = objs[0];
        double s = scores[0];
        objs[0] = objs[heapsize - 1];
        scores[0] = scores[heapsize - 1];
        objs[heapsize - 1] = Integer.MIN_VALUE;
        heapsize--;
        fixup(0);
        IntHeapPair p = new IntHeapPair();
        p.o = (int) m;
        p.score = s;
        return p;
    }

    final void swapNodes(int a, int b)
    {
        int t = objs[a];
        objs[a] = objs[b];
        objs[b] = t;
        double s = scores[a];
        scores[a] = scores[b];
        scores[b] = s;
    }

    final void fixup(int parent)
    {
        int left = parent * 2 + 1;
        int right = left + 1;

        // leaf node. exit.
        if (left >= heapsize)
            return;

        // only left node is valid
        if (right >= heapsize) {
            // do we need to swap the parent and the leaf?
            // we don't need to call fixupHeap recursively since the
            // left node must be a leaf.
            if (scores[left] > scores[parent])
            {
                swapNodes(left, parent);
                return;
            }
            return;
        }

        // general node case: both right and left are valid nodes.
        // if parent is the maximum, we're done.
        if (scores[parent] > scores[left] && scores[parent] > scores[right])
            return;

        // parent is less than either the left, right, or both
        // children
        if (scores[left] > scores[right]) {
            swapNodes(left, parent);
            fixup(left);

        } else {

            swapNodes(right, parent);
            fixup(right);
        }
    }

    public static void main(String args[])
    {
        int cap = 1000;
        int sz = 0;
        int vals[] = new int[cap];
        IntMaxHeap heap = new IntMaxHeap();
        Random r = new Random(0);
        int maxsz = 0;
        int zcnt = 0;

        for (int iter = 0; iter < 1000000; iter++) {

            double p = r.nextDouble();
            if (p < .5 && sz < cap) {
                // System.out.print("+");
                // add a value?
                int v = r.nextInt(1 << 16);
                vals[sz] = v;
                heap.add(Integer.MIN_VALUE, v);
                sz++;

            } else if (sz > 0) {

                // System.out.print("-");
                // remove a value
                int maxv = -1, maxi = -1;
                for (int i = 0; i < sz; i++) {
                    if (vals[i] > maxv) {
                        maxv = vals[i];
                        maxi = i;
                    }
                }
                vals[maxi] = vals[sz - 1];
                int hv = (int) heap.removeMaxPair().score;
                if (hv != maxv)
                    System.out.println("error");
                sz--;
            } else {
                // System.out.print("?");
            }

            if (sz > maxsz)
                maxsz = sz;
            // count how many times we've returned to zero.
            if (maxsz > 0 && sz == 0)
                zcnt++;
            System.out.flush();
        }
        System.out.println("max size: " + maxsz + " zcount: " + zcnt);
    }
}
