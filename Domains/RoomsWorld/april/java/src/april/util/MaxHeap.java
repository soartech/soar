package april.util;

import java.util.*;

/** Max heap **/
public class MaxHeap<T>
{
    Object objs[];
    double scores[];
    int    heapsize;

    public MaxHeap()
    {
        this(10);
    }

    public MaxHeap(int initialsize)
    {
        objs = new Object[initialsize];
        scores = new double[initialsize];
        heapsize = 0;
    }

    public static class Pair<T>
    {
        public T o;
        public double score;
    }

    /** Add a node to the heap, in O(log n) time. **/
    public void add(T o, double score)
    {
        // grow?
        if (heapsize == objs.length) {
            Object objs2[] = new Object[objs.length * 2];
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

    /** Remove and return the element with maximum score in O(log n) time. **/
    public T removeMax()
    {
        if (heapsize == 0)
            return null;
        Object m = objs[0];
        objs[0] = objs[heapsize - 1];
        scores[0] = scores[heapsize - 1];
        objs[heapsize - 1] = null;
        heapsize--;
        fixup(0);
        return (T) m;
    }

    public T peekMax()
    {
        if (heapsize == 0)
            return null;
        return (T) objs[0];
    }

    public boolean selfTest()
    {
        for (int i = 0; i < heapsize; i++) {
            if (scores[i] > scores[0])
                return false;
        }

        return true;
    }

    public Pair<T> peekMaxPair()
    {
        if (heapsize == 0)
            return null;
        Pair<T> p = new Pair<T>();
        p.o = (T) objs[0];
        p.score = scores[0];
        return p;
    }

    /** same as removeMax, but returns the object and its score **/
    public Pair<T> removeMaxPair()
    {
        if (heapsize == 0)
            return null;
        Object m = objs[0];
        double s = scores[0];
        objs[0] = objs[heapsize - 1];
        scores[0] = scores[heapsize - 1];
        objs[heapsize - 1] = null;
        heapsize--;
        fixup(0);
        Pair<T> p = new Pair<T>();
        p.o = (T) m;
        p.score = s;
        return p;
    }

    final void swapNodes(int a, int b)
    {
        Object t = objs[a];
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
            if (scores[left] > scores[parent]) {
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
        MaxHeap heap = new MaxHeap();
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
                heap.add(null, v);
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
