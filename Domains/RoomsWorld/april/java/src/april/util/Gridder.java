package april.util;

import java.util.*;
import april.jmat.*;

/** A lookup table in 2D for implementing nearest neighbor **/
public class Gridder<T>
{
    static class Cell
    {
        Object o;
        Cell next;
    }

    Cell cells[][];
    double x0, y0, x1, y1;
    int width, height;
    double metersPerCell;

    public Gridder(double x0, double y0, double x1, double y1, double metersPerCell)
    {
        this.x0 = x0;
        this.y0 = y0;
        this.metersPerCell = metersPerCell;

        width = (int) ((x1 - x0)/metersPerCell + 1);
        height = (int) ((y1 - y0)/metersPerCell + 1);

        this.x1 = x0 + metersPerCell*width;
        this.y1 = y0 + metersPerCell*height;

        cells = new Cell[height][width];
    }

    public void add(double x, double y, T o)
    {
        int ix = (int) ((x - x0)/metersPerCell);
        int iy = (int) ((y - y0)/metersPerCell);

        if (ix >=0 && iy >=0 && ix < width && iy < height) {
            Cell c = new Cell();
            c.o = o;
            c.next = cells[iy][ix];
            cells[iy][ix] = c;
        }
    }

    public void remove(double x, double y, T o)
    {
        int ix = (int) ((x - x0)/metersPerCell);
        int iy = (int) ((y - y0)/metersPerCell);

        if (ix >=0 && iy >=0 && ix < width && iy < height) {

            // nobody in this bucket!
            if (cells[iy][ix] == null)
                return;

            if (cells[iy][ix].o == o) {
                // The first one is our guy. Just skip over it.
                cells[iy][ix] = cells[iy][ix].next;
            } else {
                // The first guy is okay. Crawl through the list.
                Cell c = cells[iy][ix];
                Cell nc = c.next;
                c.next = null;

                while (nc != null) {
                    if (nc.o != o) {
                        Cell tmp = nc.next;
                        nc.next = c;
                        cells[iy][ix] = nc;
                        nc = tmp;
                    } else {
                        nc = nc.next;
                    }
                }
            }
        }
    }

    class MyIterator implements Iterator<T>, Iterable<T>
    {
        int ix0, ix1, iy0, iy1;

        int ix, iy;
        Cell c;

        MyIterator(double x, double y, double range)
	    {
            ix0 = (int) ((x - range - x0)/metersPerCell);
            iy0 = (int) ((y - range - y0)/metersPerCell);

            ix1 = (int) ((x + range - x0)/metersPerCell);
            iy1 = (int) ((y + range - y0)/metersPerCell);

            ix0 = Math.max(0, ix0);
            ix0 = Math.min(width-1, ix0);

            ix1 = Math.max(0, ix1);
            ix1 = Math.min(width-1, ix1);

            iy0 = Math.max(0, iy0);
            iy0 = Math.min(height-1, iy0);

            iy1 = Math.max(0, iy1);
            iy1 = Math.min(height-1, iy1);

            ix = ix0;
            iy = iy0;

            c = cells[iy][ix];
	    }

        void findNext()
        {
            if (c != null)
                c = c.next;

            if (c != null)
                return;

            ix++;
            while (true) {
                if (ix > ix1) {
                    iy++;
                    ix = ix0;
                }
                if (iy > iy1)
                    break;

                c = cells[iy][ix];
                if (c != null)
                    break;
                ix++;
            }
        }

        public boolean hasNext()
        {
            if (c == null)
                findNext();

            return (c != null);
        }

        public T next()
        {
            Cell thisc = c;
            findNext();
            return (T) thisc.o;
        }

        public void remove()
        {
            throw new UnsupportedOperationException();
        }

        public Iterator<T> iterator()
        {
            return this;
        }
    }

    public Iterable<T> find(double x, double y, double range)
    {
        return new MyIterator(x, y, range);
    }

    public static void main(String args[])
    {
        Gridder<double[]> g = new Gridder<double[]>(0, 0, 1, 1, 0.1);
        Random r = new Random();

        ArrayList<double[]> points = new ArrayList<double[]>();

        // populate some data.
        for (int i = 0; i < 1000; i++) {
            double v[] = new double[] { r.nextDouble(), r.nextDouble() };
            points.add(v);
            g.add(v[0], v[1], v);
        }

        for (int i = 0; i < 10000; i++) {
            // our query point.
            double q[] = new double[] { r.nextDouble(), r.nextDouble() };

            // search our ground truth
            double bestDist = Double.MAX_VALUE;
            int bestIndex = -1;

            for (int j = 0; j < points.size(); j++) {
                double dist = LinAlg.distance(q, points.get(j));

                if (dist < bestDist) {
                    bestDist = dist;
                    bestIndex = j;
                }
            }

            double searchRange = r.nextDouble();
            double bestDist2 = Double.MAX_VALUE;
            double best[] = null;

            for (double v[] : g.find(q[0], q[1], searchRange)) {
                double dist = LinAlg.distance(q, v);

                if (dist < bestDist2) {
                    bestDist2 = dist;
                    best = v;
                }
            }

            if (bestDist > searchRange)
                continue;

            assert(best == points.get(bestIndex));
        }
    }
}
