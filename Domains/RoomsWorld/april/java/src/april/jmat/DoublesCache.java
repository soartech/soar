package april.jmat;

import java.util.*;

public final class DoublesCache
{
    int maxdim;
    int maxcapacity;

    ArrayList<ArrayList<double[]>> cache;
    ArrayList<ArrayList<double[][]>> cache2;

    boolean debug = false;

    public DoublesCache(int maxdim, int maxcapacity)
    {
        this.maxdim = maxdim;
        this.maxcapacity = maxcapacity;

        cache = new ArrayList<ArrayList<double[]>>();
        for (int i = 0; i < maxdim; i++)
            cache.add(new ArrayList<double[]>());

        cache2 = new ArrayList<ArrayList<double[][]>>();
        for (int i = 0; i < maxdim*maxdim; i++)
            cache2.add(new ArrayList<double[][]>());
    }

    public double[] get(int d)
    {
        if (debug)
            return new double[d];

        ArrayList<double[]> ds = cache.get(d);
        int sz = ds.size();
        if (sz == 0)
            return new double[d];
        double v[] = ds.remove(sz-1);
        return v;
    }

    public void put(double v[])
    {
        ArrayList<double[]> ds = cache.get(v.length);
        if (ds.size() < maxcapacity)
            ds.add(v);
    }

    public double[][] get(int d1, int d2)
    {
        if (debug)
            return new double[d1][d2];

        int idx = d1*maxdim + d2;
        ArrayList<double[][]> ds = cache2.get(idx);
        int sz = ds.size();
        if (sz == 0)
            return new double[d1][d2];

        double v[][] = ds.remove(sz-1);
        return v;
    }

    public void put(double v[][])
    {
        int d1 = v.length, d2 = v[0].length;
        int idx = d1*maxdim + d2;

        ArrayList<double[][]> ds = cache2.get(idx);
        if (ds.size() < maxcapacity)
            ds.add(v);
    }
}
