package april.jmat;

import java.util.*;

/** Default "dense" vector implementation backed by a simple array. **/
public class DenseVec extends Vec
{
    double v[];

    public DenseVec(int length)
    {
        this.v = new double[length];
    }

    public DenseVec(double v[])
    {
        this.v = v;
    }

    public final Vec copy()
    {
        DenseVec X = new DenseVec(v.length);
        for (int i = 0; i < v.length; i++)
            X.v[i] = v[i];
        return X;
    }

    public final double[] copyArray()
    {
        return LinAlg.copy(v);
    }

    public final Vec copy(int i0, int i1)
    {
        DenseVec X = new DenseVec(i1 - i0 + 1);
        for (int i = 0; i < X.v.length; i++)
            X.v[i] = v[i0+i];
        return X;
    }

    public final Vec copyPart(int i0, int i1)
    {
        DenseVec X = new DenseVec(v.length);
        for (int i = i0; i <= i1; i++)
            X.v[i] = v[i];
        return X;
    }

    public final void resize(int newlength)
    {
        double newv[] = new double[newlength];
        for (int i = 0; i < Math.min(newlength, v.length); i++)
            newv[i] = v[i];
        v = newv;
    }

    public final double[] getDoubles()
    {
        return v;
    }

    public final int size()
    {
        return v.length;
    }

    public final int getNz()
    {
        return v.length;
    }

    public final double get(int idx)
    {
        return v[idx];
    }

    public final void set(int idx, double value)
    {
        v[idx] = value;
    }

    public final double dotProduct(Vec r)
    {
        if (r.getNz() < getNz())
            return r.dotProduct(this);

        double acc = 0;
        for (int i = 0; i < v.length; i++) {
            acc += v[i]*r.get(i);
        }

        return acc;
    }

    public final double dotProduct(Vec r, int i0, int i1)
    {
        if (r.getNz() < getNz())
            return r.dotProduct(this, i0, i1);

        double acc = 0;
        for (int i = i0; i <= i1; i++) {
            acc += v[i]*r.get(i);
        }

        return acc;
    }

    public final void timesEquals(double scale)
    {
        for (int i = 0; i < v.length; i++)
            v[i] *= scale;
    }

    public final void timesEquals(double scale, int i0, int i1)
    {
        for (int i = i0; i <= i1; i++)
            v[i] *= scale;
    }

    public final void transposeAsColumn(Matrix A, int col)
    {
        for (int i = 0; i < v.length; i++)
            A.set(i,col, v[i]);
    }

    public final void transposeAsColumn(Matrix A, int col, int i0, int i1)
    {
        for (int i = i0; i <= i1; i++)
            A.set(i,col, v[i]);
    }

    public final void addTo(Vec r, double scale)
    {
        for (int i = 0; i < v.length; i++)
            r.set(i, r.get(i) + v[i]*scale);
    }

    public final void addTo(Vec r, double scale, int i0, int i1)
    {
        for (int i = i0; i <= i1; i++)
            r.set(i, r.get(i) + v[i]*scale);
    }

    public final void clear()
    {
        for (int i = 0; i < v.length; i++)
            v[i] = 0;
    }

    public final double normF()
    {
        double acc = 0;
        for (int i = 0; i < v.length; i++)
            acc += v[i]*v[i];

        return acc;
    }

    public final Vec copyPermuteColumns(Permutation p)
    {
        DenseVec X = new DenseVec(v.length);

        for (int i = 0; i < v.length; i++) {
            X.set(i, v[p.perm[i]]);
        }

        return X;
    }
}
