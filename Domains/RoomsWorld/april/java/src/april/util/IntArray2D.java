package april.util;

/** A two dimensional array implemented as a single big array. **/
public class IntArray2D
{
    public int vs[];
    public int dim1, dim2;

    public IntArray2D(int dim1, int dim2)
    {
        this.dim1 = dim1;
        this.dim2 = dim2;

        vs = new int[dim1*dim2];
    }

    public final int get(int d1, int d2)
    {
        return vs[d1*dim2 + d2];
    }

    public final void set(int d1, int d2, int v)
    {
        vs[d1*dim2 + d2] = v;
    }

    public final void plusEquals(int d1, int d2, int v)
    {
        vs[d1*dim2 + d2] += v;
    }
}
