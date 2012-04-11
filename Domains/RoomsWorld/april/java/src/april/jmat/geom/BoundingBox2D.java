package april.jmat.geom;

/** Track the bounds of points in R^2 **/
public class BoundingBox2D
{
    double p0[] = new double[2]; // the minimum coordinate
    double p1[] = new double[2]; // the maximum coordinate

    public BoundingBox2D()
    {
        for (int i = 0; i < p0.length; i++) {
            p0[i] = Double.MAX_VALUE;
            p1[i] = -Double.MAX_VALUE;
        }
    }

    public void update(double p[])
    {
        for (int i = 0; i < p0.length; i++) {
            p0[i] = Math.min(p0[i], p[i]);
            p1[i] = Math.max(p0[i], p[i]);
        }
    }

    public double[] getSize()
    {
        double sz[] = new double[p0.length];
        for (int i = 0; i < p0.length; i++)
            sz[i] = p1[i] - p0[i];
        return sz;
    }
}
