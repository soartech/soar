package april.jmat.geom;

import april.jmat.*;

/** a four sided closed, non self-intersecting polygon. **/
public class GQuad2D
{
    double p[][]; // The points in counter-clockwise order.

    /** @param p The points in counter-clockwise order. **/
    public GQuad2D(double p[][])
    {
        assert(p.length==4);
        for (int i = 0; i < 3; i++)
            assert(p[i].length==2);

        this.p = p;
    }

    public double getArea()
    {
        // divide quad into two triangles.
        GTriangle2D t1 = new GTriangle2D(new double[][] { p[0], p[1], p[2] });
        GTriangle2D t2 = new GTriangle2D(new double[][] { p[0], p[2], p[3] });

        return t1.getArea() + t2.getArea();
    }
}
