package april.jmat.geom;

import java.util.*;
import april.jmat.*;

/** A simple (non-instersecting) and PLANAR polygon, that happens to
    be in 3D. Basically, we project these points into a plane and use
    the 2D implementation for most of the heavy lifting. No attempt is
    made to ensure that the points are co-planar: this is the
    responsibility of the user.
**/
public class Polygon3D
{
    Polygon poly2d;

    public Polygon3D(ArrayList<double[]> _points)
    {
        double p0[] = _points.get(0);
        double p1[] = _points.get(1);
        double p2[] = _points.get(2);

        // u0 and u1 will be basis vectors used to project the 3D
        // points into a plane.
        double u0[] = LinAlg.normalize(LinAlg.subtract(p1, p0));
        double u1[] = LinAlg.subtract(p2, p1);
        double dot = LinAlg.dotProduct(u0, u1);
        for (int i = 0; i < 3; i++)
            u1[i] -= dot*u0[i];

        // project the points.
        ArrayList<double[]> points2d = new ArrayList<double[]>();
        for (double p3d[] : _points) {
            double p2d[] = new double[] { p3d[0]*u0[0] + p3d[1]*u0[1] + p3d[2]*u0[2],
                                          p3d[0]*u1[0] + p3d[1]*u1[1] + p3d[2]*u1[2] };
            points2d.add(p2d);
        }

        poly2d = new Polygon(points2d);
    }

    public ArrayList<int[]> getTriangles()
    {
        return poly2d.getTriangles();
    }

    public static void main(String args[])
    {
        ArrayList<double[]> points = new ArrayList<double[]>();
        /*
          points.add(new double[] {0,0,0});
          points.add(new double[] {0,1,0});
          points.add(new double[] {1,1,0});
          points.add(new double[] {1,0,0});
        */

        points.add(new double[] {0,0,0});
        points.add(new double[] {0,0,1});
        points.add(new double[] {0,1,1});
        points.add(new double[] {0,1,0});

        Polygon3D p = new Polygon3D(points);

    }
}
