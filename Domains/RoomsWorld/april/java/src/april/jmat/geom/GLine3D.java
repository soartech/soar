package april.jmat.geom;

import java.io.*;
import java.util.*;

import april.jmat.*;

/** A 3D line **/
public class GLine3D
{
    final static long serialVersionUID=1001;

    /** Unit vector **/
    double dir[];

    /** a point on the line **/
    double p[];

    public GLine3D(double p0[], double p1[])
    {
        p = LinAlg.copy(p0);
        dir = LinAlg.normalize(LinAlg.subtract(p1, p0));
    }

    public double[] pointOnLineClosestTo(double pin[])
    {
        double dotprod = LinAlg.dotProduct(dir, LinAlg.subtract(pin, p));

        return LinAlg.add(p, LinAlg.scale(dir, dotprod));
    }

    /** Where does this line pierce the XY plane? **/
    public double[] intersectPlaneXY()
    {
        if (dir[2] == 0)
            return LinAlg.copy(p);

        double alpha = p[2]/dir[2];

        return new double[] { p[0] - dir[0]*alpha,
                              p[1] - dir[1]*alpha };
    }
    /*
      public String toString()
      {
      return String.format("(%8.3f,%8.3f,%8.3f) (%8.3f,%8.3f,%8.3f)",
      p0.get(0,0), p0.get(1,0), p0.get(2,0), p0.get(3,0),
      p1.get(0,0), p1.get(1,0), p1.get(2,0), p1.get(3,0));
      }
    */

    public double[] getPointOnLine()
    {
        return p;
    }

    public double[] getDirection()
    {
        return dir;
    }

    public double getLineCoordinate(double qp[])
    {

        return (  (qp[0] - p[0])*dir[0] +
                  (qp[1] - p[1])*dir[1] +
                  (qp[2] - p[2])*dir[2] );
    }

    public double[] getPointOfCoordinate(double coord)
    {
        return new double[] {p[0] + coord*dir[0],
                             p[1] + coord*dir[1],
                             p[2] + coord*dir[2] };
    }
}
