package april.jmat.geom;

import april.jmat.*;
import java.util.*;

/** A sphere in R^3 **/
public class GSphere
{
    double C[];
    double r;

    public GSphere(double C[], double r)
    {
        this.C = LinAlg.copy(C);
        this.r = r;
    }

    public ArrayList<double[]> getIntersections(GLine3D line)
    {
        ArrayList<double[]> points = new ArrayList<double[]>();

        double P[] = line.getPointOnLine();
        double D[] = line.getDirection();

        // solve for: | P + tD - C |^2 - r^2 = 0
        double M[] = LinAlg.subtract(P, C);
        double DdotM = LinAlg.dotProduct(D, M);
        double MdotM = LinAlg.dotProduct(M, M);
        double t0 = -DdotM + Math.sqrt(DdotM*DdotM - (MdotM - r*r));
        double t1 = -DdotM - Math.sqrt(DdotM*DdotM - (MdotM - r*r));

        if (!Double.isNaN(t0))
            points.add(LinAlg.add(C, LinAlg.scale(D, t0)));

        if (!Double.isNaN(t1))
            points.add(LinAlg.add(C, LinAlg.scale(D, t1)));

        return points;
    }
}
