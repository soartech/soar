package april.jmat.geom;

import java.util.*;
import april.jmat.*;

/*
	Based on B. Horn's "Closed-form solution of absolute
	orientation using unit quaternions". This is a 2D
	implementation, with some modifications. (eolson@mit.edu)

	For the derivation, we'll assume that both point sets are
	centered about their respective centroids. This can be
	"patched up" afterwards if it is not the case. (The
	implementation does not require this.)

	Problem statement:

	Find a rotation R() and a translation t such that:

	y ~= R(x) + t

	e_i = y_i - R(x_i) - t                (error vector)

	cost = SUM { e_i' * e_i }

	cost = SUM { y'y  -  y'R(x)  -  y't
    - R(x)'y  +  R(x)'R(x)  +   R(x)'t
    -  t'y  +  t'R(x)  +  t't         }

    = SUM { y'y + R(x)'R(x) + t't - 2y'R(x) - 2y't + 2t'R(x) }

    -----------------
    TRANSLATION
    -----------------

    dcost/dt = SUM { 2t - 2y + 2R(x) } = 0

    t = SUM { y } - SUM { R(x) }

    Note that SUM { R(x) } == SUM { x } == 0. This yields:

    t = 0

    In other words, the optimal translation between two points is
    the one that aligns their centroids.

    -----------------
    ROTATION
    -----------------

    cost = SUM { y'y + R(x)'R(x) + t't - 2y'R(x) - 2y't + 2t'R(x) }

    Setting t = 0, per above:

    cost = SUM { y'y + R(x)'R(x) - 2y'R(x) }

    Note that y'y and SUM {R(x)'R(x)} are constant WRT rotation, so
    we simplify:

    cost' = SUM { y'R(x) }

    Remember that R(x) = [ cos(theta)*x_0 - sin(theta)*x_1,]
    [ sin(theta)*x_0 + cos(theta)*x_1 ]

    cost' = SUM { cos(theta)*x_0*y_0 - sin(theta)*x_1*y_0 +
    sin(theta)*x_0*y_1 + cos(theta)*x_1*y_1 }

    Collecting sin and cos terms, we have:

    cost' = M sin(theta) + N cos(theta)

    where:
    M = SUM {x_0*y_1 - x_1*y_0 }   and

    N = SUM {x_0*y_0 + x_1*y_1 }

    dcost'/ dtheta = M cos(theta) - N sin(theta) = 0
*/

/**
   Find the rotation R() and translation t that best transforms points a_i to lie near points b_i

   i.e.: transform points according to

   b_i ~= R(a_i) + t

   Error vector:

   e_i = b_i - R(a_i) - t

   Error magnitude (sum over all points i):

   chi = SUM(e_i'*e_i)

   (dropping i subscripts for brevity:)

   = SUM( b'b      -     b'R(a)    -    b't
   -R(a)'b   +   R(a)'R(a)   +   R(a)'t
   -t'b      +     t'R(a)    +    t't )

   Collecting terms:

   = SUM( b'b + R(a)'R(a) + t't - 2b'R(a) - 2b't + 2t'R(a) )

   Optimal translation: differentiate with respect to t, set to zero, yielding:

   dchi
   ----      = SUM( 2t - 2b + 2R(a) )  =  0
   dt
   SUM(t) = SUM(b) - SUM(R(a))

   Let Centroid(a) be the average of a_i (centroid of a). We have:

   t = Centroid(b) - R(Centroid(a))

   We'll come back to this. Now, let's consider optimal rotation:

   Optimal rotation: differentiate with respect to theta, set to zero, remembering that:

   R(a) = [ cos(theta)a_x - sin(theta)a_y ]
   [ sin(theta)a_x + cos(theta)a_y ]

   Note that R(a)'R(a) is independent of theta, so derivative wrt theta is zero.

   In summary:

   dchi                 d
   ----    =  SUM (    ---  ( -2b'R(a) + 2t'R(a) )    )  =  0
   dtheta             dtheta


   dchi                 d
   ----    =  SUM (    ---  ( -b'R(a) + (Centroid(b) - R(Centroid(a)))'R(a) )    )  =  0
   dtheta             dtheta


   dchi                 d
   ----    =  SUM (    ---  ( -b'R(a) + Centroid(b)'R(a) - R(Centroid(a))'R(a) )   )  = 0
   dtheta             dtheta


   d
   =  SUM (    ---  ( (-b + Centroid(b)' R(Centroid(a))' ) 'R(a)       )   )  =  0
   dtheta

   d
   =  SUM (    ---  ( -2(b+t)'R(a) )          )  =  0
   dtheta

   With a lot of annoying algebra:

   d          cos(theta)(b_x*a_x + t_x*a_x + b_y*a_y + t_y*a_y) +
   = SUM   ----  (      sin(theta)(-b_x*a_y - t_x*a_y + b_y*a_x + t_y*a_x)       )  = 0
   dtheta

   Define M and N such that:

   d
   = SUM   ----  (    cos(theta)*M + sin(theta)*N )   = 0
   dtheta

   We can now differentiate:

   = SUM ( -sin(theta)*M + cos(theta)*N) = 0

   -sin(theta)*SUM(M) + cos(theta)*SUM(N) = 0

   thus:

   - SUM(N)
   tan(theta) = ------
   SUM(M)
**/
public class AlignPoints2D
{
    double Sax, Say, Sbx, Sby, Saxby, Saybx, Saxbx, Sayby;
    double nPoints;

    public AlignPoints2D()
    {
    }

    public void reset()
    {
        Sax = 0;
        Say = 0;
        Sbx = 0;
        Sby = 0;
        Saxby = 0;
        Saybx = 0;
        Saxbx = 0;
        Sayby = 0;
        nPoints = 0;
    }

    // a is the point being projected.
    public void addPointCorrespondence(double a[], double b[])
    {
        Sax += a[0];
        Say += a[1];
        Sbx += b[0];
        Sby += b[1];

        Saxby += a[0]*b[1];
        Saybx += a[1]*b[0];
        Saxbx += a[0]*b[0];
        Sayby += a[1]*b[1];

        nPoints ++;
    }

    public void addPointCorrespondence(double a[], double b[], double weight)
    {
        Sax += a[0]*weight;
        Say += a[1]*weight;
        Sbx += b[0]*weight;
        Sby += b[1]*weight;

        Saxby += a[0]*b[1]*weight;
        Saybx += a[1]*b[0]*weight;
        Saxbx += a[0]*b[0]*weight;
        Sayby += a[1]*b[1]*weight;

        nPoints+=weight;
    }


    public double getNumPoints()
    {
        return nPoints;
    }

    public double[] compute()
    {
        double axc = Sax / nPoints;
        double ayc = Say / nPoints;
        double bxc = Sbx / nPoints;
        double byc = Sby / nPoints;

        double M = Saxby - Sax*byc - Sby*axc + axc*byc*nPoints - Saybx + Say*bxc + Sbx*ayc - ayc*bxc*nPoints;
        double N = Saxbx - Sax*bxc - Sbx*axc + axc*bxc*nPoints + Sayby - Say*byc - Sby*ayc + ayc*byc*nPoints;

        double theta = Math.atan2(M, N);

        double c = Math.cos(theta), s = Math.sin(theta);
        double dx = (bxc-axc) - c*axc + s*ayc + axc;
        double dy = (byc-ayc) - s*axc - c*ayc + ayc;

        return new double[] {dx, dy, theta };
    }

    public static double[] align(List<double[]> as, List<double[]> bs)
    {
        AlignPoints2D ap = new AlignPoints2D();
        assert(as.size() == bs.size());

        for (int i = 0; i < as.size(); i++) {
            double a[] = as.get(i);
            double b[] = bs.get(i);

            ap.addPointCorrespondence(a, b);
        }

        return ap.compute();
    }

    /** Compute the transformation that best projects the points xs to
     * the points ys. **/
    public static double[] alignReference(List<double[]> xs, List<double[]> ys)
    {
        assert(xs.size() == ys.size());

        //////////////////////////////////////////
        // compute centroid of a and b (The optimal translation is the
        // difference in centroids.)
        double xsum[] = new double[2], ysum[] = new double[2];

        for (int i = 0; i < xs.size(); i++) {
            double x[] = xs.get(i);
            double y[] = ys.get(i);
            for (int j = 0 ; j < 2; j++) {
                xsum[j] += x[j];
                ysum[j] += y[j];
            }
        }

        double xcent[] = LinAlg.scale(xsum, 1.0/xs.size());
        double ycent[] = LinAlg.scale(ysum, 1.0/ys.size());

        double t[] = LinAlg.subtract(ycent, xcent);

        //////////////////////////////////////////
        // compute rotation

        double M = 0, N = 0;

        for (int i = 0; i < xs.size(); i++) {

            double x0 = xs.get(i)[0] - xcent[0];
            double x1 = xs.get(i)[1] - xcent[1];

            double y0 = ys.get(i)[0] - ycent[0];
            double y1 = ys.get(i)[1] - ycent[1];

            M += x0*y1 - x1*y0;
            N += x0*y0 + x1*y1;
        }

        double theta = Math.atan2(M, N);

        // our rotation was around xcenter. Compute the transformation
        // that has a rotation around the origin. (The rotation is the
        // same, but the translation changes.)
        double c = Math.cos(theta), s = Math.sin(theta);
        double dx = t[0] - c*xcent[0] + s*xcent[1] + xcent[0];
        double dy = t[1] - s*xcent[0] - c*xcent[1] + xcent[1];

        return new double[] {dx, dy, theta };
    }

    public static void main(String args[])
    {
        Random r = new Random();

        ArrayList<double[]> xs = new ArrayList<double[]>();

        for (int i = 0; i < 2; i++) {
            double x[] = { r.nextFloat(), r.nextFloat() };
            xs.add(x);
        }

        double xyt[] = { 10*(r.nextFloat()-.5),
                         10*(r.nextFloat()-.5),
                         (r.nextFloat()-.5)*2*Math.PI };

        ArrayList<double[]> ys = LinAlg.transform(xyt, xs);

        LinAlg.print(xyt);
        while (true) {
            //	    Tic tic = new Tic();
            double xytfit[] = AlignPoints2D.alignReference(xs, ys);

            //	    LinAlg.print(xytfit);

            for (int i = 0; i < 3; i++)
                assert (Math.abs(xytfit[i]-xyt[i]) < 0.00001);

            //	    System.out.println(tic.toc()*1000);
            //	    LinAlg.print(xytfit);
        }

    }
}
