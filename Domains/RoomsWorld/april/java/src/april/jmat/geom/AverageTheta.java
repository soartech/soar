package april.jmat.geom;

import java.util.*;
import april.jmat.*;

/** Given a set of theta measurements, pick the "average"
    (approximately).

    Compute the average theta in an approximate way. Each theta is
    cast out onto the unit circle, and the unit circle is aligned
    so that each point is as close as possible to (1,0). For small
    errors, the approximation is very good. For larger errors, the
    Euclidean distance can "cut across" the inside of the circle,
    and so the error is under-estimated.

    This method is by ebolson@umich.edu, inspired by a similar
    problem in Horn's "closed-form solution of absolute
    orientation using unit quaternions".

    Let a be the set of input points, and R(a_i) represent a
    rotation of point a_i around the origin:

    R(x) = [ cos(theta)*a_x - sin(theta)*a_y,]
    [ sin(theta)*a_x + cos(theta)*a_y ]

    The error is:

    X^2 = SUM  ( R(a_i) - [1 0]' )' * (R(a_i) - [1 0]')

	= SUM   R'R - 2[1 0]R(a) + [1 0][1 0]'

    Note that R'R is constant, because R and R' are
    orthogonal. (R'R = I). Dropping constant terms:

    X^2 = SUM 2[1 0]R(a)

    Differentiating with respect to theta:

    dX^2/dtheta = SUM cos(theta)a_x - sin(theta)a_y = 0

    Collecting cos and sin terms:

    cos(theta) SUM a_x = sin(theta) SUM a_y

    e.g.,:

    theta = atan2( SUM a_y , SUM a_x )

    This is the theta that rotates the a points back onto the
    origin: we want -theta.
**/
public class AverageTheta
{
    public static double averageThetaApproximate(List<Double> thetas)
    {
        double M = 0, N = 0;

        for (double t : thetas) {
            double x0 = Math.cos(t);
            double y0 = Math.sin(t);

            M += y0;
            N += x0;
        }

        return Math.atan2(M, N);
    }

    /** For a given theta, compute the MSE **/
    public static double computeMSE(List<Double> thetas, double theta)
    {
        double sqerr = 0;
        for (double t : thetas) {
            double err = MathUtil.mod2pi(t-theta);
            sqerr += err*err;
        }
        return sqerr / thetas.size();
    }

    /** Trying every theta (with a step size of dtheta), find the
        theta that results in the smallest MSE. **/
    public static double averageThetaBruteForce(List<Double> thetas, double dtheta)
    {
        double bestmse = Double.MAX_VALUE;
        double besttheta = 0;

        for (double theta = 0; theta < 2*Math.PI; theta+=dtheta) {
            double mse = computeMSE(thetas, theta);
            if (mse < bestmse) {
                bestmse = mse;
                besttheta = theta;
            }
        }
        return MathUtil.mod2pi(besttheta);
    }

    /** Test harness... **/
    public static void main(String args[])
    {
        ArrayList<Double> thetas = new ArrayList<Double>();
        Random rand = new Random();

        double worstError = 0;
        double worstRatio = 0;

        for (int trial = 0; trial < 20; trial++) {
            thetas.clear();
            double offset = MathUtil.mod2pi(2*Math.PI*rand.nextDouble());

            // add contaminated samples
            for (int i = 0; i < 100; i++) {
                thetas.add(offset+1*(rand.nextGaussian()));
            }

            // add uniform noise everywhere
            for (int i = 0; i < 100; i++) {
                thetas.add(2*Math.PI*rand.nextDouble());
            }

            double bf = averageThetaBruteForce(thetas, Math.toRadians(0.1));
            double appr = averageThetaApproximate(thetas);

            System.out.printf("Trial %d, Nominal offset: %15f:\n", trial, offset);
            System.out.printf("  Brute force: %15f  (mse %15f)\n", bf, computeMSE(thetas, bf));
            System.out.printf("  Approximate: %15f  (mse %15f)\n", appr, computeMSE(thetas, appr));

            worstError = Math.max(worstError, Math.abs(MathUtil.mod2pi(bf-appr)));
            worstRatio = Math.max(worstRatio, computeMSE(thetas,appr) / computeMSE(thetas,bf));
        }
        System.out.println("worst error (radians): "+worstError);
        System.out.println("worst error (ratio):   "+worstRatio);
    }
}
