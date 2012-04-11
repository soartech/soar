package april.laser.scanmatcher;

import april.jmat.*;

final class Chi2Data
{
    // compute a lower bound on the chi2 error for any point within
    // this search window. We assume that theta is fixed, i.e., that
    // the search window consists purely of translations.
    //
    // idea:
    //
    // 1.we factor the joint distribution P(x,y,t) into P(x,y|t)P(t),
    // since knowledge of t will influence the cost within this
    // window. But P(t) is constant within this window, which makes
    // our lives easier. We can compute P(x,y|t) as though we made a
    // noiseless observation of t using standard EKF equations. P(t)
    // is just P(x,y,t) dropping the x & y variables.
    //
    // 2. We now wish to bound (optimistically) the chi^2 error
    // anywhere in this window. This seems to be difficult to do
    // precisely, as the location of the minimum chi^2 error depends
    // on the relative location of the prior and the box.
    //
    // 3. For the moment, we are quite conservative: we move (up to) a
    // distance 'r' in the direction of the dominant eigenvector, and
    // an additional distance 'r' in the direction of the sub-dominant
    // direction.
    //
    // Note: we can precompute (for each t) almost everything:
    // P(x,y|t), P(t), u1, u2.

    double t;
    double terr;
    double tchi2; // chi2 error of t, using just P(t)

    double u[];   // mean of P(x,y | t)  (2x1)
    double P[][]; // covariance of P(x,y | t) (2x2)
    double Pinv[][]; // inverse of P (NOT THE SAME AS priorP!)

    double L[][]; // cholesky factor of Pinv, Pinv = L'L

    double v1[];  // dominant eigenvector of P (2x1)
    double v2[];  // sub-dominant eigenvector of P(2x1)

    double priorP[][];
    double priorPinv[][];

    public Chi2Data(double prior[], double priorP[][], double priorPinv[][], double t)
    {
        this.t = t;
        this.priorP = priorP;
        this.priorPinv = priorPinv;

        this.terr = MathUtil.mod2pi(t - prior[2]);
        this.tchi2 = terr*priorPinv[2][2]*terr;

        // consider priorP consistig of 4 parts:
        // [ A   B ]   where A=2x2, B=2x1, C=1x2
        // [ B'  C ]
        //
        // u' = u + B*inv(C)*(t-u_t)
        double B[] = new double[] { priorP[0][2], priorP[1][2] };
        double C = priorP[2][2];

        this.u = new double[] { prior[0] + B[0]/C*terr,
                                prior[1] + B[1]/C*terr };

        // P = A - B*inv(C)*B'
        this.P = new double[][] { { priorP[0][0] - B[0]*B[0]/C, priorP[0][1] - B[0]*B[1]/C },
                                  { priorP[1][0] - B[1]*B[0]/C, priorP[1][1] - B[1]*B[1]/C } };
        this.Pinv = LinAlg.inverse(this.P);

//        this.L = LinAlg.cholesky22(this.Pinv);

        // find dominant direction via SVD
        double rho = 0.5*Math.atan2(-2*this.Pinv[0][1],(this.Pinv[1][1] - this.Pinv[0][0]));
        double phi = rho + Math.PI / 2.0;

        // v1 is the dominant vector of the covariance, and so the
        // sub-dominant vector of the information matrix.
        this.v1 = new double[] { Math.cos(rho), Math.sin(rho) };
        this.v2 = new double[] { Math.cos(phi), Math.sin(phi) };
    }

    // Compute a lower bound of the chi^2 error at position (tx,ty,t)
    // within a circle of radius r.
    public double computeChi2(double tx, double ty, double r)
    {
        // compute error vector WRT P(x,y | t)
        double ex = tx - u[0];
        double ey = ty - u[1];

        double v2dot = v2[0]*ex + v2[1]*ey;
        double v2dist = (v2dot==0) ? 0 : v2dot*Math.min(Math.abs(v2dot), r)/Math.abs(v2dot);
        ex -= v2dist*v2[0];
        ey -= v2dist*v2[1];

        double v1dot = v1[0]*ex + v1[1]*ey;
        double v1dist = (v1dot==0) ? 0 : v1dot*Math.min(Math.abs(v1dot), r)/Math.abs(v1dot);
        ex -= v1dist*v1[0];
        ey -= v1dist*v1[1];

        double chi2 = ex*ex*Pinv[0][0] + 2*ex*ey*Pinv[0][1] + ey*ey*Pinv[1][1] + tchi2;

        return chi2;
    }

    public static void main(String args[])
    {
        double prior[] = new double[] { 15.172149, .030775, 0 };
        double priorP[][] = new double[][] { { .0056, .0012, 0},
                                             { 0.0012, 0.0035, 0},
                                             { 0, 0, 1 } };
        double t = 0;

        Chi2Data cd = new Chi2Data(prior, priorP, LinAlg.inverse(priorP), t);

        LinAlg.print(cd.Pinv);

        LinAlg.print(cd.P);
        if (true) {
            double chi2 = cd.computeChi2(15.9, -.4, .282843);
            System.out.printf("v1: %15f %15f\n", cd.v1[0], cd.v1[1]);
            System.out.printf("v2: %15f %15f\n", cd.v2[0], cd.v2[1]);
            System.out.printf("chi2: %15f\n", chi2);
        }

        if (true) {
            double chi2 = cd.computeChi2(16.1, -.6, .565685);
            System.out.printf("v1: %15f %15f\n", cd.v1[0], cd.v1[1]);
            System.out.printf("v2: %15f %15f\n", cd.v2[0], cd.v2[1]);
            System.out.printf("chi2: %15f\n", chi2);
        }

    }

}

