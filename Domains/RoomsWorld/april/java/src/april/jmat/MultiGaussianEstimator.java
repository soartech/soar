package april.jmat;

import java.util.*;

/** Given a number of samples, estimate a MultiGaussian **/
public class MultiGaussianEstimator
{
    int n;

    double P[][];
    double u[];
    double obsCount;

    public MultiGaussianEstimator(int n)
    {
        this.n = n;

        P = new double[n][n];
        u = new double[n];
    }

    /** For estimating the covariance from random samples. You can use
     * an unbiased estimator. **/
    public void observe(double v[])
    {
        observeWeighted(v, 1.0);
    }

    /** For estimating the covariance by integrating numerically.
        Note: PROB only need be accurate up to a constant
        multiplicative factor. You cannot use an unbiased estimate.
    **/
    public void observeWeighted(double v[], double prob)
    {
        for (int i = 0; i < n; i++)
            for (int j = 0; j < n; j++)
                P[i][j] += v[i]*v[j]*prob;

        for (int i = 0; i < n; i++)
            u[i] += v[i]*prob;

        obsCount += prob;
    }


    public MultiGaussian getEstimate()
    {
        return getEstimate(false);
    }

    /** CAUTION: do not request unbiased estimates if you're using
     * observeWeighted.  The N-1 math doesn't make sense in this case.
     **/
    public MultiGaussian getEstimate(boolean unbiased)
    {
        double normalization;
        if (unbiased)
            normalization = 1.0/(obsCount-1);
        else
            normalization = 1.0/obsCount;

        double tu[] = LinAlg.scale(u, normalization);
        Matrix tP = new Matrix(P).times(normalization);

        for (int i = 0; i < n; i++)
            for (int j = 0; j < n; j++)
                tP.plusEquals(i,j, -tu[i]*tu[j]);

        return new MultiGaussian(tP, tu);
    }


    public static void main(String args[])
    {
        Matrix P = new Matrix(2,2);
        P.set(0,0, 5);
        P.set(1,0, 2);
        P.set(0,1, 2);
        P.set(1,1, 4);

        ///////////////////////////////////////////////////////
        System.out.println("All three covariance/mean pairs should be close.");
        System.out.println("GROUND TRUTH:");

        double u[] = new double[] { 100, 20};
        MultiGaussian mg = new MultiGaussian(P, u);

        mg.getCovariance().print();
        LinAlg.print(mg.getMean());

        ///////////////////////////////////////////////////////
        System.out.println("SAMPLED FROM MULTIGAUSSIAN");

        MultiGaussianEstimator mge = new MultiGaussianEstimator(2);
        for (int i = 0; i < 100000; i++)
            mge.observe(mg.sample());

        MultiGaussian mg2 = mge.getEstimate();

        mg2.getCovariance().print();
        LinAlg.print(mg2.getMean());

        ///////////////////////////////////////////////////////
        System.out.println("INTEGRATED FROM MULTIGAUSSIAN");

        MultiGaussianEstimator mge2 = new MultiGaussianEstimator(2);
        double x[] = new double[2];
        double integrationRange = 20;
        double integrationResolution = .1;

        for (double i = -integrationRange; i <= integrationRange; i+=integrationResolution) {
            x[0] = u[0]+i;

            for (double j = -integrationRange; j <= integrationRange; j+=integrationResolution) {
                x[1] = u[1]+j;

                double prob = mg.prob(x);
                mge2.observeWeighted(x, prob);
            }
        }

        MultiGaussian mg3 = mge2.getEstimate();
        mg3.getCovariance().print();
        LinAlg.print(mg3.getMean());
    }

}
