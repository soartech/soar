package april.jmat;

import java.util.*;

/** Representation of an N-dimensional multi-gaussian. **/
public class MultiGaussian
{
    double u[]; // mean

    /** The covariance matrix **/
    Matrix P;
    Matrix Pinv;
    double Pdet;
    double Pscale; // the coefficient out front.
    double logPscale; // log of Pscale

    /** right triangular factor from cholesky factorization **/
    Matrix L;

    int n; // number of variables

    static Random staticr = new Random();

    /** @param P The covariance matrix. A mean of zero is assumed. **/
    public MultiGaussian(double P[][])
    {
        this(new Matrix(P));
    }

    /** @param P The covariance matrix. A mean of zero is assumed. **/
    public MultiGaussian(Matrix P)
    {
        this(P, new double[P.getRowDimension()]);
    }

    /** @param P Covariance matrix
        @param u Mean vector
    **/
    public MultiGaussian(double P[][], double u[])
    {
        this(new Matrix(P), u);
    }

    /** @param P Covariance matrix
        @param u Mean vector
    **/
    public MultiGaussian(Matrix P, double u[])
    {
        this.P = P.copy();
        this.u = LinAlg.copy(u);
        this.n = P.getColumnDimension();

        Pdet = P.det();
        if (Pdet==0) // if they pass in a zero covariance matrix, produce exact answers.
	    {
            Pinv = new Matrix(n, n);
            L = new Matrix(n, n);
            Pscale = 0;
	    }
        else
	    {
            Pinv = P.inverse();
            CholeskyDecomposition cd = new CholeskyDecomposition(P);
            if (false && !cd.isSPD()) // XXX returning wrong answer sometimes?
		    {
                System.out.println("MULTIGAUSSIAN: P is not semi-definite positive! det="+P.det());

                P.print();
		    }

            L = cd.getL();
            Pscale = 1.0/Math.pow(2*Math.PI, n/2.0)/Math.sqrt(Pdet);
	    }

        logPscale = Math.log(Pscale);
    }

    /** Returns the number of variables described by this multi-gaussian. **/
    public int getDimension()
    {
        return P.getColumnDimension();
    }


    /** Return an Nx1 vector drawn from the distribution **/
    public double[] sample()
    {
        return sample(staticr);
    }

    /** Return an Nx1 vector drawn from the distribution using the provided Random object **/
    public double[] sample(Random r)
    {
        double x[] = new double[n];

        for (int i = 0; i < n; i++)
            x[i] = r.nextGaussian();

        return LinAlg.add(L.times(x), u);
    }

    /** compute probability of Nx1 vector v **/
    public double prob(double v[])
    {
        v = LinAlg.subtract(v, u);

        double e = LinAlg.dotProduct(v, Pinv.times(v));
        double p = Math.exp(-e/2.0);
        return Pscale*p;
    }

    /** compute log probability of Nx1 vector v **/
    public double logProb(double v[])
    {
        //	return Math.log(prob(v));

        return logPscale-chi2(v)/2;
    }

    /** Return inverse of the covariance matrix. **/
    public Matrix getPinv()
    {
        return Pinv;
    }

    public double chi2(double v[])
    {
        // special case 3x3 for performance
        if (v.length == 3)
	    {
            double a = v[0] - u[0];
            double b = v[1] - u[1];
            double c = v[2] - u[2];

            // assume symmetric Pinv (of course!)
            double d = Pinv.get(0,0), e = Pinv.get(0,1), f = Pinv.get(0,2);
            double g = Pinv.get(1,1), h = Pinv.get(1,2);
            double i = Pinv.get(2,2);

            return a*a*d + b*b*g + c*c*i + 2*(a*b*e + a*c*f + b*c*h);
	    }

        v = LinAlg.subtract(v, u);
        return LinAlg.dotProduct(v, Pinv.times(v));
    }

    public Matrix getCovariance()
    {
        return P;
    }

    public double[] getMean()
    {
        return u;
    }

    public double getMahalanobisDistance(double x[])
    {
        double dx[] = LinAlg.subtract(x, u);
        return Math.sqrt(LinAlg.dotProduct(dx, Pinv.times(dx)));
    }

    public static void main(String[] args)
    {
        Matrix P = new Matrix(2,2);
        P.set(0, 0,  4);
        P.set(1, 1,  2);
        P.set(0, 1, -1);
        P.set(1, 0, -1);

        MultiGaussian mg = new MultiGaussian(P, new double[2]);

        P.print();

        System.out.println("These should approach the matrix above...");

        for (int iter=1000;iter<110001;iter+=100000)
	    {
            Matrix A = new Matrix(2,2);
            for (int i=0;i<iter;i++)
		    {
                double x[] = mg.sample();

                A = A.plus(Matrix.outerProduct(x,x));
		    }
            A=A.times(1.0/(iter));
            A.print();
	    }

        System.out.println("This should be close to 1");

        double R = 20;
        double step = 0.05;
        double v[] = new double[2];

        double acc=0;

        for (double dx=-R;dx<R;dx+=step)
	    {
            v[0] = dx;

            for (double dy=-R;dy<R;dy+=step)
		    {
                v[1] = dy;

                double prob = mg.prob(v);
                acc+=prob*step*step;
		    }
	    }

        System.out.println(acc);

        Random r = new Random();
        for (int i = 0; i < 1000; i++) {
            double x[] = new double[] {r.nextDouble(), r.nextDouble()};

            double err = Math.log(mg.prob(x)) - mg.logProb(x);
            assert(Math.abs(err) < 0.000001);
        }
    }
}
