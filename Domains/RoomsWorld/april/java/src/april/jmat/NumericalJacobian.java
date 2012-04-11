package april.jmat;

public class NumericalJacobian
{
    /** Numerically computes the jacobian, which is Y x X in
     * size. eps.lenght == x0.length.
     **/
    public static double[][] computeJacobian(Function func, double x0[], double eps[])
    {
        double y0[] = func.evaluate(x0, null);

        double J[][] = new double[y0.length][x0.length];

        double x[] = LinAlg.copy(x0);

        for (int ix = 0; ix < x0.length; ix++) {
            double origx = x[ix];
            x[ix] = origx + eps[ix];
            double y[] = func.evaluate(x, null);
            x[ix] = origx;

            for (int iy = 0; iy < y0.length; iy++)
                J[iy][ix] = (y[iy] - y0[iy])/eps[ix];
        }

        return J;
    }
}
