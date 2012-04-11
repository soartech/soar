package april.jmat;

import java.util.*;

/** QR Decomposition (Experimental, *probably* wrong) using givens
 * rotations.
 **/
public class Givens
{
    Matrix A;
    double b[];

    int nrotations;

    public Givens(Matrix _A, double _b[])
    {
        A = _A.copy();
        b = LinAlg.copy(_b);

        int rows = A.getRowDimension(), cols = A.getColumnDimension();
        assert(b.length == rows);

        for (int i = 0; i < rows; i++)
            fixRow(i);
    }

    void fixRow(int row)
    {
        int cols = A.getColumnDimension();

        int mx = Math.min(row, cols);
        for (int thiscol = 0; thiscol < mx; thiscol++) {

            double y = A.get(row, thiscol);

            if (y == 0)
                continue;

            nrotations++;

            int prevrow = thiscol;
            double x = A.get(prevrow, thiscol);

            double mag = Math.sqrt(x*x + y*y);

            // sin and cos of the angle that will rotate the
            // vector so that there is no y component.
            double s = -y/mag;
            double c = x/mag;

            Vec v0 = A.getRow(prevrow);
            Vec v1 = A.getRow(row);

            if (v0 instanceof CSRVec && v1 instanceof CSRVec) {

                CSRVec csr0 = (CSRVec) v0;
                CSRVec csr1 = (CSRVec) v1;

                int indices0[] = new int[csr0.nz + csr1.nz]; // worst-case size.
                int indices1[] = new int[csr0.nz + csr1.nz];
                int out0pos = 0;
                int out1pos = 0;

                double values0[] = new double[indices0.length];
                double values1[] = new double[indices1.length];

                int csr0pos = 0;
                int csr1pos = 0;
                while (csr0pos < csr0.nz || csr1pos < csr1.nz) {
                    int thisindex = Math.min(csr0.indices[csr0pos], csr1.indices[csr1pos]);

                    double x0 = 0;
                    double y0 = 0;
                    if (csr0pos < csr0.nz && csr0.indices[csr0pos] == thisindex)
                        x0 = csr0.values[csr0pos++];
                    if (csr1pos < csr1.nz && csr1.indices[csr1pos] == thisindex)
                        y0 = csr1.values[csr1pos++];

                    double x0r, y0r;
                    x0r = c*x0 - s*y0;
                    y0r = s*x0 + c*y0;

                    //		    if (x0r != 0) {
                    indices0[out0pos] = thisindex;
                    values0[out0pos++] = x0r;
                    //		    }

                    //		    if (y0r != 0) {
                    indices1[out1pos] = thisindex;
                    values1[out1pos++] = y0r;
                    //		    }
                }

                csr0.values = values0;
                csr0.indices = indices0;
                csr0.nz = out0pos;

                csr1.values = values1;
                csr1.indices = indices1;
                csr1.nz = out1pos;

            } else {
                for (int i = thiscol; i < cols; i++) {
                    double x0 = v0.get(i);
                    double y0 = v1.get(i);

                    v0.set(i, c*x0 - s*y0);
                    v1.set(i, s*x0 + c*y0);
                }
            }

            // update RHS
            double b0 = b[prevrow];
            double b1 = b[row];
            b[prevrow] = c*b0 - s*b1;
            b[row] = s*b0 + c*b1;

            // make sure this cell is exactly zero.
            A.set(row,thiscol, 0);
        }
    }

    public double[] solve()
    {
        int cols = A.getColumnDimension();

    	// back solve
        double x[] = new double[cols];

        for (int i = cols-1; i >= 0; i--) {
            double acc = 0;
            double diag = A.get(i,i);
            for (int j = i+1; j < cols; j++) {
                acc += A.get(i,j)*x[j];
            }
            x[i] = (b[i] - acc)/diag;
        }

        return x;
    }

    public static void main(String args[])
    {
        Matrix A = new Matrix(new double[][] {{ 1, 5, 10 },
                                              {6, 1, 12},
                                              {6, 6, 10}});
        A = A.coerceOption(Matrix.SPARSE);

        double b[] = new double[] { 5, 8, 1};

        Givens g = new Givens(A, b);
        LinAlg.print(g.solve());

        LinAlg.print(LinAlg.matrixAB(LinAlg.inverse(A.copyArray()), b));
    }
}
