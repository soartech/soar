package april.jmat;

import april.jmat.ordering.*;

import java.util.*;

/** Very basic (and incomplete) tests for sparse matrix implementations. **/
public class SparseTest
{
    public static void main(String args[])
    {
        Random r = new Random();

        int dim = 300;
        Matrix M = new Matrix(dim, dim, Matrix.SPARSE);
        for (int i = 0; i < dim; i++)
            M.set(i,i, r.nextDouble());

        for (int iter = 0; iter < 1000; iter++) {
            int i = r.nextInt(dim);
            int j = r.nextInt(dim);

            double v = r.nextDouble();
            M.plusEquals(i,j, v);
            M.plusEquals(j,i, v);
        }

        Ordering gro = new MinimumDegreeOrdering();
        //	M.print();
        reportSparsity(M);

        M.permuteRows(gro.getPermutation(M));
        //	M.print();
        reportSparsity(M);
    }

    static void reportSparsity(Matrix M)
    {
        LUDecomposition lu = new LUDecomposition(M, false);
        CholeskyDecomposition cd = new CholeskyDecomposition(M);

        System.out.printf("cd: %10d    lu: %10d\n", cd.getL().getNz(), lu.getLU().getNz());
    }
}
