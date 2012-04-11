package april.jmat;

/** Cholesky Decomposition.
    <P>
    For a symmetric, positive definite matrix A, the Cholesky decomposition
    is an lower triangular matrix L so that A = L*L'.
    <P>
    If the matrix is not symmetric or positive definite, the constructor
    returns a partial decomposition and sets an internal flag that may
    be queried by the isSPD() method.
*/
public class CholeskyDecomposition
{
    Matrix L;
    Matrix U; // contains L' and garbage in the lower diagonal

    boolean isSpd;
    boolean verbose;

    public CholeskyDecomposition(Matrix A)
    {
        this(A, false);
    }

    public CholeskyDecomposition(Matrix A, boolean verbose)
    {
        int m = A.m, n = A.n;
        this.verbose = verbose;

        CSRAllocator csralloc = new CSRAllocator();

        if (true) {
            Matrix U = A.upperRight();

            isSpd = (m == n);

            for (int i = 0; i < n; i++) {

                if (verbose && n > 1000 && i%50==0)
                    System.out.printf("%d / %d\r", i, n);

                Vec Urowi = U.getRow(i);
                double d = Math.sqrt(Urowi.get(i));
                isSpd &= (d>0);

                Urowi.timesEquals(1.0/d, i, n-1); // or the whole row

                if (Urowi instanceof CSRVec) {
                    // special case sparse row to avoid having to
                    // iterate over the whole row. (big win!)
                    CSRVec uvv = (CSRVec) Urowi;

                    for (int pos = 0; pos < uvv.nz; pos++) {
                        int j = uvv.indices[pos];
                        if (j >= i+1) {
                            double s = uvv.values[pos];

                            CSRVec Urowj = (CSRVec) U.getRow(j);

                            if (false) {
                                CSRVec newUrowj = csralloc.get(Urowi.size(), Urowi.getNz() + Urowj.getNz());

                                CSRVec.add((CSRVec) Urowi, -s, (CSRVec) Urowj, j, n-1, newUrowj);
                                U.setRow(j, newUrowj);
                                csralloc.put(Urowj);
                            } else {
                                Urowi.addTo(Urowj, -s, j, n-1); // or the whole row
                            }
                        }
                    }

                } else {
                    // default case.
                    for (int j = i+1; j < n; j++) {
                        double s = Urowi.get(j);
                        if (s==0)
                            continue;
                        Vec Urowj = U.getRow(j);
                        Urowi.addTo(Urowj, -s, j, n-1); // or the whole row
                    }
                }
            }

            L = U.upperRightTranspose();
            U = null;

        } else {
            L = new Matrix(n, n, A.getOptions());
            isSpd = (m == n);

            // Main loop.
            for (int j = 0; j < n; j++) {

                if (verbose && n > 1000)
                    System.out.printf("%d / %d\r", j, n);

                Vec Lrowj = L.getRow(j);
                Vec Arowj = A.getRow(j);

                double d = 0.0;
                for (int k = 0; k < j; k++) {
                    Vec Lrowk = L.getRow(k);

                    double s = Lrowk.dotProduct(Lrowj, 0, k-1);

                    s = (Arowj.get(k) - s)/Lrowk.get(k);
                    Lrowj.set(k, s);
                    d = d + s*s;
                    // this line tests for symmetry, but is slow.
                    //   isSpd = isSpd && Math.abs(A.get(k,j) - A.get(j,k)) < 0.0000001;
                }
                d = Arowj.get(j) - d;
                isSpd &= (d > 0.0);
                Lrowj.set(j, Math.sqrt(Math.max(d,0.0)));
            }
        }

        if (verbose && n > 1000) {
            double nzfrac = ((double) L.getNz()) / Math.pow(L.getRowDimension(),2);
            System.out.println("L size: "+L.getRowDimension()+" nz: "+L.getNz()+" (%): "+nzfrac*100);
        }
    }

    public Matrix getL()
    {
        return L;
        /*
          int n = U.getRowDimension();

          Matrix L = new Matrix(n, n);
          for (int i = 0; i < n; i++)
          for (int j = 0; j <= i; j++)
          L.set(i,j, U.get(j,i));

          return L;
        */
    }

    public boolean isSPD()
    {
        return isSpd;
    }

    public Matrix solve(Matrix B)
    {
        int m = L.m, n = L.n;
        if (B.m != m)
            throw new IllegalArgumentException("Matrix row dimensions must agree.");

        if (!isSpd)
            throw new RuntimeException("Matrix is not SPD");

        //	erp.util.Tic tic = new erp.util.Tic();

        // Solve L*Y = B
        Matrix YT = new Matrix(B.getColumnDimension(), B.getRowDimension());

        // most of the time, B will have one column.
        for (int cidx = 0; cidx < B.getColumnDimension(); cidx++) {

            for (int ridx = 0; ridx < B.getRowDimension(); ridx++) {
                double dot = L.getRow(ridx).dotProduct(YT.getRow(cidx));
                double err = (B.get(ridx,cidx) - dot) / L.get(ridx,ridx);

                YT.set(cidx,ridx, err);
            }
        }

        Matrix XT = new Matrix(B.getColumnDimension(), B.getRowDimension());
        Matrix LT = L.transpose();

        // most of the time, Y will have one column.
        // Solve L'*X = Y

        for (int cidx = 0; cidx < B.getColumnDimension(); cidx++) {

            for (int ridx = B.getRowDimension()-1; ridx >= 0; ridx--) {
                double dot = LT.getRow(ridx).dotProduct(XT.getRow(cidx));
                double err = (YT.get(cidx,ridx) - dot) / L.get(ridx,ridx);

                XT.set(cidx,ridx, err);
            }
        }

        //	if (verbose)
        //	    System.out.println("backsolve time: "+tic.toc());

        return XT.transpose();
    }

    public static void main(String args[])
    {
        java.util.Random r = new java.util.Random();

        if (true) {
            int m = 5;

            Matrix A = Matrix.random(m, m);
            // make SPD
            A = A.times(A.transpose());

            int perm[] = new int[] { 1,0,3,2,4};

            Matrix PAP = A.copyPermuteRowsAndColumns(perm);

            A.print();
            PAP.print();
        }

        //	System.exit(0);

        System.out.println("Testing factors");
        for (int iters=0; iters<1000;iters++) {
            int m = r.nextInt(10)+3;
            Matrix A = Matrix.random(m, m);
            // make SPD
            A = A.times(A.transpose());

            CholeskyDecomposition cd = new CholeskyDecomposition(A);
            Matrix L = cd.getL();

            Matrix A2 = L.times(L.transpose());

            assert(A.equals(A2));
            System.out.println("ok "+iters);
        }

        System.out.println("Testing Solve");
        for (int iters=0; iters<1000;iters++) {
            Matrix A = Matrix.random(10,10);
            A = A.times(A.transpose());
            Matrix X = Matrix.random(10,1);
            Matrix B = A.times(X);

            CholeskyDecomposition cd = new CholeskyDecomposition(A);

            Matrix X2 = cd.solve(B);
            Matrix B2 = A.times(X2);

            assert(X.equals(X2) && B.equals(B2));
            System.out.println("ok "+iters);
        }
    }
}
