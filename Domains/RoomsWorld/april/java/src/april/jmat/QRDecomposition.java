package april.jmat;

/** QR Decomposition.
    <P>
    For an m-by-n matrix A with m >= n, the QR decomposition is an m-by-n
    orthogonal matrix Q and an n-by-n upper triangular matrix R so that
    A = Q*R.
    <P>
    The QR decompostion always exists, even if the matrix does not have
    full rank, so the constructor will never fail.  The primary use of the
    QR decomposition is in the least squares solution of nonsquare systems
    of simultaneous linear equations.  This will fail if isFullRank()
    returns false.
*/

public class QRDecomposition
{
    Matrix QR;
    double Rdiag[];

    public QRDecomposition(Matrix A, boolean verbose)
    {
        this(A);
    }

    // XXX Attrociously unoptimized for sparse case
    public QRDecomposition(Matrix A)
    {
        QR = A.copy();
        int m = A.getRowDimension(), n = A.getColumnDimension();
        Rdiag = new double[n];

        // Main loop.
        for (int k = 0; k < n; k++) {

            if (n > 1000)
                System.out.printf("%d / %d\r", k, n);

            // Compute 2-norm of k-th column without under/overflow.
            double nrm = 0;
            for (int i = k; i < m; i++) {
                double QRik = QR.get(i,k);
                nrm += QRik*QRik;
            }
            nrm = Math.sqrt(nrm);

            if (nrm != 0.0) {
                // Form k-th Householder vector.
                if (QR.get(k,k) < 0)
                    nrm = -nrm;

                for (int i = k; i < m; i++)
                    QR.timesEquals(i,k, 1.0/nrm);

                QR.plusEquals(k,k, 1);

                // Apply transformation to remaining columns.
                for (int j = k+1; j < n; j++) {
                    double s = 0.0;
                    for (int i = k; i < m; i++)
                        s += QR.get(i,k)*QR.get(i,j);

                    s = -s/QR.get(k,k);
                    for (int i = k; i < m; i++)
                        QR.plusEquals(i,j, s*QR.get(i,k));
                }
            }
            Rdiag[k] = -nrm;
        }
    }

    public boolean isFullRank()
    {
        int m = QR.getRowDimension(), n = QR.getColumnDimension();
        for (int j = 0; j < n; j++) {
            if (Rdiag[j] == 0)
                return false;
        }
        return true;
    }

    public Matrix getR()
    {
        int m = QR.getRowDimension(), n = QR.getColumnDimension();
        Matrix R = new Matrix(n,n);

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (i < j)
                    R.set(i,j, QR.get(i,j));
                else if (i == j)
                    R.set(i,j, Rdiag[i]);
            }
        }
        return R;
    }

    public Matrix getQ()
    {
        int m = QR.getRowDimension(), n = QR.getColumnDimension();
        Matrix Q = new Matrix(m,n);

        for (int k = n-1; k >= 0; k--) {

            Q.set(k,k, 1);

            for (int j = k; j < n; j++) {
                if (QR.get(k,k) != 0) {
                    double s = 0.0;
                    for (int i = k; i < m; i++)
                        s += QR.get(i,k)*Q.get(i,j);

                    s = -s/QR.get(k,k);
                    for (int i = k; i < m; i++)
                        Q.plusEquals(i,j, s*QR.get(i,k));
                }
            }
        }
        return Q;
    }

    /** Least squares solution of A*X = B
        @param B    A Matrix with as many rows as A and any number of columns.
        @return     X that minimizes the two norm of Q*R*X-B.
        @exception  IllegalArgumentException  Matrix row dimensions must agree.
        @exception  RuntimeException  Matrix is rank deficient.
    */
    public Matrix solve (Matrix B) {
        int m = QR.getRowDimension(), n = QR.getColumnDimension();

        if (B.getRowDimension() != m) {
            throw new IllegalArgumentException("Matrix row dimensions must agree.");
        }
        if (!this.isFullRank()) {
            throw new RuntimeException("Matrix is rank deficient.");
        }

        // Copy right hand side
        int nx = B.getColumnDimension();
        Matrix X = B.copy();

        // Compute Y = transpose(Q)*B
        for (int k = 0; k < n; k++) {
            for (int j = 0; j < nx; j++) {
                double s = 0.0;

                for (int i = k; i < m; i++)
                    s += QR.get(i,k)*X.get(i,j);

                s = -s/QR.get(k,k);
                for (int i = k; i < m; i++)
                    X.plusEquals(i,j, s*QR.get(i,k));
            }
        }

        // Solve R*X = Y;
        for (int k = n-1; k >= 0; k--) {
            Vec Xrowk = X.getRow(k);
            Xrowk.timesEquals(1.0/Rdiag[k]);

            for (int i = 0; i < k; i++) {

                double QRik = QR.get(i,k);
                Vec Xrowi = X.getRow(i);

                Xrowk.addTo(Xrowi, - QRik);
            }
        }

        return X.copy(0, n-1, 0, nx-1);
    }

    public static void main(String args[])
    {
        java.util.Random r = new java.util.Random();

        System.out.println("Testing factors");
        for (int iters=0; iters<1000;iters++) {
            Matrix A = Matrix.random(r.nextInt(3)+3,
                                     r.nextInt(3));

            QRDecomposition qrd = new QRDecomposition(A);

            Matrix A2 = qrd.getQ().times(qrd.getR());

            assert(A.equals(A2));
            System.out.println("ok "+iters);
        }

        System.out.println("Testing Solve");
        for (int iters=0; iters<1000;iters++) {
            Matrix A = Matrix.random(10,10);
            Matrix X = Matrix.random(10,1);
            Matrix B = A.times(X);

            QRDecomposition qrd = new QRDecomposition(A);

            Matrix X2 = qrd.solve(B);
            Matrix B2 = A.times(X2);

            assert(X.equals(X2) && B.equals(B2));
            System.out.println("ok "+iters);
        }
    }

}
