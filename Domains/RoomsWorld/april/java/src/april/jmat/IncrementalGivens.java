package april.jmat;

import java.util.*;

public class IncrementalGivens
{
    int ncols;
    int nrows;
    int nrotations;

    CSRVec rows[] = new CSRVec[4]; // may be over-allocated in length. use nrows for valid length.
    double b[]    = new double[4]; // may be over-allocated in length. use nrows for valid length.

    public boolean verbose = false;

    public IncrementalGivens(int ncols)
    {
        this.ncols = ncols;
    }

    void ensureCapacity(int minsize)
    {
        if (minsize < rows.length)
            return;

        // int grow.
        int newsize = (int) Math.max(minsize*1.5, nrows*1.5);

        CSRVec newrows[] = new CSRVec[newsize];
        double newb[] = new double[newsize];

        for (int i = 0; i < nrows; i++) {
            newrows[i] = rows[i];
            newb[i] = b[i];
        }

        rows = newrows;
        b = newb;
    }

    public int getNumRotations()
    {
        return nrotations;
    }

    public int getNz()
    {
        int nz = 0;
        for (int row = 0; row < nrows; row++) {
            CSRVec csr = rows[row];
            nz += csr.getNz();
        }
        return nz;
    }

    public void addRows(CSRVec newr[], double newb[])
    {
        assert(newr.length == newb.length);

        // add!
        ensureCapacity(nrows + newr.length);

        for (int i = 0; i < newr.length; i++) {
            assert(newr[i].length == ncols);

            rows[nrows] = (CSRVec) newr[i].copy();
            b[nrows] = newb[i];
            nrows++;

            fixRow(nrows-1);

            if (verbose && nrows%10==0)
                System.out.printf("\r%d %d", nrows, nrotations);
        }
    }

    /** add n columns. **/
    public void addColumns(int n)
    {
        ncols += n;

        for (CSRVec csr : rows)
            csr.resize(ncols);
    }

    void rotate(CSRVec csr0, CSRVec csr1, double s, double c, int thiscol)
    {
        // create output rows for rotation
        int indices0[] = new int[csr0.nz + csr1.nz]; // worst-case size.
        int indices1[] = new int[csr0.nz + csr1.nz];
        double values0[] = new double[indices0.length];
        double values1[] = new double[indices1.length];
        int out0pos = 0;
        int out1pos = 0;

        int csr0pos = 0;
        int csr1pos = 0;
        while (csr0pos < csr0.nz || csr1pos < csr1.nz) {

            int thisindex = Integer.MAX_VALUE;
            if (csr0pos < csr0.nz)
                thisindex = csr0.indices[csr0pos];
            if (csr1pos < csr1.nz)
                thisindex = Math.min(thisindex, csr1.indices[csr1pos]);

            assert(thisindex != Integer.MAX_VALUE);

            double x0 = 0;
            double y0 = 0;
            if (csr0pos < csr0.nz && csr0.indices[csr0pos] == thisindex)
                x0 = csr0.values[csr0pos++];
            if (csr1pos < csr1.nz && csr1.indices[csr1pos] == thisindex)
                y0 = csr1.values[csr1pos++];

            double x0r, y0r;
            x0r = c*x0 - s*y0;
            y0r = s*x0 + c*y0;

            if (x0r != 0) {
                indices0[out0pos] = thisindex;
                values0[out0pos++] = x0r;
            }

            if (y0r != 0 && thisindex > thiscol) {
                indices1[out1pos] = thisindex;
                values1[out1pos++] = y0r;
            }
        }

        csr0.values = values0;
        csr0.indices = indices0;
        csr0.nz = out0pos;

        csr1.values = values1;
        csr1.indices = indices1;
        csr1.nz = out1pos;
    }

    int idxtmp[] = new int[1024];
    double valtmp0[] = new double[1024];
    double valtmp1[] = new double[1024];

    void rotate2(CSRVec csr0, CSRVec csr1, double s, double c, int thiscol)
    {
        int minsize = csr0.nz + csr1.nz;
        if (idxtmp.length < minsize) {
            idxtmp = new int[minsize];
            valtmp0 = new double[minsize];
            valtmp1 = new double[minsize];
        }

        // write new output rows to temp variables, then copy into place.
        int outpos = 0;

        int csr1pos = 0;
        for (int csr0pos = 0; csr0pos < csr0.nz; csr0pos++) {

            while (csr1pos < csr1.nz && csr1.indices[csr1pos] < csr0.indices[csr0pos]) {
                // csr0 is zero. csr1 is non-zero
                // x = 0, y = *
                double x0 = 0;
                double y0 = csr1.values[csr1pos];
                int idx = csr1.indices[csr1pos];

                idxtmp[outpos] = idx;
                valtmp0[outpos] = c*x0 - s*y0;
                valtmp1[outpos] = s*x0 + c*y0;
                outpos++;

                csr1pos++;
            }

            // handle case when they're equal
            if (csr1pos < csr1.nz && csr0.indices[csr0pos] == csr1.indices[csr1pos]) {
                // both are non-zero
                // x = *, y = *

                double x0 = csr0.values[csr0pos];
                double y0 = csr1.values[csr1pos];
                int idx = csr0.indices[csr0pos];

                idxtmp[outpos] = idx;
                valtmp0[outpos] = c*x0 - s*y0;
                valtmp1[outpos] = s*x0 + c*y0;
                outpos++;

                csr1pos++;
            } else {
                // they're not equal, i.e.:
                // csr0 is non-zero, csr1 is zero
                double x0 = csr0.values[csr0pos];
                double y0 = 0;
                int idx = csr0.indices[csr0pos];

                idxtmp[outpos] = idx;
                valtmp0[outpos] = c*x0 - s*y0;
                valtmp1[outpos] = s*x0 + c*y0;
                outpos++;
            }
        }

        while (csr1pos < csr1.nz) {
            // csr0 is zero, csr1 is non-zero.
            double x0 = 0;
            double y0 = csr1.values[csr1pos];
            int idx = csr1.indices[csr1pos];

            idxtmp[outpos] = idx;
            valtmp0[outpos] = c*x0 - s*y0;
            valtmp1[outpos] = s*x0 + c*y0;
            outpos++;

            csr1pos++;
        }

        ////////////

        csr0.ensureCapacity(outpos);
        csr1.ensureCapacity(outpos);
        int outpos0 = 0;
        int outpos1 = 0;
        for (int i = 0; i < outpos; i++) {
            if (valtmp0[i]!=0) {
                csr0.indices[outpos0] = idxtmp[i];
                csr0.values[outpos0] = valtmp0[i];
                outpos0++;
            }

            if (valtmp1[i]!=0 && idxtmp[i]!=thiscol) {
                csr1.indices[outpos1] = idxtmp[i];
                csr1.values[outpos1] = valtmp1[i];
                outpos1++;
            }
        }
        csr0.nz = outpos0;
        csr1.nz = outpos1;

        /*
          System.arraycopy(idxtmp, 0, csr0.indices, 0, outpos);
          System.arraycopy(valtmp0, 0, csr0.values, 0, outpos);
          csr0.nz = outpos;

          csr1.ensureCapacity(outpos-1);
          System.arraycopy(idxtmp, 1, csr1.indices, 0, outpos-1); // truncate the leading zero
          System.arraycopy(valtmp1, 1, csr1.values, 0, outpos-1);
          csr1.nz = outpos - 1;
        */
        //	assert(idxtmp[0]==thiscol);
    }

    void fixRow(int thisrow)
    {
        CSRVec csr1 = rows[thisrow];
        csr1.filterZeros();

        for (int thiscol = 0; thiscol < thisrow; thiscol++) {
            //	while (csr1.nz > 0 && csr1.indices[0] < thisrow) {

            //	    int thiscol = csr1.indices[0];
            //	    double y = csr1.values[0];

            double y = csr1.get(thiscol);
            if (y==0)
                continue;

            nrotations++;

            int prevrow = thiscol;

            CSRVec csr0 = rows[prevrow];
            double x = csr0.get(thiscol);

            double mag = Math.sqrt(x*x + y*y);

            // sin and cos of the angle that will rotate the
            // vector so that there is no y component.
            double s = -y/mag;
            double c = x/mag;

            CSRVec csr0a = (CSRVec) csr0.copy();
            CSRVec csr1a = (CSRVec) csr1.copy();
            //	    rotate(csr0a, csr1a, s, c, thiscol);
            rotate(csr0, csr1, s, c, thiscol);

            /*
              for (int i = 0; i < csr0.size(); i++)
              if (Math.abs(csr0.get(i) - csr0a.get(i)) > 0.00001)
              assert(false);

              for (int i = 0; i < csr1.size(); i++)
              if (Math.abs(csr1.get(i) - csr1a.get(i)) > 0.00001)
              assert(false);
            */

            // update RHS
            double b0 = b[prevrow];
            double b1 = b[thisrow];
            b[prevrow] = c*b0 - s*b1;
            b[thisrow] = s*b0 + c*b1;
        }
    }

    public double[] solve()
    {
        // we assume that we've been maintaining triangularization the
        // whole them. We just back solve.

    	// back solve
        DenseVec x = new DenseVec(ncols);

        for (int i = ncols-1; i >= 0; i--) {
            double diag = rows[i].get(i); // future optimization: always the first nz value.
            double acc = rows[i].dotProduct(x);

            x.set(i, (b[i] - acc)/diag);
        }

        return x.getDoubles();
    }

    static CSRVec makeCSR(double denseValues[])
    {
        CSRVec v = new CSRVec(denseValues.length);
        for (int i = 0; i < denseValues.length; i++)
            if (denseValues[i] != 0)
                v.set(i, denseValues[i]);
        return v;
    }

    static CSRVec[] matrixToCSRVec(Matrix A)
    {
        CSRVec rows[] = new CSRVec[A.getRowDimension()];
        for (int i = 0; i < rows.length; i++)
            rows[i] = (CSRVec) A.getRow(i);
        return rows;
    }

    public Matrix getMatrix()
    {
        if(nrows == 0)
            return new Matrix(0, 0);

        Matrix M  = new Matrix(nrows, rows[0].length, Matrix.SPARSE);
        for(int r = 0; r < nrows; r++){
            M.setRow(r,rows[r]);
        }

        return M;
    }

    public static void main(String args[])
    {
        Random r = new Random();

        int n = 3 + r.nextInt(5);
        int m = n + r.nextInt(10);
        IncrementalGivens incg = new IncrementalGivens(n);

        Matrix A = new Matrix(m, n);
        double b[] = new double[m];

        for (int row = 0; row < A.getRowDimension(); row++) {
            int nz = 1 + r.nextInt(n-2);
            for (int j = 0; j < nz; j++) {
                while (true) {
                    int col = r.nextInt(A.getColumnDimension());
                    if (A.get(row, col) != 0)
                        continue;
                    A.set(row, col, 10*(r.nextDouble()-.5));
                    break;
                }
            }
            b[row] = 10*(r.nextDouble()-.5);
        }
        A = A.coerceOption(Matrix.SPARSE);
        /*
          Matrix A = new Matrix( new double[][] {
          { 1, 1, 0, 0, 0},
          { 0, 1, 1, 0, 0},
          { 0, 0, 1, 1, 0},
          { 0, 0, 0, 1, 1},
          { 1, 0, 0, 0, 1},
          { 1, 0, 0, 1, 1},
          { 1, 0, 1, 0, 0} }).coerceOption(Matrix.SPARSE);

          double b[] = new double[] { 1, 5, 8, 9, 10, 12, 15 };
        */

        Matrix At = A.transpose();
        LinAlg.print(At.times(A).inverse().times(At.times(b)));

        incg.addRows(matrixToCSRVec(A), b);

        LinAlg.print(incg.solve());
        System.out.printf("nrotations: %d\n", incg.nrotations);
    }
}
