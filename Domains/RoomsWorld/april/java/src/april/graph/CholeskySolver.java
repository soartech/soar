package april.graph;

import april.jmat.*;
import april.jmat.ordering.*;
import april.util.*;

public class CholeskySolver implements GraphSolver
{
    Graph g;

    public static boolean verbose = true;

    public Ordering ordering;

    Matrix L;

    public CholeskySolver(Graph g, Ordering ordering)
    {
        this.g = g;
        this.ordering = ordering;
    }

    public Matrix makeSymbolicA()
    {
        Matrix A = new Matrix(g.nodes.size(), g.nodes.size(), Matrix.SPARSE);

        for (GEdge ge : g.edges) {
            for (int i = 0; i < ge.nodes.length; i++)
                for (int j = 0; j < ge.nodes.length; j++)
                    A.set(ge.nodes[i], ge.nodes[j], 1);
        }

        return A;
    }

    public boolean canIterate()
    {
        return true;
    }

    public void iterate()
    {
        Tic tic = new Tic();

        int n = g.getStateLength();
        Matrix A = new Matrix(n, n, Matrix.SPARSE);
        Matrix B = new Matrix(n, 1);

        // Computing A directly, rather than computing J'J, is hugely
        // faster. Each edge connects ge.nodes nodes, which will
        // create ge.nodes.length^2 contributions to A.
        for (GEdge ge : g.edges) {

            Linearization lin = ge.linearize(g, null);

            for (int i = 0; i < ge.nodes.length; i++) {

                int aidx = g.getStateIndex(ge.nodes[i]);
                double JatW[][] = LinAlg.matrixAtB(lin.J.get(i), lin.W);

                for (int j = 0; j < ge.nodes.length; j++) {

                    int bidx = g.getStateIndex(ge.nodes[j]);

                    double JatWJb[][] = LinAlg.matrixAB(JatW, lin.J.get(j));

                    A.plusEquals(aidx, bidx, JatWJb);
                }

                double JatWr[] = LinAlg.matrixAB(JatW, lin.R);
                B.plusEqualsColumnVector(aidx, 0, JatWr);
            }
        }

        if (verbose)
            System.out.printf("Build A, B: %15.5f\n", tic.toctic());

        if (true) {
            // Condition the matrix. Any un-constrained variables get a
            // weight to be unchanged.

            // characteristic weight of J'WJ. Should be between lambda_max
            // and lambda_min
            double W0 = 1000; 	    // XXX FIXME

            boolean rooted = false;

            for (GEdge ge : g.edges) {
                if (ge instanceof GXYTPosEdge) {
                    rooted = true;
                    break;
                }
            }

            if (!rooted) {
                // Add an extra constraint to pose 0.
                GNode gn = g.nodes.get(0);
                int idx = g.getStateIndex(0);
                for (int i = 0; i < gn.getDOF(); i++)
                    A.set(idx+i, idx+i, A.get(idx+i, idx+i)+W0);

                if (verbose)
                    System.out.printf("Adding a virtual edge to pose zero\n");
            }
        }

        Matrix x = null;

        if (ordering == null) {
            if (verbose)
                System.out.printf("A size: %d    nz: %d   (%%): %f\n",
                                  A.getRowDimension(),
                                  A.getNz(),
                                  A.getNzFrac()*100);

            CholeskyDecomposition cd = new CholeskyDecomposition(A, verbose);
            L = cd.getL();
            x = cd.solve(B);

        } else {
            Matrix SA = makeSymbolicA();

            int saPerm[] = ordering.getPermutation(SA);

            if (verbose)
                System.out.printf("Compute ordering: %15.5f\n", tic.toctic());

            int perm[] = new int[n];
            int pos = 0;
            for (int saidx = 0; saidx < saPerm.length; saidx++) {
                int gnidx = saPerm[saidx];
                GNode gn = g.nodes.get(gnidx);
                int gnpos = g.getStateIndex(gnidx);
                for (int i = 0; i < gn.getDOF(); i++)
                    perm[pos++] = gnpos + i;
            }

            Matrix PAP = A.copyPermuteRowsAndColumns(perm);
            Matrix PB = B.copy();
            PB.permuteRows(perm);

            if (true) {
                double acc = 0;
                int count = 0;
                boolean underconstrained = false;

                for (int i = 0; i < PAP.getRowDimension(); i++) {
                    if (PAP.get(i,i) == 0) {
                        underconstrained = true;
                    } else {
                        acc += PAP.get(i,i);
                        count ++;
                    }
                }

                if (underconstrained) {
                    System.out.println("CholeskySolver: underconstrained graph. Trying to fix it (hack!)");

                    if (count == 0) {
                        acc = 1;
                        count = 1;
                    }

                    for (int i = 0; i < PAP.getRowDimension(); i++) {
                        if (PAP.get(i,i) == 0) {
                            PAP.set(i,i, acc / count);
                        }

                        assert(PAP.get(i,i) > 0); //System.out.printf("%10d %15f\n", i, PAP.get(i,i));
                    }
                }
            }

            if (verbose)
                System.out.printf("P'AP size: %d    nz: %d   (%%): %f\n",
                                  PAP.getRowDimension(),
                                  PAP.getNz(),
                                  PAP.getNzFrac()*100);

            CholeskyDecomposition cd = new CholeskyDecomposition(PAP, verbose);
            L = cd.getL();
            x = cd.solve(PB);
            x.inversePermuteRows(perm);

            if (false) {
                /** Verify that matrix is sparse where it's supposed to be. **/
                Matrix U = cd.getL().transpose();
                for (int row = 0; row < U.getRowDimension(); row++) {
                    CSRVec csr = (CSRVec) U.getRow(row);
                    csr.filterZeros(0.0000001);
                    if (csr.nz > 0 && csr.indices[0] < row)
                        System.out.println(csr.values[0]);
                }
                System.out.println("*"+U.getNz()+"*");
            }
        }

        for (int gnidx = 0; gnidx < g.nodes.size(); gnidx++) {
            GNode gn = g.nodes.get(gnidx);
            int idx = g.getStateIndex(gnidx);
            for (int i = 0; i < gn.getDOF(); i++)
                gn.state[i] -= x.get(idx+i);
        }

        if (verbose)
            System.out.printf("Solve      : %15.5f\n", tic.toctic());
        if (verbose)
            System.out.printf("Total      : %15.5f\n\n", tic.totalTime());

    }

    public Matrix getR()
    {
        return L;
    }
}
