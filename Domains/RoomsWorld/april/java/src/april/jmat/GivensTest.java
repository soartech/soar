package april.jmat;

import april.jmat.ordering.*;
import java.util.*;

/** QR Decomposition (Experimental, *probably* wrong) using givens
 * rotations.
 **/
public class GivensTest
{
    double A[][], b[];
    int nrotations;
    double x[];

    public GivensTest(double _A[][], double _b[])
    {
        A = LinAlg.copy(_A);
        b = LinAlg.copy(_b);

        int rows = A.length, cols = A[0].length;
        assert(b.length == rows);

        for (int row = rows-1; row > 0; row--) {
            for (int col = 0; col < cols; col++) {

                if (A[row][col] == 0)
                    continue;

                nrotations++;

                int prow = col;

                // rotate vector { A[row-1][col], A[row][col] } so that
                // A[row][col] = 0.

                double x = A[prow][col], y = A[row][col];
                double mag = Math.sqrt(x*x + y*y);

                // sin and cos of the angle that will rotate the
                // vector so that there is no y component.
                double s = -y/mag;
                double c = x/mag;

                // rotate the left-most vector. By construction, this
                // rotates the vector so that it is purely along the x
                // component.
                A[prow][col] = mag;
                A[row][col] = 0;

                // rotate all the remaining vectors in this row.
                for (int i = col+1; i < cols; i++) {
                    double x0 = A[prow][i];
                    double y0 = A[row][i];

                    A[prow][i] = c*x0 - s*y0;
                    A[row][i]   = s*x0 + c*y0;
                }

                // rotate the b vector.
                double b0 = b[prow];
                double b1 = b[row];
                b[prow] = c*b0 - s*b1;
                b[row] = s*b0 + c*b1;
            }
        }

        // back solve
        x = new double[cols];

        for (int i = cols-1; i >= 0; i--) {
            double acc = 0;
            double diag = A[i][i];
            for (int j = i+1; j < cols; j++) {
                acc += A[i][j]*x[j];
            }
            x[i] = (b[i] - acc)/diag;
        }
    }

    /*
      public double getDeterminant()
      {
      double det = 1;
      for (int i = 0; i < A[0].length; i++)
      det *= A[i][i];

      return det;
      }
    */

    static Random rand = new Random();

    static double[][] noise(double A[][])
    {
        for (int i = 0; i < A.length; i++)
            for (int j = 0; j < A[i].length; j++)
                if (A[i][j] != 0)
                    A[i][j] = rand.nextDouble();
        return A;
    }

    static double[] noise(double b[])
    {
        for (int i = 0; i < b.length; i++)
            if (b[i] != 0)
                b[i] = rand.nextDouble();
        return b;
    }

    static Matrix makeAdjacencyMatrix(Matrix J)
    {
        int rows = J.getRowDimension();
        int cols = J.getColumnDimension();

        Matrix A = new Matrix(cols, cols);
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                if (J.get(i,j)==0)
                    continue;
                for (int k = j+1; k < cols; k++) {
                    if (J.get(i,k)!=0) {
                        A.set(j,k, 1);
                        A.set(k,j, 1);
                    }
                }
            }
        }

        for (int i = 0; i < cols; i++)
            A.set(i,i, 1);

        return A;
    }

    public static void main2(String args[])
    {
        if (false) {
            int links[][] = new int[][] {  {0, 2},
                                           {0, 3},
                                           {0, 4},
                                           {0, 3},
                                           {0, 4},
                                           {2, 4},
                                           {9, 7},
                                           {9, 6},
                                           {9, 5},
                                           {5, 7},
                                           {5, 8},
                                           {6, 8},
                                           {0, 7}
            };

            int N = 10;
            int M = links.length + N;
            double A[][] = new double[M][N];
            for (int i = 0; i+1 < N; i++) {
                A[i][i] = 1;
                A[i][i+1] = 1;
            }
            A[N-1][N-1] = 1;

            for (int i = 0; i < links.length; i++) {
                A[N+i][links[i][0]]=1;
                A[N+i][links[i][1]]=1;
            }

            double b[] = new double[A.length];
            for (int i = 0; i < b.length; i++)
                b[i] = 1;

            LinAlg.printPattern(A);
            noise(A); noise(b);
            GivensTest g = new GivensTest(A, b);
            System.out.printf("\n");
            LinAlg.printPattern(g.A);
            System.out.printf("nz: %d\n", LinAlg.nz(g.A));

            Matrix ADJ = makeAdjacencyMatrix(new Matrix(A));
            //	    LinAlg.printPattern(ADJ.copyArray());
            MinimumDegreeOrdering ordering = new MinimumDegreeOrdering();
            int perm[] = ordering.getPermutation(ADJ);
            for (int i = 0; i < perm.length; i++)
                System.out.printf("%3d %d\n", i, perm[i]);

            GivensTest g2 = new GivensTest(new Matrix(A).copyPermuteColumns(perm).copyArray(), b);
            LinAlg.printPattern(g2.A);
            System.out.printf("nz: %d\n", LinAlg.nz(g2.A));


        }

        if (true) {
            double A[][] = new double[][] {{ 1, 1, 0, 0, 0, 0, 0},
                                           { 0, 1, 1, 0, 0, 0, 0},
                                           { 0, 0, 1, 1, 0, 0, 0},
                                           { 0, 0, 0, 1, 1, 0, 0},
                                           { 0, 0, 0, 0, 1, 1, 0},
                                           { 0, 0, 0, 0, 0, 1, 1},
                                           { 0, 0, 0, 0, 0, 0, 1},
                                           { 0, 0, 0, 1, 0, 0, 1},
                                           { 1, 0, 0, 0, 0, 0, 1}};
            double b[] = new double[] { 1, 1, 1, 1, 1, 1, 1, 1, 1};

            LinAlg.printPattern(A);

            noise(A); noise(b);
            GivensTest g = new GivensTest(A, b);

            System.out.printf("\n");
            LinAlg.printPattern(g.A);
            System.out.println(""+g.nrotations);
        }

        if (true) {
            double A[][] = new double[][] {{ 1, 1, 0, 0, 0, 0, 0},
                                           { 1, 0, 0, 0, 0, 0, 1},
                                           { 0, 1, 1, 0, 0, 0, 0},
                                           { 0, 0, 1, 1, 0, 0, 0},
                                           { 0, 0, 0, 1, 0, 0, 1},
                                           { 0, 0, 0, 1, 1, 0, 0},
                                           { 0, 0, 0, 0, 1, 1, 0},
                                           { 0, 0, 0, 0, 0, 1, 1},
                                           { 0, 0, 0, 0, 0, 0, 1}};

            double b[] = new double[] { 1, 1, 1, 1, 1, 1, 1, 1, 1};

            noise(A); noise(b);
            GivensTest g = new GivensTest(A, b);
        }


        if (false) {
            double A[][] = new double[][] {{ 1, 1, 0, 0, 0},
                                           { 1, 0, 0, 0, 1},
                                           { 0, 1, 1, 0, 0},
                                           { 0, 0, 1, 1, 0},
                                           { 0, 0, 0, 1, 1},
                                           { 0, 0, 0, 0, 1}};

            double b[] = new double[] { 1, 1, 1, 1, 1, 1};

            noise(A); noise(b);
            GivensTest g = new GivensTest(A, b);
        }

    }

    public static void main(String args[])
    {
        Random r = new Random();

        for (int iter = 0; iter < 10000; iter++) {
            int cols = 1+r.nextInt(4);
            int rows = cols + r.nextInt(4);

            double A[][] = new double[rows][cols];
            for (int i = 0; i < rows; i++)
                for (int j = 0; j < cols; j++)
                    A[i][j] = r.nextInt(100)-50;

            double b[] = new double[rows];
            for (int i = 0; i < rows; i++)
                b[i] = r.nextInt(100)-50;

            double gsol[] = new GivensTest(A,b).x;

            double JTJ[][] = LinAlg.matrixAtB(A, A);
            double JTb[] = LinAlg.matrixAtB(A, b);

            double invJTJ[][] = LinAlg.inverse(JTJ);
            if (invJTJ == null) {
                System.out.printf("rank deficient\n");
                continue;
            }

            double sol[] = LinAlg.matrixAB(invJTJ, JTb);

            double d = LinAlg.distance(gsol, sol);
            if (d >= 1E-7) {
                System.out.printf("*********** %g\n", d);
                LinAlg.print(sol);
                LinAlg.print(gsol);
            }
        }

    }
}
