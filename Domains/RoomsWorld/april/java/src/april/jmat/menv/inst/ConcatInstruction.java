package april.jmat.menv.inst;

import java.util.*;
import april.jmat.*;
import april.jmat.menv.*;

public class ConcatInstruction extends Instruction
{
    // how many variables are specified per row?
    ArrayList<Integer> rowLengths;

    public ConcatInstruction(int r, ArrayList<ArrayList<Integer>> parts)
    {
        this.out = new int[] {r};

        rowLengths = new ArrayList<Integer>();
        for (ArrayList<Integer> partrow: parts)
            rowLengths.add(partrow.size());

        ArrayList<Integer> ins = new ArrayList<Integer>();
        for (ArrayList<Integer> row : parts)
            for (Integer i : row)
                ins.add(i);

        this.in = new int[ins.size()];
        for (int i = 0; i < ins.size(); i++)
            this.in[i] = ins.get(i);
    }

    public void execute(Environment env)
    {
        // evaluate all of the matrices.
        ArrayList<ArrayList<Matrix>> matrices = new ArrayList<ArrayList<Matrix>>();

        boolean allStringArguments = true;

        int inOffset = 0;

        for (int rowLength : rowLengths)
	    {
            ArrayList<Matrix> mrow = new ArrayList<Matrix>();

            for (int rowOffset = 0; rowOffset < rowLength; rowOffset++) {
                Object o = env.r[in[inOffset+rowOffset]];
                if (!(o instanceof String))
                    allStringArguments = false;
                mrow.add(TypeUtil.toMatrix(o));
            }
            inOffset += rowLength;

            matrices.add(mrow);
	    }

        // special case for strings.
        if (allStringArguments) {
            StringBuffer sb = new StringBuffer();
            for (int inidx = 0; inidx < in.length; inidx++)
                sb.append((String) env.r[in[inidx]]);

            env.r[out[0]] = sb.toString();
            env.pc++;
            return;
        }

        // compute dimensions.
        int rows = 0;
        int cols = -1;

        for (ArrayList<Matrix> mrow : matrices)
	    {
            int thisrows = -1;
            int thiscols = 0;

            for (Matrix m : mrow) {
                if (thisrows < 0)
                    thisrows = m.getRowDimension();
                else if (thisrows != m.getRowDimension())
                    throw new MEnvRuntimeException("matrix concat: dimensions don't agree.");

                thiscols += m.getColumnDimension();
            }

            if (cols < 0)
                cols = thiscols;
            else if (thiscols != cols)
                throw new MEnvRuntimeException("matrix concat: dimensions don't agree");

            rows += thisrows;
	    }

        // fill in the matrix.
        double A[][] = new double[rows][cols];
        int row = 0;
        for (ArrayList<Matrix> mrow : matrices)
	    {
            int col = 0;

            for (Matrix m : mrow) {
                for (int i = 0; i < m.getRowDimension(); i++)
                    for (int j = 0; j < m.getColumnDimension(); j++)
                        A[row+i][col+j] = m.get(i,j);

                col += m.getColumnDimension();
            }

            row += mrow.get(0).getRowDimension();
	    }

        env.r[out[0]] = new Matrix(A);

        env.pc++;
    }

    public String toString()
    {
        StringBuffer sb = new StringBuffer();

        sb.append(String.format("r%d <- (concat ", out[0]));

        int inOffset = 0;

        for (int rowLength : rowLengths)
	    {
            ArrayList<Matrix> mrow = new ArrayList<Matrix>();

            for (int rowOffset = 0; rowOffset < rowLength; rowOffset++)
                sb.append(String.format("r%d ", in[inOffset+rowOffset]));

            inOffset += rowLength;

            sb.append("; ");
	    }

        sb.append(")");

        return sb.toString();
    }
}
