package april.jmat.menv.inst;

import java.util.*;
import april.jmat.*;
import april.jmat.menv.*;

public class SubselStoreInstruction extends Instruction
{
    public String name;

    public SubselStoreInstruction(String name, int r, int ra, int rb, int ca, int cb)
    {
        this.in = new int[] {r, ra, rb, ca, cb};
        this.name = name;
    }

    public String toString()
    {
        return String.format("'%s'[r%d : r%d , r%d : r%d] <- r%d", name, in[1], in[2], in[3], in[4], in[0]);
    }

    public void execute(Environment env)
    {
        Matrix dst = TypeUtil.toMatrix(env.getVariable(name));
        Matrix src = TypeUtil.toMatrix(env.r[in[0]]);

        int ira = TypeUtil.toInt(env.r[in[1]]);
        int irb = TypeUtil.toInt(env.r[in[2]]);

        if (in[3] < 0) {
            // vector notation
            assert (ira==irb);
            int row = ira / src.getColumnDimension();
            int col = ira - row*src.getColumnDimension();
            dst.set(row, col, src.get(0,0));

        } else {
            int ica = TypeUtil.toInt(env.r[in[3]]);
            int icb = TypeUtil.toInt(env.r[in[4]]);

            try {
                for (int i = 0; i < src.getRowDimension(); i++)
                    for (int j = 0; j < src.getColumnDimension(); j++)
                        dst.set(i+ira,j+ica, src.get(i,j));
            } catch (ArrayIndexOutOfBoundsException ex) {
                throw new MEnvRuntimeException("Out of bounds access of variable '"+name+"'");
            }
        }

        // have to do this, or it won't work for non-matrices
        env.putVariable(name, dst);
        env.pc++;
    }
}
