package april.jmat.menv.inst;

import java.util.*;
import april.jmat.*;
import april.jmat.menv.*;

public class SubselInstruction extends Instruction
{
    public SubselInstruction(int r, int rsrc, int ra, int rb, int ca, int cb)
    {
        this.out = new int[] {r};
        this.in = new int[] {rsrc, ra, rb, ca, cb};
    }

    public void execute(Environment env)
    {
        Matrix src = TypeUtil.toMatrix(env.r[in[0]]);

        int ira = TypeUtil.toInt(env.r[in[1]]);
        int irb = TypeUtil.toInt(env.r[in[2]]);

        if (in[3] < 0) {
            // vector notation (e.g., M[4]) yields a scalar
            assert(ira == irb);
            int row = ira / src.getColumnDimension();
            int col = ira - row*src.getColumnDimension();
            env.r[out[0]] = new Matrix(src.get(row, col));

        } else {

            int ica = TypeUtil.toInt(env.r[in[3]]);
            int icb = TypeUtil.toInt(env.r[in[4]]);

            env.r[out[0]] = src.copy(ira, irb, ica, icb);
        }

        env.pc++;
    }

    public String toString()
    {
        return String.format("r%d <- (subsel r%d [ r%d : r%d , r%d : r%d] )", out[0], in[0], in[1], in[2], in[3], in[4]);
    }
}
