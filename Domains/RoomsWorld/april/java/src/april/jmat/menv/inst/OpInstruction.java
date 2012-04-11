package april.jmat.menv.inst;

import java.util.*;
import april.jmat.*;
import april.jmat.menv.*;

import april.jmat.menv.node.*;

public class OpInstruction extends Instruction
{
    public OpNode.OP op;

    public OpInstruction(int r, OpNode.OP op, int a)
    {
        this.in = new int[] {a};
        this.out = new int[] {r};
        this.op = op;
    }

    public OpInstruction(int r, OpNode.OP op, int a, int b)
    {
        this.in  = new int[] {a, b};
        this.out = new int[] {r};
        this.op = op;
    }

    public String toString()
    {
        if (in.length == 2)
            return String.format("r%d <- (r%d %s r%d)", out[0], in[0], op.getToken(), in[1]);
        else
            return String.format("r%d <- (%s r%d)", out[0], op.getToken(), in[0]);
    }

    public void execute(Environment env)
    {
        Object lo = null, ro = null;

        if (in.length==1) {
            ro = env.r[in[0]];
        } else {
            lo = env.r[in[0]];
            ro = env.r[in[1]];
        }

        Object res = null;

        if (lo instanceof Matrix || ro instanceof Matrix) {
            res = executeMatrices(env, TypeUtil.toMatrix(lo), TypeUtil.toMatrix(ro));

        } else if (lo instanceof String && ro instanceof String) {
            res = executeStrings(env, (String) lo, (String) ro);

        } else {
            assert(false);
        }

        env.r[out[0]] = res;
        env.pc++;
    }

    Object executeStrings(Environment env, String left, String right)
    {
        switch (op)
	    {
            case PLUS:
                return left+right;
            case EQ:
                return left.equals(right) ? TypeUtil.TRUE : TypeUtil.FALSE;
            default:
                return executeMatrices(env,
                                       new Matrix(Double.parseDouble(left)),
                                       new Matrix(Double.parseDouble(right)));
	    }
    }

    Object executeMatrices(Environment env, Matrix left, Matrix right)
    {
        double lv = (left!=null) ? left.get(0,0) : 0;
        double rv = right.get(0,0);

        boolean lb = TypeUtil.toBoolean(left);
        boolean rb = TypeUtil.toBoolean(right);

        switch (op)
	    {
            case PLUS:
                if (left.getRowDimension()!=right.getRowDimension() ||
                    left.getColumnDimension()!=right.getColumnDimension())
                    throw new MEnvRuntimeException("Matrix dimensions do not agree for addition");
                return left.plus(right);

            case MINUS:
                if (left == null) {
                    return right.times(-1);
                } else {
                    if (left.getRowDimension()!=right.getRowDimension() ||
                        left.getColumnDimension()!=right.getColumnDimension())
                        throw new MEnvRuntimeException("Matrix dimensions do not agree for subtraction");

                    return left.minus(right);
                }
            case MUL: {
                boolean sl = left.getColumnDimension()==1 &&
                    left.getRowDimension()==1;
                boolean sr = right.getColumnDimension()==1 &&
                    right.getRowDimension()==1;

                if (sl && !sr)
                    return right.times(left.get(0,0));
                if (!sl && sr)
                    return left.times(right.get(0,0));

                try {
                    return left.times(right);
                } catch (IllegalArgumentException ex) {
                    throw new MEnvRuntimeException("Matrix dimensions do not agree for multiply");
                }
            }
            case DIV:
                return left.times(right.inverse());

            case SOLVE:
                return left.solve(right);

            case TRANS:
                return right.transpose();

            case EXP: {
                assert(right.getColumnDimension()==1 &&
                       right.getRowDimension()==1);
                double expval = right.get(0,0);

                // scalar exponentiation
                if (left.getColumnDimension()==1 &&
                    left.getRowDimension()==1) {

                    return new Matrix(Math.pow(lv, rv));
                }

                // dumb matrix exponentiation
                if (expval == -1)
                    return left.inverse();

                assert(expval > 0);
                assert(expval == ((int) expval));

                Matrix mm = right;
                for (int i = 1; i < ((int) expval); i++) {
                    mm = mm.times(right);
                }

                return mm;
            }

            case NOT:
            case COMP:
                assert(left == null);
                return !rb ? TypeUtil.TRUE : TypeUtil.FALSE;

            case EQ:
                return TypeUtil.matrixEquals(left, right) ? TypeUtil.TRUE : TypeUtil.FALSE;

            case NEQ:
                return TypeUtil.matrixEquals(left, right) ? TypeUtil.FALSE : TypeUtil.TRUE;

            case LT:
                return lv<rv ? TypeUtil.TRUE : TypeUtil.FALSE;

            case GT:
                return lv>rv ? TypeUtil.TRUE : TypeUtil.FALSE;

            case LTE:
                return lv<=rv ? TypeUtil.TRUE : TypeUtil.FALSE;

            case GTE:
                return lv>=rv ? TypeUtil.TRUE : TypeUtil.FALSE;

            case OR:
                return (lb || rb) ? TypeUtil.TRUE : TypeUtil.FALSE;

            case AND:
                return (lb && rb) ? TypeUtil.TRUE : TypeUtil.FALSE;

            default:
                assert(false);
	    }

        return null;
    }
}
