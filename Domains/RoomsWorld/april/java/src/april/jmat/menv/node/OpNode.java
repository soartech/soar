package april.jmat.menv.node;

import java.util.*;

import april.jmat.menv.*;
import april.jmat.menv.inst.*;

public class OpNode implements Node
{
    public Node    lhs; // null for unary ops
    public Node    rhs;
    public OP      op;

    public enum OP
    {
        OR("||", -1), AND("&&", -1),
            EQ("==", 0), LT("<",0), GT(">", 0), LTE("<=", 0), GTE(">=", 0), NEQ("!=", 0),
            PLUS("+", 1), MINUS("-", 1), TRANS("'", 1),
            MUL("*", 2), DIV("/", 2),
            SOLVE("\\", 3), EXP("^", 3),
            NOT("!", 4), COMP("~", 4);

        int     precedence; // higher = higher precedence
        String  tok;        // the token
        //	boolean binaryOp;   // only a binary op

        OP(String tok, int precedence)
	    {
            this.tok = tok;
            this.precedence = precedence;
	    }

        public static OP fromToken(String tok)
        {
            for (OP op : values())
                if (tok.equals(op.tok))
                    return op;

            return null;
        }

        public int getPrecedence()
        {
            return precedence;
        }

        public String getToken()
        {
            return tok;
        }
    }

    public OpNode(String tok, Node lhs, Node rhs)
    {
        this.op = OP.fromToken(tok);
        this.lhs = lhs;
        this.rhs = rhs;
    }

    public int getPrecedence()
    {
        return op.getPrecedence();
    }

    public int emit(Emit e)
    {
        int r = e.allocVar();

        if (lhs==null) {
            int rright = rhs.emit(e);
            e.emit(new OpInstruction(r, op, rright));
        } else {
            int rleft = lhs.emit(e);
            int rright = rhs.emit(e);
            e.emit(new OpInstruction(r, op, rleft, rright));
        }

        return r;
    }

    public String toString()
    {
        if (lhs == null)
            return String.format("(%s %s)", op.getToken(), rhs);
        else
            return String.format("(%s %s %s)", op.getToken(), lhs, rhs);
    }
}
