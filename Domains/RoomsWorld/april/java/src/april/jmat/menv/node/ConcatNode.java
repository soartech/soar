package april.jmat.menv.node;

import java.util.*;

import april.jmat.*;

import april.jmat.menv.*;
import april.jmat.menv.inst.*;

public class ConcatNode implements Node
{
    ArrayList<ArrayList<Node>> nodes;

    public ConcatNode(ArrayList<ArrayList<Node>> nodes)
    {
        this.nodes = nodes;
    }

    public String toString()
    {
        StringBuffer sb = new StringBuffer();

        sb.append("[ ");
        for (ArrayList<Node> nrow : nodes)
	    {
            ArrayList<Matrix> mrow = new ArrayList<Matrix>();

            for (Node n : nrow)
                sb.append(n+" ");
            sb.append("; ");
	    }
        sb.append("]");

        return sb.toString();
    }

    public int emit(Emit e)
    {
        ArrayList<ArrayList<Integer>> parts = new ArrayList<ArrayList<Integer>>();

        for (ArrayList<Node> nrow : nodes)
	    {
            ArrayList<Integer> rrow = new ArrayList<Integer>();

            for (Node n : nrow)
                rrow.add(n.emit(e));

            parts.add(rrow);
	    }

        int r = e.allocVar();

        e.emit(new ConcatInstruction(r, parts));

        return r;
    }
}
