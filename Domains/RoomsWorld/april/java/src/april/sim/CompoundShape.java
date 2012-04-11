package april.sim;

import java.util.*;

import april.jmat.*;

public class CompoundShape implements Shape
{
    ArrayList<Object> ops = new ArrayList<Object>();

    public CompoundShape(Object ... os)
    {
        add(os);
    }

    public void add(Object ... os)
    {
        int i = 0;

        while (i < os.length) {
            if (os[i] == null) {
                i++;
                continue;
            }

            if (os[i] instanceof double[][])
                add((double[][]) os[i]);
            else
                ops.add(os[i]);

            i++;
        }
    }

    private void add(double M[][])
    {
        // if more than one rigid-body transformation in a row,
        // pre-multiply them together.
        if (false && ops.size() > 0) {
            Object o = ops.get(ops.size()-1);
            if (o instanceof double[][]) {
                ops.set(ops.size()-1, LinAlg.matrixAB((double[][]) o, M));
                return;
            }
        }

        ops.add(M);
    }
}
