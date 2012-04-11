package april.graph;

import java.io.*;
import java.util.*;
import java.lang.reflect.*;

import april.jmat.*;
import april.jmat.geom.*;
import april.util.*;

public class GXYNode extends GNode
{
    public GXYNode copy()
    {
        GXYNode g = new GXYNode();
        g.state = LinAlg.copy(state);
        g.init = LinAlg.copy(init);
        if (truth != null)
            g.truth = LinAlg.copy(truth);

        g.attributes = attributes.copy();

        return g;
    }

    public int getDOF()
    {
        return 2;
    }

    public double[] toXyzRpy(double s[])
    {
        return new double[] { s[0], s[1], 0, 0, 0, 0 };
    }
}
