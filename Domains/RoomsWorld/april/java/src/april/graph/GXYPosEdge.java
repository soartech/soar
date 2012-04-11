package april.graph;

import java.io.*;

import april.jmat.*;
import april.util.*;

/**  Constrains the position of a node directly (with respect to the
 *  origin), in contrast to GXYEdge, which constrains the position of
 *  two nodes relative to each other.
 *
 **/
public class GXYPosEdge extends GEdge
{
    public double z[]; // x, y, theta
    public double truth[];

    public double P[][]; // covariance

    double W[][];  // inverse(P)

    MultiGaussian mg;

    public GXYPosEdge()
    {
    }

    public GXYPosEdge copy()
    {
        GXYPosEdge e = new GXYPosEdge();
        e.nodes = LinAlg.copy(nodes);
        e.z = LinAlg.copy(z);
        if (truth != null)
            e.truth = LinAlg.copy(truth);
        e.P = LinAlg.copy(P);

        if (attributes != null)
            e.attributes = attributes.copy();

        return e;
    }

    public double[][] getW()
    {
        if (W == null)
            W = LinAlg.inverse(P);
        return W;
    }

    public double getChi2(Graph g)
    {
        MultiGaussian mg = getMultiGaussian();
        GXYTNode gn = (GXYTNode) g.nodes.get(nodes[0]);

        double s[] = new double[] { gn.state[0], gn.state[1] };

        return mg.chi2(s);
    }

    public MultiGaussian getMultiGaussian()
    {
        if (mg == null)
            mg = new MultiGaussian(P, z);
        return mg;
    }

    public int getDOF()
    {
        return 2;
    }

    public void write(StructureWriter outs) throws IOException
    {
        outs.writeComment("node");
        outs.writeInt(nodes[0]);

        outs.writeComment("XY");
        outs.writeDoubles(z);
        outs.writeComment("XY truth");
        outs.writeDoubles(truth);
        outs.writeComment("Covariance");
        outs.writeMatrix(P);
        Attributes.write(attributes, outs);
    }

    public void read(StructureReader ins) throws IOException
    {
        nodes = new int[1];
        nodes[0] = ins.readInt();

        z = ins.readDoubles();
        truth = ins.readDoubles();
        P = ins.readMatrix();
        attributes = Attributes.read(ins);
    }

    public Linearization linearize(Graph g, Linearization lin)
    {
        if (lin == null) {
            lin = new Linearization();
            lin.J.add(new double[2][3]);
        }

        GXYTNode gn = (GXYTNode) g.nodes.get(nodes[0]);

        if (true) {
            double J[][] = lin.J.get(0);
            J[0][0] = 1;
            J[1][1] = 1;
        }

        // compute the residual
        lin.R = new double[] { gn.state[0] - z[0], gn.state[1] - z[1] };

        lin.W = getW();
        return lin;
    }
}
