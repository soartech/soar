package april.graph;

import java.io.*;

import april.jmat.*;
import april.util.*;

/**  AT = B, where A and B are poses, and T is the constraint measured
 *  by this edge.
 *
 * Observation model is thus: T = inv(A)*B
 *
 **/
public class GXYEdge extends GEdge
{
    public double z[]; // x, y, theta
    public double truth[];
    
    public double P[][]; // covariance
    
    double W[][];  // inverse(P)
    
    MultiGaussian mg;
    
    public GXYEdge()
    {
    }
    
    public GXYEdge copy()
    {
        GXYEdge e = new GXYEdge();
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
        int a = nodes[0], b = nodes[1];
        
        GNode gna = g.nodes.get(a);
        GNode gnb = g.nodes.get(b);
        
        double xa = gna.state[0], ya = gna.state[1], ta = 0;
        double xb = gnb.state[0], yb = gnb.state[1], tb = 0;
        double sa = Math.sin(ta), ca = Math.cos(ta);
        
        if (gna instanceof GXYTNode)
            ta = gna.state[2];
        
        
        double zpred[] = LinAlg.resize(LinAlg.xytInvMul31(new double[] {xa, ya, ta},
                                                          new double[] {xb, yb, tb}),2);
        getMultiGaussian();
        
        return mg.chi2(zpred);
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
        outs.writeComment("a, b");
        outs.writeInt(nodes[0]);
        outs.writeInt(nodes[1]);
        
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
        nodes = new int[2];
        nodes[0] = ins.readInt();
        nodes[1] = ins.readInt();
        
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
            lin.J.add(new double[2][2]);
        }
        
        GNode gna = g.nodes.get(nodes[0]);
        GNode gnb = g.nodes.get(nodes[1]);
        
        double xa = gna.state[0], ya = gna.state[1], ta = gna.state[2];
        double xb = gnb.state[0], yb = gnb.state[1], tb = 0;
        double sa = Math.sin(ta), ca = Math.cos(ta);
        
        if (gna instanceof GXYTNode)
            ta = gna.state[2];
        
        // Jacobian of the constraint WRT state a
        if (true) {
            double J[][] = lin.J.get(0);
            J[0][0] = -ca;
            J[0][1] = -sa;
            J[0][2] = -sa*(xb-xa)+ca*(yb-ya);
            J[1][0] = sa;
            J[1][1] = -ca;
            J[1][2] = -ca*(xb-xa)-sa*(yb-ya);
        }
        
        // Jacobian of the constraint WRT state b
        if (true) {
            double J[][] = lin.J.get(1);
            J[0][0] = ca;
            J[0][1] = sa;
            J[1][0] = -sa;
            J[1][1] = ca;
        }
        
        // compute the residual
        lin.R = LinAlg.resize(LinAlg.xytInvMul31(new double[] {xa, ya, ta},
                                                 new double[] {xb, yb, tb}),2);
        
        LinAlg.minusEquals(lin.R, z);
        
        lin.W = getW();
        return lin;
    }
}
