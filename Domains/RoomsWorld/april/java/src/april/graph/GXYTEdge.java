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
public class GXYTEdge extends GEdge
{
    public double z[]; // x, y, theta
    public double truth[];

    public double P[][]; // covariance

    double W[][];  // inverse(P)

    MultiGaussian mg;

    public GXYTEdge()
    {
    }

    public GXYTEdge copy()
    {
        GXYTEdge e = new GXYTEdge();
        e.nodes = LinAlg.copy(nodes);
        e.z = LinAlg.copy(z);
        if (truth != null)
            e.truth = LinAlg.copy(truth);
        e.P = LinAlg.copy(P);

        if (attributes != null)
            e.attributes = attributes.copy();
        return e;
    }

    public GXYTEdge invert()
    {
        GXYTEdge ge = new GXYTEdge();
        ge.nodes = new int[] { nodes[1], nodes[0] };

        double x = z[0], y = z[1], theta = z[2];
        double s = Math.sin(theta), c = Math.cos(theta);

        ge.z = LinAlg.xytInverse(z);

        double J11 = -c, J12 = -s, J13 = -c*y + s*x;
        double J21 = s, J22 = -c, J23 = s*y + c*x;
        double P11 = P[0][0];
        double P12 = P[0][1];
        double P13 = P[0][2];
        double P22 = P[1][1];
        double P23 = P[1][2];
        double P33 = P[2][2];

        double Z[][] = new double[3][3];
        Z[0][0] = P11*J11*J11 + 2*P12*J11*J12 + 2*P13*J11*J13 + P22*J12*J12 + 2*P23*J12*J13 + P33*J13*J13;
        Z[0][1] = J21*(J11*P11 + J12*P12 + J13*P13) + J22*(J11*P12 + J12*P22 + J13*P23) + J23*(J11*P13 + J12*P23 + J13*P33);
        Z[0][2] = - J11*P13 - J12*P23 - J13*P33;
        Z[1][0] = Z[0][1];
        Z[1][1] = P11*J21*J21 + 2*P12*J21*J22 + 2*P13*J21*J23 + P22*J22*J22 + 2*P23*J22*J23 + P33*J23*J23;
        Z[1][2] = - J21*P13 - J22*P23 - J23*P33;
        Z[2][0] = Z[0][2];
        Z[2][1] = Z[1][2];
        Z[2][2] = P33;

        ge.P = Z;

        /*
        // the code above is just an unrolling of the following:

        double J[][] = new double[][] { { -c, -s, -c*y + s*x },
                                        { s,  -c,  s*y + c*x },
                                        { 0,   0,     -1     } };
        ge.P = LinAlg.matrixABCt(J, P, J);

        */

        return ge;
    }

    public GXYTEdge compose(GXYTEdge ge)
    {
        if (ge == null)
            return this;

        GXYTEdge newge = new GXYTEdge();
        double xa = z[0], ya = z[1], ta = z[2];
        double xb = ge.z[0], yb = ge.z[1], tb = ge.z[2];

        double sa = Math.sin(ta), ca = Math.cos(ta);

        double P11 = P[0][0];
        double P12 = P[0][1];
        double P13 = P[0][2];
        double P22 = P[1][1];
        double P23 = P[1][2];
        double P33 = P[2][2];

        double Q11 = ge.P[0][0];
        double Q12 = ge.P[0][1];
        double Q13 = ge.P[0][2];
        double Q22 = ge.P[1][1];
        double Q23 = ge.P[1][2];
        double Q33 = ge.P[2][2];

        double JA13 = -sa*xb - ca*yb;
        double JA23 = ca*xb - sa*yb;
        double JB11 = ca;
        double JB12 = -sa;
        double JB21 = sa;
        double JB22 = ca;

        double Z[][] = new double[3][3];
        Z[0][0] = P33*JA13*JA13 + 2*P13*JA13 + Q11*JB11*JB11 + 2*Q12*JB11*JB12 + Q22*JB12*JB12 + P11;
        Z[0][1] = P12 + JA23*(P13 + JA13*P33) + JA13*P23 + JB21*(JB11*Q11 + JB12*Q12) + JB22*(JB11*Q12 + JB12*Q22);
        Z[0][2] = P13 + JA13*P33 + JB11*Q13 + JB12*Q23;
        Z[1][0] = Z[0][1];
        Z[1][1] = P33*JA23*JA23 + 2*P23*JA23 + Q11*JB21*JB21 + 2*Q12*JB21*JB22 + Q22*JB22*JB22 + P22;
        Z[1][2] = P23 + JA23*P33 + JB21*Q13 + JB22*Q23;
        Z[2][0] = Z[0][2];
        Z[2][1] = Z[1][2];
        Z[2][2] = P33 + Q33;

        newge.P = Z;

/*
  // the code above is just an unrolling of the following:

        double JA[][] = new double[][] { { 1, 0, -sa*xb - ca*yb },
                                         { 0, 1, ca*xb - sa*yb },
                                         { 0, 0, 1 } };
        double JB[][] = new double[][] { { ca, -sa, 0 },
                                         { sa, ca, 0 },
                                         { 0,  0,  1 } };

        newge.P = LinAlg.add(LinAlg.matrixABCt(JA, P, JA),
                             LinAlg.matrixABCt(JB, ge.P, JB));
*/

        newge.z = LinAlg.xytMultiply(z, ge.z);

        if (truth != null && ge.truth != null)
            newge.truth = LinAlg.xytMultiply(truth, ge.truth);

        newge.nodes = new int[] { nodes[0], ge.nodes[1] };

        return newge;
    }

    public double[][] getW()
    {
        if (W == null)
            W = LinAlg.inverse(P);
        return W;
    }

    public double getChi2(Graph g)
    {
        GXYTNode gna = (GXYTNode) g.nodes.get(nodes[0]);
        GXYTNode gnb = (GXYTNode) g.nodes.get(nodes[1]);

        double zpred[] = LinAlg.xytInvMul31(gna.state, gnb.state);
        getMultiGaussian();
        zpred[2] = MathUtil.mod2pi(mg.getMean()[2], z[2]);

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
        return 3;
    }

    public void write(StructureWriter outs) throws IOException
    {
        outs.writeComment("a, b");
        outs.writeInt(nodes[0]);
        outs.writeInt(nodes[1]);

        outs.writeComment("XYT");
        outs.writeDoubles(z);
        outs.writeComment("XYT truth");
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
            lin.J.add(new double[3][3]);
            lin.J.add(new double[3][3]);
        }

        GXYTNode gna = (GXYTNode) g.nodes.get(nodes[0]);
        GXYTNode gnb = (GXYTNode) g.nodes.get(nodes[1]);

        double xa = gna.state[0], ya = gna.state[1], ta = gna.state[2];
        double xb = gnb.state[0], yb = gnb.state[1], tb = gnb.state[2];
        double sa = Math.sin(ta), ca = Math.cos(ta);

        // Jacobian of the constraint WRT state a
        if (true) {
            double J[][] = lin.J.get(0);
            J[0][0] = -ca;
            J[0][1] = -sa;
            J[0][2] = -sa*(xb-xa)+ca*(yb-ya);
            J[1][0] = sa;
            J[1][1] = -ca;
            J[1][2] = -ca*(xb-xa)-sa*(yb-ya);
            J[2][2] = -1;
        }

        // Jacobian of the constraint WRT state b
        if (true) {
            double J[][] = lin.J.get(1);
            J[0][0] = ca;
            J[0][1] = sa;
            J[1][0] = -sa;
            J[1][1] = ca;
            J[2][2] = 1;
        }

        // compute the residual
        lin.R = LinAlg.xytInvMul31(new double[] {xa, ya, ta},
                                   new double[] {xb, yb, tb});
        LinAlg.minusEquals(lin.R, z);
        lin.R[2] = MathUtil.mod2pi(lin.R[2]);

        lin.W = getW();

        return lin;
    }
}
