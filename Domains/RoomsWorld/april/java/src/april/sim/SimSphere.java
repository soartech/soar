package april.sim;

import java.awt.*;
import java.io.*;

import april.vis.*;
import april.jmat.*;
import april.util.*;

public class SimSphere implements SimObject
{
    double T[][] = LinAlg.identity(4);  // position
    double r = 1.0;  // radius
    Color color = Color.gray;

    public SimSphere(SimWorld sw)
    {
    }

    public double[][] getPose()
    {
        this.T[2][3] = r;
        return LinAlg.copy(T);
    }

    public void setPose(double T[][])
    {
        this.T = LinAlg.copy(T);
    }

    public Shape getShape()
    {
        return new SphereShape(r);
    }

    public VisObject getVisObject()
    {
        return new VisSphere(r, color);
    }

    /** Restore state that was previously written **/
    public void read(StructureReader ins) throws IOException
    {
        double xyzrpy[] = ins.readDoubles();
        this.T = LinAlg.xyzrpyToMatrix(xyzrpy);

        this.r = ins.readDouble();

        double c[] = ins.readDoubles();
        this.color = new Color((float) c[0], (float) c[1], (float) c[2], (float) c[3]);
    }

    /** Write one or more lines that serialize this instance. No line
     * is allowed to consist of just an asterisk. **/
    public void write(StructureWriter outs) throws IOException
    {
        outs.writeDoubles(LinAlg.matrixToXyzrpy(T));
        outs.writeDouble(r);
        float f[] = color.getRGBComponents(null);
        outs.writeDoubles(LinAlg.copyDoubles(f));
    }

    public void setRunning(boolean b)
    {
    }
}
