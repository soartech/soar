package april.sim;

import java.awt.*;
import java.io.*;

import april.vis.*;
import april.jmat.*;
import april.util.*;

public class SimBox implements SimObject
{
    double T[][] = LinAlg.identity(4);  // position
    double sxyz[] = new double[3]; // size
    Color  color = Color.gray;

    public SimBox(SimWorld sw)
    {
    }

    public double[][] getPose()
    {
        this.T[2][3] = sxyz[2] / 2;
        return LinAlg.copy(T);
    }

    public void setPose(double T[][])
    {
        this.T = LinAlg.copy(T);
    }

    public Shape getShape()
    {
        return new BoxShape(sxyz);
    }

    public VisObject getVisObject()
    {
        return new VisBox(sxyz[0], sxyz[1], sxyz[2], color);
    }

    /** Restore state that was previously written **/
    public void read(StructureReader ins) throws IOException
    {
        double xyzrpy[] = ins.readDoubles();
        this.T = LinAlg.xyzrpyToMatrix(xyzrpy);

        this.sxyz = ins.readDoubles();

        double c[] = ins.readDoubles();
        this.color = new Color((float) c[0], (float) c[1], (float) c[2], (float) c[3]);
    }

    /** Write one or more lines that serialize this instance. No line
     * is allowed to consist of just an asterisk. **/
    public void write(StructureWriter outs) throws IOException
    {
        outs.writeDoubles(LinAlg.matrixToXyzrpy(T));
        outs.writeDoubles(sxyz);
        float f[] = color.getRGBComponents(null);
        outs.writeDoubles(LinAlg.copyDoubles(f));
    }

    public void setRunning(boolean b)
    {
    }
}
