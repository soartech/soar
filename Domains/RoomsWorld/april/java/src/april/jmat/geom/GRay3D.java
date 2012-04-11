package april.jmat.geom;

import java.io.*;
import java.util.*;

import april.jmat.*;

/** A 3D ray. We use a 3D line internally, making the point the source
    of the ray. A few methods are overloaded to deal with the fact that
    the ray is not of infinite length **/
public class GRay3D
{
    GLine3D line; // the line that the ray is on

    double dir[];
    double source[];

    public GRay3D(double source[], double dir[])
    {
        this.source = source;
        this.dir = dir;
        this.line = new GLine3D(source, LinAlg.add(source, dir));

        this.dir = LinAlg.normalize(this.dir);
    }

    /** get a point a certain distance away from the source **/
    public double[] getPoint(double distance)
    {
        return new double[] {source[0] + distance*dir[0],
                             source[1] + distance*dir[1],
                             source[2] + distance*dir[2]};
    }

    public GLine3D getLine()
    {
        return line;
    }

    public double[] getSource()
    {
        return source;
    }

    public double[] getDir()
    {
        return dir;
    }

    public double[] intersectPlaneXY()
    {
        return intersectPlaneXY(0);
    }

    public double[] intersectPlaneXY(double z)
    {
        double dist = (source[2] - z)/dir[2];

        return new double[] {source[0] - dir[0]*dist,
                             source[1] - dir[1]*dist,
                             z};
    }

    public String toString()
    {
        return String.format(" source: (%10.3f,%10.3f,%10.3f) direction: [%10.3f %10.3f %10.3f]",
                             source[0], source[1], source[2],
                             dir[0], dir[1], dir[2]);
    }
}
