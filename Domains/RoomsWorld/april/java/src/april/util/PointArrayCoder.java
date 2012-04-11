package april.util;

import java.util.*;
import java.io.*;

public class PointArrayCoder implements StructureCoder
{
    public PointArrayCoder()
    {
    }

    public void write(StructureWriter outs, Object o) throws IOException
    {
        assert (o instanceof ArrayList);
        ArrayList<double[]> points = (ArrayList<double[]>) o;

        if (points.size() == 0) {
            outs.writeMatrix(null);
        } else {
            double ppoints[][] = points.toArray(new double[0][0]);
            outs.writeMatrix(ppoints);
        }
    }

    public Object read(StructureReader ins) throws IOException
    {
        ArrayList<double[]> points = new ArrayList<double[]>();

        double ppoints[][] = ins.readMatrix();

        if (ppoints != null) {
            for (double point[] : ppoints)
                points.add(point);
        }

        return points;
    }
}
