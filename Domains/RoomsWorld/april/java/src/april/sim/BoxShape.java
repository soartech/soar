package april.sim;

import april.jmat.*;
import java.util.*;

public class BoxShape implements Shape
{
    double sxyz[];

    ArrayList<double[]> planes;
    ArrayList<double[]> vertices;

    public BoxShape(double sx, double sy, double sz)
    {
        this(new double[] { sx, sy, sz } );
    }

    public BoxShape(double sxyz[])
    {
        this.sxyz = LinAlg.copy(sxyz);
        planes = new ArrayList<double[]>();
        vertices = new ArrayList<double[]>();

        // create planes and vertices
        planes.add(new double[] { -1, 0, 0, -sxyz[0]/2 });
        planes.add(new double[] {  1, 0, 0, -sxyz[0]/2 });

        planes.add(new double[] { 0, -1, 0, -sxyz[1]/2 });
        planes.add(new double[] { 0,  1, 0, -sxyz[1]/2 });

        planes.add(new double[] { 0, 0, -1, -sxyz[2]/2 });
        planes.add(new double[] { 0, 0,  1, -sxyz[2]/2 });

        for (int i = -1; i < 2; i+=2)
            for (int j = -1; j < 2; j+=2)
                for (int k = -1; k < 2; k+=2)
                    vertices.add(new double[] { i*sxyz[0]/2, j*sxyz[1]/2, k*sxyz[2]/2 });
    }

    protected BoxShape()
    {
    }

    public BoxShape transform(double T[][])
    {
        BoxShape bs = new BoxShape();
        bs.sxyz = LinAlg.copy(sxyz);
        bs.vertices = LinAlg.transform(T, vertices);
        bs.planes = LinAlg.transformPlanes(T, planes);
        return bs;
    }

}
