package april.jmat.geom;

import april.jmat.*;

public class GTriangle2D
{
    public double p[][]; // the three vertices, counter-clockwise.

    public GTriangle2D(double p[][])
    {
        assert(p.length==3);
        for (int i = 0; i < 3; i++)
            assert(p[i].length==2);

        this.p = p;
    }

    public boolean isInside(double xy[])
    {
        return LinAlg.pointInsideTriangle(p[0], p[1], p[2], xy);
    }

    public double getArea()
    {
        // (stable) heron's method.

        // lengths of the three sides
        double A = LinAlg.distance(p[0], p[1]);
        double B = LinAlg.distance(p[1], p[2]);
        double C = LinAlg.distance(p[2], p[0]);

        // sort sides so that AA >= BB >= CC
        double AA, BB, CC;
        if (A >= B && A >= C) {
            AA = A;
            BB = Math.max(B, C);
            CC = Math.min(B, C);
        } else if (B >= A && B >=C) {
            AA = B;
            BB = Math.max(A, C);
            CC = Math.min(A, C);
        } else {
            AA = C;
            BB = Math.max(A, B);
            CC = Math.min(A, B);
        }

        return 0.25*Math.sqrt((AA+(BB+CC))*(CC-(AA-BB))*(CC+(AA-BB))*(AA+(BB-CC)));
    }

    public static void main(String args[])
    {
        GTriangle2D gt = new GTriangle2D(new double[][] { {0,0},
                                                          {4,0},
                                                          {0,1} });

        System.out.println(gt.getArea());
        assert(gt.isInside(new double[] {.1,.1}));
        assert(!gt.isInside(new double[] {3, 3}));
    }
}
