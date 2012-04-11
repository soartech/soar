package april.jmat.geom;

import java.util.*;

/** A circle in the plane **/
public class GCircle2D
{
    public double p[];
    public double r;

    public GCircle2D(double p[], double r)
    {
        this.p = p;
        this.r = r;
    }

    public ArrayList<double[]> intersect(GLine2D line)
    {
        ArrayList<double[]> ps = new ArrayList<double[]>();

        // slope of the line
        double dx = line.getDx();
        double dy = line.getDy();

        // a point that goes through the line
        double lp[] = line.getPoint();

        double rx = lp[0] - p[0];
        double ry = lp[1] - p[1];

        // terms of the quadratic formula
        double a = dx*dx + dy*dy;
        double b = 2*rx*dx + 2*ry*dy;
        double c = rx*rx+ry*ry-r*r;

        double b2m4ac = b*b - 4*a*c;
        if (b2m4ac < 0)
            return ps;

        double t1 = (-b+Math.sqrt(b2m4ac))/(2*a);
        double t2 = (-b-Math.sqrt(b2m4ac))/(2*a);

        ps.add(new double[] {lp[0] + dx*t1, lp[1] + dy*t1});

        if (b2m4ac != 0)
            ps.add(new double[] {lp[0] + dx*t2, lp[1] + dy*t2});

        return ps;
    }

    public ArrayList<double[]> intersect(GLineSegment2D seg)
    {
        ArrayList<double[]> psin = intersect(seg.getLine());
        ArrayList<double[]> psout = new ArrayList<double[]>();

        double a = seg.line.getLineCoordinate(seg.p1);
        double b = seg.line.getLineCoordinate(seg.p2);

        for (double p[] : psin)
	    {
            double x = seg.line.getLineCoordinate(p);
            if (a<=x && x<=b)
                psout.add(p);
            else if (b<=x && x<=a)
                psout.add(p);
	    }

        return psout;
    }

    public ArrayList<double[]> intersect(GCircle2D circ)
    {
        ArrayList<double[]> result = new ArrayList<double[]>();
        double dx = circ.p[0] - this.p[0];
        double dy = circ.p[1] - this.p[1];

        double distance = Math.sqrt( dx*dx + dy*dy );

        if( distance > this.r + circ.r ||
            distance < Math.abs( this.r - circ.r ) ) return result;

        double a = (this.r*this.r - circ.r*circ.r + distance*distance) /
            (2 * distance);

        double h = Math.sqrt( this.r*this.r - a*a );

        double tx = this.p[0] + a * dx / distance;
        double ty = this.p[1] + a * dy / distance;

        result.add(new double[] { tx + h * dy / distance,
                                  ty - h * dx / distance });
        result.add(new double[] { tx - h * dy / distance,
                                  ty + h * dx / distance });
        return result;
    }
}
