package april.jmat.geom;

import java.io.*;
import java.util.*;

import april.jmat.*;

/** A 2D line with end points. **/
public class GLineSegment2D implements Serializable
{
    public GLine2D line;
    public double p1[];
    public double p2[];
    public int weight;

    static final long serialVersionUID=1001;

    public GLineSegment2D(double p1[], double p2[])
    {
        this.p1=p1;
        this.p2=p2;
        this.line=new GLine2D(p1,p2);
    }

    public GLine2D getLine()
    {
        return line;
    }

    /** Returns the point where this segment intersects this line, or null
     * if they do not intersect.
     **/
    public double[] intersectionWith(GLine2D l)
    {
        double p[]=line.intersectionWith(l);
        if (p==null)
            return null; //parallel!

        double a,b,c;
        a=line.getLineCoordinate(p1);
        b=line.getLineCoordinate(p2);
        c=line.getLineCoordinate(p);

        // b must be between a and c.
        if ((a<b && b<c) || (c<b && b<a))
            return p;

        return null;
    }

    public double[] intersectionWith(GLineSegment2D seg)
    {
        double p[]=line.intersectionWith(seg.line);
        if (p==null)
            return null; //parallel!

        double a,b,c;
        a=line.getLineCoordinate(p1);
        b=line.getLineCoordinate(p2);
        c=line.getLineCoordinate(p);

        // does intersection lie on first line?
        if ((c<a && c<b) || (c>a && c>b))
            return null;

        a=seg.line.getLineCoordinate(seg.p1);
        b=seg.line.getLineCoordinate(seg.p2);
        c=seg.line.getLineCoordinate(p);

        // does intersection lie on first line?
        if ((c<a && c<b) || (c>a && c>b))
            return null;

        return p;
    }

    public double[] closestPoint(double p[])
    {
        double pclosest[]=line.pointOnLineClosestTo(p);

        double a,b,c;
        a=line.getLineCoordinate(p1);
        b=line.getLineCoordinate(p2);
        c=line.getLineCoordinate(pclosest);

        if (c<a && c<b)
	    {
            if (a<b)
                return p1;
            else
                return p2;
	    }
        if (c>a && c>b)
	    {
            if (a>b)
                return p1;
            else
                return p2;
	    }

        return pclosest;
    }

    public double squaredDistanceTo(double p[])
    {
        double pclosest[] = closestPoint(p);

        return LinAlg.squaredDistance(p, pclosest);
    }

    public double length()
    {
        return LinAlg.distance(p1, p2);
    }

    public double distanceTo(double p[])
    {
        double pclosest[] = closestPoint(p);

        return LinAlg.distance(p, pclosest, 2);
    }

    public double lengthOfProjectionOnto(GLineSegment2D seg)
    {
        double pp1[]=line.pointOnLineClosestTo(seg.p1);
        double pp2[]=line.pointOnLineClosestTo(seg.p2);

        double a = line.getLineCoordinate(p1);
        double c = line.getLineCoordinate(p2);

        double l0, l1;

        double acmin = Math.min(a,c);
        double acmax = Math.max(a,c);

        l0 = line.getLineCoordinate(pp1);
        l1 = line.getLineCoordinate(pp2);

        if (l0 < acmin && l1 < acmin)
            return 0;

        if (l0 > acmax && l1 > acmax)
            return 0;

        l0 = Math.max(acmin, l0);
        l0 = Math.min(acmax, l0);
        l1 = Math.max(acmin, l1);
        l1 = Math.min(acmax, l1);

        //	System.out.print(Math.abs(l1-l0)+" ");
        return Math.abs(l1-l0);
    }

    public static GLineSegment2D lsqFit(List<double[]> points, double weights[])
    {
        GLine2D gline = GLine2D.lsqFit(points, weights);

        double maxcoord = -Double.MAX_VALUE;
        double mincoord = Double.MAX_VALUE;

        for (double p[] : points) {
            double coord = gline.getLineCoordinate(p);
            maxcoord = Math.max(maxcoord, coord);
            mincoord = Math.min(mincoord, coord);
        }

        return new GLineSegment2D(gline.getPointOfCoordinate(mincoord),
                                  gline.getPointOfCoordinate(maxcoord));
    }

    /** xyweight is a list of 3x1 arrays that will be interpreted as (x,y,weight) **/
    public static GLineSegment2D lsqFitXYW(List<double[]> xyweight)
    {
        GLine2D gline = GLine2D.lsqFitXYW(xyweight);

        double maxcoord = -Double.MAX_VALUE;
        double mincoord = Double.MAX_VALUE;

        for (double p[] : xyweight) {
            double coord = gline.getLineCoordinate(p);
            maxcoord = Math.max(maxcoord, coord);
            mincoord = Math.min(mincoord, coord);
        }

        return new GLineSegment2D(gline.getPointOfCoordinate(mincoord),
                                  gline.getPointOfCoordinate(maxcoord));
    }

}
