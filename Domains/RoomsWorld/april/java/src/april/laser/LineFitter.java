package april.laser;

import java.util.*;
import april.jmat.geom.*;
import april.jmat.*;
import april.config.*;

public class LineFitter
{
    // mean distance^2 (in meters) from the fit line
    public double              errorThresh = Math.pow(0.03, 2);

    // minimum number of points in the line
    public int                 minPoints = 4;

    // maximum gap (in meters) between any two points
    public double              maxSpan = 1.5;

    /////////////////////////////

    /** Given a set of points that are radially ordered and physically
     * proximate, (i.e., contours), fit lines through consecutive
     * points.
     *
     * For normal estimation to work correctly, points should be
     * observed from approximately the origin.
     **/
    public LineFitter(Config config)
    {
        if (config == null)
            return;
    }

    public ArrayList<LineFeature> getLineFeatures(ArrayList<double[]> points)
    {
        Instance inst = new Instance();
        inst.points = points;

        return inst.getLineFeatures();
    }

    class Instance
    {
        ArrayList<double[]> points;
        ArrayList<FitterSegment> segments = new ArrayList<FitterSegment>();
        TreeSet<Join> joins = new TreeSet<Join>();

        public ArrayList<LineFeature> getLineFeatures()
        {
            // create the segments
            for (int i = 0; i < points.size()-1; i++) {
                double a[] = points.get(i);
                double b[] = points.get(i+1);
                if (LinAlg.distance(a, b, 2) > maxSpan)
                    continue;

                segments.add(new FitterSegment(i, i+1));
            }

            // create the joins
            for (int i = 0; i < segments.size() - 1; i++) {
                FitterSegment leftSegment = segments.get(i);
                FitterSegment rightSegment = segments.get(i+1);

                Join j = new Join(leftSegment, rightSegment);

                joins.add(j);
                leftSegment.rightJoin = j;
                rightSegment.leftJoin = j;
            }

            // now join segments until we reach our error threshold.
            while (joins.size() > 0) {
                Join j = joins.first();
                joins.remove(j);

                // if this join has too great error, we're done.
                if (j.leftrightSeg.error > errorThresh)
                    break;

                // otherwise, this join is a keeper. We'll take
                // j.leftrightSeg (which is already the union of the left
                // and right segments) and create two new joins connecting
                // it to its neighbors.
                Join leftJoin = j.leftSeg.leftJoin;
                Join rightJoin = j.rightSeg.rightJoin;

                // We have no further use for the old left and right
                // segment. Rather than using j.leftrightSeg, we'll reuse
                // leftSeg. This will have the happy property that
                // connected lines will be together. Otherwise, if we just
                // added j.leftRightSeg, lines get all juggled around.
                FitterSegment leftRightSegPtr = j.leftSeg;
                leftRightSegPtr.setTo(j.leftrightSeg);

                // the rightSeg we just delete.
                j.rightSeg.deleted=true;
                FitterSegment leftSegment=null, rightSegment=null;

                if (leftJoin!=null) {

                    leftSegment=leftJoin.leftSeg;
                    joins.remove(leftJoin);

                    Join newj=new Join(leftSegment, leftRightSegPtr);
                    newj.leftSeg=leftSegment;
                    leftSegment.rightJoin=newj;
                    newj.rightSeg=leftRightSegPtr;
                    leftRightSegPtr.leftJoin=newj;

                    joins.add(newj);
                }

                if (rightJoin!=null) {

                    rightSegment=rightJoin.rightSeg;
                    joins.remove(rightJoin);

                    Join newj=new Join(leftRightSegPtr, rightSegment);
                    newj.leftSeg=leftRightSegPtr;
                    leftRightSegPtr.rightJoin = newj;
                    newj.rightSeg=rightSegment;
                    rightSegment.leftJoin=newj;

                    joins.add(newj);
                }
            }

            // reject bad segments.
            ArrayList<FitterSegment>  goodfsegments = new ArrayList<FitterSegment>();

            for (FitterSegment seg : segments) {

                if (!seg.deleted && seg.nPoints>=minPoints) {
                    GLineSegment2D newseg = new GLineSegment2D(seg.p1, seg.p2);
                    newseg.weight=seg.nPoints;
                    goodfsegments.add(seg);
                }

                seg.leftJoin=null;
                seg.rightJoin=null;
            }

            segments=goodfsegments;

            ///////////////////////////////////
            // almost done....
            ArrayList<LineFeature> lfs = new ArrayList<LineFeature>();

            for (FitterSegment seg : segments) {
                LineFeature lf = new LineFeature();
                lf.seg = new GLineSegment2D(seg.p1, seg.p2);
                lf.npoints = seg.nPoints;
                lf.normal = computeNormal(seg.p1, seg.p2);

                lfs.add(lf);
            }

            return lfs;
        }

        double computeNormal(double p0[], double p1[])
        {
            double dx = p0[0] - p1[0];
            double dy = p0[1] - p1[1];

            // compute angle of line segment perpendicular to the line OP-P
            double theta = Math.atan2(-dx, dy);

            // make sure we get the angle that points towards the laser scanner
            // (pick the possibility that is closest to the projection through the origin)
            double dumbtheta = Math.atan2(-p0[1], -p0[0]);
            double err = Math.abs(MathUtil.mod2pi(dumbtheta - theta));
            if (err > Math.PI/2)
                theta += Math.PI;

            theta = MathUtil.mod2pi(Math.PI, theta);

            return theta;
        }

        FitterSegment joinSegments(FitterSegment seg1, FitterSegment seg2)
        {
            FitterSegment outseg=new FitterSegment();

            outseg.mX=seg1.mX+seg2.mX;
            outseg.mXX=seg1.mXX+seg2.mXX;
            outseg.mY=seg1.mY+seg2.mY;
            outseg.mYY=seg1.mYY+seg2.mYY;
            outseg.mXY=seg1.mXY+seg2.mXY;

            // correct our statistics by removing any extra copies of
            // points we might have due to the segments overlapping.
            // we reorder the points so that we have:
            //
            // small indexes --------> high indexes
            //
            // a------b        or     a------b
            //     c------d                     c------d
            //
            int a, b, c, d;
            if (seg1.pLo<seg2.pLo) {
                a=seg1.pLo;
                b=seg1.pHi;
                c=seg2.pLo;
                d=seg2.pHi;
            } else {
                a=seg2.pLo;
                b=seg2.pHi;
                c=seg1.pLo;
                d=seg1.pHi;
            }

            assert(a<=b);
            assert(c<=d);
            assert(a<=d);
            assert(a<=c);
            assert(b<=d);

            // remove twice-counted nodes
            int duplicatepoints=0;
            for (int idx=c;idx<=b;idx++) {
                assert(b==c);
                duplicatepoints++;

                double p[] = points.get(idx);

                outseg.mX-=p[0];
                outseg.mY-=p[1];
                outseg.mXX-=p[0]*p[0];
                outseg.mYY-=p[1]*p[1];
                outseg.mXY-=p[0]*p[1];
            }

            outseg.pLo=a;
            outseg.pHi=d;
            outseg.nPoints=seg1.nPoints+seg2.nPoints-duplicatepoints; //d-a+1;

            outseg.fitLine();

            // check for excessive join length
            // new edges are b -> max(b+1,c)
            int e=b;
            int f=Math.max(b+1,c);
            if (LinAlg.distance(points.get(e), points.get(f))>maxSpan)
                outseg.error=Double.MAX_VALUE;

            return outseg;
        }

        ////////////////////////////////////////////////////////////////////////
        // Fitter Segment
        class Join implements Comparable
        {
            FitterSegment leftSeg, rightSeg;
            FitterSegment leftrightSeg;

            Join(FitterSegment left, FitterSegment right)
            {
                this.leftSeg=left;
                this.rightSeg=right;

                this.leftrightSeg=joinSegments(left,right);
            }

            public int compareTo(Object o)
            {
                return Double.compare(leftrightSeg.error,((Join)o).leftrightSeg.error);
            }
        }

        ////////////////////////////////////////////////////////////////////////
        // Fitter Segment
        class FitterSegment
        {
            /** end points of the line segment **/
            double p1[] = new double[2];
            double p2[] = new double[2];
            GLine2D line = new GLine2D();

            int nPoints=0;

            // moments
            double mX=0, mY=0, mXX=0, mYY=0, mXY=0;

            // average error
            double error=0;

            // Index into the PointSet where to find these points.
            int pLo=0, pHi=0;

            // used by segment internally
            Join leftJoin=null;
            Join rightJoin=null;

            // set when this linesegment is joined
            boolean deleted=false;

            static final long serialVersionUID=1001;

            FitterSegment()
            {
            }

            void setTo(FitterSegment s)
            {
                this.p1=s.p1;
                this.p2=s.p2;
                this.line=s.line;
                this.nPoints=s.nPoints;
                this.mX=s.mX;
                this.mY=s.mY;
                this.mXX=s.mXX;
                this.mYY=s.mYY;
                this.mXY=s.mXY;
                this.error=s.error;
                this.pLo=s.pLo;
                this.pHi=s.pHi;
                this.leftJoin=s.leftJoin;
                this.rightJoin=s.rightJoin;
                this.deleted=s.deleted;
            }

            // create a line segment from the points [i,j] in set p. j>=i.
            FitterSegment(int i, int j)
            {
                double p1[]=points.get(i);
                double p2[]=points.get(j);

                for (int idx=i;idx<=j;idx++)
                {
                    double p[]=points.get(idx);

                    mX+=p[0];
                    mY+=p[1];
                    mXX+=p[0]*p[0];
                    mYY+=p[1]*p[1];
                    mXY+=p[0]*p[1];
                }

                pLo=i;
                pHi=j;
                nPoints=j-i+1;

                fitLine();
            }

            // compute best line for this segment
            protected void fitLine()
            {
                // estimate the new line
                double Cxx,Cyy,Cxy,Ex,Ey;

                // compute covariances and expectations
                Cxx=mXX/nPoints-Math.pow(mX/nPoints,2);
                Cyy=mYY/nPoints-Math.pow(mY/nPoints,2);
                Cxy=mXY/nPoints-(mX/nPoints)*(mY/nPoints);
                Ex=mX/nPoints;
                Ey=mY/nPoints;

                // find dominant direction via SVD
                double phi=0.5*Math.atan2(-2*Cxy,(Cyy-Cxx));
                double rho=Ex*Math.cos(phi) + Ey*Math.sin(phi);

                // compute line parameters
                line=new GLine2D(-Math.sin(phi), Math.cos(phi), new double[] {Ex, Ey});

                // compute the error
                error=0;

                p1=line.pointOnLineClosestTo(points.get(pLo));
                p2=line.pointOnLineClosestTo(points.get(pHi));

                for (int idx=pLo;idx<=pHi;idx++) {

                    double p[]=points.get(idx);

                    GLine2D radialLine=new GLine2D(new double[2], p);
                    double inter[]=radialLine.intersectionWith(line);

                    if (inter==null) {
                        // these lines are very nearly parallel. The error would be huge.
                        error+=Double.MAX_VALUE;
                        break;
                    } else {
                        double d=LinAlg.distance(inter, p, 2);
                        error+=d*d;
                    }
                }
                error/=nPoints;
            }
        }
    }
}
