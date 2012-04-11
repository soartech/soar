package april.jmat.geom;

import april.jmat.*;

import java.util.*;

/** Simple (non-intersecting) polygon.
 **/
public class Polygon
{
    ArrayList<double[]> points;
    ArrayList<int[]> triangles; // indices give the points.

    boolean ccw; // are the points in CCW order?

    static boolean warned;

    public Polygon(ArrayList<double[]> points)
    {
        this.points = points;

        if (points.size() < 3)
            return;

        // Step one: we want the points in counter-clockwise order.
        // If the points are in clockwise order, we'll reverse them.
        double totaltheta = 0;
        double lasttheta = 0;

        // Count the angle accumulated going around the polygon. If
        // the sum is +2pi, it's CCW. Otherwise, we'll get -2pi.
        for (int i = 0; i <= points.size(); i++) {
            double p1[] = points.get((i+1)%points.size());
            double p0[] = points.get(i%points.size());

            double thistheta = Math.atan2(p1[1]-p0[1], p1[0]-p0[0]);

            if (i > 0) {
                double dtheta = MathUtil.mod2pi(thistheta-lasttheta);
                totaltheta += dtheta;
            }

            lasttheta = thistheta;
        }

        ccw = (totaltheta > 0);
    }

    public boolean getClockwise()
    {
        return !ccw;
    }

    public double[] getPoint(int idx)
    {
        return points.get(idx);
    }

    public ArrayList<double[]> getPoints()
    {
        return points;
    }

    public ArrayList<int[]> getTriangles()
    {
        tesselate();

        return triangles;
    }

    void tesselate()
    {
        if (triangles != null)
            return;

        triangles = new ArrayList<int[]>();

        int position = 0;
        int next[] = new int[points.size()];
        if (ccw) {
            for (int i = 0; i < next.length; i++)
                next[i] = (i+1)%next.length;
        } else {
            for (int i = 0; i < next.length; i++)
                next[i] = (i+next.length - 1)%next.length;
        }

        double ps[][] = new double[3][];
        double v10[] = new double[3], v21[] = new double[3];

        int pointsRemovedThisTime = 0;
        int steps = 0;
        int npointsLeft = points.size();

        /** This is a fairly naive polygon tesselation algorithm
         * requiring O(N^2) time. It works for all simple (non
         * self-intersecting) polygons obeying a left-handed winding
         * order. **/
        while (npointsLeft >= 3) {
            int idx0 = position;
            int idx1 = next[idx0];
            int idx2 = next[idx1];

            ps[0] = points.get(idx0);
            ps[1] = points.get(idx1);
            ps[2] = points.get(idx2);

            v10[0] = ps[1][0] - ps[0][0];
            v10[1] = ps[1][1] - ps[0][1];

            v21[0] = ps[2][0] - ps[1][0];
            v21[1] = ps[2][1] - ps[1][1];

            double crossProduct[] = LinAlg.crossProduct(v10, v21);

            // do the lines from p(idx)->p(idx1) and p(idx1)->pidx(2)
            // obey the convex polygon winding rule?
            boolean convex = crossProduct[2]>0;

            // the line segment (from ps[0] to ps[2]) must be
            // interior: it cannot intersect any edge
            boolean interior = true;

            if (convex && npointsLeft > 3) {

                int thispos = next[idx2];

                while (thispos != idx0) {

                    if (LinAlg.pointInsideTriangle(ps[0], ps[1], ps[2], points.get(thispos))) {
                        interior = false;
                        break;
                    }

                    thispos = next[thispos];
                }
            }

            if (!convex || !interior) {

                steps++;

                // nope, walk around to the next point.
                if (steps > points.size()) {
                    if (pointsRemovedThisTime == 0) {
                        // bad polygon
                        if (!warned) {
                            System.out.println("jmat.geom.Polygon one-time warning: Self-intersecting polygon");
                            warned = true;

                            //for (int i = 0; i < points.size(); i++)
                            // System.out.printf("%3d %15f %15f\n", i, points.get(i)[0], points.get(i)[1]);
                        }
                        // triangles.clear();
                        break;
                    }
                    pointsRemovedThisTime = 0;
                }

                position = next[position];
                continue;
            }

            int triangle[] = new int[3];
            triangle[0] = idx0;
            triangle[1] = idx1;
            triangle[2] = idx2;
            triangles.add(triangle);

            // we now remove the inner point (idx1)
            next[idx0] = idx2;
            npointsLeft--;
            pointsRemovedThisTime++;
            steps = 0;
        }
    }

    public boolean contains(double q[])
    {
        tesselate();

        for (int triangle[]: triangles) {
            double p0[] = points.get(triangle[0]);
            double p1[] = points.get(triangle[1]);
            double p2[] = points.get(triangle[2]);

            if (LinAlg.pointInsideTriangle(p0, p1, p2, q))
                return true;
        }

        return false;
    }

    public static void main(String args[])
    {
        ArrayList<double[]> points = new ArrayList<double[]>();

        if (false) {
            points.add(new double[] {0,0});
            points.add(new double[] {1,0});
            points.add(new double[] {1,1});
            points.add(new double[] {0,1});
        } else {
            points.add(new double[] {0,1});
            points.add(new double[] {1,1});
            points.add(new double[] {1,0});
            points.add(new double[] {0,0});
        }
        Polygon p = new Polygon(points);
    }
}
