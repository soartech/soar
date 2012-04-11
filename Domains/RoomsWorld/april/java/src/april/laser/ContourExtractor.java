package april.laser;

import april.config.*;
import april.jmat.*;

import java.util.*;

/** Find plausibly connected components: a pre-processing step for
 * line extraction.
 **/
public class ContourExtractor
{
    /** How many points can we skip over in order to connect contours? **/
    public int maxSkipPoints = 3;

    /** If two points are adjacent (relative to scan order), and they
        are really close together, join them even if another point is
        closer.
    **/
    public double adjacentAcceptDistance = 0.1;

    /** When adding a point to a contour, it's never okay to add a
     * point farther than this away.
     **/
    public double maxDistance = 5;

    /** When starting a new contour, pretend that the last two points
     * were this far apart. This affects maxDistanceRatio, and
     * effectively limits our willingness to create contours that
     * contain only sparsely-connected points. **/
    public double startContourMaxDistance = 0.5;

    public int minPointsPerContour = 0;

    public double maxDistanceRatio = 2.2; // just big enough to allow for a missed return.

    public double alwaysAcceptDistance = 0.15;

    /** We'll join two points when:
        1. The two points are adjacent and their distance is less than adjacentAcceptDistance

        OR

        2. The following criteria are ALL met:
	    a) The points are within maxSkipPoints apart (measured in indices)
	    b) The distance between the points is less than maxDistance
	    c) The distance ratio (n-1, n)/(n-2, n-1) is less than maxDistanceRatio OR the distance is less than alwaysAcceptDistance
    **/
    public ContourExtractor(Config config)
    {
        if (config == null)
            return;

        maxSkipPoints = config.getInt("max_skip_points", maxSkipPoints);
        adjacentAcceptDistance = config.getDouble("adjacent_accept_distance", adjacentAcceptDistance);
        maxDistance = config.getDouble("max_distance", maxDistance);
        startContourMaxDistance = config.getDouble("start_contour_max_distance", startContourMaxDistance);
        minPointsPerContour = config.getInt("min_points_per_contour", minPointsPerContour);
        maxDistanceRatio = config.getDouble("max_distance_ratio", maxDistanceRatio);
        alwaysAcceptDistance = config.getDouble("always_accept_distance", alwaysAcceptDistance);
    }

    public ContourExtractor()
    {
    }

    static class Join implements Comparable<Join>
    {
        int    a, b; // which points might we join?
        double distance;

        public int compareTo(Join j)
        {
            return Double.compare(distance, j.distance);
        }
    }

    /** We assume points come from a planar laser scanner and that the
        points correspond to some monotonically changing theta. We
        also assume that the points are in the coordinate frame of the
        sensor, i.e., the sensor is at (0,0). Points can belong to
        more than one contour.
    **/
    public ArrayList<ArrayList<double[]>> getContours(ArrayList<double[]> points)
    {
        /////////////////////////////////////////////////////////////////////////
        // Compute all candidate joins.
        ArrayList<Join> joins = new ArrayList<Join>();

        for (int i = 0; i < points.size(); i++) {
            for (int j = i + 1; j < Math.min(points.size(), i + maxSkipPoints + 2); j++) {

                double distance = LinAlg.distance(points.get(i), points.get(j));

                // If the points are adjacent and their distance is
                // less than a threshold, always accept them. This is
                // to help us keep together points that might
                // otherwise be separated due to the noise of the
                // sensor.
                if (i+1 == j && distance < adjacentAcceptDistance)
                    distance = -1;

                if (distance < maxDistance) {
                    Join join = new Join();
                    join.a = i;
                    join.b = j;
                    join.distance = distance;
                    joins.add(join);
                }
            }
        }

        /////////////////////////////////////////////////////////////////////////
        // Sort joins in order of least cost to maximum cost.
        Collections.sort(joins);

        /////////////////////////////////////////////////////////////////////////
        // Perform joins

        // Who is the left/right neighbor of each point?  If no neighbor, -1.
        int left[] = new int[points.size()];
        int right[] = new int[points.size()];

        for (int i = 0; i < points.size(); i++) {
            left[i] = -1;
            right[i] = -1;
        }

        for (int joinidx = 0; joinidx < joins.size(); joinidx++) {
            Join join = joins.get(joinidx);

            // Can't join two points that already have different neighbors.
            if (right[join.a] >=0 || left[join.b] >= 0)
                continue;

            // If the left or right point is already part of a
            // contour, what is the distance between the most recently
            // added point?  If the distance jumps suddenly, we may
            // not want to connect to this contour.
            double lastDistance = Double.MAX_VALUE;
            if (left[join.a] >= 0)
                lastDistance = Math.min(lastDistance, LinAlg.distance(points.get(join.a),
                                                                      points.get(left[join.a])));

            if (right[join.b] >= 0)
                lastDistance = Math.min(lastDistance, LinAlg.distance(points.get(join.b),
                                                                      points.get(right[join.b])));

            if (lastDistance == Double.MAX_VALUE)
                lastDistance = startContourMaxDistance;

            double distanceRatio = join.distance / lastDistance;
            if (distanceRatio > maxDistanceRatio && join.distance > alwaysAcceptDistance)
                continue;

            // join the points.
            right[join.a] = join.b;
            left[join.b] = join.a;
        }

        /////////////////////////////////////////////////////////////////////////
        // Construct the joins.

        ArrayList<ArrayList<double[]>> contours = new ArrayList<ArrayList<double[]>>();
        for (int root = 0; root < points.size(); root++) {
            // is this isn't left-most point in a chain, we've already extracted this contour.
            if (left[root] >=0)
                continue;

            ArrayList<double[]> contour = new ArrayList<double[]>();
            for (int child = root; child >= 0; child = right[child])
                contour.add(points.get(child));

            if (contour.size() >= minPointsPerContour)
                contours.add(contour);
        }

        return contours;
    }
}
