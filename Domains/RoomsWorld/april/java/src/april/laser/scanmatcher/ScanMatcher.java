package april.laser.scanmatcher;

import april.config.*;
import april.jmat.*;
import april.jmat.geom.*;
import april.graph.*;
import april.util.*;
import april.laser.*;

import javax.swing.*;
import java.awt.*;
import java.awt.image.*;
import java.util.*;

public class ScanMatcher
{
    Config config;
    Graph  g;
    GridMap gm;
    boolean gmDirty = false;

    ContourExtractor contourExtractor;
    MultiResolutionScanMatcher matcher;

    double metersPerPixel = 0.05;
    boolean useOdometry = true;
    double rangeCovariance = 0.1;
    int    maxScanHistory = 5;
    int    decimate = 1; // throw away all but every Nth scan. (1 = keep all).

    double search_x_m = 0.2;
    double search_y_m = 0.2;
    double search_theta_rad = Math.toRadians(15);
    double search_theta_res_rad = Math.toRadians(1);

    double pose_dist_thresh_m = 0.4;
    double pose_theta_thresh_rad = Math.toRadians(25);

    double gridmap_size = 50;

    int old_scan_decay = 5; // in units of gray-scale values per age of scan kept.

    public int decimateCounter;

    ArrayList<Scan> scans = new ArrayList<Scan>();

    // where do we think the robot is now (in global coordinates)?
    double xyt[] = new double[3];

    // Represents one of our historical scans which we use to build our local model.
    static class Scan
    {
        double xyt[];  // x, y, theta of robot

        // 2D points in global coordinate frame (i.e., projected by xyt)
        ArrayList<double[]> gpoints;

        // contours, in global coordinate frame.
        ArrayList<ArrayList<double[]>> gcontours;
    }

    public ScanMatcher(Config config)
    {
        this.config = config;

        this.g = new Graph();

        contourExtractor = new ContourExtractor(config.getChild("contours"));

        this.metersPerPixel = config.getDouble("meters_per_pixel", metersPerPixel);
        this.gridmap_size = config.getDouble("gridmap_size", gridmap_size);

        this.useOdometry = config.getBoolean("use_odometry", useOdometry);

        this.rangeCovariance = config.getDouble("range_covariance", rangeCovariance); // used for rendering
        this.maxScanHistory = config.getInt("max_scan_history", maxScanHistory);
        this.decimate = config.getInt("decimate", decimate);
        this.search_x_m = config.getDouble("search_x_m", search_x_m);
        this.search_y_m = config.getDouble("search_y_m", search_y_m);
        this.search_theta_rad = Math.toRadians(config.getDouble("search_theta_deg", Math.toDegrees(search_theta_rad)));
        this.search_theta_res_rad = Math.toRadians(config.getDouble("search_theta_res_deg", Math.toDegrees(search_theta_res_rad)));
        this.old_scan_decay = config.getInt("old_scan_decay", old_scan_decay);

        this.pose_dist_thresh_m = config.getDouble("pose_dist_thresh_m", pose_dist_thresh_m);
        this.pose_theta_thresh_rad = Math.toRadians(config.getDouble("pose_dist_theta_thresh_deg", Math.toDegrees(pose_theta_thresh_rad)));

        matcher = new MultiResolutionScanMatcher(config);

        gm = GridMap.makeMeters(-25, -25, gridmap_size, gridmap_size, metersPerPixel, 0);
    }

    public double[] getPosition()
    {
        return xyt;
    }

    public Graph getGraph()
    {
        return g;
    }

    public GridMap getGridMap()
    {
        return gm;
    }

    public void processOdometry(double odomxyt[], double P[][])
    {
        if (!useOdometry)
            return;

        xyt = LinAlg.xytMultiply(xyt, odomxyt);

        // XXX ignore uncertainty for now.
    }

    /** Points should be projected into robot's coordinate frame. **/
    public void processScan(ArrayList<double[]> rpoints)
    {
        if (scans.size() == 0) {
            // our first-ever scan.
            GXYTNode gn = new GXYTNode();
            gn.state = xyt;
            gn.init = xyt;
            gn.truth = xyt;

            gn.setAttribute("points", new ArrayList<double[]>(rpoints), new april.util.PointArrayCoder());
            g.nodes.add(gn);

            Scan scan = new Scan();
            scan.xyt = xyt;
            scan.gpoints = LinAlg.transform(xyt, rpoints);
            scan.gcontours = contourExtractor.getContours(scan.gpoints);
            scans.add(scan);

            drawScan(scan);
            return;
        }

        decimateCounter++;
        if (decimateCounter != decimate)
            return;
        decimateCounter = 0;

        MultiGaussian posterior = matcher.match(rpoints, xyt, null, search_x_m, search_y_m, search_theta_rad, search_theta_res_rad);
        xyt = posterior.getMean();

        // where was our last scan?
        double lastxyt[] = scans.get(scans.size()-1).xyt;
        double ddist = LinAlg.distance(xyt, lastxyt, 2);
        double dtheta = Math.abs(MathUtil.mod2pi(xyt[2] - lastxyt[2]));

        if (ddist > pose_dist_thresh_m || dtheta > pose_theta_thresh_rad) {

            GXYTNode gn = new GXYTNode();
            gn.state = xyt;
            gn.init = xyt;
            gn.truth = xyt;

            gn.setAttribute("points", new ArrayList<double[]>(rpoints), new april.util.PointArrayCoder());
            g.nodes.add(gn);

            Scan scan = new Scan();
            scan.xyt = xyt;
            scan.gpoints = LinAlg.transform(xyt, rpoints);
            scan.gcontours = contourExtractor.getContours(scan.gpoints);
            scans.add(scan);

            if (scans.size() > maxScanHistory)
                scans.remove(0);

            GXYTEdge ge = new GXYTEdge();
            ge.z = LinAlg.xytInvMul31(lastxyt, xyt);
            ge.P = LinAlg.diag(new double[] { 0.1, 0.1, 0.01});
            ge.nodes = new int[2];
            ge.nodes[0] = g.nodes.size()-2;
            ge.nodes[1] = g.nodes.size()-1;

            g.edges.add(ge);

            // trigger us to update the raster.
            drawScan(scan);
//            gmDirty = true;
        }
    }

    void drawScan(Scan s)
    {
        double minx = Double.MAX_VALUE, maxx = -Double.MAX_VALUE;
        double miny = Double.MAX_VALUE, maxy = -Double.MAX_VALUE;

        // Compute bounds of the scans.
        for (double p[] : s.gpoints) {
            minx = Math.min(minx, p[0]);
            maxx = Math.max(maxx, p[0]);
            miny = Math.min(miny, p[1]);
            maxy = Math.max(maxy, p[1]);
        }

        gm.recenter((minx+maxx)/2, (miny+maxy)/2, 5);

        gm.subtract(old_scan_decay);

        GridMap.LUT lut = gm.makeGaussianLUT(1.0, 0, 1.0 / rangeCovariance);

        for (ArrayList<double[]> c : s.gcontours) {
            for (int i = 0; i+1 < c.size(); i++) {
                double p0[] = c.get(i);
                double p1[] = c.get(i+1);

                double length = LinAlg.distance(p0, p1);

                gm.drawRectangle((p0[0]+p1[0])/2, (p0[1]+p1[1])/2, length, 0,
                                 Math.atan2(p1[1]-p0[1], p1[0]-p0[0]),
                                 lut);
            }
        }

        matcher.setModel(gm);
    }

    void updateRaster()
    {
        double minx = Double.MAX_VALUE, maxx = -Double.MAX_VALUE;
        double miny = Double.MAX_VALUE, maxy = -Double.MAX_VALUE;

        // Compute bounds of the scans.
        for (Scan s : scans) {
            for (double p[] : s.gpoints) {
                minx = Math.min(minx, p[0]);
                maxx = Math.max(maxx, p[0]);
                miny = Math.min(miny, p[1]);
                maxy = Math.max(maxy, p[1]);
            }
        }

        // create the LUT, with a bit more space than we currently
        // think we'll need (the thought being that we could add new
        // scans without having to rebuild the whole damn thing
        double margin = Math.max(1, 0.1*Math.max(maxx-minx, maxy-miny));

        gm = GridMap.makeMeters(minx, miny, (maxx-minx)+2*margin, (maxy-miny)+2*margin, metersPerPixel, 0);
        if (Math.sqrt(rangeCovariance) < metersPerPixel)
            System.out.println("WRN: ScanMatcher range covariance is small in comparison to raster resolution. Increase resolution.");

        GridMap.LUT lut = gm.makeGaussianLUT(1.0, 0, 1.0 / rangeCovariance);

        for (int sidx = 0; sidx < scans.size(); sidx++) {
            Scan s = scans.get(sidx);

            for (ArrayList<double[]> c : s.gcontours) {
                for (int i = 0; i+1 < c.size(); i++) {
                    double p0[] = c.get(i);
                    double p1[] = c.get(i+1);

                    double length = LinAlg.distance(p0, p1);

                    gm.drawRectangle((p0[0]+p1[0])/2, (p0[1]+p1[1])/2, length, 0,
                                     Math.atan2(p1[1]-p0[1], p1[0]-p0[0]),
                                     lut);
                }
            }
        }

        matcher.setModel(gm);
    }
}
