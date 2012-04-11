package edu.umich.robot.slam;

import java.util.ArrayList;

import april.config.Config;
import april.graph.GXYTEdge;
import april.graph.GXYTNode;
import april.image.SigProc;
import april.jmat.LinAlg;
import april.jmat.MathUtil;
import april.laser.scanmatcher.HillClimbing;
import april.laser.scanmatcher.MultiResolutionMatcher;
import april.util.GridMap;

public class LoopClosureMatcher {

	/**
	 * The following parameters may adjusted based on the environment.
	 * 
	 * All parameters are initialized to default values below. Pass in true
	 * values via the configuration file.
	 * 
	 * @param meteresPerPixel
	 *            Sets the resolution on the grid map used in scan matching.
	 *            Increasing resolution will slow down matching process
	 *            (specifically the initialization process).
	 * @param gridMapSize
	 *            Size of the desired grid map. This depends on the LIDAR range
	 *            capabilities (approximately twice the maximum range of the
	 *            LIDAR, although this can be altered below for efficiency based
	 *            on the true field of view of the LIDAR, see initialization
	 *            method below).
	 * @param distErr
	 *            Approximation on the movement covariance (error per meter of
	 *            movement).
	 * @param rotErr
	 *            Approximation on the rotation covariance (error per degree of
	 *            rotation).
	 * @param minMatchScore
	 *            Minimum score that must be achieved by the multi-resolution
	 *            scan matcher before an matching edge is returned.
	 * @param thetaRange
	 *            Rotation search space (in radians).
	 * @param thetaRangeRes
	 *            Rotation search space resolution (in radians).
	 * @param xyRange
	 *            Search window size (in meters).
	 * @param xCov
	 *            Hacked x covariance for a returned edge.
	 * @param yCov
	 *            Hacked y covariance for a returned edge.
	 * @param tCov
	 *            Hacked theta covariance for a returned edge.
	 * @param openLoop
	 *            Boolean flag which alters the performance of the matcher based
	 *            upon whether the matching is occurring as an open loop process
	 *            or a close loop process.
	 */
	double metersPerPixel = 0.05;
	double gridMapSize = 20;
	double distErr = 0.15;
	double rotErr = 0.1;
	double minMatchScore = 5100;
	double thetaRange = Math.toRadians(90);
	double thetaRangeRes = Math.toRadians(1);
	double xyRange = 0.15;
	double xCov = 1.0;
	double yCov = 1.0;
	double tCov = Math.toRadians(5.0);
	boolean openLoop = false;

	// loop closure scan matcher
	MultiResolutionMatcher clMatcher;

	// configuration file / grip map for the loop closure matcher
	Config config;
	GridMap gmLC;

	/**
	 * Constructor method (can be called with null as input).
	 * 
	 * @param config
	 *            Configuration file (see april.config for more information).
	 */
	public LoopClosureMatcher(Config config) {
		if (config != null) {
			this.metersPerPixel = config.getDouble("metersPerPixel",
					metersPerPixel);
			this.gridMapSize = config.getDouble("gridMapSize", gridMapSize);
			this.distErr = config.getDouble("distErr", distErr);
			this.rotErr = config.getDouble("rotErr", rotErr);
			this.minMatchScore = config.getDouble("minMatchScore",
					minMatchScore);
			this.thetaRange = Math.toRadians(config.getDouble("thetaRange",
					Math.toDegrees(thetaRange)));
			this.thetaRangeRes = Math.toRadians(config.getDouble(
					"thetaRangeRes", Math.toDegrees(thetaRangeRes)));
			this.xyRange = config.getDouble("xyRange", xyRange);
			this.xCov = config.getDouble("xCov", xCov);
			this.yCov = config.getDouble("yCov", yCov);
			this.tCov = config.getDouble("tCov", tCov);
		}

		// loop closure scan matcher
		clMatcher = new MultiResolutionMatcher();
	}

	/**
	 * Initializes the grip map for the loop closure scan matcher.
	 * 
	 * This method must be called prior to attempting to match two scans.
	 * 
	 * @param lidarPts
	 *            Set of lidar points in robot coordinate frame. Each entry
	 *            within the array list is a two element array of type double
	 *            representing a single lidar point. The first element is the
	 *            x-coordinate and the second element is the y-coordinate of the
	 *            lidar point.
	 */
	public void init(ArrayList<double[]> lidarPts) {

		// creating the grid map
		gmLC = GridMap.makeMeters(-(gridMapSize) / 2, -(gridMapSize) / 2,
				gridMapSize, gridMapSize, metersPerPixel, 0);

		double minx = Double.MAX_VALUE, maxx = -Double.MAX_VALUE;
		double miny = Double.MAX_VALUE, maxy = -Double.MAX_VALUE;

		// computing the bounds of the scan
		for (double p[] : lidarPts) {
			minx = Math.min(minx, p[0]);
			maxx = Math.max(maxx, p[0]);
			miny = Math.min(miny, p[1]);
			maxy = Math.max(maxy, p[1]);
		}

		// re-center grid map according to the bounds of the scan
		gmLC.recenter((minx + maxx) / 2, (miny + maxy) / 2, 5);

		// add each LIDAR point into the grid map
		double[] last_lp = new double[1];
		double lastpidx = 0;

		for (int pidx = 0; pidx < lidarPts.size(); pidx++) {
			double lp[] = lidarPts.get(pidx);
			gmLC.setValue(lp[0], lp[1], (byte) 255);

			if (last_lp != null && lastpidx == pidx - 1) {
				double dist = LinAlg.distance(lp, last_lp);
				if (dist < 0.25) {
					gmLC.drawLine(last_lp[0], last_lp[1], lp[0], lp[1],
							(byte) 255);
				}
			}

			last_lp = lp;
			lastpidx = pidx;
		}

		// run a gaussian filter of the grid map to blur the LIDAR scan
		float f[] = SigProc.makeGaussianFilter(1.5, 5);
		f = LinAlg.scale(f, 1.0 / LinAlg.max(f));
		gmLC.filterFactoredCenteredMax(f, f);

		// set our matcher's grid model to the blurred grid map
		clMatcher.setModel(gmLC);
	}

	/**
	 * @return Returns the size of the search window for scan matching.
	 */
	public double getSearchDist() {
		return this.xyRange;
	}

	/**
	 * Allows for the below class parameters to be changed.
	 * 
	 * Used by the Slam class to switch parameters based on whether or not the
	 * matching is being used to close the loop.
	 * 
	 * @param matchScore
	 *            Minimum score that must be achieved by the multi-resolution
	 *            scan matcher before an matching edge is returned.
	 * @param thetarange
	 *            Rotation search space (in radians).
	 * @param thetares
	 *            Rotation search space resolution (in radians).
	 * @param searchrange
	 *            Search window size (in meters).
	 */
	public void setMatchParameters(double matchScore, double thetarange,
			double thetares, double searchrange) {
		this.minMatchScore = matchScore;
		this.thetaRange = thetarange;
		this.thetaRangeRes = thetares;
		this.xyRange = searchrange;
	}

	/**
	 * Allows for the matching scheme to be changed between open loop and close
	 * loop matching.
	 * 
	 * @param flag
	 *            Boolean flag to set the matching scheme. See main matching
	 *            method below for more information.
	 */
	public void setOpenLoopMatch(boolean flag) {
		this.openLoop = flag;
	}

	/**
	 * Allows for the grid map to be changed.
	 * 
	 * @param gmap
	 *            New grid map for the multi-resolution scan matcher.
	 */
	public void setModel(GridMap gmap) {
		this.gmLC = gmap;
		clMatcher.setModel(this.gmLC);
	}

	/**
	 * Main matching method between two nodes in the SLAM graph.
	 * 
	 * Matching Node A to prior Node B. Must call initialization method using
	 * Node A's LIDAR scan prior to calling for a match.
	 * 
	 * @param a
	 *            Reference node whose LIDAR scan was used in calling the
	 *            initialization.
	 * @param b
	 *            Node whose LIDAR scan is being matched to.
	 * @param priorXYT
	 *            Prior x, y, and theta edge between the two nodes.
	 * @return If a match meets all the specified parameters above, a GXYTEdge
	 *         will be returned by this method with the appropriate variables
	 *         assigned according to the scan matcher. Otherwise, this method
	 *         will return an empty (null) GXYTEdge. Note that the edge will
	 *         point from Node A to Node B as Node A is the reference node.
	 */
	public GXYTEdge match(GXYTNode a, GXYTNode b, double[] priorXYT) {
		// new edge to return if successful match found
		GXYTEdge newEdge = new GXYTEdge();

		// nodeA is the reference scan and nodeB is the scan being matched to
		double[] poseA = a.state;
		double[] poseB = b.state;
		ArrayList<double[]> ptsB = (ArrayList<double[]>) b
				.getAttribute("points");

		// distance and change in angle between the two nodes
		double ddist = LinAlg.distance(poseA, poseB, 2);
		double dtheta = Math.abs(MathUtil.mod2pi(poseA[2] - poseB[2]));

		// estimated covariance
		double P[][] = LinAlg.diag(new double[] {
				LinAlg.sq(ddist * distErr) + 0.01,
				LinAlg.sq(ddist * distErr) + 0.01,
				LinAlg.sq(dtheta * rotErr) + Math.toRadians(1) });

		// scaling the covariance
		double priorScale = 1.0 / (ptsB.size() * 30.0);
		double priorScaled[][] = LinAlg.scale(P, priorScale);

		// matching the nodes
		double r[] = clMatcher.match(ptsB, priorXYT,
				LinAlg.inverse(priorScaled), priorXYT, xyRange, xyRange,
				thetaRange, thetaRangeRes);

		// hill climbing to improve upon the match
		HillClimbing hc = new HillClimbing(new Config());
		hc.setModel(gmLC);
		r = hc.match(ptsB, priorXYT, priorScaled, LinAlg.copy(r, 3));

		// the following places additional constraints on the matching scheme
		// and is used for open loop processes to avoid hallway issues
		if (openLoop) {
			double[] hill = LinAlg.copy(r, 3);
			double[] err = LinAlg.subtract(hill, priorXYT);
			err[2] = MathUtil.mod2pi(err[2]);
			// if the change from the new edge is larger than the search space,
			// reject the match and return a null edge
			if (Math.abs(err[0]) > xyRange || Math.abs(err[1]) > xyRange
					|| Math.abs(err[2]) > thetaRange) {
				return null;
			}
		}

		// score too low to add an edge
		if (r[3] < minMatchScore) {
			return null;
		}

		// good match, return edge with corresponding match information
		newEdge.z = new double[] { r[0], r[1], r[2] };
		newEdge.P = LinAlg.diag(new double[] { xCov, yCov, tCov });
		newEdge.setAttribute("Score", r[3]);
		return newEdge;
	}
}