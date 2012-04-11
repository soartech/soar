package edu.umich.robot.slam;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Random;

import april.config.Config;
import april.graph.CholeskySolver;
import april.graph.DijkstraProjection;
import april.graph.GEdge;
import april.graph.GXYTEdge;
import april.graph.GXYTNode;
import april.graph.GXYTPosEdge;
import april.graph.Graph;
import april.image.SigProc;
import april.jmat.LinAlg;
import april.jmat.MathUtil;
import april.jmat.MultiGaussian;
import april.jmat.ordering.MinimumDegreeOrdering;
import april.laser.scanmatcher.MultiResolutionScanMatcher;
import april.util.GridMap;

public class Slam {

	/**
	 * The following booleans control the overall SLAM setup.The configuration
	 * files hold the variables for each process. The various objects
	 * instantiate other classes used by the SLAM process.
	 * 
	 * @param useOdom
	 *            Set to true if using odometry sensors.
	 * @param closeLoop
	 *            Set to true to attempt to close loops.
	 * @param noisyOdom
	 *            Set to true to artificially inflate the odometry information
	 *            with error (used with simulation environment).
	 * @param includeDoors
	 *            Set to true to attempt to locate doors and add them to the
	 *            pose graph.
	 */
	boolean useOdom = true;
	boolean closeLoop = true;
	boolean noisyOdom = true;
	boolean includeDoors = true;
	Config slamConfig;
	Config loopMatcherConfig;
	Config loopCloserConfig;
	Config doorFinderConfig;
	DoorFinder doorFinder;
	LoopClosureMatcher loopmatcher;
	MultiResolutionScanMatcher openMatch;
	Graph g = new Graph();

	/**
	 * Open loop scan matcher parameters / variables used only if the SLAM
	 * routine does not use odometry sensors. Do not confuse these with the open
	 * loop matching routine within the loop closure matcher. Those parameters
	 * are to be set within the configuration file passed to the loop closure
	 * matcher via constructor call.
	 * 
	 * @param openMetersPerPixel
	 *            Resolution on the grid map below (meters per pixel).
	 * @param openSearchRange
	 *            Search window size (in meters).
	 * @param openSearchTheta
	 *            Rotation search space (in radians).
	 * @param openSearchThetaRes
	 *            Rotation search space resolution (in radians).
	 * @param openGridMapSize
	 *            Grid map size (in meters).
	 * @param scanDecay
	 *            Amount of decay in the open loop grid map per pose added to
	 *            the graph.
	 */
	double openMetersPerPixel = 0.05;
	double openSearchRange = 0.2;
	double openSearchTheta = Math.toRadians(15);
	double openSearchThetaRes = Math.toRadians(1);
	double openGridMapSize = 20;
	int scanDecay = 5;

	/**
	 * Loop closure SLAM process parameters. Adjust based on desired
	 * performance, environment, and robot setup. These parameters are passed
	 * into the loop closure matcher whenever a close loop match is being
	 * processed. All parameters within the configuration file for the loop
	 * closure matcher should be set for the open loop matching process which
	 * attempts to gain a better estimate on an odometry edge (see slam routine
	 * below for further understanding).
	 * 
	 * @param closeMatchScore
	 *            Minimum score that must be achieved by the multi-resolution
	 *            scan matcher before an matching edge is returned.
	 * @param closeThetaRange
	 *            Rotation search space (in radians).
	 * @param closeThetaRangeRes
	 *            Rotation search space resolution (in radians).
	 * @param closeSearchRange
	 *            Search window size (in meters).
	 */
	double closeMatchScore = 8000;
	double closeThetaRange = Math.toRadians(10);
	double closeThetaRangeRes = Math.toRadians(1);
	double closeSearchRange = 10;

	/**
	 * The following parameters / variables are for the loop closing method
	 * 'closer' below.
	 * 
	 * @variable lastEdge Index of the last edge that was created from open loop
	 *           techniques.
	 * @param maxLoopLength
	 *            Maximum loop length of the depth first search.
	 * @param maxMatchAttempts
	 *            Maximum scan match attempts per loop closing.
	 * @param maxAddedEdges
	 *            Maximum number of edges to add per node in the graph.
	 * @param maxHypothesisAge
	 *            Maximum age of a hypothesis edge before it is removed (based
	 *            on the below boolean on removing old hypothesis edges)
	 * @param minTrace
	 *            Once a scan matching edge has been validated during the loop
	 *            closing process, the trace of the covariance on the Dijkstra
	 *            projection from the current node to the matched node must be
	 *            above this value before the validated edge will be added to
	 *            the graph.
	 * @param maxMahal
	 *            Once a scan matching edge has been validated during the loop
	 *            closing process, the Mahalanobis distance between the Dijkstra
	 *            projection from the current node to the matched node must be
	 *            below this value before the validated edge will be added to
	 *            the graph.
	 * @param hypoThresh
	 *            Mahalanobis distance threshold between nodes being considered
	 *            for scan matching and the current node.
	 * @param removeOldHypoEdges
	 *            Boolean which controls whether or not the loop closing method
	 *            below removes old edge hypothesis.
	 * @variable hypoNodes Hypotheses grouped according to the nodes they
	 *           connect. Always includes odometry edges, regardless of the
	 *           routine to remove ages hypothesis edges.
	 */
	int lastEdge = 0;
	int maxLoopLength = 4;
	int maxMatchAttempts = 40;
	int maxAddedEdges = 10;
	int maxHypothesisAge = 100;
	double minTrace = 0.2;
	double maxMahal = 1.0;
	double hypoThresh = 5.0;
	boolean removeOldHypoEdges = false;
	HashMap<Integer, ArrayList<GXYTEdge>> hypoNodes = new HashMap<Integer, ArrayList<GXYTEdge>>();

	/**
	 * Laser scan class used by open loop scan matcher when no odometry
	 * information is available.
	 */
	public static class Scan {
		// pose of the robot at the time of scan
		double xyt[];

		// LIDAR scan points in global frame
		ArrayList<double[]> gpoints;
	}

	/**
	 * The following variables / parameters control various aspects of the SLAM
	 * process. See each description for further information.
	 * 
	 */
	// Index of the last pose node within graph.nodes.
	int lastPoseIndex = 0;

	// Grid map used in the open loop process if no odometry data is being
	// supplied.
	GridMap openGridMap;

	// Error estimate on odometry movement.
	double distErr = 0.15;

	// Error estimate on odometry rotation.
	double rotErr = 0.1;

	// Minimum distance that must be traveled from the last pose before a new
	// pose will be added to the graph.
	double poseDistThresh = 1.0;

	// Minimum rotation that must be traveled from the last pose before a new
	// pose will be added to the graph.
	double poseRotThresh = Math.toRadians(25);

	// Random number generator used to artificially inflate the odometry error
	// based upon SLAM setup.
	Random randomGen = new Random();

	// Sampling errors (x, y, and theta) used to calculate standard deviation
	// when artificially inflating the odometry error based upon SLAM setup.
	double[] samplingError = new double[] { 0.005, 0.005, Math.toRadians(0.005) };

	// Array list capturing poses of the true odometry movement (only available
	// in the simulation environment).
	ArrayList<double[]> trueOdom = new ArrayList<double[]>();

	// Array list capturing poses from the odometry sensors (this information
	// includes the error in the sensors).
	ArrayList<double[]> pureOdom = new ArrayList<double[]>();

	// Array list capturing poses of the SLAM corrected odometry movement.
	ArrayList<double[]> slamOdom = new ArrayList<double[]>();

	// Variable used in order to ensure that a odometry measurement (if using
	// odometry sensors) is received prior to adding a pose to the graph.
	int starter = 0;

	// Decimate the number of LIDAR scans received that are processed.
	int decimate = 1;

	// Counter for the above decimation.
	public int decimateCounter;

	// Holds the last scan received (used by the GUI for visualizing LIDAR on
	// its own).
	ArrayList<double[]> lastScan;

	// Constantly holds the best estimate of the current location (x, y, theta
	// in meters).
	double xyt[] = new double[3];

	// Holds the last true location received by the odometry sensors (only used
	// when inflating the odometry with error in the simulation environment).
	double lastTruePos[] = new double[3];

	// Assists in allowing for odometry data to be passed in as global
	// coordinates. Holds the last received odometry message at any given
	// moment.
	double XYT[] = new double[3];

	// Holds all non odometry edges added to the graph.
	ArrayList<GEdge> addedEdges = new ArrayList<GEdge>();

	/**
	 * Constructor method (can be called with null as input).
	 * 
	 * @param config
	 *            Configuration file (see april.config for more information).
	 */
	public Slam(Config config) {

		// configuration file available (if not, all parameters are set to their
		// default values currently assigned)
		if (config != null) {

			// parsing the configuration file
			slamConfig = config.getChild("Slam");
			loopMatcherConfig = config.getChild("LoopMatcher");
			loopCloserConfig = config.getChild("LoopCloser");
			doorFinderConfig = config.getChild("DoorFinder");

			// setting SLAM configuration parameters
			useOdom = slamConfig.getBoolean("useOdom", useOdom);
			closeLoop = slamConfig.getBoolean("closeLoop", closeLoop);
			noisyOdom = slamConfig.getBoolean("noisyOdom", noisyOdom);
			includeDoors = slamConfig.getBoolean("includeDoors", includeDoors);

			// setting close loop matching parameters
			closeMatchScore = slamConfig.getDouble("closeMatchScore",
					closeMatchScore);
			closeThetaRange = Math.toRadians(slamConfig.getDouble(
					"closeThetaRange", closeThetaRange));
			closeThetaRangeRes = Math.toRadians(slamConfig.getDouble(
					"closeThetaRangeRes", closeThetaRangeRes));
			closeSearchRange = slamConfig.getDouble("closeSearchRange",
					closeSearchRange);

			// setting remaining SLAM parameters
			distErr = slamConfig.getDouble("distErr", distErr);
			rotErr = slamConfig.getDouble("rotErr", rotErr);
			poseDistThresh = slamConfig.getDouble("poseDistThresh",
					poseDistThresh);
			poseRotThresh = Math.toRadians(slamConfig.getDouble(
					"poseRotThresh", Math.toDegrees(poseRotThresh)));
			decimate = slamConfig.getInt("decimate", decimate);

			// setting loop closing configuration parameters
			maxLoopLength = loopCloserConfig.getInt("maxLoopLength",
					maxLoopLength);
			maxMatchAttempts = loopCloserConfig.getInt("maxMatchAttempts",
					maxMatchAttempts);
			maxAddedEdges = loopCloserConfig.getInt("maxAddedEdges",
					maxAddedEdges);
			maxHypothesisAge = loopCloserConfig.getInt("maxHypothesisAge",
					maxHypothesisAge);
			minTrace = loopCloserConfig.getDouble("minTrace", minTrace);
			maxMahal = loopCloserConfig.getDouble("maxMahal", maxMahal);
			hypoThresh = loopCloserConfig.getDouble("hypoThresh", hypoThresh);
			removeOldHypoEdges = loopCloserConfig.getBoolean(
					"removeOldHypoEdges", removeOldHypoEdges);
		}

		// initializations if odometry information is not available
		if (!useOdom) {

			// setting open loop matching parameters
			openMetersPerPixel = slamConfig.getDouble("openMetersPerPixel",
					openMetersPerPixel);
			openSearchRange = slamConfig.getDouble("openSearchRange",
					openSearchRange);
			openSearchTheta = Math.toRadians(slamConfig.getDouble(
					"openSearchTheta", openSearchTheta));
			openSearchThetaRes = Math.toRadians(slamConfig.getDouble(
					"openSearchThetaRes", openSearchThetaRes));
			openGridMapSize = slamConfig.getDouble("openGridMapSize",
					openGridMapSize);
			scanDecay = slamConfig.getInt("scanDecay", scanDecay);

			// instantiating the open loop matcher and grid map
			openMatch = new MultiResolutionScanMatcher(new Config());
			openGridMap = GridMap.makeMeters(-(openGridMapSize / 2),
					-(openGridMapSize / 2), openGridMapSize, openGridMapSize,
					openMetersPerPixel, 0);

			// no odometry information available, thus ignore waiting for the
			// first odometry measurement
			starter = 1;
		}

		// initialization if attempting to find doors
		if (includeDoors) {

			// instantiating the door finder by passing in appropriate
			// configuration file
			doorFinder = new DoorFinder(doorFinderConfig);
		}

		// pose graph instantiation
		g = new Graph();
	}

	/**
	 * Simple updating of the current pose estimate using odometry sensor
	 * information.
	 * 
	 * @param odomxyt
	 *            Pose estimate from odometry sensors, pass in as [x, y, theta]
	 *            in global coordinate frame (units are meters | radians).
	 */
	public void processOdometry(double odomxyt[]) {

		// not using odometry according to SLAM setup, return (see above)
		if (!useOdom)
			return;

		// map the current angle between -pi to pi
		odomxyt[2] = MathUtil.mod2pi(odomxyt[2]);

		// allow for an initial offset position
		if (starter == 0) {
			xyt = LinAlg.copy(odomxyt);
			XYT = LinAlg.copy(odomxyt);
			lastTruePos = LinAlg.copy(odomxyt);
			synchronized (trueOdom) {
				trueOdom.add(LinAlg.copy(odomxyt));
			}
			synchronized (pureOdom) {
				pureOdom.add(LinAlg.copy(odomxyt));
			}
			synchronized (slamOdom) {
				slamOdom.add(LinAlg.copy(odomxyt));
			}
			starter++;
			return;
		}

		// sampling from noisy odometry according to SLAM setup (see above)
		if (noisyOdom) {
			// noisy sampling from odometry using rigid body transformation
			// process model
			double dist = LinAlg.distance(lastTruePos, odomxyt);
			double dang = Math
					.abs(MathUtil.mod2pi(lastTruePos[2] - odomxyt[2]));

			// only adding error in odometry measurement if robot has moved
			// since
			// the last time an odometry message was received
			if (dist > 0.00001 || dang > 0.00001) {
				// synchronizing for GUI threading
				synchronized (trueOdom) {
					trueOdom.add(LinAlg.copy(odomxyt));
				}

				// true movement RBT parameters
				double[] priorU = LinAlg.xytInvMul31(lastTruePos, odomxyt);
				priorU[2] = MathUtil.mod2pi(priorU[2]);
				lastTruePos = LinAlg.copy(odomxyt);

				// sampling from noise parameters, assuming error of al[num] per
				// each full unit of true movement
				double s1 = Math.abs(samplingError[0] * priorU[0]);
				double s2 = Math.abs(samplingError[1] * priorU[1]);
				double s3 = Math.abs(samplingError[2] * priorU[2]);

				double Dx = priorU[0] + Math.sqrt(s1)
						* randomGen.nextGaussian();
				double Dy = priorU[1] + Math.sqrt(s2)
						* randomGen.nextGaussian();
				double Dt = priorU[2] + Math.sqrt(s3)
						* randomGen.nextGaussian();

				// new odomxyt after sampling from noisy motion model
				double[] newU = new double[] { Dx, Dy, Dt };
				odomxyt = LinAlg.xytMultiply(XYT, newU);
				odomxyt[2] = MathUtil.mod2pi(odomxyt[2]);

				synchronized (pureOdom) {
					pureOdom.add(LinAlg.copy(odomxyt));
				}
			} else {
				return;
			}
		}

		// rigid body transformation update (XYT stores the previous odometry
		// sensor measurement)
		double[] u = LinAlg.xytInvMul31(XYT, odomxyt);

		// Reassign XYT values
		XYT = LinAlg.copy(odomxyt);

		// simple updating of the predicted pose
		xyt = LinAlg.xytMultiply(xyt, u);
		xyt[2] = MathUtil.mod2pi(xyt[2]);
	}

	/**
	 * Updating SLAM pose graph based on LIDAR scan.
	 * 
	 * Majority of the SLAM magic happens here. There are two open loop routines
	 * based on whether or not odometry sensor information is available. If it
	 * is not available, an open loop matcher estimates the movement of the
	 * robot using scan matching techniques. Once the minimum movement distance
	 * or rotation has been achieved according to scan matching, a new pose is
	 * added to the graph. This pose is then matched with the last pose added to
	 * the graph to get a better estimate of the movement. Otherwise, odometry
	 * sensor measurements are used to determine if the robot has achieved the
	 * minimum movement or rotation distance. If so, a pose is added and matched
	 * to the prior node in the graph. Depending on the SLAM setup, the
	 * algorithm then looks for doors (including a data association within the
	 * door finder class) and attempts to close the loop (method below).
	 * 
	 * @param rpoints
	 *            Set of lidar points in robot coordinate frame. Each entry
	 *            within the array list is a two element array of type double
	 *            representing a single lidar point. The first element is the
	 *            x-coordinate and the second element is the y-coordinate of the
	 *            lidar point.
	 */
	public void processScan(ArrayList<double[]> rpoints) {
		// used by GUI for displaying scans
		lastScan = rpoints;

		// wait for first position message to arrive before beginning scan
		// matching
		if (starter == 0)
			return;

		// first scan received after the initial odometry message has been
		// received
		if (g.nodes.isEmpty()) {

			// first scan ever, add a node to the graph
			GXYTNode gn = new GXYTNode();
			gn.state = xyt;
			gn.init = xyt;

			// anchor this node in the graph using a GXYTPosEdge (see
			// GXYTPosEdge for more information)
			GXYTPosEdge edge = new GXYTPosEdge();
			edge.nodes = new int[] { 0 };
			edge.z = LinAlg.copy(gn.state);
			edge.P = LinAlg.diag(new double[] { 0.001, 0.001, 0.0001 });
			g.edges.add(edge);

			// add attributes (LIDAR scan and node index within graph.nodes)
			gn.setAttribute("points", rpoints);
			gn.setAttribute("node_index", g.nodes.size());

			// add node to graph
			g.nodes.add(gn);

			// update the open loop grid map if odometry information is not
			// available
			if (!useOdom) {
				Scan scan = new Scan();
				scan.xyt = xyt;
				scan.gpoints = LinAlg.transform(xyt, rpoints);
				drawScan(scan);
			}

			// call main door finder method to find doors, associate the data,
			// and add them to the graph
			if (includeDoors) {
				doorFinder.findDoors(rpoints, g, lastPoseIndex, xyt,
						loopmatcher, poseDistThresh);

				// add door edges to our addedEdges array list for visualization
				// (see SlamGui for more information)
				for (int d = 0; d < doorFinder.edgesAdded; d++)
					addedEdges.add(g.edges.get(g.edges.size() - 1 - d));
			}

			return;
		}

		// throwing away scan based upon parameters in declaration above
		decimateCounter++;
		if (decimateCounter != decimate)
			return;
		decimateCounter = 0;

		// scan matcher using prior scans if not using odometry
		if (!useOdom) {
			MultiGaussian posterior = openMatch.match(rpoints, xyt, null,
					openSearchRange, openSearchRange, openSearchTheta,
					openSearchThetaRes);
			double[] positionUpdate = posterior.getMean();
			xyt = LinAlg.copy(positionUpdate);
		}

		// where was our last pose
		GXYTNode NA = (GXYTNode) g.nodes.get(lastPoseIndex);
		double ddist = LinAlg.distance(xyt, NA.state, 2);
		double dtheta = Math.abs(MathUtil.mod2pi(xyt[2] - NA.state[2]));

		if (ddist > poseDistThresh || dtheta > poseRotThresh) {

			// create two edges, an odometry edge and scan match edge, use the
			// best of the two based on whether or not the match meets all scan
			// matching requirements
			GXYTNode gn = new GXYTNode();
			gn.state = xyt;
			gn.init = xyt;
			gn.setAttribute("points", rpoints);
			gn.setAttribute("node_index", g.nodes.size());
			g.nodes.add(gn);

			// new edge using just odometry information
			GXYTEdge ge = new GXYTEdge();
			ge.z = LinAlg.xytInvMul31(NA.state, xyt);
			ge.P = LinAlg.diag(new double[] {
					LinAlg.sq(ddist * distErr) + 0.01,
					LinAlg.sq(ddist * distErr) + 0.01,
					LinAlg.sq(dtheta * rotErr) + Math.toRadians(1) });
			ge.nodes = new int[2];
			ge.nodes[0] = lastPoseIndex;
			ge.nodes[1] = g.nodes.size() - 1;

			// new edge using scan matching information
			loopmatcher = new LoopClosureMatcher(loopMatcherConfig);

			// initialize the loop closure matcher with the current LIDAR scan
			loopmatcher.init(rpoints);

			// current pose
			GXYTNode NB = (GXYTNode) g.nodes.get(g.nodes.size() - 1);

			// creating a prior (just odometry edge)
			double[] prior = LinAlg.xytInvMul31(NB.state, NA.state);

			// setting the loop matcher to open loop to enforce a bound on the
			// matching (this avoids the hallway problem)
			loopmatcher.setOpenLoopMatch(true);

			// invoking the match (note that this is called in a reverse manner
			// in an effort to use the same instance of the loop closure matcher
			// during the loop closing process below)
			GXYTEdge newEdge = loopmatcher.match(NB, NA, prior);

			// setting the loop matcher back to close loop for loop closure
			// below
			loopmatcher.setOpenLoopMatch(false);

			// found a good match, fix current node and add matching edge to
			// graph.edges
			if (newEdge != null) {

				// invert this edge (see matching scheme above for info), update
				// all of the edge's information, add to graph.edges, update
				// current pose's node
				newEdge.z = LinAlg.xytInverse(newEdge.z);
				newEdge.nodes = new int[2];
				newEdge.nodes[0] = lastPoseIndex;
				newEdge.nodes[1] = g.nodes.size() - 1;
				double[] tempXYZ = LinAlg.xytMultiply(NA.state, newEdge.z);
				gn.init = tempXYZ;
				gn.state = tempXYZ;
				double newdist = LinAlg.distance(gn.state, NA.state, 2);
				double newtheta = Math.abs(MathUtil.mod2pi(gn.state[2]
						- NA.state[2]));
				newEdge.P = LinAlg.diag(new double[] {
						LinAlg.sq(newdist * distErr) + 0.01,
						LinAlg.sq(newdist * distErr) + 0.01,
						LinAlg.sq(newtheta * rotErr) + Math.toRadians(1) });
				g.edges.add(newEdge);
				xyt = LinAlg.copy(tempXYZ);
			} else {
				g.edges.add(ge);
			}

			// switch scan matcher search information for loop closing
			loopmatcher.setMatchParameters(closeMatchScore, closeThetaRange,
					closeThetaRangeRes, closeSearchRange);

			// update lastPoseIndex now that a new pose has been added
			lastPoseIndex = g.nodes.size() - 1;

			// attempt to close the loop based on SLAM setup
			if (closeLoop) {
				int curEdges = g.edges.size();
				close();
				if (curEdges < g.edges.size()) {
					optimizeFull(g);
					xyt = LinAlg.copy(g.nodes.get(lastPoseIndex).state);
				}
			}

			// attempt to find doors based on SLAM setup
			if (includeDoors) {
				doorFinder.findDoors(rpoints, g, lastPoseIndex, xyt,
						loopmatcher, poseDistThresh);
				for (int d = 0; d < doorFinder.edgesAdded; d++)
					addedEdges.add(g.edges.get(g.edges.size() - 1 - d));
			}

			// updating the open loop grid map if odometry information is not
			// available
			if (!useOdom) {
				Scan scan = new Scan();
				scan.xyt = xyt;
				scan.gpoints = LinAlg.transform(xyt, rpoints);
				drawScan(scan);
			}
		}

		// add updated pose to the list of SLAM poses
		synchronized (slamOdom) {
			slamOdom.add(LinAlg.copy(xyt));
		}
	}

	/**
	 * Updates the open loop scan matching grid map which is utilized only when
	 * odometry information is not available.
	 * 
	 * @param s
	 *            Current Scan object created from the most recent pose and
	 *            LIDAR scan (see above for the actual Scan class definition).
	 */
	void drawScan(Scan s) {
		double minx = Double.MAX_VALUE, maxx = -Double.MAX_VALUE;
		double miny = Double.MAX_VALUE, maxy = -Double.MAX_VALUE;

		for (double p[] : s.gpoints) {
			minx = Math.min(minx, p[0]);
			maxx = Math.max(maxx, p[0]);
			miny = Math.min(miny, p[1]);
			maxy = Math.max(maxy, p[1]);
		}

		openGridMap.recenter((minx + maxx) / 2, (miny + maxy) / 2, 5);

		openGridMap.subtract(scanDecay);

		ArrayList<double[]> gpoints = s.gpoints;
		double[] last_lp = new double[1];
		double lastpidx = 0;
		;
		for (int pidx = 0; pidx < gpoints.size(); pidx++) {
			double lp[] = gpoints.get(pidx);
			openGridMap.setValue(lp[0], lp[1], (byte) 255);

			if (last_lp != null && lastpidx == pidx - 1) {
				double dist = LinAlg.distance(lp, last_lp);
				if (dist < 0.25) {
					openGridMap.drawLine(last_lp[0], last_lp[1], lp[0], lp[1],
							(byte) 255);
				}
			}

			last_lp = lp;
			lastpidx = pidx;
		}

		float f[] = SigProc.makeGaussianFilter(1.5, 5);
		f = LinAlg.scale(f, 1.0 / LinAlg.max(f));
		openGridMap.filterFactoredCenteredMax(f, f);

		openMatch.setModel(openGridMap);
	}

	/**
	 * Main loop closing method.
	 * 
	 * Determines which nodes are nearby using a Dikstra projection. Validates
	 * loops using a depth first search looking for complete loops beginning and
	 * ending with the most recent node added to the graph.
	 * 
	 */
	public void close() {

		// have not accumulated enough nodes (or somehow got ahead) to
		// attempt a loop closure
		if (includeDoors) {
			if (g == null || (g.nodes.size() - doorFinder.doors.size()) <= 2
					|| lastPoseIndex == g.nodes.size()) {
				return;
			}
		} else {
			if (g == null || g.nodes.size() <= 2
					|| lastPoseIndex == g.nodes.size()) {
				return;
			}
		}

		// add all odometry edges to our hypotheses pool of edges for loop
		// validation
		for (; lastEdge < g.edges.size(); lastEdge++) {

			// grab edge in the graph
			GEdge curEdge = g.edges.get(lastEdge);

			// check the type of edge, assign as odometry if it has no type
			String type = (String) curEdge.getAttribute("type");
			if (type == null)
				type = "odom";

			// this check ensures that the edge is an odometry edge
			if (!type.equals("auto") && (curEdge instanceof GXYTEdge)
					&& (!type.equals("door"))) {
				GXYTEdge ge = (GXYTEdge) curEdge;
				addHypothesis(ge);
			}
		}

		// loop closing node
		GXYTNode refnode = (GXYTNode) g.nodes.get(lastPoseIndex);

		// candidate edges for loop validation
		ArrayList<GXYTEdge> candidates = new ArrayList<GXYTEdge>();

		// remove all matured hypothesis edges
		if (removeOldHypoEdges) {

			// run through all array lists of edges within the hash map of
			// hypothesis edges
			for (ArrayList<GXYTEdge> edges : hypoNodes.values()) {

				// run through all hypothesis edges within the given array list
				for (int edgeIndex = 0; edgeIndex < edges.size(); edgeIndex++) {

					// grab the current considered hypothesis edge
					GXYTEdge ge = edges.get(edgeIndex);

					// do not remove odometry edges
					String type = (String) ge.getAttribute("type");
					if (type == null)
						continue;

					// do not remove validated edges
					if (type.equals("auto")) {
						String added = (String) ge.getAttribute("added");
						if (added != null && added.equals("yes"))
							continue;
					}

					// check the age of the hypothesis edge
					int nodeMax = Math.max(ge.nodes[0], ge.nodes[1]);
					if ((lastPoseIndex - nodeMax - doorFinder.doors.size()) > maxHypothesisAge) {

						// change the last edge in the array list to the index
						// of this aged edge and remove the last edge
						int lastEdge = edges.size() - 1;
						edges.set(edgeIndex, edges.get(lastEdge));
						edges.remove(lastEdge);
					}
				}
			}
		}

		// Dijkstra projection of the current pose graph with respect to the
		// current pose
		DijkstraProjection dp = new DijkstraProjection(g, lastPoseIndex);

		// identify the set of nodes to scan match with the current pose
		for (int i = 0; i < lastPoseIndex; i++) {

			// each edge within the Dijkstra projection connects the current
			// pose to the other nodes while overestimating the cost (or
			// covariance in this case)
			GXYTEdge dpPrior = dp.getEdge(i);

			// compute the current predicted distance to the current considered
			// node
			double curDist = LinAlg.distance(refnode.state,
					g.nodes.get(dpPrior.nodes[1]).state, 2);

			// force the considered node to at least be within the search window
			// of our scan matcher
			if (curDist > loopmatcher.getSearchDist())
				continue;

			// calculate the Mahalanobis distance that this node is within the
			// given hypothesis threshold
			double mahl = mahalanobisDistance(dpPrior, hypoThresh);

			// add this edge to our pool of candidates to attempt to match to
			if (mahl < 0.45)
				candidates.add(dpPrior);
		}

		// shuffle, shuffle, shuffle...
		Collections.shuffle(candidates);

		// matching to as many candidate nodes as is permitted by
		// maxMatchAttempts parameter
		for (int cidx = 0; cidx < Math.min(maxMatchAttempts, candidates.size()); cidx++) {

			// grab the current considered candidate edge
			GXYTEdge dpPrior = candidates.get(cidx);

			// attempting to match the nodes
			GXYTNode nodeA = (GXYTNode) g.nodes.get(dpPrior.nodes[0]);
			GXYTNode nodeB = (GXYTNode) g.nodes.get(dpPrior.nodes[1]);
			if (nodeB.getAttribute("type") != null)
				continue;
			double[] prior = LinAlg.xytInvMul31(nodeA.state, nodeB.state);
			prior[2] = MathUtil.mod2pi(prior[2]);
			GXYTEdge ge = loopmatcher.match(nodeA, nodeB, prior);

			// scan matcher returned an edge, good match according to the close
			// loop matching parameters
			if (ge != null) {
				ge.nodes = new int[] { dpPrior.nodes[0], dpPrior.nodes[1] };
				ge.setAttribute("type", "auto");
				addHypothesis(ge);
			}

		}

		// cumulative motion around loop within the depth first search for loop
		// validation
		double loopMotion[] = new double[3];

		// tracking the edges that have already been traversed during loop
		// validation
		HashSet<GXYTEdge> visitedEdges = new HashSet<GXYTEdge>();

		// tracking poses that have already been visited during loop validation
		int poseIndices[] = new int[maxLoopLength];
		poseIndices[0] = lastPoseIndex;

		// array list of accepted edges after loop validation
		ArrayList<GXYTEdge> acceptedEdges = new ArrayList<GXYTEdge>();

		// calling recursively the depth first search loop validation algorithm
		recurse(1, poseIndices, visitedEdges, loopMotion, acceptedEdges);

		// adding best edges which meet parameter requirements
		for (int i = 0; i < Math.min(maxAddedEdges, acceptedEdges.size()); i++) {

			GXYTEdge bestEdge = null;
			double bestTrace = minTrace;

			// check that the edges meet the requirements since there are less
			// edges than are allowed to be added to the graph
			if (acceptedEdges.size() < maxAddedEdges) {

				// current considered edge to be added to the graph
				GXYTEdge currentEdge = acceptedEdges.get(i);

				// going to create another DijkstraProjection which ceases
				// searching once it has found the needed nodes here
				HashSet<Integer> neededNodes = new HashSet<Integer>();
				neededNodes.add(currentEdge.nodes[1]);

				// searching for the path from current node (ge.node[0]) to
				// matched node (ge.node[1])
				DijkstraProjection dp2 = new DijkstraProjection(g,
						currentEdge.nodes[0], null, neededNodes);

				// calculating the trace on the covariance of the returned
				// Dijkstra edge
				GXYTEdge dpPrior = dp2.getEdge(currentEdge.nodes[1]);
				double trace = LinAlg.trace(dpPrior.P);

				// calculating the Mahalanobis distance of the returned
				// Disjkstra edge
				MultiGaussian mg = new MultiGaussian(dpPrior.P, dpPrior.z);
				double mahl = mg.getMahalanobisDistance(currentEdge.z);

				// checking this Mahalanobis distance
				if (mahl > maxMahal) {
					continue;
				}

				// using the trace or the Dijkstra edge as the heuristic for
				// selecting the best edge to add to the graph
				if (!(trace > minTrace)) {
					continue;
				}

				// met all requirements, add to graph
				bestEdge = currentEdge;

			} else {

				// run through all edges returned from the depth first search
				for (GXYTEdge ge : acceptedEdges) {

					// going to create another DijkstraProjection which ceases
					// searching once it has found the needed nodes here
					HashSet<Integer> neededNodes = new HashSet<Integer>();
					neededNodes.add(ge.nodes[1]);

					// searching for the path from current node (ge.node[0]) to
					// matched node (ge.node[1])
					DijkstraProjection dp2 = new DijkstraProjection(g,
							ge.nodes[0], null, neededNodes);

					// calculating the trace on the covariance of the returned
					// Dijkstra edge
					GXYTEdge dpPrior = dp2.getEdge(ge.nodes[1]);
					double trace = LinAlg.trace(dpPrior.P);

					// calculating the Mahalanobis distance of the returned
					// Disjkstra edge
					MultiGaussian mg = new MultiGaussian(dpPrior.P, dpPrior.z);
					double mahl = mg.getMahalanobisDistance(ge.z);

					// checking this Mahalanobis distance
					if (mahl > maxMahal) {
						continue;
					}

					// using the trace as the heuristic for selecting the best
					// edge
					// to add to the graph
					if (trace > bestTrace) {
						bestEdge = ge;
						bestTrace = trace;
					}
				}
			}

			// add the best edge to the graph, remove from accepted edges
			if (bestEdge != null) {
				g.edges.add(bestEdge);
				synchronized (addedEdges) {
					addedEdges.add(bestEdge);
				}
				acceptedEdges.remove(bestEdge);
			}
		}
	}

	/**
	 * Adds hypothesis edge to group of hypotheses for loop validation.
	 * 
	 * @param ge
	 *            GXYTEdge to add to the hypotheses.
	 */
	void addHypothesis(GXYTEdge ge) {
		ArrayList<GXYTEdge> a = hypoNodes.get(ge.nodes[0]);
		if (a == null) {
			a = new ArrayList<GXYTEdge>();
			hypoNodes.put(ge.nodes[0], a);
		}
		a.add(ge);

		ArrayList<GXYTEdge> b = hypoNodes.get(ge.nodes[1]);
		if (b == null) {
			b = new ArrayList<GXYTEdge>();
			hypoNodes.put(ge.nodes[1], b);
		}
		b.add(ge);
	}

	/**
	 * Depth first search where the limit on the depth is determined by the
	 * length of the array parameter 'poseIndices'.
	 * 
	 * @param currentDepth
	 *            Current depth of the recursive search. Call initially with a
	 *            depth of 1.
	 * @param poseIndices
	 *            Array which the pose indices of the loop will be written to.
	 *            Call the search with an empty array minus the first index
	 *            which should hold the starting pose index for the search. The
	 *            length of the
	 * @param visitedEdges
	 *            Holds all visited edges during the loop search.
	 * @param loopMotion
	 *            Motion around the loop. Used to verify that the search has
	 *            traversed an actual loop once finishing at the same starting
	 *            pose.
	 * @param acceptedEdges
	 *            Holds all accepted edges from the loop verification process.
	 */
	void recurse(int currentDepth, int poseIndices[],
			HashSet<GXYTEdge> visitedEdges, double loopMotion[],
			ArrayList<GXYTEdge> acceptedEdges) {

		// do not allow the depth of the search to extend further than the
		// length of the array which holds the pose indices for loop search
		if (currentDepth >= poseIndices.length)
			return;

		// grab the hypothesis edges for the prior node within the pose indices
		ArrayList<GXYTEdge> edges = hypoNodes
				.get(poseIndices[currentDepth - 1]);

		// no edges for the considered node, end search
		if (edges == null)
			return;

		// run through each edge connected to the considered pose
		for (int eidx = 0; eidx < edges.size(); eidx++) {

			// grab each considered edge
			GXYTEdge ge = edges.get(eidx);

			// determine if this edge has been traveled through yet in the
			// search
			if (visitedEdges.contains(ge))
				continue;

			// create the new loop motion for this edge
			double newLoopMotion[];

			// edge setup leading the correct way
			if (ge.nodes[0] == poseIndices[currentDepth - 1]) {
				newLoopMotion = LinAlg.xytMultiply(loopMotion, ge.z);
				poseIndices[currentDepth] = ge.nodes[1];
			}

			// edge is backwards, flip for computation of new loop motion
			else {
				newLoopMotion = LinAlg.xytMultiply(loopMotion,
						LinAlg.xytInverse(ge.z));
				poseIndices[currentDepth] = ge.nodes[0];
			}

			// checking if the edges traveled have resulting in returning to
			// initial position
			if (poseIndices[currentDepth] == poseIndices[0]) {

				// must have traveled through at least 3 edges before a loop can
				// truly exist
				if (currentDepth < 3)
					continue;

				// determine the accuracy on the loop for further loop
				// confirmation
				final double xyerr = Math.sqrt(newLoopMotion[0]
						* newLoopMotion[0] + newLoopMotion[1]
						* newLoopMotion[1]);
				final double terr = Math.abs(MathUtil.mod2pi(newLoopMotion[2]));

				// thresholding the loop
				final double xythresh = 0.05;
				final double tthresh = Math.toRadians(1);

				// additional confirmation on the loop
				if (xyerr < xythresh && terr < tthresh) {

					// check the edges on this loop and add them to the graph if
					// they do not already exist
					for (GXYTEdge te : visitedEdges) {

						// set attribute to added if adding to the graph
						if (te.getAttribute("added") != null)
							continue;

						acceptedEdges.add(te);
						te.setAttribute("added", "yes");
					}

				}
			} else {

				// avoid looping redundantly by assuring by choosing older nodes
				// to follow
				if (poseIndices[currentDepth] > poseIndices[currentDepth - 1])
					continue;

				// recurse and remove the current edge after recursing
				visitedEdges.add(ge);
				recurse(currentDepth + 1, poseIndices, visitedEdges,
						newLoopMotion, acceptedEdges);
				visitedEdges.remove(ge);

			}
		}
	}

	/**
	 * Calculates the likelihood that two nodes are within a given range of each
	 * other according to their Mahalanobis distance.
	 * 
	 * @param ge
	 *            The edge used to calculate the Mahalanobis distance between
	 *            the two nodes in question.
	 * @param range
	 *            The range with which to determine the likelihood that the two
	 *            nodes are that close to each other.
	 * @return Likelihood that the two nodes are within 'range' meters of each
	 *         other.
	 */
	double mahalanobisDistance(GXYTEdge ge, double range) {

		// distance to other node according to edge values
		double dist = Math.sqrt(ge.z[0] * ge.z[0] + ge.z[1] * ge.z[1]);

		// remaining fraction of the distance to the base node after subtracting
		// range
		double distRatio;

		if (dist < range)
			distRatio = 0;
		else
			distRatio = (dist - range) / dist;

		// positional error based on
		double err[] = new double[] { ge.z[0] * distRatio, ge.z[1] * distRatio };

		// compute Mahalanobis distance
		double W[][] = ge.getW();
		return Math.sqrt(err[0] * err[0] * W[0][0] + 2 * err[0] * err[1]
				* W[0][1] + err[1] * err[1] * W[1][1]);
	}

	/**
	 * Solves the non-linear SLAM equation for the pose graph created by
	 * reordering the nodes using a Minimum Degree Ordering heuristic and a
	 * Cholesky decomposition.
	 * 
	 * @param g
	 *            Graph object to solve.
	 */
	@SuppressWarnings("static-access")
	void optimizeFull(Graph g) {

		// use error statistics to determine when to cease iterating
		double chi2Before = g.getErrorStats().chi2normalized;

		// solver using a Cholesky decomposition of the non-linear SLAM matrix
		CholeskySolver gs = new CholeskySolver(g, new MinimumDegreeOrdering());
		gs.verbose = false;

		// must iterate due to non-linear approximations using Jacobians
		for (int iter = 0; iter < 10; iter++) {
			gs.iterate();

			double chi2After = g.getErrorStats().chi2normalized;

			if (chi2After >= 0.8 * chi2Before || chi2After < 0.00001)
				break;

			chi2Before = chi2After;
		}
	}
}
