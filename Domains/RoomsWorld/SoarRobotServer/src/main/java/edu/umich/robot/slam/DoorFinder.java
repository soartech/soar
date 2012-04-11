package edu.umich.robot.slam;

import java.util.ArrayList;
import java.util.HashMap;

import april.config.Config;
import april.graph.GXYTEdge;
import april.graph.GXYTNode;
import april.graph.Graph;
import april.jmat.LinAlg;
import april.jmat.MathUtil;

public class DoorFinder {

	// data association output option
	boolean daOutput = false;

	// mapping output option
	boolean mapOutput = false;

	/**
	 * The following parameters are used by the 'findDoors' method and may
	 * adjusted based on the environment.
	 * 
	 * @param maxPoints
	 *            The number of LIDAR points that are searched after a possible
	 *            beginning to a door has been found in order to find the other
	 *            edge to a possible door.
	 * @param angThresh
	 *            Minimum angle threshold between two LIDAR points before
	 *            considering the points a possible edge of a door (see
	 *            locateDoors method for understanding of this angle
	 *            calculation).
	 * @param lowerAngThresh
	 *            Minimum double gated angle threshold between two LIDAR points
	 *            before considering a possible door (LIDAR scan sees a door but
	 *            nothing beyond the door, the two points considered by the
	 *            method below must be below this parameter before they will be
	 *            considered as a possible door).
	 * @param distThresh
	 *            Minimum distance between two LIDAR points before considering a
	 *            possible edge of a door.
	 * @param gapThresh
	 *            Minimum distance between two LIDAR points before considering a
	 *            possible door (LIDAR scan sees a door but nothing beyond the
	 *            door, the two points considered by the method below must be
	 *            below this parameter before they will be considered as a
	 *            possible door).
	 * @param doorDist
	 *            Maximum distance the considered door must be away before
	 *            actually classifying it as a door.
	 * @param windowThresh
	 *            Setting a window size in meters around the center of a
	 *            possible door. Used to check if other LIDAR points are within
	 *            this window.
	 * @param lowerDoorSize
	 *            Minimum size of a possible door before classifying it as a
	 *            door.
	 * @param upperDoorSize
	 *            Maximum size of a possible door before classifying it as a
	 *            door.
	 * @param lineFitThresh
	 *            Maximum threshold of the linear regression used on the points
	 *            which make up a possible door.
	 * @param linePoints
	 *            Amount of additional points to use in the linear regression
	 *            algorithm. Add this number in front and behind the edges of
	 *            the door.
	 * @param maxDoors
	 *            Maximum amount of doors to find per scan.
	 */
	int maxPoints = 30;
	double angThresh = Math.toRadians(50);
	double distThresh = 0.05;
	double gapThresh = 2;
	double doorDist = 15;
	double windowThresh = 1;
	double lowerDoorSize = 1.0;
	double upperDoorSize = 5.0;
	double lineFitThresh = 0.05;
	int linePoints = 4;
	int maxDoors = 3;

	/**
	 * The following parameters are used by the 'dataAssociation' method and may
	 * be adjusted based on environment or desired results.
	 * 
	 * @param upperAssociationThresh
	 *            Double gated upper threshold on the best data association
	 *            match. If association heuristic is above this value, assign
	 *            new landmark.
	 * @param lowerAssociationThresh
	 *            Double gated lower threshold on the best data association
	 *            match. If association heuristic is below this value, associate
	 *            with corresponding landmark.
	 */
	double upperAssociationThresh = 3;
	double lowerAssociationThresh = 0.5;

	/**
	 * The following variables are used to topologically map out the current
	 * environment with respect to the rooms and doors in the environment.
	 * 
	 * @variable doors --An array list of type 'Door' of all the doors currently
	 *           found in the environment and their respective properties
	 *           according to their class (see 'Door' class below).
	 * @variable lastDoorInside --The door number the robot was last located
	 *           inside of.
	 * @variable prioPose --At any instance, the last pose of the robot within a
	 *           known door.
	 * @variable inDoor --A boolean flag representing whether or not the robot's
	 *           last pose was inside of a door's window.
	 * @variable numRooms --A current count of all of the rooms within the
	 *           environment.
	 * @variable currentRoom --The current room number the robot is located
	 *           within.
	 * @variable roomMap --A hash map holding the information of all rooms and
	 *           the doors which the room is connected to. Each key is an
	 *           integer room number corresponding to an array list of integers,
	 *           each element being an integer door number connected to the room
	 *           specified by the key (rooms begin at '1', doors begin at '0').
	 */
	ArrayList<Door> doors = new ArrayList<Door>();
	int lastDoorInside;
	double[] priorPose;
	boolean inDoor = false;
	int numRooms = 1;
	int currentRoom = 1;
	HashMap<Integer, ArrayList<Integer>> roomMap = new HashMap<Integer, ArrayList<Integer>>();

	/**
	 * Door class containing information about a door within the environment.
	 */
	public static class Door {

		// door number within the topological map
		int doorNumber;

		// node index within the pose graph
		int graphNodeIndex;

		// the room numbers which this door connects
		int[] connectsRooms = new int[2];

		// list of all the poses within the pose graph that have seen this door
		ArrayList<Integer> poseNodes = new ArrayList<Integer>();
	}

	/**
	 * Constructor method (can be called with null as input).
	 * 
	 * @param config
	 *            Configuration file (see april.config for more information).
	 */
	public DoorFinder(Config config) {

		// configuration file exists
		if (config != null) {

			// updating door finding parameters
			this.maxPoints = config.getInt("maxPoints", maxPoints);
			this.angThresh = Math.toRadians(config.getDouble("angThresh",
					Math.toDegrees(angThresh)));
			this.distThresh = config.getDouble("distThresh", distThresh);
			this.gapThresh = config.getDouble("gapThresh", gapThresh);
			this.doorDist = config.getDouble("doorDist", doorDist);
			this.windowThresh = config.getDouble("windowThresh", windowThresh);
			this.lowerDoorSize = config.getDouble("lowerDoorSize",
					lowerDoorSize);
			this.upperDoorSize = config.getDouble("upperDoorSize",
					upperDoorSize);
			this.lineFitThresh = config.getDouble("lineFitThresh",
					lineFitThresh);
			this.linePoints = config.getInt("linePoints", linePoints);
			this.maxDoors = config.getInt("maxDoors", maxDoors);

			// updating data association parameters
			this.upperAssociationThresh = config.getDouble(
					"upperAssociationThresh", upperAssociationThresh);
			this.lowerAssociationThresh = config.getDouble(
					"lowerAssociationThresh", lowerAssociationThresh);
		}

		// initialize the room map
		roomMap.put(currentRoom, new ArrayList<Integer>());
	}

	// used by the SLAM method to visualize door edges added to the graph
	int edgesAdded = 0;

	/**
	 * Main method for finding doors and adding them to the graph.
	 * 
	 * Within this method doors are located, associated, updated within the SLAM
	 * pose graph, and the the topological map is built.
	 * 
	 * @param scan
	 *            Set of LIDAR points in robot coordinate frame. Each entry
	 *            within the array list is a two element array of type double
	 *            representing a single LIDAR point. The first element is the
	 *            x-coordinate and the second element is the y-coordinate of the
	 *            LIDAR point.
	 * @param G
	 *            Pose graph being utilized for SLAM.
	 * @param poseIndex
	 *            The index of the current pose within graph.nodes corresponding
	 *            to the LIDAR scan that doors may reside within.
	 * @param curPose
	 *            The current pose of the robot (x, y, t).
	 * @param matcher
	 *            The instance of the loop closing matcher used for SLAM.
	 * 
	 * @param poseThresh
	 *            The distance that must be traveled prior to a new node being
	 *            created. Use to determine the size of a door window when
	 *            calculating whether a robot is inside or a door or not.
	 */
	public void findDoors(ArrayList<double[]> scan, Graph g, int poseIndex,
			double[] curPose, LoopClosureMatcher matcher, double poseThresh) {

		// reset 'edgesAdded' for SLAM GUI
		edgesAdded = 0;

		// find doors in the given scan
		ArrayList<double[]> doorsFound = locateDoors(scan, curPose);

		// do data associations
		int[] doorIndices = dataAssociation(doorsFound, g, poseIndex, matcher);

		// update graph according to data association
		updateGraph(doorIndices, g, poseIndex, curPose, doorsFound, poseThresh);
	}

	/**
	 * Finds doors within a given LIDAR scan.
	 * 
	 * Algorithm walks though every LIDAR point and attempts to find two edges
	 * of a door. Multiple thresholding occurs according to the parameters
	 * within this class (see above for thresholding parameters and their
	 * descriptions).
	 * 
	 * @param rpoints
	 *            Set of LIDAR points in robot coordinate frame. Each entry
	 *            within the array list is a two element array of type double
	 *            representing a single LIDAR point. The first element is the
	 *            x-coordinate and the second element is the y-coordinate of the
	 *            LIDAR point.
	 * @param xyt
	 *            Current location of the robot (x, y, t).
	 * @return Returns an array list where each element within the list is a
	 *         three element array of type double representing a single door
	 *         found within the scan. The first element is the x-coordinate, the
	 *         second element is the y-coordinate, and the third element is the
	 *         orientation of the door in the global coordinate frame. Doors are
	 *         ordered according to their distance away from the current pose of
	 *         the robot, the closest door found being first, and the farthest
	 *         door found being last within the array list.
	 */
	public ArrayList<double[]> locateDoors(ArrayList<double[]> rpoints,
			double[] xyt) {

		// boolean flags
		boolean gap = true;
		boolean closeOp = false;

		// holds points for line fitting
		ArrayList<double[]> points = new ArrayList<double[]>();

		// holds located door locations (return variable)
		ArrayList<double[]> doors = new ArrayList<double[]>();

		// initializations prior to looping over LIDAR points
		int i = linePoints + 1;
		double[] curPoint = rpoints.get(i - 1);

		// searching through all LIDAR points for possible door edges
		while (i < rpoints.size()) {

			// next and last point in our scan
			double[] nextPoint = rpoints.get(i);
			double[] lastPoint = rpoints.get(i - 2);

			// distance between next point and our current point
			double dist = LinAlg.distance(nextPoint, curPoint, 2);

			// create vectors and determine the angle between them
			// v1 is the vector from lastPoint to curPoint
			// v2 is the vector from curPoint to nextPoint
			double[] v1 = LinAlg.subtract(curPoint, lastPoint);
			double[] v2 = LinAlg.subtract(nextPoint, curPoint);
			double ang = -MathUtil.mod2pi(Math.atan2(v2[1], v2[0])
					- Math.atan2(v1[1], v1[0]));

			// determining if a possible edge for a door has been found
			if ((ang > angThresh && dist > distThresh) || (dist > gapThresh)) {
				closeOp = true;
			} else {
				curPoint = LinAlg.copy(nextPoint);
				i = i + 1;
				continue;
			}

			// an edge to a possible door has been found, now looking for
			// the other edge to the door
			if (closeOp) {

				// searching ahead for the other edge of the door
				for (int j = 1; j < maxPoints; j++) {

					// avoid getting ahead of total number of points in scan
					if (j + i + linePoints + 1 > rpoints.size()) {
						break;
					}

					// current point being considered as the other door edge
					double[] closePoint = rpoints.get(j + i);

					// find other edge (same as first edge)
					double[] aheadPoint = rpoints.get(j + i + 1);
					double[] behindPoint = rpoints.get(j + i - 1);

					// distance between close point and ahead point
					dist = LinAlg.distance(behindPoint, closePoint, 2);

					// create vectors and determine the angle between them
					// v1 is the vector from behindPoint to
					// closePoint
					// v2 is the vector from closePoint to aheadPoint
					v1 = LinAlg.subtract(closePoint, behindPoint);
					v2 = LinAlg.subtract(aheadPoint, closePoint);
					ang = -MathUtil.mod2pi(Math.atan2(v2[1], v2[0])
							- Math.atan2(v1[1], v1[0]));

					// determining if there is an edge to a door
					if ((ang > angThresh && dist > distThresh)
							|| (dist > gapThresh)) {

						// clean out array list points for line fitting
						points.clear();

						// points to attempt to fit a line to
						points.add(lastPoint);
						points.add(curPoint);
						points.add(closePoint);
						points.add(aheadPoint);

						// additional points for line fitting (padding our line)
						for (int z = 2; z < linePoints + 1; z++) {
							// point behind
							points.add(rpoints.get(i - z - 1));

							// point ahead
							points.add(rpoints.get(i + j + z));
						}

						// fit this line
						double[] line = LinAlg.fitLine(points);

						// if line is close to being 'vertical', invert x/y and
						// recompute the linear regression
						if (Math.abs(line[0]) > 20) {

							ArrayList<double[]> newPoints = new ArrayList<double[]>();
							for (double[] pc : points) {
								double[] npc = new double[] { pc[1], pc[0] };
								newPoints.add(npc);
							}

							line = LinAlg.fitLine(newPoints);
							line[0] = (1 / line[0]);
						}

						// thresholding on the line fitting
						if (line[2] < lineFitThresh) {

							// check for a true gap
							double xcenter = (curPoint[0] + closePoint[0]) / 2;
							double ycenter = (curPoint[1] + closePoint[1]) / 2;

							// looking for points between the two edges within
							// the window threshold of our gap
							for (int k = i + 2; k < i + j - 1; k++) {
								double[] gapPoint = rpoints.get(k);
								double xrange = Math.abs(gapPoint[0] - xcenter);
								double yrange = Math.abs(gapPoint[1] - ycenter);

								// final threshold check on the considered point
								// between the possible door edges
								if (xrange < windowThresh
										&& yrange < windowThresh) {

									// reset gap flag and break this loop to
									// continue searching for the other edge of
									// the door
									gap = false;
									break;
								}
							}

							// if there is a gap, continue verifying an actual
							// door has been found
							if (gap) {

								// lets test the distance of this gap now
								double gapDist = LinAlg.distance(closePoint,
										curPoint, 2);

								// checking size of door against
								if (gapDist > lowerDoorSize
										&& gapDist < upperDoorSize) {

									// checking how far away the door is
									if (Math.sqrt(Math.pow(xcenter, 2)
											+ Math.pow(ycenter, 2)) < doorDist) {

										// checking if the robot can see the
										// door based on the LIDAR scan (see
										// method below)
										int numPoints = seeDoor(rpoints, i, i
												+ j, curPoint, closePoint);

										// checking how many points obstruct our
										// view
										if (numPoints < 10) {
											// finally found a door, determine
											// orientation of door
											double AofDoor = Math.atan2(
													-(1 / line[0]), 1);
											double[] dHolder = LinAlg
													.xytMultiply(xyt,
															new double[] {
																	xcenter,
																	ycenter,
																	AofDoor });
											dHolder[2] = MathUtil
													.mod2pi(dHolder[2]);

											double[] newDoor = new double[] {
													dHolder[0], dHolder[1],
													dHolder[2], gapDist };

											// add this door according to
											// distance sorting to array list
											// for return
											doors = sortDoors(doors, newDoor,
													xyt);

											// update index i for search
											i = i + j + 2;

											// reset flags and current point
											// considered
											closeOp = false;
											curPoint = rpoints.get(i - 1);
											break;
										}
									}
								}

							} else {
								// reset gap flag and continue search for other
								// edge of door
								gap = true;
							}

						}
					}
				}
			}

			// has a door been found
			if (!closeOp) {

				// how many doors in this scan can be found
				if (doors.size() == maxDoors)
					return doors;
			}
			// a door was not found, reset index and keep searching
			else {
				i = i + 1;
				curPoint = LinAlg.copy(nextPoint);
				closeOp = false;
			}
		}
		return doors;
	}

	/**
	 * The following method places a newly found door into the original array
	 * list of found doors for a given scan at the index which it occurs with
	 * respect to its distance from the current robot pose.
	 * 
	 * @param oldDoors
	 *            The prior array list of doors found within a LIDAR scan.
	 * @param newDoor
	 *            Location of the newly found door.
	 * @param xyt
	 *            Current location of the robot.
	 * @return Array list containing original door locations as well as the new
	 *         door location, sorted according to their distance from the
	 *         robot's pose.
	 */
	public ArrayList<double[]> sortDoors(ArrayList<double[]> oldDoors,
			double[] newDoor, double[] xyt) {

		// if doors is empty, add door and return
		if (oldDoors.isEmpty()) {
			oldDoors.add(newDoor);
			return oldDoors;
		}

		// calculate distance from current door to others
		double dist = LinAlg.distance(newDoor, xyt, 2);

		// call recursive sorting method to place door in the correct location
		// according to distance to robot
		ArrayList<double[]> sortedDoors = recurseSort(oldDoors, newDoor, xyt,
				dist, 0);
		return sortedDoors;
	}

	/**
	 * The following method places a newly found door into the original array
	 * list of doors found within a scan at the index which it occurs with
	 * respect to its distance from the current robot pose. The method achieves
	 * this by recursing down the original door array and then rebuilding a door
	 * array once the new door's location in the array has been found.
	 * 
	 * @param oldDoors
	 *            The prior array list of doors found within a LIDAR scan.
	 * @param newDoor
	 *            Location of the newly found door.
	 * @param xyt
	 *            Current location of the robot.
	 * @param dist
	 *            Distance from the robot to the newly found door.
	 * @param idx
	 *            Recurse index within the original door array.
	 * @return Array list containing original door locations as well as the new
	 *         door location, sorted according to their distance from the
	 *         robot's pose.
	 */
	public ArrayList<double[]> recurseSort(ArrayList<double[]> oldDoors,
			double[] newDoor, double[] xyt, double dist, int idx) {

		// check if we have reached the end of the other doors found
		if (oldDoors.size() == idx) {
			oldDoors.add(newDoor);
			return oldDoors;
		}

		// created new array list for sorted doors
		ArrayList<double[]> sortedDoors = new ArrayList<double[]>();

		// calculate distance to current idx element
		double idxDist = LinAlg.distance(xyt, oldDoors.get(idx));

		// compare distances, recurse or add in new door
		if (dist < idxDist) {

			// add all elements in original list prior to this door
			for (int i = 0; i < idx; i++)
				sortedDoors.add(oldDoors.get(i));

			// add new door
			sortedDoors.add(newDoor);

			// add all elements in original list post new door
			for (int i = idx; i < oldDoors.size(); i++)
				sortedDoors.add(oldDoors.get(i));

			// return new sorted door list
			return sortedDoors;
		}

		// recurse while incrementing search index
		sortedDoors = recurseSort(oldDoors, newDoor, xyt, dist, idx + 1);
		return sortedDoors;
	}

	/**
	 * Determines if the robot can see a particular door given a LIDAR scan.
	 * 
	 * To determine if any points are obstructing the view of the door, all
	 * points between the open and close edge of the door are tested against the
	 * closest point to the door.
	 * 
	 * @param scan
	 *            Set of LIDAR points in robot coordinate frame. Each entry
	 *            within the array list is a two element array of type double
	 *            representing a single LIDAR point. The first element is the
	 *            x-coordinate and the second element is the y-coordinate of the
	 *            LIDAR point.
	 * @param oPidx
	 *            Index within the LIDAR scan array list of the point considered
	 *            to be the open edge of the door.
	 * @param cPidx
	 *            Index within the LIDAR scan array list of the point considered
	 *            to be the close edge of the door.
	 * @param openPoint
	 *            Location of the point considered to be the open edge of the
	 *            door.
	 * @param closePoint
	 *            Location of the point considered to be the close edge of the
	 *            door.
	 * @return Returns the amount of points within the search cone closer to the
	 *         robot than the closest point of the door.
	 */
	public int seeDoor(ArrayList<double[]> scan, int oPidx, int cPidx,
			double[] openPoint, double[] closePoint) {

		// where to start search / end search
		int start = Math.min(oPidx, cPidx);
		int stop = Math.max(oPidx, cPidx);

		// determining the minimum value of the search cone
		double rangeOpen = Math.sqrt(Math.pow(openPoint[0], 2)
				+ Math.pow(openPoint[1], 2));
		double rangeClose = Math.sqrt(Math.pow(closePoint[0], 2)
				+ Math.pow(closePoint[1], 2));
		double minRange = Math.min(rangeOpen, rangeClose);

		// counter for the points within the search cone
		int pointsWithin = 0;

		// running through all points within the search cone to determine which
		// ones are closer to us than the door
		for (int i = start + 1; i < stop; i++) {
			double[] perpPoint = scan.get(i);
			double perpRange = Math.sqrt(Math.pow(perpPoint[0], 2)
					+ Math.pow(perpPoint[1], 2));

			if (perpRange < minRange)
				pointsWithin = pointsWithin + 1;
		}

		return pointsWithin;
	}

	/**
	 * Nearest neighbor data association algorithm using 'associationHeuristic'
	 * method instead of distance.
	 * 
	 * Determines from the newly located doors which doors correspond to
	 * previously seen doors.
	 * 
	 * @param doorList
	 *            Array list of doors recently found in a LIDAR scan.
	 * @param graph
	 *            Graph being used for SLAM.
	 * @param poseIdx
	 *            Index within graph.nodes of the current robot pose.
	 * @param matcher
	 *            The instance of the loop closing matcher used for SLAM.
	 * @return Returns an array list of type integer the same size as the number
	 *         of doors passed into the method via the door array list. Each
	 *         index within the returned array corresponds to the door of the
	 *         same index within the door array list. This method uses the
	 *         'associationHeuristic' method to determine associations. The
	 *         integer values returned represent the index within graph.nodes
	 *         with which a given door has been associated with. A value of '-1'
	 *         corresponds to a new door while a value of '-2' corresponds to an
	 *         observation of a door which should be ignored according to our
	 *         double gated parameters (see class parameters above).
	 */
	public int[] dataAssociation(ArrayList<double[]> doorList, Graph graph,
			int poseIdx, LoopClosureMatcher matcher) {

		// indices within graph of data associations
		int[] indices = new int[doorList.size()];

		// no doors yet, so all doors are new and should be initialized
		if (doors.isEmpty()) {
			for (int i = 0; i < doorList.size(); i++)
				indices[i] = -1;
			return indices;
		}

		// current pose in our map that has just been added
		GXYTNode currentPose = (GXYTNode) graph.nodes.get(poseIdx);

		// run through each located door and attempt association with prior
		// doors
		for (int j = 0; j < doorList.size(); j++) {

			// current door attempting to associate
			double[] doorLocation = doorList.get(j);
			double bestVal;
			int bestDoor;

			// initiate algorithm by assuming association with the first door
			// ever found
			Door curDoor = doors.get(0);

			// get the door node from the graph based on our door map (mapping
			// door number to index within graph.nodes)
			GXYTNode doorNode = (GXYTNode) graph.nodes
					.get(curDoor.graphNodeIndex);

			// get the index within graph.nodes of the last pose that saw the
			// door being considered for association
			int lastNodeIdx = curDoor.poseNodes
					.get(curDoor.poseNodes.size() - 1);

			GXYTNode lastPose = (GXYTNode) graph.nodes.get(lastNodeIdx);

			// get association heuristic value
			bestVal = associationHeuristic(currentPose, lastPose, doorLocation,
					doorNode.state, matcher);

			// USE THIS HUERISTIC IF JUST USING EUCLIDEAN DISTANCE
			// bestVal = LinAlg.distance(doorLocation, doorNode.state, 2);

			bestDoor = curDoor.doorNumber;

			// run through the rest of the doors
			for (int p = 1; p < doors.size(); p++) {

				curDoor = doors.get(p);

				// run through each other door, determine if each associations
				// heuristic value is better than the current running 'best'
				doorNode = (GXYTNode) graph.nodes.get(curDoor.graphNodeIndex);

				lastNodeIdx = curDoor.poseNodes
						.get(curDoor.poseNodes.size() - 1);

				lastPose = (GXYTNode) graph.nodes.get(lastNodeIdx);

				double possBest = associationHeuristic(currentPose, lastPose,
						doorLocation, doorNode.state, matcher);

				// USE THIS HUERISTIC IF JUST USING EUCLIDEAN DISTANCE
				// double possBest = LinAlg.distance(doorLocation,
				// doorNode.state,
				// 2);

				if (possBest < bestVal) {
					bestVal = possBest;
					bestDoor = curDoor.doorNumber;
				}
			}

			if (daOutput)
				System.out.println("	New door location: " + doorLocation[0]
						+ ", " + doorLocation[1]);

			// double gate on our scan matching distance heuristic
			// new door to be added to the graph
			if (bestVal > upperAssociationThresh) {
				indices[j] = -1;
				if (daOutput)
					System.out.println("	Door Not Associated");
				continue;
			}

			// associated door
			if (bestVal < lowerAssociationThresh) {
				indices[j] = bestDoor;
				if (daOutput) {
					System.out
							.println("	Associated locate: "
									+ graph.nodes.get(doors.get(bestDoor).graphNodeIndex).state[0]
									+ ", "
									+ graph.nodes.get(doors.get(bestDoor).graphNodeIndex).state[1]);
					System.out.println("	Association ID: " + bestDoor);
				}
				continue;
			}

			// ignore this observation (inside gated thresholds)
			indices[j] = -2;
			if (daOutput) {
				System.out.println("	Ignoring new door");
				System.out.println();
			}
		}

		return indices;
	}

	/**
	 * Association heuristic used to inform data association method. The method
	 * attempts to scan match the LIDAR points between the robot's current scan
	 * and the robot's scan from the previous pose that saw the specified door.
	 * If that scan match succeeds based on the SLAM scan matching parameters,
	 * then the difference between the old considered door's location and the
	 * new door's location is compared with the difference between the current
	 * robot's location and the location that the scan match would suggest the
	 * robot is located (with respect to the previous node's reference frame) in
	 * order to return a value which represents how likely the new door located
	 * is to be the previously considered door.
	 * 
	 * @param curPose
	 *            Current pose of the robot.
	 * @param oldPose
	 *            Last pose of the robot that saw the considered door whose
	 *            state resided in the parameter 'oldDoorLocation'.
	 * @param curDoorLocation
	 *            Current location of the newly found door.
	 * @param oldDoorLocation
	 *            Location of the previous found door that is currently being
	 *            considered for data association.
	 * @param matcher
	 *            Scan matcher object from SLAM.
	 * @return Double value representing the likelihood that the newly found
	 *         door is truly the old door being considered.
	 */
	public double associationHeuristic(GXYTNode curPose, GXYTNode oldPose,
			double[] curDoorLocation, double[] oldDoorLocation,
			LoopClosureMatcher matcher) {

		// prior edge between current pose and last pose that saw the considered
		// door
		double[] prior = LinAlg.xytInvMul31(curPose.state, oldPose.state);
		prior[2] = MathUtil.mod2pi(prior[2]);

		// attempt to scan match current pose and last pose that saw the
		// considered door
		GXYTEdge ge = matcher.match(curPose, oldPose, prior);

		// difference vector between current doors location and the previous
		// location of the door
		double[] assocDiff = new double[] {
				oldDoorLocation[0] - curDoorLocation[0],
				oldDoorLocation[1] - curDoorLocation[1] };

		// ensuring that a scan match attempt succeeded
		if (ge != null) {

			// reversing direction of edge
			double[] updateEdge = LinAlg.xytInverse(ge.z);

			// calculating the difference vector of where the scan match edge
			// would put the current pose with respect to the last node that saw
			// the considered door's reference frame and the current pose
			double[] newPoseLocation = LinAlg.xytMultiply(oldPose.state,
					updateEdge);
			double[] poseLocatDiff = new double[] {
					newPoseLocation[0] - curPose.state[0],
					newPoseLocation[1] - curPose.state[1] };

			// returning the difference between the following vectors:
			// 1.) difference between current / old location of door
			// 2.) difference between current pose / pose of scan match
			return LinAlg.distance(poseLocatDiff, assocDiff);
		}

		// if no edge was available, simple using distance vector of old door's
		// location and new door's location as the heuristic value
		return Math.sqrt(Math.pow(assocDiff[0], 2) + Math.pow(assocDiff[1], 2));
	}

	/**
	 * Main method for updating the SLAM pose graph with the found doors as well
	 * as updating the topological map.
	 * 
	 * @param idxs
	 *            Data association indices on the newly found doors.
	 * @param graph
	 *            Pose graph from SLAM.
	 * @param poseidx
	 *            Index of the current pose within the pose graph.
	 * @param xyt
	 *            Location of the robot (x, y, t).
	 * @param foundDoors
	 *            Array list containing all the found doors from the current
	 *            scan.
	 * @param poseThresh
	 *            The distance that must be traveled prior to a new node being
	 *            created.
	 */
	public void updateGraph(int[] idxs, Graph graph, int poseidx,
			final double[] xyt, ArrayList<double[]> foundDoors,
			double poseThresh) {

		// we have crossed a door, need to update what room we are in
		if (passedThroughDoor(graph, xyt, poseThresh)) {
			// according to the door just crossed, what room are we in
			Door doorPassed = doors.get(lastDoorInside);

			if (mapOutput) {
				System.out.println("We have crossed through a door:");
				System.out.println("Door connects room:"
						+ doorPassed.connectsRooms[0]);
				System.out.println("Door connects room:"
						+ doorPassed.connectsRooms[1]);
			}

			// which room is not the one we were just in
			int room1 = doorPassed.connectsRooms[0];
			if (room1 != currentRoom) {

				// been in this room before
				if (room1 != 0) {

					// simple update of current room
					currentRoom = room1;

					if (mapOutput)
						System.out.println("Entering Old Room: " + currentRoom);

				} else {

					// give this room an identification
					numRooms = numRooms + 1;
					currentRoom = numRooms;
					doorPassed.connectsRooms[0] = currentRoom;

					if (mapOutput)
						System.out
								.println("Exploring New Room: " + currentRoom);

					// update room map
					ArrayList<Integer> doorList = new ArrayList<Integer>();
					doorList.add(doorPassed.doorNumber);
					roomMap.put(currentRoom, doorList);
				}
			} else {
				int room2 = doorPassed.connectsRooms[1];

				// been in this room before
				if (room2 != 0) {

					// simple update of current room
					currentRoom = room2;

					if (mapOutput)
						System.out.println("Entering Old Room: " + currentRoom);

				} else {

					// give this room an identification
					numRooms = numRooms + 1;
					currentRoom = numRooms;
					doorPassed.connectsRooms[1] = currentRoom;

					if (mapOutput)
						System.out
								.println("Exploring New Room: " + currentRoom);

					// update room map
					ArrayList<Integer> doorList = new ArrayList<Integer>();
					doorList.add(doorPassed.doorNumber);
					roomMap.put(currentRoom, doorList);
				}
			}
		}

		// only updating graph / door map if we are not in a door
		if (!inDoor) {

			// run through each door, process accordingly
			for (int i = 0; i < foundDoors.size(); i++) {

				// lets determine if this door is within our known room
				// parameters
				boolean connects2Room = doorConnects2Room(xyt,
						foundDoors.get(i), idxs[i], roomMap.get(currentRoom),
						graph);

				// new door
				if (idxs[i] == -1 && connects2Room) {

					// considered door
					double[] doorState = foundDoors.get(i);

					// create new door, update info, add to doors
					Door newDoor = new Door();
					newDoor.doorNumber = doors.size();
					newDoor.graphNodeIndex = graph.nodes.size();
					newDoor.poseNodes.add(poseidx);
					newDoor.connectsRooms[0] = currentRoom;
					doors.add(newDoor);

					// update room map according to if this room has any door
					// information already
					roomMap.get(currentRoom).add(newDoor.doorNumber);

					// create door node, update it's information, add to graph
					GXYTNode doorNode = new GXYTNode();
					doorNode.state = LinAlg.copy(doorState, 3);
					doorNode.init = LinAlg.copy(doorState, 3);
					doorNode.setAttribute("type", "door");
					doorNode.setAttribute("doorWidth", doorState[3]);
					doorNode.setAttribute("doorNumber", newDoor.doorNumber);
					graph.nodes.add(doorNode);

					// create edge from current pose to this new door
					GXYTEdge doorEdge = new GXYTEdge();
					doorEdge.z = LinAlg.xytInvMul31(xyt, doorNode.state);
					doorEdge.nodes = new int[] { poseidx,
							graph.nodes.size() - 1 };
					doorEdge.P = LinAlg
							.diag(new double[] { 0.01, 0.01, 0.001 });
					doorEdge.setAttribute("type", "door");
					graph.edges.add(doorEdge);
					edgesAdded += 1;
					continue;
				}

				// we have seen this door before, close loop on door mapping
				if (idxs[i] > -1 && connects2Room) {

					// check if door connection list is in accordance with
					// current room
					Door foundDoor = doors.get(idxs[i]);
					int room1 = foundDoor.connectsRooms[0];
					int room2 = foundDoor.connectsRooms[1];

					// door is in accordance with current room
					if (room1 == currentRoom || room2 == currentRoom) {

						// add pose to the doors pose list
						foundDoor.poseNodes.add(poseidx);
						continue;
					}

					// old door never explored from this direction && attached
					// to current room
					if (room1 == 0) {
						// add this room to connnectsRooms, update poseNodes,
						// update roomMap
						foundDoor.connectsRooms[0] = currentRoom;
						foundDoor.poseNodes.add(poseidx);

						if (mapOutput)
							System.out.println("Saw door: "
									+ foundDoor.doorNumber
									+ " , first time seen from this room.");

						roomMap.get(currentRoom).add(foundDoor.doorNumber);
						continue;
					}
					if (room2 == 0) {
						foundDoor.connectsRooms[1] = currentRoom;
						foundDoor.poseNodes.add(poseidx);

						if (mapOutput)
							System.out.println("Saw door: "
									+ foundDoor.doorNumber
									+ " , first time seen from this room.");

						roomMap.get(currentRoom).add(foundDoor.doorNumber);
						continue;
					}

					// if we have made it here, we are in a room we have been
					// before, and we traveled through a door which we haven't
					// from the old room to get to this room...thus we need to
					// merge the two rooms...
					if (connects2Room) {
						double[] room1Pose = graph.nodes
								.get(foundDoor.poseNodes.get(0)).state;
						int actualRoom;
						if (oppoSideDoor(xyt, room1Pose,
								graph.nodes.get(foundDoor.graphNodeIndex).state))
							actualRoom = foundDoor.connectsRooms[0];
						else
							actualRoom = foundDoor.connectsRooms[1];

						// merge current room and the actual room
						mergeRooms(actualRoom, currentRoom);

						if (mapOutput) {
							System.out.println("Saw door: "
									+ foundDoor.doorNumber);
							System.out.println("We have merged rooms: "
									+ currentRoom + " , and " + actualRoom);
							System.out
									.println("Current room is: " + actualRoom);
						}
						currentRoom = actualRoom;
					}

				}
			}
		}
	}

	/**
	 * Merges two rooms in the topological map together (identified by their
	 * respective integer room number). This occurs when the robot enters a room
	 * it has been in from a doorway it did not find when previously in the
	 * room. (Typically, this does not occur as the door finder method is rather
	 * robust. However, it has been included for additional robustness).
	 * 
	 * @param oldRoom
	 *            Integer first assigned to the current room the robot is in.
	 * @param newRoom
	 *            New integer that has been assigned to the room which must be
	 *            merged with the old room and then removed from the topological
	 *            map.
	 */
	public void mergeRooms(int oldRoom, int newRoom) {

		// grab new rooms door list to be merged
		ArrayList<Integer> newRoomDoors = roomMap.get(newRoom);

		// run through each door in new room's door list
		for (int doorIdx : newRoomDoors) {

			// grab each door from new room's door list
			Door curDoor = doors.get(doorIdx);

			// update the connecting room to be the old room
			if (curDoor.connectsRooms[0] == newRoom) {
				curDoor.connectsRooms[0] = oldRoom;
			} else {
				curDoor.connectsRooms[1] = oldRoom;
			}

			// add this door to the old rooms door list if not already present
			ArrayList<Integer> oldRoomDoors = roomMap.get(oldRoom);
			if (!oldRoomDoors.contains(curDoor.doorNumber)) {
				oldRoomDoors.add(curDoor.doorNumber);
			}
		}

		// remove from room map the new room
		roomMap.remove(newRoom);
	}

	/**
	 * Method to ensure that a door that is being seen is within the known
	 * boundaries of the current room the robot is located within. This is
	 * accomplished by first finding the intersection point between: 1.) the
	 * line formed by the current location of the robot and the newly found
	 * door, and 2.) the line orthogonal to the gateway direction of the door.
	 * If this intersection point resides outside of the window created by the
	 * both door points and the current location / newly found door, then the
	 * newly found door can be considered to be attached to the room. Note that
	 * this algorithm assumes that the boundaries of the room are known. This
	 * assumption is made valid by the fact that doors are processed according
	 * to their distance away from the current robot's location, processing
	 * those doors which are closest to the robot first.
	 * 
	 * @param curPose
	 *            Current pose of the robot (x, y, t).
	 * @param doorLocation
	 *            Location of the found door which is in question for being
	 *            attached to the current room the robot is in.
	 * @param association
	 *            Integer of the old door that this new door has been assigned
	 *            to according to the data association algorithm.
	 * @param roomDoors
	 *            Array list of the known doors connected to the current room
	 *            the robot is in.
	 * @param graph
	 *            Pose graph from SLAM.
	 * @return Returns a boolean where true represents that the newly found door
	 *         does not violate the known boundaries of the room.
	 */
	public boolean doorConnects2Room(double[] curPose, double[] doorLocation,
			int association, ArrayList<Integer> roomDoors, Graph graph) {

		// run through all doors known to be connected to the room
		if (roomDoors == null)
			return true;

		for (int i = 0; i < roomDoors.size(); i++) {

			Door oldDoor = doors.get(roomDoors.get(i));

			// ensure that the ID of the association of the new door is not
			// already present in the room's door list
			if (oldDoor.doorNumber == association)
				continue;

			// intersection calculation using determinants of two points on each
			// line
			double[] oldDoorLoc = graph.nodes.get(oldDoor.graphNodeIndex).state;
			double xEdgeDirect = Math.cos(oldDoorLoc[2]);
			double yEdgeDirect = Math.sin(oldDoorLoc[2]);
			double[] doorEdge1 = new double[] { oldDoorLoc[0] - yEdgeDirect,
					oldDoorLoc[1] + xEdgeDirect };
			double[] doorEdge2 = new double[] { oldDoorLoc[0] + yEdgeDirect,
					oldDoorLoc[1] - xEdgeDirect };
			double[] intersect = intersection(curPose, doorLocation, doorEdge1,
					doorEdge2);

			// determine if the intersecting point falls between line segment of
			// current pose to new door location
			if (((intersect[0] > curPose[0]) && (intersect[0] < doorLocation[0]))
					|| ((intersect[0] < curPose[0]) && (intersect[0] > doorLocation[0]))) {
				return false;
			}
		}

		return true;
	}

	/**
	 * Simple intersection calculation of two lines, the first being created by
	 * connecting point 1 and point 2, and the other being created by connecting
	 * point 3 and point 4.
	 * 
	 * @param p1
	 *            First point of line created by connecting point 1 and point 2.
	 * @param p2
	 *            Second point of line created by connecting point 1 and point
	 *            2.
	 * @param p3
	 *            First point of line created by connecting point 3 and point 4.
	 * @param p4
	 *            Second point of line created by connecting point 3 and point
	 *            4.
	 * @return Intersection point of lines created by connecting point 1 and
	 *         point 2 as well as connecting point 3 and point 4.
	 */
	public double[] intersection(double[] p1, double[] p2, double[] p3,
			double[] p4) {

		double x1x2 = p1[0] - p2[0];
		double x3x4 = p3[0] - p4[0];
		double y1y2 = p1[1] - p2[1];
		double y3y4 = p3[1] - p4[1];
		double den = (x1x2 * y3y4) - (y1y2 * x3x4);
		if (den == 0) {
			return new double[] { Double.MAX_VALUE, Double.MAX_VALUE };
		}

		double a = (p1[0] * p2[1]) - (p1[1] * p2[0]);
		double b = (p3[0] * p4[1]) - (p3[1] * p4[0]);

		return new double[] { ((a * x3x4) - (x1x2 * b)) / den,
				((a * y3y4) - (y1y2 * b)) / den };
	}

	/**
	 * Determines whether or not two different poses are on opposite sides of a
	 * specified door.
	 * 
	 * @param pose1
	 *            First pose of robot.
	 * @param pose2
	 *            Second pose of robot.
	 * @param doorState
	 *            Door state, to include x, y location and orientation.
	 * @return Returns a boolean where true represents that the two poses are
	 *         indeed on opposite sides of the specified door.
	 */
	public boolean oppoSideDoor(double[] pose1, double[] pose2,
			double[] doorState) {

		// last pose and current pose locations WRT door's reference frame
		double[] firstSideDoor = LinAlg.xytInvMul31(doorState, pose1);
		double[] secondSideDoor = LinAlg.xytInvMul31(doorState, pose2);

		if ((firstSideDoor[0] > 0 && secondSideDoor[0] < 0)
				|| (firstSideDoor[0] < 0 && secondSideDoor[0] > 0))
			return true;

		return false;
	}

	/**
	 * Determines if a pose of the robot is within a window of a specified door.
	 * The window of the door extends from both edges of the door, and
	 * orthogonal in both directions from the center of the door the distance
	 * which poses are added to the SLAM pose graph (to ensure that when
	 * crossing a door, there will always be a pose added to the graph of either
	 * side of a door).
	 * 
	 * @param thisDoor
	 *            Door which the method attempts to determine if the pose is
	 *            within.
	 * @param graph
	 *            Pose graph from SLAM.
	 * @param pose
	 *            Pose which the method attempts to determine is within a
	 *            specified door's window.
	 * @param poseThresh
	 *            The distance that must be traveled prior to a new node being
	 *            created.
	 * @return Returns a boolean where true represents that the current pose is
	 *         indeed within the specified door.
	 */
	public boolean insideDoor(Door thisDoor, Graph graph, double[] pose,
			double poseThresh) {

		// get door, door's directionality, door's width
		GXYTNode doorNode = (GXYTNode) graph.nodes.get(thisDoor.graphNodeIndex);
		double doorDir = doorNode.state[2];
		double doorNormDir = MathUtil.mod2pi(doorDir + (Math.PI / 2));
		double doorWidth = (Double) doorNode.getAttribute("doorWidth");

		// calculate rectangular window points
		double dirXval1 = doorNode.state[0] + poseThresh * Math.cos(doorDir);
		double dirXval2 = doorNode.state[0] - poseThresh * Math.cos(doorDir);
		double norXval1 = doorNode.state[0] + (doorWidth / 2)
				* Math.cos(doorNormDir);
		double norXval2 = doorNode.state[0] - (doorWidth / 2)
				* Math.cos(doorNormDir);
		double[] xvals = new double[] { dirXval1, dirXval2, norXval1, norXval2 };

		// calculate y values of rectangular points
		double dirYval1 = doorNode.state[1] + poseThresh * Math.sin(doorDir);
		double dirYval2 = doorNode.state[1] - poseThresh * Math.sin(doorDir);
		double norYval1 = doorNode.state[1] + (doorWidth / 2)
				* Math.sin(doorNormDir);
		double norYval2 = doorNode.state[1] - (doorWidth / 2)
				* Math.sin(doorNormDir);
		double[] yvals = new double[] { dirYval1, dirYval2, norYval1, norYval2 };

		// determine if pose within the window
		if (pose[0] <= LinAlg.max(xvals) && pose[0] >= LinAlg.min(xvals)) {
			if (pose[1] <= LinAlg.max(yvals) && pose[1] >= LinAlg.min(yvals)) {
				return true;
			}
		}

		return false;
	}

	/**
	 * The following method determines if the robot has passed through a door
	 * connected to the current room the robot is within.
	 * 
	 * @param graph
	 *            Pose graph from SLAM.
	 * @param curPose
	 *            Current pose of the robot (x, y, t).
	 * @param poseThresh
	 *            The distance that must be traveled prior to a new node being
	 *            created. Use to determine the size of a door window when
	 *            calculating whether a robot is inside or a door or not.
	 * @return Boolean value where 'true' signifies that the robot has just
	 *         passed through a door, and false signifies otherwise.
	 */
	public boolean passedThroughDoor(Graph graph, double[] curPose,
			double poseThresh) {

		// if last pose was not inside of a door, check if current pose is by
		// running through each door known to be attached to this room
		if (!inDoor) {

			// grab doors known to be attached to this room
			ArrayList<Integer> doorsAttached = roomMap.get(currentRoom);

			// initialized new door for consideration
			Door doorCheck;

			// run through each door, check if robot has entered it's window
			for (Integer doorNumber : doorsAttached) {

				// grab considered door
				doorCheck = doors.get(doorNumber);

				// check if inside of this door
				boolean inDoorNow = insideDoor(doorCheck, graph, curPose,
						poseThresh);

				// if currently inside this door, update mapping variables
				if (inDoorNow) {

					// update last door number robot was inside of
					lastDoorInside = doorCheck.doorNumber;

					// update in door flag
					inDoor = true;

					// set this pose as being the pose prior to exiting a door
					// window
					priorPose = LinAlg.copy(curPose);

					// have not passed through a door, return false
					return false;
				}
			}

			// have not passed through a door and not currently in a door,
			// return false
			return false;
		}

		// last pose was in a door, check if we are still in that door
		if (!insideDoor(doors.get(lastDoorInside), graph, curPose, poseThresh)) {

			// no longer inside door, reset flag
			inDoor = false;

			// grab graph pose of the last door the robot was located inside of
			GXYTNode doorCross = (GXYTNode) graph.nodes.get(doors
					.get(lastDoorInside).graphNodeIndex);

			// return true if the current pose of the robot is on the opposite
			// side of the door that the last pose of the robot was within when
			// entering the door
			return oppoSideDoor(curPose, priorPose, doorCross.state);
		}

		// still inside of door window, have not passed through yet
		return false;
	}

}