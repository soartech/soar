package laserloc;

import java.awt.Point;
import java.util.Arrays;

import lcmtypes.*;

public class LaserLoc extends Thread 
{
	
	static
	{
		initializeLaserData();
	}

	// Assumptions:
		// robot_z is constant
		// robot starts at a known angle
		// this level only reports laser data (next layer up decided whether to use odometry or not)
		// distances in meters
	
	// Constants:
	public static double robot_starting_yaw = 0;
	public static double laser_x = 0; // if we assume the laser is at the origin facing up the y-axis, the next 3 constants are all 0
	public static double laser_y = 0;
	public static double laser_yaw_adjust = 0; // amount to adjust for laser's yaw = 90 - laser's yaw = 0 if laser is facing positive y directly
	public static double laser_dist_adjustment = 0; // radius of tube?
	public static double translation_threshold = 0.05; // minimum translation distance to update x,y location, 0.05 total guess

	// Even though this comes in each message, it may help the compiler optimize 
	// if we make it a separate constant (can assert on it not matching the message)
	public static double laser_delta_angle = 0; // angle between laser indexes. 
	// end constants
	
	// regular state
	pose_t old_estimated_pose = new pose_t();
	pose_t new_estimated_pose = new pose_t();
	// TEMPORARY: TODO: remove, incorporate in to new_estimated_pose
	double estimated_pose_yaw = 0;	// no old estimated yaw, we don't use the old one to calculate new 

	boolean running = true;
	
	public LaserLoc( /* config params? */ )
	{
		// ?? initialize?
	}

	
	public void run()
	{
//		UpdatePose:
//			robot_yaw = robot_starting_yaw
		estimated_pose_yaw = robot_starting_yaw;
		
//			[old_robot_x, old_robot_y] = GetRobotXY()
		old_estimated_pose = getRobotXY( getLaserData() );
		System.out.print( "initial: " );
		printOldPose();
		
		while ( running )
		{
//			loop forever (or until shutdown)
//			[new_robot_x, new_robot_y] = GetRobotXY()
			new_estimated_pose = getRobotXY( getLaserData() );
			if ( new_estimated_pose == null )
			{
				// TODO: possibly sleep?
				continue;
			}
			
//			translation_dist = distance(old_robot_x, old_robot_y, new_robot_x, new_robot_y)
			double translation_dist = Point.distance( old_estimated_pose.pos[ 0 ], old_estimated_pose.pos[ 1 ], new_estimated_pose.pos[ 0 ], new_estimated_pose.pos[ 1 ] );
			System.out.print( "Moved " + translation_dist + " meters. " );
			
//			// only update location if moved enough. Don't want Soar to thrash on constantly changing x,y due to noise
//			// this also means this loop can run as fast as it can and we'll still get reasonable updates
//			// (i.e., we don't have to worry about robot not moving enough between updates)
			if( translation_dist >= translation_threshold )
			{
//				robot_yaw = tan-1 (new_robot_y - old_robot_y)/(new_robot_x - old_robot_x)
				estimated_pose_yaw = Math.atan2( new_estimated_pose.pos[ 1 ] - old_estimated_pose.pos[ 1 ], new_estimated_pose.pos[ 0 ] - old_estimated_pose.pos[ 0 ] );
//				old_robot_x = new_robot_x
//				old_robot_y = new_robot_y
				
				// TODO: figure out best way to clone
				 old_estimated_pose.pos[ 0 ] =  new_estimated_pose.pos[ 0 ];
				 old_estimated_pose.pos[ 1 ] =  new_estimated_pose.pos[ 1 ];
				 
//				generate lcm message
				 // convert yaw to pose structure
				 // FIXME: lcm publish new_estimated_pose
				 printOldPose();
			}
			else
			{
				System.out.println( "Skipping update (beneath threshold)" );
			}
		}
		System.out.println( "LaserLoc done" );
	}

	private void printOldPose() {
		System.out.println( "x,y,a: " + old_estimated_pose.pos[ 0 ] + "," + old_estimated_pose.pos[ 1 ] + "," + Math.toDegrees( estimated_pose_yaw ) );
	}

	//	laser_data = reading from sick
	// FIXME: lcm.receive laser data
	public pose_t getRobotXY( laser_t laser_data )
	{
		if ( laser_data == null )
		{
			return null;
		}
			
//		[laser_index, laser_dist] = min distance from data
		double smallest_range = Double.MAX_VALUE;
		int smallest_range_index = -1;
		for ( int index = 0; index < laser_data.nranges; ++index )
		{
			if ( laser_data.ranges[ index ] < smallest_range )
			{
				smallest_range = laser_data.ranges[ index ];
				smallest_range_index = index;
			}
		}
		assert smallest_range_index != -1;
		
//		laser_angle = laser_yaw_adjust + laser_base_angle + laser_delta_angle*laser_index
		double laser_angle = laser_yaw_adjust + laser_data.rad0 + laser_data.radstep * smallest_range_index;
		
//		laser_dist = laser_dist + laser_dist_adjustment
		double laser_dist = smallest_range + laser_dist_adjustment;
		
		pose_t new_pose = new pose_t();
//		robot_x = laser_x + laser_dist * cos(laser_angle)
		new_pose.pos[ 0 ] = laser_x + laser_dist * Math.cos( laser_angle );

//		robot_y = laser_y + laser_dist * sin(laser_angle)
		new_pose.pos[ 1 ] = laser_y + laser_dist * Math.sin( laser_angle );
		
		return new_pose;
		
	}

	static laser_t[][] test_data;
	static int current_test_data_index = 0; 
	static int current_test_index = 0; 
	public static void initializeLaserData()
	{
		test_data = new laser_t[2][];
		for ( int test_index = 0; test_index < test_data.length; ++test_index )
		{
			switch ( test_index )
			{
			case 0:
				initializeTestData0();
				break;
				
			case 1:
				initializeTestData1();
				break;
			default:
				assert false;
			}
		}
	}
	
	private static void initializeTestData0() {
		final int THIS_TEST = 0;
		
		float rad0 = (float)Math.PI / -2;
		float radstep = (float)Math.PI / 180; // one degree
		float max = 10;
		long time = -1;
		int nranges = 180;
		
		float start_range = 1;

		test_data[ THIS_TEST ] = new laser_t[ 100 ]; 
		for ( int index = 0; index < test_data[ THIS_TEST ].length; ++index )
		{
			laser_t data = new laser_t();
			data.rad0 = rad0;
			data.radstep = radstep;
			data.utime = ++time;
			data.nranges = nranges;
			data.ranges = new float[ nranges ];
			Arrays.fill( data.ranges, max );

			data.ranges[ 20 ] = start_range + 0.01f * index;
			
			test_data[ THIS_TEST ][ index ] = data;
		}
	}

	private static void initializeTestData1() {
		final int THIS_TEST = 1;

		float rad0 = (float)Math.PI / -2;
		float radstep = (float)Math.PI / 180; // one degree
		float max = 10;
		long time = -1;
		int nranges = 180;

		int start_range_index = 45;
		float data_range = 1;
		
		test_data[ THIS_TEST ] = new laser_t[ 90 ]; 
		for ( int index = 0; index < test_data[ THIS_TEST ].length; ++index )
		{
			laser_t data = new laser_t();
			data.rad0 = rad0;
			data.radstep = radstep;
			data.utime = ++time;
			data.nranges = nranges;
			data.ranges = new float[ nranges ];
			Arrays.fill( data.ranges, max );

			data.ranges[ start_range_index + index ] = data_range;
			
			test_data[ THIS_TEST ][ index ] = data;
		}
	}

	public laser_t getLaserData() 
	{
		if ( current_test_data_index >= test_data[ current_test_index ].length )
		{
			running = false;
			return null;
		}
		
		System.out.print( "Test data " + current_test_data_index + ": ");
		return test_data[ current_test_index ][ current_test_data_index++ ];
	}

	public static void main(String[] args) 
	{
		// TODO: configure:
		//  robot_starting_yaw

		// TODO: currently defaulting to test mode, should have some kind of switch
		LaserLoc lloc;
		while ( current_test_index < test_data.length )
		{
			current_test_data_index = 0;
			System.out.println( "Starting test " + ( current_test_index + 1 ) );
			lloc = new LaserLoc();
			lloc.start();
			
			while ( lloc.isAlive() )
			{
				try 
				{
					Thread.sleep( 500 );
				} 
				catch ( InterruptedException ignored ) 
				{}
			}
			++current_test_index;
		}
		System.out.println( "All tests done." );
	}

}





















