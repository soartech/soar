package edu.umich.robot.events;

import edu.umich.robot.Robot;

public class PickUpObjectEvent implements AbstractProgramEvent
{
	private Robot robot;
	private int id; // the id of the object being picked up.
	public PickUpObjectEvent(Robot robot, int id) {
		this.robot = robot;
		this.id = id;
	}
    
    public Robot getRobot() {
        return robot;
    }
    
    public int getID() {
    	return id;
    }
    
}
