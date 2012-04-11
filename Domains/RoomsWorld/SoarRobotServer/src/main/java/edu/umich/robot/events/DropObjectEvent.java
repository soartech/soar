package edu.umich.robot.events;

import edu.umich.robot.Robot;

public class DropObjectEvent implements AbstractProgramEvent
{
	private Robot robot;

	public DropObjectEvent(Robot robot) {
		this.robot = robot;
	}
    
    public Robot getRobot() {
        return robot;
    }
    
}
