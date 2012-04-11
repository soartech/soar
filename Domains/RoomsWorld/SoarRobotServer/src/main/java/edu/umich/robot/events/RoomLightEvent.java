package edu.umich.robot.events;

import edu.umich.robot.Robot;

public class RoomLightEvent implements AbstractProgramEvent
{
	private int id;
	private boolean on;

	public RoomLightEvent(int id, boolean on) {
		this.id = id;
		this.on = on;
	}
    
    public boolean isOn() {
        return on;
    }
    
    public int getID() {
    	return id;
    }
    
}
