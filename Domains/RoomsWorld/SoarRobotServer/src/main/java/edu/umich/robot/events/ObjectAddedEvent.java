package edu.umich.robot.events;

import edu.umich.robot.metamap.VirtualObject;

public class ObjectAddedEvent implements AbstractProgramEvent
{
    private final VirtualObject obj;
    public ObjectAddedEvent(VirtualObject obj) {
        this.obj = obj;
    }
    
    public VirtualObject getObject() {
        return obj;
    }
    
}
