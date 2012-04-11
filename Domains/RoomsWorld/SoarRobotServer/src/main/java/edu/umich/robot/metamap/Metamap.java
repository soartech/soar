/*
 * Copyright (c) 2011, Regents of the University of Michigan
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package edu.umich.robot.metamap;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import lcm.lcm.LCM;
import april.util.TimeUtil;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.util.concurrent.MoreExecutors;

import edu.umich.robot.Controller;
import edu.umich.robot.Robot;
import edu.umich.robot.events.DropObjectEvent;
import edu.umich.robot.events.ObjectAddedEvent;
import edu.umich.robot.events.PickUpObjectEvent;
import edu.umich.robot.events.RobotAddedEvent;
import edu.umich.robot.events.RobotRemovedEvent;
import edu.umich.robot.events.RoomLightEvent;
import edu.umich.robot.events.control.DoorCloseEvent;
import edu.umich.robot.events.control.DoorOpenEvent;
import edu.umich.robot.lcmtypes.map_metadata_t;
import edu.umich.robot.metamap.Door.State;
import edu.umich.robot.util.Pose;
import edu.umich.robot.util.RelativePose;
import edu.umich.robot.util.events.RobotEvent;
import edu.umich.robot.util.events.RobotEventListener;

/**
 * <p>
 * This is where the virtual map data is managed. There is a list of areas, a
 * virtual object manager, area states mapped by area id number, doors, initial
 * door states saved in case of a reset, robots on the map, and a few other
 * things.
 * 
 * <p>
 * All new map entities get assigned ids from the same id pool managed by this
 * glass.
 * 
 * <p>
 * As much as possible is immutable.
 * 
 * @author voigtjr@gmail.com
 */
public class Metamap implements RobotEventListener
{
    private static final double HALF_FOV = Math.PI / 2;

    private final List<AreaDescription> areaList;

    private final VirtualObjectManager objects;
    
    private final Map<Integer, AreaState> areaStates = Maps.newConcurrentMap();
    
    private final Map<Integer, Door> doors = Maps.newConcurrentMap();
    private final Map<Integer, Door> initialDoorState = Maps.newConcurrentMap();
    
    private final List<VirtualObject> robots = Lists.newArrayList();
    
    private final ScheduledExecutorService schexec = MoreExecutors.getExitingScheduledExecutorService(new ScheduledThreadPoolExecutor(1));
    
    private final String imagePath;
    private final double metersPerPixel;
    private final int[] origin;
   
    private final IdGenerator idg;

	private Controller controller;
    
    Metamap(String imagePath, double metersPerPixel, int[] origin, 
            List<AreaDescription> areaList, VirtualObjectManager objects, Map<Integer, Door> doors, IdGenerator idg)
    {
        this.idg = idg;
        this.imagePath = imagePath;
        this.metersPerPixel = metersPerPixel;
        this.origin = Arrays.copyOf(origin, origin.length);
        
        this.areaList = areaList;
        
//        StringBuilder builder = AreaDescriptions.render(null, areaList);
//        System.out.println(builder.toString());
//        List<AbridgedAreaDescription> abad = AreaDescriptions.parseAreas(builder.toString());
//        System.out.println(abad.toString());
        
        this.objects = objects;
        this.doors.putAll(doors);

        for (Door door : doors.values()) {
            initialDoorState.put(door.getId(), door.copy());
        }
        broadcastDoors();
        
        final map_metadata_t meta = new map_metadata_t();
        meta.nareas = areaList.size();
        meta.area_ids = new int[meta.nareas];
        meta.areas = new double[areaList.size()][4];
        List<Gateway> gatewayList = Lists.newArrayList();
        for (int i = 0; i < areaList.size(); ++i)
        {
            AreaDescription ad = areaList.get(i);
            meta.areas[i][0] = ad.getPose().getPos(0);
            meta.areas[i][1] = ad.getPose().getPos(1);
            meta.areas[i][2] = ad.getPose().getVel().get(0);
            meta.areas[i][3] = ad.getPose().getVel().get(1);
            gatewayList.addAll(ad.getGateways());
            meta.area_ids[i] = ad.getId();
        }
        
        meta.ngateways = gatewayList.size();
        meta.gateway_ids = new int[gatewayList.size()];
        meta.gateways = new double[gatewayList.size()][2];
        for (int i = 0; i < gatewayList.size(); ++i)
        {
            meta.gateway_ids[i] = gatewayList.get(i).getId();
            meta.gateways[i][0] = gatewayList.get(i).getPose().getPos(0);
            meta.gateways[i][1] = gatewayList.get(i).getPose().getPos(1);
        }

        schexec.scheduleAtFixedRate(new Runnable()
        {
            final LCM lcm = LCM.getSingleton();
            
            public void run()
            {
                meta.utime = TimeUtil.utime();
                lcm.publish("AREA_DESCRIPTIONS", meta);
            }
        }, 0, 1, TimeUnit.SECONDS);
    }

    /**
     * Doors are implemented as obstacles in the simulator, the same way the
     * other objects are implemented. To create these obstacles, they must be
     * added to the broadcaster so the dimensions go out over LCM. This handles
     * that.
     */
    private void broadcastDoors()
    {
        for (Door door : doors.values())
            if (door.getState() != State.OPEN)
                objects.getBroadcaster().addDoor(door);
    }

    /**
     * Get an area description from an id number, or null if none.
     * 
     * @param index
     * @return
     */
    public AreaDescription getAreaFromIndex(int index)
    {
        if (index >= areaList.size() || index < 0)
            return null;
        return areaList.get(index);
    }
    
    /**
     * Get an area description from a pose. Returns the area description of the
     * area that the passed pose falls in to.
     * 
     * @param pose
     * @return
     */
    public AreaDescription getArea(Pose pose)
    {
        // TODO optimize
        for (AreaDescription area : areaList)
        {
            Pose ap = area.getPose();
            if (pose.getX() < ap.getX())
                continue;
            if (pose.getY() < ap.getY())
                continue;
            if (pose.getX() >= ap.getX() + ap.getVX())
                continue;
            if (pose.getY() >= ap.getY() + ap.getVY())
                continue;
            return area;
        }
        return null;
    }
    
    /**
     * Get a list of all known areas, immutable.
     * 
     * @return
     */
    public List<AreaDescription> getAreaList() {
    	return areaList;
    }

    /**
     * Get area state by id.
     * 
     * @param id
     * @return
     */
    public AreaState getAreaState(int id)
    {
        AreaState ret = areaStates.get(id);
        if (ret == null)
        {
            ret = new AreaState();
            areaStates.put(id, ret);
        }
        return ret;
    }

    /**
     * Change the state of a room light by area id.
     * 
     * @param id
     * @param on
     */
    public void setRoomLight(int id, boolean on)
    {
        AreaState as = areaStates.get(id);
        if (as == null)
            return;
        as.setRoomLightOn(on);
        controller.getEventManager().fireEvent(new RoomLightEvent(id, on));
    }
    
    /**
     * Get an area id from a pose.
     * 
     * @param pose
     * @return
     */
    private int getAreaId(Pose pose)
    {
        AreaDescription area = getArea(pose);
        if (area == null)
            return -1;
        return area.getId();
    }

    /**
     * Get all visible objects from the passed robot's perspective.
     * 
     * @param robot
     * @return
     */
    public List<VirtualObject> getVisibleObjects(Robot robot)
    {
        AreaDescription area = getArea(robot.getOutput().getPose());
        if (area == null)
            return Lists.newArrayListWithCapacity(0);
        
        List<VirtualObjectImpl> placed = objects.getObjects(area.getId());
        AreaDescription ra = getArea(robot.getOutput().getPose());
        AreaState rs = getAreaState(ra.getId());
        
        List<VirtualObject> ret = Lists.newArrayList();

        // TODO night
        if (!rs.isLit(true) && !robot.getOutput().isHeadlightOn())
            return ret;
        
        // TODO optimize
        for (VirtualObject vo : placed)
        {
            if (isVisible(robot.getOutput().getPose(), vo.getPose()))
                ret.add(vo);
        }

        for (VirtualObject vo : robots)
        {
            if (isVisible(robot.getOutput().getPose(), vo.getPose()))
                ret.add(vo);
        }
        
        return ret;
    }
    
    /**
     * Find out if myPose can see otherPose using 180 degree fov. TODO: use
     * virtual object fov instead of hard-coded.
     * 
     * @param myPose source, viewer
     * @param otherPose target, viewed
     * @return
     */
    private boolean isVisible(Pose myPose, Pose otherPose)
    {
        int myAreaId = getAreaId(myPose);
        int otherAreaId = getAreaId(otherPose);
        if (otherAreaId == myAreaId)
        {
            RelativePose rp = myPose.getRelative(otherPose);
            // ignore self
            if (Double.compare(rp.getDistance(), Double.valueOf(0)) == 0)
                    return false;
            double ayaw = Math.abs(rp.getRelativeYaw());
            return HALF_FOV >= ayaw;
        }
        return false;
    }

    /**
     * Get the object carried by the passed robot.
     * 
     * @param robot
     * @return
     */
    public VirtualObject getCarried(Robot robot)
    {
        return objects.getCarried(robot);
    }

    /**
     * Someday, this will cancel an effector command issued to a robot.
     * 
     * @param robot
     * @return
     */
    public boolean cancelEffector(Robot robot)
    {
        // nothing to do, everything happens instantaneously right now
        return true;
    }

    /**
     * Have a robot pick up an object by id.
     * 
     * @param robot
     * @param id
     * @return
     */
    public boolean pickupObject(Robot robot, int id)
    {
        boolean ret = objects.pickupObject(robot, id);
		if (ret) {
			controller.getEventManager().fireEvent(new PickUpObjectEvent(robot, id));
		}
		return ret;
    }

    /**
     * Have a robot drop its currently held object.
     * 
     * @param robot
     * @return
     */
    public boolean dropObject(Robot robot)
    {
        boolean ret = objects.dropObject(robot);
		if (ret) {
			controller.getEventManager().fireEvent(new DropObjectEvent(robot));
		}
		return ret;
    }

    /**
     * Task dependent effector command.
     * 
     * @param robot
     * @param id
     * @return
     */
    public boolean diffuseObject(Robot robot, int id)
    {
        return objects.diffuseObject(robot, id);
    }

    /**
     * Task dependent effector command.
     * 
     * @param robot
     * @param id
     * @param color
     * @return
     */
    public boolean diffuseObjectByWire(Robot robot, int id, String color)
    {
        return objects.diffuseObjectByWire(robot, id, color);
    }

    /**
     * Open a door by id. Transitions from CLOSED to OPEN
     * 
     * @param robot
     * @param id
     */
    public void doorOpen(Robot robot, int id)
    {
        Door door = doors.get(id);
        if (door == null)
            return;

        switch (door.getState())
        {
        case CLOSED:
            door.setState(State.OPEN);
            objects.getBroadcaster().removeDoor(door);
			controller.getEventManager().fireEvent(new DoorOpenEvent(id));
            break;
            
        case LOCKED:
        case OPEN:
            break;
        }
    }

    /**
     * Close a door by id. Transitions from OPEN to CLOSED
     * 
     * @param robot
     * @param id
     */
    public void doorClose(Robot robot, int id)
    {
        Door door = doors.get(id);
        if (door == null)
            return;

        switch (door.getState())
        {
        case OPEN:
            door.setState(State.CLOSED);
            objects.getBroadcaster().addDoor(door);
			controller.getEventManager().fireEvent(new DoorCloseEvent(id));
            break;
            
        case LOCKED:
        case CLOSED:
            break;
        }
    }

    /**
     * Unlock a door by id and lock code. Transitions from LOCKED to CLOSED
     * 
     * @param robot
     * @param id
     * @param code
     */
    public void doorUnlock(Robot robot, int id, int code)
    {
        Door door = doors.get(id);
        if (door == null)
            return;

        switch (door.getState())
        {
        case LOCKED:
            if (door.getCode() == code)
                door.setState(State.CLOSED);
            break;
            
        case OPEN:
        case CLOSED:
            break;
        }
    }

    /**
     * Lock a door and set a lock code. Transitions from CLOSED to LOCKED
     * 
     * @param robot
     * @param id
     * @param code
     */
    public void doorLock(Robot robot, int id, int code)
    {
        Door door = doors.get(id);
        if (door == null)
            return;

        switch (door.getState())
        {
        case CLOSED:
            door.setCode(code);
            door.setState(State.LOCKED);
            break;
            
        case OPEN:
        case LOCKED:
            break;
        }
    }

    /**
     * Get a list of object names, kind of like classes of objects.
     * 
     * @return
     */
    public List<String> getObjectNames()
    {
        return objects.getObjectNames();
    }

    /**
     * Add an object by object name, like a class name, from the set returned by getObjectNames
     * 
     * @param name
     * @param pos
     * @return
     */
    public VirtualObject addObject(String name, double[] pos)
    {
        VirtualObject obj = objects.placeNew(name, new Pose(pos));
        if (obj != null) {
            controller.getEventManager().fireEvent(new ObjectAddedEvent(obj));
        }
        return obj;
    }

    public void onEvent(RobotEvent event)
    {
        if (event instanceof RobotAddedEvent)
        {
            RobotAddedEvent e = (RobotAddedEvent)event;
            
            robots.add(new VirtualObjectRobot(e.getRobot(), idg.getId()));
        }
        else if (event instanceof RobotRemovedEvent)
        {
            RobotAddedEvent e = (RobotAddedEvent)event;
            
            for (Iterator<VirtualObject> iter = robots.iterator(); iter.hasNext();)
            {
                VirtualObject o = iter.next();
                if (o.getProperties().get("name").equals(e.getRobot().getName()))
                {
                    iter.remove();
                    break;
                }
            }
        }
    }
    
    /**
     * Restore state to initial state.
     */
    public void reset()
    {
        areaStates.clear();
        objects.reset();

        for (Door door : doors.values())
        {
            Door other = initialDoorState.get(door.getId());
            door.setState(other.getState());
            if (other.getState() == Door.State.LOCKED)
                door.setCode(other.getCode());
        }
        
        broadcastDoors();
    }

    /**
     * Get a virtual object prototype by name.
     * 
     * @param name
     * @return
     */
    public VirtualObject getTemplate(String name)
    {
        return objects.getTemplate(name);
    }
    
    /**
     * Get all virtual object prototypes.
     * 
     * @return
     */
    public Collection<VirtualObjectTemplate> getTemplates() {
    	return objects.getTemplates();
    }

    /**
     * Get all objects placed on the map. This doesn't include objects carried
     * by robots.
     * 
     * @return
     */
    public List<VirtualObject> getPlacedObjects()
    {
        return objects.getPlacedObjects();
    }

    /**
     * Get the path to the image used to generate the wall data.
     * 
     * @return
     */
    public String getImagePath()
    {
        return imagePath;
    }

    /**
     * Get the scale converting the image data to map data. Meters per pixel.
     * 
     * @return
     */
    public double getMetersPerPixel()
    {
        return metersPerPixel;
    }

    /**
     * Get what pixel represents the map origin.
     * 
     * @return
     */
    public int[] getImageOrigin()
    {
        return Arrays.copyOf(origin, origin.length);
    }
    
    /**
     * Shutdown and invalidate everything, kill threads.
     */
    public void shutdown()
    {
        schexec.shutdown();
        objects.shutdown();
    }

	public void setController(Controller controller) {
		this.controller = controller;
	}
}
