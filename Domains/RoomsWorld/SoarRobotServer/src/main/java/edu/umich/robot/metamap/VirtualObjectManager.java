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

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import april.config.Config;
import april.jmat.LinAlg;
import april.lcmtypes.pose_t;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

import edu.umich.robot.Robot;
import edu.umich.robot.util.Pose;

/**
 * Manages virtual objects. Maintains what objects exist both placed and not
 * placed, manages object manipulation qualification, manages
 * templates/prototypes, creates new objects, moves objects, keeps track of
 * carried objects, updates state for broadcasting objects on LCM.
 * 
 * @author voigtjr@gmail.com
 */
class VirtualObjectManager
{
    private static final Log logger = LogFactory.getLog(VirtualObjectManager.class);

    private static final double MANIPULATION_DISTANCE = 1; // TODO pull from properties?
    
    private static final double SQUARED_MANIPULATION_DISTANCE = LinAlg.sq(MANIPULATION_DISTANCE);

    private final Map<String, VirtualObjectTemplate> templates = Maps.newHashMap();

    private final Map<Integer, VirtualObjectImpl> instances = Maps.newHashMap();
    
    private final List<VirtualObjectImpl> placed = Lists.newArrayList();
    
    private final Map<Robot, VirtualObjectImpl> carried = Maps.newHashMap();
    
    private final ObstacleBroadcaster obc = new ObstacleBroadcaster();
    
    private final Config config;
    
    private final IdGenerator idg;
    
    VirtualObjectManager(Config config, IdGenerator idg)
    {
        this.idg = idg;
        
        // april_voigt's implementation of config had a copy() method -- hope this doesn't break anything
        // this.config = (config != null) ? config.copy() : null;
        this.config = (config != null) ? config : null;
        
        initialize();
    }
    
    /**
     * Places initial object instances on the map.
     */
    private void initialize()
    {
        String[] placedString = null;
        for (String key : config.getKeys())
        {
            if (key.equals("placed"))
            {
                placedString = config.requireStrings("placed");
                continue;
            }
            
            String objectNickname = key.split("\\.", 2)[0];
            if (templates.containsKey(objectNickname))
                continue;
            addTemplate(objectNickname, config.getChild(objectNickname));
        }
        
        placeInstances(placedString);
    }
    
    /**
     * Add a new template from a configuration file.
     * 
     * @param name
     * @param config
     */
    private void addTemplate(String name, Config config)
    {
        templates.put(name, new VirtualObjectTemplate(name, config));
    }
    
    /**
     * Get all object class names, not instances.
     * 
     * @return
     */
    List<String> getObjectNames()
    {
        return Lists.newArrayList(templates.keySet());
    }
    
    /**
     * Place instances as formatted in the configuration file.
     * 
     * @param placedString
     */
    private void placeInstances(String[] placedString)
    {
        if (placedString == null)
            return;
        
        try
        {
            for (int i = 0; i < placedString.length - 2; i += 3)
            {
                Pose pose = new Pose(new double[] { 
                        Double.parseDouble(placedString[i + 1]), 
                        Double.parseDouble(placedString[i + 2]) });
                placeNew(placedString[i], pose);
            }
        } 
        catch (NumberFormatException e)
        {
            logger.error("Number format exception placing object.");
        }
    }
    
    /**
     * Place a new object by class name at pose.
     * 
     * @param name Object class name to place.
     * @param pose Where to place the object.
     * @return
     */
    VirtualObject placeNew(String name, Pose pose)
    {
        VirtualObjectTemplate template = templates.get(name);
        if (template == null)
        {
            logger.error("No template for " + name);
            return null;
        }
        
        VirtualObjectImpl vo = new VirtualObjectImpl(name, template, idg.getId());
        vo.setPose(pose);
        
        instances.put(vo.getId(), vo);
        addPlaced(vo);
        return vo;
    }
    
    /**
     * Set an object on the map.
     * 
     * @param voi
     */
    private void addPlaced(VirtualObjectImpl voi)
    {
        logger.debug("Placed " + voi.toString());

        placed.add(voi);
        if (obc != null)
            obc.addVirtualObject(voi);
    }
    
    /**
     * Remove an object from the map.
     * 
     * @param voi
     */
    private void removePlaced(VirtualObjectImpl voi)
    {
        placed.remove(voi);
        if (obc != null)
            obc.removeVirtualObject(voi);
    }
    
    /**
     * Get all objects in an area.
     * 
     * @param area
     * @return
     */
    List<VirtualObjectImpl> getObjects(int area)
    {
        return Collections.unmodifiableList(placed);
    }
    
    /**
     * Check to see if an object is in manipulation range of a robot. I don't
     * think this uses the robot's manip distance set in its config, it should.
     * 
     * @param robot
     * @param object
     * @return
     */
    private boolean isInRange(Robot robot, VirtualObject object)
    {
        pose_t rp = robot.getOutput().getPose().asLcmType();
        pose_t vp = object.getPose().asLcmType();
        
        double squaredDistance = LinAlg.squaredDistance(rp.pos, vp.pos);
        
        // decrease the distance by object's radius
        // estimating radius by largest side
        double diameter = Math.max(object.getSize().get(0), object.getSize().get(1));
        
        return (squaredDistance - LinAlg.sq(diameter / 2)) <= SQUARED_MANIPULATION_DISTANCE;
    }

    /**
     * Pick up an object by id, give it to the robot. Fails if the robot is
     * carrying something already.
     * 
     * @param robot
     * @param id
     * @return
     */
    boolean pickupObject(Robot robot, int id)
    {
        if (!carried.containsKey(robot))
        {
            VirtualObjectImpl voi = instances.get(Integer.valueOf(id));
            if (voi != null && placed.contains(voi))
            {
                if (isInRange(robot, voi))
                {
                    removePlaced(voi);
                    carried.put(robot, voi);
                    return true;
                }
            }
        }
        
        return false;   
    }

    /**
     * Drops a carried object at manip distance in front of the robot.
     * 
     * @param robot
     * @return
     */
    boolean dropObject(Robot robot)
    {
        // TODO placement restrictions
        
        VirtualObjectImpl voi = carried.get(robot);
        if (voi != null)
        {
            pose_t rp = robot.getOutput().getPose().asLcmType();
            pose_t vp = voi.getPose().asLcmType();
            double[] dpos = LinAlg.quatRotate(rp.orientation, new double[] { MANIPULATION_DISTANCE, 0, 0 });
            vp.pos = LinAlg.add(rp.pos, dpos);
            voi.setPose(new Pose(vp));
            addPlaced(voi);
            carried.remove(robot);
            return true;
        }
        
        return false;
    }

    /**
     * Diffuses an object in front of a robot.
     * 
     * @param robot
     * @param id
     * @return
     */
    boolean diffuseObject(Robot robot, int id)
    {
        VirtualObject vo = instances.get(id);
        if (vo != null && placed.contains(vo) && isInRange(robot, vo))
        {
            VirtualObjectImpl voi = (VirtualObjectImpl) vo;
            if (voi.isDiffusable())
                voi.setDiffused(true);
        }
        return false;
    }

    /**
     * Diffuses an object.
     * 
     * @param robot
     * @param id
     * @param color
     * @return
     */
    boolean diffuseObjectByWire(Robot robot, int id, String color)
    {
        VirtualObject vo = instances.get(id);
        if (vo != null && placed.contains(vo) && isInRange(robot, vo))
        {
            VirtualObjectImpl voi = (VirtualObjectImpl)vo;
            if (voi.isDiffusableByColor(color))
                voi.setDiffused(true);
        }
        return false;
    }

    /**
     * Get the object carried by a robot.
     * 
     * @param robot
     * @return
     */
    VirtualObject getCarried(Robot robot)
    {
        return carried.get(robot);
    }

    /**
     * Reset all state.
     */
    void reset()
    {
        obc.reset();
        templates.clear();
        placed.clear();
        carried.clear();
        instances.clear();
        idg.rewind();
        initialize();
    }

    /**
     * Returns the object responsible for broadcasting obstacles over lcm.
     * 
     * @return
     */
    ObstacleBroadcaster getBroadcaster()
    {
        return obc;
    }

    /**
     * Get the template object by its name (class name, not java class though).
     * 
     * @param name
     * @return
     */
    public VirtualObject getTemplate(String name)
    {
        return templates.get(name);
    }
    
    /**
     * Get all templates/prototypes.
     * 
     * @return
     */
    public Collection<VirtualObjectTemplate> getTemplates() {
    	return templates.values();
    }

    /**
     * Get all placed objects, objects on the map.
     * 
     * @return
     */
    public List<VirtualObject> getPlacedObjects()
    {
        ImmutableList.Builder<VirtualObject> b = new ImmutableList.Builder<VirtualObject>();
        b.addAll(placed);
        return b.build();
    }
    
    /**
     * Shutdown broadcaster.
     */
    public void shutdown()
    {
        obc.shutdown();
    }
}
