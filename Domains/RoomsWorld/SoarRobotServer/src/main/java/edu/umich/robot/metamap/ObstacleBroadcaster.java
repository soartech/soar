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

import java.awt.Color;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

import lcm.lcm.LCM;
import april.util.TimeUtil;

import com.google.common.collect.Maps;
import com.google.common.util.concurrent.MoreExecutors;

import edu.umich.robot.lcmtypes.map_objects_t;
import edu.umich.robot.lcmtypes.sim_obstacles_t;
import edu.umich.robot.util.Pose;

/**
 * <p>
 * This broadcasts obstacles that the simulator listens for and uses for
 * collisions and virtual laser reports. We use this for virtual objects and for
 * doors.
 * 
 * <p>
 * It is assumed that the obstacles here will not move once placed because the
 * current use case complies with this. To move an obstacle, remove and re-add
 * it. Could be changed to support moving obstacles but it would have to update
 * the list.
 * 
 * <p>
 * For the sim_obstacles message, the colors of the objects are encoded in the
 * circles field of the message. This is a hack.
 * 
 * @author voigtjr@gmail.com
 */
class ObstacleBroadcaster
{
    private final LCM lcm = LCM.getSingleton();

    private final ScheduledExecutorService schexec = MoreExecutors.getExitingScheduledExecutorService(new ScheduledThreadPoolExecutor(1));

    /**
     * Maps virtual objects and doors to their relevant data for the obstacle
     * broadcast.
     */
    private final Map<Object, Obstacle> obstacles = Maps.newConcurrentMap();
    
    /**
     * The object id, type, rectangle and color.
     * 
     * @author voigtjr
     *
     */
    private static interface Obstacle
    {
        int getId();
        int getType();
        double[] getRect();
        double[] getColorHack();
    }
    
    private static class ObstacleVO implements Obstacle
    {
        private final VirtualObject vo;
        
        /**
         * Virtual object represented as an obstacle.
         * 
         * @param vo
         */
        ObstacleVO(VirtualObject vo)
        {
            this.vo = vo;
        }
        
        public int getId()
        {
            return vo.getId();
        }
        
        public int getType()
        {
            return 1; // TODO
        }
        
        public double[] getRect()
        {
            Pose p = vo.getPose();
            List<Double> s = vo.getSize();
            return new double[] {
                    p.getX(),
                    p.getY(),
                    s.get(0),
                    s.get(1),
                    p.getYaw(),
            };
        }
        
        public double[] getColorHack()
        {
            Color c = Color.gray;
            String color = vo.getProperties().get("color");
            if (color != null)
            {
                if (color.equalsIgnoreCase("black"))
                    c = Color.black;
                else if (color.equalsIgnoreCase("blue"))
                    c = Color.blue;
                else if (color.equalsIgnoreCase("cyan"))
                    c = Color.cyan;
                else if (color.equalsIgnoreCase("cyan"))
                    c = Color.darkGray;
                else if (color.equalsIgnoreCase("gray"))
                    c = Color.gray;
                else if (color.equalsIgnoreCase("green"))
                    c = Color.green;
                else if (color.equalsIgnoreCase("lightGray"))
                    c = Color.lightGray;
                else if (color.equalsIgnoreCase("magenta"))
                    c = Color.magenta;
                else if (color.equalsIgnoreCase("orange"))
                    c = Color.orange;
                else if (color.equalsIgnoreCase("pink"))
                    c = Color.pink;
                else if (color.equalsIgnoreCase("red"))
                    c = Color.red;
                else if (color.equalsIgnoreCase("white"))
                    c = Color.white;
                else if (color.equalsIgnoreCase("yellow"))
                    c = Color.yellow;
            }
            return new double[] { c.getRed(), c.getGreen(), c.getBlue() };
        }
    }
    
    /**
     * Door represented as an obstacle.
     * 
     * @author voigtjr
     *
     */
    private static class ObstacleD implements Obstacle
    {
        private int id;
        private final double[] xywh;
        
        public ObstacleD(Door door)
        {
            id = (short)door.getId();
            this.xywh = new double[] {
                    door.getxywh().get(0) + door.getxywh().get(2) * 0.5,
                    door.getxywh().get(1) + door.getxywh().get(3) * 0.5,
                    door.getxywh().get(2),
                    door.getxywh().get(3),
                    0
            };
        }
        
        public int getId()
        {
            return id;
        }
        
        public int getType()
        {
            return 0;
        }
        
        public double[] getRect()
        {
            return xywh;
        }
        
        public double[] getColorHack()
        {
            return new double[] { 0, 0, 0 };
        }
    }
    
    /**
     * The obstacles broadcasted, cached.
     */
    private final sim_obstacles_t obs = new sim_obstacles_t();

    /**
     * Map objects broadcasted, cached.
     */
    private final map_objects_t objs = new map_objects_t();
    
    /**
     * Set to true when those lists need to be recached.
     */
    private boolean changed = true;
    
    /**
     * Protects the obstacle list from changes during updates.
     */
    private final ReentrantLock obstacleLock = new ReentrantLock();
    
    ObstacleBroadcaster()
    {
        obs.generation = 0;
        schexec.scheduleAtFixedRate(new Runnable()
        {
            public void run()
            {
                boolean c = false;
                int n = -1;
                Obstacle[] obsArray = null;
                
                obstacleLock.lock();
                try
                {
                    if (changed)
                    {
                        changed = false;
                        c = true;
                        n = obstacles.size();
                        obsArray = obstacles.values().toArray(new Obstacle[n]);
                    }
                } 
                finally
                {
                    obstacleLock.unlock();
                }
                
                if (c)
                {
                    obs.nrects = n;
                    obs.ncircles = n;
                    obs.rects = new double[n][];
                    obs.circles = new double[n][];
                    for (int i = 0; i < n; ++i)
                    {
                        obs.rects[i] = obsArray[i].getRect();
                        obs.circles[i] = obsArray[i].getColorHack();
                    }
                    
                    objs.nobjects = n;
                    objs.object_ids = new int[n];
                    objs.object_types = new int[n];
                    objs.objects = new double[n][3];
                    for (int i = 0; i < n; ++i)
                    {
                        objs.object_ids[i] = obsArray[i].getId();
                        objs.object_types[i] = obsArray[i].getType();
                        objs.objects[i][0] = obsArray[i].getRect()[0];
                        objs.objects[i][1] = obsArray[i].getRect()[1];
                        objs.objects[i][2] = obsArray[i].getRect()[4];
                    }
                }

                lcm.publish("SIM_OBSTACLES", obs);
                objs.utime = TimeUtil.utime();
                lcm.publish("MAP_OBJECTS", objs);
            }
        }, 0, 250, TimeUnit.MILLISECONDS);    
    }

    /**
     * Add a virtual object to the broadcaster list. Called when an object is
     * placed on the map.
     * 
     * @param vo
     */
    void addVirtualObject(VirtualObject vo)
    {
        obstacles.put(vo, new ObstacleVO(vo));
        changed = true;
    }
    
    /**
     * Remove a virtual object from the broadcaster list. Called when an object
     * is removed from the map.
     * 
     * @param vo
     */
    void removeVirtualObject(VirtualObject vo)
    {
        obstacles.remove(vo);
        changed = true;
    }
    
    /**
     * Add a door to the list, called when a door goes from OPEN to CLOSED
     * 
     * @param door
     */
    void addDoor(Door door)
    {
        obstacles.put(door, new ObstacleD(door));
        changed = true;
    }
    
    /**
     * Remove a door from the list, called when a door goes from CLOSED to OPEN
     * 
     * @param door
     */
    void removeDoor(Door door)
    {
        obstacles.remove(door);
        changed = true;
    }
    
    /**
     * Reset the obstacles, clearing everything. Metamap is responsible for
     * resetting initial state.
     */
    void reset()
    {
        obstacles.clear();
        changed = true;
    }
    
    /**
     * Shutdown broadcaster thread.
     */
    public void shutdown()
    {
        schexec.shutdown();
    }

}
