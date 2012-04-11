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
package edu.umich.robot.soar;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import lcm.lcm.LCM;
import april.util.TimeUtil;

import com.google.common.collect.Lists;
import com.google.common.util.concurrent.MoreExecutors;

import edu.umich.robot.lcmtypes.waypoint_list_t;
import edu.umich.robot.lcmtypes.waypoint_t;
import edu.umich.robot.util.Pose;

/**
 * A collection of points saved by the agent.
 * 
 * <p>Not thread safe.
 * 
 * @author voigtjr@gmail.com
 */
public class WaypointManager implements Iterable<Waypoint>
{
    private Map<String, Waypoint> all = new HashMap<String, Waypoint>();

    private Set<Waypoint> enabled = new CopyOnWriteArraySet<Waypoint>();
    
    private static final LCM lcm = LCM.getSingleton();
    
    private final String channel;
    
    private final ScheduledExecutorService schexec = MoreExecutors.getExitingScheduledExecutorService(new ScheduledThreadPoolExecutor(1));

    public WaypointManager(String agentName)
    {
        this.channel = "WAYPOINTS_" + agentName;

        schexec.scheduleAtFixedRate(publisher, 0, 250, TimeUnit.MILLISECONDS);
    }
    
    private final Runnable publisher = new Runnable() {
        public void run()
        {
            if (enabled.isEmpty())
                return;
            
            waypoint_list_t lcmwps = new waypoint_list_t();
            lcmwps.utime = TimeUtil.utime();
            List<waypoint_t> lcmtemplist = Lists .newArrayListWithExpectedSize(enabled.size());
            
            for (Waypoint waypoint : enabled)
            {
                waypoint_t wp = new waypoint_t();
                wp.utime = lcmwps.utime;
                wp.xLocal = waypoint.getPose().getX();
                wp.yLocal = waypoint.getPose().getY();
                wp.tLocal = waypoint.getPose().getYaw();
                lcmtemplist.add(wp);
            }
            
            lcmwps.waypoints = lcmtemplist.toArray(new waypoint_t[lcmtemplist.size()]);
            lcmwps.nwaypoints = lcmwps.waypoints.length;
            
            lcm.publish(channel, lcmwps);
        }
    };
    
    public void create(String id, String type, Pose pose)
    {
        Waypoint waypoint = new Waypoint(id, type, pose);

        all.put(waypoint.getId(), waypoint);
        enabled.add(waypoint);
    }

    public boolean enable(String id)
    {
        Waypoint waypoint = all.get(id);
        if (waypoint != null)
            return enabled.add(waypoint);
        return false;
    }

    public boolean disable(String id)
    {
        Waypoint waypoint = all.get(id);
        if (waypoint != null)
            return enabled.remove(waypoint);
        return false;
    }

    public boolean remove(String id)
    {
        Waypoint waypoint = all.remove(id);
        if (waypoint != null)
        {
            enabled.remove(waypoint);
            return true;
        }
        return false;
    }

    public void clear()
    {
        all.clear();
        enabled.clear();
    }

    public Iterator<Waypoint> iterator()
    {
        return enabled.iterator();
    }

    public void shutdown()
    {
        schexec.shutdown();
    }
}
