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
import java.util.Map.Entry;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import sml.Identifier;

import com.google.common.collect.Lists;

import edu.umich.soar.FloatWme;
import edu.umich.soar.IntWme;
import edu.umich.soar.StringWme;

/**
 * Input link management of waypoints
 * 
 * @author voigtjr@gmail.com
 */
public class WaypointsIL extends InputLinkElement
{
    private static final Log logger = LogFactory.getLog(WaypointsIL.class);

    private final SoarAgent agent;

    private final WaypointManager waypoints;

    private Map<String, PointDataIL> waypointMap = new HashMap<String, PointDataIL>();
    
    public WaypointsIL(SoarAgent agent)
    {
        super(agent, IOConstants.WAYPOINTS, agent.getSoarAgent().GetInputLink());
        this.agent = agent;
        this.waypoints = agent.getWaypoints();
        
        update();
    }

    @Override
    public void update()
    {
        List<String> seen = Lists.newArrayListWithExpectedSize(waypointMap.size());
        
        for (Waypoint waypoint : waypoints)
        {
            logger.trace("Saw waypoint " + waypoint);
            seen.add(waypoint.getId());

            PointDataIL pointData = waypointMap.get(waypoint.getId());
            if (pointData == null)
                pointData = createNewPointData(waypoint);

            logger.trace("Updating waypoint point data.");
            pointData.update();

            waypointMap.put(waypoint.getId(), pointData);
        }
        
        for (Iterator<Entry<String, PointDataIL>> iter = waypointMap.entrySet().iterator(); iter.hasNext(); )
        {
            Entry<String, PointDataIL> entry = iter.next();
            if (!seen.contains(entry.getKey()))
            {
                logger.debug("Removing waypoint " + entry.getKey());
                entry.getValue().getRoot().DestroyWME();
                iter.remove();
            }
        }
        logger.trace("Update done.");
    }

    private PointDataIL createNewPointData(Waypoint waypoint)
    {
        logger.debug("New point data for waypoint " + waypoint);
        Identifier root = getRoot().CreateIdWME(IOConstants.WAYPOINT);

        if (waypoint.getType().equals(IOConstants.INT))
            IntWme.newInstance(root, IOConstants.ID, Integer.parseInt(waypoint
                    .getId()));
        else if (waypoint.getType().equals(IOConstants.FLOAT))
            FloatWme.newInstance(root, IOConstants.ID, Double
                    .parseDouble(waypoint.getId()));
        else
            StringWme.newInstance(root, IOConstants.ID, waypoint.getId());

        return new PointDataIL(root, agent, waypoint.getPose());
    }

}
