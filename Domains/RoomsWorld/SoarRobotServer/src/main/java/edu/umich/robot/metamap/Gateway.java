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

import java.util.List;

import com.google.common.collect.ImmutableList;

import edu.umich.robot.util.ImmutablePose;
import edu.umich.robot.util.Pose;

/**
 * Gateways are points that lead to other rooms. These connect rooms and door
 * areas, and have a reference to the door object in the associated door room.
 * 
 * @author voigtjr@gmail.com
 */
public class Gateway
{
    private final int id;

    private final Pose pose;

    private final List<Integer> to;
    
    private final Door door;

    Gateway(Pose pose, List<Integer> to, Door door, int id)
    {
        this.id = id;
        this.pose = ImmutablePose.newInstance(pose);
        this.to = new ImmutableList.Builder<Integer>().addAll(to).build();
        this.door = door;
    }

    /**
     * Globally unique identifier number.
     * 
     * @return
     */
    public int getId()
    {
        return id;
    }

    /**
     * List of area index-ids that this gateway connects.
     * Returns indexes into the area list, NOT area-IDs.
     * 
     * @return
     */
    public List<Integer> getTo()
    {
        return to;
    }

    /**
     * Where this gateway is, a point.
     * 
     * @return
     */
    public Pose getPose()
    {
        return pose;
    }
    
    /**
     * Find out what wall this gateway is on in the passed area description.
     * 
     * @param ad
     * @return
     */
    public WallDir getDirection(AreaDescription ad)
    {
        WallDir ret = WallDir.NORTH;
        double retd = Math.abs(ad.getPose().getY() + ad.getPose().getVY() - pose.getY());
        
        double south = Math.abs(ad.getPose().getY() - pose.getY());
        if (south < retd)
        {
            ret = WallDir.SOUTH;
            retd = south;
        }
        
        double east = Math.abs(ad.getPose().getX() + ad.getPose().getVX() - pose.getX());
        if (east < retd)
        {
            ret = WallDir.EAST;
            retd = east;
        }
        
        double west = Math.abs(ad.getPose().getX() - pose.getX());
        if (west < retd)
        {
            ret = WallDir.WEST;
            retd = west;
        }

        return ret;
    }

    @Override
    public String toString()
    {
        StringBuilder sb = new StringBuilder("[gw ");
        sb.append(id);
        sb.append(String.format(":%2.2f,%2.2f;", pose.getX(), pose.getY()));
        boolean comma = false;
        for (Integer i : to)
        {
            if (comma)
                sb.append(",");
            sb.append(i);
            comma = true;
        }
        sb.append("]");
        return sb.toString();
    }

    /**
     * The door object in the door area associated with this gateway.
     * 
     * @return
     */
    public Door getDoor()
    {
        return door;
    }
}
