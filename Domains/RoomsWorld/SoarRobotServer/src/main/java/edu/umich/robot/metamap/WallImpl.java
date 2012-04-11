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

import java.util.ArrayList;
import java.util.List;

import com.google.common.collect.ImmutableList;

import edu.umich.robot.util.ImmutablePose;
import edu.umich.robot.util.Pose;

/**
 * @author voigtjr@gmail.com
 */
public class WallImpl implements Wall
{
    private final Pose midpoint;
    
    private final WallDir dir;
    
    private final List<Integer> to;
    
    private int id;
    private int gatewayId;
    
    WallImpl(Pose room, Pose roomMidpoint, WallDir dir, IdGenerator idg, boolean open)
    {
        this(room, roomMidpoint, dir, new ArrayList<Integer>(), idg, open);
    }
    
    WallImpl(Pose room, Pose roomMidpoint, WallDir dir, List<Integer> to, IdGenerator idg, boolean open)
    {
        this.dir = dir;
        this.to = new ImmutableList.Builder<Integer>().addAll(to).build();
        this.id = idg.getId();
        this.gatewayId = open ? idg.getId() : -1;

        Pose mp = new Pose(roomMidpoint);
        switch (dir)
        {
        case EAST:
            mp.setX(room.getX() + room.getVX());
            break;
        case NORTH:
            mp.setY(room.getY() + room.getVY());
            break;
        case SOUTH:
            mp.setY(room.getY());
            break;
        case WEST:
            mp.setX(room.getX());
            break;
        }
        this.midpoint = ImmutablePose.newInstance(mp);
    }

    public WallDir getDirection()
    {
        return dir;
    }

    public Pose getMidpoint()
    {
        return midpoint;
    }
    
    public List<Integer> getTo()
    {
        return to;
    }
    
    public int getId()
    {
        return id;
    }
    
    public int getGatewayId()
    {
        return gatewayId;
    }
    
    @Override
    public String toString()
    {
        return String.format("[Wall %s,%1.2f,%1.2f,%s]", dir, midpoint.getX(), midpoint.getY(), to.isEmpty() ? "solid" : "open");
    }

}
