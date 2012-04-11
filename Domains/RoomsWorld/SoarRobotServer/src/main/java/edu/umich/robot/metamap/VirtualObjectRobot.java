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
import java.util.Map;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;

import edu.umich.robot.Robot;
import edu.umich.robot.util.Pose;

/**
 * Virtual object representing a robot in the simulation.
 * 
 * @author voigtjr@gmail.com
 */
public class VirtualObjectRobot implements VirtualObject
{
    private final int id;
    private final Robot robot;
    private final ImmutableMap<String, String> properties;
    
    public VirtualObjectRobot(Robot robot, int id)
    {
        this.id = id;
        this.robot = robot;
        
        ImmutableMap.Builder<String, String> pb = new ImmutableMap.Builder<String, String>();
        pb.put("name", robot.getName());
        pb.put("type", "player");
        properties = pb.build();
    }
    
    public String getName()
    {
        return robot.getName();
    }
    
    public int getId()
    {
        return id;
    }

    public Pose getPose()
    {
        return robot.getOutput().getPose();
    }

    public Map<String, String> getProperties()
    {
        return properties;
    }

    public List<Double> getSize()
    {
        return Lists.newArrayList(
                robot.getOutput().getRobotConfiguration().getGeometryLength(), 
                robot.getOutput().getRobotConfiguration().getGeometryWidth());
    }

}
