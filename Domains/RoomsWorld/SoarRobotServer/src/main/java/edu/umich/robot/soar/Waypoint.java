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

import edu.umich.robot.util.ImmutablePose;
import edu.umich.robot.util.Pose;

/**
 * A point on the map set by the agent.
 * 
 * @auther voigtjr@gmail.com
 */
class Waypoint
{

    private final String id;

    private final String type;

    private final ImmutablePose pose;

    Waypoint(String id, String type, Pose pose)
    {
        if (id == null || type == null || pose == null)
            throw new NullPointerException();

        this.id = id;
        this.type = type;
        this.pose = ImmutablePose.newInstance(pose);
    }

    public String getId()
    {
        return id;
    }

    public String getType()
    {
        return type;
    }

    public ImmutablePose getPose()
    {
        return pose;
    }

    @Override
    public boolean equals(Object obj)
    {
        if (obj instanceof Waypoint)
            return ((Waypoint) obj).id.equals(id);
        return super.equals(obj);
    }

    @Override
    public int hashCode()
    {
        return id.hashCode();
    }
    
    @Override
    public String toString()
    {
        return id + ":" + type;
    }
}
