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

import edu.umich.robot.util.ImmutablePose;

/**
 * Interface to area description.
 * 
 * @author voigtjr@gmail.com
 */
public interface AreaDescription
{
    /**
     * Globally unique area id, usually starts at zero. Negative value means no
     * area information is known for current location.
     * 
     * @return
     */
    public int getId();

    /**
     * <p>
     * The room's pose information, including width and height. This is a bit
     * overloaded and not clear.
     * 
     * <p>
     * pose.pos has x, y, z (z == 0) position information for the lower-left
     * corner of the room (quadrant I in cartesian coordinates).
     * 
     * <p>
     * pose.vel has x-length, y-length, z (z == 0) dimensions for the walls.
     * 
     * @return
     */
    public ImmutablePose getPose();

    /**
     * Key-value properties for the room. Arbitrary.
     * @return
     */
    public Map<String, String> getProperties();

    /**
     * List of gateways in the room.
     * 
     * @return
     */
    public List<Gateway> getGateways();

    /**
     * List of walls in the room. Some of the information here is redundant with
     * the room's pose information.
     * 
     * @return
     */
    public List<Wall> getWalls();
    
    /**
     * Returns the center of the room.
     * 
     * @return
     */
    public ImmutablePose getMidpoint();

    /**
     * Returns the room dimensions in pixels, x, y, w, height, with y axis increasing down.
     * 
     * @return
     */
    public int[] getPixelRect();

    /**
     * 
     * @return True if this AreaDescription has changed.
     */
    public boolean hasChanged();
    
    /**
     * Sets the changed state of this AreaDescription.
     * @param changed
     */
    public void setChanged(boolean changed);
}
