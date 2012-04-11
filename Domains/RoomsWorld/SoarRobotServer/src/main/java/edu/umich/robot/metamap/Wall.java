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

import edu.umich.robot.util.Pose;

/**
 * Represents walls in area descriptions.
 * 
 * @author voigtjr@gmail.com
 */
public interface Wall
{
    /**
     * The center of the wall.
     * 
     * @return
     */
    Pose getMidpoint();
    
    /**
     * Which side of the room this wall is on.
     * 
     * @return
     */
    WallDir getDirection();
    
    /**
     * Returns an empty list if this wall is a real wall (it still may have
     * gateways though). If the wall is fully open and part of a composition of
     * areas, this will list what rooms are on the other side of the wall.
     * 
     * @return
     */
    List<Integer> getTo();
    
    int getId();
    int getGatewayId();
}
