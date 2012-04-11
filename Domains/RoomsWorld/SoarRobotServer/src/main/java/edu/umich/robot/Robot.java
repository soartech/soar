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
package edu.umich.robot;

import edu.umich.robot.events.control.AbstractControlEvent;

/**
 * <p>
 * Each type of robot in the simulation will implement this interface.
 * 
 * @author voigtjr@gmail.com
 */
public interface Robot
{
    /**
     * <p>
     * Get the robot's output interface, this is how you get the robot's current
     * state.
     * 
     * @return A read-only interface to the robot's current state.
     */
    public RobotOutput getOutput();

    /**
     * <p>
     * Get the robot's name, globally unique identifier. Should be a simple
     * string.
     * 
     * @return The name as a string.
     */
    public String getName();

    /**
     * <p>
     * This is how to tell the robot what to do. This accepts control events.
     * 
     * @param event
     *            A command for the robot.
     */
    public void handleControlEvent(AbstractControlEvent event);
    
    /**
     * <p>
     * Change how fast time is going with an integer multiplier. 1 is 1:1, 2 is
     * twice as fast, etc.
     * 
     * @param multiplier
     */
    public void setTimeScale(int multiplier);
    
    /**
     * <p>
     * Release resources, invalidate.
     */
    public void shutdown();

    /**
     * <p>
     * Returns what type this robot is. This should not really be necessary...
     * 
     * @return This robot's type.
     */
    public RobotType getType();
}
