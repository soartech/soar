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

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import edu.umich.robot.events.RobotAddedEvent;
import edu.umich.robot.events.RobotRemovedEvent;
import edu.umich.robot.util.events.RobotEventManager;

/**
 * <p>
 * Maps robots and controllers. Manages a stack of controllers for each robot.
 * Having a stack allows the ability to push a temporary controller on making it
 * in control and being able to easily revert back.
 * 
 * <p>
 * The obvious use case here is having the gamepad take over briefly during a
 * run.
 * 
 * @author voigtjr@gmail.com
 */
public class RobotManager
{
    /**
     * Maps robot names to robot instances.
     */
    private final Map<String, Robot> robots = new HashMap<String, Robot>();

    /**
     * Essentially maps robot names to controllers. The OutputEventForwarder is
     * a pipe to pass events from a controller to a robot.
     */
    private final Map<String, OutputEventForwarder> forwarders = new HashMap<String, OutputEventForwarder>();

    /**
     * Copy of reference to main program event manager (from the main program
     * Controller (not a robot controller))
     */
    private final RobotEventManager events;

    /**
     * @param events
     *            Reference to main program event manager.
     */
    public RobotManager(RobotEventManager events)
    {
        this.events = events;
    }

    /**
     * Add a robot to be managed. Refer to later by it's name (Robot.getName())
     * 
     * @param robot
     */
    public void addRobot(Robot robot)
    {
        robots.put(robot.getName(), robot);
        events.fireEvent(new RobotAddedEvent(robot));
    }

    /**
     * Remove a robot by name.
     * 
     * @param name
     */
    public void removeRobot(String name)
    {
        Robot robot = robots.remove(name);
        events.fireEvent(new RobotRemovedEvent(robot));
    }

    /**
     * Set a new controller as the primary for a specific robot. Saves the old
     * controllers in a stack so that they can easily be returned to primary
     * control with a pop action.
     * 
     * @param name
     *            The robot name to change controllers for.
     * @param controller
     *            The controller to set as primary for the named robot.
     */
    public void pushController(String name, RobotController controller)
    {
        OutputEventForwarder fw = forwarders.get(name);
        if (fw == null)
        {
            if (!robots.containsKey(name))
                throw new IllegalStateException("no such robot: " + name);
            fw = new OutputEventForwarder(robots.get(name), events);
            forwarders.put(name, fw);
        }
        fw.pushController(controller);
    }

    /**
     * Remove the current controller, restoring the controller previously used.
     * If there was no other controller, NullController is used.
     * 
     * @param name
     *            The robot name to change controllers for.
     */
    public void popController(String name)
    {
        OutputEventForwarder fw = forwarders.get(name);
        if (fw == null)
            throw new IllegalStateException("no such robot: " + name);
        fw.popController();
    }

    /**
     * Get a bag of robots.
     * 
     * @return The collection is unmodifiable, the robots are not.
     */
    public Collection<? extends Robot> getAll()
    {
        return Collections.unmodifiableCollection(robots.values());
    }
    
    /**
     * Get a specific robot by name.
     * 
     * @param name
     * @return
     */
    Robot get(String name)
    {
        return robots.get(name);
    }

    /**
     * Call shutdown on all robots.
     */
    public void shutdown()
    {
        for (Robot robot : robots.values())
            robot.shutdown();
    }
}
