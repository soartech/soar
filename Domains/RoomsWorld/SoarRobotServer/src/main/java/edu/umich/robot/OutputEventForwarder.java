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

import java.util.ArrayDeque;
import java.util.Deque;

import edu.umich.robot.events.ControllerActivatedEvent;
import edu.umich.robot.events.ControllerDeactivatedEvent;
import edu.umich.robot.events.control.AbstractControlEvent;
import edu.umich.robot.util.events.RobotEvent;
import edu.umich.robot.util.events.RobotEventListener;
import edu.umich.robot.util.events.RobotEventManager;

/**
 * <p>
 * Pipes events between robots and robot controllers. Maintains a stack of robot
 * controllers so that new ones can be pushed on top.
 * 
 * @author voigtjr@gmail.com
 */
class OutputEventForwarder
{
    private final Robot robot;

    private final RobotEventManager events;

    private final Deque<RobotController> stack = new ArrayDeque<RobotController>();

    /**
     * <p>
     * Constructor serving a robot. Takes a reference to the program event
     * controller so that it can fire controller activated/deactivated. This is
     * probably a bad dependency.
     * 
     * @param robot
     *            The robot to manage controllers for.
     * @param events
     *            Controller event manager for program events.
     */
    OutputEventForwarder(Robot robot, RobotEventManager events)
    {
        this.robot = robot;
        this.events = events;
    }

    /**
     * <p>
     * Control events are picked up here and sent on to the robot. This class
     * could alternatively just register the robot for these events directly.
     */
    private final RobotEventListener forwarder = new RobotEventListener()
    {
        public void onEvent(RobotEvent event)
        {
            robot.handleControlEvent((AbstractControlEvent) event);
        }
    };

    /**
     * <p>
     * Check if there are no controllers for this robot.
     * 
     * @return true if there are no controllers.
     */
    synchronized boolean isEmpty()
    {
        return stack.isEmpty();
    }

    /**
     * <p>
     * Push a controller on to the stack. This controller becomes the one in
     * control, the previous controller is unregistered.
     * 
     * @param controller
     *            The new controller to take over.
     */
    synchronized void pushController(RobotController controller)
    {
        RobotController temp = stack.peekFirst();
        if (temp != null)
            unregisterInternal(temp);
        stack.addFirst(controller);
        registerInternal(controller);
    }

    /**
     * <p>
     * Unregister the previously registered controller, popping it from the
     * stack of controllers.
     */
    synchronized void popController()
    {
        RobotController controller = stack.removeFirst();
        if (controller != null)
            unregisterInternal(controller);

        controller = stack.peekFirst();

        if (controller != null)
            registerInternal(controller);
    }

    /**
     * <p>
     * Pop all controllers.
     */
    synchronized void clear()
    {
        if (!stack.isEmpty())
            unregisterInternal(stack.removeFirst());
        stack.clear();
    }

    private void registerInternal(RobotController c)
    {
        c.addListener(null, forwarder);
        events.fireEvent(new ControllerActivatedEvent(robot, c));
    }

    private void unregisterInternal(RobotController c)
    {
        c.removeListener(null, forwarder);
        events.fireEvent(new ControllerDeactivatedEvent(robot, c));
    }
}
