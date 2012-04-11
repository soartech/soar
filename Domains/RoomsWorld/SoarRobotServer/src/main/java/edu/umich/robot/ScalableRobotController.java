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
import edu.umich.robot.events.control.DriveScalable;
import edu.umich.robot.util.events.RobotEventListener;
import edu.umich.robot.util.events.RobotEventManager;

/**
 * <p>
 * This is a class that scales control events as they pass through it. It is
 * currently only used as a pipe for gamepad events right now (the gamepad
 * generates control event objects and hands them to this class which then fires
 * them to listeners (the current robot it controls). In theory, the setLimits
 * function could be called to scale the values in the drive events by a number.
 * 
 * <p>
 * So, if setLimits(2, 2) was called, a DriveLinearEvent with value 1.0 would
 * change to 2.0 as it passed through this class on the way to a robot.
 * 
 * @author voigtjr@gmail.com
 */
public class ScalableRobotController implements RobotController
{
    private final String name;

    private final RobotEventManager events = new RobotEventManager();

    private double linear;

    private double angular;

    public ScalableRobotController(String name)
    {
        this.name = name;
    }

    public <T extends AbstractControlEvent> void addListener(Class<T> klass,
            RobotEventListener listener)
    {
        events.addListener(klass, listener);
    }

    public String getName()
    {
        return name;
    }

    public <T extends AbstractControlEvent> void removeListener(Class<T> klass,
            RobotEventListener listener)
    {
        events.removeListener(klass, listener);
    }

    void setLimits(double linear, double angular)
    {
        this.linear = linear;
        this.angular = angular;
    }

    public void fireEvent(AbstractControlEvent event, Class<? extends AbstractControlEvent> eventType)
    {
        if (event instanceof DriveScalable)
        {
            DriveScalable drive = (DriveScalable) event;
            drive = drive.scaleLinear(linear);
            drive = drive.scaleAngular(angular);
            events.fireEvent((AbstractControlEvent)drive, eventType);
        }
        else
            events.fireEvent(event, eventType);
    }

    @Override
    public String toString()
    {
        return getName();
    }

    public void debug()
    {
    }

}
