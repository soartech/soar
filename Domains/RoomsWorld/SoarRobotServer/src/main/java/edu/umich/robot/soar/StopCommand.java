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

import org.apache.commons.logging.LogFactory;

import sml.Identifier;
import april.jmat.LinAlg;
import edu.umich.robot.RobotOutput;
import edu.umich.robot.events.control.AbstractDriveEvent;
import edu.umich.robot.events.control.DriveAngularEvent;
import edu.umich.robot.events.control.DriveLinearEvent;
import edu.umich.robot.util.Misc;

/**
 * Gracefully stop movement.
 * 
 * @author voigtjr@gmail.com
 */
public class StopCommand extends AbstractMultipleCycleCommand
{
    static final String NAME = "stop";

    private static final double TOLERANCE = 0.01; // meters per second

    private final RobotOutput output;

    public StopCommand(Identifier wme, SoarAgent agent)
    {
        super(wme, agent.getEvents(), LogFactory.getLog(StopCommand.class));
        this.output = agent.getRobotOutput();

        addEvent(new DriveLinearEvent(0), AbstractDriveEvent.class);
        addEvent(new DriveAngularEvent(0), AbstractDriveEvent.class);
    }

    @Override
    boolean isComplete()
    {
        double[] vel = Misc.toPrimitiveDoubleArray(output.getPose().getVel());
        return Double.compare(LinAlg.magnitude(vel), TOLERANCE) < 0;
    }
}
