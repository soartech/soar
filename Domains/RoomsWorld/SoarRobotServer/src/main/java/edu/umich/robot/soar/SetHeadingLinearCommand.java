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
import edu.umich.robot.RobotOutput;
import edu.umich.robot.events.control.AbstractDriveEvent;
import edu.umich.robot.events.control.DriveHeadingAndLinearEvent;

/**
 * Set target heading and linear velocity.
 * 
 * @author voigtjr@gmail.com
 */
public class SetHeadingLinearCommand extends AbstractMultipleCycleCommand
{
    static final String NAME = "set-heading-linear";

    private static final double TOLERANCE = Math.toRadians(3);

    private final RobotOutput output;

    private final double targetYaw;

    public SetHeadingLinearCommand(Identifier wme, SoarAgent agent)
            throws SoarCommandError
    {
        super(wme, agent.getEvents(), LogFactory.getLog(SetHeadingLinearCommand.class));
        this.output = agent.getRobotOutput();

        double targetYaw = CommandParameters
                .requireDouble(wme, IOConstants.YAW);
        targetYaw = agent.getProperties().get(AgentProperties.ANGLE_UNIT).fromView(targetYaw);
        this.targetYaw = targetYaw;

        double linearVelocity = CommandParameters.requireDouble(wme,
                IOConstants.LINEAR_VELOCITY);
        linearVelocity = agent.getProperties().get(AgentProperties.LINEAR_SPEED_UNIT).fromView(linearVelocity);

        addEvent(new DriveHeadingAndLinearEvent(targetYaw, linearVelocity),
                AbstractDriveEvent.class);
    }

    @Override
    boolean isComplete()
    {
        double currentYaw = output.getPose().getYaw();
        double difference = targetYaw - currentYaw;
        difference = Math.abs(difference);

        return Double.compare(difference, TOLERANCE) < 0;
    }

}
