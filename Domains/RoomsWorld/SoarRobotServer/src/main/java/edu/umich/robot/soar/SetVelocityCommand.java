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
import edu.umich.robot.events.control.AbstractDriveEvent;
import edu.umich.robot.events.control.DriveAngularEvent;
import edu.umich.robot.events.control.DriveLinearEvent;

/**
 * Set linear and angular velocities.
 * 
 * @author voigtjr@gmail.com
 */
public class SetVelocityCommand extends AbstractCompleteOnInterruptCommand
{
    static final String NAME = "set-velocity";

    public SetVelocityCommand(Identifier wme, SoarAgent agent)
            throws SoarCommandError
    {
        super(wme, agent.getEvents(), LogFactory.getLog(SetVelocityCommand.class));

        Double linear = null;
        Double angular = null;
        {
            String linvelString = wme
                    .GetParameterValue(IOConstants.LINEAR_VELOCITY);
            String angvelString = wme
                    .GetParameterValue(IOConstants.ANGULAR_VELOCITY);

            if (linvelString == null && angvelString == null)
                throw new SoarCommandError("Must have at least one of "
                        + IOConstants.LINEAR_VELOCITY + " or "
                        + IOConstants.ANGULAR_VELOCITY + " on the command.");

            if (linvelString != null)
                linear = CommandParameters.requireDouble(wme,
                        IOConstants.LINEAR_VELOCITY);

            if (angvelString != null)
                angular = CommandParameters.requireDouble(wme,
                        IOConstants.ANGULAR_VELOCITY);
        }

        if (linear == null)
        {
            angular = agent.getProperties().get(AgentProperties.ANGULAR_SPEED_UNIT).fromView(angular);
            addEvent(new DriveAngularEvent(angular), AbstractDriveEvent.class);
        }
        else if (angular == null)
        {
            linear = agent.getProperties().get(AgentProperties.LINEAR_SPEED_UNIT).fromView(linear);
            addEvent(new DriveLinearEvent(linear), AbstractDriveEvent.class);
        }
        else
        {
            angular = agent.getProperties().get(AgentProperties.ANGULAR_SPEED_UNIT).fromView(angular);
            linear = agent.getProperties().get(AgentProperties.LINEAR_SPEED_UNIT).fromView(linear);
            addEvent(new DriveAngularEvent(angular), AbstractDriveEvent.class);
            addEvent(new DriveLinearEvent(linear), AbstractDriveEvent.class);
        }
    }
}
