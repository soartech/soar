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

import sml.Identifier;
import sml.WMElement;
import edu.umich.robot.RobotOutput;
import edu.umich.robot.util.Pose;

/**
 * Add a waypoint to the waypoint system.
 * 
 * @author voigtjr@gmail.com
 */
public class AddWaypointCommand extends OLCommand
{
    static final String NAME = "add-waypoint";

    private final RobotOutput output;

    private final WaypointManager waypoints;

    private final String type;

    private final String id;

    private final Double x;

    private final Double y;

    private final Double z;

    private final Double yaw;

    public AddWaypointCommand(Identifier wme, SoarAgent agent)
            throws SoarCommandError
    {
        super(wme);
        this.waypoints = agent.getWaypoints();
        this.output = agent.getRobotOutput();

        id = CommandParameters.requireString(wme, IOConstants.ID);

        WMElement idwme = wme.FindByAttribute(IOConstants.ID, 0);
        type = idwme.GetValueType();

        if (wme.GetParameterValue(IOConstants.X) != null)
        {
            double tmp = CommandParameters.requireDouble(wme, IOConstants.X);
            this.x = agent.getProperties().get(AgentProperties.LENGTH_UNIT).fromView(tmp);
        }
        else
        {
            this.x = null;
        }

        if (wme.GetParameterValue(IOConstants.Y) != null)
        {
            double tmp = CommandParameters.requireDouble(wme, IOConstants.Y);
            this.y = agent.getProperties().get(AgentProperties.LENGTH_UNIT).fromView(tmp);
        }
        else
        {
            this.y = null;
        }

        if (wme.GetParameterValue(IOConstants.Z) != null)
        {
            double tmp = CommandParameters.requireDouble(wme, IOConstants.Z);
            this.z = agent.getProperties().get(AgentProperties.LENGTH_UNIT).fromView(tmp);
        }
        else
        {
            this.z = null;
        }

        if (wme.GetParameterValue(IOConstants.YAW) != null)
        {
            double tmp = CommandParameters.requireDouble(wme, IOConstants.YAW);
            this.yaw = agent.getProperties().get(AgentProperties.LENGTH_UNIT).fromView(tmp);
        }
        else
        {
            this.yaw = null;
        }
    }

    @Override
    void update()
    {
        Pose waypointPose = output.getPose();
        if (x != null)
        {
            waypointPose.setX(x);
        }
        if (y != null)
        {
            waypointPose.setY(y);
        }
        if (z != null)
        {
            waypointPose.setZ(z);
        }
        if (yaw != null)
        {
            waypointPose.setYaw(yaw);
        }

        waypoints.create(id, type, waypointPose);
        setStatus(CommandStatus.COMPLETE);
    }

    @Override
    public String toString()
    {
        return toStringPrefix() + String.format(", %s(%s),%.2f,%.2f,%.2f,%.2f)", id, type, x, y, z, yaw);
    }
    
}
