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
import edu.umich.robot.util.AngSpeedUnit;
import edu.umich.robot.util.AngleResolution;
import edu.umich.robot.util.AngleUnit;
import edu.umich.robot.util.ImmutablePose;
import edu.umich.robot.util.LengthUnit;
import edu.umich.robot.util.LinSpeedUnit;
import edu.umich.robot.util.Pose;

/**
 * Configure robot.
 * 
 * @author voigtjr@gmail.com
 */
public class ConfigureCommand extends OLCommand
{
    static final String NAME = "configure";

    public ConfigureCommand(Identifier wme, SoarAgent agent) throws SoarCommandError
    {
        super(wme);

        {
            String temp = wme.GetParameterValue(IOConstants.LENGTH_UNITS);
            if (temp != null)
            {
                try
                {
                    LengthUnit lu = LengthUnit.valueOf(temp .toUpperCase());
                    agent.getProperties().set(AgentProperties.LENGTH_UNIT, lu);
                }
                catch (IllegalArgumentException e)
                {
                    throw new SoarCommandError("Unknown " + IOConstants.LENGTH_UNITS + " type " + temp);
                }
            }
        }

        {
            String temp = wme.GetParameterValue(IOConstants.LINEAR_SPEED_UNITS);
            if (temp != null)
            {
                try
                {
                    LinSpeedUnit su = LinSpeedUnit.valueOf(temp .toUpperCase());
                    agent.getProperties().set(AgentProperties.LINEAR_SPEED_UNIT, su);
                }
                catch (IllegalArgumentException e)
                {
                    throw new SoarCommandError("Unknown " + IOConstants.LINEAR_SPEED_UNITS + " type " + temp);
                }
            }
        }

        {
            String temp = wme.GetParameterValue(IOConstants.ANGULAR_SPEED_UNITS);
            if (temp != null)
            {
                try
                {
                    AngSpeedUnit su = AngSpeedUnit.valueOf(temp .toUpperCase());
                    agent.getProperties().set(AgentProperties.ANGULAR_SPEED_UNIT, su);
                }
                catch (IllegalArgumentException e)
                {
                    throw new SoarCommandError("Unknown " + IOConstants.ANGULAR_SPEED_UNITS + " type " + temp);
                }
            }
        }

        {
            String temp = wme.GetParameterValue(IOConstants.ANGLE_UNITS);
            if (temp != null)
            {
                try
                {
                    AngleUnit au = AngleUnit.valueOf(temp .toUpperCase());
                    agent.getProperties().set(AgentProperties.ANGLE_UNIT, au);
                }
                catch (IllegalArgumentException e)
                {
                    throw new SoarCommandError("Unknown " + IOConstants.ANGLE_UNITS + " type " + temp);
                }
            }
        }

        {
            String temp = wme.GetParameterValue(IOConstants.ANGLE_RESOLUTION);
            if (temp != null)
            {
                try
                {
                    AngleResolution ar = AngleResolution .valueOf(temp.toUpperCase());
                    agent.getProperties().set(AgentProperties.ANGLE_RESOLUTION, ar);
                }
                catch (IllegalArgumentException e)
                {
                    throw new SoarCommandError("Unknown " + IOConstants.ANGLE_RESOLUTION + " type " + temp);
                }
            }
        }

        {
            WMElement ptwme = wme.FindByAttribute(IOConstants.POSE_TRANSLATION, 0);
            if (ptwme != null)
            {
                if (!ptwme.IsIdentifier())
                    throw new SoarCommandError("Bad formatted command " + IOConstants.POSE_TRANSLATION);

                Identifier ptid = ptwme.ConvertToIdentifier();
                double[] xyz = new double[] { 0, 0, 0 };

                if (ptid.GetParameterValue(IOConstants.X) != null)
                {
                    double tmp = CommandParameters.requireDouble(ptid, IOConstants.X);
                    xyz[0] = agent.getProperties().get(AgentProperties.LENGTH_UNIT).fromView(tmp);
                }
                if (ptid.GetParameterValue(IOConstants.Y) != null)
                {
                    double tmp = CommandParameters.requireDouble(ptid, IOConstants.Y); 
                    xyz[1] = agent.getProperties().get(AgentProperties.LENGTH_UNIT).fromView(tmp);
                }
                if (ptid.GetParameterValue(IOConstants.Z) != null)
                {
                    double tmp = CommandParameters.requireDouble(ptid, IOConstants.Z);
                    xyz[2] = agent.getProperties().get(AgentProperties.LENGTH_UNIT).fromView(tmp);
                }

                Pose impt = ImmutablePose.newInstance(new Pose(xyz));
                agent.getProperties().set(AgentProperties.TRANSLATION, impt);
            }
        }
    }

    @Override
    void update()
    {
        setStatus(CommandStatus.COMPLETE);
    }
}
