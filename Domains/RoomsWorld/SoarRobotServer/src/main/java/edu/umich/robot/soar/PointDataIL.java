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
import edu.umich.robot.RobotOutput;
import edu.umich.robot.util.Pose;
import edu.umich.robot.util.RelativePose;
import edu.umich.robot.util.properties.PropertyManager;
import edu.umich.robot.util.soar.DistanceWme;
import edu.umich.robot.util.soar.YawWme;

/**
 * Input link management of point data, a collection of distances and angles
 * relative to the current agent's position.
 * 
 * @author voigtjr@gmail.com
 */
public class PointDataIL
{
    private final Identifier root;

    private final DistanceWme x;

    private final DistanceWme y;

    private final DistanceWme z;

    private final DistanceWme distance;

    private final YawWme yaw;

    private final YawWme relYaw;

    private final YawWme absRelYaw;

    private final RobotOutput output;

    private final Pose p;

    PointDataIL(Identifier root, SoarAgent agent, Pose p)
    {
        this.root = root;
        this.output = agent.getRobotOutput();
        this.p = p;

        PropertyManager properties = agent.getProperties();
        x = DistanceWme.newInstance(root, IOConstants.X, p.getX(), properties);
        y = DistanceWme.newInstance(root, IOConstants.Y, p.getY(), properties);
        z = DistanceWme.newInstance(root, IOConstants.Z, p.getZ(), properties);

        distance = DistanceWme.newInstance(root, IOConstants.DISTANCE, properties);
        yaw = YawWme.newInstance(root, IOConstants.YAW, properties);
        relYaw = YawWme.newInstance(root, IOConstants.RELATIVE_BEARING, properties);
        absRelYaw = YawWme.newInstance(root, IOConstants.ABS_RELATIVE_BEARING, properties);
    }

    public Identifier getRoot()
    {
        return root;
    }

    public void update()
    {
        RelativePose relative = output.getPose().getRelative(p);

        distance.update(relative.getDistance());
        yaw.update(relative.getYaw());
        relYaw.update(relative.getRelativeYaw());
        absRelYaw.update(Math.abs(relative.getRelativeYaw()));
    }

    public Pose getPose()
    {
        return p;
    }

    public void destroyChildren()
    {
        x.destroy();
        y.destroy();
        z.destroy();
        distance.destroy();
        yaw.destroy();
        relYaw.destroy();
        absRelYaw.destroy();
    }

}
