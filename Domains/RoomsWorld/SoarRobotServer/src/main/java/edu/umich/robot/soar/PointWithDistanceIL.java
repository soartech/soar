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
import edu.umich.robot.util.properties.PropertyManager;
import edu.umich.robot.util.soar.DistanceWme;

/**
 * @author voigtjr@gmail.com
 */
public class PointWithDistanceIL
{
    private final Identifier root;

    private final DistanceWme x;

    private final DistanceWme y;

    private final DistanceWme z;

    private final DistanceWme distance;

    private final RobotOutput output;

    private final Pose p;

    PointWithDistanceIL(Identifier root, SoarAgent agent, Pose p)
    {
        this.root = root;
        this.output = agent.getRobotOutput();
        this.p = p;

        PropertyManager properties = agent.getProperties();
        x = DistanceWme.newInstance(root, IOConstants.X, p.getX(), properties);
        y = DistanceWme.newInstance(root, IOConstants.Y, p.getY(), properties);
        z = DistanceWme.newInstance(root, IOConstants.Z, p.getZ(), properties);

        distance = DistanceWme.newInstance(root, IOConstants.DISTANCE, properties);
    }

    public Identifier getRoot()
    {
        return root;
    }

    public void update()
    {
        distance.update(output.getPose().getDistance(p));
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
    }

}
