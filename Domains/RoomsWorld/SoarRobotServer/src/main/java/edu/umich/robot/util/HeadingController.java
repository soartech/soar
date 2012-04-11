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
package edu.umich.robot.util;

import april.jmat.LinAlg;
import april.jmat.MathUtil;
import april.lcmtypes.pose_t;

/**
 * @author voigtjr@gmail.com
 */
public class HeadingController implements Updatable
{
    private final VelocitiesController velocities;

    private final PoseProvider pose;

    private final PIDController pid;

    private double radians;

    private double lv;

    public HeadingController(VelocitiesController velocities, PoseProvider pose,
            PIDController pid)
    {
        this.velocities = velocities;
        this.pose = pose;
        this.pid = pid;
    }

    public void setHeading(double radians)
    {
        this.radians = radians;
        this.lv = 0;
    }

    public void reset()
    {
        pid.clearIntegral();
        velocities.reset();
    }

    public void setHeadingAndLinear(double radians, double lv)
    {
        this.radians = radians;
        this.lv = lv;
    }

    public void update(double dt)
    {
        pose_t p = pose.getPose();
        if (p.orientation == null || p.orientation.length < 3)
            throw new IllegalArgumentException("pose has invalid orientation");

        double target = MathUtil.mod2pi(radians);
        double actual = MathUtil.mod2pi(LinAlg.quatToRollPitchYaw(p.orientation)[2]);
        double diff = target - actual;
        if (diff > Math.PI)
            target -= 2 * Math.PI;
        else if (diff < -Math.PI)
            target += 2 * Math.PI;
        double out = pid.compute(dt, target, actual);

        //System.out.format("r%2.3f, t%2.3f, a%2.3f, d%2.3f, o%1.5f%n", radians, target, actual, diff, out);

        velocities.setAngular(out);
        velocities.setLinear(lv);
        velocities.update(dt);
    }
}
