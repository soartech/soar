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
package edu.umich.robot.splinter;

import april.jmat.LinAlg;
import april.jmat.MathUtil;
import april.lcmtypes.pose_t;
import edu.umich.robot.lcmtypes.differential_drive_command_t;
import edu.umich.robot.util.PIDController;
import edu.umich.robot.util.PoseProvider;
import edu.umich.robot.util.VelocitiesController;

/**
 * @author voigtjr@gmail.com
 */
public class SplinterVelocities implements VelocitiesController
{
    private final SplinterDrive drive;

    private final PoseProvider pose;

    private final PIDController apid;

    private final PIDController lpid;

    private double av;

    private double lv;

    SplinterVelocities(SplinterDrive drive, PoseProvider pose,
            PIDController apid, PIDController lpid)
    {
        this.drive = drive;
        this.pose = pose;
        this.apid = apid;
        this.lpid = lpid;
    }

    public void setAngular(double av)
    {
        this.av = av;
    }

    public void setLinear(double lv)
    {
        this.lv = lv;
    }

    public void reset()
    {
        apid.clearIntegral();
        lpid.clearIntegral();
    }

    public void update(double dt)
    {
        pose_t p = pose.getPose();
        if (p.orientation == null || p.orientation.length < 3)
            throw new IllegalArgumentException("pose has invalid orientation");

        double aout = apid.compute(dt, av, p.rotation_rate[2]);
        differential_drive_command_t dc = drive.getDrive();

        if (!dc.left_enabled)
        {
            dc.left = 0;
            dc.left_enabled = true;
        }

        if (!dc.right_enabled)
        {
            dc.right = 0;
            dc.right_enabled = true;
        }

        dc.left -= aout;
        dc.right += aout;

        // convert vector to local frame and get forward (x)
        // component
        double theta = MathUtil
                .mod2pi(LinAlg.quatToRollPitchYaw(p.orientation)[2]);
        double xvel = LinAlg.rotate2(p.vel, -theta)[0];
        double lout = lpid.compute(dt, lv, xvel);
        dc.left += lout;
        dc.right += lout;

        drive.setDrive(dc.left, dc.right);
        drive.update(dt);
    }
}
