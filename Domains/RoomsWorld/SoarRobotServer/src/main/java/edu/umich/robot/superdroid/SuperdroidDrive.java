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
package edu.umich.robot.superdroid;

import lcm.lcm.LCM;
import april.util.TimeUtil;
import edu.umich.robot.lcmtypes.differential_drive_command_t;
import edu.umich.robot.util.Misc;
import edu.umich.robot.util.Updatable;

/**
 * @author voigtjr@gmail.com
 */
public class SuperdroidDrive implements Updatable
{
    private static final LCM lcm = LCM.getSingleton();
    public static final String DRIVE_CHANNEL_BASE = "DIFFERENTIAL_DRIVE_COMMAND_";

    private final String driveChannel;
    
    private final differential_drive_command_t dc = new differential_drive_command_t();

    SuperdroidDrive(String id)
    {
        this.driveChannel = DRIVE_CHANNEL_BASE + id;
        dc.left_enabled = true;
        dc.right_enabled = true;
    }

    void setDrive(double left, double right)
    {
        synchronized (dc)
        {
            dc.left = Misc.clamp(left, -1.0, 1.0);
            dc.right = Misc.clamp(right, -1.0, 1.0);

            double effort = Math.abs(dc.left) + Math.abs(dc.right);
            effort /= 2;
        }
    }

    differential_drive_command_t getDrive()
    {
        return dc.copy();
    }

    public void update(double dt)
    {
        dc.utime = TimeUtil.utime();
        lcm.publish(driveChannel, dc);
    }
}
