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

import april.jmat.MathUtil;

/**
 * Simple PID controller.
 * 
 * @author voigtjr@gmail.com
 */
public class PIDController
{
    private final double[] pid = new double[] { 0, 0, 0 };

    private double previousError;

    private double integral;

    private double previousTarget;

    private final boolean mod2pi;

    public PIDController()
    {
        this(false);
    }

    public PIDController(boolean mod2pi)
    {
        this.mod2pi = mod2pi;
    }

    public void setGains(double[] pid)
    {
        System.arraycopy(pid, 0, this.pid, 0, pid.length);
    }

    public void clearIntegral()
    {
        integral = 0;
    }

    public double compute(double dt, double target, double actual)
    {
        if (Double.compare(dt, 0) == 0)
        {
            return 0;
        }

        if (Math.signum(target) != Math.signum(previousTarget))
        {
            integral = 0;
        }

        double error = target - actual;
        if (mod2pi)
        {
            error = MathUtil.mod2pi(error);
        }
        integral = integral + error * dt;
        double derivative = (error - previousError) / dt;

        double output = pid[0] * error;
        output += pid[1] * integral;
        output += pid[2] * derivative;

        previousError = error;
        previousTarget = target;

        return output;
    }

}
