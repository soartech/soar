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

import java.util.Collections;
import java.util.List;

import april.jmat.LinAlg;

/**
 * @author voigtjr@gmail.com
 */
public class ImmutablePose extends Pose
{
    public static ImmutablePose newInstance(Pose p)
    {
        return new ImmutablePose(p);
    }

    public static ImmutablePose new2DInstance(double x, double y)
    {
        Pose p = new Pose();
        p.setPos(x, y, 0);
        return ImmutablePose.newInstance(p);
    }

    private ImmutablePose(Pose p)
    {
        super(p);
    }

    @Override
    public Pose setX(double x)
    {
        throw new UnsupportedOperationException("Immutable pose");
    }

    @Override
    public Pose setY(double y)
    {
        throw new UnsupportedOperationException("Immutable pose");
    }

    @Override
    public Pose setZ(double z)
    {
        throw new UnsupportedOperationException("Immutable pose");
    }

    @Override
    public List<Double> getPos()
    {
        return Collections.unmodifiableList(super.getPos());
    }

    @Override
    public List<Double> getVel()
    {
        return Collections.unmodifiableList(super.getVel());
    }

    @Override
    public List<Double> getOrientation()
    {
        return Collections.unmodifiableList(super.getOrientation());
    }

    @Override
    public List<Double> getRotationRate()
    {
        return Collections.unmodifiableList(super.getRotationRate());
    }

    @Override
    public List<Double> getAccel()
    {
        return Collections.unmodifiableList(super.getAccel());
    }

    @Override
    public double getYaw()
    {
        return LinAlg.quatToRollPitchYaw(Misc
                .toPrimitiveDoubleArray(getOrientation()))[2];
    }

    @Override
    public Pose setYaw(double yaw)
    {
        throw new UnsupportedOperationException("Immutable pose");
    }

}
