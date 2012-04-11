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

import java.util.Arrays;
import java.util.List;

import april.jmat.LinAlg;
import april.jmat.MathUtil;
import april.lcmtypes.pose_t;

import com.google.common.collect.Lists;


/**
 * @author voigtjr@gmail.com
 */
public class Pose
{
    private final List<Double> pos;

    private final List<Double> vel;

    private final List<Double> orientation;

    private final List<Double> rotationRate;

    private final List<Double> accel;

    public Pose()
    {
        pos = Arrays.asList(new Double[] { 0.0, 0.0, 0.0 });
        vel = Arrays.asList(new Double[] { 0.0, 0.0, 0.0 });
        orientation = Arrays.asList(new Double[] { 0.0, 0.0, 0.0, 0.0 });
        rotationRate = Arrays.asList(new Double[] { 0.0, 0.0, 0.0 });
        accel = Arrays.asList(new Double[] { 0.0, 0.0, 0.0 });
    }

    public Pose(double[] pos)
    {
        if (pos.length == 2)
            this.pos = Lists.newArrayList(pos[0], pos[1], 0.0);
        else if (pos.length >= 3)
            this.pos = Misc.fromPrimitiveArray(pos);
        else
            throw new IllegalArgumentException();
        
        vel = Arrays.asList(new Double[] { 0.0, 0.0, 0.0 });
        orientation = Arrays.asList(new Double[] { 0.0, 0.0, 0.0, 0.0 });
        rotationRate = Arrays.asList(new Double[] { 0.0, 0.0, 0.0 });
        accel = Arrays.asList(new Double[] { 0.0, 0.0, 0.0 });
    }

    public Pose(List<Double> pos)
    {
        this(Misc.toPrimitiveDoubleArray(pos));
    }

    public Pose(Pose copy)
    {
        pos = Lists.newArrayList(copy.pos);
        vel = Lists.newArrayList(copy.vel);
        orientation = Lists.newArrayList(copy.orientation);
        rotationRate = Lists.newArrayList(copy.rotationRate);
        accel = Lists.newArrayList(copy.accel);
    }

    public Pose(pose_t pose)
    {
        pos = Misc.fromPrimitiveArray(pose.pos);
        vel = Misc.fromPrimitiveArray(pose.vel);
        orientation = Misc.fromPrimitiveArray(pose.orientation);
        rotationRate = Misc.fromPrimitiveArray(pose.rotation_rate);
        accel = Misc.fromPrimitiveArray(pose.accel);
    }

    public Pose copy()
    {
        return new Pose(this);
    }

    public double getPos(int i)
    {
        return pos.get(i);
    }
    
    public double getX()
    {
        return pos.get(0);
    }

    public Pose setX(double x)
    {
        pos.set(0, x);
        return this;
    }

    public double getY()
    {
        return pos.get(1);
    }

    public Pose setY(double y)
    {
        pos.set(1, y);
        return this;
    }

    public double getZ()
    {
        return pos.get(2);
    }

    public Pose setZ(double z)
    {
        pos.set(2, z);
        return this;
    }

    public List<Double> getPos()
    {
        return pos;
    }

    public Pose setPos(double x, double y, double z)
    {
        setX(x);
        setY(y);
        setZ(z);
        return this;
    }

    public double getVX()
    {
        return vel.get(0);
    }

    public Pose setVX(double x)
    {
        vel.set(0, x);
        return this;
    }

    public double getVY()
    {
        return vel.get(1);
    }

    public Pose setVY(double y)
    {
        vel.set(1, y);
        return this;
    }

    public double getVZ()
    {
        return vel.get(2);
    }

    public Pose setVZ(double z)
    {
        vel.set(2, z);
        return this;
    }

    public List<Double> getVel()
    {
        return vel;
    }

    public Pose setVel(double vx, double vy, double vz)
    {
        setVX(vx);
        setVY(vy);
        setVZ(vz);
        return this;
    }

    public List<Double> getOrientation()
    {
        return orientation;
    }

    public List<Double> getRotationRate()
    {
        return rotationRate;
    }

    public List<Double> getAccel()
    {
        return accel;
    }

    public double getYaw()
    {
        return LinAlg.quatToRollPitchYaw(Misc.toPrimitiveDoubleArray(orientation))[2];
    }

    public Pose setYaw(double yaw)
    {
        double[] array = LinAlg
                .rollPitchYawToQuat(new double[] { 0.0, 0.0, yaw });
        for (int i = 0; i < array.length; ++i)
            orientation.set(i, array[i]);
        return this;
    }
    
    public double getDistance(Pose other)
    {
        double[] x = Misc.toPrimitiveDoubleArray(pos);
        double[] y = Misc.toPrimitiveDoubleArray(other.getPos());

        return LinAlg.distance(x, y);
    }

    public RelativePose getRelative(Pose other)
    {
        double[] x = Misc.toPrimitiveDoubleArray(pos);
        double[] y = Misc.toPrimitiveDoubleArray(other.getPos());

        double distance = LinAlg.distance(x, y);

        double[] delta = LinAlg.subtract(y, x);
        double yaw = Math.atan2(delta[1], delta[0]);

        double relativeYaw = yaw - getYaw();
        relativeYaw = MathUtil.mod2pi(relativeYaw);

        return new RelativePose(distance, yaw, relativeYaw);
    }

    public pose_t asLcmType()
    {
        pose_t ret = new pose_t();
        ret.pos = Misc.toPrimitiveDoubleArray(pos);
        ret.vel = Misc.toPrimitiveDoubleArray(vel);
        ret.orientation = Misc.toPrimitiveDoubleArray(orientation);
        ret.rotation_rate = Misc.toPrimitiveDoubleArray(rotationRate);
        ret.accel = Misc.toPrimitiveDoubleArray(accel);
        return ret;
    }

    public Pose translate(Pose pose)
    {
        setX(getX() + pose.getX());
        setY(getY() + pose.getY());
        setZ(getZ() + pose.getZ());
        return this;
    }

    @Override
    public String toString()
    {
        StringBuilder sb = new StringBuilder("[pose ");
        sb.append(String.format("%.2f,%.2f,%.2f;%.2f,%.2f,%.2f;%.2f", getX(), getY(), getZ(), getVX(), getVY(), getVZ(), getYaw()));
        sb.append("]");
        return sb.toString();
    }
}
