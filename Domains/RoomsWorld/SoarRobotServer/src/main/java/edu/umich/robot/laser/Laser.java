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
package edu.umich.robot.laser;

import java.util.List;

import april.lcmtypes.laser_t;
import april.util.TimeUtil;

import com.google.common.collect.ImmutableList;

import edu.umich.robot.util.Misc;

/**
 * This represents a laser reading. Convertable to laser_t.
 * 
 * @author voigtjr@gmail.com
 */
public class Laser
{
    
    public static class Builder {
        ImmutableList.Builder<Float> ranges = new ImmutableList.Builder<Float>();
        private final float rad0;
        private final float radstep;
        
        public Builder(float rad0, float radstep)
        {
            this.rad0 = rad0;
            this.radstep = radstep;
        }
        
        public Builder add(float range)
        {
            this.ranges.add(Float.valueOf(range));
            return this;
        }
        
        public Builder addRanges(float[] ranges)
        {
            for (float range : ranges)
                this.ranges.add(Float.valueOf(range));
            return this;
        }
        
        public Laser build()
        {
            return new Laser(this);
        }
    }
    
    private List<Float> ranges;
    private final float rad0;
    private final float radstep;

    private Laser(Builder builder)
    {
        this.rad0 = builder.rad0;
        this.radstep = builder.radstep;
        this.ranges = builder.ranges.build();
    }
    
    /**
     * Array of ranges, angle of range = rad0 + index * radstep
     * 
     * @return
     */
    public List<Float> getRanges()
    {
        return ranges;
    }

    /**
     * Minimum (first) laser reading angle.
     * @return
     */
    public float getRad0()
    {
        return rad0;
    }

    /**
     * Angle between readings.
     * 
     * @return
     */
    public float getRadStep()
    {
        return radstep;
    }
    
    public laser_t toLcm()
    {
        laser_t ret = new laser_t();
        ret.utime = TimeUtil.utime();
        ret.rad0 = rad0;
        ret.radstep = radstep;
        ret.nranges = ranges.size();
        ret.ranges = Misc.toPrimitiveArray(ranges);
        return ret;
    }
}
