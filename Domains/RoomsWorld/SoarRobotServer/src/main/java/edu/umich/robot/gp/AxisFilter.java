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
package edu.umich.robot.gp;

/**
 * @author voigtjr@gmail.com
 */
public class AxisFilter
{
    private float deadZonePercent;

    private boolean initialized;

    private float minValue;

    private float maxValue;

    public AxisFilter(float deadZonePercent)
    {
        this(deadZonePercent, 0, 0);
        initialized = false;
    }

    public AxisFilter(float deadZonePercent, float minValue, float maxValue)
    {
        setDeadZonePercent(deadZonePercent);
        this.minValue = minValue;
        this.maxValue = maxValue;
        initialized = true;
    }

    public void setDeadZonePercent(float deadZonePercent)
    {
        if (Float.compare(deadZonePercent, 0) < 0)
            throw new IllegalArgumentException("Dead zone must be positive");

        this.deadZonePercent = deadZonePercent;
    }

    public float getMinValue()
    {
        return minValue;
    }

    public float getMaxValue()
    {
        return maxValue;
    }

    public float filter(float value)
    {
        if (initialized)
        {
            minValue = Math.min(minValue, value);
            maxValue = Math.max(maxValue, value);
        }
        else
        {
            minValue = value;
            maxValue = value;
            initialized = true;
        }

        // System.out.format("min:%2.3f, max:%2.3f%n", minValue, maxValue);

        // normalize to -1..1

        // if we have no range, return zero
        float range = maxValue - minValue;
        if (Float.compare(range, 0) == 0)
        {
            return 0;
        }

        // determine percentage of range
        float pct = (value - minValue) / range;

        // apply dead zone
        if (pct < 0.5f - deadZonePercent)
        {
            pct /= 0.5f - deadZonePercent;
            pct = 1.0f - pct;
            pct *= -1;
        }
        else
        {
            pct -= 0.5f + deadZonePercent;
            if (pct > 0)
            {
                pct /= 0.5f - deadZonePercent;
            }
            else
            {
                pct = 0;
            }
        }

        return pct;
    }
}
