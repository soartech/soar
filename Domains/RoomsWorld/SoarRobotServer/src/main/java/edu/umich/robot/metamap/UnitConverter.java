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
package edu.umich.robot.metamap;

import java.util.List;

import com.google.common.collect.Lists;

/**
 * Convert coordinates from the map to meters. Deals with the flipping of the y
 * axis.
 * 
 * @author voigtjr@gmail.com
 */
public class UnitConverter
{
    private final double[] origin;

    private final double scale;

    public UnitConverter(int[] origin, double scale)
    {
        this.origin = new double[] { origin[0] * scale, origin[1] * scale };
        // pixels
        // ->
        // meters
        this.scale = scale;
    }

    public List<Double> getMeters_xywh(int[] xywh)
    {
        if (xywh.length < 4)
            return Lists.newArrayList(0.0, 0.0, 0.0, 0.0);
        
        return Lists.newArrayList(
                (xywh[0] * scale) - origin[0],
                -((xywh[1] * scale) + (xywh[3] * scale) - origin[1]),
                xywh[2] * scale, 
                xywh[3] * scale
                );
    }

    public List<Double> getMeters_xy(int[] xy)
    {
        if (xy.length < 2)
            return Lists.newArrayList(0.0, 0.0);
        
        return Lists.newArrayList(
                (xy[0] * scale) - origin[0],
                -((xy[1] * scale) - origin[1]));
    }

    public List<Double> getMeters_wh(int[] wh)
    {
        if (wh.length < 2)
            return Lists.newArrayList(0.0, 0.0);
        
        return Lists.newArrayList(wh[0] * scale, wh[1] * scale);
    }

}
