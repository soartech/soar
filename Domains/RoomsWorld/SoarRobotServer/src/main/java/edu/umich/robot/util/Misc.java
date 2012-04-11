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

import java.util.List;

import com.google.common.collect.Lists;

/**
 * @author voigtjr@gmail.com
 */
public class Misc
{
    public static double clamp(double value, double min, double max)
    {
        value = Math.max(value, min);
        value = Math.min(value, max);
        return value;
    }

    public static int[] toPrimitiveIntArray(List<Integer> l)
    {
        int[] ret = new int[l.size()];
        for (int i = 0; i < l.size(); ++i)
            ret[i] = l.get(i);
        return ret;
    }
    
    public static double[] toPrimitiveDoubleArray(List<Double> l)
    {
        double[] ret = new double[l.size()];
        for (int i = 0; i < l.size(); ++i)
            ret[i] = l.get(i);
        return ret;
    }
    
    public static float[] toPrimitiveArray(List<Float> l)
    {
        float[] ret = new float[l.size()];
        for (int i = 0; i < l.size(); ++i)
            ret[i] = l.get(i);
        return ret;
    }
    
    public static List<Integer> fromPrimitiveArray(int[] a)
    {
        List<Integer> ret = Lists.newArrayList();
        for (int d : a)
            ret.add(d);
        return ret;
    }
        
    public static List<Double> fromPrimitiveArray(double[] a)
    {
        List<Double> ret = Lists.newArrayList();
        for (double d : a)
            ret.add(d);
        return ret;
    }
    
    public static List<Float> fromPrimitiveArray(float[] a)
    {
        List<Float> ret = Lists.newArrayList();
        for (float f : a)
            ret.add(f);
        return ret;
    }
    
    public static String constantCaseToLowerDashes(String constant)
    {
        StringBuilder s = new StringBuilder(constant.toLowerCase().replaceAll("_","-"));
        return s.toString();
    }

    public static String lowerDashesToConstantCase(String lower)
    {
        StringBuilder s = new StringBuilder(lower.toUpperCase().replaceAll("-","_"));
        return s.toString();
    }
    
}
