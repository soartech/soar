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
import java.util.Map;

import edu.umich.robot.util.Pose;

/**
 * Everything a virtual object needs to do.
 * 
 * @author voigtjr@gmail.com
 */
public interface VirtualObject
{
    /**
     * Globally unique identification number, unique among instances.
     * @return
     */
    public int getId();
    
    /**
     * A "class" name of the object, unique among other classes, many instaces
     * can share the same name.
     * 
     * @return
     */
    public String getName();

    /**
     * Where the object is, null if carried and not placed.
     * 
     * @return
     */
    public Pose getPose();

    /**
     * Size in x, y.
     * 
     * @return
     */
    public List<Double> getSize();

    /**
     * Arbitrary property list.
     * 
     * @return
     */
    public Map<String, String> getProperties();
}
