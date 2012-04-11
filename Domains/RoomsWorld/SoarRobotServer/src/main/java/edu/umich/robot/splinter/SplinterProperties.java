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

import edu.umich.robot.util.properties.PropertyKey;

/**
 * @author voigtjr@gmail.com
 */
public class SplinterProperties
{
    private static final String SPLINTER_PREFIX = "splinter.";

    private static <T> PropertyKey.Builder<T> key(String name, Class<T> type)
    {
        return PropertyKey.builder(SPLINTER_PREFIX + name, type);
    }

    public static PropertyKey<String> NAME = 
        key("name", String.class)
        .build();

    public static PropertyKey<double[]> ANGULAR_PID_GAINS = 
        key("angular-pid-gains", double[].class)
        .defaultValue( new double[] { 0.0238, 0, 0.0025 })
        //.defaultValue( new double[] { 0.15, 0, 0.01 })
        .build();

    public static PropertyKey<double[]> LINEAR_PID_GAINS = 
        key("linear-pid-gains", double[].class)
        .defaultValue( new double[] { 0.12, 0, 0.025 })
        .build();

    public static PropertyKey<double[]> HEADING_PID_GAINS = 
        key("heading-pid-gains", double[].class)
        .defaultValue( new double[] { 0.6, 0, 0.125 })
        //.defaultValue( new double[] { 1.8, 0, 0.125 })
        .build();

    public static PropertyKey<String> HOSTNAME =
        key("hostname", String.class)
        .defaultValue("192.168.237.7")
        .build();
    
    public static PropertyKey<Integer> LEFT_MOTOR_PORT = 
        key("left-motor-port", Integer.class)
        .defaultValue(0)
        .build();
    
    public static PropertyKey<Boolean> LEFT_MOTOR_INVERT = 
        key("left-motor-invert", Boolean.class)
        .defaultValue(true)
        .build();
    
    public static PropertyKey<Integer> RIGHT_MOTOR_PORT = 
        key("right-motor-port", Integer.class)
        .defaultValue(0)
        .build();
    
    public static PropertyKey<Boolean> RIGHT_MOTOR_INVERT = 
        key("right-motor-invert", Boolean.class)
        .defaultValue(false)
        .build();
    
    public static PropertyKey<Integer> MILLIS_BEFORE_WARN_NO_INPUT = 
        key("millis-before-warn-no-input", Integer.class)
        .defaultValue(5000)
        .build();
    
    public static PropertyKey<Double> BASELINE = 
        key("millis-before-warn-no-input", Double.class)
        .defaultValue(0.383)
        .build();
    
    public static PropertyKey<Double> ENCODERRATE = 
        key("millis-before-warn-no-input", Double.class)
        .defaultValue(0.000043225)
        .build();
    
    public static PropertyKey<Double> MAX_THROTTLE_ACCEL =
        key("max-throttle-accel", Double.class)
        .defaultValue(2.0)
        .build();
    
    public static PropertyKey<Double> UPDATE_HZ =
        key("update-hz", Double.class)
        .defaultValue(30.0)
        .build();
    
}
