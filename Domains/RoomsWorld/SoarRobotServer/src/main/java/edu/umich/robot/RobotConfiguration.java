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
package edu.umich.robot;

import java.util.List;

import com.google.common.collect.ImmutableList;

/**
 * <p>
 * Information about the robot. Most of this isn't used by anything but it
 * should be set up at some point so that it can be used.
 * 
 * @author voigtjr@gmail.com
 */
public class RobotConfiguration
{
    /**
     * <p>
     * Used to create an immutable instance of RobotConfiguration. There are no
     * required parameters therefore there is no constructor.
     * 
     * @author voigtjr
     */
    public static class Builder
    {
        private double limitVelocityLinearMax = 0;

        private double limitVelocityLinearMin = 0;

        private double limitVelocityAngularMax = 0;

        private double limitVelocityAngularMin = 0;

        private double geometryLength = 0;
        
        private double geometryWidth = 0;
        
        private double geometryHeight = 0;
        
        private double geometryWheelbase = 0;
        
        private List<Double> gainsHeading = newPidList(new double[] {0, 0, 0});
        
        private List<Double> gainsAngular = newPidList(new double[] {0, 0, 0});
        
        private List<Double> gainsLinear = newPidList(new double[] {0, 0, 0});
        
        private double objectFieldOfView = 0;
        
        private double objectManipulationDistanceMin = 0;

        private double objectManipulationDistanceMax = 0;

        /**
         * Fastest it can go forward.
         * 
         * @param limitVelocityLinearMax
         * @return
         */
        public Builder limitVelocityLinearMax(double limitVelocityLinearMax)
        {
            this.limitVelocityLinearMax = limitVelocityLinearMax;
            return this;
        }

        /**
         * <p>
         * Fastest it can go backwards.
         * 
         * @param limitVelocityLinearMin
         * @return
         */
        public Builder limitVelocityLinearMin(double limitVelocityLinearMin)
        {
            this.limitVelocityLinearMin = limitVelocityLinearMin;
            return this;
        }

        /**
         * <p>
         * Fastest it can rotate to the left.
         * 
         * @param limitVelocityAngularMax
         * @return
         */
        public Builder limitVelocityAngularMax(double limitVelocityAngularMax)
        {
            this.limitVelocityAngularMax = limitVelocityAngularMax;
            return this;
        }

        /**
         * <p>
         * Fastest it can rotate to the right.
         * 
         * @param limitVelocityAngularMin
         * @return
         */
        public Builder limitVelocityAngularMin(double limitVelocityAngularMin)
        {
            this.limitVelocityAngularMin = limitVelocityAngularMin;
            return this;
        }

        /**
         * <p>
         * Length of the robot.
         * 
         * @param geometryLength
         * @return
         */
        public Builder geometryLength(double geometryLength)
        {
            this.geometryLength = geometryLength;
            return this;
        }
        
        /**
         * <p>
         * Width of the robot.
         * 
         * @param geometryWidth
         * @return
         */
        public Builder geometryWidth(double geometryWidth)
        {
            this.geometryWidth = geometryWidth;
            return this;
        }

        /**
         * <p>
         * Height of the robot.
         * 
         * @param geometryHeight
         * @return
         */
        public Builder geometryHeight(double geometryHeight)
        {
            this.geometryHeight = geometryHeight;
            return this;
        }

        /**
         * <p>
         * Distance between the centers of the wheels. This might not make sense
         * for some robots.
         * 
         * @param geometryWheelbase
         * @return
         */
        public Builder geometryWheelbase(double geometryWheelbase)
        {
            this.geometryWheelbase = geometryWheelbase;
            return this;
        }
        
        private static List<Double> newPidList(double[] pid)
        {
            return new ImmutableList.Builder<Double>()
            .add(pid[0])
            .add(pid[1])
            .add(pid[2])
            .build();
        }

        /**
         * <p>
         * PID gains for heading controller in array format [p, i, d].
         * 
         * @param pid
         * @return
         */
        public Builder gainsHeading(double[] pid)
        {
            this.gainsHeading = newPidList(pid);
            return this;
        }

        /**
         * <p>
         * PID gains for angular velocity controller in array format [p, i, d].
         * 
         * @param pid
         * @return
         */
        public Builder gainsAngular(double[] pid)
        {
            this.gainsAngular = newPidList(pid);
            return this;
        }

        /**
         * <p>
         * PID gains for linear velocity controller in array format [p, i, d].
         * 
         * @param pid
         * @return
         */
        public Builder gainsLinear(double[] pid)
        {
            this.gainsLinear = newPidList(pid);
            return this;
        }

        /**
         * <p>
         * Field of view for the virtual object sensor in radians.
         * 
         * @param objectFieldOfView
         * @return
         */
        public Builder objectFieldOfView(double objectFieldOfView)
        {
            this.objectFieldOfView = objectFieldOfView;
            return this;
        }

        /**
         * <p>
         * Minimum legal distance to manipulate objects, in meters.
         * 
         * @param objectManipulationDistanceMin
         * @return
         */
        public Builder objectManipulationDistanceMin( double objectManipulationDistanceMin)
        {
            this.objectManipulationDistanceMin = objectManipulationDistanceMin;
            return this;
        }

        /**
         * <p>
         * Maximum legal distance to manipulate objects, in meters.
         * 
         * @param objectManipulationDistanceMax
         * @return
         */
        public Builder objectManipulationDistanceMax( double objectManipulationDistanceMax)
        {
            this.objectManipulationDistanceMax = objectManipulationDistanceMax;
            return this;
        }

        public RobotConfiguration build()
        {
            return new RobotConfiguration(this);
        }
    }

    private final double limitVelocityLinearMax;

    private final double limitVelocityLinearMin;

    private final double limitVelocityAngularMax;

    private final double limitVelocityAngularMin;

    private final double geometryLength;
    
    private final double geometryWidth;
    
    private final double geometryHeight;
    
    private final double geometryWheelbase;
    
    private final List<Double> gainsHeading;
    
    private final List<Double> gainsAngular;
    
    private final List<Double> gainsLinear;
    
    private final double objectFieldOfView;
    
    private double objectManipulationDistanceMin;

    private double objectManipulationDistanceMax;

    private RobotConfiguration(Builder builder)
    {
        this.limitVelocityLinearMax = builder.limitVelocityLinearMax;
        this.limitVelocityLinearMin = builder.limitVelocityLinearMin;
        this.limitVelocityAngularMax = builder.limitVelocityAngularMax;
        this.limitVelocityAngularMin = builder.limitVelocityAngularMin;
        this.geometryLength = builder.geometryLength;
        this.geometryWidth = builder.geometryWidth;
        this.geometryHeight = builder.geometryHeight;
        this.geometryWheelbase = builder.geometryWheelbase;
        this.gainsHeading = builder.gainsHeading;
        this.gainsAngular = builder.gainsAngular;
        this.gainsLinear = builder.gainsLinear;
        this.objectFieldOfView = builder.objectFieldOfView;
        this.objectManipulationDistanceMin = builder.objectManipulationDistanceMin;
        this.objectManipulationDistanceMax = builder.objectManipulationDistanceMax;
    }

    /**
     * Fastest it can go forward.
     * 
     * @param limitVelocityLinearMax
     * @return
     */
    public double getLimitVelocityLinearMax()
    {
        return limitVelocityLinearMax;
    }

    /**
     * <p>
     * Fastest it can go backwards.
     * 
     * @param limitVelocityLinearMin
     * @return
     */
    public double getLimitVelocityLinearMin()
    {
        return limitVelocityLinearMin;
    }

    /**
     * <p>
     * Fastest it can rotate to the left.
     * 
     * @param limitVelocityAngularMax
     * @return
     */
    public double getLimitVelocityAngularMax()
    {
        return limitVelocityAngularMax;
    }

    /**
     * <p>
     * Fastest it can rotate to the right.
     * 
     * @param limitVelocityAngularMin
     * @return
     */
    public double getLimitVelocityAngularMin()
    {
        return limitVelocityAngularMin;
    }

    /**
     * <p>
     * Length of the robot.
     * 
     * @param geometryLength
     * @return
     */
    public double getGeometryLength()
    {
        return geometryLength;
    }

    /**
     * <p>
     * Width of the robot.
     * 
     * @param geometryWidth
     * @return
     */
    public double getGeometryWidth()
    {
        return geometryWidth;
    }

    /**
     * <p>
     * Height of the robot.
     * 
     * @param geometryHeight
     * @return
     */
    public double getGeometryHeight()
    {
        return geometryHeight;
    }

    /**
     * <p>
     * PID gains for heading controller in array format [p, i, d].
     * 
     * @param pid
     * @return
     */
    public List<Double> getGainsHeading()
    {
        return gainsHeading;
    }

    /**
     * <p>
     * PID gains for angular velocity controller in array format [p, i, d].
     * 
     * @param pid
     * @return
     */
    public List<Double> getGainsAngular()
    {
        return gainsAngular;
    }

    /**
     * <p>
     * PID gains for linear velocity controller in array format [p, i, d].
     * 
     * @param pid
     * @return
     */
    public List<Double> getGainsLinear()
    {
        return gainsLinear;
    }

    /**
     * <p>
     * Field of view for the virtual object sensor in radians.
     * 
     * @param objectFieldOfView
     * @return
     */
    public double getObjectFieldOfView()
    {
        return objectFieldOfView;
    }

    /**
     * <p>
     * Distance between the centers of the wheels. This might not make sense
     * for some robots.
     * 
     * @param geometryWheelbase
     * @return
     */
    public double getGeometryWheelbase()
    {
        return geometryWheelbase;
    }

    /**
     * <p>
     * Minimum legal distance to manipulate objects, in meters.
     * 
     * @param objectManipulationDistanceMin
     * @return
     */
    public double getObjectManipulationDistanceMin()
    {
        return objectManipulationDistanceMin;
    }

    /**
     * <p>
     * Maximum legal distance to manipulate objects, in meters.
     * 
     * @param objectManipulationDistanceMax
     * @return
     */
    public double getObjectManipulationDistanceMax()
    {
        return objectManipulationDistanceMax;
    }

}
