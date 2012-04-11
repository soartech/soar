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

import edu.umich.robot.events.feedback.AbstractFeedbackEvent;
import edu.umich.robot.laser.Laser;
import edu.umich.robot.metamap.AreaDescription;
import edu.umich.robot.metamap.AreaState;
import edu.umich.robot.metamap.Metamap;
import edu.umich.robot.metamap.VirtualObject;
import edu.umich.robot.radio.Radio;
import edu.umich.robot.util.Pose;
import edu.umich.robot.util.events.RobotEventListener;

/**
 * A read-only view in to the robot's current state.
 * 
 * @author voigtjr@gmail.com
 */
public interface RobotOutput
{
    /**
     * Get the robot's metamap.
     * 
     * @return
     */
    Metamap getMetamap();
    
    /**
     * Get the robot's current pose. At least position, orientation, and
     * velocities should all be elaborated.
     * 
     * @return
     */
    Pose getPose();

    /**
     * Get a list of objects within the virtual object sensor's field of view.
     * 
     * @return 
     */
    List<VirtualObject> getVisibleObjects();

    /**
     * Get laser sensor data.
     * 
     * @return
     */
    Laser getLaser();
    
    /**
     * 
     * @return Hi-res laser data
     */
    Laser getLaserHiRes();

    /**
     * Get the current area description. This is immutable definitions like room
     * dimensions, gateways, etc.
     * 
     * @return
     */
    AreaDescription getAreaDescription();
    
    /**
     * Get the current area state, this is stuff that can change.
     * 
     * @return
     */
    AreaState getAreaState();

    /**
     * Get an interface to the radio system to communicate with other bots and
     * the gui.
     * 
     * @return
     */
    Radio getRadio();

    /**
     * Get the object carried by the virtual object manipulator, or null if
     * nothing.
     * 
     * @return Null if nothing is carried.
     */
    VirtualObject getCarriedObject();
    
    /**
     * Register for feedback events from various actions the robot can take.
     * 
     * @param <T>
     * @param klass
     *            The event to listen for, or null for all events.
     * @param listener
     */
    <T extends AbstractFeedbackEvent> void addFeedbackEventListener(Class<T> klass, RobotEventListener listener);
    
    /**
     * Unregester for previously registered feedback events.
     * 
     * @param <T>
     * @param klass
     * @param listener
     */
    <T extends AbstractFeedbackEvent> void removeFeedbackEventListener(Class<T> klass, RobotEventListener listener);
    
    /**
     * Get immutable configuration information for the robot, such as limits and
     * geometry.
     * 
     * @return
     */
    RobotConfiguration getRobotConfiguration();
    
    /**
     * Get battery life expressed as a percentage remaining.
     * 
     * @return
     */
    double getBatteryLife();

    /**
     * Get the current headlight state.
     * 
     * @return Returns true if the headlight is on.
     */
    boolean isHeadlightOn();
    
}
