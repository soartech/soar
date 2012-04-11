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
package edu.umich.robot.soar;

import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

import sml.Identifier;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;

import edu.umich.robot.RobotConfiguration;
import edu.umich.robot.events.feedback.RobotConfigChangedEvent;
import edu.umich.robot.util.AngSpeedUnit;
import edu.umich.robot.util.Pose;
import edu.umich.robot.util.events.RobotEvent;
import edu.umich.robot.util.events.RobotEventListener;
import edu.umich.robot.util.properties.PropertyChangeEvent;
import edu.umich.robot.util.properties.PropertyListener;
import edu.umich.robot.util.properties.PropertyListenerHandle;
import edu.umich.robot.util.properties.PropertyManager;
import edu.umich.robot.util.soar.AngSpeedWme;
import edu.umich.robot.util.soar.DistanceWme;
import edu.umich.robot.util.soar.LinSpeedWme;
import edu.umich.robot.util.soar.YawWme;
import edu.umich.soar.FloatWme;
import edu.umich.soar.IntWme;
import edu.umich.soar.StringWme;

/**
 * Input link management for reporting current simulation configuration.
 * 
 * @author voigtjr@gmail.com
 */
public class ConfigurationIL extends InputLinkElement
{
    private final SoarAgent agent;

    private final List<PropertyListenerHandle<?>> propertyListeners = Lists.newArrayList();

    private final StringWme lengthUnits;

    private final StringWme angleUnits;

    private final StringWme angleResolution;

    private final StringWme linearSpeedUnits;

    private final StringWme angularSpeedUnits;

    private final List<DistanceWme> xyzPoseTranslation;

    private final List<FloatWme> headingPid;
    
    private final List<FloatWme> angularPid;
    
    private final List<FloatWme> linearPid;

    private final LinSpeedWme limitLinVelMax;

    private final LinSpeedWme limitLinVelMin;
    
    private final AngSpeedWme limitAngVelMax;
    
    private final AngSpeedWme limitAngVelMin;
    
    private final DistanceWme geomLength;
    
    private final DistanceWme geomWidth;
    
    private final DistanceWme geomHeight;
    
    private final DistanceWme geomWheelbase;
    
    private final YawWme fieldOfView;

    private final IntWme visibleTime;

    private final DistanceWme manipulationDistanceMin;
    
    private final DistanceWme manipulationDistanceMax;

    private final AtomicBoolean configChanged = new AtomicBoolean(true);
    
    private final PropertyManager properties;

    private static List<FloatWme> makePidList(Identifier root)
    {
        return new ImmutableList.Builder<FloatWme>()
        .add(FloatWme.newInstance(root, IOConstants.P))
        .add(FloatWme.newInstance(root, IOConstants.I))
        .add(FloatWme.newInstance(root, IOConstants.D))
        .build();
    }
    
    public ConfigurationIL(SoarAgent agent)
    {
        super(agent, IOConstants.CONFIGURATION, agent.getSoarAgent().GetInputLink());
        
        this.agent = agent;

        lengthUnits = StringWme .newInstance(getRoot(), IOConstants.LENGTH_UNITS);
        angleUnits = StringWme.newInstance(getRoot(), IOConstants.ANGLE_UNITS);
        angleResolution = StringWme.newInstance(getRoot(), IOConstants.ANGLE_RESOLUTION);
        linearSpeedUnits = StringWme.newInstance(getRoot(), IOConstants.LINEAR_SPEED_UNITS);
        angularSpeedUnits = StringWme.newInstance(getRoot(), IOConstants.ANGULAR_SPEED_UNITS);

        Identifier poseTranslation = getRoot().CreateIdWME(IOConstants.POSE_TRANSLATION);
        properties = agent.getProperties();
        xyzPoseTranslation = new ImmutableList.Builder<DistanceWme>()
        .add(DistanceWme.newInstance(poseTranslation, IOConstants.X, properties))
        .add(DistanceWme.newInstance(poseTranslation, IOConstants.Y, properties))
        .add(DistanceWme.newInstance(poseTranslation, IOConstants.Z, properties))
        .build();
        
        Identifier headingPidRoot = getRoot().CreateIdWME(IOConstants.HEADING);
        headingPid = makePidList(headingPidRoot);
        
        Identifier angularPidRoot = getRoot().CreateIdWME(IOConstants.ANGULAR);
        angularPid = makePidList(angularPidRoot);
        
        Identifier linearPidRoot = getRoot().CreateIdWME(IOConstants.LINEAR);
        linearPid = makePidList(linearPidRoot);
        
        visibleTime = IntWme.newInstance(getRoot(), IOConstants.VISIBLE_TIME);

        Identifier limits = getRoot().CreateIdWME(IOConstants.LIMITS);//
        Identifier velocity = limits.CreateIdWME(IOConstants.VELOCITY);//
        Identifier linear = velocity.CreateIdWME(IOConstants.LINEAR);//
        limitLinVelMax = LinSpeedWme.newInstance(linear, IOConstants.MAXIMUM, properties);
        limitLinVelMin = LinSpeedWme.newInstance(linear, IOConstants.MINIMUM, properties);
        
        Identifier angular = velocity.CreateIdWME(IOConstants.ANGULAR);
        limitAngVelMax = AngSpeedWme.newInstance(angular, IOConstants.MAXIMUM, properties);
        limitAngVelMin = AngSpeedWme.newInstance(angular, IOConstants.MINIMUM, properties);
        
        Identifier geometry = getRoot().CreateIdWME(IOConstants.GEOMETRY);//
        geomLength = DistanceWme.newInstance(geometry, IOConstants.LENGTH, properties);
        geomWidth = DistanceWme.newInstance(geometry, IOConstants.WIDTH, properties);
        geomHeight = DistanceWme.newInstance(geometry, IOConstants.HEIGHT, properties);
        geomWheelbase = DistanceWme.newInstance(geometry, IOConstants.WHEELBASE, properties);
        
        fieldOfView = YawWme.newInstance(getRoot(), IOConstants.FIELD_OF_VIEW, properties);
        manipulationDistanceMin = DistanceWme.newInstance(getRoot(), IOConstants.MANIPULATION_DISTANCE_MIN, properties);
        manipulationDistanceMax = DistanceWme.newInstance(getRoot(), IOConstants.MANIPULATION_DISTANCE_MAX, properties);

        update();
        
        agent.getRobotOutput().addFeedbackEventListener(RobotConfigChangedEvent.class, new RobotEventListener()
        {
            public void onEvent(RobotEvent event)
            {
                configChanged.set(true);
            }
        });

        // FIXME output.addConfigurationChangeListener(listener);
        propertyListeners.add(agent.getProperties().addListener(AgentProperties.OBJECT_LINGER_SECONDS, new ChangeListener<Integer>()));
        propertyListeners.add(agent.getProperties().addListener(AgentProperties.ANGULAR_SPEED_UNIT, new ChangeListener<AngSpeedUnit>()));
    }
    
    private class ChangeListener<T> implements PropertyListener<T>
    {
        public void propertyChanged(PropertyChangeEvent<T> event)
        {
            configChanged.set(true);
        }
    }

    @Override
    public void update()
    {
        if (configChanged.get())
        {
            lengthUnits.update(properties.get(AgentProperties.LENGTH_UNIT).toString().toLowerCase());
            angleUnits.update(properties.get(AgentProperties.ANGLE_UNIT).toString().toLowerCase());
            angleResolution.update(properties.get(AgentProperties.ANGLE_RESOLUTION).toString().toLowerCase());
            linearSpeedUnits.update(properties.get(AgentProperties.LINEAR_SPEED_UNIT).toString().toLowerCase());
            angularSpeedUnits.update(properties.get(AgentProperties.ANGULAR_SPEED_UNIT).toString().toLowerCase());
            
            Pose pt = properties.get(AgentProperties.TRANSLATION);
            
            RobotConfiguration c = agent.getRobotOutput().getRobotConfiguration();
            
            for (int i = 0; i < xyzPoseTranslation.size(); ++i)
            {
                xyzPoseTranslation.get(i).update(pt.getPos(i));
                headingPid.get(i).update(c.getGainsHeading().get(i));
                angularPid.get(i).update(c.getGainsAngular().get(i));
                linearPid.get(i).update(c.getGainsLinear().get(i));
            }
            
            limitLinVelMax.update(c.getLimitVelocityLinearMax());
            limitLinVelMin.update(c.getLimitVelocityLinearMin());
            limitAngVelMax.update(c.getLimitVelocityAngularMax());
            limitAngVelMin.update(c.getLimitVelocityAngularMin());

            geomHeight.update(c.getGeometryHeight());
            geomWidth.update(c.getGeometryWidth());
            geomLength.update(c.getGeometryLength());
            geomWheelbase.update(c.getGeometryWheelbase());
            
            fieldOfView.update(c.getObjectFieldOfView());
            visibleTime.update(properties.get(AgentProperties.OBJECT_LINGER_SECONDS));
            manipulationDistanceMax.update(c.getObjectManipulationDistanceMax());
            manipulationDistanceMin.update(c.getObjectManipulationDistanceMin());
            
            configChanged.set(false);
        }
    }

    @Override
    public void destroy()
    {
        super.destroy();
        
        for (PropertyListenerHandle<?> h : propertyListeners)
        {
            h.removeListener();
        }
    }

}
