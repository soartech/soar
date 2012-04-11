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

import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import april.util.TimeUtil;

import com.google.common.util.concurrent.MoreExecutors;

import edu.umich.robot.Robot;
import edu.umich.robot.RobotConfiguration;
import edu.umich.robot.RobotOutput;
import edu.umich.robot.RobotType;
import edu.umich.robot.events.control.AbstractControlEvent;
import edu.umich.robot.events.control.AbstractDriveEvent;
import edu.umich.robot.events.control.AbstractEffectorEvent;
import edu.umich.robot.events.control.DoorCloseEvent;
import edu.umich.robot.events.control.DoorLockEvent;
import edu.umich.robot.events.control.DoorOpenEvent;
import edu.umich.robot.events.control.DoorUnlockEvent;
import edu.umich.robot.events.control.DriveAngularEvent;
import edu.umich.robot.events.control.DriveEStopEvent;
import edu.umich.robot.events.control.DriveHeadingAndLinearEvent;
import edu.umich.robot.events.control.DriveHeadingEvent;
import edu.umich.robot.events.control.DriveLinearEvent;
import edu.umich.robot.events.control.DriveThrottleEvent;
import edu.umich.robot.events.control.EffectorCancelEvent;
import edu.umich.robot.events.control.EffectorDiffuseObjectByWireEvent;
import edu.umich.robot.events.control.EffectorDiffuseObjectEvent;
import edu.umich.robot.events.control.EffectorDropObjectEvent;
import edu.umich.robot.events.control.EffectorGetObjectEvent;
import edu.umich.robot.events.control.SetHeadLightEvent;
import edu.umich.robot.events.control.SetRoomLightEvent;
import edu.umich.robot.events.feedback.AbstractEffectorFeedbackEvent;
import edu.umich.robot.events.feedback.AbstractFeedbackEvent;
import edu.umich.robot.events.feedback.EffectorFailureEvent;
import edu.umich.robot.events.feedback.EffectorSuccessEvent;
import edu.umich.robot.events.feedback.RobotConfigChangedEvent;
import edu.umich.robot.laser.Laser;
import edu.umich.robot.laser.Lidar;
import edu.umich.robot.metamap.AreaDescription;
import edu.umich.robot.metamap.AreaState;
import edu.umich.robot.metamap.Metamap;
import edu.umich.robot.metamap.VirtualObject;
import edu.umich.robot.radio.Radio;
import edu.umich.robot.util.HeadingController;
import edu.umich.robot.util.PIDController;
import edu.umich.robot.util.Pose;
import edu.umich.robot.util.PoseProvider;
import edu.umich.robot.util.SimBattery;
import edu.umich.robot.util.Updatable;
import edu.umich.robot.util.VelocitiesController;
import edu.umich.robot.util.events.RobotEventListener;
import edu.umich.robot.util.events.RobotEventManager;
import edu.umich.robot.util.properties.PropertyChangeEvent;
import edu.umich.robot.util.properties.PropertyListener;
import edu.umich.robot.util.properties.PropertyManager;

/**
 * @author voigtjr@gmail.com
 */
public class Splinter implements Robot
{
    private final Radio radio;

    private final PropertyManager properties = new PropertyManager();

    private final PoseProvider pose;

    private final PIDController apid = new PIDController();

    private final PIDController lpid = new PIDController();

    private final PIDController hpid = new PIDController();

    private final SplinterDrive drive;

    private final VelocitiesController velocities;

    private final HeadingController heading;

    private final ScheduledExecutorService schexec = MoreExecutors.getExitingScheduledExecutorService(new ScheduledThreadPoolExecutor(1));

    private Updatable controller;

    private long last;

    private final Metamap metamap;
    
    private final Lidar lidar;
    
    private final RobotEventManager events = new RobotEventManager();

    private final SimBattery battery = new SimBattery(10, 0.7);
    
    private final AtomicBoolean headlightOn = new AtomicBoolean(false);
    
    private ScheduledFuture<?> commandTask;
    
    private final int PERIOD = 33;
    
    public Splinter(String name, Radio radio, Metamap metamap)
    {
        this.radio = radio;
        this.metamap = metamap;
        this.lidar = new Lidar(name, false, 5, Math.PI);

        battery.setMilliAmpCurrent("control board", 1); // made up number
        
        properties.set(SplinterProperties.NAME, name);
        pose = new SplinterPose(name);

        initGains();

        drive = new SplinterDrive(name, battery);
        velocities = new SplinterVelocities(drive, pose, apid, lpid);
        heading = new HeadingController(velocities, pose, hpid);

        commandTask = schexec.scheduleAtFixedRate(command, 0, PERIOD, TimeUnit.MILLISECONDS);
    }

    private void initGains()
    {
        properties.addListener(SplinterProperties.ANGULAR_PID_GAINS,
                new PropertyListener<double[]>()
                {
                    public void propertyChanged(
                            PropertyChangeEvent<double[]> event)
                    {
                        apid.setGains(event.getNewValue());
                        events.fireEvent(new RobotConfigChangedEvent(output.getRobotConfiguration()));
                    }
                });
        apid.setGains(properties.get(SplinterProperties.ANGULAR_PID_GAINS));

        properties.addListener(SplinterProperties.LINEAR_PID_GAINS,
                new PropertyListener<double[]>()
                {
                    public void propertyChanged(
                            PropertyChangeEvent<double[]> event)
                    {
                        lpid.setGains(event.getNewValue());
                        events.fireEvent(new RobotConfigChangedEvent(output.getRobotConfiguration()));
                    }
                });
        lpid.setGains(properties.get(SplinterProperties.LINEAR_PID_GAINS));

        properties.addListener(SplinterProperties.HEADING_PID_GAINS,
                new PropertyListener<double[]>()
                {
                    public void propertyChanged(
                            PropertyChangeEvent<double[]> event)
                    {
                        hpid.setGains(event.getNewValue());
                        events.fireEvent(new RobotConfigChangedEvent(output.getRobotConfiguration()));
                    }
                });
        hpid.setGains(properties.get(SplinterProperties.HEADING_PID_GAINS));
    }

    private final Runnable command = new Runnable()
    {
        public void run()
        {
            synchronized (this)
            {
                if (controller != null)
                {
                    long now = TimeUtil.utime();
                    if (last != 0)
                    {
                        double dt = (now - last) / (double)TimeUnit.MICROSECONDS.convert(1, TimeUnit.SECONDS);
                        controller.update(dt);
                    }
                    last = now;
                }
            }
        }
    };
    
    public RobotType getType()
    {
        return RobotType.SPLINTER;
    }
    
    public RobotOutput getOutput()
    {
        return output;
    }

    private final RobotOutput output = new RobotOutput()
    {
        public Metamap getMetamap() {
            return metamap;
        }
        
        public AreaDescription getAreaDescription()
        {
            return metamap.getArea(getPose());
        }

        public AreaState getAreaState()
        {
            AreaDescription ad = getAreaDescription();
            if (ad != null)
                return metamap.getAreaState(ad.getId());
            return null;
        }
        
        public Laser getLaser()
        {
            return lidar.getLaserLowRes();
        }
        
        public Laser getLaserHiRes()
        {
            return lidar.getLaserHiRes();
        }
        
        public Pose getPose()
        {
            return new Pose(pose.getPose());
        }

        public Radio getRadio()
        {
            return radio;
        }

        public List<VirtualObject> getVisibleObjects()
        {
            return metamap.getVisibleObjects(Splinter.this);
        }

        public VirtualObject getCarriedObject()
        {
            return metamap.getCarried(Splinter.this);
        }

        public RobotConfiguration getRobotConfiguration()
        {
            return new RobotConfiguration.Builder()
            .gainsHeading(properties.get(SplinterProperties.HEADING_PID_GAINS))
            .gainsAngular(properties.get(SplinterProperties.ANGULAR_PID_GAINS))
            .gainsLinear(properties.get(SplinterProperties.LINEAR_PID_GAINS))
            .geometryWidth(.4)
            .geometryLength(.7)
            .build();
        }

        public <T extends AbstractFeedbackEvent> void addFeedbackEventListener(
                Class<T> klass, RobotEventListener listener)
        {
            events.addListener(klass, listener);
        }

        public <T extends AbstractFeedbackEvent> void removeFeedbackEventListener(
                Class<T> klass, RobotEventListener listener)
        {
            events.removeListener(klass, listener);
        }

        public double getBatteryLife()
        {
            return battery.getRemainingLife();
        }

        public boolean isHeadlightOn()
        {
            return headlightOn.get();
        }

    };

    public void handleControlEvent(AbstractControlEvent event)
    {
        if (event instanceof AbstractDriveEvent)
        {
            if (event instanceof DriveEStopEvent)
            {
                estop();
            }
            else if (event instanceof DriveThrottleEvent)
            {
                DriveThrottleEvent d = (DriveThrottleEvent)event;
                setDrive(d.getLeft(), d.getRight());
            }
            else if (event instanceof DriveLinearEvent)
            {
                DriveLinearEvent d = (DriveLinearEvent)event;
                setLinear(d.getLinearVelocity());
            }
            else if (event instanceof DriveAngularEvent)
            {
                DriveAngularEvent d = (DriveAngularEvent)event;
                setAngular(d.getAngularVelocity());
            }
            else if (event instanceof DriveHeadingAndLinearEvent)
            {
                DriveHeadingAndLinearEvent d = (DriveHeadingAndLinearEvent)event;
                setHeadingAndLinear(d.getHeadingRadians(), d.getLinearVelocity());
            }
            else if (event instanceof DriveHeadingEvent)
            {
                DriveHeadingEvent d = (DriveHeadingEvent)event;
                setHeading(d.getHeadingRadians());
            }
            else 
                throw new UnsupportedOperationException("Splinter does not support " + event);
        } 
        else if (event instanceof AbstractEffectorEvent)
        {
            if (event instanceof EffectorCancelEvent)
            {
                boolean result = metamap.cancelEffector(this);
                fireEffectorResultEvent(result, (EffectorCancelEvent)event);
            }
            else if (event instanceof EffectorGetObjectEvent)
            {
                EffectorGetObjectEvent e = (EffectorGetObjectEvent)event;
                boolean result = metamap.pickupObject(this, e.getId());
                fireEffectorResultEvent(result, e);
            }
            else if (event instanceof EffectorDropObjectEvent)
            {
                boolean result = metamap.dropObject(this);
                fireEffectorResultEvent(result, (EffectorDropObjectEvent)event);
            }
            else if (event instanceof EffectorDiffuseObjectEvent)
            {
                EffectorDiffuseObjectEvent e = (EffectorDiffuseObjectEvent)event;
                boolean result = metamap.diffuseObject(this, e.getId());
                fireEffectorResultEvent(result, e);
            }
            else if (event instanceof EffectorDiffuseObjectByWireEvent)
            {
                EffectorDiffuseObjectByWireEvent e = (EffectorDiffuseObjectByWireEvent)event;
                boolean result = metamap.diffuseObjectByWire(this, e.getId(), e.getColor());
                fireEffectorResultEvent(result, e);
            } 
            else 
            {
                throw new UnsupportedOperationException("Splinter does not support " + event);
            }
        }
        else if (event instanceof SetRoomLightEvent)
        {
            AreaDescription ad = metamap.getArea(output.getPose());
            if (ad != null)
            {
                SetRoomLightEvent e = (SetRoomLightEvent)event;
                metamap.setRoomLight(ad.getId(), e.isOn());
            }
        }
        else if (event instanceof SetHeadLightEvent)
        {
            SetHeadLightEvent e = (SetHeadLightEvent)event;
            setHeadlight(e.isOn());
        }
        else if (event instanceof DoorOpenEvent)
        {
            DoorOpenEvent e = (DoorOpenEvent)event;
            metamap.doorOpen(this, e.getId());
        }
        else if (event instanceof DoorCloseEvent)
        {
            DoorCloseEvent e = (DoorCloseEvent)event;
            metamap.doorClose(this, e.getId());
        }
        else if (event instanceof DoorUnlockEvent)
        {
            DoorUnlockEvent e = (DoorUnlockEvent)event;
            metamap.doorUnlock(this, e.getId(), e.getCode());
        }
        else if (event instanceof DoorLockEvent)
        {
            DoorLockEvent e = (DoorLockEvent)event;
            metamap.doorLock(this, e.getId(), e.getCode());
        }
        else
        {
            throw new UnsupportedOperationException("Splinter does not support " + event);
        }
    }
    
    private void fireEffectorResultEvent(boolean result, AbstractEffectorEvent e)
    {
        if (result)
            events.fireEvent(new EffectorSuccessEvent(e), AbstractEffectorFeedbackEvent.class);
        else
            events.fireEvent(new EffectorFailureEvent(e), AbstractEffectorFeedbackEvent.class);
    }
    
    private void setHeadlight(boolean on)
    {
        synchronized (this)
        {
            boolean old = headlightOn.getAndSet(on);
            if (!old && on)
                battery.setMilliAmpCurrent("headlight", 5); // made up number
            else if (old && !on)
                battery.removeMilliAmpCurrent("headlight");
        }
    }
    
    private void estop()
    {
        synchronized (this)
        {
            if (controller != drive)
                controller = drive;
            drive.setDrive(0, 0);
        }
    }

    private void setDrive(double left, double right)
    {
        synchronized (this)
        {
            if (controller != drive)
                controller = drive;
            drive.setDrive(left, right);
        }
    }

    private void setAngular(double av)
    {
        synchronized (this)
        {
            if (controller != velocities)
            {
                velocities.reset();
                controller = velocities;
            }
            velocities.setAngular(av);
        }
    }

    private void setHeading(double radians)
    {
        synchronized (this)
        {
            if (controller != heading)
            {
                heading.reset();
                controller = heading;
            }
            heading.setHeading(radians);
        }
    }

    private void setHeadingAndLinear(double radians, double lv)
    {
        synchronized (this)
        {
            if (controller != heading)
            {
                heading.reset();
                controller = heading;
            }
            heading.setHeadingAndLinear(radians, lv);
        }
    }

    private void setLinear(double lv)
    {
        synchronized (this)
        {
            if (controller != velocities)
            {
                velocities.reset();
                controller = velocities;
            }
            velocities.setLinear(lv);
        }
    }

    public String getName()
    {
        return properties.get(SplinterProperties.NAME);
    }

    @Override
    public String toString()
    {
        return "Splinter: " + getName();
    }

    public void setTimeScale(int multiplier)
    {
        commandTask.cancel(false);
        schexec.scheduleAtFixedRate(command, 0, PERIOD / multiplier, TimeUnit.MILLISECONDS);
    }

    public void shutdown()
    {
        lidar.shutdown();
        schexec.shutdown();
        battery.shutdown();
    }

}
