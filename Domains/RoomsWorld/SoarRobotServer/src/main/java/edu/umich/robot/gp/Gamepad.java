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

import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import april.jmat.LinAlg;
import april.lcmtypes.pose_t;

import com.google.common.util.concurrent.MoreExecutors;

import edu.umich.robot.Controller;
import edu.umich.robot.RobotOutput;
import edu.umich.robot.events.control.AbstractDriveEvent;
import edu.umich.robot.events.control.DoorCloseEvent;
import edu.umich.robot.events.control.DoorOpenEvent;
import edu.umich.robot.events.control.DoorUnlockEvent;
import edu.umich.robot.events.control.DriveAngularEvent;
import edu.umich.robot.events.control.DriveHeadingAndLinearEvent;
import edu.umich.robot.events.control.DriveLinearEvent;
import edu.umich.robot.events.control.DriveThrottleEvent;
import edu.umich.robot.events.control.EffectorDropObjectEvent;
import edu.umich.robot.events.control.EffectorGetObjectEvent;
import edu.umich.robot.metamap.Door;
import edu.umich.robot.metamap.Gateway;
import edu.umich.robot.metamap.VirtualObject;

/**
 * @author voigtjr@gmail.com
 */
public class Gamepad
{
    private static final Log logger = LogFactory.getLog(Gamepad.class);

    private final GamepadJI gpji;

    private GamepadInputScheme gpInputScheme = GamepadInputScheme.JOY_VELOCITIES;

    private boolean slow = false;

    private double ly;

    private double rx;

    private double ry;

    private RobotOutput output;
    
    private AbstractDriveEvent lastEvent;
    
    private enum GamepadInputScheme
    {
        JOY_MOTOR, // left: off, right x: turn component, right y: forward
        TANK, // left y: left motor, right y: right motor
        JOY_VELOCITIES, // left: off, right x: linvel, right y: angvel
        GAS_AND_WHEEL, // left y: linvel, right: heading, right center: angvel 0
    }

    private final ScheduledExecutorService schexec = MoreExecutors.getExitingScheduledExecutorService(new ScheduledThreadPoolExecutor(1));
    
    public Gamepad()
    {
        this.gpji = GamepadJI.newInstance();
        if (gpji == null)
            return;
        
        schexec.scheduleAtFixedRate(new Runnable() {
            public void run()
            {
                if (output != null)
                {
                    logger.debug("Robot: " + output.getPose());
                    AbstractDriveEvent e = lastEvent;
                    if (e != null)
                        logger.debug("Last event: " + e);
                }
            }
        }, 1, 1, TimeUnit.SECONDS);
    }

    public void initializeGamepad(final Controller controller)
    {
        if (gpji == null)
            return;
        gpji.addListener("0", new GamepadListener()
        {
            public void stateChanged(String id, float value)
            {
                if (logger.isTraceEnabled())
                    logger.trace(id + ": " + value);
                if (Float.compare(value, 0) != 0)
                    controller.toggleGamepadOverride();
            }
        });

        gpji.addListener("1", new GamepadListener()
        {
            public void stateChanged(String id, float value)
            {
                if (logger.isTraceEnabled())
                    logger.trace(id + ": " + value);
                if (Float.compare(value, 0) != 0)
                    controller.toggleSoarRunState();
            }
        });

        gpji.addListener("6", new GamepadListener()
        {
            public void stateChanged(String id, float value)
            {
                if (logger.isTraceEnabled())
                    logger.trace(id + ": " + value);
                if (Float.compare(value, 0) != 0)
                    controller.toggleRate();
            }
        });

        gpji.addListener("3", new GamepadListener()
        {
            public void stateChanged(String id, float value)
            {
                if (logger.isTraceEnabled())
                    logger.trace(id + ": " + value);
                if (Float.compare(value, 0) != 0)
                    slow = !slow;
            }
        });

        gpji.addListener("2", new GamepadListener()
        {
            public void stateChanged(String id, float value)
            {
                if (logger.isTraceEnabled())
                    logger.trace(id + ": " + value);
                if (Float.compare(value, 0) != 0)
                {
                    int index = gpInputScheme.ordinal() + 1;
                    index %= GamepadInputScheme.values().length;
                    gpInputScheme = GamepadInputScheme.values()[index];
                    logger.info("Input changed to " + gpInputScheme);
                }
            }
        });
        
        gpji.addListener("5", new GamepadListener()
        {
            public void stateChanged(String id, float value)
            {
                if (Float.compare(value, 0) != 0)
                {
                    if (output == null)
                    {
                        logger.info("Get/Drop object called: no robot output");
                        return;
                    }
                    
                    if (output.getCarriedObject() != null)
                        controller.fireGamepadControlEvent(new EffectorDropObjectEvent());
                    else
                    {
                        VirtualObject target = null;
                        double targetDistance = Double.MAX_VALUE;
                        
                        for (VirtualObject vo : output.getVisibleObjects())
                        {
                            pose_t me = output.getPose().asLcmType();
                            pose_t targetPose = vo.getPose().asLcmType();
                            double distance = LinAlg.squaredDistance(me.pos, targetPose.pos);
                            
                            if (target == null || distance < targetDistance)
                            {
                                target = vo;
                                targetDistance = distance;
                            }
                        }
                        if (target == null)
                            logger.warn("No objects to get.");
                        else
                            controller.fireGamepadControlEvent(new EffectorGetObjectEvent(target.getId()));
                    }
                }
            }
        });

        gpji.addListener("4", new GamepadListener()
        {
            public void stateChanged(String id, float value)
            {
                if (Float.compare(value, 0) != 0)
                {
                    if (output == null)
                    {
                        logger.info("Open/close/unlock door called: no robot output");
                        return;
                    }
                    
                    Gateway target = null;
                    double targetDistance = Double.MAX_VALUE;
                    
                    for (Gateway g : output.getAreaDescription().getGateways())
                    {
                        pose_t me = output.getPose().asLcmType();
                        pose_t targetPose = g.getPose().asLcmType();
                        double distance = LinAlg.squaredDistance(me.pos, targetPose.pos);
                        
                        if (target == null || distance < targetDistance)
                        {
                            target = g;
                            targetDistance = distance;
                        }
                    }
                    
                    if (target == null)
                        logger.warn("No door to manipulate.");
                    else
                    {
                        Door door = target.getDoor();
                        switch (door.getState())
                        {
                        case CLOSED:
                            controller.fireGamepadControlEvent(new DoorOpenEvent(door.getId()));
                            break;
                            
                        case LOCKED:
                            controller.fireGamepadControlEvent(new DoorUnlockEvent(door.getId(), door.getCode()));
                            break;
                            
                        case OPEN:
                            controller.fireGamepadControlEvent(new DoorCloseEvent(door.getId()));
                            break;
                        }
                        controller.fireGamepadControlEvent(new EffectorGetObjectEvent(target.getId()));
                    }
                }
            }
        });

        gpji.addListener("LY", new GamepadListener()
        {
            public void stateChanged(String id, float value)
            {
                if (logger.isTraceEnabled())
                    logger.trace(id + ": " + value);
                ly = value;
                update(controller);
            }
        });
        gpji.addListener("RX", new GamepadListener()
        {
            public void stateChanged(String id, float value)
            {
                if (logger.isTraceEnabled())
                    logger.trace(id + ": " + value);
                rx = value;
                update(controller);
            }
        });
        gpji.addListener("RY", new GamepadListener()
        {
            public void stateChanged(String id, float value)
            {
                if (logger.isTraceEnabled())
                    logger.trace(id + ": " + value);
                ry = value;
                update(controller);
            }
        });
    }

    private void update(final Controller controller)
    {
        double left = 0;
        double right = 0;
        double angularVelocity = 0;
        double linearVelocity = 0;

        switch (gpInputScheme)
        {
        case JOY_MOTOR:
            // this should not be linear, it is difficult to precisely
            // control
            double fwd = -1 * ry; // +1 = forward, -1 = back
            double lr = -1 * rx; // +1 = left, -1 = right

            left = fwd - lr;
            right = fwd + lr;

            double max = Math.max(Math.abs(left), Math.abs(right));
            if (max > 1)
            {
                left /= max;
                right /= max;
            }

            if (slow)
            {
                left *= 0.5;
                right *= 0.5;
            }
            controller .fireGamepadControlEvent(new DriveThrottleEvent(left, right));
            break;

        case TANK:
            left = ly * -1;
            right = ry * -1;

            if (slow)
            {
                left *= 0.5;
                right *= 0.5;
            }
            controller .fireGamepadControlEvent(new DriveThrottleEvent(left, right));
            break;

        case JOY_VELOCITIES:
            angularVelocity = rx * -1;
            linearVelocity = ry * -1;
            controller.fireGamepadControlEvent(new DriveLinearEvent( linearVelocity));
            controller.fireGamepadControlEvent(new DriveAngularEvent( angularVelocity));
            break;

        case GAS_AND_WHEEL:
            linearVelocity = ly * -1;
            if (LinAlg.magnitude(new double[] { rx, ry }) < 0.5)
            {
                lastEvent = new DriveLinearEvent(linearVelocity);
                controller.fireGamepadControlEvent(new DriveAngularEvent(0));
            }
            else
            {
                double heading = Math.atan2(ry * -1, rx);
                lastEvent = new DriveHeadingAndLinearEvent( heading, linearVelocity);
            }
            controller.fireGamepadControlEvent(lastEvent);
            break;
        }
    }

    public void setDeadZonePercent(String component, float deadZonePercent)
    {
        if (gpji == null)
            return;
        gpji.setDeadZonePercent(component, deadZonePercent);
    }

    public void shutdown()
    {
        schexec.shutdown();
        if (gpji == null)
            return;
        gpji.shutdown();
    }

    public void setRobotOutput(RobotOutput output)
    {
        this.output = output;
    }

}
