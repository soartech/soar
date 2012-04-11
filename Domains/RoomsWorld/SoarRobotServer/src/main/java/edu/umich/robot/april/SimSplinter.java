package edu.umich.robot.april;

import java.io.IOException;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import lcm.lcm.LCM;
import lcm.lcm.LCMDataInputStream;
import lcm.lcm.LCMSubscriber;
import april.config.Config;
import april.jmat.LinAlg;
import april.lcmtypes.pose_t;
import april.util.TimeUtil;
import edu.umich.robot.lcmtypes.differential_drive_command_t;

// Subscribes:
//   POSE_TELEPORT
//   DIFFERENTIAL_DRIVE_COMMAND
//
// Publishes:
//   POSE_TRUTH
//   POSE  (noisy)
/** Simulates a Splinter robot. **/
public class SimSplinter implements SoarSimObject, TimeScalable
{
    public static final int      HZ         = 60; // was 200
    Config                       config;
    LCM                          lcm        = LCM.getSingleton();
    SimMotor                     leftMotor  = new SimMotor();
    SimMotor                     rightMotor = new SimMotor();
    pose_t                       pose_truth = new pose_t();
    pose_t                       pose_odom  = new pose_t();
    differential_drive_command_t driveCommand;
    //Simulator                    sim;
    SoarSimulator                    sim;
    SimWorld2D                   simworld;
    String                       name;
    RunThread			 thread;
    String			 driveChannel;
    String                       poseChannel;
    String                       encoderChannel;
    ScheduledExecutorService schexec = Executors.newSingleThreadScheduledExecutor();
    ScheduledFuture<?> task;
    final int PERIOD = 1000 / HZ;
    long last = 0;
    differential_drive_command_t totalMeters = new differential_drive_command_t();
    boolean wallCollisions = true;

    public SimSplinter(SoarSimulator sim, String name, Config config)
    {
        this.sim = sim;
        this.config = config;
        this.simworld = sim.simworld;
        this.name = name;
        
        totalMeters.left = 0;
        totalMeters.left_enabled = true;
        totalMeters.right = 0;
        totalMeters.right_enabled = true;
        
        this.wallCollisions = config.getBoolean("wallCollisions", true);
        
        double[] pos = config.getDoubles("initialPosition");
        if (pos != null) {
            System.arraycopy(pos, 0, pose_truth.pos, 0, 3);
            System.arraycopy(pos, 0, pose_odom.pos, 0, 3);
        }
        this.driveChannel = "DIFFERENTIAL_DRIVE_COMMAND_" + name;
        System.out.println("SimSplinter: " + name + " on channel index " + name);
        
        this.poseChannel = "POSE_" + name;
        this.encoderChannel = "ENCODER_" + name;
        
        pose_truth.orientation = new double[] { 1, 0, 0, 0 };
        pose_odom.orientation = new double[] { 1, 0, 0, 0 };
        this.thread = new RunThread(driveChannel);
        task = schexec.scheduleAtFixedRate(this.thread, 0, PERIOD, TimeUnit.MILLISECONDS);
        
        setTeleportActive(true);
    }

    synchronized void update(double dt)
    {
        if (driveCommand != null)
        {
            leftMotor.setVoltage(driveCommand.left * 12);
            rightMotor.setVoltage(driveCommand.right * 12);
        }
        leftMotor.update(dt);
        rightMotor.update(dt);
        double left_rad_per_sec = leftMotor.getRadPerSec();
        double right_rad_per_sec = rightMotor.getRadPerSec();
        double wheel_diameter = 0.25;
        double baseline = 0.35;
                
        double dleft = dt * left_rad_per_sec * wheel_diameter;
        totalMeters.left += dleft;
        
        double dright = dt * right_rad_per_sec * wheel_diameter;
        totalMeters.right += dright;
        
        double dl = (dleft + dright) / 2;
        double dtheta = (dright - dleft) / baseline;
        double dpos[] = LinAlg.quatRotate(pose_truth.orientation, new double[] { dl, 0, 0 });
        double dquat[] = LinAlg.rollPitchYawToQuat(new double[] { 0, 0, dtheta });
        double newpos[] = LinAlg.add(pose_truth.pos, dpos);
        // if collisions are off or it's not a collision
        if (!wallCollisions || !simworld.isCollision(newpos) )
            pose_truth.pos = newpos;
        pose_truth.orientation = LinAlg.quatMultiply(pose_truth.orientation, dquat);
        
        pose_truth.utime = TimeUtil.utime();
        pose_odom.utime = pose_truth.utime;
        totalMeters.utime = pose_truth.utime;

        pose_odom.pos = LinAlg.copy(pose_truth.pos);
        pose_odom.orientation = LinAlg.copy(pose_truth.orientation);
        
        lcm.publish(poseChannel, pose_odom);
        lcm.publish(encoderChannel, totalMeters);
    }
    
    public void setTeleportActive(boolean setting) 
    {
	if (setting)
            lcm.subscribe("POSE_TELEPORT", thread);
	else
            lcm.unsubscribe("POSE_TELEPORT", thread);
    }

    class RunThread implements LCMSubscriber, Runnable
    {
        RunThread(String driveChannel)
        {
            lcm.subscribe(driveChannel, this);
        }

        public void run()
        {
            if (last == 0)
                last = TimeUtil.utime();
            else
            {
                long t = TimeUtil.utime();
                update((t - last) / 1000000.0);
                last = t;
            }
        }

        public void messageReceived(LCM lcm, String channel, LCMDataInputStream ins)
        {
            try
            {
                messageReceivedEx(lcm, channel, ins);
            } catch (IOException ex)
            {
                System.out.println("ex: " + ex);
            }
        }

        public void messageReceivedEx(LCM lcm, String channel, LCMDataInputStream ins) throws IOException
        {
            if (channel.equals(driveChannel))
            {
                driveCommand = new differential_drive_command_t(ins);
            }
            if (channel.equals("POSE_TELEPORT"))
            {
                synchronized (SimSplinter.this)
                {
                    pose_truth = new pose_t(ins);
                    pose_odom = pose_truth.copy();
                }
            }
        }
    }

    @Override
    public void setTimeScale(int rate)
    {
        task.cancel(false);
        task = schexec.scheduleAtFixedRate(thread, 0, PERIOD / rate, TimeUnit.MILLISECONDS);
    }

    @Override
    public void dispose()
    {
        task.cancel(true);
        schexec.shutdown();
    }
}
