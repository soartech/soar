package edu.umich.robot.superdroid;

import java.io.IOException;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import lcm.lcm.LCM;
import lcm.lcm.LCMDataInputStream;
import lcm.lcm.LCMSubscriber;
import april.jmat.LinAlg;
import april.lcmtypes.pose_t;
import april.util.TimeUtil;
import edu.umich.grrc.GrrcSuperdroid;
import edu.umich.robot.lcmtypes.differential_drive_command_t;

public class SuperdroidHardware
{
    private final GrrcSuperdroid sd;

    private final LCM lcm = LCM.getSingleton();
    
    private final ScheduledExecutorService schexec = Executors.newSingleThreadScheduledExecutor();

    private final double[] rpy = { 0, 0, 0 };
    
    private final String poseChannel;
    
    private final double[] posoffset = { 0, 0, 0 };
    
    private pose_t teleport;
    
    private double thetaOffset = Double.MAX_VALUE;
    
    public SuperdroidHardware(String name, String hostname, int port, double[] offset) throws UnknownHostException, SocketException
    {
        teleport = new pose_t();
        teleport.pos = Arrays.copyOf(offset, offset.length);
        poseChannel = SuperdroidPose.POSE_CHANNEL_BASE + name;
        final String velChannel = SuperdroidVelocities.VELOCITIES_CHANNEL_BASE + name;
        
        lcm.subscribe(velChannel, new LCMSubscriber()
        {
            public void messageReceived(LCM lcm, String channel,
                    LCMDataInputStream ins)
            {
                try
                {
                    differential_drive_command_t vels = new differential_drive_command_t(ins);
                    sd.setVelocities((float)vels.left, (float)vels.right);
                }
                catch (IOException e)
                {
                    e.printStackTrace();
                }
            }
        });
        
        lcm.subscribe("POSE_TELEPORT", new LCMSubscriber()
        {
            @Override
            public void messageReceived(LCM lcm, String channel, LCMDataInputStream ins)
            {
                try
                {
                    teleport = new pose_t(ins);
                }
                catch (IOException e)
                {
                    e.printStackTrace();
                }
            }
        });
        
        sd = new GrrcSuperdroid(hostname, port);

        schexec.scheduleAtFixedRate(update, 0, (long)100, TimeUnit.MILLISECONDS);
    }
    
    private Runnable update = new Runnable()
    {
        public void run()
        {
            pose_t pose = new pose_t();
            rpy[2] = sd.getTheta();
            if (!sd.getPos(pose.pos))
                return;
            
            // not so simple
            //if (thetaOffset == Double.MAX_VALUE)
            //    thetaOffset = 0 - rpy[2];
            //rpy[2] += thetaOffset;
            
            if (teleport != null)
            {
                System.out.println("Pose: " + Arrays.toString(pose.pos));
                System.out.println("Tele: " + Arrays.toString(teleport.pos));
                LinAlg.subtract(teleport.pos, pose.pos, posoffset);
                System.out.println("Offs: " + Arrays.toString(posoffset));
                teleport = null;
            }
            LinAlg.add(pose.pos, posoffset, pose.pos);
            pose.orientation = LinAlg.rollPitchYawToQuat(rpy);
            pose.utime = TimeUtil.utime();
            lcm.publish(poseChannel, pose);
        }
    };

    public void shutdown()
    {
        sd.shutdown();
        schexec.shutdown();
    }

}
