package edu.umich.robot.april;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.Timer;

import lcm.lcm.LCM;
import lcm.lcm.LCMDataInputStream;
import lcm.lcm.LCMSubscriber;
import april.config.Config;
import april.sim.SimObject;
import edu.umich.robot.lcmtypes.sim_obstacles_t;

/** GUI for a robot simulator. **/
public class SoarSimulator implements LCMSubscriber
{
    Config               config;
    ConfigUtil           cutil;
    BufferedImage        image;
    double               metersPerPixel;
    Timer                timer      = new Timer(true);
    LCM                  lcm        = LCM.getSingleton();
    SimWorld2D           simworld;
    ArrayList<SoarSimObject> simObjects = new ArrayList<SoarSimObject>();

    public static void main(String args[])
    {
        new SoarSimulator(ConfigUtil.getDefaultConfig(args));
    }

    public SoarSimulator(Config _config)
    {
        this.config = _config.getChild("simulator");
        // ////////////////////////////////////////////////
        // Set up the SimWorld
        simworld = new SimWorld2D();
        String image_path = config.getPath("obstacles.image_path");
        if (image_path != null)
        {
            try
            {
                BufferedImage im = javax.imageio.ImageIO.read(new File(image_path));
                double meters_per_pixel = config.requireDouble("obstacles.meters_per_pixel");
                System.out.printf("(Image %s, %f meters per pixel)\n", image_path, meters_per_pixel);
                simworld.setImage(im, config.getDoubles("obstacles.image_origin", null), meters_per_pixel);
            } catch (IOException ex)
            {
                System.out.println("Couldn't read " + image_path);
            }
        }
        // ////////////////////////////////////////////////
        // Create SimObjects.
        String simobjects[] = config.getStrings("simobjects", new String[0]);
        for (String simobject : simobjects)
        {
            Config childConfig = config.getChild(simobject);
            addObject(simobject, childConfig);
        }
        // ////////////////////////////////////////////////
        // Virtual obstacles managed outside of the simulation
        lcm.subscribe("SIM_OBSTACLES", this);
        /*
         * String channels[] = config.getStrings("simulator.lasers");
         * 
         * double laser_hz = config.getDouble("simulator.laser_hz", 20); double laser_degree0 = config.getDouble("simulator.laser_degree0", -90); double laser_degreeStep = config.getDouble("simulator.laser_degreeStep", 1); int laser_nranges = config.getInt("simulator.laser_nranges", 180); double laser_max_range_m = config.getDouble("simulator.laser_max_range_m", 30); double laser_range_noise_m =
         * config.getDouble("simulator.laser_range_noise_m", 0.01); double laser_theta_noise_degrees = config.getDouble("simulator.laser_theta_noise_degrees", 0.1);
         * 
         * if (channels != null) { System.out.println("Creating lasers:");
         * 
         * 
         * for (int i = 0; i < channels.length; i++) { System.out.printf("%20s", channels[i]);
         * 
         * SimLaser lasersim = new SimLaser(simworld, Math.toRadians(laser_degree0), Math.toRadians(laser_degreeStep), laser_nranges, laser_max_range_m, laser_range_noise_m, laser_theta_noise_degrees);
         * 
         * timer.schedule(new LaserScanTask(channels[i], lasersim), 0, (int) (1000.0/laser_hz)); } System.out.printf("\n"); }
         * 
         * SimSplinter splinter = new SimSplinter(config, simworld);
         */
    }

    public SoarSimObject addObject(String name, Config config)
    {
        String className = config.requireString("class");
        try
        {
            Class<?> cls = Class.forName(className);
            Constructor<?> cons = cls.getConstructor(SoarSimulator.class, String.class, Config.class);
            SoarSimObject o = (SoarSimObject) cons.newInstance(this, name, config);
            simObjects.add(o);
            return o;
        } catch (Exception ex)
        {
            System.out.println("Simulator: Unable to create " + name + ": " + ex);
            return null;
        }
    }
    
    public void removeObject(SoarSimObject target)
    {
        if (simObjects.remove(target))
            target.dispose();
    }
    
    public void shutdown()
    {
        timer.cancel();
        timer.purge();
        for (SoarSimObject o : simObjects)
            o.dispose();
    }
    
    public void messageReceived(LCM lcm, String channel, LCMDataInputStream ins)
    {
        try
        {
            messageReceivedEx(lcm, channel, ins);
        } catch (IOException ex)
        {
        }
    }

    void messageReceivedEx(LCM lcm, String channel, LCMDataInputStream ins) throws IOException
    {
        if (channel.equals("SIM_OBSTACLES"))
        {
            sim_obstacles_t simobst = new sim_obstacles_t(ins);
            ArrayList<double[]> rects = new ArrayList<double[]>();
            for (double[] r : simobst.rects)
                rects.add(r);
            simworld.setRects(rects);
        }
    }
    /*
     * class LaserScanTask extends TimerTask { String channel; SimLaser lasersim; PoseTracker ptTruth = PoseTracker.getTruthSingleton();
     * 
     * public LaserScanTask(String channel, SimLaser lasersim) { this.channel = channel; this.lasersim = lasersim; }
     * 
     * public synchronized void run() { pose_t pose_truth = ptTruth.get();
     * 
     * if (pose_truth == null) return;
     * 
     * // sensor position in robot frame double spos[] = ConfigUtil.getPosition(config, channel); double squat[] = ConfigUtil.getQuaternion(config, channel);
     * 
     * // Where is the sensor in local coordinates double quat[] = LinAlg.quatMultiply(pose_truth.orientation, squat); double pos[] = LinAlg.add(pose_truth.pos, LinAlg.quatRotate(pose_truth.orientation, spos));
     * 
     * float ranges[] = lasersim.scan(quat, pos);
     * 
     * laser_t ldata = new laser_t(); ldata.utime = pose_truth.utime; //TimeUtil.utime(); ldata.nranges = ranges.length; ldata.ranges = ranges; ldata.nintensities = 0; ldata.rad0 = (float) lasersim.rad0; ldata.radstep = (float) lasersim.radStep;
     * 
     * lcm.publish(channel, ldata); } }
     */
}
