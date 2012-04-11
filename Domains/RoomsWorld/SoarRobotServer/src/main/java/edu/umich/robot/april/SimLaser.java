package edu.umich.robot.april;

import java.util.ArrayList;
import java.util.Random;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import lcm.lcm.LCM;
import april.config.Config;
import april.jmat.LinAlg;
import april.lcmtypes.laser_t;
import april.lcmtypes.pose_t;
import april.util.PoseTracker;

/** Simulates a planar laser range finder (like a SICK) **/
public class SimLaser implements SoarSimObject
{
    public Random r                 = new Random();
    double        rad0, radStep;
    double        maxRange;
    double        rangeNoise;                            // in meters, std. dev.
    double        thetaNoise;                            // in degrees, std. dev.
    double        quantizationNoise = 0.0;               // in meters.
    int           nranges;
    double        hz;
    SoarSimulator     sim;
    SimWorld2D    simworld;
    String        name;
    Config        config;
    LCM           lcm               = LCM.getSingleton();
    PoseTracker   pt;
    String	  channel;
    ScheduledExecutorService schexec = Executors.newSingleThreadScheduledExecutor();
    ScheduledFuture<?> task;

    public SimLaser(SoarSimulator sim, String name, Config config)
    {
        this.sim = sim;
        this.simworld = sim.simworld;
        this.name = name;
        this.config = config;
        this.rad0 = Math.toRadians(config.getDouble("degree0", -90));
        this.radStep = Math.toRadians(config.getDouble("degree_step", 1));
        this.nranges = config.getInt("nranges", 180);
        this.rangeNoise = config.getDouble("range_noise_m", 0.01);
        this.thetaNoise = Math.toRadians(config.getDouble("theta_noise_degrees", 0.01));
        this.maxRange = config.getDouble("max_range_m", 30);
        this.hz = config.getDouble("hz", 10);
        this.pt = new PoseTracker("POSE_" + config.getString("pose"), 10.0);
        this.channel = "SIM_LIDAR_FRONT_" + config.getString("pose");
        
        int millis = (int)Math.round(1000.0 / hz);
        task = schexec.scheduleAtFixedRate(new LaserScanTask(), 0, millis, TimeUnit.MILLISECONDS);
    }

    /** if filter, points at maximum range are removed. **/
    public ArrayList<double[]> scanPoints(double xyt[], boolean filter)
    {
        float ranges[] = scan(xyt);
        ArrayList<double[]> points = new ArrayList<double[]>();
        for (int i = 0; i < ranges.length; i++)
        {
            double theta = rad0 + radStep * i;
            if (ranges[i] >= maxRange)
                continue;
            points.add(new double[] { Math.cos(theta) * ranges[i], Math.sin(theta) * ranges[i] });
        }
        return points;
    }

    public float[] scan(double quat[], double pos[])
    {
        double rpy[] = LinAlg.quatToRollPitchYaw(quat);
        return scan(new double[] { pos[0], pos[1], rpy[2] });
    }

    /** XXX BUG: We only consider yaw, not pitch/roll. **/
    public float[] scan(double xyt[])
    {
        assert (xyt.length == 3);
//        double mx = xyt[0];
//        double my = xyt[1];
//        double mtheta = xyt[2];
        float ranges[] = new float[nranges];
        for (int nrange = 0; nrange < nranges; nrange++)
        {
            // theta of the ray we're about to cast
            double theta = rad0 + nrange * radStep + xyt[2];
            theta += Math.toRadians(r.nextGaussian() * thetaNoise);
            // how much do we move in each direction each iteration?
            double dx = Math.cos(theta);
            double dy = Math.sin(theta);
            
            double rangeMeters = simworld.findCollision(xyt, new double[] { dx, dy }, maxRange);
            
            rangeMeters += r.nextGaussian() * rangeNoise;
            if (quantizationNoise > 0)
            {
                int quantum = (int) (rangeMeters / quantizationNoise);
                rangeMeters = quantum * quantizationNoise;
            }
            if (rangeMeters < 0)
                rangeMeters = 0;
            if (rangeMeters > maxRange)
                rangeMeters = maxRange;
            ranges[nrange] = (float) rangeMeters;
        }
        return ranges;
    }

    class LaserScanTask implements Runnable
    {
        //PoseTracker ptTruth = PoseTracker.getTruthSingleton();
	
        public synchronized void run()
        {
            pose_t pose = pt.get();
            if (pose == null)
                return;
            // sensor position in robot frame
            double spos[] = config.requireDoubles("position");
            double squat[] = ConfigUtil.getQuaternion(config);
            // Where is the sensor in local coordinates
            double quat[] = LinAlg.quatMultiply(pose.orientation, squat);
            double pos[] = LinAlg.add(pose.pos, LinAlg.quatRotate(pose.orientation, spos));
            float ranges[] = scan(quat, pos);
            laser_t ldata = new laser_t();
            ldata.utime = pose.utime; // TimeUtil.utime();
            ldata.nranges = ranges.length;
            ldata.ranges = ranges;
            ldata.nintensities = 0;
            ldata.rad0 = (float) rad0;
            ldata.radstep = (float) radStep;
            lcm.publish(channel, ldata);
        }
    }

    @Override
    public void dispose()
    {
        task.cancel(true);
        schexec.shutdown();
        pt.dispose();
    }
}
