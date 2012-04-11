package edu.umich.robot.laser;

import java.io.IOException;
import java.util.List;

import lcm.lcm.LCM;
import lcm.lcm.LCMDataInputStream;
import lcm.lcm.LCMSubscriber;
import april.lcmtypes.laser_t;
import april.util.TimeUtil;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;

import edu.umich.robot.laser.Lidar.Binner;
import edu.umich.robot.lcmtypes.urg_range_t;

/**
 * Bins values from the Hokuyo URG sensor. TODO: refactor with Sick, they share
 * a lot of code.
 * 
 * @author voigtjr
 * 
 */
public class UrgBinner implements Binner
{
    private final LCM lcm = LCM.getSingleton();
    
    private final int nbins;
    
    private final List<Float> binned = Lists.newArrayList();
    
    private urg_range_t urg;
    
    private long last = -1;
    
    private final double fov0;
    
    private final double fovstep;
    
    private Lidar lidar = null;
    
    /**
     * @param channel The channel the URG is sending data to.
     * @param bins How many bins to use.
     * @param fov Field of view.
     */
    public UrgBinner(String channel, int bins, double fov)
    {
        if (bins <= 0 || fov <= 0)
            throw new IllegalArgumentException("bins or fov less than zero");
        
        if (fov > Math.PI * 2)
            throw new IllegalArgumentException("fov > 2pi");
        
        this.nbins = bins;
        this.fov0 = fov / -2;
        this.fovstep = fov / bins;
        
        for (int i = 0; i < bins; ++i)
            binned.add(Float.valueOf(0));
        
        lcm.subscribe(channel, subscriber);
    }
    
    public void setLidar(Lidar lidar)
    {
        this.lidar = lidar;
    }
    
    private final LCMSubscriber subscriber = new LCMSubscriber()
    {
        @Override
        public void messageReceived(LCM lcm, String channel, LCMDataInputStream ins)
        {
            try 
            {
                urg = new urg_range_t(ins);
                if (lidar != null)
                {
                    lidar.lidarChanged(UrgBinner.this);
                }
            }
            catch (IOException e)
            {
                e.printStackTrace();
            }
        }
    };

    /**
     * Must call this to update state, otherwise values will not change. This is
     * for performance reasons.
     */
    public void update()
    {
        update(urg);
    }
    
    /**
     * Takes a reference and bins it. The reference in the constructor is a copy
     * of the class member so it will not change as new ranges come in off of
     * lcm. This is "poor man's" lcm synchronization.
     * 
     * @param urg
     */
    void update(urg_range_t urg)
    {
        // shadowed this.urg on purpose

        if (urg == null)
            return;
        
        if (last >= urg.utime)
            return;
        last = urg.utime;
        
        double theta = urg.rad0;
        double nextbin = fov0;
        int bin = -1;
        float value = Float.MAX_VALUE;
        
        for (int i = 0; i < urg.num_ranges; ++i, theta += urg.radstep)
        {
            if (theta >= nextbin)
            {
                if (bin >= 0)
                {
                    binned.set(bin, value);
                    value = Float.MAX_VALUE;
                }
                
                nextbin += fovstep;
                bin += 1;
                if (bin >= nbins)
                    break;
            }
            
            if (bin < 0)
                continue;
            
            if (urg.ranges[i] < 0)
                continue;
            
            value = Math.min(value, urg.ranges[i]);
        }
        
        if (bin >= 0 && bin < nbins)
            binned.set(bin, value);
    }
    
    /**
     * Returns a list of ranges, angle = rad0 + index * radstep
     * 
     * @return
     */
    public List<Float> getBinned()
    {
        if (last == -1)
            return null;
        return new ImmutableList.Builder<Float>().addAll(binned).build();
    }
    
    public urg_range_t getUrgRange()
    {
        return urg;
    }
    
    public static void main(String[] args)
    {
        final int bins = 5;
        UrgBinner ub = new UrgBinner("URG_RANGE", bins, Math.PI);

        LCM lcm = LCM.getSingleton();
        
        boolean go = true;
        laser_t lowres = new laser_t();
        lowres.intensities = new float[0];
        lowres.nintensities = 0;
        lowres.nranges = bins;
        lowres.ranges = new float[bins];
        lowres.radstep = (float)(Math.PI / bins);
        lowres.rad0 = (float)(Math.PI / -2) + (lowres.radstep / 2);
        
        while (go)
        {
            ub.update();
            List<Float> binned = ub.getBinned();
            for (int i = 0; i < bins; ++i)
                lowres.ranges[i] = binned.get(i);
            
            lowres.utime = TimeUtil.utime();
            lcm.publish("LIDAR_LOWRES_", lowres);
            
            try
            {
                Thread.sleep(100);
            }
            catch (InterruptedException e)
            {
                e.printStackTrace();
                go = false;
            }
        }
        
    }
    
}
