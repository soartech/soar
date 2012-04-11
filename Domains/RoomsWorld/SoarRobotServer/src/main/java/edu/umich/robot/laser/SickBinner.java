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

/**
 * TODO refactor this and UrgBinner -- they share a lot of code. See URG binner
 * for detailed comments.
 * 
 * @author voigtjr
 * 
 */
public class SickBinner implements Binner
{
    private final LCM lcm = LCM.getSingleton();
    
    private final int nbins;
    
    private final List<Float> binned = Lists.newArrayList();
    
    private laser_t laser;
    
    private long last = -1;
    
    private final double fov0;
    
    private final double fovstep;
    
    private Lidar lidar = null;
    
    public SickBinner(String channel, int bins, double fov)
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
                laser = new laser_t(ins);
                lidar.lidarChanged(SickBinner.this);
            }
            catch (IOException e)
            {
                e.printStackTrace();
            }
        }
    };
    
    public void update()
    {
        update(laser);
    }
    
    void update(laser_t laser)
    {
        // shadowed this.urg on purpose

        if (laser == null)
            return;
        
        if (last >= laser.utime)
            return;
        last = laser.utime;
        
        double theta = laser.rad0;
        double nextbin = fov0;
        int bin = -1;
        float value = Float.MAX_VALUE;
        
        for (int i = 0; i < laser.nranges; ++i, theta += laser.radstep)
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
            
            if (laser.ranges[i] < 0)
                continue;
            
            value = Math.min(value, laser.ranges[i]);
        }
        
        if (bin >= 0 && bin < nbins)
            binned.set(bin, value);
    }
    
    public List<Float> getBinned()
    {
        if (last == -1)
            return null;
        return new ImmutableList.Builder<Float>().addAll(binned).build();
    }
    
    public laser_t getLaser()
    {
        return laser;
    }
    
    public static void main(String[] args)
    {
        final int bins = 5;
        SickBinner ub = new SickBinner("SIM_LIDAR_FRONT_seek", bins, Math.PI);

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
