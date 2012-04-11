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
package edu.umich.robot.laser;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import lcm.lcm.LCM;

import april.lcmtypes.laser_t;

import com.google.common.util.concurrent.MoreExecutors;

/**
 * Merges messages from a SICK and/or simulated laser sensor in to a single,
 * "lower resolution" reading of usually 5 ranges. Merges by taking the minimum
 * reading from the set of sensors.
 * 
 * Will use an old reading up to a configurable duration so that the sensor
 * doesn't "jitter" when old and new readings aren't available at the same time.
 * 
 * @author voigtjr@gmail.com
 */
public class Lidar
{
    // private static final Log logger = LogFactory.getLog(Lidar.class);

    private static final LCM lcm = LCM.getSingleton();

    private static final String SIM_LASER_CHANNEL_BASE = "SIM_LIDAR_FRONT_";

    private static final String SICK_LASER_CHANNEL_BASE = "SICK_LIDAR_FRONT_";

    private static final String URG_CHANNEL_BASE = "URG_RANGE_";

    private static final String LASER_LOWRES_CHANNEL_BASE = "LIDAR_LOWRES_";

    private final SickBinner sim;

    private final SickBinner sick;

    private final UrgBinner urg;

    private final String lowresChannel;

    private final ScheduledExecutorService schexec = MoreExecutors.getExitingScheduledExecutorService(new ScheduledThreadPoolExecutor(1));

    private final float radstep;

    private final float rad0;

    private Laser cached;

    // For lidar-change events
    public interface Binner
    {
    }

    public interface LidarChangedListener
    {
        void onLidarChanged(SickBinner binner);

        void onLidarChanged(UrgBinner binner);
    }

    private ArrayList<LidarChangedListener> lidarChangedListeners = new ArrayList<LidarChangedListener>();

    public Lidar(String name, boolean useSick, int bins, double fov)
    {
        sim = new SickBinner(SIM_LASER_CHANNEL_BASE + name, bins, fov);
        sim.setLidar(this);
        if (useSick)
        {
            sick = new SickBinner(SICK_LASER_CHANNEL_BASE + name, bins, fov);
            sick.setLidar(this);
            urg = null;
        }
        else
        {
            sick = null;
            urg = new UrgBinner(URG_CHANNEL_BASE + name, bins, fov);
            urg.setLidar(this);
        }
        lowresChannel = LASER_LOWRES_CHANNEL_BASE + name;

        radstep = (float) (Math.PI / bins);
        rad0 = (float) (Math.PI / -2) + (radstep / 2);
        Laser.Builder lb = new Laser.Builder(rad0, radstep);
        cached = lb.addRanges(new float[] { 0, 0, 0, 0, 0 }).build();

        schexec.scheduleAtFixedRate(update, 0, 100, TimeUnit.MILLISECONDS);
    }

    private final Runnable update = new Runnable()
    {
        public void run()
        {
            List<Float> simBins = null;
            List<Float> sickBins = null;
            List<Float> urgBins = null;
            if (sim != null)
            {
                sim.update();
                simBins = sim.getBinned();
            }
            if (sick != null)
            {
                sick.update();
                sickBins = sick.getBinned();
            }
            if (urg != null)
            {
                urg.update();
                urgBins = urg.getBinned();

            }

            int bins = simBins != null ? simBins.size() : 0;
            if (bins == 0)
            {
                bins = sickBins != null ? sickBins.size() : 0;
                if (bins == 0)
                {
                    bins = urgBins != null ? urgBins.size() : 0;
                    if (bins == 0) return;
                }
            }

            Laser.Builder lb = new Laser.Builder(rad0, radstep);
            for (int i = 0; i < bins; ++i)
            {
                float value = Float.MAX_VALUE;
                value = min(value, simBins, i);
                value = min(value, sickBins, i);
                value = min(value, urgBins, i);
                lb.add(value);
            }
            cached = lb.build();
            lcm.publish(lowresChannel, cached.toLcm());
        }
    };

    private final float min(float value, List<Float> bins, int i)
    {
        if (bins == null) return value;
        return Math.min(value, bins.get(i));
    }

    public Laser getLaserLowRes()
    {
        return cached;
    }

    public void shutdown()
    {
        schexec.shutdown();
    }

    public Laser getLaserHiRes()
    {
        if (sim != null)
        {
            return buildLaser(sim.getLaser());
        }
        if (sick != null)
        {
            return buildLaser(sick.getLaser());
        }

        // TODO merge urg and sim / sick types
        /*
         * if (urg != null) { return buildLaser(urg.getLaser()); }
         */

        return null;
    }

    private static Laser buildLaser(laser_t laser)
    {
        Laser.Builder lb = new Laser.Builder(laser.rad0, laser.radstep);
        for (int i = 0; i < laser.nranges; ++i)
        {
            float value = Math.min(laser.ranges[i], Float.MAX_VALUE);
            lb.add(value);
        }
        return lb.build();
    }

    public void addLidarChangedListener(LidarChangedListener listener)
    {
        lidarChangedListeners.add(listener);
    }

    public void removeLidarChangedListener(LidarChangedListener listener)
    {
        lidarChangedListeners.remove(listener);
    }

    public void lidarChanged(Binner binner)
    {
        for (LidarChangedListener listener : lidarChangedListeners)
        {
            if (binner instanceof SickBinner)
            {
                listener.onLidarChanged((SickBinner) binner);
            }
            if (binner instanceof UrgBinner)
            {
                listener.onLidarChanged((UrgBinner) binner);
            }
        }
    }
}
