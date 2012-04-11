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
package edu.umich.robot.util;

import java.util.Map;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.google.common.collect.Maps;
import com.google.common.util.concurrent.MoreExecutors;

/**
 * Super simple battery model for splinter system. Not intended model reality.
 * 
 * Thread-safe.
 * 
 * @author voigtjr
 *
 */
public class SimBattery
{
    private static final Log logger = LogFactory.getLog(SimBattery.class);
    
    private final double mAh_max;
    private final AtomicReference<Double> mAh;
    private final double mAh_min;
    private final ScheduledExecutorService schexec = MoreExecutors.getExitingScheduledExecutorService(new ScheduledThreadPoolExecutor(1));
    private final int UPDATE_DT = 5; // seconds
    private final Map<String, Double> current = Maps.newConcurrentMap();
    private final AtomicReference<ScheduledFuture<?>> updateTask = new AtomicReference<ScheduledFuture<?>>();
    
    /**
     * @param mAh_max
     * @param doc depth of charge
     */
    public SimBattery(double mAh_max, double doc)
    {
        if (doc > 1.0 || doc < 0.1)
            doc = 1.0;
        if (mAh_max < 0)
            mAh_max = 10000;

        this.mAh_max = mAh_max;
        this.mAh = new AtomicReference<Double>(Double.valueOf(mAh_max));
        this.mAh_min = mAh_max * (1 - doc);
        
        logger.trace("Battery created with max " + mAh_max + " min " + mAh_min);

        start();
    }
    
    private final Runnable update = new Runnable() 
    {
        public void run() 
        {
            double total = 0;
            for (Double c : current.values())
                total += c;
            total *= (double)UPDATE_DT / TimeUnit.SECONDS.convert(1, TimeUnit.HOURS);

            double ah = mAh.get();
            ah -= total;
            
            if (ah < mAh_min)
                ah = mAh_min;
            
            mAh.set(ah);
            
            if (logger.isTraceEnabled())
                logger.trace(String.format("Battery life: %3.3f%%, mAh: %2.5f", getRemainingLife() * 100, mAh.get()));
        }
    };
    
    public void stop()
    {
        ScheduledFuture<?> t = updateTask.getAndSet(null);
        if (t != null)
            t.cancel(false);
    }
    
    public void start()
    {
        updateTask.compareAndSet(null, schexec.scheduleAtFixedRate(update, UPDATE_DT, UPDATE_DT, TimeUnit.SECONDS));
    }
    
    public void resetCharge()
    {
        mAh.set(Double.valueOf(this.mAh_max));
    }
    
    public void setMilliAmpCurrent(String name, double mAh)
    {
        current.put(name, Double.valueOf(mAh));
    }
    
    public void removeMilliAmpCurrent(String name)
    {
        current.remove(name);
    }
    
    public double getRemainingLife()
    {
        return (mAh.get() - mAh_min) / (mAh_max - mAh_min);
    }
    
    public void shutdown()
    {
        schexec.shutdown();
    }
}
