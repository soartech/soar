package edu.umich.robot.april;

import java.util.concurrent.locks.ReentrantLock;

/** Simple time utilities. **/
public class SoarTimeUtil
{
    private static final ReentrantLock lock = new ReentrantLock();
    private static int rate = 1;
    private static long stamp = 0;
    private static long slew = 0;
    
    static
    {
        setTimeScale(1);
    }
    
    public static void setTimeScale(int rate)
    {
        lock.lock();
        try
        {
            long t = System.currentTimeMillis();
            slew = tprime(t);
            stamp = t;
            SoarTimeUtil.rate = rate;
        }
        finally
        {
            lock.unlock();
        }
    }
    
    public static long mstime()
    {
        return tprime(System.currentTimeMillis());
    }
    
    private static long tprime(long t)
    {
        lock.lock();
        try
        {
            return ((t - stamp) * rate) + slew;
        }
        finally
        {
            lock.unlock();
        }
    }
    
    public static long utime()
    {
        return mstime() * 1000;
    }
    
    public static void sleep(int ms)
    {
        try {
            Thread.sleep(ms);
        } catch (InterruptedException ex) {
        }
    }
}
