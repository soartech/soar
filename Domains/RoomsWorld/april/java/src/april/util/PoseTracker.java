package april.util;

import java.io.*;
import java.util.*;

import lcm.lcm.*;
import april.lcmtypes.*;
import april.jmat.*;

/**
 * Subscribes to pose_t and find the pose_t whose timestamp is closest
 * to the requested value.
 **/
public class PoseTracker implements LCMSubscriber
{
    static String           channel     = "POSE";
    LCM                     lcm         = LCM.getSingleton();

    LinkedList<pose_t>      queue       = new LinkedList<pose_t>();

    // how long back in time should we remember poses?

    public double           time        = 10.0;

    boolean                 warned;

    // don't use messages that are older than this... (seconds)
    public double           maxTimeErr  = 0.1;

    static PoseTracker pt;
    static final int MAX_QUEUE_SIZE     = 10000;

    public static PoseTracker getSingleton()
    {
        if (pt == null)
            pt = new PoseTracker(channel, 10);

        return pt;
    }

    public PoseTracker(String channel, double time)
    {
        this.channel = channel;
        this.time = time;

        lcm.subscribe(channel, this);
    }

    public synchronized void clear()
    {
        queue.clear();
    }

    public synchronized void messageReceived(LCM lcm, String channel, LCMDataInputStream ins)
    {
        try {
            pose_t p = new pose_t(ins);
            queue.add(p);

            // emergency shrinkage.
            while (queue.size() > MAX_QUEUE_SIZE) {
                queue.removeFirst();
                if (!warned) {
                    System.out.println("PoseTracker queue too large");
                    warned = true;
                }
            }

            while (true) {
                pose_t first = queue.getFirst();
                pose_t last = queue.getLast();

                if (Math.abs(last.utime - first.utime) > time*1000000)
                    queue.removeFirst();
                else
                    break;
            }

        } catch (IOException ex) {
            System.out.println("Exception: " + ex);
        }
    }

    public synchronized pose_t get()
    {
        if (queue.size() == 0)
            return null;
        return queue.getLast();
    }

    public synchronized pose_t get(long utime)
    {
        pose_t p0 = null, p1 = null; // two poses bracketing the desired utime

        for (pose_t p : queue) {
            if (p.utime < utime && (p0 == null || p.utime > p0.utime))
                p0 = p;
            if (p.utime > utime && (p1 == null || p.utime < p1.utime))
                p1 = p;
        }

        if (p0 != null && Math.abs(utime - p0.utime) > maxTimeErr*1000000)
            p0 = null;

        if (p1 != null && Math.abs(utime - p1.utime) > maxTimeErr*1000000)
            p1 = null;

        if (p0 != null && p1 != null) {

            if (p0.utime == p1.utime)
                return p0;

            // interpolate
            double err0 = Math.abs(p0.utime - utime);
            double err1 = Math.abs(p1.utime - utime);

            double w0 = err1 / (err0 + err1);
            double w1 = err0 / (err0 + err1);

            assert(!Double.isNaN(w0));

            pose_t p = new pose_t();
            p.utime = utime;
            p.pos = LinAlg.add(LinAlg.scale(p0.pos, w0),
                               LinAlg.scale(p1.pos, w1));
            p.vel = LinAlg.add(LinAlg.scale(p0.vel, w0),
                               LinAlg.scale(p1.vel, w1));
            p.rotation_rate = LinAlg.add(LinAlg.scale(p0.rotation_rate, w0),
                                         LinAlg.scale(p1.rotation_rate, w1));
            p.accel = LinAlg.add(LinAlg.scale(p0.accel, w0),
                                 LinAlg.scale(p1.accel, w1));

            p.orientation = LinAlg.slerp(p0.orientation, p1.orientation, w0);
            return p;
        }

        if (p0 != null)
            return p0;

        if (p1 != null)
            return p1;

        return null;
    }
    
    public void dispose()
    {
        lcm.unsubscribe(PoseTracker.channel, this);
    }
}
