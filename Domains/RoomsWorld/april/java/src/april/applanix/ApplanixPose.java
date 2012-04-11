package april.applanix;

import java.io.*;
import java.net.*;

import april.util.*;
import april.lcmtypes.*;
import april.jmat.*;

import lcm.lcm.*;

public class ApplanixPose implements LCMSubscriber
{
    LCM lcm = LCM.getSingleton();
    pose_t pose;
    GPSLinearization gpslin;
    int ngroup1;
    int ndmi;

    double elevationOffset;

    int dmiLast;
    int dmiDistance;
    double dmiScale = 1.0 / 11.38;

    public ApplanixPose()
    {
        // set intitial position
        pose = new pose_t();
        pose.orientation = LinAlg.rollPitchYawToQuat(new double[3]);

        lcm.subscribe("APPLANIX", this);
    }

    public void messageReceived(LCM lcm, String channel, LCMDataInputStream ins)
    {
        try {
            raw_t raw = new raw_t(ins);

            ApplanixMessages.Message _m = ApplanixMessages.decode(raw.buf);
            if (_m == null)
                return;

            if (_m instanceof ApplanixMessages.Group10006) {
                ndmi++;
                ApplanixMessages.Group10006 m = (ApplanixMessages.Group10006) _m;

                double dist = 0;
                m.upDown <<= 8;
                m.upDownRectified <<= 8;

                System.out.printf("%08x %08x\n", m.upDown, m.upDownRectified);


                if (ndmi > 0)
                    dist = (m.upDown - dmiLast) * dmiScale / 256.0;

                dmiLast = m.upDown;

                double dpos[] = LinAlg.quatRotate(pose.orientation, new double[] { dist, 0, 0 });
                pose.pos = LinAlg.add(pose.pos, dpos);
            }

            if (_m instanceof ApplanixMessages.Group1) {
                ngroup1++;

                ApplanixMessages.Group1 m = (ApplanixMessages.Group1) _m;
                double ll[] = new double[] { m.lle[0], m.lle[1] };
                if (gpslin == null) {
                    gpslin = new GPSLinearization(ll);
                    elevationOffset = m.lle[2];
                }

/*
                double xy[] = gpslin.ll2xy(ll);
                pose.pos[0] = xy[0];
                pose.pos[1] = xy[1];
                pose.pos[2] = m.lle[2] - elevationOffset;
*/

                pose.orientation = LinAlg.rollPitchYawToQuat(new double[] { Math.toRadians(m.rpy[0]),
                                                                            Math.toRadians(m.rpy[1]),
                                                                            Math.toRadians(-m.rpy[2]) });

                pose.utime = TimeUtil.utime(); // XXX synchronize

                pose.vel = LinAlg.copy(m.velocity);
                pose.rotation_rate = LinAlg.copy(m.angularRate);
                pose.accel = LinAlg.copy(m.accel);

                lcm.publish("POSE", pose);
            }

        } catch (IOException ex) {
            System.out.println("ex: "+ex);
        }
    }

    public static void main(String args[])
    {
        ApplanixPose ap = new ApplanixPose();

        long starttime = TimeUtil.utime();

        while (true) {
            double dt = (TimeUtil.utime() - starttime) / 1.0E6;

            System.out.printf("%15.3f ApplanixPose  group1 messages received: %-8d\n",
                              dt, ap.ngroup1);

            TimeUtil.sleep(1000);
        }
    }
}
