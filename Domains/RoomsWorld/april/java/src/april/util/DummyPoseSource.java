package april.util;

import lcm.lcm.*;
import april.lcmtypes.*;

/** Transmit phony pose_t messages. **/
public class DummyPoseSource
{
    public static void main(String args[])
    {
        LCM lcm = LCM.getSingleton();

        while (true) {
            pose_t pose = new pose_t();
	    pose.utime = TimeUtil.utime();
            pose.pos = new double[3];
            pose.orientation = new double[] { 1, 0, 0, 0 };

            lcm.publish("POSE", pose);
            TimeUtil.sleep(50);
        }
    }

}
