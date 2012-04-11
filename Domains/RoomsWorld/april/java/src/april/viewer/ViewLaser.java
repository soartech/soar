package april.viewer;

import lcm.lcm.*;
import april.lcmtypes.*;
import april.util.*;
import april.config.*;
import april.vis.*;

import java.io.*;
import java.awt.*;
import java.util.*;

public class ViewLaser implements ViewObject, LCMSubscriber
{
    Viewer      viewer;
    String      name;
    Config      config;
    LCM         lcm = LCM.getSingleton();
    PoseTracker pt  = PoseTracker.getSingleton();

    public ViewLaser(Viewer viewer, String name, Config config)
    {
        this.viewer = viewer;
        this.name = name;
        this.config = config;
        String channel = config.getString("channel", "(.*LIDAR.*)|(.*LASER.*)");
        lcm.subscribe(channel, this);
    }

    public void messageReceived(LCM lcm, String channel, LCMDataInputStream ins)
    {
        try {
            messageReceivedEx(channel, ins);
        } catch (IOException ex) {
            System.out.println("Exception: " + ex);
        }
    }

    void messageReceivedEx(String channel, LCMDataInputStream ins) throws IOException
    {
        // sensor position in robot frame
        double spos[] = ConfigUtil.getPosition(config.getRoot(), channel);
        double squat[] = ConfigUtil.getQuaternion(config.getRoot(), channel);
        Color color = ConfigUtil.getColor(config.getRoot(), channel, new Color(255, 255, 0));
        double max_range = config.getRoot().getDouble(channel + ".max_range_m", 79);
        laser_t ldata = new laser_t(ins);
        pose_t pose = pt.get(ldata.utime);
        ArrayList<double[]> points = new ArrayList<double[]>();

        for (int i = 0; i < ldata.nranges; i++) {
            double r = ldata.ranges[i];
            if (r > max_range)
                continue;
            double theta = ldata.rad0 + ldata.radstep * i;
            double s = Math.sin(theta), c = Math.cos(theta);
            points.add(new double[] { r * c, r * s });
        }

        if (pose != null) {
            VisWorld.Buffer vb = viewer.getVisWorld().getBuffer("LASER: " + channel);
            vb.addBuffered(new VisChain(pose.orientation, pose.pos, squat, spos, new VisData(points, new VisDataPointStyle(color, 4))));
            vb.switchBuffer();
        }
    }
}
