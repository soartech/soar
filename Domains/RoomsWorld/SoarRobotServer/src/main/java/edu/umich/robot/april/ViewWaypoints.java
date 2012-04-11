package edu.umich.robot.april;

import java.awt.Color;
import java.io.IOException;
import java.util.ArrayList;

import lcm.lcm.LCM;
import lcm.lcm.LCMDataInputStream;
import lcm.lcm.LCMSubscriber;
import april.config.Config;
import april.viewer.ViewObject;
import april.viewer.Viewer;
import april.vis.VisData;
import april.vis.VisDataPointStyle;
import april.vis.VisWorld;
import edu.umich.robot.lcmtypes.waypoint_list_t;


public class ViewWaypoints implements ViewObject, LCMSubscriber
{
    Viewer viewer;
    String name;
    Config config;
    LCM    lcm = LCM.getSingleton();

    public ViewWaypoints(Viewer viewer, String name, Config config)
    {
        this.viewer = viewer;
        this.name = name;
        this.config = config;
        String channel = config.getString("channel", "WAYPOINTS");
        lcm.subscribe(channel, this);
    }

    public void messageReceived(LCM lcm, String channel, LCMDataInputStream ins)
    {
        try
        {
            messageReceivedEx(channel, ins);
        } catch (IOException ex)
        {
            System.out.println("Exception: " + ex);
        }
    }

    void messageReceivedEx(String channel, LCMDataInputStream ins) throws IOException
    {
        waypoint_list_t waypoints = new waypoint_list_t(ins);
        ArrayList<double[]> points = new ArrayList<double[]>();
        for (int i = 0; i < waypoints.nwaypoints; ++i)
        {
            points.add(new double[] { waypoints.waypoints[i].xLocal, waypoints.waypoints[i].yLocal });
        }
        VisWorld.Buffer vb = viewer.getVisWorld().getBuffer("WAYPOINTS");
        vb.addBuffered(new VisData(points, new VisDataPointStyle(new Color(255, 255, 0), 4)));
        vb.switchBuffer();
    }
}
