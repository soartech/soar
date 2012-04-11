package april.viewer;

import lcm.lcm.*;


import java.io.*;
import java.awt.*;
import java.util.*;

import april.jmat.*;
import april.lcmtypes.*;
import april.util.*;
import april.config.*;
import april.vis.*;

public class ViewGamePad implements ViewObject, LCMSubscriber
{
    Viewer      viewer;
    String      name;
    Config      config;
    LCM         lcm = LCM.getSingleton();
    PoseTracker pt  = PoseTracker.getSingleton();

    public ViewGamePad(Viewer viewer, String name, Config config)
    {
        this.viewer = viewer;
        this.name = name;
        this.config = config;
        lcm.subscribe(config.getString("channel", "GAMEPAD"), this);
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
        gamepad_t gp = new gamepad_t(ins);
        int forward_axis = config.getInt("forward_axis", 3);
        int turn_axis = config.getInt("turn_axis", 2);
        double forward_scale = config.getDouble("forward_scale", -1);
        double turn_scale = config.getDouble("turn_scale", 1);
        double forward = gp.axes[forward_axis] * forward_scale;
        double turn = gp.axes[turn_axis] * turn_scale;
        double mag = Math.sqrt(forward * forward + turn * turn);
        if (mag > 1)
        {
            forward /= mag;
            turn /= mag;
        }
        VisWorld.Buffer vb = viewer.getVisWorld().getBuffer("GamePad");
        vb.addBuffered(new VisWindow(VisWindow.ALIGN.BOTTOM_RIGHT, 80, 80, new double[] { -1, -1 }, new double[] { 1, 1 }, new VisCircle(1, new VisDataFillStyle(Color.gray), new VisDataLineStyle(Color.darkGray, 2)), new VisChain(LinAlg.translate(turn, forward, 0), new VisCircle(0.1, new VisDataFillStyle(Color.white), new VisDataLineStyle(Color.black, 1)))));
        vb.switchBuffer();
    }
}
