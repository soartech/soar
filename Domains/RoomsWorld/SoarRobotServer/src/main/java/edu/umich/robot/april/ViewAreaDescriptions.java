package edu.umich.robot.april;

import java.awt.Color;
import java.io.IOException;

import lcm.lcm.LCM;
import lcm.lcm.LCMDataInputStream;
import lcm.lcm.LCMSubscriber;
import april.config.Config;
import april.viewer.ViewObject;
import april.viewer.Viewer;
import april.vis.VisText;
import april.vis.VisWorld;
import edu.umich.robot.lcmtypes.map_metadata_t;

public class ViewAreaDescriptions implements ViewObject, LCMSubscriber
{
    private final Viewer viewer;

    private final LCM lcm = LCM.getSingleton();

    public ViewAreaDescriptions(Viewer viewer, String name, Config config)
    {
        this.viewer = viewer;
        String channel = config.getString("channel", "AREA_DESCRIPTIONS");
        lcm.subscribe(channel, this);
    }

    public void messageReceived(LCM lcm, String channel, LCMDataInputStream ins)
    {
        try
        {
            messageReceivedEx(channel, ins);
        }
        catch (IOException ex)
        {
            System.out.println("Exception: " + ex);
        }
    }

    void messageReceivedEx(String channel, LCMDataInputStream ins)
            throws IOException
    {
        map_metadata_t data = new map_metadata_t(ins);
        
        VisWorld.Buffer vb = viewer.getVisWorld().getBuffer("AREA_DESCRIPTIONS");
        
        for (int i = 0; i < data.nareas; ++i)
        {
            String idString = Integer.toString(data.area_ids[i]);
            VisText vt = new VisText(new double[] {data.areas[i][0] + 0.5, data.areas[i][1] + 0.5, 0}, VisText.ANCHOR.CENTER, idString);
            //vt.defaultColor = Color.YELLOW; 
            //vt.setDropShadow(false);
            vb.addBuffered(vt);
        }

        vb.switchBuffer();
    }
}
