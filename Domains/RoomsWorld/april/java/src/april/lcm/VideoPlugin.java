package april.lcm;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.awt.geom.*;
import java.io.*;
import javax.swing.*;
import javax.imageio.*;

import lcm.lcm.*;
import lcm.spy.*;

import april.lcmtypes.*;
import april.util.*;

/** A plugin for viewing video_t data **/
public class VideoPlugin implements SpyPlugin
{
    public boolean canHandle(long fingerprint)
    {
        return fingerprint == image_t.LCM_FINGERPRINT;
    }

    class MyAction extends AbstractAction
    {
        ChannelData cd;
        JDesktopPane jdp;

        public MyAction(JDesktopPane jdp, ChannelData cd)
        {
            super("Video Viewer");
            this.jdp = jdp;
            this.cd = cd;
        }

        public void actionPerformed(ActionEvent e)
        {
            Viewer v = new Viewer(cd);
            jdp.add(v);
            v.toFront();
        }
    }

    public Action getAction(JDesktopPane jdp, ChannelData cd)
    {
        return new MyAction(jdp, cd);
    }

    class Viewer extends JInternalFrame implements LCMSubscriber
    {
        ChannelData cd;
        JImage ji;

        public Viewer(ChannelData cd)
        {
            super("Video: "+cd.name, true, true);
            this.cd = cd;

            setLayout(new BorderLayout());
            ji = new JImage(null, true);
            add(ji, BorderLayout.CENTER);
            setSize(400,300);
            setVisible(true);

            LCM.getSingleton().subscribe(cd.name, this);
        }

        public void messageReceived(LCM lcm, String channel, LCMDataInputStream ins)
        {
            try {
                image_t v = new image_t(ins);
                BufferedImage bi = image_t_util.decode(v);
                ji.setImage(bi);
            } catch (IOException ex) {
                System.out.println("ex: "+ex);
                return;
            }
        }
    }
}
