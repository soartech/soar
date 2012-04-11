package edu.umich.robot.april;

import java.awt.Color;
import java.io.IOException;
import java.util.ArrayList;

import lcm.lcm.LCM;
import lcm.lcm.LCMDataInputStream;
import lcm.lcm.LCMSubscriber;
import april.config.Config;
import april.jmat.LinAlg;
import april.viewer.ViewObject;
import april.viewer.Viewer;
import april.vis.VisBox;
import april.vis.VisChain;
import april.vis.VisObject;
import april.vis.VisWorld;
import edu.umich.robot.lcmtypes.sim_obstacles_t;

public class ViewObstaclesReadOnly implements ViewObject
{
    private Viewer viewer;

    private static class ObstacleRect
    {
        double cx, cy;

        double xsize, ysize;

        double theta;
        
        Color color;
    }

    private ArrayList<ObstacleRect> obstacles = new ArrayList<ObstacleRect>();

    private LCM lcm = LCM.getSingleton();

    LCMSubscriber receiver = new LCMSubscriber()
    {
        @Override
        public void messageReceived(LCM lcm, String channel, LCMDataInputStream ins)
        {
            try
            {
                if (channel.equals("SIM_OBSTACLES"))
                {
                    sim_obstacles_t simobst = new sim_obstacles_t(ins);
                    if (simobst.generation != 0)
                    {
                        System.err .println("WARNING: obstacle generation unknown");
                    }
                    ArrayList<ObstacleRect> rects = new ArrayList<ObstacleRect>( simobst.nrects);
                    for (int i = 0; i < simobst.nrects; ++i)
                    {
                        ObstacleRect or = new ObstacleRect();
                        double[] r = simobst.rects[i];
                        or.cx = r[0];
                        or.cy = r[1];
                        or.xsize = r[2];
                        or.ysize = r[3];
                        or.theta = r[4];
                        double[] c = simobst.circles[i];
                        or.color = new Color((int)c[0], (int)c[1], (int)c[2]);
                        rects.add(or);
                    }
                    obstacles = rects;
                    redraw();
                }
            }
            catch (IOException ex)
            {
            }
        }
    };

    public ViewObstaclesReadOnly(Viewer viewer, String name, Config config)
    {
        this.viewer = viewer;

        lcm.subscribe("SIM_OBSTACLES", receiver);
    }

    public String getName()
    {
        return "SimObstacles";
    }

    void redraw()
    {
        VisWorld.Buffer vb = viewer.getVisWorld() .getBuffer("__SIM_OBSTACLES__");
        vb.setDrawOrder(-99);

        for (ObstacleRect orect : obstacles)
        {
            Color c = orect.color;

            double height = .5;
            VisObject vo = new VisBox(0, 0, height / 2, orect.xsize, orect.ysize, height, c);

            vb.addBuffered(new VisChain(LinAlg.matrixAB(LinAlg.translate( orect.cx, orect.cy, 0), LinAlg.rotateZ(orect.theta)), vo));
        }

        vb.switchBuffer();
    }

}
