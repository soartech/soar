package edu.umich.robot.april;

import java.awt.Color;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import april.config.Config;
import april.jmat.LinAlg;
import april.lcmtypes.pose_t;
import april.util.PoseTracker;
import april.viewer.ViewObject;
import april.viewer.Viewer;
import april.vis.VisCanvas;
import april.vis.VisCanvasEventAdapter;
import april.vis.VisData;
import april.vis.VisDataLineStyle;
import april.vis.VisWorld;

/** VisCanvasEventAdapter that draws a robot and allows teleportation. **/
public class ViewTrajectory extends VisCanvasEventAdapter implements ViewObject
{
    Viewer              viewer;
    String              name;
    Config              config;
    pose_t              pose;
    ArrayList<double[]> trajectory      = new ArrayList<double[]>();
    PoseTracker pt;

    /** If interactive, teleports are enabled. **/
    public ViewTrajectory(final Viewer viewer, String name, Config config)
    {
        String poseName = config.getString("pose");

        this.viewer = viewer;
        this.name = name;
        this.config = config;
        viewer.getVisCanvas().addEventHandler(this, 0);
        this.pt = new PoseTracker("POSE_" + poseName, 10.0);
        ScheduledExecutorService exec = Executors.newSingleThreadScheduledExecutor();

        exec.scheduleAtFixedRate(new Runnable() {
	    @Override
	    public void run()
	    {
		pose = pt.get();
		if (pose == null) {
		    return;
		}
                double lastpos[] = trajectory.size() > 0 ? trajectory.get(trajectory.size() - 1) : null;
                if (lastpos == null || LinAlg.distance(lastpos, pose.pos) > 0.05) {
                    trajectory.add(LinAlg.copy(pose.pos));
                    String thisName = ViewTrajectory.this.getName();
                    VisWorld.Buffer vb = viewer.getVisWorld().getBuffer(thisName);
                    vb.addBuffered(new VisData(new VisDataLineStyle(Color.blue, 1), trajectory));
                    vb.switchBuffer();
                }
	    }
	}, 0, 50, TimeUnit.MILLISECONDS);
    }

    @Override
    public String getName()
    {
        return name;
    }

    boolean   lastCharT = false;

    @Override
    public boolean keyTyped(VisCanvas vc, KeyEvent e)
    {
        if (lastCharT)
        {
            if (e.getKeyChar() == 'c')
            {
                clear();
            }
        }
        lastCharT = e.getKeyChar() == 't';
        return false;
    }
    
    public void clear()
    {
        trajectory.clear();
    }
}
