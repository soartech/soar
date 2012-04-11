package edu.umich.robot.april;

import java.awt.Color;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.util.ArrayList;

import lcm.lcm.LCM;
import april.config.Config;
import april.jmat.LinAlg;
import april.jmat.geom.GRay3D;
import april.viewer.ViewObject;
import april.viewer.Viewer;
import april.vis.HelpOutput;
import april.vis.VisBox;
import april.vis.VisCanvas;
import april.vis.VisCanvasEventAdapter;
import april.vis.VisChain;
import april.vis.VisObject;
import april.vis.VisWorld;
import edu.umich.robot.lcmtypes.sim_obstacles_t;

/** VisCanvasEventAdapter that draws client-managed obstacles. **/
public class ViewObstacles extends VisCanvasEventAdapter implements ViewObject
{
    Viewer viewer;
    String name;
    Config config;

    static class ObstacleRect
    {
        double cx, cy;
        double xsize, ysize;
        double theta;
    }

    ArrayList<ObstacleRect> obstacles    = new ArrayList<ObstacleRect>();
    static final double     MINIMUM_SIZE = 0.25;
    ObstacleRect            selectedRect, hoverRect;
    boolean                 resizeMode   = false;
    SendThread              sendThread;
    LCM                     lcm          = LCM.getSingleton();

    public ViewObstacles(Viewer viewer, String name, Config config)
    {
        this.viewer = viewer;
        this.name = name;
        this.config = config;
        this.viewer.getVisCanvas().addEventHandler(this, 100);
        sendThread = new SendThread();
        sendThread.start();
    }

    public String getName()
    {
        return "SimObstacles";
    }

    void redraw()
    {
        VisWorld.Buffer vb = viewer.getVisWorld().getBuffer("__SIM_OBSTACLES__");
        vb.setDrawOrder(-99);
        for (ObstacleRect orect : obstacles)
        {
            Color c = Color.gray;
            if (orect == hoverRect)
                c = Color.orange;
            if (orect == selectedRect)
                c = Color.red;
            double height = 1;
            VisObject vo = new VisBox(0, 0, height / 2, orect.xsize, orect.ysize, height, c);
            vb.addBuffered(new VisChain(LinAlg.matrixAB(LinAlg.translate(orect.cx, orect.cy, 0), LinAlg.rotateZ(orect.theta)), vo));
        }
        vb.switchBuffer();
    }

    public boolean keyPressed(VisCanvas vc, KeyEvent e)
    {
        if (e.getKeyCode() == KeyEvent.VK_O)
        {
            vc.takePick(this);
            return true;
        }
        if (vc.isPicking(this) && e.getKeyCode() == KeyEvent.VK_ESCAPE)
        {
            vc.releasePick();
            return true;
        }
        if (e.getKeyCode() == KeyEvent.VK_BACK_SPACE || e.getKeyCode() == KeyEvent.VK_DELETE)
        {
            if (selectedRect != null)
            {
                obstacles.remove(selectedRect);
                selectedRect = null;
                redraw();
                return true;
            }
        }
        return false;
    }

    ObstacleRect findRect(double cxy[])
    {
        // which orect did they click near?
        for (ObstacleRect thisrect : obstacles)
        {
            double dx = cxy[0] - thisrect.cx;
            double dy = cxy[1] - thisrect.cy;
            double s = Math.sin(-thisrect.theta), c = Math.cos(-thisrect.theta);
            double x = c * dx - s * dy;
            double y = s * dx + c * dy;
            if (Math.abs(x) < thisrect.xsize / 2 && Math.abs(y) < thisrect.ysize / 2)
                return thisrect;
        }
        return null;
    }

    public boolean mousePressed(VisCanvas vc, GRay3D ray, MouseEvent e)
    {
        if (!vc.isPicking(this))
            return false;
        int mods = e.getModifiersEx();
        boolean shift = (mods & MouseEvent.SHIFT_DOWN_MASK) > 0;
        boolean control = (mods & MouseEvent.CTRL_DOWN_MASK) > 0;
        if (e.getButton() != 1)
            return false;
        double cxy[] = ray.intersectPlaneXY();
        // which orect did they click near?
        selectedRect = findRect(cxy);
        // making a new orect?
        if (selectedRect == null)
        {
            if (!control)
                return false;
            selectedRect = new ObstacleRect();
            obstacles.add(selectedRect);
            resizeMode = true;
        }
        selectedRect.cx = cxy[0];
        selectedRect.cy = cxy[1];
        selectedRect.xsize = Math.max(selectedRect.xsize, MINIMUM_SIZE);
        selectedRect.ysize = Math.max(selectedRect.ysize, MINIMUM_SIZE);
        redraw();
        return true;
    }

    public boolean mouseDragged(VisCanvas vc, GRay3D ray, MouseEvent e)
    {
        if (!vc.isPicking(this) || selectedRect == null)
            return false;
        double pxy[] = ray.intersectPlaneXY();
        int mods = e.getModifiersEx();
        boolean shift = (mods & MouseEvent.SHIFT_DOWN_MASK) > 0;
        boolean control = (mods & MouseEvent.CTRL_DOWN_MASK) > 0;
        if (control || resizeMode)
        {
            double xsize = Math.abs(pxy[0] - selectedRect.cx) * 2;
            double ysize = Math.abs(pxy[1] - selectedRect.cy) * 2;
            selectedRect.xsize = Math.max(xsize, MINIMUM_SIZE);
            selectedRect.ysize = Math.max(ysize, MINIMUM_SIZE);
        }
        else if (shift)
        {
            selectedRect.theta = Math.atan2(pxy[1] - selectedRect.cy, pxy[0] - selectedRect.cx);
        }
        else
        {
            // translate
            selectedRect.cx = pxy[0];
            selectedRect.cy = pxy[1];
        }
        redraw();
        return true;
    }

    public boolean mouseMoved(VisCanvas vc, GRay3D ray, MouseEvent e)
    {
        double cxy[] = ray.intersectPlaneXY();
        hoverRect = findRect(cxy);
        redraw();
        return false;
    }

    public boolean mouseReleased(VisCanvas vc, GRay3D ray, MouseEvent e)
    {
        if (!vc.isPicking(this) || selectedRect == null)
            return false;
        selectedRect = null;
        redraw();
        resizeMode = false;
        return true;
    }

    public void doHelp(HelpOutput houts)
    {
        houts.beginMouseCommands(this);
        houts.addMouseCommand(this, HelpOutput.LEFT | HelpOutput.CTRL | HelpOutput.CLICK, "Create obstacle (Editor mode only)");
        houts.addMouseCommand(this, HelpOutput.LEFT | HelpOutput.DRAG, "Move obstacle (Editor mode only)");
        houts.addMouseCommand(this, HelpOutput.LEFT | HelpOutput.DRAG | HelpOutput.CTRL, "Resize obstacle (Editor mode only)");
        houts.beginKeyboardCommands(this);
        houts.addKeyboardCommand(this, "o", 0, "Enter obstacle editor mode");
        houts.addKeyboardCommand(this, "ESC", 0, "Exit obstacle editor mode");
        houts.addKeyboardCommand(this, "DEL", 0, "Delete current obstacle (Editor mode only)");
    }

    class SendThread extends Thread
    {
        public SendThread()
        {
            setDaemon(true);
        }

        public void run()
        {
            while (true)
            {
                sim_obstacles_t simobst = new sim_obstacles_t();
                ArrayList<double[]> rects = new ArrayList<double[]>();
                for (ObstacleRect rect : obstacles)
                {
                    rects.add(new double[] { rect.cx, rect.cy, rect.xsize, rect.ysize, rect.theta });
                }
                simobst.generation = 0;
                simobst.nrects = rects.size();
                simobst.rects = rects.toArray(new double[0][0]);
                lcm.publish("SIM_OBSTACLES", simobst);
                try
                {
                    Thread.sleep(100);
                } catch (InterruptedException ex)
                {
                }
            }
        }
    }
}
