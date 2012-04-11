package april.vis;

import april.jmat.*;

import java.awt.*;
import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import java.util.*;

import april.jmat.geom.*;

/** Represents the view into a VisWorld, such as camera position. **/
public class VisViewManager
{
    public VisView viewGoal = new VisView();

    public double manipulationPoint[] = new double[] {0,0,0,1};

    public double interfaceMode = 2.5;

    double followPos[] = new double[3];
    double followQuat[] = new double[] {1, 0, 0, 0};

    HashMap<String, Boolean> enabledBuffers = new HashMap<String, Boolean>();

    ArrayList<VisViewListener> listeners = new ArrayList<VisViewListener>();

    VisContext vc;

    double follow_lastpos[], follow_lastquat[];

    public double zoom_dist_min = 0.05;
    public double zoom_dist_max = 2000;

    public VisViewManager(VisContext vc)
    {
        this.vc = vc;
    }

    public void addListener(VisViewListener vvl)
    {
        listeners.add(vvl);
    }

    public void removeListener(VisViewListener vvl)
    {
        listeners.remove(vvl);
    }

    public double getInterfaceMode()
    {
        return interfaceMode;
    }

    public void setInterfaceMode(double d)
    {
        interfaceMode = d;
    }

    public void setBufferEnabled(String buffer, boolean b)
    {
        enabledBuffers.put(buffer, b);

        for (VisViewListener vvl : listeners)
            vvl.viewBufferEnabledChanged(vc, buffer, b);
        vc.draw();
    }

    public boolean isBufferEnabled(String buffer)
    {
        Boolean b = enabledBuffers.get(buffer);

        return (b == null || b == true);
    }

    public VisView getView()
    {
        viewGoal.adjustForInterfaceMode(interfaceMode);
        return viewGoal.copy();
    }

    public void follow(double pos[], double quat[], boolean followYaw)
    {
        if (follow_lastpos != null) {
            follow(follow_lastpos, follow_lastquat, pos, quat, followYaw);
        }

        follow_lastpos = LinAlg.copy(pos);
        follow_lastquat = LinAlg.copy(quat);
    }

    public void follow(double lastPos[], double lastQuat[], double newPos[], double newQuat[],
                       boolean followYaw)
    {
        if (followYaw) {
            // follow X,Y, and orientation.

            // our strategy is to compute the eye,lookAt relative to
            // the vehicle position, then use the new vehicle position
            // to recompute new eye/lookAt. We'll keep 'up' as it is.

            double v2eye[] = LinAlg.subtract(lastPos, viewGoal.eye);
            double v2look[] = LinAlg.subtract(lastPos, viewGoal.lookAt);

            // this is the vector that the robot is newly pointing in
            double vxy[] = new double[] { 1, 0, 0};
            vxy = LinAlg.quatRotate(newQuat, vxy);
            vxy[2] = 0;

            // where was the car pointing last time?
            double theta = LinAlg.quatToRollPitchYaw(newQuat)[2] -
                LinAlg.quatToRollPitchYaw(lastQuat)[2];

            double zaxis[] = new double[] {0,0,1};
            double q[] = LinAlg.angleAxisToQuat(theta, zaxis);

            v2look = LinAlg.quatRotate(q, v2look);
            double newLookAt[] = LinAlg.subtract(newPos, v2look);

            v2eye = LinAlg.quatRotate(q, v2eye);
            double newEye[] = LinAlg.subtract(newPos, v2eye);
            double newUp[] = LinAlg.quatRotate(q, viewGoal.up);

            viewGoal.lookAt(newEye, newLookAt, newUp);
        } else {
            // follow in X/Y (but not yaw)

            double dpos[] = LinAlg.subtract(newPos, lastPos);

            double newEye[] = LinAlg.add(viewGoal.eye, dpos);
            double newLookAt[] = LinAlg.add(viewGoal.lookAt, dpos);

            viewGoal.lookAt(newEye, newLookAt, viewGoal.up);
        }
    }

    public void setManipulationPoint(double xyz[])
    {
        this.manipulationPoint = LinAlg.copy(xyz);
    }
}
