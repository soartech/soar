package april.vis;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;

import april.jmat.geom.*;
import april.jmat.*;

/** Default event handler implementing pan, zoom, animations, etc. **/
public class VisCanvasDefaultEventHandler extends VisCanvasEventAdapter
{
    VisCanvas vc;
    double ZOOM_ANIMATE_RATE = 1.015;
    double ZOOM_FAST = 2.0;
    double ZOOM_SLOW = 1.1;

    double ROTATE_ANIMATE_RATE = 0.1;  // rad per sec (increments)
    double ROTATE_KEY_AMOUNT = 0.2; // radians
    double PAN_KEY_AMOUNT = 5; // pixels
    int lastx, lasty;

    AnimateThread at;

    boolean mouseDown;
    boolean wasRotating;

    MouseVelocity mvel;
    double zoomRate = 1.0;

    double rotateAxis[] = new double[] {0, 0, 1};
    double rotateRadPerSec;

    HashMap<Integer, Bookmark> bookmarks = new HashMap<Integer, Bookmark>();

    static class Bookmark
    {
        double eye[];
        double lookat[];
        double up[];
        double rotateAxis[];
        double rotateRadPerSec;
        double zoomRate;
        double perspectiveness;

        Bookmark(VisView view, double rotateAxis[], double rotateRadPerSec, double zoomRate)
        {
            this.eye = LinAlg.copy(view.eye);
            this.lookat = LinAlg.copy(view.lookAt);
            this.up = LinAlg.copy(view.up);
            this.rotateAxis = LinAlg.copy(rotateAxis);
            this.rotateRadPerSec = rotateRadPerSec;
            this.zoomRate = zoomRate;
            this.perspectiveness = view.perspectiveness;
        }
    }

    public VisCanvasDefaultEventHandler(VisCanvas vc)
    {
        this.vc = vc;
        this.at = new AnimateThread();
        this.at.start();

        mvel = new MouseVelocity();
        vc.addEventHandler(mvel, 100000);
    }

    public double pickQuery(VisCanvas vc, GRay3D ray)
    {
        return -1;
    }

    public double hoverQuery(VisCanvas vc, GRay3D ray)
    {
        return -1;
    }

    public void pickNotify(boolean winner)
    {
    }

    public void hoverNotify(boolean winner)
    {
    }

    public boolean keyTyped(VisCanvas vc, KeyEvent e)
    {
        return false;
    }

    public boolean keyReleased(VisCanvas vc, KeyEvent e)
    {
        return false;
    }

    public boolean keyPressed(VisCanvas vc, KeyEvent e)
    {
        int code = e.getKeyCode();
        double dx = 0, dy = 0, dz = 0;
        double m = 0.1;
        int mods = e.getModifiersEx();
        boolean shift = (mods&MouseEvent.SHIFT_DOWN_MASK) > 0;
        boolean ctrl = (mods&MouseEvent.CTRL_DOWN_MASK) > 0;
        boolean alt = shift & ctrl;

        if (alt) {
            shift=false; ctrl=false;
        }

        boolean consumed = false;

        VisView view = vc.getLastView();

        double p2eye[] = LinAlg.subtract(view.eye, view.lookAt);
        double left[] = LinAlg.crossProduct(view.up, p2eye);

        // animation commands
        if (ctrl) {

            double upaxis[] = view.up;
            if (vc.getViewManager().getInterfaceMode() < 3)
                upaxis = new double[] {0,0,1};

            switch (code)
            {
                case KeyEvent.VK_PAGE_DOWN:
                    zoomRate *= ZOOM_ANIMATE_RATE;
                    consumed = true;
                    break;

                case KeyEvent.VK_PAGE_UP:
                    zoomRate /= ZOOM_ANIMATE_RATE;
                    consumed = true;
                    break;

                case KeyEvent.VK_LEFT:
                    rotateAxis = upaxis;
                    rotateRadPerSec -= ROTATE_ANIMATE_RATE;
                    break;

                case KeyEvent.VK_RIGHT:
                    rotateAxis = upaxis;
                    rotateRadPerSec += ROTATE_ANIMATE_RATE;
                    break;

                case KeyEvent.VK_UP:
                    rotateAxis = left;
                    rotateRadPerSec += ROTATE_ANIMATE_RATE;
                    break;

                case KeyEvent.VK_DOWN:
                    rotateAxis = left;
                    rotateRadPerSec -= ROTATE_ANIMATE_RATE;
                    break;

                case KeyEvent.VK_F1:
                case KeyEvent.VK_F2:
                case KeyEvent.VK_F3:
                case KeyEvent.VK_F4:
                case KeyEvent.VK_F5:
                case KeyEvent.VK_F6:
                case KeyEvent.VK_F7:
                case KeyEvent.VK_F8:
                case KeyEvent.VK_F9:
                case KeyEvent.VK_F10:
                    bookmarks.put(code, new Bookmark(view, rotateAxis, rotateRadPerSec, zoomRate));
                    System.out.println("Set Bookmark for key event "+code);
                    break;
            }

            return consumed;
        }

        if (shift) {

            switch (code)
            {
                case KeyEvent.VK_LEFT:
                    vc.getViewManager().viewGoal.rotate(-ROTATE_KEY_AMOUNT, view.up);
                    lastRotateAxis = LinAlg.copy(view.up);
                    break;

                case KeyEvent.VK_RIGHT:
                    vc.getViewManager().viewGoal.rotate(+ROTATE_KEY_AMOUNT, view.up);
                    lastRotateAxis = LinAlg.copy(view.up);
                    break;

                case KeyEvent.VK_UP:
                    vc.getViewManager().viewGoal.rotate(+ROTATE_KEY_AMOUNT, left);
                    break;

                case KeyEvent.VK_DOWN:
                    vc.getViewManager().viewGoal.rotate(-ROTATE_KEY_AMOUNT, left);
                    break;

            }
        }

        // keyboard panning
        if (!shift && !ctrl) {
            double x = vc.getWidth()/2.0;
            double y = vc.getHeight()/2.0;

            switch (code)
            {
                case KeyEvent.VK_LEFT:
                    windowSpacePanTo(view, view.lookAt, x+PAN_KEY_AMOUNT, y, true);
                    break;

                case KeyEvent.VK_RIGHT:
                    windowSpacePanTo(view, view.lookAt, x-PAN_KEY_AMOUNT, y, true);
                    break;

                case KeyEvent.VK_UP:
                    windowSpacePanTo(view, view.lookAt, x, y+PAN_KEY_AMOUNT, true);
                    break;

                case KeyEvent.VK_DOWN:
                    windowSpacePanTo(view, view.lookAt, x, y-PAN_KEY_AMOUNT, true);
                    break;

            }
        }

        switch (code)
	    {
            case KeyEvent.VK_PAGE_DOWN:
                zoom( shift ? ZOOM_FAST : ZOOM_SLOW);
                consumed = true;
                vc.drawNow();
                break;

            case KeyEvent.VK_PAGE_UP:
                zoom( shift ? 1.0/ZOOM_FAST : 1.0/ZOOM_SLOW);
                consumed = true;
                vc.drawNow();
                break;

            case KeyEvent.VK_F1:
            case KeyEvent.VK_F2:
            case KeyEvent.VK_F3:
            case KeyEvent.VK_F4:
            case KeyEvent.VK_F5:
            case KeyEvent.VK_F6:
            case KeyEvent.VK_F7:
            case KeyEvent.VK_F8:
            case KeyEvent.VK_F9:
            case KeyEvent.VK_F10:
                // go to bookmark
                Bookmark b = bookmarks.get(code);
                if (b==null) {
                    System.out.println("No bookmark set for key event "+code);
                } else {
                    System.out.println("Going to bookmark for key event "+code);

                    at.goToBookmark(b);

                    // instantaneous jump:
                    //    vc.view.lookAt(b.eye, b.lookat, b.up);
                }
	    }

        return consumed;
    }

    public void stopAllAnimation()
    {
        rotateRadPerSec = 0;
        zoomRate = 1.0;
    }

    public boolean mouseClicked(VisCanvas vc,  GRay3D ray, MouseEvent e)
    {
        int mods = e.getModifiersEx();
        boolean shift = (mods&MouseEvent.SHIFT_DOWN_MASK)>0;
        boolean ctrl = (mods&MouseEvent.CTRL_DOWN_MASK)>0;
        boolean alt = (mods&MouseEvent.ALT_DOWN_MASK)>0;

        if (e.getButton() == MouseEvent.BUTTON3 && ctrl) {
            vc.popupMenu.orthoItem.setState(vc.getViewManager().viewGoal.perspectiveness == 0);
            vc.popupMenu.show((Component) vc.canvas, (int) e.getPoint().getX(), (int) e.getPoint().getY());
        }

        if (e.getButton() == MouseEvent.BUTTON1) {
            /*	    if (ctrl) {
                    VisView lastView = vc.getLastView();
                    vc.getViewManager().lookAt(lastView.eye,
                    LinAlg.copy(ray.intersectPlaneXY(0)),
                    lastView.up);
                    } else {*/
            stopAllAnimation();
            //	    }
        }


        return false;
    }

    public boolean mouseReleased(VisCanvas vc, GRay3D ray, MouseEvent e)
    {
        int mods=e.getModifiersEx();
        boolean shift = (mods&MouseEvent.SHIFT_DOWN_MASK)>0;
        boolean ctrl = (mods&MouseEvent.CTRL_DOWN_MASK)>0;
        boolean alt = (mods&MouseEvent.ALT_DOWN_MASK)>0;

        mvel.update(e);
        mouseDown = false;

        if (wasRotating && alt) {
            rotateAxis = lastRotateAxis;
            rotateRadPerSec = lastRotateRadPerSec;
        }

        return false;
    }

    double lastRotateAxis[];
    double lastRotateRadPerSec;
    long mouseDraggedLastMTime;

    public boolean mouseDragged(VisCanvas vc,  GRay3D ray, MouseEvent e)
    {
        mvel.update(e);
        int mods=e.getModifiersEx();

        boolean shift = (mods&MouseEvent.SHIFT_DOWN_MASK)>0;
        boolean ctrl = (mods&MouseEvent.CTRL_DOWN_MASK)>0;

        double dx = e.getX() - lastx;
        double dy = e.getY() - lasty;

        VisView view = vc.getLastView();

        if ((mods&InputEvent.BUTTON1_DOWN_MASK)>0) {
            double interfaceMode = vc.getViewManager().getInterfaceMode();
            windowSpacePanTo(view, vc.getViewManager().manipulationPoint,
                             e.getX(), e.getY(),
                             true);

            vc.draw();

            lastx = e.getX();
            lasty = e.getY();
            return true;
        }

        if ((mods&InputEvent.BUTTON3_DOWN_MASK)>0) {

            // if shift is down, make it so that people can rotate the
            // plane in the 2D view without adding a rotation in the
            // other direction.
            double qcum[] = new double[] {1, 0, 0, 0};

            double v1[] = new double[] { e.getX() - vc.getWidth()/2,
                                         - (e.getY() - vc.getHeight()/2),
                                         0};
            double v1mag = LinAlg.magnitude(v1);

            double v0[] = new double[] { lastx - vc.getWidth()/2,
                                         - (lasty - vc.getHeight()/2),
                                         0};
            double v0mag = LinAlg.magnitude(v0);

            v1 = LinAlg.normalize(v1);
            v0 = LinAlg.normalize(v0);

            double axis[] = LinAlg.crossProduct(v0, v1);
            double dotProd = LinAlg.dotProduct(v0, v1);

            boolean only_roll = shift && !ctrl;
            boolean only_pitch = shift && ctrl;
            boolean only_yaw = !shift && ctrl;
            boolean any_rotate = !shift && !ctrl;

            // for some rotations, we must convert a motion in pixels to a rotation.
            double pixelsToRadians = 1.0/200;

            double interfaceMode = vc.getViewManager().getInterfaceMode();

            if (interfaceMode < 2) {
                only_roll = false;
                only_pitch = false;
                only_yaw = false;
                any_rotate = false;
            }

            if (interfaceMode == 2.0)
                only_roll = true;

            if (only_roll) {
                if (dx != 0 || dy != 0) {
                    double theta = MathUtil.mod2pi(Math.atan2(v1[1], v1[0]) - Math.atan2(v0[1], v0[0]));
                    double p2eye[] = LinAlg.subtract(view.eye, view.lookAt);
                    if (!Double.isNaN(theta))
                        qcum = LinAlg.quatMultiply(qcum, LinAlg.angleAxisToQuat(-theta, p2eye));
                }
            } else if (only_yaw) {
                if (dx != 0 || dy != 0) {
                    double theta = dx * pixelsToRadians;
                    double p2eye[] = LinAlg.subtract(view.eye, view.lookAt);

                    if (interfaceMode < 3) {
                        if (!Double.isNaN(theta))
                            qcum = LinAlg.quatMultiply(qcum, LinAlg.angleAxisToQuat(-theta, new double[] {0,0,1}));
                    } else {
                        if (!Double.isNaN(theta))
                            qcum = LinAlg.quatMultiply(qcum, LinAlg.angleAxisToQuat(-theta, view.up));
                    }
                }
            } else if (only_pitch) {
                double p2eye[] = LinAlg.subtract(view.eye, view.lookAt);
                double left[] = LinAlg.crossProduct(view.up, p2eye);
                if (dx != 0 || dy != 0) {
                    double theta = dy * pixelsToRadians;
                    if (!Double.isNaN(theta))
                        qcum = LinAlg.quatMultiply(qcum, LinAlg.angleAxisToQuat(-theta, left));
                }
            } else if (any_rotate) {
                // unconstrained rotation
                qcum = LinAlg.quatMultiply(qcum, LinAlg.angleAxisToQuat(-dx*pixelsToRadians, view.up));

                double p2eye[] = LinAlg.subtract(view.eye, view.lookAt);
                double left[] = LinAlg.crossProduct(view.up, p2eye);

                qcum = LinAlg.quatMultiply(qcum, LinAlg.angleAxisToQuat(-dy*pixelsToRadians, left));
            }

            long time = System.currentTimeMillis();
            double dt = (time - mouseDraggedLastMTime)/1000.0;
            double aa[] = LinAlg.quatToAngleAxis(qcum);
            lastRotateAxis = new double[] {aa[1], aa[2], aa[3]};
            lastRotateRadPerSec = aa[0] / dt;
            mouseDraggedLastMTime = time;

            vc.getViewManager().viewGoal.rotate(qcum);

            lastx = e.getX();
            lasty = e.getY();

            wasRotating = true;
            return true;
        }

        return false;
    }

    /** rotate the scene based on a motion of the mouse. **/
    void rotate(double dx, double dy)
    {
        VisView view = vc.getLastView();

        double look[] = LinAlg.subtract(view.lookAt, view.eye);

        double xy_len = LinAlg.magnitude(look);
        double init_elevation = Math.atan2(look[2], xy_len);

        double left[] = LinAlg.crossProduct(view.up, look);
        left = LinAlg.normalize(left);

        double delevation = -dy * 0.005;

        double q[] = LinAlg.angleAxisToQuat(-delevation, left);
        double newlook[] = LinAlg.quatRotate(q, look);

        double dazimuth = -dx * 0.01;
        double q1[] = LinAlg.angleAxisToQuat(dazimuth, new double[] {0, 0, 1});
        newlook = LinAlg.quatRotate(q1, newlook);
        left = LinAlg.quatRotate(q1, left);

        double newEye[] = new double[] {view.lookAt[0] - newlook[0],
                                        view.lookAt[1] - newlook[1],
                                        view.lookAt[2] - newlook[2]};

        double newUp[] = LinAlg.crossProduct(newlook, left);

        vc.getViewManager().viewGoal.lookAt(newEye, view.lookAt, newUp);

        vc.draw();
    }

    double[] getManipulationPoint(VisView view, int winx, int winy)
    {
        double xyz[] = null;

/*
        if (false) {
            FloatImage fim = vc.getDepthBuffer();
            double z = fim.get(fim.getHeight() - winy, winx);
            xyz = view.unprojectPoint(winx, winy, z);

            double dist = LinAlg.distance(view.eye, xyz);
            if (dist < 1000)
                return xyz;
        }
*/

        double interfaceMode = vc.getViewManager().getInterfaceMode();

        double normlook2eye[] = LinAlg.normalize(LinAlg.subtract(view.eye, view.lookAt));
        double dot = LinAlg.dotProduct(normlook2eye, new double[] {0,0,1});

        // If we're in 2D mode and we're looking at the XY plane
        // from a reasonable overhead angle, then use the
        // intersection with the plane. Otherwise, use the point
        // on the line closest to the lookAt point.
        if (interfaceMode < 3) // && Math.abs(dot) > 0.8)
            xyz = view.computeRay(winx, winy).intersectPlaneXY();
        else
            xyz = view.computeRay(winx, winy).getLine().pointOnLineClosestTo(view.lookAt);

        return xyz;
    }

    public boolean mousePressed(VisCanvas vc,  GRay3D ray, MouseEvent e)
    {
        mvel.reset();
        mvel.update(e);
        wasRotating = false;

        lastx = e.getX();
        lasty = e.getY();

        mouseDown = true;

        VisViewManager viewManager = vc.getViewManager();

        double xyz[] = getManipulationPoint(vc.getLastView(), lastx, lasty);

        viewManager.setManipulationPoint(xyz);

        return true;
    }

    public boolean mouseWheelMoved(VisCanvas vc, GRay3D ray, MouseWheelEvent e)
    {
        int amount=e.getWheelRotation();
        int mods=e.getModifiersEx();
        boolean shift=(mods&MouseEvent.SHIFT_DOWN_MASK)>0;
        boolean control=(mods&MouseEvent.CTRL_DOWN_MASK)>0;

        VisView view = vc.getViewManager().viewGoal;

        double mp[] = getManipulationPoint(view, e.getX(), e.getY()); //view.viewport[2]/2, view.viewport[3]/2);

        if (amount > 0)
            zoom(shift ? ZOOM_FAST : ZOOM_SLOW);
        else
            zoom(shift ? 1.0/ZOOM_FAST : 1.0/ZOOM_SLOW);

        // We want to have the part of the scene under the mouse
        // remain motionless during a zoom. I.e., the mouse is the
        // center of expansion when zooming.
        //
        // We do this by now performing a pan: we pick a point on the
        // ray that is close to the lookAt point and make sure that it
        // doesn't move. (The zoom above just made it move, so our pan
        // will move it back.)

        // the signs are tricky and confusing.
        windowSpacePanTo(view, mp, e.getX(), e.getY(), false);

        vc.drawNow();

        return true;
    }

    static final double sq(double v)
    {
        return v*v;
    }

    void zoom(double ratio)
    {
        VisView view = vc.getViewManager().viewGoal;

        double dist = LinAlg.distance(view.eye, view.lookAt) * ratio;

        dist = Math.max(vc.getViewManager().zoom_dist_min, dist);
        dist = Math.min(vc.getViewManager().zoom_dist_max, dist);

        double look2eye[] = LinAlg.scale(LinAlg.normalize(LinAlg.subtract(view.eye, view.lookAt)), dist);

        view.lookAt(LinAlg.add(view.lookAt, look2eye),
                    view.lookAt,
                    view.up);
    }

    Matrix computePanJacobian(VisView view, double dq[], double up[], double left[])
    {
        double eps = 0.00001;
        double win_dq[] = new double[3];
        GLU glu = new GLU();

        double model_matrix[] = view.getModelViewMatrix().getColumnPackedCopy();
        double proj_matrix[] = view.getProjectionMatrix().getColumnPackedCopy();

        glu.gluProject(dq[0], dq[1], dq[2],
                       model_matrix, 0, proj_matrix, 0, view.viewport, 0, win_dq, 0);

        double win_dq_up[] = new double[3];
        glu.gluProject(dq[0] + eps*up[0], dq[1] + eps*up[1], dq[2] + eps*up[2],
                       model_matrix, 0, proj_matrix, 0, view.viewport, 0, win_dq_up, 0);

        double win_dq_left[] = new double[3];
        glu.gluProject(dq[0] + eps*left[0], dq[1] + eps*left[1], dq[2] + eps*left[2],
                       model_matrix, 0,proj_matrix, 0, view.viewport, 0, win_dq_left, 0);

        // Jacobian of window panning with respect to motions in up and left directions.
        Matrix A = new Matrix(2, 2);
        A.set(0,0, (win_dq_up[0]   - win_dq[0]) / eps);
        A.set(0,1, (win_dq_left[0] - win_dq[0]) / eps);
        A.set(1,0, (win_dq_up[1]   - win_dq[1]) / eps);
        A.set(1,1, (win_dq_left[1] - win_dq[1]) / eps);

        return A;
    }

    void windowSpacePanTo(VisView view, double dq[], double x, double y, boolean preservez)
    {
        for (int i = 0; i < 10; i++) {
            double res = _windowSpacePanTo(view, dq, x, y, preservez);
            if (res < 0.001)
                break;
        }
    }

    /** Given a coordinate in scene coordinates dq, modify the camera
        such that the screen projection of point dq moves to pixel
        coordinates (x,y). If preservez is used, panning is
        implemented differently so that the height of the camera does
        not change.

        Returns the step size.
    **/
    double _windowSpacePanTo(VisView view, double dq[], double x, double y, boolean preservez)
    {
        VisViewManager viewManager = vc.getViewManager();

        y = view.viewport[3] - y;

        double win_dq[] = new double[3];
        GLU glu = new GLU();
        glu.gluProject(dq[0], dq[1], dq[2],
                       view.getModelViewMatrix().getColumnPackedCopy(), 0,
                       view.getProjectionMatrix().getColumnPackedCopy(),
                       0, view.viewport, 0, win_dq, 0);

        double up[] = LinAlg.normalize(view.up);
        double lookVec[] = LinAlg.normalize(LinAlg.subtract(view.lookAt, view.eye));
        double left[] = LinAlg.crossProduct(view.up, lookVec);

        if (preservez) {
            up = new double[] {1, 0, 0};
            left = new double[] {0, 1, 0};
        }

        Matrix A = computePanJacobian(view, dq, up, left);
        double detBefore = A.det();
        // How far did we want to move?
        Matrix b = new Matrix(2,1);
        b.set(0,0, x - win_dq[0]);
        b.set(1,0, y - win_dq[1]);

        Matrix sol = null;
        if (true) {
            // use pseudo-inverse of A for robustness.

            SingularValueDecomposition svd = new SingularValueDecomposition(A);
            double s[] = svd.getSingularValues();
            for (int i = 0; i < s.length; i++) {
                if (s[i] < 0.0001)
                    s[i] = 0;
                else
                    s[i] = 1.0 / s[i];
            }

            Matrix Ainv = svd.getV().times(Matrix.diag(s)).times(svd.getU().transpose());
            sol = Ainv.times(b);
        }

        // Ax = b, where x = how much we should move in the up and left directions.
        //	Matrix sol = A.solve(b);
        double motion[] = LinAlg.add(LinAlg.scale(up, sol.get(0,0)),
                                     LinAlg.scale(left, sol.get(1,0)));

        if (true) {
            double oldEye[] = LinAlg.copy(view.eye);
            double oldLookAt[] = LinAlg.copy(view.lookAt);
            double oldUp[] = LinAlg.copy(view.up);

            double olderr = Math.sqrt(LinAlg.sq(x - win_dq[0]) + LinAlg.sq(y - win_dq[1]));

            // try performing the move. But if it makes the pan
            // jacobian ill-conditioned, un-do the move.
            viewManager.viewGoal.lookAt(LinAlg.subtract(view.eye, motion),
                                        LinAlg.subtract(view.lookAt, motion),
                                        view.up);
            view = viewManager.viewGoal;

            A = computePanJacobian(view, dq, up, left);
            double detAfter = A.det();

            glu.gluProject(dq[0], dq[1], dq[2],
                           view.getModelViewMatrix().getColumnPackedCopy(), 0,
                           view.getProjectionMatrix().getColumnPackedCopy(),
                           0, view.viewport, 0, win_dq, 0);

            double newerr = Math.sqrt(LinAlg.sq(x - win_dq[0]) + LinAlg.sq(y - win_dq[1]));

            if (detAfter < detBefore && detAfter < 0.01) {
                viewManager.viewGoal.lookAt(oldEye, oldLookAt, oldUp);
            }

        }

        return sol.normF();
    }

    public String getName()
    {
        return "Camera Control";
    }

    class MouseVelocity extends VisCanvasEventAdapter
    {
        int lastxy[] = new int[2];
        long lastms;

        double vel[] = new double[2];

        VelocityEstimate velEstimate = new VelocityEstimate(0.5);

        public void update(MouseEvent e)
        {
            velEstimate.update(System.currentTimeMillis()/1000.0, new double[] { e.getX(), e.getY() });

            VelocityEstimate.Prediction pred = velEstimate.getPrediction(System.currentTimeMillis()/1000.0);
        }

        public double[] getVelocity()
        {
            VelocityEstimate.Prediction pred = velEstimate.getPrediction(System.currentTimeMillis()/1000.0);
            if (pred==null)
                return new double[2];

            // a high chi2 indicates they were accelerating, i.e., not
            // trying to spin us around. Most likely, just letting go
            // of the mouse.
            if (LinAlg.magnitude(pred.mse) > 20)
                return new double[2];

            // truncate really low velocities.
            double vel[] = pred.velocity;
            if (LinAlg.magnitude(vel) < 5)
                return new double[2];

            return pred.velocity;
        }

        public void reset()
        {
            velEstimate.reset();
        }

        public String getName()
        {
            return "MouseVelocity monitor";
        }
    }

    class AnimateThread extends Thread
    {
        Bookmark src, dest; // when going to a bookmark
        double bookmarkStartTime;

        public void run()
        {
            VisViewManager viewManager = vc.getViewManager();

            while (true) {

                // how long should we wait between updates?
                double dt = 1.0 / vc.targetFPS;

                // are we going to a bookmark?
                if (dest != null) {
                    double elapsed = System.currentTimeMillis()/1000.0 - bookmarkStartTime;

                    // total time for the animation?
                    double totalTime = 1.5;

                    double frac = Math.min(1.0, elapsed/totalTime); // scaled time, [0,1]

                    // our interpolation function.
                    double prog = (Math.sin(Math.PI*frac - Math.PI/2)+1)/2.0;

                    viewManager.viewGoal.lookAt(LinAlg.add(LinAlg.scale(src.eye, 1-prog), (LinAlg.scale(dest.eye, prog))),
                                                LinAlg.add(LinAlg.scale(src.lookat, 1-prog), (LinAlg.scale(dest.lookat, prog))),
                                                LinAlg.add(LinAlg.scale(src.up, 1-prog), (LinAlg.scale(dest.up, prog))));

                    viewManager.viewGoal.perspectiveness = src.perspectiveness * (1-prog) + dest.perspectiveness*prog;

                    // done with animation?
                    if (frac == 1.0) {
                        zoomRate = dest.zoomRate;
                        rotateRadPerSec = dest.rotateRadPerSec;
                        rotateAxis = dest.rotateAxis;
                        dest = null;
                    }

                    vc.canvas.repaint();

                } else {
                    // no, just a zoom/rotate animation.

                    if (zoomRate != 1.0) {
                        zoom(1.0 + (zoomRate-1.0)*dt);
                        vc.canvas.repaint();
                    }

                    if (!mouseDown) {
                        // if the direction is very dominantly in one axial direction, zero out
                        // the other direction. (probably user error).
                        double thresh = 5.0;

                        /*
                          if (Math.abs(rotateVelocity[0]) > thresh*Math.abs(rotateVelocity[1]))
                          rotateVelocity[1] = 0;

                          if (Math.abs(rotateVelocity[1]) > thresh*Math.abs(rotateVelocity[0]))
                          rotateVelocity[0] = 0;

                          if (rotateVelocity[0]!=0 || rotateVelocity[1]!=0)
                          rotate(rotateVelocity[0]*dt, rotateVelocity[1]*dt);
                        */
                        if (rotateRadPerSec != 0) {
                            viewManager.viewGoal.rotate(LinAlg.angleAxisToQuat(rotateRadPerSec*dt, rotateAxis));
                            vc.canvas.repaint();
                        }
                    }
                }

                try {
                    Thread.sleep((int) (dt*1000));
                } catch (InterruptedException ex) {
                }
            }
        }

        public void goToBookmark(Bookmark dest)
        {
            this.src = new Bookmark(vc.getLastView(), rotateAxis, rotateRadPerSec, zoomRate);
            this.dest = dest;
            this.bookmarkStartTime = System.currentTimeMillis()/1000.0;
        }
    }

    public void doHelp(HelpOutput houts)
    {
        houts.beginMouseCommands(this);
        houts.addMouseCommand(this, HelpOutput.LEFT | HelpOutput.DRAG, "Pan");
        houts.addMouseCommand(this, HelpOutput.RIGHT | HelpOutput.DRAG, "Rotate");
        houts.addMouseCommand(this, HelpOutput.RIGHT | HelpOutput.DRAG | HelpOutput.SHIFT, "Rotate (roll)");
        houts.addMouseCommand(this, HelpOutput.RIGHT | HelpOutput.DRAG | HelpOutput.CTRL, "Rotate (yaw)");
        houts.addMouseCommand(this, HelpOutput.RIGHT | HelpOutput.DRAG | HelpOutput.SHIFT | HelpOutput.CTRL, "Rotate (pitch)");
        houts.addMouseCommand(this, HelpOutput.RIGHT | HelpOutput.ALT | HelpOutput.DRAG, "Rotate (Animate)");
        houts.addMouseCommand(this, HelpOutput.WHEEL, "Zoom");
        houts.addMouseCommand(this, HelpOutput.WHEEL | HelpOutput.SHIFT, "Zoom (fast)");
        houts.addMouseCommand(this, HelpOutput.RIGHT | HelpOutput.CLICK, "Options Menu");

        houts.beginKeyboardCommands(this);
        houts.addKeyboardCommand(this, "Arrows", 0, "Pan");
        houts.addKeyboardCommand(this, "Arrows", HelpOutput.SHIFT, "Rotate (fast)");
        houts.addKeyboardCommand(this, "Arrows", HelpOutput.CTRL, "Rotate (Animate)");
        houts.addKeyboardCommand(this, "PgUp/PgDn", 0, "Zoom");
        houts.addKeyboardCommand(this, "PgUp/PgDn", HelpOutput.SHIFT, "Zoom (fast)");
        houts.addKeyboardCommand(this, "Fn X", HelpOutput.CTRL, "Set camera bookmark X");
        houts.addKeyboardCommand(this, "Fn X", 0, "Go to camera bookmark X");
    }

}
