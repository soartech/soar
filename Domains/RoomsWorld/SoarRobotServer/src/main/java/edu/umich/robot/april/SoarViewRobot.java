package edu.umich.robot.april;

import java.awt.Color;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.util.TimerTask;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import lcm.lcm.LCM;
import april.config.Config;
import april.config.ConfigUtil;
import april.jmat.LinAlg;
import april.jmat.geom.GRay3D;
import april.lcmtypes.pose_t;
import april.util.PoseTracker;
import april.viewer.ViewObject;
import april.viewer.Viewer;
import april.vis.HelpOutput;
import april.vis.VisCanvas;
import april.vis.VisCanvasEventAdapter;
import april.vis.VisChain;
import april.vis.VisCylinder;
import april.vis.VisObject;
import april.vis.VisRobot;
import april.vis.VisText;
import april.vis.VisViewManager;
import april.vis.VisWorld;

/**
 * VisCanvasEventAdapter that draws a robot and allows teleportation and
 * selection.
 **/
public class SoarViewRobot extends VisCanvasEventAdapter implements ViewObject
{
    public enum FollowMode
    {
        FOLLOW_DISABLED(0), FOLLOW_XY(1), FOLLOW_XY_THETA(2);

        int value;

        FollowMode(int value)
        {
            this.value = value;
        }

        String prettyString()
        {
            return this.name().replace('_', ' ').toLowerCase();
        }
    }

    Viewer viewer;
    String name;
    Config config;
    LCM lcm = LCM.getSingleton();
    VisRobot vrobot = new VisRobot();
    VisObject vavatar;
    VisObject vSelectionCircle;
    java.util.Timer leadTimer;
    LeadTask leadTask = new LeadTask();
    pose_t pose;
    FollowMode followMode = FollowMode.FOLLOW_DISABLED;
    double lastRobotPos[] = new double[] { 0, 0, 0 };
    double lastRobotQuat[] = new double[] { 1, 0, 0, 0 };
    PoseTracker pt;
    ScheduledExecutorService exec = Executors.newSingleThreadScheduledExecutor();

    boolean selected;

    /** If interactive, teleports are enabled. **/
    public SoarViewRobot(Viewer viewer, String name, Config config)
    {
        this.viewer = viewer;
        this.name = name;
        this.config = config;
        this.pt = new PoseTracker("POSE_" + name, 10.0);
        viewer.getVisCanvas().addEventHandler(this, 0);
        vrobot.color = Color.cyan;

        double spos[] = config.getDoubles("avatar.position", null);
        double squat[] = ConfigUtil.getQuaternion(config, "avatar");
        boolean model4 = config.getBoolean("model4", false);
        if (spos != null && squat != null)
        {
            VisObject robot = model4 ? new Model4(null, -1, Color.lightGray, 1.0) : ModelS.createRobot(Color.BLUE);
            vavatar = new VisChain(squat, spos, robot);
            vSelectionCircle = new VisCylinder(0.5, 0.01, new Color(.1f, .4f, 1f));
        }

        exec.scheduleAtFixedRate(new Runnable()
        {
            @Override
            public void run()
            {
                pose = pt.get();
                if (pose == null)
                {
                    // System.out.println(ViewRobot.this.name + " pose null");
                    return;
                }
                VisWorld.Buffer vb = SoarViewRobot.this.viewer.getVisWorld().getBuffer("Robot " + SoarViewRobot.this.name);
                // vb.addBuffered(new VisChain(pose.orientation, pose.pos,
                // vrobot));
                if (vavatar != null)
                {
                    vb.addBuffered(new VisChain(pose.orientation, pose.pos, vavatar));
                }

                if (isSelected())
                {
                    vb.addBuffered(new VisChain(pose.orientation, pose.pos, vSelectionCircle));
                }

                vb.switchBuffer();
                if (followMode != FollowMode.FOLLOW_DISABLED)
                {
                    VisViewManager viewManager = SoarViewRobot.this.viewer.getVisCanvas().getViewManager();
                    viewManager.follow(lastRobotPos, lastRobotQuat, pose.pos, pose.orientation, followMode == FollowMode.FOLLOW_XY_THETA);
                }
                lastRobotPos = LinAlg.copy(pose.pos);
                lastRobotQuat = LinAlg.copy(pose.orientation);
            }
        }, 0, 50, TimeUnit.MILLISECONDS);
    }

    @Override
    public boolean mouseDragged(VisCanvas vc, GRay3D ray, MouseEvent e)
    {
        if (!vc.isPicking(this)) return false;

        // TODO SoarApril
        /*
         * if (!isSelected()) return false;
         */
        if (pose == null) return false;
        int mods = e.getModifiersEx();
        boolean shift = (mods & MouseEvent.SHIFT_DOWN_MASK) > 0;
        boolean ctrl = (mods & MouseEvent.CTRL_DOWN_MASK) > 0;
        boolean alt = shift & ctrl;
        ctrl = ctrl & (!alt);
        shift = shift & (!alt);
        // only left mouse button.
        if ((mods & InputEvent.BUTTON1_DOWN_MASK) == 0) return false;
        if (!shift && !ctrl)
        {
            // translate. Move robot to the mouse position
            double pos[] = ray.intersectPlaneXY(pose.pos[2]);
            pose.pos = pos;
        }
        if (shift)
        {
            // rotate. Point the robot towards the mouse position
            double pos[] = ray.intersectPlaneXY(pose.pos[2]);
            double theta = Math.atan2(pos[1] - pose.pos[1], pos[0] - pose.pos[0]);
            pose.orientation = LinAlg.angleAxisToQuat(theta, new double[] { 0, 0, 1 });
        }
        if (ctrl)
        {
            if (leadTimer == null)
            {
                leadTask = new LeadTask();
                leadTask.ray = ray;
                leadTimer = new java.util.Timer(true);
                leadTimer.schedule(leadTask, 0, (int) (1000.0 / LeadTask.HZ));
            }
            leadTask.ray = ray;
            return true;
        }
        if (leadTimer != null)
        {
            leadTimer.cancel();
            leadTimer = null;
        }
        lcm.publish("POSE_TELEPORT", pose);
        return true;
    }

    class LeadTask extends TimerTask
    {
        GRay3D ray;
        static final double MAX_VELOCITY = 0.5; // m/s
        static final double MAX_ANGULAR_VELOCITY = Math.PI; // rad/s
        static final int HZ = 40;
        static final double DT = 1.0 / HZ;

        @Override
        public void run()
        {
            // lead the robot.
            double pos[] = ray.intersectPlaneXY(pose.pos[2]);
            double distance = LinAlg.distance(pos, pose.pos);
            if (distance < 0.3) return;
            double theta = Math.atan2(pos[1] - pose.pos[1], pos[0] - pose.pos[0]);
            pose.orientation = LinAlg.angleAxisToQuat(theta, new double[] { 0, 0, 1 });
            double err[] = LinAlg.subtract(pos, pose.pos);
            LinAlg.scale(err, .1); // P control, sort of.
            pose.pos = LinAlg.add(pose.pos, err);
            lcm.publish("POSE_TELEPORT", pose);
        }
    }

    @Override
    public boolean mousePressed(VisCanvas vc, GRay3D ray, MouseEvent e)
    {
        if (e.getButton() == MouseEvent.BUTTON1)
        {
            // TODO SoarApril

            if (vc.isPicking(this))
            {
                setSelected(false);
                return true;
            }
            /*
            if (isSelected())
            {
                setSelected(true);
            }
            */

        }
        return false;
    }

    @Override
    public boolean mouseReleased(VisCanvas vc, GRay3D ray, MouseEvent e)
    {
        if (!vc.isPicking(this)) return false;
        if (leadTimer != null)
        {
            leadTimer.cancel();
            leadTimer = null;
        }
        vc.releasePick();
        return true;
    }

    @Override
    public double pickQuery(VisCanvas vc, GRay3D ray)
    {
        return queryDistance(vc, ray);
    }

    @Override
    public void pickNotify(boolean winner)
    {
        vrobot.setSelectedState(winner ? 2 : 0);
    }

    @Override
    public double hoverQuery(VisCanvas vc, GRay3D ray)
    {
        return queryDistance(vc, ray);
    }

    @Override
    public void hoverNotify(boolean winner)
    {
        vrobot.setSelectedState(winner ? 1 : 0);
    }

    protected double queryDistance(VisCanvas vc, GRay3D ray)
    {
        if (pose == null) return -1;
        double pos[] = ray.intersectPlaneXY();
        double dist = Math.sqrt(LinAlg.sq(pos[0] - pose.pos[0]) + LinAlg.sq(pos[1] - pose.pos[1]));
        return dist < 1 ? dist : -1;
    }

    @Override
    public String getName()
    {
        return name;
    }

    VisObject lastFollowTemporary;

    // int findCount;
    // boolean lastCharT = false;

    @Override
    public boolean keyTyped(VisCanvas vc, KeyEvent e)
    {
        // if (e.getKeyChar() == 'f')
        // {
        // // Follow mode
        // followMode = (followMode + 1) % 3;
        // vc.getWorld().removeTemporary(lastFollowTemporary);
        // lastFollowTemporary = new VisText(VisText.ANCHOR.CENTER,
        // followString[followMode]);
        // vc.getWorld().addTemporary(lastFollowTemporary, 1.0);
        // return true;
        // }
        // if (e.getKeyChar() == 'F' && lastRobotPos != null)
        // {
        // findCount++;
        // if (findCount % 2 == 1)
        // {
        // // FIND.
        // VisViewManager viewManager = vc.getViewManager();
        // viewManager.lookAt(LinAlg.add(lastRobotPos, new double[] { 0, 0, 10
        // }), lastRobotPos, new double[] { 0, 1, 0 });
        // }
        // else
        // {
        // VisViewManager viewManager = vc.getViewManager();
        // double rpy[] = LinAlg.quatToRollPitchYaw(lastRobotQuat);
        // double behindDist = 5;
        // viewManager.lookAt(LinAlg.add(lastRobotPos, new double[] {
        // Math.cos(-rpy[2]) * behindDist, Math.sin(-rpy[2]) * behindDist, 2 }),
        // lastRobotPos, new double[] { 0, 0, 1 });
        // }
        // return true;
        // }
        // if (lastCharT)
        // {
        // if (e.getKeyChar() == 'c')
        // {
        // trajectory.clear();
        // }
        // }
        // lastCharT = e.getKeyChar() == 't';
        return false;
    }

    public boolean setFollowMode(FollowMode mode, VisCanvas vc)
    {
        // TODO SoarApril
        /*
         * if (isSelected()) { viewer.setSelectedFollowMode(mode); }
         */
        followMode = mode;
        vc.getWorld().removeTemporary(lastFollowTemporary);
        lastFollowTemporary = new VisText(VisText.ANCHOR.BOTTOM, mode.prettyString());
        vc.getWorld().addTemporary(lastFollowTemporary, 1.0);
        return true;
    }

    public boolean moveCameraToRobot(int mode, VisCanvas vc)
    {
        VisViewManager viewManager = vc.getViewManager();
        double rpy[] = LinAlg.quatToRollPitchYaw(lastRobotQuat);
        if (mode == 1)
        {
            // Above robot
            double[] eye = LinAlg.add(lastRobotPos, new double[] { 0, 0, 10 });
            double[] lookAt = lastRobotPos;
            double[] up = new double[] { Math.cos(rpy[2]), Math.sin(rpy[2]), 0 };
            viewManager.viewGoal.lookAt(eye, lookAt, up);
        }
        else
        {
            // Behind robot
            double behindDist = 1.2;
            double height = 1;
            double[] eye = LinAlg.add(lastRobotPos, new double[] { Math.cos(rpy[2] + Math.PI) * behindDist, Math.sin(rpy[2] + Math.PI) * behindDist, height });
            double[] lookAt = LinAlg.add(lastRobotPos, new double[] { 0, 0, height - .3 });
            double[] up = new double[] { 0, 0, 1 };
            viewManager.viewGoal.lookAt(eye, lookAt, up);
        }
        return true;
    }

    @Override
    public void doHelp(HelpOutput houts)
    {
        houts.beginMouseCommands(this);
        houts.addMouseCommand(this, HelpOutput.LEFT | HelpOutput.DRAG, "Teleport (translate)");
        houts.addMouseCommand(this, HelpOutput.LEFT | HelpOutput.DRAG | HelpOutput.SHIFT, "Teleport (rotate)");
        houts.beginKeyboardCommands(this);
        houts.addKeyboardCommand(this, "f", HelpOutput.CTRL, "Cycle through follow modes");
        houts.addKeyboardCommand(this, "F", HelpOutput.CTRL, "Find robot");
    }

    public boolean isSelected()
    {
        return selected;
    }

    public void setSelected(boolean selected)
    {
        this.selected = selected;
    }

    public FollowMode getFollowMode()
    {
        return followMode;
    }
}
