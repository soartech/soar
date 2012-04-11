package april.image.corner;

import java.awt.BorderLayout;
import java.awt.Color;
import java.io.IOException;
import java.util.ArrayList;

import javax.swing.JFrame;

import lcm.lcm.LCM;
import lcm.lcm.LCMDataInputStream;
import lcm.lcm.LCMSubscriber;
import april.image.Corner;
import april.jmat.LinAlg;
import april.lcmtypes.laser_t;
import april.lcmtypes.pose_t;
import april.vis.VisCanvas;
import april.vis.VisData;
import april.vis.VisDataPointStyle;
import april.vis.VisView;
import april.vis.VisWorld;

public class LaserExample implements LCMSubscriber
{
    LCM          lcm;

    JFrame       jf;
    VisWorld     vw = new VisWorld();
    VisCanvas    vc = new VisCanvas(vw);
    VisView      view = vc.getViewManager().viewGoal;

    LaserHarris  lh=new LaserHarris();

    laser_t lastLaser;
    pose_t lastPose;
    boolean hasLaser=false, hasPose=false;

    String laserName = "LIDAR_FRONT";
    String poseName = "POSE_TRUTH";
    ArrayList<double[]>allFeature=new ArrayList<double[]>();
    ArrayList<ArrayList>allPoint=new ArrayList<ArrayList>();

    public LaserExample() throws IOException
    {
        jf = new JFrame("SLAMv1.FeatureDetector");
        jf.setLayout(new BorderLayout());

        jf.add(vc, BorderLayout.CENTER);

        jf.setSize(800,600);
        jf.setVisible(true);
        lcm=new LCM();
        lcm.subscribe(laserName, this);
        lcm.subscribe(poseName, this);

        setCamera();
    }

    public void messageReceived(LCM lcm, String channel, LCMDataInputStream ins)
    {
        if (channel.equals(laserName))
        {
            try {
                lastLaser = new laser_t(ins);
            } catch (IOException e) {}
            hasLaser=true;
        }

        if (channel.equals(poseName))
        {
            try {
                lastPose = new pose_t(ins);
            } catch (IOException e) {}
            hasPose=true;
        }

        if (hasLaser&&hasPose)
        {
            double timeDif=lastLaser.utime-lastPose.utime;
            if (Math.abs(timeDif)<1000)
            {
                messageReceivedEx(lastLaser,lastPose);
            }
            else
            {
                if(timeDif>0)
                    hasPose=false;
                else
                    hasLaser=false;
            }
        }
    }

    void messageReceivedEx(laser_t l,pose_t pose)
    {
        ArrayList<Corner>harrisResults=lh.extractFeatures(l);
        double[] currentPose=lastPose.pos;
        currentPose[2]=LinAlg.quatToRollPitchYaw(lastPose.orientation)[2];
        VisWorld.Buffer vbFeature = vw.getBuffer("features");
        VisWorld.Buffer vbMap = vw.getBuffer("map");
        for (Corner c : harrisResults)
        {
            allFeature.add(LinAlg.transform(currentPose, new double[]{c.x,c.y}));
        }
        vbMap.addBuffered(new VisData(allFeature,new VisDataPointStyle(Color.red,4)));
        ArrayList<double[]>points=LaserHarris.laserToPoints(l, 10000, 0);
        allPoint.add(LinAlg.transform(currentPose, points));
        for (ArrayList<double[]>pointSet:allPoint)
            vbMap.addBuffered(new VisData(pointSet,new VisDataPointStyle(Color.yellow,1)));
        vbMap.switchBuffer();
        vbFeature.switchBuffer();
    }


    private void setCamera()
    {
        view.perspective_fovy_degrees = 45;
        view.eye =    new double[] { 0, 0, 100};
        view.lookAt = new double[] { 0, 0, 0};
        view.up =     new double[] { 0, 1, 0};
    }

    public static void main(String arg[])
    {
        try {
            new LaserExample();
        } catch (IOException e){}
    }
}
