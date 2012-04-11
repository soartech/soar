package april.laser.scanmatcher;

import java.awt.*;
import java.awt.image.*;
import java.io.*;
import javax.swing.*;
import java.util.*;

import april.config.*;
import april.jmat.*;
import april.util.*;
import april.vis.*;
import april.graph.*;
import april.lcmtypes.*;

import lcm.lcm.*;

public class ScanMatcherTest implements LCMSubscriber, ParameterListener
{
    Config      config;
    ScanMatcher scanMatcher;

    JFrame jf;
    VisWorld vw = new VisWorld();
    VisCanvas vc = new VisCanvas(vw);

    LCM lcm = LCM.getSingleton();
    ParameterGUI pg = new ParameterGUI();

    ArrayList<pose_t> poses = new ArrayList<pose_t>();

    String channel = "LIDAR";

    public static void main(String args[])
    {
        Config config = ConfigUtil.getDefaultConfig(args);

        new ScanMatcherTest(config);
    }

    public ScanMatcherTest(Config config)
    {
        this.config = config;

        pg.addCheckBoxes("showallscans", "Show all scans", true);

        pg.addButtons("clear", "clear");
        pg.addListener(this);

        jf = new JFrame("ScanMatcherTest");
        jf.setLayout(new BorderLayout());
        jf.add(vc, BorderLayout.CENTER);
        jf.add(pg.getPanel(), BorderLayout.SOUTH);
        jf.setSize(600,400);
        jf.setVisible(true);

        Config smconfig = config.getChild("scanmatcher_manager");
        this.channel = smconfig.getString("channel", channel);

        scanMatcher = new ScanMatcher(config.getChild("scanmatcher"));

        lcm.subscribe("POSE", this);
        lcm.subscribe(channel, this);
      }

    public void parameterChanged(ParameterGUI pg, String name)
    {
        if (name.equals("clear")) {
            scanMatcher = new ScanMatcher(config.getChild("scanmatcher"));
            vw.clear();
        }
    }

    public void messageReceived(LCM lcm, String channel, LCMDataInputStream ins)
    {
        try {
            messageReceivedEx(channel, ins);
        } catch (IOException ex) {
            System.out.println("ex: "+ex);
        }
    }

    void messageReceivedEx(String channel, LCMDataInputStream ins) throws IOException
    {
        ///////////////////////////////////////////////////////////
        if (channel.equals("POSE")) {
            pose_t p = new pose_t(ins);
            poses.add(p);

            pose_t lastPose = poses.get(poses.size()-1);

            double lastxyt[] = poseToXyt(lastPose);
            double nowxyt[] = poseToXyt(p);

            double odomT[] = LinAlg.xytInvMul31(lastxyt, nowxyt);
            scanMatcher.processOdometry(odomT, null);

            return;
        }

       ///////////////////////////////////////////////////////////
        if (channel.equals(this.channel)) {
            laser_t ldata = new laser_t(ins);

            //////////////////////////////
            // Project the points
            ArrayList<double[]> points = new ArrayList<double[]>();

            double maxRange = config.getRoot().getDouble(channel+".max_range", Double.MAX_VALUE);

            double mask_out_rad[] = config.getRoot().getDoubles(channel+".mask_out_deg", null);
            if (mask_out_rad != null) {
                for (int i = 0; i < mask_out_rad.length; i++)
                    mask_out_rad[i] = Math.toRadians(mask_out_rad[i]);
            }

            for (int i = 0; i < ldata.nranges; i++) {
                double theta = ldata.rad0 + ldata.radstep * i;
                double r = ldata.ranges[i];

                if (r > maxRange)
                    continue;

                if (mask_out_rad != null) {
                    boolean mask = false;

                    for (int j = 0; j < mask_out_rad.length; j+=2) {
                        if (mask_out_rad[j] <= theta && theta <= mask_out_rad[j+1])
                            mask = true;
                    }

                    if (mask)
                        continue;
                }

                double p[] = new double[] {
                    r * Math.cos(theta),
                    r * Math.sin(theta) };

                points.add(p);
            }

            if (points.size() == 0)
                return;

            // sensor to body
            double S2B[][] = ConfigUtil.getRigidBodyTransform(config.getRoot(), channel);

            ArrayList<double[]> bodyPoints = LinAlg.transform(S2B, points);

            scanMatcher.processScan(bodyPoints);

            if (true) {
                VisWorld.Buffer vb = vw.getBuffer("lastscan");
                vb.addBuffered(new VisChain(LinAlg.xytToMatrix(scanMatcher.getPosition()),
                                            new VisRobot(Color.red),
                                            new VisData(new VisDataPointStyle(Color.red, 2),
                                                        bodyPoints)));
                vb.switchBuffer();
            }

            if (true) {
                VisWorld.Buffer vb = vw.getBuffer("raster");
                vb.setDrawOrder(-100);
                GridMap gm = scanMatcher.getGridMap();
                if (gm != null) {
                    BufferedImage im = gm.makeBufferedImage();
                    vb.addBuffered(new VisImage(new VisTexture(im), gm.getXY0(), gm.getXY1()));
                }
                vb.switchBuffer();
            }

            if (true) {
                VisWorld.Buffer vb = vw.getBuffer("graph");
                vb.setDrawOrder(-99);
                Graph g = scanMatcher.getGraph();

                for (GNode gn : g.nodes) {
                    ArrayList<double[]> p = (ArrayList<double[]>) gn.getAttribute("points");
                    vb.addBuffered(new VisChain(LinAlg.xytToMatrix(gn.state),
                                                new VisRobot(Color.blue)));

                    if (pg.gb("showallscans")) {
                        vb.addBuffered(new VisChain(LinAlg.xytToMatrix(gn.state),
                                                    new VisData(new VisDataPointStyle(Color.blue, 1),
                                                                p)));
                    }
                }

                vb.switchBuffer();
            }
        }

    }


    static double[] poseToXyt(pose_t p)
    {
        double rpy[] = LinAlg.quatToRollPitchYaw(p.orientation);
        return new double[] { p.pos[0], p.pos[1], rpy[2] };
    }
 }
