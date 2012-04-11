package april.laser;

import lcm.logging.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import javax.swing.*;

import april.vis.*;
import april.jmat.*;
import april.jmat.geom.*;
import april.util.*;

import april.lcmtypes.*;

public class FeatureTest implements ParameterListener
{
    JFrame jf;

    // map display
    VisWorld     vwm       = new VisWorld();
    VisCanvas    vcm       = new VisCanvas(vwm);

    // display for 'a' slider data
    VisWorld     vwa       = new VisWorld();
    VisCanvas    vca       = new VisCanvas(vwa);

    // display for 'b' slider data
    VisWorld     vwb       = new VisWorld();
    VisCanvas    vcb       = new VisCanvas(vwb);

    ParameterGUI pg = new ParameterGUI();

    // one pose for every laser.
    ArrayList<laser_t> lasers = new ArrayList<laser_t>();
    ArrayList<pose_t> poses = new ArrayList<pose_t>();

    ArrayList<FeatureTestComponent> components = new ArrayList<FeatureTestComponent>();

    public FeatureTest(String args[]) throws IOException
    {
        // read everything
        Log log = new Log(args[0], "r");
        try {
            pose_t lastpose = null;

            while (true) {
                Log.Event e = log.readNext();

                if (e.channel.equals("POSE")) {
                    lastpose = new pose_t(e.data);
                }

                if (e.channel.contains("LIDAR")) {
                    laser_t las = new laser_t(e.data);
                    if (lastpose != null) {
                        lasers.add(las);
                        poses.add(lastpose);
                    }
                }
            }
        } catch (IOException ex) {
        }
        System.out.println("Read "+lasers.size()+" laser events");

        // register components
        components.add(new PointComponent());
        components.add(new ContourComponent());
        components.add(new LineComponent());

        // set up main GUI
        pg.addIntSlider("apos", "Position A", 0, lasers.size()-1, 0);
        pg.addIntSlider("bpos", "Position B", 0, lasers.size()-1, 0);
        for (FeatureTestComponent comp : components) {
            pg.addCheckBoxes("show:"+comp.getName(), "Show "+comp.getName(), true);
        }

        pg.addListener(this);

        jf = new JFrame(this.getClass().getName());
        jf.setLayout(new BorderLayout());
        JSplitPane jsp = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, vca, vcb);
        jsp.setDividerLocation(0.5);
        jsp.setResizeWeight(0.5);
        JSplitPane jsp2 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, vcm, jsp);
        jsp2.setDividerLocation(0.33);
        jsp2.setResizeWeight(0.33);
        jf.add(jsp2, BorderLayout.CENTER);

        JTabbedPane jtb = new JTabbedPane();
        jtb.addTab("Navigation", pg);

        for (FeatureTestComponent comp : components) {
            Component c = comp.getPanel();
            if (c != null)
                jtb.addTab(comp.getName(), c);
        }

        jf.add(jtb, BorderLayout.SOUTH);
        jf.setSize(1024, 600);
        jf.setVisible(true);

        update();
    }

    public void parameterChanged(ParameterGUI pg, String name)
    {
        update();
    }

    public static void main(String args[])
    {
        if (args.length != 1) {
            System.out.println("Specify a log file as an argument.");
            return;
        }

        try {
            new FeatureTest(args);
        } catch (IOException ex) {
            System.out.println("ex: " + ex);
        }
    }

    ArrayList<double[]> laserToPoints(laser_t laser)
    {
        ArrayList<double[]> points = new ArrayList<double[]>();
        for (int i = 0; i < laser.nranges; i++)
        {
            double theta = laser.rad0 + laser.radstep * i;
            points.add(new double[] { laser.ranges[i] * Math.cos(theta),
                                      laser.ranges[i] * Math.sin(theta) });
        }
        return points;
    }

    public void update()
    {
        laser_t lasera = lasers.get(pg.gi("apos")), laserb = lasers.get(pg.gi("bpos"));
        pose_t posea = poses.get(pg.gi("apos")), poseb = poses.get(pg.gi("bpos"));

        ArrayList<double[]> pointsa = laserToPoints(lasera);
        ArrayList<double[]> pointsb = laserToPoints(laserb);

        if (true) {
            // left-most panel: draw the odometry path
            VisWorld.Buffer vb = vwm.getBuffer("map");
            ArrayList<double[]> points = new ArrayList<double[]>();
            for (pose_t p : poses)
                points.add(p.pos);
            vb.addBuffered(new VisData(new VisDataPointStyle(Color.gray, 1), points));
            vb.addBuffered(new VisChain(posea.orientation, posea.pos, new VisRobot(Color.blue)));
            vb.addBuffered(new VisChain(poseb.orientation, poseb.pos, new VisRobot(Color.red)));
            vb.switchBuffer();
        }

        for (FeatureTestComponent comp : components)
            comp.update();
    }

    abstract class FeatureTestComponent
    {
        public abstract String getName();
        public abstract Component getPanel();
        public abstract void update();

        public boolean isEnabled()
        {
            return FeatureTest.this.pg.gb("show:"+getName());
        }
    }

    //////////////////////////////////////////////////////////////////
    class PointComponent extends FeatureTestComponent implements ParameterListener
    {
        ParameterGUI mypg = new ParameterGUI();

        public PointComponent()
        {
            mypg.addListener(this);
        }

        public void parameterChanged(ParameterGUI mypg, String name)
        {
            update();
        }

        public String getName()
        {
            return "Points";
        }

        public Component getPanel()
        {
            return mypg;
        }

        public void update()
        {
            laser_t lasera = lasers.get(pg.gi("apos")), laserb = lasers.get(pg.gi("bpos"));
            pose_t posea = poses.get(pg.gi("apos")), poseb = poses.get(pg.gi("bpos"));

            ArrayList<double[]> pointsa = laserToPoints(lasera);
            ArrayList<double[]> pointsb = laserToPoints(laserb);

            if (true) {
                // draw middle panel (laser scan a)
                VisWorld.Buffer vb = vwa.getBuffer("points");
                if (isEnabled())
                    vb.addBuffered(new VisData(new VisDataPointStyle(Color.blue, 2), pointsa));
                vb.switchBuffer();
            }

            if (true) {
                // draw right panel (laser scan b)
                VisWorld.Buffer vb = vwb.getBuffer("points");
                if (isEnabled())
                    vb.addBuffered(new VisData(new VisDataPointStyle(Color.blue, 2), pointsb));
                vb.switchBuffer();
            }
        }
    }

    //////////////////////////////////////////////////////////////////
    class ContourComponent extends FeatureTestComponent implements ParameterListener
    {
        ContourExtractor contourExtractor = new ContourExtractor();
        ParameterGUI mypg = new ParameterGUI();

        public ContourComponent()
        {
            mypg.addDoubleSlider("adjacentAcceptDistance", "Adjacent accept distance", 0, 1, contourExtractor.adjacentAcceptDistance);
            mypg.addDoubleSlider("maxDistance", "Maximum distance", 0, 10, contourExtractor.maxDistance);
            mypg.addIntSlider("minPointsPerContour", "Minimum points per contour", 0, 10, contourExtractor.minPointsPerContour);
            mypg.addIntSlider("maxSkipPoints", "Maximum skip points", 0, 10, contourExtractor.maxSkipPoints);
            mypg.addDoubleSlider("maxDistanceRatio", "Maximum distance ratio", 0, 3, contourExtractor.maxDistanceRatio);
            mypg.addDoubleSlider("alwaysAcceptDistance", "Always Accept Distance", 0, 1, contourExtractor.alwaysAcceptDistance);
            mypg.addListener(this);
        }

        public void parameterChanged(ParameterGUI mypg, String name)
        {
            update();
        }

        public String getName()
        {
            return "Contours";
        }

        public Component getPanel()
        {
            return mypg;
        }

        public void update()
        {
            laser_t lasera = lasers.get(pg.gi("apos")), laserb = lasers.get(pg.gi("bpos"));
            pose_t posea = poses.get(pg.gi("apos")), poseb = poses.get(pg.gi("bpos"));

            ArrayList<double[]> pointsa = laserToPoints(lasera);
            ArrayList<double[]> pointsb = laserToPoints(laserb);

            contourExtractor.maxSkipPoints = mypg.gi("maxSkipPoints");
            contourExtractor.minPointsPerContour = mypg.gi("minPointsPerContour");
            contourExtractor.maxDistance = mypg.gd("maxDistance");
            contourExtractor.adjacentAcceptDistance = mypg.gd("adjacentAcceptDistance");
            contourExtractor.maxDistanceRatio = mypg.gd("maxDistanceRatio");
            contourExtractor.alwaysAcceptDistance = mypg.gd("alwaysAcceptDistance");

            if (true) {
                // draw middle panel (laser scan a)
                VisWorld.Buffer vb = vwa.getBuffer("contours");

                if (isEnabled()) {
                    ArrayList<ArrayList<double[]>> contours = contourExtractor.getContours(pointsa);
                    for (ArrayList<double[]> contour : contours) {
                        vb.addBuffered(new VisData(new VisDataLineStyle(ColorUtil.randomColor(), 2), contour));
                    }
                }

                vb.switchBuffer();
            }

            if (true) {
                // draw right panel (laser scan b)
                VisWorld.Buffer vb = vwb.getBuffer("contours");
                if (isEnabled()) {
                    ArrayList<ArrayList<double[]>> contours = contourExtractor.getContours(pointsb);
                    for (ArrayList<double[]> contour : contours) {
                        vb.addBuffered(new VisData(new VisDataLineStyle(ColorUtil.randomColor(), 2), contour));
                    }
                }
                vb.switchBuffer();
            }
        }
    }

    //////////////////////////////////////////////////////////////////
    class LineComponent extends FeatureTestComponent implements ParameterListener
    {
        LineFitter lineFitter = new LineFitter(null);
        ParameterGUI mypg = new ParameterGUI();

        public LineComponent()
        {
            mypg.addDoubleSlider("ticksz", "Tick size (m)", 0, 1, 0.2);

            mypg.addListener(this);
        }

        public void parameterChanged(ParameterGUI mypg, String name)
        {
            update();
        }

        public String getName()
        {
            return "Lines";
        }

        public Component getPanel()
        {
            return mypg;
        }

        public void update()
        {
            laser_t lasera = lasers.get(pg.gi("apos")), laserb = lasers.get(pg.gi("bpos"));
            pose_t posea = poses.get(pg.gi("apos")), poseb = poses.get(pg.gi("bpos"));

            ArrayList<double[]> pointsa = laserToPoints(lasera);
            ArrayList<double[]> pointsb = laserToPoints(laserb);

            ArrayList<LineFeature> linesa = lineFitter.getLineFeatures(pointsa);
            ArrayList<LineFeature> linesb = lineFitter.getLineFeatures(pointsb);

            double sz = mypg.gd("ticksz");

            if (true) {
                // draw middle panel (laser scan a)
                VisWorld.Buffer vb = vwa.getBuffer("contours");

                if (isEnabled()) {
                    for (LineFeature lf : linesa) {
                        Color color = ColorUtil.randomColor();

                        vb.addBuffered(new VisData(lf.seg.p1, lf.seg.p2, new VisDataLineStyle(color, 2)));
                        double cx[] = new double[] {(lf.seg.p1[0] + lf.seg.p2[0]) / 2,
                                                    (lf.seg.p1[1] + lf.seg.p2[1]) / 2 };
                        vb.addBuffered(new VisData(cx, LinAlg.add(cx, new double[] { sz*Math.cos(lf.normal),
                                                                                     sz*Math.sin(lf.normal)}),
                                new VisDataLineStyle(color, 1)));
                    }
                }

                vb.switchBuffer();
            }

            if (true) {
                // draw right panel (laser scan b)
                VisWorld.Buffer vb = vwb.getBuffer("contours");

                if (isEnabled()) {
                    for (LineFeature lf : linesb) {
                        Color color = ColorUtil.randomColor();

                        vb.addBuffered(new VisData(lf.seg.p1, lf.seg.p2, new VisDataLineStyle(color, 2)));
                        double cx[] = new double[] {(lf.seg.p1[0] + lf.seg.p2[0]) / 2,
                                                    (lf.seg.p1[1] + lf.seg.p2[1]) / 2 };
                        vb.addBuffered(new VisData(cx, LinAlg.add(cx, new double[] { sz*Math.cos(lf.normal),
                                                                                     sz*Math.sin(lf.normal)}),
                                new VisDataLineStyle(color, 1)));
                    }
                }

                vb.switchBuffer();
            }
        }
    }
}
