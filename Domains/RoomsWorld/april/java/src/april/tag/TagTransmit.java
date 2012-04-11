package april.tag;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.io.*;
import java.util.*;
import javax.swing.*;

import april.jmat.*;
import april.jmat.geom.*;

import april.vis.*;
import april.jcam.*;

import april.util.*;

import lcm.lcm.*;
import april.lcmtypes.*;

public class TagTransmit implements ParameterListener
{
    JFrame jf;
    VisWorld  vw = new VisWorld();
    VisCanvas vc = new VisCanvas(vw);

    ImageSource is;

    ParameterGUI pg;

    TagFamily tagFamily;
    TagDetector detector;

    LCM lcm = LCM.getSingleton();

    public static void main(String args[])
    {
        try {
            ArrayList<String> urls = ImageSource.getCameraURLs();

            String url = null;
            if (urls.size()==1)
                url = urls.get(0);

            if (args.length > 0)
                url = args[0];

            if (url == null) {
                System.out.printf("Cameras found:\n");
                for (String u : urls)
                    System.out.printf("  %s\n", u);
                System.out.printf("Please specify one on the command line.\n");
                return;
            }

            ImageSource is = ImageSource.make(url);

            TagFamily tf = new Tag36h11();
            if (args.length >= 2) {
                tf = (TagFamily) ReflectUtil.createObject(args[1]);
            }

            TagTransmit tt = new TagTransmit(is, tf);

        } catch (IOException ex) {
            System.out.println("Ex: "+ex);
        }
    }

    public TagTransmit(ImageSource is, TagFamily tf)
    {
        this.is = is;
        this.tagFamily = tf;

        detector = new TagDetector(this.tagFamily);

        pg = new ParameterGUI();

        pg.addDoubleSlider("segsigma", "smoothing sigma (segmentation)", 0, 2, detector.segSigma);
        pg.addDoubleSlider("sigma", "smoothing sigma (sampling)", 0, 2, detector.sigma);
        pg.addDoubleSlider("minmag", "minimum magnitude", 0.0001, 0.01, detector.minMag);                   //
        pg.addDoubleSlider("maxedgecost", "maximum edge cost (radians)", 0, Math.PI, detector.maxEdgeCost); //
        pg.addDoubleSlider("magthresh", "magnitude threshold", 0, 5000, detector.magThresh);
        pg.addDoubleSlider("thetathresh", "theta threshold", 0, 5000, detector.thetaThresh);
        pg.addIntSlider("errorbits", "error recovery (bits)", 0, 5, 1);
        pg.addIntSlider("weightscale", "Weight scale", 1, 100, detector.WEIGHT_SCALE);                      //

        pg.addCheckBoxes("segDecimate", "segmentation decimate", detector.segDecimate,
                         "debug", "debug", false);

        jf = new JFrame("TagTransmit");
        jf.setLayout(new BorderLayout());
        jf.add(vc, BorderLayout.CENTER);
        jf.add(pg, BorderLayout.SOUTH);

        jf.setSize(800,600);
        jf.setVisible(true);

        vc.getViewManager().viewGoal.fit2D(new double[] {0,0}, new double[] { 752, 480});
        pg.addListener(this);

        new RunThread().start();
    }

    public void parameterChanged(ParameterGUI pg, String name)
    {
    }

    class RunThread extends Thread
    {
        public void run()
        {
            is.start();
            ImageSourceFormat fmt = is.getCurrentFormat();

            detector = new TagDetector(tagFamily);

            VisWorld.Buffer vbOriginal      = vw.getBuffer("unprocessed image");
            VisWorld.Buffer vbSegmentation  = vw.getBuffer("segmentation");
            VisWorld.Buffer vbInput         = vw.getBuffer("input");
            VisWorld.Buffer vbDetections    = vw.getBuffer("detections");
            VisWorld.Buffer vbClock         = vw.getBuffer("clock");

            detector.debugSegments  = vw.getBuffer("segments");
            detector.debugQuads     = vw.getBuffer("quads");
            detector.debugSamples   = vw.getBuffer("samples");
            detector.debugLabels    = vw.getBuffer("labels");

            while (true) {
                byte buf[] = is.getFrame();
                if (buf == null)
                    continue;

                BufferedImage im = ImageConvert.convertToImage(fmt.format, fmt.width, fmt.height, buf);

                tagFamily.setErrorRecoveryBits(pg.gi("errorbits"));

                detector.debug = pg.gb("debug");
                detector.sigma = pg.gd("sigma");
                detector.segSigma = pg.gd("segsigma");
                detector.segDecimate = pg.gb("segDecimate");
                detector.minMag = pg.gd("minmag");
                detector.maxEdgeCost = pg.gd("maxedgecost");
                detector.magThresh = pg.gd("magthresh");
                detector.thetaThresh = pg.gd("thetathresh");
                detector.WEIGHT_SCALE = pg.gi("weightscale");

                Tic tic = new Tic();
                ArrayList<TagDetection> detections = detector.process(im, new double[] {im.getWidth()/2.0, im.getHeight()/2.0});
                double dt = tic.toc();

                if (detector.debugInput!=null)
                    vbInput.addBuffered(new VisDepthTest(false, new VisLighting(false, new VisImage(detector.debugInput))));
                vbInput.switchBuffer();

                if (detector.debugSegmentation!=null)
                    vbSegmentation.addBuffered(new VisLighting(false, new VisImage(detector.debugSegmentation)));
                vbSegmentation.switchBuffer();

                vbOriginal.addBuffered(new VisDepthTest(false, new VisLighting(false, new VisImage(im))));
                vbOriginal.switchBuffer();

                vbClock.addBuffered(new VisText(VisText.ANCHOR.BOTTOM_RIGHT,
                                                VisText.JUSTIFICATION.RIGHT,
                                                String.format("<<blue>>%8.2f ms", dt*1000)));
                vbClock.switchBuffer();

                for (TagDetection d : detections) {
                    double p0[] = d.interpolate(-1,-1);
                    double p1[] = d.interpolate(1,-1);
                    double p2[] = d.interpolate(1,1);
                    double p3[] = d.interpolate(-1,1);

                    vbDetections.addBuffered(new VisChain(LinAlg.translate(0, im.getHeight(), 0),
                                                          LinAlg.scale(1, -1, 1),
                                                          new VisText(d.cxy, VisText.ANCHOR.CENTER,
                                                                      String.format("<<center,blue>>id %3d\n(err=%d, rot=%d)\n", d.id, d.hammingDistance, d.rotation)),
                                                          new VisData(new VisDataLineStyle(Color.blue, 4), p0, p1, p2, p3, p0),
                                                          new VisData(new VisDataLineStyle(Color.green, 4), p0, p1), // x axis
                                                          new VisData(new VisDataLineStyle(Color.red, 4), p0, p3))); // y axis

                    System.out.printf("id %3d err %3d\n", d.id, d.hammingDistance);
                }
                vbDetections.switchBuffer();

                tag_detection_list_t dlist = new tag_detection_list_t();
                dlist.utime = System.nanoTime()/1000;
                dlist.width = im.getWidth();
                dlist.height = im.getHeight();
                dlist.ndetections = detections.size();
                dlist.detections = new tag_detection_t[dlist.ndetections];
                for (int i = 0; i < detections.size(); i++) {
                    TagDetection d = detections.get(i);
                    tag_detection_t td = new tag_detection_t();
                    dlist.detections[i] = td;

                    td.id = d.id;
                    td.errors = d.hammingDistance;
                    td.homography = d.homography;
                    td.hxy = d.hxy;
                }

                lcm.publish("TAG_DETECTIONS", dlist);
            }
        }
    }
}
