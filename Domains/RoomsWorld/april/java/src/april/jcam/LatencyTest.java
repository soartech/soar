package april.jcam;

import april.util.*;

import java.awt.*;
import java.awt.image.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;

import java.io.*;
import java.util.*;

/** Measure latency of camera by flashing the screen between black and
 * white. The constants in this program generally need to be adjusted
 * to handle new cameras. **/
public class LatencyTest
{
    JFrame jf;
    JPanel jp;
    JImage jim;
    JLabel sampleLabel;

    public LatencyTest()
    {
        jf = new JFrame("Latency Test");
        jf.setLayout(new BorderLayout());

        jp = new JPanel();
        jim = new JImage();

        JSplitPane jsp = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, jp, jim);
        jsp.setResizeWeight(.5);

        sampleLabel = new JLabel("");

        jf.add(jsp, BorderLayout.CENTER);
        jf.add(sampleLabel, BorderLayout.SOUTH);

        jf.setSize(1000,800);
        jf.setVisible(true);
    }

    public void setColor(Color c)
    {
        jp.setBackground(c);
        jp.repaint();
        Thread.yield();
    }

    public static void main(String args[])
    {
        try {
            mainEx(args);
        } catch (IOException ex) {
            System.out.println("ex: "+ex);
        }
    }

    public static void mainEx(String args[]) throws IOException
    {
        String url = null;

        if (args.length > 0)
            url = args[0];

        if (url == null) {
            // use the first camera in the system
            // Get a list of all cameras
            ArrayList<String> urls = ImageSource.getCameraURLs();
            if (urls.size() == 0) {
                System.out.println("Did not find any cameras. Check device permissions (e.g., /dev/video*)");
                return;
            }
            url = urls.get(0);
        }

        ImageSource isrc = ImageSource.make(url);

        for (int i = 0; i < isrc.getNumFormats(); i++) {
            ImageSourceFormat ifmt = isrc.getFormat(i);
            System.out.printf("%3d: %10d x %-10d %s\n", i, ifmt.width, ifmt.height, ifmt.format);
        }

        isrc.start();

        LatencyTest lt = new LatencyTest();

        long starttime;

        while(true) {

            int thresh = 200;

            double dt0 = lt.test(isrc, Color.black, 0, thresh);
            double dt1 = lt.test(isrc, Color.white, thresh, 256);

            System.out.printf("%f %f\n", dt0, dt1);
        }

    }

    long lastmtime = 0;
    int nframes;

    void gotFrame()
    {
        long mtime = System.currentTimeMillis();

        nframes++;
        double dt = (mtime - lastmtime) / 1000.0;
        lastmtime = mtime;

        if (nframes%100 == 0)
            System.out.printf("Hz: %5.3f\n", 1.0/dt);

    }

    double test(ImageSource isrc, Color c, int min, int max)
    {
        long starttime = System.currentTimeMillis();
        setColor(c);

        while (true) {
            byte imbuf[] = isrc.getFrame();
            if (imbuf==null) {
                System.out.println("get_frame failed");
                return 0;
            }
            gotFrame();

            /*
              while (true) {
              byte imbuf2[] = isrc.getFrame(0);
              if (imbuf2 == null)
              break;
              gotFrame();
              imbuf = imbuf2;
              }
            */

            long nowtime = System.currentTimeMillis();

            ImageSourceFormat ifmt = isrc.getCurrentFormat();
            BufferedImage im = ImageConvert.convertToImage(ifmt.format, ifmt.width, ifmt.height, imbuf);
            jim.setImage(im);

            int rgb = im.getRGB(ifmt.width/2, ifmt.height/2);

            int g = (rgb>>8)&0xff;

            sampleLabel.setText(String.format("%d", g));

            if (g>=min && g<=max)
                return (nowtime - starttime)/1000.0;
        }
    }
}
