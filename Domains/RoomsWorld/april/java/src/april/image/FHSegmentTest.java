package april.image;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.io.*;
import java.util.*;
import javax.swing.*;

import april.util.*;
import april.image.*;

import april.jmat.*;
import april.jmat.geom.*;

import april.vis.*;
import april.jcam.*;

import javax.imageio.*;

public class FHSegmentTest implements ParameterListener
{
    JFrame jf;
    VisWorld  vw = new VisWorld();
    VisCanvas vc = new VisCanvas(vw);

    ImageSource is;

    ParameterGUI pg;

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
                System.out.println("Please specify a camera:");
                for (String u : urls)
                    System.out.println(" "+u);
                return;
            }

            ImageSource is = ImageSource.make(url);

            is.setFormat("BAYER_RGGB");

            new FHSegmentTest(is);

        } catch (IOException ex) {
            System.out.println("Ex: "+ex);
        }
    }

    public FHSegmentTest(ImageSource is)
    {
        this.is = is;

        pg = new ParameterGUI();
        pg.addDoubleSlider("sigma", "smoothing sigma", 0, 2, 0.8);
        pg.addIntSlider("k", "k", 0, 10000, 150);
        pg.addIntSlider("minsize", "minsize", 0, 500, 50);

        jf = new JFrame("FHSegmentTest");
        jf.setLayout(new BorderLayout());
        jf.add(vc, BorderLayout.CENTER);
        jf.add(pg, BorderLayout.SOUTH);

        jf.setSize(800,600);
        jf.setVisible(true);

        vc.getViewManager().viewGoal.fit2D(new double[] {0,0}, new double[] { 752, 480});
        new RunThread().start();

        pg.addListener(this);
    }

    public void parameterChanged(ParameterGUI pg, String name)
    {
    }

    BufferedImage blurImage(BufferedImage in, double sigma)
    {
        float filt[] = SigProc.makeGaussianFilter(sigma, 5);

        FloatImage rim = new FloatImage(in, 16);
        FloatImage gim = new FloatImage(in, 8);
        FloatImage bim = new FloatImage(in, 0);

        rim = rim.filterFactoredCentered(filt, filt);
        gim = gim.filterFactoredCentered(filt, filt);
        bim = bim.filterFactoredCentered(filt, filt);

        return FloatImage.makeImage(rim, gim, bim);
    }

    class RunThread extends Thread
    {
        public void handleImage(BufferedImage im)
        {
            BufferedImage blurim = blurImage(im, pg.gd("sigma"));

            VisWorld.Buffer vb = vw.getBuffer("image");
            vb.addBuffered(new VisImage(blurim));
            vb.switchBuffer();

            vb = vw.getBuffer("segment");
            long startmtime = System.currentTimeMillis();
            BufferedImage imseg = FHSegment.segment(blurim, pg.gi("k"), pg.gi("minsize"));
            long endmtime = System.currentTimeMillis();
            System.out.println(endmtime - startmtime);
            vb.addBuffered(new VisImage(imseg));
            vb.switchBuffer();
        }

        public void run()
        {
            is.start();
            ImageSourceFormat fmt = is.getCurrentFormat();

            while (true) {
                byte buf[] = is.getFrame();
                if (buf == null)
                    continue;

                BufferedImage im = ImageConvert.convertToImage(fmt.format, fmt.width, fmt.height, buf);
                handleImage(im);
            }
        }
    }
}
