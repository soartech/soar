package april.vis;

import java.awt.*;
import java.awt.geom.*;
import java.awt.event.*;
import java.awt.image.*;
import java.io.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.imageio.*;
import java.util.*;

import april.jmat.geom.*;

/** Test class for Vis (incomplete). **/
public class VisTextTest
{
    JFrame      frame = new JFrame("VisTextTest");

    VisWorld    vw = new VisWorld();
    VisCanvas   vc = new VisCanvas(vw);

    public static void main(String[] args)
    {
        new VisTextTest().run(args);
    }

    public void run(String[] args)
    {
        frame.setLayout(new BorderLayout());
        frame.add(vc, BorderLayout.CENTER);
        frame.setSize(600,400);
        frame.setVisible(true);

        VisData vd = new VisData(new VisDataPointStyle(Color.blue, 4.0),
                                 new VisDataLineStyle(Color.yellow, 1.0));

        for (double x = 0; x < 10; x += 0.01) {
            double p[] = new double[] { x, Math.sin(x) };
            vd.add(p);
        }

        VisWorld.Buffer vb = vw.getBuffer("stuff");

        vb.addBuffered(vd);

        if (true) {
            vb.addBuffered(new VisText(VisText.ANCHOR.TOP_LEFT, "<<red,big,width=80>>1Line<<blue,mono-normal>>A<<black,mono-normal>> TOP_LEFT\nfoog"));
            vb.addBuffered(new VisText(VisText.ANCHOR.TOP, "1 TOP\nfoog"));
            vb.addBuffered(new VisText(VisText.ANCHOR.TOP_RIGHT, "1 TOP_RIGHT\nfoog"));

            vb.addBuffered(new VisText(VisText.ANCHOR.LEFT, "1 LEFT\nfoog"));
            vb.addBuffered(new VisText(VisText.ANCHOR.CENTER, "1 CENTER\nfoo\nbar\nbag"));
            vb.addBuffered(new VisText(VisText.ANCHOR.RIGHT, "1 RIGHT\nfoog"));

            vb.addBuffered(new VisText(VisText.ANCHOR.BOTTOM_LEFT, "1 BOTTOM_LEFT\nfoog"));
            vb.addBuffered(new VisText(VisText.ANCHOR.BOTTOM, "1 BOTTOM\nfoog"));
            vb.addBuffered(new VisText(VisText.ANCHOR.BOTTOM_RIGHT, "1 BOTTOM_RIGHT\n<<left>>left\n<<center>>center\n<<right>>right"));

            vb.addBuffered(new VisText(new double[3], VisText.ANCHOR.CENTER, "C1\nC2"));
            vb.addBuffered(new VisText(new double[3], VisText.ANCHOR.LEFT, "L1\nL2"));
            vb.addBuffered(new VisText(new double[3], VisText.ANCHOR.TOP_LEFT, "TL1\nTL2"));
        }

        vb.switchBuffer();
    }
}
