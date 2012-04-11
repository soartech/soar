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
public class VisTest
{
    JFrame      frame = new JFrame("VisTest");

    VisWorld    world = new VisWorld();
    VisCanvas  canvas = new VisCanvas(world);

    public static void main(String[] args)
    {
        new VisTest().run(args);
    }

    public void run(String[] args)
    {
        frame.setLayout(new BorderLayout());
        frame.add(canvas, BorderLayout.CENTER);
        frame.setSize(600,400);
        frame.setVisible(true);

        world.getBuffer("grid").addFront(new VisGrid());

        VisData vd = new VisData(new VisDataPointStyle(Color.blue, 4.0),
                                 new VisDataLineStyle(Color.yellow, 1.0));

        for (double x = 0; x < 10; x += 0.01) {
            double p[] = new double[] { x, Math.sin(x) };
            vd.add(p);
        }

        VisWorld.Buffer vb = world.getBuffer("stuff");

        vb.addBuffered(vd);

        if (true) {
            vb.addBuffered(new VisText(VisText.ANCHOR.TOP_LEFT, "1 TOP_LEFT\nfoo"));
            vb.addBuffered(new VisText(VisText.ANCHOR.TOP, "1 TOP\nfoo"));
            vb.addBuffered(new VisText(VisText.ANCHOR.TOP_RIGHT, "1 TOP_RIGHT\nfoo"));

            vb.addBuffered(new VisText(VisText.ANCHOR.LEFT, "1 LEFT\nfoo"));
            vb.addBuffered(new VisText(VisText.ANCHOR.CENTER, "1 CENTER\nfoo\nbar\nbaz"));
            vb.addBuffered(new VisText(VisText.ANCHOR.RIGHT, "1 RIGHT\nfoo"));

            vb.addBuffered(new VisText(VisText.ANCHOR.BOTTOM_LEFT, "1 BOTTOM_LEFT\nfoo"));
            vb.addBuffered(new VisText(VisText.ANCHOR.BOTTOM, "1 BOTTOM\nfoo"));
            vb.addBuffered(new VisText(VisText.ANCHOR.BOTTOM_RIGHT, "1 BOTTOM_RIGHT\nfoo"));
        }

        if (false) {
            vb.addBuffered(new VisText(VisText.ANCHOR.TOP_LEFT, "1 TOP_LEFT"));
            vb.addBuffered(new VisText(VisText.ANCHOR.TOP, "1 TOP"));
            vb.addBuffered(new VisText(VisText.ANCHOR.TOP_RIGHT, "1 TOP_RIGHT"));

            vb.addBuffered(new VisText(VisText.ANCHOR.LEFT, "1 LEFT"));
            vb.addBuffered(new VisText(VisText.ANCHOR.CENTER, "1 CENTER"));
            vb.addBuffered(new VisText(VisText.ANCHOR.RIGHT, "1 RIGHT"));

            vb.addBuffered(new VisText(VisText.ANCHOR.BOTTOM_LEFT, "1 BOTTOM_LEFT"));
            vb.addBuffered(new VisText(VisText.ANCHOR.BOTTOM, "1 BOTTOM"));
            vb.addBuffered(new VisText(VisText.ANCHOR.BOTTOM_RIGHT, "1 BOTTOM_RIGHT"));
        }

        vb.switchBuffer();
    }
}
