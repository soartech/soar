package april.util;

// most of these dependencies are needed only for the test code.
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.io.*;
import java.util.*;
import javax.swing.*;
import april.vis.*;
import april.jmat.geom.*;
import april.jmat.*;
import april.image.*;
import javax.imageio.*;
import java.util.*;

public class Wavefront
{
    GridMap gm;

    IntMaxHeap heap;

    double costs[];

    ArrayList<double[]> path;

    // quit as soon as we've found a path--- don't compute the whole cost map.
    public boolean earlyExit = true;

    public Wavefront(GridMap gm)
    {
        this.gm = gm;
    }

    // using 8-connectivity, find the point closest to a point in sinks that is accessible from source.
    public double[] findClosestAccessiblePoint(double source[], ArrayList<double[]> sinks)
    {
        UnionFindSimple ufs = new UnionFindSimple(gm.data.length);

        // connect together all adjacent non-obstacle pixels.
        for (int iy = 1; iy + 1 < gm.height; iy++) {
            for (int ix = 1; ix + 1 < gm.width; ix++) {

                int v0 = gm.data[iy*gm.width + ix] & 0xff;
                if (v0 == 255)
                    continue;

                for (int dy = -1; dy <= 1; dy++) {
                    int y = iy+dy;

                    for (int dx = -1; dx <= 1; dx++) {
                        int x = ix+dx;

                        int v1 = gm.data[y*gm.width + x] & 0xff;
                        if (v1 == 255)
                            continue;

                        ufs.connectNodes(iy*gm.width + ix, y*gm.width + x);
                    }
                }
            }
        }

        // what connected region is the source in?
        int sx = (int) ((source[0] - gm.x0) / gm.metersPerPixel);
        int sy = (int) ((source[1] - gm.y0) / gm.metersPerPixel);
        sx = LinAlg.clamp(sx, 0, gm.width - 1);
        sy = LinAlg.clamp(sy, 0, gm.height - 1);

        int rep = ufs.getRepresentative(sy*gm.width + sx);

        double bestdist = Double.MAX_VALUE;
        double besty = -1, bestx = -1;

        for (int iy = 0; iy < gm.height; iy++) {
            for (int ix = 0; ix < gm.width; ix++) {
                if (ufs.getRepresentative(iy*gm.width + ix) == rep) {

                    for (double sink[] : sinks) {
                        double dist = LinAlg.sq(sink[0] - (gm.x0 + (ix+.5)*gm.metersPerPixel)) +
                            LinAlg.sq(sink[1] - (gm.y0 + (iy+.5)*gm.metersPerPixel));

                        if (dist < bestdist) {
                            bestx = gm.x0 + (ix+.5)*gm.metersPerPixel;
                            besty = gm.y0 + (iy+.5)*gm.metersPerPixel;
                            bestdist = dist;
                        }
                    }
                }
            }
        }

        if (bestdist == Double.MAX_VALUE)
            return null;

        return new double[] { bestx, besty };
    }

    public ArrayList<double[]> compute(double source[], ArrayList<double[]> sinks)
    {
        costs = new double[gm.data.length];
        heap = new IntMaxHeap();

        for (int i = 0; i < costs.length; i++)
            costs[i] = Double.MAX_VALUE;

        // set all sinks to cost of zero and add them to the heap
        for (double xy[] : sinks) {
            int ix = (int) ((xy[0] - gm.x0) / gm.metersPerPixel);
            int iy = (int) ((xy[1] - gm.y0) / gm.metersPerPixel);

            if (ix >=0 && iy >= 0 && ix < gm.width && iy < gm.height) {
                heap.add((ix << 16) | (iy), 0);
                costs[iy*gm.width + ix] = 0;
            }
        }

        int goal_ix = (int) ((source[0] - gm.x0) / gm.metersPerPixel);
        int goal_iy = (int) ((source[1] - gm.y0) / gm.metersPerPixel);

        // propagate wavefront until we're done.
        while (heap.size() > 0) {
            IntHeapPair pair = heap.removeMaxPair();

            double cost = -pair.score; // pretend we have a minheap
            int ix = (pair.o >>> 16);
            int iy = pair.o & 0xffff;

            if (earlyExit && ix == goal_ix && iy == goal_iy)
                break;

//            System.out.printf("%3d %3d %15f\n", ix, iy, cost);

            // pre-fetch cells around us. Our current position is c55, so left is c45, up is c54.
            int c44 = gm.getValueIndexSafe(ix - 1, iy - 1, 255);
            int c54 = gm.getValueIndexSafe(ix + 0, iy - 1, 255);
            int c64 = gm.getValueIndexSafe(ix + 1, iy - 1, 255);
            int c45 = gm.getValueIndexSafe(ix - 1, iy, 255);
            int c55 = gm.getValueIndexSafe(ix + 0, iy, 255);
            int c65 = gm.getValueIndexSafe(ix + 1, iy, 255);
            int c46 = gm.getValueIndexSafe(ix - 1, iy + 1, 255);
            int c56 = gm.getValueIndexSafe(ix + 0, iy + 1, 255);
            int c66 = gm.getValueIndexSafe(ix + 1, iy + 1, 255);

            // four cardinal directions
            tryExpand(ix - 1, iy, cost + .5 * (c55 + c45), Math.max(c55, c45));
            tryExpand(ix + 1, iy, cost + .5 * (c55 + c65), Math.max(c55, c65));
            tryExpand(ix, iy - 1, cost + .5 * (c55 + c54), Math.max(c55, c54));
            tryExpand(ix, iy + 1, cost + .5 * (c55 + c56), Math.max(c55, c56));

            // four diagonal directions (for 8 connectivity)
            tryExpand(ix - 1, iy - 1, cost + .7071 * (c55 + c44), Math.max(c55, c44));
            tryExpand(ix + 1, iy - 1, cost + .7071 * (c55 + c64), Math.max(c55, c64));
            tryExpand(ix - 1, iy + 1, cost + .7071 * (c55 + c46), Math.max(c55, c46));
            tryExpand(ix + 1, iy + 1, cost + .7071 * (c55 + c66), Math.max(c55, c66));

            if (true) {
                // 30 and 60 degree moves.
                int c43 = gm.getValueIndexSafe(ix - 1, iy - 2, 255);
                int c63 = gm.getValueIndexSafe(ix + 1, iy - 2, 255);
                int c34 = gm.getValueIndexSafe(ix - 2, iy - 1, 255);
                int c74 = gm.getValueIndexSafe(ix + 2, iy - 1, 255);
                int c36 = gm.getValueIndexSafe(ix - 2, iy + 1, 255);
                int c76 = gm.getValueIndexSafe(ix + 2, iy + 1, 255);
                int c47 = gm.getValueIndexSafe(ix - 1, iy + 2, 255);
                int c67 = gm.getValueIndexSafe(ix + 1, iy + 2, 255);

                tryExpand(ix + 2, iy + 1, cost + .55901 * (c55 + c65 + c66 + c76), max4(c55, c65, c66, c76));
                tryExpand(ix + 1, iy + 2, cost + .55901 * (c55 + c56 + c66 + c67), max4(c55, c56, c66, c67));
                tryExpand(ix + 2, iy - 1, cost + .55901 * (c55 + c65 + c64 + c74), max4(c55, c65, c64, c74));
                tryExpand(ix + 1, iy - 2, cost + .55901 * (c55 + c54 + c64 + c63), max4(c55, c54, c64, c63));
                tryExpand(ix - 1, iy - 2, cost + .55901 * (c55 + c54 + c44 + c43), max4(c55, c54, c44, c43));
                tryExpand(ix - 2, iy - 1, cost + .55901 * (c55 + c45 + c44 + c34), max4(c55, c45, c44, c34));
                tryExpand(ix - 1, iy + 2, cost + .55901 * (c55 + c56 + c46 + c47), max4(c55, c56, c46, c47));
                tryExpand(ix - 2, iy + 1, cost + .55901 * (c55 + c45 + c46 + c36), max4(c55, c45, c46, c36));
            }
        }

        ////////////////////////////////////////////////////////////////////////////////
        // Compute the best path

        ArrayList<double[]> path = new ArrayList<double[]>();
        int ix = (int) ((source[0] - gm.x0) / gm.metersPerPixel);
        int iy = (int) ((source[1] - gm.y0) / gm.metersPerPixel);

        while (true) {
            if (ix < 0 || ix >= gm.width || iy < 0 || iy >= gm.height)
                return path;

            double v = costs[iy * gm.width + ix];

            path.add(new double[] { gm.x0 + (ix + .5) * gm.metersPerPixel,
                                    gm.y0 + (iy + .5) * gm.metersPerPixel });

            // next coordinates
            int nix = -1;
            int niy = -1;
            double bestgradient = 0;
            int range = 2;

            for (int dix = -range; dix <= range; dix++) {
                for (int diy = -range; diy <= range; diy++) {

                    if (dix == 0 && diy == 0)
                        continue;

                    if ((ix + dix) >= 0 && (ix + dix) < gm.width && (iy + diy) >= 0 && (iy + diy) < gm.height) {

                        double thisv = costs[(iy + diy) * gm.width + (ix + dix)];
                        double gradient = (v - thisv) / Math.sqrt(dix * dix + diy * diy);

                        if (gradient > bestgradient) {
                            bestgradient = gradient;
                            nix = ix + dix;
                            niy = iy + diy;
                        }
                    }
                }
            }

            ix = nix;
            iy = niy;

            // if we found a goal, we're good!
            if (v == 0)
                break;
        }

        this.path = path;

        return path;
    }

    static final int max4(int a, int b, int c, int d)
    {
        return Math.max(Math.max(a, b), Math.max(c, d));
    }

    void tryExpand(int ix, int iy, double cost, int max)
    {
        // beyond grid?
        if (ix < 0 || ix >= gm.width || iy < 0 || iy >= gm.height)
            return;

        // hit an obstacle?
        if (max == 255)
            return;

        // already have a cheaper path to this node?
        if (cost >= costs[iy * gm.width + ix])
            return;

        // update costs and heap. (pretend we have a minheap)
        heap.add((ix << 16) | (iy), -cost);
        costs[iy * gm.width + ix] = cost;
    }

    public BufferedImage makeBufferedImage()
    {
        double m = 0;
        for (int i = 0; i < costs.length; i++) {
            if (costs[i] < Double.MAX_VALUE)
                m = Math.max(m, costs[i]);
        }
        for (int i = 0; i < costs.length; i++) {
            if (costs[i] == Double.MAX_VALUE)
                costs[i] = m;
        }

        return new FloatImage(gm.width, gm.height, costs).normalize().makeImage();

    }

    //////////////////////////////////////////////////////////////////////////////
    // Test code below
    static class WavefrontTest extends VisCanvasEventAdapter implements ParameterListener
    {
        GridMap gm;

        JFrame jf;
        VisWorld vw = new VisWorld();
        VisCanvas vc = new VisCanvas(vw);

        ArrayList<double[]> sinks = new ArrayList<double[]>();
        double source[];

        ParameterGUI pg = new ParameterGUI();

        public WavefrontTest(GridMap gm)
        {
            this.gm = gm;

            pg.addBoolean("earlyexit", "early exit", true);
            pg.addButtons("clear", "clear");

            jf = new JFrame("Wavefront Test");

            VisWorld.Buffer vb = vw.getBuffer("image");
            vb.addBuffered(new VisImage(new VisTexture(gm.makeBufferedImage()), gm.getXY0(), gm.getXY1()));
            vb.switchBuffer();

            vc.addEventHandler(this);
            vc.getViewManager().viewGoal.fit2D(gm.getXY0(), gm.getXY1());

            pg.addListener(this);

            jf.setLayout(new BorderLayout());
            jf.add(pg, BorderLayout.SOUTH);
            jf.add(vc, BorderLayout.CENTER);
            jf.setSize(800,600);
            jf.setVisible(true);
        }

        public void parameterChanged(ParameterGUI pg, String name)
        {
            if (name.equals("clear")) {
                sinks.clear();
                source = null;
            }
            redraw();
        }

        public String getName()
        {
            return "Wavefront Test";
        }

        public boolean mousePressed(VisCanvas vc,  GRay3D ray, MouseEvent e)
        {
            int mods=e.getModifiersEx();
            boolean shift=(mods&MouseEvent.SHIFT_DOWN_MASK)>0;
            boolean control=(mods&MouseEvent.CTRL_DOWN_MASK)>0;

            if (e.getButton() != 1)
                return false;

            double xy[] = ray.intersectPlaneXY();
            if (control) {
                sinks.add(xy);
                redraw();
                return true;
            }

            if (shift) {
                source = xy;
                redraw();
                return true;
            }
            return false;
        }

        void redraw()
        {
            if (true) {
                VisWorld.Buffer vb = vw.getBuffer("sourcesink");
                vb.setDrawOrder(50);
                vb.addBuffered(new VisData(sinks, new VisDataPointStyle(Color.blue, 5)));
                if (source != null)
                    vb.addBuffered(new VisData(source, new VisDataPointStyle(Color.red, 8)));
                vb.switchBuffer();
            }

            if (sinks.size() == 0 || source == null)
                return;

            Tic tic = new Tic();
            Wavefront wf = new Wavefront(gm);
            wf.earlyExit = pg.gb("earlyexit");
            ArrayList<double[]> path = wf.compute(source, sinks);
            double dt = tic.toc();

            if (true) {
                VisWorld.Buffer vb = vw.getBuffer("time");
                vb.setDrawOrder(100);
                vb.addBuffered(new VisText(VisText.ANCHOR.BOTTOM_RIGHT, String.format("%.2f ms\n", dt*1000)));
                vb.switchBuffer();
            }

            if (true) {
                VisWorld.Buffer vb = vw.getBuffer("path");
                vb.setDrawOrder(100);
                vb.addBuffered(new VisData(path, new VisDataLineStyle(Color.yellow, 1)));
                vb.switchBuffer();
            }

            if (true) {
                VisWorld.Buffer vb = vw.getBuffer("wavefront");
                vb.addBuffered(new VisImage(new VisTexture(wf.makeBufferedImage()), gm.getXY0(), gm.getXY1()));
                vb.switchBuffer();
            }
        }
    }

    public static void main(String args[])
    {
        try {
            BufferedImage im = ImageIO.read(new File(args[0]));
            GridMap gm = GridMap.makePixels(0, 0, im, 1, 0);

            for (int i = 0; i < gm.data.length; i++)
                gm.data[i] = (byte) Math.max(1, gm.data[i] & 0xff);

            new WavefrontTest(gm);
        } catch (IOException ex) {
            System.out.println("ex: "+ex);
        }
    }
}
