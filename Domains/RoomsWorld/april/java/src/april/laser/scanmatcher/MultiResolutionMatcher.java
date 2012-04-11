package april.laser.scanmatcher;

import java.util.*;

import april.util.*;
import april.jmat.*;
import april.config.*;

import april.vis.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

// 200 155
// 1700 1700
public class MultiResolutionMatcher
{
    int decimate = 2;
    int ndecimate = 8; // 1 + number of decimated levels

    // our grid maps, with 0 = full resolution. 1 = 1/decimate
    // resolution. 2 = 1/decimate^2 resolution
    GridMap gms[];

    static boolean debug = false;
    static MultiResolutionMatcher.DebugViewer matcherDebugViewer = debug ? new MultiResolutionMatcher.DebugViewer() : null;
    ArrayList<Debug> debugs = new ArrayList<Debug>();

    static boolean warnedHeapOrder = false;

    static class Debug
    {
        Node n;
        GridMap gm;
        ArrayList<Pt> pts;
    }

    public MultiResolutionMatcher()
    {
        gms = new GridMap[ndecimate];
    }

    public void setModel(GridMap gm0)
    {
        gms = new GridMap[ndecimate];

       // pad the gridmap with a border
        if (false) {
            int border = 512;
            GridMap gmc = GridMap.makePixels(gm0.x0 - border*gm0.metersPerPixel,
                                             gm0.y0 - border*gm0.metersPerPixel,
                                             gm0.width + 2*border,
                                             gm0.height + 2*border,
                                             gm0.metersPerPixel,
                                             gm0.defaultFill,
                                             true);

            for (int y = 0; y < gm0.height; y++) {
                for (int x = 0; x < gm0.width; x++) {
                    gmc.data[(y+border)*gmc.width + (x+border)] = gm0.data[y*gm0.width+x];
                }
            }

            gm0 = gmc;
        }


        Tic tic = new Tic();

        gms[0] = gm0;

        // create gridmap pyramid
        for (int i = 1; i < ndecimate; i++) {
            int lastdecimate = (int) Math.pow(decimate, i-1);
            int thisdecimate = (int) Math.pow(decimate, i);
            // last + k - 1 = thisdecimate
            int k = thisdecimate - lastdecimate + 1;

            gms[i] = gms[i-1].maxConvolution(k);
//            gms[i] = gms[0].maxConvolution(thisdecimate);
        }
    }

    public double[] match(ArrayList<double[]> points, double prior[], double priorinv[][],
                          double xyt0[],
                          double xrange, double yrange, double trange, double tres)
    {
        Search search = new Search(points, prior, priorinv, xyt0, xrange, yrange, trange, tres);
        double res[] = search.compute();
        if (debug)
            matcherDebugViewer.set(this, search);
        return res;
    }

    static final class Pt
    {
        // coordinates of the point, after rotation, in units of
        // metersPerPixel (of the highest-resolution gridmap) relative
        // to the gridmap's x0,y0
        int ix, iy;
        int cnt;
    }

    static class Node
    {
        double score; // upper bound on a scan matching result of this node's children.

        double match_score;
        double chi2_score;

        int tidx;
        int gmidx; // resolution (0 = full)

        // what is the search range?  tx0, ty0 are translation offsets
        // in units of metersPerPixel (of the finest resolution grid)
        // note that tx0=0, ty0=0 corresponds to the center of our
        // search range (usually the prior).
        //
        // This node represents the search range from [tx0, tx0 +
        // searchwidth), and similarly for y.
        int tx0, ty0;
        int searchwidth, searchheight; // amount to search, in units of metersPerPixel

        void print()
        {
            System.out.printf("score %15f = %15f + %15f\n", score, match_score, chi2_score);
            System.out.printf("  tidx: %-5d   gmidx: %-5d\n", tidx, gmidx);
            System.out.printf("   tx0: %-5d     ty0: %-5d\n", tx0, ty0);
            System.out.printf("    w: %-5d        h: %-5d\n", searchwidth, searchheight);
        }
    }

    static final double reduceMagnitude(double v, double reduction)
    {
        if (v <= 0)
            return Math.min(0, v + reduction);
        return Math.max(0, v - reduction);
    }

    class Search
    {
        ArrayList<double[]> points;
        double prior[];
        double xyt0[]; // center of search (usually == prior, but doesn't have to be.)
        double priorinv[][];
        double xrange, yrange;

        double t0;
        int tcnt; // how many t slices?
        double tres;

        MaxHeap<Object> heap = new MaxHeap<Object>();

        ArrayList<Pt> ptsCache[]; // tidx
        Chi2Data chi2data[];

        Search(ArrayList<double[]> points, double prior[], double priorinv[][],
               double xyt0[],
               double xrange, double yrange, double trange, double tres)
        {
            this.points = points;
            this.prior = prior;
            this.xyt0 = xyt0;
            this.priorinv = priorinv;
            this.xrange = xrange;
            this.yrange = yrange;

            this.t0 = xyt0[2] - trange;
            this.tres = tres;
            this.tcnt = (int) LinAlg.clamp(2*trange/tres + 1, 1, 2*Math.PI / tres);

            ptsCache = new ArrayList[tcnt];

            double priorP[][] = LinAlg.inverse(priorinv);

            chi2data = new Chi2Data[tcnt];
            for (int i = 0; i < tcnt; i++)
                chi2data[i] = new Chi2Data(prior, priorP, priorinv, t0 + i*tres);
        }

        double computeNodeChi2(Node n)
        {
            double cx = (n.tx0 + n.searchwidth / 2.0)*gms[0].metersPerPixel;
            double cy = (n.ty0 + n.searchheight / 2.0)*gms[0].metersPerPixel;
            double cr = gms[0].metersPerPixel*Math.sqrt(n.searchwidth*n.searchwidth/4.0 + n.searchheight*n.searchheight/4.0);

            return -chi2data[n.tidx].computeChi2(cx, cy, cr);
        }

        double[] compute()
        {
            // What is the score of the best leaf we've yet
            // encountered?  (There's no sense in expanding nodes
            // whose upper bound is worse than this.)
            double bestLeafScore = -Double.MAX_VALUE;

            // create an initial set of nodes that search over all the
            // thetas at our coarsest level of resolution.
            for (int tidx = 0; tidx < tcnt; tidx++) {
                Node n = new Node();
                n.gmidx = gms.length - 1;   // coarsest

                // round aggressively...
                n.tidx = tidx;
                n.tx0 = (int) ((xyt0[0]-xrange) / gms[0].metersPerPixel - 1);
                n.ty0 = (int) ((xyt0[1]-yrange) / gms[0].metersPerPixel - 1);
                n.searchwidth = (int) (2*xrange / gms[0].metersPerPixel + 2);
                n.searchheight = (int) (2*yrange / gms[0].metersPerPixel + 2);

                // best case score; this will cause us to expand this
                // node to see how good its children *actually* are.
                n.match_score = points.size()*255;
                n.chi2_score = computeNodeChi2(n);
                n.score = n.match_score + n.chi2_score;

                heap.add(n, n.score);
            }

            // now, create one more node that represents our fall-back
            // to the prior. We can never do worse than a score of
            // zero!
            if (true) {
                Node n = new Node();
                n.gmidx = -1; // finest
                n.tidx = (int) Math.round((prior[2] - t0) / tres);
                n.tx0 = (int) Math.round(prior[0] / gms[0].metersPerPixel);
                n.ty0 = (int) Math.round(prior[1] / gms[0].metersPerPixel);
                n.searchwidth = 1; // shouldn't be used
                n.searchheight = 1;
                n.match_score = 0; // worst-case
                n.chi2_score = 0;  // by definition
                n.score = n.match_score + n.chi2_score;

                bestLeafScore = n.score;
                heap.add(n, n.score);
            }

            // now, keep pulling off search nodes, in order of most
            // promising to least promising. When we arrive at a
            // node that is already at the finest resolution (i.e.,
            // has no children), we're done.
            Node lastNode = null;

            // count how many iterations we perform at each level for diagnostic purposes.
            int depthCount[] = new int[ndecimate];

            for (int iter = 0; true; iter++) {

                Object heapobj = heap.removeMax();

                if (heapobj instanceof ArrayList) {
                    ArrayList<Node> children = (ArrayList<Node>) heapobj;
                    for (Node child : children)
                        heap.add(child, child.score);
                    continue;
                }

                Node n = (Node) heapobj;

                if (lastNode != null && n.score > lastNode.score) {
                    if (true) {
                        if (!warnedHeapOrder) {
                            warnedHeapOrder = true;
                            System.out.println("MultiResolutionMatcher: heap ordering violated. I'll be quiet now.");
                        }
                    } else {
                        System.out.printf("*** ORDERING **************************************************\n");
                        System.out.printf("heap ordering violated %15f %15f\n", n.score, lastNode.score);

                        System.out.printf("this node: \n");
                        n.print();
                        System.out.printf("last node: \n");
                        lastNode.print();
                    }
                }
                lastNode = n;

                if (debug) {
                    // create debug records for all but the initial searches
                    if (n.gmidx!=gms.length-1) {
                        Debug dbg = new Debug();
                        dbg.n = n;
                        dbg.gm = gms[n.gmidx+1];
                        dbg.pts = getPoints(n.tidx);
                        debugs.add(dbg);
                    }
                }

                if (n.gmidx == -1) {
                    // we're done!
                    GridMap gm = gms[0];

                    double result[] = new double[] { (n.tx0+n.searchwidth/2.0)*gm.metersPerPixel,
                                                     (n.ty0+n.searchheight/2.0)*gm.metersPerPixel,
                                                     t0 + n.tidx*tres, // don't add 0.5
                                                     n.score };

                    if (false) {
                        System.out.printf("depth count: ");
                        for (int i = 0; i < ndecimate; i++)
                            System.out.printf("%8d ", depthCount[i]);
                        System.out.printf("\n");
                    }

                    return result;
                }

                // expand this node
                GridMap gm = gms[n.gmidx];

                // XXX might be better to invert the search order, i.e.,
                // go over pts first. We can then omit the bounds check
                // and get better locality

                // actually inverse resolution: how many pixels in
                // gms[0] does one pixel in gm represent?
                int resolution = (int) Math.pow(decimate, n.gmidx);

                ArrayList<Pt> pts = getPoints(n.tidx);

//                System.out.printf("%6d : res %d,  score %15f,  heapsz %6d,  ( xyt %5d %5d %5d ) (sz %5d %5d) pts %d\n",
//                                  iter, n.gmidx, n.score, heap.size(), n.tx0, n.ty0, n.tidx, n.searchwidth, n.searchheight, pts.size());

                ArrayList<Node> children = new ArrayList<Node>();
                Node bestNode = null;

                for (int dy = n.ty0; dy < n.ty0+n.searchheight; dy+=resolution) {
                    for (int dx = n.tx0; dx < n.tx0+n.searchwidth; dx+=resolution) {

                        double score = 0;

                        depthCount[n.gmidx]++;

                        for (Pt pt : pts) {
                            // (mx,my) are pixel coordinates in gms[0]
                            int mx = (pt.ix + dx);
                            int my = (pt.iy + dy);

                            // we now want to compute the largest
                            // score we could possibly obtain for a
                            // pixel starting at (mx, my) subjected to
                            // translations of [0, resolution-1] in both
                            // x and y directions.
                            int v;

                            // if mx or my is closer to zero, then for
                            // *some translation* within this block,
                            // we'll appear at pixel 0,0.
                            if (mx < 0 && mx > -resolution)
                                mx = 0;
                            if (my < 0 && my > -resolution)
                                my = 0;

                            if (mx >= 0 && my >=0 && mx < gm.width && my < gm.height) {
                                v = gm.data[my*gm.width + mx] & 0xff;
                            } else {
                                v = gm.defaultFill & 0xff;
                            }

                            score += v * pt.cnt;
                        }

                        if (score <= bestLeafScore)
                            continue;

                        // create a new search job
                        Node child = new Node();
                        child.gmidx = n.gmidx - 1;
                        child.tidx = n.tidx;
                        child.tx0 = dx;
                        child.ty0 = dy;
                        child.searchwidth = Math.min(n.tx0+n.searchwidth - dx, resolution);
                        child.searchheight = Math.min(n.ty0+n.searchheight - dy, resolution);
                        child.match_score = score;
                        child.chi2_score = computeNodeChi2(child);
                        child.score = child.match_score + child.chi2_score;

                        if (child.gmidx == -1) {
                            bestLeafScore = Math.max(bestLeafScore, child.score);
                        }

                        children.add(child);

                        if (bestNode == null || child.score > bestNode.score) {
                            bestNode = child;
                        }
                    }
                }

                if (n.gmidx == 0) {
                    // leaf node
                    if (bestNode != null)
                        heap.add(bestNode, bestNode.score);
                } else {
                    // not a leaf node
                    if (children.size() > 0)
                        heap.add(children, bestNode.score);
                }
            }
        }

        // Project our points into integer-valued coordinates relative
        // to (0,0) (not x0,y0).
        ArrayList<Pt> getPoints(int tidx)
        {
            ArrayList<Pt> pts = (ArrayList<Pt>) ptsCache[tidx];
            if (pts != null)
                return pts;

            double t = t0 + tidx*tres;
            double s = Math.sin(t), c = Math.cos(t);

            HashMap<Long, Pt> ptMap = new HashMap<Long, Pt>(points.size());
            pts = new ArrayList<Pt>(points.size());

            GridMap gm = gms[0];

            for (double p[] : points) {
                int ix = (int) ((c*p[0] - s*p[1] - gm.x0) / gm.metersPerPixel);
                int iy = (int) ((s*p[0] + c*p[1] - gm.y0) / gm.metersPerPixel);

                long key = (ix << 32) + iy;

                Pt pt = ptMap.get(key);
                if (pt != null) {
                    pt.cnt ++;
                } else {
                    pt = new Pt();
                    pt.ix = ix;
                    pt.iy = iy;
                    pt.cnt = 1;
                    pts.add(pt);
                    ptMap.put(key, pt);
                }
            }

            ptsCache[tidx] = pts;

            return pts;
        }
    }

    public static class DebugViewer implements ParameterListener
    {
        JFrame jf;
        VisWorld vw = new VisWorld();
        VisCanvas vc = new VisCanvas(vw);
        ParameterGUI pg = new ParameterGUI();

        MultiResolutionMatcher matcher;
        Search search;

        public DebugViewer()
        {
            pg.addIntSlider("nodeidx", "node index", 0, 0, 0);
            pg.addIntSlider("tx", "user translation (x)", -1000, 1000, 0);
            pg.addIntSlider("ty", "user translation (y)", -1000, 1000, 0);
            pg.addIntSlider("tt", "user rotation (theta)", 0, 180, 1);

            vc.setBackground(Color.black);
            vw.getBuffer("grid").addFront(new VisGrid());

            jf = new JFrame("ScanMatcher Debug");
            jf.setLayout(new BorderLayout());
            jf.add(vc, BorderLayout.CENTER);
            jf.add(pg, BorderLayout.SOUTH);

            jf.setSize(800,600);
            jf.setVisible(true);

            pg.addListener(this);
        }

        public void set(MultiResolutionMatcher matcher, Search search)
        {
            this.matcher = matcher;
            this.search = search;

            pg.setMinMax("nodeidx", 0, matcher.debugs.size()-1);
            pg.setMinMax("tt", 0, search.tcnt-1);

            parameterChanged(pg, "nodeidx");
        }

        public void parameterChanged(ParameterGUI pg, String name)
        {
            Debug dbg = matcher.debugs.get(pg.gi("nodeidx"));

            if (true) {
                VisWorld.Buffer vb = vw.getBuffer("gridmap");
                vb.addBuffered(new VisImage(new VisTexture(dbg.gm.makeBufferedImage()), dbg.gm.getXY0(), dbg.gm.getXY1()));
                vb.switchBuffer();
            }

            // debug output
            if (true) {
                VisWorld.Buffer vb = vw.getBuffer("points");
                ArrayList<double[]> ps = new ArrayList<double[]>();
                for (Pt pt : dbg.pts) {
                    ps.add(new double[] { matcher.gms[0].x0 + matcher.gms[0].metersPerPixel*(pt.ix + dbg.n.tx0 + .5),
                                          matcher.gms[0].y0 + matcher.gms[0].metersPerPixel*(pt.iy + dbg.n.ty0 + .5) });
                }
                vb.addBuffered(new VisData(ps, new VisDataPointStyle(Color.black, 5), new VisDataPointStyle(Color.yellow, 2)));
                vb.switchBuffer();

                vb = vw.getBuffer("score");
                vb.addBuffered(new VisText(VisText.ANCHOR.BOTTOM_LEFT, String.format("<<yellow>>score %15f (%15f + %15f), npts %d\nswidth=%5d, sheight=%5d",
                                                                                     Math.min(1E10,dbg.n.score), dbg.n.match_score, dbg.n.chi2_score,
                                                                                     dbg.pts.size(), dbg.n.searchwidth, dbg.n.searchheight)));
                vb.switchBuffer();
            }

            // user alignment output
            if (true) {
                VisWorld.Buffer vb = vw.getBuffer("userpoints");
                ArrayList<double[]> ps = new ArrayList<double[]>();
                int tx = pg.gi("tx");
                int ty = pg.gi("ty");
                int tt = pg.gi("tt");
                if (tt >= search.tcnt)
                    tt = search.tcnt - 1;

                for (Pt pt : search.getPoints(tt)) {
                    ps.add(new double[] { matcher.gms[0].x0 + matcher.gms[0].metersPerPixel*(pt.ix + tx + .5),
                                          matcher.gms[0].y0 + matcher.gms[0].metersPerPixel*(pt.iy + ty + .5) });
                }
                vb.addBuffered(new VisData(ps, new VisDataPointStyle(Color.black, 5), new VisDataPointStyle(Color.red, 2)));
                vb.switchBuffer();

                double score = dbg.gm.score(search.points, matcher.gms[0].metersPerPixel*(tx+.5), matcher.gms[0].metersPerPixel*(ty+.5), search.t0 + search.tres*tt, search.prior, search.priorinv);

                vb = vw.getBuffer("userscore");
                vb.addBuffered(new VisText(VisText.ANCHOR.BOTTOM_RIGHT, String.format("<<red>>score %15f",
                                                                                      Math.min(1E10,score))));
                vb.switchBuffer();
            }

        }
    }
}
