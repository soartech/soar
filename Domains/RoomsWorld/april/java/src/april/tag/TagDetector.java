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

import april.image.*;
import april.util.*;

public class TagDetector
{
    TagFamily tagFamily;

    /** Gaussian smoothing kernel applied to image (0 == no filter)
     * used when sampling bits. Filtering is a good idea in cases
     * where A) a cheap camera is introducing artifical sharpening, B)
     * the bayer pattern is creating artifcats, C) the sensor is very
     * noisy and/or has hot/cold pixels. However, filtering makes it
     * harder to decode very small tags. Reasonable values are 0, or
     * [0.8, 1.5].**/
    public double sigma = 0;

    /** Gaussian smoothing kernel applied to image (0 == no filter)
     * used when detecting the outline of the box. It is almost always
     * useful to have some filtering, since the loss of small details
     * won't hurt. Recommended value = 0.8. The case where sigma ==
     * segsigma has been optimized to avoid a redundant filter
     * operation. **/
    public double segSigma = 0.8;

    /** Instead of blurring the input image before segmentation, we
     * can achieve similar effects by decimating the image by a factor
     * of two. When enabled, this option applies a block LPF filter of
     * width 2, then decimates the image. With this option, not only
     * can we safely set segSigma = 0, but the slowest part of the
     * algorithm (the segmentation) runs about 4 times faster. The
     * downside is that the position of the targets is determined
     * based on the segmentation: lower resolution will result in more
     * localization error. However, the effect on quality is quite
     * modest, and this optimization is generally recommended (along
     * with segSigma = 0). If segSigma is non-zero, the filtering by
     * segSigma occurs first, followed by the block LPF, and the
     * decimation. **/
    public boolean segDecimate = false;

    /** Do not consider pixels whose gradient magnitude is less than
     * minMag. Small values make the detector more sensitive, but also
     * force us to consider many more edges resulting in slower
     * computation time. A value of 0.001 is very sensitive. A value
     * of 0.01 is quite fast. **/
    public double minMag = 0.004;

    /** When connecting edges, what is the maximum range allowed for
     * the gradient directions? **/
    public double maxEdgeCost = Math.toRadians(30);

    /** When growing components, the intra component variation is
     * allowed to grow when the component is small in size. These two
     * thresholds determine how much. **/
    public double thetaThresh = 100;
    public double magThresh = 1200;

    // in pixels. Set based on minimum plausible decoding size for Tag9 family.
    public double minimumLineLength = 4;

    // minimum number of pixels in a segment before we'll fit a line to it.
    public double minimumSegmentSize = 4;

    // minimum size of tag (in pixels) as measured along edges and diagonals.
    public double minimumTagSize = 6;

    /** Early pruning of quads which have insane aspect ratios. **/
    public double maxQuadAspectRatio = 32;

    /** Produce debugging output. If the debugging code annoys you (or
     * makes porting harder) you can delete all of the code in an
     * if(debug) block.
     **/
    public boolean debug = false;
    public BufferedImage   debugInput;         // the input image (after preprocessing)
    public BufferedImage   debugSegInput;      // the input image passed to segmentation.
    public BufferedImage   debugSegmentation;  // segmented image
    public BufferedImage   debugTheta, debugMag;
    public VisWorld.Buffer debugSegments;      // segments fit to clusters
    public VisWorld.Buffer debugQuads;         // quads fit to segments
    public VisWorld.Buffer debugSamples;       // samples taken within a quad.
    public VisWorld.Buffer debugLabels;        // labels drawn for each accepted quad.

    /** The optical center of the current frame, which is needed to correctly compute the homography. **/
    double opticalCenter[];

    /** During segmentation, the weight of an edge is related to the
     * change in theta between the two pixels. This change is
     * normalized via maxEdgeCost, resulting in a number[0,1]. We then
     * convert this number to fixed-point by multiplying it by
     * WEIGHT_SCALE. The resulting number must fit in the number of
     * bits allocated to it, currently 16 (see Step 3). Large values
     * of WEIGHT_SCALE are good because they lead to better
     * fixed-point approximations. However, small numbers are better,
     * because there are fewer discrete values of edge costs, which
     * means that there is more spatial/temporal coherence when
     * processing the sorted edges. This results in faster
     * processing. Given that these orientations are pretty noisy to
     * begin with, some quantization error is acceptable.
     **/
    public int WEIGHT_SCALE = 100;//10000;

    public TagDetector(TagFamily tagFamily)
    {
        this.tagFamily = tagFamily;
    }

    final int edgeCost(double theta0, double mag0, double theta1, double mag1)
    {
        if (mag0 < minMag || mag1 < minMag)
            return -1;

        double thetaErr = Math.abs(MathUtil.mod2pi(theta1 - theta0));
        if (thetaErr > maxEdgeCost)
            return -1;

        double normErr = thetaErr / maxEdgeCost;

        return (int) (normErr * WEIGHT_SCALE);
    }

    // lousy approximation of arctan function, but good enough for our purposes (about 4 degrees)
    final double arctan2(double y, double x)
    {
        double coeff_1 = Math.PI/4;
        double coeff_2 = 3*coeff_1;
        double abs_y = Math.abs(y)+1e-10;      // kludge to prevent 0/0 condition

        double angle;

        if (x >= 0) {
            double r = (x - abs_y) / (x + abs_y);
            angle = coeff_1 - coeff_1 * r;
        } else {
            double r = (x + abs_y) / (abs_y - x);
            angle = coeff_2 - coeff_1 * r;
        }

        if (y < 0)
            return -angle;     // negate if in quad III or IV
        else
            return angle;
    }


    /** Sort and return the first vlength values in v[] by the value
     * of v[i]&amp;mask. The maximum value in the array 'v' is maxv
     * (if maxv is negative, maxv will be found). These weights must
     * be small enough to fit in an integer. This implementation is
     * stable.
     **/
    static long[] countingSortLongArray(long v[], int vlength, int maxv, long mask)
    {
        if (maxv < 0) {
            for (int i = 0; i < vlength; i++)
                maxv = Math.max(maxv, (int) (v[i]&mask));
        }

        // For weight 'w', counts[w] will give the output position for
        // the next sample with weight w.  To build this, we begin by
        // counting how many samples there are with each weight. (Note
        // that the initial position for weight w is only affected by
        // the number of weights less than w, hence the +1 in
        // counts[w+1].
        int counts[] = new int[maxv+2];

        for (int i = 0; i < vlength; i++) {
            int w = (int) (v[i]&mask);
            counts[w+1]++;
        }

        // accumulate.
        for (int i = 1; i < counts.length; i++)
            counts[i] += counts[i-1];

        long newv[] = new long[vlength];
        for (int i = 0; i < vlength; i++) {
            int w = (int) (v[i]&mask);
            newv[counts[w]] = v[i];
            counts[w]++;
        }

/*       // test (debugging code)
        for (int i = 0; i+1 < newv.length; i++) {
            int w0 = (int) (newv[i]&mask);
            int w1 = (int) (newv[i+1]&mask);
            assert(w0 <= w1);
        }
*/

        return newv;
    }

    /** Detect the features in the specified image. We need the
     * optical center, but it is usually fine to pass in (width/2,
     * height/2).
     **/
    public ArrayList<TagDetection> process(BufferedImage im, double opticalCenter[])
    {
        this.opticalCenter = opticalCenter;

        // This is a very long function, but it can't really be
        // factored any more simply: it's just a long sequence of
        // sequential operations.

        ///////////////////////////////////////////////////////////
        // Step one. Preprocess image (convert to float (grayscale)
        // and low pass if necessary.)
        FloatImage fimOrig = new FloatImage(im);

        FloatImage fim = fimOrig;
        if (sigma > 0) {
            int filtsz = ((int) Math.max(3, 3*sigma)) | 1;
            float filt[] = SigProc.makeGaussianFilter(sigma, filtsz);
            fim = fimOrig.filterFactoredCentered(filt, filt);
        }

        if (debug) {
            debugInput = fim.makeImage();
        }

        ///////////////////////////////////////////////////////////
        // Step two. For each pixel, compute the local gradient. We
        // store the direction and magnitude.

        // This step is quite sensitive to noise, since a few bad
        // theta estimates will break up segments, causing us to miss
        // quads. It is helpful to do a Gaussian low-pass on this step
        // even if we don't want it for decoding.
        FloatImage fimseg = fimOrig; // default
        if (segSigma > 0) {
            if (segSigma == sigma) {
                // reuse the already-filtered image...
                fimseg = fim;
            } else {
                // blur anew.
                int filtsz = ((int) Math.max(3, 3*segSigma)) | 1;
                float filt[] = SigProc.makeGaussianFilter(segSigma, filtsz);
                fimseg = fimOrig.filterFactoredCentered(filt, filt);
            }
        }
        if (segDecimate)
            fimseg = fimseg.decimateAvg();

        FloatImage fimTheta = new FloatImage(fimseg.width, fimseg.height);
        FloatImage fimMag = new FloatImage(fimseg.width, fimseg.height);

        for (int y = 1; y+1 < fimseg.height; y++) {
            for (int x = 1; x+1 < fimseg.width; x++) {

                float Ix = fimseg.get(x+1, y) - fimseg.get(x-1, y);
                float Iy = fimseg.get(x,y+1) - fimseg.get(x, y-1);

                double mag = Ix*Ix + Iy*Iy;
                double theta = arctan2(Iy, Ix);

                fimTheta.set(x, y, (float) theta);
                fimMag.set(x, y, (float) mag);
            }
        }

        if (debug) {
            debugTheta = fimTheta.normalize().makeImage();
            debugMag = fimMag.normalize().makeImage();
        }

        ///////////////////////////////////////////////////////////
        // Step three. Segment the edges, grouping pixels with similar
        // thetas together. This is a greedy algorithm: we start with
        // the most similar pixels.  We use 4-connectivity.
        UnionFindSimple uf = new UnionFindSimple(fimseg.width*fimseg.height);

        if (true) {
            int width = fimseg.width;
            int height = fimseg.height;

            long edges[] = new long[width*height*4];
            int nedges = 0;

            // for efficiency, each edge is encoded as a single
            // long. The constants below are used to pack/unpack the
            // long.
            final long IDA_SHIFT = 40, IDB_SHIFT = 16, INDEX_MASK = (1<<24) - 1, WEIGHT_MASK=(1<<16)-1;

            // bounds on the thetas assigned to this group. Note that
            // because theta is periodic, these are defined such that the
            // average value is contained *within* the interval.
            double tmin[] = new double[width*height];
            double tmax[] = new double[width*height];

            double mmin[] = new double[width*height];
            double mmax[] = new double[width*height];

            for (int y = 0; y+1 < fimseg.height; y++) {
                for (int x = 0; x+1 < fimseg.width; x++) {

                    double mag0 = fimMag.get(x,y);
                    if (mag0 < minMag)
                        continue;
                    mmax[y*width+x] = mag0;
                    mmin[y*width+x] = mag0;

                    double theta0 = fimTheta.get(x,y);
                    tmin[y*width+x] = theta0;
                    tmax[y*width+x] = theta0;

                    int edgeCost;

                    edgeCost = edgeCost(theta0, mag0, fimTheta.get(x+1, y), fimMag.get(x+1,y));
                    if (edgeCost >= 0)
                        edges[nedges++] = (((long) y*width+x)<<IDA_SHIFT) + (((long) y*width+x+1)<<IDB_SHIFT) + edgeCost;

                    edgeCost = edgeCost(theta0, mag0, fimTheta.get(x, y+1), fimMag.get(x,y+1));
                    if (edgeCost >= 0)
                        edges[nedges++] = ((long) (y*width+x)<<IDA_SHIFT) + (((long) (y+1)*width+x)<<IDB_SHIFT) + edgeCost;

                    edgeCost = edgeCost(theta0, mag0, fimTheta.get(x+1, y+1), fimMag.get(x+1,y+1));
                    if (edgeCost >= 0)
                        edges[nedges++] = (((long) y*width+x)<<IDA_SHIFT) + (((long) (y+1)*width+x+1)<<IDB_SHIFT) + edgeCost;

                    edgeCost = (x == 0) ? -1 : edgeCost(theta0, mag0, fimTheta.get(x-1, y+1), fimMag.get(x-1,y+1));
                    if (edgeCost >= 0)
                        edges[nedges++] = (((long) y*width+x)<<IDA_SHIFT) + (((long) (y+1)*width+x-1)<<IDB_SHIFT) + edgeCost;

                    // XXX Would 8 connectivity help for rotated tags?
                    // (Probably not much, so long as input filtering
                    // hasn't been disabled.)
                }
            }

            // sort those edges by weight (lowest weight first).
            edges = countingSortLongArray(edges, nedges, -1, WEIGHT_MASK);

            // process edges in order of increasing weight, merging
            // clusters if we can do so without exceeding the
            // thetaThresh.
            for (int i = 0; i < nedges; i++) {
                int ida = (int) ((edges[i]>>IDA_SHIFT)&INDEX_MASK);
                int idb = (int) ((edges[i]>>IDB_SHIFT)&INDEX_MASK);

                ida = uf.getRepresentative(ida);
                idb = uf.getRepresentative(idb);

                if (ida == idb)
                    continue;

                int sza = uf.getSetSize(ida);
                int szb = uf.getSetSize(idb);

                double tmina = tmin[ida], tmaxa = tmax[ida];
                double tminb = tmin[idb], tmaxb = tmax[idb];

                double costa = (tmaxa-tmina);
                double costb = (tmaxb-tminb);

                // bshift will be a multiple of 2pi that aligns the spans
                // of b with a so that we can properly take the union of
                // them.
                double bshift = MathUtil.mod2pi((tmina+tmaxa)/2, (tminb+tmaxb)/2) - (tminb+tmaxb)/2;

                double tminab = Math.min(tmina, tminb + bshift);
                double tmaxab = Math.max(tmaxa, tmaxb + bshift);

                if (tmaxab - tminab > 2*Math.PI)  // corner case that's probably not useful to handle correctly. oh well.
                    tmaxab = tminab + 2*Math.PI;

                double mmaxab = Math.max(mmax[ida], mmax[idb]);
                double mminab = Math.min(mmin[ida], mmin[idb]);

                // merge these two clusters?
                double costab = (tmaxab - tminab);
                if (costab <= (Math.min(costa, costb) + thetaThresh/(sza+szb)) &&
                    (mmaxab-mminab) <= Math.min(mmax[ida]-mmin[ida], mmax[idb]-mmin[idb]) + magThresh/(sza+szb)) {

                    int idab = uf.connectNodes(ida, idb);

                    tmin[idab] = tminab;
                    tmax[idab] = tmaxab;

                    mmin[idab] = mminab;
                    mmax[idab] = mmaxab;
                }
            }
        }

        ///////////////////////////////////////////////////////////
        // Step four. Loop over the pixels again, collecting
        // statistics for each cluster. We will soon fit lines to
        // these points.

        if (debug) {
            debugSegmentation = new BufferedImage(fimseg.width, fimseg.height, BufferedImage.TYPE_INT_RGB);
        }

        HashMap<Integer,ArrayList<double[]>> clusters = new HashMap<Integer,ArrayList<double[]>>();
        for (int y = 0; y+1 < fimseg.height; y++) {
            for (int x = 0; x+1 < fimseg.width; x++) {
                if (uf.getSetSize(y*fimseg.width+x) < minimumSegmentSize) {
                    if (debug)
                        debugSegmentation.setRGB(x, y, 0);
                    continue;
                }

                int rep = (int) uf.getRepresentative(y*fimseg.width + x);
                if (debug)
                    debugSegmentation.setRGB(x, y, rep);

                ArrayList<double[]> points = clusters.get(rep);
                if (points == null){
                    points = new ArrayList<double[]>();
                    clusters.put(rep, points);
                }

                double pt[] = new double[] { x, y, fimMag.get(x,y) };

                points.add(pt);
            }
        }

        ///////////////////////////////////////////////////////////
        // Step five. Loop over the clusters, fitting lines (which we
        // call Segments).
        ArrayList<Segment> segments = new ArrayList<Segment>();

        for (ArrayList<double[]> points : clusters.values()) {
            GLineSegment2D gseg = GLineSegment2D.lsqFitXYW(points);

            // filter short lines
            double length =  LinAlg.distance(gseg.p1, gseg.p2);
            if (length < minimumLineLength)
                continue;

            Segment seg = new Segment();
            double dy = gseg.p2[1] - gseg.p1[1];
            double dx = gseg.p2[0] - gseg.p1[0];

            seg.theta = MathUtil.atan2(gseg.p2[1] - gseg.p1[1],
                                       gseg.p2[0] - gseg.p1[0]);
            seg.length = length;

            // We add an extra semantic to segments: the vector
            // p1->p2 will have dark on the left, white on the right.
            // To do this, we'll look at every gradient and each one
            // will vote for which way they think the gradient should
            // go. (This is way more retentive than necessary: we
            // could probably sample just one point!)
            double flip = 0, noflip = 0;
            for (double xyw[] : points) {
                double theta = fimTheta.get((int) xyw[0], (int) xyw[1]);
                double mag = fimMag.get((int) xyw[0], (int) xyw[1]);

                // err *should* be +Math.PI/2 for the correct winding,
                // but if we've got the wrong winding, it'll be around
                // -Math.PI/2.
                double err = MathUtil.mod2pi(theta - seg.theta);

                if (err < 0)
                    noflip += mag;
                else
                    flip += mag;
            }

            if (flip > noflip) {
                seg.theta += Math.PI;
            }

            double dot = dx*Math.cos(seg.theta) + dy*Math.sin(seg.theta);
            if (dot > 0) {
                seg.x0 = gseg.p2[0]; seg.y0 = gseg.p2[1];
                seg.x1 = gseg.p1[0]; seg.y1 = gseg.p1[1];
            } else {
                seg.x0 = gseg.p1[0]; seg.y0 = gseg.p1[1];
                seg.x1 = gseg.p2[0]; seg.y1 = gseg.p2[1];
            }

            if (segDecimate) {
                seg.x0 = 2*seg.x0 + .5;
                seg.y0 = 2*seg.y0 + .5;
                seg.x1 = 2*seg.x1 + .5;
                seg.y1 = 2*seg.y1 + .5;
                seg.length *= 2;
            }

            segments.add(seg);
        }

        int width = fim.width, height = fim.height;

        if (debug && debugSegments != null) {

            for (Segment seg : segments) {
                double cx = (seg.x0 + seg.x1)/2, cy = (seg.y0 + seg.y1)/2;

                double notch = Math.max(2, 0.1*seg.length);

                debugSegments.addBuffered(new VisChain(LinAlg.translate(0, height, 0),
                                                       LinAlg.scale(1, -1, 1),
                                                       new VisData(new VisDataLineStyle(Color.yellow, 1),
                                                                   new double[] { seg.x0, seg.y0},
                                                                   new double[] { seg.x1, seg.y1}),
                                                       new VisData(new VisDataLineStyle(Color.yellow, 1),
                                                                   new double[] { cx,  cy },
                                                                   new double[] { cx + notch*Math.sin(seg.theta),
                                                                                  cy - notch*Math.cos(seg.theta) }),
                                                       new VisData(new VisDataPointStyle(Color.red, 4),
                                                                   new double[] { seg.x0, seg.y0 })));
            }
        }

        ////////////////////////////////////////////////////////////////
        // Step six. For each segment, find segments that begin where
        // this segment ends. (We will chain segments together
        // next...) The gridder accelerates the search by building
        // (essentially) a 2D hash table.
        Gridder<Segment> gridder = new Gridder<Segment>(0, 0, width, height, 10);

        // add every segment to the hash table according to the
        // position of the segment's first point. (Remember that the
        // first point has a specific meaning due to our left-hand
        // rule above.)
        for (Segment seg : segments) {
            gridder.add(seg.x0, seg.y0, seg);
        }

        // Now, find child segments that begin where each parent
        // segments ends.
        for (Segment parent : segments) {

            // compute length of the line segment
            GLine2D parentLine = new GLine2D(new double[] { parent.x0, parent.y0 },
                                             new double[] { parent.x1, parent.y1 });

            for (Segment child : gridder.find(parent.x1, parent.y1, 0.5*parent.length)) {
//            for (Segment child : gridder.find(parent.x1, parent.y1, 5+parent.length)) {
                // require child to have the right handedness...
                if (MathUtil.mod2pi(child.theta - parent.theta) > 0)
                    continue;

                // compute intersection of points.
                GLine2D childLine = new GLine2D(new double[] { child.x0, child.y0 },
                                                new double[] { child.x1, child.y1 });

                double p[] = parentLine.intersectionWith(childLine);
                if (p == null)
                    continue;

                double parentDist = LinAlg.distance(p, new double[] {parent.x1, parent.y1});
                double childDist = LinAlg.distance(p, new double[] {child.x0, child.y0});

                if (Math.max(parentDist, childDist) > parent.length)
                    continue;

                // everything's okay, this child is a reasonable successor.
                parent.children.add(child);

            }
        }

        ////////////////////////////////////////////////////////////////
        // Step seven. Search all connected segments to see if any
        // form a loop of length 4. Add those to the quads list.
        ArrayList<Quad> quads = new ArrayList<Quad>();

        if (true) {
            Segment tmp[] = new Segment[5];
            for (Segment seg : segments) {
                tmp[0] = seg;
                search(quads, tmp, seg, 0);
            }
        }

        if (debug && debugQuads != null) {
            for (Quad q : quads) {
                debugQuads.addBuffered(new VisChain(LinAlg.translate(0, height, 0),
                                                    LinAlg.scale(1, -1, 1),
                                                    new VisData(new VisDataLineStyle(Color.orange, 2),
                                                                q.p[0], q.p[1], q.p[2], q.p[3], q.p[0])));
            }
        }


        ////////////////////////////////////////////////////////////////
        // Step eight. Decode the quads. For each quad, we first
        // estimate a threshold color to decided between 0 and
        // 1. Then, we read off the bits and see if they make sense.
        ArrayList<TagDetection> detections = new ArrayList<TagDetection>();

        for (Quad quad : quads) {
            // Find a threshold
            GrayModel blackModel = new GrayModel();
            GrayModel whiteModel = new GrayModel();

            VisData vdblack = null;
            VisData vdwhite = null;
            VisData vdsamp = null;

            if (debug && debugSamples != null) {
                vdblack = new VisData(new VisDataPointStyle(Color.black, 3));
                vdwhite = new VisData(new VisDataPointStyle(Color.lightGray, 3));
                vdsamp = new VisData(new VisDataPointStyle(Color.orange, 4));
            }

            // sample points around the black and white border in
            // order to calibrate our gray threshold. This code is
            // simpler if we loop over the whole rectangle and discard
            // the points we don't want.
            int dd = 2*tagFamily.blackBorder + tagFamily.d;

            for (int iy = -1; iy <= dd; iy++) {
                for (int ix = -1; ix <= dd; ix++) {
                    double y = (iy + .5) / dd;
                    double x = (ix + .5) / dd;

                    double pxy[] = quad.interpolate01(x, y);
                    int irx = (int) (pxy[0]+.5);
                    int iry = (int) (pxy[1]+.5);

                    if (irx < 0 || irx >= width || iry < 0 || iry >= height)
                        continue;

                    float v = fim.get(irx, iry);

                    if ((iy == -1 || iy == dd) || (ix == -1 || ix == dd)) {
                        // part of the outer white border.
                        whiteModel.addObservation(x, y, v);

                        if (debug && debugSamples != null)
                            vdwhite.add(pxy);
                    } else if ((iy == 0 || iy == (dd-1)) || (ix == 0 || ix == (dd-1))) {
                        // part of the outer black border.
                        blackModel.addObservation(x, y, v);

                        if (debug && debugSamples != null)
                            vdblack.add(pxy);
                    }
                }
            }

            boolean bad = false;
            long tagCode = 0;

            // Try reading off the bits.
            // XXX: todo: multiple samples within each cell and vote?
            for (int iy = tagFamily.d-1; iy >= 0; iy--) {
                for (int ix = 0; ix < tagFamily.d; ix++) {
                    double y = (tagFamily.blackBorder + iy + .5) / dd;
                    double x = (tagFamily.blackBorder + ix + .5) / dd;

                    double pxy[] = quad.interpolate01(x, y);
                    int irx = (int) (pxy[0]+.5);
                    int iry = (int) (pxy[1]+.5);

                    if (irx < 0 || irx >= width || iry < 0 || iry >= height) {
                        bad = true;
                        continue;
                    }

                    double threshold = (blackModel.interpolate(x, y) + whiteModel.interpolate(x,y))*.5;

                    if (debug && debugSamples != null)
                        vdsamp.add(pxy);

                    float v = fim.get(irx, iry);

                    tagCode = tagCode << 1;
                    if (v > threshold)
                        tagCode |= 1;
                }
            }

            if (debug && debugSamples != null) {
                debugSamples.addBuffered(new VisChain(LinAlg.translate(0, height, 0),
                                                      LinAlg.scale(1, -1, 1),
                                                      vdwhite,
                                                      vdblack,
                                                      vdsamp));
            }

            if (!bad) {
                TagDetection d = new TagDetection();
                tagFamily.decode(d, tagCode);

                // rotate points in detection according to decoded
                // orientation. Thus the order of the points in the
                // detection object can be used to determine the
                // orientation of the target.
                d.p = new double[4][];

                for (int i = 0; i < 4; i++) {
                    d.p[(4+i-d.rotation)%4] = quad.p[i];
                }

                // compute the homography (and rotate it appropriately)
                d.homography = quad.homography.getH();
                d.hxy = quad.homography.getCXY();

                if (true) {
                    double c = Math.cos(d.rotation*Math.PI/2.0);
                    double s = Math.sin(d.rotation*Math.PI/2.0);
                    double R[][] = new double[][] {{ c, -s, 0},
                                                   { s,  c, 0},
                                                   { 0,  0, 1} };
                    d.homography = LinAlg.matrixAB(d.homography, R);
                }

                if (d.good) {
                    detections.add(d);
                    d.cxy = quad.interpolate01(.5, .5);
                    d.observedPerimeter = quad.observedPerimeter;
                }
            }
        }

        if (debug) {
            if (debugSegments != null)
                debugSegments.switchBuffer();
            if (debugQuads != null)
                debugQuads.switchBuffer();
            if (debugSamples != null)
                debugSamples.switchBuffer();
            if (debugLabels != null)
                debugLabels.switchBuffer();
        }

        ////////////////////////////////////////////////////////////////
        // Step nine. Some quads may be detected more than once, due
        // to partial occlusion and our aggressive attempts to recover
        // from broken lines. When two quads (with the same id)
        // overlap, we will keep the one with the lowest error, and if
        // the error is the same, the one with the greatest observed
        // perimeter.

        ArrayList<TagDetection> goodDetections = new ArrayList<TagDetection>();

        // NOTE: allow multiple (non-overlapping) detections of the same target.
        for (TagDetection d : detections) {

            boolean newFeature = true;

            for (int odidx = 0; odidx < goodDetections.size(); odidx++) {
                TagDetection od = goodDetections.get(odidx);

                if (d.id != od.id || !detectionsOverlapTooMuch(d, od))
                    continue;

                // there's a conflict. we must pick one to keep.
                newFeature = false;

                // this detection is worse than the previous one... just don't use it.
                if (d.hammingDistance > od.hammingDistance)
                    continue;

                // otherwise, keep the new one if it either has
                // *lower* error, or has greater perimeter
                if (d.hammingDistance < od.hammingDistance || d.observedPerimeter > od.observedPerimeter)
                    goodDetections.set(odidx, d);
            }

            if (newFeature)
                goodDetections.add(d);
        }

        detections = goodDetections;

        ////////////////////////////////////////////////////////////////
        // I thought it would never end.
        return detections;
    }

    boolean detectionsOverlapTooMuch(TagDetection a, TagDetection b)
    {
        // Compute a sort of "radius" of the two targets. We'll do
        // this by computing the average length of the edges of the
        // quads (in pixels).
        double radius = 0.0625*(LinAlg.distance(a.p[0], a.p[1]) +
                                LinAlg.distance(a.p[1], a.p[2]) +
                                LinAlg.distance(a.p[2], a.p[3]) +
                                LinAlg.distance(a.p[3], a.p[0]) +
                                LinAlg.distance(b.p[0], b.p[1]) +
                                LinAlg.distance(b.p[1], b.p[2]) +
                                LinAlg.distance(b.p[2], b.p[3]) +
                                LinAlg.distance(b.p[3], b.p[0]));

        // distance (in pixels) between two tag centers.
        double d = LinAlg.distance(a.cxy, b.cxy);

        // reject pairs where the distance between centroids is
        // smaller than the "radius" of one of the tags.
        return (d < radius);
    }

    /** Represents a line fit to a set of pixels whose gradients are
     * similar.
     **/
    class Segment
    {
        double x0, y0, x1, y1;
        double theta;  // gradient direction (points towards white)
        double length; // length of line segment in pixels
        ArrayList<Segment> children = new ArrayList<Segment>();

        public double length()
        {
            return Math.sqrt((x1-x0)*(x1-x0) + (y1-y0)*(y1-y0));
        }
    }

    /** Represents four segments that form a loop, and might be a tag. **/
    class Quad
    {
        // points for the quad (in pixel coordinates), in counter
        // clockwise order. These points are the intersections of
        // segments.
        double p[][] = new double[4][];

        // The total length (in pixels) of the actual perimeter
        // observed for the quad. This is in contrast to the geometric
        // perimeter, some of which may not have been directly
        // observed but rather inferred by intersecting
        // segments. Quads with more observed perimeter are preferred
        // over others.
        double observedPerimeter;

        // Given that the whole quad spans from (0,0) to (1,1) in
        // "quad space", compute the pixel coordinates for a given
        // point within that quad. Note that for most of the Quad's
        // existence, we will not know the correct orientation of the
        // tag.
        Homography33 homography;

        /** (x,y) are the optical center of the camera, which is
         * needed to correctly compute the homography.
         **/
        public Quad(double p[][])
        {
            this.p = p;

            homography = new Homography33(opticalCenter[0], opticalCenter[1]);
            homography.addCorrespondence(-1, -1, p[0][0], p[0][1]);
            homography.addCorrespondence( 1, -1, p[1][0], p[1][1]);
            homography.addCorrespondence( 1,  1, p[2][0], p[2][1]);
            homography.addCorrespondence(-1,  1, p[3][0], p[3][1]);
        }

        // Same as interpolate, except that the coordinates are
        // interpreted between 0 and 1, instead of -1 and 1.
        double[] interpolate01(double x, double y)
        {
            return interpolate(2*x - 1, 2*y - 1);
        }

        // interpolate given that the lower left corner of the lower
        // left cell is at (-1,-1) and the upper right corner of
        // the upper right cell is at (1,1)
        double[] interpolate(double x, double y)
        {
            return homography.project(x, y);
        }
    }

    /** quads: any discovered quads will be added to this list.
        path: The segments currently part of the search.
        parent: The first segment in the quad.
        depth: how deep in the search are we?
    **/
    void search(ArrayList<Quad> quads, Segment[] path, Segment parent, int depth)
    {
        // terminal depth occurs when we've found four segments.
        if (depth == 4) {
            // Is the first segment the same as the last segment (i.e., a loop?)
            if (path[4] == path[0]) {

                // the 4 corners of the quad as computed by the intersection of segments.
                double p[][] = new double[4][];
                double observedPerimeter = 0;

                boolean bad = false;
                for (int i = 0; i < 4; i++) {
                    // compute intersections between all the
                    // lines. This will give us sub-pixel accuracy for
                    // the corners of the quad.
                    GLine2D linea = new GLine2D(new double[] { path[i].x0, path[i].y0 },
                                                new double[] { path[i].x1, path[i].y1 });
                    GLine2D lineb = new GLine2D(new double[] { path[i+1].x0, path[i+1].y0 },
                                                new double[] { path[i+1].x1, path[i+1].y1 });
                    p[i] = linea.intersectionWith(lineb);

                    observedPerimeter += path[i].length();

                    // no intersection? Occurs when the lines are almost parallel.
                    if (p[i] == null)
                        bad = true;
                }

                // eliminate quads that don't form a simply connected
                // loop (i.e., those that form an hour glass, or wind
                // the wrong way.)
                if (!bad) {
                    double t0 = MathUtil.atan2(p[1][1] - p[0][1], p[1][0] - p[0][0]);
                    double t1 = MathUtil.atan2(p[2][1] - p[1][1], p[2][0] - p[1][0]);
                    double t2 = MathUtil.atan2(p[3][1] - p[2][1], p[3][0] - p[2][0]);
                    double t3 = MathUtil.atan2(p[0][1] - p[3][1], p[0][0] - p[3][0]);

                    double ttheta = MathUtil.mod2pi(t1-t0) + MathUtil.mod2pi(t2-t1) +
                        MathUtil.mod2pi(t3-t2) + MathUtil.mod2pi(t0-t3);

                    // the magic value is -2*PI. It should be exact,
                    // but we allow for (lots of) numeric imprecision.
                    if (ttheta < -7 || ttheta > -5)
                        bad = true;
                }

                if (!bad) {
                    double d0 = LinAlg.distance(p[0], p[1]);
                    double d1 = LinAlg.distance(p[1], p[2]);
                    double d2 = LinAlg.distance(p[2], p[3]);
                    double d3 = LinAlg.distance(p[3], p[0]);
                    double d4 = LinAlg.distance(p[0], p[2]);
                    double d5 = LinAlg.distance(p[1], p[3]);

                    // check sizes
                    if (d0 < minimumTagSize || d1 < minimumTagSize || d2 < minimumTagSize ||
                        d3 < minimumTagSize || d4 < minimumTagSize || d5 < minimumTagSize)
                        bad = true;

                    // check aspect ratio
                    double dmax = Math.max(Math.max(d0, d1), Math.max(d2, d3));
                    double dmin = Math.min(Math.min(d0, d1), Math.min(d2, d3));

                    if (dmax > dmin * maxQuadAspectRatio) {
                        bad = true;
                    }
                }

                if (!bad) {
                    Quad q = new Quad(p);
                    q.observedPerimeter = observedPerimeter;
                    quads.add(q);
                }
            }
            return;
        }

        // Not terminal depth. Recurse on any children that obey the correct handedness.
        for (Segment child : parent.children) {
            // (handedness was checked when we created the children)

            // we could rediscover each quad 4 times (starting from
            // each corner). If we had an arbitrary ordering over
            // points, we can eliminate the redundant detections by
            // requiring that the first corner have the lowest
            // value. We're arbitrarily going to use theta...
            if (child.theta > path[0].theta)
                continue;

            path[depth+1] = child;
            search(quads, path, child, depth + 1);
        }
    }

    /** Fits a grayscale model over an area of the form:
        Ax + By + Cxy + D = value

        We use this model to compute spatially-varying thresholds for
        reading bits.
    **/
    static class GrayModel
    {
        // we're solving Ax = b. For each observation, we add a row to
        // A of the form [x y xy 1] and to be of the form [gray]. x is
        // the vector [A B C D].
        //
        // The least-squares solution to the system is x = inv(A'A)A'b
        double A[][] = new double[4][4]; // The A'A matrix
        double b[]   = new double[4];    // The A'b matrix
        double X[]; // our solution, [A B C D]

        int nobs; // how many observations?

        public void addObservation(double x, double y, double gray)
        {
            double xy = x*y;

            // update only upper-right elements. A'A is symmetric,
            // we'll fill the other elements in later.
            A[0][0] += x*x;
            A[0][1] += x*y;
            A[0][2] += x*xy;
            A[0][3] += x;
            A[1][1] += y*y;
            A[1][2] += y*xy;
            A[1][3] += y;
            A[2][2] += xy*xy;
            A[2][3] += xy;
            A[3][3] += 1;

            b[0] += x*gray;
            b[1] += y*gray;
            b[2] += xy*gray;
            b[3] += gray;

            nobs++;
            X = null; // force a new solution to be computed.
        }

        int getNumObservations()
        {
            return nobs;
        }

        void compute()
        {
            if (X != null) // already computed?
                return;

            if (nobs >= 6) {
                // we really only need 4 linearly independent
                // observations to fit our answer, but we'll be very
                // sensitive to noise if we don't have an
                // over-determined system. Thus, require at least 6
                // observations (or we'll use a constant model below).

                // make symmetric
                for (int i = 0; i < 4; i++)
                    for (int j = i+1; j < 4; j++)
                        A[j][i] = A[i][j];

                double Ainv[][] = LinAlg.inverse(A);
                if (Ainv != null) {
                    X = LinAlg.matrixAB(Ainv, b);
                }
            }

            if (X == null) {
                // not enough samples to fit a good model. Use a flat model.
                X = new double[4];
                X[3] = b[3] / nobs;
            }
        }

        public double interpolate(double x, double y)
        {
            compute();

            return X[0]*x + X[1]*y + X[2]*x*y + X[3];
        }
    }

    public static void main(String args[])
    {
        GrayModel gm = new GrayModel();
        gm.addObservation(0, 0, 0);
        gm.addObservation(0, 1, 1);

        gm.addObservation(1, 0, 1);
        gm.addObservation(1, 1, 1);

        for (double y = 0; y <= 1; y+= .2) {
            for (double x = 0; x <= 1; x+= .2) {
                System.out.printf("%6.3f ", gm.interpolate(x,y));
            }
            System.out.printf("\n");
        }
    }
}
