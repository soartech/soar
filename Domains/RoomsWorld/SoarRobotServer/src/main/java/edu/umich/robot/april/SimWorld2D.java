package edu.umich.robot.april;

import java.awt.image.*;
import java.util.*;

import april.util.*;

import april.jmat.*;
import april.jmat.geom.*;

/**
 * Simulates a vertically-extruded world containing either free space or impassible regions. This is factored out from Simulator in the hopes that it may be generically useful in other contexts.
 * 
 * Data can be provided in the form of a bit-map image, or in terms of geometry.
 **/
public class SimWorld2D
{
    // BufferedImage: obstacle iff blue channel >= 128.
    BufferedImage                  im;
    int                            im_pixels[];
    double                         metersPerPixel, pixelsPerMeter;
    double                         cx, cy;                                      // which pixel coordinates map to (0,0) in world
    // space? Default is width/2, height/2.
    // cx, cy, sizex, sizey, theta.
    ArrayList<double[]>            rects      = null;
    // the four corners of the rectangles given above.
    ArrayList<ArrayList<double[]>> rectpoints = null;
    // the edges of all the rectangles
    ArrayList<GLineSegment2D>      rectsegs   = new ArrayList<GLineSegment2D>();
    
    public SimWorld2D()
    {
    }

    /**
     * @param originxy
     *            pixel coordinates corresponding to the origin; if null, the center of the image is used.
     **/
    synchronized public void setImage(BufferedImage _im, double originxy[], double metersPerPixel)
    {
        this.im = ImageUtil.convertImage(_im, BufferedImage.TYPE_INT_RGB);
        this.im_pixels = ((DataBufferInt) im.getRaster().getDataBuffer()).getData();
        this.metersPerPixel = metersPerPixel;
        this.pixelsPerMeter = 1.0 / metersPerPixel;
        if (originxy == null)
        {
            this.cx = im.getWidth() / 2.0;
            this.cy = im.getHeight() / 2.0;
        }
        else
        {
            this.cx = originxy[0];
            this.cy = originxy[1];
        }
    }

    static final double[] projectCorner(double cxy[], double dx, double dy, double theta)
    {
        double s = Math.sin(theta), c = Math.cos(theta);
        return new double[] { c * dx - s * dy + cxy[0], s * dx + c * dy + cxy[1] };
    }

    synchronized public void setRects(ArrayList<double[]> rects)
    {
        this.rects = rects;
        this.rectsegs = new ArrayList<GLineSegment2D>();
        for (double rect[] : rects)
        {
            double cxy[] = new double[] { rect[0], rect[1] };
            double p0[] = projectCorner(cxy, -rect[2] / 2, -rect[3] / 2, rect[4]);
            double p1[] = projectCorner(cxy, -rect[2] / 2, +rect[3] / 2, rect[4]);
            double p2[] = projectCorner(cxy, +rect[2] / 2, +rect[3] / 2, rect[4]);
            double p3[] = projectCorner(cxy, +rect[2] / 2, -rect[3] / 2, rect[4]);
            rectsegs.add(new GLineSegment2D(p0, p1));
            rectsegs.add(new GLineSegment2D(p1, p2));
            rectsegs.add(new GLineSegment2D(p2, p3));
            rectsegs.add(new GLineSegment2D(p3, p0));
        }
    }

    /** Sample the value of the pixmap corresponding to the location pos **/
    synchronized public int sampleRGB(double pos[], int defaultrgb)
    {
        return privateSampleRGB(pos, defaultrgb);
    }

    /** Sample the value of the pixmap corresponding to the location pos **/
    final int privateSampleRGB(double pos[], int defaultrgb)
    {
        if (im == null)
            return defaultrgb;
        int ix = (int) (pos[0] * pixelsPerMeter + cx);
        int iy = (int) (-pos[1] * pixelsPerMeter + cy);
        int width = im.getWidth(), height = im.getHeight();
        if (ix >= 0 && ix < width && iy >= 0 && iy < height)
            return im_pixels[iy * width + ix];
        return defaultrgb;
    }

    /** Is the point pos in a collision state? **/
    synchronized public boolean isCollision(double pos[])
    {
        return isImageCollision(pos) || isRectCollision(pos);
    }

    /**
     * is point pos in a collision as far as the image is concerned?
     **/
    final boolean isImageCollision(double pos[])
    {
        int rgb = privateSampleRGB(pos, 0);
        return ((rgb & 0xff) >= 128);
    }

    final boolean isRectCollision(double pos[])
    {
        if (rects == null)
            return false;
        for (double rect[] : rects)
        {
            double theta = -rect[4];
            double s = Math.sin(theta), c = Math.cos(theta);
            double dx = pos[0] - rect[0];
            double dy = pos[1] - rect[1];
            double x = Math.abs(c * dx - s * dy);
            double y = Math.abs(s * dx + c * dy);
            if (x <= rect[2] && y <= rect[3])
                return true;
            /*
             * if (pos[0] >= (rect[0]-rect[2]/2) && pos[0] <= (rect[0]+rect[2]/2) && pos[1] >= (rect[1]-rect[3]/2) && pos[1] <= (rect[1]+rect[3]/2)) return true;
             */
        }
        return false;
    }

    /**
     * Starting from a position, follow a ray in the given direction. Report the distance where the first collision occurs.
     **/
    synchronized public double findCollision(double pos0[], double direction[], double maxDistance)
    {
        double unitdir[] = LinAlg.normalize(direction);
        double range = Math.min(findImageCollision(pos0, unitdir, maxDistance), findRectCollision(pos0, unitdir, maxDistance));
        return range;
    }

    static final double sign(double v)
    {
        return (v < 0 ? -1 : 1);
    }

    /*
     * This is unfinished and doesn't work quite right either. double findImageCollision(double pos0[], double unitdir[], double maxDistance) { // x and y positions in fractional pixel coordinates double px = pos0[0]*pixelsPerMeter + cx; double py = -pos0[1]*pixelsPerMeter + cy;
     * 
     * double distance = 0;
     * 
     * int width = im.getWidth(); int height = im.getHeight();
     * 
     * // pixel coordinates of a collision (if any) int ix = (int) px; int iy = (int) py;
     * 
     * if (Math.abs(unitdir[0]) >= Math.abs(unitdir[1])) { // per-iteration m double dx = sign(unitdir[0]); double dy = (dx/unitdir[0]) * unitdir[1];
     * 
     * // TODO: skip to next even X pixel boundary.
     * 
     * px = ix;
     * 
     * double ds = metersPerPixel*Math.sqrt(dx*dx + dy*dy);
     * 
     * while (distance <= maxDistance) {
     * 
     * int rgb = 0;
     * 
     * if (ix >= 0 && ix < width && iy>=0 && iy < height) rgb = im_pixels[iy*width + ix];
     * 
     * if ((rgb & 0xff) >= 128) break;
     * 
     * px += dx; py -= dy;
     * 
     * if (((int) py) != iy) { iy = (int) py; if (ix >= 0 && ix < width && iy>=0 && iy < height) rgb = im_pixels[iy*width + ix];
     * 
     * if ((rgb & 0xff) >= 128) break; }
     * 
     * ix = (int) px; iy = (int) py;
     * 
     * distance += ds; } } // TODO: handle y-direction
     * 
     * // if no collision, we're done. if (distance >= maxDistance) return maxDistance;
     * 
     * double p0[] = new double[] { (ix+0-cx)*metersPerPixel, -(iy+0-cy)*metersPerPixel }; double p1[] = new double[] { (ix+1-cx)*metersPerPixel, -(iy+0-cy)*metersPerPixel }; double p2[] = new double[] { (ix+1-cx)*metersPerPixel, -(iy+1-cy)*metersPerPixel }; double p3[] = new double[] { (ix+0-cx)*metersPerPixel, -(iy+1-cy)*metersPerPixel };
     * 
     * double posxy[] = new double[] {pos0[0], pos0[1]}; GLineSegment2D ray = new GLineSegment2D(posxy, LinAlg.add(posxy, LinAlg.scale(LinAlg.normalize(unitdir), maxDistance)));
     * 
     * GLineSegment2D s0 = new GLineSegment2D(p0, p1); GLineSegment2D s1 = new GLineSegment2D(p1, p2); GLineSegment2D s2 = new GLineSegment2D(p2, p3); GLineSegment2D s3 = new GLineSegment2D(p3, p0);
     * 
     * double d = maxDistance; double thisd = 0; double p[] = null;
     * 
     * p = ray.intersectionWith(s0); if (p != null) { thisd = LinAlg.distance(p, posxy); d = Math.min(d, thisd); }
     * 
     * p = ray.intersectionWith(s1); if (p != null) { thisd = LinAlg.distance(p, posxy); d = Math.min(d, thisd); }
     * 
     * p = ray.intersectionWith(s2); if (p != null) { thisd = LinAlg.distance(p, posxy); d = Math.min(d, thisd); }
     * 
     * p = ray.intersectionWith(s3); if (p != null) { thisd = LinAlg.distance(p, posxy); d = Math.min(d, thisd); }
     * 
     * return d; }
     */
    /** findCollision, only considering the image. **/
    double findImageCollision(double pos0[], double unitdir[], double maxDistance)
    {
        if (im == null)
            return maxDistance;
        if (isImageCollision(pos0))
            return 0;
        // how much do we move in each direction each iteration?
        double dx = unitdir[0] * metersPerPixel;
        double dy = unitdir[1] * metersPerPixel;
        // XXX: increase step size in one direction to 1 to increase speed
        double rangeMeters = 0;
        // ray coordinates
        double rxy[] = new double[] { pos0[0], pos0[1] };
        double sxy[] = new double[] { 0, 0 };
        while (rangeMeters < maxDistance)
        {
            if (isImageCollision(rxy))
            {
                // The rangeMeters result now is servicable, but has
                // quantization noise of metersPerPixel magnitude. If
                // that's okay, you can break right here.
                //
                // break;
                //
                // Otherwise, let's try to refine our
                // collision point. We do a binary
                // refinement, keeping track of the FULL
                // and EMPTY points nearest the boundary.
                // we had an instant collision... exit now.
                if (rangeMeters == 0)
                    break;
                // s0: The closest coordinate that was known EMPTY.
                double s0x = rxy[0] - dx, s0y = rxy[1] - dy;
                // s1: The cloest coordinate that was known FULL
                double s1x = rxy[0], s1y = rxy[1];
                for (int i = 0; i < 8; i++)
                {
                    // sample half way between s0 and s1.
                    sxy[0] = (s0x + s1x) / 2;
                    sxy[1] = (s0y + s1y) / 2;
                    if (isImageCollision(sxy))
                    {
                        // FULL: move s1 closer
                        s1x = sxy[0];
                        s1y = sxy[1];
                    }
                    else
                    {
                        // EMPTY: move s0 closer
                        s0x = sxy[0];
                        s0y = sxy[1];
                    }
                }
                rangeMeters = Math.sqrt(LinAlg.sq(sxy[0] - pos0[0]) + LinAlg.sq(sxy[1] - pos0[1]));
                break;
            }
            rxy[0] += dx;
            rxy[1] += dy;
            rangeMeters += metersPerPixel;
        }
        if (rangeMeters < 0)
            rangeMeters = 0;
        if (rangeMeters > maxDistance)
            rangeMeters = maxDistance;
        return rangeMeters;
    }

    private final double[] rectCollisionTemp = new double[2];
    /** not thread safe **/
    double findRectCollision(double pos0[], double unitdir[], double maxDistance)
    {
        if (rects == null)
            return maxDistance;
        double posxy[] = new double[] { pos0[0], pos0[1] };
        double distance = maxDistance;
        GLineSegment2D ray = new GLineSegment2D(posxy, LinAlg.add(posxy, LinAlg.scale(unitdir, maxDistance)));
        for (GLineSegment2D seg : rectsegs)
        {
            
            // TODO SoarApril
            double p[] = ray.intersectionWith(seg /*, rectCollisionTemp */);
            
            if (p != null)
                distance = Math.min(distance, LinAlg.distance(posxy, p));
        }
        return distance;
    }

}
