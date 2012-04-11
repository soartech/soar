package april.util;

import java.io.*;
import java.util.*;
import java.awt.image.*;
import javax.imageio.*;

import april.jmat.*;

/** A geo-referenced image. It is specified by two files: an image
 * file (e.g., "foo.png") and a tiepoints file (e.g., "foo.pngw").
 * The tiepoints file contains data exactly equivalent to that found
 * in a GeoTIFF: a list of tiepoints. Each tiepoint is listed on a
 * line by itself, and has six fields:
 *
 * pixelx pixely pixelz longitude latitude altitude
 *
 * The pixelz and altitude fields are ignored and are typically
 * zero. GPS positions correspond to pixel *centers*.
 *
 *
 *  How to create your own GeoImage:
 *
 * Download the NASA Worldwind applet for saving geotiff images from
 *     http://worldwind.arc.nasa.gov/java/demos/SectorSelectAndSave.jnlp
 *  1) Select and save the area you are trying to grab. Be sure to cycle through the layers
 *     to find the best imagery (MS is usually good) for your location
 *  2) convert foo.tif foo.png  (this may complain about some unknown fields, but ignore)
 *  3) java magic.util.TIFF > foo.pngw
 *
 * now pass the path for foo.png to an instance of this class
 **/
public class GeoImage
{
    public static class TiePoint
    {
        double image[]; // pixel coordinates
        double ll[];   // lat lon el
        double xy[];    // position in meters using linearization
    }

    BufferedImage im;

    ArrayList<TiePoint> tiepoints = new ArrayList<TiePoint>();
    GPSLinearization gpslin;

    double im2xy_P[][], im2xy_offset[];
    double im2xy_Pinv[][];

    /** gpslin can be null, in which case a linearization point is
     * selected near the coordinates given in the pngw file. **/
    public GeoImage(String path, GPSLinearization _gpslin) throws IOException
    {
        im = ImageIO.read(new File(path));

        BufferedReader ins = new BufferedReader(new FileReader(path+"w"));
        String line;
        while ((line = ins.readLine()) != null) {
            String toks[] = line.trim().split("\\s+");
            TiePoint tp = new TiePoint();
            tp.image = new double[] { Double.parseDouble(toks[0]),
                                      Double.parseDouble(toks[1]) };
            tp.ll  = new double[] {   Double.parseDouble(toks[4]),
                                      Double.parseDouble(toks[3]) };

            tiepoints.add(tp);
        }

        double mean_ll[] = new double[2];

        // BUG: won't work where lat/lon wraps around
        if (_gpslin == null) {
            for (TiePoint tp : tiepoints) {
                mean_ll[0] += tp.ll[0] / tiepoints.size();
                mean_ll[1] += tp.ll[1] / tiepoints.size();
            }

            this.gpslin = new GPSLinearization(mean_ll);
        } else {
            this.gpslin = _gpslin;
        }

        for (TiePoint tp : tiepoints) {
            tp.xy = gpslin.ll2xy(tp.ll);

            if (false) {
                System.out.printf("TiePoint: image (%15f, %15f) => gps (%15f, %15f) => xy (%15f, %15f)\n",
                                  tp.image[0], tp.image[1],
                                  tp.ll[0], tp.ll[1],
                                  tp.xy[0], tp.xy[1]);
            }
        }

        // fit a linear model that converts from pixel coordinates to meters
        // [ x ]   [ e ]   [ a b ] [ px ]
        // [ y ] = [ f ] + [ c d ] [ py ]
        //
        // (rearrange so that a,b,c,d,e,f are the unknowns, least squares solution)
        // [ x ]   [ px  py 0  0  1  0 ] [ a ]
        // [ y ] = [ 0   0  px py 0  1 ] [ b ]
        //                               [ c ]
        //                               [ d ]
        //                               [ e ]
        //                               [ f ]
        //
        // In matrix notation:
        // y = Jx

        Matrix J = new Matrix(tiepoints.size()*2, 6);
        Matrix y = new Matrix(tiepoints.size()*2, 1);

        for (int tpidx = 0; tpidx < tiepoints.size(); tpidx++) {
            TiePoint tp = tiepoints.get(tpidx);
            J.set(tpidx*2 + 0, 0, tp.image[0]);
            J.set(tpidx*2 + 0, 1, tp.image[1]);
            J.set(tpidx*2 + 0, 4, 1);
            J.set(tpidx*2 + 1, 2, tp.image[0]);
            J.set(tpidx*2 + 1, 3, tp.image[1]);
            J.set(tpidx*2 + 1, 5, 1);

            y.set(tpidx*2 + 0, 0, tp.xy[0]);
            y.set(tpidx*2 + 1, 0, tp.xy[1]);
        }

        Matrix Jt = J.transpose();
        Matrix JtJ = Jt.times(J);
        Matrix x = JtJ.inverse().times(Jt.times(y));

        im2xy_P = new double[][] { { x.get(0,0), x.get(1,0) },
                                   { x.get(2,0), x.get(3,0) } };

        im2xy_Pinv = LinAlg.inverse(im2xy_P);

        im2xy_offset = new double[] { x.get(4,0),
                                      x.get(5,0) };

        // sanity check our projection matrix by testing the tiepoints.
        for (TiePoint tp : tiepoints) {
            double xy[] = image2xy(tp.image);
            double error = LinAlg.distance(xy, tp.xy);

            if (error > 0.00001)
                System.out.printf("WARNING: TiePoint reprojection error of %15f m\n", error);

            double im[] = xy2image(xy);
            error = LinAlg.distance(im, tp.image);
            if (error > 0.00001)
                System.out.printf("WARNING: TiePoint reprojection error of %15f m\n", error);
        }
    }

    public BufferedImage getImage()
    {
        return im;
    }

    /** get a 4x4 matrix that scales and rotates the image properly. **/
    public double[][] getMatrix()
    {
        double T[][] = LinAlg.identity(4);
        for (int i = 0; i < 2; i++)
            for (int j = 0; j < 2; j++)
                T[i][j] = im2xy_P[i][j];
        T[0][3] = im2xy_offset[0];
        T[1][3] = im2xy_offset[1];
        return T;
    }

    /** Convert pixel coordinates to meters **/
    public double[] image2xy(double pxy[])
    {
        return LinAlg.add(im2xy_offset, LinAlg.matrixAB(im2xy_P, pxy));
    }

    public double[] xy2image(double xy[])
    {
        return LinAlg.matrixAB(im2xy_Pinv, LinAlg.subtract(xy, im2xy_offset));
    }

    public GPSLinearization getGPSLinearization()
    {
        return gpslin;
    }
}
