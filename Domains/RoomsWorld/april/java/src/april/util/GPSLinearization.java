package april.util;

import april.jmat.*;

import java.io.*;

/**
 *  This class enables conversion between latitude and longitdue and xy coordinates.
 *  The ellipsoid/sphere shape of the earth is linearized at a given lat/lon in
 *  decimal degrees, and allows conversion to xy location in meters.
 *  Java translation of april/core/src/common/gps_linearize.c/.h
 *
 *  @date August 2009
 *  @author Transcribed by jhstrom
 *
 */
public class GPSLinearization
{
    // Data retrieved from http://en.wikipedia.org/wiki/Earth_radius on 8/22/09
    public static final double RAD_EQ_M = 6378137.0; // RADIUS_EQUATOR_METERS
    public static final double RAD_PO_M = 6356752.3; // RADIUS_POLAR_METERS

    // indices
    public static final int LAT = 0;
    public static final int LON = 1;
    public static final int X = 0;
    public static final int Y = 1;

    double lat0_deg, lon0_deg;
    double lat0_rad, lon0_rad;
    double radius_ns, radius_ew;

    public GPSLinearization(double origin_deg[])
    {
        lat0_deg = origin_deg[LAT];
        lon0_deg = origin_deg[LON];

        final double a = RAD_EQ_M;
        final double b = RAD_PO_M;

        lat0_rad = Math.toRadians(lat0_deg);
        lon0_rad = Math.toRadians(lon0_deg);

        //Aproximating the two radii at this location:
        radius_ns = sq(a*b)/Math.pow(sq(a*Math.cos(lat0_rad)) +
                                     sq(b*Math.sin(lat0_rad)),1.5);

        radius_ew = a*a / Math.sqrt(sq(a*Math.cos(lat0_rad)) +
                                    sq(b*Math.sin(lat0_rad)));

    }

    public double[] getOriginLL()
    {
        return new double[] { lat0_deg, lon0_deg };
    }

    public double[] ll2xy(double latlon[])
    {
        final double latlon_rad [] = {
            Math.toRadians(latlon[LAT]),
            Math.toRadians(latlon[LON]) };

        final double d_rad[] = { latlon_rad[LAT] - lat0_rad,
                                 latlon_rad[LON] - lon0_rad};

        final double result[] = {
            Math.sin(d_rad[LON]) * radius_ew * Math.cos(lat0_rad),
            Math.sin(d_rad[LAT]) * radius_ns };

        return result;

    }

    public double[] xy2ll(double xy[])
    {
        double dlat_rad =
            Math.asin(xy[Y] / radius_ns);
        double dlon_rad =
            Math.asin( (xy[X] / radius_ew ) / Math.cos(lat0_rad) );

        return new double[] {Math.toDegrees(dlat_rad + lat0_rad),
                             Math.toDegrees(dlon_rad + lon0_rad)};

    }

    private final double sq(final double a)
    {
        return a*a;
    }

    /** Restore state that was previously written **/
    public void read(StructureReader ins) throws IOException
    {
        double ll_deg[] = ins.readDoubles();
        double r[] = ins.readDoubles();

        lat0_deg = ll_deg[0];
        lon0_deg = ll_deg[1];
        lat0_rad = Math.toRadians(lat0_deg);
        lon0_rad = Math.toRadians(lon0_deg);
        radius_ns = r[0];
        radius_ew = r[1];
    }

    /** Write one or more lines that serialize this instance. No line
     * is allowed to consist of just an asterisk. **/
    public void write(StructureWriter outs) throws IOException
    {
        outs.writeDoubles(new double[] { lat0_deg, lon0_deg });
        outs.writeDoubles(new double[] { radius_ns, radius_ew });
    }

    public static void main(String args[])
    {
       //testing code:
        double origin [] = {42.29277,-83.71750};
        GPSLinearization gpslin =  new GPSLinearization(origin);
        double lat_lons[][] = {{42.29138,-83.71472,0,1},
                               {42.28738,-83.71100,0,1},
                               {42.29277,-83.71750,0,1},
                               {42.10,-83.6,0,1},
                               {41.10,-82.6,0,1},
        };

        // XXX
    }
}
