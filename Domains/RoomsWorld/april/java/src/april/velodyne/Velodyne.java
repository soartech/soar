package april.velodyne;

import java.util.*;

import april.lcmtypes.*;
import april.jmat.*;

/** Parser for data from a Velodyne laser scanner. **/
public class Velodyne
{
    static final int    UPPER_MAGIC     = 0xeeff;
    static final int    LOWER_MAGIC     = 0xddff;
    static final double RADIANS_PER_LSB = 0.00017453293;
    static final double METERS_PER_LSB  = 0.002;
    static final int    NUM_LASERS      = 64;

    /** An individual hit from a Velodyne laser. **/
    public static final class Sample
    {
        public double xyz[] = new double[3]; // calibrated, projected into velodyne coordinate system
        public double rawRange;             // raw return directly from sensor
        public double range;                // corrected range
        public double ctheta;               // theta of sensor head at time of sample, **always [0, 2*PI)**
        public double theta;                // calibrated theta (horizontal/yaw angle) **always [0, 2*PI)**
        public double phi;                  // calibrated phi (veritcle/pitch angle)
        public double intensity;            // normalized intensity [0, 1]
        public int    physical;             // physical laser number (0-31 lower, 32-63 upper)
        public int    logical;              // logical laser number (in order of increasing pitch)
    }

    byte                data[];
    int                 blockOffset; // offset to the current data block
    int                 pos;        // position within current data block
    int                 laserIndex; // within this block, which laser are we processing?
    int                 laserOffset; // which physical laser corresponds to laserIndex = 0?
    double              ctheta, sin_ctheta, cos_ctheta;
    String              versionString;
    int                 revolutionCount;
    VelodyneCalibration calib;

    public Velodyne(VelodyneCalibration calib, byte data[])
    {
        this.calib = calib;
        this.data = data;
        this.blockOffset = 0;
        this.pos = 0;
        this.laserIndex = 0;
        if (data.length != 1206)
        {
            System.out.println("velodyne bad length:  " + data.length);
            this.data = new byte[0];
        }
        if (data.length == 1206)
        {
            if (data[1202] == 'v')
            {
                revolutionCount = (data[1200] & 0xff) + ((data[1201] & 0xff) << 8);
                StringBuffer sb = new StringBuffer();
                for (int i = 0; i < 4; i++)
                    sb.append((char) data[1202 + i]);
                versionString = sb.toString();
            }
            else
            {
                versionString = "";
                revolutionCount = -1;
            }
        }
    }

    public boolean next(Sample s)
    {
        // if we finished the last block, "consume" this block of data.
        if (laserIndex == 32)
        {
            blockOffset += 100;
            laserIndex = 0;
        }
        // starting a new block?
        if (laserIndex == 0)
        {
            // enough data for another block?
            if (blockOffset + 100 >= data.length)
                return false;
            int magic = (data[blockOffset] & 0xff) + ((data[blockOffset + 1] & 0xff) << 8);
            if (magic == UPPER_MAGIC)
                laserOffset = 32;
            else if (magic == LOWER_MAGIC)
                laserOffset = 0;
            else
            {
                System.out.printf("Unknown velodyne magic %4x\n", magic);
                return false;
            }
            // position of velodyne head, constant for all 32 measurements that follow
            ctheta = 2 * Math.PI - ((data[blockOffset + 2] & 0xff) + ((data[blockOffset + 3] & 0xff) << 8)) * RADIANS_PER_LSB;
            if (ctheta == 2 * Math.PI)
                ctheta = 0;
            sin_ctheta = Math.sin(ctheta);
            cos_ctheta = Math.cos(ctheta);
        }
        // decode the next laser sample
        s.physical = laserOffset + laserIndex;
        s.logical = calib.physicalToLogical(s.physical);
        VelodyneCalibration.Laser lasercal = calib.laser[s.physical];
        s.rawRange = ((data[blockOffset + 4 + laserIndex * 3] & 0xff) + ((data[blockOffset + 5 + laserIndex * 3] & 0xff) << 8)) * METERS_PER_LSB;
        s.range = (s.rawRange + lasercal.rangeOffset) * (1.0 + lasercal.rangeScaleOffset);
        s.ctheta = ctheta;
        s.theta = MathUtil.mod2pi(Math.PI, ctheta + lasercal.rcf);
        s.phi = lasercal.vcf;
        s.intensity = (data[blockOffset + 6 + laserIndex * 3] & 0xff) / 255.0;
        double sin_theta, cos_theta;
        sin_theta = Math.sin(s.theta);
        cos_theta = Math.cos(s.theta);
        double sin_phi = calib.sincos[s.physical][0];
        double cos_phi = calib.sincos[s.physical][1];
        s.xyz[0] = s.range * cos_theta * cos_phi;
        s.xyz[1] = s.range * sin_theta * cos_phi;
        s.xyz[2] = s.range * sin_phi;
        // handle horizontal offset ("parallax")
        s.xyz[0] -= lasercal.hcf * cos_ctheta;
        s.xyz[1] -= lasercal.hcf * sin_ctheta;
        laserIndex++;
        return true;
    }
}
