package april.vis;

import java.awt.*;
import lcm.lcm.*;
import java.io.*;
/** Converts scalar values to RGB colors by interpolating from a
 * user-provided look-up table. Implements a colorizer by looking at
 * only the z component. **/
public class ColorMapper implements Colorizer, VisSerializable
{
    /** Minimum/maximum value for mapped range (will be drawn opaquely). **/
    double minval;
    double maxval;
    int[] colors;

    /** If these bounds are exceeded, use transparent color **/
    double opaqueMax = Double.MAX_VALUE, opaqueMin=-Double.MAX_VALUE;

    public ColorMapper(int[] colors, double minval, double maxval)
    {
        this.colors=colors;
        this.minval=minval;
        this.maxval=maxval;
    }

    public void setMinMax(double minval, double maxval)
    {
        this.minval = minval;
        this.maxval = maxval;
    }

    public void setOpaqueMax(double opaqueMax)
    {
        this.opaqueMax = opaqueMax;
    }

    public void setOpaqueMin(double opaqueMin)
    {
        this.opaqueMin = opaqueMin;
    }

    public boolean isVisible(double v)
    {
        if (v > opaqueMax || v < opaqueMin)
            return false;
        return true;
    }

    public static ColorMapper makeGray(double min, double max) {
        return new ColorMapper(new int[] {0x000000,
                                          0xffffff},
            min,
            max);
    }

    public static ColorMapper makeJet(double min, double max) {

        return new ColorMapper(new int[] {0x000000,
                                          0x0000ff,
                                          0x008080,
                                          0x00ff00,
                                          0x808000,
                                          0xff0000},
            min,
            max);
    }

    public static ColorMapper makeJetWhite(double min, double max) {

        return new ColorMapper(new int[] {0xffffff,
                                          0x0000ff,
                                          0x008080,
                                          0x00ff00,
                                          0x808000,
                                          0xff0000,
                                          0xffff00},
            min,
            max);
    }

    public Color mapColor(double vin)
    {
        int v = map(vin);
        return new Color((v>>16)&0xff, (v>>8)&0xff, (v>>0)&0xff);
    }

    public int colorize(double p[])
    {
        return map(p[2]);
    }

    public int map(double v)
    {
        if (!isVisible(v))
            return 0x00000000; // transparent

        double normval = (colors.length)*(v-minval)/(maxval-minval);

        int a = (int) (normval);
        if (a<0)
            a=0;
        if (a>=colors.length)
            a=colors.length-1;

        int b = a + 1;
        if (b>=colors.length)
            b=colors.length-1;

        double frac = normval - a;
        if (frac<0)
            frac=0;
        if (frac>1)
            frac=1;

        int c=0;
        for (int i=0;i<4;i++)
	    {
            int r =i*8;
            int comp = (int) (((colors[a]>>r)&0xff)*(1-frac) + ((colors[b]>>r)&0xff)*frac);
            comp = comp & 0xff;
            c |= (comp<<r);
	    }

        // force opacity
        return c | 0xff000000;
    }

    // Serialization
    public ColorMapper()
    {
    }

    public void serialize(LCMDataOutputStream out) throws IOException
    {
        out.writeDouble(minval);
        out.writeDouble(maxval);
        out.writeDouble(opaqueMin);
        out.writeDouble(opaqueMax);

        out.writeInt(colors.length);
        for (int c : colors)
            out.writeInt(c);

    }

    public void unserialize(LCMDataInputStream in) throws IOException
    {
        minval = in.readDouble();
        maxval = in.readDouble();
        opaqueMin = in.readDouble();
        opaqueMax = in.readDouble();


        colors = new int[in.readInt()];
        for( int i =0; i < colors.length; i++)
            colors[i] = in.readInt();
    }

}
