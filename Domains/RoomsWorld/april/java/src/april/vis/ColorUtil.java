package april.vis;

import java.awt.*;
import java.util.*;

/** Useful color utilities. **/
public class ColorUtil
{
    static Random r = new Random();

    public static Color seededColor(int seed)
    {
        Random r = new Random(seed);
        return new Color(r.nextInt(255), r.nextInt(255), r.nextInt(255));
    }

    public static Color randomColor()
    {
        return new Color(r.nextInt(255), r.nextInt(255), r.nextInt(255));
    }

    public static Color randomColor(int alpha)
    {
        return new Color(r.nextInt(255), r.nextInt(255), r.nextInt(255), alpha);
    }

    public static Color setAlpha(Color c, int a)
    {
        return new Color(c.getRed(), c.getGreen(), c.getBlue(), a);
    }

    public static Color invertColor(Color c)
    {
        return new Color(255- c.getRed(), 255-c.getGreen(), 255-c.getBlue());
    }

    public static Color fromAARRGGBB(int aarrggbb)
    {
        return new Color((aarrggbb >> 16) & 0xff,
                         (aarrggbb >> 8) & 0xff,
                         (aarrggbb >> 0) & 0xff,
                         (aarrggbb >> 24) & 0xff);
    }

    // make the color closer to 128,128,128.
    // if frac=0, no change. if frac=1, returns 128,128,128.
    public static Color towardsGray(Color c, double frac)
    {
        int er = c.getRed() - 128;
        int eg = c.getGreen() - 128;
        int eb = c.getBlue() - 128;
        return new Color(c.getRed() - ((int) (er * frac)),
                         c.getGreen() - ((int) (eg * frac)),
                         c.getBlue() - ((int) (eb * frac)));
    }

    /**
     * Converts a color to its String representation for VisText and
     * HTML in the #RRGGBBAA format
     * @param Color
     */
    public static String toHTMLString(Color c)
    {
        // Either one or two characters long
        String r = Integer.toHexString(c.getRed());
        String g = Integer.toHexString(c.getGreen());
        String b = Integer.toHexString(c.getBlue());
        String a = Integer.toHexString(c.getAlpha());

        if ( r.length() == 1)
            r = "0"+r;
        if ( g.length() == 1)
            g = "0"+g;
        if ( b.length() == 1)
            b = "0"+b;
        if ( a.length() == 1)
            a = "0"+a;

        return "#" + r + g + b + a;
    }

    /**
     * Converts a color to its String representation for VisText and
     * HTML in the #RRGGBBAA format
     * @param int color in aarrggbb format (as from getRGB())
     */
    public static String toHTMLString(int color)
    {
        // Either one or two characters long
        String r = Integer.toHexString((color >> 16) & 0xFF);
        String g = Integer.toHexString((color >> 8)  & 0xFF);
        String b = Integer.toHexString((color >> 0)  & 0xFF);
        String a = Integer.toHexString((color >> 24) & 0xFF);

        if ( r.length() == 1)
            r = "0"+r;
        if ( g.length() == 1)
            g = "0"+g;
        if ( b.length() == 1)
            b = "0"+b;
        if ( a.length() == 1)
            a = "0"+a;

        return "#" + r + g + b + a;
    }
}

