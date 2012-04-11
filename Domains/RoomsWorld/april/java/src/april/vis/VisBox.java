package april.vis;

import java.awt.*;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;

import april.jmat.geom.*;

import java.util.*;
import java.io.*;

import lcm.lcm.*;

/** A simple 3D box. **/
public class VisBox implements VisObject, VisSerializable
{
    double cx, cy, cz;
    double sizex, sizey, sizez;
    Color  fillcolor;
    Color  linecolor;

    public VisBox()
    {
         // For Serializable
    }

    public VisBox(double sizex, double sizey, double sizez, Color fillcolor)
    {
        this(0, 0, 0, sizex, sizey, sizez, fillcolor, null);
    }

    public VisBox(double sizex, double sizey, double sizez, VisDataFillStyle style)
    {
        this(0, 0, 0, sizex, sizey, sizez, style.c, null);
    }

    public VisBox(double cx, double cy, double cz, double sizex, double sizey, double sizez, Color fillcolor)
    {
        this(cx, cy, cz, sizex, sizey, sizez, fillcolor, null);
    }

    public VisBox(double cx, double cy, double cz, double sizex, double sizey, double sizez, Color fillcolor, Color linecolor)
    {
        this.cx = cx;
        this.cy = cy;
        this.cz = cz;
        this.sizex = sizex;
        this.sizey = sizey;
        this.sizez = sizez;
        this.fillcolor = fillcolor;
        this.linecolor = linecolor;
    }

    public void serialize(LCMDataOutputStream out) throws IOException
    {
        out.writeDouble(cx);
        out.writeDouble(cy);
        out.writeDouble(cz);

        out.writeDouble(sizex);
        out.writeDouble(sizey);
        out.writeDouble(sizez);

        int fill = 0;
        if (fillcolor != null)
            fill = fillcolor.getRGB();

        int line = 0;
        if (linecolor != null)
            line = linecolor.getRGB();

        out.writeBoolean(fillcolor != null);
        out.writeInt(fill); // recreate with new Color(int, true);

        out.writeBoolean(linecolor != null);
        out.writeInt(line); // recreate with new Color(int, true);
    }

    public void unserialize(LCMDataInputStream in) throws IOException
    {
        cx = in.readDouble();
        cy = in.readDouble();
        cz = in.readDouble();

        sizex = in.readDouble();
        sizey = in.readDouble();
        sizez = in.readDouble();

        boolean fill_valid = in.readBoolean();
        int fill = in.readInt();

        boolean line_valid = in.readBoolean();
        int line = in.readInt();


        if (fill_valid)
            fillcolor = new Color(fill,true);

        if (line_valid)
            linecolor = new Color(line,true);
    }

    public void render(VisContext vc, GL gl, GLU glu)
    {
        gl.glPushMatrix();

        gl.glTranslated(cx, cy, cz);
        gl.glScaled(sizex, sizey, sizez);

        if (fillcolor != null) {
            VisUtil.setColor(gl, fillcolor);
            GLUtil.cubeFilled(gl);
        }

        if (linecolor != null) {
            VisUtil.setColor(gl, linecolor);
            GLUtil.cubeLines(gl);
        }

        gl.glPopMatrix();
    }
}
