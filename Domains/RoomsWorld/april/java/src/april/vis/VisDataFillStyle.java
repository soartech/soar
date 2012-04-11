package april.vis;

import java.awt.*;
import java.util.*;
import javax.media.opengl.*;
import javax.media.opengl.glu.*;

import april.jmat.*;
import april.jmat.geom.*;

import java.io.*;
import lcm.lcm.*;

/** Treat VisData points as a polygon and fills the interior. **/
public class VisDataFillStyle implements VisDataStyle, VisSerializable
{
    Color c;
    static boolean warned = false;
    Colorizer colorizer;
    float shininess = 0;;

    public VisDataFillStyle(Colorizer colorizer)
    {
        this.colorizer = colorizer;
    }

    public VisDataFillStyle(Color c)
    {
        this.c = c;
    }

    public VisDataFillStyle(Color c, float shininess)
    {
        this.c = c;
        this.shininess = shininess;
    }

    public void renderStyle(VisContext vc, GL gl, GLU glu, VisData vdata)
    {
        ArrayList<double[]> points = vdata.points;

        if (c != null)
            VisUtil.setColor(gl, c);

        april.jmat.geom.Polygon p = new april.jmat.geom.Polygon(points);

        gl.glMaterialf(GL.GL_FRONT_AND_BACK, GL.GL_SHININESS, shininess);
        gl.glBegin(GL.GL_TRIANGLES);

        for (int triangle[] : p.getTriangles()) {
            for (int i = 0; i < 3; i++) {
                double vertex[] = p.getPoint(triangle[i]);

                if (colorizer != null)
                    VisUtil.setColor(gl, colorizer.colorize(vertex));

                if (vertex.length == 2)
                    gl.glVertex2dv(vertex, 0);
                else
                    gl.glVertex3dv(vertex, 0);
            }
        }

        gl.glEnd();

        gl.glMaterialf(GL.GL_FRONT_AND_BACK, GL.GL_SHININESS, 0);
    }

    public VisDataFillStyle()
    {
    }

    public void serialize(LCMDataOutputStream out) throws IOException
    {
        out.writeFloat(shininess);

        int color = 0;
        if (c != null)
            color = c.getRGB();

        out.writeBoolean(c != null);
        out.writeInt(color);

        out.writeBoolean(colorizer != null && colorizer instanceof VisSerializable);

        if (c == null &&  ! (colorizer instanceof VisSerializable))
            System.out.println("WRN: Your colorizer "+colorizer.getClass().getName()+
                               " is not serializable, this log will not display correctly.");
        if (colorizer != null && colorizer instanceof VisSerializable)
            VisSerialize.serialize((VisSerializable)colorizer, out);

    }

    public void unserialize(LCMDataInputStream in) throws IOException
    {
        shininess = in.readFloat();
        if (in.readBoolean())
            c = new Color(in.readInt(), true);
        else
            in.readInt(); //burn the 0

        if (in.readBoolean())
            colorizer = (Colorizer)VisSerialize.unserialize(in);
    }

}

