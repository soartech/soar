package april.vis;

import java.awt.*;
import java.util.*;
import java.io.*;
import java.nio.*;
import javax.media.opengl.*;
import javax.media.opengl.glu.*;

import april.jmat.geom.*;

import lcm.lcm.*;

/** Render VisData as a connected line. **/
public class VisDataLineStyle implements VisDataStyle, VisSerializable
{
    Color c;
    float width;
    boolean loop;

    public VisDataLineStyle(Color c, double width, boolean loop)
    {
        this.c = c;
        this.width = (float) width;
        this.loop = loop;
    }

    public VisDataLineStyle(Color c, double width)
    {
        this(c, width, false);
    }

    public void renderStyle(VisContext vc, GL gl, GLU glu, VisData vdata)
    {
        ArrayList<double[]> points = vdata.points;
        if (points.size() == 0)
            return;

        VisUtil.setColor(gl, c);
        gl.glLineWidth(width);

        if (true) {
            DoubleBuffer vertexbuf = vdata.getVertexBuffer();
            gl.glEnableClientState(GL.GL_VERTEX_ARRAY);
            gl.glVertexPointer(3, GL.GL_DOUBLE, 0, vertexbuf.rewind());
            gl.glDrawArrays(loop ? GL.GL_LINE_LOOP : GL.GL_LINE_STRIP, 0, points.size());
            gl.glDisableClientState(GL.GL_VERTEX_ARRAY);
        } else {
            gl.glBegin( loop ? gl.GL_LINE_LOOP : gl.GL_LINE_STRIP );

            for (double p[] : points) {
                if (p.length >= 3)
                    gl.glVertex3dv(p, 0);
                else
                    gl.glVertex2dv(p, 0);
            }

            gl.glEnd();
        }
    }


    public VisDataLineStyle()
    {
    }

    public void serialize(LCMDataOutputStream out) throws IOException
    {
        out.writeInt(c.getRGB());
        out.writeFloat(width);
        out.writeBoolean(loop);
    }

    public void unserialize(LCMDataInputStream in) throws IOException
    {
        c = new Color(in.readInt(), true);
        width = in.readFloat();
        loop = in.readBoolean();
    }
}
