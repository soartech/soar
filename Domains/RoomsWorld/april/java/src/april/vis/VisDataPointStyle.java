package april.vis;

import java.awt.*;
import java.util.*;
import java.nio.*;
import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import com.sun.opengl.util.*;

import april.jmat.geom.*;

import lcm.lcm.*;
import java.io.*;

/** Render VisData as individual points. **/
public class VisDataPointStyle implements VisDataStyle, VisSerializable
{
    Colorizer colorizer;
    Color c;
    float size;

    public VisDataPointStyle(Colorizer colorizer, double size)
    {
        this.colorizer = colorizer;
        this.size = (float) size;
    }

    public VisDataPointStyle(Color c, double size)
    {
        this.c = c;
        this.size = (float) size;
    }

    public void renderStyle(VisContext vc, GL gl, GLU glu, VisData vdata)
    {
        synchronized (vdata) {
            ArrayList<double[]> points = vdata.points;

            if (points.size() == 0) // JOGL will die if we pass an empty buffer.
                return;

            if (c != null)
                VisUtil.setColor(gl, c);

            gl.glPointSize(size);

            DoubleBuffer vertexbuf = vdata.getVertexBuffer();

            IntBuffer colorbuf = null;

            gl.glEnableClientState(GL.GL_VERTEX_ARRAY);

            if (colorizer != null) {
                colorbuf = BufferUtil.newIntBuffer(points.size());
                for (int pidx = 0; pidx < points.size(); pidx++) {
                    colorbuf.put(colorizer.colorize(points.get(pidx)));
                }
                colorbuf.rewind();

                gl.glEnableClientState(GL.GL_COLOR_ARRAY);
                gl.glColorPointer(4, GL.GL_UNSIGNED_BYTE, 0, colorbuf);
            }

            gl.glVertexPointer(3, GL.GL_DOUBLE, 0, vertexbuf.rewind());
            gl.glDrawArrays(GL.GL_POINTS, 0, points.size());
            gl.glDisableClientState(GL.GL_VERTEX_ARRAY);

            if (colorbuf != null)
                gl.glDisableClientState(GL.GL_COLOR_ARRAY);
        }
    }

    public VisDataPointStyle()
    {
    }

    public void serialize(LCMDataOutputStream out) throws IOException
    {
        out.writeFloat(size);

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
        size = in.readFloat();
        if (in.readBoolean())
            c = new Color(in.readInt(), true);
        else
            in.readInt(); //burn the 0

        if (in.readBoolean())
            colorizer = (Colorizer)VisSerialize.unserialize(in);
    }
}
