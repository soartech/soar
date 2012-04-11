package april.vis;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import java.awt.*;
import april.jmat.geom.*;

import java.util.*;
import java.io.*;

import lcm.lcm.*;
/** VisObject that draws a star, inscribed in a 1.0 meter circle.**/
public class VisStar implements VisObject, VisSerializable
{
    //    Color  c = new Color(220, 220, 0);
    Color c;
    Color outlineColor = new Color(100, 100, 0);

    int  selectedState; // 0 = none, 1 = hover, 2 = select

    double vertices[][];
    double size = 1.0;
    double ratio = 0.4;

    public VisStar()
    {
        this(new Color(255,255,0), 5);
    }

    public VisStar(Color c)
    {
        this(c, 5);
    }

    public VisStar(Color c, int npoints)
    {
        this.c = c;
        vertices = new double[npoints*2][2];

        for (int i = 0; i < vertices.length; i++) {
            double r = (i&1)==0 ? size : size*ratio;
            double theta = 2*Math.PI * i / (vertices.length );

            vertices[i][0] = Math.cos(theta)*r;
            vertices[i][1] = Math.sin(theta)*r;
        }
    }

    public void render(VisContext vc, GL gl, GLU glu)
    {
        gl.glLineWidth(1f);

        VisUtil.setColor(gl, c);
        gl.glBegin(gl.GL_TRIANGLE_FAN);
        gl.glVertex2d(0, 0);
        for (int i = 0; i < vertices.length; i++) {
            gl.glVertex2d(vertices[i][0], vertices[i][1]);
        }
        gl.glVertex2d(vertices[0][0], vertices[0][1]);
        gl.glEnd();

        VisUtil.setColor(gl, outlineColor);
        gl.glBegin(gl.GL_LINE_LOOP);
        for (int i = 0; i < vertices.length; i++) {
            gl.glVertex2d(vertices[i][0], vertices[i][1]);
        }
        gl.glEnd();
    }

    public void serialize(LCMDataOutputStream out) throws IOException
    {
        out.writeInt(c.getRGB());
        out.writeInt(outlineColor.getRGB());
        out.writeInt(selectedState);

        out.writeInt(vertices.length);
        for(double point[] : vertices) {
            out.writeDouble(point[0]);
            out.writeDouble(point[1]);
        }
        out.writeDouble(size);
        out.writeDouble(ratio);
    }

    public void unserialize(LCMDataInputStream in) throws IOException
    {
        c = new Color(in.readInt(), true);
        outlineColor = new Color(in.readInt(), true);
        selectedState = in.readInt();

        vertices = new double[in.readInt()][2];
        for(double point[] : vertices) {
            point[0] = in.readDouble();
            point[1] = in.readDouble();
        }
        size = in.readDouble();
        ratio = in.readDouble();
    }

}
