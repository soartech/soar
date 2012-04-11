package april.vis;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import java.awt.*;
import java.awt.image.*;
import java.nio.*;
import javax.swing.*;
import java.util.*;

import april.jmat.*;
import april.jmat.geom.*;

import lcm.lcm.*;
import java.io.*;

/** Draw cylinders, cones, and related types. This class needs to be
 * significantly optimized and improved. **/
public class VisCylinder implements VisObject, VisSerializable
{
    Color c;
    double r0, r1, h0, h1; // r0 = radius at base, r1 = radius at apex. h0 = height (z) at base, h1 = height at apex.

    public boolean drawTop = true;
    public boolean drawBottom = true;

    static ArrayList<double[]> xys;
    static {
        xys = new ArrayList<double[]>();
        int npoints = 50;
        for (int i = 0; i < npoints; i++) {
            double theta = i*2.0*Math.PI / npoints;
            xys.add(new double[] { Math.cos(theta), Math.sin(theta) });
        }
    }

    public VisCylinder(double r, double h, VisDataFillStyle style)
    {
        this(r,r,-h/2,h/2,style.c);
    }

    public VisCylinder(double r, double h, Color c)
    {
        this(r,r,-h/2,h/2,c);
    }

    /** A cylinder or cone whose base has radius r0 and is at height
     * h0. The top has radius r1 and is at height h1.
     **/
    public VisCylinder(double r0, double r1, double h0, double h1, Color c)
    {
        this.r0 = r0;
        this.r1 = r1;
        this.h0 = h0;
        this.h1 = h1;
        this.c = c;
    }

    public void render(VisContext vc, GL gl, GLU glu)
    {
        VisUtil.pushGLState(gl);

        VisUtil.setColor(gl, c);

        gl.glBegin(GL.GL_QUAD_STRIP);

        for (int i = 0; i <= xys.size(); i++) {
            double xya[] = xys.get(i % xys.size());

            gl.glNormal3d(xya[0], xya[1], 0); // this normal isn't quite right for cones.
            gl.glVertex3d(xya[0]*r1, xya[1]*r1, h1);
            gl.glVertex3d(xya[0]*r0, xya[1]*r0, h0);

        }
        gl.glEnd();

        if (drawTop) {
            gl.glBegin(GL.GL_TRIANGLE_FAN);
            gl.glNormal3d(0, 0, 1);
            for (int i = 0; i <= xys.size(); i++) {
                double xy[] = xys.get(i % xys.size());
                gl.glVertex3d(r1*xy[0], r1*xy[1], h1);
            }
            gl.glEnd();
        }

        if (drawBottom) {
            gl.glBegin(GL.GL_TRIANGLE_FAN);
            gl.glNormal3d(0, 0, 1);
            for (int i = xys.size()-1; i >= 0; i--) {
                double xy[] = xys.get(i % xys.size());
                gl.glVertex3d(r0*xy[0], r0*xy[1], h0);
            }
            gl.glEnd();
        }

        VisUtil.popGLState(gl);
    }

    // Serialization
    public VisCylinder()
    {
    }

    public void serialize(LCMDataOutputStream out) throws IOException
    {
        out.writeInt(c.getRGB());
        out.writeDouble(r0);
        out.writeDouble(r1);
        out.writeDouble(h0);
        out.writeDouble(h1);
        out.writeBoolean(drawTop);
        out.writeBoolean(drawBottom);
    }

    public void unserialize(LCMDataInputStream in) throws IOException
    {
        c = new Color(in.readInt(), true);
        r0 = in.readDouble();
        r1 = in.readDouble();
        h0 = in.readDouble();
        h1 = in.readDouble();
        drawTop = in.readBoolean();
        drawBottom =in.readBoolean();
    }

    public static void main(String args[])
    {
        JFrame jf = new JFrame("VisCylinder");
        VisWorld vw = new VisWorld();
        VisCanvas vc = new VisCanvas(vw);
        jf.setLayout(new BorderLayout());
        jf.add(vc, BorderLayout.CENTER);
        jf.setSize(800,600);
        jf.setVisible(true);

        VisWorld.Buffer vb = vw.getBuffer("cylinder");
        vb.addBuffered(new VisCylinder(5, 8, Color.blue));
        vb.switchBuffer();
    }
}
