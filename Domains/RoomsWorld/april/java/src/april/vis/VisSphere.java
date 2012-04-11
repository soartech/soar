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

/** This class needs to be significantly optimized and improved. **/
public class VisSphere implements VisObject
{
    Color c;

    static ArrayList<Slice> slices;

    double r;

    static final class Slice
    {
        ArrayList<double[]> points;
        ArrayList<double[]> normals;
    }

    public VisSphere(double r, VisDataFillStyle style)
    {
        this(r, style.c);
    }

    public VisSphere(double r, Color c)
    {
        this.r = r;
        this.c = c;

        int resolution = 30;

        if (slices == null) {
            // create slices for a unit sphere.
            int nslices = resolution;
            slices = new ArrayList<Slice>();

            for (int s = 0; s < nslices; s++) {

                double t = Math.PI*s/(nslices-1);
                double z = Math.cos(t);
                int npoints = resolution;

                double thisr = Math.sqrt(1 - z*z);

                Slice slice = new Slice();
                slice.points = new ArrayList<double[]>();
                slice.normals = new ArrayList<double[]>();
                slices.add(slice);

                for (int i = 0; i < npoints; i++) {
                    double theta = 2*Math.PI*i/npoints;
                    double x = thisr * Math.cos(theta);
                    double y = thisr * Math.sin(theta);

                    double xyz[] = new double[] {x, y, z};
                    slice.points.add(xyz);
                    slice.normals.add(LinAlg.normalize(xyz));
                }
            }
        }
    }

    public void render(VisContext vc, GL gl, GLU glu)
    {
        VisUtil.pushGLState(gl);

        VisUtil.setColor(gl, c);
        gl.glScaled(r, r, r);

        gl.glBegin(GL.GL_QUAD_STRIP);

        for (int s = 0; s+1 < slices.size(); s++) {
            Slice s0 = slices.get(s);
            Slice s1 = slices.get(s+1);

            for (int i = 0; i < s0.points.size(); i++) {
                gl.glNormal3dv(s0.normals.get(i), 0);
                gl.glVertex3dv(s0.points.get(i), 0);
                gl.glNormal3dv(s1.normals.get(i), 0);
                gl.glVertex3dv(s1.points.get(i), 0);
            }
        }

        gl.glEnd();
        VisUtil.popGLState(gl);
    }

    public static void main(String args[])
    {
        JFrame jf = new JFrame("VisSphere");
        VisWorld vw = new VisWorld();
        VisCanvas vc = new VisCanvas(vw);
        jf.setLayout(new BorderLayout());
        jf.add(vc, BorderLayout.CENTER);
        jf.setSize(800,600);
        jf.setVisible(true);

        VisWorld.Buffer vb = vw.getBuffer("sphere");
        vb.addBuffered(new VisSphere(5, Color.blue));
        vb.switchBuffer();
    }
}
