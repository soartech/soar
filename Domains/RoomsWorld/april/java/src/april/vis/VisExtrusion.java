package april.vis;

import java.awt.*;
import java.util.*;
import april.jmat.*;
import april.jmat.geom.*;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;

/** Take a closed 2D shape and extrude it, forming a 3D shape. **/
public class VisExtrusion implements VisObject
{
    ArrayList<double[]> points;
    double depth;
    Color c;
    april.jmat.geom.Polygon p;

    public VisExtrusion(ArrayList<double[]> points, double depth, Color c)
    {
        this.points = points;
        this.depth = depth;
        this.c = c;

        p = new april.jmat.geom.Polygon(points);
    }

    void renderFace(GL gl, double z)
    {
        gl.glBegin(GL.GL_TRIANGLES);

        for (int triangle[] : p.getTriangles()) {
            for (int i = 0; i < 3; i++) {
                double vertex[] = p.getPoint(triangle[i]);

                gl.glVertex3d(vertex[0], vertex[1], z);
            }
        }

        gl.glEnd();
    }

    public void render(VisContext vc, GL gl, GLU glu)
    {
        VisUtil.setColor(gl, c);

        gl.glNormal3d(0, 0, 1);
        renderFace(gl, depth/2);

        gl.glNormal3d(0, 0, -1);
        renderFace(gl, -depth/2);

        gl.glBegin(GL.GL_QUADS);
        for (int i = 0; i < points.size(); i++) {
            double p0[] = points.get(i);
            double p1[] = points.get((i+1)%points.size());

            double cp[] = LinAlg.crossProduct(new double[] { (p1[0]-p0[0]), (p1[1]-p0[1]), 0 },
                                              new double[] { 0, 0, 1 });
            gl.glNormal3d(cp[0], cp[1], cp[2]);
            gl.glVertex3d(p0[0], p0[1], depth/2);
            gl.glVertex3d(p1[0], p1[1], depth/2);
            gl.glVertex3d(p1[0], p1[1], -depth/2);
            gl.glVertex3d(p0[0], p0[1], -depth/2);
        }
        gl.glEnd();
    }
}
