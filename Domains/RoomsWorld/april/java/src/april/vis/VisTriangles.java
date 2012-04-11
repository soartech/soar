package april.vis;

import java.awt.*;
import java.util.*;
import javax.media.opengl.*;
import javax.media.opengl.glu.*;

import april.jmat.*;
import april.jmat.geom.*;

public class VisTriangles implements VisObject
{
    Colorizer colorizer;
    ArrayList<double[]> points;
    ArrayList<int[]> triangles;
    ArrayList<double[]> vertex_normals;
    ArrayList<double[]> triangle_normals;

    public enum NORMAL_MODE { VERTEX_AVERAGE, TRIANGLE };
    public NORMAL_MODE normalMode = NORMAL_MODE.VERTEX_AVERAGE;

    public VisTriangles(Colorizer colorizer, ArrayList<double[]> points, ArrayList<int[]> triangles)
    {
        this(colorizer, points, triangles, NORMAL_MODE.VERTEX_AVERAGE);
    }

    /** Triangles: each triangle is a list of three vertices, indices
     * into p. Points in triangle should be in CCW order for normals
     * to work properly. **/
    public VisTriangles(Colorizer colorizer, ArrayList<double[]> points, ArrayList<int[]> triangles, NORMAL_MODE normalMode)
    {
        this.colorizer = colorizer;
        this.points = points;
        this.triangles = triangles;
        this.normalMode = normalMode;

        // compute normal of each triangle.
        triangle_normals = new ArrayList<double[]>(triangles.size());

        for (int tidx = 0; tidx < triangles.size(); tidx++) {
            int t[] = triangles.get(tidx);

            double p0[] = points.get(t[0]);
            double p1[] = points.get(t[1]);
            double p2[] = points.get(t[2]);
            double n[] = LinAlg.normalize(LinAlg.crossProduct(LinAlg.subtract(p1, p0), LinAlg.subtract(p2, p0)));

            triangle_normals.add(n);
        }

        // if we're using vertex average, recompute the average normal
        // for every vertex using the normals we just computed for
        // each triangle.
        if (normalMode == NORMAL_MODE.VERTEX_AVERAGE) {

            vertex_normals = new ArrayList<double[]>(points.size());
            for (int i = 0; i < points.size(); i++) {
                vertex_normals.add(new double[3]);
            }

            for (int tidx = 0; tidx < triangles.size(); tidx++) {
                int t[] = triangles.get(tidx);

                double n[] = triangle_normals.get(tidx);

                for (int i = 0; i < 3; i++) {
                    double vn[] = vertex_normals.get(t[i]);

                    for (int j = 0; j < 3; j++)
                        vn[j] += n[j];
                }

                for (int i = 0; i < points.size(); i++)
                    LinAlg.normalizeEquals(vertex_normals.get(i));
            }
        }
    }

    public void render(VisContext vc, GL gl, GLU glu)
    {
        gl.glBegin(GL.GL_TRIANGLES);

        for (int tidx = 0; tidx < triangles.size(); tidx++) {
            int t[] = triangles.get(tidx);

            if (normalMode == NORMAL_MODE.TRIANGLE)
                gl.glNormal3dv(triangle_normals.get(tidx), 0);

            for (int i = 0; i < 3; i++) {
                double p[] = points.get(t[i]);

                if (normalMode == NORMAL_MODE.VERTEX_AVERAGE)
                    gl.glNormal3dv(vertex_normals.get(t[i]), 0);

                VisUtil.setColor(gl, colorizer.colorize(p));
                gl.glVertex3dv(p, 0);
            }
        }

        gl.glEnd();
    }
}
