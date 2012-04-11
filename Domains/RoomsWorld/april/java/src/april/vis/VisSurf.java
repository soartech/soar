package april.vis;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;

import java.util.*;

import april.jmat.geom.*;

/** see the matlab 'surf' command **/
public class VisSurf implements VisObject
{
    double xs[], ys[], zs[][];
    ColorMapper cm;

    /** xs is an array of x coordinates.
        ys is an array of y coordinates.
        zs[i][j] is the value of the function at x = xs[i] and y = ys[j]
    **/
    public VisSurf(double xs[], double ys[], double zs[][], ColorMapper cm)
    {
        this.xs = xs;
        this.ys = ys;
        this.zs = zs;
        this.cm = cm;
    }

    void doVertex(GL gl, int ix, int iy)
    {
        double z = zs[ix][iy];

        VisUtil.setColor(gl, cm.mapColor(z));
        gl.glVertex3d(xs[ix], ys[iy], z);
    }

    void doVertex2(GL gl, int ix, int iy)
    {
        double z = zs[ix][iy];

        gl.glVertex3d(xs[ix], ys[iy], z);
    }


    public void render(VisContext vc, GL gl, GLU glu)
    {
        if (true) {
            gl.glBegin(GL.GL_LINES);

            gl.glColor3d(0,0,0);

            for (int ix = 0; ix+1 < xs.length; ix++) {
                for (int iy = 0; iy+1 < ys.length; iy++) {
                    doVertex2(gl, ix, iy);
                    doVertex2(gl, ix, iy+1);
                    doVertex2(gl, ix+1, iy+1);
                    doVertex2(gl, ix+1, iy);
                    doVertex2(gl, ix, iy+1);
                    doVertex2(gl, ix+1, iy+1);
                    doVertex2(gl, ix, iy);
                    doVertex2(gl, ix+1, iy);
                }
            }
            gl.glEnd();
        }

        gl.glBegin(GL.GL_QUADS);

        for (int ix = 0; ix+1 < xs.length; ix++) {
            for (int iy = 0; iy+1 < ys.length; iy++) {
                doVertex(gl, ix, iy);
                doVertex(gl, ix, iy+1);
                doVertex(gl, ix+1, iy+1);
                doVertex(gl, ix+1, iy);
            }
        }

        gl.glEnd();

        if (true) {
            gl.glBegin(GL.GL_LINES);

            gl.glColor3d(0,0,0);

            for (int ix = 0; ix+1 < xs.length; ix++) {
                for (int iy = 0; iy+1 < ys.length; iy++) {
                    doVertex2(gl, ix, iy);
                    doVertex2(gl, ix, iy+1);
                    doVertex2(gl, ix+1, iy+1);
                    doVertex2(gl, ix+1, iy);
                    doVertex2(gl, ix, iy+1);
                    doVertex2(gl, ix+1, iy+1);
                    doVertex2(gl, ix, iy);
                    doVertex2(gl, ix+1, iy);
                }
            }
            gl.glEnd();
        }
    }
}


