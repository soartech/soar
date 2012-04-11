package april.vis;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;

import java.util.*;

public class VisMesh implements VisObject
{
    double x0, y0, dx, dy;
    double data[];
    int width, height;
    Colorizer c;

    public VisMesh(double x0, double y0, double dx, double dy, int width, int height, double data[], Colorizer c)
    {
        this.x0 = x0;
        this.y0 = y0;
        this.dx = dx;
        this.dy = dy;
        this.width = width;
        this.height = height;
        this.data = data;
        this.c = c;
    }

    void doVertex(GL gl, int ix, int iy)
    {
        double tmp[] = new double[3];
        tmp[0] = x0 + ix*dx;
        tmp[1] = y0 + iy*dy;
        tmp[2] = data[iy*width+ix];
        VisUtil.setColor(gl, c.colorize(tmp));
        gl.glVertex3d(tmp[0], tmp[1], tmp[2]);
    }

    public void render(VisContext vc, GL gl, GLU glu)
    {
        gl.glBegin(GL.GL_QUADS);

        for (int iy = 0; iy+1 < height; iy++) {
            for (int ix = 0; ix+1 < width; ix++) {
                doVertex(gl, ix,iy);
                doVertex(gl, ix+1,iy);
                doVertex(gl, ix+1,iy+1);
                doVertex(gl, ix,iy+1);
            }
        }

        gl.glEnd();
    }
}
