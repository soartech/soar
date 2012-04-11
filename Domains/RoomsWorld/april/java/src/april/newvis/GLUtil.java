package april.newvis;

import javax.media.opengl.*;

public class GLUtil
{
    /** a 1x1x1 cube. **/
    public static void cubeFilled(GL gl)
    {
        gl.glBegin(GL.GL_QUADS);

        gl.glNormal3d (0, 0, -1);
        gl.glVertex3d (-0.5,  0.5, -0.5);
        gl.glVertex3d ( 0.5,  0.5, -0.5);
        gl.glVertex3d ( 0.5, -0.5, -0.5);
        gl.glVertex3d (-0.5, -0.5, -0.5);

        gl.glNormal3d (0, 1, 0);
        gl.glVertex3d (-0.5, -0.5, -0.5);
        gl.glVertex3d ( 0.5, -0.5, -0.5);
        gl.glVertex3d ( 0.5, -0.5,  0.5);
        gl.glVertex3d (-0.5, -0.5,  0.5);

        gl.glNormal3d (1, 0, 0);
        gl.glVertex3d (-0.5, -0.5,  0.5);
        gl.glVertex3d (-0.5,  0.5,  0.5);
        gl.glVertex3d (-0.5,  0.5, -0.5);
        gl.glVertex3d (-0.5, -0.5, -0.5);

        gl.glNormal3d (1, 0, 0);
        gl.glVertex3d ( 0.5,  0.5,  0.5);
        gl.glVertex3d ( 0.5, -0.5,  0.5);
        gl.glVertex3d ( 0.5, -0.5, -0.5);
        gl.glVertex3d ( 0.5,  0.5, -0.5);

        gl.glNormal3d (0, 1, 0);
        gl.glVertex3d ( 0.5,  0.5, -0.5);
        gl.glVertex3d (-0.5,  0.5, -0.5);
        gl.glVertex3d (-0.5,  0.5,  0.5);
        gl.glVertex3d ( 0.5,  0.5,  0.5);

        gl.glNormal3d (0, 0, 1);
        gl.glVertex3d (-0.5, -0.5,  0.5);
        gl.glVertex3d ( 0.5, -0.5,  0.5);
        gl.glVertex3d ( 0.5,  0.5,  0.5);
        gl.glVertex3d (-0.5,  0.5,  0.5);
        gl.glEnd ();
    }

    /** a 1x1x1 cube. **/
    public static void cubeLines(GL gl)
    {
        gl.glBegin (GL.GL_LINE_LOOP);
        gl.glVertex3d (-0.5, -0.5, -0.5);
        gl.glVertex3d (0.5, -0.5, -0.5);
        gl.glVertex3d (0.5, 0.5, -0.5);
        gl.glVertex3d (-0.5, 0.5, -0.5);
        gl.glEnd ();
        gl.glBegin (GL.GL_LINE_LOOP);
        gl.glVertex3d (-0.5, -0.5, 0.5);
        gl.glVertex3d (0.5, -0.5, 0.5);
        gl.glVertex3d (0.5, 0.5, 0.5);
        gl.glVertex3d (-0.5, 0.5, 0.5);
        gl.glEnd ();
        gl.glBegin (GL.GL_LINES);
        gl.glVertex3d (-0.5, -0.5, -0.5);
        gl.glVertex3d (-0.5, -0.5, 0.5);
        gl.glVertex3d (0.5, -0.5, -0.5);
        gl.glVertex3d (0.5, -0.5, 0.5);
        gl.glVertex3d (0.5, 0.5, -0.5);
        gl.glVertex3d (0.5, 0.5, 0.5);
        gl.glVertex3d (-0.5, 0.5, -0.5);
        gl.glVertex3d (-0.5, 0.5, 0.5);
        gl.glEnd ();
    }

    /** A pyramid with a 1x1 base in the XY plane reaching up to an apex at (0,0,1) **/
    public static void pyramidFilled(GL gl, boolean bottom)
    {
        double sqrt2 = 0.7071067812;

        gl.glBegin(GL.GL_TRIANGLES);
        gl.glNormal3d(sqrt2, 0, sqrt2);
        gl.glVertex3d(.5,-.5, 0);
        gl.glVertex3d(.5, .5, 0);
        gl.glVertex3d(0, 0, 1);

        gl.glNormal3d(0, sqrt2, sqrt2);
        gl.glVertex3d(.5, .5, 0);
        gl.glVertex3d(-.5,.5, 0);
        gl.glVertex3d(0, 0, 1);

        gl.glNormal3d(-sqrt2, 0, sqrt2);
        gl.glVertex3d(-.5, .5, 0);
        gl.glVertex3d(-.5,-.5, 0);
        gl.glVertex3d(0, 0, 1);

        gl.glNormal3d(0, -sqrt2, sqrt2);
        gl.glVertex3d(-.5, -.5, 0);
        gl.glVertex3d(.5,-.5, 0);
        gl.glVertex3d(0, 0, 1);
        gl.glEnd();

        if (bottom) {
            gl.glBegin(GL.GL_QUADS);
            gl.glVertex3d(-.5, -.5, 0);
            gl.glVertex3d(-.5, .5, 0);
            gl.glVertex3d(.5, .5, 0);
            gl.glVertex3d(.5, -.5, 0);
            gl.glEnd();
        }
    }
}
