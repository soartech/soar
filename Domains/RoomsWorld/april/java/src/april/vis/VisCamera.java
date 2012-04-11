package april.vis;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import java.awt.*;
import april.jmat.geom.*;

import java.util.*;

/** VisObject representing a camera. In the camera's coordinate frame,
 * the camera will appear to be looking down the -z axis, with y
 * up.
 **/
public class VisCamera implements VisObject
{
    Color color = new Color(128, 128, 128);

    public VisCamera()
    {
    }

    public VisCamera(Color color)
    {
        this.color = color;
    }

    public void render(VisContext vc, GL gl, GLU glu)
    {
        //	GLUtil.cubeFilled(gl);

        //	gl.glRotated(180, 0, 1, 0);
        double s = 0.25;

        VisUtil.setColor(gl, color);

        gl.glPushMatrix();
        gl.glScaled(s, s, s);
        gl.glTranslated(0, 0, -1.0);
        GLUtil.pyramidFilled(gl, false);
        gl.glPopMatrix();

        gl.glPushMatrix();
        gl.glScaled(s, s, s);
        gl.glTranslated(0, 0, 0.25);
        GLUtil.cubeFilled(gl);
        gl.glPopMatrix();

        gl.glBegin(GL.GL_LINES);
        gl.glVertex3d(0, -.2, 0);
        gl.glVertex3d(0, .4, 0);

        for (int i = 0; i < 3; i++) {
            gl.glVertex3d(0, .4, 0);
            double theta = i*2.0*Math.PI/3;
            gl.glVertex3d(.05*Math.sin(theta), .3, .05*Math.cos(theta));
        }

        gl.glEnd();

    }
}
