package april.vis;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;

import april.jmat.geom.*;

import java.util.*;

/** VisObject wrapper that manipulates the polygon mode for the rear side .

 e.g., mode = GL.GL_LINE
**/
public class VisBackPolygonMode implements VisObject
{
    VisObject os[];
    boolean enable;
    int mode;

    public VisBackPolygonMode(int mode, VisObject ... os)
    {
        this.enable = enable;
        this.mode = mode;
        this.os = os;
    }

    public void render(VisContext vc, GL gl, GLU glu)
    {
        gl.glPolygonMode(GL.GL_BACK, mode);

        for (VisObject vo : os)
            vo.render(vc, gl, glu);

        gl.glPolygonMode(GL.GL_BACK, GL.GL_FILL);
    }
}
