package april.vis;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;

import april.jmat.geom.*;

import java.util.*;

/** VisObject wrapper that shows objects only on a specific VisContext (i.e., canvas) **/
public class VisContextSpecific implements VisObject
{
    VisObject os[];
    VisContext vc;

    public VisContextSpecific(VisContext vc, VisObject ... os)
    {
        this.vc = vc;
        this.os = os;
    }

    public void render(VisContext vc, GL gl, GLU glu)
    {
        if (this.vc != vc)
            return;

        for (VisObject vo : os)
            vo.render(vc, gl, glu);
    }
}
