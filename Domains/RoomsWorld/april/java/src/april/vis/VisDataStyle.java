package april.vis;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;

import april.jmat.geom.*;

import java.util.*;

/** Base interface of a rendering style that can be used with VisData. **/
public interface VisDataStyle
{
    public void renderStyle(VisContext vc, GL gl, GLU glu, VisData vdata);
}
