package april.vis;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;

/** Base interface of all objects in a VisWorld. **/
public interface VisObject
{
    public void render(VisContext vc, GL gl, GLU glu);
}
