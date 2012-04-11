package april.newvis;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;

/** Base interface of all objects in a VisWorld. **/
public interface VisObject
{
    public void render(VisCanvas vc, GL gl, GLU glu);
}
