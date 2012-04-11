package april.vis;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;

import april.jmat.geom.*;

import java.util.*;
import java.io.*;
import lcm.lcm.*;

/** VisObject wrapper that manipulates whether depth testing is performed. **/
public class VisDepthTest implements VisObject, VisSerializable
{
    VisObject os[];
    boolean enable;

    public VisDepthTest(boolean enable, VisObject ... os)
    {
        this.enable = enable;
        this.os = os;
    }

    public void render(VisContext vc, GL gl, GLU glu)
    {
        gl.glPushAttrib(gl.GL_ENABLE_BIT);

        if (enable)
            gl.glEnable(GL.GL_DEPTH_TEST);
        else
            gl.glDisable(GL.GL_DEPTH_TEST);

        for (VisObject vo : os)
            vo.render(vc, gl, glu);

        gl.glPopAttrib();
    }

    public VisDepthTest()
    {
    }

    public void serialize(LCMDataOutputStream out) throws IOException
    {
        out.writeBoolean(enable);
        int count = 0;
        for(VisObject o : os)
            if (o instanceof VisSerializable)
                count++;
        out.writeInt(count);
        for (VisObject o : os)
            if (o instanceof VisSerializable)
                VisSerialize.serialize((VisSerializable) o, out);
    }

    public void unserialize(LCMDataInputStream in) throws IOException
    {
        enable = in.readBoolean();
        os = new VisObject[in.readInt()];
        for (int i = 0; i < os.length; i++)
            os[i] = (VisObject)VisSerialize.unserialize(in);
    }

}
