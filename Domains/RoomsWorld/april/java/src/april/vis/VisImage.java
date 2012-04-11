package april.vis;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import java.awt.*;
import java.awt.image.*;
import java.nio.*;
import javax.swing.*;
import java.util.*;

import april.jmat.geom.*;

import lcm.lcm.*;
import java.io.*;

/** If you'd like to overlay an image (in the corner of the screen,
 * for example), put the VisImage in a VisWindow; note that images
 * will be lit by default (which can result in odd appearance) which
 * can be disabled by using VisLighting.
 *
 **/
public class VisImage implements VisObject, VisSerializable
{
    public VisTexture texture;

    double x0, y0, x1, y1, z;
    double scale = 1.0;
    boolean flipy;

    static boolean sizeWarning = false;

    public Color modulateColor = Color.white;

    /** Create a new buffered image in pixel coordinates. **/
    public VisImage(BufferedImage im)
    {
        this(new VisTexture(im), new double[] { 0, 0 }, new double[] {im.getWidth(), im.getHeight()}, true);
    }

    public VisImage(VisTexture texture, double xy0[], double xy1[])
    {
        this(texture, xy0, xy1, 0, false);
    }

    public VisImage(VisTexture texture, double xy0[], double xy1[], boolean flipy)
    {
        this(texture, xy0, xy1, 0, flipy);
    }

    public VisImage(VisTexture texture, double xy0[], double xy1[], double z, boolean flipy)
    {
        this.texture = texture;
        this.x0 = xy0[0];
        this.y0 = xy0[1];
        this.x1 = xy1[0];
        this.y1 = xy1[1];
        this.z = z;
        this.flipy = flipy;
    }

    public VisImage copy()
    {
        VisImage im = new VisImage(texture, new double[] {x0, y0}, new double[] {x1, y1}, z, flipy);
        im.modulateColor = modulateColor;

        return im;
    }

    public VisTexture getTexture()
    {
        return texture;
    }

    public void render(VisContext vc, GL gl, GLU glu)
    {
        double width = texture.getWidth(), height = texture.getHeight();

        // must set a non-transparent vertex color; this color modulates the texture
        VisUtil.setColor(gl, modulateColor);

        texture.bindTexture(gl);

        gl.glBegin(gl.GL_QUADS);
        gl.glTexCoord2d(0, flipy ? height : 0);
        gl.glVertex3d(x0, y0, z);
        gl.glTexCoord2d(0, flipy ? 0 : height);
        gl.glVertex3d(x0, y1, z);
        gl.glTexCoord2d(width, flipy ? 0 : height);
        gl.glVertex3d(x1, y1, z);
        gl.glTexCoord2d(width, flipy ? height : 0);
        gl.glVertex3d(x1, y0, z);
        gl.glEnd();

        texture.unbindTexture(gl);
    }

    public VisImage()
    {
    }

    public void serialize(LCMDataOutputStream out) throws IOException
    {
        VisSerialize.serialize(texture, out);
        out.writeDouble(x0);
        out.writeDouble(y0);
        out.writeDouble(x1);
        out.writeDouble(y1);
        out.writeDouble(z);
        out.writeDouble(scale);
        out.writeBoolean(flipy);
        out.writeInt(modulateColor.getRGB());
    }

    public void unserialize(LCMDataInputStream in) throws IOException
    {
        texture = (VisTexture)VisSerialize.unserialize(in);
        x0 = in.readDouble();
        y0 = in.readDouble();
        x1 = in.readDouble();
        y1 = in.readDouble();
        z = in.readDouble();
        scale = in.readDouble();
        flipy = in.readBoolean();
        modulateColor = new Color(in.readInt(), true);
    }

}
