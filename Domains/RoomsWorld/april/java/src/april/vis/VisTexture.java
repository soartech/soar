package april.vis;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import java.awt.*;
import java.awt.image.*;
import java.awt.geom.*;
import java.nio.*;
import javax.swing.*;
import java.util.*;

import april.jmat.geom.*;

import java.io.*;
import lcm.lcm.*;

/** Represents a texture that can be painted by e.g. VisImage. This
 * texture may not work properly from multiple GL contexts.
 **/
public class VisTexture implements VisSerializable
{
    BufferedImage im;
    boolean alphaMask;

    boolean locked;
    int texids[];
    GL texidsGL;

    boolean mipmap;
    boolean magFilterEnable;

    int textureTarget; // GL_TEXTURE_RECTANGLE_ARB or GL_TEXTURE_2D (for mipmapping) ?

    int glinternal;
    int glformat;
    int gltype;
    int bytes_per_pixel;

    ArrayList<MipMapLevel> mipmaps = new ArrayList<MipMapLevel>();

    static boolean warnedSlowConversion;

    static boolean sizeWarning;

    static class MipMapLevel
    {
        BufferedImage im;
        int width;
        int height;
        int level;

        public MipMapLevel(BufferedImage im, int width, int height, int level)
        {
            this.im = im;
            this.width=width;
            this.height=height;
            this.level=level;
        }
    }

    public VisTexture(BufferedImage input)
    {
        this(input, false);
    }

    public VisTexture(BufferedImage input, boolean alphaMask)
    {
        init(input, alphaMask);
    }

    private void init(BufferedImage input, boolean alphaMask)
    {
        this.alphaMask = alphaMask;

        switch (input.getType())
        {
            case BufferedImage.TYPE_INT_ARGB: {
                im = input;
                glinternal = GL.GL_RGBA8;
                glformat = GL.GL_BGRA;
                gltype = GL.GL_UNSIGNED_INT_8_8_8_8_REV;
                bytes_per_pixel = 4;
                break;
            }
            case BufferedImage.TYPE_BYTE_GRAY: {
                im = input;
                if (alphaMask) {
                    glinternal = GL.GL_ALPHA8;
                    glformat = GL.GL_ALPHA;
                } else {
                    glinternal = GL.GL_LUMINANCE8;
                    glformat = GL.GL_LUMINANCE;
                }
                gltype = GL.GL_UNSIGNED_BYTE;
                bytes_per_pixel = 1;
                break;
            }
            case BufferedImage.TYPE_4BYTE_ABGR: {
                im = input;
                glinternal = GL.GL_RGBA8;
                glformat = GL.GL_ABGR_EXT;
                gltype = GL.GL_UNSIGNED_INT_8_8_8_8_REV;
                bytes_per_pixel = 4;
                break;
            }
            case BufferedImage.TYPE_INT_RGB: {
                im = input;
                glinternal = GL.GL_RGB8;
                glformat = GL.GL_BGRA;
                gltype = GL.GL_UNSIGNED_INT_8_8_8_8_REV;
                bytes_per_pixel = 4;
                break;
            }

            default: {
                // coerce texture format to correct type.
                im = new BufferedImage(input.getWidth(),
                                       input.getHeight(),
                                       BufferedImage.TYPE_INT_ARGB);
                Graphics g = im.getGraphics();
                g.drawImage(input, 0, 0, input.getWidth(), input.getHeight(), null);
                g.dispose();

                if (!warnedSlowConversion) {
                    System.out.println("VisTexture: Slow image type conversion for type "+input.getType());
                    warnedSlowConversion = true;
                }

                glinternal = GL.GL_RGBA8;
                glformat = GL.GL_BGRA;
                gltype = GL.GL_UNSIGNED_INT_8_8_8_8_REV;
                bytes_per_pixel = 4;
                break;
            }
        }

        if (isPowerOfTwo(im.getWidth()) && isPowerOfTwo(im.getHeight()))
            textureTarget = GL.GL_TEXTURE_2D;
        else
            textureTarget = GL.GL_TEXTURE_RECTANGLE_ARB;
    }

    boolean isPowerOfTwo(int a)
    {
        if (a!=0 && (a&(a-1))==0)
            return true;
        return false;
    }

    /** Once a texture has been copied into GL memory, prevent it from
     * being deleted. This is useful if the texture is frequently
     * redrawn.  It is up to the user to unlock the texture; else, a
     * memory leak (in graphics card memory) occurs.
     **/
    public synchronized void lock()
    {
        locked = true;
    }

    /** When unlocked, the texture will continue to work properly, but
     * will be re-uploaded to the graphics card every time it is
     * displayed. **/
    public synchronized void unlock()
    {
        if (!locked)
            return;

        locked = false;
        if (texids != null)
            texidsGL.glDeleteTextures(1, texids, 0);

        texids = null;
        texidsGL = null;
    }

    /** Must be called before the texture is rendered, specifically,
     * before the first getTextureId() **/
    public void setMipMapping(boolean mipmap)
    {
        this.mipmap = mipmap;

        if (mipmap && (!isPowerOfTwo(im.getWidth()) || !isPowerOfTwo(im.getHeight()))) {
            System.out.println("VisTexture: attempt to enable mipmapping on non power of two texture. Won't mipmap.");
            mipmap = false;
        }
    }

    public void setMagFilter(boolean enable)
    {
        magFilterEnable = enable;
    }

    public synchronized void bindTexture(GL gl)
    {
        gl.glEnable(textureTarget);

        if (texids != null) {
            gl.glBindTexture(textureTarget, texids[0]);
            return;
        }

        texids = new int[1];
        texidsGL = gl;

        int width = im.getWidth(), height = im.getHeight();
        if (Math.max(width, height) > 4096 && !sizeWarning) {
            System.out.println("VisImage: Warning, texture has a dimension greater than 4096");
            sizeWarning = true;
        }

        gl.glGenTextures(1, texids, 0);
        gl.glBindTexture(textureTarget, texids[0]);

        if (mipmap)
            gl.glTexParameteri(textureTarget, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR_MIPMAP_LINEAR);
        else
            gl.glTexParameteri(textureTarget, gl.GL_TEXTURE_MIN_FILTER, gl.GL_LINEAR);

        if (magFilterEnable)
            gl.glTexParameteri(textureTarget, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);
        else
            gl.glTexParameteri(textureTarget, GL.GL_TEXTURE_MAG_FILTER, GL.GL_NEAREST);

        if (mipmap) {
            makeMipLevels();

            for (MipMapLevel mml : mipmaps) {
                int stride = bytes_per_pixel * mml.width;
                if(stride % 2 != 0) {
                    gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);
                } else if(stride % 4 != 0) {
                    gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 2);
                } else {
                    gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 4);
                }
                gl.glTexImage2D(textureTarget, mml.level,  glinternal, mml.width, mml.height, 0,
                                glformat, gltype, toBuffer(mml.im));
            }
        } else {
            int stride = bytes_per_pixel * width;
            if(stride % 2 != 0) {
                gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);
            } else if(stride % 4 != 0) {
                gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 2);
            } else {
                gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 4);
            }
            gl.glTexImage2D(textureTarget, 0, glinternal, width, height, 0,
                            glformat, gltype, toBuffer(im));
        }

        // restore unpack alignment to default
        gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 4);
    }

    public synchronized void unbindTexture(GL gl)
    {
        gl.glBindTexture(textureTarget, 0);
        gl.glDisable(textureTarget);

        if (locked)
            return;

        gl.glDeleteTextures(1, texids, 0);
        texids = null;
    }

    /** Gives the width (in texture coordinates) of the image. This
     * may be 1.0 or the width in pixels, depending on whether the
     * texture is being implemented with a TEXTURE_2D or
     * TEXTURE_RECTANGLE_ARB; the former uses normalized texture
     * coordinates, whereas the latter uses pixel coordinates. Using
     * this function allows implementation-independent texturing. **/
    public double getWidth()
    {
        if (textureTarget == GL.GL_TEXTURE_2D)
            return 1.0;

        return im.getWidth();
    }

    /** See getWidth() **/
    public double getHeight()
    {
        if (textureTarget == GL.GL_TEXTURE_2D)
            return 1.0;

        return im.getHeight();
    }

    protected void makeMipLevels()
    {
        int level = 0;
        AffineTransform at = AffineTransform.getScaleInstance(.5,.5);
        AffineTransformOp ato = new AffineTransformOp(at, AffineTransformOp.TYPE_BILINEAR);

        while (true) {
            mipmaps.add(new MipMapLevel(im, im.getWidth(), im.getHeight(),level));

            // are we done?
            if (!mipmap)
                break;

            if (im.getWidth()<=1 && im.getHeight()<=1)
                break;

            // Never produces an image with a dimension less than one...
            im = ato.filter(im, null);

            level++;
        }
    }

    static Buffer toBuffer(BufferedImage im)
    {
        return toBuffer(im.getRaster().getDataBuffer());
    }

    static Buffer toBuffer(DataBuffer db)
    {
        if (db instanceof DataBufferInt)
            return IntBuffer.wrap(((DataBufferInt) db).getData());
        if (db instanceof DataBufferByte)
            return ByteBuffer.wrap(((DataBufferByte) db).getData());

        assert(false);
        return null;
    }

    public VisTexture()
    {
    }

    public void serialize(LCMDataOutputStream out) throws IOException
    {
        out.writeInt(im.getWidth());
        out.writeInt(im.getHeight());
        out.writeInt(im.getType());

        DataBuffer db = im.getRaster().getDataBuffer();
        int dataType = db.getDataType();

        if (dataType == DataBuffer.TYPE_BYTE) {
            byte d[] = ((DataBufferByte) (im.getRaster().getDataBuffer())).getData();
            out.write(d);
        } else if (dataType == DataBuffer.TYPE_INT) {
            int d[] = ((DataBufferInt) (im.getRaster().getDataBuffer())).getData();
            for (int i = 0; i < d.length; i++)
                out.writeInt(d[i]);
        } else {
            assert(false);
        }

        out.writeBoolean(alphaMask);

        // Skip the rest of the fields, and recall the init() method on unserialization
        // This is because when loaded from PNG, the exact image format may change

    }

    public void unserialize(LCMDataInputStream in) throws IOException
    {
        int width = in.readInt();
        int height = in.readInt();
        int type = in.readInt();

        BufferedImage im = new BufferedImage(width, height, type);
        DataBuffer db = im.getRaster().getDataBuffer();
        int dataType = db.getDataType();

        if (dataType == DataBuffer.TYPE_BYTE) {
            byte d[] = ((DataBufferByte) (im.getRaster().getDataBuffer())).getData();
            in.readFully(d);
        } else if (dataType == DataBuffer.TYPE_INT) {
            int d[] = ((DataBufferInt) (im.getRaster().getDataBuffer())).getData();
            for (int i = 0; i < d.length; i++)
                d[i] = in.readInt();
        } else {
            assert(false);
        }

        boolean alphaMask = in.readBoolean();

        init(im, alphaMask);

        // the only supported application of loading snapshots is for
        // reviewing scenes after-the-fact.  might as well lock the
        // texture down for performance.
        lock();
    }

}
