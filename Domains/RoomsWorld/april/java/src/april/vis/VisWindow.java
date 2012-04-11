package april.vis;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import java.awt.*;
import java.awt.image.*;
import java.nio.*;
import javax.swing.*;
import java.util.*;

import april.jmat.*;
import april.jmat.geom.*;

import java.io.*;
import lcm.lcm.*;

/** Performs a change of coordinates allowing rendering relative to
 * the corners of the screen. XXX should VisText be reimplemented in
 * terms of this?
 **/
public class VisWindow implements VisObject, VisSerializable
{
    public enum ALIGN { TOP_LEFT, TOP, TOP_RIGHT, LEFT, CENTER, RIGHT, BOTTOM_LEFT, BOTTOM, BOTTOM_RIGHT, COORDINATES };

    public ALIGN align;                // where is the window?
    double coords[];                   // coords (lower left) OR align
    public double xy0[], xy1[];        // coordinates that will be mapped to our window (xy0: bottom left, xy1: upper right)
    public int winwidth, winheight; // size of the window, in pixels.

    ArrayList<VisObject> objects = new ArrayList<VisObject>();

    public Color backgroundColor = null;

    /** Define a new window of size (in pixels) winwidth x winheight,
        which will be aligned with respect to the viewable region of
        the screen. A coordinate transform will be applied so that the
        coordinate xy0 represents the lower left corner, and xy1
        represents the upper right corner. Then, all the objects will
        be drawn.
    **/
    public VisWindow(ALIGN align, int winwidth, int winheight, double xy0[], double xy1[],
                     VisObject ... os)
    {
        this.align = align;
        this.winwidth = winwidth;
        this.winheight = winheight;
        this.xy0 = LinAlg.copy(xy0);
        this.xy1 = LinAlg.copy(xy1);
        this.coords = null;
        for (VisObject o : os)
            add(o);
    }

    /** Define a new window of size (in pixels) winwidth x winheight,
        which will be aligned with respect to the viewable region of
        the screen (at screen coordinate coords). A coordinate
        transform will be applied so that the coordinate xy0
        represents the lower left corner, and xy1 represents the upper
        right corner. Then, all the objects will be drawn.
    **/
    public VisWindow(double coords[], int winwidth, int winheight, double xy0[], double xy1[],
                     VisObject ... os)
    {
        assert(coords.length == 2);

        this.align = ALIGN.COORDINATES;
        this.winwidth = winwidth;
        this.winheight = winheight;
        this.xy0 = LinAlg.copy(xy0);
        this.xy1 = LinAlg.copy(xy1);
        this.coords = coords;

        for (VisObject o : os)
            add(o);
    }

    public void add(VisObject vo)
    {
        objects.add(vo);
    }

    public void clear()
    {
        objects.clear();
    }

    public void render(VisContext vc, GL gl, GLU glu)
    {
        // save original matrices
        VisUtil.pushGLWholeState(gl);

        int viewport[] = new int[4];
        gl.glGetIntegerv(gl.GL_VIEWPORT, viewport, 0);

        // compute the pixel coordinates of the window.
        int px0, px1, py0, py1;

        switch (align)
        {
            case TOP_LEFT: case LEFT: case BOTTOM_LEFT:
                px0 = 0;
                px1 = winwidth;
                break;
            case COORDINATES:
                px0 = (int)Math.round(Math.max(1, Math.min(viewport[2]- 1 - winwidth, coords[0])));
                px1 = (int)Math.round(Math.max(winwidth, Math.min(viewport[2]- 1, coords[0] + winwidth)));
                break;

            default: case TOP: case CENTER:	case BOTTOM:
                px0 = (viewport[0] + viewport[2])/2 - winwidth/2;
                px1 = px0 + winwidth;
                break;

            case TOP_RIGHT: case RIGHT: case BOTTOM_RIGHT:
                px1 = viewport[2]-1;
                px0 = px1 - winwidth;
                break;
        }

        switch (align)
        {
            case TOP_LEFT: case TOP: case TOP_RIGHT:
                // remember that y is inverted: y=0 is at bottom
                // left in GL
                py0 = viewport[3] - winheight - 1;
                py1 = py0 + winheight;
                break;

            case COORDINATES:
                py0 = (int)Math.round(Math.max(1, Math.min(viewport[3]- 1 - winheight, coords[1])));
                py1 = py0 + winheight;
                break;

            default: case LEFT: case CENTER: case RIGHT:
                py0 = (viewport[1] + viewport[3])/2 - winheight/2;
                py1 = py0 + winheight;
                break;

            case BOTTOM_LEFT: case BOTTOM: case BOTTOM_RIGHT:
                py0 = 1;
                py1 = py0 + winheight;
                break;
        }

        gl.glEnable(GL.GL_SCISSOR_TEST);
        gl.glViewport(px0, py0, px1-px0, py1-py0);
        gl.glScissor(px0, py0, px1-px0, py1-py0);

        // setup very dumb projection in pixel coordinates
        gl.glMatrixMode(gl.GL_PROJECTION);
        gl.glLoadIdentity();
        glu.gluOrtho2D(0, winwidth, 0, winheight);

        gl.glMatrixMode(gl.GL_MODELVIEW);
        gl.glLoadIdentity();

        if (backgroundColor != null) {
            gl.glClearColor(backgroundColor.getRed()/255f,
                            backgroundColor.getGreen()/255f,
                            backgroundColor.getBlue()/255f,
                            1.0f);

            gl.glClearDepth(1.0f);
            gl.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT);
        }

        if (false) {
            // visualize the window.
            gl.glColor3f(1.0f, .5f, .5f);
            gl.glBegin(GL.GL_QUADS);
            gl.glVertex3d(0, 0, 0);
            gl.glVertex3d(winwidth, 0, 0);
            gl.glVertex3d(winwidth, winheight, 0);
            gl.glVertex3d(0, winheight, 0);
            gl.glEnd();
        }

        // scale the input coordinates to [0, winwidth]
        gl.glScaled(winwidth/(xy1[0]-xy0[0]), winheight/(xy1[1]-xy0[1]), 0);
        gl.glTranslated(-xy0[0], -xy0[1], 0);

        // TODO: (optional?) Stenciling/ clip planes

        // everything will be drawn at z=0 due to our scaling, so
        // depth doesn't make sense.
        gl.glDisable(gl.GL_DEPTH_TEST);

        // render the objects
        for (VisObject o : objects) {

            VisUtil.pushGLState(gl);
            o.render(vc, gl, glu);
            VisUtil.popGLState(gl);
        }

        // restore old viewport
        gl.glViewport(viewport[0], viewport[1], viewport[2], viewport[3]);
        VisUtil.popGLWholeState(gl);
    }

    public VisWindow()
    {
    }

    public void serialize(LCMDataOutputStream out) throws IOException
    {
        out.writeInt(align.ordinal());
        out.writeDouble(xy0[0]);
        out.writeDouble(xy0[1]);

        out.writeDouble(xy1[0]);
        out.writeDouble(xy1[1]);

        out.writeInt(winwidth);
        out.writeInt(winheight);

        int count = 0;
        for(VisObject o : objects)
            if (o instanceof VisSerializable)
                count++;
        out.writeInt(count);
        for (VisObject o : objects)
            if (o instanceof VisSerializable)
                VisSerialize.serialize((VisSerializable) o, out);
            else
                System.out.println("WRN:    "+o.getClass().getName()+" is not serializable!");
    }

    public void unserialize(LCMDataInputStream in) throws IOException
    {
        align = getAlign(in.readInt());
        xy0 = new double[] { in.readDouble(), in.readDouble() };
        xy1 = new double[] { in.readDouble(), in.readDouble() };

        winwidth = in.readInt();
        winheight = in.readInt();

        int nobjs = in.readInt();
        for (int i = 0; i < nobjs; i++)
            objects.add((VisObject) VisSerialize.unserialize(in));
    }

    // Converting to enums from ints
    ALIGN getAlign(int v)
    {
        for (ALIGN a : ALIGN.values())
            if (a.ordinal() == v)
                return a;
        assert(false);
        return ALIGN.CENTER;
    }

    public static void main(String args[])
    {
        JFrame jf = new JFrame("test");
        VisWorld vw = new VisWorld();
        VisCanvas vc = new VisCanvas(vw);

        VisWorld.Buffer vb = vw.getBuffer("foo");

        for (int ix = 0; ix < 6; ix++) {
            for (int iy = 0; iy < 6; iy++) {

                vb.addBuffered(new VisWindow(new double[] {ix*100, iy*100}, 100, 100, new double[] {0,0}, new double[] {100,100},
                                             new VisBox(50, 50, 1, Color.yellow),
                                             new VisText(VisText.ANCHOR.TOP_LEFT, "TL"),
                                             new VisText(VisText.ANCHOR.TOP_RIGHT, "TR"),
                                             new VisText(VisText.ANCHOR.BOTTOM_LEFT, "BL"),
                                             new VisText(VisText.ANCHOR.BOTTOM_RIGHT, "BR"),
                                             new VisText(VisText.ANCHOR.CENTER, "CENTER")));
            }
        }

        vb.switchBuffer();

        jf.setLayout(new BorderLayout());
        jf.add(vc, BorderLayout.CENTER);
        jf.setSize(600,600);
        jf.setVisible(true);
    }
}
