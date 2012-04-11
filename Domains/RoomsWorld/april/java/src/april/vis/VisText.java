package april.vis;

import java.awt.*;
import java.awt.geom.*;
import java.awt.event.*;
import java.awt.image.*;
import java.io.*;
import java.util.*;
import java.nio.channels.*;
import java.nio.*;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import javax.swing.*;

import com.sun.opengl.util.*;

import april.jmat.geom.*;
import april.jmat.*;

import java.io.*;
import lcm.lcm.*;


/** VisObject representing text. Each block of text can consist of
 * multiple lines, and each line can have multiple styles
 * associated. Changes in style are indicated by double angle
 * brackets, i.e., like HTML, except with two angle brackets.  Inside
 * the double-angle brackets, different style attributes are separated
 * by commas. Style attributes can consist of colors (common colors
 * like "blue" can be specified by name, or custom colors can be
 * specified using HTML #RRGGBB notation), font changes ("small",
 * "normal", "big", "mono-normal", "mono-big"), and a manual width
 * specification (in pixels) for the following segment of text.  See
 * VisTextTest for an example.
 **/
public class VisText implements VisObject, VisSerializable
{
    double pos[];

    /** What color is the text if not otherwise specified? If null, it
     * will be the inverse of the VisCanvas background color.
     **/
    public Color defaultColor = null;
    public int defaultFont = GLUT.BITMAP_HELVETICA_12; //GLUT.BITMAP_9_BY_15;

    //int font = GLUT.BITMAP_9_BY_15;         // sizes: 8x13, 9x15
    //int font = GLUT.BITMAP_TIMES_ROMAN_10;  // sizes: 10, 24
    //int font = GLUT.BITMAP_HELVETICA_12;    // sizes: 10, 12, 18

    boolean dropShadow = true;

    /** What color is the drop shadow? If null, it will be the same color as the VisCanvas background. **/
    public Color dropShadowColor = null;

    /** How transparent is the drop shadow? 0.0 means fully
     * transparent (invisible). This parameter only applies when
     * dropShadowColor is null; otherwise, we use the alpha specified
     * by dropShadowColor.
     **/
    double dropShadowAlpha = 0.8;

    /** how many extra pixels around the text should the dropShadow extend? **/
    double dropShadowMargin = 3.0;

    /** When drawing text in a screen corner, how much should we indent? **/
    int SCREEN_MARGIN = 2;

    /** Align: the specified coordinate is what corner of the text?
     * This parameter affects the entire block of text.
     **/
    public enum ANCHOR { TOP_LEFT, TOP, TOP_RIGHT, LEFT, CENTER, RIGHT, BOTTOM_LEFT, BOTTOM, BOTTOM_RIGHT };
    ANCHOR anchor = ANCHOR.CENTER;

    /** How should each line be justified? This is the default value
     * used unless overridden on a per-line basis.
     **/
    public enum JUSTIFICATION { LEFT, CENTER, RIGHT };
    JUSTIFICATION defaultJustification = JUSTIFICATION.CENTER;

    /** A line of text is composed of multiple styled fragments, each
     * drawn contiguously (horizontally). The line additionally has an
     * alignment, which determines how the text is justified.
     **/
    static class Line
    {
        ArrayList<StyledFragment> fragments = new ArrayList<StyledFragment>();
        JUSTIFICATION justification;

        /** how much additional space below this line should we leave? **/
        int leading = 1;

        int getWidth(GLUT glut)
        {
            int width = 0;

            for (StyledFragment frag : fragments) {
                width += frag.getWidth(glut);
            }

            return width;
        }

        int getHeight(GLUT glut)
        {
            int maxheight = 1;

            for (StyledFragment frag : fragments) {
                maxheight = Math.max(maxheight, frag.getHeight(glut));
            }

            return maxheight + leading;
        }
    }

    /** Each fragment has a font, color, and the string itself. **/
    static class StyledFragment
    {
        int font;
        Color c;
        String s;
        int    width = -1; // width of this fragment in pixels. (-1 means compute from the font.)

        int getWidth(GLUT glut)
        {
            if (width >=0)
                return width;

            return glut.glutBitmapLength(font, s);
        }

        /** Total line height in pixels. **/
        int getHeight(GLUT glut)
        {
            return glutBitmapHeight(font);
        }

        /** What is the descent of the font? **/
        int getDescent(GLUT glut)
        {
            // hack. We don't know this data, so we make it up.
            return glutBitmapHeight(font)/5;
        }
    }

    ArrayList<Line> lines = new ArrayList<Line>();

    // This is a hack since JOGL doesn't implement glutBitmapHeight
    static final int glutBitmapHeight(int font)
    {
        switch (font)
	    {
            case GLUT.BITMAP_8_BY_13:
                return 13;
            case GLUT.BITMAP_9_BY_15:
                return 15;

            case GLUT.BITMAP_HELVETICA_10:
                return 10;
            case GLUT.BITMAP_HELVETICA_12:
                return 12;
            case GLUT.BITMAP_HELVETICA_18:
                return 18;

            case GLUT.BITMAP_TIMES_ROMAN_10:
                return 10;
            case GLUT.BITMAP_TIMES_ROMAN_24:
                return 24;

            default:
            case GLUT.STROKE_MONO_ROMAN:
            case GLUT.STROKE_ROMAN:
                return 14; // DON'T KNOW.
	    }
    }

    public VisText(double pos[], ANCHOR anchor, String s)
    {
        this(pos, anchor, null, s, 0.0, false);
    }

    /** @param pos Coordinates in world space where the text should be
     * drawn. If null, text will be drawn relative to the corners of
     * the screen (and the 'anchor' parameter determines which corner).
     *
     * @param anchor The point specified by pos will be an anchor
     * point: where is the anchor point relative to where the text
     * should be drawn? I.e., TOP_LEFT specifies that the text should
     * appear to the bottom right of the anchor point.
     *
     * @param dropShadow Should a drop shadow be drawn to help aid
     * legibility? The color of the drop shadow will be computed
     * automatically from the background color of the VisCanvas.
     **/
    public VisText(double pos[], ANCHOR anchor, String s, double alpha)
    {
        this(pos, anchor, null, s, alpha, true);
    }

    /**
     * Constructor: fully specified parameters
     **/
    public VisText(double pos[], ANCHOR anchor, JUSTIFICATION defaultJustification,
                   String s, double alpha, boolean dropShadow)
    {
        if (pos != null) {
            if (pos.length==3)
                this.pos = LinAlg.copy(pos);
            else
                this.pos = new double[] {pos[0], pos[1], 0};
        }

        if (dropShadow) {
            this.dropShadow = true;
            this.dropShadowAlpha = alpha;
        }
        if (defaultJustification != null)
            this.defaultJustification = defaultJustification;
        if (anchor != null)
            this.anchor = anchor;
        if (s != null)
            handleText(s);
    }

    /** Convenience constructor. **/
    public VisText(ANCHOR anchor, String s, double alpha)
    {
        this(null, anchor, null, s, alpha, true);
    }
    public VisText(ANCHOR anchor, String s)
    {
        this(null, anchor, null, s, 0.0, false);
    }

    /** Convenience constructor. **/
    public VisText(ANCHOR anchor, JUSTIFICATION defaultJustification, String s)
    {
        this(null, anchor, defaultJustification, s, 0.0, false);
    }

    /** Convenience constructor. **/
    public VisText(ANCHOR anchor, JUSTIFICATION defaultJustification, String s,
                   double alpha)
    {
        this(null, anchor, defaultJustification, s, alpha, true);
    }

    void handleText(String s)
    {
        String ss[] = s.split("\\n");
        for (String ts : ss) {
            addLine(ts, defaultJustification);
        }
    }

    /** Add a line of text to the display. **/
    public void addLine(String s, JUSTIFICATION justification)
    {
        int font = defaultFont;
        Color c = defaultColor;
        int  width = -1;

        Line line = new Line();
        if (lines.size() > 0) {
            Line lastLine = lines.get(lines.size()-1);
            // inherit properties of last line's last fragment.
            if (lastLine.fragments.size() > 0) {
                StyledFragment frag = lastLine.fragments.get(lastLine.fragments.size()-1);
                font = frag.font;
                c = frag.c;
            }
        }

        line.justification = justification;
        lines.add(line);

        int pos = 0;
        while (pos >= 0 && pos < s.length()) {

            // If there's not a format specifier first, consume
            // everything up until the format specifier.
            int fmtpos = s.indexOf("<<", pos);
            int endfmtpos = fmtpos >=0 ? s.indexOf(">>", fmtpos) : -1;

            if (fmtpos != pos || fmtpos < 0 || endfmtpos < 0) {
                StyledFragment frag = new StyledFragment();
                frag.font = font;
                frag.c = c;
                frag.width = width;
                width = -1; // width isn't stateful.
                if (fmtpos < 0)
                    frag.s = s.substring(pos);
                else
                    frag.s = s.substring(pos, fmtpos);

                line.fragments.add(frag);
                pos = fmtpos;
                continue;
            }

            // a format specifier begins at pos.
            String fmt = s.substring(fmtpos+2, endfmtpos);

            String toks[] = fmt.split(",");
            for (int i = 0; i < toks.length; i++) {
                toks[i] = toks[i].toLowerCase().trim();

                // #RRGGBB
                if (toks[i].startsWith("#") && (toks[i].length()==7 || toks[i].length()==9)) {
                    c = stringToColor(toks[i]);
                    continue;
                }

                if (toks[i].startsWith("dropshadow")) {
                    if (toks[i].contains("=")) {
                        String arg = toks[i].substring(toks[i].indexOf("=")+1).trim().toLowerCase();
                        if (arg.equals("true") || arg.equals("yes") || arg.equals("1"))
                            this.dropShadow = true;
                        else if (arg.equals("false") || arg.equals("no") || arg.equals("0"))
                            this.dropShadow = false;
                        else if (arg.startsWith("#")) {
                            this.dropShadow = true;
                            this.dropShadowColor = stringToColor(arg);
                        } else {
                            System.out.println("VisText: Don't understand "+toks[i]);
                        }
                    } else {
                        this.dropShadow = true;
                    }
                    continue;
                }

                if (toks[i].equals("blue")) {
                    c = Color.blue;
                    continue;
                }
                if (toks[i].equals("red")) {
                    c = Color.red;
                    continue;
                }
                if (toks[i].equals("green")) {
                    c = Color.green;
                    continue;
                }
                if (toks[i].equals("black")) {
                    c = Color.black;
                    continue;
                }
                if (toks[i].equals("orange")) {
                    c = Color.orange;
                    continue;
                }
                if (toks[i].equals("yellow")) {
                    c = Color.yellow;
                    continue;
                }
                if (toks[i].equals("cyan")) {
                    c = Color.cyan;
                    continue;
                }
                if (toks[i].equals("magenta")) {
                    c = Color.magenta;
                    continue;
                }
                if (toks[i].equals("gray")) {
                    c = Color.gray;
                    continue;
                }
                if (toks[i].equals("white")) {
                    c = Color.white;
                    continue;
                }
                if (toks[i].equals("pink")) {
                    c = Color.pink;
                    continue;
                }
                if (toks[i].equals("darkgray")) {
                    c = Color.darkGray;
                    continue;
                }

                // non-fixed width fonts
                if (toks[i].equals("big") || toks[i].equals("large")) {
                    font = GLUT.BITMAP_HELVETICA_18;
                    continue;
                }
                if (toks[i].equals("normal")) {
                    font = GLUT.BITMAP_HELVETICA_12;
                    continue;
                }
                if (toks[i].equals("small")) {
                    font = GLUT.BITMAP_HELVETICA_10;
                    continue;
                }
                // mono-spaced fonts
                if (toks[i].equals("mono-big") || toks[i].equals("mono-large")) {
                    font = GLUT.BITMAP_9_BY_15;
                    continue;
                }
                if (toks[i].equals("mono-normal")) {
                    font = GLUT.BITMAP_8_BY_13;
                    continue;
                }
                if (toks[i].equals("mono-small")) {
                    font = GLUT.BITMAP_8_BY_13;
                    continue;
                }
                // justification overrides. (For convenience)
                if (toks[i].equals("left")) {
                    line.justification = JUSTIFICATION.LEFT;
                    continue;
                }
                if (toks[i].equals("center")) {
                    line.justification = JUSTIFICATION.CENTER;
                    continue;
                }
                if (toks[i].equals("right")) {
                    line.justification = JUSTIFICATION.RIGHT;
                    continue;
                }
                // fixed-width. (manually specify width in
                // pixels. Useful for making non-fixed width fonts
                // line up.
                if (toks[i].startsWith("width=")) {
                    width = Integer.parseInt(toks[i].substring(6));
                    continue;
                }
                System.out.println("VisText: Unknown format specifier: "+toks[i]);
            }

            // skip to the end of the format specifier.
            pos = endfmtpos + 2;
        }
    }

    public VisText setDropShadow(boolean b)
    {
        dropShadow = b;

        return this;
    }

    // #ffffff input
    // #rrggbb
    // #rrggbbaa
    static Color stringToColor(String s)
    {
        if (s.length()==7)
            return new Color(Integer.parseInt(s.substring(1,3), 16),
                             Integer.parseInt(s.substring(3,5), 16),
                             Integer.parseInt(s.substring(5,7), 16));
        if (s.length()==9)
            return new Color(Integer.parseInt(s.substring(1,3), 16),
                             Integer.parseInt(s.substring(3,5), 16),
                             Integer.parseInt(s.substring(5,7), 16),
                             Integer.parseInt(s.substring(7,9), 16));
        System.out.println("VisText: Badly formatted color "+s);
        return null;

    }

    public void render(VisContext vc, GL gl, GLU glu)
    {
        // draw nothing (not even a drop shadow) if there's no text.
        if (lines.size() == 1 && lines.get(0).fragments.size()==0)
            return;

        double model_matrix[] = new double[16];
        double proj_matrix[] = new double[16];
        int viewport[] = new int[4];

        gl.glGetDoublev(gl.GL_MODELVIEW_MATRIX, model_matrix, 0);
        gl.glGetDoublev(gl.GL_PROJECTION_MATRIX, proj_matrix, 0);
        gl.glGetIntegerv(gl.GL_VIEWPORT, viewport, 0);

        ////////////////////////////////////////////////////
        // find the pixel coordinates corresponding to the specified
        // "reference" point. The text can then be positioned relative
        // (TOP_LEFT, CENTER, etc.) relative to this point.
        double winxyz[] = new double[3];

        // BUG: no way to manually specify *screen* coordinates.
        if (pos != null) {

            // align with respect to coordinates in the scene.
            if (!glu.gluProject(pos[0], pos[1], pos[2],
                                model_matrix, 0, proj_matrix, 0, viewport, 0,
                                winxyz, 0)) {
                System.out.printf("VisText: GluProject failure\n");
                return;
            }
        } else {
            // pos is null, so we'll draw relative to
            // anchor with respect to the corners of the screen
            switch (anchor)
            {
                case TOP_LEFT: case LEFT: case BOTTOM_LEFT:
                    winxyz[0] = SCREEN_MARGIN;
                    break;

                default: case TOP: case CENTER:	case BOTTOM:
                    winxyz[0] = (viewport[2])/2;
                    break;

                case TOP_RIGHT: case RIGHT: case BOTTOM_RIGHT:
                    winxyz[0] = viewport[2]-SCREEN_MARGIN;
                    break;
            }

            switch (anchor)
            {
                case TOP_LEFT: case TOP: case TOP_RIGHT:
                    // remember that y is inverted: y=0 is at bottom
                    // left in GL
                    winxyz[1] = viewport[3]-SCREEN_MARGIN;
                    break;

                default: case LEFT: case CENTER: case RIGHT:
                    winxyz[1] = (viewport[3])/2;
                    break;

                case BOTTOM_LEFT: case BOTTOM: case BOTTOM_RIGHT:
                    winxyz[1] = SCREEN_MARGIN;
                    break;
            }
        }

        VisUtil.pushGLWholeState(gl);
        // colors don't work if lighting is enabled with glutBitmap.
        gl.glDisable(GL.GL_DEPTH_TEST);
        gl.glDisable(GL.GL_LIGHTING);

        // save original matrices
        gl.glMatrixMode(gl.GL_PROJECTION);
        gl.glPushMatrix();
        gl.glMatrixMode(gl.GL_MODELVIEW);
        gl.glPushMatrix();
        gl.glPushAttrib(gl.GL_ENABLE_BIT);

        // setup very dumb projection in pixel coordinates
        gl.glMatrixMode(gl.GL_PROJECTION);
        gl.glLoadIdentity();
        glu.gluOrtho2D(0,viewport[2],0,viewport[3]);
//        glu.gluOrtho2D(viewport[0],viewport[0]+viewport[2],viewport[1],viewport[1]+viewport[3]);

        gl.glMatrixMode(gl.GL_MODELVIEW);
        gl.glLoadIdentity();

        GLUT glut = new GLUT();

        // compute the bounding dimensions of this text.
        int height = 0;
        for (Line line : lines)
            height += line.getHeight(glut);

        int max_width = 0;
        for (Line line : lines)
            max_width = Math.max(max_width, line.getWidth(glut));

        ////////////////////////////////////////////////////
        // compute lower left coordinate of the box
        int x0 = (int) winxyz[0];
        int y0 = (int) winxyz[1];

        switch (anchor)
	    {
            case TOP_LEFT: case LEFT: case BOTTOM_LEFT:
                x0 = (int) winxyz[0];
                break;

            default: case TOP: case CENTER:	case BOTTOM:
                x0 = (int) (winxyz[0] - max_width/2);
                break;

            case TOP_RIGHT: case RIGHT: case BOTTOM_RIGHT:
                x0 = (int) (winxyz[0] - max_width);
                break;
	    }

        switch (anchor)
	    {
            // remember that y should get SMALLER after every
            // line.
            case TOP_LEFT: case TOP: case TOP_RIGHT:
                y0 = (int) (winxyz[1] - height);
                break;

            default: case LEFT: case CENTER: case RIGHT:
                y0 = (int) (winxyz[1] - height/2);
                break;

            case BOTTOM_LEFT: case BOTTOM: case BOTTOM_RIGHT:
                y0 = (int) (winxyz[1]);
                break;
	    }

        ////////////////////////////////////////////////////////////
        // drop shadow
        if (dropShadow) {

            Color c = dropShadowColor != null ?
                dropShadowColor : ColorUtil.setAlpha(vc.getBackground(), (int) (255*dropShadowAlpha));;

            VisUtil.setColor(gl, c);

            gl.glBegin(gl.GL_QUADS);

            double xa = x0 - dropShadowMargin, xb = x0 + max_width + dropShadowMargin;
            double ya = y0 - dropShadowMargin, yb = y0 + height + dropShadowMargin;

            gl.glVertex2d(xa, ya);
            gl.glVertex2d(xa, yb);
            gl.glVertex2d(xb, yb);
            gl.glVertex2d(xb, ya);
            gl.glEnd();
        }

        ////////////////////////////////////////////////////////////
        // draw text
        double y = y0 + height;

        for (int i = 0; i < lines.size(); i++) {

            Line line = lines.get(i);
            int thiswidth = line.getWidth(glut);
            double x = x0;

            switch (line.justification) {
                case LEFT:
                    x = (int) x0;
                    break;

                case CENTER:
                    x = (int) (x0 + (max_width-thiswidth)/2);
                    break;

                case RIGHT:
                    x = (int) (x0 + (max_width-thiswidth));
                    break;
            }

            y -= line.getHeight(glut);
            for (StyledFragment frag : line.fragments) {
                Color c = frag.c != null ? frag.c :
                    ColorUtil.invertColor(vc.getBackground());
                VisUtil.setColor(gl, c);
                gl.glRasterPos2d(x,y + frag.getDescent(glut) + line.leading);
                glut.glutBitmapString(frag.font, frag.s);
                x += frag.getWidth(glut);
            }
        }

        // restore original matrices
        gl.glPopAttrib();
        gl.glMatrixMode(gl.GL_MODELVIEW);
        gl.glPopMatrix();
        gl.glMatrixMode(gl.GL_PROJECTION);
        gl.glPopMatrix();
        gl.glMatrixMode(gl.GL_MODELVIEW);

        VisUtil.popGLWholeState(gl);
    }

    public VisText()
    {}

    public void serialize(LCMDataOutputStream out) throws IOException
    {
        out.writeBoolean(pos != null);
        if (pos != null) {
            out.writeInt(pos.length);
            for (double d : pos)
                out.writeDouble(d);
        }
        out.writeInt(anchor.ordinal());

        // Now write each line:
        out.writeInt(lines.size());
        for (Line line : lines) {
            out.writeInt(line.fragments.size());
            for (StyledFragment frag : line.fragments) {
                out.writeInt(frag.font);
                out.writeBoolean(frag.c != null);
                if (frag.c != null)
                    out.writeInt(frag.c.getRGB());
                out.writeStringZ(frag.s);
                out.writeInt(frag.width);
            }
            out.writeInt(line.justification.ordinal());
            out.writeInt(line.leading);
        }
    }

    public void unserialize(LCMDataInputStream in) throws IOException
    {
        if (in.readBoolean()) {
            pos = new double[in.readInt()];
            for (int i =0; i < pos.length; i++)
                pos[i] = in.readDouble();
        }
        anchor = getAnchor(in.readInt());

        //Now read each line:
        int nlines = in.readInt();
        for(int i =0; i < nlines; i++) {
            Line line = new Line();
            int nfrags = in.readInt();
            for (int j =0; j <nfrags; j++) {
                StyledFragment styled = new StyledFragment();
                styled.font = in.readInt();
                if (in.readBoolean())
                    styled.c = new Color(in.readInt(), true);
                styled.s = in.readStringZ();
                styled.width = in.readInt();
                line.fragments.add(styled);
            }
            line.justification = getJustification(in.readInt());
            line.leading = in.readInt();
            lines.add(line);
        }
    }

    // Converting to enums from ints
    ANCHOR getAnchor(int v)
    {
        for (ANCHOR a : ANCHOR.values())
            if (a.ordinal() == v)
                return a;
        assert(false);
        return ANCHOR.CENTER;
    }

    JUSTIFICATION getJustification(int v)
    {
        for (JUSTIFICATION j : JUSTIFICATION.values())
            if (j.ordinal() == v)
                return j;
        assert(false);
        return JUSTIFICATION.CENTER;
    }
}
