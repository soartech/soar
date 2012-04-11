package april.vis;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import java.awt.*;
import april.jmat.geom.*;

import java.util.*;

/** VisObject representing a hersheyText. **/
public class VisHersheyText implements VisObject
{
    HersheyFont hf;
    String s;
    int cidx = -1;
    VisDataLineStyle lstyle;
    int defaultWidth = 10; // amount to step in the x direction for unknown characters.
    double scale = 1.0;

    /** Display a string, assuming that index 0 corresponds to ascii space. **/
    public VisHersheyText(HersheyFont hf, String s, VisDataLineStyle lstyle)
    {
        this.hf = hf;
        this.s = s;
        this.lstyle = lstyle;
    }

    /** Display a specific character, specified by its index. **/
    public VisHersheyText(HersheyFont hf, int cidx, VisDataLineStyle lstyle)
    {
        this.hf = hf;
        this.cidx = cidx;
        this.lstyle = lstyle;
    }

    double renderIndex(GL gl, double xoffset, int idx)
    {
        if (idx < 0 || idx >= hf.cdatas.size())
            return defaultWidth;

        byte cdata[] = hf.cdatas.get(idx);

        if (cdata == null)
            return defaultWidth;

        gl.glBegin(GL.GL_LINE_STRIP);

        // The characters plot relative to (0,0). So we move xoffset
        // over by the left-most extent *before* we draw the character.
        // Reminder: cdata[0] is negative.
        xoffset += -scale*cdata[0];

        for (int i = 2; i < cdata.length; i+=2) {
            int x0 = cdata[i];
            int y0 = cdata[i+1];

            if (x0 == 127) {
                gl.glEnd();
                gl.glBegin(GL.GL_LINE_STRIP);
                continue;
            }
            gl.glVertex2d(scale*x0 + xoffset, -scale*y0);
        }

        gl.glEnd();

        // The total amount that the cursor should be moved is the
        // size of the character.
        xoffset += scale*cdata[1];

        return xoffset;
    }

    public double getHeight()
    {
        return hf.getMaxCharacterHeight();
    }

    public double getWidth()
    {
        int width = 0;

        for (int sidx = 0; sidx < s.length(); sidx++) {
            int cidx = -32 + (s.charAt(sidx)&0xffff);

            width += hf.getCharacterWidth(cidx);
        }

        return width;
    }

    public void render(VisContext vc, GL gl, GLU glu)
    {
        double xoffset = 0;

        VisUtil.setColor(gl, lstyle.c);
        gl.glLineWidth(lstyle.width);

        if (s != null) {
            for (int sidx = 0; sidx < s.length(); sidx++) {
                int cidx = -32 + (s.charAt(sidx)&0xffff);
                xoffset = renderIndex(gl, xoffset, cidx);
            }
        }

        if (cidx >=0 ) {
            xoffset += renderIndex(gl, xoffset, cidx);
        }
    }
}
