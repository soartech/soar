package april.vis;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import com.sun.opengl.util.*;

import april.jmat.geom.*;

import java.awt.*;
import java.util.*;
import java.io.*;
import java.nio.*;

import lcm.lcm.*;

/** VisObject representing a robot. **/
public class VisRobot implements VisObject, VisSerializable
{
    public Color color = new Color(40, 40, 100);
    public Color outlineColor = new Color(40, 40, 255);

    int  selectedState; // 0 = none, 1 = hover, 2 = select

    public VisRobot()
    {
    }

    public VisRobot(Color color, Color outlineColor)
    {
        this.color = color;
        this.outlineColor = outlineColor;
    }

    public VisRobot(Color color)
    {
        this.color = color;
        this.outlineColor = color.brighter();
    }

    public void setSelectedState(int v)
    {
        this.selectedState = v;
    }

    public void render(VisContext vc, GL gl, GLU glu)
    {
        double length = 0.6;
        double width = .35;

        if (selectedState == 1) {
            VisUtil.setColor(gl, VisUtil.hoverColor);
        } else if (selectedState == 2) {
            VisUtil.setColor(gl, VisUtil.pickColor);
        }

        if (selectedState > 0) {
            gl.glBegin(gl.GL_QUADS);
            gl.glVertex2d(-length, -width);
            gl.glVertex2d(-length, width);
            gl.glVertex2d(length, width);
            gl.glVertex2d(length, -width);
            gl.glEnd();
        }

        gl.glLineWidth(1f);

        VisUtil.setColor(gl, color);
        gl.glBegin(gl.GL_TRIANGLES);
        gl.glVertex2d(-length/2, width/2);
        gl.glVertex2d(length/2, 0);
        gl.glVertex2d(-length/2, -width/2);
        gl.glEnd();

        VisUtil.setColor(gl, outlineColor);
        gl.glBegin(gl.GL_LINE_LOOP);
        gl.glVertex2d(-length/2, width/2);
        gl.glVertex2d(length/2, 0);
        gl.glVertex2d(-length/2, -width/2);
        gl.glEnd();
    }

    public void serialize(LCMDataOutputStream out) throws IOException
    {
        out.writeInt(color.getRGB());
        out.writeInt(outlineColor.getRGB());
        out.writeInt(selectedState);
    }

    public void unserialize(LCMDataInputStream in) throws IOException
    {
        color = new Color(in.readInt(), true);
        outlineColor = new Color(in.readInt(), true);
        selectedState = in.readInt();
    }
}
