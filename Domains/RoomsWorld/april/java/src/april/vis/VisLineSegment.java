/**
 *
 */
package april.vis;

import java.awt.Color;

import javax.media.opengl.GL;
import javax.media.opengl.glu.GLU;

/**
 * @author rpradeep
 *
 */
public class VisLineSegment implements VisObject
{
    double[] begin;
    double[] end;
    Color color;
    float lineWidth = 1;

    /**
     *
     * @param begin
     *            start of line segment as double[3]
     * @param end
     *            end of line segment as double[3]
     * @param color
     */
    public VisLineSegment(double[] begin, double end[], Color color)
    {
        this(begin, end, color, 1);
    }

    public VisLineSegment(double[] begin, double end[], Color color, float lineWidth)
    {
        this.begin = begin;
        this.end = end;
        this.color = color;
        this.lineWidth = lineWidth;
    }

    public VisLineSegment(double x1, double y1, double z1, double x2, double y2, double z2,
                          Color color)
    {
        this(x1, y1, z1, x2, y2, z2, color, 1);
    }

    public VisLineSegment(double x1, double y1, double z1, double x2, double y2, double z2,
                          Color color, float lineWidth)
    {
        begin = new double[] {x1,y1,z1};
        end = new double[] {x2,y2,z2};
        this.color = color;
        this.lineWidth = lineWidth;
    }

    public void render(VisContext vc, GL gl, GLU glu)
    {
        gl.glLineWidth(lineWidth);

        VisUtil.setColor(gl, color);
        gl.glBegin(GL.GL_LINE_STRIP);
        gl.glVertex3d(begin[0], begin[1], begin[2]);
        gl.glVertex3d(end[0], end[1], end[2]);
        gl.glEnd();
    }

}
