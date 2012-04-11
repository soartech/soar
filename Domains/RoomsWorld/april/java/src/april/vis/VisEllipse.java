package april.vis;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import java.awt.*;
import java.util.*;

import april.jmat.*;
import april.jmat.geom.*;

/** A 2D ellipse. **/
public class VisEllipse extends VisData
{
    double center[];
    double xx, xy, yy;

    public VisEllipse(double center[], double radius, VisDataStyle style)
    {
        this(center, 1.0/Math.pow(radius, 2), 0, 1.0/Math.pow(radius, 2), style);
    }

    /** The ellipse has the equation:

        A*x^2 + B*x*y + C*y^2 = 1

    **/
    public VisEllipse(double center[], double xx, double xy, double yy, VisDataStyle style)
    {
        this.styles.add(style);
        this.center = center;
        this.xx = xx;
        this.yy = yy;
        this.xy = xy;

        int steps = 100;
        double theta = 0;
        double thetastep = 2*Math.PI/steps;

        for (int step=0; step<steps; step++)
            addPoint(step*thetastep);

        // complete the loop
        addPoint(0);
    }

    void addPoint(double theta)
    {
        double c = Math.cos(theta);
        double s = Math.sin(theta);
        double r = Math.sqrt(1/(xx*c*c+xy*c*s+yy*s*s));
        points.add(new double[] { r*c + center[0], r*s + center[1] });
    }

    public void render(VisContext vc, GL gl, GLU glu)
    {
        for (VisDataStyle style : styles) {
            style.renderStyle(vc, gl, glu, this);
        }
    }
}
