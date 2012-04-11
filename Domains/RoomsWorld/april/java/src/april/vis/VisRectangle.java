package april.vis;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import java.awt.*;
import java.util.*;

import april.jmat.*;
import april.jmat.geom.*;

public class VisRectangle extends VisData
{
    public VisRectangle() {}
    public VisRectangle(double xy0[], double xy1[], VisDataStyle style)
    {
        points.add(new double[] { xy0[0], xy0[1]});
        points.add(new double[] { xy0[0], xy1[1]});
        points.add(new double[] { xy1[0], xy1[1]});
        points.add(new double[] { xy1[0], xy0[1]});
        styles.add(style);
    }

    public VisRectangle(double sizex, double sizey, VisDataStyle style)
    {
        points.add(new double[] { -sizex/2, -sizey/2 });
        points.add(new double[] { sizex/2, -sizey/2 });
        points.add(new double[] { sizex/2, sizey/2 });
        points.add(new double[] { -sizex/2, sizey/2 });

        styles.add(style);
    }

    public void render(VisContext vc, GL gl, GLU glu)
    {
        for (VisDataStyle style : styles) {
            if (style instanceof VisDataLineStyle)
                ((VisDataLineStyle) style).loop = true;

            style.renderStyle(vc, gl, glu, this);
        }
    }
}
