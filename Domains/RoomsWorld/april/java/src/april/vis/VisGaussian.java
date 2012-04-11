package april.vis;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import java.awt.*;
import java.util.*;

import april.jmat.*;
import april.jmat.geom.*;

/** 2D Gaussian ellipse. **/
public class VisGaussian extends VisData
{
    double mx, my;

    double xx, xy, yy;

    double sigmas[];

    public int steps = 100;

    public VisGaussian(Matrix M, VisDataStyle style)
    {
        this(new MultiGaussian(M), style);
    }

    public VisGaussian(MultiGaussian mg, VisDataStyle style)
    {
        this(mg, style, new double[] {1});
    }

    public VisGaussian(MultiGaussian mg, VisDataStyle style, double sigmas[])
    {
        this.styles.add(style);
        this.sigmas = sigmas;

        Matrix P = mg.getCovariance();
        double u[] = mg.getMean();

        mx = u[0]; // mean x, y
        my = u[1];

        double mt = u.length == 3 ? u[2] : 0;  // mean theta
        double vt = u.length == 3 ? Math.sqrt(P.get(2,2)) : 0; // var t

        boolean threed = u.length == 3;

        if (u.length > 3) {
            System.out.println("VisGaussian Warning: MultiGaussian is more than 3 dimensional");
        }

        double adbc = P.get(0,0)*P.get(1,1)-P.get(0,1)*P.get(1,0);

        // singular?
        if (adbc < 0.0000001)
            return;

        Matrix Pinv = mg.getPinv();
        xx = Pinv.get(0,0);
        xy = 2*Pinv.get(1,0);
        yy = Pinv.get(1,1);

        double theta=mt;
        double thetastep = 2*Math.PI/steps;

        for (int step=0; step<steps; step++)
            addPoint(mt + step*thetastep);

        // complete the loop
        addPoint(mt);

        // draw some lines showing error in theta
        if (threed) {
            points.add(new double[2]);
            addPoint(mt-vt*sigmas[0]);
            points.add(new double[2]);
            addPoint(mt+vt*sigmas[0]);
        }
    }

    void addPoint(double theta)
    {
        double c = Math.cos(theta);
        double s = Math.sin(theta);
        double r = Math.sqrt(1/(xx*c*c+xy*c*s+yy*s*s));
        points.add(new double[] { r*c, r*s });
    }

    public void render(VisContext vc, GL gl, GLU glu)
    {
        for (double sigma: sigmas) {
            gl.glPushMatrix();
            gl.glTranslated(mx, my, 0);
            gl.glScaled(sigma, sigma, sigma);

            for (VisDataStyle style : styles)
                style.renderStyle(vc, gl, glu, this);

            gl.glPopMatrix();
        }
    }
}
