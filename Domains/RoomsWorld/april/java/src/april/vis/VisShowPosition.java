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

public class VisShowPosition implements VisObject
{
    String visTextFormat;

    public VisShowPosition()
    {
        this("(%f, %f, %f)");
    }

    /** visTextFormat is a VisText format string taking three floats x, y, and z. E.g., "%f %f %f" **/
    public VisShowPosition(String visTextFormat)
    {
        this.visTextFormat = visTextFormat;
    }

    public void render(VisContext vc, GL gl, GLU glu)
    {
        VisView vv = vc.getRenderingView();

        // model_matrix = view.modelMatrix * transformationMatrix
        //
        // our model matrix = V*T
        double model_matrix[] = new double[16];
        gl.glGetDoublev(GL.GL_MODELVIEW_MATRIX, model_matrix, 0);

        Matrix M = Matrix.columnPackedMatrix(model_matrix, 4, 4);
        Matrix T = vv.getModelViewMatrix().inverse().times(M);
        String s = String.format(visTextFormat, T.get(0,3), T.get(1,3), T.get(2,3));

        VisText vt = new VisText(new double[] {0,0,0}, VisText.ANCHOR.CENTER, s);

        vt.render(vc, gl, glu);
    }
}

