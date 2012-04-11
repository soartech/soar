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

import lcm.lcm.*;

/** Implements a background grid. You shouldn't need to instantiate
 * this yourself, as VisCanvas provides controls for enabling and
 * disabling a grid.
 **/
public class VisGrid implements VisObject, VisSerializable
{
    // default spacing for grid. If zero, we'll auto adjust.
    double spacing;

    public boolean drawGrid = true;
    public boolean drawGround = false;
    public Color groundColor = new Color(50, 50, 50);
    public Color gridColor = new Color(128,128,128);

    // if autoColor, ground/grid colors are chosen automatically.
    public boolean autoColor = true;

    public boolean textHints = true;

    public VisGrid(double spacing)
    {
        this.spacing = spacing;
    }

    public VisGrid()
    {
        // auto spacing when spacing = 0
    }

    /** round the input number to the next number of the form 1*10^n,
     * 2*10^n, or 5*10^n. */
    static double round_to_125(double in)
    {
        double v = 0.1; // minimum allowable value. Must be of form 1*10^n.

        while (v < in) {
            if (v < in)
                v *= 2;
            if (v < in)
                v = v/2 * 5;
            if (v < in)
                v *= 2;
        }

        return v;
    }

    VisObject gridText;
    double  lastSpacing = -1;
    public void render(VisContext vc, GL gl, GLU glu)
    {
        // when looking directly down at the world, about how many grids
        // should appear across the screen?
        double grids_per_screen = 10;

        VisViewManager viewManager = vc.getViewManager();
        VisWorld world = vc.getWorld();

        double eye_dist = LinAlg.distance(viewManager.viewGoal.eye, viewManager.viewGoal.lookAt);
        double meters_per_grid = round_to_125(eye_dist / grids_per_screen );

        if (spacing != 0)
            meters_per_grid = spacing;

        if (textHints) {
            if (meters_per_grid != lastSpacing && (drawGrid || drawGround)) {
                if (gridText != null)
                    world.removeTemporary(gridText);
                // the newlines ensure we don't stomp on VisCanvas's window size
                String label = String.format("Grid: %d m", (int) meters_per_grid);
                if (meters_per_grid < 1)
                    label = String.format("Grid: %.1f m", meters_per_grid);

                gridText = new VisContextSpecific(vc, new VisText(VisText.ANCHOR.CENTER, label));
                world.addTemporary(gridText, 1.0);
                lastSpacing = meters_per_grid;
            }
        }

        double grid_ox = Math.ceil (viewManager.viewGoal.lookAt[0] / meters_per_grid) * meters_per_grid;
        double grid_oy = Math.ceil (viewManager.viewGoal.lookAt[1] / meters_per_grid) * meters_per_grid;
        double grid_oz = 0; // vc.view.lookAt[2];
        int num_lines = 500;

        if (drawGround) {
            if (autoColor)
                VisUtil.setColor(gl, ColorUtil.towardsGray(vc.getBackground(), .25));
            else
                VisUtil.setColor(gl, groundColor);

            gl.glNormal3d(0, 0, 1);
            gl.glBegin(gl.GL_TRIANGLES);
            gl.glVertex3d(grid_ox + (-num_lines/2)*meters_per_grid,
                          grid_oy + (-num_lines/2)*meters_per_grid,
                          grid_oz);
            gl.glVertex3d(grid_ox + (+num_lines/2)*meters_per_grid,
                          grid_oy + (-num_lines/2)*meters_per_grid,
                          grid_oz);
            gl.glVertex3d(grid_ox + (+num_lines/2)*meters_per_grid,
                          grid_oy + (+num_lines/2)*meters_per_grid,
                          grid_oz);
            gl.glVertex3d(grid_ox + (-num_lines/2)*meters_per_grid,
                          grid_oy + (-num_lines/2)*meters_per_grid,
                          grid_oz);
            gl.glVertex3d(grid_ox + (+num_lines/2)*meters_per_grid,
                          grid_oy + (+num_lines/2)*meters_per_grid,
                          grid_oz);
            gl.glVertex3d(grid_ox + (-num_lines/2)*meters_per_grid,
                          grid_oy + (+num_lines/2)*meters_per_grid,
                          grid_oz);
            gl.glEnd();
        }

        if (drawGrid) {
            grid_oz += .001;

            gl.glLineWidth (1);
            gl.glBegin(gl.GL_LINES);

            if (autoColor)
                VisUtil.setColor(gl, ColorUtil.towardsGray(vc.getBackground(), .5));
            else
                VisUtil.setColor(gl, gridColor);

            double big = Math.max(1000, eye_dist * 200);

            for (int i=0; i<num_lines; i++) {
                gl.glVertex3d(grid_ox + (-num_lines/2 + i) * meters_per_grid,
                              grid_oy - big, grid_oz);
                gl.glVertex3d(grid_ox + (-num_lines/2 + i) * meters_per_grid,
                              grid_oy + big, grid_oz);

                gl.glVertex3d(grid_ox - big,
                              grid_oy + (-num_lines/2 + i) * meters_per_grid, grid_oz);
                gl.glVertex3d(grid_ox + big,
                              grid_oy + (-num_lines/2 + i) * meters_per_grid, grid_oz);
            }
            gl.glEnd ();
        }
    }

    public void serialize(LCMDataOutputStream out) throws IOException
    {
        out.writeDouble(spacing);
        out.writeBoolean(drawGrid);
        out.writeBoolean(drawGround);
        out.writeInt(groundColor.getRGB());
        out.writeInt(gridColor.getRGB());
        out.writeBoolean(autoColor);
        out.writeBoolean(textHints);
    }

    public void unserialize(LCMDataInputStream in) throws IOException
    {
        spacing = in.readDouble();
        drawGrid = in.readBoolean();
        drawGround = in.readBoolean();
        groundColor  = new Color(in.readInt(), true);
        gridColor = new Color(in.readInt(), true);
        autoColor = in.readBoolean();
        textHints = in.readBoolean();
    }

}
