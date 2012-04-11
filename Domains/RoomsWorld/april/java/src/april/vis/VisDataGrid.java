package april.vis;

import java.awt.Color;

import javax.media.opengl.GL;
import javax.media.opengl.glu.GLU;


/**
 * Plots data bars on a two dimensional grid. Each datum in a cell can be described using a value
 * and a color. The value is interpreted as the height of the bar at that cell.
 * 
 * @author pradeep
 * 
 */
public class VisDataGrid implements VisObject
{
    final double values[];
    final Color colors[];
    final double ox;
    final double oy;
    final int width;
    final int height;
    final double dx;
    final double dy;
    final boolean fSolidBars;
   
    public VisDataGrid(double x, double y, int width, int height, double cellWidth, double cellHeight, boolean fSolidBars)
    {
        this.fSolidBars = fSolidBars;
        this.ox = x;
        this.oy = y;
        this.width = width;
        this.height = height;
        
        values = new double[width*height];
        colors = new Color[width*height];
        
        dx = cellWidth;
        dy = cellHeight;
    }
    
    public void set (int x, int y, double val, Color fill)
    {
        values[y*width + x] = val;
        colors[y*width + x] = fill;
    }
    
    @Override
    public void render(VisContext context, GL gl, GLU glu)
    {
        if (fSolidBars)
            renderSolid(gl);
        else
            renderFrame(gl);
    }

    private void renderSolid(GL gl)
    {
        gl.glBegin(GL.GL_QUADS);

        for(int j=0; j<height; ++j)
            for (int i=0; i<width; ++i) {
                double v = values[j*width + i];
                Color c = colors[j*width + i];
                
                if (c==null) {
                    c = Color.black;
                    v = 0;
                }
                
                final double x = ox + i*dx;
                final double y = oy + j*dy; 
                
                VisUtil.setColor(gl, c);

                gl.glNormal3d (0, 0, -1);
                gl.glVertex3d (x,    y+dy, 0);
                gl.glVertex3d (x+dx, y+dy, 0);
                gl.glVertex3d (x+dx, y,    0);
                gl.glVertex3d (x,    y,    0);

                gl.glNormal3d (0, 1, 0);
                gl.glVertex3d (x,    y,    0);
                gl.glVertex3d (x+dx, y,    0);
                gl.glVertex3d (x+dx, y,    v);
                gl.glVertex3d (x,    y,    v);

                gl.glNormal3d (1, 0, 0);
                gl.glVertex3d (x,    y,    v);
                gl.glVertex3d (x,    y+dy, v);
                gl.glVertex3d (x,    y+dy, 0);
                gl.glVertex3d (x,    y,    0);

                gl.glNormal3d (1, 0, 0);
                gl.glVertex3d (x+dx, y+dy, v);
                gl.glVertex3d (x+dx, y,    v);
                gl.glVertex3d (x+dx, y,    0);
                gl.glVertex3d (x+dx, y+dy, 0);

                gl.glNormal3d (0, 1, 0);
                gl.glVertex3d (x+dx, y+dy, 0);
                gl.glVertex3d (x,    y+dy, 0);
                gl.glVertex3d (x,    y+dy, v);
                gl.glVertex3d (x+dx, y+dy, v);

                gl.glNormal3d (0, 0, 1);
                gl.glVertex3d (x,    y,    v);
                gl.glVertex3d (x+dx, y,    v);
                gl.glVertex3d (x+dx, y+dy, v);
                gl.glVertex3d (x,    y+dy, v);
            }
        
        gl.glEnd();
    }
    
    private void renderFrame(GL gl)
    {
        for(int j=0; j<height; ++j)
            for (int i=0; i<width; ++i) {
                double v = values[j*width + i];
                Color c = colors[j*width + i];
                
                if (c==null) {
                    c = Color.black;
                    v = 0;
                }
                
                final double x = ox + i*dx;
                final double y = oy + j*dy; 

                VisUtil.setColor(gl, c);

                // TODO: Use only GL_LINES for performance
                gl.glBegin (GL.GL_LINE_LOOP);
                gl.glVertex3d (x,    y,    0);
                gl.glVertex3d (x+dx, y,    0);
                gl.glVertex3d (x+dx, y+dy, 0);
                gl.glVertex3d (x,    y+dy, 0);
                gl.glEnd ();
                
                gl.glBegin (GL.GL_LINE_LOOP);
                gl.glVertex3d (x,    y,    v);
                gl.glVertex3d (x+dx, y,    v);
                gl.glVertex3d (x+dx, y+dy, v);
                gl.glVertex3d (x,    y+dy, v);
                gl.glEnd ();
                
                gl.glBegin (GL.GL_LINES);
                gl.glVertex3d (x,    y,    0);
                gl.glVertex3d (x,    y,    v);
                gl.glVertex3d (x+dx, y,    0);
                gl.glVertex3d (x+dx, y,    v);
                gl.glVertex3d (x+dx, y+dy, 0);
                gl.glVertex3d (x+dx, y+dy, v);
                gl.glVertex3d (x,    y+dy, 0);
                gl.glVertex3d (x,    y+dy, v);
                gl.glEnd ();                
            }
    }
}
