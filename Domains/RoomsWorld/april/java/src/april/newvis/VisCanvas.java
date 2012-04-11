package april.newvis;

import java.awt.*;
import java.awt.geom.*;
import java.awt.event.*;
import java.awt.image.*;
import java.io.*;
import java.util.*;
import java.util.zip.*;
import java.nio.channels.*;
import java.nio.*;

import javax.imageio.*;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import javax.swing.*;

import com.sun.opengl.util.*;

import april.jmat.geom.*;
import april.jmat.*;
import april.image.*;
import april.util.*;

public class VisCanvas extends JPanel implements MouseMotionListener,
                                      MouseListener,
                                      MouseWheelListener,
                                      KeyListener
{
    public DefaultLayerLayoutManager layerLayoutManager = new DefaultLayerLayoutManager(this);

    GLAutoDrawable  canvas;

    int aaLevel = 0;        // what anti-aliasing level? (0 = off)
    boolean debug = false;  // look for bugs
    boolean faster = false; // try to run faster (at the expense of quality)

    ArrayList<VisLayerLayoutManager.Position> layerPositions;

    static
    {
        JoglLoader.initialize();
    }

    public VisCanvas()
    {
        debug = EnvUtil.getProperty("vis.debug", false);
        aaLevel = EnvUtil.getProperty("vis.aalevel", aaLevel);
        faster = EnvUtil.getProperty("vis.faster", faster);

        if (debug) {
            System.out.println("VisCanvas: aalevel "+aaLevel+", faster "+faster);
        }

        GLCapabilities capsv[] = new GLCapabilities[] {
            makeCapabilities(32, true, true, aaLevel),
            makeCapabilities(24, true, true, aaLevel),
            makeCapabilities(16, true, true, aaLevel),
            makeCapabilities(32, true, true, 0),
            makeCapabilities(24, true, true, 0),
            makeCapabilities(16, true, true, 0),
            makeCapabilities(24, false, false, 0),
            makeCapabilities(16, false, false, 0) };

        if (EnvUtil.getProperty("vis.glcanvas", true)) {
            canvas = createGLCanvasWorkaround(capsv);
        } else {
            // this doesn't seem to be nearly as buggy as GLCanvas
            canvas = new GLJPanel(capsv[0]);
        }

        canvas.addGLEventListener(new MyGLEventListener());

        setLayout(new BorderLayout());
        add((Component) canvas, BorderLayout.CENTER);

        // allow us to capture tab and shift-tab events, instead of
        // cycling through text fields.
        ((Component) canvas).setFocusTraversalKeysEnabled(false);

    }

    GLCapabilities makeCapabilities(int depthbits, boolean hwaccel, boolean doublebuffered, int aalevel)
    {
        GLCapabilities caps = new GLCapabilities();
        caps.setAlphaBits(8);
        caps.setRedBits(8);
        caps.setBlueBits(8);
        caps.setGreenBits(8);
        caps.setHardwareAccelerated(hwaccel);
        caps.setDoubleBuffered(doublebuffered);
        caps.setDepthBits(depthbits);
        caps.setSampleBuffers(aalevel > 0);
        if (aalevel > 0)
            caps.setNumSamples(aalevel);

        return caps;
    }

    GLCanvas createGLCanvasWorkaround(GLCapabilities capsv[])
    {
        for (GLCapabilities cap : capsv) {
            try {
                GLCanvas canvas = new GLCanvas(cap);
                if (debug)
                    System.out.println("GLCanvas has accepted: "+cap);
                return canvas;
            } catch (Exception ex) {
                System.out.println("GLCanvas has rejected "+cap);
            }
        }

        // last resort
        return new GLCanvas();
    }

    protected class MyGLEventListener implements GLEventListener
    {
        long last_draw_mtime = System.currentTimeMillis();
        double fps_dt = 0.1;

        VisObject reshapeText;

        public void init(GLAutoDrawable drawable)
        {
        }

        public void display(GLAutoDrawable drawable)
        {
            GL gl = drawable.getGL();
            GLU glu = new GLU();

            if (debug) {
                gl = new DebugGL(gl);
                System.out.println("VisCanvas.display");
            }

            gl.glEnable(GL.GL_NORMALIZE);

            gl.glEnable(GL.GL_LIGHTING);
            gl.glLightModeli(GL.GL_LIGHT_MODEL_TWO_SIDE, GL.GL_TRUE);
            gl.glEnable(GL.GL_COLOR_MATERIAL);
            gl.glColorMaterial(GL.GL_FRONT_AND_BACK, GL.GL_AMBIENT_AND_DIFFUSE);
            gl.glMaterialf(GL.GL_FRONT_AND_BACK, GL.GL_SHININESS, 0);
            gl.glMaterialfv(GL.GL_FRONT_AND_BACK, GL.GL_SPECULAR, new float[] {.1f, .1f, .1f, .1f}, 0);

            gl.glDepthFunc(GL.GL_LEQUAL);
            gl.glEnable(GL.GL_BLEND);
            gl.glBlendFunc(GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA);
            gl.glEnable(GL.GL_DEPTH_TEST);

            gl.glPolygonMode(GL.GL_FRONT, GL.GL_FILL);
            gl.glPolygonMode(GL.GL_BACK, GL.GL_FILL);

            gl.glDisable(GL.GL_LINE_STIPPLE);

            gl.glShadeModel(GL.GL_SMOOTH);

            if (faster) {
                gl.glDisable(GL.GL_POINT_SMOOTH);
                gl.glDisable(GL.GL_LINE_SMOOTH);
            } else {
                gl.glEnable(GL.GL_POINT_SMOOTH);
                gl.glHint(GL.GL_POINT_SMOOTH_HINT, GL.GL_NICEST);

                gl.glEnable(GL.GL_LINE_SMOOTH);
                gl.glHint(GL.GL_LINE_SMOOTH_HINT, GL.GL_NICEST);
            }

            int viewport[] = new int[4];
            gl.glGetIntegerv(gl.GL_VIEWPORT, viewport, 0);
            layerPositions = layerLayoutManager.layout(viewport);

            gl.glEnable(GL.GL_SCISSOR_TEST);

            for (VisLayerLayoutManager.Position layerPosition : layerPositions) {

                // x, y, width, height. remember 0,0 is lower left.
                gl.glScissor(layerPosition.viewport[0], layerPosition.viewport[1],
                             layerPosition.viewport[2], layerPosition.viewport[3]);

                ///////////////////////////////////////////////////////////////////
                // XXX make the clears conditional, so a layer can be transparent.

                // reminder: alpha is 4th channel. 1.0=opaque.
                Color backgroundColor = layerPosition.layer.backgroundColor;
                gl.glClearColor(backgroundColor.getRed()/255f,
                                backgroundColor.getGreen()/255f,
                                backgroundColor.getBlue()/255f,
                                1.0f);

                gl.glClearDepth(1.0f);
                gl.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT);

                VisWorld vw = layerPosition.layer.world;

                // set up lighting
                for (int i = 0; i < vw.lights.size(); i++) {
                    VisLight light = vw.lights.get(i);
                    gl.glLightfv(GL.GL_LIGHT0 + i, GL.GL_POSITION, light.position, 0);
                    gl.glLightfv(GL.GL_LIGHT0 + i, GL.GL_AMBIENT, light.ambient, 0);
                    gl.glLightfv(GL.GL_LIGHT0 + i, GL.GL_DIFFUSE, light.diffuse, 0);
                    gl.glLightfv(GL.GL_LIGHT0 + i, GL.GL_SPECULAR, light.specular, 0);

                    gl.glEnable(GL.GL_LIGHT0 + i);
                }

                // position the camera
                VisCameraState cameraState = layerPosition.layer.cameraManager.getCameraState(VisCanvas.this, viewport);

                // draw the objects
                vw.render(VisCanvas.this, gl, glu);

                // undo our lighting
                for (int i = 0; i < vw.lights.size(); i++) {
                    gl.glDisable(GL.GL_LIGHT0 + i);
                }
            }
        }

        public void reshape(GLAutoDrawable drawable, int i, int x, int width, int height)
        {
/*
            if (reshapeText != null)
                world.removeTemporary(reshapeText);
            reshapeText = new VisContextSpecific(VisCanvas.this, new VisText(VisText.ANCHOR.CENTER, ""+width+" x "+height));

            world.addTemporary(reshapeText, 1.0);
*/

            if (debug)
                System.out.printf("VisCanvas.reshape (%d x %d)\n", width, height);
        }

        public void displayChanged(GLAutoDrawable drawable, boolean modeChanged, boolean deviceChanged)
        {
            System.out.println("displayChanged");
        }

    }

    public void draw()
    {
        canvas.repaint();
    }

    public void keyPressed(KeyEvent e)
    {
    }

    public void keyReleased(KeyEvent e)
    {
    }

    public void keyTyped(KeyEvent e)
    {
    }

    public void mouseWheelMoved(MouseWheelEvent e)
    {
    }

    public void mouseDragged(MouseEvent e)
    {
    }

    public void mouseMoved(MouseEvent e)
    {
    }

    public void mousePressed(MouseEvent e)
    {
        int x = e.getX(), y = e.getY();
        for (VisLayerLayoutManager.Position layerPosition : layerPositions) {

        }
    }

    public void mouseReleased(MouseEvent e)
    {
    }

    public void mouseClicked(MouseEvent e)
    {
    }

    public void mouseEntered(MouseEvent e)
    {
        ((Component) canvas).requestFocus();
    }

    public void mouseExited(MouseEvent e)
    {
    }

}
