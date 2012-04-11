package april.vis;

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
import april.util.*;
import april.image.*;

public class VisOffscreenCanvas implements VisContext
{
    int       width, height;
    VisWorld  vw;
    Color     backgroundColor = Color.white;

    VisViewManager viewManager;
    VisView   thisView;

    boolean faster = false;
    int aaLevel = 0;

    GLPbuffer pbuffer;

    ArrayList<RenderData> requests = new ArrayList<RenderData>();

    public boolean debug = false;

    int zdepth = 16;

    static
    {
        JoglLoader.initialize();
    }

    public static class RenderData
    {
        public BufferedImage im;
        public FloatImage depth;
    }

    public VisOffscreenCanvas(int width, int height, VisWorld vw)
    {
        this.width = width;
        this.height = height;
        this.vw = vw;
        this.viewManager = new VisViewManager(this);

        pbuffer = GLDrawableFactory.getFactory().createGLPbuffer(makeCapabilities(zdepth, true, true, aaLevel),
                                                                 null,
                                                                 width, height,
                                                                 null);

        pbuffer.addGLEventListener(new MyGLEventListener());
    }

    public void destroy()
    {
        pbuffer.destroy();
    }

    public void setWorld(VisWorld vw)
    {
        this.vw = vw;
    }

    public void setSize(int width, int height)
    {
        if (width == this.width && height == this.height)
            return;

        this.width = width;
        this.height = height;

        destroy();

        pbuffer = GLDrawableFactory.getFactory().createGLPbuffer(makeCapabilities(zdepth, true, true, aaLevel),
                                                                 null,
                                                                 width, height,
                                                                 null);
        pbuffer.addGLEventListener(new MyGLEventListener());
    }

    public int getWidth()
    {
        return width;
    }

    public int getHeight()
    {
        return height;
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

    class MyGLEventListener implements GLEventListener
    {
        public void init(GLAutoDrawable drawable)
        {
            if (debug)
                System.out.println("init");
        }

        public void display(GLAutoDrawable drawable)
        {
            if (debug)
                System.out.println("display");

            GL gl = drawable.getGL();
            GLU glu = new GLU();

            int viewport[] = new int[4];
            gl.glGetIntegerv(gl.GL_VIEWPORT, viewport, 0);
            viewManager.viewGoal.viewport = viewport;

            thisView = viewManager.getView();

            // reminder: alpha is 4th channel. 1.0=opaque.
            Color backgroundColor = getBackground();
            gl.glClearColor(backgroundColor.getRed()/255f,
                            backgroundColor.getGreen()/255f,
                            backgroundColor.getBlue()/255f,
                            1.0f);

            gl.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT);
            gl.glClearDepth(1.0f);

            gl.glEnable(GL.GL_NORMALIZE);

            gl.glEnable(GL.GL_LIGHTING);
            gl.glLightModeli(GL.GL_LIGHT_MODEL_TWO_SIDE, GL.GL_TRUE);
            gl.glEnable(GL.GL_COLOR_MATERIAL);
            gl.glColorMaterial(GL.GL_FRONT_AND_BACK, GL.GL_AMBIENT_AND_DIFFUSE);
            gl.glMaterialf(GL.GL_FRONT_AND_BACK, GL.GL_SHININESS, 0);

            for (int i = 0; i < vw.lights.size(); i++) {
                VisLight light = vw.lights.get(i);
                gl.glLightfv(GL.GL_LIGHT0 + i, GL.GL_POSITION, light.position, 0);
                gl.glLightfv(GL.GL_LIGHT0 + i, GL.GL_AMBIENT, light.ambient, 0);
                gl.glLightfv(GL.GL_LIGHT0 + i, GL.GL_DIFFUSE, light.diffuse, 0);
                gl.glLightfv(GL.GL_LIGHT0 + i, GL.GL_SPECULAR, light.specular, 0);

                gl.glEnable(GL.GL_LIGHT0 + i);
            }

            gl.glDepthFunc(GL.GL_LEQUAL);
            gl.glEnable(GL.GL_BLEND);
            gl.glBlendFunc(GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA);
            gl.glEnable(GL.GL_DEPTH_TEST);

            gl.glPolygonMode(GL.GL_FRONT_AND_BACK, GL.GL_FILL);

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

            /////////// PROJECTION MATRIX ////////////////
            gl.glMatrixMode(gl.GL_PROJECTION);
            gl.glLoadIdentity();
            gl.glMultMatrixd(thisView.getProjectionMatrix().getColumnPackedCopy(), 0);

            /////////// MODEL MATRIX ////////////////
            gl.glMatrixMode(gl.GL_MODELVIEW);
            gl.glLoadIdentity();
            gl.glMultMatrixd(thisView.getModelViewMatrix().getColumnPackedCopy(), 0);

            if (aaLevel > 0)
                gl.glEnable(GL.GL_MULTISAMPLE);


            //////// render
            vw.render(VisOffscreenCanvas.this, gl, glu);

            BufferedImage im = new BufferedImage(width, height,
                                                 BufferedImage.TYPE_INT_RGB);

            int imdata[] = ((DataBufferInt) im.getRaster().getDataBuffer()).getData();

            // read the BGR values into the image buffer
            gl.glPixelStorei(GL.GL_PACK_ALIGNMENT, 1);
            gl.glReadPixels(0, 0, width, height, GL.GL_BGRA,
                            GL.GL_UNSIGNED_INT_8_8_8_8_REV, IntBuffer.wrap(imdata));

            VisUtil.flipImage(width, height, imdata);

            synchronized (requests) {
                FloatImage depth = null;

                for (RenderData rd : requests) {
                    synchronized(rd) {
                        rd.im = im;

                        // They're requesting a depth buffer
                        if (rd.depth != null) {

                            // we need to compute the depth buffer
                            if (depth == null) {
                                float data[] = new float[width*height];
                                depth = new FloatImage(width, height, data);

                                int e1 = gl.glGetError();

                                gl.glPixelStorei(GL.GL_PACK_ALIGNMENT, 4);
                                gl.glReadPixels(0, 0, width, height, GL.GL_DEPTH_COMPONENT,
                                                GL.GL_FLOAT, FloatBuffer.wrap(data));

                                int e2 = gl.glGetError();

                                // convert from normalized z coordinates back to depth.
                                double f = thisView.zclip_far;
                                double n = thisView.zclip_near;

                                for (int i = 0; i < data.length; i++) {
                                    double pz = data[i];

                                    data[i] = (float) (-2*f*n / ((pz-.5)*2*(f-n)-(f+n)));
                                }

                                VisUtil.flipImage(width, height, data);
                            }

                            rd.depth = depth;

                        }

                        rd.notifyAll();
                    }
                }
            }
        }

        public void reshape(GLAutoDrawable drawable, int x, int y, int width, int height)
        {
            if (debug)
                System.out.println("reshape");
        }

        public void displayChanged(GLAutoDrawable drawable, boolean modeChanged, boolean deviceChanged)
        {
            if (debug)
                System.out.println("changed");
        }
    }

    public VisWorld getWorld()
    {
        return vw;
    }

    public VisViewManager getViewManager()
    {
        return viewManager;
    }

    /** No-op: we only render frames when explicitly requested by getFrame() **/
    public void draw()
    {
        // don't do anything.
    }

    public VisView getRenderingView()
    {
        return thisView;
    }

    public java.awt.Color getBackground()
    {
        return backgroundColor;
    }

    public RenderData getImageData(boolean needDepth)
    {
        RenderData rd = new RenderData();

        if (needDepth)
            rd.depth = new FloatImage(0,0); // will be replaced. non-null entry means it's needed.

        synchronized (requests) {
            requests.add(rd);
        }

        pbuffer.repaint();

        synchronized (rd) {
            if (rd.im == null) {
                try {
                    rd.wait();
                } catch (InterruptedException ex) {
                }
            }
        }

        synchronized (requests) {
            requests.remove(rd);
        }

        return rd;
    }

    public BufferedImage getImage()
    {
        RenderData rd = getImageData(false);
        return rd.im;
    }

    public static void main(String args[])
    {
        VisWorld vw = new VisWorld();
        VisOffscreenCanvas vc = new VisOffscreenCanvas(100, 300, vw);

        vc.getViewManager().viewGoal.lookAt(new double[] {0, 0, 4},
                                            new double[] {0, 0, 0},
                                            new double[] { 0, 1, 0 });

        VisWorld.Buffer vb = vw.getBuffer("foo");
        vb.addBuffered(new VisBox(0, 0, 0, 1, 1, 1, Color.blue));
        vb.switchBuffer();

        JFrame jf = new JFrame("VisOffscreenCanvas Test");
        jf.setLayout(new BorderLayout());

        RenderData rd = vc.getImageData(true);
        FloatImage depth = rd.depth;

        double pz = rd.depth.get(depth.width/2, depth.height/2);

        System.out.printf("%15f \n", pz);

//        JImage jim = new JImage(rd.depth.makeImage());
        JImage jim = new JImage(vc.getImage());
        jf.add(jim, BorderLayout.CENTER);
        jf.setSize(600,400);
        jf.setVisible(true);
    }
}
