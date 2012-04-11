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
import april.image.*;
import april.util.*;

/** A JComponent allowing a view into a VisWorld using JOGL **/
public class VisCanvas extends JPanel implements VisWorldListener,
                                      MouseMotionListener,
                                      MouseListener,
                                      MouseWheelListener,
                                      KeyListener,
                                      VisContext
{
    VisWorld       world;
    VisViewManager viewManager;
    VisView        thisView; // the view currently being rendered
    VisView        lastView; // the last fully drawn view
    GLAutoDrawable canvas;
    public VisCanvasDefaultEventHandler defaultEventHandler;

    VisCanvasEventHandler pickingHandler, hoveringHandler;

    MouseEvent lastMousePosition;

    VisCanvasPopupMenu popupMenu;

    java.util.Timer timer = new java.util.Timer();

    // used to determine whether we need to redraw the status line
    String lastStatusLine = "";

    ArrayList<VisCanvasEventHandler> eventHandlers =
        new ArrayList<VisCanvasEventHandler>();

    HashMap<VisCanvasEventHandler, Integer> eventHandlerPriorities =
        new HashMap<VisCanvasEventHandler, Integer>();

    long last_draw_mtime;
    boolean redrawPending = false;

    double targetFPS = 100; // will be overwritten by popup menu

    ArrayList<ImageCaptureObject> imageCaptureObjects = new ArrayList<ImageCaptureObject>();

    static
    {
        JoglLoader.initialize();
    }

    GZIPOutputStream movieOutputStream;
    boolean movieAutoFrames;
    boolean movieMakeFramePending;

    int aaLevel = 0;        // what anti-aliasing level? (0 = off)
    boolean debug = false;  // look for bugs
    boolean faster = false; // try to run faster (at the expense of quality)

    public VisCanvas(VisWorld world)
    {
        this.world = world;
        this.viewManager = new VisViewManager(this);

        debug = EnvUtil.getProperty("vis.debug", false);
        aaLevel = EnvUtil.getProperty("vis.aalevel", aaLevel);
        faster = EnvUtil.getProperty("vis.faster", faster);

        GLCapabilities capsv[] = new GLCapabilities[] {
            makeCapabilities(32, true, true, aaLevel),
            makeCapabilities(24, true, true, aaLevel),
            makeCapabilities(16, true, true, aaLevel),
            makeCapabilities(32, true, true, 0),
            makeCapabilities(24, true, true, 0),
            makeCapabilities(16, true, true, 0),
            makeCapabilities(24, false, false, 0),
            makeCapabilities(16, false, false, 0) };

        if (EnvUtil.getProperty("vis.glcanvas", true))
            canvas = createGLCanvasWorkaround(capsv);
        else
            canvas = new GLJPanel(capsv[0]); // this doesn't seem to be nearly as buggy as GLCanvas

        canvas.addGLEventListener(new MyGLEventListener());

        setBackground(Color.white);

        setLayout(new BorderLayout());
        add((Component) canvas, BorderLayout.CENTER);

        world.addListener(this);

        canvas.addMouseMotionListener(this);
        canvas.addMouseListener(this);
        canvas.addMouseWheelListener(this);
        canvas.addKeyListener(this);

        // allow us to capture tab and shift-tab events, instead of cycling through text fields.
        ((Component) canvas).setFocusTraversalKeysEnabled(false);

        defaultEventHandler = new VisCanvasDefaultEventHandler(this);
        addEventHandler(defaultEventHandler, -1);

        popupMenu = new VisCanvasPopupMenu(this);

        if (debug) {
            System.out.println("VisCanvas: aalevel "+aaLevel+", faster "+faster);
        }
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

    class MyCapabilitiesChooser implements GLCapabilitiesChooser
    {
        public int chooseCapabilities(GLCapabilities desired, GLCapabilities[] available, int recommended)
        {
            if (recommended < 0)
                recommended = 0;

            int best = recommended;
            double bestscore = 0;

            for (int i = 0 ; i < available.length; i++) {
                double thisscore = scoreCaps(available[i]);
                if (thisscore > bestscore) {
                    best = i;
                    bestscore = thisscore;
                }
            }

            if (debug)
                System.out.println("going with capabilities: "+available[best]);
            return best;
        }

        double scoreCaps(GLCapabilities c)
        {
            double score = 0;

            if (c == null)
                return score;

            if (c.getHardwareAccelerated())
                score += 100;

            // XXX !
            if (c.getNumSamples() > 0)
                score += 100 + c.getNumSamples();

            if (c.getDepthBits() >= 24)
                score += 200 + c.getDepthBits();

            if (c.getDepthBits() < 24)
                score += c.getDepthBits();

            score += c.getAlphaBits();
            //	    System.out.println(c+" "+score);

            return score;
        }
    }

    /** Get the view that will be used for future drawing operations. **/
    public VisViewManager getViewManager()
    {
        return viewManager;
    }

    /** Get the view that corresponds to the most recently rendered
     * frame. You should not modify this view.
     **/
    public VisView getLastView()
    {
        if (lastView == null)
            return viewManager.getView();

        return lastView;
    }

    /** Get the view currently being rendered. **/
    public VisView getRenderingView()
    {
        if (thisView == null)
            return viewManager.getView();

        return thisView;
    }

    public void setOrthographic(boolean b)
    {
        popupMenu.orthoItem.setState(b);
        popupMenu.orthographic();
    }

    public void setTargetFPS(double fps)
    {
        if (fps > targetFPS)
            drawNow();

        this.targetFPS = fps;
    }

    public double getTargetFPS()
    {
        return targetFPS;
    }

    public void worldChanged(VisWorld w)
    {
        draw();
    }

    public VisWorld getWorld()
    {
        return world;
    }

    /** Begin drawing (asynchronously) as soon as possible. **/
    public synchronized void drawNow()
    {
        canvas.repaint();
        last_draw_mtime = System.currentTimeMillis();
    }

    /** Schedule a redraw event according to the rendering rate
     * policy. The redraw will occur asynchronously**/
    public synchronized void draw()
    {
        if (redrawPending)
            return;

        long now = System.currentTimeMillis();
        double dt = (now - last_draw_mtime) / 1000.0;

        if (dt > 1.0 / targetFPS) {
            canvas.repaint();
            last_draw_mtime = now;
        } else {
            redrawPending = true;
            int ms = (int) ((1.0 / targetFPS - dt) * 1000.0);
            last_draw_mtime = now+ms;

            timer.schedule(new RedrawTask(), ms);
        }
    }

    class RedrawTask extends TimerTask
    {
        public void run()
        {
            synchronized(VisCanvas.this) {
                redrawPending = false;
            }
            VisCanvas.this.canvas.repaint();
        }
    }

    protected class MyGLEventListener implements GLEventListener
    {
        long last_draw_mtime = System.currentTimeMillis();
        double fps_dt = 0.1;

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

            gl.glClearDepth(1.0f);
            gl.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT); // |  GL.GL_ACCUM_BUFFER_BIT | GL.GL_STENCIL_BUFFER_BIT);

            gl.glEnable(GL.GL_NORMALIZE);

            gl.glEnable(GL.GL_LIGHTING);
            gl.glLightModeli(GL.GL_LIGHT_MODEL_TWO_SIDE, GL.GL_TRUE);
            gl.glEnable(GL.GL_COLOR_MATERIAL);
            gl.glColorMaterial(GL.GL_FRONT_AND_BACK, GL.GL_AMBIENT_AND_DIFFUSE);
            gl.glMaterialf(GL.GL_FRONT_AND_BACK, GL.GL_SHININESS, 0);
            gl.glMaterialfv(GL.GL_FRONT_AND_BACK, GL.GL_SPECULAR, new float[] {.1f, .1f, .1f, .1f}, 0);

            for (int i = 0; i < world.lights.size(); i++) {
                VisLight light = world.lights.get(i);
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

            //////// hover
            if (pickingHandler == null && lastMousePosition != null) {

                GRay3D ray = thisView.computeRay(lastMousePosition.getX(), lastMousePosition.getY());

                double bestDistance = Double.MAX_VALUE;
                VisCanvasEventHandler bestHandler = null;

                synchronized(eventHandlers) {
                    for (VisCanvasEventHandler eh : eventHandlers) {
                        double dist = eh.hoverQuery(VisCanvas.this, ray);
                        if (dist < 0)
                            continue;

                        if (dist < bestDistance) {
                            bestDistance = dist;
                            bestHandler = eh;
                        }
                    }

                    hoveringHandler = bestHandler;
                    for (VisCanvasEventHandler eh : eventHandlers)
                        eh.hoverNotify(eh == hoveringHandler);
                }
            }

            /////// Update status text
            String fpsLine = "";
            if (popupMenu.fpsItem.isSelected()) {
                fpsLine = String.format("%4.1f FPS", 1.0/fps_dt);
            }

            String pickingLine = "";
            ///////
            if (pickingHandler != null)
                pickingLine = "[ "+pickingHandler.getName()+" ]";


            String newStatusLine = fpsLine + "\n" +
                pickingLine + "\n";

            if (newStatusLine.trim().length()==0)
                newStatusLine = "";

            if (true) {
                VisWorld.Buffer vb = world.getBuffer("__VISCANVAS_STATUS:" + VisCanvas.this.hashCode());

                if (!newStatusLine.equals(lastStatusLine) || fpsLine.length()>0) {
                    lastStatusLine = newStatusLine;
                    vb.addBuffered(new VisContextSpecific(VisCanvas.this,
                                                          new VisText(VisText.ANCHOR.BOTTOM_LEFT,
                                                                      newStatusLine)));
                }

                if (popupMenu.showLookAt.isSelected()) {
                    vb.addBuffered(new VisContextSpecific(VisCanvas.this,
                                                          new VisText(VisText.ANCHOR.BOTTOM_RIGHT,
                                                                      String.format("eye:    %10f %10f %10f\n" +
                                                                                    "lookAt: %10f %10f %10f\n" +
                                                                                    "up:     %10f %10f %10f\n",
                                                                                    thisView.eye[0], thisView.eye[1], thisView.eye[2],
                                                                                    thisView.lookAt[0], thisView.lookAt[1], thisView.lookAt[2],
                                                                                    thisView.up[0], thisView.up[1], thisView.up[2]))));
                }
                vb.switchBuffer();
                vb.setDrawOrder(99);
            }

            //////// render
            world.render(VisCanvas.this, gl, glu);

            if (true) {
                long now = System.currentTimeMillis();
                double dt = (now - last_draw_mtime) / 1000.0;
                // larger alpha = favor existing estimate.
                double fps_alpha = 1.0 - dt;
                fps_alpha = Math.max(0, Math.min(1, fps_alpha));

                fps_dt = fps_dt * fps_alpha + dt * (1.0 - fps_alpha);
                last_draw_mtime = now;
            }

            // Do we need a screen capture?
            synchronized(imageCaptureObjects) {

                if (movieOutputStream != null && (movieAutoFrames || movieMakeFramePending)) {
                    int width = canvas.getWidth();
                    int height = canvas.getHeight();
                    movieMakeFramePending = false;

                    byte imdata[] = new byte[width*height*3];

                    // read the BGR values into the image buffer
                    gl.glPixelStorei(GL.GL_PACK_ALIGNMENT, 1);
                    gl.glReadPixels(0, 0, width, height, GL.GL_RGB,
                                    GL.GL_UNSIGNED_BYTE, ByteBuffer.wrap(imdata));
                    VisUtil.flipImage(width*3, height, imdata);

                    try {
                        String hdr = "";
                        hdr += String.format("# mtime=%d\n", System.currentTimeMillis());
                        hdr += String.format("P6 %d %d %d\n", width, height, 255);

                        movieOutputStream.write(hdr.getBytes());
                        movieOutputStream.write(imdata);
                        movieOutputStream.flush();
                    } catch (IOException ex) {
                        System.out.println("Error writing to movie: "+ex);
                    }

                    if (!movieAutoFrames)
                        System.out.println("Made movie frame");
                }

                if (imageCaptureObjects.size() > 0) {

                    int width = canvas.getWidth();
                    int height = canvas.getHeight();

                    BufferedImage im = new BufferedImage(width, height,
                                                         BufferedImage.TYPE_3BYTE_BGR);

                    byte imdata[] = ((DataBufferByte) im.getRaster().getDataBuffer()).getData();

                    // read the BGR values into the image buffer
                    gl.glPixelStorei(GL.GL_PACK_ALIGNMENT, 1);
                    gl.glReadPixels(0, 0, width, height, GL.GL_BGR,
                                    GL.GL_UNSIGNED_BYTE, ByteBuffer.wrap(imdata));

                    VisUtil.flipImage(width*3, height, imdata);

                    for (ImageCaptureObject ico : imageCaptureObjects) {
                        if (ico.file != null)  {
                            try {
                                ImageIO.write(im, ico.format, ico.file);
                                System.out.println("Screenshot written to: "+ico.file);
                            } catch (IOException ex) {
                                System.out.println("Error writing screenshot to: "+ico.file);
                            }
                        }

                        synchronized(ico) {
                            ico.im = im;
                            ico.notifyAll();
                        }
                    }

                    imageCaptureObjects.clear();
                }
            }

            lastView = thisView;

            //	    VisCanvas.this.canvas.swapBuffers();
        }

        VisObject reshapeText;

        public void reshape(GLAutoDrawable drawable, int i, int x, int width, int height)
        {
            if (reshapeText != null)
                world.removeTemporary(reshapeText);
            reshapeText = new VisContextSpecific(VisCanvas.this, new VisText(VisText.ANCHOR.CENTER, ""+width+" x "+height));

            world.addTemporary(reshapeText, 1.0);

            if (debug)
                System.out.printf("VisCanvas.reshape (%d x %d)\n", width, height);
        }

        public void displayChanged(GLAutoDrawable drawable, boolean modeChanged, boolean deviceChanged)
        {
            System.out.println("displayChanged");
        }
    }

    public FloatImage getDepthBuffer()
    {
        GLContext glc = canvas.getContext();
        glc.makeCurrent();
        GL gl = glc.getGL();

        int width = canvas.getWidth();
        int height = canvas.getHeight();
        float data[] = new float[width*height];
        FloatImage fim = new FloatImage(width, height, data);

        int e1 = gl.glGetError();

        gl.glPixelStorei(GL.GL_PACK_ALIGNMENT, 4);
        gl.glReadPixels(0, 0, width, height, GL.GL_DEPTH_COMPONENT,
                        GL.GL_FLOAT, FloatBuffer.wrap(data));

        int e2 = gl.glGetError();

        glc.release();
        return fim;
    }

    public void movieBegin(String path) throws IOException
    {
        movieBegin(path, true);
    }

    public void movieMakeFrame()
    {
        movieMakeFramePending = true;
        drawNow();
    }

    /** If autoframes == false, you must call movieMakeFrame() to generate the next frame. **/
    public void movieBegin(String path, boolean autoframes) throws IOException
    {
        synchronized(imageCaptureObjects) {
            movieEnd();

            movieAutoFrames = autoframes;
            movieMakeFramePending = false;
            movieOutputStream = new GZIPOutputStream(new FileOutputStream(path));
            System.out.println("Capturing movie to: "+path);
        }
    }

    public void movieEnd() throws IOException
    {
        synchronized(imageCaptureObjects) {

            if (movieOutputStream == null)
                return;

            movieOutputStream.close();
            System.out.println("Movie capture ended");
            movieOutputStream = null;
        }
    }

    // Synchronously grab a screen shot
    // cannot be called from an awt thread!
    public BufferedImage screenShot()
    {
        ImageCaptureObject ico = new ImageCaptureObject();
        synchronized(imageCaptureObjects) {
            imageCaptureObjects.add(ico);
        }

        // force a redraw
        draw();

        synchronized(ico) {
            while (ico.im == null) {
                try {
                    ico.wait();
                } catch (InterruptedException ex) {
                    System.out.println("ex: "+ex);
                }
            }
        }

        return ico.im;
    }

    /** Asynchronously write a screen shot **/
    public void writeScreenShot(File file, String format)
    {
        ImageCaptureObject ico = new ImageCaptureObject();
        ico.file = file;
        ico.format = format;

        synchronized(imageCaptureObjects) {
            imageCaptureObjects.add(ico);
        }

        // force a redraw
        draw();
    }

    /////////////////////////////////////////////
    // Event Handling
    // higher priority event handlers get first dibs.
    public void addEventHandler(VisCanvasEventHandler eh)
    {
        addEventHandler(eh, 0);
    }

    public void addEventHandler(VisCanvasEventHandler eh, int priority)
    {
        synchronized(eventHandlers) {
            eventHandlers.add(eh);
            eventHandlerPriorities.put(eh, priority);

            Collections.sort(eventHandlers, new Comparator<VisCanvasEventHandler>() {
                    public int compare(VisCanvasEventHandler a, VisCanvasEventHandler b) {
                        return eventHandlerPriorities.get(b) - eventHandlerPriorities.get(a);
                    }

                    public boolean equals(Object obj) {
                        return false;
                    }
                });
        }
    }

    public boolean isPicking(VisCanvasEventHandler eh)
    {
        return eh == pickingHandler;
    }

    // EventHandlers should use this VERY sparingly. It can be used to
    // take focus when a key is pressed, for example.
    public void takePick(VisCanvasEventHandler eh)
    {
        releasePick();

        pickingHandler = eh;
    }

    public void releasePick()
    {
        if (pickingHandler != null)
            pickingHandler.pickNotify(false);

        pickingHandler = null;
    }

    public void releasePick(VisCanvasEventHandler eh)
    {
        if (pickingHandler == eh) {
            pickingHandler.pickNotify(false);

            pickingHandler = null;
        }
    }

    public void keyPressed(KeyEvent e)
    {
        if (pickingHandler != null) {
            boolean consumed = pickingHandler.keyPressed(this, e);
            if (consumed)
                return;
        }

        synchronized(eventHandlers) {
            for (VisCanvasEventHandler eh : eventHandlers) {
                boolean consumed = eh.keyPressed(this, e);
                if (consumed)
                    return;
            }
        }
    }

    public void keyReleased(KeyEvent e)
    {
        if (pickingHandler != null) {
            boolean consumed = pickingHandler.keyReleased(this, e);
            if (consumed)
                return;
        }

        synchronized(eventHandlers) {
            for (VisCanvasEventHandler eh : eventHandlers) {
                boolean consumed = eh.keyReleased(this, e);
                if (consumed)
                    return;
            }
        }
    }

    public void keyTyped(KeyEvent e)
    {
        if (pickingHandler != null) {
            boolean consumed = pickingHandler.keyTyped(this, e);
            if (consumed)
                return;
        }

        synchronized(eventHandlers) {
            for (VisCanvasEventHandler eh : eventHandlers) {
                boolean consumed = eh.keyTyped(this, e);
                if (consumed)
                    return;
            }
        }
    }

    public void mouseWheelMoved(MouseWheelEvent e)
    {
        if (lastView == null)
            return;

        GRay3D ray = lastView.computeRay(e.getX(), e.getY());

        if (pickingHandler != null) {
            boolean consumed = pickingHandler.mouseWheelMoved(this, ray, e);
            if (consumed)
                return;
        }

        synchronized(eventHandlers) {
            for (VisCanvasEventHandler eh : eventHandlers) {
                boolean consumed = eh.mouseWheelMoved(this, ray, e);
                if (consumed)
                    return;
            }
        }
    }

    public void mouseDragged(MouseEvent e)
    {
        if (lastView == null)
            return;

        GRay3D ray = lastView.computeRay(e.getX(), e.getY());

        if (pickingHandler != null) {
            boolean consumed = pickingHandler.mouseDragged(this, ray, e);
            if (consumed)
                return;
        }

        synchronized(eventHandlers) {
            for (VisCanvasEventHandler eh : eventHandlers) {
                boolean consumed = eh.mouseDragged(this, ray, e);
                if (consumed)
                    return;
            }
        }
    }

    public void mouseMoved(MouseEvent e)
    {
        this.lastMousePosition = e;

        if (lastView == null)
            return;

        GRay3D ray = lastView.computeRay(e.getX(), e.getY());

        if (pickingHandler != null) {
            boolean consumed = pickingHandler.mouseMoved(this, ray, e);
            if (consumed)
                return;
        }

        synchronized(eventHandlers) {
            for (VisCanvasEventHandler eh : eventHandlers) {
                boolean consumed = eh.mouseMoved(this, ray, e);
                if (consumed)
                    return;
            }
        }
    }

    public void mousePressed(MouseEvent e)
    {
        if (lastView == null)
            return;

        GRay3D ray = lastView.computeRay(e.getX(), e.getY());

        int mods = e.getModifiersEx();
        boolean shift = (mods&MouseEvent.SHIFT_DOWN_MASK)>0;
        boolean ctrl = (mods&MouseEvent.CTRL_DOWN_MASK)>0;

        if (pickingHandler == null && e.getButton() == 1 && !shift && !ctrl) {

            double bestDistance = Double.MAX_VALUE;
            VisCanvasEventHandler bestHandler = null;

            synchronized(eventHandlers) {
                for (VisCanvasEventHandler eh : eventHandlers) {
                    double dist = eh.pickQuery(this, ray);
                    if (dist < 0)
                        continue;

                    if (dist < bestDistance) {
                        bestDistance = dist;
                        bestHandler = eh;
                    }
                }
            }
            pickingHandler = bestHandler;
            for (VisCanvasEventHandler eh : eventHandlers)
                eh.pickNotify(eh == pickingHandler);
        }

        if (pickingHandler != null) {
            boolean consumed = pickingHandler.mousePressed(this, ray, e);
            if (consumed)
                return;
        }

        synchronized(eventHandlers) {
            for (VisCanvasEventHandler eh : eventHandlers) {
                boolean consumed = eh.mousePressed(this, ray, e);
                if (consumed)
                    return;
            }
        }
    }

    public void mouseReleased(MouseEvent e)
    {
        if (lastView == null)
            return;

        GRay3D ray = lastView.computeRay(e.getX(), e.getY());

        if (pickingHandler != null) {
            boolean consumed = pickingHandler.mouseReleased(this, ray, e);
            if (consumed)
                return;
        }

        synchronized(eventHandlers) {
            for (VisCanvasEventHandler eh : eventHandlers) {
                boolean consumed = eh.mouseReleased(this, ray, e);
                if (consumed)
                    return;
            }
        }
    }

    public void mouseClicked(MouseEvent e)
    {
        if (lastView == null)
            return;

        GRay3D ray = lastView.computeRay(e.getX(), e.getY());

        if (pickingHandler != null) {
            boolean consumed = pickingHandler.mouseClicked(this, ray, e);
            if (consumed)
                return;
        }

        synchronized(eventHandlers) {
            for (VisCanvasEventHandler eh : eventHandlers) {
                boolean consumed = eh.mouseClicked(this, ray, e);
                if (consumed)
                    return;
            }
        }
    }

    public void mouseEntered(MouseEvent e)
    {
        ((Component) canvas).requestFocus();
    }

    public void mouseExited(MouseEvent e)
    {
    }

    Dimension forceDim;

    /** Doesn't work, unfortunately. **/
    public void forceSize(Dimension d)
    {
        forceDim = d;
        if (d != null)
            setSize((int) d.getWidth(), (int) d.getHeight());
        invalidate();
        revalidate();
    }

    public Dimension getMaximumSize()
    {
        return forceDim == null ? super.getMaximumSize() : forceDim;
    }

    public Dimension getPreferredSize()
    {
        return forceDim == null ? super.getPreferredSize() : forceDim;
    }

    public Dimension getMinimumSize()
    {
        return forceDim == null ? new Dimension(1,1) : forceDim;
    }

    // A request to generate a screenshot, it will be processed the
    // next time the frame is rendered. The ImageCaptureObject will
    // then get notified().
    //
    // If path/format are non-null, then the image will be written to
    // disk, via ImageIO.write.
    class ImageCaptureObject
    {
        BufferedImage im;

        File   file;
        String format;
    }
}
