package april.sim;

import java.awt.*;
import java.awt.image.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import java.awt.*;
import javax.swing.*;

import april.config.*;
import april.util.*;
import april.jmat.*;
import april.vis.*;
import april.jmat.geom.*;

import lcm.util.*;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;

public class Simulator implements VisConsole.Listener
{
    JFrame jf;
    VisWorld vw = new VisWorld();
    VisCanvas vc = new VisCanvas(vw);
    VisConsole console = new VisConsole(vc, vw);

    SimWorld world;
    String worldFilePath = "/tmp/world.world";

    static final double MIN_SIZE = 0.25;

    String simObjectClass = "april.sim.SimBox";
    SimObject selectedObject = null;
    FindSimObjects finder = new FindSimObjects();

    GetOpt gopt;

    public Simulator(GetOpt gopt)
    {
        this.gopt = gopt;

        try {
            Config config = new Config();
            if (gopt.wasSpecified("config"))
                config = new ConfigFile(EnvUtil.expandVariables(gopt.getString("config")));

            if (gopt.getString("world").length() > 0) {
                worldFilePath = EnvUtil.expandVariables(gopt.getString("world"));
                this.world = new SimWorld(worldFilePath, config);
            } else {
                this.world = new SimWorld(config);
            }

        } catch (IOException ex) {
            System.out.println("ex: "+ex);
            ex.printStackTrace();
            return;
        }

        jf = new JFrame("Simulator");
        jf.setLayout(new BorderLayout());
        jf.add(vc, BorderLayout.CENTER);

        jf.setSize(800,600);
        jf.setVisible(true);

        vc.addEventHandler(new MyEventHandler());

        vw.getBuffer("grid").addFront(new VisGrid());

        if (true) {
            VisWorld.Buffer vb = vw.getBuffer("SimWorld");
            vb.addBuffered(new VisSimWorld());
            vb.switchBuffer();
        }

        console.addListener(this);
        console.addShortcut(VisConsole.Shortcut.makeCode("start", KeyEvent.VK_F1, 0));
        console.addShortcut(VisConsole.Shortcut.makeCode("stop", KeyEvent.VK_F2, 0));
        draw();

        if (gopt.getBoolean("start")) {
            world.setRunning(true);
        }

        vc.setTargetFPS(gopt.getInt("fps"));

        if (world.geoimage != null) {

            VisWorld.Buffer vb = vw.getBuffer("geoimage");
            vb.setDrawOrder(-100);
            BufferedImage im = ImageUtil.convertImage(world.geoimage.getImage(), BufferedImage.TYPE_INT_ARGB);

            if (true) {
                // make image grayscale and mostly transparent
                int d[] = ((DataBufferInt) (im.getRaster().getDataBuffer())).getData();
                for (int i = 0; i < d.length; i++) {
                    int rgb = d[i];
                    int r = (rgb >> 16) & 0xff;
                    int gr = (rgb >> 8) & 0xff;
                    int b = (rgb >> 0) & 0xff;
                    int m = (r + gr + b) / 3;
                    d[i] = (m<<16) + (m<<8) + (m<<0) + (90<<24);
                }
            }

            VisTexture tex = new VisTexture(im);
            tex.lock();
            tex.setMagFilter(true);
            vb.addBuffered(new VisChain(new VisDepthTest(false,
                                                         new VisImage(tex,
                                                                      world.geoimage.image2xy(new double[] {0,0}),
                                                                      world.geoimage.image2xy(new double[] {im.getWidth()-1,
                                                                                                            im.getHeight()-1})))));
            vb.addBuffered(new VisData(new double[3], new VisDataPointStyle(Color.gray, 3)));

            vb.switchBuffer();
        }
    }

    public boolean consoleCommand(VisConsole vc, PrintStream out, String command)
    {
        String toks[] = command.trim().split("\\s+");
        if (toks.length==0)
            return false;

        if (toks[0].equals("save")) {
            if (toks.length > 1)
                worldFilePath = toks[1];
            try {
                world.write(worldFilePath);
                out.printf("Saved world to: "+worldFilePath+"\n");
            } catch (IOException ex) {
                out.println("ex: "+ex);
            }
            return true;
        }

        if (toks[0].equals("class")) {
            if (toks.length==2) {
                SimObject sobj = SimWorld.createObject(world, toks[1]);

                if (sobj != null) {
                    simObjectClass = toks[1];
                    out.printf("Class set\n");
                } else {
                    out.printf("Unknown or invalid class name: "+toks[1]+"\n");
                }
                return true;
            } else {
                out.printf("usage: class <classname>\n");
                return true;
            }
        }

        if (toks[0].equals("stop")) {
            world.setRunning(false);
            out.printf("Stopped\n");
            return true;
        }

        if (toks[0].equals("start")) {
            world.setRunning(true);
            out.printf("Started\n");
            return true;
        }

        out.printf("Unknown command\n");
        return false;
    }

    public ArrayList<String> consoleCompletions(VisConsole vc, String prefix)
    {
        String cs[] = new String[] { "save", "start", "stop" };

        ArrayList<String> as = new ArrayList<String>();
        for (String s: cs)
            as.add(s);

        for (String s : finder.classes)
            as.add("class "+s);
        return as;
    }

    public static void main(String args[])
    {
        GetOpt gopt = new GetOpt();
        gopt.addBoolean('h', "help", false, "Show this help");
        gopt.addString('w', "world", "", "World file");
        gopt.addString('c', "config", "", "Configuration file");
        gopt.addBoolean('\0', "start", false, "Start simulation automatically");
        gopt.addInt('\0', "fps", 10, "Maximum frame rate");

        if (!gopt.parse(args) || gopt.getBoolean("help") || gopt.getExtraArgs().size() > 0) {
            gopt.doHelp();
            return;
        }

        Simulator editor = new Simulator(gopt);
    }

    class VisSimWorld implements VisObject
    {
        public void render(VisContext vc, GL gl, GLU glu)
        {
            synchronized(world) {
                for (SimObject obj : world.objects) {
                    VisChain v = new VisChain(obj.getPose(), obj.getVisObject());
                    v.render(vc, gl, glu);
                }
            }
        }
    }

    void draw()
    {
        if (true) {
            VisWorld.Buffer vb = vw.getBuffer("collide-info");
            boolean collide = false;

            if (selectedObject != null) {
                // does this object now collide with anything else?

                synchronized(world) {
                    for (SimObject so : world.objects) {
                        if (so != selectedObject && Collisions.collision(so.getShape(), so.getPose(),
                                                                         selectedObject.getShape(), selectedObject.getPose())) {
                            collide = true;
                            break;
                        }
                    }
                }
            }
            if (collide)
                vb.addBuffered(new VisText(VisText.ANCHOR.BOTTOM_RIGHT, "<<blue>>Collision"));

            vb.switchBuffer();
        }
    }

    class MyEventHandler extends VisCanvasEventAdapter
    {
        double sz = 1;
        Color color = new Color(50,50,50);
        double lastxy[] = null;

        public MyEventHandler()
        {
        }

        public String getName()
        {
            return "World Editor";
        }

        public boolean mouseReleased(VisCanvas vc, GRay3D ray, MouseEvent e)
        {
            lastxy = null;
            if (selectedObject == null)
                return false;

            selectedObject = null;
            draw();

            return true;
        }

        public boolean mouseDragged(VisCanvas vc,  GRay3D ray, MouseEvent e)
        {
            if (selectedObject == null)
                return false;

            int mods = e.getModifiersEx();
            boolean shift = (mods&MouseEvent.SHIFT_DOWN_MASK)>0;
            boolean ctrl = (mods&MouseEvent.CTRL_DOWN_MASK)>0;
            if ((mods & InputEvent.BUTTON1_DOWN_MASK) == 0)
                return false;

            double xy[] = ray.intersectPlaneXY();
            if (ctrl) {
                double T[][] = selectedObject.getPose();

                if (selectedObject instanceof SimBox) {
                    // resize
                    SimBox sb = (SimBox) selectedObject;

                    // Generate the four corners
                    ArrayList<double[]> corners = new ArrayList<double[]>();
                    corners.add(new double[] { -sb.sxyz[0]/2, -sb.sxyz[1]/2 });
                    corners.add(new double[] { -sb.sxyz[0]/2, sb.sxyz[1]/2 });
                    corners.add(new double[] { sb.sxyz[0]/2, sb.sxyz[1]/2 });
                    corners.add(new double[] { sb.sxyz[0]/2, -sb.sxyz[1]/2 });
                    corners = LinAlg.transform(T, corners);

                    // which corner is farthest away? (this corner will remain stationary)
                    double furthest[] = null;
                    for (double cxy[] : corners) {
                        if (furthest==null || LinAlg.distance(cxy, xy) > LinAlg.distance(furthest, xy))
                            furthest = cxy;
                    }

                    double Tinv[][] = LinAlg.inverse(T);
                    ArrayList<double[]> newcorners = new ArrayList<double[]>();
                    newcorners.add(furthest);
                    newcorners.add(xy);
                    newcorners = LinAlg.transform(Tinv, newcorners);
                    double p0[] = newcorners.get(0);
                    double p1[] = newcorners.get(1);

                    sb.sxyz[0] = Math.abs(p1[0]-p0[0]);
                    sb.sxyz[1] = Math.abs(p1[1]-p0[1]);
                    sb.T[0][3] = (xy[0] + furthest[0])/2;
                    sb.T[1][3] = (xy[1] + furthest[1])/2;
//                    sb.sxyz[0] = Math.max(MIN_SIZE, 2*Math.abs(xy[0] - T[0][3]));
//                    sb.sxyz[1] = Math.max(MIN_SIZE, 2*Math.abs(xy[1] - T[1][3]));
                } else if (selectedObject instanceof SimSphere) {
                    SimSphere s = (SimSphere) selectedObject;

                    s.r = Math.max(MIN_SIZE, Math.sqrt(LinAlg.sq(xy[0] - T[0][3]) +
                                                       LinAlg.sq(xy[1] - T[1][3])));
                }
            } else if (shift) {
                // rotate
                double T[][] = selectedObject.getPose();
                double t = Math.atan2(xy[1] - T[1][3], xy[0] - T[0][3]);
                double R[][] = LinAlg.rotateZ(t);
                for (int i = 0; i < 3; i++)
                    for (int j = 0; j < 3; j++)
                        T[i][j] = R[i][j];
                selectedObject.setPose(T);
            } else {
                // translate
                double T[][] = selectedObject.getPose();
                T[0][3] += xy[0] - lastxy[0];
                T[1][3] += xy[1] - lastxy[1];
                selectedObject.setPose(T);
            }

            draw();
            lastxy = xy;
            return true;
        }

        public boolean keyPressed(VisCanvas vc, KeyEvent e)
        {
            if (e.getKeyChar() >= '1' && e.getKeyChar() <= '9') {
                sz = Double.parseDouble(""+e.getKeyChar());

                if (selectedObject != null && selectedObject instanceof SimBox) {
                    SimBox sb = (SimBox) selectedObject;
                    sb.sxyz[2] = sz;
                    draw();
                }

                return true;
            }

            char c = e.getKeyChar();
            if (c >='a' && c <='z') {
                switch (e.getKeyChar()) {
                    case 'r':
                        color = Color.red; break;
                    case 'g':
                        color = new Color(50,50,50); break;
                    case 'b':
                        color = Color.blue; break;
                    case 'm':
                        color = Color.magenta; break;
                    case 'c':
                        color = Color.cyan; break;
                }

                if (selectedObject != null && selectedObject instanceof SimBox) {
                    ((SimBox) selectedObject).color = color;
                    draw();
                } else if (selectedObject != null && selectedObject instanceof SimSphere) {
                    ((SimSphere) selectedObject).color = color;
                    draw();
                }


                return true;
            }

            if (selectedObject != null && (e.getKeyCode()==KeyEvent.VK_DELETE || e.getKeyCode()==KeyEvent.VK_BACK_SPACE)) {
                selectedObject.setRunning(false);
                synchronized(world) {
                    world.objects.remove(selectedObject);
                }

                selectedObject = null;
                draw();
                return true;
            }
            return false;
        }

        public boolean mousePressed(VisCanvas vc,  GRay3D ray, MouseEvent e)
        {
            int mods = e.getModifiersEx();
            boolean shift = (mods&MouseEvent.SHIFT_DOWN_MASK)>0;
            boolean ctrl = (mods&MouseEvent.CTRL_DOWN_MASK)>0;
            if ((mods & InputEvent.BUTTON1_DOWN_MASK) == 0)
                return false;

            double xy[] = ray.intersectPlaneXY();

            if (ctrl) {
                // create a new object
                selectedObject = SimWorld.createObject(world, simObjectClass);

                double T[][] = LinAlg.identity(4);
                T[0][3] = xy[0];
                T[1][3] = xy[1];
                T[2][3] = sz/2;
                selectedObject.setPose(T);

                if (selectedObject instanceof SimBox) {
                    ((SimBox) selectedObject).sxyz = new double[] { 1, 1, sz };
                    ((SimBox) selectedObject).color = color;
                }

                synchronized(world) {
                    world.objects.add(selectedObject);
                }

            } else {
                // select an existing object
                double bestd = Double.MAX_VALUE;

                synchronized(world) {
                    for (SimObject obj : world.objects) {

                        double d = Collisions.collisionDistance(ray.getSource(), ray.getDir(), obj.getShape(), obj.getPose());

                        boolean b = Collisions.collision(obj.getShape(), obj.getPose(),
                                                         new SphereShape(0.1), LinAlg.translate(xy[0], xy[1], 0));

                        if (d < bestd) {
                            selectedObject = obj;
                            bestd = d;
                        }
                    }
                }

                if (selectedObject == null)
                    return false;
            }

            draw();
            lastxy = xy;
            return true;
        }
    }

    class FindSimObjects implements lcm.util.ClassDiscoverer.ClassVisitor
    {
        ArrayList<String> classes = new ArrayList<String>();

        public FindSimObjects()
        {
            ClassDiscoverer.findClasses(this);
        }

        public void classFound(String jarfile, Class cls)
        {
            boolean good = false;

            for (Class c : cls.getInterfaces()) {
                if (c.equals(SimObject.class))
                    good = true;
            }

            if (good)
                classes.add(cls.getName());
        }
    }
    
    public SimWorld getWorld()
    {
        return world;
    }
}
