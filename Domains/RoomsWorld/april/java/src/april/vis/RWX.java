package april.vis;

import java.awt.*;
import java.io.*;
import java.util.*;
import javax.swing.*;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;

import april.jmat.*;

/** VisObject representing a RWX 3D model format, including quite a
 * few of the ActiveWorlds extensions. Textures are not supported. **/
public class RWX implements VisObject
{
    public HashMap<String, Proto> protos = new HashMap<String, Proto>();
    public ArrayList<RenderOp> renderOps = new ArrayList<RenderOp>();

    BufferedReader ins;
    int displayListId = -1;
    boolean useDisplayList = true;

    double angleaxis[];
    double scale = 1;

    public RWX(String path) throws IOException
    {
        ins = new BufferedReader(new FileReader(path));

        String toks[] = nextLine();
        assert(toks[0].equals("MODELBEGIN"));

        while (true) {
            toks = nextLine();
            if (toks[0].equals("MODELEND"))
                break;
            else if (toks[0].equals("PROTOBEGIN")) {
                Proto p = new Proto(toks);
                protos.put(p.name, p);
            } else if (toks[0].equals("CLUMPBEGIN")) {
                renderOps.add(new Clump(toks));
            } else if (toks[0].equals("SCALE")) {
                //renderOps.add(new Scale(toks));
            } else {
                System.out.println("Model: unknown line: "+glue(toks));
            }
        }
    }

    void renderReal(GL gl)
    {
        VisUtil.pushGLWholeState(gl);
        //	gl.glEnable(GL.GL_DEPTH_TEST);

        for (RenderOp rop: renderOps)
            rop.renderGL(gl);

        VisUtil.popGLWholeState(gl);
    }

    public void setScale(double scale)
    {
        this.scale = scale;
    }

    public void setRotation(double angleaxis[])
    {
        this.angleaxis = LinAlg.copy(angleaxis);
    }

    public void render(VisContext vc, GL gl, GLU glu)
    {
        if (angleaxis != null)
            gl.glRotated(angleaxis[0], angleaxis[1], angleaxis[2], angleaxis[3]);
        gl.glScaled(scale, scale, scale);

        gl.glMatrixMode(GL.GL_MODELVIEW);

        if (useDisplayList) {
            if (displayListId < 0) {
                displayListId = gl.glGenLists(1);
                gl.glNewList(displayListId, GL.GL_COMPILE_AND_EXECUTE);
                renderReal(gl);
                gl.glEndList();
            } else {
                gl.glCallList(displayListId);
            }
        } else {
            renderReal(gl);
        }
    }

    abstract class RenderOp
    {
        abstract void renderGL(GL gl);
    }

    class GLPushOp extends RenderOp
    {
        void renderGL(GL gl)
        {
            VisUtil.pushGLState(gl);
        }
    }

    class GLPopOp extends RenderOp
    {
        void renderGL(GL gl)
        {
            VisUtil.popGLState(gl);
        }
    }

    class Translate extends RenderOp
    {
        float x, y, z;

        Translate(String toks[])
        {
            x = Float.parseFloat(toks[1]);
            y = Float.parseFloat(toks[2]);
            z = Float.parseFloat(toks[3]);
        }

        void renderGL(GL gl)
        {
            gl.glTranslatef(x,y,z);
        }
    }

    class Transform extends RenderOp
    {
        float m[] = new float[16];

        Transform(String toks[])
        {
            for (int i = 0; i < 16; i++)
                m[i] = Float.parseFloat(toks[1+i]);
        }

        void renderGL(GL gl)
        {
            // ActiveWorld spec says that Transform is relative to
            // global coordinate frame, but models look funny that
            // way. Also note that calling glLoadMatrix would mess up
            // display lists.

            gl.glMultMatrixf(m, 0);
        }
    }

    class Scale extends RenderOp
    {
        float sx, sy, sz;

        Scale(String toks[])
        {
            sx = Float.parseFloat(toks[1]);
            sy = Float.parseFloat(toks[2]);
            sz = Float.parseFloat(toks[3]);
        }

        void renderGL(GL gl)
        {
            gl.glScalef(sx,sy,sz);
        }
    }

    class ProtoInstance extends RenderOp
    {
        String name;

        ProtoInstance(String toks[])
        {
            name = toks[1];
        }

        void renderGL(GL gl)
        {
            Proto p = protos.get(name);
            p.renderGL(gl);
        }
    }

    RenderOp GL_PUSH = new GLPushOp(), GL_POP = new GLPopOp();

    String[] nextLine()
    {
        try {
            while (true) {
                String line = ins.readLine().trim().toUpperCase();

                int comment = line.indexOf("#");
                if (comment>=0)
                    line = line.substring(0, comment).trim();

                if (line.length()==0)
                    continue;
                if (line.startsWith("#"))
                    continue;
                return line.split("\\s+");
            }
        } catch (IOException ex) {
            System.out.println("ex: "+ex);
            return null;
        }
    }

    static final String glue(String toks[])
    {
        String s = "";
        for (String tok : toks)
            s += tok + " ";
        return s;
    }

    class Vertex
    {
        float x, y, z;
        float nx, ny, nz;
        int nnorms;

        Vertex(String toks[])
        {
            assert(toks[0].equals("VERTEX"));
            x = Float.parseFloat(toks[1]);
            y = Float.parseFloat(toks[2]);
            z = Float.parseFloat(toks[3]);
        }
    }

    class Triangle
    {
        Vertex v0, v1, v2;

        Triangle(ClumpLike clump, String tok0, String tok1, String tok2)
        {
            v0 = clump.vertices.get(Integer.parseInt(tok0) - 1);
            v1 = clump.vertices.get(Integer.parseInt(tok1) - 1);
            v2 = clump.vertices.get(Integer.parseInt(tok2) - 1);
        }

        void getNormal(float n[])
        {
            float x0 = v1.x - v0.x, y0 = v1.y-v0.y, z0 = v1.z - v0.z;
            float x1 = v2.x - v0.x, y1 = v2.y-v0.y, z1 = v2.z - v0.z;

            n[0] = y0*z1 - z0*y1;
            n[1] = z0*x1 - x0*z1;
            n[2] = x0*y1 - y0*x1;
        }
    }

    class Proto extends RenderOp
    {
        String name;
        ClumpLike clike;

        Proto(String toks[])
        {
            assert(toks[0].equals("PROTOBEGIN"));
            name = toks[1];

            clike = new ClumpLike(toks, "PROTOBEGIN", "PROTOEND");
        }

        void renderGL(GL gl)
        {
            clike.renderGL(gl);
        }
    }

    class Clump extends RenderOp
    {
        ClumpLike clike;

        Clump(String toks[])
        {
            clike = new ClumpLike(toks, "CLUMPBEGIN", "CLUMPEND");
        }

        void renderGL(GL gl)
        {
            clike.renderGL(gl);
        }
    }

    // Protos and Clumps both have very similar capabilities. This
    // implements that.
    class ClumpLike extends RenderOp
    {
        ArrayList<Vertex> vertices = new ArrayList<Vertex>();
        ArrayList<Triangle> triangles = new ArrayList<Triangle>();
        ArrayList<RenderOp> renderOps = new ArrayList<RenderOp>();
        Color color = Color.black;
        float opacity = 1.0f;
        float diffuse = 1.0f;
        float specular = 0.0f;
        float ambient = 1.0f;

        String lightSampling = "Vertex";

        String tag;

        public ClumpLike(String toks[], String starttok, String endtok)
        {
            assert(toks[0].equals(starttok));

            while (true) {
                toks = nextLine();

                if (toks[0].equals(endtok)) {
                    break;
                } else if (toks[0].equals("VERTEX")) {
                    vertices.add(new Vertex(toks));
                } else if (toks[0].equals("TRIANGLE")) {
                    triangles.add(new Triangle(this, toks[1], toks[2], toks[3]));
                } else if (toks[0].equals("QUAD")) {
                    triangles.add(new Triangle(this, toks[1], toks[2], toks[3]));
                    triangles.add(new Triangle(this, toks[1], toks[3], toks[4]));
                } else if (toks[0].equals("POLYGON")) {
                    // toks[1] is the number of vertexes. First vertex
                    // index starts at toks[2].

                    if (false) {
                        // This code "properly" tesselates the
                        // polygon, but some polygons seem to be
                        // self-intersectingfalse
                        ArrayList<double[]> points = new ArrayList<double[]>();
                        for (int i = 2; i < toks.length; i++) {
                            Vertex v = vertices.get(Integer.parseInt(toks[i])-1);
                            points.add(new double[] {v.x, v.y, v.z});
                        }
                        april.jmat.geom.Polygon3D poly3d = new april.jmat.geom.Polygon3D(points);
                        ArrayList<int[]> tris = poly3d.getTriangles();
                        for (int tri[]: tris)
                            triangles.add(new Triangle(this, toks[tri[0]+2], toks[tri[1]+2], toks[tri[2]+2]));
                    } else {
                        // This code just does radial triangles
                        for (int i = 3; i < toks.length - 1; i++) {
                            triangles.add(new Triangle(this, toks[2], toks[i], toks[i+1]));
                        }
                    }

                } else if (toks[0].equals("COLOR")) {
                    color = new Color(Float.parseFloat(toks[1]),
                                      Float.parseFloat(toks[2]),
                                      Float.parseFloat(toks[3]));
                } else if (toks[0].equals("AMBIENT")) {
                    ambient = Float.parseFloat(toks[1]);
                } else if (toks[0].equals("DIFFUSE")) {
                    diffuse = Float.parseFloat(toks[1]);
                } else if (toks[0].equals("SPECULAR")) {
                    specular = Float.parseFloat(toks[1]);
                } else if (toks[0].equals("LIGHTSAMPLING")) {
                    lightSampling = toks[1];
                } else if (toks[0].equals("SURFACE")) {
                    ambient = Float.parseFloat(toks[1]);
                    diffuse = Float.parseFloat(toks[2]);
                    specular = Float.parseFloat(toks[3]);
                } else if (toks[0].equals("TAG")) {
                    tag = toks[1];
                } else if (toks[0].equals("TRANSFORMBEGIN")) {
                    renderOps.add(GL_PUSH);
                } else if (toks[0].equals("TRANSFORMEND")) {
                    renderOps.add(GL_POP);
                } else if (toks[0].equals("OPACITY")) {
                    opacity = Float.parseFloat(toks[1]);
                } else if (toks[0].equals("CLUMPBEGIN")) {
                    renderOps.add(new Clump(toks));
                } else if (toks[0].equals("TRANSLATE")) {
                    renderOps.add(new Translate(toks));
                } else if (toks[0].equals("SCALE")) {
                    renderOps.add(new Scale(toks));
                } else if (toks[0].equals("PROTOINSTANCE")) {
                    renderOps.add(new ProtoInstance(toks));
                } else if (toks[0].equals("TEXTURE")) {
                    // not implemented. arg1 = texture name? NULL=no texture
                } else if (toks[0].equals("TEXTUREMODE") || toks[0].equals("TEXTUREMODES")) {
                    // not implemented
                } else if (toks[0].equals("HINTS")) {
                    // not implemented
                } else if (toks[0].equals("GEOMETRYSAMPLING")) {
                    // not implemented
                } else if (toks[0].equals("TRANSFORM")) {
                    renderOps.add(new Transform(toks));
                } else if (toks[0].equals("IDENTITY")) {
                    renderOps.add(new Transform(new String[] {"TRANSFORM",
                                                              "1", "0", "0", "0",
                                                              "0", "1", "0", "0",
                                                              "0", "0", "1", "0",
                                                              "0", "0", "0", "1"}));
                } else {
                    System.out.println("Clump Unknown line: " + glue(toks));
                }
            }

            // fix normals
            float tn[] = new float[3];
            for (Triangle t : triangles) {
                t.getNormal(tn);
                t.v0.nx += tn[0];
                t.v0.ny += tn[1];
                t.v0.nz += tn[2];
                t.v0.nnorms++;

                t.v1.nx += tn[0];
                t.v1.ny += tn[1];
                t.v1.nz += tn[2];
                t.v1.nnorms++;

                t.v2.nx += tn[0];
                t.v2.ny += tn[1];
                t.v2.nz += tn[2];
                t.v2.nnorms++;
            }

            for (Vertex v : vertices) {
                v.nx /= v.nnorms;
                v.ny /= v.nnorms;
                v.nz /= v.nnorms;
            }
        }

        void renderGL(GL gl)
        {
            for (RenderOp rop: renderOps)
                rop.renderGL(gl);

            VisUtil.setColor(gl, color);

            boolean prettier = true;
            /*
              if (prettier) {
              gl.glEnable(GL.GL_LIGHTING);
              gl.glLightModeli(GL.GL_LIGHT_MODEL_TWO_SIDE, GL.GL_TRUE);
              gl.glLightfv(GL.GL_LIGHT0, GL.GL_AMBIENT, new float[] {.4f, .4f, .4f, 1.0f}, 0);
              gl.glLightfv(GL.GL_LIGHT0, GL.GL_DIFFUSE, new float[] {.8f, .8f, .8f, 1.0f}, 0);
              gl.glLightfv(GL.GL_LIGHT0, GL.GL_SPECULAR, new float[] {.5f, .5f, .5f, 1.0f}, 0);
              gl.glLightfv(GL.GL_LIGHT0, GL.GL_POSITION, new float[] {100f, 150f, 120f, 1}, 0);
              }

              gl.glEnable(GL.GL_COLOR_MATERIAL);
              gl.glColorMaterial(GL.GL_FRONT_AND_BACK, GL.GL_AMBIENT_AND_DIFFUSE);
              gl.glMaterialfv(GL.GL_FRONT_AND_BACK, GL.GL_SPECULAR, new float[] {specular, specular, specular, 1}, 0);
              gl.glMateriali(GL.GL_FRONT_AND_BACK, GL.GL_SHININESS, 10);
              gl.glEnable(GL.GL_LIGHT0);
            */

            //            gl.glColor4f(mcolor[0], mcolor[1], mcolor[2], mcolor[3]);

            gl.glBegin(GL.GL_TRIANGLES);

            for (Triangle t: triangles) {
                gl.glNormal3f(t.v0.nx, t.v0.ny, t.v0.nz);
                gl.glVertex3f(t.v0.x, t.v0.y, t.v0.z);

                gl.glNormal3f(t.v1.nx, t.v1.ny, t.v1.nz);
                gl.glVertex3f(t.v1.x, t.v1.y, t.v1.z);

                gl.glNormal3f(t.v2.nx, t.v2.ny, t.v2.nz);
                gl.glVertex3f(t.v2.x, t.v2.y, t.v2.z);
            }
            gl.glEnd();

            /*
              if (prettier) {
              gl.glDisable(GL.GL_LIGHTING);
              gl.glDisable(GL.GL_COLOR_MATERIAL);
              gl.glDisable(GL.GL_LIGHT0);
              }
              gl.glColorMaterial(GL.GL_FRONT_AND_BACK, GL.GL_AMBIENT_AND_DIFFUSE);
            */
        }
    }

    public static void main(String args[])
    {
        JFrame f = new JFrame("RWXObject");
        f.setLayout(new BorderLayout());

        VisWorld vw = new VisWorld();
        VisCanvas vc = new VisCanvas(vw);

        try {
            RWX rwx = new RWX(args[0]);
            VisWorld.Buffer vb = vw.getBuffer("rwx");
            vb.addBuffered(rwx);
            vb.switchBuffer();
        } catch (IOException ex) {
            System.out.println("ex: "+ex);
        }

        vc.getViewManager().viewGoal.lookAt(new double[] {0,0,.2}, new double[3], new double[] {0,1,0});
        f.add(vc);
        f.setSize(600, 400);
        f.setVisible(true);

    }
}
