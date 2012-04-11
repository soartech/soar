package april.vis;

import april.jmat.geom.*;
import april.jmat.*;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import java.awt.*;
import java.util.*;
import java.io.*;

import lcm.lcm.*;

/** Perform a series of cumulative Matrix transformations and object
 * draws. You can pass these in any order, in any combination.
 *
 * You can pass in Matrix, double[][], or double quat[] + double pos[] for a transformation.
 * You can pass in a VisObject to render something.
 **/
public class VisChain implements VisObject, VisSerializable
{
    ArrayList<Object> operations = new ArrayList<Object>();

    int displayListId;
    GL displayListGL;
    boolean lock;

    public VisChain()
    {
    }

    public VisChain(Object ... os)
    {
        add(os);
    }


    public synchronized void lock()
    {
        lock = true;
    }

    public synchronized void unlock()
    {
        if (displayListGL != null) {
            displayListGL.glDeleteLists(displayListId, 1);
            displayListGL = null;
            displayListId = -1;
        }
    }

    // this method must be added to disabiguate between a
    // two-dimensional array being interpreted as a varargs call
    // consisting of several one-dimensional doubles.
    public void add(double M[][])
    {
        operations.add(LinAlg.copy(M));
    }

    public void add(Object ... os)
    {
        int i = 0;

        while (i < os.length) {
            if (os[i] == null) {
                i++;
                continue;
            }

            if (os[i] instanceof double[]) {

                double tmp[] = (double[]) os[i];
                if (tmp.length==6) {
                    operations.add(LinAlg.xyzrpyToMatrix(tmp));
                    i++;
                } else {
                    assert(i+1 < os.length);
                    double q[] = (double[]) os[i];
                    double p[] = (double[]) os[i+1];

                    double T[][] = LinAlg.quatPosToMatrix(q, p);
                    operations.add(T);
                    i+=2;
                }
                continue;
            }

            if (os[i] instanceof double[][]) {
                operations.add(LinAlg.copy((double[][]) os[i]));
                i++;
                continue;
            }

            if (os[i] instanceof Matrix) {
                Matrix T = (Matrix) os[i];
                operations.add(T.copyArray());
                i++;
                continue;
            }

            if (os[i] instanceof VisObject) {
                operations.add((VisObject) os[i]);
                i++;
                continue;
            }

            // unknown type!
            System.out.println("VisChain: Unknown object added to chain: "+os[i]);
            assert(false);
            i++;
        }
    }

    public synchronized void render(VisContext vc, GL gl, GLU glu)
    {
        if (lock) {
            if (displayListGL != null && displayListGL == gl) {
                gl.glCallList(displayListId);
                return;
            }

            displayListGL = gl;
            displayListId = gl.glGenLists(1);
            gl.glNewList(displayListId, GL.GL_COMPILE);
        }

        boolean pushed = false;

        for (Object o : operations) {

            if (o instanceof double[][]) {
                if (!pushed) {
                    VisUtil.pushGLState(gl);
                    pushed = true;
                }

                VisUtil.multiplyMatrix(gl, (double[][]) o);
                continue;
            }

            if (o instanceof VisObject) {
                VisObject vo = (VisObject) o;
                vo.render(vc, gl, glu);
            }
        }

        if (pushed)
            VisUtil.popGLState(gl);

        if (lock) {
            gl.glEndList();
            gl.glCallList(displayListId);
        }
    }

    public void serialize(LCMDataOutputStream out) throws IOException
    {
        for (Object o : operations) {
            if (o instanceof double[][]) {
                out.writeStringZ("double[][]");
                double mat[][] = (double[][]) o;
                out.writeInt(mat.length);
                out.writeInt(mat.length > 0 ? mat[0].length : 0);
                if (mat.length == 0)
                    continue;
                for (int i =0; i < mat.length; i++) {
                    for (int j = 0; j < mat[0].length; j++) {
                        out.writeDouble(mat[i][j]);
                    }
                }
            } else if (o instanceof VisSerializable) {
                out.writeStringZ("VisSerializable");
                VisSerialize.serialize((VisSerializable)o, out);
            } else {
                System.out.println(o.getClass().getName()+" is not serializable. Log will not look right");
            }
        }
    }

    public void unserialize(LCMDataInputStream in) throws IOException
    {
        while (in.available() != 0) {
            String type = in.readStringZ();
            if (type.equals("double[][]")) {
                int rows = in.readInt();
                int cols = in.readInt();
                double mat[][] = new double[rows][cols];
                for (int r = 0; r < rows; r++) {
                    for (int c = 0; c < cols; c++) {
                        mat[r][c] = in.readDouble();
                    }
                }
                operations.add(mat);

            } else if (type.equals("VisSerializable")) {

                VisSerializable obj =  VisSerialize.unserialize(in);
                if (obj != null)
                    operations.add((VisObject)obj);
            } else {
                assert(false);
            }
        }
    }
}
