package april.vis;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import com.sun.opengl.util.*;

import april.jmat.geom.*;

import java.util.*;
import java.nio.*;
import java.io.*;

import lcm.lcm.*;

/** Workhorse of 2D and 3D data viewing. **/
public class VisData implements VisObject, VisSerializable
{
    ArrayList<double[]> points = new ArrayList<double[]>();
    ArrayList<VisDataStyle> styles = new ArrayList<VisDataStyle>();

    DoubleBuffer vertexbuf;

    public VisData(Object ... args)
    {
        add(args);
    }

    public synchronized void clear()
    {
        points.clear();
        vertexbuf = null;
    }

    public synchronized void add(Object ... args)
    {
        for (int i = 0; i < args.length; i++) {

            Object o = args[i];

            if (o == null)
                continue;

            if (o instanceof VisDataStyle) {
                styles.add((VisDataStyle) o);
            } else if (o instanceof double[][]) {
                double [][] l = (double[][])o;
                for(int j = 0; j < l.length; j++){
                    points.add(l[j]);
                }
            } else if (o instanceof double[]) {
                points.add((double[]) o);
            } else if (o instanceof ArrayList) {
                ArrayList al = (ArrayList) o;

                points.ensureCapacity(points.size() + al.size());

                for (Object p : al) {
                    if (p == null)
                        continue;
                    assert (p instanceof double[]);
                    points.add((double[]) p);
                }
            } else {
                System.out.println("VisData: Unknown type "+o);
            }
        }
        vertexbuf = null;
    }

    public synchronized int getNumPoints()
    {
        return points.size();
    }

    public synchronized void render(VisContext vc, GL gl, GLU glu)
    {
        for (VisDataStyle style : styles) {
            style.renderStyle(vc, gl, glu, this);
        }
    }

    public synchronized DoubleBuffer getVertexBuffer()
    {
        if (vertexbuf == null) {
            vertexbuf = BufferUtil.newDoubleBuffer(points.size()*3);
            for (int pidx = 0; pidx < points.size(); pidx++) {
                double p[] = points.get(pidx);
                for (int i = 0; i < 3; i++) {
                    if (i < p.length)
                        vertexbuf.put(p[i]);
                    else
                        vertexbuf.put(0);
                }
            }
        }

        vertexbuf.rewind();
        return vertexbuf;
    }

    // Serialization
    public VisData()
    {
    }

    public void serialize(LCMDataOutputStream out) throws IOException
    {
        // 1) write points (We only support homogeneous point clouds (no mixing 2D and 3D)
        out.writeInt(points.size());
        int dim = points.size() > 0 ? points.get(0).length : 0;
        out.writeInt(dim);

        for (double[] point : points) {
            assert(point.length == dim);
            for (int i = 0; i < dim; i++)
                out.writeDouble(point[i]);
        }

        // 2) Recursively write Data Styles
        out.writeInt(styles.size());
        for (VisDataStyle style : styles)
            if (style instanceof VisSerializable)
                VisSerialize.serialize((VisSerializable)style, out);
            else
                System.out.println("ERR:    "+style.getClass().getName()+" is not Serializable. This snapshot will be unreadable!");
    }

    public void unserialize(LCMDataInputStream in) throws IOException
    {
        // 1) read points
        int npoints = in.readInt();
        int dim = in.readInt();

        for (int j =0; j < npoints; j++) {
            double point[] = new double[dim];
            for (int i =0; i < dim; i++) {
                point[i] = in.readDouble();
            }
            points.add(point);
        }

        // 2) read styles
        int nstyles = in.readInt();
        for (int i = 0; i < nstyles; i++) {
            VisSerializable obj = VisSerialize.unserialize(in);
            if (obj != null)
                styles.add((VisDataStyle) obj);
        }
    }
}
