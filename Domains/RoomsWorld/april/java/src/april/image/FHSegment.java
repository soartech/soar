package april.image;

import java.awt.image.*;
import java.util.*;

import april.util.*;

/** Implementation of Felzenswalb Huttenlocher segmentation:
    "Efficient Graph-Based Image Segmentation"
**/
public class FHSegment
{
    // an edge has three fields, ida, idb, and weight.  We represent
    // all three in a single long
    // (ida << IDA_SHIFT) + (idb << IDB_SHIFT) + weight.
    //
    // maximum id = 2^24 ==> maximum image size is 8192x8192.
    static final int IDA_SHIFT = 40, IDB_SHIFT = 16, WEIGHT_SHIFT = 0;
    static final int WEIGHT_MASK = 65535;
    static final int INDEX_MASK = (1<<24)-1;

    // we can handle only integer edge weights [0, WEIGHT_MASK].
    static final int rgbError(int rgba, int rgbb)
    {
        int r = ((rgba>>16)&0xff) - ((rgbb>>16)&0xff);
        int g = ((rgba>>8)&0xff) - ((rgbb>>8)&0xff);
        int b = ((rgba)&0xff) - ((rgbb)&0xff);

        int e = (int) (100.0*Math.sqrt(r*r + g*g + b*b));
        //	int e = Math.abs(r) + Math.abs(g) + Math.abs(b);

        return e;
    }

    public static BufferedImage segment(BufferedImage _in, double k, int minSize)
    {
        int width = _in.getWidth(), height = _in.getHeight();
        int in[] = ((DataBufferInt) (_in.getRaster().getDataBuffer())).getData();

        // make the edges...
        long edges[] = new long[(width-1)*(height-1)*4];
        int nedges = 0;

        for (int y = 1; y+1 < height; y++) {
            for (int x = 0; x+1 < width; x++) {
                long ida  = y*width + x;
                long idb1 = y*width + x + 1;
                long idb2 = (y+1)*width + x;
                long idb3 = (y+1)*width + x + 1;
                long idb4 = (y-1)*width + x + 1;

                int rgb0 = in[(int) ida];
                int rgb1 = in[(int) idb1];
                int rgb2 = in[(int) idb2];
                int rgb3 = in[(int) idb3];
                int rgb4 = in[(int) idb4];

                long w1 = rgbError(rgb0, rgb1);
                long w2 = rgbError(rgb0, rgb2);
                long w3 = rgbError(rgb0, rgb3);
                long w4 = rgbError(rgb0, rgb4);

                edges[nedges++] = (ida<<IDA_SHIFT) + (idb1<<IDB_SHIFT) + w1;
                edges[nedges++] = (ida<<IDA_SHIFT) + (idb2<<IDB_SHIFT) + w2;
                edges[nedges++] = (ida<<IDA_SHIFT) + (idb3<<IDB_SHIFT) + w3;
                edges[nedges++] = (ida<<IDA_SHIFT) + (idb4<<IDB_SHIFT) + w4;
            }
        }
        //	assert (nedges == edges.length);

        // threshold for each cluster (nominally the sum of the
        // maximum internal difference within the cluster plus
        // k/sz(cluster).
        double thresh[] = new double[width*height];
        for (int i = 0; i < thresh.length; i++)
            thresh[i] = k;

        UnionFindSimple uf = new UnionFindSimple(width*height);

        // process edges in order of increasing weight
        if (true) {
            // counting sort

            int maxv = rgbError(0xffffff, 0x000000) + 1;
            int counts[] = new int[maxv];
            for (int i = 0; i < edges.length; i++) {
                int w = (int) (edges[i]&WEIGHT_MASK);
                counts[w]++;
            }

            // accumulate.
            for (int i = 1; i < counts.length; i++)
                counts[i] += counts[i-1];

            long newedges[] = new long[edges.length];

            for (int i = 0; i < edges.length; i++) {
                int w = (int) (edges[i]&WEIGHT_MASK);
                counts[w]--;
                newedges[counts[w]] = edges[i];
            }

            edges = newedges;
        }

        // actually connect the components. (core part of the algorithm)
        for (int i = 0; i < edges.length; i++) {
            int ida = (int) ((edges[i]>>IDA_SHIFT)&INDEX_MASK);
            int idb = (int) ((edges[i]>>IDB_SHIFT)&INDEX_MASK);
            double w = ((edges[i]>>WEIGHT_SHIFT)&WEIGHT_MASK) / 100.0;

            ida = (int) uf.getRepresentative(ida);
            idb = (int) uf.getRepresentative(idb);

            if (ida == idb)
                continue;

            if (w <= thresh[ida] && w <= thresh[idb]) {

                int root = (int) uf.connectNodes(ida, idb);

                thresh[root] = w + k / uf.getSetSize(root);
            }
        }

        // enforce minimum component size. Join components that are
        // too small by using best edges first.
        for (int i = 0; i < edges.length; i++) {
            int ida = (int) ((edges[i]>>IDA_SHIFT)&INDEX_MASK);
            int idb = (int) ((edges[i]>>IDB_SHIFT)&INDEX_MASK);
            int w = (int) ((edges[i]>>WEIGHT_SHIFT)&WEIGHT_MASK);

            int sza = uf.getSetSize(ida);
            int szb = uf.getSetSize(idb);

            if (sza < minSize || szb < minSize)
                uf.connectNodes(ida, idb);
        }

        BufferedImage _out = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        int out[] = ((DataBufferInt) (_out.getRaster().getDataBuffer())).getData();

        for (int y = 0; y+1 < height; y++) {
            for (int x = 0; x+1 < width; x++) {
                out[y*width+x] = (int) uf.getRepresentative(y*width+x);
            }
        }

        return _out;
    }

    /** Sort and return the first vlength values in v[] by the value
     * of v[i]&amp;mask. The maximum value in the array 'v' is maxv
     * (if maxv is negative, maxv will be found). These weights must
     * be small enough to fit in an integer. This implementation is
     * not stable.
     **/
    public static long[] countingSortLongArray(long v[], int vlength, int maxv, long mask)
    {
        if (maxv < 0) {
            for (int i = 0; i < vlength; i++)
                maxv = Math.max(maxv, (int) (v[i]&mask));
        }

        int counts[] = new int[maxv+1];

        for (int i = 0; i < vlength; i++) {
            int w = (int) (v[i]&mask);
            counts[w]++;
        }

        // accumulate.
        for (int i = 1; i < counts.length; i++)
            counts[i] += counts[i-1];

        long newv[] = new long[vlength];
        for (int i = 0; i < vlength; i++) {
            int w = (int) (v[i]&mask);
            counts[w]--;
            newv[counts[w]] = v[i];
        }

        return newv;
    }
}
