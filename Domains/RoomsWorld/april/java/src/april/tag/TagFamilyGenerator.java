package april.tag;

import java.io.*;
import java.util.*;
import java.awt.image.*;
import javax.imageio.*;

public class TagFamilyGenerator
{
    int nbits;
    int minhamming;
    int mincomplexity;
    int d;

    ArrayList<Long> codelist;
    long starttime;

    long rotcodes[] = new long[16384];
    int nrotcodes = 0;

    public TagFamilyGenerator(int nbits, int minhamming, int mincomplexity)
    {
        this.nbits = nbits;
        this.minhamming = minhamming;
        this.mincomplexity = mincomplexity;
        this.d = (int) Math.sqrt(nbits);

        if (d*d != nbits) {
            System.out.println("WARNING: nbits is not a square. This code may do something stupid.\n");
        }
    }

    static final void printBoolean(PrintStream outs, long v, int nbits)
    {
        for (int b = nbits-1; b >= 0; b--)
            outs.printf("%d", (v & (1L<<b)) > 0 ? 1 : 0);
    }

    static final void printCodes(long codes[], int nbits)
    {
        for (int i = 0; i < codes.length; i++) {
            long w = codes[i];
            System.out.printf("%5d ", i);
            printBoolean(System.out, w, nbits);
            System.out.printf("    %0"+((int) Math.ceil(nbits/4))+"x\n", w);
        }
    }

    public static void main(String args[])
    {
        if (args.length < 2) {
            System.out.printf("usage: <nbits> <minhammingdistance> <mincomplexity>\n");
            System.out.printf("(For all standard tags, nbits is a square.)\n");
            return;
        }

        int nbits = Integer.parseInt(args[0]);
        int minhamming = Integer.parseInt(args[1]);

        // default complexity is a function of the nbits. Values
        // before were tuned by hand so they were "reasonable".

        // Size (bits)  |   Min complexity
        // -------------|-----------------
        //       9      |        3
        //      16      |        5
        //      25      |        8
        //      36      |       10

        // This is approximately: complexity = 0.3 * nbits.
        int reccomplexity = Math.min(10, nbits / 3);

        int mincomplexity = args.length > 2 ? Integer.parseInt(args[2]) : reccomplexity;

        TagFamilyGenerator tfg = new TagFamilyGenerator(nbits, minhamming, mincomplexity);
        tfg.compute();
        tfg.report();
    }

/*
  class ComputeThread extends Thread
  {
  boolean stop = false;
  long iter0;
  int nthreads;
  int threadid;

  public ComputeThread(long iter0, int nthreads, int threadid)
  {
  this.iter0 = iter0;
  this.nthreads = nthreads;
  this.threadid = threadid;
  }

  public void run()
  {
  long v = v0;

  for (long iter = iter0; iter < (1L<<nbits) && !stop; iter += nthreads) {
  v = v0 + 982451653*(iter+1);
  v &= ((1L<<nbits) - 1);

  boolean good = true;

  for (int widx = 0; widx < nrotcodes; widx++) {

  long w = rotcodes[widx];

  if (!hammingDistanceAtLeast(v, w, minhamming)) {
  good = false;
  break;
  }
  }

  if (good) {
  int complexity = computeComplexity(v, d);
  if (complexity < mincomplexity)
  good = false;
  }

  if (good) {

  }
  }
  }
  }
*/

    public TagFamily compute()
    {
        assert(codelist == null);

        codelist = new ArrayList<Long>(); // code lists
        starttime = System.currentTimeMillis();

        // begin our search at a random position to avoid any bias
        // towards small numbers (which tend to have larger regions of
        // solid black).
        long V0 = new Random(nbits*10000 + minhamming*100 + mincomplexity).nextLong();

        long lastreporttime = starttime;
        long lastprogresstime = starttime;
        long lastprogressiters = 0;

        long v = V0;

        for (long iter = 0; iter < (1L<<nbits); iter++) {

            v += 982451653; // big prime.
            v &= ((1L<<nbits) - 1);

            // print a progress report.
            long now = System.currentTimeMillis();
            if (now - lastprogresstime > 5000) {

                if (now - lastreporttime > 60000) {
                    report();
                    lastreporttime = now;
                }

                double donepercent = (iter*100.0)/(1L<<nbits);
                double dt = (now - lastprogresstime)/1000.0;
                long diters = iter - lastprogressiters;
                double rate = diters / dt; // iterations per second
                double secremaining = ((long) (1L<<nbits) - iter) / rate;
                System.out.printf("%8.2f%%  codes: %-5d (%.0f iters/sec, %.2f minutes = %.2f hours)           \r", donepercent, codelist.size(), rate, secremaining/(60.0), secremaining/3600.0);
                lastprogresstime = now;
                lastprogressiters = iter;
            }

            boolean good = true;

            // The tag must be different from itself when rotated.
            if (true) {
                long rv1 = TagFamily.rotate90(v, d);
                long rv2 = TagFamily.rotate90(rv1, d);
                long rv3 = TagFamily.rotate90(rv2, d);

                if (!hammingDistanceAtLeast(v, rv1, minhamming) ||
                    !hammingDistanceAtLeast(v, rv2, minhamming) ||
                    !hammingDistanceAtLeast(v, rv3, minhamming) ||
                    !hammingDistanceAtLeast(rv1, rv2, minhamming) ||
                    !hammingDistanceAtLeast(rv1, rv3, minhamming) ||
                    !hammingDistanceAtLeast(rv2, rv3, minhamming)) {
                    good = false;
                }
            }

            // tag must be different from other tags.
            if (good) {
                for (int widx = 0; widx < nrotcodes; widx++) {

                    long w = rotcodes[widx];

                    if (!hammingDistanceAtLeast(v, w, minhamming)) {
                        good = false;
                        break;
                    }
                }
            }

            // tag must be reasonably complex
            if (good) {
                int complexity = computeComplexity(v, d);
                if (complexity < mincomplexity)
                    good = false;
            }

            // If we like the tag, add it to the db.
            if (good) {
                codelist.add(v);
                long rv1 = TagFamily.rotate90(v, d);
                long rv2 = TagFamily.rotate90(rv1, d);
                long rv3 = TagFamily.rotate90(rv2, d);

                if (nrotcodes + 4 >= rotcodes.length) {
                    long newrotcodes[] = new long[rotcodes.length*2];
                    for (int i = 0; i < rotcodes.length; i++)
                        newrotcodes[i] = rotcodes[i];
                    rotcodes = newrotcodes;
                }

                rotcodes[nrotcodes++] = v;
                rotcodes[nrotcodes++] = rv1;
                rotcodes[nrotcodes++] = rv2;
                rotcodes[nrotcodes++] = rv3;
            }
        }

        long codes[] = new long[codelist.size()];
        for (int i = 0; i < codelist.size(); i++)
            codes[i] = codelist.get(i);

        TagFamily tagFamily = new TagFamily(nbits, minhamming, codes);

        if (true) {
            try {
                tagFamily.writeAllImages("/tmp");
                tagFamily.writeAllImagesMosaic("/tmp/mosaic.png");
            } catch (IOException ex) {
                System.out.println("ex: "+ex);
            }
        }

        return tagFamily;
    }

    void report()
    {
        long codes[] = new long[codelist.size()];
        for (int i = 0; i < codelist.size(); i++)
            codes[i] = codelist.get(i);

        int hds[] = new int[nbits+1];
        int hdtotal = 0;

        // compute hamming distance table
        for (int i = 0; i < codelist.size(); i++) {
            long rv0 = codelist.get(i);
            long rv1 = TagFamily.rotate90(rv0, d);
            long rv2 = TagFamily.rotate90(rv1, d);
            long rv3 = TagFamily.rotate90(rv2, d);

            for (int j = i+1; j < codelist.size(); j++) {
                int dist = Math.min(Math.min(TagFamily.hammingDistance(rv0, codelist.get(j)),
                                             TagFamily.hammingDistance(rv1, codelist.get(j))),
                                    Math.min(TagFamily.hammingDistance(rv2, codelist.get(j)),
                                             TagFamily.hammingDistance(rv3, codelist.get(j))));


                hds[dist]++;
                if (dist < minhamming) {
                    System.out.printf("ERROR, dist = %3d: %d %d\n", dist, i, j);
                }
                hdtotal++;
            }
        }

        System.out.printf("\n\npackage april.tag;\n\n");
        String cname = String.format("Tag%dh%d", nbits, minhamming);
        System.out.printf("/** Tag family with %d distinct codes.\n", codes.length);
        System.out.printf("    bits: %d,  minimum hamming: %d,  minimum complexity: %d\n\n", nbits, minhamming, mincomplexity);

        // compute some ROC statistics, assuming randomly-visible targets
        // as a function of how many bits we're willing to correct.
        System.out.printf("    Max bits corrected       False positive rate\n");

        for (int cbits = 0; cbits <= (minhamming-1)/2; cbits++) {
            int validCodes = 0; // how many input codes will be mapped to a single valid code?
            // it's the number of input codes that have 0 errors, 1 error, 2 errors, ..., cbits errors.
            for (int i = 0; i <= cbits; i++)
                validCodes += choose(nbits, i);

            validCodes *= codes.length; // total number of codes

            System.out.printf("          %3d             %15f %%\n", cbits, (100.0*validCodes)/(1L<<nbits));
        }

        System.out.printf("\n    Generation time: %f s\n\n", (System.currentTimeMillis() - starttime)/1000.0);

        System.out.printf("    Hamming distance between pairs of codes (accounting for rotation):\n\n");
        for (int i = 0; i < hds.length ;i++) {
            System.out.printf("    %4d  %d\n", i, hds[i]);
        }

        System.out.printf("**/\n");

        System.out.printf("public class %s extends TagFamily\n",cname);
        System.out.printf("{\n");
        System.out.printf("\tpublic %s()\n", cname);
        System.out.printf("\t{\n");
        System.out.printf("\t\tsuper(%d, %d, ", nbits, minhamming);
        System.out.printf("new long[] { ");
        for (int i = 0; i < codes.length; i++) {
            long w = codes[i];
            System.out.printf("0x%0"+((int) Math.ceil(nbits/4))+"xL", w);
            if (i+1 == codes.length)
                System.out.printf(" });\n");
            else
                System.out.printf(", ");
        }
        System.out.printf("\t}\n");
        System.out.printf("}\n");
        System.out.printf("\n");
    }

    static long choose(int n, int c)
    {
        long v = 1;
        for (int i = 0; i < c; i++)
            v *= (n-i);
        for (int i = 1; i <= c; i++)
            v /= i;
        return v;
    }

    static int computeComplexity(long v, int d)
    {
        int a[][] = new int[d][d];

        for (int y = 0; y < d; y++) {
            for (int x = 0; x < d; x++) {
                a[y][x] = (v & 1) > 0 ? 1 : 0;
                v = v>>1;
            }
        }

        return computeComplexity(a);
    }

/** Compute the hamming distance between two longs. **/
    public static final int hammingDistance(long a, long b)
    {
        return popCount2(a^b);
    }

    public static final boolean hammingDistanceAtLeast(long a, long b, int minval)
    {
        long w = a^b;

        int count = 0;

        while (w != 0) {
            count += popCountTable[(int) (w&(popCountTable.length-1))];
            if (count >= minval)
                return true;

            w >>= popCountTableShift;
        }

        return false;
    }

/** How many bits are set in the long? **/
    static final int popCountReal(long w)
    {
        int cnt = 0;
        while (w != 0) {
            w &= (w-1);
            cnt++;
        }
        return cnt;
    }

    public static final int popCount(long x)
    {
        int c;

        x = (x & 0x5555555555555555L) + ((x>>1) & 0x5555555555555555L);
        x = (x & 0x3333333333333333L) + ((x>>2) & 0x3333333333333333L);
        c = ((int)x) + ((int)(x>>32));
        c = (c & 0x0f0f0f0f) + ((c>>4 ) & 0x0f0f0f0f);
        c = (c & 0x00ff00ff) + ((c>>8 ) & 0x00ff00ff);
        c = (c & 0x0000ffff) + ((c>>16) & 0x0000ffff);

        return c;
    }

    static final int popCountTableShift = 16;
    static final byte[] popCountTable = new byte[1<<popCountTableShift];
    static {
        for (int i = 0; i < popCountTable.length; i++) {
            popCountTable[i] = (byte) popCountReal(i);
        }
    }

    public static final int popCount2(long w)
    {
        int count = 0;

        while (w != 0) {
            count += popCountTable[(int) (w&(popCountTable.length-1))];
            w >>= popCountTableShift;
        }
        return count;
    }

/** Given a 2D array of "pixels", what is the minimum number of
 * rectangles needed to draw that pattern? This is a measure of
 * the complexity of the pattern.
 *
 * The problem itself is NP-hard, but we employ a greedy
 * approximation that, at each time step, tries *every* rectangle
 * and picks the rectangle that reduces the number of errors. This
 * is horrifically slow, but for our purposes, we're only doing
 * very small images...
 *
 * @param foo
 **/
    static int computeComplexity(int d[][])
    {
        boolean verbose = false;

        int width = d[0].length, height = d.length;

        int out[][] = new int[height][width];
        int numrectangles = 0;

        // initialize output to invalid color.
        for (int y = 0; y < height; y++)
            for (int x = 0; x < width; x++)
                out[y][x] = -1;

        while (true) {

            // What is our error now?
            int error = 0;

            for (int y = 0; y < height; y++)
                for (int x = 0; x < width; x++)
                    if (d[y][x] != out[y][x])
                        error++;

            // are we done?
            if (error == 0)
                break;

            int bestimprovement = 0;
            int besty0 = -1, bestx0 = -1, besty1 = -1, bestx1 = -1, bestv = -1;

            // search over all rectangles: which one will reduce the
            // error the most?
            for (int y0 = 0; y0 < height; y0++) {
                for (int y1 = y0; y1 < height; y1++) {
                    for (int x0 = 0; x0 < width; x0++) {
                        for (int x1 = x0; x1 < width; x1++) {

                            for (int v = 0; v < 2; v++) {

                                int improvement = 0;

                                for (int y = y0; y <= y1; y++) {
                                    for (int x = x0; x <= x1; x++) {
                                        if (d[y][x] == out[y][x]) {
                                            if (d[y][x] == v) {
                                                // no change, still right.
                                            } else {
                                                improvement--;
                                            }
                                        } else {
                                            if (d[y][x] == v) {
                                                improvement++;
                                            } else {
                                                // no change, still wrong.
                                            }
                                        }
                                    }
                                }

                                if (improvement > bestimprovement) {
                                    besty0 = y0;
                                    bestx0 = x0;
                                    besty1 = y1;
                                    bestx1 = x1;
                                    bestv = v;
                                    bestimprovement = improvement;
                                }
                            }
                        }
                    }
                }
            }

            // implement change
            for (int y = besty0; y <= besty1; y++) {
                for (int x = bestx0; x <= bestx1; x++) {
                    out[y][x] = bestv;
                }
            }

            numrectangles++;

            if (verbose)
                System.out.printf("(%d %d) (%d %d): color %d improvement %d\n",
                                  bestx0, besty0, bestx1, besty1, bestv, bestimprovement);
        }

        return numrectangles;
    }
}
