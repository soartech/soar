package april.tag;

/** Compute geometric complexity of a tag. **/
public class Complexity
{
    /** Compute rectangle covering for tag v of dimensions dxd **/
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

    /** Given a 2D array of "pixels", what is the minimum number of
     * rectangles needed to draw that pattern? This is a measure of
     * the complexity of the pattern.
     *
     * The problem itself is NP-hard (I believe), but we employ a
     * greedy approximation that, at each time step, tries *every*
     * rectangle and picks the rectangle that reduces the number of
     * errors. This is horrifically slow, but for our purposes, we're
     * only doing very small images...
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
