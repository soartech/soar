package april.laser.scanmatcher;

import java.util.*;

import april.util.*;
import april.jmat.*;
import april.config.*;

public class MultiResolutionScanMatcher
{
    public int decimate = 10;
    public boolean debug = false;

    public boolean refine;
    public double refine_initial_stepsize[];
    public double refine_minimum_stepsize[];
    public double refine_shrink_ratio;
    public int    refine_max_iterations;

//    public boolean reweight;
//    public double  reweight_distance;

    // maximum number of times to pick a low resolution cell to be
    // researched at high resolution
    public int    max_search_iterations;

    GridMap gm;  // grid map
    GridMap dgm; // decimated grid map

    /** Pass in a Config whose scope is already set to the scanmatcher. **/
    public MultiResolutionScanMatcher(Config config)
    {
        this.decimate = config.getInt("decimate", decimate);
        this.debug = config.getBoolean("debug", debug);

        this.refine = config.getBoolean("refine", true);
        this.refine_initial_stepsize = config.getDoubles("refine_initial_stepsize", new double[] { 0.1, 0.1, Math.toRadians(2) });
        this.refine_minimum_stepsize = config.getDoubles("refine_minimum_stepsize", new double[] { 0.001, 0.001, Math.toRadians(0.05) });
        this.refine_shrink_ratio = config.getDouble("refine_shrink_ratio", 0.7);
        this.refine_max_iterations = config.getInt("refine_max_iterations", 1000);

//        this.reweight = config.getBoolean("reweight", true);
//        this.reweight_distance = config.getDouble("reweight_distance", 0.1);

        this.max_search_iterations = config.getInt("max_search_iterations", 100);
    }

    /** Set a new model for future scanmatching operations.
     * @param gm The high-resolution model. It will be decimated as necessary.
     **/
    public void setModel(GridMap gm)
    {
        this.gm = gm;
        if (decimate <= 1) {
            dgm = gm;
        } else {
            dgm = gm.decimateMax(decimate);
            dgm = dgm.max4(); // necessary to avoid quantization problems.
        }
    }

    /** @param points New laser points projected into the robot's coordinate frame.
        @param prior The prior. The mean encodes the center of the
        search area. NOTE ON THE COVARIANCE: The prior is weighed
        against the correlation function and thus must be scaled
        appropriately. The correlation function produces values
        between [0, 255] for every point, and the covariance needs to
        be able to 'compete' with that. A good rule of thumb is to
        scale the covariance matrix by (1.0 / (npoints*10)) or so.
        @param xrange,yrange Search range, in meters. (The search area will have width xrange*2).
        @param thetaRange search range in radians (The search volume will have height thetaRange*2)
        @param thetaResolution Initial search step size for theta, in
        radians (sane value Math.toRadians(0.5)).
        @param boolean refine Refine the result using hill climbing.

        If the search range is too small, the covariance won't be
        meaningful. Specifically, the covariance is computed using the
        decimated search space, and so the search window should be
        reasonably large. (3x3 at a bare minimum, 10x10 better). If
        you don't care about the covariance, then this is irrelevant.
    **/
    public MultiGaussian match(ArrayList<double[]> points, double priormean[], double priorvar[][],
                               double xrange, double yrange, double thetaRange, double thetaResolution)
    {
        double pinv[][] = priorvar == null ? null : LinAlg.inverse(priorvar);
        MultiGaussian mg = matchRaw(points, priormean, pinv,
                                    xrange, yrange, thetaRange, thetaResolution);

        // do refinement
        if (refine) {
            double xyt0[] = mg.getMean();
            double xyt1[] = refine(points, xyt0[0], xyt0[1], xyt0[2], priormean, pinv);
            mg = new MultiGaussian(mg.getCovariance(), xyt1);
        }

        // we're done.
        return mg;
    }

    /** Estimate the distribution p(xyt | model, points), i.e., not paying attention to the prior. **/
    MultiGaussian matchRaw(ArrayList<double[]> points, double priorxyt[], double pinv[][],
                           double xrange, double yrange, double thetaRange, double thetaResolution)
    {
        /////////////////////////////////////////////////////////////////
        // Step 1. Perform search at low resolution
        double lowResXYT[] = new double[] { priorxyt[0] - xrange,
                                            priorxyt[1] - yrange,
                                            priorxyt[2] - thetaRange };

        IntArray2D lowResScores[] = dgm.scores3D(points,
                                                 lowResXYT[0], (int) (2*xrange/dgm.metersPerPixel + 1),
                                                 lowResXYT[1], (int) (2*yrange/dgm.metersPerPixel + 1),
                                                 lowResXYT[2], thetaResolution,
                                                 Math.max(1, (int) (2*thetaRange/thetaResolution)),
                                                 priorxyt, pinv);


        /////////////////////////////////////////////////////////////////
        // Step 1. Compute the covariance by fitting a Gauassian to the
        // low-resolution samples.
        MultiGaussianEstimator mge = new MultiGaussianEstimator(3);

        if (false) {
            double xyt[] = new double[3];

            for (int tidx = 0; tidx < lowResScores.length; tidx++) {
                xyt[2] = lowResXYT[2] + tidx*thetaResolution;

                for (int yidx = 0; yidx < lowResScores[tidx].dim1; yidx++) {
                    xyt[1] = lowResXYT[1] + yidx*dgm.metersPerPixel;

                    for (int xidx = 0; xidx < lowResScores[tidx].dim2; xidx++) {

                        xyt[0] = lowResXYT[0] + xidx*dgm.metersPerPixel;
                        int v = lowResScores[tidx].get(yidx, xidx);
                        mge.observeWeighted(xyt, MathUtil.exp(-256.0 + v));
                    }
                }
            }
        }

        /////////////////////////////////////////////////////////////////
        // If no decimation has been requested, return a result now.
        if (decimate <= 1) {
            int bestidx0[] = bestIndices(lowResScores);
            double xyt0score = lowResScores[bestidx0[2]].get(bestidx0[1], bestidx0[0]);

            double u[] = new double[] { lowResXYT[0] + bestidx0[0]*dgm.metersPerPixel,
                                        lowResXYT[1] + bestidx0[1]*dgm.metersPerPixel,
                                        lowResXYT[2] + bestidx0[2]*thetaResolution };


            // Heuristic: posterior's mean is the MLE, posterior's
            // covariance is the fit covariance.
            MultiGaussian mg = mge.getEstimate();
            return new MultiGaussian(mg.getCovariance(), u);
        }

        /////////////////////////////////////////////////////////////////
        // Step 3. Search promising low-resolution voxels at high resolution.
        // Consider the peaks at low resolution in decreasing order. For each, compute
        // the corresponding cell at high resolution.
        int bestLowResIdx[] = new int[3];
        int bestHighResScore = -1;
        double bestHighResXYT[] = new double[3];

        for (int iters = 0; true; iters++) {

            if (debug) {
                if ((iters >= 100 && (iters & (iters - 1))==0) || iters == max_search_iterations)
                    System.out.printf("WRN: MultiResolutionScanMatcher: many iterations (%d)\n", iters);
            }

            // Find the next best score that's less than maxscore
            // This implementation just researches the entire low
            // resolution volume. We could do better with a MaxHeap
            // type data structure, but emperically, this does not
            // appear to take a significant amount of time.
            int thisBestLowResScore = -1;

            for (int tidx = 0; tidx < lowResScores.length; tidx++) {
                for (int yidx = 0; yidx < lowResScores[tidx].dim1; yidx++) {
                    for (int xidx = 0; xidx < lowResScores[tidx].dim2; xidx++) {
                        int score = lowResScores[tidx].get(yidx, xidx);
                        if (score > thisBestLowResScore) {
                            thisBestLowResScore = score;
                            bestLowResIdx[0] = xidx;
                            bestLowResIdx[1] = yidx;
                            bestLowResIdx[2] = tidx;
                        }
                    }
                }
            }

            if (iters > max_search_iterations || bestHighResScore >= thisBestLowResScore) {
                // we're done: we have not found another low
                // resolution voxel that needs to be searched. Thus,
                // we return a result now.
                MultiGaussian mg = mge.getEstimate();
                return new MultiGaussian(mg.getCovariance(), bestHighResXYT);
            }

            lowResScores[bestLowResIdx[2]].set(bestLowResIdx[1], bestLowResIdx[0], - thisBestLowResScore);

            // evaluate this grid at high resolution.
            // xyt1 is the lower-left corner of the cell that contained the maximum.
            double xyt1[] = new double[] { lowResXYT[0] + bestLowResIdx[0]*dgm.metersPerPixel,
                                           lowResXYT[1] + bestLowResIdx[1]*dgm.metersPerPixel,
                                           lowResXYT[2] + bestLowResIdx[2]*thetaResolution };

            IntArray2D highResScores = gm.scores2D(points,
                                                   xyt1[0], decimate,
                                                   xyt1[1], decimate,
                                                   xyt1[2],
                                                   priorxyt, pinv);

            int thisBestHighResIdx[] = bestIndices(highResScores);
            int thisBestHighResScore = highResScores.get(thisBestHighResIdx[1], thisBestHighResIdx[0]);

            if (thisBestHighResScore > bestHighResScore) {
                bestHighResScore = thisBestHighResScore;
                bestHighResXYT[0] = xyt1[0] + thisBestHighResIdx[0]*gm.metersPerPixel;
                bestHighResXYT[1] = xyt1[1] + thisBestHighResIdx[1]*gm.metersPerPixel;
                bestHighResXYT[2] = xyt1[2];
            }

            if (thisBestHighResScore > thisBestLowResScore && debug) {
                // TODO: Investigate cases where this
                // happens. Numerical precision problems?
                System.out.printf("DEBUG: MultiResolutionScanMatcher %10d %10d %10d [%5d %5d %5d] [%5d %5d]\n",
                                  thisBestLowResScore, thisBestHighResScore,
                                  thisBestHighResScore - thisBestLowResScore,
                                  bestLowResIdx[0], bestLowResIdx[1], bestLowResIdx[2],
                                  thisBestHighResIdx[0], thisBestHighResIdx[1]);
            }
        }
    }

    static int[] bestIndices(IntArray2D scores)
    {
        double bestscore = -Double.MAX_VALUE;
        int bestidx[] = new int[2];

        for (int yidx = 0; yidx < scores.dim1; yidx++) {
            for (int xidx = 0; xidx < scores.dim2; xidx++) {
                int score = scores.get(yidx, xidx);
                if (score > bestscore) {
                    bestscore = score;
                    bestidx[0] = xidx;
                    bestidx[1] = yidx;
                }
            }
        }

        return bestidx;
    }

    static int[] bestIndices(IntArray2D scores[])
    {
        double bestscore = -Double.MAX_VALUE;
        int bestidx[] = new int[3];

        for (int tidx = 0; tidx < scores.length; tidx++) {
            for (int yidx = 0; yidx < scores[tidx].dim1; yidx++) {
                for (int xidx = 0; xidx < scores[tidx].dim2; xidx++) {
                    int score = scores[tidx].get(yidx, xidx);
                    if (score > bestscore) {
                        bestscore = score;
                        bestidx[0] = xidx;
                        bestidx[1] = yidx;
                        bestidx[2] = tidx;
                    }
                }
            }
        }

        return bestidx;
    }

    public double[] refine(ArrayList<double[]> points, double x, double y, double t, double prior[], double pinv[][])
    {
        double score = gm.score(points, x, y, t, prior, pinv);
        double stepsize[] = LinAlg.copy(refine_initial_stepsize);

        double newxyts[][] = new double[6][3];

        int iterations = 0;

//        double weights[] = computeWeights(points);

        for (iterations = 0; iterations < refine_max_iterations; iterations++) {

            newxyts[0][0] = x + stepsize[0];
            newxyts[0][1] = y;
            newxyts[0][2] = t;

            newxyts[1][0] = x - stepsize[0];
            newxyts[1][1] = y;
            newxyts[1][2] = t;

            newxyts[2][0] = x;
            newxyts[2][1] = y + stepsize[1];
            newxyts[2][2] = t;

            newxyts[3][0] = x;
            newxyts[3][1] = y - stepsize[1];
            newxyts[3][2] = t;

            newxyts[4][0] = x;
            newxyts[4][1] = y;
            newxyts[4][2] = t + stepsize[2];

            newxyts[5][0] = x;
            newxyts[5][1] = y;
            newxyts[5][2] = t - stepsize[2];

            // move in the best direction. (Roughly a local gradient search.)
            boolean stepped = false;
            for (int i = 0; i < 6; i++) {
                double newscore = gm.score(points, newxyts[i][0], newxyts[i][1], newxyts[i][2], prior, pinv);

                if (newscore > score) {
                    stepped = true;
                    score = newscore;
                    x = newxyts[i][0];
                    y = newxyts[i][1];
                    t = newxyts[i][2];
                }
            }

            // That step was good. Keep going at the same step size.
            if (stepped)
                continue;

            // We've rejected that step. Reduce our step size.
            boolean searchMore = false;
            for (int i = 0; i < 3; i++) {
                if (stepsize[i] > refine_minimum_stepsize[i]) {
                    searchMore = true;
                    stepsize[i] = Math.max(refine_minimum_stepsize[i], stepsize[i]*refine_shrink_ratio);
                }
            }

            if (!searchMore)
                break;
        }

        return new double[] {x, y, t};
    }

/*
    double[] computeWeights(ArrayList<double[]> points)
    {
        if (!reweight) {
            double weights[] = new double[points.size()];
            for (int i = 0; i < points.size(); i++) {
                weights[i] = 1.0;
            }
            return weights;
        }

        double weights[] = new double[points.size()];
        for (int i = 0; i < points.size(); i++) {
            double mindist = reweight_distance;
            if (i > 0)
                mindist = Math.min(mindist, LinAlg.distance(points.get(i), points.get(i-1)));
            if (i+1 < points.size())
                mindist = Math.min(mindist, LinAlg.distance(points.get(i), points.get(i+1)));

            weights[i] = mindist / 0.1;
        }

        return weights;
    }
*/
}
