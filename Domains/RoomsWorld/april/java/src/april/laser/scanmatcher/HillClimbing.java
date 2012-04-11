package april.laser.scanmatcher;

import java.util.*;

import april.util.*;
import april.jmat.*;
import april.config.*;

public class HillClimbing
{
    GridMap gm;

    public boolean refine;
    public double refine_initial_stepsize[];
    public double refine_minimum_stepsize[];
    public double refine_shrink_ratio;
    public int    refine_max_iterations;

    public HillClimbing(Config config)
    {
        this.refine = config.getBoolean("refine", true);
        this.refine_initial_stepsize = config.getDoubles("refine_initial_stepsize", new double[] { 0.1, 0.1, Math.toRadians(2) });
        this.refine_minimum_stepsize = config.getDoubles("refine_minimum_stepsize", new double[] { 0.001, 0.001, Math.toRadians(0.05) });
        this.refine_shrink_ratio = config.getDouble("refine_shrink_ratio", 0.7);
        this.refine_max_iterations = config.getInt("refine_max_iterations", 1000);
    }

    public void setModel(GridMap gm)
    {
        this.gm = gm;
    }

    public double[] matchGreedy(ArrayList<double[]> points, double prior[], double pinv[][],
                                double xyt0[])
    {
        double x = xyt0[0];
        double y = xyt0[1];
        double t = xyt0[2];

        double score = gm.score(points, x, y, t, prior, pinv);
        double stepsize[] = LinAlg.copy(refine_initial_stepsize);

        double newxyt[] = new double[3];

        int iterations = 0;
        int direction = 0;

        for (iterations = 0; iterations < refine_max_iterations; iterations++) {

            switch (direction) {
                case 0 :
                    newxyt[0] = x + stepsize[0];
                    newxyt[1] = y;
                    newxyt[2] = t;
                    break;

                case 3:
                    newxyt[0] = x - stepsize[0];
                    newxyt[1] = y;
                    newxyt[2] = t;
                    break;

                case 1:
                    newxyt[0] = x;
                    newxyt[1] = y + stepsize[1];
                    newxyt[2] = t;
                    break;

                case 4:
                    newxyt[0] = x;
                    newxyt[1] = y - stepsize[1];
                    newxyt[2] = t;
                    break;

                case 2:
                    newxyt[0] = x;
                    newxyt[1] = y;
                    newxyt[2] = t + stepsize[2];
                    break;

                case 5:
                    newxyt[0] = x;
                    newxyt[1] = y;
                    newxyt[2] = t - stepsize[2];
                    break;
            }

            // move in the best direction. (Roughly a local gradient search.)
            double newscore = gm.score(points, newxyt[0], newxyt[1], newxyt[2], prior, pinv);

            if (newscore > score) {
                score = newscore;
                x = newxyt[0];
                y = newxyt[1];
                t = newxyt[2];

                // repeat with same step size and direction
                continue;
            }

            // score didn't go up. try a different direction.
            direction = (direction + 1) % 6;

            // have we tried all the directions?
            if (direction == 0) {
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

        }

        return new double[] {x, y, t, score};
    }

    public double[] match(ArrayList<double[]> points, double prior[], double pinv[][],
                          double xyt0[])
    {
        double x = xyt0[0];
        double y = xyt0[1];
        double t = xyt0[2];

        double score = gm.score(points, x, y, t, prior, pinv);
        double stepsize[] = LinAlg.copy(refine_initial_stepsize);

        double newxyts[][] = new double[6][3];

        int iterations = 0;

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

        return new double[] {x, y, t, score};
    }
}
