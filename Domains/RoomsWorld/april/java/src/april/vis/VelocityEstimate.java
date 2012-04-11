package april.vis;

import java.util.*;
import april.jmat.*;

/** Compute least-squares estimate of velocity and position given a history of 2D positions.
 **/
public class VelocityEstimate
{
    /** Prediction of where the object was at a given time. **/
    public static class Prediction
    {
        public double time;
        public double position[];
        public double velocity[];
        public double mse[];       // mean squared error

        public String toString()
        {
            return String.format("VelocityEstimate [ t = %15.5f, xy=(%15.5f, %15.5f), vel=(%15.5f, %15.5f), err = (%15.5f, %15.5f)",
                                 time, position[0], position[1], velocity[0], velocity[1], mse[0], mse[1]);
        }
    }

    class PositionRecord
    {
        double time;
        double position[];
    }

    ArrayList<PositionRecord> positions = new ArrayList<PositionRecord>();
    double maxAge;

    public VelocityEstimate(double maxAge)
    {
        this.maxAge = maxAge;
    }

    void removeOldSamples(double time)
    {
        for (int i = 0; i < positions.size(); i++) {
            double age = time - positions.get(i).time;
            if (age > maxAge) {
                positions.remove(i);
                i--;
            }
            break;
        }
    }

    public void reset()
    {
        positions.clear();
    }


    public void update(double time, double pos[])
    {
        removeOldSamples(time);

        PositionRecord rec = new PositionRecord();
        rec.time = time;
        rec.position = LinAlg.copy(pos);

        positions.add(rec);
    }

    public Prediction getPrediction(double time)
    {
        removeOldSamples(time);

        ////////////////////////////////////
        // not-enough data special cases
        if (positions.size()==0)
            return null;

        if (positions.size()==1) {
            Prediction pred = new Prediction();
            pred.time = time;
            pred.position = LinAlg.copy(positions.get(0).position);
            pred.velocity = new double[2];
            pred.mse = new double[2];
            return pred;
        }

        ////////////////////////////////////
        // we have enough data.
        double A[][] = new double[2][2];
        double BX[] = new double[2];
        double BY[] = new double[2];
        double X2 = 0, Y2 = 0;

        double minage = Double.MAX_VALUE;
        double maxage = -Double.MAX_VALUE;

        for (PositionRecord rec : positions) {
            double age = time - rec.time;
            minage = Math.min(minage, age);
            maxage = Math.max(maxage, age);

            double W = 1; // allow weighting as a f(age) later.
            double W2 = W*W;

            A[0][0] += W2;
            A[0][1] += -W2*age;
            A[1][0] += -W2*age;
            A[1][1] += W2*age*age;

            BX[0] += W2*rec.position[0];
            BX[1] += -W2*rec.position[0]*age;

            BY[0] += W2*rec.position[1];
            BY[1] += -W2*rec.position[1]*age;

            X2 += W2*rec.position[0]*rec.position[0];
            Y2 += W2*rec.position[1]*rec.position[1];
        }

        if (Math.abs(maxage-minage) < 0.001) {
            Prediction pred = new Prediction();
            pred.time = time;
            pred.position = LinAlg.copy(positions.get(0).position);
            pred.velocity = new double[2];
            pred.mse = new double[2];
            return pred;
        }

        double Ainv[][] = LinAlg.inverse(A);
        double xpred[] = LinAlg.matrixAB(Ainv, BX);
        double ypred[] = LinAlg.matrixAB(Ainv, BY);

        double x0 = xpred[0], vx = xpred[1];
        double y0 = ypred[0], vy = ypred[1];

        double xchi = x0*(A[0][0]*x0 + A[0][1]*vx) +
            vx*(A[1][0]*x0 + A[1][1]*vx) +
            - 2 * (x0 * BX[0] + vx * BX[1]) +
            X2;

        double ychi = y0*(A[0][0]*y0 + A[0][1]*vy) +
            vy*(A[1][0]*y0 + A[1][1]*vy) +
            - 2 * (y0 * BY[0] + vy * BY[1]) +
            Y2;

        xchi /= positions.size();
        ychi /= positions.size();

        Prediction pred = new Prediction();
        pred.time = time;
        pred.position = new double[] {xpred[0], ypred[0]};
        pred.velocity = new double[] {xpred[1], ypred[1]};
        pred.mse = new double[] {xchi, ychi};

        return pred;
    }
}
