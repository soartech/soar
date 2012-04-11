package april.image.corner;

import april.jmat.*;
import april.image.*;

public class KanadeTomasi
{
    int halfsize;

    /** @param halfsize Controls the window size; for 3x3 use 1. For
     * 5x5 use 2, and so on. **/
    public KanadeTomasi(int halfsize)
    {
        this.halfsize = halfsize;
    }

    public FloatImage computeResponse(FloatImage data)
    {
        return computeResponse(data, null);
    }

    /** Compute the corner response, except each pixel also has a
     * confidence mask. Pixels with small confidences do not
     * contribute to the gradients. **/
    public FloatImage computeResponse(FloatImage data, FloatImage conf)
    {
        int width = data.width;
        int height = data.height;

        ///////////////////////////////////////////////
        // Compute gradients at every pixel.
        FloatImage response = new FloatImage(width, height);
        FloatImage fimIx2 = new FloatImage(width, height);
        FloatImage fimIxIy = new FloatImage(width, height);
        FloatImage fimIy2 = new FloatImage(width, height);

        for (int y = 1; y + 1 < height; y++) {
            for (int x = 1; x + 1 < width; x++) {
                float vp0 = data.d[y*width + x + 1];
                float vn0 = data.d[y*width + x - 1];
                float v0p = data.d[y*width + x + width];
                float v0n = data.d[y*width + x - width];
                float Ix = (vp0 - vn0);
                float Iy = (v0p - v0n);

                if (conf != null) {
                    float cp0 = conf.d[y*width + x + 1];
                    float cn0 = conf.d[y*width + x - 1];
                    float c0p = conf.d[y*width + x + width];
                    float c0n = conf.d[y*width + x - width];

                    Ix *= Math.min(cp0, cn0);
                    Iy *= Math.min(c0p, c0n);
                }

                fimIx2.set(x, y, Ix * Ix);
                fimIxIy.set(x, y, Ix * Iy);
                fimIy2.set(x, y, Iy * Iy);
            }
        }

        ///////////////////////////////////////////////
        // Convolve with gaussian. This makes it rotationally invariant.
        double sigma = 0.5 * halfsize;
        float gaussian[] = SigProc.makeGaussianFilter(sigma, halfsize * 2 + 1);


        fimIx2 = fimIx2.filterFactoredCentered(gaussian, gaussian);
        fimIxIy = fimIxIy.filterFactoredCentered(gaussian, gaussian);
        fimIy2 = fimIy2.filterFactoredCentered(gaussian, gaussian);

        ///////////////////////////////////////////////
        // Compute filter response.
        for (int y = 1; y + 1 < height; y++) {
            for (int x = 1; x + 1 < width; x++) {
                double A = fimIx2.get(x, y), B = fimIxIy.get(x, y), D = fimIy2.get(x, y);
                // The ellipse (autocorrelation matrix) is [A B; C D]
                // = [A B; B D]. We're really interested in the
                // minimum eigenvalue, but this is slower to
                // compute. Instead, we can get some relevant
                // information by considering the determinant (product
                // of the eigenvalues) and trace (sum of the
                // eigenvalues).
                response.set(x, y, (float) minEig(A, B, D));
            }
        }

        return response;
    }

    // Compute the minimum eigenvalue of the matrix [a b; b d]
    static final double minEig(double a, double b, double d)
    {
        // 2a^2 + 4b^2 + 2d^2 - 2*(a+d)sqrt(a^2 - 2ad + d^2 + 4b^2)
        double SA = (a + d) * (a + d) * (a * a - 2 * a * d + d * d + 4 * b * b);
        assert (SA >= 0);
        SA = Math.sqrt(SA);
        double SB = 2 * a * a + 4 * b * b + 2 * d * d - 2 * SA;
        if (SB < 0) // can be negative due to numerical precision.
            return 0;
        SB = Math.sqrt(SB);
        return 0.5 * SB;
    }
}
