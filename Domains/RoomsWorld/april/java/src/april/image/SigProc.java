package april.image;

import april.jmat.*;

public class SigProc
{
    static boolean warned = false;

    /** Convolve the input 'a' (which begins at offset aoff and is
     * alen elements in length) with the filter 'f', depositing the
     * result in 'r' at the offset 'roff'. f.length should be odd. The
     * output is shifted by -f.length/2, so that there is no net time
     * delay.
     **/
    public static final void convolveSymmetricCentered(float a[], int aoff, int alen, float f[], float r[], int roff)
    {
        if ((f.length&1)==0 && !warned) {
            System.out.println("SigProc.convolveSymmetricCentered Warning: filter is not odd length");
            warned = true;
        }

        for (int i = f.length/2; i < f.length; i++) {
            double acc = 0;
            for (int j = 0; j < f.length; j++) {
                if ((i - j) < 0 || (i - j) >= alen)
                    acc += a[aoff] * f[j];
                else
                    acc += a[aoff + i - j] * f[j];
            }
            r[roff + i - f.length/2] = (float) acc;
        }

        for (int i = f.length; i < alen; i++) {
            double acc = 0;
            for (int j = 0; j < f.length; j++) {
                acc += a[aoff + i - j] * f[j];
            }
            r[roff + i - f.length/2] = (float) acc;
        }

        for (int i = alen; i < alen + f.length/2; i++) {
            double acc = 0;
            for (int j = 0; j < f.length; j++) {
                if ((i - j) >= alen || (i - j) < 0)
                    acc += a[aoff + alen - 1] * f[j];
                else
                    acc += a[aoff + i - j] * f[j];
            }
            r[roff + i - f.length/2] = (float) acc;
        }
    }

    public static final void convolveSymmetricCenteredMax(float a[], int aoff, int alen, float f[], float r[], int roff)
    {
        if ((f.length&1)==0 && !warned) {
            System.out.println("SigProc.convolveSymmetricCentered Warning: filter is not odd length");
            warned = true;
        }

        for (int i = f.length/2; i < f.length; i++) {
            double acc = 0;
            for (int j = 0; j < f.length; j++) {
                if ((i - j) < 0 || (i - j) >= alen)
                    acc = Math.max(acc, a[aoff] * f[j]);
                else
                    acc = Math.max(acc, a[aoff + i - j] * f[j]);
            }
            r[roff + i - f.length/2] = (float) acc;
        }

        for (int i = f.length; i < alen; i++) {
            double acc = 0;
            for (int j = 0; j < f.length; j++) {
                acc = Math.max(acc, a[aoff + i - j] * f[j]);
            }
            r[roff + i - f.length/2] = (float) acc;
        }

        for (int i = alen; i < alen + f.length/2; i++) {
            double acc = 0;
            for (int j = 0; j < f.length; j++) {
                if ((i - j) >= alen || (i - j) < 0)
                    acc = Math.max(acc, a[aoff + alen - 1] * f[j]);
                else
                    acc = Math.max(acc, a[aoff + i - j] * f[j]);
            }
            r[roff + i - f.length/2] = (float) acc;
        }
    }

    public static final void convolveCenteredDisc2DMaxCont(int a[], int width, int height, double radius, double mpp, int r[])
    {
        int pxRadius = (int) Math.ceil(radius/mpp);
        for (int l=-pxRadius; l <= pxRadius; l++) {
            for (int k=-pxRadius; k <= pxRadius; k++) {
                // is it within the radius?
                double k_m = k*mpp;
                double l_m = l*mpp;
                double c00_m = Math.sqrt((k_m - mpp/2)*(k_m - mpp/2) + (l_m - mpp/2)*(l_m - mpp/2));
                double c01_m = Math.sqrt((k_m - mpp/2)*(k_m - mpp/2) + (l_m + mpp/2)*(l_m + mpp/2));
                double c10_m = Math.sqrt((k_m + mpp/2)*(k_m + mpp/2) + (l_m - mpp/2)*(l_m - mpp/2));
                double c11_m = Math.sqrt((k_m + mpp/2)*(k_m + mpp/2) + (l_m + mpp/2)*(l_m + mpp/2));
                double rad = Math.min(Math.min(c00_m, c01_m), Math.min(c10_m, c11_m));

                if (rad >= radius)
                    continue;

                int ymin = Math.max(0, 0-k);
                int ymax = Math.min(height, height - k);
                int xmin = Math.max(0, 0-l);
                int xmax = Math.min(width, width - l);

                for (int y = ymin; y < ymax; y++) {
                    int n = (y+k)*width + (xmin+l);
                    int o = y*width + xmin;

                    for (int x = xmin; x < xmax; x++) {
                        r[o] = (int) Math.max(r[o], a[n++]);
                        o++;
                    }
                }
            }
        }
    }

    public static final void convolveCenteredDisc2DMax(int a[], int width, int height, int radius, int r[])
    {
        for (int k=-radius; k <= radius; k++) {
            for (int l=-radius; l <= radius; l++) {
                // is it within the radius?
                if (k*k + l*l > radius*radius)
                    continue;

                int ymin = Math.max(0, 0-k);
                int ymax = Math.min(height, height - k);
                int xmin = Math.max(0, 0-l);
                int xmax = Math.min(width, width - l);

                for (int y = ymin; y < ymax; y++) {
                    int n = (y+k)*width + (xmin+l);
                    int o = y*width + xmin;

                    for (int x = xmin; x < xmax; x++) {
                        r[o] = (int) Math.max(r[o], a[n++]);
                        o++;
                    }
                }
            }
        }
    }

    public static final void convolveCenteredDisc2DMax(float a[], int width, int height, int radius, float r[])
    {
        for (int k=-radius; k <= radius; k++) {
            for (int l=-radius; l <= radius; l++) {
                // is it within the radius?
                if (k*k + l*l > radius*radius)
                    continue;

                int ymin = Math.max(0, 0-k);
                int ymax = Math.min(height, height - k);
                int xmin = Math.max(0, 0-l);
                int xmax = Math.min(width, width - l);

                for (int y = ymin; y < ymax; y++) {
                    int n = (y+k)*width + (xmin+l);
                    int o = y*width + xmin;

                    for (int x = xmin; x < xmax; x++) {
                        r[o] = (float) Math.max(r[o], a[n++]);
                        o++;
                    }
                }
            }
        }
    }

    static final byte clampByte(double v)
    {
        if (v < 0)
            return 0;
        if (v > 255)
            return (byte) 255;
        return (byte) v;
    }

    public static final void convolveSymmetricCenteredMax(byte a[], int aoff, int alen, float f[], byte r[], int roff)
    {
        if ((f.length&1)==0 && !warned) {
            System.out.println("SigProc.convolveSymmetricCentered Warning: filter is not odd length");
            warned = true;
        }

        for (int i = f.length/2; i < f.length; i++) {
            double acc = 0;
            for (int j = 0; j < f.length; j++) {
                if ((i - j) < 0 || (i - j) >= alen)
                    acc = Math.max(acc, (a[aoff]&0xff) * f[j]);
                else
                    acc = Math.max(acc, (a[aoff + i - j]&0xff) * f[j]);
            }
            r[roff + i - f.length/2] = clampByte(acc);
        }

        for (int i = f.length; i < alen; i++) {
            double acc = 0;
            for (int j = 0; j < f.length; j++) {
                acc = Math.max(acc, (a[aoff + i - j]&0xff) * f[j]);
            }
            r[roff + i - f.length/2] = clampByte(acc);
        }

        for (int i = alen; i < alen + f.length/2; i++) {
            double acc = 0;
            for (int j = 0; j < f.length; j++) {
                if ((i - j) >= alen || (i - j) < 0)
                    acc = Math.max(acc, (a[aoff + alen - 1]&0xff) * f[j]);
                else
                    acc = Math.max(acc, (a[aoff + i - j]&0xff) * f[j]);
            }
            r[roff + i - f.length/2] = clampByte(acc);
        }
    }

    public static final float[] convolve(float a[], float b[])
    {
        float r[] = new float[a.length + b.length - 1];
        for (int i = 0; i < r.length; i++) {
            double acc = 0;
            for (int j = 0; j < b.length; j++) {
                if (i-j<0 || i-j >= a.length)
                    continue;
                acc += a[i-j]*b[j];
            }
            r[i] = (float) acc;
        }

        return r;
    }

    /** Computes gaussian low-pass filter with L1 Norm of 1.0 (all
     * elements add up). N should be odd. **/
    public static float[] makeGaussianFilter(double sigma, int n)
    {
        // n should be odd (or we won't be able to keep image stationary).
        // assert((n&1)==1);

        float f[] = new float[n];

        // special case.
        if (sigma == 0) {
            f[f.length/2] = 1;
            return f;
        }

        // N=3, N/2 = 1
        // 0-1 = -1
        // 1-1 = 0
        // 2-1 = 1
        for (int i = 0; i < n; i++) {
            int j = i - n/2;

            f[i] = (float) Math.exp(-j*j/(2*sigma*sigma));
        }

        return LinAlg.normalizeL1(f);
    }

    public static void main(String args[])
    {
        float a[] = new float[] {1,1};

        LinAlg.print(convolve(a,a));
        LinAlg.print(convolve(a,convolve(a,a)));
    }
}
