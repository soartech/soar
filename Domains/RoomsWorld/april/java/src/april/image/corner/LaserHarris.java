package april.image.corner;

import java.util.*;

import april.jmat.*;
import april.util.*;
import april.image.*;
import april.laser.*;

import april.lcmtypes.*;

public class LaserHarris implements ParameterListener
{
	/*input laser points
	 *output: corners
	 *
	 */
    // when two features are detect near each other, we suppress all but the maximum.
    static final double NEIGHBOR_SUPPRESSION_RANGE = 0.1;//unit: meters
    static final double NEIGHBOR_SUPPRESSION_RATIO = 1.0;
    static final int pyramidLevelLimitation=2;//2 is good enough for both intel and victoria datasets

    final double minLengthX = 1,minLengthY = 1,border = 0.2;
    final double metersperpixel=0.02;
    final double cometTailLength=1.5;

    final int minImageSz=6;//minimum image size, unit: pixels
    public double k = 0.04;//standard parameter for harris corner
    int halfsize=2;
    float filt[] = SigProc.makeGaussianFilter(1, 5);
    double sigma = 1.5/halfsize;

	float gaussian[] = SigProc.makeGaussianFilter(sigma, halfsize*2+1);
    int argb[] ;

    final double harrisThresh=0.4;//for experiments only, real usage should be 0.4--0.5
    final double minPrecision=NEIGHBOR_SUPPRESSION_RANGE;

    boolean eliminatorType=false;
    int descriptorType=1;//0: no descriptor, 1: SIFT, 2: angles

    //SIFT descriptor parameters
	int windowSz=4;
	int descriptorSz=4;
	double[][]gaussianWeightedCircularWindow=new double[windowSz+1][windowSz+1];
	int localImageWidth=(2*(windowSz+1)+1);//extra two coloums

    public LaserHarris()
    {
	    argb = new int[256];
	    for (int i = 1; i < 256; i++) // argb[0] = 0 (transparent)
            argb[i] = (i<<24) + (0xff<<16) + (0xff<<8) + (0xff<<0);
	    gaussian = new float[gaussian.length];
		for (int i = 0; i < gaussian.length; i++)
		    gaussian[i] = (float) Math.sqrt(1.0/gaussian.length);
		for (int icol=0;icol<=windowSz;icol++)
		{
			for (int irow=0;irow<=windowSz;irow++)
			{
				gaussianWeightedCircularWindow[icol][irow]=Math.exp(-Math.sqrt(Math.pow(icol-windowSz/2,2)+Math.pow(irow-windowSz/2,2))/2.25);
			}
		}
    }

    public ArrayList<Corner> extractFeatures(laser_t laser)
    {
    	ArrayList<double[]> points=laserToPoints(laser,70,0.5);
		ContourExtractor contourExtractor = new ContourExtractor(null);
		ArrayList<ArrayList<double[]>> contours = contourExtractor.getContours(points);

		ArrayList<Corner> allCorners = new ArrayList<Corner>();
		for (ArrayList<double[]> contour : contours) {

		    if (contour.size() < 2)		continue;

		    double minx = Double.MAX_VALUE, maxx = -Double.MAX_VALUE;
		    double miny = Double.MAX_VALUE, maxy = -Double.MAX_VALUE;

		    for (double xy[] : contour)
		    {
				minx = Math.min(minx, xy[0]);
				maxx = Math.max(maxx, xy[0]);
				miny = Math.min(miny, xy[1]);
				maxy = Math.max(maxy, xy[1]);
		    }

		    if (maxx == -Double.MAX_VALUE)
                continue;

		    double xsize = Math.max(maxx-minx+border, minLengthX);
		    double ysize = Math.max(maxy-miny+border, minLengthY);

		    GridMap gm = GridMap.makeMeters(minx, miny, xsize, ysize, metersperpixel, 0);

		    for (int i = 0; i+1 < contour.size(); i++)
		    {
				double xy0[] = contour.get(i);
				double xy1[] = contour.get(i+1);

				double range = LinAlg.magnitude(xy0);

				GridMap.LUT lut = getLut(gm, range);

				gm.drawLine(xy0[0], xy0[1], xy1[0], xy1[1], lut);
		    }

            //			draw comet tails
			double xy[] = contour.get(0);
			double theta = Math.atan2(xy[1], xy[0]) - Math.toRadians(1);
			double range = LinAlg.magnitude(xy) + cometTailLength;
			double exy[] = new double[] { range*Math.cos(theta), range*Math.sin(theta) };
			gm.drawLine(xy[0], xy[1], exy[0], exy[1], getLut(gm, range));

			xy = contour.get(contour.size()-1);
			theta = Math.atan2(xy[1], xy[0]) + Math.toRadians(1);
			range = LinAlg.magnitude(xy) + cometTailLength;
			exy = new double[] { range*Math.cos(theta), range*Math.sin(theta) };
			gm.drawLine(xy[0], xy[1], exy[0], exy[1], getLut(gm, range));

			FloatImage fim = new FloatImage(gm.makeBufferedImageARGB(argb));
			ArrayList<FloatImage> octaves = new ArrayList<FloatImage>();
			octaves.add(fim);
			while (Math.min(fim.width, fim.height) > minImageSz&&octaves.size()<pyramidLevelLimitation) {
			    fim = fim.filterFactoredCentered(filt, filt).decimate();
			    octaves.add(fim);
			}
		    ArrayList<Corner> corners = compute(octaves);
		    ArrayList<Corner>newCorners=new ArrayList<Corner>();
		    if (eliminatorType)
		    {
		    	eliminateFalse(corners);
			    // project x,y,sz into robot-relative space.
			    for (Corner c : corners)
			    {
			    	newCorners.add(new Corner(c.scale*gm.metersPerPixel
                                              ,c.x*gm.metersPerPixel + gm.x0
                                              ,c.y*gm.metersPerPixel + gm.y0
                                              ,c.strength
                                              ,new Matrix(c.P).inverse().times(c.scale*gm.metersPerPixel*gm.metersPerPixel).copyArray()
                                              ,null));
			    }
		    }
		    else
		    {
		    	boolean deletedForm[]=eliminateFalse2(corners);
			    for (int i=0; i< deletedForm.length; i++)
			    {
			    	if (deletedForm[i]) continue;
			    	Corner c=corners.get(i);
			    	newCorners.add(new Corner(c.scale*gm.metersPerPixel
                                              ,c.x*gm.metersPerPixel + gm.x0
                                              ,c.y*gm.metersPerPixel + gm.y0
                                              ,c.strength
                                              ,new Matrix(c.P).inverse().times(c.scale*gm.metersPerPixel*gm.metersPerPixel).copyArray()
                                              ,null));
			    }
		    }
		    //reject occlusion points
		    for (int i=newCorners.size()-1;i>=0;i--)
		    {
		    	Corner c=newCorners.get(i);
		    	int index=(int)(Math.atan2(c.y, c.x)/Math.toRadians(1))+90;
		    	double tempRange=Math.sqrt(c.y*c.y+c.x*c.x);
		    	if (index>170||index<10)
		    	{
		    		//System.out.println("rejected for near boundary "+index);
		    		newCorners.remove(i);
		    		continue;
		    	}
		    	if (Math.abs(laser.ranges[index]-tempRange)>minPrecision*c.scale/metersperpixel
                   &&Math.abs(laser.ranges[index-1]-tempRange)>minPrecision*c.scale/metersperpixel
                   &&Math.abs(laser.ranges[index+1]-tempRange)>minPrecision*c.scale/metersperpixel)
		    	{
		    		newCorners.remove(i);
		    		continue;
		    	}
		    	if (laser.ranges[index-1]-tempRange<-0.1||laser.ranges[index+1]-tempRange<-0.1)
		    	{
		    		//System.out.println("rejected for being a occlusion point "+index);
		    		newCorners.remove(i);
		    		continue;
		    	}

		    }
		    allCorners.addAll(newCorners);

		}

		//	extractFeaturesBulk(points);
		return allCorners;
    }

    public ArrayList<Corner> compute(ArrayList<FloatImage> fims)
    {
		ArrayList<Corner> corners = new ArrayList<Corner>();
		for (int oidx = 0; oidx < fims.size(); oidx++)
		{
			int scaleFactor=1<<oidx;
			FloatImage fim=fims.get(oidx);
			int width = fim.width;
			int height = fim.height;

			FloatImage fimIx2 = new FloatImage(width, height);
			FloatImage fimIxIy = new FloatImage(width, height);
			FloatImage fimIy2 = new FloatImage(width, height);

			for (int y = 1; y+1 < height; y++) {
			    for (int x = 1; x+1 < width; x++) {

                    float Ix = fim.get(x+1, y) - fim.get(x-1, y);
                    float Iy = fim.get(x,y+1) - fim.get(x, y-1);

                    fimIx2.set(x,y, Ix*Ix);
                    fimIxIy.set(x,y, Ix*Iy);
                    fimIy2.set(x,y, Iy*Iy);
			    }
			}

			fimIx2 = fimIx2.filterFactoredCentered(gaussian, gaussian);
			fimIxIy = fimIxIy.filterFactoredCentered(gaussian, gaussian);
			fimIy2 = fimIy2.filterFactoredCentered(gaussian, gaussian);

			for (int y = 1; y+1 < height; y++) {
			    for (int x = 1; x+1 < width; x++) {

                    double A = fimIx2.get(x,y), B = fimIxIy.get(x,y), D = fimIy2.get(x,y);

                    float strength;
                    if (false) {
                        strength=(float) (A*D-B*B - k*LinAlg.sq(A+D));
                    } else {
                        strength=(float) minEig(A, B, D);
                    }
                    if (strength>harrisThresh)
                    {
                        corners.add(new Corner(oidx,(x+.5)*scaleFactor,(y+.5)*scaleFactor,strength,new double[][]{{A,B},{B,D}}));
                    }
			    }
			}
		}
		return corners;
    }

    ArrayList<Corner>eliminateFalse(ArrayList<Corner>corners)
    {
    	// the OCCLUSION points are not eliminated in the code
    	int NEIGHBOR=(int)Math.ceil((double)NEIGHBOR_SUPPRESSION_RANGE/metersperpixel);
    	for (int iout=corners.size()-1;iout>0;)
		{
			Corner c=corners.get(iout);
			boolean badCorner=false;
			boolean killedInLoop=false;
			for (int iin=iout-1;iin>=0;)
			{
				Corner cc=corners.get(iin);
				if (cc.scale<=c.scale+1&&cc.scale>=c.scale-1)//
				{
                    if (Math.abs(c.x*-cc.x)<=NEIGHBOR||Math.abs(c.y-cc.y)<=NEIGHBOR)//near
                    {
                        if (c.strength>NEIGHBOR_SUPPRESSION_RATIO*cc.strength)
                        {
                            corners.remove(iin);
                            iin--;
                            iout--;
                            killedInLoop=true;
                            //System.out.println("iin--"+iin+" size="+corners.size());
                            continue;
                        }
                        else
                        {
                            badCorner=true;
                        }
                    }
				}
				iin--;
			}
			if (badCorner)
			{
				corners.remove(iout);
				iout--;
			}
			else
			{
				if (!killedInLoop)
					iout--;
			}
		}
		return corners;
    }

    boolean[] eliminateFalse2(ArrayList<Corner>corners)
    {
    	boolean[] deletedElements=new boolean[corners.size()];
    	// the OCCLUSION points are not eliminated in the code
    	int NEIGHBOR=(int)Math.ceil((double)NEIGHBOR_SUPPRESSION_RANGE/metersperpixel);
    	for(int iout=corners.size()-1;iout>0;iout--)
		{
    		if (deletedElements[iout]) continue;
			Corner c=corners.get(iout);
			for (int iin=iout-1;iin>=0;iin--)
			{
				if (deletedElements[iin]) continue;
				Corner cc=corners.get(iin);
				if (cc.scale<=c.scale+1&&cc.scale>=c.scale-1)
				{
                    if (Math.abs(c.x*-cc.x)<=NEIGHBOR||Math.abs(c.y-cc.y)<=NEIGHBOR)//near
                    {
                        if (c.strength>NEIGHBOR_SUPPRESSION_RATIO*cc.strength)
                        {
                            deletedElements[iin]=true;
                            //System.out.println("iin--"+iin+" size="+corners.size());
                            continue;
                        }
                        else
                        {
                            deletedElements[iout]=true;
                        }
                    }
				}
			}
		}
		return deletedElements;
    }

    GridMap.LUT getLut(GridMap gm, double range)
    {
        double rangevar = LinAlg.sq(3*0.1);
        double thetavar = LinAlg.sq(4*Math.sin(Math.toRadians(1))*range);
        double alpha = 1.0 / (rangevar + thetavar);

        return gm.makeGaussianLUT(1.0, 0, alpha);
    }

    double minEig(double a, double b, double d)
    {
        // 2a^2 + 4b^2 + 2d^2 - 2*(a+d)sqrt(a^2 - 2ad + d^2 + 4b^2)
        double SA = (a+d)*(a+d)*(a*a-2*a*d+d*d+4*b*b);
        assert(SA >= 0);
        SA = Math.sqrt(SA);

        double SB = 2*a*a+4*b*b+2*d*d-2*SA;
        if (SB < 0) // can be negative due to numerical precision.
        {
            return 0;
        }

        SB = Math.sqrt(SB);
        return 0.5 * SB;
    }

    public void parameterChanged(ParameterGUI pg, String name)
    {
    	if (name.equals("eliminator"))
    	{
    		eliminatorType=pg.gb("eliminator");
    		System.out.println("type changed");
    	}
    }

    public static ArrayList<double[]> laserToPoints(laser_t laser,double maxSpan,double minSpan)
    {
        ArrayList<double[]> points = new ArrayList<double[]>();

        for (int i = 0; i < laser.nranges; i++) {
            double xy[];
            double theta = laser.rad0 + i * laser.radstep;
            if (laser.ranges[i]<maxSpan&&laser.ranges[i]>minSpan)
            {
                xy = new double[] { laser.ranges[i] * Math.cos(theta),
                                    laser.ranges[i] * Math.sin(theta) };
                points.add(xy);
            }
            //else{xy=new double[] {0,0};}
            //points.add(xy);
        }

        return points;
    }

    public static double[] countHistogram(double[][]inputValue,double maxRange,double minRange,int howmanyBins)
    {
    	double[] histogram = new double[howmanyBins];
    	double range = maxRange - minRange;
        assert(range>0):range;
    	for (int i=0;i<inputValue[0].length;i++)
    	{
    		if (inputValue[1][i]==0)continue;
    		int index=(int)((inputValue[0][i]-minRange)/(double)(range/howmanyBins));
            assert(index >=0 && index <= howmanyBins):inputValue[0][i];
    		if (index == howmanyBins) index--;
    		histogram[index]+=inputValue[1][i];
    	}
    	return histogram;
    }

}
