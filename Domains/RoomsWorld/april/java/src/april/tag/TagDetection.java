package april.tag;

public class TagDetection
{
    /** Is the detection good enough? **/
    public boolean good;

    /** Observed code **/
    public long obsCode;

    /** Matched code **/
    public long code;

    /** What was the ID of the detected tag? **/
    public int id;

    /** The hamming distance between the detected code and the true code. **/
    public int hammingDistance;

    /** How many 90 degree rotations were required to align the code. **/
    public int rotation;

    ////////////////// Fields below here are filled in by TagDetector ////////////////
    /** Position (in fractional pixel coordinates) of the detection. The
     * points travel around counter clockwise around the target,
     * always starting from the same corner of the tag. Dimensions
     * [4][2]. **/
    public double p[][];

    /** Center of tag in pixel coordinates. **/
    public double cxy[];

    /** Measured in pixels, how long was the observed perimeter
     * (i.e., excluding inferred perimeter which is used to
     * connect incomplete quads) **/
    public double observedPerimeter;

    /** A 3x3 homography that computes pixel coordinates from
     * tag-relative coordinates. Both the input and output coordinates
     * are 2D homogenous vectors, with y = Hx. y are pixel
     * coordinates, x are tag-relative coordinates. Tag coordinates
     * span from (-1,-1) to (1,1). The orientation of the homography
     * reflects the orientation of the target. **/
    public double homography[][];

    /** The homography is relative to image center, whose coordinates
     * are below.
     **/
    public double hxy[];

    /** interpolate point given (x,y) is in tag coordinate space from (-1,-1) to (1,1) **/
    public double[] interpolate(double x, double y)
    {
        double z = homography[2][0]*x + homography[2][1]*y + homography[2][2];
        return new double[] { (homography[0][0]*x + homography[0][1]*y + homography[0][2])/z + hxy[0],
                              (homography[1][0]*x + homography[1][1]*y + homography[1][2])/z + hxy[1]};
    }

    public String toString()
    {
        return String.format("[TagDetection code 0x%010x   id=%-5d   errors=%d   position =  (%8.2f,%8.2f) @ %3d deg]",
                             code, id, hammingDistance, cxy[0], cxy[1], rotation*90);
    }
}
