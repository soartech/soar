package april.image;

import java.util.*;

public class Corner
{
    public double             scale;     // in pixels (not octaves!)
    public double             x, y;      // relative to original image.
    public double             strength;
    public double[][]         P;
    public ArrayList<float[]> descriptor;

    public Corner(double scale, double x, double y, double strength,
                  double[][] P, ArrayList<float[]> descriptor)
    {
        this.scale = scale;
        this.x = x;
        this.y = y;
        this.strength = strength;
        this.P = P;
        this.descriptor = descriptor;
    }

    public Corner(double scale, double x, double y, double strength)
    {
        this(scale, x, y, strength, null);
    }

    public Corner(double scale, double x, double y, double strength, double[][] P)
    {
        this(scale, x, y, strength, P, null);
    }
}
