package april.newvis;

import april.jmat.*;

public class VisCameraState
{
    public double eye[], lookat[], up[];

    public double perspectiveness = 1.0;
    public double perspective_fovy_degrees = 50;

    public double zclip_near = 0.01;
    public double zclip_far = 5000;

    public VisCameraState()
    {
        eye = new double[] { 0, 0, 10 };
        lookat = new double[] { 0, 0, 0};
        up = new double[] { 0, 1, 0 };
    }

    public VisCameraState copy()
    {
        VisCameraState s = new VisCameraState();
        s.eye = LinAlg.copy(eye);
        s.lookat = LinAlg.copy(lookat);
        s.up = LinAlg.copy(up);
        s.perspectiveness = perspectiveness;
        s.perspective_fovy_degrees = perspective_fovy_degrees;
        s.zclip_near = zclip_near;
        s.zclip_far = zclip_far;

        return s;
    }
}
