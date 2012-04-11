package april.newvis;

public class VisCameraManager
{
    // This shouldn't be modified externally.
    public VisCameraState current;

    public VisCameraState goal;

    public VisCameraState getCameraState(VisCanvas vc, int viewport[])
    {
        return new VisCameraState();
    }
}
