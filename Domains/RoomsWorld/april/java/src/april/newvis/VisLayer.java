package april.newvis;

import java.util.*;
import java.awt.*;

public class VisLayer
{
    public boolean   enabled;
    public VisWorld  world;
    public VisCameraManager cameraManager = new VisCameraManager();

    public ArrayList<VisEventHandler> eventHandlers;

    public Color backgroundColor = Color.white;


    public VisLayer(VisWorld world)
    {
        this.world = world;
    }
}
