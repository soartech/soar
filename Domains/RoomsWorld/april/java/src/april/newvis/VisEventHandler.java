package april.newvis;

import java.awt.event.*;
import april.jmat.geom.*;

public interface VisEventHandler
{
    public boolean mouseMove(VisCanvas vc, VisLayer vl, GRay3D ray, MouseEvent e);
    public boolean mouseClicked(VisCanvas vc, VisLayer vl, GRay3D ray, MouseEvent e);

    public boolean mousePressed(VisCanvas vc, VisLayer vl, GRay3D ray, MouseEvent e);
    public boolean mouseDragged(VisCanvas vc, VisLayer vl, GRay3D ray, MouseEvent e);
    public boolean mouseReleased(VisCanvas vc, VisLayer vl, GRay3D ray, MouseEvent e);

    public String getName();
}
