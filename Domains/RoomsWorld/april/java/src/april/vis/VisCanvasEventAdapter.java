package april.vis;

import java.awt.event.*;
import april.jmat.geom.*;

/** Classes which are interested in user-events inside a VisCanvas implement this interface. **/
public abstract class VisCanvasEventAdapter implements VisCanvasEventHandler
{
    public double pickQuery(VisCanvas vc, GRay3D ray)
    {
        return -1;
    }

    public double hoverQuery(VisCanvas vc, GRay3D ray)
    {
        return -1;
    }

    public void pickNotify(boolean winner)
    {
    }

    public void hoverNotify(boolean winner)
    {
    }

    /** Event handlers return true if they consumed the event. **/
    public boolean keyTyped(VisCanvas vc, KeyEvent e)
    {
        return false;
    }

    public boolean keyPressed(VisCanvas vc, KeyEvent e)
    {
        return false;
    }

    public boolean keyReleased(VisCanvas vc, KeyEvent e)
    {
        return false;
    }

    public boolean mouseWheelMoved(VisCanvas vc,  GRay3D ray, MouseWheelEvent e)
    {
        return false;
    }

    public boolean mouseClicked(VisCanvas vc,  GRay3D ray, MouseEvent e)
    {
        return false;
    }

    public boolean mouseReleased(VisCanvas vc, GRay3D ray, MouseEvent e)
    {
        return false;
    }

    public boolean mouseDragged(VisCanvas vc,  GRay3D ray, MouseEvent e)
    {
        return false;
    }

    public boolean mousePressed(VisCanvas vc,  GRay3D ray, MouseEvent e)
    {
        return false;
    }

    public boolean mouseMoved(VisCanvas vc,  GRay3D ray, MouseEvent e)
    {
        return false;
    }

    public void doHelp(HelpOutput houts)
    {

    }
}
