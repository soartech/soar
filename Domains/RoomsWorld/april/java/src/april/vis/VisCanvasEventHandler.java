package april.vis;

import java.awt.event.*;
import april.jmat.geom.*;

/** Classes which are interested in user-events inside a VisCanvas
 * implement this interface.
 *
 * Event handlers have a fixed priority, with the exception that at
 * any given time, one handler may be promoted to the "Picking"
 * handler. The Picking handler essentially has focus-- first dibs
 * for consuming events.
 *
 * Whenever a frame is drawn and there is not currently a picking
 * handler, all event handlers are asked to rank themselves as to how
 * well they could handle a mouse down. This is the hoverQuery
 * call. Typically, an event handler's hoverQuery method should return
 * the distance from the ray to an object in the scene that the event
 * handler can manipulate.
 *
 * If the mouse is pressed, the best event handler (according to
 * hoverQuery) will become the "picking" handler.
 *
 * Handlers can also promote themselves to the "picking" handler. A
 * reasonable pattern is that an event handler might promote itself to
 * 'picking' if there is no current picking handler and some keyboard
 * hotkey is pressed.
 **/
public interface VisCanvasEventHandler
{
    /** Return -1 to indicate no pick/hover possible. Otherwise, the
        distance from the ray to the nearest object. If you are the
        best event handler, you will get a pickNotify. **/
    public double pickQuery(VisCanvas vc, GRay3D ray);

    /** A new hover competition is taking place. Reset your hover
        state, and await a (possible) hoverNotify callback. Return
        less than zero if the handler cannot do anything useful with a
        mouse down.
    **/
    public double hoverQuery(VisCanvas vc, GRay3D ray);

    /** These methods called when an event handler wins or loses a pick/hover contest. **/
    public void pickNotify(boolean winner);
    public void hoverNotify(boolean winner);

    /** Event handlers return true if they consumed the event. **/
    public boolean keyTyped(VisCanvas vc, KeyEvent e);
    public boolean keyPressed(VisCanvas vc, KeyEvent e);
    public boolean keyReleased(VisCanvas vc, KeyEvent e);

    public boolean mouseWheelMoved(VisCanvas vc,  GRay3D ray, MouseWheelEvent e);
    public boolean mouseClicked(VisCanvas vc,  GRay3D ray, MouseEvent e);
    public boolean mouseReleased(VisCanvas vc, GRay3D ray, MouseEvent e);
    public boolean mouseDragged(VisCanvas vc,  GRay3D ray, MouseEvent e);
    public boolean mousePressed(VisCanvas vc,  GRay3D ray, MouseEvent e);
    public boolean mouseMoved(VisCanvas vc,  GRay3D ray, MouseEvent e);

    public String getName();

    /** The user has requested help. Print out some information **/
    public void doHelp(HelpOutput houts);
}
