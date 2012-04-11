package april.vis;

public interface VisContext
{
    public VisWorld getWorld();

    public VisViewManager getViewManager();

    /** Request a redraw as soon as possible (but the redraw may occur
     * asynchronously.)
     **/
    public void draw();

    public VisView getRenderingView();

    /**
     * TODO should this be moved into VisWorld or VisView?
     */
    public java.awt.Color getBackground();
}
