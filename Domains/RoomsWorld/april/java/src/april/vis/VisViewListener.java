package april.vis;

/** Interface for listening to changes in the VisView **/
public interface VisViewListener
{
    public void viewBufferEnabledChanged(VisContext vc, String bufferName, boolean enabled);
}
