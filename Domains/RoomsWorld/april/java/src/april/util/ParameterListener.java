package april.util;

/** Interface for receiving notifications when a parameter changes
 * (see ParameterGUI). **/
public interface ParameterListener
{
    public void parameterChanged(ParameterGUI pg, String name);
}

