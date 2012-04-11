package april.vis;

import java.io.*;

import lcm.lcm.*;

// Please see VisSerialize for documentation
public interface VisSerializable
{
    // NOTE: You must ALSO specify a default constructor, e.g. MyVisObject(), if you extend this class

    // automatically writes appropriate header information for this class
    public void serialize(LCMDataOutputStream out) throws IOException;
    public void unserialize(LCMDataInputStream in) throws IOException;

}