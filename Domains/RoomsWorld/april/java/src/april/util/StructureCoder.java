package april.util;

import java.io.*;

/** Abstract interface for writing an object of arbitrary type. **/
public interface StructureCoder
{
    public void write(StructureWriter outs, Object o) throws IOException;
    public Object read(StructureReader ins) throws IOException;
}
