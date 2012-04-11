package april.util;

import java.util.*;
import java.io.*;

public class IntCoder implements StructureCoder
{
    public IntCoder()
    {
    }

    public void write(StructureWriter outs, Object o) throws IOException
    {
        outs.writeInt((Integer) o);
    }

    public Object read(StructureReader ins) throws IOException
    {
        return ins.readInt();
    }
}
