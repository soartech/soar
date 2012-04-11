package april.util;

import java.util.*;
import java.io.*;

public class LongCoder implements StructureCoder
{
    public LongCoder()
    {
    }

    public void write(StructureWriter outs, Object o) throws IOException
    {
        outs.writeLong((Long) o);
    }

    public Object read(StructureReader ins) throws IOException
    {
        return ins.readLong();
    }
}
