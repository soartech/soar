package april.util;

import java.util.*;
import java.io.*;

public class DoublesCoder implements StructureCoder
{
    public DoublesCoder()
    {
    }

    public void write(StructureWriter outs, Object o) throws IOException
    {
        outs.writeDoubles((double[]) o);
    }

    public Object read(StructureReader ins) throws IOException
    {
        return ins.readDoubles();
    }
}
