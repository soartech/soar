package april.util;

import java.util.*;
import java.io.*;

public class IntsCoder implements StructureCoder
{
    public IntsCoder()
    {
    }

    public void write(StructureWriter outs, Object o) throws IOException
    {
        outs.writeInts((int[]) o);
    }

    public Object read(StructureReader ins) throws IOException
    {
        return ins.readInts();
    }
}
