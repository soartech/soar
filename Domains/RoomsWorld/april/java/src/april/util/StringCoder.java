package april.util;

import java.util.*;
import java.io.*;

public class StringCoder implements StructureCoder
{
    public StringCoder()
    {
    }

    public void write(StructureWriter outs, Object o) throws IOException
    {
        outs.writeString((String) o);
    }

    public Object read(StructureReader ins) throws IOException
    {
        return ins.readString();
    }
}
