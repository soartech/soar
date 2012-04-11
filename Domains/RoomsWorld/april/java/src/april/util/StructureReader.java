package april.util;

import java.io.*;
import java.util.*;

/** Interface for reading basic types (floats, strings, etc.). **/
public interface StructureReader
{
    public int readInt() throws IOException;
    public int[] readInts() throws IOException;

    public long readLong() throws IOException;

    public float readFloat() throws IOException;
    public float[] readFloats() throws IOException;

    public double readDouble() throws IOException;
    public double[] readDoubles() throws IOException;
    public double[][] readMatrix() throws IOException;

    public StructureReader readBlock() throws IOException;

    public String readString() throws IOException;

    public byte[] readBytes() throws IOException;

    public void blockBegin() throws IOException;
    public void blockEnd() throws IOException;

    public void close() throws IOException;
}
