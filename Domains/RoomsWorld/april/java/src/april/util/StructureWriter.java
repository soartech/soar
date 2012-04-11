package april.util;

import java.io.*;
import java.util.*;

/** Interface for reading common types (floats, strings, etc.) **/
public interface StructureWriter
{
    public void writeComment(String s) throws IOException;

    // not allowed to contain a newline
    public void writeString(String s) throws IOException;

    public void writeInt(int v) throws IOException;
    public void writeInts(int v[]) throws IOException;

    public void writeLong(long v) throws IOException;

    public void writeFloat(float v) throws IOException;
    public void writeFloats(float v[]) throws IOException;

    public void writeDouble(double v) throws IOException;
    public void writeDoubles(double v[]) throws IOException;
    public void writeMatrix(double v[][]) throws IOException;

    public void writeBytes(byte b[]) throws IOException;

    public void blockBegin() throws IOException;
    public void blockEnd() throws IOException;

    public void close() throws IOException;
}
