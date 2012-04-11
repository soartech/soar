package april.util;

import java.io.*;
import java.util.*;

/** Writes a structure as a human-readable text file. **/
public class TextStructureWriter implements StructureWriter
{
    BufferedWriter outs;
    int indent = 0;

    public TextStructureWriter(BufferedWriter outs)
    {
        this.outs = outs;
    }

    String escapeString(String s)
    {
        StringBuffer sb = new StringBuffer();

        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);

            if (c=='\n')
                sb.append("\\n");
            else if (c=='\r')
                sb.append("\\r");
            else if (c=='\\')
                sb.append("\\\\");
            else if (c=='\"')
                sb.append("\\\"");
            else
                sb.append(c);
        }

        return sb.toString();
    }

    /** Write an arbitrary string that might contain newlines or other
        troublesome characters.  The string is preserved exactly,
        including white space.
    **/
    public void writeString(String s) throws IOException
    {
        doIndent();
        String escapedString = s.replace("\\", "\\\\").replace("\"", "\\\\\"").replace("\n","\\n");
        outs.write("\""+escapeString(s)+"\"\n");
    }

    public void writeComment(String s) throws IOException
    {
        if (s==null || s.length()==0)
            outs.write("\n");
        else {
            doIndent();
            outs.write("# "+s+"\n");
        }
    }

    public void writeInt(int v) throws IOException
    {
        doIndent();
        outs.write(String.format("%d\n", v));
    }

    public void writeLong(long v) throws IOException
    {
        doIndent();
        outs.write(String.format("%d\n", v));
    }

    public void writeFloat(float v) throws IOException
    {
        doIndent();
        outs.write(String.format("%.8g\n", v));
    }

    public void writeInts(int v[]) throws IOException
    {
        doIndent();

        if (v==null) {
            outs.write("ivec -1\n");
            return;
        }

        outs.write("ivec "+v.length+"\n");

        doIndent();
        for (int i = 0; i < v.length; i++)
            outs.write(String.format("%d ", v[i]));
        outs.write("\n");
    }

    public void writeFloats(float v[]) throws IOException
    {
        doIndent();

        if (v==null) {
            outs.write("fvec -1\n");
            return;
        }

        outs.write("fvec "+v.length+"\n");

        doIndent();
        for (int i = 0; i < v.length; i++)
            outs.write(String.format("%.8g ", v[i]));
        outs.write("\n");
    }

    public void writeDouble(double v) throws IOException
    {
        doIndent();
        outs.write(String.format("%.15g\n", v));
    }

    public void writeDoubles(double v[]) throws IOException
    {
        doIndent();

        if (v==null) {
            outs.write("vec -1\n");
            return;
        }

        outs.write("vec "+v.length+"\n");

        doIndent();
        for (int i = 0; i < v.length; i++)
            outs.write(String.format("%.15g ", v[i]));
        outs.write("\n");
    }

    public void writeMatrix(double v[][]) throws IOException
    {
        doIndent();

        if (v==null) {
            outs.write("mat -1 -1\n");
            return;
        }

        outs.write("mat "+v.length+" "+v[0].length+"\n");

        for (int i = 0; i < v.length; i++) {
            doIndent();
            for (int j = 0; j < v[i].length; j++)
                outs.write(String.format("%.15g ", v[i][j]));
            outs.write("\n");
        }
    }

    public void writeBytes(byte b[]) throws IOException
    {
        String lines[] = Base64.encode(b);
        writeInt(lines.length);

        for (String line : lines) {
            doIndent();
            outs.write(line);
            outs.write("\n");
        }
    }

    void doIndent() throws IOException
    {
        for (int i = 0; i < indent; i++)
            outs.write("\t");
    }

    public void blockBegin() throws IOException
    {
        doIndent();
        outs.write("{\n");
        indent++;
    }

    public void blockEnd() throws IOException
    {
        indent--;
        assert(indent >= 0);
        doIndent();
        outs.write("}\n");
    }

    public void close() throws IOException
    {
        outs.flush();
        outs.close();
    }
}
