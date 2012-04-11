package april.util;

import java.io.*;

public class EndianDataInputStream
{
    InputStream ins;
    boolean littleEndian;

    static class RAFInputStream extends InputStream
    {
        RandomAccessFile raf;

        public RAFInputStream(RandomAccessFile raf)
        {
            this.raf = raf;
        }

        public int read() throws IOException
        {
            return raf.read();
        }
    }

    public EndianDataInputStream(byte b[], boolean littleEndian) throws IOException
    {
        this(new ByteArrayInputStream(b), littleEndian);
    }

    public EndianDataInputStream(RandomAccessFile raf, boolean littleEndian) throws IOException
    {
        this(new RAFInputStream(raf), littleEndian);
    }

    public EndianDataInputStream(InputStream ins, boolean littleEndian) throws IOException
    {
        this.ins = ins;
        this.littleEndian = littleEndian;
    }

    public void setLittleEndian(boolean v)
    {
        this.littleEndian = v;
    }

    public int read() throws IOException
    {
        return ins.read();
    }

    public short readShort() throws IOException
    {
        short v = 0;

        if (littleEndian) {
            v |= (ins.read());
            v |= (ins.read()<<8);
        } else {
            v |= (ins.read()<<8);
            v |= (ins.read());
        }

        return v;
    }

    public int readInt() throws IOException
    {
        int v = 0;

        if (littleEndian) {
            v |= (ins.read()<<0);
            v |= (ins.read()<<8);
            v |= (ins.read()<<16);
            v |= (ins.read()<<24);
        } else {
            v |= (ins.read()<<24);
            v |= (ins.read()<<16);
            v |= (ins.read()<<8);
            v |= (ins.read()<<0);
        }
        return v;
    }

    public long readLong() throws IOException
    {
        long v = 0;

        if (littleEndian) {
            v |= (((long) read())<<0);
            v |= (((long) read())<<8);
            v |= (((long) read())<<16);
            v |= (((long) read())<<24);
            v |= (((long) read())<<32);
            v |= (((long) read())<<40);
            v |= (((long) read())<<48);
            v |= (((long) read())<<56);
        } else {
            v |= (((long) read())<<56);
            v |= (((long) read())<<48);
            v |= (((long) read())<<40);
            v |= (((long) read())<<32);
            v |= (((long) read())<<24);
            v |= (((long) read())<<16);
            v |= (((long) read())<<8);
            v |= (((long) read()));
        }
        return v;
    }

    public float readFloat() throws IOException
    {
        return Float.intBitsToFloat(readInt());
    }

    public double readDouble() throws IOException
    {
        return Double.longBitsToDouble(readLong());
    }
}

