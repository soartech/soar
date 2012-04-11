package april.jcam;

import java.io.*;

import april.util.*;

public class ISLogWriter
{
    OutputStream outs;

    public static final long MAGIC = 0x17923349ab10ea9aL;

    public ISLogWriter(OutputStream outs)
    {
        this.outs = outs;
    }

    public void close() throws IOException
    {
        outs.flush();
        outs.close();
    }

    public void write(ImageSourceFormat ifmt, byte imbuf[]) throws IOException
    {
        ByteArrayOutputStream bouts = new ByteArrayOutputStream();
        DataOutputStream dbouts = new DataOutputStream(bouts);

        dbouts.writeLong(MAGIC);
        dbouts.writeLong(TimeUtil.utime());
        dbouts.writeInt(ifmt.width);
        dbouts.writeInt(ifmt.height);
        dbouts.writeInt(ifmt.format.length());
        dbouts.write(ifmt.format.getBytes());
        dbouts.writeInt(imbuf.length);
        dbouts.flush();

        byte hdr[] = bouts.toByteArray();

        outs.write(hdr);
        outs.write(imbuf);
    }
}
