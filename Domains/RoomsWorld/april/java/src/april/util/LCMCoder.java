package april.util;

import java.util.*;
import java.io.*;

import lcm.lcm.*;

public class LCMCoder implements StructureCoder
{
    public LCMCoder()
    {
    }

    public void write(StructureWriter outs, Object o) throws IOException
    {
        LCMEncodable lcmobj = (LCMEncodable) o;

        LCMDataOutputStream bouts = new LCMDataOutputStream();
        lcmobj.encode(bouts);

        outs.writeString(lcmobj.getClass().getName());
        outs.writeBytes(bouts.toByteArray());
    }

    public Object read(StructureReader ins) throws IOException
    {
        String classname = ins.readString();
        byte b[] = ins.readBytes();

        try {
            Class cls = Class.forName(classname);
            Object o = cls.getConstructor(DataInput.class).newInstance(new LCMDataInputStream(b));
            return o;
        } catch (Exception ex) {
            System.out.println("ReflectUtil.createObject ex: "+ex);
            ex.printStackTrace();
            return null;
        }
    }

}
