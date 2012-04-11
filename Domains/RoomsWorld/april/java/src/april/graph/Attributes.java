package april.graph;

import java.io.*;
import java.util.*;
import java.lang.reflect.*;
import april.util.*;

import lcm.lcm.*;

public class Attributes
{

    public static class Attr
    {
        public Object          o;
        public StructureCoder  coder;

        public Attr(Object o, StructureCoder coder)
        {
            this.o = o;
            this.coder = coder;
        }
    }

    public HashMap<String, Attr> attrs = new HashMap<String, Attr>();


    static Attributes read(StructureReader ins ) throws IOException
    {
        int nattributes = ins.readInt();
        if (nattributes == 0)
            return null;
        Attributes a = new Attributes();

        for (int i = 0; i < nattributes; i++) {

            String key = ins.readString();
            String codername = ins.readString();
            StructureCoder coder = (StructureCoder) ReflectUtil.createObject(codername);

            ins.blockBegin();
            Object o = coder.read(ins);
            ins.blockEnd();
            a.setAttribute(key, o, coder);
        }
        return a;
    }

    static void write(Attributes a, StructureWriter outs) throws IOException
    {
        outs.writeComment("num attributes");
        if (a == null) {
            outs.writeInt(0);
        } else {
            // count the attributes that actually have valid coders
            // and data. We won't serialize those that don't.
            int nonnullattributes = 0;
            for (String key : a.attrs.keySet())
                if (a.attrs.get(key).coder != null && a.attrs.get(key).o != null)
                    nonnullattributes++;

            outs.writeInt(nonnullattributes);

            int keyidx = -1;
            for (String key : a.attrs.keySet()) {
                Attr attr = a.attrs.get(key);

                if (attr.coder == null || attr.o == null)
                    continue;

                keyidx++;

                outs.writeComment("attribute "+keyidx);
                outs.writeString(key);
                outs.writeString(attr.coder.getClass().getName());

                outs.blockBegin();
                attr.coder.write(outs, attr.o);
                outs.blockEnd();
            }
        }
    }

    Attributes copy()
    {
        Attributes a = new Attributes();
        a.attrs = (HashMap<String, Attr>) attrs.clone();
        return a;
    }

    void setAttribute(String key, Object o)
    {
        StructureCoder coder = null;
        if (o instanceof Integer)
            coder = new IntCoder();
        if (o instanceof String)
            coder = new StringCoder();
        if (o instanceof Long)
            coder = new LongCoder();
        if (o instanceof double[])
            coder = new DoublesCoder();
        if (o instanceof int[])
            coder = new IntsCoder();
        if (o instanceof LCMEncodable)
            coder = new LCMCoder();
        setAttribute(key, o, coder);
    }

    // code can be null
    void setAttribute(String key, Object o, StructureCoder coder)
    {
        attrs.put(key, new Attr(o, coder));
    }

    Object getAttribute(String key)
    {
        Attr a = attrs.get(key);
        if (a == null)
            return null;
        return a.o;
    }

}