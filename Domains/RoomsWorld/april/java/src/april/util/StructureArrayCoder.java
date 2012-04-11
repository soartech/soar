package april.util;

import java.util.*;
import java.io.*;

// handles the type:  ArrayList<StructureCoder>

/** Concrete implementation of StructureCoder for ArrayList. **/
public class StructureArrayCoder implements StructureCoder
{
    public StructureArrayCoder()
    {
    }

    public void write(StructureWriter outs, Object o) throws IOException
    {
        assert(o instanceof ArrayList);
        ArrayList os = (ArrayList) o;

        outs.writeInt(os.size());

        for (Object tobj: os) {
            if (!(tobj instanceof StructureCoder))
                System.out.println(tobj);
            assert(tobj instanceof StructureCoder);
            outs.writeString(tobj.getClass().getName());
            outs.blockBegin();
            ((StructureCoder) tobj).write(outs, tobj);
            outs.blockEnd();
        }
    }

    public Object read(StructureReader ins) throws IOException
    {
        int sz = ins.readInt();

        ArrayList al = new ArrayList();

        for (int i = 0; i < sz; i++) {
            String className = ins.readString();
            Object os = ReflectUtil.createObject(className);
            ins.blockBegin();
            al.add(((StructureCoder) os).read(ins));
            ins.blockEnd();
        }

        return al;
    }
}
