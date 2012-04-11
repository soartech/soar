package april.vis;

import java.io.*;
import java.util.*;
import java.util.zip.*;
import java.lang.reflect.*;

import lcm.lcm.*;

import april.util.*;

/**
 * How-to make your VisObject also be VisSerializable.
 *  1. Implement the VisSerializable Interface
 *  2. Implement an public, no-args constructor
 *  3. Fill in the serialize, and unserialize methods in aforementioned interface
 *    a) If you have children that are VisObjects and VisSerializable,
 *       you can use the methods in this class:
 *             void serialize(VisSerializable, LCMDataOutputStream)
 *                             -- and --
 *             VisSerializable unserialize(LCMDataInputStream)
 *        to make your life easier.
 *     b) Refer to LCMDataInput/OutputStream for how to serialize primitive types
 *     c) If you have children that are VisObjects, but not serializable, you should
 *        not serialize them! (or modify those classes..)
 *  4. For reference, consult a simple example (e.g VisBox), a medium example
 *    (VisChain, VisData), or a 'hard' example (VisTexture)
 *
 *  NOTE: please be aware of the distinction between VisSerializable and
 *        VisSerialize if you get compilation errors
 */

/**
 * How-to save your .vis snapshot:  call VisSerialzie.serialize(VC) (API) or use VisCanvasPopopMenu (GUI)
 */
public class VisSerialize
{
    static boolean debug = false;
    static final int DEFAULT_BUFFER_SIZE = 32768;

    public static void serialize(VisSerializable o, LCMDataOutputStream out) throws IOException
    {
        if (debug) System.out.println("DBG:  "+o.getClass().getName());

        // Write the name of the class e.g. april.vis.VisBox
        out.writeStringZ(o.getClass().getName());

        // Encode this object in its own data stream
        LCMDataOutputStream  dout = new LCMDataOutputStream();
        o.serialize(dout);

        // Write the number of bytes for this object, and then the data
        out.writeInt(dout.size());
        out.write(dout.getBuffer(),0,dout.size());
    }

    public static VisSerializable unserialize(LCMDataInputStream in) throws IOException
    {
        // Grab class name
        String obj_name = in.readStringZ();
        if (debug) System.out.println("DBG: Loading "+obj_name);

        // Grab obj. data
        int olen = in.readInt();
        byte obj_buf[] = new byte[olen];
        in.readFully(obj_buf);
        LCMDataInputStream obj_in = new LCMDataInputStream(obj_buf);

        // Instantiate
        VisSerializable obj = (VisSerializable)ReflectUtil.createObject(obj_name);
        if (obj == null) {
            System.out.println("WRN Failed to read class '"+obj_name+"'!");
            return null;
        }

        // Unserialize
        obj.unserialize(obj_in);
        assert(obj_in.available() == 0);

        return obj;
    }

    // The rest of this file pertains to writing the VisCanvas to a file
    public static void writeVCToFile(VisCanvas vc, String filename)
    {
        try  {
            LCMDataOutputStream out = serializeVC(vc);
            FileOutputStream fout = new FileOutputStream(filename);
            byte data[] = compress(out.getBuffer(), 0, out.size());
            fout.write(data);
            fout.close();
        } catch(IOException e) {
            System.out.println("WRN: Failed to write vc to"+filename+" ex: "+e); e.printStackTrace();
        }
        System.out.println("Wrote canvas to "+filename);
    }

    public static LCMDataOutputStream serializeVC(VisCanvas vc) throws IOException
    {
        // Step 1: Get all the objects out
        HashMap<String, ArrayList<VisObject>> fronts = new HashMap<String, ArrayList<VisObject>>();
        // 1a) Copy out all the "front" parts of each Buffer (don't 'lock' vis this whole time)
        synchronized(vc.world.buffers) {
            for(String key : vc.world.bufferMap.keySet()) {
                VisWorld.Buffer buf  = vc.world.bufferMap.get(key);
                fronts.put(key, buf.getFront());
            }
        }

        LCMDataOutputStream gout = new LCMDataOutputStream(); //global out

        // Step 0 : Copy the camera
        serialize(vc.viewManager.viewGoal,gout);

        // 1b) Serialize each buffer
        for (String key : fronts.keySet()) {
            if (debug) System.out.println("DBG: Processing buffer "+key);

            gout.writeStringZ(key);
            Boolean enabled = vc.viewManager.enabledBuffers.get(key);
            gout.writeBoolean(enabled == null || enabled);
            gout.writeInt(vc.world.bufferMap.get(key).drawOrder);

            // Get only the objects for this buffer
            LCMDataOutputStream bout = new LCMDataOutputStream(); //buffer out
            for (VisObject obj : fronts.get(key)) {
                if (!(obj instanceof VisSerializable)) {
                    if (debug) System.out.println("DBG: Skipping "+obj.getClass().getName());
                    continue;
                }
                serialize((VisSerializable)obj, bout);
            }
            gout.writeInt(bout.size());
            gout.write(bout.getBuffer(),0,bout.size());
        }

        return gout;
    }

    public static VisCanvas readVCFromFile(String filename)
    {
        try  {
            File f = new File(filename);

            byte buffer[] = new byte[(int)f.length()];
            FileInputStream fin = new FileInputStream(filename);
            int len = fin.read(buffer);
            if (len != buffer.length) {
                System.out.println("WRN: Failed to read file fully "+filename);
                return null;
            }
            fin.close();

            buffer = uncompress(buffer, null);

            LCMDataInputStream in = new LCMDataInputStream(buffer);

            return unserializeVC(in);
        } catch(IOException e) {
            System.out.println("WRN: Failed to read vc from"+filename+" ex: "+e); e.printStackTrace();
        }
        return null;
    }

    public static VisCanvas unserializeVC(LCMDataInputStream global_in) throws IOException
    {
        VisWorld vw = new VisWorld();
        VisCanvas vc = new VisCanvas(vw);
        vc.viewManager.viewGoal = (VisView)unserialize(global_in);

        // Read each buffer individually
        while(global_in.available() > 0) {
            String buf_name = global_in.readStringZ();
            boolean enabled = global_in.readBoolean();
            VisWorld.Buffer vb = vw.getBuffer(buf_name);
            vc.viewManager.enabledBuffers.put(buf_name, enabled);
            vb.setDrawOrder(global_in.readInt());

            int len = global_in.readInt();
            if (debug) System.out.println("DBG: Reading buffer "+buf_name + " len ="+len);
            byte buf[] = new byte[len];
            global_in.readFully(buf);
            LCMDataInputStream buffer_in = new LCMDataInputStream(buf);
            while (buffer_in.available() > 0) {
                VisSerializable obj = unserialize(buffer_in);
                if (obj == null) {
                    System.out.println("WRN: Can't continue reading buffer "+buf_name+"!");
                    break;
                }
                // Add to world
                vb.addBuffered((VisObject)obj);
            }
            vb.switchBuffer();
        }
        return vc;
    }

    // Compress an entire byte array
    public static byte[] compress(byte decompressed[]) throws IOException
    {
        return compress(decompressed, 0, decompressed.length);
    }

    // Compress a section of a byte array
    public static byte[] compress(byte decompressed[], int start, int len) throws IOException
    {
        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        GZIPOutputStream zout = new GZIPOutputStream(bout);

        zout.write(decompressed, start , len);
        zout.finish();
        byte compressed[] = bout.toByteArray();
        zout.close();

        return compressed;
    }

    // Decompress from a byte array where the expected output size is
    // known. If the initial size of 'output' is not sufficient, a
    // larger array will be created, and returned. Pass 'output' as
    // null to have the default buffer size used
    public static byte[] uncompress(byte compressed[], byte output[]) throws IOException
    {
        ByteArrayInputStream bin = new ByteArrayInputStream(compressed);
        GZIPInputStream zin = new GZIPInputStream(bin);

        if (output == null)
            output = new byte[DEFAULT_BUFFER_SIZE];

        int total = 0; // cumulative count
        int n = 0; // count from single read
        while ((n = zin.read(output, total, output.length - total) ) != -1)
        {
            total += n;

            // Read forward a small bit to see if we actually need to
            // expand array If user passes in exactly sized array,
            // this may not be necessary
            byte small[] = new byte[1];
            int smallread = zin.read(small, 0, small.length);

            if (total == output.length && smallread != -1) {
                // We've got to allocate more
                byte tmp[] = output;
                output = new byte[total * 2];
                System.arraycopy(tmp, 0, output, 0, total);

            }

            if (smallread != -1) {
                // write in the test read
                output[total] = small[0];
                total++;
            }
        }

        // If we read exactly the right amount, then we can return now
        if (total  == output.length)
            return output;

        // If we didn't exactly fill the buffer, then we need to crop the array
        byte cropped[] = new byte[total];
        System.arraycopy(output, 0, cropped, 0, total);

        return cropped;
    }
}
