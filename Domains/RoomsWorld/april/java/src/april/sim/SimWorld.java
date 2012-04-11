package april.sim;

import java.awt.*;
import java.io.*;
import java.util.*;

import april.vis.*;
import april.jmat.*;
import april.util.*;
import april.config.*;

public class SimWorld
{
    public GPSLinearization gpslin;
    public GeoImage geoimage;

    public Config config;

    public ArrayList<SimObject> objects = new ArrayList<SimObject>();

    public SimWorld(Config config)
    {
        this.config = config;
        handleConfig();
    }

    public SimWorld(String path, Config config) throws IOException
    {
        this.config = config;
        handleConfig();

        StructureReader ins = new TextStructureReader(new BufferedReader(new FileReader(path)));

        while (true) {
            String cls = ins.readString();

            if (cls == null) // EOF?
                break;

            try {
                SimObject so = createObject(this, cls);
                if (so != null) {
                    ins.blockBegin();
                    so.read(ins);
                    synchronized(this) {
                        objects.add(so);
                    }
                    ins.blockEnd();
                }
            } catch (Exception ex) {
                System.out.println("ex: "+ex);
            }
        }

        ins.close();
    }

    void handleConfig()
    {
        if (config == null)
            config = new Config();

        double ll[] = config.getDoubles("simulator.latlon", null);
        if (ll != null)
            gpslin = new GPSLinearization(ll);

        String geopath = config.getString("simulator.geoimage", null);

        if (geopath != null) {
            geopath = EnvUtil.expandVariables(geopath);

            try {
                geoimage = new GeoImage(geopath, null);
            } catch (IOException ex) {
                System.out.println("ex: "+ex);
                System.exit(1);
            }
            gpslin = geoimage.getGPSLinearization();
        }


    }

    public static SimObject createObject(SimWorld sw, String cls)
    {
        try {
            Object obj = Class.forName(cls).getConstructor(SimWorld.class).newInstance(sw);
            assert (obj instanceof SimObject);
            SimObject so = (SimObject) obj;
            return so;

        } catch (Exception ex) {
            System.out.println("ex: "+ex);
        }

        return null;
    }

    public synchronized void setRunning(boolean b)
    {
        for (SimObject so : objects)
            so.setRunning(b);
    }

    public void write(String path) throws IOException
    {
        FileWriter outs = new FileWriter(path);
        write(new BufferedWriter(outs));
        outs.close();
    }

    public void write(BufferedWriter _outs) throws IOException
    {
        StructureWriter outs = new TextStructureWriter(_outs);

        for (SimObject so : objects) {
            outs.writeString(so.getClass().getName());
            outs.blockBegin();
            so.write(outs);
            outs.blockEnd();
        }

        outs.close();
    }

    public synchronized double collisionDistance(double pos[], double dir[], HashSet<SimObject> ignore)
    {
        double dist = Double.MAX_VALUE;

        for (SimObject so : objects) {
            if (ignore != null && ignore.contains(so))
                continue;

            dist = Math.min(dist, Collisions.collisionDistance(pos, dir, so.getShape(), so.getPose()));
        }

        return dist;
    }
}
