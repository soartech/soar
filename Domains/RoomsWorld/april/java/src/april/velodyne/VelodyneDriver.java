package april.velodyne;

import java.util.*;
import java.io.*;
import java.net.*;

import april.lcmtypes.*;
import april.jmat.*;
import april.util.*;

import lcm.lcm.*;

public class VelodyneDriver
{
    DatagramSocket sock;
    LCM lcm = LCM.getSingleton();
    int nmsgs;

    public VelodyneDriver() throws IOException
    {
        sock = new DatagramSocket(2368); //, Inet4Address.getByName("192.168.0.200"));

        new RunThread().start();
    }

    class RunThread extends Thread
    {
        public void run()
        {
            try {
                while (true) {
                    byte buf[] = new byte[8192];
                    DatagramPacket p = new DatagramPacket(buf, buf.length);
                    sock.receive(p);

                    nmsgs++;

                    velodyne_t v = new velodyne_t();
                    v.utime = TimeUtil.utime();
                    v.datalen = p.getLength();
                    v.data = buf;

                    lcm.publish("VELODYNE", v);
                }
            } catch (IOException ex) {
                System.out.println("ex: "+ex);
                System.exit(1);
            }
        }
    }

    public static void main(String args[])
    {
        try {
            VelodyneDriver driver = new VelodyneDriver();

            long starttime = TimeUtil.utime();

            while (true) {
                double dt = (TimeUtil.utime() - starttime) / 1.0E6;

                System.out.printf("%15.3f VelodyneDriver nmsgs: %-8d\n",
                                  dt, driver.nmsgs);

                TimeUtil.sleep(1000);
            }

        } catch (IOException ex) {
            System.out.println("ex: "+ex);
        }
        System.exit(1);
    }
}
