package april.applanix;

import java.io.*;
import java.net.*;

import april.util.*;
import april.lcmtypes.*;

import lcm.lcm.*;

/** TODO: Send commands to Applanix in order to configure the data rate. Right now, we assume that group 1 messages have been configured to be sent on the logging port at a high rate (e.g., 100 Hz). **/

public class ApplanixDriver
{
    Socket sock;
    DataInputStream ins;

    LCM lcm = LCM.getSingleton();

    int syncerr = 0; // didn't find synchronization sequence
    int frameerr = 0; // didn't find end-of-frame marker
    int chkerr = 0; // checksum errors
    int msgs = 0; // messages correctly received

    public ApplanixDriver() throws IOException
    {
        sock = new Socket("129.100.0.231", 5603);
        ins = new DataInputStream(sock.getInputStream());

        new ReaderThread().start();
    }

    class ReaderThread extends Thread
    {
        public void run()
        {
            try {
                while (true) {

                    int c;

                    byte hdr[] = new byte[8];
                    hdr[0] = (byte) (ins.read() & 0xff);
                    if (hdr[0] != '$') {
                        syncerr++;
                        continue;
                    }

                    ins.readFully(hdr, 1, 7);

                    if (!(hdr[1]=='G' && hdr[2]=='R' && hdr[3]=='P') &&
                        !(hdr[1]=='M' && hdr[2]=='S' && hdr[3]=='G') &&
                        !(hdr[1]=='I' && hdr[2]=='M' && hdr[3]=='U')) {
                        syncerr++;
                    }

                    int length = (hdr[6]&0xff) + (hdr[7]&0xff)*256;

                    byte msg[] = new byte[8 + length];
                    for (int i = 0; i < 8; i++)
                        msg[i] = hdr[i];

                    ins.readFully(msg, 8, length);

                    int chk = 0;
                    for (int i = 0; i < msg.length; i+=2)
                        chk += (msg[i]&0xff) + (msg[i+1]&0xff)*256;

                    if ((chk&0xffff) != 0) {
                        chkerr++;
                        continue;
                    }

                    if (msg[msg.length-2]=='$' && msg[msg.length-1]=='#') {
                        raw_t r = new raw_t();
                        r.utime = TimeUtil.utime();
                        r.len = msg.length;
                        r.buf = msg;
                        lcm.publish("APPLANIX", r);

                        msgs++;
                    } else {
                        frameerr++;
                    }
                }
            } catch (IOException ex) {
                return;
            }
        }
    }

    public static void main(String args[])
    {
        ApplanixDriver driver = null;

        try {
            driver = new ApplanixDriver();
        } catch (IOException ex) {
            System.out.println("ex: "+ex);
            System.exit(1);
        }

        long starttime = TimeUtil.utime();

        while (true) {
            double dt = (TimeUtil.utime() - starttime) / 1.0E6;

            System.out.printf("%15.3f ApplanixDriver  msgs: %-8d syncerr: %-8d  frameerr: %-8d  chkerr: %-8d\n",
                              dt, driver.msgs, driver.syncerr, driver.frameerr, driver.chkerr);

            TimeUtil.sleep(1000);
        }
    }
}
