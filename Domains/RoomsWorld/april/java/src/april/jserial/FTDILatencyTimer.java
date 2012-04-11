package april.jserial;

import java.io.*;

/** Try to set the latency timer to a small (sane) value on Linux **/
public class FTDILatencyTimer
{
    /** Returns success **/
    public static boolean setLatencyTimer(String devicepath, int latency_timer)
    {
        try {
            Process proc = Runtime.getRuntime().exec("udevadm info --query=path --name="+devicepath);
            BufferedReader ins = new BufferedReader(new InputStreamReader(proc.getInputStream()));

            String line = null;
            while ((line = ins.readLine()) != null) {

                line = stripLastDirectory(line);
                line = stripLastDirectory(line);

                String ltpath = "/sys/"+line+"/latency_timer";

                File f = new File(ltpath);
                if (!f.exists())
                    continue;

                System.out.println("FTDILatencyTimer: trying "+ltpath);

                BufferedWriter outs = new BufferedWriter(new FileWriter(ltpath));
                outs.write(""+latency_timer);
                outs.close();

                return true;
            }

        } catch (Exception ex) {
            System.out.println("ex: "+ex);
        }

        return false;
    }

    static String stripLastDirectory(String in)
    {
        int idx = in.lastIndexOf("/");
        return in.substring(0, idx);
    }

    public static void main(String args[])
    {
        String devpath = args.length > 0 ? args[0] : "/dev/ttyUSB0";
        boolean res = setLatencyTimer(devpath, 0);
        System.out.println(res ? "SUCCESS" : "FAIL");
    }
}
