package april.util;

/** Simple time utilities. **/
public class TimeUtil
{
    public static long utime()
    {
        return System.currentTimeMillis()*1000;
    }

    public static void sleep(int ms)
    {
        try {
            Thread.sleep(ms);
        } catch (InterruptedException ex) {
        }
    }
}
