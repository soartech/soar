package april.util;

import java.io.*;
import java.util.*;

import april.jmat.*;
import april.util.*;

/** This class is a driver for the 3Dconnexion SpaceNavigator joystick.
   This is a 6-DOF joystick with two side buttons. While many joysticks
   support 3-DOF (roll, pitch, and yaw), this device measures translation
   on each axis, making it ideal for navigating 3D environments. Two popular
   applications which use this device are Google Earth and Google Sketchup.<br>
   <br>
   <font color="red">Important!</font>
   This driver was written for Linux (tested on Ubuntu 10.04) and expects the
   device at /dev/spacenav. To automatically mount the device here, add a file
   named<br>
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;/etc/udev/rules.d/99-<some name>.rules<br>
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;e.g. /etc/udev/rules.d/99-spacenav.rules<br>
   <br>
    and paste in it the following line:<br>
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;ATTRS{name}=="3Dconnexion SpaceNavigator", NAME="spacenav", MODE="0666"<br>
   <br>
    This will tell udev to mount a device with this name at /dev/spacenav with
    suitable permissions for user access.<br>
   <br>
    This driver minorly supports unplug actions (up to a fixed number of tries)
    and will quit once max_attempts tries have failed sequentially.<br>
   <br>
    This driver does not currently support:<br>
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;* notification on device open/close events<br>
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;* setting device parameters (sensitivities, axes inversions)<br>
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;* reading device parameters (^^)<br>
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;* notification on button press/release (only current button status)<br>
   <br>
    <b>Notes on udev</b>
    As an aside, if you want to use udev for another device, you can find
    relevant parameters for the device<br>
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;udevadm info -q all --attribute-walk -n /dev/spacenav<br>
    where you should replace spacenav with the name of your device. /dev/input/
    and /dev may contain devices of interest. One comprehensive udev guide
    exists at:<br>
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;http://reactivated.net/writing_udev_rules.html<br>
   **/
public class SpaceNavigator
{
    boolean hexdump;

    int open_attempts = 0;
    int max_attempts = 10;

    String path = "/dev/spacenav";

    int values[] = new int[8];
    boolean button_event = false;

    ArrayList<Listener> listeners = new ArrayList<Listener>();

    public SpaceNavigator()
    {
        this(false);
    }

    public SpaceNavigator(boolean hexdump)
    {
        this.hexdump = hexdump;

        // initialize stream
        FileInputStream f = initialize(false);

        // start parsing thread
        ParsingThread t = new ParsingThread(f);
        t.start();
    }

    public class MotionEvent
    {
        public int x;
        public int y;
        public int z;

        public int roll;
        public int pitch;
        public int yaw;

        public boolean left;
        public boolean right;

        public double[][] getMatrix()
        {
            return LinAlg.xyzrpyToMatrix(new double[] {x, y, z, roll, pitch, yaw});
        }

        public MotionEvent(int values[])
        {
            x       = -values[1];
            y       = -values[0];
            z       = -values[2];
            roll    = -values[4];
            pitch   = -values[3];
            yaw     = -values[5];

            left  = (values[6] == 1);
            right = (values[7] == 1);
        }
    }

    public interface Listener
    {
        public void handleUpdate(MotionEvent me);
    }

    public void addListener(Listener listener)
    {
        listeners.add(listener);
    }

    public FileInputStream initialize(boolean retry)
    {
        FileInputStream f = null;

        try {
            f = new FileInputStream(path);
        } catch (IOException e) {
            System.err.printf("Error: no device at path '%s'\n",
                               path);
        }

        if (f == null) {
            open_attempts++;

            if (open_attempts > max_attempts) {
                System.err.printf("Failed to re-open %d times. Exiting.\n",
                                  max_attempts);
                System.exit(-1);
            }

            if (!retry) {
                System.err.println("Failed to open on launch. Exiting.");
                System.exit(-1);
            }
        } else {
            // reset attempts counter
            open_attempts = 0;
        }

        return f;
    }
    private class ParsingThread extends Thread
    {
        FileInputStream is;

        public ParsingThread(FileInputStream is)
        {
            this.is = is;
        }

        public void run()
        {
            while (true) {
                byte buf[] = new byte[24];

                try {
                    int r = is.read(buf);

                    short type  = (short) (((buf[17] & 0xFF) << 8) | ((buf[16] & 0xFF)));
                    short index = (short) (((buf[19] & 0xFF) << 8) | ((buf[18] & 0xFF)));
                    int value   = ((buf[23] & 0xFF) << 24) | ((buf[22] & 0xFF) << 16) | ((buf[21] & 0xFF) << 8) | ((buf[20] & 0xFF));

                    switch (type) {
                        // motion event
                        case 2:
                            values[index] = value;
                            button_event = false;
                            break;

                        // key press
                        case 4:
                            button_event = true;
                            break;

                        // key release
                        case 1:
                            int button_id = index & 0xFF;
                            int depressed = value;

                            values[button_id + 6] = depressed;

                            // clear the button event
                            button_event = false;
                            break;

                        // flush
                        case 0:
                            // notify listeners
                            for (Listener listener : listeners)
                                listener.handleUpdate(new MotionEvent(values));

                            break;

                        // other
                        default:
                            System.out.printf("Unrecognized type: %3d index: %3d value: %5d\n",
                                              type, index, value);
                            break;
                    }

                    if (hexdump) {
                        for (int i=0; i < buf.length; i++)
                            System.out.printf("%2X ", buf[i] & 0xFF);
                        System.out.println();
                    }

                } catch (IOException e) {
                    //System.err.println("Exception: "+e);
                    //e.printStackTrace();
                    System.out.println("Error: Device broken.");

                    // re-initialize
                    TimeUtil.sleep(1000);
                    FileInputStream f = initialize(true);

                    if (f != null)
                        is = f;
                }
            }
        }
    }

    public static void main(String args[])
    {
        GetOpt opts  = new GetOpt();

        opts.addBoolean('h',"help",false,"See this help screen");
        opts.addBoolean('x',"hexdump",false,"Enable hex dump");

        if (!opts.parse(args)) {
            System.out.println("option error: "+opts.getReason());
	    }

        if (opts.getBoolean("help")) {
            System.out.println("Usage:");
            opts.doHelp();
            System.exit(1);
        }

        boolean hexdump = opts.getBoolean("hexdump");

        SpaceNavigator sn = new SpaceNavigator(hexdump);
        sn.addListener(new ExampleListener());
    }
}

class ExampleListener implements SpaceNavigator.Listener
{
    public ExampleListener()
    {
    }

    @Override
    public void handleUpdate(SpaceNavigator.MotionEvent me)
    {
        System.out.printf("[MotionEvent] x: %4d y: %4d z: %4d roll: %4d pitch: %4d yaw: %4d left: %d right: %d\n",
                          me.x, me.y, me.z, me.roll, me.pitch, me.yaw, me.left ? 1 : 0, me.right ? 1 : 0);
    }
}
