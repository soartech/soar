package april.util;

import java.io.*;
import java.nio.*;

public class GamePad
{
    public static final int NAXES = 16;
    public static final int NBUTTONS = 16;

    String paths[] = new String[] { "/dev/js0", "/dev/input/js0" };
    int    axes[] = new int[NAXES];
    int    buttons[] = new int[NBUTTONS];

    boolean reconnect;
    boolean present = false, lastPresent = true;

    ///////////////////////////////////////////////////////
    public GamePad(boolean _reconnect)
    {
        reconnect = _reconnect;

        new ReaderThread().start();
    }

    public GamePad(String path, boolean _reconnect)
    {
        this.reconnect = _reconnect;
        this.paths = new String[] { path };

        new ReaderThread().start();
    }

    // returns [-1, 1]
    public double getAxis(int axis)
    {
        if (axis >= axes.length)
            return 0;

        return axes[axis] / 32767.0;
    }

    public boolean getButton(int button)
    {
        if (button >= buttons.length)
            return false;

        return buttons[button] > 0;
    }

    /** Return first 32 buttons encoded as one bit per button. **/
    public int getButtons()
    {
        int b = 0;

        for (int i = 0; i < Math.min(32, buttons.length); i++)
            if (buttons[i] > 0)
                b |= (1 << i);

        return b;
    }

    public boolean isPresent()
    {
        return present;
    }

    /** Returns once any button has been pressed, returning the button
     * id. This is useful with cordless game pads as a way of ensuring
     * that there's actually a device connected.
     **/
    public int waitForAnyButtonPress()
    {
        boolean buttonState[] = new boolean[16];
        for (int i = 0; i < buttonState.length; i++)
            buttonState[i] = getButton(i);

        while (true) {

            for (int i = 0; i < buttonState.length; i++)
                if (getButton(i) != buttonState[i])
                    return i;

            TimeUtil.sleep(10);
        }
    }

    class ReaderThread extends Thread
    {
        ReaderThread()
        {
            setDaemon(true);
        }

        public void run()
        {
            while (true) {
                try {
                    runEx();
                } catch (IOException ex) {
                }

                if (!reconnect)
                    System.exit(-1);

                present = false;
                if (lastPresent != present)
                    System.out.println("DEBUG: No Gamepad Present");
                lastPresent = present;

                for (int i = 0; i < axes.length; i++)
                    axes[i] = 0;
                for (int i = 0; i < buttons.length; i++)
                    buttons[i] = 0;

                TimeUtil.sleep(500);
            }
        }

        // Make one attempt to open and read from the joystick,
        // returning if an error occurs.
        void runEx() throws IOException
        {
            String devicePath = null;

            for (int i = 0; i < paths.length; i++) {
                String path = paths[i];
                File f = new File(path);
                if (f.exists()) {
                    devicePath = path;
                    break;
                }
            }

            if (devicePath == null)
                return;

            boolean pressedButton = false;

            FileInputStream fins = new  FileInputStream(new File(devicePath));
            byte buf[] = new byte[8];

            while (true) {
                fins.read(buf);

                int mstime = (buf[0]&0xff) | ((buf[1]&0xff)<<8) | ((buf[2]&0xff)<<16) | ((buf[3]&0xff)<<24);
                int value  = (buf[4]&0xff) | ((buf[5]&0xff)<<8);

                if ((value & 0x8000)>0) // sign extend
                    value |= 0xffff0000;

                int type   = buf[6]&0xff;
                int number = buf[7]&0xff;

                if ((type&0x3)==1) {
                    if (number < buttons.length) {
                        buttons[number] = value;
                        if (value == 1)
                            pressedButton = true;
                    } else {
                        System.out.println("GamePad: "+number+" buttons!");
                    }
                }

                if ((type&0x3) == 2) {
                    if (number < axes.length) {
                        if (pressedButton)
                            axes[number] = value;
                    } else {
                        System.out.println("GamePad: "+number+" axes!");
                    }
                }

                present = true;
                if (lastPresent != present)
                    System.out.println("DEBUG: Gamepad Now Present");
                lastPresent = present;
            }
        }
    }

    ///////////////////////////////////////////////////////
    public static void main(String args[])
    {
        GamePad gp = new GamePad(true);

        int nAxes = 6;
        int nButtons = 16;

        for (int i = 0; i < nAxes; i++) {
            System.out.printf("%10s ", "Axis "+i);
        }

        for (int i = 0; i < nButtons; i++) {
            int v = i & 0xf;
            System.out.printf("%c", v>=10 ? 'a'+(v-10) : '0'+v);
        }
        System.out.printf("\n");

        while (true) {
            String s = "";
            for (int i = 0; i < nButtons; i++)
                s=s+(gp.getButton(i) ? 1 : 0);

            System.out.printf("\r");
            for (int i = 0; i < nAxes; i++)
                System.out.printf("%10f ", gp.getAxis(i));
            System.out.printf("%s", s);

            try {
                Thread.sleep(10);
            } catch (InterruptedException ex) {
            }
        }
    }
}
