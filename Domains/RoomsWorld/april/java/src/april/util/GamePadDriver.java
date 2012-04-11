package april.util;

import april.util.*;
import lcm.lcm.*;
import april.lcmtypes.*;

public class GamePadDriver
{
    public static void main(String args[])
    {
        GetOpt gopt = new GetOpt();
        gopt.addString('c', "channel", "GAMEPAD", "LCM channel to send on");
        gopt.addBoolean('h', "help", false, "Show this help");

        if (!gopt.parse(args) || gopt.getBoolean("help")) {
            gopt.doHelp();
            System.exit(1);
        }

        GamePad gp = new GamePad(true);
        LCM lcm = LCM.getSingleton();

        boolean gotPress = false;
        long oldTime = 0;

        while (true) {

            TimeUtil.sleep(gp.isPresent() ? 25 : 250);

            gamepad_t msg = new gamepad_t();
            msg.utime = TimeUtil.utime();

            msg.naxes = 6;
            msg.axes = new double[msg.naxes];
            for (int i = 0; i < msg.naxes; i++)
                msg.axes[i] = gp.getAxis(i);

            msg.buttons = 0;
            for (int i = 0; i < 32; i++)
                if (gp.getButton(i))
                    msg.buttons |= (1<<i);

            if (msg.buttons > 0)
                gotPress = true;

            msg.present = gp.isPresent();

            lcm.publish(gopt.getString("channel"), msg);
        }
    }
}
