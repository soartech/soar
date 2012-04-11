package april.config;

import java.awt.*;
import java.io.*;

import april.jmat.*;
import april.jmat.geom.*;

public class ConfigUtil
{
    public static Config getDefaultConfig(String args[])
    {
        // Did they manually specify one?
        if (args != null) {
            for (int i = 0; i < args.length; i++) {

                if (i+1 < args.length && (args[i].equals("-c") || args[i].equals("--config"))) {
                    try {
                        return new ConfigFile(args[i+1]);
                    } catch (IOException ex) {
                        System.out.println("Error loading config file: "+args[i+1]);
                        System.exit(-1);
                    }
                }

                if (args[i].startsWith("--config")) {
                    String path = args[i].substring(9);
                    try {
                        return new ConfigFile(path);
                    } catch (IOException ex) {
                        System.out.println("Error loading config file: "+args[i+1]);
                        System.exit(-1);
                    }
                }

            }
        }

        // try environment variable
        String path = System.getenv("APRIL_CONFIG");
        if (path != null) {
            try {
                return new ConfigFile(path);
            } catch (IOException ex) {
                System.out.println("Error loading config file: "+path);
                System.exit(-1);
            }
        }

        // no luck. Return an empty config.
        System.out.println("Specify a config file with --config <configpath> or with the APRIL_CONFIG environment variable.");

        return new Config();
    }

    public static Color getColor(Config config, String sensorName, Color defaultColor)
    {
        double v[] = config.getDoubles(sensorName+".color");
        if (v == null)
            return defaultColor;

        if (v.length == 3)
            return new Color((float) v[0], (float) v[1], (float) v[2]);
        if (v.length == 4)
            return new Color((float) v[0], (float) v[1], (float) v[2], (float) v[3]);

        System.out.println("Badly formatted color specification for "+sensorName);
        return defaultColor;
    }

    public static double[] getPosition(Config config, String sensorName)
    {
        return config.requireDoubles(sensorName+".position");
    }

    public static double[] getQuaternion(Config config, String sensorName)
    {
        if (config.hasKey(sensorName+".quaternion")) {
            double q[] = config.requireDoubles(sensorName+".quaternion");
            return q;
        }

        if (config.hasKey(sensorName+".rollpitchyaw_degrees")) {
            double rpy[] = config.requireDoubles(sensorName+".rollpitchyaw_degrees");
            for (int i = 0; i < rpy.length; i++)
                rpy[i] = Math.toRadians(rpy[i]);
            return LinAlg.rollPitchYawToQuat(rpy);
        }

        if (config.hasKey(sensorName+".rollpitchyaw_radians")) {
            double rpy[] = config.requireDoubles(sensorName+".rollpitchyaw_radians");
            return LinAlg.rollPitchYawToQuat(rpy);
        }

        System.out.println("No orientation specified for sensor "+sensorName);
        return null;
    }

    /** Returns 4x4 transformation matrix. **/
    public static double[][] getRigidBodyTransform(Config config, String sensorName)
    {
        double q[] = getQuaternion(config, sensorName);
        double pos[] = getPosition(config, sensorName);

        return LinAlg.quatPosToMatrix(q, pos);
    }

}
