package edu.umich.robot.april;

import java.awt.Color;
import java.io.File;
import java.io.IOException;

import april.config.Config;
import april.config.ConfigFile;
import april.jmat.LinAlg;

/** Useful configuration utilities. **/
public class ConfigUtil
{
    // TODO SoarApril
    /*
    public static void processIncludes(ConfigFile config, String includeKey)
    {
        if (!config.hasKey(includeKey)) {
            return;
        }
        for (String include : config.getPaths(includeKey)) {
            try {
                config.merge(new File(include));
            } catch (IOException e) {
                System.err.println("Error loading include " + include);
            }
        }
    }
    */

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
        double v[] = sensorName == null ? config.getDoubles("color") : config.getDoubles(sensorName+".color");
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

    public static double[] getQuaternion(Config config)
    {
        return getQuaternion(config, null);
    }
    
    public static double[] getQuaternion(Config config, String sensorName)
    {
        String base = sensorName == null ? "" : sensorName + ".";
        if (config.hasKey(base + "quaternion")) {
            double q[] = config.requireDoubles(base + "quaternion");
            return q;
        }

        if (config.hasKey(base + "rollpitchyaw_degrees")) {
            double rpy[] = config.requireDoubles(base + "rollpitchyaw_degrees");
            for (int i = 0; i < rpy.length; i++)
                rpy[i] = Math.toRadians(rpy[i]);
            return LinAlg.rollPitchYawToQuat(rpy);
        }

        if (config.hasKey(base + "rollpitchyaw_radians")) {
            double rpy[] = config.requireDoubles(base + "rollpitchyaw_radians");
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
