/*
 * Copyright (c) 2011, Regents of the University of Michigan
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package edu.umich.robot;

import javax.swing.SwingUtilities;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import april.config.Config;
import april.config.ConfigUtil;
import edu.umich.robot.util.swing.SwingTools;

/**
 * <p> Top level entry point for the application.
 *
 * <p> Creates either GUI or Headless/batch runner depending on config file.
 *
 * @author voigtjr@gmail.com
 */
public class Application
{
    private static final Log logger = LogFactory.getLog(Application.class);
    
    /**
     * <p> When running in batch mode, this generates the command line for a
     * run.
     */
    private String[] toArgs(String config)
    {
        return new String[] { "--config", config };
    }

    private static int arrayFind(String target, String[] args) {
        for (int i = 0; i < args.length; ++i) {
            if (args[i].equals(target)) return i;
        }
        return -1;
    }

    private static String[] debuggerArgs(String[] args) {
        int debuggerIndex = arrayFind("-debugger", args);
        if (debuggerIndex == -1) return null;
        String[] ret = new String[args.length - debuggerIndex];
        for (int i = 0; i < ret.length; ++i) {
            ret[i] = args[debuggerIndex + i];
        }
        return ret;
    }
    
    public static void main(final String[] args)
    {
        String[] debuggerArgList = debuggerArgs(args);
        if (debuggerArgList == null) {
            new Application(args);
        } else {
            new edu.umich.soar.debugger.Application(args, false, null);
        }
    }

    /**
     * <p> Call with --config CONFIG_FILE or with no args to start a file
     * chooser.
     * 
     * <p> If the config file contains the key "multiple-runs.configs", a
     * headless batch run will be started using each config file in the array.
     * Otherwise the GUI is started.
     */
    public Application(final String[] args)
    {
        // Config config = ConfigUtil.getDefaultConfig(args);
        Config config = (args.length > 0) ? ConfigUtil.getDefaultConfig(args) : GuiApplication.promptForConfig(null);
        if (config == null || !config.hasKey("multiple-runs.configs"))
            runGui(config);
        else
            multipleRuns(config.getChild("multiple-runs"));
    }
    
    /**
     * <p> Loops, running each config file in a batch mode.
     * 
     * <p> Each run continues until a specified timeout or a number of cycles
     * is exceeded. Both or neither may be specified.  The run also will stop
     * if Soar stops for whatever reason.
     *
     * <p> After a run, the garbage collector is called and the system sleeps
     * for a few seconds. This helps Java manage its memory and threads.
     */
    private void multipleRuns(Config config)
    {
        String[] configs = config.requireStrings("configs");
        int cycles = config.requireInt("cycles");
        int seconds = config.requireInt("seconds");
        
        for (int i = 0; i < configs.length; ++i)
        {
            logger.info("Running " + configs[i] + " for " + cycles + " cycles, timeout " + seconds + " seconds.");
            HeadlessApplication h = new HeadlessApplication(toArgs(configs[i]), cycles, seconds);
            if (h.go())
                break;
            logger.info("Done, garbage collect and 5 second sleep.");
            System.gc();
            try
            {
                Thread.sleep(5000);
            }
            catch (InterruptedException e)
            {
                e.printStackTrace();
            }
        }
        logger.info("Done.");
    }
    
    private void runGui(final Config config)
    {
        SwingTools.initializeLookAndFeel();
        SwingUtilities.invokeLater(new Runnable()
        {
            public void run()
            {
                new GuiApplication(config);
            }
        });
    }
    
    /**
     * Utility code placed here to avoid duplication.
     *
     * Modifies the config file for the simulator.
     */
    public static void setupSimulatorConfig(Config config)
    {
        addImageData(config, "simulator.obstacles.");
    }
    
    /**
     * Utility code placed here to avoid duplication.
     *
     * Modifies the config file for the simulator.
     */
    public static void addImageData(Config config, String prefix)
    {
        config.setStrings(prefix + "image_path", config.requireStrings("image_path"));
        config.setStrings(prefix + "image_origin", config.requireStrings("image_origin"));
        config.setStrings(prefix + "meters_per_pixel", config.requireStrings("meters_per_pixel"));
        config.setString(prefix + "floor_texture_path", "../common/floor.jpg");
        config.setString(prefix + "wall_texture_path", "../common/wall.jpg");
    }
    
}

