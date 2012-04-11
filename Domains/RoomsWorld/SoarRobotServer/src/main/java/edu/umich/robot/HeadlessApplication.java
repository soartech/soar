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

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import april.config.Config;
import april.config.ConfigUtil;

import com.google.common.util.concurrent.MoreExecutors;

import edu.umich.robot.events.SoarStoppedEvent;
import edu.umich.robot.soar.SoarProperties;
import edu.umich.robot.util.Configs;
import edu.umich.robot.util.Pose;
import edu.umich.robot.util.events.RobotEvent;
import edu.umich.robot.util.events.RobotEventListener;

/**
 * <p>
 * Top-level class for batch runs of the simulator without a GUI.
 * 
 * @author voigtjr@gmail.com
 */
public class HeadlessApplication
{
    private static final Log logger = LogFactory.getLog(HeadlessApplication.class);

    /**
     * <p>
     * Max number of cycles.
     */
    private final int cycles;
    
    /**
     * <p>
     * Timeout.
     */
    private final int seconds;
    
    /**
     * <p>
     * Set to true when the shutdown handler (usually control-c) is used to stop
     * the simulation. Helps callers tell if they should run again.
     */
    private final AtomicBoolean shutdown = new AtomicBoolean(false);
    
    /**
     * <p>
     * Current configuration.
     */
    private final Config config;

    /**
     * <p>
     * Constructor. 
     * 
     * @param args Configuration file from the command line. Should probably just pass the config instead of command line args.
     * @param cycles Maximum number of decision cycles before aborting, use negative to ignore.
     * @param seconds Maximum number of seconds before aborting, use <= 0 to ignore.
     */
    public HeadlessApplication(String[] args, int cycles, int seconds)
    {
        this.cycles = cycles;
        this.seconds = seconds;
        
        config = ConfigUtil.getDefaultConfig(args);
        if (config == null)
        {
            logger.error("Missing config file on command line.");
            System.exit(1);
        }
        
        Application.setupSimulatorConfig(config);
        
        Configs.toLog(logger, config);
    }

    /**
     * <p>
     * Start the simulation. Creates a contorller, adds stuff, calls run (see
     * also).
     * 
     * @return True if JVM shutdown was the cause for the run stopping, so that
     *         caller can break.
     */
    public boolean go()
    {
        Controller controller = new Controller(config, null);

        try {
            controller.getSoarProperties().set(SoarProperties.SPAWN_DEBUGGERS, Boolean.FALSE);
    
            String[] splinters = config.getStrings("splinters", new String[0]);
            if (splinters == null || splinters.length == 0)
            {
                logger.error("Must have at least one splinter in config file.");
                System.exit(1);
            }
                
            for (String s : splinters)
            {
                double[] pos = config.getDoubles(s + ".position");
                if (pos == null)
                {
                    logger.error("Splinter indexed in config file but no position defined: " + s);
                    System.exit(1);
                }
                
                Pose pose = new Pose(pos);
                String prods = config.getString(s + ".productions");
                boolean collisions = config.getBoolean(s + ".wallCollisions", true);

                controller.createSplinterRobot(s, pose, collisions);
                controller.createSimSplinter(s);
                controller.createSimLaser(s); 
                if (prods == null)
                {
                    logger.error("Splinter needs productions: " + s);
                    System.exit(1);
                }
                
                logger.info("Sleeping 5 seconds after creating robot.");
                Thread.sleep(5000);
    
                controller.createSoarController(s, s, prods, config.getChild(s + ".properties"));
            }
            
            logger.info("Sleeping 5 seconds before running.");
            Thread.sleep(5000);

            run(controller);
        } 
        catch (InterruptedException e)
        {
            logger.error("Interrupted.");
            e.printStackTrace();
        }
        finally
        {
            // stops soar
            controller.shutdown();
        }
        return shutdown.get();
    }
    
    /**
     * <p>
     * Start Soar, wait for timeout or Soar to stop.
     * 
     * @param controller
     *            Simulation controller initialized.
     * @throws InterruptedException
     *             Thrown on thread interrupt.
     */
    private void run(Controller controller) throws InterruptedException
    {
        final CountDownLatch doneSignal = new CountDownLatch(1);

        Thread shutdownHook = new Thread() {
            @Override
            public void run()
            {
               logger.warn("Shutdown detected.");
               shutdown.set(true);
               doneSignal.countDown();
            } 
        };
        
        Runtime.getRuntime().addShutdownHook(shutdownHook);
        
        try 
        {
            controller.addListener(SoarStoppedEvent.class, new RobotEventListener()
            {
                public void onEvent(RobotEvent event)
                {
                    logger.info("Soar stop detected.");
                    doneSignal.countDown();
                }
            });
            
            ScheduledExecutorService schexec = MoreExecutors.getExitingScheduledExecutorService(new ScheduledThreadPoolExecutor(1));
            ScheduledFuture<?> task = null;
            if (seconds > 0)
            {
                task = schexec.schedule(new Runnable() {
                    public void run()
                    {
                        logger.info("Time up.");
                        doneSignal.countDown();
                    }
                }, seconds, TimeUnit.SECONDS);
            }
            
            controller.startSoar(cycles);
            
            doneSignal.await();
            
            if (task != null)
                task.cancel(true);
            schexec.shutdown();
        }
        finally
        {
            if (!shutdown.get())
                Runtime.getRuntime().removeShutdownHook(shutdownHook);
        }
    }
}
