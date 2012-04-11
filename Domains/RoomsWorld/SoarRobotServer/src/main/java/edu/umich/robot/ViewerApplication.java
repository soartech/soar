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

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.IOException;
import java.util.prefs.Preferences;

import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.ToolTipManager;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileNameExtensionFilter;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import april.config.Config;
import april.config.ConfigFile;
import april.config.ConfigUtil;
import april.viewer.Viewer;
import edu.umich.robot.april.SoarViewRobot;
import edu.umich.robot.april.ViewTrajectory;
import edu.umich.robot.util.Configs;
import edu.umich.robot.util.swing.SwingTools;

/**
 * This is for read-only viewing of a simulation running on the machine.
 * Subscribes to a bunch of LCM channels and displays what it sees using the
 * configuration file it is given.
 * 
 * @author voigtjr@gmail.com
 */
public class ViewerApplication
{
    private static final Log logger = LogFactory.getLog(ViewerApplication.class);

    private final Preferences PREFERENCES = Preferences.userRoot().node("edu/umich/robot");

    private final JFrame frame = new JFrame();

    private final Viewer viewer;
    
    private final ViewerView viewerView;
    
    private Config promptForConfig()
    {
        JFileChooser fc = new JFileChooser("config");
        fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
        FileFilter filter = new FileNameExtensionFilter("Text Config File", "txt");
        fc.setFileFilter(filter);
        fc.setMultiSelectionEnabled(false);
        int ret = fc.showOpenDialog(frame);
        if (ret == JFileChooser.APPROVE_OPTION)
        {
            try
            {
                return new ConfigFile(fc.getSelectedFile().getAbsolutePath());
            }
            catch (IOException e)
            {
                logger.error(e.getMessage());
            }
        }
        return null;
    }
    
    public ViewerApplication(String[] args)
    {
        JPopupMenu.setDefaultLightWeightPopupEnabled(false);
        ToolTipManager.sharedInstance().setLightWeightPopupEnabled(false);
        
        Config config = (args.length > 0) ? ConfigUtil.getDefaultConfig(args) : promptForConfig();
        if (config == null)
            System.exit(1);

        setupViewerConfig(config);
        viewer = new Viewer(config);
        viewer.getVisCanvas().getViewManager().setInterfaceMode(3);

        frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        frame.addWindowListener(new WindowAdapter()
        {
            @Override
            public void windowClosed(WindowEvent e)
            {
                frame.dispose();

                try
                {
                    Thread.sleep(500);
                }
                catch (InterruptedException ignored)
                {
                }
                System.exit(0); // No way to shut down april threads
            }
        });
        frame.setLayout(new BorderLayout());

        viewerView = new ViewerView(viewer.getVisCanvas());
        
        // TODO SoarApril
        // viewer.getVisCanvas().setDrawGround(true);

        frame.add(viewerView, BorderLayout.CENTER);
        
        Preferences windowPrefs = getWindowPreferences();
        if (windowPrefs.get("x", null) != null)
        {
            frame.setBounds(
                    windowPrefs.getInt("x", 0), 
                    windowPrefs.getInt("y", 0), 
                    windowPrefs.getInt("width", 800), 
                    windowPrefs.getInt("height", 800));
        }
        else
        {
            frame.setBounds(
                    windowPrefs.getInt("x", 0), 
                    windowPrefs.getInt("y", 0), 
                    windowPrefs.getInt("width", 600), 
                    windowPrefs.getInt("height", 600));
            frame.setLocationRelativeTo(null); // center
        }

        frame.getRootPane().registerKeyboardAction(new ActionListener()
                {
                    public void actionPerformed(ActionEvent e)
                    {
                        frame.dispose();
                    }
                }, KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0),
                JComponent.WHEN_IN_FOCUSED_WINDOW);

        frame.pack();
        
        frame.setVisible(true);

        String[] splinters = config.getStrings("splinters", new String[0]);
        for (String s : splinters)
        {
            addViewRobot(s);
            addViewLidars(s);
            addViewWaypoints(s);
            
            // TODO SoarApril
            addViewTrajectory(s);
        }
    }
    
    private Preferences getWindowPreferences()
    {
        return PREFERENCES.node("window");
    }
    
    private void setupViewerConfig(Config config)
    {
        config.setStrings("viewer.viewobjects", new String[] { "obstacles", "walls", "areas" });
        
        config.setString("viewer.obstacles.class", "april.viewer.ViewObstaclesReadOnly");
        
        config.setString("viewer.walls.class", "april.viewer.ViewWalls");
        Application.addImageData(config, "viewer.walls.obstacles.");
        
//        config.setString("viewer.floor.class", "april.viewer.ViewFloor"); // need to add floor to viewobjects list
//        addImageData(config, "viewer.floor.obstacles.");

        config.setString("viewer.areas.class", "april.viewer.ViewAreaDescriptions");

//        config.setString("viewer.skybox.class", "april.viewer.ViewSkybox"); // need to add skybox to viewobjects list
//        config.setString("viewer.skybox.north_image", "north.jpg");
//        config.setString("viewer.skybox.south_image", "south.jpg");
//        config.setString("viewer.skybox.east_image", "east.jpg");
//        config.setString("viewer.skybox.west_image", "west.jpg");
//        config.setString("viewer.skybox.up_image", "top.jpg");
//        config.setString("viewer.skybox.down_image", "floor.jpg");
    }
    
    private void addPositionInfo(Config config, String prefix,
            double[] position, double[] rpy, int[] color)
    {
        config.setDoubles(prefix + "position", position);
        config.setDoubles(prefix + "rollpitchyaw_degrees", rpy);
        if (color != null)
            config.setInts(prefix + "color", color);
    }

    private SoarViewRobot addViewRobot(String name)
    {
        Config config = new Config();
        config.setString("class", "edu.umich.robot.april.SoarViewRobot");
        addPositionInfo(config, "avatar.", new double[] { 0, 0, 0 },
                new double[] { 0, 0, 0 }, null);

        Configs.toLog(logger, config);
        return (SoarViewRobot) viewer.addObject(name, config);
    }

    private void addViewLidars(String name)
    {
        Config config = new Config();
        config.setString("class", "april.viewer.ViewLaser");
        config.setString("pose", name);
        config.setStrings("channels", new String[] { "SIM_LIDAR_FRONT",
                "SICK_LIDAR_FRONT", "LIDAR_LOWRES", });
        addPositionInfo(config, "SIM_LIDAR_FRONT_" + name + ".", new double[] {
                0, 0, 0.4 }, new double[] { 0, 0, 0 }, new int[] { 1, 0, 0 });
        addPositionInfo(config, "SICK_LIDAR_FRONT_" + name + ".", new double[] {
                0, 0, 0.4 }, new double[] { 0, 0, 0 }, new int[] { 0, 1, 0 });
        addPositionInfo(config, "LIDAR_LOWRES_" + name + ".", new double[] { 0,
                0, 0.4 }, new double[] { 0, 0, 0 }, new int[] { 0, 0, 1 });

        Configs.toLog(logger, config);
        viewer.addObject(name + "lidars", config);
    }

    private void addViewWaypoints(String name)
    {
        Config config = new Config();
        config.setString("class", "april.viewer.ViewWaypoints");
        config.setString("channel", "WAYPOINTS_" + name);

        Configs.toLog(logger, config);
        viewer.addObject(name + "waypoints", config);
    }

    private ViewTrajectory addViewTrajectory(String name)
    {
        Config config = new Config();
        config.setString("class", "april.viewer.ViewTrajectory");
        config.setString("pose", name);

        Configs.toLog(logger, config);
        return (ViewTrajectory)viewer.addObject(name + "trajectory", config);
    }
    
    public static void main(final String[] args)
    {
        SwingTools.initializeLookAndFeel();
        SwingUtilities.invokeLater(new Runnable()
        {
            public void run()
            {
                new ViewerApplication(args);
            }
        });

    }
}
