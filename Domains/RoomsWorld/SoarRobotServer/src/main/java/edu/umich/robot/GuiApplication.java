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
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.geom.Point2D;
import java.io.File;
import java.io.IOException;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.util.Map;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.JSplitPane;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.KeyStroke;
import javax.swing.ToolTipManager;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileNameExtensionFilter;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import april.config.Config;
import april.config.ConfigFile;
import april.jmat.geom.GRay3D;
import april.viewer.Viewer;
import april.vis.VisCanvas;
import april.vis.VisCanvasEventAdapter;

import com.google.common.collect.Maps;
import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

import edu.umich.robot.actions.ActionManager;
import edu.umich.robot.actions.AddObjectAction;
import edu.umich.robot.actions.ConnectSuperdroidAction;
import edu.umich.robot.actions.CreateSplinterRobotAction;
import edu.umich.robot.actions.CreateSuperdroidRobotAction;
import edu.umich.robot.actions.DisableFollowAction;
import edu.umich.robot.actions.ExitAction;
import edu.umich.robot.actions.FollowPositionAction;
import edu.umich.robot.actions.FollowPositionAndThetaAction;
import edu.umich.robot.actions.MoveCameraAboveAction;
import edu.umich.robot.actions.MoveCameraBehindAction;
import edu.umich.robot.actions.ResetAction;
import edu.umich.robot.actions.ResetPreferencesAction;
import edu.umich.robot.actions.SaveMapAction;
import edu.umich.robot.actions.SimSpeedAction;
import edu.umich.robot.actions.SoarDataAction;
import edu.umich.robot.actions.SoarParametersAction;
import edu.umich.robot.actions.SoarStepAction;
import edu.umich.robot.actions.SoarToggleAction;
import edu.umich.robot.actions.TextMessageAction;
import edu.umich.robot.april.SoarViewRobot;
import edu.umich.robot.april.SoarViewRobot.FollowMode;
import edu.umich.robot.april.ViewTrajectory;
import edu.umich.robot.events.AfterResetEvent;
import edu.umich.robot.events.ControllerActivatedEvent;
import edu.umich.robot.events.ControllerDeactivatedEvent;
import edu.umich.robot.events.RobotAddedEvent;
import edu.umich.robot.events.RobotRemovedEvent;
import edu.umich.robot.gp.Gamepad;
import edu.umich.robot.util.Configs;
import edu.umich.robot.util.Pose;
import edu.umich.robot.util.events.RobotEvent;
import edu.umich.robot.util.events.RobotEventListener;

/**
 * <p>
 * Top-level class for applications that start with an interactive GUI. See also
 * Headless Application.
 * 
 * @author voigtjr@gmail.com
 */
public class GuiApplication
{
    private static final Log logger = LogFactory.getLog(GuiApplication.class);

    /**
     * <p>
     * Used primarily for window location and stuff like that.
     */
    private final Preferences PREFERENCES = Preferences.userRoot().node("edu/umich/robot");

    private final JFrame frame = new JFrame();

    /**
     * <p>
     * The main hub for the simulation components and events.
     */
    private final Controller controller;

    /**
     * <p>
     * The 3D view in to the simulation.
     */
    private final Viewer viewer;
    
    /**
     * <p>
     * True forces a reset of the various window settings and such that normally
     * persist across sessions.
     */
    private boolean resetPreferencesAtExit = false;
    
    /**
     * <p>
     * Container for the 3D view (viewer).
     */
    private final ViewerView viewerView;
    
    /**
     * <p>
     * The list of robots and controllers.
     */
    private final RobotsView robotsView;

    /**
     * <p>
     * The console text output for debugging feedback.
     */
    private final ConsoleView consoleView;
    
    /**
     * <p>
     * The interface for chatting with the robots.
     */
    private final ChatView chatView;
    
    /**
     * <p>
     * Viewer state saved so that robots can be removed from the simulation.
     * 
     * @author voigtjr
     */
    private static class RobotData
    {
        public RobotData(SoarViewRobot vr, ViewTrajectory vt)
        {
            this.vr = vr;
            this.vt = vt;
        }
        
        final SoarViewRobot vr;
        final ViewTrajectory vt;
    }

    /**
     * <p>
     * Maps robot names to robot data.
     */
    private final Map<String, RobotData> robotData = Maps.newConcurrentMap();
    
    /**
     * <p>
     * Container for the status bar at the bottom of the window.
     */
    private final StatusBar status;
    
    /**
     * <p>
     * When adding an object to the simulator, after selecting what object is to
     * be added, this gets filled in with that object type's name.
     */
    private String objectToAdd;
    
    /**
     * <p>
     * Manager and sort of container for the actions used in the GUI.
     */
    private ActionManager actionManager;
    
    /**
     * <p>
     * Firing up this application requires a configuration file. If no
     * configuration file is presented on the command line, this is called to
     * prompt the user to select a configuration file.
     * 
     * <p>
     * Future work should probably include some default instead of doing this.
     * 
     * @return The selected, loaded configuration file.
     */
    public static Config promptForConfig(Component parent)
    {
        
        System.out.println("CLASSPATH: " + System.getenv("CLASSPATH"));
        System.out.println("DYLD_LIBRARY_PATH: " + System.getenv("DYLD_LIBRARY_PATH"));
        System.out.println("LD_LIBRARY_PATH: " + System.getenv("LD_LIBRARY_PATH"));
        System.out.println("SOAR_HOME: " + System.getenv("SOAR_HOME"));
        System.out.println("java.library.path: " + System.getProperty("java.library.path"));
        
        JFileChooser fc = new JFileChooser(System.getProperty("user.dir"));
        fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
        FileFilter filter = new FileNameExtensionFilter("Text Config File", "txt");
        fc.setFileFilter(filter);
        fc.setMultiSelectionEnabled(false);
        int ret = fc.showOpenDialog(parent);
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
    
    /**
     * Entry point.
     * 
     * @param args Args from command line.
     */
    public GuiApplication(Config config)
    {
        // Heavyweight is not desirable but it is the only thing that will
        // render in front of the Viewer on all platforms. Blame OpenGL
        JPopupMenu.setDefaultLightWeightPopupEnabled(false);
        ToolTipManager.sharedInstance().setLightWeightPopupEnabled(false);
        
        // must have config
        //Config config = (args.length > 0) ? ConfigUtil.getDefaultConfig(args) : promptForConfig(frame);
        if (config == null)
            System.exit(1);

        // Add more stuff to the config file that doesn't change between runs.
        Application.setupSimulatorConfig(config);
        setupViewerConfig(config);
        
        Configs.toLog(logger, config);

        controller = new Controller(config, new Gamepad());
        controller.initializeGamepad();

        viewer = new Viewer(config, frame);
        // This puts us in full 3d mode by default. The Viewer GUI doesn't
        // reflect this in its right click drop-down, a bug.
        viewer.getVisCanvas().getViewManager().setInterfaceMode(3);

        controller.addListener(RobotAddedEvent.class, listener);
        controller.addListener(RobotRemovedEvent.class, listener);
        controller.addListener(AfterResetEvent.class, listener);
        controller.addListener(ControllerActivatedEvent.class, listener);
        controller.addListener(ControllerDeactivatedEvent.class, listener);
        
        actionManager = new ActionManager(this);
        initActions();
        
        frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        frame.addWindowListener(new WindowAdapter()
        {
            @Override
            public void windowClosed(WindowEvent e)
            {
                controller.shutdown();
                System.exit(0);
            }
        });
        frame.setLayout(new BorderLayout());

        JMenuBar menuBar = new JMenuBar();
        JMenu fileMenu = new JMenu("File");
        fileMenu.add(actionManager.getAction(CreateSplinterRobotAction.class));
        fileMenu.add(actionManager.getAction(CreateSuperdroidRobotAction.class));
        fileMenu.add(new JSeparator());
        fileMenu.add(actionManager.getAction(ConnectSuperdroidAction.class));
        fileMenu.add(new JSeparator());
        fileMenu.add(actionManager.getAction(ResetPreferencesAction.class));

        /*
        fileMenu.add(new JSeparator());
        fileMenu.add(actionManager.getAction(TextMessageAction.class));
        */
        
        fileMenu.add(new JSeparator());
        fileMenu.add(actionManager.getAction(SaveMapAction.class));
        fileMenu.add(new JSeparator());
        fileMenu.add(actionManager.getAction(ExitAction.class));
        menuBar.add(fileMenu);

        JMenu cameraMenu = new JMenu("Camera");
        cameraMenu.add(actionManager.getAction(DisableFollowAction.class));
        cameraMenu.add(actionManager.getAction(FollowPositionAction.class));
        cameraMenu.add(actionManager.getAction(FollowPositionAndThetaAction.class));
        cameraMenu.add(new JSeparator());
        cameraMenu.add(actionManager.getAction(MoveCameraBehindAction.class));
        cameraMenu.add(actionManager.getAction(MoveCameraAboveAction.class));
        menuBar.add(cameraMenu);
        
        JMenu objectMenu = new JMenu("Objects");
        boolean added = false;
        for (String objectName : controller.getObjectNames())
        {
            added = true;
            objectMenu.add(new AddObjectAction(this, objectName));
        }
        if (!added)
            objectMenu.add(new JLabel("No objects available"));
        menuBar.add(objectMenu);

        menuBar.revalidate();
        frame.setJMenuBar(menuBar);

        JToolBar toolBar = new JToolBar();
        toolBar.setFloatable(false);
        toolBar.setRollover(true);
        toolBar.add(actionManager.getAction(SoarParametersAction.class));
        toolBar.add(actionManager.getAction(SoarDataAction.class));
        toolBar.add(actionManager.getAction(ResetAction.class));
        toolBar.add(actionManager.getAction(SoarToggleAction.class));
        toolBar.add(actionManager.getAction(SoarStepAction.class));
        toolBar.add(actionManager.getAction(SimSpeedAction.class));
        frame.add(toolBar, BorderLayout.PAGE_START);

        viewerView = new ViewerView(viewer.getVisCanvas());
        robotsView = new RobotsView(this, actionManager);
        
        final JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, 
                viewerView, robotsView);
        splitPane.setDividerLocation(0.75);
        
        // TODO SoarApril
        /*
        viewer.addRobotSelectionChangedListener(robotsView);
        viewer.getVisCanvas().setDrawGround(true);
        */
        
        viewer.getVisCanvas().addEventHandler(new VisCanvasEventAdapter() {

            public String getName() {
                return "Place Object";
            }

            @Override
            public boolean mouseClicked(VisCanvas vc, GRay3D ray, MouseEvent e) {
                boolean ret = false;
                synchronized (GuiApplication.this)
                {
                    if (objectToAdd != null && controller != null)
                    {
                        controller.addObject(objectToAdd, ray.intersectPlaneXY());
                        objectToAdd = null;
                        ret = true;
                    }
                    else
                    {
                    	double[] click = ray.intersectPlaneXY();
                    	chatView.setClick(new Point2D.Double(click[0], click[1]));
                    }
                }
                status.setMessage(String.format("%3.1f,%3.1f", ray.intersectPlaneXY()[0], ray.intersectPlaneXY()[1]));
                return ret;
            }
        });
        
        consoleView = new ConsoleView();
        chatView = new ChatView(this);
        controller.getRadio().addRadioHandler(chatView);
        
        final JSplitPane bottomPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
        		consoleView, chatView);
        bottomPane.setDividerLocation(200);
        
        final JSplitPane splitPane2 = new JSplitPane(JSplitPane.VERTICAL_SPLIT, 
                splitPane, bottomPane); 
        splitPane2.setDividerLocation(0.75);
        
        frame.add(splitPane2, BorderLayout.CENTER);
        
        status = new StatusBar();
        frame.add(status, BorderLayout.SOUTH);

        /*
        frame.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e)
            {
                final Preferences windowPrefs = getWindowPreferences();
                final Rectangle r = frame.getBounds();
                if(frame.getExtendedState() == JFrame.NORMAL)
                {
                    windowPrefs.putInt("x", r.x);
                    windowPrefs.putInt("y", r.y);
                    windowPrefs.putInt("width", r.width);
                    windowPrefs.putInt("height", r.height);
                    windowPrefs.putInt("divider", splitPane.getDividerLocation());
                }
                
                exit();
            }});

        Preferences windowPrefs = getWindowPreferences();
        if (windowPrefs.get("x", null) != null)
        {
            frame.setBounds(
                    windowPrefs.getInt("x", 0), 
                    windowPrefs.getInt("y", 0), 
                    windowPrefs.getInt("width", 1200), 
                    windowPrefs.getInt("height", 900));
            splitPane.setDividerLocation(windowPrefs.getInt("divider", 500));
        }
        else
        {
            frame.setBounds(
                    windowPrefs.getInt("x", 0), 
                    windowPrefs.getInt("y", 0), 
                    windowPrefs.getInt("width", 1200), 
                    windowPrefs.getInt("height", 900));
            splitPane.setDividerLocation(0.75);
            frame.setLocationRelativeTo(null); // center
        }
        */
        
        frame.getRootPane().setBounds(0, 0, 1600, 1200);

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

        for (String s : config.getStrings("splinters", new String[0]))
        {
            double[] pos = config.getDoubles(s + ".position");
            if (pos == null)
            {
                logger.error("Splinter indexed in config file but no position defined: " + s);
                continue;
            }
            
            Pose pose = new Pose(pos);
            String prods = config.getString(s + ".productions");
            boolean collisions = config.getBoolean(s + ".wallCollisions", true);
            
            controller.createSplinterRobot(s, pose, collisions);
            boolean simulated = config.getBoolean(s + ".simulated", true);
            if (simulated)
                controller.createSimSplinter(s);
            else
                controller.createRealSplinter(s);
            controller.createSimLaser(s);
            if (prods != null)
            {
                controller.createSoarController(s, s, prods, config.getChild(s + ".properties"));
                PREFERENCES.put("lastProductions", prods);
            }
        }
        

        for (String s : config.getStrings("superdroids", new String[0]))
        {
            double[] pos = config.getDoubles(s + ".position");
            if (pos == null)
            {
                logger.error("Superdroid indexed in config file but no position defined: " + s);
                continue;
            }
            
            Pose pose = new Pose(pos);
            String prods = config.getString(s + ".productions");
            boolean collisions = config.getBoolean(s + ".wallCollisions", true);
            
            controller.createSuperdroidRobot(s, pose, collisions);
            boolean simulated = config.getBoolean(s + ".simulated", true);
            if (simulated)
                controller.createSimSuperdroid(s);
            else
            {
                try
                {
                    controller.createRealSuperdroid(s, "192.168.1.165", 3192);
                }
                catch (UnknownHostException e1)
                {
                    e1.printStackTrace();
                }
                catch (SocketException e1)
                {
                    e1.printStackTrace();
                }
            }
            controller.createSimLaser(s);
            if (prods != null)
            {
                // wait a sec
                try
                {
                    Thread.sleep(1000);
                }
                catch (InterruptedException ex)
                {
                }
                controller.createSoarController(s, s, prods, config.getChild(s + ".properties"));
                PREFERENCES.put("lastProductions", prods);
            }
        }
        
    }
    
    /**
     * <p>
     * Initialize all actions here, together, they all report to the
     * actionManager which stores them.
     */
    private void initActions()
    {
        new ResetAction(actionManager);
        new SaveMapAction(actionManager);
        new SoarDataAction(actionManager);
        new SoarParametersAction(actionManager);
        new SoarToggleAction(actionManager);
        new SoarStepAction(actionManager);
        new CreateSplinterRobotAction(actionManager);
        new CreateSuperdroidRobotAction(actionManager);
        new ConnectSuperdroidAction(actionManager);
        new ResetPreferencesAction(actionManager);
        new ExitAction(actionManager);
        new DisableFollowAction(actionManager);
        new FollowPositionAction(actionManager);
        new FollowPositionAndThetaAction(actionManager);
        new MoveCameraAboveAction(actionManager);
        new MoveCameraBehindAction(actionManager);
        new SimSpeedAction(actionManager);
        //new TextMessageAction(actionManager);
    }
    
    /**
     * <p>
     * Returns the controller instance so things like actions can use it.
     * 
     * @return The controller instance.
     */
    public Controller getController()
    {
        return controller;
    }
    
    /**
     * <p>
     * Get the selected robot, or null.
     * 
     * @return
     */
    private SoarViewRobot getRobot()
    {
        String name = controller.getSelectedRobotName();
        if (name == null)
            return null;

        return robotData.get(name).vr;
    }
    
    /**
     * <p>
     * Change the follow mode in the viewer. A robot must currently be selected.
     * 
     * @param mode
     *            New follow mode.
     */
    // TODO SoarApril
    public void setFollowMode(FollowMode mode)
    {
        SoarViewRobot vr = getRobot();
        if (vr == null)
            return;
        vr.setFollowMode(mode, viewer.getVisCanvas());
    }
    
    /**
     * <p>
     * Moves the camera to the robot using the specified mode. See
     * robot.moveCameraToRobot.
     * 
     * @param mode
     *            See robot.moveCameraToRobot
     */
    // TODO SoarApril
    public void snapCamera(int mode)
    {
        SoarViewRobot robot = getRobot();
        if (robot != null) 
            robot.moveCameraToRobot(mode, viewer.getVisCanvas());
    }

    public void dispose()
    {
        frame.dispose();
    }
    
    private void exit()
    {
        if (resetPreferencesAtExit)
        {
            try
            {
                PREFERENCES.removeNode();
            }
            catch (BackingStoreException e)
            {
                logger.error(e);
            }
        }
    }
    
    /**
     * <p>
     * Return persistent window preferences.
     * 
     * @return Persistent window preferences.
     */
    public Preferences getWindowPreferences()
    {
        return PREFERENCES.node("window");
    }

    /**
     * <p>
     * Pops up a window to create a new splinter robot to add to the simulation.
     */
    public void createSplinterRobotDialog()
    {
        final Pose pose = new Pose();
        
        FormLayout layout = new FormLayout(
                "right:pref, 4dlu, 30dlu, 4dlu, right:pref, 4dlu, 30dlu",
                "pref, 2dlu, pref, 2dlu, pref");

        layout.setRowGroups(new int[][] {{1, 3}});
        
        final JDialog dialog = new JDialog(frame, "Create Splinter Robot", true);
        dialog.setLayout(layout);
        final JTextField name = new JTextField();
        final JTextField x = new JTextField(Double.toString((pose.getX())));
        final JTextField y = new JTextField(Double.toString((pose.getY())));
        final JButton cancel = new JButton("Cancel");
        final JButton ok = new JButton("OK");

        CellConstraints cc = new CellConstraints();
        dialog.add(new JLabel("Name"), cc.xy(1, 1));
        dialog.add(name, cc.xyw(3, 1, 5));
        dialog.add(new JLabel("x"), cc.xy(1, 3));
        dialog.add(x, cc.xy(3, 3));
        dialog.add(new JLabel("y"), cc.xy(5, 3));
        dialog.add(y, cc.xy(7, 3));
        dialog.add(cancel, cc.xyw(1, 5, 3));
        dialog.add(ok, cc.xyw(5, 5, 3));

        x.addFocusListener(new FocusAdapter()
        {
            @Override
            public void focusLost(FocusEvent e)
            {
                try
                {
                    pose.setX(Double.parseDouble(x.getText()));
                }
                catch (NumberFormatException ex)
                {
                    x.setText(Double.toString(pose.getX()));
                }
            }
        });
        
        y.addFocusListener(new FocusAdapter()
        {
            @Override
            public void focusLost(FocusEvent e)
            {
                try
                {
                    pose.setY(Double.parseDouble(y.getText()));
                }
                catch (NumberFormatException ex)
                {
                    y.setText(Double.toString(pose.getX()));
                }
            }
        });
        
        final ActionListener okListener = new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                String robotName = name.getText().trim();
                if (robotName.isEmpty())
                {
                    logger.error("Create splinter: robot name empty");
                    return;
                }
                for (char c : robotName.toCharArray())
                    if (!Character.isDigit(c) && !Character.isLetter(c))
                    {
                        logger.error("Create splinter: illegal robot name");
                        return;
                    }

                controller.createSplinterRobot(robotName, pose, true);
                controller.createSimSplinter(robotName);
                controller.createSimLaser(robotName);
                dialog.dispose();
            }
        };
        name.addActionListener(okListener);
        x.addActionListener(okListener);
        y.addActionListener(okListener);
        ok.addActionListener(okListener);

        ActionListener cancelAction = new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                dialog.dispose();
            }
        };
        cancel.addActionListener(cancelAction);
        dialog.getRootPane().registerKeyboardAction(cancelAction, 
                KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), 
                JComponent.WHEN_IN_FOCUSED_WINDOW);
        
        dialog.setLocationRelativeTo(frame);
        dialog.pack();
        dialog.setVisible(true);
    }

    public void createSuperdroidRobotDialog()
    {
        final Pose pose = new Pose();
        
        FormLayout layout = new FormLayout(
                "right:pref, 4dlu, 30dlu, 4dlu, right:pref, 4dlu, 30dlu",
                "pref, 2dlu, pref, 2dlu, pref");

        layout.setRowGroups(new int[][] {{1, 3}});
        
        final JDialog dialog = new JDialog(frame, "Create Superdroid Robot", true);
        dialog.setLayout(layout);
        final JTextField name = new JTextField();
        final JTextField x = new JTextField(Double.toString((pose.getX())));
        final JTextField y = new JTextField(Double.toString((pose.getY())));
        final JButton cancel = new JButton("Cancel");
        final JButton ok = new JButton("OK");

        CellConstraints cc = new CellConstraints();
        dialog.add(new JLabel("Name"), cc.xy(1, 1));
        dialog.add(name, cc.xyw(3, 1, 5));
        dialog.add(new JLabel("x"), cc.xy(1, 3));
        dialog.add(x, cc.xy(3, 3));
        dialog.add(new JLabel("y"), cc.xy(5, 3));
        dialog.add(y, cc.xy(7, 3));
        dialog.add(cancel, cc.xyw(1, 5, 3));
        dialog.add(ok, cc.xyw(5, 5, 3));

        x.addFocusListener(new FocusAdapter()
        {
            @Override
            public void focusLost(FocusEvent e)
            {
                try
                {
                    pose.setX(Double.parseDouble(x.getText()));
                }
                catch (NumberFormatException ex)
                {
                    x.setText(Double.toString(pose.getX()));
                }
            }
        });
        
        y.addFocusListener(new FocusAdapter()
        {
            @Override
            public void focusLost(FocusEvent e)
            {
                try
                {
                    pose.setY(Double.parseDouble(y.getText()));
                }
                catch (NumberFormatException ex)
                {
                    y.setText(Double.toString(pose.getX()));
                }
            }
        });
        
        final ActionListener okListener = new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                String robotName = name.getText().trim();
                if (robotName.isEmpty())
                {
                    logger.error("Create Superdroid: robot name empty");
                    return;
                }
                for (char c : robotName.toCharArray())
                    if (!Character.isDigit(c) && !Character.isLetter(c))
                    {
                        logger.error("Create Superdroid: illegal robot name");
                        return;
                    }

                controller.createSuperdroidRobot(robotName, pose, true);
                controller.createSimSuperdroid(robotName);
                dialog.dispose();
            }
        };
        name.addActionListener(okListener);
        x.addActionListener(okListener);
        y.addActionListener(okListener);
        ok.addActionListener(okListener);

        ActionListener cancelAction = new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                dialog.dispose();
            }
        };
        cancel.addActionListener(cancelAction);
        dialog.getRootPane().registerKeyboardAction(cancelAction, 
                KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), 
                JComponent.WHEN_IN_FOCUSED_WINDOW);
        
        dialog.setLocationRelativeTo(frame);
        dialog.pack();
        dialog.setVisible(true);
    }

    /**
     * <p>
     * Responds to events coming from the controller.
     */
    private final RobotEventListener listener = new RobotEventListener()
    {
        public void onEvent(RobotEvent event)
        {
            if (event instanceof RobotAddedEvent)
            {
                RobotAddedEvent e = (RobotAddedEvent) event;

                SoarViewRobot vr = addViewRobot(e.getRobot().getName(), e.getRobot().getType());
                addViewLidars(e.getRobot().getName());
                addViewWaypoints(e.getRobot().getName());
                ViewTrajectory vt = addViewTrajectory(e.getRobot().getName());

                robotData.put(e.getRobot().getName(), new RobotData(vr, vt));
            }
            
            // TODO SoarApril
            /*
            else if (event instanceof AfterResetEvent)
            {
                for (RobotData rd : robotData.values())
                    rd.vt.clear();
            }
            */
            
            // there are a few unhandled actions

            actionManager.updateActions();
        }
    };

    /**
     * <p>
     * Elaborates unchanging stuff for the viewer. This stuff used to be in the
     * config files but it doesn't change so it was moved here.
     * 
     * @param config
     *            The config file to elaborate.
     */
    private void setupViewerConfig(Config config)
    {
        config.setStrings("viewer.viewobjects", new String[] { "obstacles", "walls", "areas" });
        
        config.setString("viewer.obstacles.class", "edu.umich.robot.april.ViewObstaclesReadOnly");
        
        config.setString("viewer.walls.class", "edu.umich.robot.april.ViewWalls");
        Application.addImageData(config, "viewer.walls.obstacles.");
        
        // The following code enables other viewing options.
        
//        config.setString("viewer.floor.class", "edu.umich.robot.april.ViewFloor"); // need to add floor to viewobjects list
//        addImageData(config, "viewer.floor.obstacles.");

        config.setString("viewer.areas.class", "edu.umich.robot.april.ViewAreaDescriptions");

        config.setString("viewer.skybox.class", "edu.umich.robot.april.ViewSkybox"); // need to add skybox to viewobjects list
        config.setString("viewer.skybox.north_image", "../common/north.jpg");
        config.setString("viewer.skybox.south_image", "../common/south.jpg");
        config.setString("viewer.skybox.east_image", "../common/east.jpg");
        config.setString("viewer.skybox.west_image", "../common/west.jpg");
        config.setString("viewer.skybox.up_image", "../common/top.jpg");
        config.setString("viewer.skybox.down_image", "../common/floor.jpg");
    }
    
    /**
     * <p>
     * Adds position information to the config appended to the passed prefix.
     * Specifically adds prefix.position, prefix.rollpitchyaw_degrees, and
     * prefix.color.
     * 
     * @param config
     *            The config to elaborate.
     * @param prefix
     *            The prefix to add the position to.
     * @param position
     *            The position to add.
     * @param rpy
     *            Roll pitch yaw to add.
     * @param color
     *            Color to add, or null to skip.
     */
    private void addPositionInfo(Config config, String prefix,
            double[] position, double[] rpy, int[] color)
    {
        config.setDoubles(prefix + "position", position);
        config.setDoubles(prefix + "rollpitchyaw_degrees", rpy);
        if (color != null)
            config.setInts(prefix + "color", color);
    }

    /**
     * <p>
     * Add a robot to the viewer, by name.
     * 
     * @param name
     *            Robot name to add.
     * 
     * @return The viewer object representing the robot.
     */
    private SoarViewRobot addViewRobot(String name, RobotType type)
    {
        Config config = new Config();
        config.setString("class", "edu.umich.robot.april.SoarViewRobot");
        addPositionInfo(config, "avatar.", new double[] { 0, 0, 0 },
                new double[] { 0, 0, 0 }, null);

        if (type == RobotType.SUPERDROID)
            config.setBoolean("model4", true);
        
        Configs.toLog(logger, config);
        return (SoarViewRobot) viewer.addObject(name, config);
    }

    /**
     * <p>
     * Add lidar sensor feedback to the viewer by robot name.
     * 
     * @param name
     *            Robot name to add sensor for.
     */
    private void addViewLidars(String name)
    {
        Config config = new Config();
        config.setString("class", "april.viewer.ViewLaser");
        config.setString("pose", name);
        config.setStrings("channels", new String[] { "SIM_LIDAR_FRONT",
                "SICK_LIDAR_FRONT", "LIDAR_LOWRES", "URG_RANGE" });
        addPositionInfo(config, "SIM_LIDAR_FRONT_" + name + ".", new double[] {
                0, 0, 0.4 }, new double[] { 0, 0, 0 }, new int[] { 1, 0, 0 });
        addPositionInfo(config, "SICK_LIDAR_FRONT_" + name + ".", new double[] {
                0, 0, 0.4 }, new double[] { 0, 0, 0 }, new int[] { 0, 1, 0 });
        addPositionInfo(config, "LIDAR_LOWRES_" + name + ".", new double[] { 0,
                0, 0.4 }, new double[] { 0, 0, 0 }, new int[] { 0, 0, 1 });
        addPositionInfo(config, "URG_RANGE_" + name + ".", new double[] { 0,
                0, 0.4 }, new double[] { 0, 0, 0 }, new int[] { 0, 1, 0 });

        Configs.toLog(logger, config);
        viewer.addObject(name + "lidars", config);
    }

    /**
     * <p>
     * Add waypoint feedback to viewer by robot name.
     * 
     * @param name
     *            Robot name to add waypoint feedback for.
     */
    private void addViewWaypoints(String name)
    {
        Config config = new Config();
        config.setString("class", "edu.umich.robot.april.ViewWaypoints");
        config.setString("channel", "WAYPOINTS_" + name);

        Configs.toLog(logger, config);
        
        viewer.addObject(name + "waypoints", config);
    }

    /**
     * <p>
     * Add view trajectory to viewer by robot name.
     * 
     * @param name
     *            The robot name to add trajectory for.
     * @return The viewer object representing the trajectory.
     */
    private ViewTrajectory addViewTrajectory(String name)
    {
        Config config = new Config();
        config.setString("class", "edu.umich.robot.april.ViewTrajectory");
        config.setString("pose", name);

        Configs.toLog(logger, config);
        return (ViewTrajectory)viewer.addObject(name + "trajectory", config);
    }

    /**
     * <p>
     * Sets the object type to add at the location of the next click in the
     * viewer.
     * 
     * @param name
     *            Object type to add at next click.
     */
    public void addObjectOnNextClick(String name)
    {
        synchronized (this)
        {
            objectToAdd = name;
        }
        status.setMessage("Click on map to place object.");
    }
    
    /**
     * <p>
     * Change the message in the status bar.
     * 
     * @param message
     *            The new message.
     */
    public void setStatusBarMessage(String message)
    {
        status.setMessage(message);
    }

    public Component getTopLevelAncestor()
    {
        return frame;
    }

    /**
     * <p>
     * Request that window preferences be reset on exit.
     * 
     * @param b True to request reset.
     */
    public void setResetPreferencesAtExit(boolean b)
    {
        resetPreferencesAtExit = b;
    }

    public Preferences getPreferences()
    {
        return PREFERENCES;
    }

    public void connectSuperdroidRobotDialog()
    {
        final int defaultPort = 3192;
        
        FormLayout layout = new FormLayout(
                "right:pref, 4dlu, 35dlu, 4dlu, 35dlu",
                "pref, 2dlu, pref, 2dlu, pref, 2dlu, pref");

        final JDialog dialog = new JDialog(frame, "Connect to Superdroid", true);
        dialog.setLayout(layout);
        final JTextField namefield = new JTextField("charlie");
        final JTextField hostfield = new JTextField("192.168.1.165");
        final JTextField portfield = new JTextField(Integer.toString(defaultPort));
        final JButton cancel = new JButton("Cancel");
        final JButton ok = new JButton("OK");

        CellConstraints cc = new CellConstraints();
        dialog.add(new JLabel("Name"), cc.xy(1, 1));
        dialog.add(namefield, cc.xyw(3, 1, 3));
        dialog.add(new JLabel("Host"), cc.xy(1, 3));
        dialog.add(hostfield, cc.xyw(3, 3, 3));
        dialog.add(new JLabel("Port"), cc.xy(1, 5));
        dialog.add(portfield, cc.xyw(3, 5, 3));
        dialog.add(cancel, cc.xy(3, 7));
        dialog.add(ok, cc.xy(5, 7));

        portfield.addFocusListener(new FocusAdapter()
        {
            @Override
            public void focusLost(FocusEvent e)
            {
                int p = defaultPort;
                try
                {
                    p = Integer.parseInt(portfield.getText());
                    if (p < 1)
                        p = 1;
                    if (p > 65535)
                        p = 65535;
                }
                catch (NumberFormatException ex)
                {
                }
                portfield.setText(Integer.toString(p));
            }
        });
        
        final ActionListener okListener = new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                String robotName = namefield.getText().trim();
                if (robotName.isEmpty())
                {
                    logger.error("Connect Superdroid: robot name empty");
                    return;
                }
                
                for (char c : robotName.toCharArray())
                {
                    if (!Character.isDigit(c) && !Character.isLetter(c))
                    {
                        logger.error("Create Superdroid: illegal robot name");
                        return;
                    }
                }
                
                try
                {
                    controller.createRealSuperdroid(robotName, hostfield.getText(), Integer.valueOf(portfield.getText()));
                }
                catch (UnknownHostException ex)
                {
                    ex.printStackTrace();
                    logger.error("Connect Superdroid: " + ex);
                }
                catch (SocketException ex)
                {
                    ex.printStackTrace();
                    logger.error("Connect Superdroid: " + ex);
                }
                dialog.dispose();
            }
        };
        
        namefield.addActionListener(okListener);
        hostfield.addActionListener(okListener);
        portfield.addActionListener(okListener);
        ok.addActionListener(okListener);

        ActionListener cancelAction = new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                dialog.dispose();
            }
        };
        cancel.addActionListener(cancelAction);
        
        dialog.getRootPane().registerKeyboardAction(cancelAction, 
                KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), 
                JComponent.WHEN_IN_FOCUSED_WINDOW);
        
        dialog.setLocationRelativeTo(frame);
        dialog.pack();
        dialog.setVisible(true);
    }
}
