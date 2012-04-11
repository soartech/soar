package april.viewer;

import java.awt.*;
import java.awt.geom.*;
import java.awt.event.*;
import java.awt.image.*;
import java.io.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.imageio.*;
import java.util.*;
import java.net.*;

import april.config.*;
import april.util.*;

import april.jmat.*;
import april.vis.*;

/** Generic robot viewer. **/
public class Viewer
{
    JFrame jf;
    VisWorld   vw;
    VisCanvas  vc;
    JPanel paramsPanel;

    Config     config;

    ArrayList<ViewObject> viewObjects = new ArrayList<ViewObject>();

    public static void main(String args[])
    {
        Config config = ConfigUtil.getDefaultConfig(args);

        Viewer v = new Viewer(config);
    }

    /**
     * Creates Viewer in an existing window.  This allows all of the viewer code to be used
     * inside another application (possibly in a seperate JPanel)
     *
     * @param _config     file containing configuration information
     * @param _jf     JFrame from parent application (window)
     */
    public Viewer(Config _config, JFrame _jf)
    {
        vw = new VisWorld();
        vc = new VisCanvas(vw);
        jf = _jf;

        initialize(_config);
    }

    /**
     * Creates default Viewer in new window (frame)
     *
     * @param _config     file containing configuration information
     */
    public Viewer(Config _config)
    {
        vw = new VisWorld();
        vc = new VisCanvas(vw);

        jf = new JFrame("Viewer");

        jf.setLayout(new BorderLayout());

        paramsPanel = new JPanel();
        paramsPanel.setLayout(new VFlowLayout());
        JSplitPane jsp = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, vc, paramsPanel);
        jsp.setDividerLocation(1.0);
        jsp.setResizeWeight(1.0);

        jf.add(jsp, BorderLayout.CENTER);
        jf.setSize(600,400);
        jf.setVisible(true);
        jf.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        jf.setSize(1000+2, 600+28);

        initialize(_config);
    }

    public void initialize(Config _config)
    {
        this.config = _config.getChild("viewer");

        //vw.getBuffer("__VISCANVAS_GROUND").addFront(new VisGrid());

        String viewobjects[] = config.requireStrings("viewobjects");

        for (String viewobject : viewobjects) {

            Config childConfig = config.getChild(viewobject);
            if (addObject(viewobject, childConfig) == null)
                System.exit(0);
        }
    }
    
    public ViewObject addObject(String name, Config objectConfig)
    {
        String className = objectConfig.requireString("class");
        try {
            Class<?> cls = Class.forName(className);
            ViewObject o = (ViewObject) cls.getConstructor(Viewer.class, String.class, Config.class).newInstance(this, name, objectConfig);
            viewObjects.add(o);
            //viewObjectsMap.put(name, o);
            return o;
        } catch (Exception ex) {
            System.out.println("Viewer: Unable to create "+name+": "+ex);
            return null;
        }
    }

    public VisWorld getVisWorld()
    {
        return vw;
    }

    public VisCanvas getVisCanvas()
    {
        return vc;
    }

    public void addParamPanel(JComponent c)
    {
        paramsPanel.add(c);
        paramsPanel.invalidate();
    }
}
