package april.vis;

import java.awt.*;
import java.awt.geom.*;
import java.awt.event.*;
import java.awt.image.*;
import java.io.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.imageio.*;
import java.util.*;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;

import april.jmat.geom.*;
import april.jmat.*;

class VisCanvasPopupMenu extends JPopupMenu
{
    VisCanvas vc;
    JCheckBoxMenuItem fpsItem = new JCheckBoxMenuItem("Show FPS", false);

    double fpsRates[] = new double[] {1, 2, 5, 15, 20, 30, 60};
    int fpsRateDefaultIndex = 6;
    ButtonGroup fpsGroup = new ButtonGroup();
    JMenu  fpsMenu;
    JRadioButtonMenuItem fpsItems[];
    JCheckBoxMenuItem orthoItem;
    JCheckBoxMenuItem movieItem = new JCheckBoxMenuItem("Record Movie", false);

    String interfaceModes[] = { "1.9D", "2D", "2.5D", "3D"};
    int interfaceModeDefaultIndex = 2;
    JRadioButtonMenuItem interfaceModeItems[];
    JMenu interfaceModeMenu;
    ButtonGroup interfaceModeGroup = new ButtonGroup();

    JCheckBoxMenuItem gridEnabledItem;
    JCheckBoxMenuItem groundEnabledItem;
    JCheckBoxMenuItem originEnabledItem;
    JCheckBoxMenuItem showLookAt;

    JMenuItem helpItem;

    JMenu buffersMenu = new JMenu("Select Buffers", true);

    public VisCanvasPopupMenu(VisCanvas vc)
    {
        super("Menu");

        this.vc=vc;

        JMenuItem jmi;

        ///////////// camera control
        jmi=new JMenuItem("Reset camera");
        jmi.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                resetCamera();
            }
        });
        add(jmi);

        jmi=new JMenuItem("Snap to nearest axis");
        jmi.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                snapTo();
            }
        });
        add(jmi);

        orthoItem=new JCheckBoxMenuItem("Orthographic projection");
        orthoItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                orthographic();
            }
        });
        add(orthoItem);

        interfaceModeMenu = new JMenu("Interface Mode");
        interfaceModeItems = new JRadioButtonMenuItem[interfaceModes.length];

        for (int i = 0; i < interfaceModes.length; i++) {
            interfaceModeItems[i] = new JRadioButtonMenuItem(""+interfaceModes[i]);
            interfaceModeGroup.add(interfaceModeItems[i]);
            interfaceModeItems[i].addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e)
                {
                    updateInterfaceMode();
                }
            });
            interfaceModeMenu.add(interfaceModeItems[i]);
        }
        interfaceModeItems[interfaceModeDefaultIndex].setSelected(true);
        add(interfaceModeMenu);

        ///////////// recording
        addSeparator();

        jmi=new JMenuItem("Save image (.png)");
        jmi.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                saveImage();
            }
        });
        add(jmi);

        jmi=new JMenuItem("Save scene (.vsc)");
        jmi.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                saveScene();
            }
        });
        add(jmi);

        movieItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                makeMovie();
            }
        });
        add(movieItem);

        jmi=new JMenuItem("Toggle background color");
        jmi.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                toggleBackgroundColor();
            }
        });
        add(jmi);


        ////////////// fps control
        addSeparator();

        add(fpsItem);

        // request a redraw so that we'll start showing fps label
        fpsItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                requestRedraw();
            }
	    });

        fpsMenu = new JMenu("Maximum FPS");
        fpsItems = new JRadioButtonMenuItem[fpsRates.length];

        for (int i = 0; i < fpsRates.length; i++)
        {
            fpsItems[i] = new JRadioButtonMenuItem(""+fpsRates[i]);
            fpsGroup.add(fpsItems[i]);
            fpsItems[i].addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e)
                {
                    double fps = Double.parseDouble(((JMenuItem) e.getSource()).getText());
                    setTargetFPS((JMenuItem) e.getSource());
                }
            }
                );
            fpsMenu.add(fpsItems[i]);
        }

        fpsItems[fpsRateDefaultIndex].setSelected(true);
        vc.setTargetFPS(fpsRates[fpsRateDefaultIndex]);

        add(fpsMenu);

        ////////////// grid control
        addSeparator();
        JMenu groundMenu = new JMenu("Ground/Grid");
        gridEnabledItem = new JCheckBoxMenuItem("Enable XY Grid");
        gridEnabledItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e)
            {
                updateGround();
            }
	    });

        groundMenu.add(gridEnabledItem);
        groundEnabledItem = new JCheckBoxMenuItem("Draw Ground");
        groundMenu.add(groundEnabledItem);
        groundEnabledItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e)
            {
                updateGround();
            }
	    });

        originEnabledItem = new JCheckBoxMenuItem("Draw Origin");
        groundMenu.add(originEnabledItem);
        originEnabledItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e)
            {
                updateGround();
            }
	    });

        add(groundMenu);

        add(buffersMenu);

        addSeparator();

        showLookAt = new JCheckBoxMenuItem("Show camera parameters");
        add(showLookAt);

        addSeparator();

        helpItem = new JMenuItem("Help");
        helpItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e)
            {
                doHelp();
            }
	    });
        add(helpItem);

        updateGround();

        setLightWeightPopupEnabled(false);
    }

    EnabledBuffersFrame ebf;

    void makeBuffersWindow()
    {
        if (ebf == null) {
            String name = "VisCanvas";

            Component parent = vc;
            while (parent.getParent() != null) {
                parent = parent.getParent();
            }

            if (parent instanceof JFrame)
                name = ((JFrame) parent).getTitle();

            ebf = new EnabledBuffersFrame(name+" Buffers", vc);
        }

        ebf.setVisible(true);
    }

    void resetCamera()
    {
        vc.defaultEventHandler.stopAllAnimation();
        findFollow();
    }

    void updateGround()
    {
        VisWorld.Buffer vb = vc.world.getBuffer("__VISCANVAS_GROUND");
        vb.setDrawOrder(-10000);
        VisGrid vg = new VisGrid();
        vg.drawGrid = gridEnabledItem.isSelected();
        vg.drawGround = groundEnabledItem.isSelected();
        vb.addBuffered(new VisDepthTest(false, vg));

        if (originEnabledItem.isSelected())
            vb.addBuffered(new VisData(new VisDataPointStyle(Color.gray, 4),
                                       new double[3]));
        vb.switchBuffer();
    }

    void orthographic()
    {
        vc.getViewManager().viewGoal.perspectiveness = orthoItem.getState() ? 0 : 1;
        vc.draw();
    }

    void setTargetFPS(JMenuItem item)
    {
        double fps = 1000;

        for (int i = 0; i < fpsRates.length; i++) {
            if (item == fpsItems[i])
                fps = fpsRates[i];
        }

        vc.setTargetFPS(fps);
    }

    void requestRedraw()
    {
        vc.draw();
    }

    void updateInterfaceMode()
    {
        for (int i = 0; i < interfaceModeItems.length; i++) {
            if (interfaceModeItems[i].isSelected())  {
                double v = Double.parseDouble(interfaceModeItems[i].getText());
                vc.getViewManager().setInterfaceMode(v);
                return;
            }
        }

        assert(false);
    }

    void makeMovie()
    {
        try {

            if (movieItem.isSelected()) {
                Calendar c = new GregorianCalendar();
                String s = String.format("%4d%02d%02d_%02d%02d%02d_%03d.ppms.gz", c.get(Calendar.YEAR),
                                         c.get(Calendar.MONTH)+1,
                                         c.get(Calendar.DAY_OF_MONTH),
                                         c.get(Calendar.HOUR_OF_DAY),
                                         c.get(Calendar.MINUTE),
                                         c.get(Calendar.SECOND),
                                         c.get(Calendar.MILLISECOND)
                    );

                vc.movieBegin(s);
            } else {
                vc.movieEnd();
            }
        } catch (IOException ex) {
            System.out.println("Making movie error: "+ex);
        }
    }

    // all we can do is enqueue the request for the next draw.
    public void saveImage()
    {
        Calendar c = new GregorianCalendar();

        String s = String.format("%4d%02d%02d_%02d%02d%02d_%03d", c.get(Calendar.YEAR),
                                 c.get(Calendar.MONTH)+1,
                                 c.get(Calendar.DAY_OF_MONTH),
                                 c.get(Calendar.HOUR_OF_DAY),
                                 c.get(Calendar.MINUTE),
                                 c.get(Calendar.SECOND),
                                 c.get(Calendar.MILLISECOND)
            );

        String path = "p"+s+".png";

        vc.writeScreenShot(new File(path), "png");
    }

    public void saveScene()
    {
        Calendar c = new GregorianCalendar();

        String s = String.format("%4d%02d%02d_%02d%02d%02d_%03d", c.get(Calendar.YEAR),
                                 c.get(Calendar.MONTH)+1,
                                 c.get(Calendar.DAY_OF_MONTH),
                                 c.get(Calendar.HOUR_OF_DAY),
                                 c.get(Calendar.MINUTE),
                                 c.get(Calendar.SECOND),
                                 c.get(Calendar.MILLISECOND)
            );

        String path = "s"+s+".vsc";

        VisSerialize.writeVCToFile(vc, path);
    }

    public void toggleBackgroundColor()
    {
        if (vc.getBackground() == Color.black)
        {
            vc.setBackground(Color.white);
            return;
        }
        vc.setBackground(Color.black);
        vc.draw();
    }

    double[] snapTo(double v[])
    {
        int maxidx = 0;
        double maxvalue = 0;

        for (int i = 0; i < v.length; i++) {
            if (Math.abs(v[i]) > Math.abs(maxvalue)) {
                maxidx = i;
                maxvalue = v[i];
            }
            v[i] = 0;
        }

        double w[] = new double[v.length];
        w[maxidx] = maxvalue > 0 ? 1 : -1;
        return w;
    }

    public void snapTo()
    {
        VisView view = vc.getLastView();

        double lookvec[] = LinAlg.subtract(view.lookAt, view.eye);
        double lookdist = LinAlg.distance(view.lookAt, view.eye);

        lookvec = snapTo(lookvec);

        vc.getViewManager().viewGoal.lookAt(LinAlg.subtract(view.lookAt, LinAlg.scale(lookvec, lookdist)),
                                            view.lookAt,
                                            snapTo(view.up));
    }

    public void findFollow()
    {
        VisViewManager viewManager = vc.getViewManager();

        viewManager.viewGoal.lookAt(LinAlg.add(viewManager.followPos, new double[] {0, 0, 100}),
                                    viewManager.followPos,
                                    new double[] {0, 1, 0});
    }

    JFrame helpFrame;
    JTextPane helpTextPane;

    public void doHelp()
    {
        HelpOutput houts = new HelpOutput();

        for (VisCanvasEventHandler handler: vc.eventHandlers) {
            handler.doHelp(houts);
        }

        if (helpFrame == null) {
            helpFrame = new JFrame("Vis Help");
            helpTextPane = new JTextPane();
            helpTextPane.setEditable(false);

            helpFrame.add(new JScrollPane(helpTextPane), BorderLayout.CENTER);

            helpFrame.setSize(400,600);
        }

        helpFrame.setVisible(true);
        helpTextPane.setFont(new Font("Monospaced", Font.PLAIN, 12));
        helpTextPane.setText(houts.toString());
    }

    public void setVisible(boolean b)
    {
        buffersMenu.removeAll();

        ArrayList<VisWorld.Buffer> buffers = new ArrayList<VisWorld.Buffer>();
        VisWorld vw = vc.getWorld();
        for (VisWorld.Buffer vb : vw.buffers) {
            buffers.add(vb);
        }

        Collections.sort(buffers, new Comparator<VisWorld.Buffer>() {
                public int compare(VisWorld.Buffer a, VisWorld.Buffer b) {
                    return a.name.compareTo(b.name);
                }
            });

        JMenuItem enableAllItem = new JMenuItem("Enable all");
        enableAllItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e)
            {
                for (VisWorld.Buffer vb : vc.getWorld().buffers)
                    vc.getViewManager().setBufferEnabled(vb.name, true);
                vc.draw();
            }
	    });

        JMenuItem tearOffItem = new JMenuItem("Detach...");
        tearOffItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e)
            {
                makeBuffersWindow();
            }
	    });

        buffersMenu.add(tearOffItem);

        buffersMenu.add(enableAllItem);

        JMenuItem disableAllItem = new JMenuItem("Disable all");
        disableAllItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e)
            {
                for (VisWorld.Buffer vb : vc.getWorld().buffers)
                    vc.getViewManager().setBufferEnabled(vb.name, false);
                vc.draw();
            }
	    });
        buffersMenu.add(disableAllItem);

        buffersMenu.add(new JSeparator());

        for (VisWorld.Buffer buffer : buffers) {
            JCheckBoxMenuItem jcb = new JCheckBoxMenuItem(buffer.name, vc.getViewManager().isBufferEnabled(buffer.name));

            jcb.addActionListener(new VisibleActionListener(jcb, buffer));
            buffersMenu.add(jcb);
        }

        super.setVisible(b);
    }

    class VisibleActionListener implements ActionListener
    {
        JCheckBoxMenuItem jcb;
        VisWorld.Buffer vb;

        VisibleActionListener(JCheckBoxMenuItem jcb, VisWorld.Buffer vb)
        {
            this.jcb = jcb;
            this.vb = vb;
        }

        public void actionPerformed(ActionEvent e) {
            vc.getViewManager().setBufferEnabled(vb.name, jcb.getState());
            VisCanvasPopupMenu.this.vc.draw();
        }
    }
}
