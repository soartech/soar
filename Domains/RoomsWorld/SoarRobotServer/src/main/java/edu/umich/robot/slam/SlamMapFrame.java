package edu.umich.robot.slam;

import java.awt.BorderLayout;

import javax.swing.JFrame;

import april.util.ParameterGUI;
import april.vis.VisCanvas;
import april.vis.VisWorld;

public class SlamMapFrame extends JFrame
{
    private static final long serialVersionUID = -8185297380626933710L;

    VisWorld world = new VisWorld();
    VisCanvas canvas = new VisCanvas(world);
    ParameterGUI parameterGui = new ParameterGUI();

    public SlamMapFrame(String title)
    {
        super(title);
        
        parameterGui.addCheckBoxes("hypothesis", "Show Added Edges", true);
        parameterGui.addCheckBoxes("showscan", "Show Scan", false);
        parameterGui.addCheckBoxes("showcurpose", "Show Current Pose", true);
        parameterGui.addCheckBoxes("gTruth", "Show Ground Truth Movement", false);
        parameterGui.addCheckBoxes("pureOdom", "Show Noisy Odometry Movement", false);
        parameterGui.addCheckBoxes("slamPose", "Show SLAM Corrected Movement", false);

        canvas.getViewManager().setInterfaceMode(3);

        setLayout(new BorderLayout());
        add(canvas, BorderLayout.CENTER);
        add(parameterGui, BorderLayout.SOUTH);
        setSize(785, 785);
        setVisible(true);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    }

    public ParameterGUI getParameterGui()
    {
        return parameterGui;
    }

    public VisWorld getWorld()
    {
        return world;
    }

    public VisCanvas getCanvas()
    {
        return canvas;
    }
    
}
