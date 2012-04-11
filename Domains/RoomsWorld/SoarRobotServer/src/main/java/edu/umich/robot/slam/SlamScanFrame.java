package edu.umich.robot.slam;

import java.awt.BorderLayout;

import javax.swing.JFrame;

import april.vis.VisCanvas;
import april.vis.VisWorld;

public class SlamScanFrame extends JFrame
{
    private static final long serialVersionUID = -7645618312825925496L;
    
    VisWorld world = new VisWorld();
    VisCanvas canvas = new VisCanvas(world);
    
    public SlamScanFrame(String title)
    {
        super(title);
        
        canvas.getViewManager().setInterfaceMode(3);
        setLayout(new BorderLayout());
        add(canvas, BorderLayout.CENTER);
        setSize(485, 485);

    }

    public VisWorld getWorld()
    {
        return world;
    }
}
