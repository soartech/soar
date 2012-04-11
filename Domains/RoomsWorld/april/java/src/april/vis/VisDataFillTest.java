package april.vis;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import javax.swing.*;

import april.jmat.*;
import april.jmat.geom.*;

/** Test VisDataFill. **/
public class VisDataFillTest extends VisCanvasEventAdapter
{
    JFrame jf;
    VisWorld vw = new VisWorld();
    VisCanvas vc = new VisCanvas(vw);
    JButton clearButton = new JButton();
    ArrayList<double[]> points = new ArrayList<double[]>();

    public static void main(String args[])
    {
        /*
          System.out.println(LinAlg.pointLeftOf(new double[] {0,0,0}, new double[] {1,1,0},
          new double[] {0,5,0}));
          System.out.println(LinAlg.pointLeftOf(new double[] {0,0,0}, new double[] {1,1,0},
          new double[] {5,0,0}));
        */
        new VisDataFillTest();
    }


    public VisDataFillTest()
    {
        jf = new JFrame("VisDataFillTest");
        jf.setLayout(new BorderLayout());
        jf.add(vc, BorderLayout.CENTER);

        clearButton = new JButton("Clear");
        clearButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                clear();
            }
	    });

        vc.addEventHandler(this);

        jf.add(clearButton, BorderLayout.SOUTH);

        jf.setSize(600,400);
        jf.setVisible(true);
    }

    public String getName()
    {
        return "VisDataFillTest";
    }

    void clear()
    {
        points = new ArrayList<double[]>();
        redraw();
    }

    public boolean mouseClicked(VisCanvas vc,  GRay3D ray, MouseEvent e)
    {
        if (e.getButton()!=1)
            return false;

        double xy[] = ray.intersectPlaneXY();
        points.add(xy);
        redraw();

        return true;
    }

    public void redraw()
    {
        VisWorld.Buffer vb = vw.getBuffer("test");

        vb.addBuffered(new VisData(new VisDataLineStyle(Color.blue, 2),
                                   new VisDataPointStyle(Color.blue, 6),
                                   new VisDataFillStyle(Color.red),
                                   points));

        vb.switchBuffer();
    }
}
