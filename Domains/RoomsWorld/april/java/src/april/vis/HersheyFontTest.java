package april.vis;

import javax.swing.*;
import java.awt.*;
import java.io.*;
import java.awt.event.*;

import april.jmat.*;
import april.jmat.geom.*;

public class HersheyFontTest
{
    JFrame jf;
    VisWorld vw = new VisWorld();
    VisCanvas vc = new VisCanvas(vw);
    HersheyFont hf;

    int numRows;
    int numCols;
    double gridSize;

    public static void main(String args[])
    {
        try {
            HersheyFont hf = new HersheyFont(args[0]);
            new HersheyFontTest(hf);
        } catch (IOException ex) {
            System.out.println("ex: "+ex);
        }
    }

    public HersheyFontTest(HersheyFont hf)
    {
        this.hf = hf;

        jf = new JFrame("HersheyFontTest");
        jf.setLayout(new BorderLayout());
        jf.add(vc, BorderLayout.CENTER);
        jf.setSize(800,600);
        jf.setVisible(true);

        VisWorld.Buffer vb = vw.getBuffer("characters");

        int hfSize = hf.getMaxCharacterHeight();

        vb.addBuffered(new VisChain(LinAlg.translate(0, 1),
                                    LinAlg.scale(1.0/hfSize, 1.0/hfSize, 1),
                                    new VisHersheyText(hf, "Hello, world.", new VisDataLineStyle(Color.blue, 2))));

        numCols = (int) (Math.sqrt(hf.cdatas.size()) + 1);
        numRows = hf.cdatas.size()/numCols;
        gridSize = 1.5; //hf.getMaxCharacterHeight();

        for (int idx = 0; idx < hf.cdatas.size(); idx++) {

            int rowx = idx % numCols;
            int rowy = idx / numCols;

            vb.addBuffered(new VisChain(LinAlg.translate(gridSize*rowx + gridSize/2, - gridSize*rowy - gridSize / 2, 0),
                                        LinAlg.scale(1.0/hfSize, 1.0/hfSize, 1),
                                        LinAlg.translate(-hf.getCharacterHorizontalOffset(idx), 0, 0),
                                        new VisHersheyText(hf, idx, new VisDataLineStyle(Color.blue, 2))));

            double x0 = rowx*gridSize, x1 = (rowx+1)*gridSize;
            double y0 = -rowy*gridSize, y1 = -(rowy+1)*gridSize;

            vb.addBuffered(new VisData(new VisDataLineStyle(new Color(200,200,200), 1, true),
                                       new double[] {x0, y0},
                                       new double[] {x0, y1},
                                       new double[] {x1, y1},
                                       new double[] {x1, y0}));
        }

        vb.switchBuffer();

        double cx = numCols*gridSize/2;
        double cy = -numRows*gridSize/2;
        vc.getViewManager().viewGoal.lookAt(new double[] {cx, cy, 2*Math.sqrt(cx*cx + cy*cy)},
                                            new double[] {cx, cy, 0},
                                            new double[] {0, 1, 0});

        vc.addEventHandler(new MyEventHandler());
    }

    class MyEventHandler extends VisCanvasEventAdapter
    {
        public String getName()
        {
            return "HersheyFontTest Hover";
        }

        public boolean mouseMoved(VisCanvas vc,  GRay3D ray, MouseEvent e)
        {
            double xy[] = ray.intersectPlaneXY();

            int row = (int) (-xy[1] / gridSize);
            int col = (int) (xy[0] / gridSize);

            int idx = row*numCols + col;

            VisWorld.Buffer vb = vw.getBuffer("overlay");

            if (row >=0 && row < numRows && col >=0 && col < numCols)
                vb.addBuffered(new VisText(VisText.ANCHOR.BOTTOM_RIGHT, "Character index: "+idx));
            vb.switchBuffer();

            return false;
        }

    }
}
