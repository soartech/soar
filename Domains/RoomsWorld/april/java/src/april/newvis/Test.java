package april.newvis;

import java.awt.*;
import javax.swing.*;

public class Test
{
    public static void main(String args[])
    {
        JFrame jf = new JFrame("Test");
        VisCanvas vc = new VisCanvas();
        VisWorld vw1 = new VisWorld();
        VisWorld vw2 = new VisWorld();

        VisLayer layer1 = new VisLayer(vw1);
        VisLayer layer2 = new VisLayer(vw2);

        layer1.backgroundColor = Color.red;
        layer2.backgroundColor = Color.yellow;

        if (true) {
            VisWorld.Buffer vb = vw1.getBuffer("foo");
            vb.addBuffered(new VisBox(1, 1, 1, Color.gray));
            vb.switchBuffer();
        }

        vc.layerLayoutManager.addLayer(layer1, new double[] {0, 0, 0.5, 1.0 });
        vc.layerLayoutManager.addLayer(layer2, new double[] {0.5, 0, 0.5, 1.0 });

        jf.setLayout(new BorderLayout());
        jf.add(vc);
        jf.setSize(600,400);
        jf.setVisible(true);

    }
}
