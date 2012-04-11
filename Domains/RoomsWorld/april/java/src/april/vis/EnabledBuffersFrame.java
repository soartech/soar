package april.vis;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.util.*;

import april.util.*;

public class EnabledBuffersFrame extends JFrame
{
    JFrame buffersFrame;
    EnabledBuffersPanel panel;

    public EnabledBuffersFrame(String name, VisCanvas vc)
    {
        super(name);

        panel = new EnabledBuffersPanel(vc);
        add(panel, BorderLayout.CENTER);

        setSize(300, 500);
        setVisible(true);
    }

}
