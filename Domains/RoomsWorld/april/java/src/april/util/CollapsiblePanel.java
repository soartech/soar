package april.util;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/** A *really* ugly collapsible panel. This should be made nicer sometime. **/
public class CollapsiblePanel extends JPanel implements ActionListener
{
    JPanel top = new JPanel();
    JButton button = new JButton("hide");
    boolean showing = true;

    JComponent c;

    public CollapsiblePanel(String name, JComponent c)
    {
        this.c = c;

        top.setLayout(new BorderLayout());
        top.add(button, BorderLayout.EAST);
        top.add(new JLabel(name), BorderLayout.WEST);
        setLayout(new BorderLayout());

        add(top, BorderLayout.NORTH);
        add(Box.createHorizontalStrut(10), BorderLayout.WEST);;
        add(c, BorderLayout.CENTER);

        button.addActionListener(this);
    }

    public void actionPerformed(ActionEvent e)
    {
        showing = !showing;

        if (showing) {
            button.setText("hide");
            add(c, BorderLayout.CENTER);
        } else {
            button.setText("show");
            remove(c);
        }

        invalidate();
    }
}

