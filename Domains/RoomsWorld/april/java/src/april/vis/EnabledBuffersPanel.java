package april.vis;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.util.*;

import april.util.*;

public class EnabledBuffersPanel extends JPanel implements VisViewListener, VisWorldListener
{
    VisCanvas vc;

    JButton enableAllButton = new JButton("Enable all");
    JButton disableAllButton = new JButton("Disable all");
    //    JButton refreshListButton = new JButton("Refresh list");

    JPanel checkboxPanel;

    HashMap<String, JCheckBox> checkboxMap = new HashMap<String, JCheckBox>();

    public EnabledBuffersPanel(VisCanvas vc)
    {
        this.vc = vc;

        JPanel buttonPanel = new JPanel();
        buttonPanel.setLayout(new GridLayout(1,3));
        buttonPanel.add(enableAllButton);
        buttonPanel.add(disableAllButton);

        enableAllButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e)
            {
                VisCanvas vc = EnabledBuffersPanel.this.vc;

                for (VisWorld.Buffer vb : vc.getWorld().buffers) {
                    vc.getViewManager().setBufferEnabled(vb.name, true);
                    checkboxMap.get(vb.name).setSelected(true);
                }

                vc.draw();
            }
	    });

        disableAllButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e)
            {
                VisCanvas vc = EnabledBuffersPanel.this.vc;

                for (VisWorld.Buffer vb : vc.getWorld().buffers) {
                    vc.getViewManager().setBufferEnabled(vb.name, false);
                    checkboxMap.get(vb.name).setSelected(false);
                }
                vc.draw();
            }
	    });

        checkboxPanel = new JPanel();
        checkboxPanel.setLayout(new VFlowLayout());

        setLayout(new BorderLayout());
        add(checkboxPanel, BorderLayout.CENTER);
        add(buttonPanel, BorderLayout.SOUTH);

        buffersChanged();

        vc.getWorld().addListener(this);
    }

    public void viewBufferEnabledChanged(VisContext vc, String bufferName, boolean enabled)
    {
        JCheckBox jcb = checkboxMap.get(bufferName);
        if (jcb == null)
            buffersChanged();
        else
            jcb.setEnabled(enabled);
    }

    void buffersChanged()
    {
        SwingUtilities.invokeLater(new RebuildBuffersList());
    }

    class RebuildBuffersList implements Runnable
    {
        public void run()
        {
            ArrayList<String> bufferNames = new ArrayList<String>();
            VisWorld vw = vc.getWorld();

            synchronized(vw.buffers) {
                for (VisWorld.Buffer vb : vw.buffers) {
                    bufferNames.add(vb.name);
                }
            }

            Collections.sort(bufferNames);

            checkboxPanel.removeAll();

            for (String s : bufferNames) {
                JCheckBox jcb = new JCheckBox(s, vc.getViewManager().isBufferEnabled(s));
                jcb.addActionListener(new VisibleActionListener(jcb, vc, s));
                checkboxPanel.add(jcb);

                checkboxMap.put(s, jcb);
            }

            checkboxPanel.revalidate();
        }
    }

    class VisibleActionListener implements ActionListener
    {
        JCheckBox jcb;
        VisCanvas vc;
        String bufferName;

        VisibleActionListener(JCheckBox jcb, VisCanvas vc, String bufferName)
        {
            this.jcb = jcb;
            this.vc = vc;
            this.bufferName = bufferName;
        }

        public void actionPerformed(ActionEvent e) {
            vc.getViewManager().setBufferEnabled(bufferName, jcb.isSelected());
            vc.draw();
        }
    }

    int lastnbuffs = 0;

    public void worldChanged(VisWorld vw)
    {
        boolean dirty = false;
        synchronized(this) {
            dirty = (vw.buffers.size() != lastnbuffs);
            lastnbuffs = vw.buffers.size();
        }

        if (dirty)
            buffersChanged();
    }

}
