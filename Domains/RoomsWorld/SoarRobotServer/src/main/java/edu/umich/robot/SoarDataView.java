/*
 * Copyright (c) 2011, Regents of the University of Michigan
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package edu.umich.robot;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.Map;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

import com.google.common.collect.Maps;
import com.jgoodies.forms.builder.DefaultFormBuilder;
import com.jgoodies.forms.layout.FormLayout;

import edu.umich.robot.soar.SoarProperties;
import edu.umich.robot.soar.SoarDataCollector.DataCollectionMode;
import edu.umich.robot.util.properties.PropertyManager;

/**
 * <p>
 * Form for data collection settings.
 * 
 * @author voigtjr@gmail.com
 */
public class SoarDataView
{
    private final JPanel panel;
    private final PropertyManager properties;
    private final JTextField periodCycles;
    private final JTextField periodMillis;
    private final JLabel fileLabel;
    private File file;
    private final Map<DataCollectionMode, JRadioButton> modeButtons = Maps.newHashMap();
    
    public SoarDataView(PropertyManager properties)
    {
        this.properties = properties;
        
        FormLayout layout = new FormLayout("right:pref, 4dlu, 60dlu", "");
        DefaultFormBuilder builder = new DefaultFormBuilder(layout);
        builder.setDefaultDialogBorder();

        builder.appendSeparator("collection mode");
        ButtonGroup group = new ButtonGroup();
        for (DataCollectionMode s : DataCollectionMode.values())
        {
            JRadioButton b = new JRadioButton();
            b.setSelected(properties.get(SoarProperties.DATA_COLLECTION_MODE) == s);
            modeButtons.put(s, b);
            group.add(b);
            builder.append(s.toString().toLowerCase(), b);
        }
        
        builder.appendSeparator("collection period");
        periodCycles = new JTextField();
        periodCycles.setText(Integer.toString(properties.get(SoarProperties.PERIOD_CYCLES)));
        builder.append("Collect data every n decision cycles", periodCycles);
        
        periodMillis = new JTextField();
        periodMillis.setText(Integer.toString(properties.get(SoarProperties.PERIOD_MILLIS)));
        builder.append("Collect data every n milliseconds", periodMillis);
        
        ActionListener al = new ActionListener()
        {
            public void actionPerformed(ActionEvent arg0)
            {
                periodCycles.setEnabled(modeButtons.get(DataCollectionMode.DECISION_CYCLES).isSelected());
                periodMillis.setEnabled(modeButtons.get(DataCollectionMode.ELAPSED_TIME).isSelected());
            }
        };
        al.actionPerformed(null);
        
        modeButtons.get(DataCollectionMode.DECISION_CYCLES).addActionListener(al);
        modeButtons.get(DataCollectionMode.ELAPSED_TIME).addActionListener(al);

        builder.appendSeparator("file");
        file = properties.get(SoarProperties.DATA_FILE);
        fileLabel = new JLabel();
        updateFileLabel();
        chooser = new JButton("...");
        builder.append("Data file", fileLabel);
        builder.append("Click to pick data file:");
        builder.append(chooser);
        
        cancel = new JButton("Cancel");
        builder.append(cancel);

        ok = new JButton("OK");
        builder.append(ok);
        
        panel = builder.getPanel();
    }
    
    private void updateFileLabel()
    {
        fileLabel.setText(file != null ? file.getName() : "none");
    }
    
    private final JButton cancel;
    private final JButton ok;
    private final JButton chooser;
    
    public void showDialog(final JFrame top)
    {
        chooser.addActionListener(new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                JFileChooser fc = new JFileChooser(new File(System.getProperty("user.dir")));
                fc.setMultiSelectionEnabled(false);
                int ret = fc.showSaveDialog(top);
                if (ret == JFileChooser.APPROVE_OPTION)
                {
                    file = fc.getSelectedFile();
                    updateFileLabel();
                }
            }
        });

        
        final JDialog d = new JDialog(top, "Soar Data");
        cancel.addActionListener(new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                d.dispose();
            }
        });
        
        ok.addActionListener(new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                for (Map.Entry<DataCollectionMode, JRadioButton> entry : modeButtons.entrySet())
                    if (entry.getValue().isSelected())
                        properties.set(SoarProperties.DATA_COLLECTION_MODE, entry.getKey());

                properties.set(SoarProperties.PERIOD_CYCLES, Integer.parseInt(periodCycles.getText()));
                properties.set(SoarProperties.PERIOD_MILLIS, Integer.parseInt(periodMillis.getText()));
                properties.set(SoarProperties.DATA_FILE, file);
                
                d.dispose();
            }
        });

        d.setContentPane(panel);
        d.pack();
        d.setLocationRelativeTo(top);
        d.setVisible(true);
    }
}
