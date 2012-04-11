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
import java.util.List;
import java.util.Map;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;

import com.google.common.base.Joiner;
import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.jgoodies.forms.builder.DefaultFormBuilder;
import com.jgoodies.forms.layout.FormLayout;

import edu.umich.robot.soar.AgentProperties;
import edu.umich.robot.soar.AgentProperties.LearnSetting;
import edu.umich.robot.soar.AgentProperties.Mission;
import edu.umich.robot.util.properties.PropertyKey;
import edu.umich.robot.util.properties.PropertyManager;

/**
 * <p>
 * Form for agent parameter settings.
 * 
 * @author voigtjr@gmail.com
 */
public class SoarParametersView
{
    private final JPanel panel;
    private final PropertyManager properties;

    private final Map<LearnSetting, JRadioButton> learnButtons = Maps.newHashMap();
    private final JCheckBox epmemLearn = new JCheckBox();
    private final JCheckBox smemLearn = new JCheckBox();
    
    private final Map<Mission, JRadioButton> missionButtons = Maps.newHashMap();
    private final JTextField epmemExclusions = new JTextField();
    
    private final JTextArea misc = new JTextArea();

    private final Map<PropertyKey<String>, JTextField> spFields = Maps.newHashMap();
    
    public SoarParametersView(PropertyManager properties)
    {
        this.properties = properties;
        
        FormLayout layout = new FormLayout(
                "right:pref, 4dlu, 100dlu",
                "");
        
        DefaultFormBuilder builder = new DefaultFormBuilder(layout);
        builder.setDefaultDialogBorder();
        
        builder.appendSeparator(AgentProperties.LEARN.toString());
        ButtonGroup learnGroup = new ButtonGroup();
        for (LearnSetting s : LearnSetting.values())
        {
            JRadioButton b = new JRadioButton();
            b.setSelected(properties.get(AgentProperties.LEARN) == s);
            learnButtons.put(s, b);
            learnGroup.add(b);
            builder.append(s.toString().toLowerCase(), b);
        }
        
        builder.appendSeparator("epmem, smem");
        
        epmemLearn.setSelected(properties.get(AgentProperties.EPMEM_LEARNING));
        builder.append(AgentProperties.EPMEM_LEARNING.toString(), epmemLearn);
        
        smemLearn.setSelected(properties.get(AgentProperties.SMEM_LEARNING));
        builder.append(AgentProperties.SMEM_LEARNING.toString(), smemLearn);
        
        Joiner joiner = Joiner.on(" ");
        epmemExclusions.setText(joiner.join(properties.get(AgentProperties.EPMEM_EXCLUSIONS)));
        builder.append(AgentProperties.EPMEM_EXCLUSIONS.toString(), epmemExclusions);

        builder.appendSeparator("working memory parameters");

        addSpField(builder, AgentProperties.DEFAULT_STORAGE_AREA_ID);
        addSpField(builder, AgentProperties.AREAS_HELD_IN);
        addSpField(builder, AgentProperties.OBJECTS_HELD_IN);
        addSpField(builder, AgentProperties.LOOK_AHEAD_PLANNING);
        addSpField(builder, AgentProperties.SEARCH_CONTROL_GO_TO_GATEWAY);
        addSpField(builder, AgentProperties.DELETE_OLD_AREAS);

        builder.appendSeparator("working memory parameters: mission");

        ButtonGroup missionGroup = new ButtonGroup();
        for (Mission s : Mission.values())
        {
            JRadioButton b = new JRadioButton();
            b.setSelected(properties.get(AgentProperties.MISSION) == s);
            missionButtons.put(s, b);
            missionGroup.add(b);
            builder.append(s.toString(), b);
        }
        
        builder.appendSeparator("miscellaneous commands");
              
        misc.setRows(4);
        joiner = Joiner.on("\n");
        misc.setText(joiner.join(properties.get(AgentProperties.MISC_COMMANDS)));
        builder.append(new JScrollPane(misc), 3);
        
        cancel = new JButton("Cancel");
        ok = new JButton("OK");
        
        builder.append(cancel);
        builder.append(ok);
        
        panel = builder.getPanel();
    }
    
    private void addSpField(DefaultFormBuilder builder, PropertyKey<String> key)
    {
        JTextField spField = new JTextField(properties.get(key));
        builder.append(key.toString(), spField);
        spFields.put(key, spField);
    }
    
    private final JButton cancel;
    private final JButton ok;
    
    public void showDialog(JFrame top)
    {
        final JDialog d = new JDialog(top, "Soar Parameters");
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
                for (Map.Entry<LearnSetting, JRadioButton> entry : learnButtons.entrySet())
                    if (entry.getValue().isSelected())
                        properties.set(AgentProperties.LEARN, entry.getKey());
                
                properties.set(AgentProperties.EPMEM_LEARNING, epmemLearn.isSelected());
                properties.set(AgentProperties.SMEM_LEARNING, smemLearn.isSelected());
                
                Splitter splitter = Splitter.onPattern("\\s+");
                List<String> exclusions = Lists.newArrayList(splitter.split(epmemExclusions.getText()));
                properties.set(AgentProperties.EPMEM_EXCLUSIONS, exclusions.toArray(new String[exclusions.size()]));
                
                for (Map.Entry<PropertyKey<String>, JTextField> entry : spFields.entrySet())
                    properties.set(entry.getKey(), entry.getValue().getText());

                for (Map.Entry<Mission, JRadioButton> entry : missionButtons.entrySet())
                    if (entry.getValue().isSelected())
                        properties.set(AgentProperties.MISSION, entry.getKey());
                
                splitter = Splitter.on("\n").omitEmptyStrings();
                List<String> miscCommands = Lists.newArrayList(splitter.split(misc.getText()));
                properties.set(AgentProperties.MISC_COMMANDS, miscCommands.toArray(new String[miscCommands.size()]));
                    
                d.dispose();
            }
        });

        d.setContentPane(panel);
        d.pack();
        d.setLocationRelativeTo(top);
        d.setVisible(true);
    }
}
