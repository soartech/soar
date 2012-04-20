package edu.umich.dice3.ui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JScrollPane;
import javax.swing.JTextField;

import org.ini4j.Wini;

public class AgentChooser extends JDialog implements ActionListener
{

    private static final long serialVersionUID = 7754138833004097725L;

    private JList list;
    private JScrollPane listScroll;
    private List<String> selected;
    private JButton ok;
    private JList text;
    private JTextField numGamesText;
    private JComboBox runTypeBox;
    private JCheckBox noGuiBox;
    private JCheckBox noMetaBox;
    
    public static enum RunType
    {
        Single_Game,
        All_Permutations,
        First_vs_Each,
        RL;
        
        public String toString() {
            return super.toString().replace('_', ' ');
        }
        
    }
    
    public AgentChooser(Wini wini, String userString)
    {
        super();
        setTitle("Choose Agents");
        setModal(true);
        BoxLayout layout = new BoxLayout(getContentPane(), BoxLayout.Y_AXIS);
        setLayout(layout);
        selected = new ArrayList<String>();
        
        Collection<String> names = new ArrayList<String>();
        names.add(userString);
        for (String name : wini.keySet())
        {
            if (!name.equals("dice"))
            {
                names.add(name);
            }
        }
        
        
        list = new JList(names.toArray());
        list.addMouseListener(new MouseAdapter()
        {
            @Override
            public void mouseClicked(MouseEvent e)
            {
                if (e.getClickCount() == 2)
                {
                    Object obj = list.getSelectedValue();
                    if (obj != null)
                    {
                        AgentChooser.this.selected.add(obj.toString());
                        AgentChooser.this.refresh();
                    }
                }
            }
        });
        
        listScroll = new JScrollPane(list);
        
        text = new JList(selected.toArray());
        text.addMouseListener(new MouseAdapter()
        {
            @Override
            public void mouseClicked(MouseEvent e)
            {
                if (e.getClickCount() == 2)
                {
                    int index = text.getSelectedIndex();
                    if (index >= 0)
                    {
                        AgentChooser.this.selected.remove(index);
                        AgentChooser.this.refresh();
                    }
                }
            }
        });
        
        runTypeBox = new JComboBox(RunType.values());
        noGuiBox = new JCheckBox("No GUI");
        noGuiBox.setSelected(false);
        noMetaBox = new JCheckBox("No Metadata");
        noMetaBox.setSelected(false);

        numGamesText = new JTextField("1");
        
        ok = new JButton("OK");
        ok.addActionListener(this);
        JLabel agentsLabel = new JLabel("Available Agents:");
        JLabel textLabel = new JLabel("Selected Agents:");
        JLabel numGamesLabel = new JLabel("Number of Matches");
        
        Box inner = new Box(BoxLayout.X_AXIS);
        Box left = new Box(BoxLayout.Y_AXIS);
        Box right = new Box(BoxLayout.Y_AXIS);
        inner.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        left.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        right.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        listScroll.setAlignmentY(TOP_ALIGNMENT);
        listScroll.setAlignmentX(LEFT_ALIGNMENT);
        agentsLabel.setAlignmentY(TOP_ALIGNMENT);
        agentsLabel.setAlignmentX(LEFT_ALIGNMENT);
        left.add(agentsLabel);
        left.setAlignmentY(TOP_ALIGNMENT);
        left.add(listScroll);
        inner.add(left);
        text.setAlignmentY(TOP_ALIGNMENT);
        text.setAlignmentX(LEFT_ALIGNMENT);
        textLabel.setAlignmentY(TOP_ALIGNMENT);
        textLabel.setAlignmentX(LEFT_ALIGNMENT);
        right.add(textLabel);
        right.add(text);
        right.setAlignmentY(TOP_ALIGNMENT);
        inner.add(right);
        add(inner);
        
        add(numGamesLabel);
        add(numGamesText);
        
        add(new JLabel("Run Mode:"));
        add(runTypeBox);
        
        add(noGuiBox);
        add(noMetaBox);
        
        ok.setAlignmentX(RIGHT_ALIGNMENT);
        add(ok);
        refresh();
    }
    
    public void refresh()
    {
        text.setListData(selected.toArray());
        pack();
        validate();
    }
    
    public List<String> getSelectedAgents()
    {
        return selected;
    }
    
    public boolean getNoGui()
    {
        return noGuiBox.isSelected();
    }
    
    public boolean getNoMeta()
    {
        return noMetaBox.isSelected();
    }

    @Override
    public void actionPerformed(ActionEvent event)
    {
        if (event.getSource() == ok)
        {
            setVisible(false);
        }
    }

    public int getNumGames()
    {
        try
        {
            int ret = Integer.valueOf(numGamesText.getText());
            return ret;
        }
        catch (NumberFormatException e)
        {
            e.printStackTrace();
            return -1;
        }
    }
    
    public RunType getSelectedRunType()
    {
        return (RunType) runTypeBox.getSelectedItem();
    }
    
}
