package edu.umich.dice3.ui;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.List;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import edu.umich.dice3.gamestate.HistoryItem;

public class HistoryPanel extends JPanel implements ListSelectionListener, ActionListener
{

    private static final long serialVersionUID = -3101841721081853839L;

    private List<HistoryItem> history;
    public JList list;
    private JScrollPane scroll;
    private JTextArea text;
    private JCheckBox box;

    public HistoryPanel(List<HistoryItem> history)
    {
        super();
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        this.history = history;
        this.list = new JList();
        list.addListSelectionListener(this);
        JScrollPane listScroll = new JScrollPane(list);

        text = new JTextArea();
        scroll = new JScrollPane(text);

        JSplitPane split = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true);
        split.add(listScroll);
        split.add(scroll);
        split.setAlignmentX(TOP_ALIGNMENT);
        split.setAlignmentY(LEFT_ALIGNMENT);
        split.setDividerLocation(400);
        add(split);

        box = new JCheckBox("Show Hidden State");
        box.addActionListener(this);
        box.setAlignmentX(TOP_ALIGNMENT);
        box.setAlignmentY(LEFT_ALIGNMENT);

        add(box);

        // setPreferredSize(new Dimension(300, -1));

        refresh();
    }

    public void setHistory(List<HistoryItem> history, boolean doStuff)
    {
        this.history = history;
        if (doStuff)
        {
            int index = list.getSelectedIndex();
            Object[] ar = history.toArray();
            list.setListData(ar);
            list.setSelectedIndex(index);
            refresh();
        }
    }

    public void refresh()
    {
        int index = list.getSelectedIndex();
        if (index >= 0)
        {
            HistoryItem item = history.get(index);
            if (box.isSelected())
            {
                text.setText(item + "\n" + item.getState());
            }
            else
            {
                text.setText("");
            }
        }
        else
        {
            text.setText("");
        }
        validate();
    }

    @Override
    public void valueChanged(ListSelectionEvent event)
    {
        refresh();
    }

    @Override
    public void actionPerformed(ActionEvent event)
    {
        Object source = event.getSource();
        if (source == box)
        {
            refresh();
        }
    }

    public List<HistoryItem> getHistory()
    {
        return history;
    }

}
