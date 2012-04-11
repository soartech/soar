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

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.jdesktop.swingx.JXTable;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

import edu.umich.robot.actions.ActionManager;
import edu.umich.robot.actions.CreateSplinterRobotAction;
import edu.umich.robot.april.SoarViewRobot;

/**
 * The robot & robot controller table.
 * 
 * @author voigtjr@gmail.com
 */
// TODO SoarApril
public class RobotsView extends JPanel /* implements RobotSelectionChangedListener */
{
    private static final long serialVersionUID = 7404227862171554005L;

    private final GuiApplication app;

    private final RobotsTableModel model;

    private final JXTable table;
    
    public RobotsView(GuiApplication app, ActionManager manager)
    {
        super(new BorderLayout());
        this.app = app;
        this.model = new RobotsTableModel(app.getController());
        table = new JXTable(model);
        
        final JPopupMenu popup = new JPopupMenu();

        popup.add(associateSoarAgent);
        popup.add(manager.getAction(CreateSplinterRobotAction.class));
        table.add(popup);
        table.addMouseListener(new MouseAdapter()
        {
            @Override
            public void mousePressed(MouseEvent e)
            {
                if (SwingUtilities.isRightMouseButton(e))
                {
                    // get the coordinates of the mouse click
                    Point p = e.getPoint();

                    // get the row index that contains that coordinate
                    int rowNumber = table.rowAtPoint(p);

                    // Get the ListSelectionModel of the JTable
                    ListSelectionModel model = table.getSelectionModel();

                    // set the selected interval of rows. Using the "rowNumber"
                    // variable for the beginning and end selects only that one
                    // row.
                    model.setSelectionInterval(rowNumber, rowNumber);
                }
                maybeShowPopup(e);
            }

            @Override
            public void mouseReleased(MouseEvent e)
            {
                maybeShowPopup(e);
            }

            private void maybeShowPopup(MouseEvent e)
            {
                if (e.isPopupTrigger())
                {
                    if (table.getSelectedRow() == -1)
                        associateSoarAgent.setEnabled(false);
                    else
                        associateSoarAgent.setEnabled(true);
                    popup.show(e.getComponent(), e.getX(), e.getY());
                }
            }
        });

        table.getSelectionModel().setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        table.getSelectionModel().addListSelectionListener(
            new ListSelectionListener()
            {
                public void valueChanged(ListSelectionEvent e)
                {
                    if (e.getValueIsAdjusting() || e.getFirstIndex() == -1)
                        return;
                    
                    int row = table.getSelectedRow();
                    if (row < 0)
                        return;
                    Robot robot = (Robot) model.getValueAt(row, RobotsTableModel.ROBOT_COL);
                    RobotsView.this.app.getController().selectRobot(robot.getName());
                }
            });
        add(table.getTableHeader(), BorderLayout.PAGE_START);
        add(table, BorderLayout.CENTER);
    }

    private final AbstractAction associateSoarAgent = new AbstractAction(
            "Associate Soar Agent")
    {
        private static final long serialVersionUID = -6678064325985681483L;

        public void actionPerformed(ActionEvent e)
        {
            final Robot robot = (Robot) model.getValueAt(
                    table.getSelectedRow(), RobotsTableModel.ROBOT_COL);
            if (robot == null)
                return; // TODO

            FormLayout layout = new FormLayout(
                    "right:pref, 4dlu, 200dlu, 4dlu, pref",
                    "pref, 2dlu, pref, 2dlu, pref");

            layout.setRowGroups(new int[][] {{1, 3}});

            final JDialog dialog = new JDialog((Frame) app.getTopLevelAncestor(), "Create Soar Agent", true);
            dialog.setLayout(layout);

            final JTextField name = new JTextField(robot.getName());
            final JTextField productions = new JTextField();
            final JButton browse = new JButton("...");

            CellConstraints cc = new CellConstraints();
            dialog.add(new JLabel("Name"), cc.xy(1, 1));
            dialog.add(name, cc.xy(3, 1));
            dialog.add(new JLabel("Productions"), cc.xy(1, 3));
            dialog.add(productions, cc.xy(3, 3));
            dialog.add(browse, cc.xy(5, 3));
            final JPanel buttons = new JPanel(new FlowLayout());
            final JButton cancel = new JButton("Cancel");
            buttons.add(cancel);
            final JButton ok = new JButton("OK");
            buttons.add(ok);
            dialog.add(buttons, cc.xyw(1, 5, 5));
            
            productions.setText(app.getPreferences().get("lastProductions", ""));
            
            browse.addActionListener(new ActionListener()
            {
                public void actionPerformed(ActionEvent e)
                {
                    JFileChooser fc = new JFileChooser(System.getProperty("user.dir"));
                    fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
                    fc.setMultiSelectionEnabled(false);
                    int ret = fc.showOpenDialog(dialog);
                    if (ret == JFileChooser.APPROVE_OPTION)
                    {
                        productions.setText(fc.getSelectedFile().getAbsolutePath());
                        app.getPreferences().put("lastProductions", productions.getText());
                    }
                }
            });
            
            final ActionListener okListener = new ActionListener()
            {
                public void actionPerformed(ActionEvent e)
                {
                    String soarName = name.getText().trim();
                    if (soarName.isEmpty())
                        return; // TODO
                    for (char c : soarName.toCharArray())
                        if (!Character.isDigit(c) && !Character.isLetter(c))
                            return; // TODO

                    app.getController().createSoarController(soarName, robot.getName(), productions.getText(), null);
                    dialog.dispose();
                }
            };
            name.addActionListener(okListener);
            productions.addActionListener(okListener);
            ok.addActionListener(okListener);

            ActionListener cancelAction = new ActionListener()
            {
                public void actionPerformed(ActionEvent e)
                {
                    dialog.dispose();
                }
            };
            cancel.addActionListener(cancelAction);
            dialog.getRootPane().registerKeyboardAction(cancelAction, 
                    KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), 
                    JComponent.WHEN_IN_FOCUSED_WINDOW);

            dialog.setLocationRelativeTo(app.getTopLevelAncestor());
            dialog.pack();
            dialog.setVisible(true);
        }
    };

    public void onRobotSelectionChanged(SoarViewRobot selectedRobot)
    {
        if (selectedRobot == null)
            table.getSelectionModel().clearSelection();
        else
        {
            int row = model.getRowByName(selectedRobot.getName());
            if (row >= 0)
                table.getSelectionModel().setSelectionInterval(row, row);
        }
    }

}
