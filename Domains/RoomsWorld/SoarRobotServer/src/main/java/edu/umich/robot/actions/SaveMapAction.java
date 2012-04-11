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
package edu.umich.robot.actions;

import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileNameExtensionFilter;

/**
 * @author voigtjr@gmail.com
 */
public class SaveMapAction extends AbstractRobotAction
{
    private static final long serialVersionUID = -277711713927395709L;

    public SaveMapAction(ActionManager manager)
    {
        super(manager, "Save Map...");
    }

    public void actionPerformed(ActionEvent e)
    {
        JFileChooser fc = new JFileChooser("config");
        fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
        FileFilter filter = new FileNameExtensionFilter("Text Config File",
                "txt");
        fc.setFileFilter(filter);
        fc.setMultiSelectionEnabled(false);
        int ret = fc.showSaveDialog(getApplication().getTopLevelAncestor());
        if (ret == JFileChooser.APPROVE_OPTION)
        {
            File file = fc.getSelectedFile();
            if (!file.getName().endsWith(".txt"))
                file = new File(file.getAbsolutePath() + ".txt");
            if (file.exists())
            {
                int confirm = JOptionPane.showConfirmDialog(getApplication().getTopLevelAncestor(), 
                        file.getName() + " exists, overwrite?", "Confirm Overwrite", JOptionPane.OK_CANCEL_OPTION);
                if (confirm != JOptionPane.OK_OPTION)
                    return;
            }
            
            try
            {
                getApplication().getController().saveMap(file);
                getApplication().setStatusBarMessage("Map file '" + file.getAbsolutePath() + "' saved.");
            }
            catch (IOException e1)
            {
                e1.printStackTrace();
            }
        }
    }

    @Override
    public void update()
    {
    }

}
