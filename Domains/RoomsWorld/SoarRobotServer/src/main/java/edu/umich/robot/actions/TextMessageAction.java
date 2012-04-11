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
import java.util.ArrayList;

import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import edu.umich.robot.radio.RadioMessage;

/**
 * @author voigtjr@gmail.com
 */
public class TextMessageAction extends AbstractRobotAction
{
	private static final long serialVersionUID = -1489848480598387790L;

	public TextMessageAction(ActionManager manager)
    {
        super(manager, "Text Message Robot");
        setToolTip("Send a text message to an agent");
    }
    
    public void actionPerformed(ActionEvent e)
    {
    	String robot = getApplication().getController().getSelectedRobotName();
    	
    	if (robot == null) {
    		JOptionPane.showMessageDialog(getApplication().getTopLevelAncestor(), "No Robot Selected.");
    		return;
    	}
    	
    	String message = (String) JOptionPane.showInputDialog("Enter message:");
    	
    	if (message == null || message.length() == 0) {
    		return;
    	}
    	
    	String[] ar = message.split(" ");
    	
    	if (ar.length == 0) {
    		JOptionPane.showMessageDialog(getApplication().getTopLevelAncestor(), "Bad message: \"" + message + "\"");
    		return;
    	}
    	
    	ArrayList<String> tokens = new ArrayList<String>();
    	
    	for (String s : ar) {
    		tokens.add(s);
    	}
    	
    	RadioMessage.Builder radioBuilder = new RadioMessage.Builder("GUI");
    	
    	for (String token : tokens)
    	{
    		radioBuilder.token(token);
    	}
    	
    	RadioMessage comm = radioBuilder.build();
        getApplication().getController().getRadio().postRadioMessage(comm);
        update();
    }

    @Override
    public void update()
    {
    }

}
