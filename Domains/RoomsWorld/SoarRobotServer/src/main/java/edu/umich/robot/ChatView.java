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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Point2D;
import java.util.ArrayList;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.DocumentFilter;

import edu.umich.robot.radio.RadioHandler;
import edu.umich.robot.radio.RadioMessage;

/**
 * <p> Simple panel that shows the last few lines from the console output,
 * helpful for debugging.
 *
 * @author voigtjr@gmail.com
 */

public class ChatView extends JPanel implements RadioHandler
{
    private static final long serialVersionUID = 5579225590193881099L;

    private final JScrollPane scrollPane;

    private final JTextArea textArea;
    private final JTextField textField;
    private final GuiApplication application;
    
    private Point2D.Double lastClick;
    
    private ActionListener chatActionListener = new ActionListener() {
		@Override
		public void actionPerformed(ActionEvent event) {
			String text = textField.getText();
			ArrayList<String> tokens = new ArrayList<String>();
			for (String token : text.split(" "))
			{
				tokens.add(token);
			}
			String robot = application.getController().getSelectedRobotName();
			if (robot != null)
			{
				RadioMessage.Builder b = new RadioMessage.Builder("User").destination(robot);
				for (String token : tokens)
				{
					b.token(token);
				}
				if (lastClick.x > 0.0 && lastClick.y < 0.0)
				{
					b.data("x", lastClick.x);
					b.data("y", lastClick.y);
				}
				application.getController().getRadio().postRadioMessage(b.build());
				textField.setText("");
			}
		}
    };
    
    /** 
     * <p> Length of scroll-back buffer.
     *
     * <p> Changing  this with the current implementation can severely affect
     * performance.
     */
    private static final int maxLines = 10;
    
    public ChatView(GuiApplication application)
    {
        super(new BorderLayout());

        textArea = new JTextArea();
        textArea.setEditable(false);
        
        textField = new JTextField();
        textField.addActionListener(chatActionListener);
        
        lastClick = new Point2D.Double(-1.0f, 1.0f);
        
        this.application = application;
        
        Document doc = textArea.getDocument();
        if (doc instanceof AbstractDocument)
        {
             AbstractDocument adoc = (AbstractDocument)doc;
             adoc.setDocumentFilter(new DocumentFilter()
             {
                 private int lines = 0;

                 @Override
                 public void insertString(FilterBypass fb, int offset, String string, AttributeSet attr) throws BadLocationException
                 {
                     super.insertString(fb, offset, string, attr);
                     
                     int from = 0;
                     while (from == 0)
                     {
                         from = string.indexOf("\n", from);
                         if (from > 0)
                             lines += 1;
                     }
                     
                     if (lines > maxLines)
                     {
                         try
                         {
                             Document doc = fb.getDocument();
                             String text = doc.getText(0, doc.getLength());
                             int rIndex = -1;
                             while (lines - maxLines > 0)
                             {
                                 rIndex = text.indexOf("\n", rIndex + 1);
                                 lines -= 1;
                             }
                             doc.remove(0, rIndex + 1);
                         } 
                         catch (Exception e)
                         {
                             System.out.println();
                         }
                     }
                     
                 }
             });
        }
        
        scrollPane = new JScrollPane(textArea);        
        add(scrollPane, BorderLayout.CENTER);
        add(textField, BorderLayout.SOUTH);
    }

	@Override
	public void radioMessageReceived(RadioMessage comm) {
		textArea.append(comm.getFrom() + " to " + comm.getDestination() + ": " + comm.getConcatenatedTokens(" ") + '\n');
	}
	
	public void setClick(Point2D.Double click)
	{
		this.lastClick = click;
	}
}
