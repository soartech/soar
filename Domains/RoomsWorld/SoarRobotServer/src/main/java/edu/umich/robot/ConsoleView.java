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
import java.awt.Dimension;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.DocumentFilter;

import org.apache.log4j.AppenderSkeleton;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;
import org.apache.log4j.spi.LoggingEvent;

/**
 * <p> Simple panel that shows the last few lines from the console output,
 * helpful for debugging.
 *
 * @author voigtjr@gmail.com
 */

public class ConsoleView extends JPanel
{
    private static final long serialVersionUID = 5579225590193881099L;

    private final JScrollPane scrollPane;

    private final JTextArea textArea;

    private class ConsoleAppender extends AppenderSkeleton
    {
        private final PatternLayout layout = new PatternLayout("%m%n");
        
        public ConsoleAppender()
        {
            setLayout(layout);
        }
        
        public boolean requiresLayout()
        {
            return true;
        }
        
        public void close()
        {
        }
        
        @Override
        protected void append(final LoggingEvent le)
        {
            SwingUtilities.invokeLater(new Runnable()
            {
                public void run()
                {
                    String msg = getLayout().format(le);
                    textArea.append(msg);
                    textArea.setCaretPosition(textArea.getDocument().getLength());
                }
            });
        }
    }
    
    /** 
     * <p> Length of scroll-back buffer.
     *
     * <p> Changing  this with the current implementation can severely affect
     * performance.
     */
    private static final int maxLines = 10;
    
    public ConsoleView()
    {
        super(new BorderLayout());

        textArea = new JTextArea();
        textArea.setEditable(false);
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
        scrollPane.setPreferredSize(new Dimension(0, 60));

        add(scrollPane, BorderLayout.CENTER);
        
        Logger.getRootLogger().addAppender(new ConsoleAppender());
    }
}
