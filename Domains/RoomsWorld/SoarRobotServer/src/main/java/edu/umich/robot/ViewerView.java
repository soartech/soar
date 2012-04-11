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

import april.vis.VisCanvas;

/**
 * @author voigtjr@gmail.com
 */
public class ViewerView extends JPanel
{
    private static final long serialVersionUID = 3830427699054499409L;

    ViewerView(VisCanvas visCanvas)
    {
        super(new BorderLayout());
        visCanvas.setPreferredSize(new Dimension(500, 500));
        this.add(visCanvas, BorderLayout.CENTER);

        // FIXME: not drawing grid causes major lighting error
        // TODO left over from SoarApril
        // visCanvas.setDrawGrid(false);

        this.setVisible(true);
    }
}
