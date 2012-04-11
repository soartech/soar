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

import java.net.URL;

import javax.swing.ImageIcon;

/**
 * <p>
 * Images loaded once. Most of these are used in the GUI.
 * 
 * @author voigtjr@gmail.com
 */
public class Images
{
    public static final ImageIcon DATABASE = loadImage("/edu/umich/robot/db.png");
    public static final ImageIcon PLAYSTOP = loadImage("/edu/umich/robot/playstop.png");
    public static final ImageIcon RESET = loadImage("/edu/umich/robot/reset.png");
    public static final ImageIcon SETTINGS = loadImage("/edu/umich/robot/settings.png");
    public static final ImageIcon STEP = loadImage("/edu/umich/robot/step.png");
    
    private static ImageIcon loadImage(String file)
    {
        URL url = Images.class.getResource(file);
        return new ImageIcon(url);
    }

}
