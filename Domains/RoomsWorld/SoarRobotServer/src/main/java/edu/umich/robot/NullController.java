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

import edu.umich.robot.events.control.AbstractControlEvent;
import edu.umich.robot.util.events.RobotEventListener;

/**
 * <p>
 * Robot controller that does nothing, nameed "none". I think this is here to be
 * used instead of an actual null reference.
 * 
 * @author voigtjr@gmail.com
 */
public enum NullController implements RobotController
{
    INSTANCE;

    public <T extends AbstractControlEvent> void addListener(Class<T> klass,
            RobotEventListener listener)
    {
        throw new AssertionError();
    }

    public String getName()
    {
        return "none";
    }

    public <T extends AbstractControlEvent> void removeListener(Class<T> klass,
            RobotEventListener listener)
    {
        throw new AssertionError();
    }

    @Override
    public String toString()
    {
        return getName();
    }

    public void debug()
    {
        throw new AssertionError();
    }

}
