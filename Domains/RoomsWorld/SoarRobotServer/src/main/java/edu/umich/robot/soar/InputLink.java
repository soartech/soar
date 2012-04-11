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
package edu.umich.robot.soar;

import java.util.ArrayList;
import java.util.List;

/**
 * Input link manager.
 * 
 * @author voigtjr@gmail.com
 */
public class InputLink
{
    static InputLink newInstance(SoarAgent agent)
    {
        return new InputLink(agent);
    }

    /**
     * Contains top-level input-link elements or "modules".
     */
    private final List<InputLinkElement> elements = new ArrayList<InputLinkElement>();

    public InputLink(SoarAgent agent)
    {
        elements.add(new TimeIL(agent));
        elements.add(new SelfIL(agent));
        elements.add(new ConfigurationIL(agent));
        elements.add(new WaypointsIL(agent));
        elements.add(new ReceivedMessagesIL(agent));
        elements.add(new AreaDescriptionIL(agent));
        elements.add(new ObjectsIL(agent));
        elements.add(new LidarIL(agent));
    }

    /**
     * Calls update on all the top level input-link "modules"
     */
    public void update()
    {
        for (InputLinkElement element : elements)
            element.update();
    }

    /**
     * Calls destroy on everything.
     */
    public void destroy()
    {
        for (InputLinkElement element : elements)
            element.destroy();
    }

}
