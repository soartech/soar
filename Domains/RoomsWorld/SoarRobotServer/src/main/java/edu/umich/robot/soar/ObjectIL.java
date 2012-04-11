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

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import sml.Identifier;
import edu.umich.robot.metamap.VirtualObject;
import edu.umich.soar.FloatWme;
import edu.umich.soar.IntWme;
import edu.umich.soar.StringWme;

/**
 * @author voigtjr@gmail.com
 */
class ObjectIL extends InputLinkElement
{

    private PointWithDistanceIL pointData;

    private final SoarAgent agent;

    private long invisibleTimestamp = 0;

    private StringWme visible;

    private final VirtualObject o;

    private final Map<String, Object> props = new HashMap<String, Object>();
    
    ObjectIL(SoarAgent agent, VirtualObject o, Identifier root)
    {
        super(agent, IOConstants.OBJECT, root);
        this.o = o;
        this.agent = agent;
        
        pointData = new PointWithDistanceIL(getRoot(), agent, o.getPose());

        for (Entry<String, String> e : o.getProperties().entrySet())
            createProp(e.getKey(), e.getValue());

        IntWme.newInstance(getRoot(), IOConstants.ID, o.getId());
        visible = StringWme.newInstance(getRoot(), IOConstants.VISIBLE, IOConstants.TRUE);
    }
    
    private void createProp(String key, String value)
    {
        // Try to make it a float or int wme, but if those fail, go with string.
        try
        {
            double temp = Double.valueOf(value);
            FloatWme w = FloatWme.newInstance(getRoot(), key, temp);
            props.put(key, w);
            return;
        }
        catch (NumberFormatException e)
        {
        }
        try
        {
            long temp = Long.valueOf(value);
            IntWme w = IntWme.newInstance(getRoot(), key, temp);
            props.put(key, w);
            return;
        }
        catch (NumberFormatException e)
        {
        }
        StringWme w = StringWme.newInstance(getRoot(), key, value);
        props.put(key, w);
    }

    @Override
    void update()
    {
        for (Entry<String, String> e : o.getProperties().entrySet())
        {
            Object wobj = props.get(e.getKey());
            if (wobj == null)
                createProp(e.getKey(), e.getValue());
            else
            {
                if (wobj instanceof StringWme)
                {
                    StringWme w = (StringWme) wobj;
                    w.update(e.getValue());
                }
                else
                {
                    try
                    {
                        if (wobj instanceof IntWme)
                        {
                            IntWme w = (IntWme) wobj;
                            w.update(Long.valueOf(e.getValue()));
                        }
                        else
                        {
                            FloatWme w = (FloatWme) wobj;
                            w.update(Double.valueOf(e.getValue()));
                        }
                    }
                    catch (NumberFormatException ex)
                    {
                        if (wobj instanceof IntWme)
                            ((IntWme) wobj).destroy();
                        else if (wobj instanceof FloatWme)
                            ((FloatWme) wobj).destroy();
                        createProp(e.getKey(), e.getValue());
                    }
                }
            }
        }

        if (!o.getPose().equals(pointData.getPose()))
        {
            pointData.destroyChildren();
            pointData = new PointWithDistanceIL(getRoot(), agent, o.getPose());
        }
        pointData.update();

        if (invisibleTimestamp > 0)
        {
            invisibleTimestamp = 0;
            visible.update(IOConstants.TRUE);
        }

    }
    
    void setVisible(boolean setting)
    {
        visible.update(setting ? IOConstants.TRUE : IOConstants.FALSE);
    }

}
