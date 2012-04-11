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
package edu.umich.robot.util.soar;

import sml.Identifier;
import edu.umich.robot.soar.AgentProperties;
import edu.umich.robot.util.properties.PropertyManager;
import edu.umich.soar.FloatWme;
import edu.umich.soar.Wme;

/**
 * Automatic conversion of units for angular speed working memory elements.
 * 
 * @author voigtjr@gmail.com
 */
public class AngSpeedWme implements Wme
{
    public static AngSpeedWme newInstance(Identifier parent, String attr, PropertyManager properties)
    {
        return new AngSpeedWme(parent, attr, properties);
    }

    public static AngSpeedWme newInstance(Identifier parent, String attr, double speed, PropertyManager properties)
    {
        AngSpeedWme temp = new AngSpeedWme(parent, attr, properties);
        temp.update(speed);
        return temp;
    }

    private final PropertyManager properties;

    private final FloatWme wme;

    private AngSpeedWme(Identifier parent, String attr, PropertyManager properties)
    {
        wme = FloatWme.newInstance(parent, attr);
        this.properties = properties;
    }

    public void update(double speed)
    {
        wme.update(properties.get(AgentProperties.ANGULAR_SPEED_UNIT).toView(speed));
    }
    
    public void destroy()
    {
        wme.destroy();
    }
    
    @Override
    public String toString()
    {
        return wme.toString();
    }
}
