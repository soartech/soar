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
package edu.umich.robot.gp;

import java.util.ArrayList;
import java.util.List;

import net.java.games.input.Component;
import net.java.games.input.Controller;

/**
 * @author voigtjr@gmail.com
 */
class ComponentHandler
{
    private final Component component;

    private final String id;

    private final AxisFilter af;

    private float value = 0;

    private final List<GamepadListener> listeners = new ArrayList<GamepadListener>();

    ComponentHandler(String id, Controller controller, AxisFilter af)
    {
        this.id = id;
        this.component = getComponent(id, controller);
        this.af = af;
    }

    private static Component getComponent(String id, Controller controller)
    {
        try
        {
            int buttonId = Integer.parseInt(id);
            return controller.getComponents()[buttonId];
        }
        catch (NumberFormatException e)
        {
        }

        return controller.getComponent(AxisMapping.valueOf(id).getAxis());
    }

    public void setDeadZonePercent(float deadZonePercent)
    {
        af.setDeadZonePercent(deadZonePercent);
    }

    void handle()
    {
        float pollValue = component.getPollData();

        if (component.isAnalog())
            pollValue = af.filter(pollValue);

        if (Float.compare(value, pollValue) == 0)
            return;

        value = pollValue;
        fireEvent();
    }

    private void fireEvent()
    {
        for (GamepadListener listener : listeners)
            listener.stateChanged(id, value);
    }

    void addListener(GamepadListener listener)
    {
        listeners.add(listener);
    }

    void removeListener(GamepadListener listener)
    {
        listeners.remove(listener);
    }

    boolean isEmpty()
    {
        return listeners.isEmpty();
    }
}
