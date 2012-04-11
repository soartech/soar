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

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import net.java.games.input.Controller;
import net.java.games.input.ControllerEnvironment;

import com.google.common.util.concurrent.MoreExecutors;

/**
 * @author voigtjr@gmail.com
 */
public class GamepadJI
{
    public static GamepadJI newInstance()
    {
        try
        {
            return new GamepadJI();
        }
        catch (IllegalStateException e)
        {
        }
        return null;
    }

    private final ScheduledExecutorService schexec = MoreExecutors.getExitingScheduledExecutorService(new ScheduledThreadPoolExecutor(1));

    private Controller controller;

    public GamepadJI()
    {
        // TODO: save/restore old calibration
        ControllerEnvironment ce = ControllerEnvironment
                .getDefaultEnvironment();
        for (Controller c : ce.getControllers())
        {
            if (c.getType() == Controller.Type.GAMEPAD)
            {
                System.out.println("Set controller: " + c);

                this.controller = c;

                break;
            }
        }

        if (controller == null)
            throw new IllegalStateException("No gamepad.");

        schexec.scheduleAtFixedRate(thread, 500, 50, TimeUnit.MILLISECONDS);
    }

    private Map<String, ComponentHandler> handlers = new HashMap<String, ComponentHandler>();

    private final Runnable thread = new Runnable()
    {
        public void run()
        {
            controller.poll();
            for (ComponentHandler handler : handlers.values())
                handler.handle();
        }
    };

    public void addListener(String component, GamepadListener listener)
    {
        addListener(component, listener, 0.00f);
    }

    public void addListener(String component, GamepadListener listener,
            float deadZonePercent)
    {
        try
        {
            ComponentHandler handler = handlers.get(component);
            if (handler == null)
            {
                handler = new ComponentHandler(component, controller,
                        new AxisFilter(deadZonePercent));
                handlers.put(component, handler);
            }
            handler.addListener(listener);
        }
        catch (IllegalArgumentException e)
        {
            e.printStackTrace();
        }
    }

    public void setDeadZonePercent(String component, float deadZonePercent)
    {
        ComponentHandler handler = handlers.get(component);
        if (handler == null)
            throw new IllegalArgumentException("No such component: "
                    + component);

    }

    public void removeListener(String component, GamepadListener listener)
    {
        ComponentHandler handler = handlers.get(component);
        if (handler == null)
            return;
        handler.removeListener(listener);
        if (handler.isEmpty())
            handlers.remove(component);
    }

    public void shutdown()
    {
        schexec.shutdown();
    }

}
