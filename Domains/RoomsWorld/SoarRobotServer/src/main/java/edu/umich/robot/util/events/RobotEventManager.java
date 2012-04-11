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
package edu.umich.robot.util.events;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Taken from jSoar, modified.
 * 
 * @author ray
 * @author voigtjr@gmail.com
 */
public class RobotEventManager
{
    /**
     * List of listeners for events of any type
     */
    private final List<RobotEventListener> listenersForAny = new CopyOnWriteArrayList<RobotEventListener>();

    /**
     * Listeners indexed by event type
     */
    private Map<Class<? extends RobotEvent>, List<RobotEventListener>> listeners = Collections
            .synchronizedMap(new HashMap<Class<? extends RobotEvent>, List<RobotEventListener>>());

    public RobotEventManager()
    {
    }

    /**
     * Add a listener for a particular type of event. A listener may be
     * registered for more than one type of event.
     * 
     * <p>
     * It is safe to call this method from any thread, or from an event listener
     * callback.
     * 
     * @param <T>
     *            The type of event
     * @param klass
     *            The class of event. If null of RobotEvent, then the listener
     *            will receive all events.
     * @param listener
     *            The listener
     * @throws NullPointerException
     *             if listener is <code>null</code>
     */
    public <T extends RobotEvent> void addListener(Class<T> klass,
            RobotEventListener listener)
    {
        if (listener == null)
        {
            throw new NullPointerException("listener");
        }
        getListenersForEventType(klass).add(listener);
    }

    /**
     * Remove a listener previously added with
     * {@link #addListener(Class, RobotEventListener)}.
     * 
     * <p>
     * It is safe to call this method from any thread, or from an event listener
     * callback.
     * 
     * @param <T>
     *            The event type
     * @param klass
     *            The class of the event type to remove the listener from, or
     *            <code>null</code> to completely remove the listener from the
     *            manager.
     * @param listener
     *            The listener to remove
     */
    public <T extends RobotEvent> void removeListener(Class<T> klass,
            RobotEventListener listener)
    {
        if (klass == null)
        {
            listenersForAny.remove(listener);
            synchronized (listeners)
            {
                for (List<RobotEventListener> list : listeners.values())
                {
                    list.remove(listener);
                }
            }
        }
        else
        {
            getListenersForEventType(klass).remove(listener);
        }
    }

    /**
     * Fire the given event to all listeners.
     * 
     * <p>
     * It is safe to call this method from any thread, but may negatively affect
     * listeners that expect to be called on a particular thread.
     * 
     * <p>
     * It is safe to call this method from an event listener callback.
     * 
     * @param <T>
     *            The event type
     * @param event
     *            The event object. Must be non-null.
     * @throws NullPointerException
     *             if event is <code>null</code>.
     */
    public <T extends RobotEvent> void fireEvent(T event)
    {
        fireEvent(event, event.getClass());
    }

    /**
     * Fire the given event to all listeners registered for the given event
     * type. This extended version of {@link #fireEvent(RobotEvent)} allows a
     * particular event class to be specified so that sub-classes of an existing
     * event class can be properly routed to listeners of that parent class.
     * 
     * @param <T>
     *            Event type
     * @param event
     *            The event object. Must be non-null.
     * @param eventType
     *            Event type used to route to listeners
     */
    public <T extends RobotEvent> void fireEvent(T event,
            Class<? extends T> eventType)
    {
        if (event == null)
        {
            throw new NullPointerException("event");
        }

        for (RobotEventListener l : getListenersForEventType(eventType))
        {
            l.onEvent(event);
        }
        for (RobotEventListener l : listenersForAny)
        {
            l.onEvent(event);
        }
    }

    private <T extends RobotEvent> List<RobotEventListener> getListenersForEventType(
            Class<T> klass)
    {
        if (klass == null || klass.equals(RobotEvent.class))
        {
            return listenersForAny;
        }
        synchronized (listeners)
        {
            List<RobotEventListener> list = listeners.get(klass);
            if (list == null)
            {
                list = new CopyOnWriteArrayList<RobotEventListener>();
                listeners.put(klass, list);
            }
            return list;
        }
    }
}
