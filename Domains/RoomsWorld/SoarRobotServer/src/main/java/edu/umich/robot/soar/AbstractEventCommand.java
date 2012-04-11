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

import org.apache.commons.logging.Log;

import sml.Identifier;
import edu.umich.robot.events.control.AbstractControlEvent;
import edu.umich.robot.util.events.RobotEvent;
import edu.umich.robot.util.events.RobotEventListener;
import edu.umich.robot.util.events.RobotEventManager;

/**
 * <p>
 * Abstract base class for a command that fires an event in order to issue the
 * command it represents. This also registers for events on the same manager in
 * case that other fired events could stop or change the behavior of this
 * command.
 * 
 * <p>
 * Override the onEvent to check for other events that fire after this one.
 * 
 * @author voigtjr@gmail.com
 */
abstract class AbstractEventCommand extends OLCommand
{
    private final Log logger;

    private final RobotEventManager events;

    private final List<EventInfo> eventInstances = new ArrayList<EventInfo>(2);

    protected AbstractEventCommand(Identifier id, RobotEventManager events, Log logger)
    {
        super(id);
        this.events = events;
        this.logger = logger;
    }
    
    /**
     * Override to receive other events that fire after this one, in case it is
     * necessary to cancel or finish this command if another conflicting command fires.
     * @param event
     */
    abstract void onEvent(RobotEvent event);
    
    /**
     * Add an event to fire when executing the command. There may be more than
     * one event to fire.
     * 
     * @param event
     * @param klass
     */
    protected void addEvent(AbstractControlEvent event, Class<? extends AbstractControlEvent> klass)
    {
        assert event != null;
        this.eventInstances.add(new EventInfo(event, klass));
    }

    @Override
    void stopEvent()
    {
        unregisterEvents();
    }
    
    @Override
    void startEvent()
    {
        fireAndRegisterEvents();
    }
    
    /**
     * Fires events and registers for the same events.
     */
    protected void fireAndRegisterEvents()
    {
        for (EventInfo e : eventInstances)
        {
            logger.debug("Firing " + e.getEvent());
            events.fireEvent(e.getEvent(), e.getEventClass());
        }
        for (EventInfo e : eventInstances)
            events.addListener(e.getEventClass(), listener);
    }
    
    /**
     * Listener for the robot events that fire after this one.
     */
    RobotEventListener listener = new RobotEventListener() {
        public void onEvent(RobotEvent event)
        {
            AbstractEventCommand.this.onEvent(event);
        }
    };
    
    @Override
    void dispose()
    {
        unregisterEvents();
    }

    /**
     * Removes listeners.
     */
    protected void unregisterEvents()
    {
        for (EventInfo e : eventInstances)
            events.removeListener(e.getEventClass(), listener);
    }

    /* (non-Javadoc)
     * @see edu.umich.robot.soar.OLCommand#update()
     */
    @Override
    void update() throws SoarCommandError
    {
        // Overriding this is optional, depends on the nature of the event.
    }

    @Override
    public String toString()
    {
        StringBuilder sb = new StringBuilder(toStringPrefix());
        for (EventInfo e : eventInstances)
        {
            sb.append(", ");
            sb.append(e.getEvent());
        }
        sb.append(")");
        return sb.toString();
    }

}
