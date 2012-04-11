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
import edu.umich.robot.util.events.RobotEvent;
import edu.umich.robot.util.events.RobotEventManager;

/**
 * <p>
 * This is an abstract base class for commands that are OK and intended to be
 * interrupted. These are usually commands that would continue forever such as
 * "drive forward".
 * 
 * <p>
 * If there is any event, the command is finished with status complete.
 * 
 * @author voigtjr@gmail.com
 */
abstract class AbstractCompleteOnInterruptCommand extends AbstractEventCommand
{
    private final List<EventInfo> eventInstances = new ArrayList<EventInfo>(2);

    protected AbstractCompleteOnInterruptCommand(Identifier id, RobotEventManager events, Log logger)
    {
        super(id, events, logger);
    }

    void onEvent(RobotEvent event)
    {
        setStatus(CommandStatus.COMPLETE);
    }

    @Override
    public void update()
    {
        assert !eventInstances.isEmpty();
        if (getStatus() == CommandStatus.ACCEPTED)
        {
            setStatus(CommandStatus.EXECUTING);
            fireAndRegisterEvents();
        }
    }

}
