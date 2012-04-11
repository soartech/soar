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

import org.apache.commons.logging.Log;

import sml.Identifier;
import edu.umich.robot.util.events.RobotEvent;
import edu.umich.robot.util.events.RobotEventManager;

/**
 * <p>
 * Abstract base class for a command that takes more than one cycle to complete.
 * 
 * @author voigtjr@gmail.com
 */
abstract class AbstractMultipleCycleCommand extends AbstractEventCommand
{
    protected AbstractMultipleCycleCommand(Identifier id, RobotEventManager events, Log logger)
    {
        super(id, events, logger);
    }

    @Override
    void onEvent(RobotEvent event)
    {
        setStatus(CommandStatus.INTERRUPTED);
    }

    /**
     * Check to see if the command is complete.
     * 
     * @return true if whatever state the command was trying for is achieved.
     * @throws SoarCommandError
     */
    abstract boolean isComplete() throws SoarCommandError;

    @Override
    void update() throws SoarCommandError
    {
        if (getStatus() == CommandStatus.ACCEPTED)
        {
            setStatus(CommandStatus.EXECUTING);
            fireAndRegisterEvents();
        }

        if (isComplete())
        {
            unregisterEvents();
            setStatus(CommandStatus.COMPLETE);
        }
    }
    
}
