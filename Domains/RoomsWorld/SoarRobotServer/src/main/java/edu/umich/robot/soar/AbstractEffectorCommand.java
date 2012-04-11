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
import edu.umich.robot.RobotOutput;
import edu.umich.robot.events.control.AbstractEffectorEvent;
import edu.umich.robot.events.feedback.AbstractEffectorFeedbackEvent;
import edu.umich.robot.events.feedback.EffectorSuccessEvent;
import edu.umich.robot.util.events.RobotEvent;
import edu.umich.robot.util.events.RobotEventListener;

/**
 * <p>
 * This is an abstract base class for effector commands. These commands listen for effector feedback
 * to finish the command and to set the status code and message.
 * @author voigtjr@gmail.com
 */
public abstract class AbstractEffectorCommand extends AbstractMultipleCycleCommand
{
    private AbstractEffectorFeedbackEvent status;

    private final RobotOutput output;

    public AbstractEffectorCommand(Identifier wme, SoarAgent agent, Log logger)
    {
        super(wme, agent.getEvents(), logger);
        this.output = agent.getRobotOutput();
        
        output.addFeedbackEventListener(AbstractEffectorFeedbackEvent.class, listener);
    }
    
    RobotEventListener listener = new RobotEventListener()
    {
        public void onEvent(RobotEvent event)
        {
            AbstractEffectorFeedbackEvent ae = (AbstractEffectorFeedbackEvent) event;
            if (status == null && isCorresponding(ae.getSource()))
                status = ae;
        }
    };
    
    protected abstract boolean isCorresponding(AbstractEffectorEvent event);

    @Override
    boolean isComplete() throws SoarCommandError
    {
        if (status == null)
            return false;
        
        if (status instanceof EffectorSuccessEvent)
            return true;
        
        throw new SoarCommandError("Effector error");
    }
    
    @Override
    void dispose()
    {
        output.removeFeedbackEventListener(AbstractEffectorFeedbackEvent.class, listener);
        super.dispose();
    }

}
