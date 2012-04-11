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

import org.apache.commons.logging.LogFactory;

import sml.Identifier;
import edu.umich.robot.events.control.AbstractEffectorEvent;
import edu.umich.robot.events.control.EffectorDiffuseObjectByWireEvent;

/**
 * Domain specific command to diffuse an object using a wire.
 * 
 * @author voigtjr@gmail.com
 */
public class DiffuseObjectByWireCommand extends AbstractEffectorCommand
{
    static final String NAME = "diffuse-object-by-wire";

    private final int id;
    
    private final String color;
    
    public DiffuseObjectByWireCommand(Identifier wme, SoarAgent agent)
            throws SoarCommandError
    {
        super(wme, agent, LogFactory.getLog(DiffuseObjectByWireCommand.class));

        id = CommandParameters.requireInteger(wme, IOConstants.ID);
        color = CommandParameters.requireString(wme, IOConstants.COLOR);

        addEvent(new EffectorDiffuseObjectByWireEvent(id, color),
                AbstractEffectorEvent.class);
    }

    @Override
    protected boolean isCorresponding(AbstractEffectorEvent event)
    {
        if (event instanceof EffectorDiffuseObjectByWireEvent)
        {
            EffectorDiffuseObjectByWireEvent e = (EffectorDiffuseObjectByWireEvent)event;
            return e.getId() == id && e.getColor().equals(color);
        }
        return false;
    }
}
