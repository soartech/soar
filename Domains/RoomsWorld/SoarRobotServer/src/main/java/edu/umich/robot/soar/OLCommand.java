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
import org.apache.commons.logging.LogFactory;

import sml.Identifier;

/**
 * Base class for output link commands. Different commands will likely extend other abstract
 * base classes in this heirarchy. 
 * @author voigtjr@gmail.com
 */
abstract class OLCommand
{
    private static final Log logger = LogFactory.getLog(OLCommand.class);
    
    private final Identifier id;

    private final String name;

    private final long tt;

    private CommandStatus status = CommandStatus.ACCEPTED;

    private String statusMessage;

    protected OLCommand(Identifier id)
    {
        this.id = id;

        // cache these so they are accessible after the wme is invalid
        name = id.GetAttribute();
        tt = id.GetTimeTag();
    }

    /**
     * Some commands have a lot to print out before the closing brace, this
     * separates the first part from the closing brace. Super-classes can
     * override to string and call this before calling their own stuff.
     * 
     * @return
     */
    protected String toStringPrefix()
    {
        StringBuilder sb = new StringBuilder("(");
        sb.append(name);
        sb.append(": ");
        sb.append(tt);
        return sb.toString();
    }

    @Override
    public String toString()
    {
        return toStringPrefix() + ")";
    }

    /**
     * Time tag of the main command wme.
     * 
     * @return
     */
    public Long getTimeTag()
    {
        return tt;
    }

    /**
     * Soar identifier for the command wme.
     * 
     * @return
     */
    protected Identifier getId()
    {
        return id;
    }

    /**
     * Status for the command.
     * 
     * @param status
     */
    protected void setStatus(CommandStatus status)
    {
        assert status != null;
        logger.debug(tt + ": " + status);
        this.status = status;
    }

    /**
     * Set status and a message to help with agent debugging. Usually used for
     * errors.
     * 
     * @param status
     * @param message
     */
    protected void setStatus(CommandStatus status, String message)
    {
        setStatus(status);
        logger.debug(tt + ": " + message);
        this.statusMessage = message;
    }

    @Override
    public boolean equals(Object obj)
    {
        if (obj instanceof OLCommand)
        {
            OLCommand olc = (OLCommand) obj;
            return tt == olc.tt;
        }
        return super.equals(obj);
    }

    @Override
    public int hashCode()
    {
        return Long.valueOf(tt).hashCode();
    }

    abstract void update() throws SoarCommandError;

    /**
     * Called when Soar stops, override to do something interesting at this
     * point.
     */
    void stopEvent()
    {
    }

    /**
     * Called when Soar starts, override to do something interesting at this
     * point. This is useful for re-firing drive commands after a stop so that
     * the robot gets moving again.
     */
    void startEvent()
    {
    }

    /**
     * Get rid of the command and any resources that need to be disposed.
     */
    void dispose()
    {
    }

    /**
     * Returns the last set status.
     * 
     * @return
     */
    public CommandStatus getStatus()
    {
        return status;
    }

    /**
     * Returns the status message if any.
     * 
     * @return
     */
    public String getStatusMessage()
    {
        return statusMessage;
    }
}
