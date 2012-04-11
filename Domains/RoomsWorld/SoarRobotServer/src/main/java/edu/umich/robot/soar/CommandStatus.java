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

import sml.Identifier;
import sml.StringElement;

/**
 * Commands have a number of status flags. Flags usually don't go away, for example, a flag can have both ACCEPTED
 * and EXECUTING at the same time.
 * 
 * @author voigtjr@gmail.com
 */
enum CommandStatus
{
    /**
     * The command has been recognized and accepted in to the system. Syntax is
     * ok, at least on a first pass. This is the first valid state a command
     * will have.
     */
    ACCEPTED,

    /**
     * The command has been accepted and is currently executing. Only commands
     * that take more than one cycle to complete can ever see this flag.
     */
    EXECUTING,

    /**
     * The command has finished processing with non-failure.
     */
    COMPLETE,
    
    /**
     * The command has failed for whatever reason.
     */
    ERROR,

    /**
     * The command was interrupted before it was able to complete and the result
     * may be partial success.
     */
    INTERRUPTED;

    private static final String STATUS = "status";

    StringElement addStatus(Identifier command)
    {
        return command.CreateStringWME(STATUS, this.toString().toLowerCase());
    }

    StringElement addStatus(Identifier command, String message)
    {
        command.CreateStringWME("message", message);
        return command.CreateStringWME(STATUS, this.toString().toLowerCase());
    }

    boolean isTerminated()
    {
        return this.equals(COMPLETE) || this.equals(INTERRUPTED)
                || this.equals(ERROR);
    }
}
