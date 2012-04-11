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
import edu.umich.robot.radio.Radio;
import edu.umich.robot.radio.RadioMessage;

/**
 * Send a communication message.
 * 
 * @author voigtjr@gmail.com
 */
public class SendMessageCommand extends OLCommand
{
    static final String NAME = "send-message";

    private final Radio radio;

    private final RadioMessage message;

    public SendMessageCommand(Identifier wme, SoarAgent agent)
            throws SoarCommandError
    {
        super(wme);
        this.radio = agent.getRobotOutput().getRadio();

        RadioMessage.Builder builder = new RadioMessage.Builder(agent.getName());

        // could be null, that's ok (broadcast)
        builder.destination(wme.GetParameterValue(IOConstants.DESTINATION));

        try
        {
            Identifier next = wme.FindByAttribute(IOConstants.FIRST, 0)
                    .ConvertToIdentifier();
            while (true)
            {
                String word = CommandParameters.requireString(next,
                        IOConstants.WORD);
                builder.token(word);

                // ^next can't be null and if it exists, will have a string
                // representation
                String nextnext = CommandParameters.requireString(next,
                        IOConstants.NEXT);
                // if that string representation is "nil", we stop
                if (nextnext.equals(IOConstants.NIL))
                {
                    break;
                }

                // otherwise, it must be an identifier to another word structure
                next = next.FindByAttribute(IOConstants.NEXT, 0)
                        .ConvertToIdentifier();
                if (next == null)
                    throw new SoarCommandError(IOConstants.NEXT
                            + " is not identifier");
            }
        }
        catch (NullPointerException e)
        {
            throw new SoarCommandError("malformed message");
        }

        message = builder.build();

        if (message == null)
            throw new SoarCommandError("no words for message");
    }

    @Override
    void update()
    {
        radio.postRadioMessage(message);
        setStatus(CommandStatus.COMPLETE);
    }
}
