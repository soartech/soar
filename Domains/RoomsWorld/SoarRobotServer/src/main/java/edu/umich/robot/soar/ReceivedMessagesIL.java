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

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentMap;

import sml.Identifier;

import com.google.common.collect.Lists;

import edu.umich.robot.radio.RadioMessage;
import edu.umich.soar.FloatWme;
import edu.umich.soar.IntWme;
import edu.umich.soar.StringWme;

/**
 * Input link management of received messages.
 * 
 * @author voigtjr@gmail.com
 */
public class ReceivedMessagesIL extends InputLinkElement
{
    private class MessageIL
    {
        private final Identifier messageRoot;

        private MessageIL(RadioMessage message)
        {
            messageRoot = getRoot().CreateIdWME(IOConstants.MESSAGE);

            StringWme.newInstance(messageRoot, IOConstants.FROM, message
                    .getFrom());
            IntWme.newInstance(messageRoot, IOConstants.ID, (int) message
                    .getId());
            Identifier next = null;
            for (String word : message.getTokens())
            {
                // first one is "first", rest are next
                next = (next == null) ? messageRoot
                        .CreateIdWME(IOConstants.FIRST) : next
                        .CreateIdWME(IOConstants.NEXT);
                // Make each owrd an int or float wme if possible.
                // TODO make more efficient? regexs?
                try
                {
                	int ival = Integer.parseInt(word);
                	IntWme.newInstance(next, IOConstants.WORD, ival);
                }
                catch (NumberFormatException e)
                {
                	try
                	{
                		double dval = Double.parseDouble(word);
                		FloatWme.newInstance(next, IOConstants.WORD, dval);
                	}
                	catch (NumberFormatException ex)
                	{
                		StringWme.newInstance(next, IOConstants.WORD, word);
                	}
                }
            }
            StringWme.newInstance(next, IOConstants.NEXT, IOConstants.NIL);
        }

        private void destroy()
        {
            messageRoot.DestroyWME();
        }
    }

    private final Map<Long, MessageIL> all = new HashMap<Long, MessageIL>();

    private final ConcurrentMap<Long, RadioMessage> messages;

    public ReceivedMessagesIL(SoarAgent agent)
    {
        super(agent, IOConstants.RECEIVED_MESSAGES, agent.getSoarAgent().GetInputLink());

        messages = agent.getRadioMessages();
        
        update();
    }

    @Override
    public void update()
    {
        List<Long> seen = Lists.newArrayListWithExpectedSize(messages.size());
        for (Entry<Long, RadioMessage> e : messages.entrySet())
        {
            MessageIL m = all.get(e.getKey());
            if (m == null)
                all.put(e.getKey(), new MessageIL(e.getValue()));
            seen.add(e.getKey());
        }

        for (Iterator<Entry<Long, MessageIL>> iter = all.entrySet().iterator(); iter
                .hasNext();)
        {
            Entry<Long, MessageIL> e = iter.next();
            if (!seen.contains(e.getKey()))
            {
                e.getValue().destroy();
                iter.remove();
            }
        }
    }

}
