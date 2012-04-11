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
package edu.umich.robot.radio;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;

/**
 * <p>
 * Radio message data for transmission in simulated radio.
 * 
 * <p>
 * Immutable.
 * 
 * @author voigtjr
 */
public class RadioMessage
{
    private static final AtomicLong counter = new AtomicLong();

    /**
     * Create a new one from these arguments.
     * 
     * @param from
     *            Sender
     * @param destination
     *            Receiver
     * @param tokens
     *            The message
     * @return
     */
    static RadioMessage newCommunication(String from, String destination,
            List<String> tokens)
    {
        return new RadioMessage(from, destination, tokens, new HashMap<String, Object>());
    }

    /**
     * Create a new message for broadcast to all recipients.
     * 
     * @param from
     *            Sender
     * @param tokens
     *            The message
     * @return
     */
    static RadioMessage newCommunication(String from, List<String> tokens)
    {
        return newCommunication(from, null, tokens);
    }

    public static class Builder
    {
        private final String from;

        private final List<String> tokens = new ArrayList<String>();

        private String destination;
        
        private final Map<String, Object> data = new HashMap<String, Object>();

        public Builder(String from)
        {
            this.from = from;
        }

        public Builder destination(String destination)
        {
            this.destination = destination;
            return this;
        }

        public Builder token(String token)
        {
            tokens.add(token);
            return this;
        }
        
        public Builder tokens(List<String> tokens)
        {
        	this.tokens.addAll(tokens);
            return this;
        }
        
        public Builder data(String key, Object value)
        {
        	data.put(key, value);
        	return this;
        }

        public RadioMessage build()
        {
            Iterator<String> iter = tokens.iterator();
            while (iter.hasNext())
            {
                String token = iter.next();
                if (token.length() == 0)
                {
                    iter.remove();
                }
            }

            if (tokens.isEmpty())
                return null;

            return new RadioMessage(from, destination, tokens, data);
        }
    }

    private final long id;

    private final String from;

    private final List<String> tokens;

    private final String destination;
    
    private final Map<String, Object> data;
    
    public RadioMessage(String from, String destination, List<String> tokens, Map<String, Object> data)
    {
        this.id = counter.getAndIncrement();
        this.from = from;
        this.destination = destination;
        this.tokens = Collections
                .unmodifiableList(new ArrayList<String>(tokens));
        this.data = Collections.unmodifiableMap(data);
    }

    /**
     * Who the message is targeted at. Null means everyone.
     * 
     * @return
     */
    public String getDestination()
    {
        return destination;
    }

    /**
     * Who the message is from, usually a robot name.
     * 
     * @return
     */
    public String getFrom()
    {
        return from;
    }

    /**
     * Unique identifier across messages.
     * 
     * @return
     */
    public long getId()
    {
        return id;
    }

    /**
     * The message.
     * 
     * @return
     */
    public List<String> getTokens()
    {
        return tokens;
    }
    
    public String getConcatenatedTokens(String join)
    {
		StringBuilder b = new StringBuilder();
		for (String s : tokens) {
			b.append(s);
			b.append(join);
		}
		String message = (b.length() > 0) ? b.substring(0, b.length() - join.length()) : b.toString();
		return message;
    }

    /**
     * True if there is no destination and it should go to all listeners.
     * @return
     */
    public boolean isBroadcast()
    {
        return destination == null;
    }
}
