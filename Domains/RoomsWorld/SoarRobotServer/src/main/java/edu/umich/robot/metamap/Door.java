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
package edu.umich.robot.metamap;

import java.util.List;

import com.google.common.collect.ImmutableList;

/**
 * Represents a door that can be opened, closed, locked. Note that this
 * represents a physical door and not the area that makes up the room that the
 * door is in. The area that contains a door is simply tagged with a door
 * property and is otherwise a normal area description.
 * 
 * @author voigtjr@gmail.com
 */
public class Door
{
    public enum State
    {
        OPEN, CLOSED, LOCKED
    }
    
    /**
     * Doors start open.
     */
    private State state = State.OPEN;
    
    /**
     * -1 means no locking code.
     */
    private int code = -1;
    
    /**
     * This is the location of the door in meters, x, y, width, height.
     */
    private final List<Double> xywh;
    
    /**
     * Globally unique door identifier number.
     */
    private final int id;
    
    public Door(int id, List<Double> xywh)
    {
        this.id = id;
        this.xywh = new ImmutableList.Builder<Double>()
        .addAll(xywh)
        .build();
    }
    
    /**
     * Exact copy, same id.
     * 
     * @return
     */
    Door copy()
    {
        Door ret = new Door(id, xywh);
        ret.state = state;
        ret.code = code;
        return ret;
    }
    
    /**
     * Check if the door is open, closed, locked.
     * 
     * @return
     */
    public synchronized State getState()
    {
        return state;
    }
    
    /**
     * Set the door to be open, closed, locked.
     * 
     * @param state
     */
    synchronized void setState(State state)
    {
        this.state = state;
    }
    
    /**
     * Get the door's lock code.
     * 
     * @return
     */
    public synchronized int getCode()
    {
        return code;
    }
    
    /**
     * Set the door's lock code.
     * 
     * @param code
     */
    synchronized void setCode(int code)
    {
        this.code = code;
    }
    
    /**
     * Find out where the door is.
     * 
     * @return x, y, width, height
     */
    public synchronized List<Double> getxywh()
    {
        return xywh;
    }

    /**
     * Get the door's global identifier number.
     * 
     * @return
     */
    public synchronized int getId()
    {
        return id;
    }
    
}
