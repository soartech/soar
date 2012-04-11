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

/**
 * Various state variables for a room. A lot of this is unimplemented or
 * untested.
 * 
 * @author voigtjr@gmail.com
 */
public class AreaState
{
    private boolean isRoomLightBroken = false;
    private boolean roomLightOn = true;
    private boolean hasWindows = false;
    private boolean areWindowShadesBroken = false;
    private boolean windowShadesActivated = false;
    
    /**
     * Returns true if the light in the room is malfunctioning.
     * 
     * @param broken
     */
    void setRoomLightBroken(boolean broken)
    {
        this.isRoomLightBroken = broken;
    }
    
    /**
     * Returns true if the room light is on.
     * 
     * @param on
     */
    void setRoomLightOn(boolean on)
    {
        if (isRoomLightBroken)
            return;
        this.roomLightOn = on;
    }
    
    /**
     * Returns true if the room has windows.
     * 
     * @param hasWindows
     */
    void setHasWindows(boolean hasWindows)
    {
        this.hasWindows = hasWindows;
    }
    
    /**
     * Returns true if the room's window shades are broken.
     * 
     * @param broken
     */
    void setWindowShadesBroken(boolean broken)
    {
        this.areWindowShadesBroken = broken;
    }
    
    /**
     * Returns true if the window shades are down.
     * 
     * @param activated
     */
    void setWindowShadesActivated(boolean activated)
    {
        if (areWindowShadesBroken)
            return;
        this.windowShadesActivated = activated;
    }
    
    /**
     * Returns true if the room has windows.
     * 
     * @return
     */
    public boolean hasWindows()
    {
        return hasWindows;
    }
    
    /**
     * Returns true if there is any light in the room. Does not check robot headlights.
     * 
     * @param daytime
     * @return
     */
    public boolean isLit(boolean daytime)
    {
        return roomLightOn || (daytime && hasWindows && !windowShadesActivated);
    }

    /**
     * Returns true if the room light is on.
     * 
     * @return
     */
    public boolean isRoomLightOn()
    {
        return roomLightOn;
    }
}
