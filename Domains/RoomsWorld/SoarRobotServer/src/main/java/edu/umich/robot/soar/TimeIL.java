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

import java.util.concurrent.TimeUnit;

import edu.umich.robot.util.WallClock;
import edu.umich.soar.IntWme;

/**
 * @author voigtjr@gmail.com
 */
public class TimeIL extends InputLinkElement
{
    private final IntWme secondswme;

    private final IntWme microsecondswme;

    private final WallClock clock;
    
    TimeIL(SoarAgent agent)
    {
        super(agent, IOConstants.TIME, agent.getSoarAgent().GetInputLink());
        clock = agent.getWallClock();
        secondswme = IntWme.newInstance(getRoot(), IOConstants.SECONDS);
        microsecondswme = IntWme.newInstance(getRoot(), IOConstants.MICROSECONDS);
    }

    @Override
    public void update()
    {
        long mstime = clock.getMillis();
        long seconds = TimeUnit.SECONDS.convert(mstime, TimeUnit.MILLISECONDS); // truncates
        long milliseconds = (mstime % TimeUnit.MILLISECONDS.convert(1, TimeUnit.SECONDS));
        long microseconds = milliseconds * 1000;
        
        secondswme.update(seconds);
        microsecondswme.update(microseconds);
    }

}
