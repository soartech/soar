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

import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import sml.Identifier;

import com.google.common.collect.Lists;

import edu.umich.robot.laser.Laser;
import edu.umich.robot.util.soar.DistanceWme;
import edu.umich.robot.util.soar.YawWme;
import edu.umich.soar.IntWme;

/**
 * Input link management of laser data.
 * 
 * @author voigtjr@gmail.com
 */
public class LidarIL extends InputLinkElement
{
    private static final Log logger = LogFactory.getLog(LidarIL.class);

    private class Range
    {
        private final Identifier rangewme;

        private final DistanceWme distancewme;

        private boolean valid = true;

        private Range(int id, double relativeBearing)
        {
            rangewme = getRoot().CreateIdWME(IOConstants.RANGE);
            IntWme.newInstance(rangewme, IOConstants.ID, id);
            YawWme.newInstance(rangewme, IOConstants.RELATIVE_BEARING, relativeBearing, agent.getProperties());
            distancewme = DistanceWme.newInstance(rangewme, IOConstants.DISTANCE, agent.getProperties());

            logger.debug(String.format(
                    "Created range %d relative-bearing %1.3f", id,
                    relativeBearing));
        }

        private void update(double distance)
        {
            if (!valid)
            {
                throw new IllegalStateException();
            }
            distancewme.update(distance);
        }

        private void destroy()
        {
            valid = false;
            rangewme.DestroyWME();
        }
    }

    private final SoarAgent agent;

    private final List<Range> ranges;

    LidarIL(SoarAgent agent)
    {
        super(agent, IOConstants.LIDAR, agent.getSoarAgent().GetInputLink());
        this.agent = agent;
        ranges = Lists.newArrayListWithExpectedSize(5);
        update();
    }

    @Override
    public void update()
    {
        Laser laser = agent.getRobotOutput().getLaser();
        if (laser == null)
        {
            logger.debug("laser from robot is null");
            return;
        }

        if (ranges.size() != laser.getRanges().size())
        {
            logger.debug("creating ranges");
            createRanges(laser);
        }

        for (int index = 0; index < laser.getRanges().size(); ++index)
        {
            if (logger.isTraceEnabled())
            {
                logger.trace(String.format(
                        "range index %d distance update %1.3f", index, laser
                                .getRanges().get(index)));
            }
            ranges.get(index).update(laser.getRanges().get(index));
        }
    }

    private void createRanges(Laser laser)
    {
        for (Range range : ranges)
        {
            logger.trace("destroying old ranges");
            range.destroy();
        }

        ranges.clear();

        double relativeBearing = laser.getRad0();
        for (int index = 0; index < laser.getRanges().size(); ++index, relativeBearing += laser
                .getRadStep())
        {
            Range range = new Range(index - (laser.getRanges().size() / 2),
                    relativeBearing);
            ranges.add(range);
        }
    }

}
