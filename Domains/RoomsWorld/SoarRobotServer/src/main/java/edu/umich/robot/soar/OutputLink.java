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

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import sml.Identifier;
import sml.WMElement;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

/**
 * Main output link interface. Manually watches for new commands and manages
 * their state.
 * 
 * @author voigtjr@gmail.com
 */
public class OutputLink
{
    private static final Log logger = LogFactory.getLog(OutputLink.class);

    /**
     * Create a new output link manager for the passed agent.
     * 
     * @param agent
     * @return
     */
    static OutputLink newInstance(SoarAgent agent)
    {
        return new OutputLink(agent);
    }

    /**
     * Maps an output link command with a status. The status is also stored in
     * the command so I forget why this is here. I think it is because the
     * command can become null or invalid or something.
     * 
     * @author voigtjr
     */
    private static class OLCommandPair
    {
        final OLCommand command;

        CommandStatus status = CommandStatus.ACCEPTED;

        public OLCommandPair(OLCommand command)
        {
            this.command = command;
        }
    }

    private final OLCommandManager cm;

    private final Identifier ol;

    private final Map<Long, OLCommandPair> commands = Maps.newHashMap();

    /**
     * This turns off output-link change tracking, the changes are all tracked
     * manually.
     * 
     * @param agent
     */
    public OutputLink(SoarAgent agent)
    {
        this.ol = agent.getSoarAgent().GetOutputLink();
        this.cm = new OLCommandManager(agent);
        
        agent.getSoarAgent().SetOutputLinkChangeTracking(false);
    }

    /**
     * Update the output link looking for and instantiating new commands.
     */
    void update()
    {
        // walk output link, creating and updating, marking keeps
        int children = ol.GetNumberChildren();
        
        if (logger.isTraceEnabled())
            logger.trace("OutputLink children: " + children);
        
        List<Long> seen = Lists.newArrayListWithExpectedSize(children);
        List<OLCommandPair> multicycles = Lists.newArrayListWithExpectedSize(children);
        
        for (int i = 0; i < children; ++i)
        {
            WMElement wme = ol.GetChild(i);
            Identifier id = wme.ConvertToIdentifier();
            if (id == null)
                continue;

            Long tt = Long.valueOf(wme.GetTimeTag());
            seen.add(tt);
            if (logger.isTraceEnabled())
                logger.trace("Seen tt: " + tt);

            OLCommandPair pair = commands.get(tt);
            if (pair != null)
            {
                // do new commands first so that they cancel older commands
                multicycles.add(pair);
                if (logger.isTraceEnabled())
                    logger.trace("Added to multicycles: " + pair.command);
                continue;
            }
            
            try
            {
                pair = new OLCommandPair(cm.newInstance(id));
                if (pair.command == null)
                    continue;

                commands.put(tt, pair);
                processCommand(pair);
            }
            catch (SoarCommandError e)
            {
                logger.error("Error with tt:" + e.getMessage());
            }
        }

        logger.trace("Processing multicycles");
        // do older commands next so that new commands cancel them
        for (OLCommandPair pair : multicycles)
        {
            if (logger.isTraceEnabled())
                logger.trace("Processing multicycle: " + pair.command);
            processCommand(pair);
        }        
        
        // remove stale commands
        logger.trace("Processing stales");
        for (Iterator<Entry<Long, OLCommandPair>> iter = commands.entrySet()
                .iterator(); iter.hasNext(); )
        {
            Entry<Long, OLCommandPair> entry = iter.next();
            if (!seen.contains(entry.getKey()))
            {
                logger.trace("disposing " + entry.getKey());
                entry.getValue().command.dispose();
                iter.remove();
            }
        }
        logger.trace("OL Update done");
    }
    
    private void processCommand(OLCommandPair pair)
    {
        if (!pair.status.isTerminated())
        {
            Long tt = pair.command.getTimeTag();
            Identifier id = pair.command.getId();
            
            try
            {
                logger.trace("updating " + tt);
                pair.command.update();
                if (pair.status != pair.command.getStatus())
                {
                    pair.status = pair.command.getStatus();
                    pair.status.addStatus(id);
                }
            }
            catch (SoarCommandError e)
            {
                logger.trace("error: " + e.getMessage());
                pair.status = CommandStatus.ERROR;
                pair.status.addStatus(id, e.getMessage());
            }

        }
    }

    void destroy()
    {
        Iterator<Map.Entry<Long, OLCommandPair>> iter = commands.entrySet()
                .iterator();
        while (iter.hasNext())
        {
            Map.Entry<Long, OLCommandPair> entry = iter.next();
            entry.getValue().command.dispose();
            iter.remove();
        }
    }

    void stopEvent()
    {
        for (OLCommandPair p : commands.values())
            p.command.stopEvent();
    }

    void startEvent()
    {
        for (OLCommandPair p : commands.values())
            p.command.startEvent();
    }

}
