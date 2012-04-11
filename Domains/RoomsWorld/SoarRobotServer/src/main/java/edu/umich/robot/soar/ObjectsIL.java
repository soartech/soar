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
import java.util.concurrent.TimeUnit;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import april.util.TimeUtil;

import com.google.common.collect.Lists;

import edu.umich.robot.metamap.VirtualObject;
import edu.umich.robot.util.properties.PropertyChangeEvent;
import edu.umich.robot.util.properties.PropertyListener;
import edu.umich.robot.util.properties.PropertyManager;

/**
 * Input link management of data representing objects in the world.
 * 
 * @author voigtjr@gmail.com
 */
public class ObjectsIL extends InputLinkElement
{
    private static final Log logger = LogFactory.getLog(ObjectsIL.class);

    private final Map<Integer, ObjectIL> objMap = new HashMap<Integer, ObjectIL>();

    private final Map<Integer, Long> invisibleTimestamps = new HashMap<Integer, Long>();

    private long lingerMillis;

    private final SoarAgent agent;

    public ObjectsIL(SoarAgent agent)
    {
        super(agent, IOConstants.OBJECTS, agent.getSoarAgent().GetInputLink());
        this.agent = agent;

        PropertyManager properties = agent.getProperties();
        properties.addListener(AgentProperties.OBJECT_LINGER_SECONDS, new PropertyListener<Integer>()
        {
            public void propertyChanged(PropertyChangeEvent<Integer> event)
            {
                updateLingerMillis(event.getNewValue());
            }
        });
        updateLingerMillis(properties.get(AgentProperties.OBJECT_LINGER_SECONDS));

        update();
    }

    private void updateLingerMillis(Integer seconds)
    {
        lingerMillis = TimeUnit.MILLISECONDS.convert(seconds, TimeUnit.SECONDS);
    }

    @Override
    public void update()
    {
        List<Integer> seen = Lists.newArrayListWithExpectedSize(objMap.size());
        
        List<VirtualObject> visibleObjects = agent.getRobotOutput().getVisibleObjects();
        
        for (VirtualObject o : visibleObjects)
        {
            Integer id = Integer.valueOf(o.getId());
            ObjectIL oil = objMap.get(id);
            if (oil == null)
            {
                oil = new ObjectIL(agent, o, getRoot());
                objMap.put(id, oil);
                if (logger.isDebugEnabled())
                    logger.debug("Object for " + id + " created.");
            }

            if (invisibleTimestamps.containsKey(id))
            {
                invisibleTimestamps.remove(id);
                oil.setVisible(true);
                if (logger.isDebugEnabled())
                    logger.debug("Object " + id + " went visible.");
            }
            
            oil.update();
            seen.add(o.getId());
        }

        for (Iterator<Entry<Integer, ObjectIL>> iter = objMap.entrySet() .iterator(); iter.hasNext();)
        {
            Entry<Integer, ObjectIL> e = iter.next();
            if (!seen.contains(e.getKey()))
            {
                Long it = invisibleTimestamps.get(e.getKey());
                if (it == null)
                {
                    invisibleTimestamps.put(e.getKey(), TimeUtil.utime());
                    e.getValue().setVisible(false);
                    if (logger.isDebugEnabled())
                        logger.debug("Object " + e.getKey() + " went invisible.");
                }
                else
                {
                    long ms = TimeUtil.utime() - it;

                    if (ms > lingerMillis)
                    {
                        if (logger.isDebugEnabled())
                        {
                            double sec = ms / (double)TimeUnit.MILLISECONDS.convert(1, TimeUnit.SECONDS);
                            logger.debug("Object " + e.getKey() + " went invalid (" + sec + " sec).");
                        }
                        invisibleTimestamps.remove(e.getKey());
                        e.getValue().destroy();
                        iter.remove();
                        continue;
                    }
                }

                e.getValue().update();
            }
        }

    }

}
