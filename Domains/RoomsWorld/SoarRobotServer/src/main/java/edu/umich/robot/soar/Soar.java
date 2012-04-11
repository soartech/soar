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

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import april.config.Config;

import sml.Agent;
import sml.Kernel;
import sml.smlAgentEventId;
import sml.smlSystemEventId;
import sml.smlUpdateEventId;
import sml.Kernel.AgentEventInterface;
import sml.Kernel.SystemEventInterface;
import sml.Kernel.UpdateEventInterface;
import edu.umich.robot.RobotController;
import edu.umich.robot.RobotOutput;
import edu.umich.robot.events.AfterResetEvent;
import edu.umich.robot.events.BeforeResetEvent;
import edu.umich.robot.events.control.AbstractDriveEvent;
import edu.umich.robot.events.control.DriveEStopEvent;
import edu.umich.robot.radio.RadioHandler;
import edu.umich.robot.radio.RadioMessage;
import edu.umich.robot.util.WallClock;
import edu.umich.robot.util.events.RobotEvent;
import edu.umich.robot.util.events.RobotEventListener;
import edu.umich.robot.util.properties.PropertyManager;

/**
 * The main Soar interface class. Listens for reset events to init agents.
 * 
 * @author voigtjr@gmail.com
 */
public class Soar implements RobotEventListener, RadioHandler
{
    private static final Log logger = LogFactory.getLog(Soar.class);

    private final PropertyManager properties = new PropertyManager();

    private final Kernel kernel;

    private final List<SoarAgent> agents = new ArrayList<SoarAgent>();

    private final ExecutorService exec = Executors.newSingleThreadExecutor();

    private Future<?> soarTask;

    private final WallClock clock = new WallClock();
    
    private final SoarDataCollector data;
    
    public Soar(Config config)
    {
        data = new SoarDataCollector(clock, properties, config);
        // kernel = Kernel.CreateKernelInNewThread(Kernel.kUseAnyPort);
        kernel = Kernel.CreateKernelInNewThread();
        if (kernel.HadError())
            throw new IllegalStateException("Soar error: "
                    + kernel.GetLastErrorDescription());
        
        int port = kernel.GetListenerPort();
        
        while (port == -1)
        {
            try
            {
                Thread.sleep(20);
            }
            catch (InterruptedException e)
            {
                e.printStackTrace();
            }
            port = kernel.GetListenerPort();
        }

        System.out.println("Started Soar kernel on port: " + port);
        
        kernel.SetAutoCommit(false);
        kernel.SetTraceCommunications(false);

        kernel.RegisterForUpdateEvent(
                smlUpdateEventId.smlEVENT_AFTER_ALL_OUTPUT_PHASES,
                new UpdateEventInterface()
                {
                    public void updateEventHandler(int eventID, Object d,
                            Kernel kernel, int arg3)
                    {
                        logger.trace("smlEVENT_AFTER_ALL_OUTPUT_PHASES");

                        if (Thread.interrupted())
                        {
                            logger.debug("Stopping Soar");
                            kernel.StopAllAgents();
                        }

                        for (SoarAgent agent : agents)
                            agent.update();
                        
                        data.incrementDecisionCycle();
                        if (data.shouldCollectData())
                            for (SoarAgent agent : agents)
                                data.collect(agent);

                        logger.trace("smlEVENT_AFTER_ALL_OUTPUT_PHASES done");
                    }
                }, null);
        
        kernel.RegisterForSystemEvent(smlSystemEventId.smlEVENT_SYSTEM_START,
                systemHandler, null);
        kernel.RegisterForSystemEvent(smlSystemEventId.smlEVENT_SYSTEM_STOP,
                systemHandler, null);
        kernel.RegisterForAgentEvent(smlAgentEventId.smlEVENT_AFTER_AGENT_REINITIALIZED, agentHandler, null);
    }
    
    public long registerForSystemEvent(smlSystemEventId id, SystemEventInterface handlerObject, Object callbackData)
    {
        return kernel.RegisterForSystemEvent(id, handlerObject, callbackData);
    }
    
    public void unregisterForSystemEvent(long callbackHandle)
    {
        kernel.UnregisterForSystemEvent(callbackHandle);
    }
    
    public RobotController createRobotController(String name, RobotOutput output, String productions, Config propc)
            throws SoarException
    {
        if (agents.contains(name))
            throw new SoarException("Agent name already exists.");
        
        Agent agent = kernel.CreateAgent(name);
        if (kernel.HadError())
            throw new SoarException(kernel.GetLastErrorDescription());

        SoarAgent sa = new SoarAgent(agent, output, clock, propc);
        sa.initialize();
        agents.add(sa);

        if (productions != null && !productions.isEmpty())
                sa.loadProductions(productions);

        kernel.RegisterForAgentEvent(
                smlAgentEventId.smlEVENT_BEFORE_AGENT_REINITIALIZED, sa
                        .getAgentHandler(), null);
        kernel.RegisterForAgentEvent(
                smlAgentEventId.smlEVENT_AFTER_AGENT_REINITIALIZED, sa
                        .getAgentHandler(), null);

        if (properties.get(SoarProperties.SPAWN_DEBUGGERS))
        {
            kernel.GetAllConnectionInfo();
            int cons = kernel.GetNumberConnections();
            boolean debug = true;
            for (int i = 0; i < cons; ++i)
            {
                if ("java-debugger".equals(kernel.GetConnectionInfo(i).GetName()))
                {
                    debug = false;
                    break;
                }
            }
            if (debug)
                sa.debug();
        }

        return sa;
    }

    private final SystemEventInterface systemHandler = new Kernel.SystemEventInterface()
    {
        public void systemEventHandler(int eventId, Object arg1, Kernel arg2)
        {
            if (eventId == smlSystemEventId.smlEVENT_SYSTEM_START.swigValue())
            {
                logger.trace("smlEVENT_SYSTEM_START");

                for (SoarAgent agent : agents)
                    agent.startEvent();

                clock.start();
                
                logger.info("Soar started.");
            }
            else if (eventId == smlSystemEventId.smlEVENT_SYSTEM_STOP.swigValue())
            {
                logger.trace("smlEVENT_SYSTEM_STOP");
                clock.stop();

                for (SoarAgent agent : agents)
                {
                    data.collect(agent);
                    agent.stopEvent();
                    agent.getEvents().fireEvent(DriveEStopEvent.INSTANCE, AbstractDriveEvent.class);
                }
                
                logger.info("Soar stopped.");
            }
        }
    };
    
    private final AgentEventInterface agentHandler = new Kernel.AgentEventInterface()
    {
        public void agentEventHandler(int eventID, Object d, String agentName)
        {
            clock.reset();
            data.reset();
        }
    };

    /**
     * @return <code>true</code> if Soar is now running.
     */
    public boolean toggleRunState()
    {
        synchronized (exec)
        {
            logger.debug("Toggle Soar requested");
            if (soarTask == null || soarTask.isDone())
            {
                soarTask = exec.submit(new Runnable()
                {
                    public void run()
                    {
                        kernel.RunAllAgentsForever();
                    }
                });
                return true;
			}
			soarTask.cancel(true);
			return false;
        }
    }
    
    public void startSoar(final int cycleLimit)
    {
        synchronized (exec)
        {
            if (soarTask == null || soarTask.isDone())
            {
                logger.debug("Start Soar requested");
                soarTask = exec.submit(new Runnable()
                {
                    public void run()
                    {
                        if (cycleLimit > 0)
                            kernel.RunAllAgents(cycleLimit);
                        else
                            kernel.RunAllAgentsForever();
                    }
                });
            }
        }
    }

    public void stopSoar()
    {
        synchronized (exec)
        {
            if (soarTask != null)
            {
                logger.debug("Stop Soar requested");
                soarTask.cancel(true);
            }
        }
    }
    
    public void shutdown()
    {
        stopSoar();
        
        for (SoarAgent a : agents)
            a.shutdown();

        kernel.Shutdown();
        kernel.delete();
        logger.info("Soar interface down");
        
        exec.shutdown();
        data.shutdown();
    }

    public void onEvent(RobotEvent event)
    {
        if (event instanceof BeforeResetEvent)
            stopSoar();
        else if (event instanceof AfterResetEvent)
        {
            for (SoarAgent a : agents)
                a.getSoarAgent().InitSoar();
        }
    }
    
    public PropertyManager getProperties()
    {
        return properties;
    }
    
    public PropertyManager getAgentProperties(String name)
    {
        if (name != null)
        {
            for (SoarAgent a : agents)
            {
                if (a.getName().equals(name))
                    return a.getProperties();
            }
            return null;
        }        
        
        if (agents.isEmpty())
            return null;

        return agents.get(0).getProperties();
    }

    public SoarDataCollector getSoarDataCollector()
    {
        return data;
    }

    public boolean hasSoarAgents()
    {
        return !agents.isEmpty();
    }

	@Override
	public void radioMessageReceived(RadioMessage comm) {
		for (SoarAgent a : agents) {
			if (a.getName().equals(comm.getDestination())) {
				a.radioMessageReceived(comm);
			}
		}
	}

}
