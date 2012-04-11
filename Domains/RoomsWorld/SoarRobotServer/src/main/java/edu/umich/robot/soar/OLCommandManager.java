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

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import sml.Identifier;

/**
 * This class takes a command off the input link and returns a new command
 * instance for it.
 * 
 * @author voigtjr@gmail.com
 */
class OLCommandManager
{
    private static final Log logger = LogFactory.getLog(OLCommandManager.class);

    private final Map<String, Class<? extends OLCommand>> commands = new HashMap<String, Class<? extends OLCommand>>();

    private final SoarAgent agent;
    private final String agentPrompt;

    public OLCommandManager(SoarAgent agent)
    {
        this.agent = agent;
        this.agentPrompt = agent.getName() + ": ";

        // TODO screaming for reflection
        commands.put(EStopCommand.NAME, EStopCommand.class);
        commands.put(StopCommand.NAME, StopCommand.class);
        commands.put(MotorCommand.NAME, MotorCommand.class);
        commands.put(SetLinearVelocityCommand.NAME, SetLinearVelocityCommand.class);
        commands.put(SetAngularVelocityCommand.NAME, SetAngularVelocityCommand.class);
        commands.put(SetVelocityCommand.NAME, SetVelocityCommand.class);
        commands.put(SetHeadingCommand.NAME, SetHeadingCommand.class);
        commands.put(SetHeadingLinearCommand.NAME, SetHeadingLinearCommand.class);

        commands.put(ConfigureCommand.NAME, ConfigureCommand.class);
        
        commands.put(AddWaypointCommand.NAME, AddWaypointCommand.class);
        commands.put(EnableWaypointCommand.NAME, EnableWaypointCommand.class);
        commands.put(DisableWaypointCommand.NAME, DisableWaypointCommand.class);
        commands.put(RemoveWaypointCommand.NAME, RemoveWaypointCommand.class);

        commands.put(SendMessageCommand.NAME, SendMessageCommand.class);
        commands.put(RemoveMessageCommand.NAME, RemoveMessageCommand.class);
        commands.put(ClearMessagesCommand.NAME, ClearMessagesCommand.class);

        commands.put(GetObjectCommand.NAME, GetObjectCommand.class);
        commands.put(DropObjectCommand.NAME, DropObjectCommand.class);
        commands.put(DiffuseObjectCommand.NAME, DiffuseObjectCommand.class);
        commands.put(DiffuseObjectByWireCommand.NAME, DiffuseObjectByWireCommand.class);
        
        commands.put(SetHeadlightCommand.NAME, SetHeadlightCommand.class);
        commands.put(SetRoomLightCommand.NAME, SetRoomLightCommand.class);

        commands.put(DoorOpenCommand.NAME, DoorOpenCommand.class);
        commands.put(DoorCloseCommand.NAME, DoorCloseCommand.class);
        commands.put(DoorUnlockCommand.NAME, DoorUnlockCommand.class);
        commands.put(DoorLockCommand.NAME, DoorLockCommand.class);
    }

    /**
     * Convert a wme in to a command instance for processing. If the command
     * returns from this, it is accepted and status is set as such.
     * 
     * @param id
     * @return
     * @throws SoarCommandError
     */
    public OLCommand newInstance(Identifier id) throws SoarCommandError
    {
        String name = id.GetAttribute();

        Class<? extends OLCommand> klass = commands.get(name);
        if (klass != null)
        {
            try
            {
                Constructor<? extends OLCommand> ctor = klass
                        .getConstructor(new Class<?>[] { Identifier.class,
                                SoarAgent.class });
                OLCommand command = ctor.newInstance(id, agent);
                if (command != null)
                    CommandStatus.ACCEPTED.addStatus(id);
                logger.debug(agentPrompt + command);
                return command;

            }
            catch (InvocationTargetException e)
            {
                if (e.getCause() instanceof SoarCommandError)
                {
                    SoarCommandError sce = (SoarCommandError) e.getCause();
                    CommandStatus.ERROR.addStatus(id, sce.getMessage());
                    logger.error(sce.getMessage());
                }
                return null;
            }
            catch (NoSuchMethodException e)
            {
                logger.error(e.getMessage());
                return null;
            }
            catch (IllegalAccessException e)
            {
                logger.error(e.getMessage());
                return null;
            }
            catch (InstantiationException e)
            {
                logger.error(e.getMessage());
                return null;
            }
        }

        logger.warn("No such command: " + name);
        return null;
    }

}
