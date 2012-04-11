/*
 * Copyright (c) 2008  Dave Ray <daveray@gmail.com>
 *
 * Created on Oct 25, 2008
 */
package edu.umich.robot.actions;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import edu.umich.robot.GuiApplication;

/**
 * Based on jsoar action manager. Modified.
 * 
 * @author ray
 * @author voigtjr
 */
public class ActionManager
{
    private static final Log logger = LogFactory.getLog(ActionManager.class);
    
    private GuiApplication app;
    private List<AbstractRobotAction> actions = new ArrayList<AbstractRobotAction>();
    private Map<String, AbstractRobotAction> actionCache = new HashMap<String, AbstractRobotAction>();
    
    /**
     * @param app The owning application
     */
    public ActionManager(GuiApplication app)
    {
        this.app = app;
    }
    
    /**
     * @return The owning application
     */
    public GuiApplication getApplication()
    {
        return app;
    }
    
    public AbstractRobotAction getAction(String id)
    {
        AbstractRobotAction r = actionCache.get(id);
        if(r != null)
        {
            return r;
        }
        
        for(AbstractRobotAction action : actions)
        {
            if(id.equals(action.getId()))
            {
                r = action;
                break;
            }
        }
        
        if(r != null)
        {
            actionCache.put(r.getId(), r);
        }
        
        return r;
    }
    
    public <T extends AbstractRobotAction> T getAction(Class<T> klass)
    {
        return klass.cast(getAction(klass.getCanonicalName()));
    }
    
    /**
     * Add an action that is managed by the application
     * 
     * @param action The action to add
     */
    public void addAction(AbstractRobotAction action)
    {
        if(!actionCache.containsKey(action.getId()))
        {
            actionCache.put(action.getId(), action);
        }
        actions.add(action);
    }
    
    public void updateActions()
    {
        for(AbstractRobotAction action : actions)
        {
            action.update();
        }
    }

    public void executeAction(String id)
    {
        AbstractRobotAction action = getAction(id);
        if(action != null)
        {
            action.actionPerformed(null);
        }
        else
        {
            logger.error("No action found with id '" + id + "'");
        }
    }
}
