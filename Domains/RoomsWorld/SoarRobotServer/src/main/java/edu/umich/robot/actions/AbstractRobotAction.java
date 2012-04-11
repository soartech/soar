/*
 * Copyright (c) 2008  Dave Ray <daveray@gmail.com>
 *
 * Created on Oct 25, 2008
 */
package edu.umich.robot.actions;

import javax.swing.AbstractAction;
import javax.swing.Icon;
import javax.swing.KeyStroke;

import edu.umich.robot.GuiApplication;

/**
 * @author ray
 */
public abstract class AbstractRobotAction extends AbstractAction
{
    private static final long serialVersionUID = -3672388761691148039L;

    private final ActionManager actionManager;
    
    public AbstractRobotAction(String label)
    {
        super(label);
        this.actionManager = null;
    }

    public AbstractRobotAction(String label, Icon icon)
    {
        super(label, icon);
        this.actionManager = null;
    }
    
    public AbstractRobotAction(ActionManager actionManager, String label)
    {
        super(label);
        this.actionManager = actionManager;
        this.actionManager.addAction(this);
    }

    public AbstractRobotAction(ActionManager actionManager, String label, Icon icon)
    {
        super(label, icon);
        this.actionManager = actionManager;
        this.actionManager.addAction(this);
    }
    
    public void setToolTip(String tip)
    {
        this.putValue(SHORT_DESCRIPTION, tip);
    }
    
    public void setAcceleratorKey(KeyStroke key)
    {
        this.putValue(ACCELERATOR_KEY, key);
    }
    
    public void setLabel(String label)
    {
        this.putValue(NAME, label);
    }

    public abstract void update();
    
    public String getId()
    {
        return getClass().getCanonicalName();
    }
    
    public ActionManager getActions()
    {
        return actionManager;
    }
    
    public GuiApplication getApplication()
    {
        return actionManager != null ? actionManager.getApplication() : null;
    }
    
}
