package edu.umich.soarrobot.SoarRobotTablet.objects;

import java.util.ArrayList;
import java.util.List;

import javax.microedition.khronos.opengles.GL10;

import edu.umich.robot.metamap.AbridgedWall;
import edu.umich.soarrobot.SoarRobotTablet.layout.GLUtil;

import android.graphics.Color;
import android.graphics.Point;
import android.graphics.PointF;
import android.graphics.Rect;
import android.graphics.RectF;

public class SimArea extends SimObject
{   
	private int id;
    private Rect rect;
    private RectF scaledRect;
    private boolean lightsOn;
    private boolean doorClosed;
    private boolean isDoorway;
    private float position;
    
    // Scaling factor
    private static float metersPerUnit;
    private static Point imageOrigin;

	public static void setMetersPerUnit(float metersPerUnit) {
		SimArea.metersPerUnit = metersPerUnit;
	}
	
	public static void setImageOrigin(Point origin) {
		SimArea.imageOrigin = origin;
	}
	
	public static RectF scaleRect(Rect r)
	{
		return new RectF(
				(r.left - imageOrigin.x) * metersPerUnit,
				(r.top + imageOrigin.y) * metersPerUnit,
				(r.right - imageOrigin.x) * metersPerUnit,
				(r.bottom + imageOrigin.y) * metersPerUnit);
	}
	
	public static PointF scalePoint(Point p)
	{
		return new PointF(
				(p.x - imageOrigin.x) * metersPerUnit,
				(p.y + imageOrigin.y) * metersPerUnit);
	}
    
    public SimArea(int id, Rect r, String type) {
    	super("area", id, null);
    	this.id = id;
        this.rect = r;
        this.scaledRect = scaleRect(r);
        this.location = new PointF(scaledRect.left, scaledRect.bottom);
    	this.size = new PointF(scaledRect.width(), -scaledRect.height());
        this.isDoorway = type.equals("door");
        this.position = 0.0f;
        lightsOn = true;
        doorClosed = false;
    }
    
    public void draw(GL10 gl) {
    	String color = (position < 0.5f && isDoorway) ? GLUtil.DARK_GRAY : (lightsOn ? GLUtil.WHITE : GLUtil.DARK_GRAY);
    	if (doorClosed) {
    	    position -= 0.04f;
    	    if (position <= -0.5f) {
    	        position = -0.5f;
    	    }
    	} else {
    	    position += 0.04f;
    	    if (position >= 0.5f) {
    	        position = 0.5f;
    	    }
    	}
    	/*
    	 * -0.5f - door is closed
    	 * 0.5f - door is open
    	 * */
    	
    	if (position < 0.5f && isDoorway) {
    		float width = scaledRect.right - scaledRect.left;
    		float height = scaledRect.top - scaledRect.bottom;
    		float centerX = scaledRect.left + width / 2.0f;
    		float centerY = scaledRect.bottom + height / 2.0f;
    		GLUtil.drawCube(gl, centerX, centerY, position, width,
    				height, 1.0f, color);
    	} else {
    		GLUtil.drawRect(gl, scaledRect.left, scaledRect.bottom, 0.0f, scaledRect.right - scaledRect.left,
    				scaledRect.top - scaledRect.bottom, color);
    	}
    }
    
    public boolean getLightsOn() {
    	return lightsOn;
    }
    
    public void setLightsOn(boolean lightsOn) {
    	this.lightsOn = lightsOn;
    }
    
    public boolean getDoorClosed() {
    	return doorClosed;
    }
    
    public void setDoorClosed(boolean doorClosed) {
    	if (!isDoorway) return;
    	this.doorClosed = doorClosed;
    }
    
    public int getID() {
    	return id;
    }
    
    public Rect getRect() {
    	return rect;
    }
    
    public String getPropertiesString() {
        StringBuilder sb = new StringBuilder();
        sb.append(type + " id=" + getID() + "; x=" + location.x + "; y=" + location.y + "; ");
        sb.append("lights=" + (lightsOn ? "on" : "off") + "; ");
        if (isDoorway)
        {
        	sb.append("door-closed=" + doorClosed + "; ");
        }
        for (String str : attributes.keySet()) {
            sb.append(str + "=" + attributes.get(str) + "; ");
        }
        return sb.toString();
    }
    
    @Override
    public ArrayList<TemplateAction> getActions() {
    	ArrayList<TemplateAction> ret = super.getActions();
    	if (isDoorway)
    	{
    		ArrayList<TemplateAction> all = new ArrayList<TemplateAction>();
    		all.addAll(ret);
    		all.add(new TemplateAction("area", "open-door", true, true));
    		all.add(new TemplateAction("area", "close-door", true, true));
    		return all;
    	}
    	return ret;
    }
}
