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

/**
 * a=This class handles the representation of objects in the simulation.
 * Because the definitions for the different types of objects are sent
 * to us over the network when the simulation begins, we can't hard-code
 * those definitions into this class -- we need to be more flexible.
 */

package edu.umich.soarrobot.SoarRobotTablet.objects;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Scanner;

import javax.microedition.khronos.opengles.GL10;

import android.graphics.Color;
import android.graphics.PointF;
import edu.umich.soarrobot.SoarRobotTablet.layout.GLUtil;

public class SimObject {
	
	public static class TemplateAction
	{
		private String type;
		private String text;
		private boolean includeID;
		private boolean includeXY;
		public TemplateAction(String type, String text, boolean includeID, boolean indluceXY)
		{
			this.type = type;
			this.text = text;
			this.includeID = includeID;
			this.includeXY = indluceXY;
		}
		
		public String getType() { return type; }
		public String getText() { return text; }
		public boolean getIncludeID() { return includeID; }
		public boolean getIncludeXY() { return includeXY; }
		
		public String getCommand(SimObject obj, PointF xy) {
			StringBuilder b = new StringBuilder(text + " " + type);
			if (includeID)
			{
				b.append(" " + obj.getID());
			}
			if (includeXY)
			{
				b.append(" " + xy.x + " " + xy.y);
			}
			return b.toString();
		}
	}
	
	/*
	 * Static variables and methods
	 */
	
	// First key is the object class name
	// Second key is the name of the property.
	// Value is of type String or Float, or an array
	// of one of those types.
	private static HashMap<String, HashMap<String, String>> classes;
	private static HashMap<String, ArrayList<TemplateAction>> templateActions = new HashMap<String, ArrayList<TemplateAction>>();
	
	private static void addTemplateAction(TemplateAction ta)
	{
		String clazz = ta.getType();
		ArrayList<TemplateAction> actions = templateActions.get(clazz);
		if (actions == null)
		{
			actions = new ArrayList<TemplateAction>();
			templateActions.put(clazz, actions);
		}
		actions.add(ta);
	}
	
	static
	{
		classes = new HashMap<String, HashMap<String,String>>();
		classes.put("area", new HashMap<String, String>());
		
		// Default actions for each class
		addTemplateAction(new TemplateAction("area", "go-to", true, false));
		addTemplateAction(new TemplateAction("area", "turn-light-on", true, false));
		addTemplateAction(new TemplateAction("area", "turn-light-off", true, false));
		addTemplateAction(new TemplateAction("area", "drop-off-object", true, true));
		addTemplateAction(new TemplateAction("green-cube", "pick-up", true, true));
	}
	
	public static void init(String classesString) {
		for (String classString : classesString.split(";")) {
			Scanner s = new Scanner(classString);
			s.useDelimiter(" ");
			String name = s.next();
			s.next("\\{");
			HashMap<String, String> properties = new HashMap<String, String>();
			while(true) {
				String key = s.next();
				if (key.equals("}")) {
					break;
				}
				s.next(":");
				String value = s.next();
				s.next(",");
				properties.put(key, value);
			}
			classes.put(name, properties);
		}
	}
	
	public static String[] getClassNames() {
	    return classes.keySet().toArray(new String[] {});
	}
	
	/*
	 * Instance variables
	 */
	
	protected String type;
	protected HashMap<String, String> attributes;
	protected PointF location;
	protected float theta;
	protected PointF size;
	private String color;
	protected int id;
	protected boolean selected;
	protected boolean visible;
	
	// These variable could fit better in a robot-themed subclass.
	
	/*
	 * Instance methods
	 */
	
	/**
	 * Class constructor.
	 * 
	 * @param type
	 * @param instanceAttributes
	 */
	public SimObject(String type, int id, PointF location) {
		this.type = type;
		this.id = id;
		this.attributes = classes.get(type);
		this.location = location;
		this.theta = 0.0f;
		this.visible = true;

		selected = false;
		
		// Set default values
		color = GLUtil.BLACK;
		if (attributes.containsKey("color")) {
			try {
				String colorString = (String) attributes.get("color");
				int c = Color.parseColor(colorString);
				float r = Color.red(c) / (float) 0xFF;
				float g = Color.green(c) / (float) 0xFF;
				float b = Color.blue(c) / (float) 0xFF;
				color = "rgb(" + r + "," + g + "," + b + ")";
			} catch (ClassCastException e) {
				e.printStackTrace();
			} catch (IllegalArgumentException e) {
				e.printStackTrace();
			}
		}
		if (type.equals("splinter"))
		{
			size = new PointF(0.75f, 0.5f);
		}
		else
		{
			size = new PointF(0.25f, 0.25f);
		}
	}

    /**
     * Draws the object to the canvas.
     * 
     * @param c
     */
    public void draw(GL10 gl)
    {
        if (!visible) {
            return;
        }
        
		GLUtil.drawCube(gl, location.x, location.y, -0.5f, size.x, size.y, 1.0f, color);
	}
	
	/*
	 * Utility methods
	 */
	
	/**
	 * Check if a point intersects this <code>SimObject</code>.
	 * @param p The point to check.
	 * @return true if the point <code>p</code> intersects this <code>SimObject</code>.
	 */
	public boolean intersectsPoint(PointF p, float within) {
		// Might be different for non-rectangle shapes
		return p.x + within >= location.x && p.x - within < location.x + size.x
				&& p.y + within >= location.y && p.y - within < location.y + size.y;
	}
	
	public boolean intersectsPoint(PointF p) {
	    return intersectsPoint(p, 0.0f);
	}
	
    public String getPropertiesString() {
        StringBuilder sb = new StringBuilder();
        sb.append(type + " id=" + getID() + "; x=" + location.x + "; y=" + location.y + "; ");
        for (String str : attributes.keySet()) {
            sb.append(str + "=" + attributes.get(str) + "; ");
        }
        return sb.toString();
    }
	
	/*
	 * Accessor methods.
	 */
	
	public String getType() {
		return type;
	}
	
	public boolean isOfType(String type) {
		return this.type.equals(type);
	}
	
	public String getAttribute(String attribute) {
		return attributes.get(attribute);
	}	
	
	public void setAttribute(String attribute, String value) {
		attributes.put(attribute, value);
	}
	
	public PointF getLocation() {
		return location;
	}
	
	public void setLocation(PointF location) {
		this.location = location;
	}
	
	public float getTheta()
	{
		return theta;
	}
	
	public void setTheta(float theta) {
		this.theta = theta;
	}
	
	public void setSelected(boolean selected) {
		this.selected = selected;
	}
	
	public PointF getSize() {
		return size;
	}
	
	public int getID() {
		return id;
	}
    
    public void setVisible(boolean visible) {
    	this.visible = visible;
    }
    
    public boolean isVisible() {
    	return visible;
    }
    
    public ArrayList<TemplateAction> getActions()
    {
    	ArrayList<TemplateAction> ret = templateActions.get(type);
    	return ret;
    }
}
