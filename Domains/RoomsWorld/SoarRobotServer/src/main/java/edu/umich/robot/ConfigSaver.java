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
package edu.umich.robot;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import april.config.Config;

import com.google.common.collect.Lists;

import edu.umich.robot.metamap.AreaDescription;
import edu.umich.robot.metamap.Metamap;
import edu.umich.robot.metamap.VirtualObject;
import edu.umich.robot.soar.AgentProperties;
import edu.umich.robot.soar.SoarAgent;
import edu.umich.robot.soar.SoarProperties;
import edu.umich.robot.soar.AgentProperties.Mission;
import edu.umich.robot.util.Misc;
import edu.umich.robot.util.properties.PropertyKey;
import edu.umich.robot.util.properties.PropertyManager;

/**
 * <p>
 * Utility class to generate a new configuration from the current program state.
 * 
 * <p>
 * Also writes out the configuration to a file in a halfway-readable format.
 * 
 * @author voigtjr@gmail.com
 */
public class ConfigSaver
{
	
	public static void main(String[] args) {
		for (String arg : args) {
			System.out.println(arg);
		}
	}
	
    private final Config config = new Config();
    
    /**
     * <p>
     * Creates the base configuration based on the supplied metamap and
     * properties.
     * 
     * @param metamap
     * @param soarProperties
     */
    public ConfigSaver(Metamap metamap, PropertyManager soarProperties)
    {
        config.setString("image_path", metamap.getImagePath());
        config.setInts("image_origin", metamap.getImageOrigin());
        config.setDouble("meters_per_pixel", metamap.getMetersPerPixel());
        
        Config meta = config.getChild("metadata");
        List<Integer> doors = Lists.newArrayList();
        for (int i = 0; metamap.getAreaFromIndex(i) != null; ++i)
        {
            AreaDescription ad = metamap.getAreaFromIndex(i);
            meta.setInts(Integer.toString(i), ad.getPixelRect());
            String type = ad.getProperties().get("type");
            if (type != null && type.equals("door"))
                doors.add(i);
        }
        if (doors.size() > 0)
            meta.setInts("doors", Misc.toPrimitiveIntArray(doors));
        
        Config objects = meta.getChild("objects");
        for (String name : metamap.getObjectNames())
        {
            VirtualObject tpl = metamap.getTemplate(name);
            Config c = objects.getChild(name);
            c.setDoubles("size", Misc.toPrimitiveDoubleArray(tpl.getSize()));
            for(Map.Entry<String, String> e : tpl.getProperties().entrySet())
                c.setString(e.getKey(), e.getValue());
        }
        
        List<String> placed = Lists.newArrayList();
        for (VirtualObject o : metamap.getPlacedObjects())
        {
            placed.add(o.getName());
            placed.add(Double.toString(o.getPose().getX()));
            placed.add(Double.toString(o.getPose().getY()));
        }
        objects.setStrings("placed", placed.toArray(new String[placed.size()]));
        
        Config soar = config.getChild("soar.properties");
        saveProperty(soarProperties, SoarProperties.DATA_COLLECTION_MODE, soar);
        saveProperty(soarProperties, SoarProperties.DATA_FILE, soar);
        saveProperty(soarProperties, SoarProperties.PERIOD_CYCLES, soar);
        saveProperty(soarProperties, SoarProperties.PERIOD_MILLIS, soar);
        saveProperty(soarProperties, SoarProperties.SPAWN_DEBUGGERS, soar);
    }
    
    /**
     * <p> Writes the current position and associated Soar controller
     * productions of a Splinter bot.
     *
     * <p> Null ok to pass if there is no agent associated with the Splinter.
     */
    public void addSplinter(String name, List<Double> position, SoarAgent sa)
    {
        List<String> splinters = Lists.newArrayList(config.getStrings("splinters", new String[0]));
        splinters.add(name);
        config.setStrings("splinters", splinters.toArray(new String[splinters.size()]));
        
        config.setDoubles(name + ".position", Misc.toPrimitiveDoubleArray(position));
        
        if (sa == null)
            return;
        
        if (sa.getProductionsFile() != null)
            config.setString(name + ".productions", sa.getProductionsFile());
        
        PropertyManager ap = sa.getProperties();
        Config cc = config.getChild(name + ".properties");
        saveProperty(ap, AgentProperties.LEARN, cc);
        saveProperty(ap, AgentProperties.EPMEM_LEARNING, cc);
        saveProperty(ap, AgentProperties.SMEM_LEARNING, cc);
        saveProperty(ap, AgentProperties.EPMEM_EXCLUSIONS, cc);
        saveProperty(ap, AgentProperties.DEFAULT_STORAGE_AREA_ID, cc);
        saveProperty(ap, AgentProperties.AREAS_HELD_IN, cc);
        saveProperty(ap, AgentProperties.OBJECTS_HELD_IN, cc);
        saveProperty(ap, AgentProperties.LOOK_AHEAD_PLANNING, cc);
        saveProperty(ap, AgentProperties.SEARCH_CONTROL_GO_TO_GATEWAY, cc);
        saveProperty(ap, AgentProperties.DELETE_OLD_AREAS, cc);
        saveProperty(ap, AgentProperties.MISSION, cc);
        saveProperty(ap, AgentProperties.MISC_COMMANDS, cc);
    }
    
    private <T> void saveProperty(PropertyManager p, PropertyKey<T> key, Config cc)
    {
        if (key.getType() == String[].class)
        {
            String[] val = (String[])p.get(key);
            if (val == null)
                return;
            cc.setStrings(key.getName(), val);
        }
        else if (key.getType() == File.class)
        {
            File val = (File)p.get(key);
            if (val == null)
                return;
            cc.setString(key.getName(), val.getAbsolutePath());
        }
        else if (key.getType() == Mission.class)
        {
            Mission val = (Mission)p.get(key);
            if (val == null)
                return;
            cc.setString(key.getName(), val.name());
        }
        else
        {
            if (p.get(key) == null)
                return;
            String val = p.get(key).toString();
            cc.setString(key.getName(), val);
        }
    }

    /**
     * <p> Creates or overwrites the passed file with this current
     * configuration.
     */
    public void write(File selectedFile) throws IOException
    {
        selectedFile.createNewFile();
        FileOutputStream fos = new FileOutputStream(selectedFile);
        OutputStreamWriter out = new OutputStreamWriter(fos);
        PrintWriter pout = new PrintWriter(out);
        
        List<String> list = Lists.newLinkedList();
        for (String key : config.getKeys())
        {
            StringBuilder sb = new StringBuilder();
            for (String v : config.getStrings(key))
                sb.append("\"").append(v.replaceAll("\\\\", "\\\\\\\\")).append("\"").append(",");
            if (sb.length() > 0)
                sb.deleteCharAt(sb.length() - 1);
            list.add(String.format("%s = [%s];", key, sb.toString()));
        }
        
        Collections.sort(list);
        
        for (String element : list)
            pout.println(element);

        pout.flush();
        pout.close();
    }
}

