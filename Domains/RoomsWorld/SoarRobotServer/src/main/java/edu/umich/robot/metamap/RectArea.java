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
package edu.umich.robot.metamap;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

import edu.umich.robot.util.ImmutablePose;
import edu.umich.robot.util.Pose;

/**
 * An area description based on rectangular areas.
 * 
 * @author voigtjr@gmail.com
 */
public class RectArea implements AreaDescription
{
    public static class Builder
    {
        private final int id;
        
        private final int[] xywhPixels;

        private final Pose pose = new Pose();
        
        private final Pose midpoint = new Pose();

        private final Map<WallDir, Wall> wallMap = Maps.newEnumMap(WallDir.class);

        private final ImmutableList.Builder<Gateway> gateways = new ImmutableList.Builder<Gateway>();

        private final ImmutableMap.Builder<String, String> properties = new ImmutableMap.Builder<String, String>();

        private final IdGenerator idg;
        
        public Builder(int[] xywhPixels, List<Double> xywh, IdGenerator idg)
        {
            this.idg = idg;
            this.id = idg.getId();
            this.xywhPixels = Arrays.copyOf(xywhPixels, xywhPixels.length);
            
            pose.setPos(xywh.get(0), xywh.get(1), 0);
            pose.setVel(xywh.get(2), xywh.get(3), 0);
            
            midpoint.setX(xywh.get(0) + xywh.get(2) / 2.0);
            midpoint.setY(xywh.get(1) + xywh.get(3) / 2.0);
            
            for (WallDir dir : WallDir.values())
                wallMap.put(dir, new WallImpl(pose, midpoint, dir, idg, false));
        }

        public int getId()
        {
            return id;
        }
        
        public Builder gateway(Gateway gw)
        {
            gateways.add(gw);
            return this;
        }
        
        /**
         * Breaks from builder pattern because it doesn't return this
         * 
         * @param dir
         * @param id
         * @return
         */
        public Gateway gateway(WallDir dir, Door door)
        {
            Wall wall = wallMap.get(dir);
            List<Integer> to = Lists.newArrayList(this.id);
            to.addAll(wall.getTo());
            
            Gateway gw = new Gateway(wall.getMidpoint(), to, door, idg.getId());
            gateways.add(gw);
            return gw;
        }

        public Builder property(String key, String value)
        {
            properties.put(key, value);
            return this;
        }
        
        public void openWall(WallDir dir, List<Integer> to)
        {
            WallImpl wall = new WallImpl(pose, midpoint, dir, to, idg, true);
            wallMap.put(dir, wall);
        }

        public RectArea build()
        {
            return new RectArea(this);
        }
    }

    private final int id;

    private final int[] xywhPixels;

    private final ImmutablePose pose;

    private final ImmutablePose midpoint;

    private final List<Wall> walls;

    private final List<Gateway> gateways;

    private final Map<String, String> properties;
    
    private boolean changed = true;
    
    private RectArea(Builder builder)
    {
        this.id = builder.id;
        this.xywhPixels = builder.xywhPixels;
        this.pose = ImmutablePose.newInstance(builder.pose);
        this.midpoint = ImmutablePose.newInstance(builder.midpoint);

        this.walls = new ImmutableList.Builder<Wall>().addAll(builder.wallMap.values()).build();

        this.gateways = builder.gateways.build();

        this.properties = builder.properties.build();
    }

    public ImmutablePose getPose()
    {
        return pose;
    }

    @Override
    public String toString()
    {
        StringBuilder sb = new StringBuilder("Area ");
        sb.append(id);

        for (Entry<String, String> e : properties.entrySet())
            sb.append(String.format(" (%s:%s)", e.getKey(), e.getValue()));

        sb.append(String.format(" [%2.2f, %2.2f]", pose.getX(), pose.getY()));
        sb.append(String.format(" [%2.2f, %2.2f]", pose.getVX(), pose.getVY()));

        sb.append(" gw:");
        for (Gateway g : gateways)
            sb.append(String.format(" %s", g.toString()));

        return sb.toString();
    }

    public Map<String, String> getProperties()
    {
        return properties;
    }

    public List<Wall> getWalls()
    {
        return walls;
    }

    public List<Gateway> getGateways()
    {
        return gateways;
    }

    public int getId()
    {
        return id;
    }

    public ImmutablePose getMidpoint()
    {
        return midpoint;
    }

    public int[] getPixelRect()
    {
        return Arrays.copyOf(xywhPixels, xywhPixels.length);
    }

	@Override
	public boolean hasChanged() {
		return changed;
	}

	@Override
	public void setChanged(boolean changed) {
		this.changed = changed;
	}
}
