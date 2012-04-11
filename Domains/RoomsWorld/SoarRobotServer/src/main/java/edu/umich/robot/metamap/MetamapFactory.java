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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import april.config.Config;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

import edu.umich.robot.metamap.Door.State;

/**
 * This creates a metamap from a configuration file. As much as possible is made
 * immutable.
 * 
 * @author voigtjr@gmail.com
 */
public class MetamapFactory
{
    private static final Log logger = LogFactory.getLog(MetamapFactory.class);
    
    private final UnitConverter u;
    
    private final List<int[]> pixelRooms = Lists.newArrayList();
    
    private final Map<WallDir, Map<Integer, List<Integer>>> wallMaps = Maps.newEnumMap(WallDir.class);
    
    private final List<Integer> doorIds = Lists.newArrayList();
    
    private final List<Integer> closedIds = Lists.newArrayList();
    
    private final Map<Integer, Integer> lockedCodes = Maps.newHashMap();
    
    private final Map<Integer, RectArea.Builder> absMap = Maps.newHashMap();
    private final Map<Integer, RectArea.Builder> idsMap = Maps.newHashMap();
    
    private final String imagePath;
    private final double metersPerPixel;
    private final int[] origin;
    
    private final Config oc;

//    private MetamapFactory(List<int[]> pixelRooms, List<Integer> doors)
//    {
//        this.imagePath = "config/test.png";
//        logger.trace(imagePath);
//        this.origin = new int[] { 0, 0 };
//        this.metersPerPixel = 0.10;
//        this.pixelRooms.addAll(pixelRooms);
//        this.doorIds.addAll(doors);
//        
//        u = new UnitConverter(origin, metersPerPixel);
//        
//        oc = null;
//    }
//    
    
    /**
     * This is the same id generator that gets handed to the metamap instance
     * for future object creation.
     */
    private final IdGenerator idg = new IdGenerator();
    
    public MetamapFactory(Config config)
    {
        Config mc = config.getChild("metadata");
        imagePath = config.requireString("image_path");
        origin = config.requireInts("image_origin");
        metersPerPixel = config.requireDouble("meters_per_pixel");
        u = new UnitConverter(origin, metersPerPixel);

        for (int index = 0; mc.hasKey(Integer.toString(index)); ++index)
            pixelRooms.add(mc.requireInts(Integer.toString(index)));
        
        for (int id : mc.getInts("doors", new int[0]))
            doorIds.add(id);
        
        for (int id : mc.getInts("closed", new int[0]))
            closedIds.add(id);
        
        int[] locked = mc.getInts("locked", new int[0]);
        for (int i = 0; i + 1 < locked.length; i += 2)
        {
            int id = locked[i];
            int code = locked[i + 1];
            this.lockedCodes.put(id, code);
        }
        oc = config.getChild("metadata.objects");
    }
    
    public Metamap build()
    {
        for (WallDir dir : WallDir.values())
            wallMaps.put(dir, new HashMap<Integer, List<Integer>>());
        
        for (int index = 0; index < pixelRooms.size(); ++index)
        {
            int[] xywh = pixelRooms.get(index);
            
            if (!doorIds.contains(index))
            {
                add(WallDir.NORTH, xywh[1], index);
                add(WallDir.EAST, xywh[0] + xywh[2], index);
                add(WallDir.SOUTH, xywh[1] + xywh[3], index);
                add(WallDir.WEST, xywh[0], index);
            }
            
            RectArea.Builder ab = new RectArea.Builder(xywh, u.getMeters_xywh(xywh), idg);
            absMap.put(index, ab);
            idsMap.put(ab.getId(), ab);
        }

        for (int index = 0; index < pixelRooms.size(); ++index)
        {
            // RectArea.Builder ab = absMap.get(index);

            int[] xywh = pixelRooms.get(index);
            logger.debug("Room " + index);
            setToList(absMap, index, WallDir.WEST, xywh[0], 1, xywh);
            setToList(absMap, index, WallDir.EAST, xywh[0] + xywh[2], 1, xywh);
            setToList(absMap, index, WallDir.NORTH, xywh[1], 0, xywh);
            setToList(absMap, index, WallDir.SOUTH, xywh[1] + xywh[3], 0, xywh);
        }
        
        Map<Integer, Door> doors = Maps.newConcurrentMap();
        for (Integer id : doorIds)
        {
            RectArea.Builder ab = absMap.get(id);
            int[] xywh = pixelRooms.get(id);
            
            Door door = new Door(id, u.getMeters_xywh(xywh));
            Integer code = lockedCodes.get(id);
            if (code != null)
            {
                door.setState(State.LOCKED);
                door.setCode(code);
            } 
            else if (closedIds.contains(id))
            {
                door.setState(State.CLOSED);
            }
            doors.put(id, door);
            
            ab.property("type", "door");
            
            logger.debug("Door: " + id);
            // determine direction, will be bigger
            if (xywh[2] > xywh[3])
            {
                // width is larger: door runs north/south
                gateways(WallDir.NORTH, ab, door);
                gateways(WallDir.SOUTH, ab, door);
            } 
            else
            {
                // east/west
                gateways(WallDir.EAST, ab, door);
                gateways(WallDir.WEST, ab, door);
            }
        }
        
        ImmutableList.Builder<AreaDescription> areaList = new ImmutableList.Builder<AreaDescription>();
        for (int i = 0; i < absMap.size(); ++i)
            areaList.add(absMap.get(i).build());
        
        idg.mark();
        return new Metamap(imagePath, metersPerPixel, origin, areaList.build(), new VirtualObjectManager(oc, idg), doors, idg);
    }
    
    private void gateways(WallDir dir, RectArea.Builder current, Door door)
    {
        // create a gateway at the midpoint of the <dir> wall of this room.
        Gateway gw = current.gateway(dir, door);
        logger.debug(dir + ": " + gw);
        for (Integer toId : gw.getTo())
        {
            if (toId.equals(current.getId()))
                continue;
            logger.trace("telling " + toId);
            // Tell connected room about the gateway
            idsMap.get(toId).gateway(gw);
        }
    }
    
    private void printConnections(String m, List<Integer> c)
    {
        StringBuilder sb = new StringBuilder(m);
        if (c.isEmpty())
            sb.append("None");
        else
        {
            for (Integer id : c)
                sb.append(id).append(" ");
        }
        logger.debug(sb.toString());
    }
    
    private void add(WallDir dir, Integer v, Integer i)
    {
        Map<Integer, List<Integer>> m = wallMaps.get(dir);
        List<Integer> ids = m.get(v);
        if (ids == null)
        {
            ids = Lists.newArrayList();
            m.put(v, ids);
        }
        ids.add(i);
    }
    
    /**
     * @param p The line to index m with.
     * @param a The other axis for looking at wall lengths, 0 or 1 (x or y).
     * @param xywh Target room dimensions.
     */
    private void setToList(Map<Integer, RectArea.Builder> builderMap, int index, WallDir dir, int p, int a, int[] xywh)
    {
        RectArea.Builder ab = absMap.get(index);
        Map<Integer, List<Integer>> m = wallMaps.get(dir.opposite());
        List<Integer> tos = Lists.newArrayList();

        List<Integer> ids = m.get(p);
        if (ids != null)
        {
            for (Integer id : ids)
            {
                int[] xywhOther = pixelRooms.get(id);

                if (isOverlap(xywh[a], xywh[a + 2], xywhOther[a], xywhOther[a + 2]))
                    tos.add(absMap.get(id).getId());
            }
        }
        
        if (logger.isDebugEnabled())
            printConnections(dir.toString().toLowerCase() + " connections: ", tos);

        if (!tos.isEmpty())
            ab.openWall(dir, tos);
    }
    
    private boolean isOverlap(int a0, int al, int b0, int bl)
    {
        boolean left = a0 < b0 + bl && a0 + al > b0;
        boolean right = a0 >= b0 && a0 < b0 + bl;
        
        return left || right;
    }

}
