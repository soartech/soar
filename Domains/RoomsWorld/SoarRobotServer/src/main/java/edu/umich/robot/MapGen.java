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
import java.util.List;

import com.google.common.collect.Lists;

/**
 * <p>
 * Simple file template generator, not too useful.
 * 
 * @author voigtjr@gmail.com
 */
public class MapGen
{
    public static class Rect
    {
        public Rect(int x, int y, int w, int h)
        {
            this.x = x;
            this.y = y;
            this.w = w;
            this.h = h;
        }
        
        @Override
        public String toString()
        {
            return String.format("[%2d,%2d,%2d,%2d]", x, y, w, h);
        }
        
        final int x;
        final int y;
        final int h;
        final int w;
    }

    private final List<Rect> rooms = Lists.newArrayList();
    private int firstDoor = -1;
    private final int size;
    private final int roomSize = 9;
    private final int doorSize = 3;
    
    public MapGen(int size)
    {
        this.size = size;
    }
    
    public void generate(OutputStreamWriter out) throws IOException
    {
        rooms();
        
        firstDoor = rooms.size();
        verticalDoors();
        horizontalDoors();
        
        for (int i = 0; i < rooms.size(); ++i)
            out.append(String.format("    %3d = %s;\n", i, rooms.get(i).toString()));
        
        out.append("    doors = [");
        for (int i = firstDoor; i < rooms.size(); ++i)
        {
            out.append(Integer.toString(i)).append(",");
            if (i % 10 == 9)
                out.append("\n");
        }
        out.append("];\n");
    }

    private void rooms()
    {
        int y = 1;
        for (int i = 0; i < size; ++i)
        {
            int x = 1;
            for (int j = 0; j < size; ++j)
            {
                rooms.add(new Rect(x, y, roomSize, roomSize));
                x += 10;
            }
            y += 10;
        }
    }

    private void verticalDoors()
    {
        int y = 4;
        for (int i = 0; i < size; ++i)
        {
            int x = 10;
            for (int j = 0; j < size - 1; ++j)
            {
                rooms.add(new Rect(x, y, 1, doorSize));
                x += 10;
            }
            y += 10;
        }
    }
    
    private void horizontalDoors()
    {
        int y = 10;
        for (int i = 0; i < size - 1; ++i)
        {
            int x = 4;
            for (int j = 0; j < size; ++j)
            {
                rooms.add(new Rect(x, y, doorSize, 1));
                x += 10;
            }
            y += 10;
        }
    }
    
    public static void main(String[] args)
    {
        try
        {
            MapGen mg = new MapGen(100);
            File file = File.createTempFile("mapgen-", ".txt", new File(System.getProperty("user.dir") + "/config"));
            System.out.println(file.getAbsolutePath());
            FileOutputStream fos = new FileOutputStream(file);
            OutputStreamWriter out = new OutputStreamWriter(fos);
            mg.generate(out);
            out.close();
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
    }
}
