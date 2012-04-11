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

import java.util.List;

/**
 * Utility class for converting an area description in to a string.
 * 
 * @author voigtjr
 *
 */
public class AreaDescriptions
{
    public static StringBuilder render(StringBuilder builder, List<AreaDescription> areas)
    {
        if (builder == null)
            builder = new StringBuilder();
        
        for (AreaDescription ad : areas)
            render(builder, ad).append(" ; ");
        
        return builder;
    }
    
    /**
     * "r id x y w h [g id x y]*" 
     * 
     * r: literal (room)
     * id: int
     * x y w h: double
     * type: string (usually "door" or "room")
     * []: aren't printed
     * g: literal (gateway)
     * *: previous construct repeated 0 or more times
     * 
     * Example:
     * 
     * r 0 3.2 4.525 5.2 4.8 g 1 6 8.2 3.3 3.3
     * 
     * @param builder
     * @param area
     * @return
     */
    public static StringBuilder render(StringBuilder builder, AreaDescription area)
    {
        if (builder == null)
            builder = new StringBuilder();

        builder.append("r ").append(area.getId());
        
        builder.append(" ").append(area.getPose().getX());
        builder.append(" ").append(area.getPose().getY());
        builder.append(" ").append(area.getPose().getVel().get(0));
        builder.append(" ").append(area.getPose().getVel().get(1));
        
        for (Gateway gw : area.getGateways())
        {
            builder.append(" g ").append(gw.getId());
            builder.append(" ").append(gw.getPose().getX());
            builder.append(" ").append(gw.getPose().getY());
        }
        
        return builder;
    }
    
}
