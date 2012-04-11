package edu.umich.robot.metamap;

import java.util.List;

import com.google.common.collect.ImmutableList;

/**
 * Immutable.
 * 
 * @author voigtjr
 */
public class AbridgedGateway
{
    public final int id;
    public final List<Double> xy;
    
    public AbridgedGateway(int id, ImmutableList<Double> xy)
    {
        this.id = id;
        this.xy = xy;
    }
    
    @Override
    public String toString()
    {
        StringBuilder builder = new StringBuilder("g ");
        builder.append(id).append(": (")
        .append(xy.get(0)).append(",").append(xy.get(1)).append(")");
        
        return builder.toString();
    }
}

