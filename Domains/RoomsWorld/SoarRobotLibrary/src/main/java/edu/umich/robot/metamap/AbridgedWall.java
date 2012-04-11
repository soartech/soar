package edu.umich.robot.metamap;

import java.util.List;

import com.google.common.collect.ImmutableList;

/**
 * Immutable.
 * 
 * @author voigtjr
 */
public class AbridgedWall
{
    public final List<Double> midpoint;
    public final WallDir direction;
    public final ImmutableList<Integer> getTo;
    
    public AbridgedWall(ImmutableList<Double> midpoint, WallDir direction, ImmutableList<Integer> getTo)
    {
        this.midpoint = midpoint;
        this.direction = direction;
        this.getTo = getTo;
    }
    
    @Override
    public String toString()
    {
        StringBuilder builder = new StringBuilder("w ");
        builder.append(midpoint.get(0)).append(",").append(midpoint.get(1)).append(")")
        .append(direction.name());
        for (Integer i : getTo) {
        	builder.append(",").append(i);
        }
        
        builder.append(" ;");
        
        return builder.toString();
    }
}

