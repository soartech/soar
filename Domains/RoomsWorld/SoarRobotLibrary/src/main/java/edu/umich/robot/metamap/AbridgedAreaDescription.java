package edu.umich.robot.metamap;

import java.util.List;

import com.google.common.collect.ImmutableList;

/**
 * Immutable.
 * 
 * @author voigtjr
 */
public class AbridgedAreaDescription
{
    public final int id;
    public final List<Integer> xywh;
    public final String type;
    public final List<AbridgedGateway> gateways;
    
    public AbridgedAreaDescription(int id, ImmutableList<Integer> xywh, String type, ImmutableList<AbridgedGateway> gateways)
    {
        this.id = id;
        this.xywh = xywh;
        this.type = type;
        this.gateways = gateways;
    }
    
    @Override
    public String toString()
    {
        StringBuilder builder = new StringBuilder("r ");
        builder.append(id).append(": (")
        .append(xywh.get(0)).append(",").append(xywh.get(1)).append(",")
        .append(xywh.get(2)).append(",").append(xywh.get(3)).append(") ")
        .append(type);
        
        for (AbridgedGateway g : gateways) {
            builder.append("\t").append(g.toString()).append(" ");
        }
        
        return builder.toString();
    }
}

