package edu.umich.robot.metamap;

import java.util.List;
import java.util.Scanner;
import java.util.regex.Pattern;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;

public class AbridgedAreaDescriptions
{
    public static List<AbridgedAreaDescription> parseAreas(String string)
    {
        Scanner scanner = new Scanner(string);
        scanner.useDelimiter(";");
        
        List<AbridgedAreaDescription> areas = Lists.newArrayList();
        
        while (scanner.hasNext())
            areas.add(parseArea(scanner.next()));
        return areas;
    }
    
    public static AbridgedAreaDescription parseArea(String string)
    {
        Scanner scanner = new Scanner(string);
        scanner.useDelimiter(Pattern.compile("[ \\n\\t:\\(\\\"),]"));
        if (!scanner.hasNext("r"))
            return null;
        scanner.next("r");

        int id = Integer.valueOf(scanner.next()).intValue();
        ImmutableList.Builder<Integer> xywh = new ImmutableList.Builder<Integer>();
        String next = nextString(scanner);
        xywh.add(Integer.valueOf(next));
        next = nextString(scanner);
        xywh.add(Integer.valueOf(next));
        next = nextString(scanner);
        xywh.add(Integer.valueOf(next));
        next = nextString(scanner);
        xywh.add(Integer.valueOf(next));
        
        next = nextString(scanner);
        String type = next;

        ImmutableList.Builder<AbridgedGateway> gateways = new ImmutableList.Builder<AbridgedGateway>();

        while (true)
        {
        	String g = nextString(scanner);
        	if (g == null)
        		break;
            if (!g.equals("g"))
                return null;
            
            int gid = Integer.valueOf(nextString(scanner)).intValue();
            ImmutableList.Builder<Double> xy = new ImmutableList.Builder<Double>();
            xy.add(Double.valueOf(nextString(scanner)));
            xy.add(Double.valueOf(nextString(scanner)));
            
            gateways.add(new AbridgedGateway(gid, xy.build()));
        }
        
        return new AbridgedAreaDescription(id, xywh.build(), type, gateways.build());
    }
    
    private static String nextString(Scanner s) {
    	if (!s.hasNext())
    		return null;
    	String ret = s.next().trim();
    	while (ret.length() == 0) {
        	if (!s.hasNext())
        		return null;
    		ret = s.next().trim();
    	}
    	return ret;
    }
    
    public static void main(String[] args) {
    	ImmutableList<Integer> xywh = new ImmutableList.Builder<Integer>().add(1, 2, 3, 4).build();
    	ImmutableList<Double> xy = new ImmutableList.Builder<Double>().add(5.0, 6.0).build();
    	ImmutableList<AbridgedGateway> gateways = new ImmutableList.Builder<AbridgedGateway>().add(new AbridgedGateway(7, xy)).build();
    	AbridgedAreaDescription aad = new AbridgedAreaDescription(0, xywh, "door", gateways);
    	System.out.println("Built aad:\n" + aad.toString());
    	AbridgedAreaDescription aad2 = AbridgedAreaDescriptions.parseArea(aad.toString());
    	System.out.println("Parsed aad:\n" + aad2.toString());
	}
}
