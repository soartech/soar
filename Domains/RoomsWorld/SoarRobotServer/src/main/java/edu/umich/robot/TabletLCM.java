package edu.umich.robot;

import java.io.IOException;
import java.util.HashMap;

import lcm.lcm.LCM;
import lcm.lcm.LCMDataInputStream;
import lcm.lcm.LCMSubscriber;

public class TabletLCM
{
	//private static final long MIN_INTERVAL = 100L;
	private static final long MIN_INTERVAL = 0L;
	
    private final LCM input;
    private final LCM output;
    private HashMap<String, Long> lastMessages;
    
    public TabletLCM(String connectionString) throws IOException, IllegalArgumentException
    {
    	lastMessages = new HashMap<String, Long>();
        output = new LCM(connectionString);
        input = LCM.getSingleton();
        input.subscribe("POSE_seek", subscriber);
        input.subscribe("SIM_LIDAR_FRONT_seek", subscriber);
        input.subscribe("LIDAR_LOWRES_seek", subscriber);
        input.subscribe("WAYPOINTS_seek", subscriber);
        input.subscribe("MAP_VISIBILITY_seek", subscriber);
        input.subscribe("SIM_OBSTACLES", subscriber);
        input.subscribe("MAP_OBJECTS", subscriber);
    }
    
    private final LCMSubscriber subscriber = new LCMSubscriber() 
    {
        public void messageReceived(LCM lcm, String channel, LCMDataInputStream ins) 
        {
            try
            {
            	long now = System.currentTimeMillis();
            	if (!lastMessages.containsKey(channel) || now - lastMessages.get(channel) > MIN_INTERVAL) {
            		output.publish(channel, ins.getBuffer(), ins.getBufferOffset(), ins.available());
                	lastMessages.put(channel, now);
            	}
            }
            catch (IOException e)
            {
                e.printStackTrace();
            }
        }
    };

    public void close()
    {
        input.unsubscribe("POSE_seek", subscriber);
        input.unsubscribe("SIM_LIDAR_FRONT_seek", subscriber);
        input.unsubscribe("LIDAR_LOWRES_seek", subscriber);
        input.unsubscribe("WAYPOINTS_seek", subscriber);
        input.subscribe("MAP_VISIBILITY_seek", subscriber);
        input.unsubscribe("SIM_OBSTACLES", subscriber);
        input.unsubscribe("MAP_OBJECTS", subscriber);
        output.close();
    }
}
