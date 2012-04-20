package edu.umich.dice3.args;

import java.util.HashMap;
import java.util.Map;

public class ArgsParser
{
    
    public static Map<String, String> parseArgs(String[] args)
    {
        Map<String, String> argsMap = new HashMap<String, String>();
        
        String key = null;
        
        for (String arg : args)
        {
            if (arg.startsWith("--"))
            {
                key = arg.substring(2);
            }
            else if (arg.startsWith("-"))
            {
                key = arg.substring(1);
            }
            else
            {
                if (key != null)
                {
                    argsMap.put(key, arg);
                }
                key = null;
            }
        }
        return argsMap;
    }
}
