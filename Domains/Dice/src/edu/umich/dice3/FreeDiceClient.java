package edu.umich.dice3;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import org.ini4j.Profile;
import org.ini4j.Wini;
import org.ini4j.spi.EscapeTool;

public class FreeDiceClient
{

    public static FreeDiceAgentConfiguration attemptConfig(Wini ini, String agentId, String beginTime)
    {
        return attemptConfig(ini, agentId, beginTime, null);
    }

    public static FreeDiceAgentConfiguration attemptConfig(Wini ini, String agentId, String beginTime, String writeOverride)
    {
        FreeDiceAgentConfiguration returnVal = null;

        try
        {

            if (!ini.containsKey(agentId))
            {
                throw new Exception("Invalid configuration: no agent section, agentId: " + agentId);
            }

            String[] paramNames = { "source", "write", "qna", "gp" };
            String[] paramValues = new String[paramNames.length];

            Profile.Section agentSection = ini.get(agentId);
            // EscapeTool esc = EscapeTool.getInstance();

            for (int i = 0; i < paramNames.length; ++i)
            {
                String paramName = paramNames[i];
                if (!agentSection.containsKey(paramName))
                {
                    throw new Exception("Invalid configuration: no agent." + paramName + " value");
                }
                String paramValue = agentSection.get(paramName);
                if (paramValue.startsWith("\"") && paramValue.endsWith("\""))
                {
                    paramValue = paramValue.substring(1, paramValue.length() - 1);
                }

                if (!paramValue.isEmpty())
                {
                    paramValues[i] = paramValue;
                }
            }

            // convert to absolute path
            /*
             * File filePath = new
             * File(esc.unquote(agentSection.get("source")));
             * 
             * File rlPath = null; String escapedPath =
             * esc.unquote(agentSection.get("rl")); if (!escapedPath.isEmpty())
             * { rlPath = new File(escapedPath); }
             */

            /*
             * // split up Map<String, String> qna = null; { if
             * (!esc.unquote(agentSection.get("qna")).isEmpty()) { String[]
             * qnaString = esc.unquote(agentSection.get("qna")).split(";");
             * 
             * if (qnaString.length != 0) { qna = new HashMap<String, String>();
             * 
             * for (int i = 0; i < qnaString.length; i++) { String[] temp =
             * qnaString[i].split("="); if (temp.length == 2) { qna.put(temp[0],
             * temp[1]); } } } } }
             */
            File iniFile = ini.getFile();
            File parentFile = iniFile.getAbsoluteFile().getParentFile();
            String parentPath = parentFile.getAbsolutePath();
            returnVal = new FreeDiceAgentConfiguration(parentPath, beginTime, paramValues[0], paramValues[1], paramValues[2], paramValues[3], writeOverride);
        }
        catch (Exception e)
        {
            e.printStackTrace();
            System.exit(1);
        }

        return returnVal;
    }
}
