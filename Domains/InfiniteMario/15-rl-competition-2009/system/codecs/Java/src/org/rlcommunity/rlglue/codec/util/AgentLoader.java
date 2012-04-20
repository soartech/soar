/* 
 * Copyright (C) 2007, Brian Tanner
 * 
http://rl-glue-ext.googlecode.com/
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 * 
 *  $Revision: 802 $
 *  $Date: 2009-02-27 21:42:44 -0500 (Fri, 27 Feb 2009) $
 *  $Author: brian@tannerpages.com $
 *  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Java/src/org/rlcommunity/rlglue/codec/util/AgentLoader.java $
 * 
 */
package org.rlcommunity.rlglue.codec.util;

import java.net.InetAddress;
import org.rlcommunity.rlglue.codec.AgentInterface;
import org.rlcommunity.rlglue.codec.RLGlueCore;
import org.rlcommunity.rlglue.codec.network.ClientAgent;
import org.rlcommunity.rlglue.codec.network.Network;

/**
 * This class can be called from the command line to load an agent and create
 * an executable RL agent program.  
 * 
 * We've recently refactored it to make it useful if anyone ever wants to create
 * local instances of network-bound agents from inside a JVM (like Matlab)
 * @author btanner
 */
public class AgentLoader implements Runnable {

    String host = Network.kDefaultHost;
    int port = Network.kDefaultPort;
    AgentInterface theAgent = null;
    ClientAgent theClient = null;

    public AgentLoader(AgentInterface theAgent) {
        this.theAgent = theAgent;
        String envVariableHostString = System.getenv("RLGLUE_HOST");
        String envVariablePortString = System.getenv("RLGLUE_PORT");
        setHostAndPorts(envVariableHostString, envVariablePortString);
    }

    public AgentLoader(String hostString, String portString, AgentInterface theAgent) {
        this(theAgent);
        setHostAndPorts(hostString, portString);
    }

    /**
     * Try these new settings.  We'll only actually set them if they seem valid.
     * @param hostString
     * @param portString
     */
    private void setHostAndPorts(String hostString, String portString) {

        //Now override the default or env variable port and string with these specific settings
        if (hostString != null) {
            try {
                InetAddress theAddress = InetAddress.getByName(hostString);
                host = hostString;
            } catch (Exception e) {
                System.err.println("Problem resolving requested hostname: " + hostString + " so using default.");
            }
        }

        if (portString != null) {

            try {
                int parsedPort = Integer.parseInt(portString);

                if (parsedPort < 0 || parsedPort > 65535) {
                    System.err.println("Could not use port you requested: " + parsedPort + " is not a valid port number.\n");
                } else {
                    port = parsedPort;
                }
            } catch (Exception e) {
                System.err.println("Could not use port you requested: " + portString + " could not be parsed as an int.");
                e.printStackTrace();
            }
        }
    }

    /**
     * Loads the class agentClassName as an rl-glue agent.
     * Doesn't make much sense to do the work in here... we can do it in a constructor
     * and then get things straight about who is supposed to be setting things from 
     * the environment variables.
    
     * @param agentClassName
     */
    public static AgentLoader loadAgent(String agentClassName) {
        AgentInterface agent = null;
        try {
            //Have to use the system classloader because if the codec is installed,
            //class.forName will use the ext classloader which won't find your classes.
            agent = (AgentInterface) ClassLoader.getSystemClassLoader().loadClass(agentClassName).newInstance();
        } catch (Exception ex) {
            System.err.println("loadAgent(" + agentClassName + ") threw Exception: " + ex);
            ex.printStackTrace();
            System.exit(1);
        }
        AgentLoader theLoader = new AgentLoader(agent);
        return theLoader;
    }

    public void killProcess() {
        theClient.killProcess();
    }

    public void run() {
        String ImplementationVersion = RLGlueCore.getImplementationVersion();
        String SpecVersion = RLGlueCore.getSpecVersion();

        System.out.println("RL-Glue Java Agent Codec Version: " + SpecVersion + " (" + ImplementationVersion + ")");
        System.out.println("\tConnecting to " + host + " on port " + port + "...");
        theClient = new ClientAgent(theAgent);

        try {
            theClient.connect(host, port, Network.kRetryTimeout);
            System.out.println("\tAgent Codec Connected");
            theClient.runAgentEventLoop();
            theClient.close();
        } catch (Exception e) {
            System.err.println("AgentLoader run(" + theAgent.getClass() + ") threw Exception: " + e);
            e.printStackTrace();
        }
    }

    public static void main(String[] args) throws Exception {
        String usage = "java AgentLoader <Agent> -classpath <Path To RLGlue>";

        String envVars = "The following environment variables are used by the agent to control its function:\n" +
                "RLGLUE_HOST  : If set the agent will use this ip or hostname to connect to rather than " + Network.kDefaultHost + "\n" +
                "RLGLUE_PORT  : If set the agent will use this port to connect on rather than " + Network.kDefaultPort + "\n";
        if (args.length < 1) {
            System.out.println(usage);
            System.out.println(envVars);
           System.exit(1);
        }
        AgentLoader theLoader = loadAgent(args[0]);
        theLoader.run();
    }
}
