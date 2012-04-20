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
 *  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Java/src/org/rlcommunity/rlglue/codec/util/EnvironmentLoader.java $
 * 
 */
package org.rlcommunity.rlglue.codec.util;

import java.net.InetAddress;
import org.rlcommunity.rlglue.codec.EnvironmentInterface;
import org.rlcommunity.rlglue.codec.RLGlueCore;
import org.rlcommunity.rlglue.codec.network.ClientEnvironment;
import org.rlcommunity.rlglue.codec.network.Network;

/**
 * This class can be called from the command line to load an environment and create
 * an executable RL environment program.  
 * 
 * We've recently refactored it to make it useful if anyone ever wants to create
 * local instances of network-bound environment from inside a JVM (like Matlab)
 * @author btanner
 */
public class EnvironmentLoader implements Runnable {

    String host = Network.kDefaultHost;
    int port = Network.kDefaultPort;
    final EnvironmentInterface theEnvironment;
    ClientEnvironment theClient = null;

    public EnvironmentLoader(EnvironmentInterface theEnvironment) {
        assert theEnvironment != null : "theEnvironment is null in EnvironmentLoader Constructor";
        this.theEnvironment = theEnvironment;
        String envVariableHostString = System.getenv("RLGLUE_HOST");
        String envVariablePortString = System.getenv("RLGLUE_PORT");
        setHostAndPorts(envVariableHostString, envVariablePortString);
    }

    public EnvironmentLoader(String hostString, String portString, EnvironmentInterface theEnvironment) {
        this(theEnvironment);
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

    public void killProcess() {
        theClient.killProcess();
    }

    public void run() {
        String ImplementationVersion = RLGlueCore.getImplementationVersion();
        String SpecVersion = RLGlueCore.getSpecVersion();

        System.out.println("RL-Glue Java Environment Codec Version: " + SpecVersion + " (" + ImplementationVersion + ")");
        System.out.println("\tConnecting to " + host + " on port " + port + "...");

        theClient = new ClientEnvironment(theEnvironment);
        try {
            theClient.connect(host, port, Network.kRetryTimeout);
            System.out.println("\tEnvironment Codec Connected");
            theClient.runEnvironmentEventLoop();
            theClient.close();
        } catch (Exception e) {
            System.err.println("EnvironmentLoader run(" + theEnvironment.getClass() + ") threw Exception: " + e);
            e.printStackTrace();
        }
    }

    /**
     * Loads the class envClassName as an rl-glue environment.
     * @param envClassName
     */
    public static EnvironmentLoader loadEnvironment(String envClassName) {
        EnvironmentInterface env = null;

        //Had to use the system classloader in case the codec was actually installed.
        //If its installed, it uses a different classloader.
        try {
            env = (EnvironmentInterface) ClassLoader.getSystemClassLoader().loadClass(envClassName).newInstance();
        } catch (Exception e) {
            System.err.println("loadEnvironment(" + envClassName + ") threw Exception: " + e);
            e.printStackTrace();
            System.exit(1);
        }

        return new EnvironmentLoader(env);
    }

    public static void main(String[] args) throws Exception {
        String usage = "java EnvironmentLoader <Environment> -classpath <Path To RLGlue>";

        String envVars = "The following environment variables are used by the environment to control its function:\n" +
                "RLGLUE_HOST  : If set the environment will use this ip or hostname to connect to rather than " + Network.kDefaultHost + "\n" +
                "RLGLUE_PORT  : If set the environment will use this port to connect on rather than " + Network.kDefaultPort + "\n";

        if (args.length < 1) {
            System.out.println(usage);
            System.out.println(envVars);
           System.exit(1);
        }
        EnvironmentLoader theEnvironmentLoader = loadEnvironment(args[0]);
        theEnvironmentLoader.run();
    }
}
