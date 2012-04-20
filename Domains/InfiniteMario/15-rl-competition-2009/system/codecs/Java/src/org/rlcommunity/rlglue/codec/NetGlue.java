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
 *  $Revision: 638 $
 *  $Date: 2009-02-07 16:17:29 -0500 (Sat, 07 Feb 2009) $
 *  $Author: brian@tannerpages.com $
 *  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Java/src/org/rlcommunity/rlglue/codec/NetGlue.java $
 * 
 */
package org.rlcommunity.rlglue.codec;

import java.io.IOException;
import java.net.InetAddress;
import org.rlcommunity.rlglue.codec.network.Network;
import org.rlcommunity.rlglue.codec.network.RLGlueDisconnectException;
import org.rlcommunity.rlglue.codec.types.Action;
import org.rlcommunity.rlglue.codec.types.Observation;
import org.rlcommunity.rlglue.codec.types.Observation_action;
import org.rlcommunity.rlglue.codec.types.RL_abstract_type;
import org.rlcommunity.rlglue.codec.types.Reward_observation_action_terminal;
import org.rlcommunity.rlglue.codec.types.Reward_observation_terminal;

/**
 * This is the network connection for an experiment program that will talk to the 
 * C/C++ rl_glue executable over sockets.
 * @since 2.0
 * @author btanner
 */
public class NetGlue implements RLGlueInterface {

    private Network network;
    private String host = Network.kDefaultHost;
    private int port = Network.kDefaultPort;

    /**
     * @since 2.0
     * Sets default host and port
     */
    public NetGlue() {
        String envVariableHostString = System.getenv("RLGLUE_HOST");
        String envVariablePortString = System.getenv("RLGLUE_PORT");
        setHostAndPorts(envVariableHostString, envVariablePortString);

    }

    /**
     * @since 2.0
     * Custom host, default port
     */
    public NetGlue(String host) {
        setHostAndPorts(host, null);
    }

    /**
     * Specify custom host and port to connect to rl_glue 
     * @since 2.0
     */
    public NetGlue(String host, int port) {
        setHostAndPorts(host, "" + port);
    }

    /**
     * Try these new settings.  We'll only actually set them if they seem valid.
     * I realize it's bad that we have this copied in AgentLoader, EnvLoader, and netGlue.
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
            }
        }
    }

    public synchronized String RL_init() {
        forceConnection();

        sendEmpty(Network.kRLInit, "RL_init");
        String task_spec = network.getString();
        return task_spec;
    }

    public synchronized Observation_action RL_start() {
        sendEmpty(Network.kRLStart, "RL_start");
        Observation_action obsact = new Observation_action();

        obsact.o = network.getObservation();
        obsact.a = network.getAction();
        return obsact;
    }
    public synchronized Observation RL_env_start() {
        sendEmpty(Network.kRLEnvStart, "RL_env_start");
        Observation obs = new Observation();

        obs = network.getObservation();
        return obs;
    }

    public synchronized Reward_observation_terminal RL_env_step(Action theAction) {
        send_abstract_type(theAction, Network.kRLEnvStep, "RL_env_step");
        Reward_observation_terminal rot = new Reward_observation_terminal();
        rot.terminal = network.getInt();
        rot.r = network.getDouble();
        rot.o = network.getObservation();
        return rot;
    }

    public synchronized Action RL_agent_start(Observation theObservation) {
        send_abstract_type(theObservation,Network.kRLAgentStart,"RL_agent_start");
        Action theAction=network.getAction();
        return theAction;
    }

    public synchronized Action RL_agent_step(double theReward, Observation theObservation) {
        send_reward_observation(theReward,theObservation,Network.kRLAgentStep,"RL_agent_step");
        Action theAction=network.getAction();
        return theAction;
    }

    public synchronized void RL_agent_end(double theReward) {
        sendDouble(theReward,Network.kRLAgentEnd,"RL_agent_end");
    }

    
    public synchronized Reward_observation_action_terminal RL_step() {
        sendEmpty(Network.kRLStep, "RL_step");

        Reward_observation_action_terminal roat = new Reward_observation_action_terminal();
        roat.terminal = network.getInt();
        roat.r = network.getDouble();
        roat.o = network.getObservation();
        roat.a = network.getAction();

        return roat;
    }

    public synchronized void RL_cleanup() {
        sendEmpty(Network.kRLCleanup, "RL_cleanup");
    }

    public synchronized String RL_agent_message(String message) {
        forceConnection();

        sendString(message, Network.kRLAgentMessage, "RL_agent_message");
        String response = network.getString();
        return response;

    }

    public synchronized String RL_env_message(String message) {
        forceConnection();

        sendString(message, Network.kRLEnvMessage, "RL_env_message");
        String response = network.getString();
        return response;
    }

    public synchronized double RL_return() {
        sendEmpty(Network.kRLReturn, "RL_return");

        double reward = network.getDouble();

        return reward;
    }

    public synchronized int RL_num_steps() {
        sendEmpty(Network.kRLNumSteps, "RL_num_steps");

        int numSteps = network.getInt();

        return numSteps;
    }

    public synchronized int RL_num_episodes() {
        sendEmpty(Network.kRLNumEpisodes, "RL_num_episodes");

        int numEpisodes = network.getInt();

        return numEpisodes;
    }

    public synchronized int RL_episode(int numSteps) {
        sendInt(numSteps, Network.kRLEpisode, "RL_episode");
        int exitStatus = network.getInt();
        return exitStatus;
    }

    /**
     * 
     * 
     * 
     * PRIVATE METHODS BELOW ARE HELPERS
     * 
     * 
     * 
     */
    private void forceConnection() {
        if (network == null) {
            String ImplementationVersion = RLGlueCore.getImplementationVersion();
            String SpecVersion = RLGlueCore.getSpecVersion();

            System.out.println("RL-Glue Java Experiment Codec Version: " + SpecVersion + " (" + ImplementationVersion + ")");
            System.out.println("\tConnecting to " + host + " on port " + port + "...");

            network = new Network();



            // Connect
            network.connect(host, port, Network.kRetryTimeout);
            System.out.println("\tExperiment Codec Connected");
            network.clearSendBuffer();
            network.putInt(Network.kExperimentConnection);
            network.putInt(0);
            network.flipSendBuffer();

            try {
                network.send();
            } catch (IOException ioException) {
                ioException.printStackTrace();
               System.exit(1);
            }
        }
    }

    /**
     * This method is called by the experiment program codec after sending something.
     * February 9 2009 :: I am making it robust to non-blocking network.recv.
     * @param state
     * @throws java.io.IOException
     */
    private synchronized void doStandardRecv(int state) throws IOException {
        network.clearRecvBuffer();
        try{
        int actualRecvSize=network.recv(8);
        int recvSizeWithoutHeader = actualRecvSize-8;

        int glueState = network.getInt(0);
        int dataSize = network.getInt(Network.kIntSize);
        int remaining = dataSize - recvSizeWithoutHeader;

        if (remaining < 0) {
            remaining = 0;
        }
        int remainingReceived = network.recv(remaining);

        network.flipRecvBuffer();

        // Discard the header - we should have a more elegant method for doing this.
        network.getInt();
        network.getInt();

        if (glueState != state) {
            System.err.println("Not synched with server. glueState = " + glueState + " but should be " + state);
           System.exit(1);
        }
        }catch(RLGlueDisconnectException e){
            System.err.println(e.getMessage());
           System.exit(1);
        }
    }

    private synchronized void doCallWithNoParams(int state) throws IOException {
        network.clearSendBuffer();
        network.putInt(state);
        network.putInt(0);
        network.flipSendBuffer();
        network.send();
    }

    private synchronized void sendString(String theString, int theCode, String callerName) {
        try {
            network.clearSendBuffer();
            network.putInt(theCode);
            network.putInt(Network.sizeOf(theString));
            network.putString(theString);
            network.flipSendBuffer();
            network.send();

            doStandardRecv(theCode);

        } catch (IOException ioException) {
            ioException.printStackTrace();
           System.exit(1);
        } catch (NullPointerException nullException) {
            System.err.println("You must call RL_init before calling " + callerName);
            nullException.printStackTrace();
           System.exit(1);
        }

    }

    private synchronized void sendEmpty(int theCode, String callerName) {
        try {
            doCallWithNoParams(theCode);
            doStandardRecv(theCode);
        } catch (IOException ioException) {
            ioException.printStackTrace();
           System.exit(1);
        } catch (NullPointerException nullException) {
            System.err.println("You must call RL_init before calling " + callerName);
            nullException.printStackTrace();
           System.exit(1);
        }

    }

    private synchronized void sendInt(int intToSend, int theCode, String callerName) {
        try {
            network.clearSendBuffer();
            network.putInt(theCode);
            network.putInt(Network.kIntSize);
            network.putInt(intToSend);
            network.flipSendBuffer();
            network.send();

            doStandardRecv(theCode);
        } catch (IOException ioException) {
            ioException.printStackTrace();
           System.exit(1);
        } catch (NullPointerException nullException) {
            System.err.println("You must call RL_init before calling " + callerName);
            nullException.printStackTrace();
           System.exit(1);
        }

    }
    private synchronized void sendDouble(double doubleToSend, int theCode, String callerName) {
        try {
            network.clearSendBuffer();
            network.putInt(theCode);
            network.putInt(Network.sizeOf(doubleToSend));
            network.putDouble(doubleToSend);
            network.flipSendBuffer();
            network.send();

            doStandardRecv(theCode);
        } catch (IOException ioException) {
            ioException.printStackTrace();
           System.exit(1);
        } catch (NullPointerException nullException) {
            System.err.println("You must call RL_init before calling " + callerName);
            nullException.printStackTrace();
           System.exit(1);
        }

    }
    /**
     * Added by Brian Tanner to simplify the code in here.
     * @param theObject
     * @param theCode
     * @param callerName
     */
    private synchronized void send_abstract_type(RL_abstract_type theObject, int theCode, String callerName) {
        try {
            network.clearSendBuffer();
            network.putInt(theCode);
            network.putInt(Network.sizeOf(theObject));
            network.putAbstractType(theObject);
            network.flipSendBuffer();
            network.send();

            doStandardRecv(theCode);
        } catch (IOException ioException) {
            ioException.printStackTrace();
           System.exit(1);
        } catch (NullPointerException nullException) {
            System.err.println("You must call RL_init before calling " + callerName);
            nullException.printStackTrace();
           System.exit(1);
        }

    }
    /**
     * Added by Brian Tanner to simplify the code in here.
     * @param theObject
     * @param theCode
     * @param callerName
     */
    private synchronized void send_reward_observation(double theReward,RL_abstract_type theObservation, int theCode, String callerName) {
        try {
            network.clearSendBuffer();
            network.putInt(theCode);
            network.putInt(Network.sizeOf(theObservation)+Network.sizeOf(theReward));
            network.putDouble(theReward);
            network.putAbstractType(theObservation);
            network.flipSendBuffer();
            network.send();

            doStandardRecv(theCode);
        } catch (IOException ioException) {
            ioException.printStackTrace();
           System.exit(1);
        } catch (NullPointerException nullException) {
            System.err.println("You must call RL_init before calling " + callerName);
            nullException.printStackTrace();
           System.exit(1);
        }

    }

}
