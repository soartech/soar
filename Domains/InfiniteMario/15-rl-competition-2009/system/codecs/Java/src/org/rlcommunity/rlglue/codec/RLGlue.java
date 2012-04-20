/*
Copyright 2007 Brian Tanner
brian@tannerpages.com
http://brian.tannerpages.com

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */
package org.rlcommunity.rlglue.codec;

import org.rlcommunity.rlglue.codec.types.Action;
import org.rlcommunity.rlglue.codec.types.Observation;
import org.rlcommunity.rlglue.codec.types.Observation_action;
import org.rlcommunity.rlglue.codec.types.Reward_observation_action_terminal;
import org.rlcommunity.rlglue.codec.types.Reward_observation_terminal;

/**
 * This is the main RLGlue interface class for Experiment Programs.
 * <p>This class is meant to be accessed statically, like:
 * <ul>
 * <li>RLGlue.RL_init()
 * <li>RLGlue.RL_episode(5)
 * <li>etc.
 * </ul>
 * <p>For future flexibility, we have made this class use an instance of type
 * RLGlueInterface.  This will allow RL-Viz and other project swap out the network
 * codec for a local (direct-compile) implementation without requiring any 
 * changes to the experiment program.
 * <p>Since this class offers some convenient extensions, the traditional RL-Glue
 * methods have all been labeled as 'RL-Glue Core Method'.
 * @author btanner
 */
public class RLGlue {

    private static RLGlueInterface instance = null;
    static boolean inited = false;
    static boolean currentEpisodeOver = true;

    /**
     * Non-Core Method.
     * <p> If you have an alternate RL-Glue engine (like the localGlue engine
     * from RL-Viz), you can use this method to specify the engine instead
     * of the network engine.
     * @param alternateGlueEngine The alternate glue engine.
     * @since 2.0
     */
    static public void setGlue(RLGlueInterface alternateGlueEngine) {
        resetGlueProxy();
        instance = alternateGlueEngine;
    }

    /**
     * Non-Core Method.
     * <p> If you wanted to run multiple independent experiments within a single
     * Experiment program, you could call this method in between.  This will disconnect
     * you from the glue.
     * <p> This has not been thoroughly tested : it is useful for RL-Viz.
     * @since 2.0
     */
    static public void resetGlueProxy() {
        instance = null;
        inited = false;
        currentEpisodeOver = true;
    }

    /**
     * Non-Core Method.
     * Has RL_init been called yet? (or since the last cleanup?)
     * @return Whether RL_init has been called.
     * @since 2.0
     */
    static public boolean isInited() {
        return inited;
    }

    /**
     *  RLGlue is only meant to be accessed statically, so this private 
     * constructor ensures we can never create an instance of RLGlue.
     */
    private RLGlue() {
    }

    private static void checkInstance() {
        if (instance == null) {
            instance = new NetGlue();
        }
    }

    /**
     * RL-Glue Core Method.
     */
    public static String RL_agent_message(String message) {
        checkInstance();
        if (message == null) {
            message = "";
        }
        String response = instance.RL_agent_message(message);
        if (response == null) {
            response = "";
        }
        return response;
    }

    /**
     * RL-Glue Core Method.
     */
    public static String RL_env_message(String message) {
        checkInstance();
        if (message == null) {
            message = "";
        }
        String response = instance.RL_env_message(message);
        if (response == null) {
            response = "";
        }
        return response;
    }

    /**
     * RL-Glue Core Method.
     */
    public static void RL_cleanup() {
        checkInstance();
        if (!inited) {
            System.err.println("-- Warning From RLGlue :: RL_cleanup() was called without matching RL_init() call previously.");
        }
        instance.RL_cleanup();
        inited = false;
    }

    /**
     * RL-Glue Core Method.
     */
    public static int RL_episode(int numSteps) {
        checkInstance();
        int terminationCondition = instance.RL_episode(numSteps);
        return terminationCondition;
    }


    /**
     * RL-Glue Core Method.
     */
    public static String RL_init() {
        checkInstance();
        if (inited) {
            System.err.println("-- Warning From RLGlue :: RL_init() was called more than once without RL_cleanup.");
        }
        String taskSpec = instance.RL_init();
        if (taskSpec == null) {
            taskSpec = "";
        }
        inited = true;
        currentEpisodeOver = true;
        return taskSpec;
    }

    /**
     * RL-Glue Core Method.
     */
    public static int RL_num_episodes() {
        checkInstance();
        return instance.RL_num_episodes();
    }

    /**
     * RL-Glue Core Method.
     */
    public static int RL_num_steps() {
        checkInstance();
        return instance.RL_num_steps();
    }

    /**
     * RL-Glue Core Method.
     */
    public static double RL_return() {
        checkInstance();
        return instance.RL_return();
    }


    /**
     * RL-Glue Core Method.
     */
    public static Observation_action RL_start() {
        checkInstance();
        if (!inited) {
            System.err.println("-- Warning From RLGlue :: RL_start() was called without RL_init().");
        }
        currentEpisodeOver = false;
        Observation_action returnObs = instance.RL_start();
        if (returnObs == null) {
            System.err.println("-- Warning From RLGlue :: RL_start() response was NULL, that should be impossible.");
            returnObs = new Observation_action();
        }
        return returnObs;
    }
    
    public static Observation RL_env_start(){
        checkInstance();
        if (!inited) {
            System.err.println("-- Warning From RLGlue :: RL_env_start() was called without RL_init().");
        }
        currentEpisodeOver = false;
        Observation returnObs = instance.RL_env_start();
        if (returnObs == null) {
            System.err.println("-- Warning From RLGlue :: RL_env_start() response was NULL, that should be impossible.");
            returnObs = new Observation();
        }
        return returnObs;
    }
    
    public static Action RL_agent_start(Observation theObservation){
        checkInstance();
        if (!inited) {
            System.err.println("-- Warning From RLGlue :: RL_agent_start() was called without RL_init().");
        }
        Action returnAct=instance.RL_agent_start(theObservation);
        if (returnAct == null) {
            System.err.println("-- Warning From RLGlue :: RL_agent_start() response was NULL, that should be impossible.");
            returnAct = new Action();
        }
        return returnAct;
    }

    /**
     * RL-Glue Core Method.
     */
    public static Reward_observation_action_terminal RL_step() {
        checkInstance();
        if (!inited) {
            System.err.println("-- Warning From RLGlue :: RL_step() was called without RL_init().");
        }
        Reward_observation_action_terminal stepResponse = instance.RL_step();
        if (stepResponse == null) {
            System.err.println("-- Warning From RLGlue :: RL_step() response was NULL, that should be impossible.");
            stepResponse = new Reward_observation_action_terminal();
        }

        currentEpisodeOver = (stepResponse.terminal == 1);
        return stepResponse;
    }
    /**
     * RL-Glue Core Method.
     */
    public static Reward_observation_terminal RL_env_step(Action theAction) {
        checkInstance();
        if (!inited) {
            System.err.println("-- Warning From RLGlue :: RL_env_step() was called without RL_init().");
        }
        Reward_observation_terminal stepResponse = instance.RL_env_step(theAction);
        if (stepResponse == null) {
            System.err.println("-- Warning From RLGlue :: RL_env_step() response was NULL, that should be impossible.");
            stepResponse = new Reward_observation_terminal();
        }
        currentEpisodeOver = stepResponse.isTerminal();
        return stepResponse;
    }
    
    
    public static Action RL_agent_step(double theReward, Observation theObservation){
        checkInstance();
        if (!inited) {
            System.err.println("-- Warning From RLGlue :: RL_agent_step() was called without RL_init().");
        }
        Action theAction=instance.RL_agent_step(theReward, theObservation);
        if (theAction == null) {
            System.err.println("-- Warning From RLGlue :: RL_agent_step() response was NULL, that should be impossible.");
            theAction = new Action();
        }
        return theAction;
    }
    public static void RL_agent_end(double theReward){
        checkInstance();
        if (!inited) {
            System.err.println("-- Warning From RLGlue :: RL_agent_end() was called without RL_init().");
        }
        instance.RL_agent_end(theReward);
    }

    /**
     * Non-Core Method.  Returns whether the current episode is active or not.
     * <p>Will return true BEFORE RL_start is called, and after a terminal response
     * has been returned from the environment (until next RL_start call).
     */
    public static boolean isCurrentEpisodeOver() {
        return currentEpisodeOver;
    }
}
