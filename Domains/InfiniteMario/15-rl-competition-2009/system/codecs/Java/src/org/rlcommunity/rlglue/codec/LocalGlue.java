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

import org.rlcommunity.rlglue.codec.types.Reward_observation_terminal;
import org.rlcommunity.rlglue.codec.types.Reward_observation_action_terminal;

/**
 * This is a local implementation of RL-Glue. It should be identical in behavior
 * to the RL-Glue code in the C/C++ RLGlueCore project.
 * @since 2.03
 * @author btanner
 */
public class LocalGlue implements RLGlueInterface {

    EnvironmentInterface E = null;
    AgentInterface A = null;
    Action lastAction = null;
    boolean isTerminal = false;
    int numSteps = 0;
    double totalReward = 0.0d;
    int numEpisodes = 0;

    public LocalGlue(EnvironmentInterface E, AgentInterface A) {
        this.E = E;
        this.A = A;
    }

    public synchronized String RL_env_message(String theString) {
        String incomingMessage = theString;
        if (incomingMessage == null) {
            incomingMessage = "";
        }
        String returnMessage = E.env_message(incomingMessage);
        if (returnMessage == null) {
            returnMessage = "";
        }
        return returnMessage;
    }

    public synchronized String RL_agent_message(String theString) {
        String incomingMessage = theString;
        if (incomingMessage == null) {
            incomingMessage = "";
        }
        String returnMessage = A.agent_message(incomingMessage);
        if (returnMessage == null) {
            returnMessage = "";
        }
        return returnMessage;
    }

    public synchronized String RL_init() {
        String taskSpec = E.env_init();
        A.agent_init(taskSpec);
        numEpisodes = 0;
        numSteps = 0;
        return taskSpec;
    }

    public synchronized Observation_action RL_start() {
        Observation o = RL_env_start();
        lastAction = RL_agent_start(o);
        Observation_action ao = new Observation_action(o, lastAction);
        return ao;
    }

    public synchronized Observation RL_env_start() {
        numSteps = 1;
        isTerminal = false;
        totalReward = 0.0d;

        Observation o = E.env_start();
        if (o == null) {
            System.err.println("o came back as null from RL_start");
        }
        return o;
    }
    public synchronized Action RL_agent_start(Observation theObservation) {
        Action theAction=A.agent_start(theObservation);
            if (theAction == null) {
            System.err.println("theAction came back as null from RL_start");
        }
        return theAction;
    }
    
    public synchronized Reward_observation_terminal RL_env_step(Action theAction) {
        Reward_observation_terminal RO = E.env_step(theAction);
        if (RO == null) {
            System.err.println("RO came back as null from RL_step");
        }
        if (RO.getObservation() == null) {
            System.err.println("Ro.o came back as null from RL_step");
        }

        totalReward += RO.getReward();

        if (RO.isTerminal()) {
            numEpisodes++;
        } else {
            numSteps++;
        }
        return RO;
    }


    public synchronized Action RL_agent_step( double theReward, Observation theObservation) {
        Action theAction=A.agent_step(theReward, theObservation);
            if (theAction == null) {
                System.err.println("theAction came back as null from agent_step");
            }
        return theAction;
    }

    public synchronized void RL_agent_end(double theReward) {
        A.agent_end(theReward);
    }

    public synchronized Reward_observation_action_terminal RL_step() {
        if (lastAction == null) {
            System.err.println("lastAction came back as null from RL_step");
        }
        Reward_observation_terminal RO=RL_env_step(lastAction);
       

        if (RO.isTerminal()) {
            RL_agent_end(RO.getReward());
        } else {
           lastAction = RL_agent_step(RO.getReward(), RO.getObservation());
        }
        return new Reward_observation_action_terminal(RO.getReward(), RO.getObservation(), lastAction, RO.isTerminal());
    }

    public synchronized void RL_cleanup() {
        E.env_cleanup();
        A.agent_cleanup();
    }

//Btanner: Jan 13 : Changing this to make it more like RL_glue.c
//Btanner: Sept 19 2008 : Re-ported directly from RL_glue.c
    public synchronized int RL_episode(int maxStepsThisEpisode) {
        Reward_observation_action_terminal rlStepResult = new Reward_observation_action_terminal(0, null, null, 0);
        int currentStep = 0;
        RL_start();
        /* RL_start sets current step to 1, so we should start x at 1 */
        for (currentStep = 1; rlStepResult.terminal != 1 && (maxStepsThisEpisode == 0 ? true : currentStep < maxStepsThisEpisode); currentStep++) {
            rlStepResult = RL_step();
        }

        /*Return the value of terminal to tell the caller whether the episode ended naturally or was cut off*/
        return rlStepResult.terminal;
    }

    public synchronized int RL_num_episodes() {
        return numEpisodes;
    }

    public synchronized int RL_num_steps() {
        return numSteps;
    }

    public synchronized double RL_return() {
        return totalReward;
    }
}
