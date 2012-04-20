/*
 * Copyright 2008 Brian Tanner
 * http://rl-glue-ext.googlecode.com/
 * brian@tannerpages.com
 * http://brian.tannerpages.com
 * 
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *  $Revision: 991 $
 *  $Date: 2009-02-08 19:29:20 -0500 (Sun, 08 Feb 2009) $
 *  $Author: brian@tannerpages.com $
 *  $HeadURL: http://rl-library.googlecode.com/svn/trunk/projects/packages/examples/mines-sarsa-java/SampleSarsaAgent.java $
 *
 */

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Random;
import org.rlcommunity.rlglue.codec.AgentInterface;
import org.rlcommunity.rlglue.codec.types.Action;
import org.rlcommunity.rlglue.codec.types.Observation;
import org.rlcommunity.rlglue.codec.util.AgentLoader;
import org.rlcommunity.rlglue.codec.taskspec.TaskSpec;

/**
This is a very simple Sarsa agent for discrete-action, discrete-state
environments.  It uses epsilon-greedy exploration.

We've made a decision to store the previous action and observation in
their raw form, as structures.  This code could be simplified and you
could store them just as ints.
 * @author Brian Tanner
 */
public class SampleSarsaAgent implements AgentInterface {

    private Random randGenerator = new Random();
    private Action lastAction;
    private Observation lastObservation;
    private double[][] valueFunction = null;
    private double sarsa_stepsize = 0.1;
    private double sarsa_epsilon = 0.1;
    private double sarsa_gamma = 1.0;
    private int numActions = 0;
    private int numStates = 0;
    private boolean policyFrozen = false;
    private boolean exploringFrozen = false;

    /**
     * Parse the task spec, make sure it is only 1 integer observation and
     * action, and then allocate the valueFunction.
     *
     * @param taskSpecification
     */
    public void agent_init(String taskSpecification) {
        TaskSpec theTaskSpec = new TaskSpec(taskSpecification);

        /* Lots of assertions to make sure that we can handle this problem.  */
        assert (theTaskSpec.getNumDiscreteObsDims() == 1);
        assert (theTaskSpec.getNumContinuousObsDims() == 0);
        assert (!theTaskSpec.getDiscreteObservationRange(0).hasSpecialMinStatus());
        assert (!theTaskSpec.getDiscreteObservationRange(0).hasSpecialMaxStatus());
        numStates = theTaskSpec.getDiscreteObservationRange(0).getMax() + 1;

        assert (theTaskSpec.getNumDiscreteActionDims() == 1);
        assert (theTaskSpec.getNumContinuousActionDims() == 0);
        assert (!theTaskSpec.getDiscreteActionRange(0).hasSpecialMinStatus());
        assert (!theTaskSpec.getDiscreteActionRange(0).hasSpecialMaxStatus());
        numActions = theTaskSpec.getDiscreteActionRange(0).getMax() + 1;

        sarsa_gamma=theTaskSpec.getDiscountFactor();

        valueFunction = new double[numActions][numStates];

    }

    /**
     * Choose an action e-greedily from the value function and store the action
     * and observation.
     * @param observation
     * @return
     */
    public Action agent_start(Observation observation) {
        int newActionInt = egreedy(observation.getInt(0));

        /**
         * Create a structure to hold 1 integer action
         * and set the value
         */
        Action returnAction = new Action(1, 0, 0);
        returnAction.intArray[0] = newActionInt;

        lastAction = returnAction.duplicate();
        lastObservation = observation.duplicate();

        return returnAction;
    }

    /**
     * Choose an action e-greedily from the value function and store the action
     * and observation.  Update the valueFunction entry for the last
     * state,action pair.
     * @param reward
     * @param observation
     * @return
     */
    public Action agent_step(double reward, Observation observation) {
        int newStateInt = observation.getInt(0);
        int lastStateInt = lastObservation.getInt(0);
        int lastActionInt = lastAction.getInt(0);

        int newActionInt = egreedy(newStateInt);

        double Q_sa = valueFunction[lastActionInt][lastStateInt];
        double Q_sprime_aprime = valueFunction[newActionInt][newStateInt];

        double new_Q_sa = Q_sa + sarsa_stepsize * (reward + sarsa_gamma * Q_sprime_aprime - Q_sa);
        /*	Only update the value function if the policy is not frozen */
        if (!policyFrozen) {
            valueFunction[lastActionInt][lastStateInt] = new_Q_sa;
        }

        /* Creating the action a different way to showcase variety */
        Action returnAction = new Action();
        returnAction.intArray = new int[]{newActionInt};

        lastAction = returnAction.duplicate();
        lastObservation = observation.duplicate();

        return returnAction;
    }

    /**
     * The episode is over, learn from the last reward that was received.
     * @param reward
     */
    public void agent_end(double reward) {
        int lastStateInt = lastObservation.getInt(0);
        int lastActionInt = lastAction.getInt(0);

        double Q_sa = valueFunction[lastActionInt][lastStateInt];
        double new_Q_sa = Q_sa + sarsa_stepsize * (reward - Q_sa);

        /*	Only update the value function if the policy is not frozen */
        if (!policyFrozen) {
            valueFunction[lastActionInt][lastStateInt] = new_Q_sa;
        }
        lastObservation = null;
        lastAction = null;
    }

    /**
     * Release memory that is no longer required/used.
     */
    public void agent_cleanup() {
        lastAction = null;
        lastObservation = null;
        valueFunction = null;
    }

    /**
     * This agent responds to some simple messages for freezing learning and
     * saving/loading the value function to a file.
     * @param message
     * @return
     */
    public String agent_message(String message) {

        if (message.equals("freeze learning")) {
            policyFrozen = true;
            return "message understood, policy frozen";
        }
        if (message.equals("unfreeze learning")) {
            policyFrozen = false;
            return "message understood, policy unfrozen";
        }
        if (message.equals("freeze exploring")) {
            exploringFrozen = true;
            return "message understood, exploring frozen";
        }
        if (message.equals("unfreeze exploring")) {
            exploringFrozen = false;
            return "message understood, exploring unfrozen";
        }
        if (message.startsWith("save_policy")) {
            String[] parts = message.split(" ");
            saveValueFunction(parts[1]);
            System.out.println("Saved.");
            return "message understood, saving policy";
        }
        if (message.startsWith("load_policy")) {
            String[] parts = message.split(" ");
            loadValueFunction(parts[1]);
            System.out.println("Loaded.");
            return "message understood, loading policy";
        }

        return "SampleSarsaAgent(Java) does not understand your message.";

    }

    /**
     *
     * Selects a random action with probability 1-sarsa_epsilon,
     * and the action with the highest value otherwise.  This is a
     * quick'n'dirty implementation, it does not do tie-breaking.

     * @param theState
     * @return
     */
    private int egreedy(int theState) {
        if (!exploringFrozen) {
            if (randGenerator.nextDouble() <= sarsa_epsilon) {
                return randGenerator.nextInt(numActions);
            }
        }

        /*otherwise choose the greedy action*/
        int maxIndex = 0;
        for (int a = 1; a < numActions; a++) {
            if (valueFunction[a][theState] > valueFunction[maxIndex][theState]) {
                maxIndex = a;
            }
        }
        return maxIndex;
    }

    /**
     * This is a trick we can use to make the agent easily loadable.  Using this
     * trick you can directly execute the class and it will load itself through
     * AgentLoader and connect to the rl_glue server.
     * @param args
     */
    public static void main(String[] args) {
        AgentLoader theLoader = new AgentLoader(new SampleSarsaAgent());
        theLoader.run();
    }

    /**
     * Dumps the value function to a file named theFileName.  Not fancy. Must be
     * called after init but before cleanup.
     * @param theFileName
     */
    private void saveValueFunction(String theFileName) {
        try {
            DataOutputStream DO = new DataOutputStream(new FileOutputStream(new File(theFileName)));
            for (int a = 0; a < numActions; a++) {
                for (int s = 0; s < numStates; s++) {
                    DO.writeDouble(valueFunction[a][s]);
                }
            }
            DO.close();
        } catch (FileNotFoundException ex) {
            System.err.println("Problem saving value function to file: " + theFileName + " :: " + ex);
        } catch (IOException ex) {
            System.err.println("Problem writing value function to file:: " + ex);
        }

    }

    /**
     * Loads the value function from a file named theFileName.  Must be called after
     * init but before cleanup.
     * @param theFileName
     */
    private void loadValueFunction(String theFileName) {
        try {
            DataInputStream DI = new DataInputStream(new FileInputStream(new File(theFileName)));
            for (int a = 0; a < numActions; a++) {
                for (int s = 0; s < numStates; s++) {
                    valueFunction[a][s] = DI.readDouble();
                }
            }
            DI.close();
        } catch (FileNotFoundException ex) {
            System.err.println("Problem loading value function from file: " + theFileName + " :: " + ex);
        } catch (IOException ex) {
            System.err.println("Problem reading value function from file:: " + ex);
        }
    }
}
