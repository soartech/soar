/*
Copyright 2007 Brian Tanner
http://rl-library.googlecode.com/
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


import java.util.Random;

import rlVizLib.general.ParameterHolder;
import rlVizLib.general.hasVersionDetails;

import org.rlcommunity.rlglue.codec.AgentInterface;
import org.rlcommunity.rlglue.codec.taskspec.TaskSpec;
import org.rlcommunity.rlglue.codec.types.Action;
import org.rlcommunity.rlglue.codec.types.Observation;
import org.rlcommunity.rlglue.codec.util.AgentLoader;

/**
 * Simple random agent that can do multidimensional continuous or discrete
 * actions.
 * @author btanner
 */
public class OctopusAgent implements AgentInterface {

    private Action action;
    double totalRew;
    
    // Simple discretization of the action space
    int numDiscretizedActions = 7;

    private Random random = new Random();
    TaskSpec TSO = null;
    
    public OctopusAgent() {
        this(getDefaultParameters());
    }
    
    public void agent_cleanup() {
        
    }
    
    public void agent_end(double reward) {
        // do something with the reward here, if you want
        totalRew += reward;

    }
    
    public void agent_init(String taskSpec) {
        TSO = new TaskSpec(taskSpec);
         
        action = new Action(TSO.getNumDiscreteActionDims(),TSO.getNumDiscreteActionDims());
        totalRew = 0;               
    }
    
    public Action agent_start(Observation o) {
        int len = o.doubleArray.length;
        int numCompartments = (len - 2) / 8;
        
        // Pick a random action according to the discretization
        int simpleAction = random.nextInt(numDiscretizedActions);
        
        // re-initialize the array to clear the previous actions
        action.doubleArray = new double[2 + 10 * 3];
        double[] actionArray = action.doubleArray;
        discretizeAction(simpleAction, numCompartments, actionArray);
        
        return action;
       
    }
    
    
    
    

    public OctopusAgent(ParameterHolder p) {
        super();
    }

    /**
     * Tetris doesn't really have any parameters
     * @return
     */
    public static ParameterHolder getDefaultParameters() {
        ParameterHolder p = new ParameterHolder();
        rlVizLib.utilities.UtilityShop.setVersionDetails(p, new DetailsProvider());
        return p;
    }

    public static void main(String[] args){
        AgentLoader L=new AgentLoader(new OctopusAgent());
        L.run();
    }
    

    

    private int step=0;
    public Action agent_step(double arg0, Observation o) {
        step++;
        totalRew += arg0;

        if(step%500==0)System.out.println("Agent on step: "+step);
        
        int len = o.doubleArray.length;
        
        /*
         * Octopus state space:
         * - angular position of the base (1 entry)
           - angular velocity of the base (1 entry)
           - particle description for each node along the (upper) side of the arm
                from base to tip (4 entries per node)
         * -particle description for each node along the (lower) side of the arm, 
                from base to tip (4 entries per node)
         * Each particle description consists of the following:
                - The x component of the particle’s position
                - The y component of the particle’s position
                - The x component of the particle’s velocity
                - The y component of the particle’s velocity
         */
        int numCompartments = (len - 2) / 8;
        
        // As an example, decode some of the state space: 
        // Get the information for the base and the lower last tail compartment
        double basePos = o.doubleArray[0];
        double baseVel = o.doubleArray[1];
        
        int tailIndex = 2 + 8 * (numCompartments - 1);
        double tailPos_x = o.doubleArray[tailIndex];
        double tailPos_y = o.doubleArray[tailIndex+1];
        double tailVel_x = o.doubleArray[tailIndex+2];
        double tailVel_y = o.doubleArray[tailIndex+3];
        
        // At the end, just choose to rotate the base, according to a 
        // simple discretization (see discretizeAction for more details)
        // Pick a random action according to the discretization
        int simpleAction = random.nextInt(numDiscretizedActions);
        
        // re-initialize the array to clear the previous actions
        action.doubleArray = new double[2 + 10 * 3];
        double[] actionArray = action.doubleArray;
        discretizeAction(simpleAction, numCompartments, actionArray);
        
        return action;
    }

    
    // Sample discretization
    /**
    - 0: rotate the base of the arm counter-clockwise
    - 1: rotate the base of the arm clockwise
    - 2: fully contract all dorsal muscles on the lower half of the arm
    - 3: fully contract all transversal muscles on the lower half of the arm
    - 4: fully contract all ventral muscles on the lower half of the arm
    - 5: fully contract all dorsal muscles on the upper half of the arm
    - 6: fully contract all transversal muscles on the upper half of the arm
    - 7: fully contract all ventral muscles on the upper half of the arm
     **/
    private void discretizeAction(int discreteAction, int numCompartments, double[] actionArray) {
        // check if this action is related to the base
        if (discreteAction == 0) {
            actionArray[0] = 1.0d;
        }
        if (discreteAction == 1) {
            actionArray[1] = 1.0d;
        }

        // otherwise, check which set of muscles to contract on the upper half
        for (int i = 0; i < numCompartments / 2; i++) {
            if (discreteAction == 2) {
                actionArray[2 + 3 * i] = 1.0d;
            }
            if (discreteAction == 3) {
                actionArray[2 + 3 * i + 1] = 1.0d;
            }
            if (discreteAction == 4) {
                actionArray[2 + 3 * i + 2] = 1.0d;
            }
        }
        // on the lower half
        for (int i = numCompartments / 2; i < numCompartments; i++) {
            if (discreteAction == 5) {
                actionArray[2 + 3 * i] = 1.0d;
            }
            if (discreteAction == 6) {
                actionArray[2 + 3 * i + 1] = 1.0d;
            }
            if (discreteAction == 7) {
                actionArray[2 + 3 * i + 2] = 1.0d;
            }
        }
    }
       
    public String agent_message(String theMessage) {
        return null;
    }   
}
/**
 * This is a little helper class that fills in the details about this environment
 * for the fancy print outs in the visualizer application.
 * @author btanner
 */
class DetailsProvider implements hasVersionDetails {

    public String getName() {
        return "Silly Octopus 1.0";
    }

    public String getShortName() {
        return "Octopus Agent";
    }

    public String getAuthors() {
        return "Monica Dinculescu, Brian Tanner";
    }

    public String getInfoUrl() {
        return "http://code.google.com/p/rl-library/wiki/RandomAgent";
    }

    public String getDescription() {
        return "RL-Library Java Version of the random agent.  Can handle multi dimensional continuous and discrete actions.";
    }
}

