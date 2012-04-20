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
*  $Revision: 676 $
*  $Date: 2009-02-08 20:15:04 -0500 (Sun, 08 Feb 2009) $
*  $Author: brian@tannerpages.com $
*  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Java/examples/skeleton-sample/SkeletonEnvironment.java $
* 
*/

import org.rlcommunity.rlglue.codec.EnvironmentInterface;
import org.rlcommunity.rlglue.codec.types.Action;
import org.rlcommunity.rlglue.codec.types.Observation;
import org.rlcommunity.rlglue.codec.types.Reward_observation_terminal;
import org.rlcommunity.rlglue.codec.util.EnvironmentLoader;
import org.rlcommunity.rlglue.codec.taskspec.TaskSpecVRLGLUE3;
import org.rlcommunity.rlglue.codec.taskspec.TaskSpec;
import org.rlcommunity.rlglue.codec.taskspec.ranges.IntRange;
import org.rlcommunity.rlglue.codec.taskspec.ranges.DoubleRange;

/**
 *  This is a very simple environment with discrete observations corresponding to states labeled {0,1,...,19,20}
    The starting state is 10.

    There are 2 actions = {0,1}.  0 decrements the state, 1 increments the state.

    The problem is episodic, ending when state 0 or 20 is reached, giving reward -1 or +1, respectively.  The reward is 0 on 
    all other steps.
 * @author Brian Tanner
 */
public class SkeletonEnvironment implements EnvironmentInterface {
    private int currentState=10;
    
    public String env_init() {
	
	//Create a task spec programatically.  This task spec encodes that state, action, and reward space for the problem.
	//You could forgo the task spec if your agent and environment have been created specifically to work with each other
	//ie, there is no need to share this information at run time.  You could also use your own ad-hoc task specification language,
	//or use the official one but just hard code the string instead of constructing it this way.
	    TaskSpecVRLGLUE3 theTaskSpecObject = new TaskSpecVRLGLUE3();
        theTaskSpecObject.setEpisodic();
        theTaskSpecObject.setDiscountFactor(1.0d);
	//Specify that there will be an integer observation [0,20] for the state
        theTaskSpecObject.addDiscreteObservation(new IntRange(0, 20));
	//Specify that there will be an integer action [0,1]
        theTaskSpecObject.addDiscreteAction(new IntRange(0, 1));
	//Specify the reward range [-1,1]
        theTaskSpecObject.setRewardRange(new DoubleRange(-1, 1));

        String taskSpecString = theTaskSpecObject.toTaskSpec();
        TaskSpec.checkTaskSpec(taskSpecString);

		//This actual string this makes is:
		//VERSION RL-Glue-3.0 PROBLEMTYPE episodic DISCOUNTFACTOR 1.0 OBSERVATIONS INTS (1 0 20)  ACTIONS INTS (1 0 1)  REWARDS (1 -1.0 1.0)  EXTRA
		
		//This could be simplified a bit if you made it manually to
		//VERSION RL-Glue-3.0 PROBLEMTYPE episodic DISCOUNTFACTOR 1.0 OBSERVATIONS INTS (0 20)  ACTIONS INTS (0 1)  REWARDS (-1.0 1.0)  EXTRA
		return taskSpecString;
    }

    public Observation env_start() {
        currentState=10;
        
        Observation returnObservation=new Observation(1,0,0);
        returnObservation.intArray[0]=currentState;
        return returnObservation;
    }

    public Reward_observation_terminal env_step(Action thisAction) {
        boolean episodeOver=false;
        double theReward=0.0d;
        
        if(thisAction.intArray[0]==0)
            currentState--;
        if(thisAction.intArray[0]==1)
            currentState++;
        
        if(currentState<=0){
            currentState=0;
            theReward=-1.0d;
            episodeOver=true;
        }
        
        if(currentState>=20){
            currentState=20;
            episodeOver=true;
            theReward=1.0d;
        }
        Observation returnObservation=new Observation(1,0,0);
        returnObservation.intArray[0]=currentState;
        
        Reward_observation_terminal returnRewardObs=new Reward_observation_terminal(theReward,returnObservation,episodeOver);
        return returnRewardObs;
    }

    public void env_cleanup() {
    }

    public String env_message(String message) {
        if(message.equals("what is your name?"))
            return "my name is skeleton_environment, Java edition!";

	return "I don't know how to respond to your message";
    }
    
   /**
     * This is a trick we can use to make the agent easily loadable.
     * @param args
     */
    public static void main(String[] args){
        EnvironmentLoader theLoader=new EnvironmentLoader(new SkeletonEnvironment());
        theLoader.run();
    }


}
