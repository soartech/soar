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
 * This is the definition of an RLGlue 'engine'.  The network codec is one such engine.
 * <p>If you want to create your own network codec, or a 'direct-compile' RL-Glue 
 * engine (like the one in RL-Viz), you should implement this interface.
 * @since 2.0
 * @author btanner
 */
public interface RLGlueInterface {
	public String RL_init();
	public Observation_action RL_start();
        public Observation RL_env_start();
        public Reward_observation_terminal RL_env_step(Action theAction);
        public Action RL_agent_start(Observation theObservation);
        public Action RL_agent_step(double theReward, Observation theObservation);
        public void RL_agent_end(double theReward);
	public Reward_observation_action_terminal RL_step();
	public void RL_cleanup();
	public String RL_agent_message(String message);
	public String RL_env_message(String message);
	public double RL_return();
	public int RL_num_steps();
	public int RL_num_episodes();
	public int RL_episode(int numSteps);
}
