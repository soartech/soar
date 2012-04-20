/* 
* Copyright (C) 2008, Brian Tanner

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

#include <string.h> /*strcmp*/
#include <stdio.h> /*printf*/
#include <stdlib.h>

#include <rlglue/Environment_common.h>/* env_ function prototypes and RL-Glue types */
#include <rlglue/utils/C/RLStruct_util.h> /* helpful functions for allocating structs and cleaning them up */

/* This is a very simple environment with discrete observations corresponding to states labeled {0,1,...,19,20}
The starting state is 10.

There are 2 actions = {0,1}.  0 decrements the state, 1 increments the state.

The problem is episodic, ending when state 0 or 20 is reached, giving reward -1 or +1, respectively.  The reward is 0 on 
all other steps.
*/
  
observation_t this_observation;
reward_observation_terminal_t this_reward_observation;
int current_state=0;


const char* env_init()
{    
	char* task_spec="VERSION RL-Glue-3.0 PROBLEMTYPE episodic DISCOUNTFACTOR 1.0 OBSERVATIONS INTS (0 20) ACTIONS INTS (0 1)  REWARDS (-1.0 1.0)  EXTRA skeleton_environment(C/C++) by Brian Tanner.";

	/* Allocate the observation variable */
	allocateRLStruct(&this_observation,1,0,0);
	/* That is equivalent to:
		 this_observation.numInts     =  1;
		 this_observation.intArray    = (int*)calloc(1,sizeof(int));
		 this_observation.numDoubles  = 0;
		 this_observation.doubleArray = 0;
		 this_observation.numChars    = 0;
		 this_observation.charArray   = 0;
	*/
	/* Setup the reward_observation variable */
	this_reward_observation.observation=&this_observation;
	this_reward_observation.reward=0;
	this_reward_observation.terminal=0;

   return task_spec;
}

const observation_t *env_start()
{ 
	current_state=10;
	this_observation.intArray[0]=current_state;
  	return &this_observation;
}

const reward_observation_terminal_t *env_step(const action_t *this_action)
{
	int episode_over=0;
	double the_reward=0;
	
	if(this_action->intArray[0]==0)
		current_state--;
	if(this_action->intArray[0]==1)
		current_state++;

	if(current_state<=0){
		current_state=0;
		episode_over=1;
		the_reward=-1;
	}
	if(current_state>=20){
		current_state=20;
		episode_over=1;
		the_reward=1;
	}

	this_reward_observation.observation->intArray[0] = current_state;
	this_reward_observation.reward = the_reward;
	this_reward_observation.terminal = episode_over;

	return &this_reward_observation;
}

void env_cleanup()
{
	clearRLStruct(&this_observation);
}

const char* env_message(const char* inMessage) {
	if(strcmp(inMessage,"what is your name?")==0)
		return "my name is skeleton_environment!";

	return "I don't know how to respond to your message";
}

