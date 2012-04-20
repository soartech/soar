%  Copyright 2008 Brian Tanner
%  http://rl-glue-ext.googlecode.com/
%  brian@tannerpages.com
%  http://research.tannerpages.com
%  
%   Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at
%  
%       http://www.apache.org/licenses/LICENSE-2.0
%  
%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.
%  
%   $Revision: 637 $
%   $Date: 2009-02-07 16:02:45 -0500 (Sat, 07 Feb 2009) $
%   $Author: brian@tannerpages.com $
%  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Matlab/examples/skeleton/skeleton_environment.m $
%

%To use this Environment with the rest of the example together
%use the runAllTogether function.

% TO USE THIS Environment on its own[order doesn't matter]
%   -  Start the rl_glue executable socket server on your computer
%   -  Run the skeleton_agent and skeleton_experiment from a
%   different codec (Python, Java, C, Lisp should all be fine)
%   -  Load this environment like:
%       >> theEnvironment=skeleton_environment()
%       >> runEnvironment(theEnvironment);
%   NOTE: Type CTRL-C to abort the connection.
function theEnvironment=skeleton_environment()
%Assign members of the returning struct to be function pointers
	theEnvironment.env_init=@skeleton_environment_init;
	theEnvironment.env_start=@skeleton_environment_start;
	theEnvironment.env_step=@skeleton_environment_step;
	theEnvironment.env_cleanup=@skeleton_environment_cleanup;
	theEnvironment.env_message=@skeleton_environment_message;
 end

	% This is a very simple environment with discrete observations corresponding to states labeled {0,1,...,19,20}
	%     The starting state is 10.
	% 
	%     There are 2 actions = {0,1}.  0 decrements the state, 1 increments the state.
	% 
	%     The problem is episodic, ending when state 0 or 20 is reached, giving reward -1 or +1, respectively.  The reward is 0 on 
	%     all other steps.
function taskSpec=skeleton_environment_init()
	global skeleton_environment_struct;
	skeleton_environment_struct.currentState=10;

    taskSpec='VERSION RL-Glue-3.0 PROBLEMTYPE episodic DISCOUNTFACTOR 1.0 OBSERVATIONS INTS (0 20)  ACTIONS INTS (0 1)  REWARDS (-1.0 1.0)  EXTRA skeleton_environment(Matlab) by Brian Tanner.';
end    

function theObservation=skeleton_environment_start()
	global skeleton_environment_struct;
	skeleton_environment_struct.currentState=10;
	
	theObservation = org.rlcommunity.rlglue.codec.types.Observation();
	theObservation.intArray=[skeleton_environment_struct.currentState];
end

function rewardObservation=skeleton_environment_step(thisAction)
global skeleton_environment_struct;

	episodeOver=0;
	theReward=0;
	
	if thisAction.intArray(1)==0
		skeleton_environment_struct.currentState=skeleton_environment_struct.currentState-1;
	end

	if thisAction.intArray(1)==1
		skeleton_environment_struct.currentState=skeleton_environment_struct.currentState+1;
	end
	
	if skeleton_environment_struct.currentState <=0
		skeleton_environment_struct.currentState=0;
		theReward=-1;
		episodeOver=1;
	end

	if skeleton_environment_struct.currentState >=20
		skeleton_environment_struct.currentState=20;
		theReward=1;
		episodeOver=1;
	end

	theObservation = org.rlcommunity.rlglue.codec.types.Observation();
	theObservation.intArray=[skeleton_environment_struct.currentState];


	rewardObservation=org.rlcommunity.rlglue.codec.types.Reward_observation_terminal(theReward,theObservation,episodeOver);
end

function returnMessage=skeleton_environment_message(theMessageJavaObject)
%Java strings are objects, and we want a Matlab string
	inMessage=char(theMessageJavaObject);

	if strcmp(inMessage,'what is your name?')==1
		returnMessage='my name is skeleton_environment, Matlab edition!';
	else
		returnMessage='I don\''t know how to respond to your message';
    end
end

function skeleton_environment_cleanup()
	global skeleton_environment_struct;
	skeleton_environment_struct=rmfield(skeleton_environment_struct,'currentState');
	clear skeleton_environment_struct;

end
