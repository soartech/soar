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
%  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Matlab/src/tests/test_speed_environment.m $
%


%Return an instance of this environment
function theEnvironment=test_speed_environment()
    theEnvironment=test_speed_environment_construct();
end


function theEnvironment=test_speed_environment_construct()
    theEnvironment.env_init=@test_speed_environment_init;
    theEnvironment.env_start=@test_speed_environment_start;
    theEnvironment.env_step=@test_speed_environment_step;
    theEnvironment.env_cleanup=@test_speed_environment_cleanup;
    theEnvironment.env_message=@test_speed_environment_message;
end

function taskSpec=test_speed_environment_init()
    global test_speed_environment_struct;

	test_speed_environment_struct.whichEpisode=0;
	test_speed_environment_struct.stepCount=0;
	taskSpec='';
end    
   

function theObservation=test_speed_environment_start()
    global test_speed_environment_struct;

	test_speed_environment_struct.whichEpisode=test_speed_environment_struct.whichEpisode+1;
	test_speed_environment_struct.stepCount=0;
	test_speed_environment_struct.o=org.rlcommunity.rlglue.codec.types.Observation;
	theObservation=test_speed_environment_struct.o;
end

function rewardObservation=test_speed_environment_step(theAction)
    global test_speed_environment_struct;
	test_speed_environment_struct.stepCount=test_speed_environment_struct.stepCount+1;

	        org.rlcommunity.rlglue.codec.tests.TestUtility.clean_abstract_type(test_speed_environment_struct.o);


	if mod(test_speed_environment_struct.whichEpisode,2) == 0
        org.rlcommunity.rlglue.codec.tests.TestUtility.set_k_ints_in_abstract_type(test_speed_environment_struct.o, 50000);
        org.rlcommunity.rlglue.codec.tests.TestUtility.set_k_doubles_in_abstract_type(test_speed_environment_struct.o, 50000);
		terminal=0;
		if(test_speed_environment_struct.stepCount==200)
			terminal=1;
        end
        rewardObservation=org.rlcommunity.rlglue.codec.types.Reward_observation_terminal(1.0,test_speed_environment_struct.o,terminal);	
	else
        org.rlcommunity.rlglue.codec.tests.TestUtility.set_k_ints_in_abstract_type(test_speed_environment_struct.o, 5);
        org.rlcommunity.rlglue.codec.tests.TestUtility.set_k_doubles_in_abstract_type(test_speed_environment_struct.o,5);
		terminal=0;
		if(test_speed_environment_struct.stepCount==5000)
			terminal=1;
		end
		rewardObservation=org.rlcommunity.rlglue.codec.types.Reward_observation_terminal(1.0,test_speed_environment_struct.o,terminal);	
	end
end
	


function test_speed_environment_cleanup()
end

function returnMessage=test_speed_environment_message(theMessageJavaObject)
	returnMessage='';
end