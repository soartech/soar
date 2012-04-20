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
%  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Matlab/src/tests/test_message_environment.m $
%
function theEnvironment=test_message_environment()
    theEnvironment.env_init=@test_message_environment_init;
    theEnvironment.env_start=@test_message_environment_start;
    theEnvironment.env_step=@test_message_environment_step;
    theEnvironment.env_cleanup=@test_message_environment_cleanup;
    theEnvironment.env_message=@test_message_environment_message;
end

function taskSpec=test_message_environment_init()
    global test_message_environment_struct;

	test_message_environment_struct.emptyObservation=org.rlcommunity.rlglue.codec.types.Observation();
	taskSpec='';
end    

function theObservation=test_message_environment_start()
    global test_message_environment_struct;

	theObservation=test_message_environment_struct.emptyObservation;
end


function rewardObservation=test_message_environment_step(theAction)
    global test_message_environment_struct;

	rewardObservation=org.rlcommunity.rlglue.codec.types.Reward_observation_terminal();
	rewardObservation.o=test_message_environment_struct.emptyObservation;
end

function returnMessage=test_message_environment_message(theMessageJavaObject)
    theMessage=char(theMessageJavaObject);

	if isempty(theMessage)
		returnMessage='empty';
		return
	end

	if strcmp(theMessage,'null')
		returnMessage='';
		return
	end

	if strcmp(theMessage,'empty')
		returnMessage='';
		return
	end
	
	returnMessage=sprintf('%s',theMessage);
end