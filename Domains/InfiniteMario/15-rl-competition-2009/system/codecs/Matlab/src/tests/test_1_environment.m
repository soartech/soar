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
%  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Matlab/src/tests/test_1_environment.m $
%
function theEnvironment=test_1_environment()
    theEnvironment.env_init=@test_1_environment_init;
    theEnvironment.env_start=@test_1_environment_start;
    theEnvironment.env_step=@test_1_environment_step;
    theEnvironment.env_cleanup=@test_1_environment_cleanup;
    theEnvironment.env_message=@test_1_environment_message;
end

function taskSpec=test_1_environment_init()
	taskSpec='sample task spec';
end    


function theObservation=test_1_environment_start()
    global test_1_environment_struct;
    test_1_environment_struct.stepCount=0;
    theObservation = org.rlcommunity.rlglue.codec.types.Observation();
	theObservation.intArray=[0];
	theObservation.doubleArray=[0/2 1/2];
	theObservation.charArray=['abc'];
end

function rewardObservation=test_1_environment_step(theAction)
    global test_1_environment_struct;

	theObservation = org.rlcommunity.rlglue.codec.types.Observation();
%Gabor has made it so this environment will step past terminal.  This is
%not something we want to do in general at all.
%But, in order to keep the other tests all working, I'll allow it

    if 5 > test_1_environment_struct.stepCount
    	theObservation.intArray=[test_1_environment_struct.stepCount];

        test_1_environment_struct.stepCount=test_1_environment_struct.stepCount+1;

    	terminal=0;
	
        if test_1_environment_struct.stepCount==5
            terminal=1;
        end
    	rewardObservation=org.rlcommunity.rlglue.codec.types.Reward_observation_terminal(1.0,theObservation,terminal);
    else
    	theObservation.intArray=[173 -173 2147483647 0 -2147483648];
    	theObservation.doubleArray=[0.0078125 -0.0078125 0.0 0.0078125e150 -0.0078125e150];
    	theObservation.charArray=['g' 'F' '?' ' ' '&'];
    	rewardObservation=org.rlcommunity.rlglue.codec.types.Reward_observation_terminal(-1.0,theObservation,0);
    end
	
end

function returnMessage=test_1_environment_message(theMessageJavaObject)
	global test_1_environment_struct;
    theMessage=char(theMessageJavaObject);
    
    timesToPrint=mod(test_1_environment_struct.stepCount,3);
    
    returnMessage=sprintf('%s|',theMessage);
    %Start at one because the top end will be inclusive unlike C for loop
    for i=1:1:timesToPrint
        returnMessage=sprintf('%s%d.',returnMessage,test_1_environment_struct.stepCount);
    end
    returnMessage=sprintf('%s|%s',returnMessage,theMessage);
end


function test_1_environment_cleanup()
end
