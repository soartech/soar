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
%  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Matlab/src/tests/test_empty_agent.m $
%
function theAgent=test_empty_agent()
    theAgent.agent_init=@test_empty_agent_init;
    theAgent.agent_start=@test_empty_agent_start;
    theAgent.agent_step=@test_empty_agent_step;
    theAgent.agent_end=@test_empty_agent_end;
    theAgent.agent_cleanup=@test_empty_agent_cleanup;
    theAgent.agent_message=@test_empty_agent_message;
end

function test_empty_agent_init(taskSpec)
    global test_empty_agent_struct;

	test_empty_agent_struct.whichEpisode=0;
	test_empty_agent_struct.emptyAction=org.rlcommunity.rlglue.codec.types.Action(0,0,0);
	test_empty_agent_struct.nonEmptyAction=org.rlcommunity.rlglue.codec.types.Action(7,3,1);

	test_empty_agent_struct.nonEmptyAction.intArray=[0 1 2 3 4 5 6];
	test_empty_agent_struct.nonEmptyAction.doubleArray=[0/3 1/3 2/3];
	test_empty_agent_struct.nonEmptyAction.charArray=['a'];
end    


function theAction=test_empty_agent_start(theObservation)
    global test_empty_agent_struct;
    test_empty_agent_struct.whichEpisode=test_empty_agent_struct.whichEpisode+1;

	if mod(test_empty_agent_struct.whichEpisode,2)==0
		theAction=test_empty_agent_struct.emptyAction;
	else
		theAction=test_empty_agent_struct.nonEmptyAction;
	end
end

function theAction=test_empty_agent_step(theReward, theObservation)
    global test_empty_agent_struct;

	if mod(test_empty_agent_struct.whichEpisode,2)==0
		theAction=test_empty_agent_struct.emptyAction;
	else
		theAction=test_empty_agent_struct.nonEmptyAction;
	end
end

function test_empty_agent_end(theReward)
end

function test_empty_agent_cleanup()
    global test_empty_agent_struct;
end

function returnMessage=test_empty_agent_message(theMessageJavaObject)
	returnMessage='';
end