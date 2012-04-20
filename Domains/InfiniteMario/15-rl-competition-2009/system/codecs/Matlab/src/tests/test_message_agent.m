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
%  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Matlab/src/tests/test_message_agent.m $
%
function theAgent=test_message_agent()
    theAgent.agent_init=@test_message_agent_init;
    theAgent.agent_start=@test_message_agent_start;
    theAgent.agent_step=@test_message_agent_step;
    theAgent.agent_end=@test_message_agent_end;
    theAgent.agent_cleanup=@test_message_agent_cleanup;
    theAgent.agent_message=@test_message_agent_message;
end

function test_message_agent_init(taskSpec)
end    

function theAction=test_message_agent_start(theObservation)
	theAction=org.rlcommunity.rlglue.codec.types.Action(0,0,0);
end

function theAction=test_message_agent_step(theReward, theObservation)
	theAction=org.rlcommunity.rlglue.codec.types.Action(0,0,0);
end

function test_message_agent_end(theReward)
end

function test_message_agent_cleanup()
end

function returnMessage=test_message_agent_message(theMessageJavaObject)
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
