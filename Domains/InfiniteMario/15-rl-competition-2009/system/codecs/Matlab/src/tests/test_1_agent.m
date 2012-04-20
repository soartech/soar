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
%  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Matlab/src/tests/test_1_agent.m $
%
function theAgent=test_1_agent()
    theAgent.agent_init=@test_1_agent_init;
    theAgent.agent_start=@test_1_agent_start;
    theAgent.agent_step=@test_1_agent_step;
    theAgent.agent_end=@test_1_agent_end;
    theAgent.agent_cleanup=@test_1_agent_cleanup;
    theAgent.agent_message=@test_1_agent_message;
end


function test_1_agent_init(taskSpec)
    global test_1_agent_struct;
    test_1_agent_struct.stepCount=0;
end    

function theAction=test_1_agent_start(theObservation)
    global test_1_agent_struct;
    test_1_agent_struct.stepCount=0;
    theAction = org.rlcommunity.rlglue.codec.types.Action(theObservation);
end


function theAction=test_1_agent_step(theReward, theObservation)
    global test_1_agent_struct;
    test_1_agent_struct.stepCount=test_1_agent_struct.stepCount+1;
    theAction = org.rlcommunity.rlglue.codec.types.Action(theObservation);
end

function test_1_agent_end(theReward)
end

function returnMessage=test_1_agent_message(theMessageJavaObject)
  
    global test_1_agent_struct;
    if(isempty(test_1_agent_struct))
        test_1_agent_struct.stepCount=0;
    end
    if(isempty(test_1_agent_struct.stepCount))
        test_1_agent_struct.stepCount=0;
    end
    
    theMessage=char(theMessageJavaObject);
    
    timesToPrint=mod(test_1_agent_struct.stepCount,3);
    
    returnMessage=sprintf('%s|',theMessage);
    %Start at one because the top end will be inclusive unlike C for loop
    for i=1:1:timesToPrint
        returnMessage=sprintf('%s%d.',returnMessage,test_1_agent_struct.stepCount);
    end
    returnMessage=sprintf('%s|%s',returnMessage,theMessage);
end

function test_1_agent_cleanup()
    global test_1_agent_struct;
end