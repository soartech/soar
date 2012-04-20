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
%  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Matlab/src/glue/RL_episode.m $
%
function exitStatus=RL_episode(numSteps)
    global p__rlglueStruct;

    %Send the data to the glue that RL_episode should be executed
    p__rlglueStruct.network.clearSendBuffer();
    p__rlglueStruct.network.putInt(org.rlcommunity.rlglue.codec.network.Network.kRLEpisode);
    p__rlglueStruct.network.putInt(org.rlcommunity.rlglue.codec.network.Network.kIntSize);
    p__rlglueStruct.network.putInt(numSteps);
    p__rlglueStruct.network.flipSendBuffer();
    p__rlglueStruct.network.send();
    
    experimentDidSomething=false;
    
    %We will alternate calling the environmen, agent, and experimentuntil
    %there is data ready for the experiment.  
    global p__rlglueSettings;

    while ~experimentDidSomething
        if(p__rlglueSettings.hasAgent)
            [agentShouldQuit,agentDidSomething]=runAgentLoop();
        end

        if(p__rlglueSettings.hasEnvironment)
            [environmentShouldQuit,envDidSomething]=runEnvironmentLoop();
        end
        experimentDidSomething=doStandardRecv(org.rlcommunity.rlglue.codec.network.Network.kRLEpisode);
    end

    exitStatus=p__rlglueStruct.network.getInt();
end