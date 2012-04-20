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
%  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Matlab/src/glue/RL_agent_message.m $
%
function response=RL_agent_message(message)
		forceConnection();
        global p__rlglueStruct;

        %Send the data to the glue that RL_agent_message should be executed
        p__rlglueStruct.network.clearSendBuffer();
        p__rlglueStruct.network.putInt(org.rlcommunity.rlglue.codec.network.Network.kRLAgentMessage);
        p__rlglueStruct.network.putInt(org.rlcommunity.rlglue.codec.network.Network.sizeOf(message));
        p__rlglueStruct.network.putString(message);
        p__rlglueStruct.network.flipSendBuffer();
        p__rlglueStruct.network.send();

        %If there is an agent (if we're running more than one
        %component together), then make sure it executes (agent_message)
        ensureAgentExecutesIfNecessary();
        
        %Receive the response from rl_glue

        forceStandardRecv(org.rlcommunity.rlglue.codec.network.Network.kRLAgentMessage);
        response = p__rlglueStruct.network.getString();
end