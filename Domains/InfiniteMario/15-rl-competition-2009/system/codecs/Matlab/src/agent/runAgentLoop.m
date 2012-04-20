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
%  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Matlab/src/agent/runAgentLoop.m $
%
%It gets the agent from a struct that was created when the
%agent was connected.
%This is a non-blocking method.
% - didSomething will return true if it got some data, false otherwise.
% - shouldQuit will return true if it received the rlTerm signal
function [shouldQuit,didSomething]=runAgentLoop()
    global p__rlglueAgentStruct;
%This is all just copied in from ClientAgent in the Java codec    
    
    shouldQuit=false;
    didSomething=false;
    network=p__rlglueAgentStruct.network;
    theAgent=p__rlglueAgentStruct.theAgent;
    network.clearRecvBuffer();
    actualReceiveSize=network.recvNonBlock(8);
    
    if(actualReceiveSize>0)
        didSomething=true;
    else
        return;
    end
    
    recvSize = actualReceiveSize - 8; %// We may have received the header and part of the payload
                                    %// We need to keep track of how much
                                    %of the payload was recv'd
                                    
                                    
                                    
    agentState = network.getInt(0);
    dataSize = network.getInt(org.rlcommunity.rlglue.codec.network.Network.kIntSize);

    remaining = dataSize - recvSize;
    if remaining < 0
        fprintf(1,'Remaining was less than 0!\n');
    end

    amountReceived = network.recv(remaining);			

    network.flipRecvBuffer();

    %// We have already received the header, now we need to discard it.
    network.getInt();
    network.getInt();

    switch(agentState)
        
    case {org.rlcommunity.rlglue.codec.network.Network.kAgentInit}
		taskSpec = network.getString();
		theAgent.agent_init(taskSpec);

		network.clearSendBuffer();
		network.putInt(org.rlcommunity.rlglue.codec.network.Network.kAgentInit);
		network.putInt(0); % No data following this header

    case {org.rlcommunity.rlglue.codec.network.Network.kAgentStart}
		observation = network.getObservation();
		action = theAgent.agent_start(observation);
		size = org.rlcommunity.rlglue.codec.network.Network.sizeOf(action); 
		network.clearSendBuffer();
		network.putInt(org.rlcommunity.rlglue.codec.network.Network.kAgentStart);
		network.putInt(size);
		network.putAction(action);

    case {org.rlcommunity.rlglue.codec.network.Network.kAgentStep}
		reward = network.getDouble();
		observation = network.getObservation();
		action = theAgent.agent_step(reward, observation);

		size = org.rlcommunity.rlglue.codec.network.Network.sizeOf(action); 
		network.clearSendBuffer();
		network.putInt(org.rlcommunity.rlglue.codec.network.Network.kAgentStep);
		network.putInt(size);
		network.putAction(action);

    case {org.rlcommunity.rlglue.codec.network.Network.kAgentEnd}
		reward = network.getDouble();
		theAgent.agent_end(reward);

		network.clearSendBuffer();
		network.putInt(org.rlcommunity.rlglue.codec.network.Network.kAgentEnd);
		network.putInt(0); %No data in this packet

    case {org.rlcommunity.rlglue.codec.network.Network.kAgentCleanup}
		theAgent.agent_cleanup();
		network.clearSendBuffer();
		network.putInt(org.rlcommunity.rlglue.codec.network.Network.kAgentCleanup);
		network.putInt(0); % No data in this packet


    case {org.rlcommunity.rlglue.codec.network.Network.kAgentMessage}
		message = network.getString();
		reply = theAgent.agent_message(message);
		
		network.clearSendBuffer();
		network.putInt(org.rlcommunity.rlglue.codec.network.Network.kAgentMessage);
		network.putInt(org.rlcommunity.rlglue.codec.network.Network.sizeOf(reply));
		network.putString(reply);

    case {org.rlcommunity.rlglue.codec.network.Network.kRLTerm}
        disconnectAgent();
        shouldQuit=true;
        return;
   otherwise
        errormessage=sprintf('Unknown state in runAgentLoop %d\n',agentState);
		error(errormessage,'AgentUnknownState');
    end
    
    network.flipSendBuffer();
    network.send();
end