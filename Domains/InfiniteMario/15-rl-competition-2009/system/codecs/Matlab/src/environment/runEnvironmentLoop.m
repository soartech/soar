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
%  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Matlab/src/environment/runEnvironmentLoop.m $
%
%It gets the environment from a struct that was created when the
%environment was connected.
%This is a non-blocking method.
% - didSomething will return true if it got some data, false otherwise.
% - shouldQuit will return true if it received the rlTerm signal
function [shouldQuit,didSomething]=runEnvironmentLoop()
    global p__rlglueEnvStruct;
    %This is all just copied in from ClientEnv in the Java codec    
    
    shouldQuit=false;
    didSomething=false;
    network=p__rlglueEnvStruct.network;
    env=p__rlglueEnvStruct.theEnviroment;
    network.clearRecvBuffer();
    actualReceiveSize=network.recvNonBlock(8);
    
    if(actualReceiveSize>0)
        didSomething=true;
    else
        return;
    end
    
    recvSize = actualReceiveSize - 8; %// We may have received the header and part of the payload
                                    %// We need to keep track of how much of the payload was recv'd

    envState = network.getInt(0);
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

    switch(envState)
        
    case {org.rlcommunity.rlglue.codec.network.Network.kEnvInit}
		taskSpec = env.env_init();

		network.clearSendBuffer();
		network.putInt(org.rlcommunity.rlglue.codec.network.Network.kEnvInit);
		network.putInt(org.rlcommunity.rlglue.codec.network.Network.sizeOf(taskSpec)); %// This is different than taskSpec.length(). It also includes
		network.putString(taskSpec);

    case {org.rlcommunity.rlglue.codec.network.Network.kEnvStart}
		obs = env.env_start();

		network.clearSendBuffer();
		network.putInt(org.rlcommunity.rlglue.codec.network.Network.kEnvStart);
		network.putInt(org.rlcommunity.rlglue.codec.network.Network.sizeOf(obs));
		network.putObservation(obs);

    case {org.rlcommunity.rlglue.codec.network.Network.kEnvStep}
		action = network.getAction();
		rewardObservation = env.env_step(action);	
		
		network.clearSendBuffer();
		network.putInt(org.rlcommunity.rlglue.codec.network.Network.kEnvStep);
		network.putInt(org.rlcommunity.rlglue.codec.network.Network.sizeOf(rewardObservation));

		network.putRewardObservation(rewardObservation);

    case {org.rlcommunity.rlglue.codec.network.Network.kEnvCleanup}
		network.clearSendBuffer();
		network.putInt(org.rlcommunity.rlglue.codec.network.Network.kEnvCleanup);
		network.putInt(0);

    case {org.rlcommunity.rlglue.codec.network.Network.kEnvMessage}
		message = network.getString();
		reply = env.env_message(message);

		network.clearSendBuffer();
		network.putInt(org.rlcommunity.rlglue.codec.network.Network.kEnvMessage);
		network.putInt(org.rlcommunity.rlglue.codec.network.Network.sizeOf(reply));
		network.putString(reply);

    case {org.rlcommunity.rlglue.codec.network.Network.kRLTerm}
        disconnectEnvironment();
        shouldQuit=true;
        return;
   otherwise
		errormessage=sprintf('Unknown state in runEnvironmentLoop %d\n',envState);
		error(errormessage,'EnvironmentStateError');
    end
    
    network.flipSendBuffer();
    network.send();
end