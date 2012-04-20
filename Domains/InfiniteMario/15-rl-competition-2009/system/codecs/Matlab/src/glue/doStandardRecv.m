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
%  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Matlab/src/glue/doStandardRecv.m $
%
%This is a non-blocking method.  It will return true if it got some data,
%false otherwise.
function didSomething=doStandardRecv(state)
global p__rlglueStruct;
    didSomething=false;
    p__rlglueStruct.network.clearRecvBuffer();

    
    actualReceiveSize=p__rlglueStruct.network.recvNonBlock(8);
    
    if(actualReceiveSize>0)
        didSomething=true;
    else
        return;
    end
    
    recvSize = actualReceiveSize - 8; %// We may have received the header and part of the payload
                                    %// We need to keep track of how much
                                    %of the payload was recv'd

    glueState = p__rlglueStruct.network.getInt(0);
    dataSize = p__rlglueStruct.network.getInt(org.rlcommunity.rlglue.codec.network.Network.kIntSize);
    remaining = dataSize - recvSize;

    if remaining < 0
        remaining = 0;
    end

    remainingReceived=p__rlglueStruct.network.recv(remaining);

    p__rlglueStruct.network.flipRecvBuffer();	

    % Discard the header - we should have a more elegant method for doing this.
    p__rlglueStruct.network.getInt();
    p__rlglueStruct.network.getInt();

    if glueState ~= state
        errormessage=sprintf('Not synched with server. glueState = %d but should be %d\n',glueState,state);
		error(errormessage,'gluestaterror');
    end
end
        
       
