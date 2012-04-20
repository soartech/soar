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
%   $Revision: 798 $
%   $Date: 2009-02-26 21:11:35 -0500 (Thu, 26 Feb 2009) $
%   $Author: brian@tannerpages.com $
%  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Matlab/src/agent/disconnectAgent.m $
%
function disconnectAgent()
    global p__rlglueAgentStruct;
    global p__rlglueSettings;
    
    if isfield(p__rlglueAgentStruct,'network')
        p__rlglueAgentStruct.network.close();
		p__rlglueAgentStruct=rmfield(p__rlglueAgentStruct,'network');
    end
    
    if(isfield(p__rlglueSettings,'agent'))
        p__rlglueSettings=rmfield(p__rlglueSettings,'agent');
        p__rlglueSettings.hasAgent=false;
    end
end