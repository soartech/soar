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
%  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Matlab/src/glue/ensureAgentExecutesIfNecessary.m $
%
%If we are running a mult-experiment, this function can be trusted to 
%check if an agent is involved, and block until the agent acts.
function ensureAgentExecutesIfNecessary()
global p__rlglueSettings;

    if(p__rlglueSettings.hasAgent)
        agentDidSomething=false;
        while ~agentDidSomething
              [agentShouldQuit,agentDidSomething]=runAgentLoop();
        end
    end
end