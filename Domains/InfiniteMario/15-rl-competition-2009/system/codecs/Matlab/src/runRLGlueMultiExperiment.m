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
%  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Matlab/src/runRLGlueMultiExperiment.m $
%
%
% This function will run any combination of agent/environment/experiment
% together in Matlab.  It expects a struct with at least one (but possibly
% all three) of the following fields set:
%multiStruct.agent <--an agent struct, just like runAgent expects
%multiStruct.environment<--environment struct, like runEnvironment expects
%multiStruct.experiment<-- a function pointer to an experiment function
%   For example, like multiStruct.experiment=@skeleton_experiment
function experimentReturned=runRLGlueMultiExperiment(multiStruct)
experimentReturned=[];
%Since this kills some globals we might want
checkForJavaCodec();

global p__rlglueSettings;


%These are harmless if not connected already.
disconnectAgent();
disconnectEnvironment();
disconnectGlue();

%Clear out old settings
if(isfield(p__rlglueSettings,'environment'))
   p__rlglueSettings=rmfield(p__rlglueSettings,'environment');
end

if(isfield(p__rlglueSettings,'agent'))
   p__rlglueSettings=rmfield(p__rlglueSettings,'agent');
end

if(isfield(p__rlglueSettings,'experiment'))
   p__rlglueSettings=rmfield(p__rlglueSettings,'experiment');
end


%Set new Settings
p__rlglueSettings.hasEnvironment=false;
p__rlglueSettings.hasExperiment=false;
p__rlglueSettings.hasAgent=false;

if(isfield(multiStruct,'environment'))
   p__rlglueSettings.environment=multiStruct.environment;
   p__rlglueSettings.hasEnvironment=true;
end

if(isfield(multiStruct,'agent'))
   p__rlglueSettings.agent=multiStruct.agent;
   p__rlglueSettings.hasAgent=true;
end

if(isfield(multiStruct,'experiment'))
   p__rlglueSettings.experiment=multiStruct.experiment;
   p__rlglueSettings.hasExperiment=true;
end
   

%Connect the Agent
if(p__rlglueSettings.hasAgent)
    connectAgent(p__rlglueSettings.agent);
end

%Connect the Environment
if(p__rlglueSettings.hasEnvironment)
    connectEnvironment(p__rlglueSettings.environment);
end

%If there is an experiment, run it, and the RL_* methods will be sure to
%step the agent and environment if they are set.  Otherwise, we will set
%the whole thing ourselves.
%Connect the Environment
if(p__rlglueSettings.hasExperiment)
    %This is a tricky hack so we can either support experiments that return
    %a value or don't.
    if nargout(p__rlglueSettings.experiment) >= 1
        experimentReturned=p__rlglueSettings.experiment();
    else
        p__rlglueSettings.experiment();
    end
    %These are harmless if not connected already.
    disconnectAgent();
    disconnectEnvironment();
    disconnectGlue();
    return;
end

%Step the agent and environment ourselves
environmentShouldQuit=~p__rlglueSettings.hasEnvironment;
agentShouldQuit=~p__rlglueSettings.hasAgent;

while(~agentShouldQuit || ~environmentShouldQuit)
   if(~environmentShouldQuit)
       [environmentShouldQuit,envDidSomething]=runEnvironmentLoop();
   end
   if(~agentShouldQuit)
       [agentShouldQuit,agentDidSomething]=runAgentLoop();
   end
    
end
    
%These are harmless if not connected already.
disconnectAgent();
disconnectEnvironment();
disconnectGlue();

end