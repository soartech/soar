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
%  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Matlab/src/glue/ensureEnvExecutesIfNecessary.m $
%
%If we are running a mult-experiment, this function can be trusted to 
%check if an environment is involved, and block until the environment acts.
function ensureEnvExecutesIfNecessary()
global p__rlglueSettings;

    if(p__rlglueSettings.hasEnvironment)
        envDidSomething=false;
        while ~envDidSomething
              [environmentShouldQuit,envDidSomething]=runEnvironmentLoop();
        end
    end
end