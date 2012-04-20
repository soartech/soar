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
%  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Matlab/examples/skeleton/runAllTogether.m $
%
%This is a simple function that automates the steps of running the skeleton
%agent, environment, and experiment all together.

%It just bundles the three into a single structure and then calls
%runRLGlueMultiExperiment

%It's worth noting that you can use any combination of the three, just
%don't set the ones you don't want in togetherStruct.
%runRLGlueMultiExperiment will take care of the rest, but you WILL have 
%to run the missing components in a different codec.
function runAllTogether()
    theAgent=skeleton_agent();
    theEnvironment=skeleton_environment();
    theExperimentFunc=@skeleton_experiment;
    
    togetherStruct.agent=theAgent;
    togetherStruct.environment=theEnvironment;
    togetherStruct.experiment=theExperimentFunc;
    
    runRLGlueMultiExperiment(togetherStruct);
    
end