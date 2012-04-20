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
%   $Revision: 1001 $
%   $Date: 2009-02-09 12:35:56 -0500 (Mon, 09 Feb 2009) $
%   $Author: brian@tannerpages.com $
%  $HeadURL: http://rl-library.googlecode.com/svn/trunk/projects/packages/examples/mines-sarsa-matlab/runAllTogether.m $
%
%This is a simple function that automates the steps of running the
%mines-sarsa-sample agent, environment, and experiment all together.

%It just bundles the three into a single structure and then calls
%runRLGlueMultiExperiment

%It's worth noting that you can use any combination of the three, just
%don't set the ones you don't want in togetherStruct.
%runRLGlueMultiExperiment will take care of the rest, but you WILL have 
%to run the missing components in a different codec.
function runAllTogether()
    theAgent=sample_sarsa_agent();
    theEnvironment=sample_mines_environment();
    theExperimentFunc=@sample_experiment;
    
    togetherStruct.agent=theAgent;
    togetherStruct.environment=theEnvironment;
    togetherStruct.experiment=theExperimentFunc;
    
    runRLGlueMultiExperiment(togetherStruct);
    
end