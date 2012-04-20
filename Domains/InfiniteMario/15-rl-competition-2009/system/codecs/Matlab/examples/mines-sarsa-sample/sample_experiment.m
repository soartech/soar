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
%  $HeadURL: http://rl-library.googlecode.com/svn/trunk/projects/packages/examples/mines-sarsa-matlab/sample_experiment.m $
%
function sample_experiment()
%  * Experiment program that does some of the things that might be important when
%  * running an experiment.  It runs an agent on the environment and periodically
%  * asks the agent to "freeze learning": to stop updating its policy for a number
%  * of episodes in order to get an estimate of the quality of what has been learned
%  * so far.
%  *
%  * The experiment estimates statistics such as the mean and standard deviation of
%  * the return gathered by the policy.  Instead of writing them to a file like the C/Java/Python
%  * sample experiments, this matlab experiment will plot them.
%  *
%  * This experiment also shows off some other features that can be achieved easily
%  * through the RL-Glue env/agent messaging system by freezing learning (described
%  * above), having the environment start in specific starting states, and saving
%  * and loading the agent's value function to/from a binary data file.
%  * @author Brian Tanner

%To use this experiment with the rest of the project together
%use the runAllTogether Function.

%TO USE THIS EXPERIMENT ON ITS OWN
%order doesn't matter
%   -  Start the rl_glue executable socket server on your computer
%   -  Run the SampleSarsaAgent and SampleMinesEnvironment from any
%    codec (Python, Java, C, Lisp should all be fine)
%   -  Execute this sample_experiment() function
%   NOTE: Type CTRL-C to abort the connection.
 
	RL_init();
    fprintf(1,'Starting offline demo\n----------------------------\nWill alternate learning for 25 episodes, then freeze policy and evaluate for 10 episodes.\n\n');
	fprintf(1,'After Episode\tMean Return\tStandard Deviation\n-------------------------------------------------------------------------\n');
	offline_demo();
	
	
	fprintf(1,'\nNow we will save the agent''s learned value function to a file....\n');

	RL_agent_message('save_policy results.dat');

	fprintf(1,'\nCalling RL_cleanup and RL_init to clear the agent''s memory...\n');

	RL_cleanup();
	RL_init();


	fprintf(1,'Evaluating the agent''s default policy:\n\t\tMean Return\tStandardDeviation\n------------------------------------------------------\n');
    
	[theMean,theStdDev]=evaluate_agent();
	print_score(0,theMean,theStdDev);
	
	fprintf(1,'\nLoading up the value function we saved earlier.\n');
	RL_agent_message('load_policy results.dat');

	fprintf(1,'Evaluating the agent after loading the value function:\n\t\tMean Return\tStandardDeviation\n------------------------------------------------------\n');
	[theMean,theStdDev]=evaluate_agent();
	print_score(0,theMean,theStdDev);

    fprintf(1,'Telling the environment to use fixed start state of 2,3.\n');
    RL_env_message('set-start-state 2 3');
    RL_start();
    fprintf(1,'Telling the environment to print the current state to the screen\n');
    RL_env_message('print-state');

    fprintf(1,'Evaluating the agent a few times from a fixed start state of 2,3:\n\t\tMean Return\tStandardDeviation\n-------------------------------------------\n');
	[theMean,theStdDev]=evaluate_agent();
	print_score(0,theMean,theStdDev);

	fprintf(1,'Evaluating the agent again with the random start state:\n\t\tMean Return\tStandardDeviation\n-----------------------------------------------------\n');
    RL_env_message('set-random-start-state');
	[theMean,theStdDev]=evaluate_agent();
	print_score(0,theMean,theStdDev);

    
	RL_cleanup();
    disconnectGlue();
	fprintf(1,'\nProgram Complete.\n');

end


% /**
% * Tell the agent to stop learning, then execute n episodes with his current
% * policy.  Estimate the mean and variance of the return over these episodes.
% * @return
% */
function[theMean,theStdDev]= evaluate_agent()
    n=10;
    sum=0;
    sum_of_squares=0;
 
    RL_agent_message('freeze learning');
    for i=1:n
        % We use a cutoff here in case the policy is bad and will never end
        % an episode
        RL_episode(5000);
        this_return=RL_return();
        sum=sum+this_return;
        sum_of_squares=sum_of_squares+this_return*this_return;
    end
 
    theMean=sum/n;
    variance = (sum_of_squares - n*theMean*theMean)/(n - 1.0);
    theStdDev=sqrt(variance);

    RL_agent_message('unfreeze learning');
end
 

%
%	This function will freeze the agent's policy and test it after every 25 episodes.
%
function offline_demo()

    statistics=[];
    
	[theMean,theStdDev]=evaluate_agent();
	print_score(0,theMean,theStdDev);
    
    %This is probably a bad way to do this, but
        %First column is the episode number
        %Second column is the mean at that evaluation
        %Third is the std deviation at that evaluation
    %Statistics are estimated from not many samples.  Beware!
      
    statistics=[statistics; 0,theMean,theStdDev];

    
    for i=1:20
        for j=1:25
			RL_episode(0);
        end
    	[theMean,theStdDev]=evaluate_agent();
		print_score(i*25,theMean,theStdDev);
        statistics=[statistics; i*25,theMean,theStdDev];
    end
	
    errorbar(statistics(:,1),statistics(:,2),statistics(:,3))

end


function print_score(afterEpisodes, theMean, theStdDev)
    fprintf(1,'%d\t\t%.2f\t\t%.2f\n', afterEpisodes,theMean, theStdDev);
end



