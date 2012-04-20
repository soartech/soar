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
%  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Matlab/examples/skeleton/skeleton_experiment.m $
%
%To use this experiment with the rest of the project together
%use the runAllTogether Function.

%TO USE THIS EXPERIMENT ON ITS OWN
%order doesn't matter
%   -  Start the rl_glue executable socket server on your computer
%   -  Run the skeleton_agent and skeleton_environment from any
%    codec (Python, Java, C, Lisp should all be fine)
%   -  Execute this skeleton_experiment() function
%   NOTE: Type CTRL-C to abort the connection.

function skeleton_experiment()
    	taskSpec = RL_init();
        global whichEpisode;
        whichEpisode = 0;

        fprintf(1,'Experiment starting up!\n');
        taskSpec = RL_init();
        fprintf(1,'RL_init called, the environment sent task spec: %s\n',char(taskSpec));

        fprintf(1,'\n\n----------Sending some sample messages----------\n');

        %Talk to the agent and environment a bit...
        responseMessage = RL_agent_message('what is your name?');
        fprintf(1,'Agent responded to \''what is your name?\'' with: %s\n',char(responseMessage));

        responseMessage = RL_agent_message('If at first you don\''t succeed; call it version 1.0');
        fprintf(1,'Agent responded to \''If at first you don\''t succeed; call it version 1.0  \'' with: %s\n',char(responseMessage));

        responseMessage = RL_env_message('what is your name?');
        fprintf(1,'Environment responded to \''what is your name?\'' with: %s\n',char(responseMessage));
        responseMessage = RL_env_message('If at first you don\''t succeed; call it version 1.0');
        fprintf(1,'Environment responded to \''If at first you don\''t succeed; call it version 1.0  \'' with: %s\n',char(responseMessage));

        fprintf(1,'\n\n----------Running a few episodes----------\n');
        runEpisode(100);
        runEpisode(100);
        runEpisode(100);
        runEpisode(100);
        runEpisode(100);
        runEpisode(1);
        %  Remember that stepLimit of 0 means there is no limit at all!
        runEpisode(0);
        RL_cleanup();

        fprintf(1,'\n\n----------Stepping through an episode----------\n');
        % We could also start over and do another experiment 
        taskSpec = RL_init();

         % We could run one step at a time instead of one episode at a time 
         % Start the episode 
        startResponse = RL_start();

        firstObservation = startResponse.o.intArray(1);
        firstAction = startResponse.a.intArray(1);
        fprintf(1,'First observation and action were: %d and: %d\n',firstObservation,firstAction);

        % Run one step 
        stepResponse = RL_step();

        % Run until the episode ends
        while (stepResponse.terminal ~= 1) 
            stepResponse = RL_step();
            if (stepResponse.terminal ~= 1) 
                % Could optionally print state,action pairs 
                % printf('(%d,%d) ',stepResponse.o.intArray[0],stepResponse.a.intArray[0]);
            end
        end

        fprintf(1,'\n\n----------Summary----------\n');

        totalSteps = RL_num_steps();
        totalReward = RL_return();
        fprintf(1,'It ran for %d, total reward was: %f\n',totalSteps,totalReward);
        RL_cleanup();
        
        disconnectGlue();
    end

    %  Run One Episode of length maximum cutOff
function runEpisode(stepLimit) 
	global whichEpisode;
    terminal = RL_episode(stepLimit);

    totalSteps = RL_num_steps();
    totalReward = RL_return();

    fprintf(1,'Episode %d\t %d steps \t %f total reward\t natural end %d\n',whichEpisode,totalSteps,totalReward,terminal);

    whichEpisode=whichEpisode+1;
end

