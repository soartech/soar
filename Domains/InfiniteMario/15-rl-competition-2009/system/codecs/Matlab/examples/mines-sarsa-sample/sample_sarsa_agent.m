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
%  $HeadURL: http://rl-library.googlecode.com/svn/trunk/projects/packages/examples/mines-sarsa-matlab/sample_sarsa_agent.m $

%To use this agent with the rest of the example together:
%>use the runAllTogether function.

% TO USE THIS Agent on its own[order doesn't matter]
%   -  Start the rl_glue executable socket server on your computer
%   -  Run the SampleMinesEnvironment and SampleExperiment from a
%   different codec (Python, Java, C, Lisp should all be fine)
%   -  Load this agent like:
%       >> theAgent=sample_sarsa_agent()
%       >> runAgent(theAgent);
%   NOTE: Type CTRL-C to abort the connection.
%
function theAgent=sample_sarsa_agent()
    theAgent.agent_init=@sarsa_agent_init;
    theAgent.agent_start=@sarsa_agent_start;
    theAgent.agent_step=@sarsa_agent_step;
    theAgent.agent_end=@sarsa_agent_end;
    theAgent.agent_cleanup=@sarsa_agent_cleanup;
    theAgent.agent_message=@sarsa_agent_message;
end

% This is a very simple Sarsa agent for discrete-action, discrete-state
% environments.  It uses epsilon-greedy exploration.
% 
% We've made a decision to store the previous action and observation in
% their raw form, as structures.  This code could be simplified and you
% could store them just as ints.
% @author Brian Tanner
function sarsa_agent_init(taskSpecJavaString)
	
	global sarsa_vars;
    sarsa_vars.policyFrozen=false;
    sarsa_vars.exploringFrozen=false;
    sarsa_vars.epsilon=.1;
    sarsa_vars.stepsize=.1;
    
    theTaskSpec = org.rlcommunity.rlglue.codec.taskspec.TaskSpec(taskSpecJavaString);
    % Lots of assertions to make sure that we can handle this problem.
    assert (theTaskSpec.getNumDiscreteObsDims() == 1);
    assert (theTaskSpec.getNumContinuousObsDims() == 0);
    assert (~theTaskSpec.getDiscreteObservationRange(0).hasSpecialMinStatus());
    assert (~theTaskSpec.getDiscreteObservationRange(0).hasSpecialMaxStatus());
    sarsa_vars.numStates = theTaskSpec.getDiscreteObservationRange(0).getMax() + 1;

    assert (theTaskSpec.getNumDiscreteActionDims() == 1);
    assert (theTaskSpec.getNumContinuousActionDims() == 0);
    assert (~theTaskSpec.getDiscreteActionRange(0).hasSpecialMinStatus());
    assert (~theTaskSpec.getDiscreteActionRange(0).hasSpecialMaxStatus());
    sarsa_vars.numActions = theTaskSpec.getDiscreteActionRange(0).getMax() + 1;

    sarsa_vars.gamma=theTaskSpec.getDiscountFactor();

    sarsa_vars.valueFunction = zeros(sarsa_vars.numActions,sarsa_vars.numStates);
end    

%
% Choose an action e-greedily from the value function and store the action
% and observation.
function theAction=sarsa_agent_start(theObservation)
global sarsa_vars;

    newActionInt = egreedy(theObservation.getInt(0));

    theAction = org.rlcommunity.rlglue.codec.types.Action(1, 0, 0);
    theAction.setInt(0,newActionInt);
    
    sarsa_vars.lastAction=theAction.duplicate();
    sarsa_vars.lastObservation = theObservation.duplicate();
end

% Choose an action e-greedily from the value function and store the action
% and observation.  Update the valueFunction entry for the last
% state,action pair.
function theAction=sarsa_agent_step(theReward, theObservation)
        global sarsa_vars;
        
        newStateInt = theObservation.getInt(0);
        lastStateInt = sarsa_vars.lastObservation.getInt(0);
        lastActionInt = sarsa_vars.lastAction.getInt(0);

        newActionInt = egreedy(newStateInt);

        Q_sa = sarsa_vars.valueFunction(lastActionInt+1,lastStateInt+1);
        Q_sprime_aprime = sarsa_vars.valueFunction(newActionInt+1,newStateInt+1);

        new_Q_sa = Q_sa + sarsa_vars.stepsize * (theReward + sarsa_vars.gamma * Q_sprime_aprime - Q_sa);

        %Only update the value function if the policy is not frozen
        if ~sarsa_vars.policyFrozen
            sarsa_vars.valueFunction(lastActionInt+1,lastStateInt+1) = new_Q_sa;
        end
        

        %Creating the action a different way to showcase variety */
        theAction = org.rlcommunity.rlglue.codec.types.Action();
        theAction.intArray=[newActionInt];

        sarsa_vars.lastAction=theAction.duplicate();
        sarsa_vars.lastObservation = theObservation.duplicate();
end

% The episode is over, learn from the last reward that was received.
function sarsa_agent_end(theReward)
global sarsa_vars;
        
    lastStateInt = sarsa_vars.lastObservation.getInt(0);
    lastActionInt = sarsa_vars.lastAction.getInt(0);

    Q_sa = sarsa_vars.valueFunction(lastActionInt+1,lastStateInt+1);

    new_Q_sa = Q_sa + sarsa_vars.stepsize * (theReward - Q_sa);
    %Only update the value function if the policy is not frozen
    if ~sarsa_vars.policyFrozen
        sarsa_vars.valueFunction(lastActionInt+1,lastStateInt+1) = new_Q_sa;
   end
end

function returnMessage=sarsa_agent_message(theMessageJavaObject)
global sarsa_vars;
%Java strings are objects, and we want a Matlab string
    inMessage=char(theMessageJavaObject);

   	if strcmp(inMessage,'freeze learning')
		sarsa_vars.policyFrozen=true;
		returnMessage='message understood, policy frozen';
        return;
    end

    if strcmp(inMessage,'unfreeze learning')
		sarsa_vars.policyFrozen=false;
		returnMessage='message understood, policy unfrozen';
        return;
	end
	if strcmp(inMessage,'freeze exploring')
		sarsa_vars.exploringFrozen=true;
		returnMessage='message understood, exploring frozen';
        return;
	end
	if strcmp(inMessage,'unfreeze exploring')
		sarsa_vars.exploringFrozen=false;
		returnMessage='message understood, exploring unfrozen';
        return;
    end

    if strncmp(inMessage,'save_policy',11)
        [commandString,remainder]=strtok(inMessage);
        fileName=strtok(remainder);
        
		fprintf(1,'Saving value function...');
        saveValueFunction(fileName);
		fprintf(1,'Saved.\n');
		returnMessage='message understood, saving policy';
        return;
	end
	if strncmp(inMessage,'load_policy',11)
        [commandString,remainder]=strtok(inMessage);
        fileName=strtok(remainder);
        loadValueFunction(fileName);
		fprintf(1,'Loaded.\n');
		returnMessage='message understood, loading policy';
        return;
    end

	
	returnMessage='SampleSarsaAgent(Matlab) does not understand your message.';
    
end

function sarsa_agent_cleanup()
global sarsa_vars;

    sarsa_vars=rmfield(sarsa_vars,'valueFunction');
    sarsa_vars=rmfield(sarsa_vars,'gamma');
    sarsa_vars=rmfield(sarsa_vars,'epsilon');
    sarsa_vars=rmfield(sarsa_vars,'stepsize');

    clear mines_struct;

end

% Selects a random action with probability 1-sarsa_epsilon,
% and the action with the highest value otherwise.  This is a
% quick'n'dirty implementation, it does not do tie-breaking.
function theActionInt=egreedy(theStateInt)
global sarsa_vars;

    theStateInt=theStateInt+1;


    if ~sarsa_vars.exploringFrozen
        if rand() <= sarsa_vars.epsilon
            theActionInt=floor(rand()*sarsa_vars.numActions);
            return;
        end
    end

    %otherwise choose the greedy action
    [maxValue,theActionInt]=max(sarsa_vars.valueFunction(:,theStateInt));
    theActionInt=theActionInt-1;
end

function loadValueFunction(fileName)
global sarsa_vars;
    loadedStruct=load(fileName,'-mat');
    
    sarsa_vars.valueFunction=loadedStruct.valueFunction;
end

function saveValueFunction(fileName)
global sarsa_vars;
%I know this looks weird, trying to make:
% >>save fileName -mat -struct 'sarsa_vars' 'valueFunction'

    theSaveCommand=sprintf('save %s -mat -struct ''sarsa_vars'' ''valueFunction''',fileName);
    eval(theSaveCommand);
end
