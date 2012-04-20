%  Copyright 2008 Brian Tanner
%  http://rl-glue-ext.googlecode.com/
%  brian@tannerpages.com
%  http://research.tannerpages.com
%  
%   Licensed under the Apache License, Version 2.0 (the "License');
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
%  $HeadURL: http://rl-library.googlecode.com/svn/trunk/projects/packages/examples/mines-sarsa-matlab/sample_mines_environment.m $
%

%To use this Environment with the rest of the example together
%use the runAllTogether fnction.

% TO USE THIS Environment on its own[order doesn't matter]
%   -  Start the rl_glue executable socket server on your computer
%   -  Run the SampleSarsaAgent and SampleExperiment from a
%   different codec (Python, Java, C, Lisp should all be fine)
%   -  Load this environment like:
%       >> theEnvironment=sample_mines_environment()
%       >> runEnvironment(theEnvironment);
%   NOTE: Type CTRL-C to abort the connection.


function theEnvironment=sample_mines_environment()
%Assign members of the returning struct to be function pointers
	theEnvironment.env_init=@mines_init;
	theEnvironment.env_start=@mines_start;
	theEnvironment.env_step=@mines_step;
	theEnvironment.env_cleanup=@mines_cleanup;
	theEnvironment.env_message=@mines_message;
end

 % This code is adapted from the Mines.cpp code written by Adam White
 % for earlier versions of RL-Glue.
 %
 %	This is a very simple discrete-state, episodic grid world that has
 %	exploding mines in it.  If the agent steps on a mine, the episode
 %	ends with a large negative reward.
 %
 %	The reward per step is -1, with +10 for exiting the game successfully
 %	and -100 for stepping on a mine.
 %
 %  We're going to keep row and col indexes starting at 1 instead of 0.
 %  We'll be sure to subtract one before creating the observations.
 
 % We use a struct called mines_struct as a general holder for all of the
 % state information that the environment keeps around.  It is global, and
 % each method that needs a handle to it can do: global mines_struct
 
 
%This is what will be called for env_init
function taskSpecString=mines_init()
	global mines_struct;
    
    %mines_struct act like 'this' would from Java.  It's where we store
    %what would be member variables if this was an object.
    mines_struct.fixedStartState=false;
    mines_struct.startRow=1;
    mines_struct.startCol=1;
    
    mines_struct.WORLD_FREE = 0;
    mines_struct.WORLD_OBSTACLE = 1;
    mines_struct.WORLD_MINE = 2;
    mines_struct.WORLD_GOAL = 3;

    

    theWorld.map= [  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1;
                        1, 0, 0, 0, 0, 0, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 1;
                        1, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1;
                        1, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 0, 0, 0, 0, 1, 1;
                        1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 1;
                        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1];
                    
    numRows=size(theWorld.map,1);
    numCols=size(theWorld.map,2);
                    
    theWorld.agentRow=1;
    theWorld.agentCol=1;

    mines_struct.theWorld=theWorld;
    
    theTaskSpecObject = org.rlcommunity.rlglue.codec.taskspec.TaskSpecVRLGLUE3();
    theTaskSpecObject.setEpisodic();
    theTaskSpecObject.setDiscountFactor(1.0);

    observationRange=org.rlcommunity.rlglue.codec.taskspec.ranges.IntRange(0, numRows*numCols - 1);
    %Specify that there will be an integer observation [0,108] for the state
    theTaskSpecObject.addDiscreteObservation(observationRange);

    actionRange=org.rlcommunity.rlglue.codec.taskspec.ranges.IntRange(0,3);
    %Specify that there will be an integer action [0,4]
    theTaskSpecObject.addDiscreteAction(actionRange);


    %Specify the reward range [-100,10]
    rewardRange=org.rlcommunity.rlglue.codec.taskspec.ranges.DoubleRange(-100,10);

    theTaskSpecObject.setRewardRange(rewardRange);

    theTaskSpecObject.setExtra('SampleMinesEnvironment(Matlab) by Brian Tanner.');

    taskSpecString = theTaskSpecObject.toTaskSpec();
end

%This is what will be called for env_start
function theObservation=mines_start()
	global mines_struct;
    if mines_struct.fixedStartState==true
        mines_struct.theWorld.agentRow=mines_struct.startRow;
        mines_struct.theWorld.agentCol=mines_struct.startCol;

        stateIsValid=set_agent_state(mines_struct.startRow,mines_struct.startCol);

        if ~stateIsValid
            set_random_state();
        end
    else
        set_random_state();
    end

    theObservation = org.rlcommunity.rlglue.codec.types.Observation(1, 0, 0);
    theObservation.setInt(0, getState());
end



%This is what will be called for env_step
function rewardObservation=mines_step(thisAction)
    global mines_struct;

%Make sure the action is valid
    assert (thisAction.getNumInts() == 1,'Expecting a 1-dimensional integer action.');
    assert (thisAction.getInt(0) >= 0,'Action too small, should be in [0,4].');
    assert (thisAction.getInt(0) < 4,'Action too large, should be in [0,4].');
        
	updatePosition(thisAction.getInt(0));
    
    
	newStateInt = getState();
	newReward=getReward();
    isTerminalBoolean=check_terminal(mines_struct.theWorld.agentRow,mines_struct.theWorld.agentCol);

    if isTerminalBoolean
        isTerminalInt=1;
    else
        isTerminalInt=0;
    end

	theObservation = org.rlcommunity.rlglue.codec.types.Observation();
	theObservation.intArray=[newStateInt];

	rewardObservation=org.rlcommunity.rlglue.codec.types.Reward_observation_terminal(newReward,theObservation,isTerminalInt);
end


%This is what will be called for env_message
function returnMessage=mines_message(theMessageJavaObject)
    global mines_struct;
%Java strings are objects, and we want a Matlab string
    inMessage=char(theMessageJavaObject);
    
        
    %Message Description
    %'set-random-start-state'
    %Action: Set flag to do random starting states (the default)
    if strcmp(inMessage,'set-random-start-state')
        mines_struct.fixedStartState=false;
        returnMessage='Message understood.  Using random start state.';
        return;
    end
    
    
    %Message Description
    %'set-start-state X Y'
    %Action: Set flag to do fixed starting states, (row=X, col=Y)
    %This will be 0-indexed, so we should add 1 to it
	if strncmp(inMessage,'set-start-state',15)
        [firstPart,Remainder]=strtok(inMessage);
        [rowString,Remainder]=strtok(Remainder);
        colString=strtok(Remainder);

        mines_struct.startRow=str2double(rowString)+1;
        mines_struct.startCol=str2double(colString)+1;
        mines_struct.fixedStartState=true;
        returnMessage='Message understood.  Using fixed start state.';
        return;
    end
    
    %Message Description
    %'print-state'
    %Action: Print the map and the current agent location
    if strcmp(inMessage,'print-state')
        printState();
        returnMessage='Message understood.  Printed the state.';
        return;
    end

    returnMessage='SamplesMinesEnvironment(Matlab) does not respond to that message.';

end

function mines_cleanup()
	global mines_struct;
	mines_struct=rmfield(mines_struct,'fixedStartState');
	mines_struct=rmfield(mines_struct,'startRow');
	mines_struct=rmfield(mines_struct,'startCol');
	mines_struct=rmfield(mines_struct,'WORLD_FREE');
	mines_struct=rmfield(mines_struct,'WORLD_OBSTACLE');
	mines_struct=rmfield(mines_struct,'WORLD_MINE');
	mines_struct=rmfield(mines_struct,'WORLD_GOAL');
    
    mines_struct.theWorld=rmfield(mines_struct.theWorld,'map');
    mines_struct.theWorld=rmfield(mines_struct.theWorld,'agentRow');
    mines_struct.theWorld=rmfield(mines_struct.theWorld,'agentCol');
	mines_struct=rmfield(mines_struct,'theWorld');

    clear mines_struct;

end


%
%
%Utility functions below
%
%
function isTerminal=check_terminal(row,col)
global mines_struct;

	if mines_struct.theWorld.map(row,col) == mines_struct.WORLD_GOAL || mines_struct.theWorld.map(row,col) == mines_struct.WORLD_MINE
    	isTerminal=true;
    else
        isTerminal=false;
    end
end

%Checks if a row,col is valid (not a wall or out of bounds)
function isValid=check_valid(row,col)
global mines_struct;

    numRows=size(mines_struct.theWorld.map,1);
    numCols=size(mines_struct.theWorld.map,2);

    isValid=false;

    if row <= numRows && row >= 1 && col <= numCols && col >= 1
        if mines_struct.theWorld.map(row,col) ~= mines_struct.WORLD_OBSTACLE
            isValid=true;
        end
    end
end


%Sets state and returns true if valid, false if invalid or terminal 
function isValid = set_agent_state(row, col)
global mines_struct;

    mines_struct.theWorld.agentRow=row;
    mines_struct.theWorld.agentCol=col;
	
	isValid = check_valid(row,col) && ~check_terminal(row,col);
end

%Put the agent in a random, valid, nonterminal state
function set_random_state()
global mines_struct;
    
  
    numRows=size(mines_struct.theWorld.map,1);
    numCols=size(mines_struct.theWorld.map,2);

    startRow=ceil(rand()*numRows);
    startCol=ceil(rand()*numCols);

 	while ~set_agent_state(startRow,startCol)
        startRow=ceil(rand()*numRows);
        startCol=ceil(rand()*numCols);
    end
    
    mines_struct.theWorld.agentRow=startRow;
    mines_struct.theWorld.agentCol=startCol;

end

%Returns a number in [0,rows*cols)
function theFlatState=getState()
global mines_struct;
    
    agentRow=mines_struct.theWorld.agentRow-1;
    agentCol=mines_struct.theWorld.agentCol-1;
    numRows=size(mines_struct.theWorld.map,1);
    
    theFlatState=agentCol * numRows + agentRow;

end

%Updates the agent's position based on the action provided.
%When the move would result in hitting an obstacles, the agent doesn't move 
function updatePosition(theIntAction)
global mines_struct;
    
    newRow=mines_struct.theWorld.agentRow;
    newCol=mines_struct.theWorld.agentCol;

	
	if theIntAction == 0%move down
		newCol = newCol - 1;
	end
	if theIntAction == 1 %move up
		newCol = newCol + 1;
	end
	if theIntAction == 2%move left
		newRow = newRow - 1;
	end
	if theIntAction == 3%move right
		newRow = newRow + 1;
    end
    
    
	%Check if new position is out of bounds or inside an obstacle 
	if check_valid(newRow,newCol)
   		mines_struct.theWorld.agentRow = newRow;
   		mines_struct.theWorld.agentCol = newCol;
	end
end

%Calculate the reward for the current state
function theReward=getReward()
global mines_struct;
    agentRow=mines_struct.theWorld.agentRow;
    agentCol=mines_struct.theWorld.agentCol;

    if mines_struct.theWorld.map(agentRow,agentCol) == mines_struct.WORLD_GOAL
        theReward=10.0;
        return;
    end
    if mines_struct.theWorld.map(agentRow,agentCol) == mines_struct.WORLD_MINE
        theReward=-100.0;
        return;
    end
        
    theReward=-1.0;
end


%These are 1-indexed, so decrement them when printing to make them 0-index
function printState()
global mines_struct;
    agentRow=mines_struct.theWorld.agentRow;
    agentCol=mines_struct.theWorld.agentCol;

    fprintf(1,'Agent is at: %d,%d\n',agentRow-1,agentCol-1);
	fprintf(1,'  Columns:0-10                10-17\n');
	fprintf(1,'Col      ');
    
    for col=1:18
		fprintf(1,'%d ',mod(col-1,10));
    end

    for row=1:6
        fprintf(1,'\nRow: %d   ',row-1);

        for col=1:18
            if(agentRow==row && agentCol==col)
                fprintf(1,'A ');
            else
                if(mines_struct.theWorld.map(row,col)==mines_struct.WORLD_GOAL)
                    fprintf(1,'G ');
                end
                if(mines_struct.theWorld.map(row,col)==mines_struct.WORLD_MINE)
                    fprintf(1,'M ');
                end
                if(mines_struct.theWorld.map(row,col)==mines_struct.WORLD_OBSTACLE)
                    fprintf(1,'* ');
                end
                if(mines_struct.theWorld.map(row,col)==mines_struct.WORLD_FREE)
                    fprintf(1,'  ');
                end
            end
        end
    end
	fprintf(1,'\n');
end


