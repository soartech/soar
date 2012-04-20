/* 
	Copyright (C) 2008, Brian Tanner

	Licensed under the Apache License, Version 2.0 (the "License");
	you may not use this file except in compliance with the License.
	You may obtain a copy of the License at

	    http://www.apache.org/licenses/LICENSE-2.0

	Unless required by applicable law or agreed to in writing, software
	distributed under the License is distributed on an "AS IS" BASIS,
	WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
	See the License for the specific language governing permissions and
	limitations under the License.

	This code is adapted from the Mines.cpp code written by Adam White
	for earlier versions of RL-Glue.
	
	*  $Revision: 996 $
	*  $Date: 2009-02-08 20:48:32 -0500 (Sun, 08 Feb 2009) $
	*  $Author: brian@tannerpages.com $
	*  $HeadURL: http://rl-library.googlecode.com/svn/trunk/projects/packages/examples/mines-sarsa-c/SampleMinesEnvironment.c $
	
*/

#include <string.h> /*strcmp*/
#include <stdio.h> /*printf*/
#include <stdlib.h>
#include <assert.h> /*assert*/

#include <rlglue/Environment_common.h>	  /* env_ function prototypes types */
#include <rlglue/utils/C/RLStruct_util.h> /* helpful functions for allocating
 											  structs and cleaning them up */

/* 	
	This is a very simple discrete-state, episodic grid world that has 
	exploding mines in it.  If the agent steps on a mine, the episode
	ends with a large negative reward.
	
	The reward per step is -1, with +10 for exiting the game successfully
	and -100 for stepping on a mine.
*/

#define WORLD_FREE 0
#define WORLD_OBSTACLE 1
#define WORLD_MINE 2 
#define WORLD_GOAL 3

/*
	world_description_t is a structure that holds all of the information
	about the world, including the placement of the mines and the position
	of the agent, etc.
*/
typedef struct 
{
  int numRows;		/* Number of rows in the world		*/
  int numCols;		/* Number of columns in the world	*/
  int agentRow;		/* Agent current row				*/
  int agentCol;		/* Agent current column				*/
} world_description_t;


/* HELPER FUNCTIONS.  PROTOTYPES HERE, CODE AT THE BOTTOM OF FILE */

/* Changes the (row,col) position of the agent into a scalar     */	
int calculate_flat_state(world_description_t aWorld);


/* Returns 1 of the current state is terminal (mine or goal), 0 otherwise */
int check_terminal(int row, int col);


/* Returns 1 of the current state is inside map and not blocked) */
int check_valid(const world_description_t* aWorld,int row, int col);

/*  Calculates the reward in the current state.
	-100 if on a mine
	+10 if at the exit
	-1 otherwise 
*/
double calculate_reward(world_description_t aWorld);

/* Calculates and sets the next state, given an action */
void updatePosition(world_description_t *aWorld, int theAction);


/* Prints out the map to the screen */
void print_state();


/*
	world_map is an array that describes the world.

	To read this: the world is a 6 by 18 grid in any position the number 
	corresponds to one of {START, GOAL, FREE, OBSTACLE, MINE}

	For example in env_init the start position is labelled by a 0
	so we can see the initial start position in this particular map is at
	position [12][1]
	
*/

int world_map[6][18] = 
{ 
	{ 1, 1, 1, 1, 1, 1 ,1 ,1, 1, 1 ,1 ,1 ,1, 1, 1, 1, 1, 1 }, 
	{ 1, 0, 0, 0, 0, 0, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 1 }, 
	{ 1, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
	{ 1, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 0, 0, 0, 0, 1, 1 },
	{ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 1 },
	{ 1, 1, 1, 1, 1, 1 ,1 ,1, 1, 1 ,1 ,1 ,1, 1, 1, 1, 1, 1 }
};


/* GLOBAL VARIABLES FOR RL-GLUE methods (global for convenience) */  
static world_description_t the_world;
static observation_t this_observation;
static reward_observation_terminal_t this_reward_observation;


/* Used if a message is sent to the environment to use fixed start states */
static int fixed_start_state=0;
static int start_row=1;
static int start_col=1;


/*****************************

	RL-Glue Methods 
	
*******************************/
const char* env_init(){    
	char *task_spec_string="VERSION RL-Glue-3.0 PROBLEMTYPE episodic "
							 "DISCOUNTFACTOR 1 OBSERVATIONS INTS (0 107) "
							 "ACTIONS INTS (0 3)  REWARDS (-100.0 10.0) "
							"EXTRA SampleMinesEnvironment(C/C++) by Brian Tanner.";

	  the_world.numRows = 6;
	  the_world.numCols = 18;


	/* Allocate the observation variable */
	allocateRLStruct(&this_observation,1,0,0);
	/* That is equivalent to:
		 this_observation.numInts     =  1;
		 this_observation.intArray    = (int*)calloc(1,sizeof(int));
		 this_observation.numDoubles  = 0;
		 this_observation.doubleArray = 0;
		 this_observation.numChars    = 0;
		 this_observation.charArray   = 0;
	*/
	/* Setup the reward_observation variable */
	this_reward_observation.observation=&this_observation;
	this_reward_observation.reward=0;
	this_reward_observation.terminal=0;

   return task_spec_string;
}

/* Sets state and returns 1 if valid, 0 if invalid or terminal */
int set_agent_state(int row, int col){
	the_world.agentRow=row;
	the_world.agentCol=col;
	
	return check_valid(&the_world,row,col) && !check_terminal(row,col);
}

void set_random_state(){
	int startRow=rand()%6;
	int startCol=rand()%18;

	while(!set_agent_state(startRow,startCol)){
		startRow=rand()%6;
		startCol=rand()%18;
	}
}
/*
	Standard RL-Glue method. Sets an initial state and returns
	the corresponding observation.
*/
const observation_t *env_start()
{ 
	if(fixed_start_state){
        int state_valid=set_agent_state(start_row,start_col);
        if(!state_valid){
            set_random_state();
        }
    }else{
        set_random_state();
    }
    
	this_observation.intArray[0]=calculate_flat_state(the_world);
  	return &this_observation;
}


const reward_observation_terminal_t *env_step(const action_t *this_action)
{
	/* Make sure the action is valid */
	assert(this_action->numInts==1);
	assert(this_action->intArray[0]>=0);
	assert(this_action->intArray[0]<4);

	updatePosition(&the_world,this_action->intArray[0]);
	this_reward_observation.observation->intArray[0] = calculate_flat_state(the_world);
	this_reward_observation.reward = calculate_reward(the_world);
	this_reward_observation.terminal = check_terminal(the_world.agentRow,the_world.agentCol);

	return &this_reward_observation;
}

void env_cleanup()
{
	clearRLStruct(&this_observation);
}

const char* env_message(const char* inMessage) {
	/*	Message Description
 	 * 'set-random-start-state'
	 * Action: Set flag to do random starting states (the default)
	 */
	if(strcmp(inMessage,"set-random-start-state")==0){
        fixed_start_state=0;
        return "Message understood.  Using random start state.";
    }
    
	/*	Message Description
 	 * 'set-start-state X Y'
	 * Action: Set flag to do fixed starting states (row=X, col=Y)
	 */
	if(strncmp(inMessage,"set-start-state",15)==0){
	 	{
			char *p;
			char *inMessageCopy=(char *)malloc((strlen(inMessage)+1)*sizeof(char));
		
			strcpy(inMessageCopy,inMessage);
		
			/* p will have set-start-state */
			p = strtok (inMessageCopy," ");
			/* p will have the row number string */
			p = strtok (0," ");
			start_row=atoi(p);
			/* p will have the col number string */
			p = strtok (0," ");
			start_col=atoi(p);
			free(inMessageCopy);
		
	        fixed_start_state=1;

	        return "Message understood.  Using fixed start state.";
    	}
	}

	/*	Message Description
 	 * 'print-state'
	 * Action: Print the map and the current agent location
	 */
	if(strcmp(inMessage,"print-state")==0){
		print_state();
		return "Message understood.  Printed the state.";
    }
	return "SamplesMinesEnvironment(C/C++) does not respond to that message.";
}


/*****************************

	Helper Methods 
	
*******************************/

int calculate_flat_state(world_description_t aWorld){
	return aWorld.agentCol * aWorld.numRows + aWorld.agentRow;
}

int check_terminal(int row, int col){
	if (world_map[row][col] == WORLD_GOAL || world_map[row][col] == WORLD_MINE){    
		return 1;
	}
	return 0;
}

int check_valid(const world_description_t* aWorld, int row, int col){
	int valid=0;
	if(row < aWorld->numRows && row >= 0 && col < aWorld->numCols && col >= 0){
		if(world_map[row][col] != WORLD_OBSTACLE){
			valid=1;
		}
	}
	return valid;
}

double calculate_reward(world_description_t aWorld){
	if(world_map[aWorld.agentRow][aWorld.agentCol] == WORLD_GOAL){
		return 10.0f;
	}

	if(world_map[aWorld.agentRow][aWorld.agentCol] == WORLD_MINE){
		return -100.0f;
	}
	
	return -1.0f;
}

void updatePosition(world_description_t *aWorld, int theAction){
	/* When the move would result in hitting an obstacles, the agent simply doesn't move */
	int newRow = aWorld->agentRow;
	int newCol = aWorld->agentCol;

	
	if (theAction == 0){/*move down*/
		newCol = aWorld->agentCol - 1;
	}
	if (theAction == 1){ /*move up*/
		newCol = aWorld->agentCol + 1;
	}
	if (theAction == 2){/*move left*/
		newRow = aWorld->agentRow - 1;
	}
	if (theAction == 3){/*move right*/
		newRow = aWorld->agentRow + 1;
	}


	/*Check if new position is out of bounds or inside an obstacle */
	if(check_valid(aWorld,newRow,newCol)){
   		aWorld->agentRow = newRow;
   		aWorld->agentCol = newCol;
	}
}

void print_state(){
	int row,col;
	printf("Agent is at: %d,%d\n",the_world.agentRow,the_world.agentCol);
	printf("Columns:0-10                10-17\n");
	printf("Col    ");
	for(col=0;col<18;col++){
		printf("%d ",col%10);
	}

	for(row=0;row<6;row++){
		printf("\nRow: %d ",row);
		
		for(col=0;col<18;col++){
			if(the_world.agentRow==row && the_world.agentCol==col)
				printf("A ");
			else{
				if(world_map[row][col]==WORLD_GOAL)
					printf("G ");
				if(world_map[row][col]==WORLD_MINE)
					printf("M ");
				if(world_map[row][col]==WORLD_OBSTACLE)
					printf("* ");
				if(world_map[row][col]==WORLD_FREE)
					printf("  ");
			}
		}
	}
	printf("\n");
}

