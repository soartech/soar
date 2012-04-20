/* 
* Copyright (C) 2007, Brian Tanner
* 
* This program is free software; you can redistribute it and/or
* modify it under the terms of the GNU General Public License
* as published by the Free Software Foundation; either version 2
* of the License, or (at your option) any later version.
* 
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
* 
* You should have received a copy of the GNU General Public License
* along with this program; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <assert.h>
#include "ZeroAgent.h"
#include <rlglue/utils/C/RLStruct_util.h> /* helpful functions for structs */
#include <rlglue/utils/C/TaskSpec_Parser.h> /* task spec parser */

action_t* this_action=0;

void zeroify();

void agent_init(const char* task_spec)
{
	/*Struct to hold the parsed task spec*/
	taskspec_t *ts=(taskspec_t*)malloc(sizeof(taskspec_t));
	int decode_result = decode_taskspec( ts, task_spec );
	if(decode_result!=0){
		printf("Could not decode task spec, code: %d for task spec: %s\n",decode_result,task_spec);
		exit(1);
	}
	this_action=allocateRLStructPointer(getNumIntAct(ts),getNumDoubleAct(ts),0);
}

const action_t *agent_start(const observation_t *this_observation) {
	zeroify();
	return this_action;
}

const action_t *agent_step(double reward, const observation_t *this_observation) {
	zeroify();
	return this_action;
}

void agent_end(double reward) {
}

void agent_cleanup() {
}

void agent_freeze() {
}

const char* agent_message(const char* inMessage) {
	return "Zero agent does not respond to messages.";
}

void zeroify(){
	int i;
	for (i=0;i<this_action->numInts;i++) {
			this_action->intArray[i] = 0;
		}
	for (i=0;i<this_action->numDoubles;i++) {
			this_action->doubleArray[i] = 0.0;
		}
}
