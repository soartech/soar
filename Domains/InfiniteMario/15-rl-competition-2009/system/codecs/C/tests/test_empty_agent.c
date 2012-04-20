/* 
* Copyright (C) 2007, Brian Tanner

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

* 
*  $Revision: 367 $
*  $Date: 2008-11-04 01:29:26 -0500 (Tue, 04 Nov 2008) $
*  $Author: brian@tannerpages.com $
*  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/C/tests/test_empty_agent.c $
* 
*/
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif


/**
This agent is used for testing.  It will mostly just return whatever it receives.

This agent doesn't implement all the methods.. isn't that bad?
**/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <rlglue/Agent_common.h>
#include <rlglue/utils/C/RLStruct_util.h>

#include "useful_functions.h"


static char* responseMessage=0;
static action_t *emptyAction=0;
static action_t *nonEmptyAction=0;
static int whichEpisode=0;

void agent_init(const char * task_spec){
	emptyAction=allocateRLStructPointer(0,0,0);
	nonEmptyAction=allocateRLStructPointer(0,0,0);
	
	set_k_ints_in_abstract_type(nonEmptyAction,7);
	set_k_doubles_in_abstract_type(nonEmptyAction,3);
	set_k_chars_in_abstract_type(nonEmptyAction,1);

	whichEpisode=0;
}

const action_t *agent_start(const observation_t *o) {
	whichEpisode++;
	
	if(whichEpisode%2==0)
		return emptyAction;
	
	return nonEmptyAction;
}

const action_t *agent_step(const double reward, const observation_t *o) {
	if(whichEpisode%2==0)
		return emptyAction;
	
	return nonEmptyAction;
}

void agent_end(const double reward) {
}

void agent_cleanup() {
	freeRLStructPointer(emptyAction);
	freeRLStructPointer(nonEmptyAction);
	emptyAction=0;
	nonEmptyAction=0;
	if(responseMessage!=0){
		free(responseMessage);
		responseMessage=0;
	}
}

const char* agent_message(const char* inMessage) {
	return "";
}
