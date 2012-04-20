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
*  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/C/tests/test_1_agent.c $
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
static action_t *action=0;
static char* responseMessage=0;
static int stepCount=0;



void agent_init(const char * task_spec){
}

const action_t *agent_start(const observation_t *o) {
	__RL_CHECK_STRUCT(o);
	stepCount=0;

	freeRLStructPointer(action);
	action=duplicateRLStructToPointer(o);
	__RL_CHECK_STRUCT(action)

	return action;
}

const action_t *agent_step(const double reward, const observation_t *o) {
	__RL_CHECK_STRUCT(o);
	stepCount++;

	freeRLStructPointer(action);
	action=duplicateRLStructToPointer(o);
	__RL_CHECK_STRUCT(action)
	return action;
}

void agent_end(const double reward) {
}

void agent_cleanup() {
	freeRLStructPointer(action);
	action=0;
	if(responseMessage!=0){
		free(responseMessage);
		responseMessage=0;
	}
}

const char* agent_message(const char* inMessage) {
	int timesToPrint=stepCount%3;
	int i;
	char tmpBuffer[1024];
	
	sprintf(tmpBuffer,"%s|",inMessage);
	for(i=0;i<timesToPrint;i++){
		sprintf(tmpBuffer,"%s%d.", tmpBuffer,stepCount);
	}
	sprintf(tmpBuffer,"%s|%s",tmpBuffer,inMessage);

	if(responseMessage!=0){
		free(responseMessage);
		responseMessage=0;
	}
	responseMessage=(char *)calloc(strlen(tmpBuffer)+1,sizeof(char));
	sprintf(responseMessage,"%s",tmpBuffer);
	return responseMessage;
}
