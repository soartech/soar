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
*  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/C/tests/test_message_agent.c $
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



void agent_init(const char * task_spec){
	clearRLStruct(emptyAction);
}

const action_t *agent_start(const observation_t *o) {
	return emptyAction;
}

const action_t *agent_step(const double reward, const observation_t *o) {
	return emptyAction;
}

void agent_end(const double reward) {
}

void agent_cleanup() {
	if(responseMessage!=0){
		free(responseMessage);
		responseMessage=0;
	}
	freeRLStructPointer(emptyAction);
	emptyAction=0;
}

const char* agent_message(const char* inMessage) {
	char tmpBuffer[1024];
	
	if(inMessage==0)
		return "null";
	if(strcmp(inMessage,"")==0)
		return "empty";
	if(strcmp(inMessage,"null")==0)
		return 0;
	if(strcmp(inMessage,"empty")==0)
		return "";
	
	sprintf(tmpBuffer,"%s", inMessage);

	if(responseMessage!=0){
		free(responseMessage);
		responseMessage=0;
	}
	responseMessage=(char *)calloc(strlen(tmpBuffer)+1,sizeof(char));
	sprintf(responseMessage,"%s",tmpBuffer);
	return responseMessage;
}
