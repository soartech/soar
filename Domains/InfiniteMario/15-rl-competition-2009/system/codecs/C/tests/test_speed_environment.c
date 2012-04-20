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
*  $Revision: 365 $
*  $Date: 2008-11-04 01:19:46 -0500 (Tue, 04 Nov 2008) $
*  $Author: brian@tannerpages.com $
*  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/C/tests/test_speed_environment.c $
* 
*/
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif


#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <rlglue/Environment_common.h>
#include <rlglue/utils/C/RLStruct_util.h>
#include "useful_functions.h"


static observation_t *o=0;
static reward_observation_terminal_t ro={0};
static int stepCount=0;
static int episodeCount=0;

const char* env_init()
{    
	o=allocateRLStructPointer(0,0,0);
	return "sample task spec";
}

const observation_t *env_start()
{
	episodeCount++;
	stepCount=0;
	clearRLStruct(o);
	__RL_CHECK_STRUCT(o)
	return o;
}

const reward_observation_terminal_t *env_step(const action_t *a)
{
	int terminal=0;
	stepCount++;
	clearRLStruct(o);
	    
        /*Short episode with big observations*/
        if(episodeCount%2==0){
			__RL_CHECK_STRUCT(o)
            set_k_ints_in_abstract_type(o, 50000);
			__RL_CHECK_STRUCT(o)
            set_k_doubles_in_abstract_type(o, 50000);
			__RL_CHECK_STRUCT(o)

            if(stepCount==200)terminal=1;
        }
        /*Longer episode with smaller observations*/
        if(episodeCount%2==1){
			__RL_CHECK_STRUCT(o)
            set_k_ints_in_abstract_type(o, 5);
		__RL_CHECK_STRUCT(o)
            set_k_doubles_in_abstract_type(o, 5);
		__RL_CHECK_STRUCT(o)

            if(stepCount==5000)terminal=1;
        }

	
	ro.observation=o;
	ro.reward=1.0;
	ro.terminal=terminal;
	__RL_CHECK_STRUCT(ro.observation)
    return &ro;
}

void env_cleanup()
{
  freeRLStructPointer(o);
}


const char* env_message(const char* inMessage) {
	return "";
}
