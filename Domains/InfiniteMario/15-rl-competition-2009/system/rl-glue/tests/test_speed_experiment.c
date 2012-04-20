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
*  $Revision: 148 $
*  $Date: 2008-09-17 19:20:32 -0600 (Wed, 17 Sep 2008) $
*  $Author: brian@tannerpages.com $
*  $HeadURL: https://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/C/tests/test_speed_experiment.c $
* 
*/
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif



#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <rlglue/RL_glue.h>
#include <rlglue/utils/C/RLStruct_util.h>


#ifdef HAVE_SYS_TIME_H
#ifdef HAVE_GETTIMEOFDAY

/* This is probably not portable and we should really check it with configure */
#include <sys/time.h>
	long get_current_ms_time() {
	  struct timeval tval;
	  static long s_time_at_start= 0;
	  gettimeofday(&tval,0);

	  if ( 0 == s_time_at_start ) 
	    s_time_at_start= tval.tv_sec;

	  return (tval.tv_sec - s_time_at_start) * 1000 + tval.tv_usec / 1000;
	}
#endif
#endif 

#ifndef HAVE_SYS_TIME_H
#include <time.h>
long get_current_ms_time(){
	printf("Warning :: Good timing not available because we don't have gettimeofday\n");
	return (long)time(0);
}
#endif

int tests_failed=0;
int test_count=0;

void check_fail(int condition){
	test_count++;
	if(condition!=0){
		printf("Failed check: %d\n",test_count);
		tests_failed++;
	}
}
	
	
int main(int argc, char *argv[]) {
  const char* task_spec;
	long t0,t1;
	int steps;

    task_spec=RL_init();
	//Run an episode to get the System warmed up, etc             
	RL_episode(500);
        
	t0=get_current_ms_time();
	RL_episode(0);
	t1=get_current_ms_time();
	
	steps=RL_num_steps();
	
	printf ("\telapsed time in ms: %ld, per step is %f\n", (t1 - t0), ((float)(t1-t0)/(float)steps));
		
	t0=get_current_ms_time();
	
	RL_episode(0);
	
	t1=get_current_ms_time();

	steps=RL_num_steps();

	printf ("\telapsed time in ms: %ld, per step is %f\n", (t1 - t0), ((float)(t1-t0)/(float)steps));
        
	RL_cleanup();

	if(tests_failed!=0)
		printf("Failed %d / %d checks in %s\n",tests_failed,test_count, __FILE__);
	else
		printf("Passed all %d checks in %s\n",test_count,__FILE__);
	return tests_failed;
}
