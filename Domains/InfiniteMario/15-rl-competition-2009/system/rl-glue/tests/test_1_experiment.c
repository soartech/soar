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
*  $Revision: 967 $
*  $Date: 2009-02-04 15:14:33 -0500 (Wed, 04 Feb 2009) $
*  $Author: brian@tannerpages.com $
*  $HeadURL: http://rl-glue.googlecode.com/svn/trunk/tests/test_1_experiment.c $
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
  const reward_observation_action_terminal_t *roat;
  const char* task_spec;

    task_spec=RL_init();

	RL_start();
	
	roat=RL_step();

	check_fail(roat->observation->numInts!=1);
	check_fail(roat->observation->numDoubles!=0);
	check_fail(roat->observation->numChars!=0);
	check_fail(roat->observation->intArray[0]!=0);
    check_fail(strcmp("one|1.|one",RL_env_message("one"))!=0);
    check_fail(strcmp("one|1.|one",RL_agent_message("one"))!=0);
	check_fail(roat->terminal!=0);
	

	roat=RL_step();

    check_fail(strcmp("two|2.2.|two",RL_env_message("two"))!=0);
    check_fail(strcmp("two|2.2.|two",RL_agent_message("two"))!=0);
	check_fail(roat->terminal!=0);
	check_fail(roat->observation->numInts!=1);
	check_fail(roat->observation->numDoubles!=0);
	check_fail(roat->observation->numChars!=0);
	check_fail(roat->observation->intArray[0]!=1);

	roat=RL_step();

    check_fail(strcmp("three||three",RL_env_message("three"))!=0);
    check_fail(strcmp("three||three",RL_agent_message("three"))!=0);
	check_fail(roat->terminal!=0);
	check_fail(roat->observation->numInts!=1);
	check_fail(roat->observation->numDoubles!=0);
	check_fail(roat->observation->numChars!=0);	
	check_fail(roat->observation->intArray[0]!=2);

	roat=RL_step();
    check_fail(strcmp("four|4.|four",RL_env_message("four"))!=0);
    check_fail(strcmp("four|4.|four",RL_agent_message("four"))!=0);
	check_fail(roat->terminal!=0);
	check_fail(roat->observation->numInts!=1);
	check_fail(roat->observation->numDoubles!=0);
	check_fail(roat->observation->numChars!=0);
	check_fail(roat->observation->intArray[0]!=3);
	

	roat=RL_step();
    check_fail(strcmp("five|5.5.|five",RL_env_message("five"))!=0);
	check_fail(strcmp("five|4.|five",RL_agent_message("five"))!=0);
	check_fail(roat->terminal==0);
	/* Gabor has made it so this environment will step past terminal.  This is
	   not something we want to do in general at all.

	   But, in order to keep the other tests all working, I'll allow it*/
	
	roat=RL_step();
	check_fail(roat->observation->numInts!=5);
	check_fail(roat->observation->numDoubles!=5);
	check_fail(roat->observation->numChars!=5);
	check_fail(roat->observation->intArray[0]!=173);
	check_fail(roat->observation->intArray[1]!=-173);
	check_fail(roat->observation->intArray[2]!=2147483647);
	check_fail(roat->observation->intArray[3]!=0);
	
	check_fail(roat->observation->intArray[4]!=-2147483648);
	check_fail(roat->observation->doubleArray[0]!=0.0078125);
	check_fail(roat->observation->doubleArray[1]!=-0.0078125);
	check_fail(roat->observation->doubleArray[2]!=0);
	check_fail(roat->observation->doubleArray[3]!=0.0078125e150);
	check_fail(roat->observation->doubleArray[4]!=-0.0078125e150);
	check_fail(roat->observation->charArray[0]!='g');
	check_fail(roat->observation->charArray[1]!='F');
	check_fail(roat->observation->charArray[2]!='?');
	check_fail(roat->observation->charArray[3]!=' ');
	check_fail(roat->observation->charArray[4]!='&');


	RL_cleanup();

	if(tests_failed!=0)
		printf("Failed %d / %d checks in %s\n",tests_failed,test_count, __FILE__);
	else
		printf("Passed all %d checks in %s\n",test_count,__FILE__);
	return tests_failed;
}
