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
*  $Revision: 921 $
*  $Date: 2008-10-14 18:34:08 -0400 (Tue, 14 Oct 2008) $
*  $Author: brian@tannerpages.com $
*  $HeadURL: http://rl-glue.googlecode.com/svn/trunk/tests/test_rl_episode_experiment.c $
* 
*/
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif


#include <stdio.h>
#include <rlglue/utils/C/RLStruct_util.h>

#include <rlglue/RL_glue.h>
#include <string.h>

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
	RL_init();
	/* No cutoff */
	int isTerminal = RL_episode(0);
	check_fail(isTerminal!=1);
	check_fail(RL_num_steps()!=5);
	

	isTerminal = RL_episode(1);

	check_fail(isTerminal!=0);
	check_fail(RL_num_steps()!=1);

	isTerminal = RL_episode(2);
	check_fail(isTerminal!=0);
	check_fail(RL_num_steps()!=2);

	isTerminal = RL_episode(4);
	check_fail(isTerminal!=0);
	check_fail(RL_num_steps()!=4);

	isTerminal = RL_episode(5);
	check_fail(isTerminal!=0);
	check_fail(RL_num_steps()!=5);

	isTerminal = RL_episode(6);
	check_fail(isTerminal!=1);
	check_fail(RL_num_steps()!=5);

	isTerminal = RL_episode(7);
	check_fail(isTerminal!=1);
	check_fail(RL_num_steps()!=5);
	
	RL_cleanup();

	if(tests_failed!=0)
		printf("Failed %d / %d checks in %s\n",tests_failed,test_count, __FILE__);
	else
		printf("Passed all %d checks in %s\n",test_count,__FILE__);
	return tests_failed;
}
