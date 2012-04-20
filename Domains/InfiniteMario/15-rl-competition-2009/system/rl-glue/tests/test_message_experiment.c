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
*  $Revision: 918 $
*  $Date: 2008-10-14 14:38:05 -0400 (Tue, 14 Oct 2008) $
*  $Author: brian@tannerpages.com $
*  $HeadURL: http://rl-glue.googlecode.com/svn/trunk/tests/test_message_experiment.c $
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
	const char *theResponse;
	
	RL_init();

	check_fail(strcmp("empty",RL_env_message(0))!=0);

	check_fail(strcmp("empty",RL_env_message(""))!=0);

	check_fail(strcmp("empty",RL_agent_message(0))!=0);

	check_fail(strcmp("empty",RL_agent_message(""))!=0);

	check_fail(strcmp("",RL_env_message("empty"))!=0);

	check_fail(strcmp("",RL_agent_message("empty"))!=0);

	theResponse=RL_env_message("null");
	check_fail(!(theResponse!=0 ||strcmp("",theResponse)!=0));
	
	theResponse=RL_agent_message("null");
	check_fail(!(theResponse!=0 ||strcmp("",theResponse)!=0));


	check_fail(strcmp("1",RL_env_message("1"))!=0);
	check_fail(strcmp("1",RL_agent_message("1"))!=0);

	check_fail(strcmp("1000000000000000000000",RL_env_message("1000000000000000000000"))!=0);
	check_fail(strcmp("1000000000000000000000",RL_agent_message("1000000000000000000000"))!=0);

	check_fail(strcmp("21111111111111111111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111111113",RL_env_message("21111111111111111111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111111113"))!=0);
	check_fail(strcmp("45555555555555555555555555555555555555555555555555555555655555555555555555555555555555555555555555555555555555559",RL_agent_message("45555555555555555555555555555555555555555555555555555555655555555555555555555555555555555555555555555555555555559"))!=0);

	RL_cleanup();
	if(tests_failed!=0)
		printf("Failed %d / %d checks in %s\n",tests_failed,test_count, __FILE__);
	else
		printf("Passed all %d checks in %s\n",test_count,__FILE__);
	return tests_failed;
}
