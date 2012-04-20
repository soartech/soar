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
*  $Revision: 339 $
*  $Date: 2008-10-14 18:44:42 -0400 (Tue, 14 Oct 2008) $
*  $Author: brian@tannerpages.com $
*  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/C/tests/test_sanity_experiment.c $
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

	
#define LITTLE_ENDIAN_CHECK 0
#define BIG_ENDIAN_CHECK    1

int machineEndianness()
{
  int i = 1;
  char *p = (char *) &i;
  if (p[0] == 1) // Lowest address contains the least significant byte
    return LITTLE_ENDIAN_CHECK;
  else
    return BIG_ENDIAN_CHECK;
}

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

	if(machineEndianness()==LITTLE_ENDIAN_CHECK)
		printf("This machine is LITTLE ENDIAN\n");
	if(machineEndianness()==BIG_ENDIAN_CHECK)
		printf("This machine is BIGENDIAN\n");

	printf("Sizes: char %u int %u long %u float %u double %u\n",(unsigned int)sizeof(char),(unsigned int)sizeof(int),(unsigned int)sizeof(long),(unsigned int)sizeof(float),(unsigned int)sizeof(double));

	task_spec=RL_init();

	check_fail(strcmp(task_spec,"sample task spec")!=0);

	RL_cleanup();

	if(tests_failed!=0)
	printf("Failed %d / %d checks in %s\n",tests_failed,test_count, __FILE__);
	else
	printf("Passed all %d checks in %s\n",test_count,__FILE__);
	return tests_failed;
}
