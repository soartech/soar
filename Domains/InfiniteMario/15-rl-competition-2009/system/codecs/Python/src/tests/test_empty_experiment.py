# 
# Copyright (C) 2008, Brian Tanner
# 
#http://rl-glue-ext.googlecode.com/
#
# Licensed under the Apache License, Version 2.0 (the "License")
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
#  $Revision: 617 $
#  $Date: 2009-02-05 04:24:12 -0500 (Thu, 05 Feb 2009) $
#  $Author: gabalz $
#  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Python/src/tests/test_empty_experiment.py $

import sys

import rlglue.RLGlue as RLGlue
from glue_test import glue_test
tester =glue_test("test_empty")


task_spec=RLGlue.RL_init()


for whichEpisode in range(1, 5):
	startTuple=RLGlue.RL_start()
	
	if(whichEpisode%2==0):
		tester.check_fail(len(startTuple.a.intArray)!=0)
		tester.check_fail(len(startTuple.a.doubleArray)!=0)
		tester.check_fail(len(startTuple.a.charArray)!=0)

		tester.check_fail(len(startTuple.o.intArray)!=0)
		tester.check_fail(len(startTuple.o.doubleArray)!=0)
		tester.check_fail(len(startTuple.o.charArray)!=0)
	else:
		tester.check_fail(len(startTuple.a.intArray)!=7)
		tester.check_fail(len(startTuple.a.doubleArray)!=3)
		tester.check_fail(len(startTuple.a.charArray)!=1)

		tester.check_fail(len(startTuple.o.intArray)!=2)
		tester.check_fail(len(startTuple.o.doubleArray)!=4)
		tester.check_fail(len(startTuple.o.charArray)!=5)
	
	
	for whichStep in range(0,5):
		stepTuple=RLGlue.RL_step()
		tester.check_fail(stepTuple.terminal!=0)
		tester.check_fail(stepTuple.r!=0)

		if(whichEpisode%2==0):
			tester.check_fail(len(stepTuple.a.intArray)!=0)
			tester.check_fail(len(stepTuple.a.doubleArray)!=0)
			tester.check_fail(len(stepTuple.a.charArray)!=0)

			tester.check_fail(len(stepTuple.o.intArray)!=0)
			tester.check_fail(len(stepTuple.o.doubleArray)!=0)
			tester.check_fail(len(stepTuple.o.charArray)!=0)
		else:
			tester.check_fail(len(stepTuple.a.intArray)!=7)
			tester.check_fail(len(stepTuple.a.doubleArray)!=3)
			tester.check_fail(len(stepTuple.a.charArray)!=1)

			tester.check_fail(len(stepTuple.o.intArray)!=2)
			tester.check_fail(len(stepTuple.o.doubleArray)!=4)
			tester.check_fail(len(stepTuple.o.charArray)!=5)
		

print tester.get_summary()
sys.exit(tester.getFailCount())

