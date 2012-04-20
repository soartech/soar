# 
# Copyright (C) 2008, Brian Tanner
# 
#http://rl-glue-ext.googlecode.com/
#
# Licensed under the Apache License, Version 2.0 (the "License");
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
# Last modifed 22-1-2009 by Jose Antonio Martin H.
# Improving the test processTaskSpec 
#  $Revision: 617 $
#  $Date: 2009-02-05 04:24:12 -0500 (Thu, 05 Feb 2009) $
#  $Author: gabalz $
#  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Python/src/tests/test_taskspec.py $

import sys

from rlglue.utils import TaskSpecVRLGLUE3 
from glue_test import glue_test
tester =glue_test("test_taskspec")

def processTaskSpec(ts):
# you can cut the taskspec by the main words with new line
#ts= """VERSION RL-Glue-3.0 PROBLEMTYPE episodic DISCOUNTFACTOR 1 OBSERVATIONS INTS (3 0 1) DOUBLES (2 -1.2 0.5) (-.07 .07) CHARCOUNT 1024
#     ACTIONS INTS (2 0 4) CHARCOUNT 1024 REWARDS (-5.0 UNSPEC) EXTRA some other stuff goes here"""
	print
	print
	print "======================================================================================================="
	print ts
	print 
	print
	TaskSpec = TaskSpecVRLGLUE3.TaskSpecParser(ts)
	if TaskSpec.valid:                
		print "======================================================================================================="
		print "Version: ["+TaskSpec.getVersion()+"]"
		print "ProblemType: ["+TaskSpec.getProblemType()+"]"
		print "DiscountFactor: ["+TaskSpec.getDiscountFactor()+"]"
		print "======================================================================================================="
		print "\t \t \t \t Observations"
		print "======================================================================================================="
		print "Observations: ["+TaskSpec.getObservations()+"]"
		print "Integers:",TaskSpec.getIntObservations()
		print "Doubles: ",TaskSpec.getDoubleObservations()
		print "Chars:   ",TaskSpec.getCharCountObservations()
		print "======================================================================================================="
		print "\t \t \t \t Actions"
		print "======================================================================================================"
		print "Actions: ["+TaskSpec.getActions()+"]"
		print "Integers:",TaskSpec.getIntActions()
		print "Doubles: ",TaskSpec.getDoubleActions()
		print "Chars:   ",TaskSpec.getCharCountActions()
		print "======================================================================================================="        
		print "Reward :["+TaskSpec.getReward()+"]"
		print "Reward Range:",TaskSpec.getRewardRange()
		print "Extra: ["+TaskSpec.getExtra()+"]"
		print "remeber that by using len() you get the cardinality of lists!"
		print "Thus:"
		print "len(Doubles) ==> ",len(TaskSpec.getDoubleObservations())," Double Observations"
	else:
		print "Task spec was invalid, but I can try to get version: "+TaskSpec.getVersion();
			


f=open('sample_task_specs.txt', 'r')

for ts in f:
                processTaskSpec(ts)

f.close()

print tester.get_summary()
sys.exit(tester.getFailCount())


