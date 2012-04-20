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
#  $Revision: 617 $
#  $Date: 2009-02-05 04:24:12 -0500 (Thu, 05 Feb 2009) $
#  $Author: gabalz $
#  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Python/src/tests/test_1_experiment.py $

import sys

import rlglue.RLGlue as RLGlue
from glue_test import glue_test
tester=glue_test("test_1")

task_spec=RLGlue.RL_init();

RLGlue.RL_start();

roat=RLGlue.RL_step();
tester.check_fail("one|1.|one"!=RLGlue.RL_env_message("one"));
tester.check_fail("one|1.|one"!=RLGlue.RL_agent_message("one"));
tester.check_fail(roat.terminal!=0);
tester.check_fail(roat.r!=1.0);
tester.check_fail(len(roat.o.intArray)!=1);
tester.check_fail(len(roat.o. doubleArray)!=0);
tester.check_fail(len(roat.o. charArray)!=0);
tester.check_fail(roat.o.intArray[0]!=0);

roat=RLGlue.RL_step();
tester.check_fail("two|2.2.|two"!=RLGlue.RL_env_message("two"));
tester.check_fail("two|2.2.|two"!=RLGlue.RL_agent_message("two"));
tester.check_fail(roat.terminal!=0);
tester.check_fail(roat.r!=1.0);
tester.check_fail(len(roat.o.intArray)!=1);
tester.check_fail(len(roat.o. doubleArray)!=0);
tester.check_fail(len(roat.o. charArray)!=0);
tester.check_fail(roat.o.intArray[0]!=1);

roat=RLGlue.RL_step();
tester.check_fail("three||three"!=RLGlue.RL_env_message("three"));
tester.check_fail("three||three"!=RLGlue.RL_agent_message("three"));
tester.check_fail(roat.terminal!=0);
tester.check_fail(roat.r!=1.0);
tester.check_fail(len(roat.o.intArray)!=1);
tester.check_fail(len(roat.o. doubleArray)!=0);
tester.check_fail(len(roat.o. charArray)!=0);	
tester.check_fail(roat.o.intArray[0]!=2);

roat=RLGlue.RL_step();
tester.check_fail("four|4.|four"!=RLGlue.RL_env_message("four"));
tester.check_fail("four|4.|four"!=RLGlue.RL_agent_message("four"));
tester.check_fail(roat.terminal!=0);
tester.check_fail(roat.r!=1.0);
tester.check_fail(len(roat.o.intArray)!=1);
tester.check_fail(len(roat.o. doubleArray)!=0);
tester.check_fail(len(roat.o. charArray)!=0);
tester.check_fail(roat.o.intArray[0]!=3);


roat=RLGlue.RL_step();
tester.check_fail("five|5.5.|five"!=RLGlue.RL_env_message("five"));
tester.check_fail("five|4.|five"!=RLGlue.RL_agent_message("five"));
tester.check_fail(roat.terminal==0);
tester.check_fail(roat.r!=1.0);

roat=RLGlue.RL_step();
tester.check_fail(roat.terminal!=0);
tester.check_fail(roat.r!=-2.0);
tester.check_fail(len(roat.o.intArray)!=5);
tester.check_fail(len(roat.o.doubleArray)!=5);
tester.check_fail(len(roat.o.charArray)!=5);
tester.check_fail(roat.o.intArray[0]!=173);
tester.check_fail(roat.o.intArray[1]!=-173);
tester.check_fail(roat.o.intArray[2]!=2147483647);
tester.check_fail(roat.o.intArray[3]!=0);
tester.check_fail(roat.o.intArray[4]!=-2147483648);
tester.check_fail(roat.o.doubleArray[0]!=0.0078125);
tester.check_fail(roat.o.doubleArray[1]!=-0.0078125);
tester.check_fail(roat.o.doubleArray[2]!=0.0);
tester.check_fail(roat.o.doubleArray[3]!=0.0078125e150);
tester.check_fail(roat.o.doubleArray[4]!=-0.0078125e150);
tester.check_fail(roat.o.charArray[0]!='g');
tester.check_fail(roat.o.charArray[1]!='F');
tester.check_fail(roat.o.charArray[2]!='?');
tester.check_fail(roat.o.charArray[3]!=' ');
tester.check_fail(roat.o.charArray[4]!='&');

print tester.get_summary()
sys.exit(tester.getFailCount())

