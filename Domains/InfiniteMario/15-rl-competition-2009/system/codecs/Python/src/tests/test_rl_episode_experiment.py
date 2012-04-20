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
#  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Python/src/tests/test_rl_episode_experiment.py $

import sys

import rlglue.RLGlue as RLGlue
from glue_test import glue_test
tester =glue_test("test_rl_episode")


task_spec=RLGlue.RL_init()

isTerminal = RLGlue.RL_episode(0);
tester.check_fail(isTerminal!=1);
tester.check_fail(RLGlue.RL_num_steps()!=5);

isTerminal = RLGlue.RL_episode(1);
tester.check_fail(isTerminal!=0);
tester.check_fail(RLGlue.RL_num_steps()!=1);

isTerminal = RLGlue.RL_episode(2);
tester.check_fail(isTerminal!=0);
tester.check_fail(RLGlue.RL_num_steps()!=2);

isTerminal = RLGlue.RL_episode(4);
tester.check_fail(isTerminal!=0);
tester.check_fail(RLGlue.RL_num_steps()!=4);

isTerminal = RLGlue.RL_episode(5);
tester.check_fail(isTerminal!=0);
tester.check_fail(RLGlue.RL_num_steps()!=5);

isTerminal = RLGlue.RL_episode(6);
tester.check_fail(isTerminal!=1);
tester.check_fail(RLGlue.RL_num_steps()!=5);

isTerminal = RLGlue.RL_episode(7);
tester.check_fail(isTerminal!=1);
tester.check_fail(RLGlue.RL_num_steps()!=5);


print tester.get_summary()
sys.exit(tester.getFailCount())

