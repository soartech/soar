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
#  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Python/src/tests/test_message_experiment.py $

import sys

import rlglue.RLGlue as RLGlue
from glue_test import glue_test
tester =glue_test("test_message")


task_spec=RLGlue.RL_init()

tester.check_fail("empty"!=RLGlue.RL_env_message(None));

tester.check_fail("empty"!=RLGlue.RL_env_message(""));

tester.check_fail("empty"!=RLGlue.RL_agent_message(None));

tester.check_fail("empty"!=RLGlue.RL_agent_message(""));

tester.check_fail(""!=RLGlue.RL_env_message("empty"));

tester.check_fail(""!=RLGlue.RL_agent_message("empty"));

theResponse=RLGlue.RL_env_message("null");
tester.check_fail(not(theResponse!=None or ""!=theResponse));

theResponse=RLGlue.RL_agent_message("null");
tester.check_fail(not(theResponse!=None or ""!=theResponse));


tester.check_fail("1"!=RLGlue.RL_env_message("1"));
tester.check_fail("1"!=RLGlue.RL_agent_message("1"));

tester.check_fail("1000000000000000000000"!=RLGlue.RL_env_message("1000000000000000000000"));
tester.check_fail("1000000000000000000000"!=RLGlue.RL_agent_message("1000000000000000000000"));

tester.check_fail("21111111111111111111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111111113"!=RLGlue.RL_env_message("21111111111111111111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111111113"));
tester.check_fail("45555555555555555555555555555555555555555555555555555555655555555555555555555555555555555555555555555555555555559"!=RLGlue.RL_agent_message("45555555555555555555555555555555555555555555555555555555655555555555555555555555555555555555555555555555555555559"));


print tester.get_summary()
sys.exit(tester.getFailCount())

