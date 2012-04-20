# 
# Copyright (C) 2007, Mark Lee
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
#
#  $Revision: 446 $
#  $Date: 2009-01-22 22:20:21 -0500 (Thu, 22 Jan 2009) $
#  $Author: brian@tannerpages.com $
#  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Python/src/rlglue/environment/Environment.py $

from rlglue.types import Action
from rlglue.types import Observation
from rlglue.types import Reward_observation_terminal

class Environment:
	# () -> string
	def env_init():
		pass
	
	# () -> Observation
 	def env_start():
		pass
	
	# (Action) -> Reward_observation_terminal
	def env_step(action):
		pass
	
	# () -> void
	def env_cleanup():
		pass
	
	# (string) -> string
	def env_message(message):
		pass
