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
#  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Python/src/rlglue/environment/EnvironmentLoaderScript.py $

import sys
import os
import rlglue.network.Network as Network
from ClientEnvironment import ClientEnvironment
import EnvironmentLoader as EnvironmentLoader
def main():
	usage = "PYTHONPATH=<Path to RLGlue> python -c 'import rlglue.environment.EnvironmentLoaderScript' <Environment>";

	envVars = "The following environment variables are used by the environment to control its function:\n" + \
	"RLGLUE_HOST  : If set the agent will use this ip or hostname to connect to rather than " + Network.kLocalHost + "\n" + \
	"RLGLUE_PORT  : If set the agent will use this port to connect on rather than " + str(Network.kDefaultPort) + "\n"

	if (len(sys.argv) < 2):
		print usage
		print envVars
		sys.exit(1)
	
	EnvironmentLoader.loadEnvironmentLikeScript()

main()