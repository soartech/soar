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
#  $Revision: 473 $
#  $Date: 2009-01-29 22:50:12 -0500 (Thu, 29 Jan 2009) $
#  $Author: brian@tannerpages.com $
#  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Python/src/rlglue/agent/AgentLoader.py $

import sys
import os
import rlglue.network.Network as Network
from ClientAgent import ClientAgent

from rlglue.versions import get_svn_codec_version
from rlglue.versions import get_codec_version

def loadAgent(theAgent):
	theSVNVersion=get_svn_codec_version()
	theCodecVersion=get_codec_version()
	client = ClientAgent(theAgent)

	host = Network.kLocalHost
	port = Network.kDefaultPort

	hostString = os.getenv("RLGLUE_HOST")
	portString = os.getenv("RLGLUE_PORT")

	if (hostString != None):
		host = hostString

	try:
		port = int(portString)
	except TypeError:
		port = Network.kDefaultPort
		

	print "RL-Glue Python Agent Codec Version: "+theCodecVersion+" (Build "+theSVNVersion+")"
	print "\tConnecting to " + host + " on port " + str(port) + "..."
	sys.stdout.flush()

	client.connect(host, port, Network.kRetryTimeout)
	print "\t Agent Codec Connected"
	client.runAgentEventLoop()
	client.close()


def loadAgentLikeScript():
	agentModule = __import__(sys.argv[1])
	agentClass = getattr(agentModule,sys.argv[1])
	agent = agentClass()

	client = ClientAgent(agent)

	loadAgent(agent)
