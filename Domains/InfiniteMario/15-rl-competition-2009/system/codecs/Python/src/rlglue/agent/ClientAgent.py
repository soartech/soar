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
#  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Python/src/rlglue/agent/ClientAgent.py $

import sys

import rlglue.network.Network as Network
from rlglue.types import Action
from rlglue.types import Observation

class ClientAgent:
	kUnknownMessage = "Unknown Message: "
	network = None
	agent = None

	# (agent) -> void
	def __init__(self, agent):
		self.agent = agent
		self.network = Network.Network()

	# () -> void
	def onAgentInit(self):
		taskSpec = self.network.getString()
		self.agent.agent_init(taskSpec)
		self.network.clearSendBuffer()
		self.network.putInt(Network.kAgentInit)
		self.network.putInt(0) # No data following this header

	# () -> void
	def onAgentStart(self):
		observation = self.network.getObservation()
		action = self.agent.agent_start(observation)
		size = self.network.sizeOfAction(action)
		self.network.clearSendBuffer()
		self.network.putInt(Network.kAgentStart)
		self.network.putInt(size)
		self.network.putAction(action)

	# () -> void
	def onAgentStep(self):
		reward = self.network.getDouble()
		observation = self.network.getObservation()
		action = self.agent.agent_step(reward, observation)
		size = self.network.sizeOfAction(action)
		self.network.clearSendBuffer()
		self.network.putInt(Network.kAgentStep)
		self.network.putInt(size)
		self.network.putAction(action)

	# () -> void
	def onAgentEnd(self):
		reward = self.network.getDouble()
		self.agent.agent_end(reward)
		self.network.clearSendBuffer()
		self.network.putInt(Network.kAgentEnd)
		self.network.putInt(0) # No data in this packet

	# () -> void
	def onAgentCleanup(self):
		self.agent.agent_cleanup()
		self.network.clearSendBuffer()
		self.network.putInt(Network.kAgentCleanup)
		self.network.putInt(0) # No data in this packet

	# () -> void
	def onAgentMessage(self):
		message = self.network.getString()
		reply = self.agent.agent_message(message)
		self.network.clearSendBuffer()
		self.network.putInt(Network.kAgentMessage)
		if reply == None:
			#Brian Tanner added payload even for empty message (IE: send that the size is 0)
			self.network.putInt(4)
			self.network.putInt(0)
		else:
			#Brian Tanner, added 4 to the payload size because we putString sends the string AND its size
			self.network.putInt(len(reply) + 4)
			self.network.putString(reply)

	# (string, int, int) -> void
	def connect(self, host, port, timeout):
		self.network.connect(host, port, timeout);
		self.network.clearSendBuffer()
		self.network.putInt(Network.kAgentConnection)
		self.network.putInt(0) # No body to this packet
		self.network.send()

	# () -> void
	def close(self):
		self.network.close()

	# () -> void
	def runAgentEventLoop(self):
		agentState = 0
		dataSize = 0
		recvSize = 0
		remaining = 0

		while agentState != Network.kRLTerm:
			self.network.clearRecvBuffer();
			recvSize = self.network.recv(8) - 8; # We may have received the header and part of the payload
											# We need to keep track of how much of the payload was recv'd
			agentState = self.network.getInt()
			dataSize = self.network.getInt()
			
			remaining = dataSize - recvSize;
			if (remaining < 0):
				print("Remaining was less than 0!")
				remaining = 0

			amountReceived = self.network.recv(remaining)
			
			# Already read the header, discard it
			self.network.getInt()
			self.network.getInt()

			switch = {
				Network.kAgentInit: lambda self: self.onAgentInit(),
				Network.kAgentStart: lambda self: self.onAgentStart(),
				Network.kAgentStep: lambda self: self.onAgentStep(),
				Network.kAgentEnd: lambda self: self.onAgentEnd(),
				Network.kAgentCleanup: lambda self: self.onAgentCleanup(),
				Network.kAgentMessage: lambda self: self.onAgentMessage() }
			if agentState in switch:
				switch[agentState](self)
			elif agentState == Network.kRLTerm:
				pass
			else:
				sys.stderr.write(Network.kUnknownMessage % (str(agentState)))
				sys.exit(1)

			self.network.send()
