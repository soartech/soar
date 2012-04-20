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
#  $Revision: 592 $
#  $Date: 2009-02-04 18:24:59 -0500 (Wed, 04 Feb 2009) $
#  $Author: brian@tannerpages.com $
#  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Python/src/rlglue/network/Network.py $

#
#The Network class is defined in here
#

import socket
import struct
import array
import time
import sys
import StringIO
try:
    import numpy
    numpy_int_type = numpy.dtype('int32').newbyteorder('>')
    numpy_float_type = numpy.dtype('float64').newbyteorder('>')
    numpy_char_type = 'S1'#numpy.dtype('uint8').newbyteorder('>')
except:
    pass

from rlglue.types import Action
from rlglue.types import Observation
from rlglue.types import Reward_observation_terminal
from rlglue.types import RL_Abstract_Type

# RL-Glue needs to know what type of object is trying to connect.
kExperimentConnection  = 1
kAgentConnection       = 2
kEnvironmentConnection = 3

kAgentInit    = 4 # agent_* start by sending one of these values
kAgentStart   = 5 # to the client to let it know what type of
kAgentStep    = 6 # event to respond to
kAgentEnd     = 7
kAgentCleanup = 8
kAgentMessage = 10

kEnvInit          = 11
kEnvStart         = 12
kEnvStep          = 13
kEnvCleanup       = 14
kEnvMessage       = 19

kRLInit           = 20
kRLStart          = 21
kRLStep           = 22
kRLCleanup        = 23
kRLReturn         = 24
kRLNumSteps       = 25
kRLNumEpisodes    = 26
kRLEpisode        = 27
kRLAgentMessage   = 33
kRLEnvMessage     = 34

kRLTerm           = 35

kLocalHost = "127.0.0.1"
kDefaultPort = 4096
kRetryTimeout = 2

kDefaultBufferSize = 4096
kIntSize = 4
kDoubleSize = 8
kCharSize = 1

kUnknownMessage = "Unknown Message: %s\n"

class Network:
	
	def __init__(self):
		self.sock = None
		self.recvBuffer = StringIO.StringIO('')
		self.sendBuffer = StringIO.StringIO('')

                if 'numpy' in globals():
                    self.getAbstractType = self.getAbstractType_numpy
                else:
                    self.getAbstractType = self.getAbstractType_list
	
	def connect(self, host=kLocalHost, port=kDefaultPort, retryTimeout=kRetryTimeout):
		while self.sock == None:
			try:
				self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
				self.sock.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
				self.sock.connect((host, port))
			except socket.error, msg:
				self.sock = None
				time.sleep(retryTimeout)
			else:
				break
	
	def close(self):
		self.sock.close()
		
	def send(self):
		self.sock.sendall(self.sendBuffer.getvalue())
	
	def recv(self,size):
		s = ''
		while len(s) < size:
			s += self.sock.recv(size - len(s))
		self.recvBuffer.write(s)
		self.recvBuffer.seek(0)
		return len(s)
	
	def clearSendBuffer(self):
		self.sendBuffer.close()
		self.sendBuffer = StringIO.StringIO()
	
	def clearRecvBuffer(self):
		self.recvBuffer.close()
		self.recvBuffer = StringIO.StringIO()
	
	def flipSendBuffer(self):
		self.clearSendBuffer()

	def flipRecvBuffer(self):
		self.clearRecvBuffer()
	
	def getInt(self):
		s = self.recvBuffer.read(kIntSize)
		return struct.unpack("!i",s)[0]
	
	def getDouble(self):
		s = self.recvBuffer.read(kDoubleSize)
		return struct.unpack("!d",s)[0]
	
	def getString(self):
		#If you read 0 you get "" not None so that's fine
		length = self.getInt()
		return self.recvBuffer.read(length)
	
	
	def getAbstractType_list(self):
		numInts = self.getInt()
		numDoubles = self.getInt()		
		numChars = self.getInt()		
		returnStruct=RL_Abstract_Type()
		
		if numInts > 0:
			s = self.recvBuffer.read(numInts*kIntSize)
			returnStruct.intArray = list(struct.unpack("!%di" % (numInts),s))
		if numDoubles > 0:
			s = self.recvBuffer.read(numDoubles*kDoubleSize)
			returnStruct.doubleArray = list(struct.unpack("!%dd" % (numDoubles),s))
		if numChars > 0:
			s = self.recvBuffer.read(numChars*kCharSize)
			returnStruct.charArray = list(struct.unpack("!%dc" % (numChars),s))
		return returnStruct

	def getAbstractType_numpy(self):
		numInts = self.getInt()
		numDoubles = self.getInt()		
		numChars = self.getInt()		
		returnStruct=RL_Abstract_Type()
		
		if numInts > 0:
			s = self.recvBuffer.read(numInts*kIntSize)
                        assert kIntSize == 4
                        returnStruct.intArray = numpy.frombuffer(s,
                                dtype=numpy_int_type,
                                count=numInts)
		if numDoubles > 0:
			s = self.recvBuffer.read(numDoubles*kDoubleSize)
                        returnStruct.doubleArray = numpy.frombuffer(s, count=numDoubles,
                                dtype=numpy_float_type)
		if numChars > 0:
			s = self.recvBuffer.read(numChars*kCharSize)
			returnStruct.charArray = numpy.frombuffer(s, count=numChars,
                                dtype=numpy_char_type)
		return returnStruct
		
	def getObservation(self):
		return Observation.fromAbstractType(self.getAbstractType())

	def getAction(self):
		return Action.fromAbstractType(self.getAbstractType())

	def putInt(self,value):
		self.sendBuffer.write(struct.pack("!i",value))
	
	def putDouble(self,value):
		self.sendBuffer.write(struct.pack("!d",value))
	
	def putString(self,value):
		if value == None:
			value = ''
		self.putInt(len(value))
		self.sendBuffer.write(value)
	
	def putObservation(self,obs):
		self.putAbstractType(obs)
	
	def putAction(self,action):
		self.putAbstractType(action)

	def putAbstractType(self, theItem):
		self.putInt(len(theItem.intArray))
		self.putInt(len(theItem.doubleArray))
		self.putInt(len(theItem.charArray))
		if len(theItem.intArray) > 0:
			self.sendBuffer.write(struct.pack("!%di" % (len(theItem.intArray)),*(theItem.intArray)))
		if len(theItem.doubleArray) > 0:
			self.sendBuffer.write(struct.pack("!%dd" % (len(theItem.doubleArray)),*(theItem.doubleArray)))
		if len(theItem.charArray) > 0:
			self.sendBuffer.write(struct.pack("!%dc" % (len(theItem.charArray)),*(theItem.charArray)))
		
	def putRewardObservation(self,rewardObservation):
		self.putInt(rewardObservation.terminal);
		self.putDouble(rewardObservation.r);
		self.putObservation(rewardObservation.o);
	
	
	def sizeOfAbstractType(self, theItem):
		size = kIntSize * 3
		intSize = 0
		doubleSize = 0
		charSize = 0
		if theItem != None:
			if theItem.intArray != None:
				intSize = kIntSize * len(theItem.intArray)
			if theItem.doubleArray != None:
				doubleSize = kDoubleSize * len(theItem.doubleArray)
			if theItem.charArray != None:
				charSize = kCharSize * len(theItem.charArray)
		return size + intSize + doubleSize + charSize


	def sizeOfAction(self,action):
		return self.sizeOfAbstractType(action)
	
	def sizeOfObservation(self,observation):
		return self.sizeOfAbstractType(observation)

	def sizeOfRewardObservation(self,reward_observation):
		return kIntSize + kDoubleSize + self.sizeOfObservation(reward_observation.o)
