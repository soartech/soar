# 
# Copyright (C) 2007, Mark Lee
# 
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

import random
import sys
from rlglue.agent.Agent import Agent
from rlglue.types import Action
from rlglue.types import Observation
from rlglue.agent import AgentLoader as AgentLoader
from rlglue.utils.TaskSpecVRLGLUE3 import TaskSpecParser


from random import choice
import string

def GenPasswd():
    chars = string.letters + string.digits
    for i in range(8):
            newpasswd = newpasswd + choice(chars)
    return newpasswd

def GenPasswd2(length=8, chars=string.letters + string.digits):
    return [choice(chars) for i in range(length)]


class RandomAgent(Agent):
	
	def agent_init(self,taskSpecString):
		TaskSpec = TaskSpecParser(taskSpecString);
		if TaskSpec.valid:
			print "Task spec was valid";
		else:
			print "Task Spec could not be parsed: "+taskSpecString;
			exit()

		#parse action
		self.action = Action()
		self.int_action_ranges    = TaskSpec.getIntActions()
		self.double_action_ranges = TaskSpec.getDoubleActions()
		self.action.numInts       = len(self.int_action_ranges)
		self.action.numDoubles    = len(self.double_action_ranges)
		self.action.numChars      = TaskSpec.getCharCountActions()
		
		#print "int",self.int_action_ranges,self.action.numInts 
		#print "doubles",self.double_action_ranges,self.action.numDoubles
		#print "chars",self.action.numChars 
		
		
		random.seed(0)
	
	def agent_start(self,observation):
		self.randomify()
		return self.action
	
	def agent_step(self,reward, observation):
		self.randomify()
		return self.action
	
	def agent_end(self,reward):
		pass
	
	def agent_cleanup(self):
		pass
	
	def agent_freeze(self):
		pass
	
	def agent_message(self,inMessage):
		return None
	
	def randomify(self):
		self.action.intArray = []
		self.action.doubleArray = []
		

		for min_action,max_action in self.int_action_ranges:					
				act = random.randrange(min_action,max_action+1)
				self.action.intArray.append(act)

		for min_action,max_action in self.double_action_ranges:					
				act = random.uniform(min_action,max_action)
				self.action.doubleArray.append(act)
				
		self.action.charArray   = GenPasswd2(self.action.numChars)
		#print self.action.intArray
		#print self.action.doubleArray
		#print self.action.charArray
		
		

			

if __name__=="__main__":        
	AgentLoader.loadAgent(RandomAgent())
