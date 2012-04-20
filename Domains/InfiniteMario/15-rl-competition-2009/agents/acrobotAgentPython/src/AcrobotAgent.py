#
# Copyright (C) 2009, Jose Antonio Martin H.
#
#
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#	 http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
#
# Example Agent in Python for the RL2009 Competition
# Copyright Jose Antonio Martin H. 27-1-2009
# This class requires numpy installed
# obtanin numpy from http://www.scipy.org/Download also under some linux based system you can install it with
# your default package manager.
print "Demo Acrobot Agent in Python"
try:
	from numpy import *
except ImportError:
	print "Error: you should install numpy"
	print """ This class requires numpy installed
			  obtanin numpy from http://www.scipy.org/Download also under some linux based system you can installit with
			  your default package manager.
		   """
	raise ImportError




import random as rnd
from rlglue.agent.Agent import Agent
from rlglue.types import Action
from rlglue.types import Observation
from rlglue.agent import AgentLoader as AgentLoader
from rlglue.utils.TaskSpecVRLGLUE3 import TaskSpecParser

from NNQ import NNQ

#speed up this a little bit.
# try to find psyco for improving the speed.
try:
	import psyco
	psyco.full()
except ImportError:
	pass


class AcrobotAgent(Agent):

	total_episodes=0
	proving = 0
	F=None
	Name = "Demo Python Actobot Agent"


	def agent_init(self,taskSpec):

		self.episode = 0
		self.steps   = 1
		

		print "TaskSpec:"
		print  taskSpec
		print
		self.action = Action()
		self.action_types = []
		self.action.intArray = []
		self.action.doubleArray = []
		self.action.numInts  = 1        
		
		rnd.seed(0)
		TaskSpec = TaskSpecParser(taskSpec)
		
		self.input_ranges = TaskSpec.getDoubleObservations()        
		print 'observations ranges',self.input_ranges




		#parse action
		self.action_ranges = TaskSpec.getIntActions()[0]
		print "action ranges",self.action_ranges
		
		
		self.actionlist    = range(self.action_ranges[0],self.action_ranges[1]+1)
		self.nactions      = len(self.actionlist)
		print "action list",self.actionlist

		#build a nearest neighbor function approximator
		self.Q = NNQ(nactions=self.nactions,input_ranges=self.input_ranges,nelemns=[7,7,3,3],alpha=0.5,lm=0.8)        
		
		self.SelectAction = e_greedy_selection(epsilon=0.1) #set eplison for e-greedy exploration
		self.SelectAction.parent = self

		# discount factor
		self.gamma   = 1.0#TaskSpec.getDiscountFactor()

		
		self.proving+= 1



	def agent_start(self,observation):

		self.steps = 1
		self.s     = array(observation.doubleArray)
		self.sp    = array(observation.doubleArray)

		self.a               = self.SelectAction(self.s)
		act                  = self.actionlist[self.a]
		self.action.intArray = [act]
		return self.action

	def agent_step(self,reward, observation):
		self.sp = array(observation.doubleArray)
		
		# select action prime
		self.ap              = self.SelectAction(self.sp)
		act                  = self.actionlist[self.ap]
		self.action.intArray = [act]


		# Update the Qtable, that is,  learn from the experience        
		# We update in both ways, a little trick
		target_value = reward + self.gamma*max(self.Q(self.sp)) #off-policy
		self.Q.Update(self.s,self.a,target_value)

		target_value = reward + self.gamma*self.Q(self.sp,self.ap) #on-policy 
		self.Q.Update(self.s,self.a,target_value)


		#update the current variables
		self.s = self.sp
		self.a = self.ap

		

		self.steps+= 1

		return self.action

	def agent_end(self,reward):
		self.Q.Update(self.s,self.a,reward+100)
		self.Q.ResetTraces()
		self.SelectAction.epsilon *=  0.9       
		self.total_episodes+=1
		self.episode+=1
		print "Episode:",self.episode,'steps:',self.steps,'total:',self.total_episodes,'proving:',self.proving,'alpha',self.Q.alpha,self.SelectAction.epsilon


	def agent_cleanup(self):
		pass

	def agent_freeze(self):
		self.Q.alpha              = 0.0
		self.SelectAction.epsilon = 0.0

	def agent_message(self,inMessage):
		return (self.Name + " does not respond to any messages.")
		


class e_greedy_selection:
	def __init__(self,epsilon):
		self.epsilon = epsilon
		self.parent=None

	def __call__(self,s):
		return self.select_action(s)

	def select_action(self,s):
		#selects an action using Epsilon-greedy strategy
		# s: the current state
		v = self.parent.Q(s)

		if (rnd.random()>self.epsilon):
			a = argmax(v)
		else:
			# selects a random action based on a uniform distribution
			a  = rnd.randint(0,self.parent.nactions-1)

		return a



if __name__=="__main__":
	AgentLoader.loadAgent(AcrobotAgent())


