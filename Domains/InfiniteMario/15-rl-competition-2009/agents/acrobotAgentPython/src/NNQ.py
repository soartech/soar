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
# This class requires numpy installed
# obtanin numpy from http://www.scipy.org/Download also under some linux based system you can installit with
# your default package manager.
# good luck in the competition
# Jose Antonio Martin H. 27-01-20009
#

try:
	from numpy import *
	from numpy.random import *
	from numpy.linalg import *
except ImportError:
	print "Error: you should install numpy"
	print """ This class requires numpy installed
			  obtanin numpy from http://www.scipy.org/Download also under some linux based system you can installit with
			  your default package manager.
		   """
	raise ImportError

class NNQ():
	"""
	Simple Nearest Neighborn Function Approximator
	parameters:
	nactions: the number of actions, i.e. if there are threee actios [-1,0,1] then nactions should be 3
	input_ranges : a list of tuples indicating the min and max value of echa variable in observation
	nelemns: a list of numbers indicating the number of elements in the interval between min and max for each input dimension
	alpha: learning rate
	lm: lamda TD parameter [0,1]
	"""
	
	def __call__(self,s,a=None):
		""" implement here the returned Qvalue of state (s) and action(a)
			e.g. Q.GetValue(s,a) is equivalent to Q(s,a)
		"""
		if a==None:
			return self.GetValue(s)
		
		return self.GetValue(s,a)

	def __init__(self,nactions,input_ranges,nelemns=[],alpha=0.3,lm=0.90):        
		
		self.cl        = self.ndlinspace(input_ranges,nelemns)
		self.shape      = self.cl.shape
		self.nactions   = nactions
		self.Q          = zeros((self.shape[0],nactions))
		self.e          = zeros((self.shape[0],nactions))
		self.NN         = 0
		self.alpha      = alpha
		self.lm         = lm
		self.last_state = self.cl[0]

	def ndlinspace(self,input_ranges,nelems):
		""" ndlinspace: n-dimensional linspace function
			input_ranges = [[-1,1],[-0.07,0.07]]
			nelems = (5,5)
		"""
		x = indices(nelems).T.reshape(-1,len(nelems))+1.0
		lbounds = []
		ubounds = []
		from_b  = array(nelems,float)
		for r in input_ranges:
			lbounds.append(r[0])
			ubounds.append(r[1])


		lbounds = array(lbounds,float)
		ubounds = array(ubounds,float)
		y = (lbounds) + (((x-1)/(from_b-1))*((ubounds)-(lbounds)))
		return y
	
        def ResetTraces(self):
                self.e = zeros((self.shape[0],self.nactions))


	def GetNN(self,s):                
                if allclose(self.last_state,s):
                        return self.nn
                else:
                        self.last_state = s
                        
		state   = array(s)
		self.d  = sum((self.cl-state)**2,1) # calculate distances from (s)
		self.nn = self.d.argmin()           # find index of the nearest neighbor        
		return self.nn    

	def GetValue(self,s,a=None):
		""" Return the Q(s,a) value of state (s) for action (a)
			or al values for Q(s)
		"""
		NN = self.GetNN(s)
		if a==None:
			return self.Q[NN]
		
		return self.Q[NN,a]


	def Update(self,s,a,v):
		""" update action value for action(a)
		"""
		NN = self.GetNN(s)
		self.e[NN]   =  0.0
                self.e[NN,a] =  1.0

		TD_error    =  v - self.Q[NN,a]
                self.Q +=  self.alpha * (TD_error) * self.e                
                self.e *= self.lm
		






