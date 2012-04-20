# console Trainer for RL Competition
# Copyright (C) 2007, Brian Tanner brian@tannerpages.com (http://brian.tannerpages.com/)
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

import rlglue.RLGlue as RLGlue
from consoleTrainerHelper import *

def main():
	whichTrainingMDP = 1
	# Uncomment ONE of the following lines to choose your experiment
	#loadTetris(whichTrainingMDP); #put the desired parameter set in where MDP is in [0,19]
	#loadHelicopter(whichTrainingMDP); #put the desired parameter set in where MDP is in [0,9]
 	#loadAcrobot(whichTrainingMDP); #put the desired parameter set in where MDP is in [1,49] #0 is standard acrobot
 	#loadPolyathlon(whichTrainingMDP); #put the desired parameter set in where MDP is in [0,5]
	loadMario(True, True, 121, 0, 10, whichTrainingMDP);


	# and then,
	#		just run the experiment:
	RLGlue.RL_init()
	episodesToRun = 10
	totalSteps = 0
	for i in range(episodesToRun):
		RLGlue.RL_episode(20000)
		thisSteps = RLGlue.RL_num_steps()
		print "Total steps in episode %d is %d" %(i, thisSteps)
		totalSteps += thisSteps

	print "Total steps : %d\n" % (totalSteps)
	RLGlue.RL_cleanup()

if __name__ == "__main__":
	main()
