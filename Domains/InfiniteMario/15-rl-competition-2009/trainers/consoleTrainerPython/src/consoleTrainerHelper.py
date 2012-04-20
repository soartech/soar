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
from ParameterHolder import *

BENCHMARK = 0
ENVSHELL = 1
LISTQUERY = 1
NOVALUE = 3
STRINGLIST = 0
LOADQUERY = 2
UNLOADQUERY = 3

# (string,ParameterHolder) -> void
def load(envNameString, theParams):
	loadPayLoad = envNameString+":"+theParams.stringSerialize()
	theRequest = "TO=%d FROM=%d CMD=%d VALTYPE=%d VALS=%s" % (ENVSHELL, BENCHMARK, LOADQUERY, STRINGLIST,loadPayLoad)
	RLGlue.RL_env_message(theRequest)

#(string) -> ParameterHolder
def preload(envNameString):
	theRequest = "TO=%d FROM=%d CMD=%d VALTYPE=%d VALS=NULL" % (ENVSHELL, BENCHMARK, LISTQUERY, NOVALUE)
	theResponse = RLGlue.RL_env_message(theRequest)
	lastColonPos = theResponse.rfind("=")
	thePayLoad = theResponse[lastColonPos+1:]
	if thePayLoad[-1] == ':':
		thePayLoad = thePayLoad[:-1]
	items = thePayLoad.split(':')
	theNames = []
	theParamHolders = []
	for i in range(1,len(items),2):
		theNames.append(items[i])
		theParamHolders.append(ParameterHolder(items[i+1]))
	
	for i in range(len(theNames)):
		if theNames[i] == envNameString:
			indexOfMyEnv = i
	
	return theParamHolders[indexOfMyEnv]

#(string) -> void
def preloadAndLoad(envNameString):
	p = preload(envNameString)
	load(envNameString,p)

#
# Tetris has an integer parameter called pnum that takes values in [0,19]
# Setting this parameter changes the exact tetris problem you are solving
#
#(int) -> void
def loadTetris(whichParamSet):
	theEnvString = "GeneralizedTetris - Java"
	theParams = preload(theEnvString)
	theParams.setIntegerParam("pnum",whichParamSet)
	load(theEnvString, theParams)

#
# MountainCar has an integer parameter called pnum that takes values in [0,29]
# Setting this parameter changes the exact mountain car problem you are solving
#
# (int) -> void
def loadMountainCar(whichParamSet):
	theEnvString = "GeneralizedMountainCar - Java"
	theParams = preload(theEnvString)
	theParams.setIntegerParam("pnum",whichParamSet)
	load(theEnvString, theParams)

#
# Acrobot has an integer parameter called pnum that takes values in [0,49]
# Setting this parameter changes the exact Acrobot problem you are solving
#
# (int) -> void
def loadAcrobot(whichParamSet):
	theEnvString = "GeneralizedAcrobot - Java"
	theParams = preload(theEnvString)
	theParams.setIntegerParam("pnum",whichParamSet)
	load(theEnvString, theParams)



#
# Helicopter has an integer parameter called pnum that takes values in [0,9]
# Setting this parameter changes the exact helicopter problem you are solving
#
#(int) -> void
def loadHelicopter(whichParamSet):
	theEnvString = "GeneralizedHelicopter - Java"
	theParams = preload(theEnvString)
	theParams.setIntegerParam("pnum",whichParamSet)
	load(theEnvString, theParams)

def loadPolyathlon(whichParamSet):
	theEnvString = "TrainingPolyathlon - Java"
	theParams = preload(theEnvString)
	theParams.setIntegerParam("whichDomain",whichParamSet)
	load(theEnvString, theParams)

#
# Mario has 6 params:
#  fast - determines if Mario runs very fast or at playable-speed. Set it to true to train your agent, false if you want to actually see what is going on.
#  dark - make Mario visible or not. Set it to true to make it invisible when training.
#  levelSeed - determines Marios behavior 
#  levelType - 0..2: outdoors/subterranean/other
#  levelDifficulty - 0..10, how hard it is. 
#  instance - 0..9, determines which Mario you run.  
#
#(int) -> void
def loadMario(fast = True, dark = True, levelSeed = 121, levelType = 0, levelDifficulty = 0, instance = 0):
    envNameString = "GeneralizedMario - Java"
    theParams = preload(envNameString)
    
    theParams.setBoolParam("fast",fast)
    theParams.setBoolParam("dark",dark)
    theParams.setIntegerParam("level seed",levelSeed)
    theParams.setIntegerParam("level difficulty",levelDifficulty)
    theParams.setIntegerParam("level type",levelType)
    theParams.setIntegerParam("instance",instance)
    
    load(envNameString, theParams)
