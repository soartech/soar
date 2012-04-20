#!/bin/bash


basePath=.
systemPath=$basePath/system
#Source a script that sets all important functions and variables
source $systemPath/rl-competition-includes.sh

##Agent Stuff
#Item for the class path so your agent can be found
AgentExtraPath=agents/randomAgentJava/bin		 
#Name of the agent class
AgentClassName=RandomAgent
#Max amount of memory to give the agent (Java default is often too low)
AgentMaxMemory=128M

startRLGlueInBackGround
startEnvShellInBackGround
startJavaAgentInBackGround $AgentExtraPath $AgentClassName $AgentMaxMemory
startNetGuiTrainerDynamicEnvironmentStandardAgent

waitForAgentToDie
waitForEnvShellToDie
waitForRLGlueToDie
