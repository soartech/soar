#!/bin/bash
basePath=../..
systemPath=$basePath/system
#Source a script that sets all important functions and variables
source $systemPath/rl-competition-includes.sh
BIN_CLASSPATH=bin


#Utility functions from rl-competition-includes.sh
startRLGlueInBackGround
startEnvShellInBackGround

java -Xmx128M -classpath $basePath/system/libraries/rl-viz/RLVizLib.jar:$BIN_CLASSPATH consoleTrainer

#Utility functions from rl-competition-includes.sh
waitForEnvShellToDie
waitForRLGlueToDie

