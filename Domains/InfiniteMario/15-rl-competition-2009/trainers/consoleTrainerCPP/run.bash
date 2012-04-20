#!/bin/bash
basePath=../..
systemPath=$basePath/system
#Source a script that sets all important functions and variables
source $systemPath/rl-competition-includes.sh

consoleTrainer=./bin/consoleTrainer

#Utility functions from rl-competition-includes.sh
startRLGlueInBackGround
startEnvShellInBackGround

$consoleTrainer

#Utility functions from rl-competition-includes.sh
waitForEnvShellToDie
waitForRLGlueToDie
