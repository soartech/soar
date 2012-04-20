#!/bin/bash
basePath=$COMP_HOME #point $COMP_HOME to local installation of competition software
agentPath=$AGENT_HOME #point $AGENT_HOME to MarioSoar
systemPath=$basePath/system
#Source a script that sets all important functions and variables
source $systemPath/rl-competition-includes.sh
soarPath=$SOAR_HOME/share/java/sml.jar #point $SOAR_HOME to local install of SoarSuite/out/
className=edu.umich.SoarMarioAgent    #Fully Qualified Name of the agent class
maxMemory=128M			 #Max amount of memory to give the agent (Java default is often too low)
extraPath=$AGENT_HOME/java/bin/	 #Item for the class path so your agent can be found
startJavaAgent $extraPath $className $maxMemory $soarPath $1