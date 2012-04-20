#Environments and Agent Jars are generally in here
jarsPath=$systemPath/environmentJars

#Path to all of the RL-Viz libraries and stuff
libPath=$systemPath/libraries
rlvizlibPath=$systemPath/libraries/rl-viz
rlVizLibPath=$rlvizlibPath/RLVizLib.jar
VIZAPP_JAR=$rlvizlibPath/RLVizApp.jar
ENVSHELL_JAR=$rlvizlibPath/EnvironmentShell.jar
AGENTSHELL_JAR=$rlvizlibPath/AgentShell.jar

glueExe=$systemPath/build/bin/rl_glue

guiLib=$libPath/forms-1.1.0.jar

pKillScript=$systemPath/bin/pkill

ENV_AGENT_JARS_PATH=$PWD/$jarsPath

setMacAboutName ()
{ # This is about as simple as functions get.
if [ "$(uname)" == "Darwin" ]; then
	osExtras="-Dcom.apple.mrj.application.apple.menu.about.name=RLVizApp"
fi
}


setCygwinPaths ()
{
if [[ `uname` == CYGWIN* ]]
then
	glueExe="$glueExe.exe"
	ENV_AGENT_JARS_PATH=`cygpath -wp $ENV_AGENT_JARS_PATH`
	VIZAPP_JAR=`cygpath -wp $VIZAPP_JAR`
	rlVizLibPath=`cygpath -wp $rlVizLibPath`
	ENVSHELL_JAR=`cygpath -wp $ENVSHELL_JAR`
	AGENTSHELL_JAR=`cygpath -wp $AGENTSHELL_JAR`
fi
}

makeLine(){
echo "--------------------------------------------------------------------"
}

checkIfRLGlueExists(){
if [ ! -x "$glueExe" ]       # Check if file exists.
  then
	makeLine
    echo "rl_glue not found at $glueExe.  "
	echo "Did you remember to install RL-Glue first?  Check out http://glue.rl-community.org"
  	makeLine
    exit
   fi
}


startEnvShellInBackGround(){
java -enableassertions -Xmx128M -jar $ENVSHELL_JAR environment-jar-path=$ENV_AGENT_JARS_PATH &
envShellPID=$!
echo "-- Starting up dynamic environment loader - PID=$envShellPID"
}

waitForEnvShellToDie(){
echo "-- Waiting for the dynamic environment loader to die..."
wait $envShellPID
echo "++ Dynamic environment loader terminated"
}

startAgentShellInBackGround(){
java -enableassertions -Xmx128M -jar $AGENTSHELL_JAR agent-jar-path=$ENV_AGENT_JARS_PATH &
agentShellPID=$!
echo "-- Starting up dynamic agent loader - PID=$agentShellPID"
}


waitForAgentShellToDie(){
echo "-- Waiting for the dynamic agent loader to die..."
wait $agentShellPID
echo "++ Dynamic agent loader terminated"
}

startLocalGuiTrainer(){
echo "-- Starting up Gui Trainer"
java -enableassertions -Xmx128M $osExtras -jar $VIZAPP_JAR agent-environment-jar-path=$ENV_AGENT_JARS_PATH list-agents=true list-environments=true env-viz=true local-glue=true
echo "++ Gui Trainer is finished"
}
startLocalGuiTrainerBothViz(){
echo "-- Starting up Gui Trainer"
java -enableassertions -Xmx128M $osExtras -jar $VIZAPP_JAR agent-environment-jar-path=$ENV_AGENT_JARS_PATH list-agents=true list-environments=true env-viz=true agent-viz=true local-glue=true
echo "++ Gui Trainer is finished"
}

startNetGuiTrainer(){
echo "-- Starting up Networked Gui Trainer"
java -enableassertions -Xmx128M $osExtras -jar $VIZAPP_JAR agent-environment-jar-path=$ENV_AGENT_JARS_PATH list-agents=true list-environments=true env-viz=true
echo "++ Gui Trainer is finished"
}

startNetGuiTrainerDynamicEnvironmentStandardAgent(){
echo "-- Starting up Networked Gui Trainer"
java -enableassertions -Xmx128M $osExtras -jar $VIZAPP_JAR agent-environment-jar-path=$ENV_AGENT_JARS_PATH list-environments=true env-viz=true
echo "++ Gui Trainer is finished"
}
startNetGuiTrainerDynamicAgentStandardEnvironment(){
echo "-- Starting up Networked Gui Trainer"
java -enableassertions -Xmx128M $osExtras -jar $VIZAPP_JAR agent-environment-jar-path=$ENV_AGENT_JARS_PATH list-agents=true env-viz=true
echo "++ Gui Trainer is finished"
}
startNetGuiTrainerDynamicAgentStandardEnvironmentBothViz(){
echo "-- Starting up Networked Gui Trainer"
java -enableassertions -Xmx128M $osExtras -jar $VIZAPP_JAR agent-environment-jar-path=$ENV_AGENT_JARS_PATH list-agents=true env-viz=true agent-viz=true
echo "++ Gui Trainer is finished"
}



killRLGlue(){
$pKillScript rl_glue
}
startRLGlueInBackGround(){
checkIfRLGlueExists
#Make sure its not running from before
killRLGlue
$glueExe &
gluePID=$!
echo "Starting up RL-glue - PID=$gluePID"
}

waitForRLGlueToDie(){
echo "-- Waiting for RL_glue to die..."
wait $gluePID
echo "++ RL_glue terminated"
}

startJavaAgent(){
privateExtraPath="$1"
agentQualifiedName="$2"
privateMaxMemory="$3"
privateSoarPath="$4" #soar specific configs
configFile="$5" 
agentPath=$rlVizLibPath:$privateExtraPath:$privateSoarPath

echo "Soar path = $privateSoarPath"
#Sortof a hack for now
if [[ `uname` == CYGWIN* ]]
then 
	agentPath=`cygpath -wp $agentPath`
fi
java -Xmx$privateMaxMemory -cp $agentPath $agentQualifiedName $configFile
#java -Xmx$privateMaxMemory $agentQualifiedName $configFile
}

startJavaAgentInBackGround(){
privateExtraPath="$1"
agentQualifiedName="$2"
privateMaxMemory="$3"

agentPath=$rlVizLibPath:$privateExtraPath
#Sortof a hack for now
if [[ `uname` == CYGWIN* ]]
then 
	agentPath=`cygpath -wp $agentPath`
fi

java -Xmx$privateMaxMemory -cp $agentPath $agentQualifiedName &
agentPID=$!
}
waitForAgentToDie(){
echo "-- Waiting for the agent to die..."
wait $agentPID
echo "++ Agent terminated"
}

startJavaEnvironmentInBackGround(){
privateExtraPath="$1"
privatePackageName="$2"
privateClassName="$3"
privateMaxMemory="$4"

envPath=$rlVizLibPath:$privateExtraPath
#Sortof a hack for now
if [[ `uname` == CYGWIN* ]]
then 
	envPath=`cygpath -wp $envPath`
fi

java -Xmx$privateMaxMemory -cp $envPath org.rlcommunity.rlglue.codec.util.EnvironmentLoader $privatePackageName.$privateClassName &
envPID=$!
}
waitForEnvironmentToDie(){
echo "-- Waiting for the environment to die..."
wait $envPID
echo "++ Agent terminated"
}

setMacAboutName
setCygwinPaths
